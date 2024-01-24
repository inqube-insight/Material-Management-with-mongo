server <- function(input, output, session) {
  
  addClass(selector = "body", class = "sidebar-collapse")
  
  observeEvent(week.data, {

    updatePickerInput(session, 'week',
                      choices = week.data$Week %>% unique() %>% sort() %>% na.omit(),
                      selected = week.data$Week %>% unique() %>% sort() %>% na.omit(),
    )

  })
  
  observeEvent(input$week, {
    
    
    req(input$week)
    
    recon.data.season=sales.data %>% filter(Week %in% input$week)
    
    updatePickerInput(session, 'customer',
                      choices = recon.data.season$Buyer %>%  unique() %>% sort() %>% na.omit(),
                      selected = recon.data.season$Buyer %>% unique() %>% sort() %>% na.omit()
    )
    
  })
  
  observeEvent(input$customer, {
    
    req(input$customer,input$week)
    
    recon.data.season = sales.data %>% filter(Buyer %in% input$customer)

    updatePickerInput(session, 'season',
                      choices = recon.data.season$Season %>% unique() %>% sort() %>% na.omit(),
                      selected = recon.data.season$Season %>% unique() %>% sort() %>% na.omit()
    )
  })
  
  
  observeEvent(input$season, {
    
    req(input$season,input$customer,input$week)
     
    recon.data.plant = sales.data %>% filter(Week %in% input$week & Buyer %in% input$customer & Season %in% input$season)

    recon.data.plant=recon.data.plant %>% 
      filter(Plant!='INQUBE PRODUCTION ENGENEERING')
    
    updatePickerInput(session, 'plant',
                      choices = recon.data.plant$Plant %>% unique() %>% sort() ,
                      selected = recon.data.plant$Plant %>% unique() %>% sort(),
    )
  })
  
  
  observeEvent(input$plant, {
    
    recon.data.category = sales.data %>% 
      filter(Week %in% input$week & 
               Buyer %in% input$customer & 
               Season %in% input$season &
               Plant %in% input$plant)
    
    updatePickerInput(session, 'product.category',
                      choices = recon.data.category$P_L_Category %>% unique() %>% sort() %>% na.omit(),
                      selected = recon.data.category$P_L_Category %>% unique() %>% sort() %>% na.omit(),
    )
  })
  
  observeEvent(input$product.category, {
    
    recon.data.style = sales.data %>% 
      filter(Week %in% input$week & 
               Buyer %in% input$customer & 
               Season %in% input$season &
               Plant %in% input$plant &
               P_L_Category %in% input$product.category)
    
    print('cbeche')
    print(head(recon.data.style$Style_Code))
    
    
    updatePickerInput(session, 'style',
                      choices = recon.data.style$Style_Code %>% unique() %>% sort() %>% na.omit(),
                      selected = recon.data.style$Style_Code %>% unique() %>% sort() %>% na.omit(),
    )
    
  })
    
  observeEvent(input$style, {
    
    
    recon.data.oc.type = sales.data %>% 
      filter(Week %in% input$week & 
               Buyer %in% input$customer & 
               Season %in% input$season &
               Plant %in% input$plant &
               P_L_Category %in% input$product.category &
               Style_Code %in% input$style 
             )
    
    updatePickerInput(session, 'oc.type',
                      choices = recon.data.oc.type$OCType %>% unique() %>% sort() %>% na.omit(),
                      selected = recon.data.oc.type$OCType %>% unique() %>% sort() %>% na.omit(),
    )
    
  })
  
  observeEvent(input$oc.type, {
    
    recon.data.oc.type = sales.data %>% 
      filter(Week %in% input$week & 
               Buyer %in% input$customer & 
               Season %in% input$season &
               Plant %in% input$plant &
               P_L_Category %in% input$product.category &
               Style_Code %in% input$style &
               OCType %in% input$oc.type
      )
    
    updatePickerInput(session, 'oc.no',
                      choices = recon.data.oc.type$OCNo %>% unique() %>% sort() %>% na.omit(),
                      selected = recon.data.oc.type$OCNo %>% unique() %>% sort() %>% na.omit(),
    )
    
  })
  
  
  # Open Qty Data 
  
  open.qty =function(){
    
    open=mongo("VPO_Update_Open",db='Weekly_Reconcilation',url=mongo_url,options = ssl_options(weak_cert_validation = TRUE))
    open.qty =open$find(fields='{"Style_Code":1,"Season":1,"Open_Qty":1,"Buyer":1}')
    
    open.qty.data=open.qty %>% mutate(Style_Code=str_sub(Style_Code,1,9))
    
    return(open.qty.data)
  }
  
  #Get WriteOff data
  
  write.off.data = function(){
    
    w.off.data =mongo("Write_Off_Details",db='Weekly_Reconcilation',url=mongo_url,options = ssl_options(weak_cert_validation = TRUE))
    W.Off.Data=w.off.data$find('{}')
    
    return(W.Off.Data)
  }
  
  
  ratio.data = function(){
    
    ratio=mongo("Ratio_Summary",db='Weekly_Reconcilation',url=mongo_url,options = ssl_options(weak_cert_validation = TRUE))
    ratio.data=ratio$find(query='{"Status":"Active"}')
    ratio.data =ratio.data %>%
      mutate(Plant= case_when(
        grepl('QUANTUM CLOTHING HORANA', Plant, ignore.case = T) ~ 'QCL',
        grepl('SUB-QUANTUM APPAREL CAMBODIA', Plant, ignore.case = T) ~ 'QAC',
        grepl('BRANDIX ATHLEISURE POLONNARUWA', Plant, ignore.case = T) ~ 'BALP',
        grepl('BRANDIX ATHLEISURE GIRITALE', Plant, ignore.case = T) ~ 'BALG',
        grepl('MINUWANGODA', Plant, ignore.case = T) ~ 'BAIM',
        grepl('INQUBE SOLUTION SEAMLESS-SEW', Plant, ignore.case = T) ~ 'ISSW',
        TRUE ~ Plant
      ))
    
    return(ratio.data)
  }

  # Calculate Over Issued
  cal.over.issued =function(){
    
    over.deviation.data.con1 =filtered.recon.data() %>% filter(Export != 0)%>%filter(Indent_Type != 'Adhoc Indent') %>% # replace_na(list(select(where(is.numeric))=0,select(where(is.character))='unknown')) %>%
      filter(Total_Order_Qty<=Export & RM_Net_Issued >Required_Qty ) %>% 
      mutate(over.deviation=(RM_Net_Issued-Required_Qty)*Unit_Value)
    
    over.deviation.data.con2 =filtered.recon.data()%>%filter(Export != 0) %>%filter(Indent_Type != 'Adhoc Indent') %>%
      filter(Total_Order_Qty>Export & RM_Net_Issued >Export*Costed_YY_Include_Wastage) %>%
      mutate(over.deviation=(RM_Net_Issued-Costed_YY_Include_Wastage*Export)*Unit_Value)
    
    
    over.deviation.data=over.deviation.data.con1 %>% bind_rows(over.deviation.data.con2)
    over.deviation.value=over.deviation.data  %>% 
      group_by(Week) %>% 
      summarise(Over_Issued=sum(over.deviation,na.rm=T)) %>%mutate(Category='Over Issued')
    

  }
  
  
  filtered.sales.data <-reactive({
   
    sales.data =sales.data %>%
      filter(Plant!='INQUBE PRODUCTION ENGENEERING') %>%
      filter(Week %in% input$week &
             Buyer %in% input$customer &
             Season %in% input$season  &
             Plant %in% input$plant &
             P_L_Category %in% input$product.category &
             OCNo %in% input$oc.no &
             Style_Code %in% input$style)
    
    return(sales.data)
    
  })
  
  rm.data <<-rm.data %>%
    left_join(write.off.data(),by=c('OCNo'='SOC','Material_Item_Code'='Article_Code','Material_Mat_Color_Code'='Article_Color_Code','Style_Code'))
  
  
  filtered.rm.data <-reactive({
    
    rm.data =rm.data %>% filter(Plant!='INQUBE PRODUCTION ENGENEERING') %>%
      filter(Week %in% input$week &
               Buyer %in% input$customer &
               Season %in% input$season &
               Plant %in% input$plant &
               P_L_Category %in% input$product.category &
               Style_Code %in% input$style &
               OCType %in% input$oc.type &
               OCNo %in% input$oc.no )
    return(rm.data)
    
  })
  
  filtered.ratio.data <-reactive({
    
    # print(head(ratio.data))
    
    ratio.data =ratio.data() %>% filter(Plant!='INQUBE PRODUCTION ENGENEERING') %>%
      filter(Week %in% input$week &
               Buyer %in% input$customer &
               Season %in% input$season &
               Plant %in% input$plant &
               P_L_Category %in% input$product.category &
               Style_Code %in% input$style &
               # OCType %in% input$oc.type &
               OCNo %in% input$oc.no )
    
    return(ratio.data)
    
  })
  
  #######Display Sales Value  
  
  output$actual.sales.value <- renderValueBox({
    

    if(isTruthy(input$oc.no)){
      
      Value =filtered.sales.data()
      
      actual.sale.value=sum(Value$Actual_Sales_Value,na.rm=TRUE)
      

      if(nchar(actual.sale.value)>7){
        actual.sale.value=paste0('$ ',round(actual.sale.value/1000000,2),'M')
        
      }
      else{
        actual.sale.value=paste0('$ ',round(actual.sale.value/1000,2),'k')
        
      }
      
      setBorderColor(valueBox(tags$p(actual.sale.value, style = "font-size: 50%;"), tags$p("Actual Sales", style = "font-size: 80%;border-color: #FF0000;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("Actual Sales", style = "font-size: 80%;"),color='black',icon = icon('dollar')),'#FFFFFB')
      
    }
    
  })
  
  
  # Display Actual RMC Value
  
  output$actual.rmc <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value =filtered.rm.data()
      
      actual.rmc.value=paste0(round(sum(Value$Material$Actual_RM_Value,na.rm=TRUE)/sum(filtered.sales.data()$Actual_Sales_Value,na.rm=TRUE)*100,2),'%')
      
      setBorderColor(valueBox(tags$p(actual.rmc.value, style = "font-size: 50%;"), tags$p("Actual RMC", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("Actual RMC", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  # Display Planned RMC Value
  
  output$planned.rmc <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value =filtered.rm.data()

      planned.rmc.value=paste0(round(sum(Value$Material$Costed_RM_Value,na.rm=TRUE)/sum(filtered.sales.data()$Planned_Sales_Value,na.rm=TRUE)*100,2),'%')
      
      setBorderColor(valueBox(tags$p(planned.rmc.value, style = "font-size: 50%;"), tags$p("Planned RMC", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("Planned RMC", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  # Display Planned Wastage
  
  
  output$planned.wastage <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      planned.wastage=sum(filtered.rm.data()$Material$Planned_Wastage,na.rm=TRUE)/sum(filtered.sales.data()$Planned_Sales_Value,na.rm=TRUE)
      
      planned.wastage=round(planned.wastage*100,2)
      
      setBorderColor(valueBox(tags$p(paste0(planned.wastage,'%'), style = "font-size: 50%;"), tags$p("Planned Wastage", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("Planned Wastage", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  output$write.off <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      write_off=sum(as.numeric(filtered.rm.data()$Value),na.rm=TRUE)/sum(filtered.sales.data()$Actual_Sales_Value,na.rm=TRUE)
      
      write_off=round(write_off*100,2)
      
      setBorderColor(valueBox(tags$p(paste0(write_off,'%'), style = "font-size: 50%;"), tags$p("W/Off", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("W/Off", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  
  # Display Over Deviation Value
  
  output$over.deviation <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value =filtered.rm.data()
      
      over.deviation.value=paste0('$ ',round(abs(sum(Value$Material$Over_Deviation,na.rm=TRUE)/1000),2),'k')
      
      setBorderColor(valueBox(tags$p(over.deviation.value, style = "font-size: 50%;"), tags$p("Over Deviation", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("Over Deviation", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  
  # Display Under Deviation Value
  
  
  output$under.deviation <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value =filtered.rm.data()
      
      under.deviation.value=paste0('$ ',round(sum(Value$Material$Under_Deviation,na.rm=TRUE)/1000,2),'k')
      
      setBorderColor(valueBox(tags$p(under.deviation.value, style = "font-size: 50%;"), tags$p("Under Deviation", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("Under Deviation", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  # Display Opportunity Loss
  
  
  output$opportunity.loss <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value =filtered.rm.data()
      
      opportunity.loss.value=paste0('$ ',round(sum(Value$Material$Opportunity_Loss,na.rm=TRUE)/1000,2),'k')
      
      setBorderColor(valueBox(tags$p(opportunity.loss.value, style = "font-size: 50%;"), tags$p("Opportunity Loss", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("Opportunity Loss", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  # Display Optimum RMC
  
  
  output$optimum.rmc <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value =filtered.rm.data()
      
      optimum.rmc.value=paste0(round(sum((Value$Material$Optimum_RMC),na.rm=TRUE)/sum(filtered.sales.data()$Optimum_Sales,na.rm=TRUE)*100,2),'%')
      
      setBorderColor(valueBox(tags$p(optimum.rmc.value, style = "font-size: 50%;"), tags$p("Optimum RMC", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("Optimum RMC", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  # Display W/Off Value
  
  output$write.off.value <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value =filtered.rm.data()
      
      optimum.rmc.value=paste0(round(sum((as.numeric(Value$Value)),na.rm=TRUE)/1000,2),'k')
      
      setBorderColor(valueBox(tags$p(optimum.rmc.value, style = "font-size: 50%;"), tags$p("W/Off Value", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("W/Off Value", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  
  rmc.chart.df <- reactive({
    
    
    filtered.rm.data=rm.data %>% filter(Week %in% input$week)
    filtered.sales.data=sales.data %>% filter(Week %in% input$week)
    x=''

    if(input$rmc.category=="Product Type"){
      
      Actual_RM_Value = filtered.rm.data %>% group_by(P_L_Category) %>% summarise(Actual_RM_Value=sum(Material$Actual_RM_Value,na.rm=TRUE))
      Actual_Sales_Value =filtered.sales.data%>% group_by(P_L_Category) %>% summarise(Actual_Sales_Value=sum(Actual_Sales_Value,na.rm=TRUE))
      Actual_Percentage= Actual_RM_Value %>% inner_join(Actual_Sales_Value,by='P_L_Category')%>% mutate(Value=(Actual_RM_Value/Actual_Sales_Value)*100,
                                                                                                    Category='Actual')
      
      Costed_RM_Value= filtered.rm.data %>% group_by(P_L_Category) %>% summarise(Costed_RM_Value=sum(Material$Costed_RM_Value,na.rm=TRUE))
      Planned_Sales_Value =filtered.sales.data %>% group_by(P_L_Category) %>% summarise(Planned_Sales_Value=sum(Planned_Sales_Value,na.rm=TRUE))
      Costed_Percentage= Costed_RM_Value %>% inner_join(Planned_Sales_Value,by='P_L_Category')%>% mutate(Value=(Costed_RM_Value/Planned_Sales_Value)*100,
                                                                                                     Category='Costed')
      
      Optimum_RM_Value= filtered.rm.data %>% group_by(P_L_Category) %>% summarise(Optimum_RMC=sum(Material$Optimum_RMC,na.rm=TRUE))
      Optimum_Sales_Value = filtered.sales.data %>% group_by(P_L_Category) %>% summarise(Optimum_Sales=sum(Optimum_Sales,na.rm=TRUE))
      Optimum_Percentage= Optimum_RM_Value %>% left_join(Optimum_Sales_Value,by='P_L_Category')%>%
        mutate(Value=(Optimum_RMC/Optimum_Sales)*100, Category='Optimum')
      
      x=Actual_Percentage %>% select(P_L_Category,Category,Value) %>% bind_rows(Costed_Percentage %>%select(P_L_Category,Category,Value))%>%
        bind_rows(Optimum_Percentage %>%select(P_L_Category,Category,Value)) %>%
        na.omit() %>% mutate(Category=as.factor(Category))%>%  filter(P_L_Category !='NA') %>%
        group_by(Category,P_L_Category) %>%
        summarise(Value = sum(Value,na.rm=TRUE), .groups = 'drop') %>%
        arrange(P_L_Category) 
      
      
    }
    else if(input$rmc.category=="Buyer"){
      
      Actual_RM_Value = filtered.rm.data %>% group_by(Buyer) %>% summarise(Actual_RM_Value=sum(Material$Actual_RM_Value,na.rm=TRUE))
      Actual_Sales_Value =filtered.sales.data %>% group_by(Buyer) %>% summarise(Actual_Sales_Value=sum(Actual_Sales_Value,na.rm=TRUE))
      Actual_Percentage= Actual_RM_Value %>% inner_join(Actual_Sales_Value,by='Buyer')%>% mutate(Value=(Actual_RM_Value/Actual_Sales_Value)*100,
                                                                                                    Category='Actual')
      
      Costed_RM_Value= filtered.rm.data %>% group_by(Buyer) %>% summarise(Costed_RM_Value=sum(Material$Costed_RM_Value,na.rm=TRUE))
      Planned_Sales_Value = filtered.sales.data %>% group_by(Buyer) %>% summarise(Planned_Sales_Value=sum(Planned_Sales_Value,na.rm=TRUE))
      Costed_Percentage= Costed_RM_Value %>% inner_join(Planned_Sales_Value,by='Buyer')%>% mutate(Value=(Costed_RM_Value/Planned_Sales_Value)*100,
                                                                                                     Category='Costed')

      Optimum_RM_Value= filtered.rm.data %>% group_by(Buyer) %>% summarise(Optimum_RMC=sum(Material$Optimum_RMC,na.rm=TRUE))
      Optimum_Sales_Value = filtered.sales.data%>% group_by(Buyer) %>% summarise(Optimum_Sales=sum(Optimum_Sales,na.rm=TRUE))
      Optimum_Percentage= Optimum_RM_Value %>% 
        left_join(Optimum_Sales_Value,by='Buyer')%>%
        mutate(Value=(Optimum_RMC/Optimum_Sales)*100,
               Category='Optimum')
      
     x= Actual_Percentage %>% select(Buyer,Category,Value) %>% bind_rows(Costed_Percentage %>%select(Buyer,Category,Value))%>%
        bind_rows(Optimum_Percentage %>%select(Buyer,Category,Value)) %>%
        na.omit() %>% mutate(Category=as.factor(Category))%>%  filter(Buyer !='NA') %>%
        group_by(Category,Buyer) %>%
        summarise(Value = sum(Value,na.rm=TRUE), .groups = 'drop') %>%
        arrange(Buyer) 
    }
    else{
      
      Actual_RM_Value = filtered.rm.data %>% group_by(Plant) %>% summarise(Actual_RM_Value=sum(Material$Actual_RM_Value,na.rm=TRUE))
      Actual_Sales_Value =filtered.sales.data%>% group_by(Plant) %>% summarise(Actual_Sales_Value=sum(Actual_Sales_Value,na.rm=TRUE))
      Actual_Percentage= Actual_RM_Value %>% inner_join(Actual_Sales_Value,by='Plant')%>% 
        mutate(Value=(Actual_RM_Value/Actual_Sales_Value)*100,Category='Actual')
      
      Costed_RM_Value= filtered.rm.data %>% group_by(Plant) %>% summarise(Costed_RM_Value=sum(Material$Costed_RM_Value,na.rm=TRUE))
      Planned_Sales_Value =filtered.sales.data %>% group_by(Plant) %>% summarise(Planned_Sales_Value=sum(Planned_Sales_Value,na.rm=TRUE))
      Costed_Percentage= Costed_RM_Value %>% inner_join(Planned_Sales_Value,by='Plant')%>% mutate(Value=(Costed_RM_Value/Planned_Sales_Value)*100,Category='Costed')
      
      Optimum_RM_Value= filtered.rm.data %>% group_by(Plant) %>% summarise(Optimum_RMC=sum(Material$Optimum_RMC,na.rm=TRUE))
      Optimum_Sales_Value = filtered.sales.data %>% group_by(Plant) %>% summarise(Optimum_Sales=sum(Optimum_Sales,na.rm=TRUE))
      Optimum_Percentage= Optimum_RM_Value %>% left_join(Optimum_Sales_Value,by='Plant')%>% mutate(Value=(Optimum_RMC/Optimum_Sales)*100,
                                                                                                      Category='Optimum')
      
      x=Actual_Percentage %>% select(Plant,Category,Value) %>% bind_rows(Costed_Percentage %>%select(Plant,Category,Value))%>%
        bind_rows(Optimum_Percentage %>%select(Plant,Category,Value)) %>%
        na.omit() %>% mutate(Category=as.factor(Category))%>%  filter(Plant !='NA') %>%
        group_by(Category,Plant) %>%
        summarise(Value = sum(Value,na.rm=TRUE), .groups = 'drop') %>%
        arrange(Plant) 
    }
    return(x)
  })
  
  
  
  output$rmc.chart <- renderApexchart({
    
    req(input$rmc.category)
    
    chart=''

    if(input$rmc.category=="Product Type"){
      
      chart=rmc.chart.df()%>%
        apex(aes(x =P_L_Category , y = Value, group = Category), type = 'area')
    }
    
    else if(input$rmc.category=="Buyer"){
      
      chart=rmc.chart.df()%>%
        apex(aes(x =Buyer , y = Value, group = Category), type = 'area')
      
    }
    else{
      
      
      chart=rmc.chart.df()%>%
        apex(aes(x =Plant , y = Value, group = Category), type = 'area')
      
    }
    
    # rmc.chart.df()%>%
      # apex(aes(x = P_L_Category, y = Value, group = Category), type = 'area') %>%
    chart%>%
      ax_stroke(curve = 'smooth') %>% ax_chart(foreColor='#FFFFFB')%>%
      # ax_colors_manual(list('Actual' = '#F5A65B', 'Costed' = '#4392F1','Optimum'='#26A96C') %>% as.list()) %>%
      ax_markers(size = 5)%>%ax_grid(borderColor = '#FFFFFB') %>%
      # ax_colors("#FF0000") %>%
      ax_title(text='Planned RMC,Actual RMC and Optimum RMC',style=list(fontSize=14,color='#FFFFFB')) %>%
      ax_legend(position = 'top', horizontalAlign = 'left',labels=list(foreColor=c('#FFFFFB','#00000'))) %>%
      ax_tooltip(theme='light',y = list(formatter = format_num(',.4s', suffix = '% '))) %>%
      ax_yaxis(axisBorder = list(show = TRUE),labels = list(formatter = format_num(',.4s', suffix = '% ')),title = list(text='Percentage'),
      min = NULL,max=NULL, forceNiceScale = F) %>% #e_title(text = "Weekly RMC") %>%
      ax_xaxis(axisBorder = list(show = TRUE),labels=list(trim=TRUE,maxHeight=80))%>%#,title = list(text='Product Types'))%>%
      suppressWarnings() %>% ax_grid(borderColor = '#637081') 
    
  })
  
  
  output$deviation.chart <- renderApexchart({
    
    filtered.rm.data=rm.data %>% filter(Week %in% input$week)

    Over_Deviation_Data=filtered.rm.data %>% 
      with_groups(c(Buyer),
                 summarise,
                 Value=sum(Material$Over_Deviation,na.rm=TRUE)) %>% mutate(Category='Over Deviation') %>%
      bind_rows(filtered.rm.data %>%with_groups(c(Buyer),
                                       summarise,
                                       Value=sum(Material$Under_Deviation,na.rm=TRUE)) %>% mutate(Category='Under Deviation')) 
    
    Over_Deviation_Data %>%apex(aes(x = Buyer, y = Value, group = Category),type = 'bar') %>%
      ax_legend(position = 'top', horizontalAlign = 'left',labels=list(foreColor=c('#FFFFFB','#00000'))) %>%
      ax_tooltip(theme='light',y = list(formatter = format_num(',.4s', suffix = ' '))) %>% ax_chart(foreColor='#FFFFFB') %>%
      ax_title(text='Deviation',style=list(fontSize=14,color='#FFFFFB')) %>%
      ax_xaxis(labels = list(formatter = format_num(',.2s')), min = NULL,max=NULL, forceNiceScale = T)      
    
  })
  
  
  # Get Opportunity Loss Data
  
  opportunity.loss.df=reactive({
    
    if(input$top.ten.opportunity.loss=='Style'){
      
      summary.left.stock= rm.data %>%filter(Week %in% input$week & Buyer %in% input$customer ) %>% 
        with_groups(c(Buyer,Style_Code),
                    summarise,
                    Over_Deviation=round(abs(sum(Material$Over_Deviation,na.rm=TRUE)),2),
                    Under_Deviation=round(sum(Material$Under_Deviation,na.rm=TRUE),2),
                    Opportunity_Loss=round(sum(Material$Opportunity_Loss,na.rm=TRUE),2))%>% 
        mutate(Style_Code=str_sub(Style_Code,1,9)) %>% arrange(desc(Opportunity_Loss))
    }
    else{
      summary.left.stock= rm.data %>%filter(Week %in% input$week & Buyer %in% input$customer ) %>% 
        group_by(Material$Item_Code,Buyer) %>%
        summarise(Over_Deviation=round(abs(sum(Material$Over_Deviation,na.rm=TRUE)),2),
                  Under_Deviation=round(sum(Material$Under_Deviation,na.rm=TRUE),2),
                  Opportunity_Loss=round(sum(Material$Opportunity_Loss,na.rm=TRUE),2))%>% 
       arrange(desc(Opportunity_Loss)) %>%
       rename('Item_Code'='Material$Item_Code')
  
    }
    
    
  })
  
  
  # Download Opportunity Loss Data
  
  output$download.opportunity.loss <- downloadHandler(
    
    filename = function() { 'Opportunity Loss.xlsx' },
    content = function(cont) {
      opportunity.loss.df()%>%
        setNames(nm = gsub("_", " ", colnames(.))) %>%
        writexl::write_xlsx(cont,col_names = TRUE,format_headers = TRUE) 
    }
  )
  
  # Display top 10 in opportunity loss
  
  output$opportunity.loss.summary <- renderReactable({
    
    summary.left.stock=opportunity.loss.df()
    
    cols=''
    
    if(input$top.ten.opportunity.loss=='Style'){
      cols=list(
        
        Opportunity_Loss = colDef(
          name = 'Opportunity Loss($)', 
          format = colFormat(separators = T, digits = 0)
        ),
        Style_Code = colDef(
          name = 'Style Code',
        ),
        Under_Deviation = colDef(
          name = 'Under Deviation($)', 
          format = colFormat(separators = T, digits = 0)
        ),
        Over_Deviation = colDef(
          name = 'Over Deviation($)', 
          format = colFormat(separators = T, digits = 0)
        )
        
      )
    }
    else{
      
      cols=list(
        Opportunity_Loss = colDef(
          name = 'Opportunity Loss($)', 
          format = colFormat(separators = T, digits = 0)
        ),
        Item_Code = colDef(
          name = 'Item Code',
        ),
        Under_Deviation = colDef(
          name = 'Under Deviation($)', 
          format = colFormat(separators = T, digits = 0)
        ),
        Over_Deviation = colDef(
          name = 'Over Deviation($)', 
          format = colFormat(separators = T, digits = 0)
        )
        
      )
      
    }

    reactable(head(summary.left.stock,10),searchable = F, highlight = T,
              wrap = T, outlined = T, borderless = F,resizable = TRUE,
              pagination = F, sortable = T,
              theme = reactableTheme(
                color = '#FFFFFB',
                backgroundColor = '#000000',
                searchInputStyle = list(width = "30%"),
                headerStyle = list(background = "#05204A", color = "white", fontWeight = "normal"),
                footerStyle = list(fontWeight = "normal", background = "#FFFFFB"),
                rowSelectedStyle = list(backgroundColor = "#FFFF99", boxShadow = "inset 2px 0 0 0 #41ab5d")
              ), 
              columns = cols,
              
    )
  })
  
  #######Display Planned Sales Value
  
  output$planned.sales.value <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value =filtered.sales.data() 
      
      planned.sales=sum(Value$Planned_Sales_Value,na.rm=TRUE)
      
      
      if(nchar(planned.sales)>7){
        planned.sales=paste0('$ ',round(planned.sales/1000000,2),'M')
        
      }
      else{
        planned.sales=paste0('$ ',round(planned.sales/1000,2),'k')
        
      }
      
      # planned.sales=paste0('$ ',round(sum((Value$Planned_Sales_Value),na.rm=TRUE)/1000,2),'k')
      
      setBorderColor(valueBox(tags$p(planned.sales, style = "font-size: 50%;"), tags$p("Planned Sales", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("Planned Sales" , style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  
  #######Display Sales Value  
  
  output$actual.sales <- renderValueBox({
    

    if(isTruthy(input$oc.no)){
      
      Value =filtered.sales.data()
      
      actual.sale.value=sum(Value$Actual_Sales_Value,na.rm=TRUE)
      
      
      if(nchar(actual.sale.value)>7){
        
        actual.sale.value=paste0('$ ',round(actual.sale.value/1000000,2),'M')
        
      }
      else{
        
        actual.sale.value=paste0('$ ',round(actual.sale.value/1000,2),'k')
        
      }
      

      setBorderColor(valueBox(tags$p(actual.sale.value, style = "font-size: 50%;"), tags$p("Actual Sales", style = "font-size: 80%;border-color: #FF0000;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("Actual Sales", style = "font-size: 80%;"),color='black',icon = icon('dollar')),'#FFFFFB')
      
    }
    
  })
  
  #######Display Optimum Sales Value
  
  
  output$optimum.sales <- renderValueBox({
    
    if(isTruthy(input$oc.no)){

      Value =filtered.sales.data()
      
      optimum.sales.value=sum(Value$Optimum_Sales,na.rm=TRUE)
      
      
      if(nchar(optimum.sales.value)>7){
        optimum.sales.value=paste0('$ ',round(optimum.sales.value/1000000,2),'M')
        
      }
      else{
        optimum.sales.value=paste0('$ ',round(optimum.sales.value/1000,2),'k')
        
      }
      

      setBorderColor(valueBox(tags$p(optimum.sales.value, style = "font-size: 50%;"), tags$p("Optimum Sales", style = "font-size: 80%;border-color: #FF0000;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("Optimum Sales", style = "font-size: 80%;"),color='black',icon = icon('dollar')),'#FFFFFB')
      
    }
    
  })
  
  #######Display Loss of Sales Value
  

  output$loss.of.sales <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value =filtered.sales.data()
      
      loss.of.sales.value=paste0('$ ',round(sum(Value$Loss_Of_Sales,na.rm=TRUE)/1000,2),'k')
      
      setBorderColor(valueBox(tags$p(loss.of.sales.value, style = "font-size: 50%;"), tags$p("Loss of Sales", style = "font-size: 80%;border-color: #FF0000;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("Loss of Sales'", style = "font-size: 80%;"),color='black',icon = icon('dollar')),'#FFFFFB')
      
    }
    
  })
  
  
  #######Display Order to Ship Ratio
  
  output$order.to.ship <- renderEcharts4r({
    

    req(input$oc.no)
    
    data=filtered.ratio.data()

    Order_to_Ship_Value= sum(data$OS_Ship_Qty,na.rm=TRUE)/sum(data$OS_Total_Order_Qty,na.rm=TRUE)
    
    order.ship.value=round(Order_to_Ship_Value*100,2)
    
    e_chart() %>%
      e_gauge(value = order.ship.value, " ",rm_x=TRUE,rm_y=TRUE,
              startAngle =180,
              endAngle = 360,
              min = 0, max = 100,
              splitNumber = 4,
              axisLabel = list(color = "white"),
              formatString = '{order.ship.value*100}% Complete',
              axisLine = list(lineStyle = list(width = 3,
                                               color = list(
                                                 list(0.2, '#66bd63'),
                                                 list(0.4, '#a6d96a'),
                                                 list(0.6, '#fee08b'),
                                                 list(0.8, '#fdae61'),
                                                 list(1, '#d73027')
                                               )
              )
              ),
              axisTick = list(length = 10,
                              lineStyle = list(
                                color = 'auto',
                                width = 1
                              )),
              splitLine = list(length = 20,
                               lineStyle = list(color = 'auto')),
              pointer = list(itemStyle = list(color = 'white')),
              title = list(fontSize = 15,
                           offsetCenter = list('0%', '-100%')),
              detail = list(fontSize = 15,offsetCenter = list('-5%', '60%'),formatter = "{value}%",color='white'
              )
              # tags$script("$('.gauge.html-widget').css({height: '100px', width: '22%'})")
              
              
      )
  })
  
  
  #######Display Cut to Ship Ratio
  
  output$cut.to.ship <- renderEcharts4r({
    
    req(input$oc.no)
    
    data=filtered.ratio.data()
    
    Cut_to_Ship_Value= sum(data$CS_Ship_Qty,na.rm=TRUE)/sum(data$CS_Received_Qty,na.rm=TRUE)
    
    cut.ship.value=round(Cut_to_Ship_Value*100,2)
    

    e_chart() %>%
      e_gauge(value = cut.ship.value, " ",rm_x=TRUE,rm_y=TRUE,
              startAngle =180,
              endAngle = 360,
              min = 0, max = 100,
              splitNumber = 4,
              axisLabel = list(color = "white"),
              axisLine = list(lineStyle = list(width = 3,
                                               color = list(
                                                 list(0.2, '#66bd63'),
                                                 list(0.4, '#a6d96a'),
                                                 list(0.6, '#fee08b'),
                                                 list(0.8, '#fdae61'),
                                                 list(1, '#d73027')
                                               )
              )
              ),
              axisTick = list(length = 10,
                              lineStyle = list(
                                color = 'auto',
                                width = 1
                              )),
              splitLine = list(length = 20,
                               lineStyle = list(color = 'auto')),
              pointer = list(itemStyle = list(color = 'white')),
              title = list(fontSize = 15,
                           offsetCenter = list('0%', '-100%')),
              detail = list(fontSize = 15,offsetCenter = list('-5%', '60%'),formatter = "{value}%",color='white')
              
      )
  })
  
  #######Display Order to Cut Ratio
  
  output$order.to.cut <- renderEcharts4r({
    
    req(input$oc.no)
    
    data =filtered.ratio.data()
    
    Order_to_Cut_Value=sum(data$OC_Received_Qty,na.rm=TRUE)/sum(data$OC_Total_Order_Qty,na.rm=TRUE)
      
    order.cut.value=round(Order_to_Cut_Value*100,2)
    
    e_chart() %>% #e_title(text = "Order to Cut") %>%
      e_gauge(value = order.cut.value, "",rm_x=TRUE,rm_y=TRUE,
              startAngle =180,
              endAngle = 360,
              min = 0, max = 100,
              splitNumber = 4,
              axisLabel = list(color = "white"),
              axisLine = list(lineStyle = list(width = 3,
                                               color = list(
                                                 list(0.2, '#66bd63'),
                                                 list(0.4, '#a6d96a'),
                                                 list(0.6, '#fee08b'),
                                                 list(0.8, '#fdae61'),
                                                 list(1, '#d73027')
                                               )
              )
              ),
              axisTick = list(length = 10,
                              lineStyle = list(
                                color = 'auto',
                                width = 1
                              )),
              splitLine = list(length = 20,
                               lineStyle = list(color = 'auto')),
              pointer = list(itemStyle = list(color = 'white')),
              title = list(fontSize = 15,
                           offsetCenter = list('0%', '-100%')),
              detail = list(fontSize = 15,offsetCenter = list('-5%', '60%'),formatter = "{value}%",color='white')
      ) %>% e_tooltip(formatter = JS("function(params) {
        return params.value + '%';
      }"))
  })
  
  
  #######Display Cut to Produce Ratio
  
  
  output$cut.to.prod <- renderEcharts4r({
    
    req(input$oc.no)
    
    data=filtered.ratio.data() %>% filter(!grepl('SUB|CAMBODIA',Plant,ignore.case = T))
    
    Cut_to_Produce_Value=sum(data$CP_Produce_Received_Qty,na.rm=TRUE)/sum(data$CP_Cutting_Received_Qty,na.rm=TRUE)
    
    cut.prod.value=round(Cut_to_Produce_Value*100,2)     
    e_chart() %>%
      e_gauge(value = cut.prod.value, " ",rm_x=TRUE,rm_y=TRUE,
              startAngle =180,
              endAngle = 360,
              min = 0, max = 100,
              axisLabel = list(color = "white"),
              splitNumber = 4,
              axisLine = list(lineStyle = list(width = 3,
                                               color = list(
                                                 list(0.2, '#66bd63'),
                                                 list(0.4, '#a6d96a'),
                                                 list(0.6, '#fee08b'),
                                                 list(0.8, '#fdae61'),
                                                 list(1, '#d73027')
                                               )
              )
              ),
              axisTick = list(length = 10,
                              lineStyle = list(
                                color = 'auto',
                                width = 1
                              )),
              splitLine = list(length = 20,
                               lineStyle = list(color = 'auto')),
              pointer = list(itemStyle = list(color = 'white')),
              title = list(object='Cut to Produce Ratio',color='white',fontSize = 15,
                           offsetCenter = list('0%', '-100%')),
              detail = list(fontSize = 15,offsetCenter = list('-5%', '60%'),formatter = "{value}%",color='white')
              
      )
  })
  
  output$ratio.chart <- renderApexchart({

    req(input$oc.no)

    data =ratio.data() %>% filter(Buyer %in% input$customer)
    Order_to_Ship=data %>%group_by(Week) %>% summarise(Value=sum(OS_Ship_Qty,na.rm=TRUE)/sum(OS_Total_Order_Qty,na.rm=TRUE)) %>% mutate(Category='Order_to_Ship')
    Cut_to_Ship=data %>%group_by(Week) %>% summarise(Value=sum(CS_Ship_Qty,na.rm=TRUE)/sum(CS_Received_Qty,na.rm=TRUE))%>% mutate(Category='Cut_to_Ship')
    Order_to_Cut=data %>%group_by(Week) %>% summarise(Value=sum(OC_Received_Qty,na.rm=TRUE)/sum(OC_Total_Order_Qty,na.rm=TRUE))%>% mutate(Category='Order_to_Cut')
    Cut_to_Produce=data %>%filter(!grepl('SUB|CAMBODIA',Plant,ignore.case = T)) %>% group_by(Week) %>% summarise(Value=sum(CP_Produce_Received_Qty,na.rm=TRUE)/sum(CP_Cutting_Received_Qty,na.rm=TRUE))%>% mutate(Category='Cut_to_Produce')


    Order_to_Ship %>% select(Week,Category,Value) %>% bind_rows(Cut_to_Ship %>%select(Week,Category,Value))%>%
      bind_rows(Order_to_Cut %>%select(Week,Category,Value))%>%bind_rows(Cut_to_Produce %>%select(Week,Category,Value))%>%
      na.omit() %>% mutate(Category=as.factor(Category))%>%  filter(Week !='NA') %>%
      group_by(Category,Week) %>%
      summarise(Value = sum(Value,na.rm=TRUE), .groups = 'drop') %>%
      arrange(Week) %>%
      apex(aes(x = Week, y = Value*100, group = Category), type = 'line') %>%
      ax_stroke(curve = 'smooth') %>% ax_chart(foreColor='#FFFFFB')%>%
      ax_colors_manual(c('Cut_to_Produce' = '#66bd63', 'Order_to_Cut' = '#a6d96a','Cut_to_Ship'='#fdae61', 'Order_to_Ship'='#d73027') %>% as.list()) %>% #'#92c5de', '#e6f598', '#053061', '#fb6a4a'
      ax_markers(size = 5)%>% 
      ax_colors("#FF0000") %>%ax_grid(borderColor = '##000000') %>%
      ax_title(text='Trend',style=list(fontSize=14,color='#FFFFFB')) %>%
      ax_legend(position = 'top', horizontalAlign = 'left',labels=list(foreColor=c('#FFFFFB','#00000'))) %>%
      ax_tooltip(theme='light',y = list(formatter = format_num(',.4s', suffix = '% '))) %>%
      ax_yaxis(labels = list(formatter = format_num(',.4s', suffix = '% ')),title = list(text = "Percentage"),
               min = NULL,max=NULL, forceNiceScale = T) %>% #e_title(text = "Weekly RMC") %>%
      ax_xaxis(title = list(text = "Week")) %>%
      suppressWarnings()

  })
  
  ratio.category.df <- reactive({
    
    filtered.ratio.data=ratio.data() %>% filter(Week %in% input$week)
    
    x=''
    
    if(input$ratio.category=="Buyer"){
      
      order.to.ship=filtered.ratio.data %>%with_groups(c(Buyer), 
                                                                summarise,
                                                                Value=sum(OS_Ship_Qty,na.rm=TRUE)/sum(OS_Total_Order_Qty,na.rm=TRUE)*100) %>% 
        mutate(Category='Actual')
      
      optimum.qty=filtered.ratio.data %>%with_groups(c(Buyer), 
                                                   summarise,
                                                   Value=sum(Optimum_Qty,na.rm=TRUE)/sum(DOC_Order_Qty,na.rm=TRUE)*100) %>%
        mutate(Category='Target')

      
      x=order.to.ship %>% bind_rows(optimum.qty)
      
      
    }
    else{
      
      order.to.ship=filtered.ratio.data %>%with_groups(c(Plant), 
                                                                summarise,
                                                                Value=sum(OS_Ship_Qty,na.rm=TRUE)/sum(OS_Total_Order_Qty,na.rm=TRUE)*100) %>%
        mutate(Category='Actual')
      
      optimum.qty=filtered.ratio.data %>%with_groups(c(Plant), 
                                                   summarise,
                                                   Value=sum(Optimum_Qty,na.rm=TRUE)/sum(DOC_Order_Qty,na.rm=TRUE)*100) %>%
        mutate(Category='Target')
      

      x=order.to.ship %>% bind_rows(optimum.qty)
      
    }
    
    return(x)
    
  })
  
  
  output$ratio.category.chart <- renderApexchart({
    
    req(input$ratio.category)

    chart=''
    
    if(input$ratio.category=="Buyer"){
      
      chart=ratio.category.df()%>%
        apex(aes(x =Buyer , y = Value,color = Category), type = 'area')
    }
    
    else{
      
      chart=ratio.category.df()%>%
        apex(aes(x =Plant , y = Value, group = Category), type = 'bar')
      
    }
    # rmc.chart.df()%>%
    #   apex(aes(x = , y = Value, group = Category), type = 'area') %>%
    chart%>%
      # ax_stroke(curve = 'smooth') %>% 
      ax_chart(foreColor='#FFFFFB')%>%
      # ax_colors_manual(c('Actual' = '#F5A65B', 'Costed' = '#4392F1','Optimum'='#26A96C') %>% as.list()) %>%
      ax_markers(size = 5)%>%ax_grid(borderColor = '#FFFFFB') %>%
      # ax_colors("#FF0000") %>%
      ax_title(text='Order to Ship Ratio',style=list(fontSize=14,color='#FFFFFB')) %>%
      ax_legend(position = 'top', horizontalAlign = 'left',labels=list(foreColor=c('#FFFFFB','#00000'))) %>%
      ax_tooltip(theme='light',y = list(formatter = format_num(',.4s', suffix = '% '))) %>%
      ax_yaxis(axisBorder = list(show = TRUE),labels = list(formatter = format_num(',.4s', suffix = '% ')),title = list(text='Percentage'),
               min = NULL,max=NULL, forceNiceScale = F) %>% #e_title(text = "Weekly RMC") %>%
      ax_xaxis(axisBorder = list(show = TRUE),labels=list(trim=TRUE,maxHeight=80),tooltip=list(enabled=TRUE))%>%#,title = list(text='Product Types'))%>%
      suppressWarnings() %>% ax_grid(borderColor = '#637081') 
    
  })
  
  output$myHC <- renderHighchart({
    
    req(input$oc.no)
    data=filtered.rm.data()
    
    avLE = filtered.sales.data() %>% filter(Loss_Of_Sales>0) %>%
      group_by(Buyer) %>% 
      mutate(Loss_of_Sales_Value = round(sum(Loss_Of_Sales),2)) %>% # average by Plant
      ungroup() %>% group_by(Plant,Buyer) %>% 
      mutate(caLE = round(sum(Loss_Of_Sales),2)) %>%
      group_by(Plant,Buyer,P_L_Category) %>% 
      mutate(PBTLE = round(sum(Loss_Of_Sales),2)) %>% # average by P_L_Category
      ungroup() %>% arrange(desc(Loss_of_Sales_Value)) %>%  # order by life expectancy for Plants
    mutate_if(is.numeric, round, 2)  # round to 2 decimals
    
    gapDD2 = avLE %>% 
      arrange(P_L_Category) %>%  
      group_nest(Plant, Buyer, caLE) %>% # keep these variables!
      mutate(id = Plant,
             type = "column", 
             data = map(data, mutate, name = P_L_Category, y = PBTLE,
                        color = colorize(P_L_Category)), # set the color (easier with #)
             data = map(data, list_parse))
    
    gapDD1 = avLE %>% 
      arrange(Plant) %>%  # arrange by Buyer, set as ordered, then find colors
      mutate(Plant = ordered(Plant, levels = unique(Plant))) %>%
      mutate(color = ordered(colorize(Plant),     # colors/countries align
                             levels = unique(colorize(Plant)))) %>% 
      group_nest(Buyer) %>% 
      mutate(id = Buyer,
             type = "column", 
             data = map(data, mutate, name = Plant, y = caLE, 
                        color = color,  # set the color (a few more steps than with #s)
                        drilldown = Plant),
             data = map(data, list_parse))
    
    gapCol = avLE %>%  # set the continets in the validated avLE as ordered
      group_by(Buyer) %>% 
      mutate(color = colorize(Buyer),
             Buyer = ordered(Buyer, 
                             levels = unique(avLE$Buyer)))
    
    
    hchart(gapCol, "column", name = "Loss of Sales",
           hcaes(x = Buyer, color = Buyer, y = Loss_of_Sales_Value,
                 name = "Buyer", drilldown = "Buyer")) %>%
      hc_drilldown(allowPointsDrillDown = T,
                   series = c(list_parse(gapDD1), list_parse(gapDD2))
                   ) %>% hc_tooltip(backgroundColor='#000000') %>%
      # hc_chart(type = "column",
      #           options3d = list(enabled = TRUE, beta = 15, alpha = 15)
      #        ) %>%
      hc_exporting(enabled = TRUE,filename = "custom-file-name") %>%
      hc_title(text = "Loss of Sales",align='left') 
  
    
  })
  
  ratio.summary.df <- reactive({
    
    
    cut.to.ship=filtered.ratio.data()%>% with_groups(c(Buyer,P_L_Category,Plant,Season,Style_Code),
                                                            summarise,
                                                            Cut_to_Ship=sum(CS_Ship_Qty,na.rm=TRUE)/sum(CS_Received_Qty,na.rm=TRUE)*100) 
    
    order.to.cut=filtered.ratio.data() %>% with_groups(c(Buyer,P_L_Category,Plant,Season,Style_Code),
                                                             summarise,
                                                             Order_to_Cut=sum(OC_Received_Qty,na.rm=TRUE)/sum(OC_Total_Order_Qty,na.rm=TRUE)*100)

    order.to.ship=filtered.ratio.data() %>%with_groups(c(Buyer,P_L_Category,Plant,Season,Style_Code), 
                                                              summarise,
                                                              OS_Ship_Qty=sum(OS_Ship_Qty,na.rm=TRUE),
                                                              OS_Total_Order_Qty=sum(OS_Total_Order_Qty,na.rm=TRUE),
                                                              Order_to_Ship=sum(OS_Ship_Qty,na.rm=TRUE)/sum(OS_Total_Order_Qty,na.rm=TRUE)*100) 
    cut.to.produce=filtered.ratio.data()%>% filter(!grepl('SUB|CAMBODIA',Plant,ignore.case = T)) %>% with_groups(c(Buyer,P_L_Category,Plant,Season,Style_Code),
                                                                 summarise,
                                                                 Cut_to_Produce=sum(CP_Produce_Received_Qty,na.rm=TRUE)/sum(CP_Cutting_Received_Qty,na.rm=TRUE)*100)
    
    actual.sales.data=filtered.sales.data() %>% with_groups(c(Buyer,P_L_Category,Plant,Season,Style_Code),
                                                                   summarise,
                                                                   Actual_Sales_Value=sum(Actual_Sales_Value,na.rm=TRUE))
        
    summary.data=cut.to.ship %>% merge(order.to.ship,by=c('Buyer','Plant','P_L_Category','Season','Style_Code')) %>%
      merge(order.to.cut,by=c('Buyer','Plant','P_L_Category','Season','Style_Code')) %>%
      merge(cut.to.produce,by=c('Buyer','Plant','P_L_Category','Season','Style_Code')) %>% 
      merge(actual.sales.data,by=c('Buyer','Plant','P_L_Category','Season','Style_Code')) %>% 
      mutate(across(where(is.numeric), ~round(.,digits=2)))

    return(summary.data)
  })
  
  # Download Ratios
  
  output$download.ratios <- downloadHandler(
    
    filename = function() { 'Ratios.xlsx' },
    content = function(file) {
      
      withProgress(message = 'Please wait... gethering data...', value = 0,{
        
        setProgress(.50, detail = "Gathering Data...")
        all.data = ratio.summary.df() %>% select(-Actual_Sales_Value)
        
        wb <- createWorkbook()
        
        modifyBaseFont(wb, fontSize = 8, fontColour = "black", fontName = "Arial")
        
        setProgress(.90, detail = "Downloading file...")
        
        addWorksheet(wb, 'Ratio', gridLines = F)
        writeData(wb, 'Ratio', all.data %>%
                    setNames(nm = str_replace_all(names(.), '[.]', ' ')),
                  headerStyle = createStyle(wrapText = T,border = c("top", "bottom", "left", "right"),
                                            fgFill = '#deebf7',
                                            textDecoration= 'bold',
                                            halign = 'center',
                                            valign = 'center') ,colNames = T, borders = 'all')
        
        saveWorkbook(wb,file)
      })
    }
  )
  
  
  # Display Ratios
  
  output$ratio.summary <- renderReactable({
    
    summary.left.stock=ratio.summary.df()%>% select(-Actual_Sales_Value) %>%
      relocate(Order_to_Ship, .before = OS_Ship_Qty) %>%
      relocate(Order_to_Cut, .before = Order_to_Ship) %>%
      relocate(Cut_to_Produce, .before = Order_to_Cut) %>%
      rename(Product_Type=P_L_Category)


    reactable(summary.left.stock %>% select(-Cut_to_Produce) ,
              groupBy=c('Buyer','Product_Type','Plant','Season','Style_Code'),
              searchable = F, highlight = T,
              wrap = F, outlined = T, borderless = F,resizable = TRUE,
              pagination = F, sortable = T,
              theme = reactableTheme(
                color = '#FFFFFB',
                backgroundColor = '#000000',
                searchInputStyle = list(width = "30%"),
                headerStyle = list(background = "#44546A", color = "white", fontWeight = "normal"),
                footerStyle = list(fontWeight = "normal", background = "#FFFFFB"),
                rowSelectedStyle = list(backgroundColor = "#FFFF99", boxShadow = "inset 2px 0 0 0 #41ab5d")
              ), 
              columns = list(
                # Actual_Sales_Value = colDef(name = "Actual_Sales_Value",
                #                 aggregate = "sum",
                #                 format=(colFormat(separators = T,digits=0))),
                OS_Ship_Qty = colDef(name = "Ship Qty",
                                        aggregate = "sum",
                                        format=(colFormat(separators = T,digits=0))),
                OS_Total_Order_Qty = colDef(name = "Total Order Qty",
                                aggregate = "sum",
                                format=(colFormat(separators = T,digits=0))),
                Order_to_Ship = colDef(name="Order to Ship",
                  # Calculate the aggregate Avg.Price as `sum(Export) / sum(Total_Order_Qty)`
                  aggregate = JS("function(values, rows) {
                                  let totalPrice = 0
                                  let totalUnits = 0
                                  rows.forEach(function(row) {
                                    totalPrice += row['OS_Ship_Qty']
                                    totalUnits += row['OS_Total_Order_Qty']
                                  })
                                  return totalPrice / totalUnits *100
                                }"),
                  format = colFormat(suffix = "%",digits=2)
                ),
                Cut_to_Ship = colDef(name="Cut to Ship",
                                     format = colFormat(suffix = "%",digits=2)
                                     ),
                Order_to_Cut=colDef(name="Order to Cut",
                                    format = colFormat(suffix = "%",digits=2)
                ),
                Product_Type=colDef(name="Product Type",
                ),
                Style_Code=colDef(name="Style Code",
                )
                
              )
              # columns = list(
              #   Export = colDef(aggregate = "sum",format = colFormat(separators = TRUE)),
              #   Opportunity_Loss = colDef(
              #     name = 'Opportunity_Loss($)', 
              #     format = colFormat(separators = T, digits = 0)
              #   ),
              #   
              #   Buyer = colDef(
              #     html = TRUE,
              #     cell =  function(value) {
              #       tippy(
              #         div(
              #           value,
              #         ),
              #         tooltip = value
              #       )
              #     },
              #   )
              #   
              # ),
              
    ) 
  })
  
  
  # Display Over Deviation Value in Over Deviation Tab
  
  output$od.over.deviation <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value=filtered.rm.data()
      
      over.deviation.value=paste0('$ ',round(abs(sum(Value$Material$Over_Deviation,na.rm=TRUE)/1000),2),'k')
      
      setBorderColor(valueBox(tags$p(over.deviation.value, style = "font-size: 50%;"), tags$p("Over Deviation", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("Over Deviation", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  # Display Over Deviation Against Sales
  
  output$od.against.sales <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      over.deviation.value =sum(filtered.rm.data()$Material$Over_Deviation,na.rm=TRUE)
      
      sales.value=sum(filtered.sales.data()$Actual_Sales_Value,na.rm=TRUE)
      
      ov.against.sale.value=paste0(round((abs(over.deviation.value)/sales.value)*100,2),'%')
      
      setBorderColor(valueBox(tags$p(ov.against.sale.value, style = "font-size: 50%;"), tags$p("Over Deviation Against Sales", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("Over Deviation Against Sales", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  # Display Adhoc Value
  
  output$adhoc.value <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value =filtered.rm.data()
      
      adhoc.value=paste0('$ ',round(sum(Value$Material$Adhoc,na.rm=TRUE)/1000,2),'k')
      
      setBorderColor(valueBox(tags$p(adhoc.value, style = "font-size: 50%;"), tags$p("Adhoc", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("Adhoc", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  
  # Display Under Produce Value
  
  output$under.produce <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value =filered.rm.data()
      

      under.produce.value=paste0('$ ',round(sum(Value$Under_Produce,na.rm=TRUE)/1000,2),'k')
      
      setBorderColor(valueBox(tags$p(under.produce.value, style = "font-size: 50%;"), tags$p("Under Production", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("Under Production", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  # Display FG Stock Value
  
  output$fg.stock <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value =filtered.ratio.data()
      
      fg.stock.value=paste0('$ ',round(sum(Value$FG_Stock,na.rm=TRUE)/1000,2),'k')
      
      setBorderColor(valueBox(tags$p(fg.stock.value, style = "font-size: 50%;"), tags$p("FG Stock", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("FG Stock", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  # Display Over Issued Value
  
  output$over.issued <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value =cal.over.issued()
      
      over.issued.value=paste0('$ ',round(sum(Value$Over_Issued,na.rm=TRUE)/1000,2),'k')
      
      setBorderColor(valueBox(tags$p(over.issued.value, style = "font-size: 50%;"), tags$p("Over Issued", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("Over Issued", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  
  # Display Other reasons
  
  output$others <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      over.issued.value=sum(cal.over.issued()$Over_Issued,na.rm=TRUE)
      
      fg.stock.value=sum(filtered.ratio.data()$FG_Stock,na.rm=TRUE)

      under.produce.value=sum(filtered.ratio.data()$Under_Produce,na.rm=TRUE)
      
      others.value=fg.stock.value+over.issued.value+under.produce.value
      
      others.value=paste0('$ ',round(others.value/1000,2),'k')
      setBorderColor(valueBox(tags$p(others.value, style = "font-size: 50%;"), tags$p("Others", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("Others", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  # Display Over Deviation Plant Wise
  
  output$over.deviation.chart <- renderApexchart({
    
    req(input$oc.no)
    Over_Deviation_Data=filtered.rm.data() %>% 
      group_by(Buyer) %>%
      summarise(Value=sum(Material$Over_Deviation,na.rm=TRUE)) %>%
      arrange(desc(Value))
   
    Over_Deviation_Data %>%apex(aes(x = Buyer, y = Value),type = 'bar') %>%
      ax_legend(position = 'top', horizontalAlign = 'left',labels=list(foreColor=c('#FFFFFB','#00000'))) %>%
      ax_tooltip(theme='light',y = list(formatter = format_num(',.4s', suffix = ' '))) %>% ax_chart(foreColor='#FFFFFB') %>%
      ax_title(text='Over Deviation',style=list(fontSize=14,color='#FFFFFB')) %>%
      ax_xaxis(labels = list(formatter = format_num(',.2s')), min = NULL,max=NULL, forceNiceScale = T)#%>%
      # ax_yaxis(labels = list(formatter = format_num(',.2s', suffix = '% ')),title='Product Types',
      #          min = NULL,max=NULL, forceNiceScale = T)

  })
  
  
  # Display Over Deviation Trend Chart
  
  output$over.deviation.trend <- renderApexchart({
    
    req(input$oc.no)
    
    filtered.rm.data=filtered.rm.data()
    
    Over_Deviation_Data=filtered.rm.data %>% 
      group_by(Week) %>%
      summarise(Value=sum(Material$Over_Deviation,na.rm=TRUE)) %>% 
      mutate(Category='Over Deviation') 
  
    Over_Deviation_Percentage=filtered.rm.data %>% 
      with_groups(c(Week),
                  summarise,
                  Over_Deviation=sum(Material$Over_Deviation,na.rm=TRUE))%>%
      left_join(filtered.sales.data() %>%with_groups(c(Week),
                                                     summarise,
                                                     Actual_sales=sum(Actual_Sales_Value,na.rm=TRUE),by=c('Week'))) %>%
      replace_na(list(Actual_sales=0))%>%
      mutate(Value=abs(Over_Deviation)/Actual_sales*100,
             Category='Over Deviation Percentage') %>% select(Week,Value,Category)
  
    Over_Deviation_Data %>% bind_rows(Over_Deviation_Percentage) %>%
      subset(Category %in% c("Over Deviation", "Over Deviation Percentage")) %>% 
      transform(Value = round(Value,2)) %>%
      apex(aes(x = Week, y = Value, color = Category), type = "line") %>%
      ax_yaxis(title = list(text = "Value"),labels = list(formatter = format_num(',.2s'))) %>%
      ax_yaxis2(opposite = TRUE, title = list(text = "Percentage"),axisBorder = list(
        show = TRUE,
        color = "#00E396"
      )) %>%
      ax_legend(position = 'top', horizontalAlign = 'left',labels=list(foreColor=c('#FFFFFB','#00000'))) %>%
      ax_title(text='Over Deviation Trend Chart',style=list(fontSize=14,color='#FFFFFB')) %>%
      ax_chart(foreColor='#FFFFFB')  %>% ax_grid(borderColor = '#000000') 
      
      # apex(aes(x = Week, y = Value, group = Category), type = 'line') %>%
      # ax_stroke(curve = 'smooth') %>% ax_chart(foreColor='#FFFFFB')%>%
      # # ax_colors_manual(c('Cut_to_Produce' = '#66bd63', 'Order_to_Cut' = '#a6d96a','Cut_to_Ship'='#fdae61', 'Order_to_Ship'='#d73027') %>% as.list()) %>% #'#92c5de', '#e6f598', '#053061', '#fb6a4a'
      # ax_markers(size = 5)%>%ax_grid(borderColor = '#FFFFFB') %>%
      # ax_colors("#FF0000") %>%
      # ax_title(text='Ratios',style=list(fontSize=14,color='#FFFFFB')) %>%
      # ax_legend(position = 'top', horizontalAlign = 'left',labels=list(foreColor=c('#FFFFFB','#00000'))) %>%
      # ax_tooltip(theme='light',y = list(formatter = format_num(',.4s', suffix = '% '))) %>%
      # ax_yaxis(labels = list(formatter = format_num(',.4s', suffix = '% ')),title='Product Types',
      #          min = NULL,max=NULL, forceNiceScale = T) %>% #e_title(text = "Weekly RMC") %>%
      # suppressWarnings()
    
  })
  
  over.deviation.df<- reactive({

    summary.data =rm.data  %>%filter(Week %in% input$week) 

    summary.over.deviation= summary.data  %>%
      mutate(Item_Code=Material$Item_Code,
             Mat_Size_Code=Material$Mat_Size_Code)%>%
      group_by(Buyer,Plant,P_L_Category,Style_Code,Item_Code,Mat_Size_Code,Material$Item_Code,Material$Mat_Size_Code,OCNo)%>%
       summarise(Over_Deviation=round(sum(Material$Over_Deviation,na.rm=TRUE),2)) %>% 
      select(-matches('Material'))
    
    summary.over.deviation=summary.over.deviation %>%
      mutate(Style_Code=str_sub(Style_Code,1,9))
    
    # open.qty.data=open.qty %>% mutate(Style_Code=str_sub(Style_Code,1,9),
    #                                        Open_Qty=as.double(DOC_Order_Qty)-as.double(Ship_Qty))

    open.qty.data=open.qty()%>%
      filter(Open_Qty>0)%>%
      group_by(Style_Code,Buyer)%>%
      summarise(Open_Qty=sum(Open_Qty)) %>%
      mutate(Style_Code=str_sub(Style_Code,1,9))


    summary.over.deviation=summary.over.deviation %>%
      mutate(Style_Code=str_sub(Style_Code,1,9)) %>%
      left_join(open.qty.data, by=c('Buyer','Style_Code'))%>%
      replace_na(list(Open_Qty=0))%>%
      relocate(Open_Qty, .before = Over_Deviation) %>% arrange(Over_Deviation) %>%
      group_by(Buyer,Plant,P_L_Category,Style_Code,Item_Code,Mat_Size_Code,OCNo) %>%
      summarise(Open_Qty=first(Open_Qty),
                Over_Deviation=round(sum(Over_Deviation,na.rm=TRUE),2))
    
    return(summary.over.deviation)
    
  })
  
  # Download Over Deviation Data
  
  output$download.over.deviation <- downloadHandler(
    
    filename = function() { 'Over Deviation Data.xlsx' },
    content = function(cont) {
      over.deviation.df() %>%
        writexl::write_xlsx(cont) 
    }
  )

  output$over.deviation.summary <- renderReactable({
    
    summary.over.deviation=over.deviation.df() %>% 
      group_by(Buyer,Plant,P_L_Category,Style_Code,Item_Code,Mat_Size_Code) %>%
      summarise(Open_Qty=first(Open_Qty),
                Over_Deviation=round(sum(Over_Deviation,na.rm=TRUE),2))
    
    reactable(summary.over.deviation,groupBy=c('Buyer','Plant','P_L_Category','Style_Code','Item_Code','Mat_Size_Code'),
              searchable = F, highlight = T,
              wrap = F, outlined = T, borderless = F,bordered=TRUE,
              showSortable =FALSE,resizable = T,
              defaultSorted=list(Over_Deviation = "asc"),
              pagination = F, sortable = T,
              width = "auto",
              theme = reactableTheme(
                color = '#FFFFFB',
                backgroundColor = '#000000',
                # searchInputStyle = list(width = "30%"),
                # headerStyle = list(background = "#44546A", color = "white", fontWeight = "normal"),
                # footerStyle = list(fontWeight = "normal", background = "#FFFFFB"),
                # rowSelectedStyle = list(backgroundColor = "#FFFF99", boxShadow = "inset 2px 0 0 0 #41ab5d")
              ),
              columns = list(
                Over_Deviation = colDef(name = "Over Deviation",
                              aggregate = "sum",
                              format=(colFormat(separators = T,digits=0)))
                ),
              
              
    )
  })
  

  # Display Under Deviation Value in Under Deviation Tab
  
  
  
  # Display Under Produce Value

  output$ud.under.deviation <- renderValueBox({
  
    if(isTruthy(input$oc.no)){
  
      Value =cal.over.issued()
  
      over.issued.value=paste0('$ ',round(sum(Value$Over_Issued,na.rm=TRUE)/1000,2),'k')
  
      setBorderColor(valueBox(tags$p(over.issued.value, style = "font-size: 50%;"), tags$p("Over Issued", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
  
    }
    else{
  
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("Over Issued", style = "font-size: 80%;"),color='black'),'#FFFFFB')
  
    }
  
  })
  
  output$ud.under.deviation <- renderValueBox({
  
    if(isTruthy(input$oc.no)){
  
      Value =filtered.rm.data()
  
      under.deviation.value=paste0('$ ',round(sum(Value$Material$Under_Deviation,na.rm=TRUE)/1000,2),'k')
  
      setBorderColor(valueBox(tags$p(under.deviation.value, style = "font-size: 50%;"), tags$p("Under Deviation", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
  
    }
    else{
  
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("Under Deviation", style = "font-size: 80%;"),color='black'),'#FFFFFB')
  
    }
  
  })
  
  # Display Under Deviation Against Sales
  
  output$ud.against.sales <- renderValueBox({
  
    if(isTruthy(input$oc.no)){
  
      under.deviation.value =sum(filtered.rm.data()$Material$Under_Deviation,na.rm=TRUE)
  
      sales.value=sum(filtered.sales.data()$Actual_Sales_Value,na.rm=TRUE)
  
      ud.against.sale.value=paste0(round((abs(under.deviation.value)/sales.value)*100,2),'%')
  
      setBorderColor(valueBox(tags$p(ud.against.sale.value, style = "font-size: 50%;"), tags$p("Under Deviation Against Sales", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
  
    }
    else{
  
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("Under Deviation Against Sales", style = "font-size: 80%;"),color='black'),'#FFFFFB')
  
    }
  
  })
  
  output$ud.under.consumption <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value =filtered.rm.data()
      
      under.deviation.value=paste0('$ ',round(sum(Value$Material$Under_Consumption,na.rm=TRUE)/1000,2),'k')
      
      setBorderColor(valueBox(tags$p(under.deviation.value, style = "font-size: 50%;"), tags$p("Under Consumption", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("Under Consumption", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  output$other.reason <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value =sum(filtered.rm.data()$Material$Under_Deviation,na.rm=TRUE)-sum(filtered.rm.data()$Material$Under_Consumption,na.rm=TRUE)
      
      under.deviation.value=paste0('$ ',round(sum(Value,na.rm=TRUE)/1000,2),'k')
      
      setBorderColor(valueBox(tags$p(under.deviation.value, style = "font-size: 50%;"), tags$p("Other", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("Other", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  # Display Over Deviation Plant Wise
  
  output$under.deviation.chart <- renderApexchart({
    
    req(input$oc.no)
    Under_Deviation_Data=filtered.rm.data() %>% 
      with_groups(c(Buyer),
                  summarise,
                  Value=sum(Material$Under_Deviation,na.rm=TRUE))%>%
      arrange(desc(Value)) 
    
    Under_Deviation_Data %>%apex(aes(x = Buyer, y = Value),type = 'bar') %>%
      ax_legend(position = 'top', horizontalAlign = 'left',labels=list(foreColor=c('#FFFFFB','#00000'))) %>%
      ax_tooltip(theme='light',y = list(formatter = format_num(',.4s', suffix = ' '))) %>% ax_chart(foreColor='#FFFFFB') %>%
      ax_title(text='Under Deviation',style=list(fontSize=14,color='#FFFFFB')) %>%
      ax_xaxis(labels = list(formatter = format_num(',.2s')), min = NULL,max=NULL, forceNiceScale = T)#%>%
      # ax_yaxis(labels = list(formatter = format_num(',.2s')), min = NULL,max=NULL, forceNiceScale = T)
    
  })
  
  
  # Display Over Deviation Trend Chart
  
  output$under.deviation.trend <- renderApexchart({
    
    req(input$oc.no)
    
    Under_Deviation_Data=filtered.rm.data() %>% 
      with_groups(c(Week),
                  summarise,
                  Value=sum(Material$Under_Deviation,na.rm=TRUE)) %>% mutate(Category='Under Deviation') 
    
    
    Under_Deviation_Percentage=filtered.rm.data() %>% 
      with_groups(c(Week),
                  summarise,
                  Under_Deviation=sum(Material$Under_Deviation,na.rm=TRUE))%>%
      left_join(filtered.sales.data() %>%replace_na(list(Actual_sales=0))%>%with_groups(c(Week),
                                                                        summarise,
                                                                        Actual_sales=sum(Actual_Sales_Value,na.rm=TRUE),by=c('Week'))) %>%
      mutate(Value=(abs(Under_Deviation)/Actual_sales)*100,
             Category='Under Deviation Percentage') %>% select(Week,Value,Category)
    
    Under_Deviation_Data %>% bind_rows(Under_Deviation_Percentage) %>%
      subset(Category %in% c("Under Deviation", "Under Deviation Percentage")) %>% 
      transform(Value = round(Value,2)) %>%
      apex(aes(x = Week, y = Value, color = Category), type = "line") %>%
      ax_yaxis(title = list(text = "Value"),labels = list(formatter = format_num(',.2s'))) %>%
      ax_yaxis2(opposite = TRUE, title = list(text = "Percentage"),axisBorder = list(
        show = TRUE,
        color = "#00E396"
      )) %>%
      ax_legend(position = 'top', horizontalAlign = 'left',labels=list(foreColor=c('#FFFFFB','#00000'))) %>%
      ax_title(text='Under Deviation Trend Chart',style=list(fontSize=14,color='#FFFFFB')) %>%
      ax_chart(foreColor='#FFFFFB')  %>% ax_grid(borderColor = '#000000') 
    
    # apex(aes(x = Week, y = Value, group = Category), type = 'line') %>%
    # ax_stroke(curve = 'smooth') %>% ax_chart(foreColor='#FFFFFB')%>%
    # # ax_colors_manual(c('Cut_to_Produce' = '#66bd63', 'Order_to_Cut' = '#a6d96a','Cut_to_Ship'='#fdae61', 'Order_to_Ship'='#d73027') %>% as.list()) %>% #'#92c5de', '#e6f598', '#053061', '#fb6a4a'
    # ax_markers(size = 5)%>%ax_grid(borderColor = '#FFFFFB') %>%
    # ax_colors("#FF0000") %>%
    # ax_title(text='Ratios',style=list(fontSize=14,color='#FFFFFB')) %>%
    # ax_legend(position = 'top', horizontalAlign = 'left',labels=list(foreColor=c('#FFFFFB','#00000'))) %>%
    # ax_tooltip(theme='light',y = list(formatter = format_num(',.4s', suffix = '% '))) %>%
    # ax_yaxis(labels = list(formatter = format_num(',.4s', suffix = '% ')),title='Product Types',
    #          min = NULL,max=NULL, forceNiceScale = T) %>% #e_title(text = "Weekly RMC") %>%
    # suppressWarnings()
    
  })
  
  # Get Under Deviation Summary
  
  under.deviation.df <- reactive({
    
    summary.data =rm.data%>%filter(Week %in% input$week) 
    
    
    summary.under.deviation= summary.data %>%
      mutate(Item_Code=Material$Item_Code,
             Mat_Size_Code=Material$Mat_Size_Code)%>%
      group_by(Buyer,P_L_Category,Season,Plant,Style_Code,OCNo,Material$Item_Code,Material$Mat_Size_Code,Item_Code,Mat_Size_Code)%>%
      summarise(Under_Deviation=round(sum(Material$Under_Deviation,na.rm=TRUE),2)) %>%
      select(-matches('Material'))
    
    summary.under.deviation= summary.under.deviation %>% mutate(Style_Code=str_sub(Style_Code,1,9))
    
   
    # query.open.qty= glue::glue("select Style_Code,Color_Code,DOC_Order_Qty,Size_Code,Ship_Qty from VPO_Update_Open")
    # open.qty.data <<- fetch(dbSendQuery(con,query.open.qty), -1)
    # dbClearResult(dbListResults(con)[[1]])
    
    # open.qty.data=open.qty %>% mutate(Style_Code=str_sub(Style_Code,1,9),
    #                                        Open_Qty=as.double(DOC_Order_Qty)-as.double(Ship_Qty)) 
    open.qty.data=open.qty()%>%filter(Open_Qty>0)%>%  with_groups(c(Style_Code,Buyer),
                                                                     summarise,
                                                                     Open_Qty=sum(Open_Qty)) %>%
      mutate(Style_Code=str_sub(Style_Code,1,9))
   
   
    summary.under.deviation=summary.under.deviation %>% 
      left_join(open.qty.data, by=c('Buyer','Style_Code')) %>%
      arrange(Under_Deviation)%>%replace_na(list(Open_Qty=0))%>%
      relocate(Open_Qty, .before = Under_Deviation) 
    
    
    return(summary.under.deviation)
    
    
    
    
  })
  
  
  
  # Download Under Deviation Data
  
  output$download.under.deviation <- downloadHandler(
    
    filename = function() { 'Under Deviation Data.xlsx' },
    content = function(cont) {
      under.deviation.df() %>%
        writexl::write_xlsx(cont) 
    }
  )
  
  
  
  output$under.deviation.summary <- renderReactable({
    
    summary.under.deviation=under.deviation.df() %>% 
      group_by(Buyer,P_L_Category,Style_Code,Season,Item_Code,Mat_Size_Code) %>%
      summarise(Open_Qty=sum(Open_Qty),
                Under_Deviation=round(sum(Under_Deviation,na.rm=TRUE),2))
  
    reactable(summary.under.deviation,groupBy=c('Buyer','P_L_Category','Style_Code','Season','Item_Code','Mat_Size_Code'),searchable = F, highlight = T,
              wrap = F, outlined = T, borderless = F,bordered=T,width='auto',
              pagination = F, sortable = T,resizable = T,
              defaultSorted=list(Under_Deviation = "desc"),
              theme = reactableTheme(
                color = '#FFFFFB',
                backgroundColor = '#000000',
                searchInputStyle = list(width = "30%"),
                headerStyle = list(background = "#44546A", color = "white", fontWeight = "normal"),
                footerStyle = list(fontWeight = "normal", background = "#FFFFFB"),
                rowSelectedStyle = list(backgroundColor = "#FFFF99", boxShadow = "inset 2px 0 0 0 #41ab5d")
              ), 
              columns = list(
                Under_Deviation = colDef(name = "Under_Deviation",
                                        aggregate = "sum",
                                        format=(colFormat(separators = T,digits=0)))
                    
                
                ),
              
    )
  })
  
  
  
  output$lo.sales.value <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value =filtered.sales.data()
      
      actual.sale.value=paste0('$ ',round(sum(Value$Actual_Sales_Value,na.rm=TRUE)/1000,2),'k')
      
      setBorderColor(valueBox(tags$p(actual.sale.value, style = "font-size: 50%;"), tags$p("Actual Sales", style = "font-size: 80%;border-color: #FF0000;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("Actual Sales", style = "font-size: 80%;"),color='black',icon = icon('dollar')),'#FFFFFB')
      
    }
    
  })
  
  
  output$left.over.stock <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value =filtered.recon.data()
      
      left.over.stock.value=paste0('$ ',round(sum(Value$Current_Stock_Value,na.rm=TRUE)/1000,2),'k')
      
      setBorderColor(valueBox(tags$p(left.over.stock.value, style = "font-size: 50%;"), tags$p("LeftOver Stock", style = "font-size: 80%;border-color: #FF0000;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("LeftOver Stock", style = "font-size: 80%;"),color='black',icon = icon('dollar')),'#FFFFFB')
      
    }
    
  })
  
  
  output$stock.against.sales <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      stock.value =sum(filtered.recon.data()$Current_Stock_Value,na.rm=TRUE)
      
      sales.value=sum(filtered.sales.data()$Actual_Sales_Value,na.rm=TRUE)
      
      stock.against.sale.value=paste0(round((abs(stock.value)/sales.value)*100,2),'%')
      
      setBorderColor(valueBox(tags$p(stock.against.sale.value, style = "font-size: 50%;"), tags$p("Left Over Stock Against Sales", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("Left Over Stock Against Sales", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  left_over_df <-reactive({
    
    query.get.left.over.stock.data= glue::glue("select sum(Rejected_Stock*Unit_Value) as Rejected_Stock,sum(Short_Ship*Unit_Value) as Short_Ship,sum(YY_Saving*Unit_Value) as YY_Saving,
                                               sum(MOQ_MCQ*Unit_Value) as MOQ_MCQ,sum(Supplier_Tolerance*Unit_Value) as Supplier_Tolerance,
                                               sum(Over_Purchase*Unit_Value) as Over_Purchase,sum(Cancelled_RM*Unit_Value) as Cancelled_RM,sum(Item_Color_or_Size_Chnage_Qty*Unit_Value) as Item_Color_or_Size_Chnage_Qty,
                                               sum(Reorder*Unit_Value) as Reorder,sum(Others*Unit_Value) as Others,Week from Recon_Data GROUP BY Week")
    left.over.stock.data = fetch(dbSendQuery(con,query.get.left.over.stock.data), -1)
    dbClearResult(dbListResults(con)[[1]])
    
    
    left.over.stock.data=left.over.stock.data %>% filter(Week %in% input$week) %>%
      summarise(across(where(is.numeric), ~sum(.)))
    

  })
  
  output$left.stock.trend.chart <- renderApexchart({
    
    req(input$oc.no)
    
    Left_Stock_Data=filtered.recon.data() %>% 
      with_groups(c(Week),
                  summarise,
                  Value=sum(Current_Stock_Value,na.rm=TRUE)) %>% mutate(Category='Current Stock Value') 
    
    
    Left_Stock_Percentage=Left_Stock_Data%>%
      left_join(filtered.sales.data() %>%with_groups(c(Week),
                summarise,
                Actual_sales=sum(Actual_Sales_Value,na.rm=TRUE),by=c('Week'))) %>%replace_na(list(Actual_sales=0))%>%
      mutate(Value=abs(Value)/Actual_sales*100,
             Category='Current Stock Percentage') %>% select(Week,Value,Category)
    
    Left_Stock_Data %>% bind_rows(Left_Stock_Percentage) %>%
      subset(Category %in% c("Current Stock Value", "Current Stock Percentage")) %>% 
      transform(Value = round(Value,2)) %>%
      apex(aes(x = Week, y = Value, color = Category), type = "line") %>%
      ax_yaxis(title = list(text = "Value"),labels = list(formatter = format_num(',.2s'))) %>%
      ax_yaxis2(opposite = TRUE, title = list(text = "Percentage"),axisBorder = list(
        show = TRUE,
        color = "#00E396"
      )) %>%
      ax_legend(position = 'top', horizontalAlign = 'left',labels=list(foreColor=c('#FFFFFB','#00000'))) %>%
      ax_title(text='Left Over Stock Trend Chart',style=list(fontSize=14,color='#FFFFFB')) %>%
      ax_chart(foreColor='#FFFFFB')  %>% ax_grid(borderColor = '#000000') #%>%
      # ax_tooltip(theme='light',y = list(formatter = format_num(',.4s', suffix = '% ')))
    
    # apex(aes(x = Week, y = Value, group = Category), type = 'line') %>%
    # ax_stroke(curve = 'smooth') %>% ax_chart(foreColor='#FFFFFB')%>%
    # # ax_colors_manual(c('Cut_to_Produce' = '#66bd63', 'Order_to_Cut' = '#a6d96a','Cut_to_Ship'='#fdae61', 'Order_to_Ship'='#d73027') %>% as.list()) %>% #'#92c5de', '#e6f598', '#053061', '#fb6a4a'
    # ax_markers(size = 5)%>%ax_grid(borderColor = '#FFFFFB') %>%
    # ax_colors("#FF0000") %>%
    # ax_title(text='Ratios',style=list(fontSize=14,color='#FFFFFB')) %>%
    # ax_legend(position = 'top', horizontalAlign = 'left',labels=list(foreColor=c('#FFFFFB','#00000'))) %>%
    # ax_tooltip(theme='light',y = list(formatter = format_num(',.4s', suffix = '% '))) %>%
    # ax_yaxis(labels = list(formatter = format_num(',.4s', suffix = '% ')),title='Product Types',
    #          min = NULL,max=NULL, forceNiceScale = T) %>% #e_title(text = "Weekly RMC") %>%
    # suppressWarnings()
    
  })
  
  
  output$left.stock.reason.chart <- renderApexchart({
    
    
    req(input$oc.no)

    
    Left_Over_Data=left_over_df() %>% pivot_longer(everything(),
                                                   names_to = "Reason",
                                                   values_to = "Value")

    Left_Over_Data %>%
      arrange(desc(Value)) %>%apex(aes(x = Reason, y = Value),type = 'bar') %>%
      ax_legend(position = 'top', horizontalAlign = 'left',labels=list(foreColor=c('#FFFFFB','#00000'))) %>%
      ax_tooltip(theme='light',y = list(formatter = format_num(',.4s', suffix = ' '))) %>% ax_chart(foreColor='#FFFFFB') %>%
      ax_title(text='Left Over Stock',style=list(fontSize=14,color='#FFFFFB')) %>%
      ax_xaxis(labels = list(formatter = format_num(',.2s')), min = NULL,max=NULL, forceNiceScale = T)
    
  })
  
  # Download Over Deviation Data
  
  output$download.left.over.stock <- downloadHandler(
    
    filename = function() { 'Left Over Stock.xlsx' },
    content = function(cont) {
      left.over.stock.df() %>%
        writexl::write_xlsx(cont) 
    }
  )
  
  left.over.stock.df <- reactive({
    
    summary.left.stock= recon.data %>%filter(Week %in% input$week) %>%filter(Export !=0)%>%with_groups(c(Style_Code,Item_Code,Mat_Color_Code,Mat_Size_Code),
                                                                                                       summarise,
                                                                                                       Current_Stock_Value=round(sum(Current_Stock_Value,na.rm=TRUE),2))%>% 
      mutate(Style_Code=str_sub(Style_Code,1,9))
    
  })
  
  output$left.over.stock.summary <- renderReactable({
    
    
    reactable(left.over.stock.df(),groupBy=c('Style_Code','Item_Code','Mat_Color_Code','Mat_Size_Code'),searchable = F, highlight = T,
              wrap = T, outlined = T, borderless = F,
              pagination = F, sortable = T,
              theme = reactableTheme(
                color = '#FFFFFB',
                backgroundColor = '#000000',
                searchInputStyle = list(width = "30%"),
                headerStyle = list(background = "#44546A", color = "white", fontWeight = "normal"),
                footerStyle = list(fontWeight = "normal", background = "#FFFFFB"),
                rowSelectedStyle = list(backgroundColor = "#FFFF99", boxShadow = "inset 2px 0 0 0 #41ab5d")
              ), 
              columns = list(
                Current_Stock_Value = colDef(name = "Current_Stock_Value",
                                         aggregate = "sum",
                                         format=(colFormat(separators = T,digits=0))
                                         )),
              
    )
  })

  
  fi.report.tbl.df <- reactive({
    

    summary.left.stock= recon.data %>%filter(Week %in% input$week) %>%
      filter(Export !=0)%>%mutate(
        Actual_RM_Value=RM_Net_Issued*Unit_Value,
        Costed_RM_Value=if_else(Indent_Type =='Adhoc Indent',0,Required_Qty*Unit_Value),
        Logical_Consumption=if_else(Export<OCQty,(Required_Qty/OCQty)*Export*Unit_Value,Required_Qty*Unit_Value)
      )%>%
      with_groups(c(Buyer,Plant,OCNo,Item_Code,Mat_Color_Code,Mat_Size_Code),
                  summarise,
                  Actual_RM_Value=round(sum(Actual_RM_Value),2),
                  Costed_RM_Value=round(sum(Costed_RM_Value),2),
                  Logical_Consumption=round(sum(Logical_Consumption),2)
                  )

  })
  
  output$fi.report.tbl <- renderReactable({
    
    reactable(fi.report.tbl.df(),
              searchable = F, highlight = T,
              wrap = T, outlined = T, borderless = F,
              pagination = TRUE,
              # showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 25, 50, 100), defaultPageSize = 25,
              sortable = T, resizable = T,
              theme = reactableTheme(
                color = '#FFFFFB',
                backgroundColor = '#000000',
                searchInputStyle = list(width = "30%"),
                headerStyle = list(background = "#44546A", color = "white", fontWeight = "normal"),
                footerStyle = list(fontWeight = "normal", background = "#FFFFFB"),
                rowSelectedStyle = list(backgroundColor = "#FFFF99", boxShadow = "inset 2px 0 0 0 #41ab5d")
              ), 
              # columns = list(
              #   Current_Stock_Value = colDef(name = "Current_Stock_Value",
              #                                aggregate=NULL,
              #                                format=(colFormat(separators = T,digits=0))
              #   ),
              #   
              #   Actual_RM_Value = colDef(name = "Actual_RM_Value",
              #                                format=(colFormat(separators = T,digits=0))
              #   ),
              #   
              #   Costed_RM_Value = colDef(name = "Costed_RM_Value",
              #                                format=(colFormat(separators = T,digits=0))
              #   ),
              #   
                # ),
              
    )
  })
  
  # Download FI Report
  
  output$download.fi.report <- downloadHandler(
    
    filename = function() { 'FI Report.xlsx' },
    content = function(file) {
      
      withProgress(message = 'Please wait... gethering data...', value = 0,{
        
        setProgress(.50, detail = "Gathering Data...")
        all.data = fi.report.tbl.df()
        
        wb <- createWorkbook()
        
        modifyBaseFont(wb, fontSize = 8, fontColour = "black", fontName = "Arial")
        
        setProgress(.90, detail = "Downloading file...")
        
        addWorksheet(wb, 'FI Report', gridLines = F)
        writeData(wb, 'FI Report', all.data %>%
                    setNames(nm = str_replace_all(names(.), '[.]', ' ')),
                  headerStyle = createStyle(wrapText = T,border = c("top", "bottom", "left", "right"),
                                            fgFill = '#deebf7',
                                            textDecoration= 'bold',
                                            halign = 'center',
                                            valign = 'center') ,colNames = T, borders = 'all')
        
        saveWorkbook(wb,file)
      })
    }
  )
  
  filtered.po.data <- reactive({
    
    req(input$oc.no)
    
    po=mongo("PO_Summary",db='Weekly_Reconcilation',url=mongo_url,options = ssl_options(weak_cert_validation = TRUE))
    
    query <- paste('{"OCNo" : {"$in":',toJSON(input$oc.no, auto_unbox=TRUE),'}}', sep='')
    
    po.data=po$find()
    
    
    po.data = po.data %>% filter(OCNo %in% input$oc.no)
    po.data %>%
      mutate(PO=sum(GRN_Value,na.rm =TRUE)/sum(Planned_RMC_Value_wO_Adhoc,na.rm=TRUE),
             GRN=sum(PO_Value,na.rm =TRUE)/sum(Planned_RMC_Value_wO_Adhoc,na.rm=TRUE),
             GRN_TRANS=sum(GRN_Value,na.rm =TRUE)+sum(Transfer_Value,na.rm =TRUE)/sum(Planned_RMC_Value_wO_Adhoc,na.rm=TRUE),
             Issued=sum(Issued_Value,na.rm =TRUE)/sum(Planned_RMC_Value_wO_Adhoc,na.rm =TRUE),
             Adhoc=sum(Issued_Value,na.rm =TRUE)/sum(Planned_RMC_Value_wO_Adhoc,na.rm =TRUE)
      )
    

    
  })
  
  
  
  # Display Over Deviation Value in Over Deviation Tab
  
  output$ov.po.value <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value=filtered.po.data()
      
      po.data.value=paste0(round(sum(abs(Value$GRN_Value),na.rm =TRUE)/sum(Value$Planned_RMC_Value_wO_Adhoc,na.rm=TRUE)*100,2),'%')
      
      setBorderColor(valueBox(tags$p(po.data.value, style = "font-size: 50%;"), tags$p("PO Coverage", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("PO Coverage", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  output$ov.grn.value <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value=filtered.po.data()
      
      grn.data.value=paste0(round((sum(Value$PO_Value,na.rm =TRUE)/sum(Value$Planned_RMC_Value_wO_Adhoc,na.rm=TRUE))*100,2),'%')
      
      setBorderColor(valueBox(tags$p(grn.data.value, style = "font-size: 50%;"), tags$p("GRN Coverage", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("GRN Coverage", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  output$ov.tran.grn.value <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value=filtered.po.data()
      
      grn.trans.data=paste0(round(((sum(Value$GRN_Value,na.rm =TRUE)+sum(Value$Transfer_Value,na.rm =TRUE))/sum(Value$Planned_RMC_Value_wO_Adhoc,na.rm=TRUE))*100,2),'%')
      
      setBorderColor(valueBox(tags$p(grn.trans.data, style = "font-size: 50%;"), tags$p("GRN+Trans Coverage", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"),NULL,color='black'),'#FFFFFB')
      
    }
    
  })
  
  output$ov.issue.value <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value=filtered.po.data()
      
      issue.data=paste0(round((sum(Value$Issued_Value,na.rm =TRUE)/sum(Value$Planned_RMC_Value_wO_Adhoc,na.rm =TRUE))*100,2),'%')
      
      setBorderColor(valueBox(tags$p(issue.data, style = "font-size: 50%;"), tags$p('Issue', style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p('Issue', style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  output$ov.po.adhoc.value <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value=filtered.po.data()
      
      adhoc.data=paste0(round((sum(Value$Adhoc_Value,na.rm =TRUE)/sum(Value$Planned_RMC_Value_wO_Adhoc,na.rm =TRUE))*100,2),'%')
      
      setBorderColor(valueBox(tags$p(adhoc.data, style = "font-size: 50%;"), tags$p("Adhoc", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("Adhoc", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  output$fb.po.value <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value=filtered.po.data()%>% filter(Category=='Textiles/Fabric')
      
      po.data.value=paste0(round(sum(abs(Value$GRN_Value),na.rm =TRUE)/sum(Value$Planned_RMC_Value_wO_Adhoc,na.rm=TRUE)*100,2),'%')
      
      setBorderColor(valueBox(tags$p(po.data.value, style = "font-size: 50%;"), tags$p("PO Coverage", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("PO Coverage", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  output$fb.grn.value <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value=filtered.po.data()%>% filter(Category=='Textiles/Fabric')
      
      grn.data.value=paste0(round((sum(Value$PO_Value,na.rm =TRUE)/sum(Value$Planned_RMC_Value_wO_Adhoc,na.rm=TRUE))*100,2),'%')
      
      setBorderColor(valueBox(tags$p(grn.data.value, style = "font-size: 50%;"), tags$p("GRN Coverage", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("GRN Coverage", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  output$fb.tran.grn.value <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value=filtered.po.data()%>% filter(Category=='Textiles/Fabric')
      
      grn.trans.data=paste0(round(((sum(Value$GRN_Value,na.rm =TRUE)+sum(Value$Transfer_Value,na.rm =TRUE))/sum(Value$Planned_RMC_Value_wO_Adhoc,na.rm=TRUE))*100,2),'%')
      
      setBorderColor(valueBox(tags$p(grn.trans.data, style = "font-size: 50%;"), tags$p("GRN+Trans Coverage", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("GRN+Trans Coverage", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  output$fb.issue.value <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value=filtered.po.data()%>% filter(Category=='Textiles/Fabric')
      
      issue.data=paste0(round((sum(Value$Issued_Value,na.rm =TRUE)/sum(Value$Planned_RMC_Value_wO_Adhoc,na.rm =TRUE))*100,2),'%')
      
      setBorderColor(valueBox(tags$p(issue.data, style = "font-size: 50%;"), tags$p("Issue", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("Issue", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  output$fb.po.adhoc.value <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value=filtered.po.data()%>% filter(Category=='Textiles/Fabric')
      
      adhoc.data=paste0(round((sum(Value$Adhoc_Value,na.rm =TRUE)/sum(Value$Planned_RMC_Value_wO_Adhoc,na.rm =TRUE))*100,2),'%')
      
      setBorderColor(valueBox(tags$p(adhoc.data, style = "font-size: 50%;"), tags$p("Adhoc", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("Adhoc", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  output$tr.po.value <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value=filtered.po.data()%>% filter(Category=='Trims')

      po.data.value=paste0(round(sum(abs(Value$GRN_Value),na.rm =TRUE)/sum(Value$Planned_RMC_Value_wO_Adhoc,na.rm=TRUE)*100,2),'%')
      
      setBorderColor(valueBox(tags$p(po.data.value, style = "font-size: 50%;"), tags$p("PO Coverage", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("PO Coverage", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  output$tr.grn.value <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value=filtered.po.data()%>% filter(Category=='Trims')
      
      grn.data.value=paste0(round((sum(Value$PO_Value,na.rm =TRUE)/sum(Value$Planned_RMC_Value_wO_Adhoc,na.rm=TRUE))*100,2),'%')
      
      setBorderColor(valueBox(tags$p(grn.data.value, style = "font-size: 50%;"), tags$p("GRN Coverage", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("GRN Coverage", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  output$tr.tran.grn.value <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value=filtered.po.data() %>% filter(Category=='Trims')
      
      grn.trans.data=paste0(round(((sum(Value$GRN_Value,na.rm =TRUE)+sum(Value$Transfer_Value,na.rm =TRUE))/sum(Value$Planned_RMC_Value_wO_Adhoc,na.rm=TRUE))*100,2),'%')
      
      setBorderColor(valueBox(tags$p(grn.trans.data, style = "font-size: 50%;"), tags$p("GRN+Trans Coverage", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("GRN+Trans Coverage", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  output$tr.issue.value <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value=filtered.po.data()%>% filter(Category=='Trims')
      
      issue.data=paste0(round((sum(Value$Issued_Value,na.rm =TRUE)/sum(Value$Planned_RMC_Value_wO_Adhoc,na.rm =TRUE))*100,2),'%')
      
      setBorderColor(valueBox(tags$p(issue.data, style = "font-size: 50%;"), tags$p("Issue", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("Issue", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  
  output$tr.po.adhoc.value <- renderValueBox({
    
    if(isTruthy(input$oc.no)){
      
      Value=filtered.po.data()%>% filter(Category=='Trims')
      
      adhoc.data=paste0(round((sum(Value$Adhoc_Value,na.rm =TRUE)/sum(Value$Planned_RMC_Value_wO_Adhoc,na.rm =TRUE))*100,2),'%')
      
      setBorderColor(valueBox(tags$p(adhoc.data, style = "font-size: 50%;"), tags$p("Adhoc", style = "font-size: 80%;"),color='black',icon = icon("dollar")),'#FFFFFB')
      
    }
    else{
      
      setBorderColor(valueBox(tags$p('0%', style = "font-size: 50%;"), tags$p("Adhoc", style = "font-size: 80%;"),color='black'),'#FFFFFB')
      
    }
    
  })
  

  po.summary.df <-reactive({
    
    req(input$oc.no)
    
    other.ratio.data=mongo("Other_Ratio_Summary",db='Weekly_Reconcilation',url=mongo_url,options = ssl_options(weak_cert_validation = TRUE))
    # query <- paste('{"OCNo" : {"$in":',toJSON(input$oc.no, auto_unbox=TRUE),'}}', sep='')
    additional.ratio.data=other.ratio.data$find()
    

    additional.ratio =additional.ratio.data %>%
      # filter(OCNo %in% input$oc.no )%>%
      filter(!grepl('SUB|CAMBODIA',Plant,ignore.case = T))
    
    additional.ratio=additional.ratio %>%
      mutate(across(matches('Qty'), ~replace(., is.na(.), "0")),
             across(matches('Qty'), as.numeric)) %>%
      summarise(across(where(is.numeric), sum))
  
    
    ratio.data.cal =additional.ratio %>%
      mutate( Cut_Order=additional.ratio$Cutting_Received_Qty /additional.ratio$Total_Order_Qty,
              SewIn_Order=additional.ratio$Sewing_Received_Qty /additional.ratio$Total_Order_Qty,
              SewOut_Order=additional.ratio$Sewing_SFG_Qty  /additional.ratio$Total_Order_Qty,
              FG_Order=additional.ratio$FG_Received_Qty /additional.ratio$Total_Order_Qty,
              Delivery_Order=additional.ratio$Ship_Qty /additional.ratio$Total_Order_Qty,
              
              SewIn_Cut=additional.ratio$Sewing_Received_Qty /additional.ratio$Cutting_Received_Qty,
              SewOut_Cut=additional.ratio$Sewing_SFG_Qty  /additional.ratio$Cutting_Received_Qty,
              FG_Cut=additional.ratio$FG_Received_Qty /additional.ratio$Cutting_Received_Qty,
              Delivery_Cut=additional.ratio$Ship_Qty /additional.ratio$Cutting_Received_Qty,
              
              SewOut_SewIn=additional.ratio$Sewing_SFG_Qty  /additional.ratio$Sewing_Received_Qty,
              FG_SewIn=additional.ratio$FG_Received_Qty /additional.ratio$Sewing_Received_Qty,
              Delivery_SewIn=additional.ratio$Ship_Qty /additional.ratio$Sewing_Received_Qty,
              
              FG_SewOut=additional.ratio$FG_Received_Qty /additional.ratio$Sewing_SFG_Qty,
              Delivery_SewOut=additional.ratio$Ship_Qty /additional.ratio$Sewing_SFG_Qty,
              
              Delivery_FG=additional.ratio$Ship_Qty /additional.ratio$FG_Received_Qty) %>%
      select(-matches('Qty'))
    
    ratio.data.cal.arrange =ratio.data.cal %>% 
      pivot_longer(cols=everything()) %>%
      separate(name, c("Numerator", "Denominator"), "_") %>%
      pivot_wider(names_from=Numerator,values_from = value) %>% 
      rename(Ratio=Denominator)
   
    
    
    
  })
  
  output$po.summary <- renderReactable({
    
    req(input$oc.no)
    
    reactable(po.summary.df(),searchable = F, highlight = T,
              wrap = F, outlined = T, borderless = F,bordered=T,width='auto',
              pagination = F, sortable = T,resizable = T,
              defaultColDef=colDef(format = colFormat(percent = T, digits = 2)),
              theme = reactableTheme(
                color = '#FFFFFB',
                backgroundColor = '#000000',
                searchInputStyle = list(width = "30%"),
                headerStyle = list(background = "#44546A", color = "white", fontWeight = "normal"),
                footerStyle = list(fontWeight = "normal", background = "#FFFFFB"),
                rowSelectedStyle = list(backgroundColor = "#FFFF99", boxShadow = "inset 2px 0 0 0 #41ab5d")
              )
    )
    
  })
  
  output$over.consumption.scatter <- renderApexchart({
    
    req(input$oc.no)
        
    x= gsub(" ", "_", input$consumption.category)
    Under_Deviation_Data=c()
    
    if(input$consumption.category=='Over Deviation'){
      
      Under_Deviation_Data=rm.data$Material %>% 
        with_groups(c(Item_Code),
                    summarise,
                    Value=round((sum(abs(Over_Deviation),na.rm=TRUE)/sum(Costed_RM_Value,na.rm=TRUE))*100),2)%>%
        filter(Value>0)
      
    }
    else if(input$consumption.category=='Over Deviation'){
      Under_Deviation_Data=rm.data$Material %>% 
        with_groups(c(Item_Code),
                    summarise,
                    Value=round((sum(Under_Deviation,na.rm=TRUE)/sum(Costed_RM_Value,na.rm=TRUE))*100),2)%>%
        filter(Value>0)
    }else{
      
      Under_Deviation_Data=rm.data$Material %>% 
        with_groups(c(Item_Code),
                    summarise,
                    Value=round((sum(Under_Deviation+abs(Over_Deviation),na.rm=TRUE)/sum(Costed_RM_Value,na.rm=TRUE))*100),2)%>%
        filter(Value>0)
      
    }
    
    
    
    Under_Deviation_Data %>% apex(aes(x = Item_Code, y = Value),type = 'scatter') %>%
      ax_yaxis(labels = list(formatter = format_num(',.4s', suffix = '% ')),title=list(text='Percentage'),
                        min = NULL,max=NULL, forceNiceScale = TRUE,axisBorder=list(show=TRUE)) %>%
      ax_grid(show=FALSE) %>%
      ax_xaxis(axisTicks=list(show=FALSE),axisBorder=list(show=TRUE),labels = list(show=FALSE),title = list(text="Item_Code"),forceNiceScale = TRUE)
    
      # grid()
      # ax_plotOptions(
      #   plotOptions = list(
      #     scatter = list(
      #       size = 2 ,
      #       color='#2D0320'# Set the point size here
      #     )
      #   )
      # ) 
      # ax_xaxis(labels=list(trim=F,maxHeight=20),forceNiceScale = )
      # ax_legend(position = 'top', horizontalAlign = 'left',labels=list(foreColor=c('#FFFFFB','#00000'))) %>%
      # ax_tooltip(theme='light',y = list(formatter = format_num(',.4s', suffix = ' '))) %>% ax_chart(foreColor='#FFFFFB') %>%
      # ax_title(text='Under Deviation',style=list(fontSize=14,color='#FFFFFB')) %>%
      # ax_xaxis(labels = list(formatter = format_num(',.2s')), min = NULL,max=NULL, forceNiceScale = T)
    
  })
  
  output$over.consumption.treemap <- renderApexchart({
    
    req(input$oc.no)
    
    Under_Deviation_Data=c()
    
    if(input$consumption.category=='Over Deviation'){
      
      Under_Deviation_Data=rm.data$Material %>% 
        with_groups(c(Item_Code),
                    summarise,
                    Value=round(sum(abs(Over_Deviation),na.rm=TRUE),2)) %>%
        filter(Value!=0)%>% arrange(desc(Value)) %>% head(100)
      
    }
    else if(input$consumption.category=='Under Deviation'){
      Under_Deviation_Data=rm.data$Material %>% 
        with_groups(c(Item_Code),
                    summarise,
                    Value=round(sum(Under_Deviation,na.rm=TRUE),2)) %>%
        filter(Value!=0)%>% arrange(desc(Value)) %>% head(100)
    }
    else{
      Under_Deviation_Data=rm.data$Material %>% 
        with_groups(c(Item_Code),
                    summarise,
                    Value=round(sum(Under_Deviation+abs(Over_Deviation),na.rm=TRUE),2)) %>%
        filter(Value!=0)%>% arrange(desc(Value)) %>% head(100)
    }

    Under_Deviation_Data %>%
      apex(aes(x = Item_Code, y = Value,fill=Item_Code),type = 'treemap',options = list(colors = custom_palette)) #%>%
      # ax_colors()
    # ax_legend(position = 'top', horizontalAlign = 'left',labels=list(foreColor=c('#FFFFFB','#00000'))) %>%
    # ax_tooltip(theme='light',y = list(formatter = format_num(',.4s', suffix = ' '))) %>% ax_chart(foreColor='#FFFFFB') %>%
    # ax_title(text='Under Deviation',style=list(fontSize=14,color='#FFFFFB')) %>%
    # ax_xaxis(labels = list(formatter = format_num(',.2s')), min = NULL,max=NULL, forceNiceScale = T)
    
  })
  
  over.consumption.heatmap.df <-reactive({
    
    Under_Deviation_Data=c()
    
    
    filtered.rm.data=filtered.rm.data() %>% 
      mutate(Item_Code=Material$Item_Code,
             Mat_Size_Code=Material$Mat_Size_Code) 
    
    
    
    if(input$consumption.category=='Over Deviation'){
      
      Under_Deviation_Data=filtered.rm.data %>%
        with_groups(c(Item_Code,Week),
                    summarise,
                    Value=round((sum(abs(Material$Over_Deviation),na.rm=TRUE)/sum(Material$Costed_RM_Value,na.rm=TRUE)),2))%>%
        filter(Value!=0)
      
    }
    else if(input$consumption.category=='Under Deviation'){
      Under_Deviation_Data=filtered.rm.data %>% 
        with_groups(c(Item_Code,Week),
                    summarise,
                    Value=round((sum(Material$Under_Deviation,na.rm=TRUE)/sum(Material$Costed_RM_Value,na.rm=TRUE))*100,2)) %>%
        filter(Value!=0) 
    }else{
      
      Under_Deviation_Data=filtered.rm.data %>% 
        with_groups(c(Item_Code,Week),
                    summarise,
                    Value=round((sum(Material$Under_Deviation+abs(Material$Over_Deviation),na.rm=TRUE)/sum(Material$Costed_RM_Value,na.rm=TRUE))*100,2)) %>%
        filter(Value!=0) 
    }
    
    Under_Deviation_Data.Final=Under_Deviation_Data %>% arrange(Week) %>%
      pivot_wider(names_from = 'Week',values_from ='Value') %>%
      mutate(across(everything(), ~replace(., is.na(.), "0")))

     return(Under_Deviation_Data.Final)
    
    
  })
  
  
  output$over.consumption.heatmap <- renderReactable({
    
    df=over.consumption.heatmap.df()
    
    # cols= list(

    cols <- list();
    
    index = 1
    for(i in colnames(df)){
      
      if(str_detect(i, "20")){
        x = list(colDef(
          align = "center",
          format = colFormat(percent = T, digits = 2,suffix='%'),
          style = function(value) {
            value=as.numeric(value)
            if (value > 0 & value<=3) {
              color = '#A393BF'
            }else if (value>3 & value<=5) {
              color = '#AAF683'
            }else if (value>5 & value<=10) {
              color = '#F5E2C8'
            }else if (value>10 & value<= 20) {
              color = '#FDCA40'
            } else if(value>20 & value<100){
              color = '#088A08'
            }else if(value>=100){
              color = '#FF0000'
            }else{
              color='black'
            }
            list(background = color, color='black',border = "0.25px solid black")
          }
          
          ))
      }
      else{
        x = list(colDef(
          sticky = 'left',
          filterable=TRUE,
          align = "center",
          style = function(value) {
            color <- "#A0AAB2"
            list(background = color, color="black",border = "0.25px solid black")
          }))
        
      }
      
      
      cols[index] = (x)
      index = index + 1
    }
    
    cols <- setNames(cols, names(df))
    
    reactable(over.consumption.heatmap.df(), searchable = F, highlight = T,
              filterable = F, wrap = T, outlined = T,
              showPageSizeOptions = FALSE,
              pageSizeOptions = c(10, 25, 50, 100), defaultPageSize = 50,
              sortable = T, resizable = T,
              bordered = TRUE, striped = TRUE,
              rowStyle = list(cursor = "pointer"),compact=T,
              theme = reactableTheme(
                color = 'white',
                backgroundColor = 'black',
                borderColor = '#FFFFFB',
                headerStyle = list(background = "#00052F", color = "white", fontWeight = "normal",border = "0.25px white"),
                filterInputStyle=list(background = "white", color = "black", fontWeight = "normal",border = "0.25px black"),
              ),
              style = list(fontSize = "10.5px"),
              columns=cols,
              ) 

    
    
  })
  
  # output$over.consumption.heatmap <- renderApexchart({
  #   
  #   req(input$oc.no)
  #   
  #   Under_Deviation_Data=c()
  #   
  # 
  #   filtered.rm.data=filtered.rm.data() %>% 
  #     mutate(Item_Code=Material$Item_Code,
  #            Mat_Size_Code=Material$Mat_Size_Code) 
  # 
  # 
  # 
  #   if(input$consumption.category=='Over Deviation'){
  #     
  #     Under_Deviation_Data=filtered.rm.data %>%
  #       with_groups(c(Item_Code,Week),
  #                   summarise,
  #                   Value=round((sum(abs(Material$Over_Deviation),na.rm=TRUE)/sum(Material$Costed_RM_Value,na.rm=TRUE))*100,2))%>%
  #       filter(Value!=0) %>% arrange(desc(Value)) 
  #     
  #     
  #     x=Under_Deviation_Data %>% head(10)  %>% select(Item_Code)
  #     
  #     Under_Deviation_Data=Under_Deviation_Data %>% filter(Item_Code %in% x$Item_Code)
  #     
  #     
  #   }
  #   else{
  #     Under_Deviation_Data=filtered.rm.data %>% 
  #       with_groups(c(Item_Code,Week),
  #                   summarise,
  #                   Value=round((sum(Material$Under_Deviation,na.rm=TRUE)/sum(Material$Costed_RM_Value,na.rm=TRUE))*100,2)) %>%
  #       filter(Value!=0) %>% head(20)
  #   }
  #   
  #   print(head(Under_Deviation_Data))
  #   
  #   Under_Deviation_Data %>% group_by(Item_Code) %>%
  #     apex(aes(x = Week, y = Item_Code,fill=Value),type = 'heatmap',options = list(colors = custom_palette)) %>%
  #     ax_plotOptions(
  #       heatmap = heatmap_opts(
  #         enableShades = TRUE,
  #         colorScale = list(
  #           ranges = list(
  #             list(from = 100, to = 2000, color = "#FF0000"),
  #             list(from = 80, to = 100, color = "#D11936"),
  #             list(from = 60, to = 80, color = "#EE3E38"),
  #             list(from = 40, to = 60, color = "#F86E51"),
  #             list(from = 20, to = 40, color = "#FBA465"),
  #             list(from = 0, to = 20, color = "#F2C85B")
  #             
  #           )
  #         )
  #       )
  #     ) %>%
  #   # ax_colors()
  #   # ax_legend(position = 'top', horizontalAlign = 'left',labels=list(foreColor=c('#FFFFFB','#00000'))) %>%
  #   # ax_tooltip(theme='light',y = list(formatter = format_num(',.4s', suffix = ' '))) %>% ax_chart(foreColor='#FFFFFB') %>%
  #   # ax_title(text='Under Deviation',style=list(fontSize=14,color='#FFFFFB')) %>%
  #   ax_xaxis(labels = list(formatter = format_num(',.2s')), min = NULL,max=NULL, forceNiceScale = T) %>%
  #     ax_grid(show=TRUE)
  #   
  # })
  
  
  saving.item.wise.tbl.df <-reactive({
    
    saving.data =filtered.rm.data()$Material %>% 
      with_groups(c(Item_Code),
                  summarise,
                  Actual_RMC=sum(Actual_RM_Value,na.rm=TRUE),
                  Costed_RMC=sum(Costed_RM_Value,na.rm=TRUE),
                  Under_Deviation=sum(Under_Deviation,na.rm=TRUE),
                  Over_Deviation=abs(sum(Over_Deviation,na.rm=TRUE)),
                  Under_Deviation_Percentage=sum(Under_Deviation,na.rm=TRUE)/sum(Costed_RM_Value,na.rm=TRUE),
                  Over_Deviation_Percentage=abs(sum(Over_Deviation,na.rm=TRUE))/sum(Costed_RM_Value,na.rm=TRUE)
                  )  %>%
      mutate(Total_Deviation=Under_Deviation + Over_Deviation)
  
    
    if(input$deviation.category=='Under Deviation'){
      
      saving.data=saving.data %>% arrange(desc(Under_Deviation)) %>% head(5)
      
    }
    else if(input$deviation.category=='Over Deviation'){
      saving.data=saving.data %>% 
        arrange(desc(Over_Deviation)) %>% 
        head(5) %>% 
        relocate(Over_Deviation, .before = Under_Deviation)
      
    }
    else{
      
      saving.data=saving.data %>% 
        arrange(desc(Total_Deviation)) %>% 
        head(5) %>% 
        relocate(Over_Deviation, .before = Under_Deviation)
    }
    
  })
  
  
  output$saving.item.wise.tbl <- renderReactable({
    
    req(input$oc.no)
    
    reactable(saving.item.wise.tbl.df(),searchable = F, highlight = T,
              wrap = F, outlined = T, borderless = F,bordered=T,width='auto',
              pagination = F, sortable = T,resizable = T,
              selection="single",
              defaultSelected=c(1),
              defaultColDef=colDef(format = colFormat(digits = 0,separators=TRUE)),
              columns=list(
                Under_Deviation_Percentage=colDef(
                  name="Under_Deviation_Percentage",
                  format = colFormat(percent = T, digits = 2)),
                Over_Deviation_Percentage=colDef(
                  name="Over_Deviation_Percentage",
                  format = colFormat(percent = T, digits = 2))
                
              ),
              theme = reactableTheme(
                color = '#FFFFFB',
                backgroundColor = '#000000',
                searchInputStyle = list(width = "30%"),
                headerStyle = list(background = "#44546A", color = "white", fontWeight = "normal"),
                footerStyle = list(fontWeight = "normal", background = "#FFFFFB"),
                rowSelectedStyle = list(backgroundColor = "#18206F", boxShadow = "inset 2px 0 0 0 #41ab5d")
              )
    )
    
  })
  
  
  saving.oc.wise.tbl.df <-reactive({
    
    req(saving.item.wise.tbl.df()[getReactableState('saving.item.wise.tbl', "selected", session),] )
    
    Item.Code=saving.item.wise.tbl.df()[getReactableState('saving.item.wise.tbl', "selected", session),] 
    
    saving.data =filtered.rm.data() %>% 
      filter(Material$Item_Code %in% Item.Code$Item_Code) %>%
      with_groups(c(input$group.category),
                  summarise,
                  Actual_RMC=sum(Material$Actual_RM_Value,na.rm=TRUE),
                  Costed_RMC=sum(Material$Costed_RM_Value,na.rm=TRUE),
                  Under_Deviation=sum(Material$Under_Deviation,na.rm=TRUE),
                  Over_Deviation=abs(sum(Material$Over_Deviation,na.rm=TRUE)),
                  Under_Deviation_Percentage=sum(Material$Under_Deviation,na.rm=TRUE)/sum(Material$Costed_RM_Value,na.rm=TRUE),
                  Over_Deviation_Percentage=abs(sum(Material$Over_Deviation,na.rm=TRUE))/sum(Material$Costed_RM_Value,na.rm=TRUE)
      )
    
    if(input$deviation.category=='Under Deviation'){
      
      saving.data=saving.data %>% arrange(desc(Under_Deviation)) %>% head(5)
      
    }
    else{
      saving.data=saving.data %>% 
        arrange(desc(Over_Deviation)) %>% 
        head(5) %>% 
        relocate(Over_Deviation, .before = Under_Deviation)
      
    }
    
  })
  
  
  output$saving.oc.wise.tbl <- renderReactable({
    
    req(input$oc.no)
    
    reactable(saving.oc.wise.tbl.df(),searchable = F, highlight = T,
              wrap = F, outlined = T, borderless = F,bordered=T,width='auto',
              pagination = F, sortable = T,resizable = T,
              defaultColDef=colDef(format = colFormat(digits = 0,separators=TRUE)),
              columns=list(
                Under_Deviation_Percentage=colDef(
                  name="Under_Deviation_Percentage",
                  format = colFormat(percent = T, digits = 2)),
                Over_Deviation_Percentage=colDef(
                  name="Over Deviation Percentage",
                  format = colFormat(percent = T, digits = 2))
                
              ),
              theme = reactableTheme(
                color = '#FFFFFB',
                backgroundColor = '#000000',
                searchInputStyle = list(width = "30%"),
                headerStyle = list(background = "#44546A", color = "white", fontWeight = "normal"),
                footerStyle = list(fontWeight = "normal", background = "#FFFFFB"),
                rowSelectedStyle = list(backgroundColor = "#18206F", boxShadow = "inset 2px 0 0 0 #41ab5d")
              )
    )
    
  })
}