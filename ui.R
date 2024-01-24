ui <- dashboardPage(
  title="Material Management Dashboard",
  
  dashboardHeader(title=span("Material Management",style = "color: white; font-size: 20px"),
                  tags$li(class = "dropdown",
                          tags$img(height = '50', alt="SNAP Logo", src="Capture.JPG"))),#)),
  
  dashboardSidebar(sidebarMenu(id = "sidebarid",
                               menuItem(tabName = "dashboard", text = "Selection Bar"),
                               conditionalPanel(
                                 'input.sidebarid == "dashboard"',
                                 # fluidRow(tags$h4("Week Range",style="font-weight: bold"),
                                 # column(10,
                                 # sliderInput('actual.month', label = NULL,
                                 #             min = Sys.Date()- days(30),
                                 #             max = Sys.Date()+ days(30),
                                 #             value = Sys.Date(),
                                 #             timeFormat = '%b-%y',
                                 #             width = '100%'
                                 # # )
                                 # ),
                                 
                                 pickerInput(inputId = "week",
                                             choices = "",
                                             label = "Week",
                                             width='170px',
                                             options = pickerOptions(actionsBox = T,liveSearch = T),
                                             multiple = T,
                                             inline = TRUE
                                             
                                 ),
                                 br(),
                                 
                                 pickerInput(inputId = "customer",
                                             choices = "",
                                             label = "Customer",
                                             width='170px',
                                             options = pickerOptions(actionsBox = T,liveSearch = T),
                                             multiple = T,
                                             inline = TRUE
                                             
                                 ),
                                 br(),
                                 pickerInput(inputId = "season",
                                             choices = "",
                                             label = "Season",
                                             width='170px',
                                             options = pickerOptions(actionsBox = T,liveSearch = T),
                                             multiple = T,
                                             inline = TRUE
                                             
                                 ),
                                 br(),
                                 pickerInput(inputId = "plant",
                                             choices = "",
                                             label = "Plant",
                                             width='170px',
                                             options = pickerOptions(actionsBox = T,liveSearch = T),
                                             multiple = T,
                                             inline = TRUE
                                             
                                 ),
                                 # br(),
                                 # pickerInput(inputId = "category",
                                 #             choices = "",
                                 #             label = "Category",
                                 #             width='170px',
                                 #             selected = "",
                                 #             options = pickerOptions(actionsBox = T),
                                 #             multiple = T,
                                 #             inline = TRUE
                                 #             
                                 # ),
                                 
                                 br(),
                                 # pickerInput(inputId = "sub.category",
                                 #             choices = "",
                                 #             label = "Sub Category",
                                 #             width='170px',
                                 #             selected = "",
                                 #             options = pickerOptions(actionsBox = T,liveSearch = T),
                                 #             multiple = T,
                                 #             inline = TRUE
                                 #             
                                 # ),
                                 # br(),
                                 # pickerInput(inputId = "color",
                                 #             choices = "",
                                 #             label = "Color",
                                 #             width='170px',
                                 #             selected = "",
                                 #             options = pickerOptions(actionsBox = T),
                                 #             multiple = T,
                                 #             inline = TRUE
                                 #             
                                 # ),
                                 # br(),
                                 pickerInput(inputId = "product.category",
                                             choices = "",
                                             label = "Product Category",
                                             width='170px',
                                             options = pickerOptions(actionsBox = T,liveSearch = T),
                                             multiple = T,
                                             inline = TRUE
                                             
                                 ),
                                 br(),
                                 pickerInput(inputId = "style",
                                             choices = "",
                                             label = "Style",
                                             width='170px',
                                             options = pickerOptions(actionsBox = T,liveSearch = T),
                                             multiple = T,
                                             inline = TRUE
                                             
                                 ),
                                 br(),
                                 pickerInput(inputId = "oc.type",
                                             choices = "",
                                             label = "OC Type",
                                             selected = "",
                                             options = pickerOptions(actionsBox = T,liveSearch = T),
                                             width='170px',
                                             multiple = T,
                                             inline = TRUE

                                 ),
                                 br(),
                                 pickerInput(inputId = "oc.no",
                                             choices = "",
                                             label = "OC No",
                                             width='170px',
                                             options = pickerOptions(actionsBox = T,liveSearch = T),
                                             multiple = T,
                                             inline = TRUE
                                             
                                 ),
                                 
                                 
                                 icon = NULL,
                                 startExpanded = TRUE
                               )
  )),
  
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "dashboard",
              tags$head(tags$style(HTML('body {font-family: Arial;color:white}
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #FFFFFB;
                                color :#000000;
                                }
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #000000;
                                }
                                
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #000000;
                                }
                                
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #000000;
                                }
                                
                                /* navbar (rest of the header) */
                               .skin-blue .main-header .navbar {
                               background-color: #000000;
                               }
                               
                               /* other links in the sidebarmenu when hovered */
                               .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                               background-color: #FFFFFB;
                               color :#000000;
                               }
                               
                               /* toggle button when hovered  */
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #000000;
                              }
                              
                              
                              /* body */
                              .content-wrapper, .right-side {
                              background-color: #000000;
                              } 
                              
                              
                             /* active selected tab in the sidebarmenu */
                             .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                             background-color: #000000;
                             color: #FFFFFB;
                             }
                             
                             # .tabs-above > .nav > li[class=active] > a {
                             # background-color: #000;
                             # color: #FFF;
                             # }

                             
                             .nav-tabs {
                             background-color: #000;
                             color: #FFF;
                             }
                             .{
                             background-color:#000;
                             }
                            
                            .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
                            background-color:#FFFFFB;
                            border-color: #FFFFFB;
                            }
                            .custom-value-box {
                            height: 100px; /* Adjust the height as needed */
                            }
                            .nav-tabs-custom .nav-tabs li.active {
                                border-top-color: #FFFFFB;
                            }
                            
  
                            h4{margin-bottom: 0px;}
                              .chart-container{margin-top:0px;margin-bottom:0px;}
                            
                            .chart-container {
                                margin:0px;
                            }
                            .box{padding:0px;border-radius: 0px;margin-bottom:0px;}
                            
                            .tabbable > .nav > li[class=active]    > a {background-color: black; color:white}
                            
                            text {
                            fill: white !important;
                            }
                          
                            .apexcharts-tooltip {
                            background: white;
                            color: black;
                            }
                          
                          .apexcharts-menu{
                          background-color: white!important;
                          color:black;
                          border: 0px;
                          }
                          .highcharts-drillup-button rect {
                          background-color: white!important;
                          color:#FF5733
                          }
                        .highcharts-drillup-button {
                        background-color: white!important;
                        color:#FF5733;
                        font-size: 10px;
                        }
                        #myHC 
                        .highcharts-drillup-button {
                        background-color: #FF5733; /* Replace with your desired color */
                        color: #FF5733; /* Text color */
                      }
                        
                        .highcharts-contextbutton{
                        symbolFill:black;
                        }

                                        '))),
              tabsetPanel(type = 'tabs',
                          tabPanel('Overview',
                                   br(),
                                   fluidRow(tags$style(".small-box{border-radius: 15px;}"),
                                     column(2,img(src = "dollar.jpg",width='150px',height='100px')),
                                     valueBoxOutput('actual.sales.value',width=2),
                                     valueBoxOutput('actual.rmc',width=2),
                                     valueBoxOutput('planned.rmc',width=2),
                                     valueBoxOutput('planned.wastage',width=2),
                                     valueBoxOutput('write.off',width=2)
                                   ),
                                   
                                   fluidRow(tags$style(".small-box{border-radius: 15px;}"),
                                            valueBoxOutput('over.deviation',width=2),
                                            valueBoxOutput('under.deviation',width=2),
                                            valueBoxOutput('opportunity.loss',width=2),
                                            valueBoxOutput('optimum.rmc',width=2),
                                            valueBoxOutput('write.off.value',width=2),
                                            column(2,img(src = "download.png",width='100px',height='100px',align='centre')),
                                            
                                   ),
                                   fluidRow(
                                     
                                     radioGroupButtons(
                                       inputId = "rmc.category",
                                       choices = c("Product Type", "Buyer", "Plant"),
                                       status = "primary",
                                       size='xs',
                                       checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                                     ),
                                     
                                     column(6,
                                            withSpinner(apexchartOutput('rmc.chart', height = '250px'), type = 5, size = 0.5)
                                     ),
                                     column(6,
                                            withSpinner(apexchartOutput('deviation.chart', height = '250px'), type = 5, size = 0.5)
                                     ),
                                     
                                   ),
                                   
                                   br(),
                                   div(
                                     h3('Top Opportunity Loss Styles', style = "float: left;"),
                                     downloadButton('download.opportunity.loss', label = 'Download', style = "float: right;"),
                                     style = "display:inline-block; width: 100%;"
                                   ),
                                   radioGroupButtons(
                                     inputId = "top.ten.opportunity.loss",
                                     choices = c("Style", "Material"),
                                     status = "primary",
                                     size='xs',
                                     checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                                   ),
                                   reactableOutput('opportunity.loss.summary')
                                   
                                   ),
                          tabPanel('Sales Insights',
                                   
                                   br(),
                                   fluidRow(tags$style(".small-box{border-radius: 15px;}"),
                                            valueBoxOutput('planned.sales.value',width=3),
                                            valueBoxOutput('actual.sales',width=3),
                                            valueBoxOutput('optimum.sales',width=3),
                                            valueBoxOutput('loss.of.sales',width=3),
                                   ),
                                   
                                   fluidRow(
                                     column(3,
                                            h4("Order to Ship"),
                                            withSpinner(echarts4rOutput("order.to.ship",height = "300px"))
                                     ),
                                     
                                     column(3,
                                            h4("Cut to Ship Ratio "),
                                            withSpinner(echarts4rOutput("cut.to.ship",height = "300px"))
                                     ),
                                     
                                     column(3,
                                            h4("Order to Cut"),
                                            withSpinner(echarts4rOutput("order.to.cut",height = "300px"))
                                     ),
                                     
                                     column(3,
                                            h4("Cut to Produce"),
                                            withSpinner(echarts4rOutput("cut.to.prod",height = "300px"))
                                     ),
                              
                                   ),
                                   
                                   fluidRow(style = "margin-top:0px;",
                                     
                                     radioGroupButtons(
                                       inputId = "ratio.category",
                                       choices = c("Buyer", "Plant"),
                                       status = "primary",
                                       size='xs',
                                       checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                                     ),
                                       
                                     br(),
                                     
                                      column(6,
                                              withSpinner(apexchartOutput('ratio.category.chart', height = '250px'), type = 5, size = 0.5)
                                      ),
                                     
                                     column(6,
                                            withSpinner(apexchartOutput('ratio.chart', height = '250px'), type = 5, size = 0.5)
                                     ),
                                     
                                   ),
                                   
                                   fluidRow(
                                     
                                     withSpinner(highchartOutput("myHC",height = '300px'), type = 5, size = 0.5)
                                     
                                   ),
                                     
                                    
                                   
                                   br(),
                                   
                                   div(
                                     h3('Ratios', style = "float: left;"),
                                     downloadButton('download.ratios', label = 'Download', style = "float: right;"),
                                     style = "display:inline-block; width: 100%;"
                                   ),
                                   reactableOutput('ratio.summary')

                                   ),
                          tabPanel('Over Deviation',
                                   br(),
                                   fluidRow(tags$style(".small-box{border-radius: 15px;}"),
                                            valueBoxOutput('od.over.deviation',width=3),
                                            valueBoxOutput('od.against.sales',width=3),
                                            valueBoxOutput('adhoc.value',width=3),
                                            # valueBoxOutput('others',width=3)
                                            
                                   ),
                                   # fluidRow(tags$style(".small-box{border-radius: 15px;}"),
                                   #          valueBoxOutput('adhoc.value',width=3),
                                   #          # valueBoxOutput('under.produce',width=3),
                                   #          # valueBoxOutput('fg.stock',width=3),
                                   #          # valueBoxOutput('over.issued',width=3),
                                   #          valueBoxOutput('others',width=3)
                                   # ),
                                   fluidRow(
                                     br(),
                                     column(6,
                                            withSpinner(apexchartOutput('over.deviation.chart', height = '250px'), type = 5, size = 0.5)
                                     ),
                                     column(6,
                                            withSpinner(apexchartOutput('over.deviation.trend', height = '250px'), type = 5, size = 0.5)
                                     ),
                                     
                                   ),
                                   br(),
                                   
                                   div(
                                     h3('Over Deviation Summary', style = "float: left;"),
                                     downloadButton('download.over.deviation', label = 'Download', style = "float: right;"),
                                     style = "display:inline-block; width: 100%;"
                                   ),
                                   
                                   withSpinner(reactableOutput('over.deviation.summary'), type = 5, size = 0.5)
                                   
                                   ),
                          tabPanel('Under Deviation',
                                   
                                   br(),
                                   fluidRow(tags$style(".small-box{border-radius: 15px;}"),
                                            # valueBoxOutput('ud.under.deviation',width=3),
                                            valueBoxOutput('ud.under.consumption',width=3),
                                            valueBoxOutput('ud.against.sales',width=3),
                                            # valueBoxOutput('other.reason',width=3)
                                            
                                   ),
                                   
                                   fluidRow(
                                     br(),
                                     column(6,
                                            withSpinner(apexchartOutput('under.deviation.chart', height = '250px'), type = 5, size = 0.5)
                                     ),
                                     column(6,
                                            withSpinner(apexchartOutput('under.deviation.trend', height = '250px'), type = 5, size = 0.5)
                                     ),
                                     
                                   ),
                                   br(),
                                   
                                   br(),
                                   
                                   div(
                                     h3('Under Deviation Summary', style = "float: left;"),
                                     downloadButton('download.under.deviation', label = 'Download', style = "float: right;"),
                                     style = "display:inline-block; width: 100%;"
                                   ),
                                   
                                   reactableOutput('under.deviation.summary')
                                   ),
                          tabPanel('RM Insights',
                                   br(),
                                   
                                   div(
                                     h3('RM Build (Raw Material Demand Coverage Analysis)', style = "float: left;"),
                                     style = "display:inline-block; width: 100%;"
                                   ),
                                   # h3('RM Build(Raw Material Demand Coverage Analysis)', style = "float: left;"),
                                   br(),

                                   fluidRow(tags$style(".small-box{border-radius: 15px;}"),
                                            valueBoxOutput('ov.po.value',width=2),
                                            valueBoxOutput('ov.grn.value',width=2),
                                            valueBoxOutput('ov.tran.grn.value',width=2),
                                            valueBoxOutput('ov.issue.value',width=2),
                                            valueBoxOutput('ov.po.adhoc.value',width=2),
                                            valueBox(tags$p('0%', style = "font-size: 0%;"), tags$p("OVERALL", style = "font-size: 120%;"),color='black',width=2),
                                            
                                   ),

                                   fluidRow(tags$style(".small-box{border-radius: 15px;}"),
                                            valueBoxOutput('fb.po.value',width=2),
                                            valueBoxOutput('fb.grn.value',width=2),
                                            valueBoxOutput('fb.tran.grn.value',width=2),
                                            valueBoxOutput('fb.issue.value',width=2),
                                            valueBoxOutput('fb.po.adhoc.value',width=2),
                                            valueBox(tags$p('0%', style = "font-size: 0%;"), tags$p("FABRIC", style = "font-size: 120%;"),color='black',width=2),
                                            
                                   ),
                                   
                                   fluidRow(tags$style(".small-box{border-radius: 15px;}"),
                                            valueBoxOutput('tr.po.value',width=2),
                                            valueBoxOutput('tr.grn.value',width=2),
                                            valueBoxOutput('tr.tran.grn.value',width=2),
                                            valueBoxOutput('tr.issue.value',width=2),
                                            valueBoxOutput('tr.po.adhoc.value',width=2),
                                            valueBox(tags$p('0%', style = "font-size: 0%;"), tags$p("TRIMS", style = "font-size: 120%;"),color='black',width=2),
                                            
                                   ),
                                   br(),
                                   div(
                                     h3('Ratio Analysis', style = "float: left;"),
                                     style = "display:inline-block; width: 100%;"
                                   ),
                                   withSpinner(reactableOutput('po.summary', height = 'auto'), type = 5, size = 0.5),
                                   br(),
                                   radioGroupButtons(
                                     inputId = "consumption.category",
                                     choices = c("Under Deviation", "Over Deviation","Total Deviation"),
                                     status = "primary",
                                     size='xs',
                                     checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                                   ),
                                   fluidRow(
                                     
                                     column(6,
                                            div(
                                              h3('Deviation Distribution', style = "float: left;"),
                                              style = "display:inline-block; width: 100%;"
                                            ),
                                            
                                            withSpinner(apexchartOutput('over.consumption.scatter', height = '250px'), type = 5, size = 0.5)
                                     ),
                                     column(6,
                                            div(
                                              h3('Deviation by Item (Top 100)', style = "float: left;"),
                                              style = "display:inline-block; width: 100%;"
                                            ),
                                            withSpinner(apexchartOutput('over.consumption.treemap', height = '250px'), type = 5, size = 0.5)
                                     ),
                                     
                                   ),
                                   br(),
                                   div(
                                     h3('HeatMap', style = "float: left;"),
                                     style = "display:inline-block; width: 100%;"
                                   ),
                                   withSpinner(reactableOutput('over.consumption.heatmap', height = '300px'), type = 5, size = 0.5),
                                   div(id = "colorLegend", 
                                       style = "position: relative; background-color: #000; padding: 10px; border: 1px solid #000;",
                                       "Legend:",
                                       div(style = "background-color: #A393BF; width: 20px; height: 20px; display: inline-block;"),
                                       "0% - 3%",
                                       div(style = "background-color: #AAF683; width: 20px; height: 20px; display: inline-block;"),
                                       "3% - 5%",
                                       div(style = "background-color: #F5E2C8; width: 20px; height: 20px; display: inline-block;"),
                                       "5% - 10%",
                                       div(style = "background-color: #FDCA40; width: 20px; height: 20px; display: inline-block;"),
                                       "10% - 20%",
                                       div(style = "background-color: #088A08; width: 20px; height: 20px; display: inline-block;"),
                                       "20% - 100%",
                                       div(style = "background-color: #FF0000; width: 20px; height: 20px; display: inline-block;"),
                                       "100% or above"
                                   ),
                                   div(
                                     h3('Top 5 Item by Deviation', style = "float: left;"),
                                     style = "display:inline-block; width: 100%;"
                                   ),
                                   br(),
                                   radioGroupButtons(
                                     inputId = "deviation.category",
                                     choices = c("Over Deviation", "Under Deviation","Total Deviation"),
                                     status = "primary",
                                     size='xs',
                                     checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                                   ),
                                   
                                   withSpinner(reactableOutput('saving.item.wise.tbl', height = 'auto'), type = 5, size = 0.5),
                                   br(),
                                   radioGroupButtons(
                                     inputId = "group.category",
                                     choices = c("OCNo", "Style_Code", "Plant"),
                                     status = "primary",
                                     size='xs',
                                     checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                                   ),
                                   
                                   withSpinner(reactableOutput('saving.oc.wise.tbl', height = 'auto'), type = 5, size = 0.5),
                                   
                                   


                          ),
                          # tabPanel('W/Off')
                          # tabPanel('LeftOver Stock',
                          #          
                          #          br(),
                          #          fluidRow(tags$style(".small-box{border-radius: 15px;}"),
                          #                   valueBoxOutput('lo.sales.value',width=3),
                          #                   valueBoxOutput('left.over.stock',width=3),
                          #                   valueBoxOutput('stock.against.sales',width=3),
                          # 
                          #          ),
                          #          fluidRow(
                          #            br(),
                          #            column(6,
                          #                   withSpinner(apexchartOutput('left.stock.reason.chart', height = '250px'), type = 5, size = 0.5)
                          #            ),
                          #            column(6,
                          #                   withSpinner(apexchartOutput('left.stock.trend.chart', height = '250px'), type = 5, size = 0.5)
                          #            ),
                          #            
                          #          ),
                          #          
                          #          br(),
                          #          
                          #          div(
                          #            h3('LeftOver Stock Summary', style = "float: left;"),
                          #            downloadButton('download.left.over.stock', label = 'Download', style = "float: right;"),
                          #            style = "display:inline-block; width: 100%;"
                          #          ),
                          #          
                          #          
                          #          reactableOutput('left.over.stock.summary')
                          #          ),
                         

              
                          ),

              )
      )
    )
    
  
)