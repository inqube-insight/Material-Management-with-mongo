library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(openxlsx)
library(bslib)
library(shinythemes)
library(dplyr)
library(tidyr)
library(stats)
library(shinyjs)
library(tidyverse)
library(excelR)
library(treemap)
library(echarts4r)
library(highcharter)
library(Cairo)
library(ggiraph) 
library(plotly)
library(shinycssloaders)
library(lubridate)
library(RMySQL)
library(rsconnect)
library(apexcharter)
library(shinyWidgets)
library(stringr)
library(reactable)
library(htmltools)
library(tippy)
library(mongolite)
library(jsonlite)
library(scales)
library(viridis)
library(reactablefmtr)

recon.data=data.frame()
options(shiny.usecairo=T) 

# mongo_url <- 'mongodb+srv://kalharig:AidFRD0oXjq32hgd@cluster0.nxi2d3z.mongodb.net/Weekly_Reconcilation_Report'
mongo_url <-"mongodb://MongodbAdmin:t%262uO%7Cr_8%5D4%5DJ68@SG-PROD-NLB-56d694edf100cb2d.elb.ap-southeast-1.amazonaws.com:27017/?tls=true&directConnection=true&tlsAllowInvalidHostnames=true"


source('ui.R')
source('server.R')

# m=mongo("Recon_Data",db='Weekly_Reconcilation_Report',url= mongo_url)
# recon.data =m$find('{}')
# 
# recon.data=recon.data %>%
#   mutate(across(-matches('Code|Type|No|Name|Week|Category|Status|UOM|Plant|Season|Buyer|Closed.On'), as.double)) 
# print('SALES START')
# print(Sys.time())

s =mongo("Sales_Summary",db='Weekly_Reconcilation',url=mongo_url,options = ssl_options(weak_cert_validation = TRUE))
sales.data=s$find('{}')
sales.data =sales.data %>% filter(!Week %in% c('2023-20','2023-21','2023-22'))%>%
  mutate(Plant= case_when(
    grepl('QUANTUM CLOTHING HORANA', Plant, ignore.case = T) ~ 'QCL',
    grepl('SUB-QUANTUM APPAREL CAMBODIA', Plant, ignore.case = T) ~ 'QAC',
    grepl('BRANDIX ATHLEISURE POLONNARUWA', Plant, ignore.case = T) ~ 'BALP',
    grepl('BRANDIX ATHLEISURE GIRITALE', Plant, ignore.case = T) ~ 'BALG',
    grepl('MINUWANGODA', Plant, ignore.case = T) ~ 'BAIM',
    grepl('INQUBE SOLUTION SEAMLESS-SEW', Plant, ignore.case = T) ~ 'ISSW',
    TRUE ~ Plant
  ))
# print('SALES FINISH')
# print(Sys.time())

r=mongo("RM_Summary",db='Weekly_Reconcilation',url=mongo_url,options = ssl_options(weak_cert_validation = TRUE))
rm.data=r$find('{}')


rm.data =rm.data%>%
  with_groups(c(Buyer,OCNo),
              summarise,
              across(everything(),first)
  )

rm.data=rm.data %>%
  mutate(Plant= case_when(
    grepl('QUANTUM CLOTHING HORANA', Plant, ignore.case = T) ~ 'QCL',
    grepl('SUB-QUANTUM APPAREL CAMBODIA', Plant, ignore.case = T) ~ 'QAC',
    grepl('BRANDIX ATHLEISURE POLONNARUWA', Plant, ignore.case = T) ~ 'BALP',
    grepl('BRANDIX ATHLEISURE GIRITALE', Plant, ignore.case = T) ~ 'BALG',
    grepl('MINUWANGODA', Plant, ignore.case = T) ~ 'BAIM',
    grepl('INQUBE SOLUTION SEAMLESS-SEW', Plant, ignore.case = T) ~ 'ISSW',
    TRUE ~ Plant
  ))
# print('RM FINISH')
# print(x=Sys.time())
# w.off.data =mongo("Write_Off_Details",db='Weekly_Reconcilation',url=mongo_url,options = ssl_options(weak_cert_validation = TRUE))
# W.Off.Data=w.off.data$find('{}')

rm.data =rm.data %>%
  mutate(Material_Item_Code=Material$Item_Code,
         Material_Mat_Color_Code=Material$Mat_Color_Code
         )

# ratio=mongo("Ratio_Summary",db='Weekly_Reconcilation',url=mongo_url,options = ssl_options(weak_cert_validation = TRUE))
# ratio.data=ratio$find(query='{"Status":"Active"}')
# ratio.data =ratio.data %>%
#   mutate(Plant= case_when(
#     grepl('QUANTUM CLOTHING HORANA', Plant, ignore.case = T) ~ 'QCL',
#     grepl('SUB-QUANTUM APPAREL CAMBODIA', Plant, ignore.case = T) ~ 'QAC',
#     grepl('BRANDIX ATHLEISURE POLONNARUWA', Plant, ignore.case = T) ~ 'BALP',
#     grepl('BRANDIX ATHLEISURE GIRITALE', Plant, ignore.case = T) ~ 'BALG',
#     grepl('MINUWANGODA', Plant, ignore.case = T) ~ 'BAIM',
#     grepl('INQUBE SOLUTION SEAMLESS-SEW', Plant, ignore.case = T) ~ 'ISSW',
#     TRUE ~ Plant
#   ))

# open=mongo("VPO_Update_Open",db='Weekly_Reconcilation',url=mongo_url,options = ssl_options(weak_cert_validation = TRUE))
# open.qty =open$find(fields='{"Style_Code":1,"Season":1,"Open_Qty":1,"Buyer":1}')
# 
# open.qty.data=open.qty %>% mutate(Style_Code=str_sub(Style_Code,1,9))


week.data =sales.data %>% distinct(Week)

setBorderColor <- function(valueBoxTag, color){tagQuery(valueBoxTag)$find("div.small-box")$addAttrs("style" = sprintf("border-style: solid; border-color: %s; height: 80px;", color))$allTags()}


shinyApp(ui = ui, server = server)