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

s =mongo("Sales_Summary",db='Weekly_Reconcilation_Report',url=mongo_url,options = ssl_options(weak_cert_validation = TRUE))
sales.data=s$find('{}')


r=mongo("RM_Summary",db='Weekly_Reconcilation_Report',url=mongo_url,options = ssl_options(weak_cert_validation = TRUE))
rm.data=r$find('{}')


rm.data =rm.data%>%
  with_groups(c(Buyer,OCNo),
              summarise,
              across(everything(),first)
  )

ratio=mongo("Ratio_Summary",db='Weekly_Reconcilation_Report',url=mongo_url,options = ssl_options(weak_cert_validation = TRUE))
ratio.data=ratio$find('{}')

open=mongo("VPO_Update_Open",db='Weekly_Reconcilation_Report',url=mongo_url,options = ssl_options(weak_cert_validation = TRUE))
open.qty =open$find(fields='{"Style_Code":1,"Color_Code":1,"DOC_Order_Qty":1,"Ship_Qty":1}')

open.qty.data=open.qty %>% mutate(Style_Code=str_sub(Style_Code,1,9),
                                  Open_Qty=as.double(DOC_Order_Qty)-as.double(Ship_Qty))

open.qty.data=open.qty.data%>%
  filter(Open_Qty>0)%>%
  group_by(Style_Code)%>%
  summarise(Open_Qty=sum(Open_Qty)) %>%
  mutate(Style_Code=str_sub(Style_Code,1,9))

week.data =sales.data %>% distinct(Week)

setBorderColor <- function(valueBoxTag, color){tagQuery(valueBoxTag)$find("div.small-box")$addAttrs("style" = sprintf("border-style: solid; border-color: %s; height: 80px;", color))$allTags()}


shinyApp(ui = ui, server = server)