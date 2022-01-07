library(jsonlite)
library(httr)
library(shiny)
library(bs4Dash)
library(shinyjs)
library(echarts4r)
library(data.table)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)

#request a list of companies currently fundraising using httr


r <- GET("http://localhost:9000/getdata")
  #convert to text object using httr
raise <- content(r, as="text")
  #parse JSON
new <- fromJSON(raise)
while (length(new) == 0) {
  r <- GET("http://localhost:9000/getdata")
  #convert to text object using httr
  raise <- content(r, as="text")
  #parse JSON
  new <- fromJSON(raise)
  Sys.sleep(10)
}


store <-  new %>% filter(datacomefrom == 1)
store$time<-as.POSIXct(store$time,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS")
system <- new %>% filter(datacomefrom == 2) 
system$time<-as.POSIXct(system$time,tz="Europe/Istanbul",format= "%d.%m.%y %H:%M")
final <- rbind(store,system)
final$mnth<-month(final$time) 

ui <- dashboardPage(
      header = dashboardHeader(),
      sidebar = dashboardSidebar(disable = T),
      body = dashboardBody(
        fluidRow(
          bs4InfoBoxOutput("Total_Income_from_store",width = 6),
          bs4InfoBoxOutput("Total_Income_from_system",width = 6)
        ),
        fluidRow(
          bs4InfoBoxOutput("system_Daily_mean_Income_Infobox1",width = 4),
          bs4InfoBoxOutput("system_Weekly_mean_Income_Infobox2",width = 4),
          bs4InfoBoxOutput("system_Monthly_mean_Income_Infobox3",width = 4)
        ),     
        fluidRow(
          bs4InfoBoxOutput("store_Daily_mean_Income_Infobox1",width = 4),
          bs4InfoBoxOutput("store_Weekly_mean_Income_Infobox2",width = 4),
          bs4InfoBoxOutput("store_Monthly_mean_Income_Infobox3",width = 4)
        ),
        fluidRow(
          column(6,echarts4rOutput("total_bar_monthly")),
          column(6,echarts4rOutput("total_pie_")) 
        ),
        
      ),
      
      
)
    
server <- function(input, output) {
      
      
      output$Total_Income_from_store <- renderbs4InfoBox({
        df <- final %>% filter(datacomefrom == 1)
        value <- sum(df$payment)
        bs4InfoBox("Toplam Kazanç (Şube)",paste0(format(value,big.mark=",",scientific=FALSE)," ₺")
                   , icon = icon("fas fa-lira-sign"),
                   color = "navy", fill = TRUE )
        
      })
      
      
      
      output$Total_Income_from_system <- renderbs4InfoBox({
        df <- final %>% filter(datacomefrom == 2)
        value <- sum(df$payment)
        bs4InfoBox("Toplam Kazanç (Sistem)",paste0(format(value,big.mark=",",scientific=FALSE)," ₺")
                   , icon = icon("fas fa-lira-sign"),
                   color = "white", fill = TRUE )
        
      })
      
      
      output$system_Daily_mean_Income_Infobox1 <- renderbs4InfoBox({
        df <- final %>% filter(datacomefrom == 2)
        value1 <- sum(df$payment)
        value <- value1/((as.numeric(max(df$time))-as.numeric(min(df$time)))/(24*60*60))
        bs4InfoBox("Günlük Ortalama Kazanç (Sistem)",paste0(format(value,big.mark=",",scientific=FALSE)," ₺")
                   , icon = icon("fas fa-lira-sign"),
                   color = "white", fill = TRUE )
        
      })
      
      
      
      output$system_Weekly_mean_Income_Infobox2 <- renderbs4InfoBox({
        df <- final %>% filter(datacomefrom == 2)
        value1 <- sum(df$payment)
        value <- value1/((as.numeric(max(df$time))-as.numeric(min(df$time)))/(24*60*60*7))
        bs4InfoBox("Haftalık Ortalama Kazanç (Sistem)",paste0(format(value,big.mark=",",scientific=FALSE)," ₺")
                   , icon = icon("fas fa-lira-sign"),
                   color = "white", fill = TRUE )
        
      })
      
      
      output$system_Monthly_mean_Income_Infobox3 <- renderbs4InfoBox({
        df <- final %>% filter(datacomefrom == 2)
        value1 <- sum(df$payment)
        value <- value1/((as.numeric(max(df$time))-as.numeric(min(df$time)))/(24*60*60*30))
        bs4InfoBox("Aylık Ortalama Kazanç (Sistem)",paste0(format(value,big.mark=",",scientific=FALSE)," ₺")
                   , icon = icon("fas fa-lira-sign"),
                   color = "white", fill = TRUE )
        
      })
      
      
      output$store_Daily_mean_Income_Infobox1 <- renderbs4InfoBox({
        df <- final %>% filter(datacomefrom == 1)
        value1 <- sum(df$payment)
        value <- value1/((as.numeric(max(df$time))-as.numeric(min(df$time)))/(24*60*60))
        bs4InfoBox("Günlük Ortalama Kazanç (Şube)",paste0(format(value,big.mark=",",scientific=FALSE)," ₺")
                   , icon = icon("fas fa-lira-sign"),
                   color = "navy", fill = TRUE )
        
      })
      
      
      output$store_Weekly_mean_Income_Infobox2 <- renderbs4InfoBox({
        df <- final %>% filter(datacomefrom == 1)
        value1 <- sum(df$payment)
        value <- value1/((as.numeric(max(df$time))-as.numeric(min(df$time)))/(24*60*60*7))
        bs4InfoBox("Haftalık Ortalama Kazanç (Şube)",paste0(format(value,big.mark=",",scientific=FALSE)," ₺")
                   , icon = icon("fas fa-lira-sign"),
                   color = "navy", fill = TRUE )
        
      })
      
      
      
      output$store_Monthly_mean_Income_Infobox3 <- renderbs4InfoBox({
        df <- final %>% filter(datacomefrom == 1)
        value1 <- sum(df$payment)
        value <- value1/((as.numeric(max(df$time))-as.numeric(min(df$time)))/(24*60*60*30))
        bs4InfoBox("Aylık Ortalama Kazanç (Şube)",paste0(format(value,big.mark=",",scientific=FALSE)," ₺")
                   , icon = icon("fas fa-lira-sign"),
                   color = "navy", fill = TRUE )
        
      })
      
      
      output$total_bar_monthly<-renderEcharts4r({
        
        newdf<-final %>% group_by(mnth,datacomefrom) %>% summarise(total = sum(payment,na.rm = T))
        
        
        
        newdf  %>% group_by(datacomefrom) %>%
          e_charts(total)%>%
          e_legend(show = T)%>%
          e_bar(mnth) %>%
          
          e_legend(orient = 'vertical', 
                   right = '1', top = '5%')%>%
          
          e_flip_coords() %>%
          e_datazoom(x_index = c(0, 1)) %>%
          e_tooltip() 
      })
      
      
      output$total_pie_<-renderEcharts4r({
        n<-final$datacomefrom
        n<-gsub(1,"Şube",n)
        n<-gsub(2,"Sistem",n)
        final$type<-n
        val1<- final %>% filter(datacomefrom == 1)
        val1<-sum(val1$payment)
        val2 <-  final %>% filter(datacomefrom == 2)
        val2<-sum(val2$payment)
        newdf<-data.frame("type"=c("Şube","Sistem"),val=c(val1,val2))
        newdf  %>% 
          e_charts(type)%>%
          e_pie(val) %>%
          e_tooltip() 
      })
      
      
      
      
    }
    
    # Define UI for application that draws a histogram
    
shinyApp(ui = ui, server = server)
