#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(dplyr)
library(ggpubr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggbeeswarm)

library(viridis)


load("./data/C02NorthernHemisphere.Rdata")
load("./data/C02Worldwide.Rdata")
load("./data/CanadianMeanTemp.Rdata")
load("./data/CanadianMinTemp.Rdata")
load("./data/CanadianAvgSnow.Rdata")
load("./data/CanadianMaxTemp.Rdata")
load("./data/CanadianPrecip.Rdata")

#data cleaning--------------------------------------------------------------------------------
maxname=names(MaxTemp)
for(i in 1:(length(maxname))){
  MaxTemp[,i][MaxTemp[,i]==-9999.9]<-NA
  
}
for(i in 1:ncol(MaxTemp)){
    MaxTemp[is.na(MaxTemp[,i]), i] <-round(mean(MaxTemp[,i], na.rm = TRUE))

} 

a<-MaxTemp %>%
  filter(`InfoTemp[2]`=="VANCOUVER")

#data cleaning
meanname=names(MeanTemp)
for(i in 1:(length(meanname))){
  MeanTemp[,i][MeanTemp[,i]==-9999.9]<-NA
}
for(i in 1:ncol(MeanTemp)){
  MeanTemp[is.na(MeanTemp[,i]), i] <-round(mean(MeanTemp[,i], na.rm = TRUE))
  
}
b<-MeanTemp %>%
  filter(`InfoTemp[2]`=="VANCOUVER")
b=b%>%filter(Year!=1896)
#data cleaning
Minname=names(MinTemp)
for(i in 1:(length(Minname))){
  MinTemp[,i][MinTemp[,i]==-9999.9]<-NA
}
for(i in 1:ncol(MinTemp)){
  MinTemp[is.na(MinTemp[,i]), i] <-round(mean(MinTemp[,i], na.rm = TRUE))
}
c<-MinTemp %>%
  filter(`InfoTemp[2]`=="VANCOUVER")

#data cleaning
precipname=names(AllPrecip)
for(i in 1:(length(precipname))){
  AllPrecip[,i][AllPrecip[,i]==-9999.9]<-NA
  
}
for(i in 1:ncol(AllPrecip)){
  AllPrecip[is.na(AllPrecip[,i]), i] <-round(mean(AllPrecip[,i], na.rm = TRUE))
} 
d<-AllPrecip %>%
  filter(`InfoTemp[2]`=="VANCOUVER")

#data cleaning
snowname=names(AllSnow)
for(i in 1:(length(snowname))){
  AllSnow[,i][AllSnow[,i]==-9999.9]<-NA
  
}
for(i in 1:ncol(AllSnow)){
  AllSnow[is.na(AllSnow[,i]), i] <-round(mean(AllSnow[,i], na.rm = TRUE))
  
} 
e<-AllSnow %>%
  filter(`InfoTemp[2]`=="VANCOUVER")










###########start shiny function###############--------------------------------------------------
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    if(input$data=="Vancouver Temperature"){
      if(input$check4=="TRUE"){
        cData3=gather(b,month,Temperature,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)
        cData3$month=as.factor(cData3$month)
        cData3$month= factor(cData3$month,levels(cData3$month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
        levels(cData3$month)=c(1,2,3,4,5,6,7,8,9,10,11,12)
        cData3=filter(cData3,month==input$bins)
        ggplot(cData3,aes(x=Year,y=Temperature,color=Temperature))+geom_point()+ scale_colour_viridis()+scale_color_gradient(low="lightblue", high="red")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Vancouver Monthly Temperature")+ylim(-10,30)+geom_smooth()
      }
      else if(input$check4=="FALSE"){
        cData3=gather(b,month,Temperature,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)
        cData3$month=as.factor(cData3$month)
        cData3$month= factor(cData3$month,levels(cData3$month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
        levels(cData3$month)=c(1,2,3,4,5,6,7,8,9,10,11,12)
        cData3=filter(cData3,month==input$bins)
        ggplot(cData3,aes(x=Year,y=Temperature,color=Temperature))+geom_point()+ scale_colour_viridis()+scale_color_gradient(low="lightblue", high="red")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Vancouver Monthly Temperature")+ylim(-10,30)
      }
    }
  })
  

  output$seasonplot<-renderPlot({
    if(input$data=="Vancouver Temperature"){
      
    if(input$season=="Spring"){
      p=ggplot(b,aes(Year,Spring))+geom_line(na.rm = T)+ggtitle("Year vs Spring Temperature")+ylab("Temperature")
      p
    }
    
    
    else if(input$season=="Summer"){
      p=ggplot(b,aes(Year,Summer))+geom_line(na.rm = T)+ggtitle("Year vs Summer Temperature")+ylab("Temperature")
      p
    }
    else if(input$season=="Autumn"){
      p=ggplot(b,aes(Year,Autumn))+geom_line(na.rm = T)+ggtitle("Year vs Autumn Temperature")+ylab("Temperature")
      p
    }
    else if(input$season=="Winter"){
      p=ggplot(b,aes(Year,Winter))+geom_line(na.rm = T)+ggtitle("Year vs Winter Temperature")+ylab("Temperature")
      p
    }
    if (input$check=="TRUE" & (input$check3=="TRUE")){
      p3=p+geom_smooth(method=loess)
      MaxTemplong2=gather(b,season,Temperature,Winter,Spring,Summer,Autumn)
      MaxTemplong2 %>% 
        select(Year,season,Temperature) %>%
        group_by(Year,season) %>%
        summarise(value=mean(Temperature))-> cData5
      p2=ggplot(cData5,aes(Year,value,color=season))+geom_line()+ggtitle("Year vs Temperature within different seasons")+geom_smooth()
      p2
    }
    else if (input$check=="FALSE"& (input$check3=="TRUE")){
      p
      MaxTemplong2=gather(b,season,Temperature,Winter,Spring,Summer,Autumn)
      MaxTemplong2 %>% 
        select(Year,season,Temperature) %>%
        group_by(Year,season) %>%
        summarise(value=mean(Temperature))-> cData5
      p2=ggplot(cData5,aes(Year,value,color=season))+geom_line()+ggtitle("Year vs Temperature within different seasons")
      p2
    }
    else if (input$check=="FALSE"& (input$check3=="FALSE")){
      p
    }
    else if (input$check=="TRUE"& (input$check3=="FALSE")){
      p+geom_smooth(method=loess)
    }
    }

  })
 
  # output$densityplot<-renderPlot({
  #   hold=MaxTemp[MaxTemp$`InfoTemp[3]`==input$city,]
  #   plot(density(hold[,2]),type="n",ylim=c(0,0.4),xlim=c(-50,40))
  #   for(i in 1:12){
  #     lines(density(hold[,i+1]),col=i,lwd=2)
  #     colnames(MaxTemp)[2:13]
  #     legend("topleft",col=1:12,lwd=2,colnames(MaxTemp)[2:13])
  #   }
  # })
  # 
  
  
  
  
  
#------------------------------------------------------------------------------------------------
  
  output$seasonplot2<-renderPlot({
    if(input$data=="Vancouver Rainfall"){
      
      if(input$season2=="Spring"){
        p=ggplot(d,aes(Year,Spring))+geom_line(na.rm = T)+ggtitle("Year vs Spring precipitation")+ylab("precipitation")
        p
      }
      
      
      else if(input$season2=="Summer"){
        p=ggplot(d,aes(Year,Summer))+geom_line(na.rm = T)+ggtitle("Year vs Summer precipitation")+ylab("precipitation")
        p
      }
      else if(input$season2=="Autumn"){
        p=ggplot(d,aes(Year,Autumn))+geom_line(na.rm = T)+ggtitle("Year vs Autumn precipitation")+ylab("precipitation")
        p
      }
      else if(input$season2=="Winter"){
        p=ggplot(d,aes(Year,Winter))+geom_line(na.rm = T)+ggtitle("Year vs Winter precipitation")+ylab("precipitation")
        p
      }
      if (input$check43=="TRUE" & (input$check44=="TRUE")){
        p3=p+geom_smooth(method=loess)
        MaxTemplong2=gather(d,season,precipitation,Winter,Spring,Summer,Autumn)
        MaxTemplong2 %>% 
          select(Year,season,precipitation) %>%
          group_by(Year,season) %>%
          summarise(value=mean(precipitation))-> cData5
        p2=ggplot(cData5,aes(Year,value,color=season))+geom_line()+ggtitle("Year vs precipitation within different seasons")+geom_smooth()
        p2
      }
      else if (input$check43=="FALSE"& (input$check44=="TRUE")){
        p
        MaxTemplong2=gather(d,season,precipitation,Winter,Spring,Summer,Autumn)
        MaxTemplong2 %>% 
          select(Year,season,precipitation) %>%
          group_by(Year,season) %>%
          summarise(value=mean(precipitation))-> cData5
        p2=ggplot(cData5,aes(Year,value,color=season))+geom_line()+ggtitle("Year vs precipitation within different seasons")
        p2
      }
      else if (input$check43=="FALSE"& (input$check44=="FALSE")){
        p
      }
      else if (input$check43=="TRUE"& (input$check44=="FALSE")){
        p+geom_smooth(method=loess)
      }
    }
    
  })
  
  # output$densityplot<-renderPlot({
  #   hold=MaxTemp[MaxTemp$`InfoTemp[3]`==input$city,]
  #   plot(density(hold[,2]),type="n",ylim=c(0,0.4),xlim=c(-50,40))
  #   for(i in 1:12){
  #     lines(density(hold[,i+1]),col=i,lwd=2)
  #     colnames(MaxTemp)[2:13]
  #     legend("topleft",col=1:12,lwd=2,colnames(MaxTemp)[2:13])
  #   }
  # })
  # 
  output$distPlot3<-renderPlot({
    if(input$data=="Vancouver Rainfall"){
      if(input$check42=="TRUE"){
        cData3=gather(d,month,precipitation,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)
        cData3$month=as.factor(cData3$month)
        cData3$month= factor(cData3$month,levels(cData3$month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
        levels(cData3$month)=c(1,2,3,4,5,6,7,8,9,10,11,12)
        cData3=filter(cData3,month==input$bins2)
        ggplot(cData3,aes(x=Year,y=precipitation,color=precipitation))+geom_point()+ scale_colour_viridis()+scale_color_gradient(low="lightblue", high="blue")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Vancouver Monthly precipitation")+ylim(0,400)+geom_smooth()
      }
      else if(input$check42=="FALSE"){
        cData3=gather(d,month,precipitation,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)
        cData3$month=as.factor(cData3$month)
        cData3$month= factor(cData3$month,levels(cData3$month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
        levels(cData3$month)=c(1,2,3,4,5,6,7,8,9,10,11,12)
        cData3=filter(cData3,month==input$bins2)
        ggplot(cData3,aes(x=Year,y=precipitation,color=precipitation))+geom_point()+ scale_colour_viridis()+scale_color_gradient(low="lightblue", high="blue")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Vancouver Monthly precipitation")+ylim(0,400)
      }
    }
  })
  
  
#-----------------------------------------------------------------------------------------------------
  output$CO2plot<-renderPlot({
    if(input$data=="Vancouver CO2"){
    Co2North=filter(Co2North,YearDecimal<=input$bins3)
    ggplot(Co2North,aes(YearDecimal,Latitude49value))+geom_point()+xlab("Year")+ylab("CO2")+ggtitle("Year vs CO2")
    }
  })
#-----------------------------------------------------------------------------------------------------
  output$snowplot<-renderPlot({
    cData7=gather(e,month,snow,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)
    cData7$month=as.factor(cData7$month)
    cData7$month= factor(cData7$month,levels(cData7$month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
    levels(cData7$month)=c(1,2,3,4,5,6,7,8,9,10,11,12)
    p10=cData7%>% ggplot(aes(month,snow,color=snow))+ geom_quasirandom() + scale_colour_viridis()+theme_dark()+ scale_colour_viridis()+scale_color_gradient(low="white", high="lightblue")+ggtitle("Vancouver snowfall over the years every month")+ylab("snow(centimetre)")
    if(input$check50=="TRUE"){
      p10+geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=1)
    }
    else if (input$check50=="FALSE"){
      p10
    }
  })                                                                                                      
  
  
  
  
  
  
  
  
  
  
#-----------------------------------------------------------------------------------------------------
  b<-MeanTemp %>%
    filter(`InfoTemp[2]`=="VANCOUVER")
  b=b%>%filter(Year!=1896)
  MaxTemplong=gather(b,season,Temperature,Winter,Spring,Summer,Autumn)
  output$distplot2<-renderPlot({
    
    MaxTemplong %>% 
      select(Year,season,Temperature) %>%
      group_by(Year,season) %>%
      summarise(value=mean(Temperature))-> cData5
    p2=ggplot(cData5,aes(Year,value,color=season))+geom_line()+ggtitle("Year vs Temperature within different seasons")+geom_smooth(method='loess')
    p2
  
  })
  
  
  output$Co2<-renderPlot({
    ggplot(Co2North,aes(YearDecimal,Latitude49value))+geom_point()+xlab("Year")+ylab("CO2")+ggtitle("Year vs CO2")+geom_smooth(method="lm")
  })
  output$temp<-renderPlot({
    par(mfrow=c(2,2))
    
    plot(b$Year,b$Annual,ylab="average temperature in Vancouver",xlab="year",main="Temperature vs year",pch=20)
    abline(lm(b$Annual~b$Year),col="blue",lwd=3)
    plot(d$Year,d$Annual,ylab="rainfall total",xlab="year",main="rainfall vs year",pch=20)
    abline(lm(d$Annual~d$Year),col="blue",lwd=3)
    plot(e$Year,e$Annual,ylab="snow total",xlab="year",main="snowfall vs year",pch=20)
    abline(lm(e$Annual~e$Year),col="blue",lwd=3)
    plot(Co2North$YearDecimal,Co2North$Latitude49value,xlab="year",ylab="Co2",main="Co2 vs year",pch=20)
    abline(lm(Co2North$Latitude49value~Co2North$YearDecimal),col="blue",lwd=3)
  })
  
})

