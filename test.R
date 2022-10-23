load("C02NorthernHemisphere.Rdata")
load("C02Worldwide.Rdata")
load("CanadianMeanTemp.Rdata")
load("CanadianMinTemp.Rdata")
load("CanadianAvgSnow.Rdata")
load("CanadianMaxTemp.Rdata")
load("CanadianPrecip.Rdata")

#data cleaning
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


#------------------------------------------------------------------------------------------
data_test=filter(MaxTemp,Year==2015)


levels(data_test$`InfoTemp[3]`)=c("British Columbia","Yukon","Northwest Territories","Nunavut","Alberta","Saskatchewan","Manitoba",
                      "Ontario","Quebec","New Brunswick","Nova Scotia"," Prince Edward Island",
                                  "Newfoundland and Labrador")
                                  
                                  


#' select columns of interest
data_test %>% 
  select(Year,Annual,`InfoTemp[3]`) %>%
  group_by(Year,`InfoTemp[3]`) %>%
  summarise(value=mean(Annual))-> cData4
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggbeeswarm)
library(gridExtra)
library(viridis)

min(a$Year)
max(a$Year)
cData3=gather(a,month,Temperature,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)
cData3$month=as.factor(cData3$month)
levels(cData3$month)=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
ggplot(cData3,aes(x=month,y=Temperature,color=Temperature))+geom_smooth()+geom_point()+ scale_colour_viridis()+scale_color_gradient(low="blue", high="red")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Vancouver Monthly Temperature")



MaxTemplong2=gather(a,season,Temperature,Winter,Spring,Summer,Autumn)
MaxTemplong2 %>% 
  select(Year,season,Temperature) %>%
  group_by(Year,season) %>%
  summarise(value=mean(Temperature))-> cData5
par(mfrow=c(2,1))
ggplot(cData5,aes(Year,value,color=season))+geom_line()+ggtitle("Year vs Temperature within different seasons")+geom_smooth()

ggplot(a,aes(Year,Spring))+geom_line(na.rm = T)+ggtitle("Year vs Spring Temperature")+geom_smooth(method=loess)+ylab("Temperature")


cData3=gather(d,month,Temperature,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)
cData3$month=as.factor(cData3$month)
levels(cData3$month)=c(1,2,3,4,5,6,7,8,9,10,11,12)
cData3=filter(cData3,month==1)
ggplot(cData3,aes(x=Year,y=Temperature,color=Temperature))+geom_point()+ scale_colour_viridis()+scale_color_gradient(low="blue", high="red")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Vancouver Monthly Temperature")+geom_smooth()

#------------------------------------------------------------------------------------------------------------
ggplot(d,aes(Year,Summer))+geom_line(na.rm = T)+ggtitle("Year vs Spring Temperature")+geom_smooth(method=loess)+ylab("Temperature")





cData7=gather(e,month,Temperature,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)
cData7$month=as.factor(cData7$month)
cData7$month= factor(cData7$month,levels(cData7$month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
levels(cData7$month)=c(1,2,3,4,5,6,7,8,9,10,11,12)
cData7%>% ggplot(aes(month,Temperature,color=Temperature))+ geom_quasirandom() + scale_colour_viridis()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                                                                                                                      outlier.size=1)















boxplot(MaxTemp$Jan~MaxTemp$`InfoTemp[3]`,las=2)
plot(x=MaxTemp$Year,y=MaxTemp$Jan, xlim=c(1890, 2015),type="l")

BC=MaxTemp[(MaxTemp$`InfoTemp[3]`=="BC"),]

plot(density(BC[,2]),type="n")

haha=filter(MaxTemp,`InfoTemp[3]`=="BC")

unique(MaxTemp$`InfoTemp[3]`)

type=unique(MaxTemp$`InfoTemp[3]`)
type=as.character(type)
for(k in 1:(length(type))){
  hold=MaxTemp[MaxTemp$`InfoTemp[3]`==type[k],]
  plot(density(hold[,2]),type="n")
  for(i in 1:12){
    lines(density(hold[,i+1]),col=i,lwd=2)
    colnames(MaxTemp)[2:13]
    legend("topleft",col=1:12,lwd=2,colnames(MaxTemp)[2:13])
  }
  
}
year=unique(MaxTemp$Year)
  plot(haha$Year,haha$Annual)
  
type=unique(MaxTemp$`InfoTemp[3]`)
type=as.character(type) 
for(i in 1:length(type)){
a<- MaxTemp %>%filter(`InfoTemp[3]`==type[i])%>%
    group_by(Year) 
    plot(a$Year,a$Annual,las=2, main = type[i])
}


M1 = MaxTemp %>%
  group_by(`InfoTemp[3]`) %>%
  mutate(mean = mean(Annual))
plot(M1$Year, M1$mean,main=type[i])



MaxTemp<-MaxTemp %>%
  group_by(Year) %>%
  mutate(mean_winter=mean(Winter),
         mean_summer=mean(Summer),
         mean_spring=mean(Spring),
         mean_autumn=mean(Autumn))


plot(a$Year,a$mean,las=2)

plot(MaxTemp$Year,MaxTemp$mean_summer,las=2)
abline(lm(MaxTemp$mean_summer~MaxTemp$Year))
barplot(MaxTemp$mean_autumn)

plot(Co2World$YearDecimal,Co2World$Value)



precipname=names(AllPrecip)
for(i in 1:(length(precipname))){
  AllPrecip[,i][AllPrecip[,i]==-9999.9]<-NA
  
}
for(i in 1:ncol(AllPrecip)){
  AllPrecip[is.na(AllPrecip[,i]), i] <-round(mean(AllPrecip[,i], na.rm = TRUE))
  
} 

hist(a$Annual,100,freq=F)
lines(density(a$Annual,from = 0))


plot(AllPrecip$Year,AllPrecip$Annual)




plot(a$Year,a$Spring,las=2,xlab="year",ylab="temperature",col=1)

a<-MaxTemp %>%
  filter(`InfoTemp[2]`=="VANCOUVER")

boxplot(a$Annual)









#time series
b<-MeanTemp %>%
  filter(`InfoTemp[2]`=="VANCOUVER")
b=b%>%filter(Year!=1896)
names(b)
b=b%>%select(-Annual,-Winter,-Spring,-Summer,-Autumn,-`InfoTemp[1]`, -`InfoTemp[2]`, -`InfoTemp[3]`,-Year)
tsss=ts(data=b,start=c(1897,1),end=c(2017,12),frequency = 12)
ts_info(tsss)

Co2North=filter(Co2North,YearDecimal<=input$bins3)
ggplot(Co2North,aes(YearDecimal,Latitude49value))+geom_point()+xlab("Year")+ylab("CO2")+ggtitle("Year vs CO2")+geom_smooth(method = "lm")
slope=lm(Co2North$Latitude49value~Co2North$YearDecimal)
slope$coef[[2]]
#The average Co2 increases 1.772 ppm every year





#__________________________________________________

MaxTemplong2=gather(b,season,Temperature,Winter,Spring,Summer,Autumn)
MaxTemplong2 %>% 
  select(Year,season,Temperature) %>%
  group_by(Year,season) %>%
  summarise(value=mean(Temperature))-> cData5
p2=ggplot(cData5,aes(Year,value,color=season))+geom_line()+ggtitle("Year vs Temperature within different seasons")+geom_smooth(method='loess')
p2


par(mfrow=c(2,2))
plot(b$Year,b$Annual,ylab="average temperature in Vancouver",xlab="year",main="Temperature vs year")
abline(lm(b$Annual~b$Year),col="red",lwd=2)
plot(d$Year,d$Annual,ylab="rainfall total",xlab="year",main="rainfall vs year")
abline(lm(d$Annual~d$Year),col="red",lwd=2)
plot(e$Year,e$Annual,ylab="snow total",xlab="year",main="snowfall vs year")
abline(lm(e$Annual~e$Year),col="red",lwd=2)
plot(Co2North$YearDecimal,Co2North$Latitude49value,xlab="year",ylab="Co2",main="Co2 vs year")
abline(lm(Co2North$Latitude49value~Co2North$YearDecimal),col="red",lwd=2)

plot1=ggplot(b,aes(x=Year,y=Annual))+geom_point()+geom_line()+geom_smooth(method="lm")+ylab("year")+xlab("average temperature in Vancouver")+ggtitle("Temperature vs year")
plot2=ggplot(d,aes(x=Year,y=Annual))+geom_point()+geom_line()+geom_smooth(method="lm")+ylab("year")+xlab("average rainfall in Vancouver")+ggtitle("Rainfall vs year")
plot3=ggplot(e,aes(x=Year,y=Annual))+geom_point()+geom_line()+geom_smooth(method="lm")+ylab("year")+xlab("average snow in Vancouver")+ggtitle("Snow vs year")
plot4=ggplot(Co2North,aes(x=YearDecimal,y=Latitude49value))+geom_point()+geom_line()+geom_smooth(method="lm")+ylab("year")+xlab("Co2")+ggtitle("Co2 vs year")
multiplot(plot1, plot2, plot3, plot4, cols = 2)


tabPanel("Findings",h5("From this plot, I find that in all four seasons, the average temperature is gradually increasing year after year. Each set of data is a representative to one season, and each trend line is upward sloping. Therefore, our climate is becoming warmer and warmer."),
         plotOutput("distplot2"),h5("In the plot of 'Co2 vs year', Co2 is increasing dramatically over time with an estimated slope of 1.772 which means that the average Co2 increases 1.772 ppm every year. This might be one of the primary reason that causes global warming. From the plot, we can see that Co2 is fluctuating in in a regular pattern. It seems like in some certain months, the Co2 decreases and increases in some other months. However, the overall pattern is increasing."),
         plotOutput("Co2"),h5("From the plots below, it seems like the temperature has positive linear relationships with rainfall, snowfall, and Co2. Therefore, we can expect that the climate will become hotter, more snow, more rain, and more Co2 in the air in the future."),plotOutput("temp")),
tabPanel("About",source("about.R")$value()),
