# Import Data
library(readxl) #Function to read the excel 
library(ggplot2) #Data visualization 
births <- read_excel("PATH HERE/Book1.xlsx") #Read in the data

library(forecast) #library for time-series forecasting
ts<-as.ts(births$Births) #Transform births as time-series
pop<-as.ts(births$Population) #Transform population as time-series
sm<-ma(ts,order=5) #Calculate the moving averages 
plot(ts) #plot the original time-series
lines(sm,col="red") #plot the projected moving average

births$forecasted_births_ma<-sm #send the moving average to data

ts1<-auto.arima(ts) #Simple ARIMA Model
births$forecasted_arima<-as.ts(ts1$fitted) #Move the estimates value to the dataset
ts2<-auto.arima(ts, xreg = pop) #ARIMA with controls for population 
births$forecasted_arima_pop<-as.ts(ts2$fitted) #Move estimates to the dataset

plot(ts) #Plot the time serie, births 
lines(births$forecasted_births,col="red") #red line for moving average
lines(births$forecasted_arima,col="blue") #blue line for simple ARIMA
lines(births$forecasted_arima,col="red",type="p") #red points for ARIMA with controls

#Here I create the date variable
births$Month_Cont2<-seq(as.Date("2000/1/1"), by = "month", length.out = 228) 

#Plot of births (observed values)
p1<-ggplot(data=births, aes(x=Month_Cont2, y=as.ts(Births))) +
  geom_line(aes(x=Month_Cont2, y=as.ts(Births)))+
  theme_classic() +
  labs(title="Births by Month in Puerto Rico", subtitle="September 2017 and 2018 (red lines)",
       x = "Month", y = "Births per month")+
  scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  geom_vline(xintercept = as.numeric(as.Date("2017-09-01")), linetype=4, color="red",size=1)+
  geom_vline(xintercept = as.numeric(as.Date("2018-09-01")), linetype=4, color="red",size=1)+
  theme(legend.position = "none")

#Plot the moving average
p2<-ggplot(data=births, aes(x=Month_Cont2, y=as.ts(Births))) +
  geom_line(aes(x=Month_Cont2, y=forecasted_births,col="orange"))+
  theme_classic() +
  labs(title="Moving Average by Month in Puerto Rico", subtitle="September 2017 and 2018 (red lines)",
       x = "Month", y = "Births per month")+
  scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  geom_vline(xintercept = as.numeric(as.Date("2017-09-01")), linetype=4, color="red",size=1)+
  geom_vline(xintercept = as.numeric(as.Date("2018-09-01")), linetype=4, color="red",size=1)+
  theme(legend.position = "none")

#Plot the ARIMA models
p3<-ggplot(data=births, aes(x=Month_Cont2, y=as.ts(Births))) +
  geom_line(aes(x=Month_Cont2, y=forecasted_arima,col="red"))+
  theme_classic() +
  geom_line(aes(x=Month_Cont2, y=forecasted_arima_pop,col="blue"))+
    labs(title="ARIMA by Month in Puerto Rico", subtitle="September 2017 and 2018 (red lines) and Red model without controls, blue is controlling for population size",
       x = "Month", y = "Births per month")+
  scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  geom_vline(xintercept = as.numeric(as.Date("2017-09-01")), linetype=4, color="red",size=1)+
  geom_vline(xintercept = as.numeric(as.Date("2018-09-01")), linetype=4, color="red",size=1)+
  theme(legend.position = "none")

library(gridExtra) #Library to put the three graphs together
grid.arrange(p1,p2,p3,nrow = 3) #Plot the three images