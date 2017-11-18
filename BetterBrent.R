# BetterBrent
library(lubridate)
library(tidyverse)
library(zoo)
library(forecast)
reading.date <- ymd("2017-11-16")
Brent <- read.csv("data/RBRTEd-2.csv",
                  header=TRUE, sep=";", 
                  dec=",",skip=3,col.names=c("Day","Price"),
                  na.strings="NA",
                  stringsAsFactors = FALSE)
Brent$Day <- mdy(Brent$Day)
Brent17 <- Brent %>% filter(year(Day)==2017 & Day <= reading.date) %>% arrange(Day)
ggplot(data=Brent17) +
  geom_line(aes(x=Day,y=Price))
allDates <- data.frame(Day=seq(min(Brent17$Day) , max(Brent17$Day), "day"))
Brent17_ext <- left_join(allDates,Brent17,by="Day")
Brent17_ext$interpol <- na.approx(Brent17_ext$Price)
ggplot(data=Brent17_ext) +
  geom_point(aes(x=Day,y=interpol), color="green") +
  geom_point(aes(x=Day, y=Price),color="red")
Brent17_ts <- ts(Brent17_ext$interpol, start = decimal_date(as.Date("2017-01-03")), frequency = 7)
Brent_hw <- HoltWinters(Brent17_ts, gamma=TRUE)
end.date <- ymd("2017-12-28")
start.date <- max(Brent17$Day)
days.ahead <- as.numeric(end.date - start.date)
print(days.ahead)
forecast <- predict(Brent_hw, n.ahead = days.ahead, prediction.interval = T, level = 0.95)
forecast.df <- as.data.frame(forecast)
forecast.df$time <- seq(start.date+days(1),end.date,"day")
p1 <- ggplot(data=Brent17_ext) + geom_line(aes(x=Day,y=interpol))
p1 <- p1 + geom_line(data=forecast.df, aes(x=time,y=upr), color="red")
p1 <- p1 + geom_line(data=forecast.df, aes(x=time,y=fit), color="green")
p1 <- p1 + geom_line(data=forecast.df, aes(x=time,y=lwr), color="blue")
p1
decomp_ts <- stl(Brent17_ts,s.window="periodic",robust=TRUE)$time.series
decomp_stl <- data.frame(Price=c(Brent17_ext$interpol,as.numeric(decomp_ts)),
                         Date=rep(Brent17_ext[,"Day"],ncol(decomp_ts)+1),
                         Type=factor(rep(c("original data",colnames(decomp_ts)),
                                          each=nrow(decomp_ts)),
                                     levels=c("original data",colnames(decomp_ts))))
ggplot(decomp_stl, aes(x=Date,y=Price)) +
  geom_line() +
  facet_grid(Type~., scales="free_y", switch="y")
trend_given <- ts(decomp_ts[,"trend"])
trend_fit <- auto.arima(trend_given)
trend_forecast <- forecast(trend_fit, 47)$mean
trend_data <- data.frame(Price=c(decomp_ts[,2],trend_forecast),
                         Date=c(Brent17_ext$Day, seq(ymd("2017-11-12"),ymd("2017-12-28"),"day")),
                         Type=c(rep("Given", nrow(Brent17_ext)),rep("Forecast",47)))
ggplot(trend_data, aes(Date,Price, color=Type)) +
  geom_line(size=1.2) +
  labs(title=paste(trend_fit))
