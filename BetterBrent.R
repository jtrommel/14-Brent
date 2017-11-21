# BetterBrent
library(lubridate)
library(tidyverse)
library(zoo)
library(forecast)
# Inlezen data
reading.date <- ymd("2017-11-16")
Brent <- read.csv("data/RBRTEd-2.csv",
                  header=TRUE, sep=";", 
                  dec=",",skip=3,col.names=c("Day","Price"),
                  na.strings="NA",
                  stringsAsFactors = FALSE)
Brent$Day <- mdy(Brent$Day)
# Beperken tot het jaar 2017
Brent17 <- Brent %>% filter(year(Day)==2017 & Day <= reading.date) %>% arrange(Day)
ggplot(data=Brent17) +
  geom_line(aes(x=Day,y=Price))
# Imputing van de missende gegevens (weekends en feestdagen). Interpolatie door pakket "zoo"
allDates <- data.frame(Day=seq(min(Brent17$Day) , max(Brent17$Day), "day"))
Brent17_ext <- left_join(allDates,Brent17,by="Day")
Brent17_ext$interpol <- na.approx(Brent17_ext$Price)
ggplot(data=Brent17_ext) +
  geom_point(aes(x=Day,y=interpol), color="green") +
  geom_point(aes(x=Day, y=Price),color="red")
# Maken van een tijdsreeks. Bij dagelijkse waarden raadt Rob Hyndman aan om frequency=7 te zetten
Brent17_ts <- ts(Brent17_ext$interpol, start = decimal_date(as.Date("2017-01-03")), frequency = 7)
# Decompose van de time series
decomp_ts <- stl(Brent17_ts,s.window="periodic",robust=TRUE)$time.series
# Reordering the results so that the dates are right and we add the variable Type which can have the values "original
# data", "seasonal", "trend" and "remainder".
decomp_stl <- data.frame(Price=c(Brent17_ext$interpol,as.numeric(decomp_ts)),
                         Date=rep(Brent17_ext[,"Day"],ncol(decomp_ts)+1),
                         Type=factor(rep(c("original data",colnames(decomp_ts)),
                                         each=nrow(decomp_ts)),
                                     levels=c("original data",colnames(decomp_ts))))
# Grafieken van de uitgerafelde tijdsreeks
ggplot(decomp_stl, aes(x=Date,y=Price)) +
  geom_line() +
  facet_grid(Type~., scales="free_y", switch="y")
#
# Pause
#
# Uit deze figuur blijkt dat het seizoenselement nauwelijks belang heeft. De amplitude is 500 keer kleiner dan
# de basiswaarde. De trend is duidelijk. De remainder kunnen we nog onderzoeken met FFT
#
# We kunnen een forecast doen gebaseerd op level en op trend (AAN-type)
end.date <- ymd("2017-12-29")
start.date <- max(Brent17$Day)
days.ahead <- as.numeric(end.date - start.date)
fit.Brent <- ets(Brent17_ext$interpol,model="AAN")
fc.Brent <- forecast(fit.Brent,h=days.ahead)
plot(fc.Brent, main="Forecast model AAN", ylab="Price", xlab="Date", flty=2)
# Een licht trend effect bij de start, maar daarna bijna constant. We kunnen de ets-functie zelf het model laten bepalen
fit.Brent2 <- ets(Brent17_ext$interpol)
fit.Brent2
fc.Brent2 <- forecast(fit.Brent2,h=days.ahead)
plot(fc.Brent2, main="Forecast optimies parameters", ylab="Price", xlab="Date", flty=2)
#
# FFT analyse van de remainder
#
Fs <- 1 # sample frequentie (hier 1 per dag)
Ts <- 1/Fs # sample periode (hier 1 dag)
N <- nrow(Brent17_ext) # aantal samples 
freq.resolution <- Fs/N # frequentie resolutie
print(freq.resolution)
t <- c(0 : (N-1)*Ts) # tijdsverloop
x <- decomp_stl$Price[decomp_stl$Type=="remainder"] # tijdssignaal
signal <- data.frame(t,x)
signal %>% ggplot() + geom_line(aes(t,x))
# Berekening van het double sided en single sided spectrum
z <- fft(x)
P2 <- Mod(z/N)
P1 <- P2[1:((N/2)+1)]
P1[2:(length(P1)-1)] <- 2*P1[2:(length(P1)-1)]
freq <- seq(0, (Fs/2)-(Fs/N), Fs/N)
freqspec <- data.frame(freq=freq,amp=P1[1:(N/2)],fasehoek=0)
ggplot(data=freqspec) + 
  geom_line(aes(freq,amp)) +
  labs(x="f (in 'per dag')",y="amp",title="Frequentiesamenstelling van de remainder")
freqspec$fasehoek <- Arg(z[1:nrow(freqspec)])/pi
grens <- 0.1*max(freqspec$amp)
resultaat <- freqspec %>% filter(amp>grens)
print(resultaat)
#
# Met een grens van 10% van de maximale amplitude zijn er 144 frequenties die in aanmerking komen. Dat lijkt
# op witte ruis.
# Met ARIMA krijgen we iets heel anders
trend_given <- ts(decomp_ts[,"trend"])
trend_fit <- auto.arima(trend_given)
trend_forecast <- forecast(trend_fit, 47)$mean
trend_data <- data.frame(Price=c(decomp_ts[,2],trend_forecast),
                         Date=c(Brent17_ext$Day, seq(ymd("2017-11-12"),ymd("2017-12-28"),"day")),
                         Type=c(rep("Given", nrow(Brent17_ext)),rep("Forecast",47)))
ggplot(trend_data, aes(Date,Price, color=Type)) +
  geom_line(size=1.2) +
  labs(title=paste(trend_fit))

