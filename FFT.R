# Fast Fourier Transform
library(tidyverse)
Fs <- 1000 # sample frequentie (hier 1 kHz)
Ts <- 1/Fs # sample periode (hier 1 msec)
N <- 1000 # aantal samples 
freq.resolution <- Fs/N # frequentie resolutie
print(freq.resolution)
t <- c(0 : (N-1)*Ts) # tijdsverloop
F0 <- 50 # frequentie van het tijdssignaal
x <- sin(2*pi*F0*t) + 0.4*cos(2*pi*3*F0*t + pi/3) - 0.2*sin(2*pi*8*F0*t - pi/4) # tijdssignaal
signal <- data.frame(t,x)
signal %>% filter(t<=0.1) %>% ggplot() + geom_line(aes(t,x))
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