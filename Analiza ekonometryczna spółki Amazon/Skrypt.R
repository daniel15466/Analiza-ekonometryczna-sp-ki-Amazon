install.packages("forecast")
install.packages("fGarch")

library(aTSA)
library(forecast)
library(lmtest)
library(tseries)
library(fGarch)

ama_d <- read.csv(file = "Amazon daily.csv",sep = ";", header = TRUE, dec = ",")
ama_d$logstopy <- c(0)
ama_d$logstopy[1] = 1.2e-3

for (i in 2:1048)
{
  ama_d$logstopy[i] <- log(ama_d$Zamkniecie[i]/ama_d$Zamkniecie[i-1])
}

#stacjonarnosc
adf.test(ama_d$logstopy)


#Budowa modelu ARMA/ARIMA
modelarima <- auto.arima(ama_d$logstopy) # ARIMA(0,0,1)

#test reszt modelu na autokorelacje
acf(modelarima$residuals)

#model ARIMA z najmiejszym aic to model ARIMA(0,0,1) - aic = -5198,657
arima1 <- arima(ama_d$logstopy, order = c(0,0,1))

#test na wystepowanie efektu arch
arch.test(arima1)
plot(arima1$residuals)

#tworzenie modelu GARCH
garchmodel <- garch(arima1$residuals, order = c(1, 1))

#test normalnosci shapiro na resztach z modelu GARCH
shapiro.test(garchmodel$residuals)

#t-test
t.test(garchmodel$residuals, conf.level = 0.95)

#estymacja modeli garch o podanych rozkladach 
#Warunkowy rozklad normalny
garch_1 <- garchFit(ama_d$logstopy, formula=~garch(1,1), cond.dist = "norm")
garch_1@residuals
sd <- sd(garch_1@residuals)
std_residuals <- garch_1@residuals/sd

shapiro.test(fGarch::residuals(garch_1))
acf(residuals(garch_1))
Box.test(residuals(garch_1), lag = 1)
Box.test(residuals(garch_1), lag = 2)
Box.test(residuals(garch_1), lag = 3)
Box.test(residuals(garch_1), lag = 4)
Box.test(residuals(garch_1), lag = 5)
Box.test(residuals(garch_1), lag = 6)
Box.test(residuals(garch_1), lag = 7)
Box.test(residuals(garch_1), lag = 8)
Box.test(residuals(garch_1), lag = 9)
Box.test(residuals(garch_1), lag = 10)


#Warunkowy rozklad studenta
garch_2 <- garchFit(ama_d$logstopy, formula=~garch(1,1), cond.dist = "sstd")
garch_2@residuals
sd <- sd(garch_2@residuals)
std_residuals <- garch_2@residuals/sd

shapiro.test(fGarch::residuals(garch_2))
acf(residuals(garch_2))
Box.test(residuals(garch_1), lag = 1)
Box.test(residuals(garch_1), lag = 2)
Box.test(residuals(garch_1), lag = 3)
Box.test(residuals(garch_1), lag = 4)
Box.test(residuals(garch_1), lag = 5)
Box.test(residuals(garch_1), lag = 6)
Box.test(residuals(garch_1), lag = 7)
Box.test(residuals(garch_1), lag = 8)
Box.test(residuals(garch_1), lag = 9)
Box.test(residuals(garch_1), lag = 10)


#autokorelacja reszt modeli ARIMA-GARCH
acf(garchmodel$residuals[2:1048])


#prognozy
arimaforecast <- aTSA::forecast(arima1, lead = 5)

logrates <- c(ama_d$logstopy[1048],arimaforecast[6:10])
logratesupper95 <- c(0,arimaforecast[21:25])
lograteslower95 <- c(0,arimaforecast[16:20])
logprices <- c(log(ama_d$Zamkniecie[1048]),0,0,0,0,0)
logpricessupper95 <- c(0,0,0,0,0,0)
logpriceslower95 <- c(0,0,0,0,0,0)
prices <- c(ama_d$Zamkniecie[1048],0,0,0,0,0)
pricesupper95 <- c(0,0,0,0,0,0)
priceslower95 <- c(0,0,0,0,0,0)

forecasts <- data.frame(logrates,logratesupper95,lograteslower95, logprices,logpricessupper95,logpriceslower95, prices,pricesupper95,priceslower95)


for (j in 2:6)
  {
    forecasts$prices[j] <- exp(forecasts$logrates[j])*forecasts$prices[j-1]
    forecasts$pricesupper95[j] <- exp(forecasts$logratesupper95[j])*forecasts$prices[j-1]
    forecasts$priceslower95[j] <- exp(forecasts$lograteslower95[j])*forecasts$prices[j-1]
    forecasts$logprices[j] <- log(forecasts$prices[j])
    forecasts$logpricessupper95[j] <- log(forecasts$pricesupper95[j])
    forecasts$logpriceslower95[j] <- log(forecasts$priceslower95[j])
  }




