library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)
library(betareg)
library(car)
library(gapminder)
library(dplyr)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(psych) # for descriptive analysis
library(forecast)
library(lmtest)
library(tseries)
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)
library(zoo)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(psych)
require(MASS) # to access Animals data sets
require(scales) # to access break formatting functions

library(mgcv)
library(GGally)
library(mgcv)
library(visreg)

#ARIMA
setwd('E:\\Dengue\\Aminul Weather')
Dengue <- read.csv("Final_Data.csv")

Dengue

#Descriptive
describe(Dengue$WSA)
describe(Dengue$TA)
describe(Dengue$DA)
describe(Dengue$RHA)
describe(Dengue$RA)
describe(Dengue$PSA)
describe(Dengue$Deng)

deng <- Dengue[4:10]

colnames(deng) <- c('Wind speed', 
                             'Temperature', 
                             'Dew point',
                             'Relative humidity',
                             'Rainfall',
                             'Surface pressure',
                             'Dengue')

ggpairs(deng, 
        columnLabels = gsub('.', ' ', colnames(deng), fixed = T), 
        labeller = label_wrap_gen(10))


DengueTS <- ts(Dengue$Deng, frequency=12, start=c(2000,1))

auto.arima(DengueTS)
Fit<-Arima(DengueTS,order=c(1,1,2))
summary(Fit)
fcast <- forecast(Fit, h=12)

z <- autoplot(fcast)  +
  xlab("Years") + ylab("Number of dengue cases") +ggtitle("ARIMA Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") +
  theme( legend.text = element_text(color = "Black", size = 18),
         text = element_text(size = 18))+
  theme(plot.title = element_text(hjust = 0.5))
z

# rn <- nrow(Dengue)
# #R2
# SSE <- sum((resid(Fit[1:275]))^2)
# SST <- sum((Dengue$Deng[1:275] - mean(Dengue$Deng[1:275]))^2)
# R_square <- 1 - SSE / SST
# R_square



#SARIMA
auto.arima(DengueTS, D = T)

Arima(DengueTS, order=c(0,0,2), seasonal=list(order=c(2,1,0),period=12))

Fit<-Arima(DengueTS, order=c(0,0,2), seasonal=list(order=c(2,1,0),period=12))
summary(Fit)
fcast <- forecast(Fit, h=12)

n <- autoplot(fcast)  +
  xlab("Years") + ylab("Number of dengue cases") +ggtitle("SARIMA Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") +
  theme( legend.text = element_text(color = "Black", size = 18),
         text = element_text(size = 18))+
  theme(plot.title = element_text(hjust = 0.5))
n

# rn <- nrow(Dengue)
# #R2
# SSE <- sum((resid(Fit[1:275]))^2)
# SST <- sum((Dengue$Deng[1:275] - mean(Dengue$Deng[1:275]))^2)
# R_square <- 1 - SSE / SST
# R_square

####SES########

library(tidyverse) 
library(fpp2) 
library(ggfortify)
ses.goog <- ses(DengueTS,  
                h = 12) 
summary(ses.goog)

fcast <- forecast(ses.goog, h=12)

x <- autoplot(ses.goog)  +
  xlab("Years") + ylab("Number of dengue cases") +ggtitle("SES Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") +
  theme( legend.text = element_text(color = "Black", size = 18),
         text = element_text(size = 18))+
  theme(plot.title = element_text(hjust = 0.5))
x

# rn <- nrow(Dengue)
# #R2
# SSE <- sum((resid(fcast[1:275]))^2)
# SST <- sum((Dengue$Deng[1:275] - mean(Dengue$Deng[1:275]))^2)
# R_square <- 1 - SSE / SST
# R_square

#Prophet

history <- data.frame(ds = seq(as.Date('2000-01-01'), as.Date('2023-01-31'), by = 'm'),
                      y = Dengue$Deng)


lower = quantile(history$y, .05)
upper = quantile(history$y, .95)

history <- history %>% mutate(floor = lower, cap = upper)

m3 <- prophet(history, changepoint.prior.scale=0.01,
              growth = 'logistic')
future <- make_future_dataframe(m3, periods = 12) %>% mutate(floor = lower, cap = upper)
fcst3 <- predict(m3, future)
# 
# y <-plot(m3, fcst3, xlab="Years", ylab="Number of dengue cases") + ggtitle("Prophet Model") + theme(
#   plot.title = element_text(size=18),
#   legend.text = element_text(color = "Black", size = 18),
#   text = element_text(size = 18))
# plot(y)

# SSE <- sum((history$y[1:275] - fcst3$yhat[c(1:275)])^2)
# SST <- sum((history$y[1:275] - mean(history$y[1:275]))^2)
# R_square <- 1 - SSE / SST
# R_square
# 
# last_fcst3 <- fcst3[275,]
# rmse <- sqrt(mean((history$y - fcst3$yhat[c(1:275)])^2))
# mae <- mean(abs((history$y - fcst3$yhat[c(1:275)])))
# final <- cbind(last_fcst3, rmse, mae)
# final


# #Menn kendal
# library(Kendall)
# library(trend)
# 
# myts <- ts(Dengue$Deng)
# t.test(Dengue$Deng)$"conf.int"
# mean(Dengue$Deng)
# 
# MannKendall(myts)
# sens.slope(myts, conf.level = 0.95)


###GAM

Dengue$WSA = as.numeric(Dengue$WSA)
Dengue$TA = as.numeric(Dengue$TA)
Dengue$DA = as.numeric(Dengue$DA)
Dengue$RHA = as.numeric(Dengue$RHA)
Dengue$RA = as.numeric(Dengue$RA)
Dengue$PSA = as.numeric(Dengue$PSA)
Dengue$Deng = as.numeric(Dengue$Deng)

Dengue$ds <- history$ds

Dengue$ds = as.numeric(Dengue$ds)
gamint3 = gam(Deng ~  s(ds) + WSA + TA + DA + RHA + RA + PSA, data=Dengue)
summary(gamint3)
coefci(gamint3)

library(gamlr)
AIC(gamint3)
BIC(gamint3)
AICc(gamint3)

visreg(gamint3, "ds", ylab="Number of dengue cases", xlab="Months (in numeric)", main="GA Model", las = 3)

#ARIMAX
library(forecast)
library(lubridate)
library(tseries)
library(fpp)
library(TSA)

options(scipen = 999)
xreg <- cbind(Dengue$WSA, Dengue$TA, Dengue$DA, 
              Dengue$RHA, Dengue$RA, Dengue$PSA)

modArima <- Arima(DengueTS, xreg=xreg, order=c(1,1,2))
summary(modArima)
Forecasted_values<-forecast(modArima,xreg=xreg)
Forecasted_values

library(ggplot2)
fc <- forecast(modArima,xreg=xreg)
yy <- autoplot(fcast)  +
  xlab("Years") + ylab("Number of dengue cases") +ggtitle("ARIMAX Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") +
  theme( legend.text = element_text(color = "Black", size = 18),
         text = element_text(size = 18))+
  theme(plot.title = element_text(hjust = 0.5))


plot(yy)

# SSE <- sum((resid(modArima[1:275]))^2)
# SST <- sum((Dengue$Deng[1:275] - mean(Dengue$Deng[1:275]))^2)
# R_square <- 1 - SSE / SST
# R_square

library(lmtest)
coeftest(modArima)

coefci(modArima)



# #Menn kendal
# library(Kendall)
# library(trend)
# 
# myts <- ts(Dengue$Values)
# t.test(Dengue$Values)$"conf.int"
# mean(Dengue$Values)
# 
# MannKendall(myts)
# sens.slope(myts, conf.level = 0.95)


library(gridExtra)
tiff("plot.tiff", units="in", width=24, height=12, res=300)
grid.arrange(x, z, n, yy,  ncol=2)
dev.off()



library(glmmTMB)
library(DHARMa)
library(performance)


library(dplyr)
Dengue$Dengnew <- log10(Dengue$Deng+1)

fit <- glmmTMB(Dengnew  ~ WSA + TA + DA + RHA + RA + PSA + (1|Season), na.action=na.omit, family = "poisson", data = Dengue)

library(car)
summary(fit)
round(exp(confint(fit)),3)
options(scipen = 999)
performance::performance(fit)


library(MASS)
require(foreign)
require(ggplot2)
require(maptools)



Denguewinter <- Dengue[ which(Dengue$Season=='winter'), ]

#GHSI

model.3nb <- glm(Deng  ~ WSA + TA  + RHA + RA , family = "poisson", data = Denguewinter)
summary(model.3nb)

round(exp(model.3nb$coefficients),2)
round(exp(confint(model.3nb)),2)





Denguesummer <- Dengue[ which(Dengue$Season=='summer'), ]

#GHSI

model.3nb <- glm(Deng ~ WSA + TA + DA + RHA + RA + PSA, family = "poisson", data = Denguesummer)
summary(model.3nb)

round(exp(model.3nb$coefficients),2)
round(exp(confint(model.3nb)),2)

Denguemonsoon <- Dengue[ which(Dengue$Season=='monsoon'), ]

#GHSI

model.3nb <- glm(Deng  ~ WSA + TA +  RHA + RA + PSA, family = "poisson", data = Denguemonsoon)
summary(model.3nb)

round(exp(model.3nb$coefficients),2)
round(exp(confint(model.3nb)),2)

