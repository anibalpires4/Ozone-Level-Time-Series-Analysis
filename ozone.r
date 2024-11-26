
#### Libraries ####

library(ggplot2) 
library(ggpubr)
library(latex2exp)
library(imputeTS)
library(scales)
library(forecast)
library(tseries)
library(openxlsx)
library(lmtest)
library(hydrostats)
library(PerformanceAnalytics)

#### Import Dataset ####

alfragide_amadora <- read.xlsx("2021 Alfragide-Amadora.xlsx")
names(alfragide_amadora) <- c('time','ozono') 

reboleira <- read.xlsx("2021 Reboleira.xlsx")
names(reboleira) <- c('time','ozono') 

beato <- read.xlsx("2021 Beato.xlsx")
names(beato) <- c('time','ozono') 

olivais <- read.xlsx("2021 Olivais.xlsx")
names(olivais) <- c('time','ozono') 

entrecampos <- read.xlsx("2021 Entrecampos.xlsx")
names(entrecampos) <- c('time','ozono') 

# Time series with missing values 
ggplot_na_distribution(alfragide_amadora$ozono, title = "Distribution of Missing Values - Alfragide")
ggplot_na_distribution(reboleira$ozono, title = "Distribution of Missing Values - Reboleira")
ggplot_na_distribution(beato$ozono, title = "Distribution of Missing Values - Beato")
ggplot_na_distribution(olivais$ozono, title = "Distribution of Missing Values - Olivais")
ggplot_na_distribution(entrecampos$ozono, title = "Distribution of Missing Values - Entrecampos")

# Checking for missing values
sum(is.na(alfragide_amadora))
sum(is.na(reboleira))
sum(is.na(beato))
sum(is.na(olivais))
sum(is.na(entrecampos))

### Imputation ###

# Imputation using interpolation
alfragide_amadora <- na_interpolation(alfragide_amadora, option = "linear")
reboleira <- na_interpolation(reboleira, option = "linear")
beato <- na_interpolation(beato, option = "linear")
olivais <- na_interpolation(olivais, option = "linear")
entrecampos <- na_seadec(entrecampos, algorithm = "interpolation", find_frequency = TRUE)

sum(is.na(alfragide_amadora))
sum(is.na(reboleira))
sum(is.na(beato))
sum(is.na(olivais))
sum(is.na(entrecampos))

#### Initial examination of the data ####

# Histograms
hist(alfragide_amadora$ozono, main = "Alfragide", col='skyblue3', xlab = "µg/m³", ylab="Frequency")
hist(reboleira$ozono, main = "Reboleira", col='skyblue3', xlab = "µg/m³", ylab="Frequency")
hist(beato$ozono, main = "Beato", col='skyblue3', xlab = "µg/m³", ylab="Frequency")
hist(olivais$ozono, main = "Olivais", col='skyblue3', xlab = "µg/m³", ylab="Frequency")
hist(entrecampos$ozono, main = "Entrecampos", col='skyblue3', xlab = "µg/m³", ylab="Frequency")

# Summary
summary(alfragide_amadora)
sd(alfragide_amadora$ozono)
summary(reboleira)
sd(reboleira$ozono)
summary(beato)
sd(beato$ozono)
summary(olivais)
sd(olivais$ozono)
summary(entrecampos)
sd(entrecampos$ozono)

# Extracting time frames from the data
time <- seq.POSIXt(from = as.POSIXct("2021-01-01 00:00:00",format="%Y-%m-%d %H",tz="Europe/Lisbon"), length.out = 8760, by = "60 mins")
Year <- as.numeric(format(time, '%Y'));
Month <- as.numeric(format(time, '%m'));
Day <- as.numeric(format(time, '%d'));
Weekday <- as.numeric(format(time, '%u'));
Hour <- as.numeric(format(time, '%H'));

# Boxplots for each region
ozono_1 <- as.numeric(alfragide_amadora$ozono)
data_hour_extended_1 <- as.data.frame(cbind(Year, Month, Day, Weekday, Hour, ozono_1))
par(mfrow=c(2,2),mai = c(0.7, 0.7, 0.1, 0.1))
boxplot(ozono_1~Year,data=data_hour_extended_1,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
boxplot(ozono_1~Month,data=data_hour_extended_1,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
boxplot(ozono_1~Weekday,data=data_hour_extended_1,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
boxplot(ozono_1~Day,data=data_hour_extended_1,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
dev.off()

ozono_2 <- as.numeric(reboleira$ozono)
data_hour_extended_2 <- as.data.frame(cbind(Year, Month, Day, Weekday, Hour, ozono_2))
par(mfrow=c(2,2),mai = c(0.7, 0.7, 0.1, 0.1))
boxplot(ozono_2~Year,data=data_hour_extended_2,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
boxplot(ozono_2~Month,data=data_hour_extended_2,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
boxplot(ozono_2~Weekday,data=data_hour_extended_2,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
boxplot(ozono_2~Day,data=data_hour_extended_2,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
dev.off()

ozono_3 <- as.numeric(beato$ozono)
data_hour_extended_3 <- as.data.frame(cbind(Year, Month, Day, Weekday, Hour, ozono_3))
par(mfrow=c(2,2),mai = c(0.7, 0.7, 0.1, 0.1))
boxplot(ozono_3~Year,data=data_hour_extended_3,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
boxplot(ozono_3~Month,data=data_hour_extended_3,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
boxplot(ozono_3~Weekday,data=data_hour_extended_3,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
boxplot(ozono_3~Day,data=data_hour_extended_3,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
dev.off()

ozono_4 <- as.numeric(olivais$ozono)
data_hour_extended_4 <- as.data.frame(cbind(Year, Month, Day, Weekday, Hour, ozono_4))
par(mfrow=c(2,2),mai = c(0.7, 0.7, 0.1, 0.1))
boxplot(ozono_4~Year,data=data_hour_extended_4,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
boxplot(ozono_4~Month,data=data_hour_extended_4,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
boxplot(ozono_4~Weekday,data=data_hour_extended_4,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
boxplot(ozono_4~Day,data=data_hour_extended_4,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
dev.off()

ozono_5 <- as.numeric(entrecampos$ozono)
data_hour_extended_5 <- as.data.frame(cbind(Year, Month, Day, Weekday, Hour, ozono_5))
par(mfrow=c(2,2),mai = c(0.7, 0.7, 0.1, 0.1))
boxplot(ozono_5~Year,data=data_hour_extended_5,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
boxplot(ozono_5~Month,data=data_hour_extended_5,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
boxplot(ozono_5~Weekday,data=data_hour_extended_5,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
boxplot(ozono_5~Day,data=data_hour_extended_5,col="lightblue",ylab=(TeX('Ozone ($\\mu g /m^3 $)')))
dev.off()

# Time series
alfragide_TS = ts(alfragide_amadora$ozono, start=c(1, 1), end=c(365, 24), frequency=24)
plot(alfragide_TS, main = "Alfragide", xlab = "Day", mgp=c(1.5,0.5,0), ylab = ~ "Ozone values " (mu / m^3)) 

reboleira_TS = ts(reboleira$ozono, start=c(1, 1), end=c(365, 24), frequency=24)
plot(reboleira_TS, main = "Reboleira", xlab = "Day", mgp=c(1.5,0.5,0), ylab = ~ "Ozone values " (mu / m^3)) 

beato_TS = ts(beato$ozono, start=c(1, 1), end=c(365, 24), frequency=24)
plot(beato_TS, main = "Beato", xlab = "Day", mgp=c(1.5,0.5,0), ylab = ~ "Ozone values " (mu / m^3)) 

olivais_TS = ts(olivais$ozono, start=c(1, 1), end=c(365, 24), frequency=24)
plot(olivais_TS, main = "Olivais", xlab = "Day", mgp=c(1.5,0.5,0), ylab = ~ "Ozone values " (mu / m^3)) 

entrecampos_TS = ts(entrecampos$ozono, start=c(1, 1), end=c(365, 24), frequency=24)
plot(entrecampos_TS, main = "Entrecampos", xlab = "Day", mgp=c(1.5,0.5,0), ylab = ~ "Ozone values " (mu / m^3)) 

#################
### Alfragide ###
#################

## Model fitting

# BoxCox transformation
alfragide_TSc <- alfragide_TS + 0.0001
alfragide_lambda = BoxCox.lambda(alfragide_TSc) # 1.067555
alfragide_boxcox = BoxCox(alfragide_TSc, lambda = alfragide_lambda)
plot(alfragide_boxcox, main = "Alfragide", xlab = "Day", ylab = ~ "Ozone values " (mu / m^3))
ggtsdisplay(alfragide_boxcox)

# QQ-plot
par(mfrow=c(1,2))
qqnorm(alfragide_TS, main = "Alfragide Time Series")
qqline(alfragide_TS, col = 'blue')
qqnorm(alfragide_boxcox, main = "Alfragide Box-Cox")
qqline(alfragide_boxcox,col = 'blue')

# Seasonal Diff (0,0,0)(0,1,0)[24]
alfragige_boxcox_seasonal=diff(alfragide_boxcox,lag = 24)
plot(alfragige_boxcox_seasonal)
ggtsdisplay(alfragige_boxcox_seasonal)

# Auto ARIMA
alfragide_model <- auto.arima(alfragide_TS, d = 0, max.p = 2, max.q = 2, max.P = 2, max.Q = 2, max.D = 1, seasonal = TRUE, lambda = alfragide_lambda)
alfragide_model # AIC=64869.61   AICc=64869.63   BIC=64919.16
autoarima_alfragide <- Arima(alfragide_TSc, order = c(1,0,2), seasonal = c(2,0,0), lambda = alfragide_lambda)
checkresiduals(autoarima_alfragide)
ggtsdisplay(autoarima_alfragide$residuals)

## Models ##

# Without seasonal diff
alfragide_model0 <- Arima(alfragide_TSc, order = c(0,0,0), seasonal = c(1,0,0), lambda = alfragide_lambda)
ggtsdisplay(alfragide_model0$residuals)
summary(alfragide_model0) # AIC=80422.4   AICc=80422.4   BIC=80443.63

alfragide_model1 <- Arima(alfragide_TSc, order = c(1,0,1), seasonal = c(1,0,0), lambda = alfragide_lambda)
ggtsdisplay(alfragide_model1$residuals)
summary(alfragide_model1) # AIC=65054.54   AICc=65054.55   BIC=65089.93

alfragide_model2 <- Arima(alfragide_TSc, order = c(1,0,2), seasonal = c(1,0,0), lambda = alfragide_lambda)
ggtsdisplay(alfragide_model2$residuals)
summary(alfragide_model2) # AIC=65033.81   AICc=65033.82   BIC=65076.28

alfragide_model3 <- Arima(alfragide_TSc, order = c(2,0,2), seasonal = c(1,0,0), lambda = alfragide_lambda)
ggtsdisplay(alfragide_model3$residuals)
summary(alfragide_model3) # AIC=65028.86   AICc=65028.87   BIC=65078.4

alfragide_model4 <- Arima(alfragide_TSc, order = c(1,0,1), seasonal = c(1,0,1), lambda = alfragide_lambda)
ggtsdisplay(alfragide_model4$residuals)
summary(alfragide_model4) # AIC=64132.96   AICc=64132.97   BIC=64175.43

alfragide_model5 <- Arima(alfragide_TSc, order = c(1,0,2), seasonal = c(1,0,1), lambda = alfragide_lambda)
ggtsdisplay(alfragide_model5$residuals)
summary(alfragide_model5) # AIC=64134.39   AICc=64134.4   BIC=64183.93

alfragide_model6 <- Arima(alfragide_TSc, order = c(2,0,2), seasonal = c(1,0,1), lambda = alfragide_lambda)
ggtsdisplay(alfragide_model6$residuals)
summary(alfragide_model6) # AIC=64136.04   AICc=64136.05   BIC=64192.66

# With seasonal diff
alfragide_model00 <- Arima(alfragide_TSc, order = c(1,0,1), seasonal = c(0,1,0), lambda = alfragide_lambda)
ggtsdisplay(alfragide_model00$residuals)
summary(alfragide_model00) # AIC=68860.51   AICc=68860.51   BIC=68881.73

alfragide_model01 <- Arima(alfragide_TSc, order = c(1,0,1), seasonal = c(0,1,1), lambda = alfragide_lambda)
ggtsdisplay(alfragide_model01$residuals)
summary(alfragide_model01) # AIC=64015.25   AICc=64015.26   BIC=64043.55

alfragide_model02 <- Arima(alfragide_TSc, order = c(1,0,2), seasonal = c(0,1,1), lambda = alfragide_lambda)
ggtsdisplay(alfragide_model02$residuals)
summary(alfragide_model02) # AIC=64016.29   AICc=64016.3   BIC=64051.67

alfragide_model03 <- Arima(alfragide_TSc, order = c(2,0,2), seasonal = c(0,1,1), lambda = alfragide_lambda)
ggtsdisplay(alfragide_model03$residuals)
summary(alfragide_model03) # AIC=64017.85   AICc=64017.86   BIC=64060.3

alfragide_model04 <- Arima(alfragide_TSc, order = c(1,0,1), seasonal = c(1,1,1), lambda = alfragide_lambda)
ggtsdisplay(alfragide_model04$residuals)
summary(alfragide_model04) # AIC=63971.15   AICc=63971.15   BIC=64006.52

alfragide_model05 <- Arima(alfragide_TSc, order = c(1,0,2), seasonal = c(1,1,1), lambda = alfragide_lambda)
ggtsdisplay(alfragide_model05$residuals)
summary(alfragide_model05) # AIC=63972.06   AICc=63972.07   BIC=64014.51

alfragide_model06 <- Arima(alfragide_TSc, order = c(2,0,2), seasonal = c(1,1,1), lambda = alfragide_lambda)
ggtsdisplay(alfragide_model06$residuals)
summary(alfragide_model06) # AIC=63973.17   AICc=63973.18   BIC=64022.7

# Best model: (1,0,1)(1,1,1)[24] 
alfragide_bestmodel = alfragide_model04
coeftest(alfragide_bestmodel)
checkresiduals(alfragide_bestmodel, lag = 10)
ggtsdisplay(alfragide_bestmodel$residuals)
summary(alfragide_bestmodel) # AIC=63971.15   AICc=63971.15   BIC=64006.52

# Original time series vs Chosen model
autoplot(alfragide_TS)+autolayer(alfragide_bestmodel$fitted, series = "Fit")

#################
### Reboleira ###
#################

## Model fitting

# BoxCox transformation
reboleira_TSc <- reboleira_TS + 0.0001
reboleira_lambda = BoxCox.lambda(reboleira_TSc) # 1.098486
reboleira_boxcox = BoxCox(reboleira_TSc, lambda = reboleira_lambda)
plot(reboleira_boxcox, main = "Reboleira", xlab = "Day", ylab = ~ "Ozone values " (mu / m^3))
ggtsdisplay(reboleira_boxcox)

# QQ-plot
par(mfrow=c(1,2))
qqnorm(reboleira_TS)
qqline(reboleira_TS, col = 'blue', main = "Reboleira Time Series")
qqnorm(reboleira_boxcox)
qqline(reboleira_boxcox,col = 'blue', main = "Reboleira Box-Cox")

# Seasonal Diff (0,0,0)(0,1,0)[24]
reboleira_boxcox_seasonal=diff(reboleira_boxcox,lag = 24)
plot(reboleira_boxcox_seasonal)
ggtsdisplay(reboleira_boxcox_seasonal)

# Auto ARIMA
reboleira_model <- auto.arima(reboleira_TSc, d = 0, max.p = 2, max.q = 2, max.P = 2, max.Q = 2, max.D = 1, seasonal = TRUE, lambda = reboleira_lambda)
reboleira_model # AIC=64468.92   AICc=64468.93   BIC=64525.54
autoarima_reboleira <- Arima(reboleira_TSc, order = c(2,0,2), seasonal = c(2,0,0), lambda = reboleira_lambda)
checkresiduals(autoarima_reboleira)
ggtsdisplay(autoarima_reboleira$residuals)

## Model ##

# Without seasonal diff
reboleira_model0 <- Arima(reboleira_TSc, order = c(0,0,0), seasonal = c(1,0,0), lambda = reboleira_lambda)
ggtsdisplay(reboleira_model0$residuals)
summary(reboleira_model0) # AIC=82305.29   AICc=82305.29   BIC=82326.52

reboleira_model1 <- Arima(reboleira_TSc, order = c(1,0,1), seasonal = c(1,0,0), lambda = reboleira_lambda)
ggtsdisplay(reboleira_model1$residuals)
summary(reboleira_model1) # AIC=64657.79   AICc=64657.79   BIC=64693.18

reboleira_model2 <- Arima(reboleira_TSc, order = c(1,0,2), seasonal = c(1,0,0), lambda = reboleira_lambda)
ggtsdisplay(reboleira_model2$residuals)
summary(reboleira_model2) # AIC=64644.37   AICc=64644.38   BIC=64686.83

reboleira_model3 <- Arima(reboleira_TSc, order = c(2,0,2), seasonal = c(1,0,0), lambda = reboleira_lambda)
ggtsdisplay(reboleira_model3$residuals)
summary(reboleira_model3) # AIC=64638.48   AICc=64638.49   BIC=64688.02

reboleira_model4 <- Arima(reboleira_TSc, order = c(1,0,1), seasonal = c(1,0,1), lambda = reboleira_lambda)
ggtsdisplay(reboleira_model4$residuals)
summary(reboleira_model4) # AIC=63704.62   AICc=63704.63   BIC=63747.09

reboleira_model5 <- Arima(reboleira_TSc, order = c(1,0,2), seasonal = c(1,0,1), lambda = reboleira_lambda)
ggtsdisplay(reboleira_model5$residuals)
summary(reboleira_model5) # AIC=63704.55   AICc=63704.56   BIC=63754.09

reboleira_model6 <- Arima(reboleira_TSc, order = c(2,0,2), seasonal = c(1,0,1), lambda = reboleira_lambda)
ggtsdisplay(reboleira_model6$residuals)
summary(reboleira_model6) # AIC=63670.79   AICc=63670.8   BIC=63727.41

# With seasonal diff
reboleira_model00 <- Arima(reboleira_TSc, order = c(1,0,1), seasonal = c(0,1,0), lambda = reboleira_lambda)
ggtsdisplay(reboleira_model00$residuals)
summary(reboleira_model00) # AIC=68526.71   AICc=68526.71   BIC=68547.93

reboleira_model01 <- Arima(reboleira_TSc, order = c(1,0,1), seasonal = c(0,1,1), lambda = reboleira_lambda)
ggtsdisplay(reboleira_model01$residuals)
summary(reboleira_model01) # AIC=63586.01   AICc=63586.01   BIC=63614.31

reboleira_model02 <- Arima(reboleira_TSc, order = c(1,0,2), seasonal = c(0,1,1), lambda = reboleira_lambda)
ggtsdisplay(reboleira_model02$residuals)
summary(reboleira_model02) # AIC=63585.18   AICc=63585.19   BIC=63620.56

reboleira_model03 <- Arima(reboleira_TSc, order = c(2,0,2), seasonal = c(0,1,1), lambda = reboleira_lambda)
ggtsdisplay(reboleira_model03$residuals)
summary(reboleira_model03) # AIC=63587.54   AICc=63587.55   BIC=63629.99

reboleira_model04 <- Arima(reboleira_TSc, order = c(1,0,1), seasonal = c(1,1,1), lambda = reboleira_lambda)
ggtsdisplay(reboleira_model04$residuals)
summary(reboleira_model04) # AIC=63550.71   AICc=63550.72   BIC=63586.09

reboleira_model05 <- Arima(reboleira_TSc, order = c(1,0,2), seasonal = c(1,1,1), lambda = reboleira_lambda)
ggtsdisplay(reboleira_model05$residuals)
summary(reboleira_model05) # AIC=63549.67   AICc=63549.68   BIC=63592.12

reboleira_model06 <- Arima(reboleira_TSc, order = c(2,0,2), seasonal = c(1,1,1), lambda = reboleira_lambda)
ggtsdisplay(reboleira_model06$residuals)
summary(reboleira_model06) # AIC=63552.47   AICc=63552.48   BIC=63602

# Best model: (1,0,2)(1,1,1)[24] 
reboleira_bestmodel = reboleira_model05
coeftest(reboleira_bestmodel)
checkresiduals(reboleira_bestmodel, lag = 10)
ggtsdisplay(reboleira_bestmodel$residuals)
summary(reboleira_bestmodel) # AIC=63549.67   AICc=63549.68   BIC=63592.12

# Original time series vs Chosen model
autoplot(reboleira_TS)+autolayer(reboleira_bestmodel$fitted, series = "Fit")

#############
### Beato ###
#############

## Model fitting

# BoxCox transformation
beato_TSc <- beato_TS + 0.0001
beato_lambda = BoxCox.lambda(beato_TSc) # 1.187847
beato_boxcox = BoxCox(beato_TS, lambda = beato_lambda)
plot(beato_boxcox, main = "Beato", xlab = "Day", ylab = ~ "Ozone values " (mu / m^3))
ggtsdisplay(beato_boxcox)

# QQ-plot
par(mfrow=c(1,2))
qqnorm(beato_TS)
qqline(beato_TS, col = 'blue', main = "Beato Time Series")
qqnorm(beato_boxcox)
qqline(beato_boxcox,col = 'blue', main = "Beato Box-Cox")

# Seasonal Diff (0,0,0)(0,1,0)[24]
beato_boxcox_seasonal=diff(beato_boxcox,lag = 24)
plot(beato_boxcox_seasonal)
ggtsdisplay(beato_boxcox_seasonal)

# Auto ARIMA
beato_model <- auto.arima(beato_TSc, d = 0, max.p = 2, max.q = 2, max.P = 2, max.Q = 2, max.D = 1, seasonal = TRUE, lambda = beato_lambda)
beato_model # AIC=71022.83   AICc=71022.85   BIC=71072.38
autoarima_beato <- Arima(beato_TSc, order = c(2,0,2), seasonal = c(2,0,0), lambda = beato_lambda)
checkresiduals(autoarima_beato)
ggtsdisplay(autoarima_beato$residuals)

## Model ##

# Without seasonal diff
beato_model0 <- Arima(beato_TSc, order = c(0,0,0), seasonal = c(1,0,0), lambda = beato_lambda)
ggtsdisplay(beato_model0$residuals)
summary(beato_model0) # AIC=87959.33   AICc=87959.34   BIC=87980.57

beato_model1 <- Arima(beato_TSc, order = c(1,0,1), seasonal = c(1,0,0), lambda = beato_lambda)
ggtsdisplay(beato_model1$residuals)
summary(beato_model1) # AIC=71048.47   AICc=71048.47   BIC=71083.86

beato_model2 <- Arima(beato_TSc, order = c(1,0,2), seasonal = c(1,0,0), lambda = beato_lambda)
ggtsdisplay(beato_model2$residuals)
summary(beato_model2) # AIC=71045.46   AICc=71045.47   BIC=71087.93

beato_model3 <- Arima(beato_TSc, order = c(2,0,2), seasonal = c(1,0,0), lambda = beato_lambda)
ggtsdisplay(beato_model3$residuals)
summary(beato_model3) # AIC=71022.83   AICc=71022.85   BIC=71072.38

beato_model4 <- Arima(beato_TSc, order = c(1,0,1), seasonal = c(1,0,1), lambda = beato_lambda)
ggtsdisplay(beato_model4$residuals)
summary(beato_model4) # AIC=70028.35   AICc=70028.36   BIC=70070.82

beato_model5 <- Arima(beato_TSc, order = c(1,0,2), seasonal = c(1,0,1), lambda = beato_lambda)
ggtsdisplay(beato_model5$residuals)
summary(beato_model5) # AIC=70018.2   AICc=70018.21   BIC=70067.74

beato_model6 <- Arima(beato_TSc, order = c(2,0,2), seasonal = c(1,0,1), lambda = beato_lambda)
ggtsdisplay(beato_model6$residuals)
summary(beato_model6) # AIC=70013.73   AICc=70013.74   BIC=70070.35

# With seasonal diff
beato_model00 <- Arima(beato_TSc, order = c(1,0,1), seasonal = c(0,1,0), lambda = beato_lambda)
ggtsdisplay(beato_model00$residuals)
summary(beato_model00) # AIC=74657.57   AICc=74657.58   BIC=74678.8

beato_model01 <- Arima(beato_TSc, order = c(1,0,1), seasonal = c(0,1,1), lambda = beato_lambda)
ggtsdisplay(beato_model01$residuals)
summary(beato_model01) # AIC=69883.48   AICc=69883.49   BIC=69911.78

beato_model02 <- Arima(beato_TSc, order = c(1,0,2), seasonal = c(0,1,1), lambda = beato_lambda)
ggtsdisplay(beato_model02$residuals)
summary(beato_model02) # AIC=69871.52   AICc=69871.53   BIC=69906.9

beato_model03 <- Arima(beato_TSc, order = c(2,0,2), seasonal = c(0,1,1), lambda = beato_lambda)
ggtsdisplay(beato_model03$residuals)
summary(beato_model03) # AIC=69868.13   AICc=69868.14   BIC=69910.58

beato_model04 <- Arima(beato_TSc, order = c(1,0,1), seasonal = c(1,1,1), lambda = beato_lambda)
ggtsdisplay(beato_model04$residuals)
summary(beato_model04) # AIC=69826.63   AICc=69826.64   BIC=69862.01

beato_model05 <- Arima(beato_TSc, order = c(1,0,2), seasonal = c(1,1,1), lambda = beato_lambda)
ggtsdisplay(beato_model05$residuals)
summary(beato_model05) # AIC=69815.04   AICc=69815.05   BIC=69857.49

beato_model06 <- Arima(beato_TSc, order = c(2,0,2), seasonal = c(1,1,1), lambda = beato_lambda)
ggtsdisplay(beato_model06$residuals)
summary(beato_model06) # AIC=69810.22   AICc=69810.24   BIC=69859.75

# Best model: (2,0,2)(1,1,1)[24] 
beato_bestmodel = beato_model06
coeftest(beato_bestmodel)
checkresiduals(beato_bestmodel, lag = 10)
ggtsdisplay(beato_bestmodel$residuals)
summary(beato_bestmodel) # AIC=69810.22   AICc=69810.24   BIC=69859.75

# Original time series vs Chosen model
autoplot(beato_TS)+autolayer(beato_bestmodel$fitted, series = "Fit")

###############
### Olivais ###
###############

## Model fitting

# BoxCox transformation
olivais_TSc <- olivais_TS + 0.0001
olivais_lambda = BoxCox.lambda(olivais_TSc) # 1.062574
olivais_boxcox = BoxCox(olivais_TS, lambda = olivais_lambda)
plot(olivais_boxcox, main = "Olivais", xlab = "Day", ylab = ~ "Ozone values " (mu / m^3))
ggtsdisplay(olivais_boxcox)

# QQ-plot
par(mfrow=c(1,2))
qqnorm(olivais_TS, main = "Olivais Time Series")
qqline(olivais_TS, col = 'blue')
qqnorm(olivais_boxcox, main = "Olivais Box-Cox")
qqline(olivais_boxcox,col = 'blue')

# Seasonal Diff (0,0,0)(0,1,0)[24]
olivais_boxcox_seasonal=diff(olivais_boxcox,lag = 24)
plot(olivais_boxcox_seasonal)
ggtsdisplay(olivais_boxcox_seasonal)

# Auto ARIMA
olivais_model <- auto.arima(olivais_TSc, d = 0, max.p = 2, max.q = 2, max.P = 2, max.Q = 2, max.D = 1, seasonal = TRUE, lambda = olivais_lambda)
olivais_model # AIC=65605.3   AICc=65605.32   BIC=65654.85
autoarima_olivais <- Arima(olivais_TSc, order = c(1,0,2), seasonal = c(2,0,0), lambda = olivais_lambda)
checkresiduals(autoarima_olivais)
ggtsdisplay(autoarima_olivais$residuals)

## Model ##

# Without seasonal diff
olivais_model0 <- Arima(olivais_TSc, order = c(0,0,0), seasonal = c(1,0,0), lambda = olivais_lambda)
ggtsdisplay(olivais_model0$residuals)
summary(olivais_model0) # AIC=82002.11   AICc=82002.11   BIC=82023.34

olivais_model1 <- Arima(olivais_TSc, order = c(1,0,1), seasonal = c(1,0,0), lambda = olivais_lambda)
ggtsdisplay(olivais_model1$residuals)
summary(olivais_model1) # AIC=65879.42   AICc=65879.42   BIC=65914.81

olivais_model2 <- Arima(olivais_TSc, order = c(1,0,2), seasonal = c(1,0,0), lambda = olivais_lambda)
ggtsdisplay(olivais_model2$residuals)
summary(olivais_model2) # AIC=65869.91   AICc=65869.92   BIC=65912.38

olivais_model3 <- Arima(olivais_TSc, order = c(2,0,2), seasonal = c(1,0,0), lambda = olivais_lambda)
ggtsdisplay(olivais_model3$residuals)
summary(olivais_model3) # AIC=65861.64   AICc=65861.65   BIC=65911.19

olivais_model4 <- Arima(olivais_TSc, order = c(1,0,1), seasonal = c(1,0,1), lambda = olivais_lambda)
ggtsdisplay(olivais_model4$residuals)
summary(olivais_model4) # AIC=64715.94   AICc=64715.95   BIC=64758.41

olivais_model5 <- Arima(olivais_TSc, order = c(1,0,2), seasonal = c(1,0,1), lambda = olivais_lambda)
ggtsdisplay(olivais_model5$residuals)
summary(olivais_model5) # AIC=64709.47   AICc=64709.49   BIC=64759.02

olivais_model6 <- Arima(olivais_TSc, order = c(2,0,2), seasonal = c(1,0,1), lambda = olivais_lambda)
ggtsdisplay(olivais_model6$residuals)
summary(olivais_model6) # AIC=64709.37   AICc=64709.39   BIC=64766

# With seasonal diff
olivais_model00 <- Arima(olivais_TSc, order = c(1,0,1), seasonal = c(0,1,0), lambda = olivais_lambda)
ggtsdisplay(olivais_model00$residuals)
summary(olivais_model00) # AIC=69524.32   AICc=69524.32   BIC=69545.55

olivais_model01 <- Arima(olivais_TSc, order = c(1,0,1), seasonal = c(0,1,1), lambda = olivais_lambda)
ggtsdisplay(olivais_model01$residuals)
summary(olivais_model01) # AIC=64587.17   AICc=64587.18   BIC=64615.47

olivais_model02 <- Arima(olivais_TSc, order = c(1,0,2), seasonal = c(0,1,1), lambda = olivais_lambda)
ggtsdisplay(olivais_model02$residuals)
summary(olivais_model02) # AIC=64579.07   AICc=64579.08   BIC=64614.45

olivais_model03 <- Arima(olivais_TSc, order = c(2,0,2), seasonal = c(0,1,1), lambda = olivais_lambda)
ggtsdisplay(olivais_model03$residuals)
summary(olivais_model03) # AIC=64579.01   AICc=64579.02   BIC=64621.46

olivais_model04 <- Arima(olivais_TSc, order = c(1,0,1), seasonal = c(1,1,1), lambda = olivais_lambda)
ggtsdisplay(olivais_model04$residuals)
summary(olivais_model04) # AIC=64555.53   AICc=64555.54   BIC=64590.91

olivais_model05 <- Arima(olivais_TSc, order = c(1,0,2), seasonal = c(1,1,1), lambda = olivais_lambda)
ggtsdisplay(olivais_model05$residuals)
summary(olivais_model05) # AIC=64547.73   AICc=64547.74   BIC=64590.18

olivais_model06 <- Arima(olivais_TSc, order = c(2,0,2), seasonal = c(1,1,1), lambda = olivais_lambda)
ggtsdisplay(olivais_model06$residuals)
summary(olivais_model06) # AIC=64521.93   AICc=64521.94   BIC=64571.46

# Best model: (2,0,2)(1,1,1)[24]
olivais_bestmodel = olivais_model06
coeftest(olivais_bestmodel)
checkresiduals(olivais_bestmodel, lag = 24)
ggtsdisplay(olivais_bestmodel$residuals)
summary(olivais_bestmodel) # AIC=64521.93   AICc=64521.94   BIC=64571.46

# Original time series vs Chosen model
autoplot(olivais_TS)+autolayer(olivais_bestmodel$fitted, series = "Fit")

###################
### Entrecampos ###
###################

## Model fitting

# BoxCox transformation
entrecampos_TSc <- entrecampos_TS + 0.0001
entrecampos_lambda = BoxCox.lambda(entrecampos_TSc) # 1.12739
entrecampos_boxcox = BoxCox(entrecampos_TS, lambda = entrecampos_lambda)
plot(entrecampos_boxcox, main = "Entrecampos", xlab = "Day", ylab = ~ "Ozone values " (mu / m^3))
ggtsdisplay(entrecampos_boxcox)

# QQ-plot
par(mfrow=c(1,2))
qqnorm(entrecampos_TS, main = "Entrecampos Time Series")
qqline(entrecampos_TS, col = 'blue')
qqnorm(entrecampos_boxcox, main = "Entrecampos Box-Cox")
qqline(entrecampos_boxcox,col = 'blue')

# Seasonal Diff (0,0,0)(0,1,0)[24]
entrecampos_boxcox_seasonal=diff(entrecampos_boxcox,lag = 24)
plot(entrecampos_boxcox_seasonal)
ggtsdisplay(entrecampos_boxcox_seasonal)

# Auto ARIMA
entrecampos_model <- auto.arima(entrecampos_TSc, d = 0, max.p = 2, max.q = 2, max.P = 2, max.Q = 2, max.D = 1, seasonal = TRUE, lambda = entrecampos_lambda)
entrecampos_model # AIC=69350.19   AICc=69350.2   BIC=69399.73
autoarima_entrecampos <- Arima(entrecampos_TS, order = c(1,0,2), seasonal = c(2,0,0))
checkresiduals(autoarima_entrecampos)
ggtsdisplay(autoarima_entrecampos$residuals)

## Model ##

# Without seasonal diff
entrecampos_model0 <- Arima(entrecampos_TSc, order = c(0,0,0), seasonal = c(1,0,0), lambda = entrecampos_lambda)
ggtsdisplay(entrecampos_model0$residuals)
summary(entrecampos_model0) # AIC=85706.07   AICc=85706.08   BIC=85727.31

entrecampos_model1 <- Arima(entrecampos_TSc, order = c(1,0,1), seasonal = c(1,0,0), lambda = entrecampos_lambda)
ggtsdisplay(entrecampos_model1$residuals)
summary(entrecampos_model1) # AIC=69634.67   AICc=69634.68   BIC=69670.06

entrecampos_model2 <- Arima(entrecampos_TSc, order = c(1,0,2), seasonal = c(1,0,0), lambda = entrecampos_lambda)
ggtsdisplay(entrecampos_model2$residuals)
summary(entrecampos_model2) # AIC=69620.2   AICc=69620.21   BIC=69662.67

entrecampos_model3 <- Arima(entrecampos_TSc, order = c(2,0,2), seasonal = c(1,0,0), lambda = entrecampos_lambda)
ggtsdisplay(entrecampos_model3$residuals)
summary(entrecampos_model3) # AIC=69621.94   AICc=69621.95   BIC=69671.48

entrecampos_model4 <- Arima(entrecampos_TSc, order = c(1,0,1), seasonal = c(1,0,1), lambda = entrecampos_lambda)
ggtsdisplay(entrecampos_model4$residuals)
summary(entrecampos_model4) # AIC=68563.53   AICc=68563.54   BIC=68606

entrecampos_model5 <- Arima(entrecampos_TSc, order = c(1,0,2), seasonal = c(1,0,1), lambda = entrecampos_lambda)
ggtsdisplay(entrecampos_model5$residuals)
summary(entrecampos_model5) # AIC=68564.17   AICc=68564.18   BIC=68613.71

entrecampos_model6 <- Arima(entrecampos_TSc, order = c(2,0,2), seasonal = c(1,0,1), lambda = entrecampos_lambda)
ggtsdisplay(entrecampos_model6$residuals)
summary(entrecampos_model6) # AIC=68520.54   AICc=68520.55   BIC=68577.16

# With seasonal diff
entrecampos_model00 <- Arima(entrecampos_TSc, order = c(1,0,1), seasonal = c(0,1,0), lambda = entrecampos_lambda)
ggtsdisplay(entrecampos_model00$residuals)
summary(entrecampos_model00) # AIC=73292.89   AICc=73292.89   BIC=73314.11

entrecampos_model01 <- Arima(entrecampos_TSc, order = c(1,0,1), seasonal = c(0,1,1), lambda = entrecampos_lambda)
ggtsdisplay(entrecampos_model01$residuals)
summary(entrecampos_model01) # AIC=68436.32   AICc=68436.32   BIC=68464.62

entrecampos_model02 <- Arima(entrecampos_TSc, order = c(1,0,2), seasonal = c(0,1,1), lambda = entrecampos_lambda)
ggtsdisplay(entrecampos_model02$residuals)
summary(entrecampos_model02) # AIC=68436.49   AICc=68436.49   BIC=68471.86

entrecampos_model03 <- Arima(entrecampos_TSc, order = c(2,0,2), seasonal = c(0,1,1), lambda = entrecampos_lambda)
ggtsdisplay(entrecampos_model03$residuals)
summary(entrecampos_model03) # AIC=68393.13   AICc=68393.14   BIC=68435.58

entrecampos_model04 <- Arima(entrecampos_TSc, order = c(1,0,1), seasonal = c(1,1,1), lambda = entrecampos_lambda)
ggtsdisplay(entrecampos_model04$residuals)
summary(entrecampos_model04) # AIC=68404.2   AICc=68404.21   BIC=68439.58

entrecampos_model05 <- Arima(entrecampos_TSc, order = c(1,0,2), seasonal = c(1,1,1), lambda = entrecampos_lambda)
ggtsdisplay(entrecampos_model05$residuals)
summary(entrecampos_model05) # AIC=68404.3   AICc=68404.31   BIC=68446.75

entrecampos_model06 <- Arima(entrecampos_TSc, order = c(2,0,2), seasonal = c(1,1,1), lambda = entrecampos_lambda)
ggtsdisplay(entrecampos_model06$residuals)
summary(entrecampos_model06) # AIC=68405.31   AICc=68405.32   BIC=68454.84

# Best model: (2,0,2)(0,1,1)[24] 
entrecampos_bestmodel <- entrecampos_model03
coeftest(entrecampos_bestmodel)
checkresiduals(entrecampos_bestmodel, lag = 10)
ggtsdisplay(entrecampos_bestmodel$residuals)
summary(entrecampos_bestmodel) # AIC=68393.13   AICc=68393.14   BIC=68435.58

# Original time series vs Chosen model
autoplot(entrecampos_TS)+autolayer(entrecampos_bestmodel$fitted, series = "Fit")

################
### Forecast ###
################

# Alfragide
alfragide_forecast<-forecast(alfragide_bestmodel, h=5, level=95)
summary(alfragide_forecast)
x <- 1:length(alfragide_forecast$lower)
plot(x, alfragide_forecast$lower, type = "l", col = "lightblue", ylim = range(alfragide_forecast$lower, alfragide_forecast$upper), xlab = "Time in hours", ylab = "Forecasted Values", main = "Alfragide Forecast with 95% Confidence Interval")
lines(x, alfragide_forecast$mean, type = "l")
lines(x, alfragide_forecast$upper, type = "l", col = "blue")

# Reboleira
reboleira_forecast<-forecast(reboleira_bestmodel, h=5, level=95)
summary(reboleira_forecast)
x <- 1:length(reboleira_forecast$lower)
plot(x, reboleira_forecast$lower, type = "l", col = "lightblue", ylim = range(reboleira_forecast$lower, reboleira_forecast$upper), xlab = "Time in hours", ylab = "Forecasted Values", main = "Reboleira Forecast with 95% Confidence Interval")
lines(x, reboleira_forecast$mean, type = "l")
lines(x, reboleira_forecast$upper, type = "l", col = "blue")

# Beato
beato_forecast<-forecast(beato_bestmodel, h=5, level=95)
summary(beato_forecast)
x <- 1:length(beato_forecast$lower)
plot(x, beato_forecast$lower, type = "l", col = "lightblue", ylim = range(beato_forecast$lower, beato_forecast$upper), xlab = "Time in hours", ylab = "Forecasted Values", main = "Beato Forecast with 95% Confidence Interval")
lines(x, beato_forecast$mean, type = "l")
lines(x, beato_forecast$upper, type = "l", col = "blue")

# Olivais
olivais_forecast<-forecast(olivais_bestmodel, h=5, level=95)
summary(olivais_forecast)
x <- 1:length(olivais_forecast$lower)
plot(x, olivais_forecast$lower, type = "l", col = "lightblue", ylim = range(olivais_forecast$lower, olivais_forecast$upper), xlab = "Time in hours", ylab = "Forecasted Values", main = "Olivais Forecast with 95% Confidence Interval")
lines(x, olivais_forecast$mean, type = "l")
lines(x, olivais_forecast$upper, type = "l", col = "blue")

# Entrecampos
entrecampos_forecast<-forecast(entrecampos_bestmodel, h=5, level=95)
summary(entrecampos_forecast)
x <- 1:length(entrecampos_forecast$lower)
plot(x, entrecampos_forecast$lower, type = "l", col = "lightblue", ylim = range(alfragide_forecast$lower, alfragide_forecast$upper), xlab = "Time in hours", ylab = "Forecasted Values", main = "Entrecampos Forecast with 95% Confidence Interval")
lines(x, entrecampos_forecast$mean, type = "l")
lines(x, entrecampos_forecast$upper, type = "l", col = "blue")
