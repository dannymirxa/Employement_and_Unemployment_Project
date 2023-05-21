# Libraries
library(tseries)
library(tidyverse)
library(forecast)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(randomForest)

data <- read.csv(file.choose())


# Plot
ggplot(data, aes(x = year)) +
  geom_line(aes(y = m_lf, color = "male workforce")) +
  geom_line(aes(y = f_lf, color = "fmale workforce")) +
  geom_line(aes(y = age15.59_lf, color = "age15-60 workforece")) +
  geom_line(aes(y = age60.64_lf,color="age60-64 workforce")) +
  geom_line(aes(y=m_lf+f_lf,color="overall workforce"))+
  labs(x = "year", y = "number of workforce",title="Trends of aging workforce in Malaysia") +
  guides(size=none)+
  theme(axis.text = element_text(size=12),
        plot.title = element_text(hjust=0.5))
  scale_color_manual(values = c("#0072BD", "#D95319", "#EDB120","#7E2F8E","#77AC30"),
                     labels = c("male workforce", "fmale workforce", "age15-60 workforece","age60-64 workforce","overall workforce"))



#Calculate the unemployment rate and add it to the unemployment_rate column of data
data$unemployment_rate <- round((rowSums(data[,c('m_lf','f_lf')])-rowSums(data[,c('age15.59_employ','age60.64_employ')]))/ rowSums(data[,c('m_lf','f_lf')]),2)

#Pick out the unemployment rate and the year
unemployment_data <- data %>% select(year,unemployment_rate)
#covert to time series data
ts_unemployment_data <- ts(unemployment_data$unemployment_rate,start=c(1982),frequency = 1)
plot(ts_unemployment_data)

#Judging how many splits are needed to transform into a stationary sequence
ndiffs(ts_unemployment_data)

#First order difference processing
dts_unemployment_data <- diff(ts_unemployment_data)
plot(dts_unemployment_data)

#Augmented Dickey-Fuller
ADF <- adf.test(dts_unemployment_data)
ADF

#Model order determination and fitting
fit <- auto.arima(ts_unemployment_data)
fit
accuracy(fit)

#model diagnosis
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals,type="Ljung-Box")

#prediction
forecast(fit,3)
plot(forecast(fit,3),xlab = "Year",ylab = "unemployment rate")

#prophet
library(prophet)
library(zoo)
prophet_unemployment_data <- unemployment_data
prophet_unemployment_data$year <- as.yearmon(prophet_unemployment_data$year)
colnames(prophet_unemployment_data) <- c("ds","y")
prophet_model <- prophet(prophet_unemployment_data,
                         growth = "linear",
                         yearly.seasonality = TRUE,weekly.seasonality = FALSE,
                         daily.seasonality = FALSE,seasonality.mode = "multiplicative")

future <- make_future_dataframe(prophet_model,periods = 24,freq = "month")
myForecast <- predict(prophet_model,future)
plot(prophet_model,myForecast)



#lm
lm_model <- lm(age60.64_employ~.-year-unemployment_rate,data=data[1:30,])
lm_predictions <- predict(lm_model, newdata = data[31:40,])
plot(c(data$year[31:40]),data$age60.64_employ[31:40],type="l",col="blue",lwd=3,lty=1)
lines(c(data$year[31:40]),lm_predictions,col="red",lwd=3,lty=2)
legend("topleft", legend = c("Actual value", "Predictive value"), col = c("blue", "red"), lty = 1, lwd = 2)



