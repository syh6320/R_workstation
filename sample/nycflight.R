
age <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip = 3)
age_time_ser <- ts(age)
plot.ts(age_time_ser)

library(tseries)
kpss.test(age_time_ser)
age_time_ser1 <- diff(age_time_ser,differences = 1)
kpss.test(age_time_ser1)

library(forecast)
tsdisplay(age_time_ser1, max.lag = 30)

tot <- 0
for(i in c(0:3)){
  tot <- tot + 
    (gamma(11)/(gamma(i+1) * gamma(10 - i + 1))) * (1/4)^i * (3/4)^(10 - i)
}
