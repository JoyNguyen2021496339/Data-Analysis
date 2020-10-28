########## NAME ###########                                         ####### SPRING 2020 ######
#### UYEN NGUYEN (JOY) ####                                         ##### FERM6320_FINAL #####
###########################                                         ##########################

install.packages("tseries")
install.packages("forecast")
################################### (a)+(b)+(c) IP ANALYSIS ###################################

library(quantmod)
getSymbols("INDPRO",src="FRED")
IP = INDPRO[1093:1201,]
View(IP)

plot(IP, type = 'l',xlab="Date",ylab="Production") 
#-> We can see the upward trend pattern in the series of IP

par(mfrow=c(2,1))
acf = acf(IP,lag.max = 15, main ="ACF_IP") #die off gradually
pacf = pacf(IP,lag.max = 15, main = "PACF_IP") #cut off after 1 lag
#-> Possibly Model AR(1)

## Stationary Test of IP ##
library(tseries)
adf.test(IP) #pvalue is much higher than 5% level of significant -> not reject the null
# -> Data contains unit roots -> Non-stationary

#################################### (d)+(e) ONE TIME LAG IP ################################

IP$dIP = diff(log(IP$INDPRO))
IP = IP[-1,]

par(mfrow=c(1,1))
plot(IP$dIP, type = 'l',xlab="Date",ylab="Difference in IP")

par(mfrow=c(2,1))
acf = acf(IP$dIP,lag.max = 15, main = "ACF_dIP")
pacf = pacf(IP$dIP,lag.max = 15, main = "PACF_dIF")
#Not observe any strong cut-off from acf or pacf -> possibly model ARMA

adf.test(IP$dIP) #pvalue is smaller than 5% level of significant -> reject the null
# -> dIP does not contain unit roots -> stationary

###################################### (f) ARIMA MODEL #######################################

AIC_table = matrix(NA, nrow = 7, ncol = 7, 
                   dimnames=list(c(paste("AR(",0:6,")")), c(paste("MA(",0:6,")"))))
for (ar in 0:6) {
  for (ma in 0:6) {
    ARIMA = arima(IP$dIP,order = c(ar,0,ma))
    AIC_table[ar+1,ma+1] = AIC(ARIMA)
  }
}
AIC_table

order = which(AIC_table == min(AIC_table), arr.ind = TRUE)
order
model.optim = arima(IP$dIP, order=c(order[,1]-1,0,order[,2]-1))
model.optim

###################################### (g) LJUNG-BOX TEST ####################################

res = model.optim$residuals
Box.test(res,type="Ljung-Box") #High pvalue -> Not reject the null
# -> No auto-corelation for residuals in ARIMA model

library(stats)
tsdiag(model.optim) 

################################## (h) FORECAST 12 MONTH IP #################################

library(forecast)

pred_dIP = forecast(model.optim,h=12)
plot(pred_dIP)

fc = matrix(NA, nrow = 13, ncol = 2, 
            dimnames=list(c("current", paste("next",1:12)), c("fc_dIP","fc_IP")))
fc[,1] = c(NA, pred_dIP$mean)
fc[1,2] = IP$INDPRO[nrow(IP)]
for( i in 2:13){
  fc[i,2] = exp(fc[i,1])*fc[i-1,2]
}
fc





