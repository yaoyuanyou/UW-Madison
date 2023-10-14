library(TSA)
set.seed(132435); series=arima.sim(n=48,list(ar=0.6))
acf(series)[1:5]
r1 = rep(NA,10000)
r5=r1
for (k in 1:10000) {
  series=arima.sim(n=48,list(ar=0.6))
  r1[k]=acf(series,lag.max = 1,plot = F)$acf[2]
  r5[k]=acf(series,lag.max = 5,plot = F)$acf[5]
}
hist(r1)
mean(r1)
sd(r1)
median(r1)
hist(r5)
mean(r5)
sd(r5)
median(r5)
set.seed(132435); series=arima.sim(n=36,list(ma=c(-0.7,0.5)))
theta1=0.7
theta2=-0.5
ACF=ARMAacf(ma=c(-theta1,-theta2),lag.max = 10)
jpeg('3b.jpg')
acf(series)
dev.off()
series2=arima.sim(n=1000,list(ma=c(-0.7,0.5)))
jpeg('3d.jpg')
pacf(series)
dev.off()
set.seed(2)
series=arima.sim(n=200,list(ar=0.8,ma=-0.4))
phi=0.8
theta=0.4
ACF=ARMAacf(ar=phi,ma=-theta,lag.max=20)
jpeg('4a.jpg')
plot(y=ACF[-1],x=1:20,xlab ='Lag',ylab='ACF',type ='h',ylim = c(-.2,.6))
abline(h=0)
dev.off()
jpeg('4f.jpg')
acf(series)
dev.off()
eacf(series)
