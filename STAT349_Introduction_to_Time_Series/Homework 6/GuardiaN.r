library(TSA)
set.seed(1)
series = arima.sim(n=30,list(ar=0.5))
model=arima(series,order = c(1,0,0))
a=rstandard(model)
jpeg('3a.jpg')
plot(a,ylab='Standard Residuals',type='o')
abline(h=0)
dev.off()
jpeg('3b.jpg')
qqnorm(a)
qqline(a)
dev.off()
jpeg('3c.jpg')
acf(a)
dev.off()
LB.test(model,lag = 8)
data(hare)
model=arima(sqrt(hare),order = c(3,0,0))
jpeg('4a.jpg')
acf(rstandard(model))
dev.off()
LB.test(model,lag = 9)
runs(rstandard(model))
jpeg('4d.jpg')
qqnorm(rstandard(model))
qqline(rstandard(model))
dev.off()
shapiro.test(residuals(model))
