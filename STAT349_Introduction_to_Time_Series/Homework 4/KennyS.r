library(TSA)
data("winnebago")
jpeg('1.jpg')
plot(winnebago,ylab="Monthly Sales", type ='l')
points(y=winnebago, x=time(winnebago),pch=as.vector(season(winnebago)))
dev.off()
jpeg('2.jpg')
plot(log(winnebago),ylab="Log Monthly Sales", type ='l')
points(y=log(winnebago), x=time(winnebago),pch=as.vector(season(winnebago)))
dev.off()
x=winnebago
y=vector()
z=vector()
for (i in 2:64) {
  y[i-1] = (x[i]-x[i-1])/x[i-1]
  z[i-1] = log(x[i])-log(x[i-1])
}
jpeg('3.jpg')
plot(y,type = 'b')
lines(z,type = 'b',pch=2)
legend("topleft",c("Fractional Relative Changes","Differences of Logarithms"),pch=c(1,2))
dev.off
data("gold")
jpeg('4.jpg')
plot(gold,ylab="Daily Price", type ='l')
dev.off()
x=gold
z=vector()
for (i in 2:252) {
  z[i-1] = log(x)[i]-log(x)[i-1]
}
jpeg('5.jpg')
plot(z,ylab="Differences of Logarithms", type ='l')
dev.off()
jpeg('6.jpg')
acf(z)
dev.off()
jpeg('7.jpg')
hist(z)
dev.off()
jpeg('8.jpg')
qqnorm(z)
dev.off()
