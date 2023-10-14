source('Gla1ve.r')
x.original=read.csv('AAL.csv',row.names = 1)
x= timeSeries(x.original$X[-1],charvec = row.names(x.original)[-1])
GARCH.Finding(x)
GARCH.Fitting(x)
plot(x,type='h',ylab='Index Return')
plot(residuals(GARCH.Model(x)),type='h',ylab='standardized residuals')
qqnorm(residuals(GARCH.Model(x))) 
qqline(residuals(GARCH.Model(x)))
acf(residuals(GARCH.Model(x))^2,na.action=na.omit)
shapiro.test(na.omit(residuals(GARCH.Model(x))))
jarque.bera.test(na.omit(residuals(GARCH.Model(x))))
skewness(na.omit(residuals(GARCH.Model(x))))
kurtosis(na.omit(residuals(GARCH.Model(x))))
gBox(GARCH.Model(x),x=x,method='squared')
gBox(GARCH.Model(x),x=x,lags=20,plot=F,method='squared')$pvalue

y=read.csv('Data.csv',row.names = 1)
y.return=cbind(y$AAL,y$BBY,y$BIIB,y$BSX,y$BXP,y$COG,y$GS,y$INTC,y$NEE,y$TXN)
sam=(y$BBY+y$BIIB+y$BSX+y$TXN+y$AAL+y$NEE+y$INTC+y$BXP+y$COG+y$GS)/10
sam.mean = mean(sam)
sam.sd=sd(sam)
c=rep(1/10,10)
object=function(x){
  1/mean(x[1]*y.return[,1]+x[2]*y.return[,2]++x[3]*y.return[,3]+x[4]*y.return[,4]+x[5]*y.return[,5]+x[6]*y.return[,6]+x[7]*y.return[,7]+x[8]*y.return[,8]+x[9]*y.return[,9]++x[10]*y.return[,10])
}
par.l=rep(0,10)
par.u=rep(1,10)
A = matrix(rep(1,10),1,byrow=TRUE)
lin.l=1
lin.u=1
nlcon= function(x){
  sd(x[1]*y.return[,1]+x[2]*y.return[,2]++x[3]*y.return[,3]+x[4]*y.return[,4]+x[5]*y.return[,5]+x[6]*y.return[,6]+x[7]*y.return[,7]+x[8]*y.return[,8]+x[9]*y.return[,9]++x[10]*y.return[,10])
}
nlcon1=function(x){
  mean(x[1]*y.return[,1]+x[2]*y.return[,2]++x[3]*y.return[,3]+x[4]*y.return[,4]+x[5]*y.return[,5]+x[6]*y.return[,6]+x[7]*y.return[,7]+x[8]*y.return[,8]+x[9]*y.return[,9]++x[10]*y.return[,10])
}
nlin.l = c(0,0)
nlin.u = c(sam.sd,+Inf)
donlp2(c, object, par.u=par.u, par.l=par.l,
       A,¡¡lin.l=lin.l,lin.u=lin.u,
       nlin=list(nlcon,nlcon1),
       nlin.u=nlin.u, nlin.l=nlin.l)$par

x1=residuals(GARCH.Model(y$AAL))
x2=residuals(GARCH.Model(y$BBY))
x3=residuals(GARCH.Model(y$BIIB))
x4=residuals(GARCH.Model(y$BSX))
x5=residuals(GARCH.Model(y$BXP))
x6=residuals(GARCH.Model(y$COG))
x7=residuals(GARCH.Model(y$GS))
x8=residuals(GARCH.Model(y$INTC))
x9=residuals(GARCH.Model(y$NEE))
x10=residuals(GARCH.Model(y$TXN))
sam.1=(x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)[-2:-1]/10
sam.1.mean=mean(sam.1)
sam.1.sd=sd(sam.1)
object.1=function(x){
  1/mean(x[1]*x1[-2:-1]+x[2]*x2[-2:-1]+x[3]*x3[-2:-1]+x[4]*x4[-2:-1]+x[5]*x5[-2:-1]+x[6]*x6[-2:-1]+x[7]*x7[-2:-1]+x[8]*x8[-2:-1]+x[9]*x9[-2:-1]+x[10]*x10[-2:-1])
  }
nlcon.1= function(x){
sd(x[1]*x1[-2:-1]+x[2]*x2[-2:-1]+x[3]*x3[-2:-1]+x[4]*x4[-2:-1]+x[5]*x5[-2:-1]+x[6]*x6[-2:-1]+x[7]*x7[-2:-1]+x[8]*x8[-2:-1]+x[9]*x9[-2:-1]+x[10]*x10[-2:-1])
}
nlcon1.1=function(x){
  mean(x[1]*x1[-2:-1]+x[2]*x2[-2:-1]+x[3]*x3[-2:-1]+x[4]*x4[-2:-1]+x[5]*x5[-2:-1]+x[6]*x6[-2:-1]+x[7]*x7[-2:-1]+x[8]*x8[-2:-1]+x[9]*x9[-2:-1]+x[10]*x10[-2:-1])
}
donlp2(c, object.1, par.u=par.u, par.l=par.l,
       A,¡¡lin.l=lin.l,lin.u=lin.u,
       nlin=list(nlcon.1,nlcon1.1),
       nlin.u=c(sam.1.sd,+Inf), nlin.l=c(0,sam.1.mean))$par

y.price=cbind(y$Close.8,y$Close,y$Close.1,y$Close.2,y$Close.3,y$Close.4,y$Close.5,y$Close.6,y$Close.7,y$Close.9)
sam.2=log(rowSums(y.price[-1,])/10)-log(rowSums(y.price[-nrow(y.price),]/10))
sam.2.mean=mean(sam.2)
sam.2.sd=sd(sam.2)
object.2=function(x){
  1/mean((log(x[1]*y.price[-1,1]+x[2]*y.price[-1,2]+x[3]*y.price[-1,3]+x[4]*y.price[-1,4]+x[5]*y.price[-1,5]+x[6]*y.price[-1,6]+x[7]*y.price[-1,7]+x[8]*y.price[-1,8]+x[9]*y.price[-1,9]+x[10]*y.price[-1,10])-
            log(x[1]*y.price[-nrow(y.price),1]+x[2]*y.price[-nrow(y.price),2]+x[3]*y.price[-nrow(y.price),3]+x[4]*y.price[-nrow(y.price),4]+x[5]*y.price[-nrow(y.price),5]+x[6]*y.price[-nrow(y.price),6]+x[7]*y.price[-nrow(y.price),7]+x[8]*y.price[-nrow(y.price),8]+x[9]*y.price[-nrow(y.price),9]+x[10]*y.price[-nrow(y.price),10])))
}
nlcon.2= function(x){
  sd(log(x[1]*y.price[-1,1]+x[2]*y.price[-1,2]+x[3]*y.price[-1,3]+x[4]*y.price[-1,4]+x[5]*y.price[-1,5]+x[6]*y.price[-1,6]+x[7]*y.price[-1,7]+x[8]*y.price[-1,8]+x[9]*y.price[-1,9]+x[10]*y.price[-1,10])-log(x[1]*y.price[-nrow(y.price),1]+x[2]*y.price[-nrow(y.price),2]+x[3]*y.price[-nrow(y.price),3]+x[4]*y.price[-nrow(y.price),4]+x[5]*y.price[-nrow(y.price),5]+x[6]*y.price[-nrow(y.price),6]+x[7]*y.price[-nrow(y.price),7]+x[8]*y.price[-nrow(y.price),8]+x[9]*y.price[-nrow(y.price),9]+x[10]*y.price[-nrow(y.price),10]))
}
nlcon1.2=function(x){
  mean((log(x[1]*y.price[-1,1]+x[2]*y.price[-1,2]+x[3]*y.price[-1,3]+x[4]*y.price[-1,4]+x[5]*y.price[-1,5]+x[6]*y.price[-1,6]+x[7]*y.price[-1,7]+x[8]*y.price[-1,8]+x[9]*y.price[-1,9]+x[10]*y.price[-1,10])-
            log(x[1]*y.price[-nrow(y.price),1]+x[2]*y.price[-nrow(y.price),2]+x[3]*y.price[-nrow(y.price),3]+x[4]*y.price[-nrow(y.price),4]+x[5]*y.price[-nrow(y.price),5]+x[6]*y.price[-nrow(y.price),6]+x[7]*y.price[-nrow(y.price),7]+x[8]*y.price[-nrow(y.price),8]+x[9]*y.price[-nrow(y.price),9]+x[10]*y.price[-nrow(y.price),10])))
}
donlp2(c, object.2, par.u=par.u, par.l=par.l,
       A,¡¡lin.l=lin.l,lin.u=lin.u,
       nlin=list(nlcon.2,nlcon1.2),
       nlin.u=c(sam.2.sd,+Inf), nlin.l=c(0,sam.2.mean))$par

mean(residuals(GARCH.Model(sam.2)),na.rm = T)
sd(residuals(GARCH.Model(sam.2)),na.rm = T)
