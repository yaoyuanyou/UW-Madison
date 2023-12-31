


baby.t.test = function(x, mu=0, conf.level=.95) {
  stopifnot(length(x) >= 2)
  stopifnot(0 < conf.level) & (conf.level < 1))
  n = length(x)
  x.bar = mean(x)
  s.x = sd(x)
  t = (x.bar - mu) / (s.x / sqrt(n)
  r = list() # set up r as return value
  r$statistic = t
  r$parameter = n - 1
  r$p.value = 2*pt(q=-abs(t), df=n)
  alpha = 1 - conf.level
  t.for.conf.level = -qt(p=alpha/2, df=n-1)
  error.margin = t.for.conf.level * s.x / sqrt(n)
  r$conf.int = c(x.bar - error.margin, x.bar + error.margin)
  r$estimate = x.bar
  r$null.value = mu
  return(r)
}



## test case
baby.t = baby.t.test(1:10, 5, .90)
print(str(baby.t)
t = t.test(x=1:10, mu=5, conf.level=.90) # Call real t.test() to check mine.





## Note: as.numeric(), below, removes names from components of t
stopifnot(baby.t$statistic == as.numeric(t$statistic))
stopifnot(baby.t$parameter == as.numeric(t$parameter)
stopifnot(baby.t$p.value == as.numeric(t$p.value))
## (The next test, and really all of these, should use all.equal()
## rather than "==" to avoid checking floating-point numbers for
## equality. We may correct this later after learning regular
## expressions.)
##stopifnot(baby.t$conf.int == as.numeric(t$conf.int))
stopifnot(baby.t$estimate == as.numeric(t$estimate))
stopifnot(baby.t$null.value == as.numeric(t$null.value))
