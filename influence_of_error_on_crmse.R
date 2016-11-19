ntries = 10000
nvariables = 5
exp_err = 0.2
cRMSE = c()
for (i in seq(1,ntries)){
  a = rnorm(nvariables,sd=exp_err)
  b = rnorm(nvariables,sd=exp_err)
  a = a - mean(a)
  b = b - mean(b)
  cRMSE = c(cRMSE,sqrt(sum((b-a)**2)/nvariables))
}
plot(cRMSE)
hist(cRMSE)
median(cRMSE)