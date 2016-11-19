library(ggplot2)
library(reshape2)

normalize_values = function(x,min,max) {
  return((x - min)/(max-min))
}

# get 12000 random values between 0 and 1 for 6000 pairs of random series.
rvector_random = c()
while (length(rvector_random) != 12000) {
  temp = rnorm(1, mean = 0.5, 4)
  if (temp >= 0) {
    if (temp <= 1) {
      rvector_random = c(rvector_random,temp)
    }
  }  
}
# extract 6000 pairs of series from the random 12000 values.
# calculate the similarity of these pairs to have a baseline for the similarity model. 
crmse = c()
pear = c()
spear = c()
manh = c()
eucl = c()
ddp = c()
for (i in seq(1,12000,12)) {
  series_1 = c()
  for (j in seq(i,i+5)) {
    series_1 = c(series_1, rvector_random[j])
  }
  series_2 = c()
  for (j in seq(i+6,i+11)) {
    series_2 = c(series_2, rvector_random[j])
  }
  x_y = rbind(series_1, series_2)
  pear = c(pear, cor(series_2, series_1, method='pearson'))
  spear = c(spear, cor(series_2, series_1, method='spearman'))
  manh = c(manh,dist(x_y, method = "manhattan"))
  eucl = c(eucl,dist(x_y, method = "euclidean"))
  y_series = series_2 - mean(series_2)
  x_series = series_1 - mean(series_1)
  crmse = c(crmse, sqrt(sum((y_series - x_series)**2)/length(x)))
  ddp = c(ddp, y_series[6] - x_series[6])
}
eucl_normed = sapply(eucl, normalize_values,min=min(eucl),max=max(eucl))
crmse_normed = sapply(crmse, normalize_values,min=min(crmse),max=max(crmse))
manh_normed = sapply(manh, normalize_values,min=min(manh),max=max(manh))

# how do the metrics perform when random with random series are compared.
crmse = c()
pear = c()
spear = c()
manh = c()
eucl = c()
x_series_vector = c()
y_series_vector = c()
exp_err = 0.44
ntries = 1000
index = c(1,2,3,4,5,6)

j = 0
for (i in seq(1,ntries)) {
  x = rnorm(6,sd=0.17)
  y = rnorm(6,sd=0.17)
  x_y = rbind(x, y)
  pear = c(pear, cor(y, x, method='pearson'))
  spear = c(spear, cor(y, x, method='spearman'))
  manh = c(manh,dist(x_y, method = "manhattan"))
  eucl = c(eucl,dist(x_y, method = "euclidean"))
  y_series = y - mean(y)
  x_series = x - mean(x)
  crmse = c(crmse, sqrt(sum((y_series - x_series)**2)/length(x)))
  x_series_vector = c(x_series_vector, x_series)
  y_series_vector = c(y_series_vector, y_series)
  j = j + 1
}

# transform the value ranges of eucl, manh, and crmse into the range from 0 to 1
eucl_normed = sapply(eucl, normalize_values,min=min(eucl),max=max(eucl))
crmse_normed = sapply(crmse, normalize_values,min=min(crmse),max=max(crmse))
manh_normed = sapply(manh, normalize_values,min=min(manh),max=max(manh))
plot(crmse)
plot(pear)
plot(spear)
plot(manh)
plot(eucl)
hist(eucl_normed)
hist(crmse_normed)
hist(manh_normed)
summary(crmse_normed)
summary(pear)
summary(spear)
summary(manh_normed)
summary(eucl_normed)
