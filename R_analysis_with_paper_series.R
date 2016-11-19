# Analysis of parallel series. 
# How do the metrics perform? Do they really rank it as 100% similar? 

install.packages("flexclust")
library(flexclust)
library(ggplot2)

normalize_values <- function(x,min,max) {
  return((x - min)/(max-min))
}

# How do metrics progress with a parallel series?
crmse = c()
pear = c()
spear = c()
manh = c()
eucl = c()

ntries = 1000
x = c(5,6,7,8,9,10)
y = c(5,6,7,8,9,10)
y_series = y - mean(y)
x_series = x - mean(x)

x_y = rbind(x_series, y_series)
j = 0
for (i in seq(1,ntries)) {
  crmse = c(crmse, sqrt(sum((y_series - x_series)**2)/length(x)))
  pear = c(spear, cor(y_series,x_series, method='pearson'))
  spear = c(pear, cor(y_series,x_series, method='spearman'))
  manh = c(manh,dist(x_y, method = "manhattan"))
  eucl = c(eucl,dist(x_y, method = "euclidean"))
  j = j + 1
}
plot(crmse)
plot(pear)
plot(spear)
plot(manh)
plot(eucl)

# How do the metrics perform with a parallel series with an offset
crmse = c()
pear = c()
spear = c()
manh = c()
eucl = c()

ntries = 1000
x = c(5,6,7,8,9,10)
y = c(6,7,8,9,10,11)
y_series = y - mean(y)
x_series = x - mean(x)

x_y = rbind(x_series, y_series)
j = 0
for (i in seq(1,ntries)) {
  crmse = c(crmse, sqrt(sum((y_series - x_series)**2)/length(x)))
  pear = c(pear, cor(y_series,x_series, method='pearson'))
  spear = c(spear, cor(y_series,x_series, method='spearman'))
  manh = c(manh,dist(x_y, method = "manhattan"))
  eucl = c(eucl,dist(x_y, method = "euclidean"))
  j = j + 1
}
plot(crmse)
plot(pear)
plot(spear)
plot(manh)
plot(eucl)

# How do the metrics perform for parallel series with slope?
crmse = c()
pear = c()
spear = c()
manh = c()
eucl = c()

ntries = 1000
index = c(1,2,3,4,5,6)
x = c(5,6,7,8,9,10)
y = c(5,6.5,8,9.5,11,12.5)
y_series = y - mean(y)
x_series = x - mean(x)

x_y = rbind(x_series, y_series)
j = 0
for (i in seq(1,ntries)) {
  crmse = c(crmse, sqrt(sum((y_series - x_series)**2)/length(x)))
  pear = c(pear, cor(y_series,x_series, method='pearson'))
  spear = c(spear, cor(y_series,x_series, method='spearman'))
  manh = c(manh,dist(x_y, method = "manhattan"))
  eucl = c(eucl,dist(x_y, method = "euclidean"))
  j = j + 1
}
plot(crmse)
plot(pear)
plot(spear)
plot(manh)
plot(eucl)

# How do the metrics perform for parallel series with uncertainty? Absolute difference between pairs bigger than average error.
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
  x = c(5,8,11,14,17,20)
  y = c(5,8,11,14,17,20)
  random_values_x = rnorm(6,sd=exp_err)
  random_values_y = rnorm(6,sd=exp_err)
  x = x + random_values_x
  y = y + random_values_y
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
plot(crmse_normed)
plot(pear)
plot(spear)
plot(manh_normed)
plot(eucl_normed)
summary(crmse_normed)
summary(pear)
summary(spear)
summary(manh_normed)
summary(eucl_normed)

# Absolute difference between pairs bigger than average error with offset
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
  x = c(5,8,11,14,17,20)
  y = c(5,10,15,20,25,30)
  random_values_x = rnorm(6,sd=exp_err)
  random_values_y = rnorm(6,sd=exp_err)
  x = x + random_values_x
  y = y + random_values_y
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
plot(crmse)
plot(pear)
plot(spear)
plot(manh)
plot(eucl)

# Absolute difference between pairs bigger than average error with differing slope
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
  x = c(5,8,11,14,17,20)
  y = c(5,12,19,26,34,41)
  random_values_x = rnorm(6,sd=exp_err)
  random_values_y = rnorm(6,sd=exp_err)
  x = x + random_values_x
  y = y + random_values_y
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
plot(crmse)
plot(pear)
plot(spear)
plot(manh)
plot(eucl)

# Is the range of the crmse independent from the absolute values? 
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
  x = c(1,2,3,4,5,6)
  y = c(1,8,15,22,29,36)
  random_values_x = rnorm(6,sd=exp_err)
  random_values_y = rnorm(6,sd=exp_err)
  x = x + random_values_x
  y = y + random_values_y
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
plot(crmse)
plot(pear)
plot(spear)
plot(manh)
plot(eucl)
