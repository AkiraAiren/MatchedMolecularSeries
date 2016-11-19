# Analysis of parallel series. 
# How do the metrics perform? Do they really rank it as 100% similar? 
library(flexclust)
library(ggplot2)

normalize_values <- function(x,min,max) {
  return((x - min)/(max-min))
}

# How do the metrics perform for parallel series with uncertainty? Absolute difference between pairs bigger than average error.
crmse = c()
pear = c()
spear = c()
manh = c()
eucl = c()
x_series_vector = c()
y_series_vector = c()
exp_err = 0.1
ntries = 1000
index = c(1,2,3,4,5,6)

j = 0
for (i in seq(1,ntries)) {
  x = c(5,6,7,8,9,10)
  y = c(5,6,7,8,9,10)
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
# eucl_normed = sapply(eucl, normalize_values,min=min(eucl),max=max(eucl))
# crmse_normed = sapply(crmse, normalize_values,min=min(crmse),max=max(crmse))
# manh_normed = sapply(manh, normalize_values,min=min(manh),max=max(manh))
plot(crmse)
plot(pear)
plot(spear)
plot(manh)
plot(eucl)
summary(crmse)
summary(pear)
summary(spear)
summary(manh)
summary(eucl)

# How do the metrics perform for parallel series with uncertainty? Absolute difference between pairs bigger than average error.
crmse_middle = c()
pear_middle = c()
spear_middle = c()
manh_middle = c()
eucl_middle = c()
x_series_vector = c()
y_series_vector = c()
exp_err = 0.1
ntries = 1000
index = c(1,2,3,4,5,6)

j = 0
for (i in seq(1,ntries)) {
  x = c(15,16,17,18,19,20)
  y = c(15,16,17,18,19,20)
  random_values_x = rnorm(6,sd=exp_err)
  random_values_y = rnorm(6,sd=exp_err)
  x = x + random_values_x
  y = y + random_values_y
  x_y = rbind(x, y)
  pear_middle = c(pear_middle, cor(y, x, method='pearson'))
  spear_middle = c(spear_middle, cor(y, x, method='spearman'))
  manh_middle = c(manh_middle,dist(x_y, method = "manhattan"))
  eucl_middle = c(eucl_middle,dist(x_y, method = "euclidean"))
  y_series = y - mean(y)
  x_series = x - mean(x)
  crmse_middle = c(crmse_middle, sqrt(sum((y_series - x_series)**2)/length(x)))
  x_series_vector = c(x_series_vector, x_series)
  y_series_vector = c(y_series_vector, y_series)
  j = j + 1
}
# transform the value ranges of eucl, manh, and crmse into the range from 0 to 1
# eucl_normed = sapply(eucl, normalize_values,min=min(eucl),max=max(eucl))
# crmse_normed = sapply(crmse, normalize_values,min=min(crmse),max=max(crmse))
# manh_normed = sapply(manh, normalize_values,min=min(manh),max=max(manh))
plot(crmse_middle)
plot(pear_middle)
plot(spear_middle)
plot(manh_middle)
plot(eucl_middle)
summary(crmse_middle)
summary(pear_middle)
summary(spear_middle)
summary(manh_middle)
summary(eucl_middle)

# How do the metrics perform for parallel series with uncertainty? Absolute difference between pairs bigger than average error.
crmse_max = c()
pear_max = c()
spear_max = c()
manh_max = c()
eucl_max = c()
x_series_vector = c()
y_series_vector = c()
exp_err = 0.1
ntries = 1000
index = c(1,2,3,4,5,6)

j = 0
for (i in seq(1,ntries)) {
  x = c(30,31,32,33,34,35)
  y = c(30,31,32,33,34,35)
  random_values_x = rnorm(6,sd=exp_err)
  random_values_y = rnorm(6,sd=exp_err)
  x = x + random_values_x
  y = y + random_values_y
  x_y = rbind(x, y)
  pear_max = c(pear_max, cor(y, x, method='pearson'))
  spear_max = c(spear_max, cor(y, x, method='spearman'))
  manh_max = c(manh_max,dist(x_y, method = "manhattan"))
  eucl_max = c(eucl_max,dist(x_y, method = "euclidean"))
  y_series = y - mean(y)
  x_series = x - mean(x)
  crmse_max = c(crmse_max, sqrt(sum((y_series - x_series)**2)/length(x)))
  x_series_vector = c(x_series_vector, x_series)
  y_series_vector = c(y_series_vector, y_series)
  j = j + 1
}
# transform the value ranges of eucl, manh, and crmse into the range from 0 to 1
# eucl_normed = sapply(eucl, normalize_values,min=min(eucl),max=max(eucl))
# crmse_normed = sapply(crmse, normalize_values,min=min(crmse),max=max(crmse))
# manh_normed = sapply(manh, normalize_values,min=min(manh),max=max(manh))
plot(crmse_max)
plot(pear_max)
plot(spear_max)
plot(manh_max)
plot(eucl_max)
summary(crmse_max)
summary(pear_max)
summary(spear_max)
summary(manh_max)
summary(eucl_max)
