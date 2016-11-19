# ddp calculation for random series and for series with small difference in pairs and big difference in pairs
library(ggplot2)
library(reshape2)

normalize_values = function(x,min,max) {
  return((x - min)/(max-min))
}

# get 12000 random values between 0 and 1 for 6000 pairs of random series.
rvector_random = c()
while (length(rvector_random) != 12000) {
  temp = rnorm(1, mean = 0.5, sd = 4)
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
  series_1_set_of_5 = series_1[0:5]
  series_2_set_of_5 = series_2[0:5]
  x_y = rbind(series_1_set_of_5, series_2_set_of_5)
  pear = c(pear, cor(series_2_set_of_5, series_1_set_of_5, method='pearson'))
  spear = c(spear, cor(series_2_set_of_5, series_1_set_of_5, method='spearman'))
  manh = c(manh,dist(x_y, method = "manhattan"))
  eucl = c(eucl,dist(x_y, method = "euclidean"))
  y_series = series_2_set_of_5 - mean(series_2_set_of_5)
  x_series = series_1_set_of_5 - mean(series_1_set_of_5)
  crmse = c(crmse, sqrt(sum((y_series - x_series)**2)/length(x_series)))
  series_1 = series_1 - mean(series_1)
  series_2 = series_2 - mean(series_2)
  ddp = c(ddp, (series_2[6] - mean(series_2_set_of_5)) - (series_1[6] - mean(series_1_set_of_5)))
}

# performance of the ordered series with small differences between the pairs
crmse_ordered_series = c()
pear_ordered_series = c()
spear_ordered_series = c()
manh_ordered_series = c()
eucl_ordered_series = c()
x_series_vector = c()
y_series_vector = c()
exp_err = 0.2
ntries = 1000
ddp_ordered_series = c()
j = 0
for (i in seq(1,ntries)) {
  x = c(0,0.2,0.4,0.6,0.8,1.0)
  y = c(0,0.2,0.4,0.6,0.8,1.0)
  random_values_x = rnorm(6,sd=exp_err)
  random_values_y = rnorm(6,sd=exp_err)
  x = x + random_values_x
  y = y + random_values_y
  x_set_of_5 = x[0:5]
  y_set_of_5 = y[0:5]
  x_y = rbind(x_set_of_5, y_set_of_5)
  pear_ordered_series = c(pear_ordered_series, cor(y_set_of_5, x_set_of_5, method='pearson'))
  spear_ordered_series = c(spear_ordered_series, cor(y_set_of_5, x_set_of_5, method='spearman'))
  manh_ordered_series = c(manh_ordered_series,dist(x_y, method = "manhattan"))
  eucl_ordered_series = c(eucl_ordered_series,dist(x_y, method = "euclidean"))
  y_series_set_of_5 = y_set_of_5 - mean(y_set_of_5)
  x_series_set_of_5 = x_set_of_5 - mean(x_set_of_5)
  crmse_ordered_series = c(crmse_ordered_series, sqrt(sum((y_series_set_of_5 - x_series_set_of_5)**2)/length(x_set_of_5)))
  x_series = x
  y_series = y
  ddp_ordered_series = c(ddp_ordered_series, (y_series[6] - mean(y_series_set_of_5)) - (x_series[6] - mean(x_series_set_of_5)))
  j = j + 1
}

hist(pear)
hist(pear_ordered_series)
hist(spear)
hist(spear_ordered_series)
hist(ddp)
hist(ddp_ordered_series)
hist(crmse)
hist(crmse_ordered_series)
hist(eucl)
hist(eucl_ordered_series)
hist(manh)
hist(manh_ordered_series)

plot(crmse, ddp)
plot(crmse_ordered_series, ddp_ordered_series)
