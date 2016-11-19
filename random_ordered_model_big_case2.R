# ddp calculation for random series and for series with small difference in pairs and big difference in pairs
library(ggplot2)
library(reshape2)

normalize_values = function(x,min,max) {
  return((x - min)/(max-min))
}

# get 12000 random values between 0 and 1 for 6000 pairs of random series.
rvector_random_big = c()
while (length(rvector_random_big) != 12000) {
  temp = rnorm(1, mean = 15, sd = 15)
  if (temp >= 5) {
    if (temp <= 20) {
      rvector_random_big = c(rvector_random_big,temp)
    }
  }  
}

# extract 6000 pairs of series from the random 12000 values.
# calculate the similarity of these pairs to have a baseline for the similarity model of big differences. 
crmse_rb = c()
pear_rb = c()
spear_rb = c()
manh_rb = c()
eucl_rb = c()
ddp_rb = c()
for (i in seq(1,12000,12)) {
  series_1_rb = c()
  for (j in seq(i,i+5)) {
    series_1_rb = c(series_1_rb, rvector_random_big[j])
  }
  series_2_rb = c()
  for (j in seq(i+6,i+11)) {
    series_2_rb = c(series_2_rb, rvector_random_big[j])
  }
  series_1_set_of_5_rb = series_1_rb[0:5]
  series_2_set_of_5_rb = series_2_rb[0:5]
  x_y = rbind(series_1_set_of_5_rb, series_2_set_of_5_rb)
  pear_rb = c(pear_rb, cor(series_2_set_of_5_rb, series_1_set_of_5_rb, method='pearson'))
  spear_rb = c(spear_rb, cor(series_2_set_of_5_rb, series_1_set_of_5_rb, method='spearman'))
  manh_rb = c(manh_rb,dist(x_y, method = "manhattan"))
  eucl_rb = c(eucl_rb,dist(x_y, method = "euclidean"))
  y_series_rb = series_2_set_of_5_rb - mean(series_2_set_of_5_rb)
  x_series_rb = series_1_set_of_5_rb - mean(series_1_set_of_5_rb)
  crmse_rb = c(crmse_rb, sqrt(sum((y_series_rb - x_series_rb)**2)/length(x_series_rb)))
  series_1_rb = series_1_rb - mean(series_1_rb)
  series_2_rb = series_2_rb - mean(series_2_rb)
  ddp_rb = c(ddp_rb, (series_2_rb[6] - mean(series_2_set_of_5_rb)) - (series_1_rb[6] - mean(series_1_set_of_5_rb)))
}
# ddp for series with big difference between the pairs
crmse_ordered_series_big = c()
pear_ordered_series_big = c()
spear_ordered_series_big = c()
manh_ordered_series_big = c()
eucl_ordered_series_big = c()
x_series_vector = c()
y_series_vector = c()
exp_err = 0.2
ntries = 1000
ddp_ordered_series_big = c()
for (i in seq(1,ntries)) {
  x = c(5,8,11,14,17,20)
  y = c(5,8,11,14,17,20)
  random_values_x = rnorm(6,sd=exp_err)
  random_values_y = rnorm(6,sd=exp_err)
  x = x + random_values_x
  y = y + random_values_y
  x_set_of_5 = x[0:5]
  y_set_of_5 = y[0:5]
  x_y = rbind(x_set_of_5, y_set_of_5)
  pear_ordered_series_big = c(pear_ordered_series_big, cor(y_set_of_5, x_set_of_5, method='pearson'))
  spear_ordered_series_big = c(spear_ordered_series_big, cor(y_set_of_5, x_set_of_5, method='spearman'))
  manh_ordered_series_big = c(manh_ordered_series_big,dist(x_y, method = "manhattan"))
  eucl_ordered_series_big = c(eucl_ordered_series_big,dist(x_y, method = "euclidean"))
  y_series_set_of_5 = y_set_of_5 - mean(y_set_of_5)
  x_series_set_of_5 = x_set_of_5 - mean(x_set_of_5)
  crmse_ordered_series_big = c(crmse_ordered_series_big, sqrt(sum((y_series_set_of_5 - x_series_set_of_5)**2)/length(x_set_of_5)))
  x_series = x
  y_series = y
  ddp_ordered_series_big = c(ddp_ordered_series_big, (y_series[6] - mean(y_series_set_of_5)) - (x_series[6] - mean(x_series_set_of_5)))
}
hist(rvector_random_big)
hist(pear_rb)
hist(pear_ordered_series_big)
hist(spear_rb)
hist(spear_ordered_series_big)
hist(ddp_rb)
hist(ddp_ordered_series_big)
hist(crmse_rb)
hist(crmse_ordered_series_big)
hist(eucl_rb)
hist(eucl_ordered_series_big)
hist(manh_rb)
hist(manh_ordered_series_big)

plot(crmse_rb, ddp_rb)
plot(crmse_ordered_series_big, ddp_ordered_series_big)
