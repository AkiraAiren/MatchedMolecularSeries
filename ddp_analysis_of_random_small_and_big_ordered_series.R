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
  ddp = c(ddp, series_2[6] - series_1[6])
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
  ddp_rb = c(ddp_rb, series_2_rb[6] - series_1_rb[6])
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
  x = c(0.16,0.32,0.48,0.64,0.80,0.96)
  y = c(0.16,0.32,0.48,0.64,0.80,0.96)
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
  ddp_ordered_series = c(ddp_ordered_series, y_series[6] - x_series[6])
  j = j + 1
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
  ddp_ordered_series_big = c(ddp_ordered_series_big, y_series[6] - x_series[6])
}
plot(crmse_ordered_series,ddp_ordered_series)
plot(crmse_ordered_series_big,ddp_ordered_series_big)
plot(crmse,ddp)
plot(crmse_rb, ddp_rb)