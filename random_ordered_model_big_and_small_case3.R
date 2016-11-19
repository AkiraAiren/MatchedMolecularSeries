# ddp calculation for random series and for series with small difference in pairs and big difference in pairs
library(ggplot2)
library(reshape2)

normalize_values = function(x,min,max) {
  return((x - min)/(max-min))
}

# get 12000 random values between 0 and 1 for 6000 pairs of random series.
rvector_random = c()
while (length(rvector_random) != 12000) {
  if (length(rvector_random) <= 2000) {
    temp = rnorm(1, mean = 0.5, sd = 4)
    if (temp >= 0) {
      if (temp <= 1) {
        rvector_random = c(rvector_random, temp)
      }
    }  
  }
  if (length(rvector_random) > 2000 & length(rvector_random) <= 4000) {
    temp = rnorm(1, mean = 3, sd = 6.2)
    if (temp >= 1) {
      if (temp <= 4) {
        rvector_random = c(rvector_random, temp)
      }
    }    
  }
  if (length(rvector_random) > 4000 & length(rvector_random) <= 6000) {
    temp = rnorm(1, mean = 6, sd = 8.4)
    if (temp >= 2) {
      if (temp <= 8) {
        rvector_random = c(rvector_random, temp)
      }
    }    
  }
  if (length(rvector_random) > 6000 & length(rvector_random) <= 8000) {
    temp = rnorm(1, mean = 9, sd = 10.6)
    if (temp >= 3) {
      if (temp <= 12) {
        rvector_random = c(rvector_random, temp)
      }
    }    
  }
  if (length(rvector_random) > 8000 & length(rvector_random) <= 10000) {
    temp = rnorm(1, mean = 12, sd = 12.8)
    if (temp >= 4) {
      if (temp <= 16) {
        rvector_random = c(rvector_random, temp)
      }
    }    
  }
  if (length(rvector_random) > 10000 & length(rvector_random) <= 12000) {
    temp = rnorm(1, mean = 15, sd = 15)
    if (temp >= 5) {
      if (temp <= 20) {
        rvector_random = c(rvector_random, temp)
      }
    }    
  }
}
# To actually get random series with value taken from the whole range, another sampling
# step is conducted during the similarity calculation. 
crmse_rb = c()
pear_rb = c()
spear_rb = c()
manh_rb = c()
eucl_rb = c()
ddp_rb = c()
for (i in seq(1,6000)) {
  series_1_rb = sample(rvector_random, 6)
  series_2_rb = sample(rvector_random, 6)
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



# ordered series that span the whole range of values from 0 to 20. Goal is to show 
# that the distribution or ddp and crmse values for these series show the 
# characterisic cone we want to see when we rank heterogenous series.
crmse_ordered = c()
pear_ordered = c()
spear_ordered = c()
manh_ordered = c()
eucl_ordered = c()
ddp_ordered = c()
exp_err = 0.2
ntries = 1000
small_series = c(0.16,0.32,0.48,0.64,0.80,0.96)
int_1_series = c(1,1.6,2.4,3,3.6,4)
int_2_series =c(2,3.2,4.4,5.6,6.8,8)
int_3_series = c(3,4.8,6.6,8.4,10,12)
int_4_series = c(4,6.4,8.8,11.2,13.6,16)
big_series = c(5,8,11,14,17,20)
series_vector = list(small_series, int_1_series, int_2_series, int_3_series, int_4_series, big_series)
for (series in series_vector) {
  for (i in seq(1,ntries)) {
    x = series
    y = series
    random_values_x = rnorm(6,sd=exp_err)
    random_values_y = rnorm(6,sd=exp_err)
    x = x + random_values_x
    y = y + random_values_y
    x_set_of_5 = x[0:5]
    y_set_of_5 = y[0:5]
    x_y = rbind(x_set_of_5, y_set_of_5)
    pear_ordered = c(pear_ordered, cor(y_set_of_5, x_set_of_5, method='pearson'))
    spear_ordered = c(spear_ordered, cor(y_set_of_5, x_set_of_5, method='spearman'))
    manh_ordered = c(manh_ordered,dist(x_y, method = "manhattan"))
    eucl_ordered = c(eucl_ordered,dist(x_y, method = "euclidean"))
    y_series_set_of_5 = y_set_of_5 - mean(y_set_of_5)
    x_series_set_of_5 = x_set_of_5 - mean(x_set_of_5)
    crmse_ordered = c(crmse_ordered, sqrt(sum((y_series_set_of_5 - x_series_set_of_5)**2)/length(x_set_of_5)))
    x_series = x
    y_series = y
    ddp_ordered = c(ddp_ordered, (y_series[6] - mean(y_series_set_of_5)) - (x_series[6] - mean(x_series_set_of_5)))
  }
}

# create a combination of these datasets to obtain the cone form
summed_crmse = c(crmse_ordered, crmse_rb)
summed_ddp = c(ddp_ordered, ddp_rb)

# commands for the plots
hist(pear_rb)
hist(pear_ordered)
hist(spear_rb)
hist(spear_ordered)
hist(ddp_rb)
hist(ddp_ordered)
hist(crmse_rb)
hist(crmse_ordered)
hist(eucl_rb)
hist(eucl_ordered)
hist(manh_rb)
hist(manh_ordered)

plot(crmse_rb, ddp_rb)
plot(crmse_ordered, ddp_ordered)
plot(summed_crmse, summed_ddp)
