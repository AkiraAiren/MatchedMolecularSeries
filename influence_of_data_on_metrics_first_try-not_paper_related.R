install.packages('ggplot2')
library(ggplot2)

# sampling distribution
# --> all numbers are evenly drawn
chosen = c()
x = c(1,2,3,4,5,6,7,8,9,10)
for (i in seq(0, 100000)) {
  chosen = c(chosen, sample(x,1))
}
hist(chosen)


y = c(1,2,3,4,5,6,7,8)
for (i in seq(1,length(y))){
  x = c(1,2,3,4,5,6,7,8)
  sampled = sample(x, 2)
  x[sampled[1]] = sampled[2]
  x[sampled[2]] = sampled[1]
}

# Random tries on how the cRMSE is influenced by alterations to its series
x = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
# x = c(1,2,3,4,5,6,7,9,8,10)
y = c(1.0,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.1)
# y = c(1,2,3,4,5,6,7,8,9,10)
nvariables = length(x)
x_series = x - mean(x)
y_series = y - mean(y)
# calculate cRMSE
sqrt(sum((y_series-x_series)**2)/length(x))


ntries = 10000
nvariables = 10
exp_err = 0.2
eucl = c()
man = c()
cram = c()
crmse = c()
pear = c()

# How does the cRMSE progress with a growing series?
### No, the series are centered to the mean. Therefore,
### as long as a vector with same values is added to the 
### initial series, no effect (other than the rounding error)
### can be observed/expected. 
crmse = c()
ntries = 100
x = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
y = c(1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)
y_series = y - mean(y)
j = 0
for (i in seq(1,ntries)) {
  x = x + c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1) * j
  x_series = x - mean(x)
  print(x_series)
  crmse = c(crmse, sqrt(sum((y_series - x_series)**2)/length(x)))
  j = j + 1
}
plot(crmse)

# Wich influence does the alteration of one variable have? 
# Not the exchange but the actual alteration of one value with
# a value of the normal distribution
ntries = 10000
nvariables = 10
exp_err = 0.2
crmse = c()
pear = c()
j = 10
for (i in seq(1,ntries)){
  x = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
  y = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
  sampled = sample(x, 1)
  subtracted = sampled - rnorm(2,sd=exp_err)
  x[which(x == sampled)] = subtracted[1]
  #x[sampled[2]] = subtracted[2]
  x_series = x - mean(x)
  y_series = y - mean(y)
  crmse = c(crmse,sqrt(sum((y_series-x_series)**2)/nvariables))
  print(x)
  print(y)
  pear = c(pear, cor(y_series,x_series, method='pearson'))
}
plot(crmse)
hist(crmse)
plot(pear)

# Check what influence the permutation of two positions has on the crmse
ntries = 100000
crmse = c()
dist_x = c()
dist_y = c()
pos = c(1,2,3,4,5,6,7,8,9,10)
for (i in seq(1,ntries)){
  x = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
  # x = c(1,2,3,4,5,6,7,8,9,10)
  y = c(1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)
  # y = c(1,2,3,4,5,6,7,8,9,10)
  nvariables = length(x)
  sampled = sample(pos, 2)
  x_sampled = x[which(x == sampled[1])]
  x[which(x == sampled[1])] = x[which(x == sampled[2])]
  x[which(x == sampled[2])] = x_sampled
  dist_x = c(dist_x, abs(sampled[1] - sampled[2]))
#   sampled = sample(pos, 2)
#   y_sampled = y[sampled[1]]
#   y[sampled[1]] = y[sampled[2]]
#   y[sampled[2]] = y_sampled
  x_series = x - mean(x)
  y_series = y - mean(y)
  # dist_y = c(dist_y, abs(sampled[1] - sampled[2]))
  crmse = c(crmse,sqrt(sum((y_series-x_series)**2)/nvariables))
}
plot(crmse)
hist(crmse)
plot(dist_x, crmse)
ggplot(data.frame(crmse), aes(x=crmse)) + 
  geom_histogram(fill = "white", colour = "black") + 
  labs(title = "Influence on the cRMSE of permutating one series", x = "cRMSE", y = "Count") +
  theme_bw() + #has to be placed before the other theme definitions
  theme(plot.title = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(2)), 
        axis.text = element_text(size = rel(2)))

# how is the cRMSE influenced by the uncertainty?
ntries = 10000
exp_err = 0.2
crmse = c()
for (i in seq(1,ntries)){
  # a = c(1,2,3,4,5,6,7,8,9,10)
  a = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.10)
  a_n = rnorm(length(a),sd=exp_err)
  b_n = rnorm(length(a),sd=exp_err)
  a = a + a_n
  b = a + b_n
  ab = list(a,b)
  a_series = a - mean(a)
  b_series = b - mean(b)
  crmse = c(crmse,sqrt(sum((b_series-a_series)**2)/length(a)))
}
plot(crmse)
hist(crmse)
ggplot(data.frame(crmse), aes(x=crmse)) + 
  geom_histogram(fill = "white", colour = "black") + 
  labs(title = "Influence of uncertainty on cRMSE", x = "cRMSE", y = "Count") +
  theme_bw() + #has to be placed before the other theme definitions
  theme(plot.title = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(2)), 
        axis.text = element_text(size = rel(2)))



#### Is the influence of the uncertainty independent of the value range? ####
## Write the loop for the crmse_2 values new!!
### These plots are already finished in another script!
cRMSE_broad = c()
cRMSE_2 = c()
cRMSE_3 = c()
cRMSE_4 = c()
cRMSE_narrow = c()

pear_broad = c()
pear_2 = c()
pear_3 = c()
pear_4 = c()
pear_narrow = c()

ntries = 1000
for (i in seq(1,ntries)){
  a = c(5,6,7,8,9)
  a_n = rnorm(length(a),sd=exp_err)
  b_n = rnorm(length(a),sd=exp_err)
  a = a + a_n
  b = a + b_n
  ab = list(a,b)
  a_series = a - mean(a)
  b_series = b - mean(b)
  
  cRMSE_broad = c(cRMSE_broad,sqrt(sum((b_series-a_series)**2)/length(a)))
  pear_broad = c(pear_broad, cor(a_series,b_series, method='pearson'))
}

for (i in seq(1,ntries)){
  a = c(5.75,6.5,7.25,8.0,8.75)
  a_n = rnorm(length(a),sd=exp_err)
  b_n = rnorm(length(a),sd=exp_err)
  a = a + a_n
  b = a + b_n
  ab = list(a,b)
  a_series = a - mean(a)
  b_series = b - mean(b)
  cRMSE_2 = c(cRMSE_2,sqrt(sum((b_series-a_series)**2)/length(a)))
  pear_2 = c(pear_2, cor(a_series,b_series, method='pearson'))
}

for (i in seq(1,ntries)){
  a = c(5.5,6.0,6.5,7.0,7.5)
  a_n = rnorm(length(a),sd=exp_err)
  b_n = rnorm(length(a),sd=exp_err)
  a = a + a_n
  b = a + b_n
  ab = list(a,b)
  a_series = a - mean(a)
  b_series = b - mean(b)
  cRMSE_3 = c(cRMSE_3,sqrt(sum((b_series-a_series)**2)/length(a)))
  pear_3 = c(pear_3, cor(a_series,b_series, method='pearson'))
}

for (i in seq(1,ntries)){
  a = c(5.2,5.4,5.6,5.8,6.0)
  a_n = rnorm(length(a),sd=exp_err)
  b_n = rnorm(length(a),sd=exp_err)
  a = a + a_n
  b = a + b_n
  ab = list(a,b)
  a_series = a - mean(a)
  b_series = b - mean(b)
  cRMSE_4 = c(cRMSE_4,sqrt(sum((b_series-a_series)**2)/length(a)))
  pear_4 = c(pear_4, cor(a_series,b_series, method='pearson'))
}

for (i in seq(1,ntries)){
  a = c(5.1,5.2,5.3,5.4,5.6)
  a_n = rnorm(length(a),sd=exp_err)
  b_n = rnorm(length(a),sd=exp_err)
  a = a + a_n
  b = a + b_n
  ab = list(a,b)
  a_series = a - mean(a)
  b_series = b - mean(b)
  cRMSE_narrow = c(cRMSE_narrow,sqrt(sum((b_series-a_series)**2)/length(a)))
  pear_narrow = c(pear_narrow, cor(a_series,b_series, method='pearson'))
}

corrs = data.frame(pear_broad,
                   pear_2,
                   pear_3,
                   pear_4,
                   pear_narrow,
                   cRMSE_narrow,
                   cRMSE_2,
                   cRMSE_3,
                   cRMSE_4,
                   cRMSE_broad)
ggplot(corrs) + geom_density(aes(pear_narrow),size=1.1) + 
  geom_density(aes(pear_broad),colour="red",size=1.1) +
  geom_density(aes(pear_2),colour="green",size=1.1) + 
  geom_density(aes(pear_3),colour="yellow",size=1.1) + 
  geom_density(aes(pear_4),colour="orange",size=1.1) +
  labs(title = "Influence of value range and uncertainty Pearson", x = "Series", y = "Density") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(2)), 
        axis.text = element_text(size = rel(2)))
  
ggplot(corrs) + geom_density(aes(cRMSE_narrow),size=1.1) + 
  geom_density(aes(cRMSE_narrow),colour="blue",size=1.1) +
  geom_density(aes(cRMSE_2),colour="green",size=1.1) + 
  geom_density(aes(cRMSE_3),colour="yellow",size=1.1) + 
  geom_density(aes(cRMSE_4),colour="orange",size=1.1) +
  theme_bw() +
  labs(title = "Influence of value range and uncertainty cRMSE", x = "Series", y = "Density") +
  theme(plot.title = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(2)), 
        axis.text = element_text(size = rel(2)))
