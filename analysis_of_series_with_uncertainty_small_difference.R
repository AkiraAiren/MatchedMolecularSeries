library(ggplot2)
library(reshape2)

normalize_values = function(x,min,max) {
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
exp_err = 0.2
ntries = 10000
ddp = c()
j = 0
for (i in seq(1,ntries)) {
  x = c(0.1,0.25,0.4,0.55,0.7,0.85)
  y = c(0.1,0.25,0.4,0.55,0.7,0.85)
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
  ddp = c(ddp, y_series[6] - x_series[6])
  j = j + 1
}
summary(crmse)
summary(pear)
summary(spear)
summary(manh)
summary(eucl)
# transform the value ranges of eucl, manh, and crmse into the range from 0 to 1
eucl_normed = sapply(eucl, normalize_values,min=min(eucl),max=max(eucl))
crmse_normed = sapply(crmse, normalize_values,min=min(crmse),max=max(crmse))
manh_normed = sapply(manh, normalize_values,min=min(manh),max=max(manh))
plot(crmse)
plot(pear)
plot(spear)
plot(manh)
plot(eucl)
summary(crmse_normed)
summary(pear)
summary(spear)
summary(manh_normed)
summary(eucl_normed)
hist(crmse)
hist(pear)
hist(spear)
hist(manh)
hist(eucl)
# Plot the distribution of the normed Euclidean, Manhattan and cRMSE
## without normalization
hist_data_unnormed_unmelted = data.frame(crmse, eucl, manh)
hist_data_unnormed = melt(hist_data_unnormed_unmelted)
## with normalization
hist_data = data.frame(crmse_normed, eucl_normed, manh_normed)
hist_data_unmelted = hist_data
hist_data = melt(hist_data)
## for the pearson and the spearman values
hist_data_ps = data.frame(spear,pear)
hist_data_ps_melted = melt(hist_data_ps)
# density plot of the three similarity values - not normalized 
ggplot(hist_data_unnormed, aes(value,fill = variable)) + geom_density(alpha=0.2) + 
  labs(title = "Distribution of not normalized values", x = "Absolute Values", y = "Density") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(2)), 
        axis.text = element_text(size = rel(2)))
# density plot of the three similarity values - normed values
ggplot(hist_data, aes(value,fill = variable)) + geom_density(alpha=0.2) + 
  labs(title = "Distribution of normalized values", x = "Normalized Values", y = "Density") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(2)), 
        axis.text = element_text(size = rel(2)))
# density plot of the three similarity values with normal distribution (z_scores) - normed values
ggplot(hist_data, aes(value,fill = variable)) + geom_density(alpha=0.2) + 
  labs(title = "Distribution of normalized values", x = "Normalized Values", y = "Density") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(2)), 
        axis.text = element_text(size = rel(2))) + 
  stat_function(fun = dnorm, args = list(mean=mean(hist_data_unmelted$crmse_normed), sd = sd(hist_data_unmelted$crmse_normed)), size =1, colour = "red") + 
  stat_function(fun = dnorm, args = list(mean=mean(hist_data_unmelted$eucl_normed), sd = sd(hist_data_unmelted$eucl_normed)), size =1, colour = "green") + 
  stat_function(fun = dnorm, args = list(mean=mean(hist_data_unmelted$manh_normed), sd = sd(hist_data_unmelted$manh_normed)), size =1, colour = "blue")
# density plot of the pearson and the spearman values
ggplot(hist_data_ps_melted, aes(value, fill = variable)) + geom_density(alpha = 0.2) + 
  labs(title = "Distribution of correlation values", x = "Values", y = "Density") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(2)), 
        axis.title = element_text(size = rel(2)), 
        axis.text = element_text(size = rel(2)))