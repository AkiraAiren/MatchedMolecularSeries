library(ggplot2)
library(reshape2)

normalize_values = function(x,min,max) {
  return((x - min)/(max-min))
}

# How do the metrics perform when measuring gapped series
crmse = c()
pear = c()
spear = c()
manh = c()
eucl = c()
x_series_vector = c()
y_series_vector = c()
ntries = 1000

j = 0
for (i in seq(1,ntries)) {
  x = c(5,6,7,8,9,10)
  y = c(5,6,7,8,9,10)
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


# How do the metrics perform when measuring gapped series where only one is gapped. 
crmse = c()
pear = c()
spear = c()
manh = c()
eucl = c()
x_series_vector = c()
y_series_vector = c()
ntries = 1000

j = 0
for (i in seq(1,ntries)) {
  x = c(5,6,7,10,11,12)
  y = c(5,6,7,8,9,10)
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