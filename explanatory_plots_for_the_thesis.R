# plots for the thesis to depict the various series combinations
library(RColorBrewer)
library(ggplot2)
library(tikzDevice)
cbbPalette = brewer.pal(n = 12,"Paired")
##################
# parallel series
tikz('./parallel_series.tex', width = 5, height = 3.5, standAlone = TRUE)
series_1 = data.frame(values = c(1,2,3,4,5,6))
series_1$subs = c(1,2,3,4,5,6)
series_1$label = "series 1"
series_2 = data.frame(values = c(3,4,5,6,7,8))
series_2$subs = c(1,2,3,4,5,6)
series_2$label = "series 2"
data = rbind(series_1,series_2)
data
ggplot(data, aes(x = subs, y = values)) + 
  geom_point(aes(colour = label),size = 2) +
  scale_colour_manual(values = cbbPalette) +
  theme_bw() +
  labs(x = "Substituent", y = "pActivity value") +
  theme(legend.position="none") 
dev.off()


# parallel with offset
tikz('./parallel_series_offset.tex', width = 5, height = 3.5, standAlone = TRUE)
series_1 = data.frame(values = c(1,2,3,4,5,6))
series_1$subs = c(1,2,3,4,5,6)
series_1$label = "series 1"
series_2 = data.frame(values = c(10,11,12,13,14,15))
series_2$subs = c(1,2,3,4,5,6)
series_2$label = "series 2"
data = rbind(series_1,series_2)
data
ggplot(data, aes(x = subs, y = values)) + 
  geom_point(aes(colour = label),size = 2) +
  scale_colour_manual(values = cbbPalette) +
  theme_bw() +
  labs(x = "Substituent", y = "pActivity value") +
  theme(legend.position="none") 
dev.off()

# parallel with different slope
tikz('./parallel_series_slope.tex', width = 5, height = 3.5, standAlone = TRUE)
series_1 = data.frame(values = c(1,2,3,4,5,6))
series_1$subs = c(1,2,3,4,5,6)
series_1$label = "series 1"
series_2 = data.frame(values = c(3,5,7,9,11,13))
series_2$subs = c(1,2,3,4,5,6)
series_2$label = "series 2"
data = rbind(series_1,series_2)
data
ggplot(data, aes(x = subs, y = values)) + 
  geom_point(aes(colour = label),size = 2) +
  scale_colour_manual(values = cbbPalette) +
  theme_bw() +
  labs(x = "Substituent", y = "pActivity value") +
  theme(legend.position="none") 
dev.off()

# parallel with gap in both
tikz('./parallel_series_gap_both.tex', width = 5, height = 3.5, standAlone = TRUE)
series_1 = data.frame(values = c(1,2,3,7,8,9))
series_1$subs = c(1,2,3,4,5,6)
series_1$label = "series 1"
series_2 = data.frame(values = c(3,4,5,9,10,11))
series_2$subs = c(1,2,3,4,5,6)
series_2$label = "series 2"
data = rbind(series_1,series_2)
data
ggplot(data, aes(x = subs, y = values)) + 
  geom_point(aes(colour = label),size = 2) +
  scale_colour_manual(values = cbbPalette) +
  theme_bw() +
  labs(x = "Substituent", y = "pActivity value") +
  theme(legend.position="none") 
dev.off()


# parallel with gap in one
tikz('./parallel_series_gap_one.tex', width = 5, height = 3.5, standAlone = TRUE)
series_1 = data.frame(values = c(1,2,3,4,5,6))
series_1$subs = c(1,2,3,4,5,6)
series_1$label = "series 1"
series_2 = data.frame(values = c(3,4,5,9,10,11))
series_2$subs = c(1,2,3,4,5,6)
series_2$label = "series 2"
data = rbind(series_1,series_2)
data
ggplot(data, aes(x = subs, y = values)) + 
  geom_point(aes(colour = label),size = 2) +
  scale_colour_manual(values = cbbPalette) +
  theme_bw() +
  labs(x = "Substituent", y = "pActivity value") +
  theme(legend.position="none") 
dev.off()


# parallel with uncertainty
tikz('./parallel_series_uncertainty.tex', width = 5, height = 3.5, standAlone = TRUE)
series_1 = data.frame(values = c(1,2,3,4,5,6))
series_1$subs = c(1,2,3,4,5,6)
series_1$label = "series 1"
series_1$std = c(0.7,0.7,0.7,0.7,0.7,0.7)
series_2 = data.frame(values = c(2,3,4,5,6,7))
series_2$subs = c(1,2,3,4,5,6)
series_2$label = "series 2"
series_2$std = c(0.7,0.7,0.7,0.7,0.7,0.7)
limits = aes(ymax = values + std, ymin = values - std)
data = rbind(series_1,series_2)
data
ggplot(data, aes(x = subs, y = values, colour = label)) + 
  geom_point(size = 2) +
  geom_errorbar(limits, width = 0.25) +
  scale_colour_manual(values = cbbPalette) +
  theme_bw() +
  labs(x = "Substituent", y = "pActivity value") +
  theme(legend.position="none") 
dev.off()
