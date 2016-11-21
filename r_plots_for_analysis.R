library(ggplot2)
library(tikzDevice)
library(reshape2)


############################################################################################
### Kinase analysis /// see below for protease analysis
############################################################################################
setwd("/home/kelsier/Dokumente/MastersThesis/chembl_analysis/kinases_and_proteases")
kicount = read.csv("./kinase_analysis/Kinase_Ki_count.csv")
iccount = read.csv("./kinase_analysis/Kinase_IC50_count.csv")
Combined = read.csv("./kinase_analysis/Kinase_combined_count.csv")

# plots for particular activity and combination
ggplot(data=kicount, aes(x = length, y = count)) + 
  geom_bar(stat = 'identity') + 
  xlim(0,70) +
  ylim(0,16000)
ggplot(data=iccount, aes(x = length, y = count)) + 
  geom_bar(stat = 'identity') + 
  xlim(0,70) +
  ylim(0,16000) 
ggplot(data=Combined, aes(x = length, y = count)) + 
  geom_bar(stat = 'identity') + 
  xlim(0,70) +
  ylim(0,31000)

# plot with all three combined - stacked
tikz("../../output_files/kinase_analysis_stacked.tex", width = 6, height = 3.5)
merged_1 = merge(kicount,iccount,by="length",all = TRUE)
merged_2 = merge(Combined, merged_1,by="length", all = TRUE)
melted_merge = melt(merged_2, id="length")
melted_merge_no_na = na.exclude(melted_merge)
ggplot(data = melted_merge_no_na, aes(x=length, y=value, fill=factor(variable))) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_brewer(direction = 1) +
  xlim(0,30) +
  labs(x = "Length of series", y = "Number of series found") +
  theme(axis.title.y = element_text(margin = margin(r = 20))) + 
  theme_bw() +
  guides(fill=FALSE)
dev.off()

# Plot with all three counts combined - overlayed
tikz("../../output_files/kinase_analysis_overlayed.tex", width = 6, height = 3.5)
ggplot(NULL, aes(x = length, y = count)) + 
  geom_bar(stat = 'identity', alpha = 0.5, data = Combined, aes(fill = "Combined")) + 
  geom_bar(data=iccount, aes(fill = "iccount"),stat='identity', alpha = 0.5) +
  geom_bar(data=kicount, aes(fill = "kicount"),stat='identity', alpha = 0.5) +
  scale_fill_brewer(direction = 1) +
  xlim(0,30) +
  labs(x = "Length of series", y = "Number of series found") +
  theme(axis.title.y = element_text(margin = margin(r = 20))) +
  theme_bw() +
  guides(fill=FALSE)
dev.off()
##### Plots for appendix - to show the existance of the lengths around 67
# plot with all three combined - stacked
tikz("../../output_files/kinase_analysis_stacked_log.tex", width = 6, height = 3.5)
merged_1 = merge(kicount,iccount,by="length",all = TRUE)
merged_2 = merge(Combined, merged_1,by="length", all = TRUE)
melted_merge = melt(merged_2, id="length")
melted_merge_no_na = na.exclude(melted_merge)
ggplot(data = melted_merge_no_na, aes(x=length, y=value, fill=factor(variable))) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_brewer(direction = 1) +
  scale_y_log10() +
  labs(x = "Length of series", y = "Number of series found / log 10 scale") +
  theme(axis.title.y = element_text(margin = margin(r = 20))) + 
  theme_bw() +
  guides(fill=FALSE)
dev.off()
# Plot with all three counts combined - overlayed
tikz("../../output_files/kinase_analysis_overlayed_log.tex", width = 6, height = 3.5)
loggedic50 = iccount
loggedki = kicount
loggedcomb = Combined
loggedic50$count = log10(iccount$count)
loggedic50$length = iccount$length
loggedki$count = log10(kicount$count)
loggedki$length = kicount$length
loggedcomb$count = log10(Combined$count)
loggedcomb$length = Combined$length
ggplot(NULL, aes(x = length, y = count)) + 
  geom_bar(stat = 'identity', alpha = 0.5, data = loggedcomb, aes(fill = "loggedcomb")) + 
  geom_bar(data=loggedic50, aes(fill = "loggedic50"),stat='identity', alpha = 0.5) +
  geom_bar(data=loggedki, aes(fill = "loggedki"),stat='identity', alpha = 0.5) +
  scale_fill_brewer(direction = 1) +
  labs(x = "Length of series", y = "Number of series found / log 10 scale") +
  theme(axis.title.y = element_text(margin = margin(r = 20))) + 
  theme_bw() +
  guides(fill=FALSE)
dev.off()
############################################################################################
### Protease analysis
############################################################################################
setwd("/home/kelsier/Dokumente/MastersThesis/chembl_analysis/kinases_and_proteases")
kicount = read.csv("./protease_analysis/Protease_Ki_count.csv")
iccount = read.csv("./protease_analysis/Protease_IC50_count.csv")
Combined = read.csv("./protease_analysis/Protease_combined_count.csv")

# plots for particular activity and combination

ggplot(data=kicount, aes(x = length, y = count)) + 
  geom_bar(stat = 'identity') + 
  xlim(0,70) +
  theme_bw(axis.title.y = element_text(margin = margin(r = 20)))
ggplot(data=iccount, aes(x = length, y = count)) + 
  geom_bar(stat = 'identity') + 
  xlim(0,70) +
  theme_bw(axis.title.y = element_text(margin = margin(r = 20)))
ggplot(data=Combined, aes(x = length, y = count)) + 
  geom_bar(stat = 'identity') + 
  xlim(0,70) +
  theme_bw(axis.title.y = element_text(margin = margin(r = 20)))

# plot with all three combined - stacked
tikz("../../output_files/protease_analysis_stacked.tex", width = 6, height = 3.5)
merged_1 = merge(kicount,iccount,by="length",all = TRUE)
merged_2 = merge(Combined, merged_1,by="length", all = TRUE)
melted_merge = melt(merged_2, id="length")
melted_merge_no_na = na.exclude(melted_merge)
ggplot(data = melted_merge_no_na, aes(x=length, y=value, fill=factor(variable))) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_brewer(direction = 1) +
  xlim(0,30) +
  labs(x = "Length of series", y = "Number of series found") +
  theme(axis.title.y = element_text(margin = margin(r = 20))) + 
  theme_bw() +
  guides(fill=FALSE)
dev.off()

# Plot with all three counts combined - overlayed
tikz("../../output_files/protease_analysis_overlayed.tex", width = 6, height = 3.5)
ggplot(NULL, aes(x = length, y = count)) + 
  geom_bar(stat = 'identity', alpha = 0.5, data = Combined, aes(fill = "Combined")) + 
  geom_bar(data=iccount, aes(fill = "iccount"),stat='identity', alpha = 0.5) +
  geom_bar(data=kicount, aes(fill = "kicount"),stat='identity', alpha = 0.5) +
  scale_fill_brewer(direction = 1) +
  xlim(0,30) +
  labs(x = "Length of series", y = "Number of series found") +
  theme(axis.title.y = element_text(margin = margin(r = 20))) + 
  theme_bw() +
  guides(fill=FALSE)
dev.off()

##### Plots for appendix - to show the existance of the lengths around 67
# plot with all three combined - stacked
tikz("../../output_files/protease_analysis_stacked_log.tex", width = 6, height = 3.5)
merged_1 = merge(kicount,iccount,by="length",all = TRUE)
merged_2 = merge(Combined, merged_1,by="length", all = TRUE)
melted_merge = melt(merged_2, id="length")
melted_merge_no_na = na.exclude(melted_merge)
ggplot(data = melted_merge_no_na, aes(x=length, y=value, fill=factor(variable))) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_brewer(direction = 1) +
  scale_y_log10() +
  labs(x = "Length of series", y = "Number of series found  / log 10 scale") +
  theme(axis.title.y = element_text(margin = margin(r = 20))) + 
  theme_bw() +
  guides(fill=FALSE)
dev.off()
# Plot with all three counts combined - overlayed
tikz("../../output_files/protease_analysis_overlayed_log.tex", width = 6, height = 3.5)
loggedic50 = iccount
loggedki = kicount
loggedcomb = Combined
loggedic50$count = log10(iccount$count)
loggedic50$length = iccount$length
loggedki$count = log10(kicount$count)
loggedki$length = kicount$length
loggedcomb$count = log10(Combined$count)
loggedcomb$length = Combined$length
ggplot(NULL, aes(x = length, y = count)) + 
  geom_bar(stat = 'identity', alpha = 0.5, data = loggedcomb, aes(fill = "loggedcomb")) + 
  geom_bar(data=loggedic50, aes(fill = "loggedic50"),stat='identity', alpha = 0.5) +
  geom_bar(data=loggedki, aes(fill = "loggedki"),stat='identity', alpha = 0.5) +
  scale_fill_brewer(direction = 1) +
  labs(x = "Length of series", y = "Number of series found / log 10 scale") +
  theme(axis.title.y = element_text(margin = margin(r = 20))) + 
  theme_bw() +
  guides(fill=FALSE)
dev.off()