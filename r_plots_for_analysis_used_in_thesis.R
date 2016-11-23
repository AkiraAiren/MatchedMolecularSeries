library(ggplot2)
library(tikzDevice)
library(reshape2)


############################################################################################
### Kinase analysis /// see below for protease analysis
############################################################################################
# setwd("/home/kelsier/Dokumente/MastersThesis/chembl_analysis/kinases_and_proteases")
setwd(dir = "W://Uni Innsbruck Unterlagen/Master Chemie/Masterarbeit/chembl_analysis/kinases_and_proteases/")
kicount = read.csv("./kinase_analysis/Kinase_Ki_count.csv")
iccount = read.csv("./kinase_analysis/Kinase_IC50_count.csv")
Combined = read.csv("./kinase_analysis/Kinase_combined_count.csv")

# Plot with all three counts combined - overlayed truncated
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

# Plot with all three counts combined - overlayed whole dataset
tikz("../../output_files/kinase_analysis_overlayed_whole_set.tex", width = 6, height = 3.5)
ggplot(NULL, aes(x = length, y = count)) + 
  geom_bar(stat = 'identity', alpha = 0.5, data = Combined, aes(fill = "Combined")) + 
  geom_bar(data=iccount, aes(fill = "iccount"),stat='identity', alpha = 0.5) +
  geom_bar(data=kicount, aes(fill = "kicount"),stat='identity', alpha = 0.5) +
  scale_fill_brewer(direction = 1) +
  labs(x = "Length of series", y = "Number of series found") +
  theme(axis.title.y = element_text(margin = margin(r = 20))) +
  theme_bw() +
  guides(fill=FALSE)
dev.off()

############################################################################################
### Protease analysis
############################################################################################
# setwd("/home/kelsier/Dokumente/MastersThesis/chembl_analysis/kinases_and_proteases")
kicount = read.csv("./protease_analysis/Protease_Ki_count.csv")
iccount = read.csv("./protease_analysis/Protease_IC50_count.csv")
Combined = read.csv("./protease_analysis/Protease_combined_count.csv")

# Plot with all three counts combined - overlayed truncated
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

# Plot with all three counts combined - overlayed whole dataset
tikz("../../output_files/protease_analysis_overlayed_whole_set.tex", width = 6, height = 3.5)
ggplot(NULL, aes(x = length, y = count)) + 
  geom_bar(stat = 'identity', alpha = 0.5, data = Combined, aes(fill = "Combined")) + 
  geom_bar(data=iccount, aes(fill = "iccount"),stat='identity', alpha = 0.5) +
  geom_bar(data=kicount, aes(fill = "kicount"),stat='identity', alpha = 0.5) +
  scale_fill_brewer(direction = 1) +
  labs(x = "Length of series", y = "Number of series found") +
  theme(axis.title.y = element_text(margin = margin(r = 20))) + 
  theme_bw() +
  guides(fill=FALSE)
dev.off()