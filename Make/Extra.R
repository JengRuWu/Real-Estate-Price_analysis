library(dplyr)
library(ggplot2)
setwd("~/Desktop/R Assignment/Make")
glimpse(data_adult)
continuous <-select_if(data_adult, is.numeric)
summary(continuous)

library(GGally)
# Convert data to numeric
corr <- data.frame(lapply(data_adult, as.integer))
# Plot the graph
ggcorr(corr,
method = c("pairwise", "spearman"),
nbreaks = 6,
hjust = 0.8,
label = TRUE,
label_size = 3,
color = "grey50")