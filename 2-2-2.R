rm(list=ls(all=TRUE))
library(ggplot2)
library(doBy)
data <- read.csv('doing_data_science/dds_datasets/nyt1.csv')
types = c(-Inf, 17, 24, 34, 44, 54, 64, Inf)
data$agecat = cut(data$Age, types)
data$scode[data$Impressions==0] <- 'NoImps'
data$scode[data$Impressions>0] <- 'Imps'
data$scode[data$Clicks>0] <- 'Clicks'
data$scode = factor(data$scode)
ggplot(subset(data, Impressions>0), aes(x=Clicks/Impressions, colour=agecat)) + geom_density()

etable <- summaryBy(Impressions~scode+Gender+agecat, data=data, FUN = function(x){c(length(x))})
