rm(list=ls())
#dylyr viewing of the data
library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)
metadata <- read.csv("/Users/hannahdong/Downloads/petsetc.csv",header=TRUE)

#Looks at pre-pregnancy data and creates a boxplot and histogram with outliers and without outliers.
goodData <- metadata[!duplicated(metadata[c("subject")]),]
preWeight <- goodData$motherPrePregnancyWeight
summary(preWeight)
boxplot(preWeight, ylab = "Weight (lbs)",main = "Pre-pregnancy Weight of Mother")
hist(preWeight,xlab = "Weight (lbs)",main = "Pre-pregnancy Weight of Mother")
gain = goodData$weightGained
summary(gain)
gain = gain[gain!=369]
gain = gain[gain!=240]
gain = gain[gain!=165]
gain = gain[gain!=100]
gain = gain[gain!=-40]
gain = gain[gain!=-21]
boxplot(gain, ylab = "Weight (lbs)", main = "Weight Gain")
hist(gain,xlab = "Weight (lbs)", main = "Weight Gain")
summary(gain)
