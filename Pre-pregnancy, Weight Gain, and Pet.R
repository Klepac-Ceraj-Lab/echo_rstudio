rm(list=ls())
#dylyr viewing of the data
#imports libraries
library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)
metadata <- read.csv("/Users/hannahdong/Downloads/petsetc.csv",header=TRUE)

#separating child Data from mother data and getting rid of duplicates (by subject)
child <- startsWith(as.character(metadata$sample),"C")
childData <- subset(metadata,child==TRUE)
betterChild <- childData[!duplicated(childData[c("subject")]),]
motherData <- subset(metadata, child==FALSE)
betterMother <- motherData[!duplicated(motherData[c("subject")]),]
noDupData <- metadata[!duplicated(metadata[c("subject")]),]

#Pre-pregnancy Weight Data from Child Sample IDs
preWeight = c(betterChild$motherPrePregnancyWeight)
#creating a boxplot and histogram
boxplot(preWeight, ylab = "Weight (lbs)",main = "Pre-pregnancy Weight of Mother")
hist(preWeight,xlab = "Weight (lbs)",main = "Pre-pregnancy Weight of Mother")
#summary
summary(preWeight)

#Weight Gain Data from Child Sample IDs 
gain = betterChild$weightGained

#removing outliers
gain = gain[gain!=240]
gain = gain[gain!=165]
gain = gain[gain!=100]
gain = gain[gain!=-15]
gain = gain[gain!= 80]
gain = gain[gain!= 75]
#summary
summary(gain)
#creating a boxplot and histogram
boxplot(gain, ylab = "Weight (lbs)", main = "Weight Gain")
hist(gain, xlab = "Weight (lbs)", main = "Weight Gain")

#Pet Data 
pType = noDupData$petType
summary(pType)
#labeling Nas and combining levels
pType = as.factor(ifelse(is.na(pType), "NA", pType))
pType = combineLevels(pType,levs = c("5","6","7"), newLabel = c("17"))
#summary of pet data with new level names 
summary(pType)
#creates new table with only two levels (cats and dogs v. no pets)
mammalsData <- subset(pType,pType==17|pType==1)
#creates a barplot with 1 = no pets and 17 = cats and dogs
barplot(table(factor(mammalsData)), xlab = "Response", ylab = "Count", main = "Pet Ownership", col = "red")
summary(mammalsData)
