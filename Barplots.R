rm(list=ls())
#dylyr viewing of the data
#import libraries 
library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)
#storing metadata as a variable
metadata <- read.csv("/Users/hannahdong/Downloads/hannah_jacqueline_wide.csv",header=TRUE)
#removing all the duplicates (based on subject)
goodData <- metadata[!duplicated(metadata[c("subject")]),]

#Pet Type Data
#creating a variable for pet types
pType = goodData$petType
#returns a summary of pet data
summary(pType)
#finds the levels of pet type
levels(pType)
#creates a barplot of all the animals
barplot(table(pType),main = "Animals",col = "red")

#Allergies Data
allergies = goodData$allergy
#creates barplot of all the allergies
barplot(table(allergies),xlab = "Repsponse", ylab = "Count", main = "Allergies", col = "red")

#Pre-pregnancy Weight Data
preWeight = c(goodData$motherPrePregnancyWeight)
#gets summary of preWeight Data
summary(preWeight)
#creates a boxplot
boxplot(preWeight, ylab = "Weight (lbs)",main = "Pre-pregnancy Weight of Mothers")

#Weight Gain Data
gain = goodData$weightGained
#gets summary of weight gain data
summary(gain)
#creates boxplot of weight gain data
boxplot(gain, ylab = "Weight (lbs)", main = "Weight Gain")

#Illnesses Data
illnesses = goodData$pregnancyIllnesses
#summary
summary(illnesses)
#barplot
barplot(table(illnesses), ylab = "Count", xlab = "Response", main = "Pregnancy Illnesses",col = "lightslateblue")

#Fertility Treatment Data
fTreat = goodData$fertilityTreatment
#summary
summary(fTreat)
#barplot
barplot(table(fTreat), exclude = NULL, xlab = "Response", ylab = "Count", main = "Fertility Treatment",col = "lightslateblue")

#Sperm Donation Data
sperm = goodData$spermDonation
#summary
summary(sperm)
#barplot
barplot(table(sperm), xlab = "Response", ylab = "Count", main = "Sperm Donation", col = "lightslateblue")

#Antibiotics v. Prescription Data
drugs = goodData$antibioticOrPrescription
#summary
summary(drugs)
#barplot
barplot(table(drugs), xlab = "Response", ylab = "Count", main = "Antibiotics or Prescription Drugs", names.arg = c("Neither", "Antibiotics", "Prescription"),col = "lightslateblue")

#rH Compatible Data
rh = goodData$rhCompatible
#summary
summary(rh)
#barplot
barplot(table(rh), xlab = "Response", ylab = "Count", main = "Rh Compatibility", names.arg = c("NA","No","Unknown","Yes"),col = "lightslateblue")    

#Antibiotics Data
#summary
antb = goodData$antibiotics
#barplot
barplot(table(antb), xlab = "Response", ylab = "Count", main = "Antibiotics", col = "red")
#trying to create data frame to show percentages 
percenttableNA <- data.frame((colSums(is.na(goodData))/nrow(goodData))*100)


