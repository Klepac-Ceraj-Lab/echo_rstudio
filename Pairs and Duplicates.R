rm(list=ls())
#dylyr viewing of the data
#imports libraries
library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)
metadata <- read.csv("/Users/hannahdong/Downloads/petsetc.csv",header=TRUE)

#orders the metadata by sample ID
metadata <- metadata[order(metadata$sample),]
#creates a table with all the subjects and the frequency at which they occur
n_occur <- data.frame(table(metadata$subject))
#gives all the subjects that occur more than once
n_occur[n_occur$Freq > 1,]
#creates a table with all the duplicates 
dupData <- metadata[metadata$subject %in% n_occur$Var1[n_occur$Freq > 1],]
#gets child sample IDs
child <- startsWith(as.character(dupData$sample),"C")
#subsets the child data
childData <- subset(dupData,child==TRUE)
#remove duplicates
betterChild <- childData[!duplicated(childData[c("subject")]),]
#subsetting the mother data
motherData <- subset(dupData, child==FALSE)
#remove duplicates
betterMother <- motherData[!duplicated(motherData[c("subject")]),]
#finds the total of mothers and childs without duplicates
total <- rbind(betterChild,betterMother)
#creates a data frame with subjects that occur more than once
n_occur2 <- data.frame(table(total$subject))
n_occur2[n_occur2$Freq > 1,]
#creates a table with pairs
pairs <- total[total$subject %in% n_occur2$Var1[n_occur2$Freq > 1],]
write.csv(pairs, "/Users/hannahdong/Downloads/pairs.csv", sep="\t")

