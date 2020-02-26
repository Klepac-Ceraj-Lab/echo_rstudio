#This script creates various barplots and histograms related to maternal drug use, 
#including antibiotics, antidepressents, and opioids.
#Author: Jacquelyn Cai

library(plotrix)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(reshape2)
library(breakDown)
#clear 
rm(list = ls())
library(readr)
echo <- read_csv("C:\\Users\\Jacquelyn\\Downloads\\petsetc.csv")

#Plots Maternal Antibiotics Usage separated by Trimester in a Bar Chart with Axis Break
echo$antibioticsFirst <- lapply(echo$antibiotics, (function(x) grepl("First", x)))
echo$antibioticsSecond <- lapply(echo$antibiotics, (function(x) grepl("Second", x)))
echo$antibioticsThird <- lapply(echo$antibiotics, (function(x) grepl("Third", x)))
onlyAntibiotics <- echo %>% select(sample, subject, antibiotics, antibioticsFirst, antibioticsSecond, antibioticsThird)

# Remove duplicated rows based on subject and opioids
onlyAntibiotics <- onlyAntibiotics %>% distinct(subject, antibiotics, .keep_all = TRUE)
firstV <- unlist(onlyAntibiotics$antibioticsFirst)
countFirst <- sum(firstV, na.rm = TRUE)
secondV <- unlist(onlyAntibiotics$antibioticsSecond)
countSecond <- sum(secondV, na.rm = TRUE)
thirdV <- unlist(onlyAntibiotics$antibioticsThird)
countThird <- sum(thirdV, na.rm = TRUE)
totalTerms <- countFirst+countSecond+countThird
numNAsA <- sum(is.na(onlyAntibiotics$antibiotics))
counts <- c(countFirst, countSecond, countThird, totalTerms, numNAsA)
fromA <- 60
toA <- 430

#bar plot
gap.barplot(counts, gap=c(50, 400), main = "Antibiotics Usage", ytics=c(0, 10, 20, 30, 40, 430), xaxlab = c("First", "Second", "Third", "Total Terms", "NA"),
            xlab="Term Used", ylab="Number of Mothers")

#format axis break
axis.break(2, from, breakcol="snow", style="gap")
axis.break(2, from*(1+0.02), breakcol="black", style="slash")
axis.break(4, from*(1+0.02), breakcol="black", style="slash")
axis(2, at=from)

antibioticsTotal<-barplot(counts, main="Antibiotics Usage",
        names.arg=c("First", "Second", "Third", "Total Terms", "NA"), ylim=c(0,500))

#######################################################################################

#Split Bar Plot of Antibiotic Use Distribution with Duplicates
antibioticsTable <- read.table(text="Term OneTerm     TwoTerms     ThreeTerms
1    6    9    4
2    9    8    4
3    7    11    4", header=TRUE)

antibioticsTable <- melt(antibioticsTable, id.var="Term")
ggplot(antibioticsTable, aes(x = Term, y = value, fill = variable)) + 
  geom_bar(stat = "identity")

############################################################################

#Opioids Use Distribution Bar Chart with Axis Split
opData <- echo %>% select(sample, subject, opioids)
opData$opFirst <- lapply(opData$opioids, (function(x) grepl("First", x)))
opData$opSecond <- lapply(opData$opioids, (function(x) grepl("Second", x)))
opData$opThird <- lapply(opData$opioids, (function(x) grepl("Third", x)))

# Remove duplicated rows based on subject
opData <- opData %>% distinct(subject, opioids, .keep_all = TRUE)

firstVO <- unlist(opData$opFirst)
countFirstO <- sum(firstVO, na.rm = TRUE)
secondVO <- unlist(opData$opSecond)
countSecondO <- sum(secondVO, na.rm = TRUE)
thirdVO <- unlist(opData$opThird)
countThirdO <- sum(thirdVO, na.rm = TRUE)
totalTermsO <- countFirstO+countSecondO+countThirdO

numNAs <- sum(is.na(opData$opioids))
countsO <- c(countFirstO, countSecondO, countThirdO, totalTermsO, numNAs)

from <- 15
to <- 430

gap.barplot(countsO, gap=c(from,to), main = "Opioids Usage", ytics=c(2,4,6,8,10,430,450), xaxlab = c("First", "Second", "Third", "Total Terms", "NA"),
             xlab="Term Used", ylab="Number of Mothers")

axis.break(2, from, breakcol="snow", style="gap")
axis.break(2, from*(1+0.02), breakcol="black", style="slash")
axis.break(4, from*(1+0.02), breakcol="black", style="slash")
axis(2, at=from)

#######################################################################################

#Antidepressants Use Distribution Bar Chart with Axis Split
adData <- echo %>% select(sample, subject, andidepressants)
adData$adFirst <- lapply(adData$andidepressants, (function(x) grepl("First", x)))
adData$adSecond <- lapply(adData$andidepressants, (function(x) grepl("Second", x)))
adData$adThird <- lapply(adData$andidepressants, (function(x) grepl("Third", x)))

# Remove duplicated rows based on subject
adData <- adData %>% distinct(subject, andidepressants, .keep_all = TRUE)

firstVad <- unlist(adData$adFirst)
countFirstad <- sum(firstVad, na.rm = TRUE)
secondVad <- unlist(adData$adSecond)
countSecondad <- sum(secondVad, na.rm = TRUE)
thirdVad <- unlist(adData$adThird)
countThirdad <- sum(thirdVad, na.rm = TRUE)
totalTermsad <- countFirstad+countSecondad+countThirdad

numNAsAD <- sum(is.na(adData$andidepressants))
countsAD <- c(countFirstad, countSecondad, countThirdad, totalTermsad, numNAsAD)

from <- 100
to <- 400

gap.barplot(countsAD, gap=c(from,to), main = "Antidepressant Usage", ytics=c(0,30,60, 90,410), xaxlab = c("First", "Second", "Third", "Total Terms", "NA"),
            xlab="Term Used", ylab="Number of Mothers")

axis.break(2, from, breakcol="snow", style="gap")
axis.break(2, from*(1+0.02), breakcol="black", style="slash")
axis.break(4, from*(1+0.02), breakcol="black", style="slash")
axis(2, at=from)

#############################################################
#Antibiotics Use in Children vs Mother Data

child <- startsWith(as.character(echo$sample), "C")
childData <- subset(echo, child==TRUE)
childData <- childData[!duplicated(childData[c("subject")]),]
motherData <- subset(echo, child == FALSE)
motherData <- motherData[!duplicated(motherData[c("subject")]),]

#antibiotics only children
count(childData, antibiotics)
per_dataAntibChild <-
  count(childData, antibiotics) 

#Histogram for antibiotics use only in mother data
per_dataAntib <-
  count(motherData, antibiotics) 
x= per_dataAntib$antibiotics
y= per_dataAntib$n
barplot(y, main= "Antibiotic Use in Mothers", xlab= "Term", ylab= "Frequency", 
        names.arg= x, col = "plum")

#######################################################################################

#Histogram for antidepressants for mother data 
count(motherData, andidepressants)
count(childData, andidepressants)
count(echo, andidepressants)
per_dataAntid <- count(motherData, andidepressants) 
x= per_dataAntid$andidepressants
y= per_dataAntid$n
barplot(y, main= "Antidepressant Use in Mothers", xlab= "Term", ylab= "Frequency", 
        names.arg= x, col = "plum")

#Histogram for child antidepressant data
per_dataAntidChild <- count(childData$andidepressants)
x= per_dataAntidChild$x
y= per_dataAntidChild$freq
barplot(y, main= "Antidepressant Use Associated w/ Children Samples", xlab = "Term", ylab= "Frequency", 
        names.arg= x, col = "blue")


