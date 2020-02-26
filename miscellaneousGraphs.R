#Various (unrefined) graphs relating to raw data on drugs, pregnancy illness, sperm donation, and pets

library(tidyverse)
library(breakDown)
library(ggplot2)
library(dplyr)
#clear 
rm(list = ls())
library(readr)

echoWide <- read_csv("C:\\Users\\Jacquelyn\\Downloads\\hannah_jacqueline_wide.csv")
goodData <- echoWide

#antibiotics percents
str(goodData)
count(goodData, antibiotics)
per_dataAntib <-goodData %>%
  count(goodData$antibiotics) %>%
  mutate(per = 100*n / sum(n))

percentsAntib <- per_dataAntib$per
terms <- per_dataAntib$`goodData$antibiotics`
barplot(percentsAntib,
        main = "Percent Distribution of Maternal Antibiotic Use",
        xlab = "Trimester",
        ylab = "Percent",
        names.arg = terms,
        col = "plum", ylim = c(0,100), cex.names = 0.9)

#antidepressants
str(goodData)
per_dataAntid <-goodData %>%
  count(goodData$andidepressants) %>%
  mutate(per = 100*n / sum(n))

percentsAntid <- per_dataAntid$per
terms2 <- per_dataAntid$`goodData$andidepressants`
barplot(percentsAntid,
        main = "Percent Distribution of Maternal Antidepressants Use",
        xlab = "Trimester",
        ylab = "Percent",
        names.arg = terms2,
        col = "cyan", ylim = c(0,100), cex.names = 0.9)

#antianxiety
str(goodData)
per_dataAntia <-goodData %>%
  count(goodData$antianxiety) %>%
  mutate(per = 100*n / sum(n))

percentsAntia <- per_dataAntia$per
terms3 <- per_dataAntia$`goodData$antianxiety`
barplot(percentsAntia,
        main = "Percent Distribution of Maternal Antianxiety Meds Use",
        xlab = "Trimester",
        ylab = "Percent",
        names.arg = terms3,
        col = "green", ylim = c(0,100), cex.names = 0.9)

#opioids
per_dataOp <-goodData %>%
  count(goodData$opioids) %>%
  mutate(per = 100*n / sum(n))

percentsOp <- per_dataOp$per
terms4 <- per_dataOp$`goodData$opioids`
barplot(percentsOp,
        main = "Percent Distribution of Maternal Opiods Use",
        xlab = "Trimester",
        ylab = "Percent",
        names.arg = terms4,
        col = "yellow", ylim = c(0,100), cex.names = 0.9)

#pregnancy illness 
per_dataPIll <-goodData %>%
  count(goodData$pregnancyIllnesses) %>%
  mutate(per = 100*n / sum(n))

percentsPIll <- per_dataPIll$per
terms5 <- per_dataPIll$`goodData$pregnancyIllnesses`
barplot(percentsPIll,
        main = "Percent Distribution of Pregnancy Illness",
        ylab = "Percent",
        names.arg = terms5,
        col = "lightblue", ylim = c(0,100), cex.names = 1.5)

#sperm donation
per_dataSD <-goodData %>%
  count(goodData$spermDonation) %>%
  mutate(per = 100*n / sum(n))
percentsSD <- per_dataSD$per
terms6 <- per_dataSD$`goodData$spermDonation`
barplot(percentsSD,
        main = "Percent Distribution of Sperm Donation",
        ylab = "Percent",
        names.arg = terms6,
        col = "yellow", ylim = c(0,100), cex.names = 1.5)


counts <- table(goodData$petType, useNA = "ifany")
names(counts)[is.na(names(counts))] <- "NA"
counts <- scale(counts, FALSE, colSums(counts)) * 100
barplot(counts, main="Pet Distribution",
        xlab="Pet Type", col="red",cex.names=0.84, ylim=c(0,500), decreasing = T)

percents <- per_data$per
pets <- per_data$`goodData$petType`
barplot(percents,
        main = "Percent Distribution of Pets",
        xlab = "Pet Type",
        ylab = "Percent",
        names.arg = pets,
        col = "darkred", ylim = c(0,100), cex.names = 0.75)

str(goodData)
per_data <-goodData %>%
  count(goodData$petType) %>%
  mutate(per = 100*n / sum(n) ,
         per_label = paste0(round(per*100, digits = 2)))

counts <- table(goodData$petType, useNA = "ifany")
names(counts)[is.na(names(counts))] <- "NA"
counts <- scale(counts, FALSE, colSums(counts)) * 100
barplot(counts, main="Pet Distribution",
        xlab="Pet Type", col="blue",cex.names=0.84, ylim=c(0,500))


ggplot(data=per_data, aes(x=echoWide$petType)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)


ggplot(per_data, aes(x = reorder(echoWide$petType, -per), y = per)) +
  geom_bar(stat = "identity", fill = "Pink", color = "black") + 
  geom_text(aes(label = per_label), vjust = -.025) + 
  labs(x = "Pet Type",
       y = "Percentage",
       title = "Pet Distribution") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()



counts <- table(echoWide$petType, useNA = "ifany")
names(counts)[is.na(names(counts))] <- "NA"
counts <- scale(counts, FALSE, colSums(counts)) * 100
barplot(counts, main="Pet Distribution",
        xlab="Pet Type", col="red",cex.names=0.84, ylim=c(0,500), decreasing = T)


#pets
petTypes <- echo %>%
  filter(parent_table == "Pets")
petTypes <- echo %>%
  filter(echo$metadatum == "petType")
animals <- c(petTypes$value)
ggplot(data.frame(animals), aes(x=animals, y = ..prop.., group = 1)) +
  geom_bar() + scale_y_continuous(labels = scales::percent_format())
