#Performs various filtering to categorize subjects based on breastfeeding type
library(tidyverse)
library(dplyr)
library(ggplot2)
#clear 
rm(list = ls())
library(readr)
#change column type to characters instead of default double
merged <- read_csv("C:\\Users\\Jacquelyn\\Downloads\\merged.csv", col_types = list(sample = col_character(), studyID = col_double(), timepoint = col_double(), metadatum = col_character(), value = col_character(), parent_table = col_character()))

breastMerged <- merged %>%
  filter(parent_table == "BreastfeedingDone" | parent_table == "BreastfeedingStill")

#done <- breastMerged %>% filter()
still <- c(" A1", "A2 ", "A3", "A4", "A5 ")

stillTable <- breastMerged %>% filter(parent_table == "BreastfeedingStill")
doneTable <- breastMerged %>% filter(parent_table == "BreastfeedingDone") 
still <- c(stillTable$subject)
done <-c(doneTable$subject)
bothStillAndDone <- intersect(still,done)

feedStill <- stillTable %>%  filter(metadatum == "numberFormulaFeedsPerDay" 
                                    | metadatum == "typicalNumberOfEpressedMilkFeeds" | 
                                     metadatum == "typicalNumberOfFeedsFromBreast") 
spreadFeedStill <- spread(feedStill, "metadatum", "value") 
spreadFeedStill[is.na(spreadFeedStill)] <- 0

spreadFeedStill$typicalNumberOfEpressedMilkFeeds <- as.numeric(spreadFeedStill$typicalNumberOfEpressedMilkFeeds)
spreadFeedStill$typicalNumberOfFeedsFromBreast <- as.numeric(spreadFeedStill$typicalNumberOfFeedsFromBreast)
spreadFeedStill$numberFormulaFeedsPerDay <- as.numeric(spreadFeedStill$numberFormulaFeedsPerDay)

spreadFeedStill$percent <- (spreadFeedStill$typicalNumberOfEpressedMilkFeeds + spreadFeedStill$typicalNumberOfFeedsFromBreast)/(spreadFeedStill$numberFormulaFeedsPerDay + spreadFeedStill$typicalNumberOfEpressedMilkFeeds + spreadFeedStill$typicalNumberOfFeedsFromBreast) * 100

exclusiveBFTable <- spreadFeedStill %>% filter(percent >= 80)
exclusiveBFSubs <- exclusiveBFTable$subject


exclusiveBF <- merged %>% filter((metadatum == "exclusivelyBottlefedBreastmilk" | metadatum == "exclusivelyNursed") & value == "Yes")
exclusiveForm <- merged %>% filter(metadatum == "exclusiveFormulaFed" & value == "Yes")

onlyBFsubs <- exclusiveBF$subject
onlyNursedsubs <- exclusiveForm$subject

bothBFandForm <- intersect(onlyBFsubs, onlyNursedsubs)
