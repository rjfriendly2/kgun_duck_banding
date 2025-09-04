### The goal for this is to separate the trap happy birds from the novel recaps.

###activate data manipulation packages
library(readxl)
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(openxlsx)

###Read in files
n2025 <- read_excel("data/2025/kgun_NOPI_2025.xlsx") #read in nopi file
#n2025 <- n2025[-13] #delete column 13
a2025 <- read_excel("data/2025/kgun_AGWT_2025.xlsx") #read in agwt file
m2025 <- read_excel("data/2025/kgun_MALL_2025.xlsx") #read in mall file

###Combine the 3 dataframes into 1 dataframe
all_birds <- rbind(n2025,a2025,m2025)

###Read in recap files
nrecap <- read_excel("data/2025/kgun_NOPI_2025_recap.xlsx")
arecap <- read_excel("data/2025/kgun_AGWT_2025_recap.xlsx")
mrecap <- read_excel("data/2025/kgun_MALL_2025_recap.xlsx")

head(nrecap)


###Combine recap data
all_recap <- rbind(nrecap,a2025,mrecap)

###Create band number column to use as a reference to separate novel and trap happy birds
###Concatenate prefix and suffix of the bands
all_birds$band_number <- paste(all_birds$PREFIX,all_birds$SUFFIX,sep = "-")
all_recap$band_number <- paste(all_recap$PREFIX,all_recap$SUFFIX,sep = "-")

###Subset only recaps that weren't banded in the same year (novelty birds only)
#this takes away individuals that appear in recaps and in'all_birds'
novel_recaps <- all_recap[!all_recap$band_number %in% all_birds$band_number,]

###Now that you have the novel recaps separated from the trap happy birds, time to use the 
#method from previous to get it reformatted
###create disposition column
novel_recaps$disposition <- rep("R")

###separate year month and day by column
novel_recaps <- novel_recaps %>%
  mutate(date = ymd(DATE)) %>%
  mutate_at(vars(DATE), funs(year, month, day))

###create how captured column
novel_recaps$howcaptured <- rep("Swim-in trap")

###create location column
novel_recaps$location <- rep("KGUNLAKE")

###create 'how aged' column
novel_recaps$how_aged <- rep("PL")

###create bird status for birds that we collected AI and did not collect AI
#assign value '300' for birds that weren't sampled for AI
#assign value '314' for birds we swabbed for AI
novel_recaps$AI_yes_no <- ifelse(is.na(novel_recaps$NOTES),0,1)
novel_recaps$bird_status <- ifelse(novel_recaps$AI_yes_no == 1,314,
                                   ifelse(novel_recaps$AI_yes_no == 0,300,0))

###create column for AI birds, mouth and cloacal swabs
novel_recaps$mouth_swabs <- ifelse(novel_recaps$bird_status == 314,"Y","N")
novel_recaps$cloacal_swabs <- ifelse(novel_recaps$bird_status == 314,"Y","N")


###create column for 'how optained' for recaps
novel_recaps$how_obtained <- rep("66") #'66' means that it was banded in our banding operations from previous year and now released


###create column for 'present condition' for recaps
novel_recaps$present_condition <- rep("07") #'07 means that it was alive, released, and band was still there

###create a new dataframe with selected columns so it'll be easier to copy and paste to bbl excel template sheet
#(subsetting)
kgun_recaps_bbl <- novel_recaps[c("band_number","SPECIES","disposition","year","month","day",
                                  "AGE","how_aged","SEX","HOWSEX","bird_status","how_obtained","present_condition","location",
                                  "NOTES","mouth_swabs","cloacal_swabs","BANDER","howcaptured")]

###export file to output folder
write.xlsx(kgun_recaps_bbl, "output/2025/kgun_recaps_bbl.xlsx")
