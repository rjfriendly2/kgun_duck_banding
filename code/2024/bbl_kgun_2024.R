### This script is to reformat the banding data to submit for the bird banding laboratory

###activate packages
library(readxl)
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(openxlsx)



####################################################
###Lets do this for NOPIs
####################################################

###load data to environment
nopi2024 <- read_excel("data/2024/kgun_NOPI_2024.xlsx")
###look at data
head(nopi2024)

###concatenate prefix and suffix of the bands
nopi2024$band_number <- paste(nopi2024$PREFIX,nopi2024$SUFFIX,sep = "-")
head(nopi2024)
#view(nopi2024)

###create disposition column
nopi2024$disposition <- rep("1")

###create 'how aged' column
nopi2024$how_aged <- rep("PL")

###separate year month and day by column
nopi2024 <- nopi2024 %>%
  mutate(date = ymd(DATE)) %>%
  mutate_at(vars(DATE), funs(year, month, day))

###create how captured column
nopi2024$howcaptured <- rep("Swim-in trap")

###create location column
nopi2024$location <- rep("KGUNLAKE")

###create bird status for birds that we collected AI and did not collect AI
#assign value '300' for birds that weren't sampled for AI
#assign value '314' for birds we swabbed for AI
nopi2024$AI_yes_no <- ifelse(is.na(nopi2024$NOTES),0,1)
nopi2024$bird_status <- ifelse(nopi2024$AI_yes_no == 1,314,
                               ifelse(nopi2024$AI_yes_no == 0,300,0))

###create column for AI birds, mouth and cloacal swabs
nopi2024$mouth_swabs <- ifelse(nopi2024$bird_status == 314,"Y","N")
nopi2024$cloacal_swabs <- ifelse(nopi2024$bird_status == 314,"Y","N")

###create a new dataframe with selected columns so it'll be easier to copy and paste to bbl excel template sheet
#(subsetting)
kgun_nopi_bbl <- nopi2024[c("band_number","SPECIES","disposition","year","month","day",
                            "AGE","how_aged","SEX","HOWSEX","bird_status","location","NOTES","mouth_swabs","cloacal_swabs",
                            "BANDER","howcaptured")]

###export file to output folder
write.xlsx(kgun_nopi_bbl, "output/2024/kgun_nopi_bbl.xlsx")



####################################################
###Lets do this for AGWTs
####################################################

###load data to environment
agwt2024 <- read_excel("data/2024/kgun_AGWT_2024.xlsx")
###look at data
head(agwt2024)

###concatenate prefix and suffix of the bands
agwt2024$band_number <- paste(agwt2024$PREFIX,agwt2024$SUFFIX,sep = "-")
head(agwt2024)


###create disposition column
agwt2024$disposition <- rep("1")

###create 'how aged' column
agwt2024$how_aged <- rep("PL")

###separate year month and day by column
agwt2024 <- agwt2024 %>%
  mutate(date = ymd(DATE)) %>%
  mutate_at(vars(DATE), funs(year, month, day))

###create how captured column
agwt2024$howcaptured <- rep("Swim-in trap")

###create location column
agwt2024$location <- rep("KGUNLAKE")

###create bird status for birds that we collected AI and did not collect AI
#assign value '300' for birds that weren't sampled for AI
#assign value '314' for birds we swabbed for AI
agwt2024$AI_yes_no <- ifelse(is.na(agwt2024$NOTES),0,1)
agwt2024$bird_status <- ifelse(agwt2024$AI_yes_no == 1,314,
                               ifelse(agwt2024$AI_yes_no == 0,300,0))

###create column for AI birds, mouth and cloacal swabs
agwt2024$mouth_swabs <- ifelse(agwt2024$bird_status == 314,"Y","N")
agwt2024$cloacal_swabs <- ifelse(agwt2024$bird_status == 314,"Y","N")

###create a new dataframe with selected columns so it'll be easier to copy and paste to bbl excel template sheet
#(subsetting)
kgun_agwt_bbl <- agwt2024[c("band_number","SPECIES","disposition","year","month","day",
                            "AGE","how_aged","SEX","HOWSEX","bird_status","location","NOTES","mouth_swabs","cloacal_swabs",
                            "BANDER","howcaptured")]

###export file to output folder
write.xlsx(kgun_agwt_bbl, "output/2024/kgun_agwt_bbl.xlsx")



####################################################
###Lets do this for MALLs
####################################################

###load data to environment
mall2024 <- read_excel("data/2024/kgun_MALL_2024.xlsx")
###look at data
head(mall2024)

###concatenate prefix and suffix of the bands
mall2024$band_number <- paste(mall2024$PREFIX,mall2024$SUFFIX,sep = "-")
head(mall2024)


###create disposition column
mall2024$disposition <- rep("1")

###create 'how aged' column
mall2024$how_aged <- rep("PL")

###separate year month and day by column
mall2024 <- mall2024 %>%
  mutate(date = ymd(DATE)) %>%
  mutate_at(vars(DATE), funs(year, month, day))

###create how captured column
mall2024$howcaptured <- rep("Swim-in trap")

###create location column
mall2024$location <- rep("KGUNLAKE")

###create bird status for birds that we collected AI and did not collect AI
#assign value '300' for birds that weren't sampled for AI
#assign value '314' for birds we swabbed for AI
mall2024$AI_yes_no <- ifelse(is.na(mall2024$NOTES),0,1)
mall2024$bird_status <- ifelse(mall2024$AI_yes_no == 1,314,
                               ifelse(mall2024$AI_yes_no == 0,300,0))

###create column for AI birds, mouth and cloacal swabs
mall2024$mouth_swabs <- ifelse(mall2024$bird_status == 314,"Y","N")
mall2024$cloacal_swabs <- ifelse(mall2024$bird_status == 314,"Y","N")

###create a new dataframe with selected columns so it'll be easier to copy and paste to bbl excel template sheet
#(subsetting)
kgun_mall_bbl <- mall2024[c("band_number","SPECIES","disposition","year","month","day",
                            "AGE","how_aged","SEX","HOWSEX","bird_status","location","NOTES","mouth_swabs","cloacal_swabs",
                            "BANDER","howcaptured")]

###export file to output folder
write.xlsx(kgun_mall_bbl, "output/2024/kgun_mall_bbl.xlsx")


###Merge all of the data frames into 1
all_species <- rbind(kgun_nopi_bbl,kgun_mall_bbl)
###export all species file
write.xlsx(all_species, "output/2024/all_species.xlsx")

