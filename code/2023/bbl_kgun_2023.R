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
nopi2023 <- read_excel("data/2023/kgun_NOPI_2023.xlsx")
###look at data
head(nopi2023)

###concatenate prefix and suffix of the bands
nopi2023$band_number <- paste(nopi2023$PREFIX,nopi2023$SUFFIX,sep = "-")
head(nopi2023)
#view(nopi2023)

###create disposition column
nopi2023$disposition <- rep("1")

###create 'how aged' column
nopi2023$how_aged <- rep("PL")

###separate year month and day by column
nopi2023 <- nopi2023 %>%
  mutate(date = ymd(DATE)) %>%
  mutate_at(vars(DATE), funs(year, month, day))

###create how captured column
nopi2023$howcaptured <- rep("Swim-in trap")

###create location column
nopi2023$location <- rep("KGUNLAKE")

###create bird status for birds that we collected AI and did not collect AI
#assign value '300' for birds that weren't sampled for AI
#assign value '314' for birds we swabbed for AI
nopi2023$AI_yes_no <- ifelse(is.na(nopi2023$NOTES),0,1)
nopi2023$bird_status <- ifelse(nopi2023$AI_yes_no == 1,314,
                               ifelse(nopi2023$AI_yes_no == 0,300,0))

###create column for AI birds, mouth and cloacal swabs
nopi2023$mouth_swabs <- ifelse(nopi2023$bird_status == 314,"Y","N")
nopi2023$cloacal_swabs <- ifelse(nopi2023$bird_status == 314,"Y","N")

###create a new dataframe with selected columns so it'll be easier to copy and paste to bbl excel template sheet
#(subsetting)
kgun_nopi_bbl <- nopi2023[c("band_number","SPECIES","disposition","year","month","day",
                            "AGE","how_aged","SEX","HOWSEX","bird_status","location","NOTES","mouth_swabs","cloacal_swabs",
                            "BANDER","howcaptured")]

###export file to output folder
write.xlsx(kgun_nopi_bbl, "output/2023/kgun_nopi_bbl.xlsx")



####################################################
###Lets do this for AGWTs
####################################################

###load data to environment
agwt2023 <- read_excel("data/2023/kgun_AGWT_2023.xlsx")
###look at data
head(agwt2023)

###concatenate prefix and suffix of the bands
agwt2023$band_number <- paste(agwt2023$PREFIX,agwt2023$SUFFIX,sep = "-")
head(agwt2023)


###create disposition column
agwt2023$disposition <- rep("1")

###create 'how aged' column
agwt2023$how_aged <- rep("PL")

###separate year month and day by column
agwt2023 <- agwt2023 %>%
  mutate(date = ymd(DATE)) %>%
  mutate_at(vars(DATE), funs(year, month, day))

###create how captured column
agwt2023$howcaptured <- rep("Swim-in trap")

###create location column
agwt2023$location <- rep("KGUNLAKE")

###create bird status for birds that we collected AI and did not collect AI
#assign value '300' for birds that weren't sampled for AI
#assign value '314' for birds we swabbed for AI
agwt2023$AI_yes_no <- ifelse(is.na(agwt2023$NOTES),0,1)
agwt2023$bird_status <- ifelse(agwt2023$AI_yes_no == 1,314,
                               ifelse(agwt2023$AI_yes_no == 0,300,0))

###create column for AI birds, mouth and cloacal swabs
agwt2023$mouth_swabs <- ifelse(agwt2023$bird_status == 314,"Y","N")
agwt2023$cloacal_swabs <- ifelse(agwt2023$bird_status == 314,"Y","N")

###create a new dataframe with selected columns so it'll be easier to copy and paste to bbl excel template sheet
#(subsetting)
kgun_agwt_bbl <- agwt2023[c("band_number","SPECIES","disposition","year","month","day",
                            "AGE","how_aged","SEX","HOWSEX","bird_status","location","NOTES","mouth_swabs","cloacal_swabs",
                            "BANDER","howcaptured")]

###export file to output folder
write.xlsx(kgun_agwt_bbl, "output/2023/kgun_agwt_bbl.xlsx")



####################################################
###Lets do this for MALLs
####################################################

###load data to environment
mall2023 <- read_excel("data/2023/kgun_MALL_2023.xlsx")
###look at data
head(mall2023)

###concatenate prefix and suffix of the bands
mall2023$band_number <- paste(mall2023$PREFIX,mall2023$SUFFIX,sep = "-")
head(mall2023)


###create disposition column
mall2023$disposition <- rep("1")

###create 'how aged' column
mall2023$how_aged <- rep("PL")

###separate year month and day by column
mall2023 <- mall2023 %>%
  mutate(date = ymd(DATE)) %>%
  mutate_at(vars(DATE), funs(year, month, day))

###create how captured column
mall2023$howcaptured <- rep("Swim-in trap")

###create location column
mall2023$location <- rep("KGUNLAKE")

###create bird status for birds that we collected AI and did not collect AI
#assign value '300' for birds that weren't sampled for AI
#assign value '314' for birds we swabbed for AI
mall2023$AI_yes_no <- ifelse(is.na(mall2023$NOTES),0,1)
mall2023$bird_status <- ifelse(mall2023$AI_yes_no == 1,314,
                               ifelse(mall2023$AI_yes_no == 0,300,0))

###create column for AI birds, mouth and cloacal swabs
mall2023$mouth_swabs <- ifelse(mall2023$bird_status == 314,"Y","N")
mall2023$cloacal_swabs <- ifelse(mall2023$bird_status == 314,"Y","N")

###create a new dataframe with selected columns so it'll be easier to copy and paste to bbl excel template sheet
#(subsetting)
kgun_mall_bbl <- mall2023[c("band_number","SPECIES","disposition","year","month","day",
                            "AGE","how_aged","SEX","HOWSEX","bird_status","location","NOTES","mouth_swabs","cloacal_swabs",
                            "BANDER","howcaptured")]

###export file to output folder
write.xlsx(kgun_mall_bbl, "output/2023/kgun_mall_bbl.xlsx")


###Merge all of the data frames into 1
all_species <- rbind(kgun_nopi_bbl,kgun_agwt_bbl,kgun_mall_bbl)
###export all species file
write.xlsx(all_species, "output/2023/all_species.xlsx")

