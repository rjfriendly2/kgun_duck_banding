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
nopi2025 <- read_excel("data/2025/kgun_NOPI_2025.xlsx")
###look at data
head(nopi2025)

###concatenate prefix and suffix of the bands
nopi2025$band_number <- paste(nopi2025$PREFIX,nopi2025$SUFFIX,sep = "-")
head(nopi2025)
#view(nopi2025)

###create disposition column
nopi2025$disposition <- rep("1")

###create 'how aged' column
nopi2025$how_aged <- rep("PL")

###separate year month and day by column
nopi2025 <- nopi2025 %>%
  mutate(date = ymd(DATE)) %>%
  mutate_at(vars(DATE), funs(year, month, day))

###create how captured column
nopi2025$howcaptured <- rep("Swim-in trap")

###create location column
nopi2025$location <- rep("KGUNLAKE")

###create bird status for birds that we collected AI and did not collect AI
#assign value '300' for birds that weren't sampled for AI
#assign value '314' for birds we swabbed for AI
nopi2025$bird_status <- ifelse(nopi2025$AI == "Y",314,300)
#nopi2025$AI_yes_no <- ifelse(is.na(nopi2025$NOTES),0,1)
#nopi2025$bird_status <- ifelse(nopi2025$AI_yes_no == 1,314,
                               #ifelse(nopi2025$AI_yes_no == 0,300,0))

###create column for AI birds, mouth and cloacal swabs
nopi2025$mouth_swabs <- ifelse(nopi2025$bird_status == 314,"Y","N")
nopi2025$cloacal_swabs <- ifelse(nopi2025$bird_status == 314,"Y","N")

###create a new dataframe with selected columns so it'll be easier to copy and paste to bbl excel template sheet
#(subsetting)
kgun_nopi_bbl <- nopi2025[c("band_number","SPECIES","disposition","year","month","day",
                            "AGE","how_aged","SEX","HOWSEX","bird_status","location","NOTES","mouth_swabs","cloacal_swabs",
                            "BANDER","howcaptured")]

###export file to output folder
write.xlsx(kgun_nopi_bbl, "output/2025/kgun_nopi_bbl.xlsx")



####################################################
###Lets do this for AGWTs
####################################################

###load data to environment
agwt2025 <- read_excel("data/2025/kgun_AGWT_2025.xlsx")
###look at data
head(agwt2025)

###concatenate prefix and suffix of the bands
agwt2025$band_number <- paste(agwt2025$PREFIX,agwt2025$SUFFIX,sep = "-")
head(agwt2025)


###create disposition column
agwt2025$disposition <- rep("1")

###create 'how aged' column
agwt2025$how_aged <- rep("PL")

###separate year month and day by column
agwt2025 <- agwt2025 %>%
  mutate(date = ymd(DATE)) %>%
  mutate_at(vars(DATE), funs(year, month, day))

###create how captured column
agwt2025$howcaptured <- rep("Swim-in trap")

###create location column
agwt2025$location <- rep("KGUNLAKE")

###create bird status for birds that we collected AI and did not collect AI
#assign value '300' for birds that weren't sampled for AI
#assign value '314' for birds we swabbed for AI
agwt2025$bird_status <- ifelse(agwt2025$AI == "Y",314,300)
#agwt2025$AI_yes_no <- ifelse(is.na(agwt2025$NOTES),0,1)
#agwt2025$bird_status <- ifelse(agwt2025$AI_yes_no == 1,314,
                               #ifelse(agwt2025$AI_yes_no == 0,300,0))

###create column for AI birds, mouth and cloacal swabs
agwt2025$mouth_swabs <- ifelse(agwt2025$bird_status == 314,"Y","N")
agwt2025$cloacal_swabs <- ifelse(agwt2025$bird_status == 314,"Y","N")

###create a new dataframe with selected columns so it'll be easier to copy and paste to bbl excel template sheet
#(subsetting)
kgun_agwt_bbl <- agwt2025[c("band_number","SPECIES","disposition","year","month","day",
                            "AGE","how_aged","SEX","HOWSEX","bird_status","location","NOTES","mouth_swabs","cloacal_swabs",
                            "BANDER","howcaptured")]

###export file to output folder
write.xlsx(kgun_agwt_bbl, "output/2025/kgun_agwt_bbl.xlsx")



####################################################
###Lets do this for MALLs
####################################################

###load data to environment
mall2025 <- read_excel("data/2025/kgun_MALL_2025.xlsx")
###look at data
head(mall2025)

###concatenate prefix and suffix of the bands
mall2025$band_number <- paste(mall2025$PREFIX,mall2025$SUFFIX,sep = "-")
head(mall2025)


###create disposition column
mall2025$disposition <- rep("1")

###create 'how aged' column
mall2025$how_aged <- rep("PL")

###separate year month and day by column
mall2025 <- mall2025 %>%
  mutate(date = ymd(DATE)) %>%
  mutate_at(vars(DATE), funs(year, month, day))

###create how captured column
mall2025$howcaptured <- rep("Swim-in trap")

###create location column
mall2025$location <- rep("KGUNLAKE")

###create bird status for birds that we collected AI and did not collect AI
#assign value '300' for birds that weren't sampled for AI
#assign value '314' for birds we swabbed for AI
mall2025$bird_status <- ifelse(mall2025$AI == "Y",314,300)
#mall2025$AI_yes_no <- ifelse(is.na(mall2025$NOTES),0,1)
#mall2025$bird_status <- ifelse(mall2025$AI_yes_no == 1,314,
                               #ifelse(mall2025$AI_yes_no == 0,300,0))

###create column for AI birds, mouth and cloacal swabs
mall2025$mouth_swabs <- ifelse(mall2025$bird_status == 314,"Y","N")
mall2025$cloacal_swabs <- ifelse(mall2025$bird_status == 314,"Y","N")

###create a new dataframe with selected columns so it'll be easier to copy and paste to bbl excel template sheet
#(subsetting)
kgun_mall_bbl <- mall2025[c("band_number","SPECIES","disposition","year","month","day",
                            "AGE","how_aged","SEX","HOWSEX","bird_status","location","NOTES","mouth_swabs","cloacal_swabs",
                            "BANDER","howcaptured")]

###export file to output folder
write.xlsx(kgun_mall_bbl, "output/2025/kgun_mall_bbl.xlsx")


####################################################
###Lets do this for NSHOs
####################################################

###load data to environment
nsho2025 <- read_excel("data/2025/kgun_nsho_2025.xlsx")
###look at data
head(nsho2025)

###concatenate prefix and suffix of the bands
nsho2025$band_number <- paste(nsho2025$PREFIX,nsho2025$SUFFIX,sep = "-")
head(nsho2025)


###create disposition column
nsho2025$disposition <- rep("1")

###create 'how aged' column
nsho2025$how_aged <- rep("PL")

###separate year month and day by column
nsho2025 <- nsho2025 %>%
  mutate(date = ymd(DATE)) %>%
  mutate_at(vars(DATE), funs(year, month, day))

###create how captured column
nsho2025$howcaptured <- rep("Swim-in trap")

###create location column
nsho2025$location <- rep("KGUNLAKE")

###create bird status for birds that we collected AI and did not collect AI
#assign value '300' for birds that weren't sampled for AI
#assign value '314' for birds we swabbed for AI
nsho2025$bird_status <- ifelse(nsho2025$AI == "Y",314,300)
#nsho2025$AI_yes_no <- ifelse(is.na(nsho2025$NOTES),0,1)
#nsho2025$bird_status <- ifelse(nsho2025$AI_yes_no == 1,314,
#ifelse(nsho2025$AI_yes_no == 0,300,0))

###create column for AI birds, mouth and cloacal swabs
nsho2025$mouth_swabs <- ifelse(nsho2025$bird_status == 314,"Y","N")
nsho2025$cloacal_swabs <- ifelse(nsho2025$bird_status == 314,"Y","N")

###create a new dataframe with selected columns so it'll be easier to copy and paste to bbl excel template sheet
#(subsetting)
kgun_nsho_bbl <- nsho2025[c("band_number","SPECIES","disposition","year","month","day",
                            "AGE","how_aged","SEX","HOWSEX","bird_status","location","NOTES","mouth_swabs","cloacal_swabs",
                            "BANDER","howcaptured")]

###export file to output folder
write.xlsx(kgun_nsho_bbl, "output/2025/kgun_nsho_bbl.xlsx")


###Merge all of the data frames into 1
all_species <- rbind(kgun_nopi_bbl,kgun_agwt_bbl,kgun_mall_bbl,kgun_nsho_bbl)
summary(all_species)
###export all species file
write.xlsx(all_species, "output/2025/all_species.xlsx")

