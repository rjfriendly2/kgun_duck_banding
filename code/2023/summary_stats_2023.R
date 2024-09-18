###This script is to start and create a summary statistics for the annual pre-season duck banding at Kgun

###activate data manipulation packages
library(readxl)
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(openxlsx)



###Read in banding data for all species.
#NOTE the excel file for nopis will have one extra column because it has multiple sheets.
n2023 <- read_excel("data/2023/kgun_NOPI_2023.xlsx") #read in nopi file
n2023 <- n2023[-13] #delete column 13
a2023 <- read_excel("data/2023/kgun_AGWT_2023.xlsx") #read in agwt file
m2023 <- read_excel("data/2023/kgun_MALL_2023.xlsx") #read in mall file


###Combine the three data frames into one
all_species <- rbind(n2023,m2023,a2023)
summary(all_species)


##########Check to make sure the data is clean###############


###Check unique entries for soon to be factor variables.
unique(all_species$BANDER)
#There was a mistake in some of the entries. Let's fix it.
all_species$BANDER[all_species$BANDER == "RFJ"] <- "RJF"
unique(all_species$BANDER) #fixed

unique(all_species$SPECIES) #asirtuq
unique(all_species$SEX) #asirtuq
unique(all_species$HOWSEX) #asirtuq
unique(all_species$AGE) #asirtuq
unique(all_species$TRAP_ID) #asirtuq


###Make factor variables into factor variables
is.factor(all_species$SPECIES) #it was not factor variable
all_species$SPECIES <- as.factor(all_species$SPECIES)
is.factor(all_species$SPECIES)

is.factor(all_species$BANDER)
all_species$BANDER <- as.factor(all_species$BANDER)
is.factor(all_species$BANDER)

is.factor(all_species$SEX)
all_species$SEX <- as.factor(all_species$SEX)
is.factor(all_species$SEX)

is.factor(all_species$HOWSEX)
all_species$HOWSEX <- as.factor(all_species$HOWSEX)
is.factor(all_species$HOWSEX)

is.factor(all_species$AGE)
all_species$AGE <- as.factor(all_species$AGE)
is.factor(all_species$AGE)

is.factor(all_species$TRAP_ID)
all_species$TRAP_ID <- as.factor(all_species$TRAP_ID)
is.factor(all_species$TRAP_ID)

###Let's try something new
#library(psych) #used for data summary
#describe(all_species) nah we don't need this

summary(all_species) #Just to look at. 


table_sas <- all_species %>% count(SPECIES, AGE, SEX) #count by species and age and sex
# AGWT = AHYM:4   HYF:2
# MALL = AHYM:13   HYM:3   HYF:3
# NOPI = AHYM:98   AHYF:136   HYM:110   HYF:109   LM:2   LF:5

all_species %>% count(SPECIES) #count by species
# AGWT:6
# MALL:19
# NOPI:460

###Obtain percentages of age and sex of total for each species
# NOPI AHYM: 98/460=21.3%
# NOPI AHYF: 136/460=29.6%
# NOPI HYM: 110/460=23.9%
# NOPI HYF: 109/460=23.6%
# NOPI LM: 2/460=0.4%
# NOPI LF: 5/460=1%

# MALL AHYM: 13/19=68.4
# MALL AHYF: 
# MALL HYM: 3/19=15.8
# MALL HYF: 3/19=15.8
# MALL LM: 
# MALL LF: 

# AGWT AHYM: 4/6=66.66
# AGWT AHYF: 
# AGWT HYM: 
# AGWT HYF: 2/6=33.3
# AGWT LM: 
# AGWT LF: 


all_species %>% count(TRAP_ID, SPECIES) #count by trap location and species
# N1 = AGWT:1   MALL:12   NOPI:148
# N2 = MALL:7   NOPI:223
# N3 = AGWT:5   NOPI:68
# NOPI2 = NOPI: 21

all_species %>% count(TRAP_ID) #count by trap location
# N1:161
# N2:230
# N3:73
# NOPI2:21



#all_species %>% count(BANDER, SPECIES) #just for fun

###Lets manually create a table that has columns: trap, agwt, nopi, and mall
# It will be in that order. Run the codes above to get count information.
# This will be the time where you will have look up and manually create tables
bird_table <- data.frame(Trap = c('N1','N2','N3','NOPI2'),
                         AGWT = c('1','0','5','0'),
                         NOPI = c('148','223','68','21'),
                         MALL = c('12','7','0','0'),
                         Total = c('161','230','73','21'))

bird_age_table <- data.frame(Age = c('L','HY','AHY'),
                             AGWT = c('0','2','4'),
                             NOPI = c('7','219','234'),
                             MALL = c('0','6','13'))


###Count the number of AI samples
is.factor(all_species$NOTES)
all_species$NOTES <- as.factor(all_species$NOTES)
is.factor(all_species$NOTES)

summary(all_species)
#there are 150. Lets try another way
sum(is.na(all_species$NOTES))
# We get 150 again.
485-150
##335 AI samples collected
##Plus 4 from recap file!! so 339. Recap file is from bbl_kgun_recaps_20230829.R

##We are going to subset birds that were sampled for AI
AI_birds <- all_species[complete.cases(all_species$NOTES), ] # this code gave us 339, Remember we have some in recaps

##before running this code make sure you have recaps data after running the script 'bbl_kgun_recaps_20230829.R'
AI_recaps <- all_recaps[complete.cases(all_recaps$NOTES), ]
AI_recaps <- AI_recaps[-13] #delete column 13

##Bind together ai_birds and ai_recaps
AI_all <- rbind(AI_birds, AI_recaps) #THIS DOESN'T WORK

###Are there duplicate AI ids?
n_occur <- data.frame(table(AI_all$NOTES))
n_occur[n_occur$Freq > 1,] #it is zero so there are no mistakes here.
#AI_all[AI_all$NOTES %in% n_occur$Var1[n_occur$Freq > 1],] #will tell you which NOTES occured more than once. IN case ther was

###view data information
summary(AI_all)
#Number of each species sampled for AI
# reminder. 339 total birds swabbed
# AGWT: 4
# MALL: 9
# NOPI: 326


### Create histograms that shows number of birds caught in each year for all species
##Read in excel file
nopi_years <- read_excel("data/2023/nopi_numbers_2023.xlsx") #read in nopi file

###Note total average is 788.4 birds
summary(nopi_years)
#find 32 year average
mean(nopi_years$Total[1:32]) #798.625

##activate packages
library(ggplot2)

ggplot(nopi_years, aes(x = as.factor(Year), y = Total)) +
  geom_col(fill = "lightblue", color = "black") +
  xlab("Year") + ylab("Total Number of Northern Pintail Banded") +
  geom_hline(yintercept = mean(nopi_years$Total), color = "black", lty = "dashed") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 62, hjust = 1)) +
  theme(axis.title.y = element_text(
    size = 10,
    family = "Times",
    face = "bold"
  )) +
  theme(axis.title.x = element_text(
    size = 10,
    family = "Times",
    face = "bold"
  ))
ggsave("output/2023/nopi_table_2023.jpg", width = 6.5, height = 3.25, units = "in", dpi = 300)
  

###Time for mallards
##Read in excel file
mall_years <- read_excel("data/2023/mall_numbers_2023.xlsx") #read in mall file

###Note total average is 57.38 birds
summary(mall_years)
#find 31 year average
mean(mall_years$Total[1:31]) #58.6129

ggplot(mall_years, aes(x = as.factor(Year), y = Total)) +
  geom_col(fill = "lightgreen", color = "black") +
  xlab("Year") + ylab("Total Number of Mallards Banded") +
  geom_hline(yintercept = mean(mall_years$Total), color = "black", lty = "dashed") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 62, hjust = 1)) +
  theme(axis.title.y = element_text(
    size = 10,
    family = "Times",
    face = "bold"
  )) +
  theme(axis.title.x = element_text(
    size = 10,
    family = "Times",
    face = "bold"
  ))
ggsave("output/2023/mall_table_2023.jpg", width = 6.5, height = 3.25, units = "in", dpi = 300)


###Time for american green-winged teals
##Read in excel file
agwt_years <- read_excel("data/2023/agwt_numbers_2023.xlsx") #read in agwt file

###Note total average is 329.7 birds
summary(agwt_years)
#find 32 year average
mean(agwt_years$Total[1:32]) #339.8438

ggplot(agwt_years, aes(x = as.factor(Year), y = Total)) +
  geom_col(fill = "orange", color = "black") +
  xlab("Year") + ylab("Total Number of American Green-winged 
  Teal Banded") +
  geom_hline(yintercept = mean(agwt_years$Total), color = "black", lty = "dashed") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 62, hjust = 1)) +
  theme(axis.title.y = element_text(
    size = 10,
    family = "Times",
    face = "bold"
  )) +
  theme(axis.title.x = element_text(
    size = 10,
    family = "Times",
    face = "bold"
  ))
ggsave("output/2023/agwt_table_2023.jpg", width = 6.5, height = 3.25, units = "in", dpi = 300)
