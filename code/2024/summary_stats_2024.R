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
n2024 <- read_excel("data/2024/kgun_NOPI_2024.xlsx") #read in nopi file
#n2024 <- n2024[-13] #delete column 13
a2024 <- read_excel("data/2024/kgun_AGWT_2024.xlsx") #read in agwt file
m2024 <- read_excel("data/2024/kgun_MALL_2024.xlsx") #read in mall file


###Combine the three data frames into one
all_species <- rbind(n2024,m2024,a2024)
summary(all_species)


##########Check to make sure the data is clean###############


###Check unique entries for soon to be factor variables.
unique(all_species$BANDER)
#There was a mistake in some of the entries. Let's fix it.
all_species$BANDER[all_species$BANDER == "NEW"] <- "NWR"
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
all_species %>% count(SPECIES) #count by species
all_species %>% count(TRAP_ID, SPECIES) #count by trap location and species
all_species %>% count(TRAP_ID) #count by trap location

###Create a table to make a summary of number of ducks caught in each trap for each species
trap_sum <- all_species %>%
  group_by(TRAP_ID) %>%
  summarize(
    NOPI = sum(SPECIES == "NOPI"),
    MALL = sum(SPECIES == "MALL")
  )

trap_sum$total_captured_by_trap <- rowSums(trap_sum[, c("NOPI","MALL")])

















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
##Plus 4 from recap file!! so 339. Recap file is from bbl_kgun_recaps_20240829.R

##We are going to subset birds that were sampled for AI
AI_birds <- all_species[complete.cases(all_species$NOTES), ] # this code gave us 339, Remember we have some in recaps

##before running this code make sure you have recaps data after running the script 'bbl_kgun_recaps_20240829.R'
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
nopi_years <- read_excel("data/2024/nopi_numbers_2024.xlsx") #read in nopi file

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
ggsave("output/2024/nopi_table_2024.jpg", width = 6.5, height = 3.25, units = "in", dpi = 300)


###Time for mallards
##Read in excel file
mall_years <- read_excel("data/2024/mall_numbers_2024.xlsx") #read in mall file

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
ggsave("output/2024/mall_table_2024.jpg", width = 6.5, height = 3.25, units = "in", dpi = 300)


###Time for american green-winged teals
##Read in excel file
agwt_years <- read_excel("data/2024/agwt_numbers_2024.xlsx") #read in agwt file

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
ggsave("output/2024/agwt_table_2024.jpg", width = 6.5, height = 3.25, units = "in", dpi = 300)