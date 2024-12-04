###This script is to start and create a summary statistics for the annual pre-season duck banding at Kgun

###activate data manipulation packages
library(readxl)
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(tidyr)



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

###Make date a 'date' variable
all_species$DATE <- as.Date(all_species$DATE)
is.Date(all_species$DATE)

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

###save trap sum table
write.csv(trap_sum, "output/2024/trap_sum.csv")


###Show the number of birds banded per day for each of the four trap locations
grouped_data <- all_species %>%
  group_by(DATE, TRAP_ID) %>%
  summarise(count = n(), .groups = "drop")

ggplot(grouped_data, aes(x = DATE, y = count)) +
  geom_bar(stat = "identity", position = "dodge", fill = "burlywood", color = "black") +
  facet_wrap(~ TRAP_ID, ncol = 2) +
  labs(
    x = "Date",
    y = "Number of Birds Banded"
  ) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  scale_x_date(
    date_breaks = "2 day",
    date_labels = "%b %d"
  ) + 
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
ggsave("output/2024/trap_site_hist.jpg", width = 7, height = 5, units = "in", dpi = 600)  







###Count the number of AI samples
is.character(all_species$NOTES)
all_species$NOTES <- as.character(all_species$NOTES)
is.character(all_species$NOTES)

count_ai <- sum(nchar(all_species$NOTES) > 0, na.rm = TRUE)

#211 ai samples
##Plus 1 from recap file

##We are going to subset birds that were sampled for AI
AI_birds <- all_species[complete.cases(all_species$NOTES), ] 

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
# reminder. 212 total birds swabbed
# AGWT: 
# MALL: 1
# NOPI: 211


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


###Include the Bethel banding data
bethel_ducks <- read_excel("data/2024/BETHEL_Banding_Data_Upload.xlsx")

###clean up and fix the data

#keep the rows 1-44 the other are empty
bethel_ducks <- bethel_ducks[1:44, ]

#keep certain columns to work with
bethel_ducks <- subset(bethel_ducks, select = c("Band Number", "Bander ID","Species", "How Sexed","Banding Year",
                                                "Banding Month", "Banding Day", "Age", "Sex",
                                                "Location", "Remarks"))

###Work on merging bethel ducks and kgun ducks
#first need to reformat data

bethel_ducks <- separate(bethel_ducks, "Band Number", into = c("PREFIX", "SUFFIX"), sep = "-")

bethel_ducks <- bethel_ducks %>% rename("BANDER" = "Bander ID",
                                        "SPECIES" = "Species",
                                        "SEX" = "Sex",
                                        "HOWSEX" = "How Sexed",
                                        "AGE" = "Age",
                                        "TRAP_ID" = "Location",
                                        "NOTES" = "Remarks")

bethel_ducks$DATE <- as.Date(ISOdate(bethel_ducks$`Banding Year`, bethel_ducks$`Banding Month`, bethel_ducks$`Banding Day`))

bethel_ducks <- bethel_ducks[, !colnames(bethel_ducks) %in% c("Banding Year", "Banding Month", "Banding Day")]

bethel_ducks$`START TIME` <- NA
bethel_ducks$`END TIME` <- NA

ordercolumns <- c("PREFIX", "SUFFIX", "BANDER", "SPECIES", "SEX", "HOWSEX", "AGE",
                  "DATE", "START TIME", "END TIME", "TRAP_ID", "NOTES")
setcolorder(bethel_ducks, c(ordercolumns, setdiff(names(bethel_ducks), ordercolumns)))

###combine kgun ducks and bethel ducks
combined_ducks <- rbind(all_species, bethel_ducks)



##Make factor variables into factor variables
is.factor(combined_ducks$SPECIES) #it was not factor variable
combined_ducks$SPECIES <- as.factor(combined_ducks$SPECIES)
is.factor(combined_ducks$SPECIES)

is.factor(combined_ducks$BANDER)
combined_ducks$BANDER <- as.factor(combined_ducks$BANDER)
is.factor(combined_ducks$BANDER)

is.factor(combined_ducks$SEX)
combined_ducks$SEX <- as.factor(combined_ducks$SEX)
is.factor(combined_ducks$SEX)

is.factor(combined_ducks$HOWSEX)
combined_ducks$HOWSEX <- as.factor(combined_ducks$HOWSEX)
is.factor(combined_ducks$HOWSEX)

is.factor(combined_ducks$AGE)
combined_ducks$AGE <- as.factor(combined_ducks$AGE)
is.factor(combined_ducks$AGE)

is.factor(combined_ducks$TRAP_ID)
combined_ducks$TRAP_ID <- as.factor(combined_ducks$TRAP_ID)
is.factor(combined_ducks$TRAP_ID)

###Make date a 'date' variable
combined_ducks$DATE <- as.Date(combined_ducks$DATE)
is.Date(combined_ducks$DATE)


summary(combined_ducks) #Just to look at. 
table_sas2 <- combined_ducks %>% count(SPECIES, AGE, SEX) #count by species and age and sex
combined_ducks %>% count(SPECIES) #count by species
combined_ducks %>% count(TRAP_ID, SPECIES) #count by trap location and species
combined_ducks %>% count(TRAP_ID) #count by trap location
combined_ducks %>% count(SPECIES, SEX, AGE)

###Create a table to make a summary of number of ducks caught in each trap for each species
trap_sum2 <- combined_ducks %>%
  group_by(TRAP_ID) %>%
  summarize(
    NOPI = sum(SPECIES == "NOPI"),
    MALL = sum(SPECIES == "MALL"),
    NSHO = sum(SPECIES == "NSHO"),
    AGWT = sum(SPECIES == "AGWT"),
    AMWI = sum(SPECIES == "AMWI")
  )


###Show the number of birds banded per day for each of the four trap locations
grouped_data <- combined_ducks %>%
  group_by(DATE, TRAP_ID) %>%
  summarise(count = n(), .groups = "drop")

ggplot(grouped_data, aes(x = DATE, y = count)) +
  geom_bar(stat = "identity", position = "dodge", fill = "burlywood", color = "black") +
  facet_wrap(~ TRAP_ID, ncol = 3) +
  labs(
    x = "Date",
    y = "Number of Birds Banded"
  ) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  scale_x_date(
    date_breaks = "2 day",
    date_labels = "%b %d"
  ) + 
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
ggsave("output/2024/trap_site_hist.jpg", width = 7, height = 5, units = "in", dpi = 600)  






