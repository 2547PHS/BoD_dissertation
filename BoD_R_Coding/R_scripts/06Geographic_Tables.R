#------------------------------------------------------------------------------#
#Project : BoD U.S. adult-onset asthma and NO2 exposure
#Sub     : Geographical
#Part    : (06)  Geographic tables
#Purpose : Create geographic tables of census tracts
#Created by PHS Candidate 2547
#Date Created: 19-June-2022
#Last Updated: 23-July-2022
#------------------------------------------------------------------------------#

##Note: must run "01Burden_DataSet.R" script first before proceeding##



##PRELIMINARY STEPS: SETTING WORKSPACE ----

#Set working directory
setwd("~/BoD_R_Coding")

#Check working directory
getwd()

#Install packages relevant for subsequent analyses
my_packages <- c("readr", "tidyverse", "dplyr", "tidyr", "data.table")
for (p in my_packages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}




##PART 6.1: TABLES OF NUMBER OF ADULT-POPULATED U.S. CENSUS TRACTS ----

#Number of adult-populated census tracts across US
Table_CensusTract_ALL <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  group_by(sum(TOTAL_ADULTS)) %>% 
  summarise(CENSUS_TRACTS = length(NAME.x)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename column
colnames(Table_CensusTract_ALL) <- c("TOTAL_ADULT_POP", "CENSUS_TRACTS")

#View table
view(Table_CensusTract_ALL)


#Number of adult non-populated census tracts across US
Table_EmptyCensusTract_ALL <- burden %>% 
  filter(TOTAL_ADULTS == 0) %>%
  group_by(sum(TOTAL_ADULTS)) %>% 
  summarise(CENSUS_TRACTS = length(NAME.x)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename column
colnames(Table_EmptyCensusTract_ALL) <- c("TOTAL_ADULT_POP", "CENSUS_TRACTS")

#View table
view(Table_EmptyCensusTract_ALL)



##PART 6.1.1: TABLES OF NUMBER OF ADULT-POPULATED U.S. CENSUS TRACTS BY SOCIODEMOGRAPHIC TRAITS ----

#White pop
Table_CensusTract_white <- burden %>% 
  filter(WHITE_ALONE > 0) %>%
  group_by(sum(WHITE_ALONE)) %>% 
  summarise(CENSUS_TRACTS = length(NAME.x)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_CensusTract_white) <- c("TOTAL_ADULT_POP", "CENSUS_TRACTS")

#View table
view(Table_CensusTract_white)


#Black pop
Table_CensusTract_black <- burden %>% 
  filter(BLACK_ALONE > 0) %>%
  group_by(sum(BLACK_ALONE)) %>% 
  summarise(CENSUS_TRACTS = length(NAME.x)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_CensusTract_black) <- c("TOTAL_ADULT_POP", "CENSUS_TRACTS")

#View table
view(Table_CensusTract_black)


#AIAN pop
Table_CensusTract_AIAN <- burden %>% 
  filter(AIAN_ALONE > 0) %>%
  group_by(sum(AIAN_ALONE)) %>% 
  summarise(CENSUS_TRACTS = length(NAME.x)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_CensusTract_AIAN) <- c("TOTAL_ADULT_POP", "CENSUS_TRACTS")

#View table
view(Table_CensusTract_AIAN)


#Asian pop
Table_CensusTract_asian <- burden %>% 
  filter(ASIAN_ALONE > 0) %>%
  group_by(sum(ASIAN_ALONE)) %>% 
  summarise(CENSUS_TRACTS = length(NAME.x)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_CensusTract_asian) <- c("TOTAL_ADULT_POP", "CENSUS_TRACTS")

#View table
view(Table_CensusTract_asian)


#NHPI pop
Table_CensusTract_NHPI <- burden %>% 
  filter(NHPI_ALONE > 0) %>%
  group_by(sum(NHPI_ALONE)) %>% 
  summarise(CENSUS_TRACTS = length(NAME.x)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_CensusTract_NHPI) <- c("TOTAL_ADULT_POP", "CENSUS_TRACTS")

#View table
view(Table_CensusTract_NHPI)


#Other pop
Table_CensusTract_other <- burden %>% 
  filter(OTHER_ALONE > 0) %>%
  group_by(sum(OTHER_ALONE)) %>% 
  summarise(CENSUS_TRACTS = length(NAME.x)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_CensusTract_other) <- c("TOTAL_ADULT_POP", "CENSUS_TRACTS")

#View table
view(Table_CensusTract_other)


#Two or more races pop
Table_CensusTract_twomore <- burden %>% 
  filter(TWO_MORE > 0) %>%
  group_by(sum(TWO_MORE)) %>% 
  summarise(CENSUS_TRACTS = length(NAME.x)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_CensusTract_twomore) <- c("TOTAL_ADULT_POP", "CENSUS_TRACTS")

#View table
view(Table_CensusTract_twomore)


#Hispanic Latino total pop
Table_CensusTract_HisLat <- burden %>% 
  filter(HisLat_TOTAL > 0) %>%
  group_by(sum(HisLat_TOTAL)) %>% 
  summarise(CENSUS_TRACTS = length(NAME.x)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_CensusTract_HisLat) <- c("TOTAL_ADULT_POP", "CENSUS_TRACTS")

#View table
view(Table_CensusTract_HisLat)


#Not Hispanic Latino total pop
Table_CensusTract_NotHisLat <- burden %>% 
  filter(NotHisLat_TOTAL > 0) %>%
  group_by(sum(NotHisLat_TOTAL)) %>% 
  summarise(CENSUS_TRACTS = length(NAME.x)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_CensusTract_NotHisLat) <- c("TOTAL_ADULT_POP", "CENSUS_TRACTS")

#View table
view(Table_CensusTract_NotHisLat)


#Combine race and Hispanic Latino tables
CensusTract_RaceHisLat_combined <- rbind(Table_CensusTract_white, Table_CensusTract_black,
                                         Table_CensusTract_AIAN, Table_CensusTract_asian,
                                         Table_CensusTract_NHPI, Table_CensusTract_other,
                                         Table_CensusTract_twomore, Table_CensusTract_HisLat,
                                         Table_CensusTract_NotHisLat)
#Rename row names
rownames(CensusTract_RaceHisLat_combined) <- c("WHITE_ALONE", "BLACK_ALONE", "AIAN_ALONE",
                                               "ASIAN_ALONE", "NHPI_ALONE", "OTHER_ALONE",
                                               "TWO_MORE", "Hispanic_Latino", 
                                               "Not_Hispanic_Latino")
#View table
view(CensusTract_RaceHisLat_combined)

#Save combined tables as csv file
write.csv(CensusTract_RaceHisLat_combined, 
          "R_tables/06Geographic_counts_tables/CensusTract_RaceHisLat_combined.csv")


#By median household income
Table_CensusTract_income <- burden_clean_income %>%
  filter(TOTAL_ADULTS > 0) %>%
  group_by(INCOME_GROUP) %>%
  summarise(CENSUS_TRACTS = length(NAME.x)) %>%
  mutate_if(is.numeric, round, digits = 0)  %>%
  arrange(factor(INCOME_GROUP, levels = c("<$27,000", "$27,000 to <$52,000",
                                          "$52,000 to <$85,000",
                                          "$85,000 to <$141,000", ">=$141,000")))
#View table
view(Table_CensusTract_income)

#Save table as csv file
write.csv(Table_CensusTract_income, 
          "R_tables/06Geographic_counts_tables/Table_CensusTract_income.csv")


#By living location
Table_CensusTract_LivLoc <- burden_clean_livinglocation %>% 
  group_by(UrbanRural) %>%  
  summarise(CENSUS_TRACTS = length(NAME.x)) %>% 
  mutate_if(is.numeric, round, digits = 0)

#View table
view(Table_CensusTract_LivLoc)

#Save table as csv file
write.csv(Table_CensusTract_LivLoc, 
          "R_tables/06Geographic_counts_tables/Table_CensusTract_LivLoc.csv")


#Manually join all sociodemographics tables together in Word doc or Excel sheet
#Save combined table to working directory




##PART 6.1.2: TABLE OF NUMBER OF ADULT-POPULATED U.S. CENSUS TRACTS BY STATE ----

#Table of number of census tracts in each state
Table_CensusTract_state <- burden %>% 
  group_by(STATE.x) %>%  
  summarise(CENSUS_TRACTS = length(NAME.x)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#View table
view(Table_CensusTract_state)

#Save table as csv file
write_csv(Table_CensusTract_state, 
          "R_tables/06Geographic_counts_tables/Table_CensusTract_state.csv")




