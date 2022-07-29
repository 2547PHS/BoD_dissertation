#------------------------------------------------------------------------------#
#Project : BoD U.S. adult-onset asthma and NO2 exposure
#Sub     : Demographics
#Part    : (05)  Demographic tables
#Purpose : Create demographic tables
#Created by PHS candidate 2547
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



##PART 5.1: TABLES OF ADULT POPULATION COUNT ----

#Total U.S. adult population & mean, minimum, and maximum population in tracts
Table_Adults_Total <- burden %>% 
  group_by(sum(TOTAL_ADULTS)) %>%
  summarise(sum(TOTAL_ADULTS)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename column
colnames(Table_Adults_Total) <- c("TOTAL_ADULT_POP")

#View table
view(Table_Adults_Total)



##PART 5.1.1: TABLES OF ADULT POPULATION COUNT FOR SOCIODEMOGRAPHIC TRAITS ----

#White pop
Table_White_Total <- burden %>% 
  group_by(sum(WHITE_ALONE)) %>%
  summarise(sum(WHITE_ALONE)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename column
colnames(Table_White_Total) <- c("TOTAL_ADULT_POP")

#View table
view(Table_White_Total)


#Black pop
Table_Black_Total <- burden %>% 
  group_by(sum(BLACK_ALONE)) %>%
  summarise(sum(BLACK_ALONE)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename column
colnames(Table_Black_Total) <- c("TOTAL_ADULT_POP")

#View table
view(Table_Black_Total)


#AIAN pop
Table_AIAN_Total <- burden %>% 
  group_by(sum(AIAN_ALONE)) %>%
  summarise(sum(AIAN_ALONE)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename column
colnames(Table_AIAN_Total) <- c("TOTAL_ADULT_POP")

#View table
view(Table_AIAN_Total)


#Asian pop
Table_Asian_Total <- burden %>% 
  group_by(sum(ASIAN_ALONE)) %>%
  summarise(sum(ASIAN_ALONE)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename column
colnames(Table_Asian_Total) <- c("TOTAL_ADULT_POP")

#View table
view(Table_Asian_Total)


#NHPI pop
Table_NHPI_Total <- burden %>% 
  group_by(sum(NHPI_ALONE)) %>%
  summarise(sum(NHPI_ALONE)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename column
colnames(Table_NHPI_Total) <- c("TOTAL_ADULT_POP")

#View table
view(Table_NHPI_Total)


#Other pop
Table_Other_Total <- burden %>% 
  group_by(sum(OTHER_ALONE)) %>%
  summarise(sum(OTHER_ALONE)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename column
colnames(Table_Other_Total) <- c("TOTAL_ADULT_POP")

#View table
view(Table_Other_Total)


#Two or more race pop
Table_TwoMore_Total <- burden %>% 
  group_by(sum(TWO_MORE)) %>%
  summarise(sum(TWO_MORE)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename column
colnames(Table_TwoMore_Total) <- c("TOTAL_ADULT_POP")

#View table
view(Table_TwoMore_Total)


#Hispanic Latino pop
Table_HisLat_Total <- burden %>% 
  group_by(sum(HisLat_TOTAL)) %>%
  summarise(sum(HisLat_TOTAL)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename column
colnames(Table_HisLat_Total) <- c("TOTAL_ADULT_POP")

#View table
view(Table_HisLat_Total)


#Not Hispanic Latino pop
Table_NotHisLat_Total <- burden %>% 
  group_by(sum(NotHisLat_TOTAL)) %>%
  summarise(sum(NotHisLat_TOTAL)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename column
colnames(Table_NotHisLat_Total) <- c("TOTAL_ADULT_POP")

#View table
view(Table_NotHisLat_Total)


#Combine race and Hispanic Latino tables adult pop count tables
RaceHisLat_total_combined <- rbind(Table_White_Total, Table_Black_Total, Table_AIAN_Total,
                                   Table_Asian_Total, Table_NHPI_Total, Table_Other_Total,
                                   Table_TwoMore_Total, Table_HisLat_Total,
                                   Table_NotHisLat_Total)
#Rename row names
rownames(RaceHisLat_total_combined) <- c("WHITE_ALONE", "BLACK_ALONE", "AIAN_ALONE", 
                                         "ASIAN_ALONE", "NHPI_ALONE", "OTHER_ALONE",
                                         "TWO_MORE", "Hispanic_Latino", "Not_Hispanic_Latino")
#View table
view(RaceHisLat_total_combined)

#Save combined tables as csv file
write.csv(RaceHisLat_total_combined, 
          "R_tables/05Population_counts_tables/RaceHisLat_total_combined.csv")


#By median household income
Table_AdultsTotal_income <- burden_clean_income %>%
  group_by(INCOME_GROUP) %>%
  summarise(TOTAL_ADULT_POP = sum(TOTAL_ADULTS, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0)  %>%
  arrange(factor(INCOME_GROUP, levels = c("<$27,000", "$27,000 to <$52,000",
                                          "$52,000 to <$85,000",
                                          "$85,000 to <$141,000", ">=$141,000")))
#View table
view(Table_AdultsTotal_income)

#Save combined tables as csv file
write.csv(Table_AdultsTotal_income, 
          "R_tables/05Population_counts_tables/Table_AdultsTotal_income.csv")


#By living location
Table_AdultsTotal_LivLoc <- burden_clean_livinglocation %>% 
  group_by(UrbanRural) %>%  
  summarise(TOTAL_ADULT_POP = sum(TOTAL_ADULTS, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0)

#View table
view(Table_AdultsTotal_LivLoc)

#Save combined tables as csv file
write.csv(Table_AdultsTotal_LivLoc, 
          "R_tables/05Population_counts_tables/Table_AdultsTotal_LivLoc.csv")

#Manually join all sociodemographics tables together in Word doc or Excel sheet
#Save combined table to working directory



##PART 5.1.2: TABLE OF ADULT POPULATION COUNT BY STATE ----

#Table of adults by state 
Table_AdultsTotal_state <- burden %>% 
  group_by(STATE.x) %>%  
  summarise(TOTAL_POP = sum(TOTAL_ADULTS, na.rm = T))

#View table
view(Table_AdultsTotal_state)

#Save table as csv file
write_csv(Table_AdultsTotal_state, 
          "R_tables/05Population_counts_tables/Table_AdultsTotal_state.csv")



