#------------------------------------------------------------------------------#
#Project : BoD U.S. adult-onset asthma and NO2 exposure
#Sub     : Pollutant estimate
#Part    : (03)  Air pollution concentration tables
#Purpose : Create NO2 concentration tables
#Created by: PHS candidate 2547
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
my_packages <- c("tidyverse", "dplyr", "tidyr", "data.table")
for (p in my_packages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}



##PART 3.1: TABLE SUMMARY OF NO2 CONCENTRATIONS ACROSS U.S. ----

#Summary across adult-populated U.S. census tracts
NO2_summary_table <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(Mean = mean(NO2_LEVELS, na.rm = T),
             Minimum = min(NO2_LEVELS, na.rm = T), 
             Lower25 = quantile(NO2_LEVELS, 0.25, na.rm = T),
             Median = median(NO2_LEVELS, na.rm = T),
             Upper25 = quantile(NO2_LEVELS, 0.75, na.rm = T),
             Maximum = max(NO2_LEVELS, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 1) %>% 
  as.data.frame() %>% 
  t() %>%  
  as.data.frame()

#View created table
view(NO2_summary_table)

#Rename column
colnames(NO2_summary_table) <- c("NO2_concentration (micrograms/cubic metre)")

#View changes made
view(NO2_summary_table)

#Save table as csv file
write.csv(NO2_summary_table, "R_tables/03NO2_concentration_tables/NO2_summary_table.csv")



##PART 3.2: TABLE SUMMARY OF NO2 LEVELS BY SOCIODEMOGRAPHIC TRAIT ----

#White pop
NO2_white_table <- burden %>%
  filter(TOTAL_ADULTS > 0) %>% 
  group_by(sum(WHITE_ALONE)) %>%
  summarise(MEAN_NO2 = weighted.mean(NO2_LEVELS, WHITE_ALONE, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  as.data.frame()

#Rename columns
colnames(NO2_white_table) <- c("TOTAL_ADULT_POP", "MEAN_NO2 (micrograms/cubic metre)")

#View table
view(NO2_white_table)


#Black pop
NO2_black_table <- burden %>%
  filter(TOTAL_ADULTS > 0) %>%
  group_by(sum(BLACK_ALONE)) %>%
  summarise(MEAN_NO2 = weighted.mean(NO2_LEVELS, BLACK_ALONE, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  as.data.frame()

#Rename columns
colnames(NO2_black_table) <- c("TOTAL_ADULT_POP", "MEAN_NO2 (micrograms/cubic metre)")

#View table
view(NO2_black_table)


#AIAN pop
NO2_AIAN_table <- burden %>%
  filter(TOTAL_ADULTS > 0) %>%
  group_by(sum(AIAN_ALONE)) %>%
  summarise(MEAN_NO2 = weighted.mean(NO2_LEVELS, AIAN_ALONE, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  as.data.frame()

#Rename columns
colnames(NO2_AIAN_table) <- c("TOTAL_ADULT_POP", "MEAN_NO2 (micrograms/cubic metre)")

#View table
view(NO2_AIAN_table)


#Asian pop
NO2_asian_table <- burden %>%
  filter(TOTAL_ADULTS > 0) %>%
  group_by(sum(ASIAN_ALONE)) %>%
  summarise(MEAN_NO2 = weighted.mean(NO2_LEVELS, ASIAN_ALONE, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  as.data.frame()

#Rename columns
colnames(NO2_asian_table) <- c("TOTAL_ADULT_POP", "MEAN_NO2 (micrograms/cubic metre)")

#View table
view(NO2_asian_table)


#NHPI pop
NO2_NHPI_table <- burden %>%
  filter(TOTAL_ADULTS > 0) %>%
  group_by(sum(NHPI_ALONE)) %>%
  summarise(MEAN_NO2 = weighted.mean(NO2_LEVELS, NHPI_ALONE, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  as.data.frame()

#Rename columns
colnames(NO2_NHPI_table) <- c("TOTAL_ADULT_POP", "MEAN_NO2 (micrograms/cubic metre)")

#View table
view(NO2_NHPI_table)


#Other pop
NO2_other_table <- burden %>%
  filter(TOTAL_ADULTS > 0) %>%
  group_by(sum(OTHER_ALONE)) %>%
  summarise(MEAN_NO2 = weighted.mean(NO2_LEVELS, OTHER_ALONE, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  as.data.frame()

#Rename columns
colnames(NO2_other_table) <- c("TOTAL_ADULT_POP", "MEAN_NO2 (micrograms/cubic metre)")

#View table
view(NO2_other_table)


#Two or more races pop
NO2_twomore_table <- burden %>%
  filter(TOTAL_ADULTS > 0) %>%
  group_by(sum(TWO_MORE)) %>%
  summarise(MEAN_NO2 = weighted.mean(NO2_LEVELS, TWO_MORE, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  as.data.frame()

#Rename columns
colnames(NO2_twomore_table) <- c("TOTAL_ADULT_POP", "MEAN_NO2 (micrograms/cubic metre)")

#View table
view(NO2_twomore_table)


#Hispanic Latino total pop
NO2_HisLat_table <- burden %>%
  filter(TOTAL_ADULTS > 0) %>%
  group_by(sum(HisLat_TOTAL)) %>%
  summarise(MEAN_NO2 = weighted.mean(NO2_LEVELS, HisLat_TOTAL, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  as.data.frame()

#Rename columns
colnames(NO2_HisLat_table) <- c("TOTAL_ADULT_POP", "MEAN_NO2 (micrograms/cubic metre)")

#View table
view(NO2_HisLat_table)


#Non-Hispanic Latino total pop
NO2_NotHisLat_table <- burden %>%
  filter(TOTAL_ADULTS > 0) %>%
  group_by(sum(NotHisLat_TOTAL)) %>%
  summarise(MEAN_NO2 = weighted.mean(NO2_LEVELS, NotHisLat_TOTAL, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  as.data.frame()

#Rename columns
colnames(NO2_NotHisLat_table) <- c("TOTAL_ADULT_POP", "MEAN_NO2 (micrograms/cubic metre)")

#View table
view(NO2_NotHisLat_table)


#Combine race and Hispanic Latino tables
NO2_RaceHisLat_combined <- rbind(NO2_white_table, NO2_black_table, NO2_AIAN_table,
                                 NO2_asian_table, NO2_NHPI_table, NO2_other_table,
                                 NO2_twomore_table, NO2_HisLat_table, NO2_NotHisLat_table)

#Rename row names
rownames(NO2_RaceHisLat_combined) <- c("WHITE_ALONE", "BLACK_ALONE", "AIAN_ALONE",
                                   "ASIAN_ALONE", "NHPI_ALONE", "OTHER_ALONE",
                                   "TWO_MORE", "Hispanic_Latino", "Not_Hispanic_Latino")

#View table
view(NO2_RaceHisLat_combined)

#Save combined tables as csv file
write.csv(NO2_RaceHisLat_combined, "R_tables/03NO2_concentration_tables/NO2_RaceHisLat_combined.csv")



#By median household income
NO2_income_table <- burden_clean_income %>% 
  filter(TOTAL_ADULTS > 0) %>%
  group_by(INCOME_GROUP) %>% 
  summarise(MEAN_NO2 = mean(NO2_LEVELS, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 1) %>%
  select(INCOME_GROUP, MEAN_NO2) %>%
  arrange(factor(INCOME_GROUP, levels = c("<$27,000", "$27,000 to <$52,000",
                                          "$52,000 to <$85,000",
                                          "$85,000 to <$141,000", ">=$141,000")))
#Rename columns
colnames(NO2_income_table) <- c("INCOME_GROUP", "MEAN_NO2 (micrograms/cubic metre)")

#View table
view(NO2_income_table)

#Save table as csv file
write.csv(NO2_income_table, "R_tables/03NO2_concentration_tables/NO2_income_table.csv")


#By living location
NO2_LivLoc_table <- burden_clean_livinglocation %>% 
  filter(TOTAL_ADULTS > 0) %>%
  group_by(UrbanRural) %>% 
  summarise(MEAN_NO2 = mean(NO2_LEVELS, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 1) %>%
  select(UrbanRural, MEAN_NO2)

#Rename columns
colnames(NO2_LivLoc_table) <- c("Urban_Rural", "MEAN_NO2 (micrograms/cubic metre)")

#View table
view(NO2_LivLoc_table)

#Save table as csv file
write.csv(NO2_LivLoc_table, "R_tables/03NO2_concentration_tables/NO2_LivLoc_table.csv")


#Manually join all sociodemographics tables together in Word doc or Excel sheet
#Save combined table to working directory



##PART 3.3: TABLE SUMMARY OF NO2 CONCENTRATION LEVELS BY STATE ----

#Table of mean NO2 concentration by state
NO2_state_table <-  burden %>% 
  group_by(STATE.x) %>% 
  summarise(Mean = mean(NO2_LEVELS, na.rm = T),
            Minimum = min(NO2_LEVELS, na.rm = T), 
            Lower25 = quantile(NO2_LEVELS, 0.25, na.rm = T),
            Median = median(NO2_LEVELS, na.rm = T),
            Upper25 = quantile(NO2_LEVELS, 0.75, na.rm = T),
            Maximum = max(NO2_LEVELS, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 1) %>% 
  as.data.frame()

#Rename columns
colnames(NO2_state_table) <- c("STATE", "MEAN", "MINIMUM", "LOWER25", "MEDIAN",
                               "UPPER25", "MAXIMUM")

#View recently created table
view(NO2_state_table)

#Save table as csv file
write.csv(NO2_state_table, "R_tables/03NO2_concentration_tables/NO2_state_table.csv")





##PART 3.4: TABLE SUMMARY OF U.S. CENSUS TRACTS WITH MISSING NO2 CONCENTRATIONS ----

#Create table of census tracts with missing NO2 concentrations and columns of key 
#sociodemographic parameters to describe these tracts
NO2_missing_tracts <- burden %>%
  group_by(GEOID) %>%
  filter(is.na(NO2_LEVELS)) %>% 
  select(GEOID, STATE.x, NO2_LEVELS, TOTAL_ADULTS, WHITE_ALONE, BLACK_ALONE, AIAN_ALONE, 
         ASIAN_ALONE, NHPI_ALONE, OTHER_ALONE, TWO_MORE, HisLat_TOTAL, NotHisLat_TOTAL,
         INCOME_GROUP, UrbanRural) %>%
  rename(STATE = STATE.x)

#View table
view(NO2_missing_tracts)

#Save table as csv file
write.csv(NO2_missing_tracts, 
          "R_tables/03NO2_concentration_tables/NO2_missing_tracts.csv")



