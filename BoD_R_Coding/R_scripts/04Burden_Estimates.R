#------------------------------------------------------------------------------#
#Project : BoD U.S. adult-onset asthma and NO2 exposure
#Sub     : Pollutant estimate
#Part    : (04)  Burden tables
#Purpose : Create burden tables
#Created by: PHS candidate 2547
#Date Created: 19-June-2022
#Last Updated: 23-July-2022
#------------------------------------------------------------------------------#

##Note: must run "01Burden_DataSets.R" script first##


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



##PART 4.1: TABLES OF ADULT ASTHMA INCIDENT CASES ----

#Total adult asthma incident cases
Table_IncidentCases_ALL <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  group_by(sum(TOTAL_ADULTS)) %>% 
  summarise(IncidentCases = sum(EXPECTED_ALLCAUSE_CASES, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_IncidentCases_ALL) <- c("TOTAL_ADULT_POP", "IncidentCases")

#View table
view(Table_IncidentCases_ALL)



##PART 4.1.1: TABLES OF ADULT ASTHMA INCIDENT CASES BY SOCIODEMOGRAPHIC TRAITS ----

#White pop
#First calculate expected all-cause cases for white pop
EXPECTED_WHITE <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(EXPECTED_CASES_WHITE = (WHITE_ALONE - (WHITE_ALONE * PRV)) * IR) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Make table of incident cases for white pop
Table_IncidentCases_white <- burden %>% 
  group_by(sum(WHITE_ALONE)) %>% 
  summarise(IncidentCases = sum(EXPECTED_WHITE, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_IncidentCases_white) <- c("TOTAL_ADULT_POP", "IncidentCases")

#View table
view(Table_IncidentCases_white)


#Black pop
#First calculate expected all-cause cases for Black pop
EXPECTED_BLACK <- burden %>% 
  filter(BLACK_ALONE > 0) %>%
  summarise(EXPECTED_CASES_BLACK = (BLACK_ALONE - (BLACK_ALONE * PRV)) * IR) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Make table of incident cases for Black pop
Table_IncidentCases_black <- burden %>% 
  group_by(sum(BLACK_ALONE)) %>% 
  summarise(IncidentCases = sum(EXPECTED_BLACK, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_IncidentCases_black) <- c("TOTAL_ADULT_POP", "IncidentCases")

#View table
view(Table_IncidentCases_black)


#AIAN pop
#First calculate expected all-cause cases for AIAN pop
EXPECTED_AIAN <- burden %>% 
  filter(AIAN_ALONE > 0) %>%
  summarise(EXPECTED_CASES_AIAN = (AIAN_ALONE - (AIAN_ALONE * PRV)) * IR) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Make table of incident cases for AIAN pop
Table_IncidentCases_AIAN <- burden %>% 
  group_by(sum(AIAN_ALONE)) %>% 
  summarise(IncidentCases = sum(EXPECTED_AIAN, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_IncidentCases_AIAN) <- c("TOTAL_ADULT_POP", "IncidentCases")

#View table
view(Table_IncidentCases_AIAN)


#Asian pop
#First calculate expected all-cause cases for Asian pop
EXPECTED_ASIAN <- burden %>% 
  filter(ASIAN_ALONE > 0) %>%
  summarise(EXPECTED_CASES_ASIAN = (ASIAN_ALONE - (ASIAN_ALONE * PRV)) * IR) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Make table of incident cases for Asian pop
Table_IncidentCases_asian <- burden %>% 
  group_by(sum(ASIAN_ALONE)) %>% 
  summarise(IncidentCases = sum(EXPECTED_ASIAN, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_IncidentCases_asian) <- c("TOTAL_ADULT_POP", "IncidentCases")

#View table
view(Table_IncidentCases_asian)


#NHPI pop
#First calculate expected all-cause cases for NHPI pop
EXPECTED_NHPI <- burden %>% 
  filter(NHPI_ALONE > 0) %>%
  summarise(EXPECTED_CASES_NHPI = (NHPI_ALONE - (NHPI_ALONE * PRV)) * IR) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Make table of incident cases for NHPI pop
Table_IncidentCases_NHPI <- burden %>% 
  group_by(sum(NHPI_ALONE)) %>% 
  summarise(IncidentCases = sum(EXPECTED_NHPI, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_IncidentCases_NHPI) <- c("TOTAL_ADULT_POP", "IncidentCases")

#View table
view(Table_IncidentCases_NHPI)


#Other pop
#First calculate expected all-cause cases for Other race pop
EXPECTED_OTHER <- burden %>% 
  filter(OTHER_ALONE > 0) %>%
  summarise(EXPECTED_CASES_OTHER = (OTHER_ALONE - (OTHER_ALONE * PRV)) * IR) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Make table of incident cases for Other race pop
Table_IncidentCases_other <- burden %>% 
  group_by(sum(OTHER_ALONE)) %>% 
  summarise(IncidentCases = sum(EXPECTED_OTHER, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_IncidentCases_other) <- c("TOTAL_ADULT_POP", "IncidentCases")

#View table
view(Table_IncidentCases_other)


#Two or more races pop
#First calculate expected all-cause cases for two or more races pop
EXPECTED_TWOMORE <- burden %>% 
  filter(TWO_MORE > 0) %>%
  summarise(EXPECTED_CASES_TWOMORE = (TWO_MORE - (TWO_MORE * PRV)) * IR) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Make table of incident cases for two or more races pop
Table_IncidentCases_twomore <- burden %>% 
  group_by(sum(TWO_MORE)) %>% 
  summarise(IncidentCases = sum(EXPECTED_TWOMORE, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_IncidentCases_twomore) <- c("TOTAL_ADULT_POP", "IncidentCases")

#View table
view(Table_IncidentCases_twomore)


#Hispanic Latino total pop
#First calculate expected all-cause cases for Hispanic Latino pop
EXPECTED_HisLat <- burden %>% 
  filter(HisLat_TOTAL > 0) %>%
  summarise(EXPECTED_CASES_HisLat = (HisLat_TOTAL - (HisLat_TOTAL * PRV)) * IR) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Make table of incident cases for Hispanic Latino pop
Table_IncidentCases_HisLat <- burden %>% 
  group_by(sum(HisLat_TOTAL)) %>% 
  summarise(IncidentCases = sum(EXPECTED_HisLat, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_IncidentCases_HisLat) <- c("TOTAL_ADULT_POP", "IncidentCases")

#View table
view(Table_IncidentCases_HisLat)


#Not Hispanic Latino total pop
#First calculate expected all-cause cases for Non-Hispanic Latino pop
EXPECTED_NotHisLat <- burden %>% 
  filter(NotHisLat_TOTAL > 0) %>%
  summarise(EXPECTED_CASES_NotHisLat = (NotHisLat_TOTAL - (NotHisLat_TOTAL * PRV)) * IR) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Make table of incident cases for Non-Hispanic Latino pop
Table_IncidentCases_NotHisLat <- burden %>% 
  group_by(sum(NotHisLat_TOTAL)) %>% 
  summarise(IncidentCases = sum(EXPECTED_NotHisLat, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_IncidentCases_NotHisLat) <- c("TOTAL_ADULT_POP", "IncidentCases")

#View table
view(Table_IncidentCases_NotHisLat)


#Combine race and Hispanic Latino tables
NO2_IncidentCases_RaceHisLat_combined <- rbind(Table_IncidentCases_white,
                                               Table_IncidentCases_black,
                                               Table_IncidentCases_AIAN,
                                               Table_IncidentCases_asian,
                                               Table_IncidentCases_NHPI,
                                               Table_IncidentCases_other,
                                               Table_IncidentCases_twomore,
                                               Table_IncidentCases_HisLat,
                                               Table_IncidentCases_NotHisLat)
#Rename row names
rownames(NO2_IncidentCases_RaceHisLat_combined) <- c("WHITE_ALONE", "BLACK_ALONE", 
                                                     "AIAN_ALONE", "ASIAN_ALONE", 
                                                     "NHPI_ALONE", "OTHER_ALONE",
                                                     "TWO_MORE", "Hispanic_Latino", 
                                                     "Not_Hispanic_Latino")
#View table
view(NO2_IncidentCases_RaceHisLat_combined)

#Save combined tables as csv file
write.csv(NO2_IncidentCases_RaceHisLat_combined, 
          "R_tables/04Burden_Estimates_tables/Incident_cases_tables/NO2_IncidentCases_RaceHisLat_combined.csv")



#Median household income
Table_IncidentCases_income <- burden_clean_income %>%
  filter(TOTAL_ADULTS > 0) %>%
  group_by(INCOME_GROUP) %>%
  summarise(IncidentCases = sum(EXPECTED_ALLCAUSE_CASES, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  arrange(factor(INCOME_GROUP, levels = c("<$27,000", "$27,000 to <$52,000",
                                          "$52,000 to <$85,000",
                                          "$85,000 to <$141,000", ">=$141,000")))
#View table
view(Table_IncidentCases_income)

#Save table as csv file
write.csv(Table_IncidentCases_income, 
          "R_tables/04Burden_Estimates_tables/Incident_cases_tables/Table_IncidentCases_income.csv")


#Living location
Table_IncidentCases_LivLoc <- burden_clean_livinglocation %>% 
  filter(TOTAL_ADULTS > 0) %>%
  group_by(UrbanRural) %>%
  summarise(IncidentCases = sum(EXPECTED_ALLCAUSE_CASES, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0)

#View table
view(Table_IncidentCases_LivLoc)

#Save table as csv file
write.csv(Table_IncidentCases_LivLoc, 
          "R_tables/04Burden_Estimates_tables/Incident_cases_tables/Table_IncidentCases_LivLoc.csv")


#Manually join all sociodemographics tables together in Word doc or Excel sheet
#Save combined table to working directory



##PART 4.1.2: TABLE OF ADULT ASTHMA INCIDENT CASES BY STATE ----

#Table of incident cases by state
Table_IncidentCases_state <- burden %>% 
  group_by(STATE.x) %>%  
  summarise(IncidentCases = sum(EXPECTED_ALLCAUSE_CASES, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0)

#View table
view(Table_IncidentCases_state)

#Save table as csv file
write_csv(Table_IncidentCases_state, 
          "R_tables/04Burden_Estimates_tables/Incident_cases_tables/Table_IncidentCases_state.csv")



##PART 4.2: TABLES OF ATTRIBUTABLE CASES ----

#Total attributable cases (AC)
Table_AttributableCases_ALL <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  group_by(sum(TOTAL_ADULTS)) %>% 
  summarise(AttributableCases = sum(AC, na.rm = T), 
            LowerAttributableCases = sum(AC_LOWER_CI, na.rm = T),
            UpperAttributableCases = sum(AC_UPPER_CI, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_AttributableCases_ALL) <- c("TOTAL_ADULT_POP", "AttributableCases",
                                           "LowerAttributableCases",
                                           "UpperAttributableCases")
#View table
view(Table_AttributableCases_ALL)



##PART 4.2.1: TABLES OF ATTRIBUTABLE CASES BY SOCIODEMOGRAPHIC TRAITS ----

#White pop
#First calculate AC for white pop - estimated, lower limit, and upper limit values
AC_WHITE <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC = PIF * EXPECTED_WHITE) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_WHITE_LOWER <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC_LOWER_CI = PIF_LOWER_CI * EXPECTED_WHITE) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_WHITE_UPPER <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC_UPPER_CI = PIF_UPPER_CI * EXPECTED_WHITE) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Make table of AC for white pop
Table_AttributableCases_white <- burden %>% 
  group_by(sum(WHITE_ALONE)) %>%
  summarise(AttributableCases = sum(AC_WHITE, na.rm = T),
            LowerAttributableCases = sum(AC_WHITE_LOWER, na.rm = T),
            UpperAttributableCases = sum(AC_WHITE_UPPER, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_AttributableCases_white) <- c("TOTAL_ADULT_POP", "AttributableCases",
                                           "LowerAttributableCases",
                                           "UpperAttributableCases")
#View table
view(Table_AttributableCases_white)



#Black pop
AC_BLACK <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC = PIF * EXPECTED_BLACK) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_BLACK_LOWER <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC_LOWER_CI = PIF_LOWER_CI * EXPECTED_BLACK) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_BLACK_UPPER <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC_UPPER_CI = PIF_UPPER_CI * EXPECTED_BLACK) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Make table of AC for Black pop
Table_AttributableCases_black <- burden %>% 
  group_by(sum(BLACK_ALONE)) %>%
  summarise(AttributableCases = sum(AC_BLACK, na.rm = T),
            LowerAttributableCases = sum(AC_BLACK_LOWER, na.rm = T),
            UpperAttributableCases = sum(AC_BLACK_UPPER, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_AttributableCases_black) <- c("TOTAL_ADULT_POP", "AttributableCases",
                                             "LowerAttributableCases",
                                             "UpperAttributableCases")
#View table
view(Table_AttributableCases_black)



#AIAN pop
AC_AIAN <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC = PIF * EXPECTED_AIAN) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_AIAN_LOWER <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC_LOWER_CI = PIF_LOWER_CI * EXPECTED_AIAN) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_AIAN_UPPER <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC_UPPER_CI = PIF_UPPER_CI * EXPECTED_AIAN) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Make table of AC for AIAN pop
Table_AttributableCases_AIAN <- burden %>% 
  group_by(sum(AIAN_ALONE)) %>%
  summarise(AttributableCases = sum(AC_AIAN, na.rm = T),
            LowerAttributableCases = sum(AC_AIAN_LOWER, na.rm = T),
            UpperAttributableCases = sum(AC_AIAN_UPPER, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_AttributableCases_AIAN) <- c("TOTAL_ADULT_POP", "AttributableCases",
                                             "LowerAttributableCases",
                                             "UpperAttributableCases")
#View table
view(Table_AttributableCases_AIAN)



#Asian pop
AC_ASIAN <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC = PIF * EXPECTED_ASIAN) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_ASIAN_LOWER <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC_LOWER_CI = PIF_LOWER_CI * EXPECTED_ASIAN) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_ASIAN_UPPER <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC_UPPER_CI = PIF_UPPER_CI * EXPECTED_ASIAN) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Make table of AC for Asian pop
Table_AttributableCases_asian <- burden %>% 
  group_by(sum(ASIAN_ALONE)) %>%
  summarise(AttributableCases = sum(AC_ASIAN, na.rm = T),
            LowerAttributableCases = sum(AC_ASIAN_LOWER, na.rm = T),
            UpperAttributableCases = sum(AC_ASIAN_UPPER, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_AttributableCases_asian) <- c("TOTAL_ADULT_POP", "AttributableCases",
                                            "LowerAttributableCases",
                                            "UpperAttributableCases")
#View table
view(Table_AttributableCases_asian)



#NHPI pop
AC_NHPI <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC = PIF * EXPECTED_NHPI) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_NHPI_LOWER <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC_LOWER_CI = PIF_LOWER_CI * EXPECTED_NHPI) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_NHPI_UPPER <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC_UPPER_CI = PIF_UPPER_CI * EXPECTED_NHPI) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Make table of AC for NHPI pop
Table_AttributableCases_NHPI <- burden %>% 
  group_by(sum(NHPI_ALONE)) %>%
  summarise(AttributableCases = sum(AC_NHPI, na.rm = T),
            LowerAttributableCases = sum(AC_NHPI_LOWER, na.rm = T),
            UpperAttributableCases = sum(AC_NHPI_UPPER, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_AttributableCases_NHPI) <- c("TOTAL_ADULT_POP", "AttributableCases",
                                             "LowerAttributableCases",
                                             "UpperAttributableCases")
#View table
view(Table_AttributableCases_NHPI)



#Other pop
AC_OTHER <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC = PIF * EXPECTED_OTHER) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_OTHER_LOWER <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC_LOWER_CI = PIF_LOWER_CI * EXPECTED_OTHER) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_OTHER_UPPER <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC_UPPER_CI = PIF_UPPER_CI * EXPECTED_OTHER) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Make table of AC for other race pop
Table_AttributableCases_other <- burden %>% 
  group_by(sum(OTHER_ALONE)) %>%
  summarise(AttributableCases = sum(AC_OTHER, na.rm = T),
            LowerAttributableCases = sum(AC_OTHER_LOWER, na.rm = T),
            UpperAttributableCases = sum(AC_OTHER_UPPER, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_AttributableCases_other) <- c("TOTAL_ADULT_POP", "AttributableCases",
                                            "LowerAttributableCases",
                                            "UpperAttributableCases")
#View table
view(Table_AttributableCases_other)



#Two or more races pop
AC_TWOMORE <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC = PIF * EXPECTED_TWOMORE) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_TWOMORE_LOWER <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC_LOWER_CI = PIF_LOWER_CI * EXPECTED_TWOMORE) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_TWOMORE_UPPER <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC_UPPER_CI = PIF_UPPER_CI * EXPECTED_TWOMORE) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Make table of AC for two or more races pop
Table_AttributableCases_twomore <- burden %>% 
  group_by(sum(TWO_MORE)) %>%
  summarise(AttributableCases = sum(AC_TWOMORE, na.rm = T),
            LowerAttributableCases = sum(AC_TWOMORE_LOWER, na.rm = T),
            UpperAttributableCases = sum(AC_TWOMORE_UPPER, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_AttributableCases_twomore) <- c("TOTAL_ADULT_POP", "AttributableCases",
                                               "LowerAttributableCases",
                                               "UpperAttributableCases")
#View table
view(Table_AttributableCases_twomore)



#Hispanic Latino total pop
AC_HisLat <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC = PIF * EXPECTED_HisLat) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_HisLat_LOWER <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC_LOWER_CI = PIF_LOWER_CI * EXPECTED_HisLat) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_HisLat_UPPER <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC_UPPER_CI = PIF_UPPER_CI * EXPECTED_HisLat) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Make table of AC for Hispanic Latino pop
Table_AttributableCases_HisLat <- burden %>% 
  group_by(sum(HisLat_TOTAL)) %>%
  summarise(AttributableCases = sum(AC_HisLat, na.rm = T),
            LowerAttributableCases = sum(AC_HisLat_LOWER, na.rm = T),
            UpperAttributableCases = sum(AC_HisLat_UPPER, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_AttributableCases_HisLat) <- c("TOTAL_ADULT_POP", "AttributableCases",
                                              "LowerAttributableCases",
                                              "UpperAttributableCases")
#View table
view(Table_AttributableCases_HisLat)



#Non-Hispanic Latino total pop
AC_NotHisLat <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC = PIF * EXPECTED_NotHisLat) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_NotHisLat_LOWER <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC_LOWER_CI = PIF_LOWER_CI * EXPECTED_NotHisLat) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_NotHisLat_UPPER <- burden %>% 
  filter(TOTAL_ADULTS > 0) %>%
  summarise(AC_UPPER_CI = PIF_UPPER_CI * EXPECTED_NotHisLat) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Make table of AC for Non-Hispanic Latino pop
Table_AttributableCases_NotHisLat <- burden %>% 
  group_by(sum(NotHisLat_TOTAL)) %>%
  summarise(AttributableCases = sum(AC_NotHisLat, na.rm = T),
            LowerAttributableCases = sum(AC_NotHisLat_LOWER, na.rm = T),
            UpperAttributableCases = sum(AC_NotHisLat_UPPER, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Rename columns
colnames(Table_AttributableCases_NotHisLat) <- c("TOTAL_ADULT_POP", "AttributableCases",
                                              "LowerAttributableCases",
                                              "UpperAttributableCases")
#View table
view(Table_AttributableCases_NotHisLat)


#Combine race and Hispanic Latino tables
NO2_AttributableCases_RaceHisLat_combined <- rbind(Table_AttributableCases_white,
                                               Table_AttributableCases_black,
                                               Table_AttributableCases_AIAN,
                                               Table_AttributableCases_asian,
                                               Table_AttributableCases_NHPI,
                                               Table_AttributableCases_other,
                                               Table_AttributableCases_twomore,
                                               Table_AttributableCases_HisLat,
                                               Table_AttributableCases_NotHisLat)
#Rename row names
rownames(NO2_AttributableCases_RaceHisLat_combined) <- c("WHITE_ALONE", "BLACK_ALONE", 
                                                     "AIAN_ALONE", "ASIAN_ALONE", 
                                                     "NHPI_ALONE", "OTHER_ALONE",
                                                     "TWO_MORE", "Hispanic_Latino", 
                                                     "Not_Hispanic_Latino")
#View table
view(NO2_AttributableCases_RaceHisLat_combined)

#Save combined tables as csv file
write.csv(NO2_AttributableCases_RaceHisLat_combined, 
          "R_tables/04Burden_Estimates_tables/Attributable_cases_tables/NO2_AttributableCases_RaceHisLat_combined.csv")



#By median household income
Table_AttributableCases_income <- burden_clean_income %>%
  filter(TOTAL_ADULTS > 0) %>%
  group_by(INCOME_GROUP) %>%
  summarise(AttributableCases = sum(AC, na.rm = T),
            LowerAttributableCases = sum(AC_LOWER_CI, na.rm = T),
            UpperAttributableCases = sum(AC_UPPER_CI, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0)  %>%
  arrange(factor(INCOME_GROUP, levels = c("<$27,000", "$27,000 to <$52,000",
                                          "$52,000 to <$85,000",
                                          "$85,000 to <$141,000", ">=$141,000")))
#View table
view(Table_AttributableCases_income)

#Save table as csv file
write.csv(Table_AttributableCases_income, 
          "R_tables/04Burden_Estimates_tables/Attributable_cases_tables/Table_AttributableCases_income.csv")


#By living location
Table_AttributableCases_LivLoc <- burden_clean_livinglocation %>% 
  filter(TOTAL_ADULTS > 0) %>%
  group_by(UrbanRural) %>%  
  summarise(AttributableCases = sum(AC, na.rm = T), 
            LowerAttributableCases = sum(AC_LOWER_CI, na.rm = T),
            UpperAttributableCases = sum(AC_UPPER_CI, na.rm = T)) %>% 
  mutate_if(is.numeric, round, digits = 0)

#View table
view(Table_AttributableCases_LivLoc)

#Save table as csv file
write.csv(Table_AttributableCases_LivLoc, 
          "R_tables/04Burden_Estimates_tables/Attributable_cases_tables/Table_AttributableCases_LivLoc.csv")


#Manually join all sociodemographics tables together in Word doc or Excel sheet
#Save combined table to working directory



##PART 4.2.2: TABLE OF ATTRIBUTABLE CASES BY STATE ----

#AC table by state
Table_AttributableCases_state <- burden %>%
  group_by(STATE.x) %>%
  summarise(AttributableCases = sum(AC, na.rm = T),
            LowerAttributableCases = sum(AC_LOWER_CI, na.rm = T),
            UpperAttributableCases = sum(AC_UPPER_CI, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0)

#View table
view(Table_AttributableCases_state)

#Save table as csv file
write.csv(Table_AttributableCases_state, 
          "R_tables/04Burden_Estimates_tables/Attributable_cases_tables/Table_AttributableCases_state.csv")




##PART 4.3: TABLES OF POPULATION IMPACT FRACTION (PIF) ----

#Table of PIF by race
view(Table_IncidentCases_ALL) #view incidence table
view(Table_AttributableCases_ALL) #view AC table

#Combine both tables
Table_IncidentAttributable_ALL_combined <- Table_IncidentCases_ALL %>% 
  left_join(Table_AttributableCases_ALL, by = "TOTAL_ADULT_POP")

#View table
view(Table_IncidentAttributable_ALL_combined)

#Create table with PIF calculations
Table_PIF_ALL <- Table_IncidentAttributable_ALL_combined %>% 
  mutate(ImpactFraction = ((as.numeric(AttributableCases)/as.numeric(IncidentCases))*100),
         LowerImpactFraction = ((as.numeric(LowerAttributableCases)/as.numeric(IncidentCases))*100),
         UpperImpactFraction = ((as.numeric(UpperAttributableCases)/as.numeric(IncidentCases))*100)) %>%
  mutate_if(is.numeric, round, digits = 1)

#View table
view(Table_PIF_ALL)

#Save table as csv file
write_csv(Table_PIF_ALL, "R_tables/04Burden_Estimates_tables/PIF_tables/Table_PIF_ALL.csv")



##PART 4.3.1: TABLES OF POPULATION IMPACT FRACTION BY SOCIODEMOGRAPHIC TRAITS ----

#Table of PIF by race
view(NO2_IncidentCases_RaceHisLat_combined) #view incidence table
view(NO2_AttributableCases_RaceHisLat_combined) #view AC table

#Combine both tables
Table_IncidentAttributable_Race_combined <- NO2_IncidentCases_RaceHisLat_combined %>% 
  left_join(NO2_AttributableCases_RaceHisLat_combined, by = "TOTAL_ADULT_POP")

#Rename row names
rownames(Table_IncidentAttributable_Race_combined) <- c("WHITE_ALONE", "BLACK_ALONE",
                                                   "AIAN_ALONE", "ASIAN_ALONE",
                                                   "NHPI_ALONE", "OTHER_ALONE",
                                                   "TWO_MORE", "Hispanic_Latino",
                                                   "Not_Hispanic_Latino")
#View table
view(Table_IncidentAttributable_Race_combined)

#Create table with PIF calculations
Table_PIF_Race <- Table_IncidentAttributable_Race_combined %>% 
  mutate(ImpactFraction = ((as.numeric(AttributableCases)/as.numeric(IncidentCases))*100),
         LowerImpactFraction = ((as.numeric(LowerAttributableCases)/as.numeric(IncidentCases))*100),
         UpperImpactFraction = ((as.numeric(UpperAttributableCases)/as.numeric(IncidentCases))*100)) %>%
  mutate_if(is.numeric, round, digits = 1)

#View table
view(Table_PIF_Race)

#Save table as csv file
write_csv(Table_PIF_Race, "R_tables/04Burden_Estimates_tables/PIF_tables/Table_PIF_Race.csv")


#Table of PIF by income
view(Table_IncidentCases_income) #view incidence table
view(Table_AttributableCases_income) #view AC table

#Combine both tables
Table_IncidentAttributable_income_combined <- Table_IncidentCases_income %>% 
  left_join(Table_AttributableCases_income, by = "INCOME_GROUP")

#View table
view(Table_IncidentAttributable_income_combined)

#Create table with PIF calculations
Table_PIF_income <- Table_IncidentAttributable_income_combined %>% 
  mutate(ImpactFraction = ((as.numeric(AttributableCases)/as.numeric(IncidentCases))*100),
         LowerImpactFraction = ((as.numeric(LowerAttributableCases)/as.numeric(IncidentCases))*100),
         UpperImpactFraction = ((as.numeric(UpperAttributableCases)/as.numeric(IncidentCases))*100)) %>%
  mutate_if(is.numeric, round, digits = 1)

#View table
view(Table_PIF_income)

#Save table as csv file
write_csv(Table_PIF_income, "R_tables/04Burden_Estimates_tables/PIF_tables/Table_PIF_income.csv")


#Table of PIF by living location
view(Table_IncidentCases_LivLoc) #view incidence table
view(Table_AttributableCases_LivLoc) #view AC table

#Combine both tables
Table_IncidentAttributable_LivLoc_combined <- Table_IncidentCases_LivLoc %>% 
  left_join(Table_AttributableCases_LivLoc, by = "UrbanRural")

#View table
view(Table_IncidentAttributable_LivLoc_combined)

#Create table with PIF calculations
Table_PIF_LivLoc <- Table_IncidentAttributable_LivLoc_combined %>% 
  mutate(ImpactFraction = ((as.numeric(AttributableCases)/as.numeric(IncidentCases))*100),
         LowerImpactFraction = ((as.numeric(LowerAttributableCases)/as.numeric(IncidentCases))*100),
         UpperImpactFraction = ((as.numeric(UpperAttributableCases)/as.numeric(IncidentCases))*100)) %>%
  mutate_if(is.numeric, round, digits = 1)

#View table
view(Table_PIF_LivLoc)

#Save table as csv file
write_csv(Table_PIF_LivLoc, "R_tables/04Burden_Estimates_tables/PIF_tables/Table_PIF_LivLoc.csv")

#Manually join all sociodemographics tables together in Word doc or Excel sheet
#Save combined table to working directory




##PART 4.3.2: TABLE OF IMPACT FRACTION BY STATE ----

view(Table_IncidentCases_state)
view(Table_AttributableCases_state)

#Combine previous two tables
Table_IncidentCases_state_combined <- Table_IncidentCases_state %>% 
  left_join(Table_AttributableCases_state, by = "STATE.x") %>% 
  as_tibble()

#View table
view(Table_IncidentCases_state_combined)

#Create table with PIF calculations
Table_PIF_state <- Table_IncidentCases_state_combined %>% 
  mutate(ImpactFraction = ((as.numeric(AttributableCases)/as.numeric(IncidentCases))*100),
         LowerImpactFraction = ((as.numeric(LowerAttributableCases)/as.numeric(IncidentCases))*100),
         UpperImpactFraction = ((as.numeric(UpperAttributableCases)/as.numeric(IncidentCases))*100)) %>%
  mutate_if(is.numeric, round, digits = 1)

#View table
view(Table_PIF_state)

#Save PIF table as csv
write_csv(Table_PIF_state, 
          "R_tables/04Burden_Estimates_tables/PIF_tables/Table_PIF_state.csv")



