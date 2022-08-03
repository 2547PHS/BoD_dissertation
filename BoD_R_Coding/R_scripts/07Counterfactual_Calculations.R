#------------------------------------------------------------------------------#
#Project : BoD U.S. adult-onset asthma and NO2 exposure
#Sub     : Demographics
#Part    : (07)  Counterfactual scenarios
#Purpose : Create counterfactual scenarios and tables
#Created by PHS Candidate 2547
#Date Created: 19-June-2022
#Last Updated: 30-July-2022
#------------------------------------------------------------------------------#

##Note: must run "01Burden_DataSet.R" script first##


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




##PART 7.1: COUNTERFACTUAL FOR U.S. ADULT POP ----

burden_WHO_ALL <- burden %>%
  select(YEAR, TOTAL_ADULTS, NO2_LEVELS, EXPECTED_ALLCAUSE_CASES, ERF, ERF_LOWER_CI, 
         ERF_UPPER_CI, UNIT, EXPECTED_ALLCAUSE_CASES_LOWER_CI, EXPECTED_ALLCAUSE_CASES_UPPER_CI) %>% 
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew), 
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC = PIF * EXPECTED_ALLCAUSE_CASES,
         AC_LOWER_CI = PIF_LOWER_CI * EXPECTED_ALLCAUSE_CASES_LOWER_CI,
         AC_UPPER_CI = PIF_UPPER_CI * EXPECTED_ALLCAUSE_CASES_UPPER_CI) %>%
  select(-ERF, -UNIT, -RRnew) %>% 
  group_by(sum(TOTAL_ADULTS)) %>% 
  summarise(AttributableCases = sum(AC, na.rm = T),
            LowerAttributableCases = sum(AC_LOWER_CI, na.rm = T),
            UpperAttributableCases = sum(AC_UPPER_CI, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

#Change column name
colnames(burden_WHO_ALL) <- c("TOTAL_ADULT_POP", "AttributableCases",
                              "LowerAttributableCases", "UpperAttributableCases")


#Combine counterfactual and baseline total adults pop tables
burden_counterfactual_ALL <- bind_cols(Table_AttributableCases_ALL, burden_WHO_ALL)
burden_counterfactual_ALL <- burden_counterfactual_ALL[,-c(5)] #get rid of unnecessary column

#Rename columns
colnames(burden_counterfactual_ALL) <- c("TOTAL_ADULT_POP", "AttributableCases_baseline",
                                         "LowerAttributableCases_baseline",
                                         "UpperAttributableCases_baseline",
                                         "AttributableCases_counterfactual",
                                         "LowerAttributableCases_counterfactual",
                                         "UpperAttributableCases_counterfactual")
#Check table
view(burden_counterfactual_ALL)

#Find the difference between baseline and counterfactual ACs
burden_counterfactual_ALL <- burden_counterfactual_ALL %>%
  mutate(AttributableCases_difference = AttributableCases_baseline - AttributableCases_counterfactual,
         LowerAttributableCases_difference = LowerAttributableCases_baseline - LowerAttributableCases_counterfactual,
         UpperAttributableCases_difference = UpperAttributableCases_baseline - UpperAttributableCases_counterfactual)

#View table
view(burden_counterfactual_ALL)

#Save combined tables as csv file
write.csv(burden_counterfactual_ALL, 
          "R_tables/07Counterfactual_tables/burden_counterfactual_ALL.csv")



##PART 7.2: COUNTERFACTUAL BY RACE AND ETHNICITY ----

#White pop
#First, join 'EXPECTED_WHITE' column to 'burden' dataset for calculations.
#This time, without filtering for WHITE_ALONE > 0 to match row numbers
EXPECTED_WHITE <- burden %>%
  select(WHITE_ALONE, PRV, PRV_LOWER, PRV_UPPER, PIF, PIF_LOWER_CI, PIF_UPPER_CI,
         RRnew, RRnew_LOWER_CI, RRnew_UPPER_CI, TOTAL_ADULTS) %>% 
  mutate(EXPECTED_CASES_WHITE = (WHITE_ALONE - (WHITE_ALONE * PRV)) * IR,
         EXPECTED_CASES_WHITE_LOWER_CI =
           (WHITE_ALONE - (WHITE_ALONE * PRV_LOWER)) * IR_LOWER,
         EXPECTED_CASES_WHITE_UPPER_CI =
           (WHITE_ALONE - (WHITE_ALONE * PRV_UPPER)) * IR_UPPER) %>%
  mutate_if(is.numeric, round, digits = 3)

burden <- burden %>%
  mutate(EXPECTED_CASES_WHITE = (WHITE_ALONE - (WHITE_ALONE * PRV)) * IR,
         EXPECTED_CASES_WHITE_LOWER_CI = (WHITE_ALONE - (WHITE_ALONE * PRV_LOWER)) * IR_LOWER, 
         EXPECTED_CASES_WHITE_UPPER_CI = (WHITE_ALONE - (WHITE_ALONE * PRV_UPPER)) * IR_UPPER)

#Calculate counterfactual scenario for white pop
burden_WHO_white <- burden %>%
  select(YEAR, WHITE_ALONE, NO2_LEVELS, EXPECTED_CASES_WHITE, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT, EXPECTED_CASES_WHITE_LOWER_CI, 
         EXPECTED_CASES_WHITE_UPPER_CI, TOTAL_ADULTS) %>% 
  filter(TOTAL_ADULTS > 0) %>%
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew),
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC_WHITE = PIF * EXPECTED_CASES_WHITE,
         AC_WHITE_LOWER_CI = PIF_LOWER_CI * EXPECTED_CASES_WHITE_LOWER_CI,
         AC_WHITE_UPPER_CI = PIF_UPPER_CI * EXPECTED_CASES_WHITE_UPPER_CI) %>%
  select(-ERF, -UNIT, -RRnew) %>%
  group_by(sum(WHITE_ALONE)) %>%
  summarise(AttributableCases = sum(AC_WHITE, na.rm = T),
            LowerAttributableCases = sum(AC_WHITE_LOWER_CI, na.rm = T),
            UpperAttributableCases = sum(AC_WHITE_UPPER_CI, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0)

#Rename columns
colnames(burden_WHO_white) <- c("TOTAL_ADULT_POP", "AttributableCases",
                                "LowerAttributableCases", "UpperAttributableCases")
#View table
view(burden_WHO_white)


#Black pop
EXPECTED_BLACK <- burden %>%
  select(BLACK_ALONE, PRV, PRV_LOWER, PRV_UPPER, PIF, PIF_LOWER_CI, PIF_UPPER_CI,
         RRnew, RRnew_LOWER_CI, RRnew_UPPER_CI, TOTAL_ADULTS) %>% 
  mutate(EXPECTED_CASES_BLACK = (BLACK_ALONE - (BLACK_ALONE * PRV)) * IR,
         EXPECTED_CASES_BLACK_LOWER_CI =
           (BLACK_ALONE - (BLACK_ALONE * PRV_LOWER)) * IR_LOWER,
         EXPECTED_CASES_BLACK_UPPER_CI =
           (BLACK_ALONE - (BLACK_ALONE * PRV_UPPER)) * IR_UPPER) %>%
  mutate_if(is.numeric, round, digits = 3)

burden <- burden %>%
  mutate(EXPECTED_CASES_BLACK = (BLACK_ALONE - (BLACK_ALONE * PRV)) * IR,
         EXPECTED_CASES_BLACK_LOWER_CI =
           (BLACK_ALONE - (BLACK_ALONE * PRV_LOWER)) * IR_LOWER,
         EXPECTED_CASES_BLACK_UPPER_CI =
           (BLACK_ALONE - (BLACK_ALONE * PRV_UPPER)) * IR_UPPER)

#Calculate counterfactual scenario for Black pop
burden_WHO_black <- burden %>%
  select(YEAR, BLACK_ALONE, NO2_LEVELS, EXPECTED_CASES_BLACK, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT, EXPECTED_CASES_BLACK_LOWER_CI, 
         EXPECTED_CASES_BLACK_UPPER_CI, TOTAL_ADULTS) %>% 
  filter(TOTAL_ADULTS > 0) %>%
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew),
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC_BLACK = PIF * EXPECTED_CASES_BLACK,
         AC_BLACK_LOWER_CI = PIF_LOWER_CI * EXPECTED_CASES_BLACK_LOWER_CI,
         AC_BLACK_UPPER_CI = PIF_UPPER_CI * EXPECTED_CASES_BLACK_UPPER_CI) %>%
  select(-ERF, -UNIT, -RRnew) %>%
  group_by(sum(BLACK_ALONE)) %>%
  summarise(AttributableCases = sum(AC_BLACK, na.rm = T),
            LowerAttributableCases = sum(AC_BLACK_LOWER_CI, na.rm = T),
            UpperAttributableCases = sum(AC_BLACK_UPPER_CI, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0)

#Rename columns
colnames(burden_WHO_black) <- c("TOTAL_ADULT_POP", "AttributableCases",
                                "LowerAttributableCases", "UpperAttributableCases")
#View table
view(burden_WHO_black)


#AIAN pop
EXPECTED_AIAN <- burden %>%
  select(AIAN_ALONE, PRV, PRV_LOWER, PRV_UPPER, PIF, PIF_LOWER_CI, PIF_UPPER_CI,
         RRnew, RRnew_LOWER_CI, RRnew_UPPER_CI, TOTAL_ADULTS) %>% 
  mutate(EXPECTED_CASES_AIAN = (AIAN_ALONE - (AIAN_ALONE * PRV)) * IR,
         EXPECTED_CASES_AIAN_LOWER_CI =
           (AIAN_ALONE - (AIAN_ALONE * PRV_LOWER)) * IR_LOWER,
         EXPECTED_CASES_AIAN_UPPER_CI =
           (AIAN_ALONE - (AIAN_ALONE * PRV_UPPER)) * IR_UPPER) %>%
  mutate_if(is.numeric, round, digits = 3)

burden <- burden %>%
  mutate(EXPECTED_CASES_AIAN = (AIAN_ALONE - (AIAN_ALONE * PRV)) * IR,
         EXPECTED_CASES_AIAN_LOWER_CI =
           (AIAN_ALONE - (AIAN_ALONE * PRV_LOWER)) * IR_LOWER,
         EXPECTED_CASES_AIAN_UPPER_CI =
           (AIAN_ALONE - (AIAN_ALONE * PRV_UPPER)) * IR_UPPER)

#Calculate counterfactual scenario for AIAN pop
burden_WHO_AIAN <- burden %>%
  select(YEAR, AIAN_ALONE, NO2_LEVELS, EXPECTED_CASES_AIAN, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT, EXPECTED_CASES_AIAN_LOWER_CI, 
         EXPECTED_CASES_AIAN_UPPER_CI, TOTAL_ADULTS) %>% 
  filter(TOTAL_ADULTS > 0) %>%
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew),
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC_AIAN = PIF * EXPECTED_CASES_AIAN,
         AC_AIAN_LOWER_CI = PIF_LOWER_CI * EXPECTED_CASES_AIAN_LOWER_CI,
         AC_AIAN_UPPER_CI = PIF_UPPER_CI * EXPECTED_CASES_AIAN_UPPER_CI) %>%
  select(-ERF, -UNIT, -RRnew) %>%
  group_by(sum(AIAN_ALONE)) %>%
  summarise(AttributableCases = sum(AC_AIAN, na.rm = T),
            LowerAttributableCases = sum(AC_AIAN_LOWER_CI, na.rm = T),
            UpperAttributableCases = sum(AC_AIAN_UPPER_CI, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0)

#Rename columns
colnames(burden_WHO_AIAN) <- c("TOTAL_ADULT_POP", "AttributableCases",
                                "LowerAttributableCases", "UpperAttributableCases")
#View table
view(burden_WHO_AIAN)


#Asian pop
EXPECTED_ASIAN <- burden %>%
  select(ASIAN_ALONE, PRV, PRV_LOWER, PRV_UPPER, PIF, PIF_LOWER_CI, PIF_UPPER_CI,
         RRnew, RRnew_LOWER_CI, RRnew_UPPER_CI, TOTAL_ADULTS) %>% 
  mutate(EXPECTED_CASES_ASIAN = (ASIAN_ALONE - (ASIAN_ALONE * PRV)) * IR,
         EXPECTED_CASES_ASIAN_LOWER_CI =
           (ASIAN_ALONE - (ASIAN_ALONE * PRV_LOWER)) * IR_LOWER,
         EXPECTED_CASES_ASIAN_UPPER_CI =
           (ASIAN_ALONE - (ASIAN_ALONE * PRV_UPPER)) * IR_UPPER) %>%
  mutate_if(is.numeric, round, digits = 3)

burden <- burden %>%
  mutate(EXPECTED_CASES_ASIAN = (ASIAN_ALONE - (ASIAN_ALONE * PRV)) * IR,
         EXPECTED_CASES_ASIAN_LOWER_CI =
           (ASIAN_ALONE - (ASIAN_ALONE * PRV_LOWER)) * IR_LOWER,
         EXPECTED_CASES_ASIAN_UPPER_CI =
           (ASIAN_ALONE - (ASIAN_ALONE * PRV_UPPER)) * IR_UPPER)

#Calculate counterfactual scenario for Asian pop
burden_WHO_asian <- burden %>%
  select(YEAR, ASIAN_ALONE, NO2_LEVELS, EXPECTED_CASES_ASIAN, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT, EXPECTED_CASES_ASIAN_LOWER_CI, 
         EXPECTED_CASES_ASIAN_UPPER_CI, TOTAL_ADULTS) %>% 
  filter(TOTAL_ADULTS > 0) %>%
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew),
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC_ASIAN = PIF * EXPECTED_CASES_ASIAN,
         AC_ASIAN_LOWER_CI = PIF_LOWER_CI * EXPECTED_CASES_ASIAN_LOWER_CI,
         AC_ASIAN_UPPER_CI = PIF_UPPER_CI * EXPECTED_CASES_ASIAN_UPPER_CI) %>%
  select(-ERF, -UNIT, -RRnew) %>%
  group_by(sum(ASIAN_ALONE)) %>%
  summarise(AttributableCases = sum(AC_ASIAN, na.rm = T),
            LowerAttributableCases = sum(AC_ASIAN_LOWER_CI, na.rm = T),
            UpperAttributableCases = sum(AC_ASIAN_UPPER_CI, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0)

#Rename columns
colnames(burden_WHO_asian) <- c("TOTAL_ADULT_POP", "AttributableCases",
                                "LowerAttributableCases", "UpperAttributableCases")
#View table
view(burden_WHO_asian)


#NHPI pop
EXPECTED_NHPI <- burden %>%
  select(NHPI_ALONE, PRV, PRV_LOWER, PRV_UPPER, PIF, PIF_LOWER_CI, PIF_UPPER_CI,
         RRnew, RRnew_LOWER_CI, RRnew_UPPER_CI, TOTAL_ADULTS) %>% 
  mutate(EXPECTED_CASES_NHPI = (NHPI_ALONE - (NHPI_ALONE * PRV)) * IR,
         EXPECTED_CASES_NHPI_LOWER_CI =
           (NHPI_ALONE - (NHPI_ALONE * PRV_LOWER)) * IR_LOWER,
         EXPECTED_CASES_NHPI_UPPER_CI =
           (NHPI_ALONE - (NHPI_ALONE * PRV_UPPER)) * IR_UPPER) %>%
  mutate_if(is.numeric, round, digits = 3)

burden <- burden %>%
  mutate(EXPECTED_CASES_NHPI = (NHPI_ALONE - (NHPI_ALONE * PRV)) * IR,
         EXPECTED_CASES_NHPI_LOWER_CI =
           (NHPI_ALONE - (NHPI_ALONE * PRV_LOWER)) * IR_LOWER,
         EXPECTED_CASES_NHPI_UPPER_CI =
           (NHPI_ALONE - (NHPI_ALONE * PRV_UPPER)) * IR_UPPER)

#Calculate counterfactual scenario for NHPI pop
burden_WHO_NHPI <- burden %>%
  select(YEAR, NHPI_ALONE, NO2_LEVELS, EXPECTED_CASES_NHPI, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT, EXPECTED_CASES_NHPI_LOWER_CI, 
         EXPECTED_CASES_NHPI_UPPER_CI, TOTAL_ADULTS) %>% 
  filter(TOTAL_ADULTS > 0) %>%
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew),
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC_NHPI = PIF * EXPECTED_CASES_NHPI,
         AC_NHPI_LOWER_CI = PIF_LOWER_CI * EXPECTED_CASES_NHPI_LOWER_CI,
         AC_NHPI_UPPER_CI = PIF_UPPER_CI * EXPECTED_CASES_NHPI_UPPER_CI) %>%
  select(-ERF, -UNIT, -RRnew) %>%
  group_by(sum(NHPI_ALONE)) %>%
  summarise(AttributableCases = sum(AC_NHPI, na.rm = T),
            LowerAttributableCases = sum(AC_NHPI_LOWER_CI, na.rm = T),
            UpperAttributableCases = sum(AC_NHPI_UPPER_CI, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0)

#Rename columns
colnames(burden_WHO_NHPI) <- c("TOTAL_ADULT_POP", "AttributableCases",
                                "LowerAttributableCases", "UpperAttributableCases")
#View table
view(burden_WHO_NHPI)


#Other races pop
EXPECTED_OTHER <- burden %>%
  select(OTHER_ALONE, PRV, PRV_LOWER, PRV_UPPER, PIF, PIF_LOWER_CI, PIF_UPPER_CI,
         RRnew, RRnew_LOWER_CI, RRnew_UPPER_CI, TOTAL_ADULTS) %>% 
  mutate(EXPECTED_CASES_OTHER = (OTHER_ALONE - (OTHER_ALONE * PRV)) * IR,
         EXPECTED_CASES_OTHER_LOWER_CI =
           (OTHER_ALONE - (OTHER_ALONE * PRV_LOWER)) * IR_LOWER,
         EXPECTED_CASES_OTHER_UPPER_CI =
           (OTHER_ALONE - (OTHER_ALONE * PRV_UPPER)) * IR_UPPER) %>%
  mutate_if(is.numeric, round, digits = 3)

burden <- burden %>%
  mutate(EXPECTED_OTHER_CASES = (OTHER_ALONE - (OTHER_ALONE * PRV)) * IR,
         EXPECTED_CASES_OTHER_LOWER_CI =
           (OTHER_ALONE - (OTHER_ALONE * PRV_LOWER)) * IR_LOWER,
         EXPECTED_CASES_OTHER_UPPER_CI =
           (OTHER_ALONE - (OTHER_ALONE * PRV_UPPER)) * IR_UPPER)

#Calculate counterfactual scenario for other race pop
burden_WHO_other <- burden %>%
  select(YEAR, OTHER_ALONE, NO2_LEVELS, EXPECTED_OTHER_CASES, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT, EXPECTED_CASES_OTHER_LOWER_CI, 
         EXPECTED_CASES_OTHER_UPPER_CI, TOTAL_ADULTS) %>% 
  filter(TOTAL_ADULTS > 0) %>%
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew),
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC_OTHER = PIF * EXPECTED_OTHER_CASES,
         AC_OTHER_LOWER_CI = PIF_LOWER_CI * EXPECTED_CASES_OTHER_LOWER_CI,
         AC_OTHER_UPPER_CI = PIF_UPPER_CI * EXPECTED_CASES_OTHER_UPPER_CI) %>%
  select(-ERF, -UNIT, -RRnew) %>%
  group_by(sum(OTHER_ALONE)) %>%
  summarise(AttributableCases = sum(AC_OTHER, na.rm = T),
            LowerAttributableCases = sum(AC_OTHER_LOWER_CI, na.rm = T),
            UpperAttributableCases = sum(AC_OTHER_UPPER_CI, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0)

#Rename columns
colnames(burden_WHO_other) <- c("TOTAL_ADULT_POP", "AttributableCases",
                                "LowerAttributableCases", "UpperAttributableCases")
#View table
view(burden_WHO_other)


#Two or more races pop
EXPECTED_TWOMROE <- burden %>%
  select(TWO_MORE, PRV, PRV_LOWER, PRV_UPPER, PIF, PIF_LOWER_CI, PIF_UPPER_CI,
         RRnew, RRnew_LOWER_CI, RRnew_UPPER_CI, TOTAL_ADULTS) %>% 
  mutate(EXPECTED_CASES_TWOMORE = (TWO_MORE - (TWO_MORE * PRV)) * IR,
         EXPECTED_CASES_TWOMORE_LOWER_CI =
           (TWO_MORE - (TWO_MORE * PRV_LOWER)) * IR_LOWER,
         EXPECTED_CASES_TWO_MORE_UPPER_CI =
           (TWO_MORE - (TWO_MORE * PRV_UPPER)) * IR_UPPER) %>%
  mutate_if(is.numeric, round, digits = 3)

burden <- burden %>%
  mutate(EXPECTED_TWOMORE_CASES = (TWO_MORE - (TWO_MORE * PRV)) * IR,
         EXPECTED_CASES_TWOMORE_LOWER_CI =
           (TWO_MORE - (TWO_MORE * PRV_LOWER)) * IR_LOWER,
         EXPECTED_CASES_TWOMORE_UPPER_CI =
           (TWO_MORE - (TWO_MORE * PRV_UPPER)) * IR_UPPER)

#Calculate counterfactual scenario for two or more races pop
burden_WHO_twomore <- burden %>%
  select(YEAR, TWO_MORE, NO2_LEVELS, EXPECTED_TWOMORE_CASES, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT, EXPECTED_CASES_TWOMORE_LOWER_CI, 
         EXPECTED_CASES_TWOMORE_UPPER_CI, TOTAL_ADULTS) %>% 
  filter(TOTAL_ADULTS > 0) %>%
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew),
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC_TWOMORE = PIF * EXPECTED_TWOMORE_CASES,
         AC_TWOMORE_LOWER_CI = PIF_LOWER_CI * EXPECTED_CASES_TWOMORE_LOWER_CI,
         AC_TWOMORE_UPPER_CI = PIF_UPPER_CI * EXPECTED_CASES_TWOMORE_UPPER_CI) %>%
  select(-ERF, -UNIT, -RRnew) %>%
  group_by(sum(TWO_MORE)) %>%
  summarise(AttributableCases = sum(AC_TWOMORE, na.rm = T),
            LowerAttributableCases = sum(AC_TWOMORE_LOWER_CI, na.rm = T),
            UpperAttributableCases = sum(AC_TWOMORE_UPPER_CI, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0)

#Rename columns
colnames(burden_WHO_twomore) <- c("TOTAL_ADULT_POP", "AttributableCases",
                                "LowerAttributableCases", "UpperAttributableCases")
#View table
view(burden_WHO_twomore)


#Hispanic Latino pop
EXPECTED_HisLat <- burden %>%
  select(HisLat_TOTAL, PRV, PRV_LOWER, PRV_UPPER, PIF, PIF_LOWER_CI, PIF_UPPER_CI,
         RRnew, RRnew_LOWER_CI, RRnew_UPPER_CI, TOTAL_ADULTS) %>% 
  mutate(EXPECTED_CASES_HisLat = (HisLat_TOTAL - (HisLat_TOTAL * PRV)) * IR,
         EXPECTED_CASES_HisLat_LOWER_CI =
           (HisLat_TOTAL - (HisLat_TOTAL * PRV_LOWER)) * IR_LOWER,
         EXPECTED_CASES_HisLat_UPPER_CI =
           (HisLat_TOTAL - (HisLat_TOTAL * PRV_UPPER)) * IR_UPPER) %>%
  mutate_if(is.numeric, round, digits = 3)

burden <- burden %>%
  mutate(EXPECTED_HisLat_CASES = (HisLat_TOTAL - (HisLat_TOTAL * PRV)) * IR,
         EXPECTED_CASES_HisLat_LOWER_CI =
           (HisLat_TOTAL - (HisLat_TOTAL * PRV_LOWER)) * IR_LOWER,
         EXPECTED_CASES_HisLat_UPPER_CI =
           (HisLat_TOTAL - (HisLat_TOTAL * PRV_UPPER)) * IR_UPPER)

#Calculate counterfactual scenario for Hispanic Latino pop
burden_WHO_HisLat <- burden %>%
  select(YEAR, HisLat_TOTAL, NO2_LEVELS, EXPECTED_HisLat_CASES, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT, EXPECTED_CASES_HisLat_LOWER_CI, 
         EXPECTED_CASES_HisLat_UPPER_CI, TOTAL_ADULTS) %>% 
  filter(TOTAL_ADULTS > 0) %>%
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew),
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC_HisLat = PIF * EXPECTED_HisLat_CASES,
         AC_HisLat_LOWER_CI = PIF_LOWER_CI * EXPECTED_CASES_HisLat_LOWER_CI,
         AC_HisLat_UPPER_CI = PIF_UPPER_CI * EXPECTED_CASES_HisLat_UPPER_CI) %>%
  select(-ERF, -UNIT, -RRnew) %>%
  group_by(sum(HisLat_TOTAL)) %>%
  summarise(AttributableCases = sum(AC_HisLat, na.rm = T),
            LowerAttributableCases = sum(AC_HisLat_LOWER_CI, na.rm = T),
            UpperAttributableCases = sum(AC_HisLat_UPPER_CI, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0)

#Rename columns
colnames(burden_WHO_HisLat) <- c("TOTAL_ADULT_POP", "AttributableCases",
                               "LowerAttributableCases", "UpperAttributableCases")
#View table
view(burden_WHO_HisLat)


#Non-Hispanic Latino pop
EXPECTED_NotHisLat <- burden %>%
  select(NotHisLat_TOTAL, PRV, PRV_LOWER, PRV_UPPER, PIF, PIF_LOWER_CI, PIF_UPPER_CI,
         RRnew, RRnew_LOWER_CI, RRnew_UPPER_CI, TOTAL_ADULTS) %>% 
  mutate(EXPECTED_CASES_NotHisLat = (NotHisLat_TOTAL - (NotHisLat_TOTAL * PRV)) * IR,
         EXPECTED_CASES_NotHisLat_LOWER_CI =
           (NotHisLat_TOTAL - (NotHisLat_TOTAL * PRV_LOWER)) * IR_LOWER,
         EXPECTED_CASES_NotHisLat_UPPER_CI =
           (NotHisLat_TOTAL - (NotHisLat_TOTAL * PRV_UPPER)) * IR_UPPER) %>%
  mutate_if(is.numeric, round, digits = 3)

burden <- burden %>%
  mutate(EXPECTED_NotHisLat_CASES = (NotHisLat_TOTAL - (NotHisLat_TOTAL * PRV)) * IR,
         EXPECTED_CASES_NotHisLat_LOWER_CI =
           (NotHisLat_TOTAL - (NotHisLat_TOTAL * PRV_LOWER)) * IR_LOWER,
         EXPECTED_CASES_NotHisLat_UPPER_CI =
           (NotHisLat_TOTAL - (NotHisLat_TOTAL * PRV_UPPER)) * IR_UPPER)

#Calculate counterfactual scenario for Non-Hispanic Latino pop
burden_WHO_NotHisLat <- burden %>%
  select(YEAR, NotHisLat_TOTAL, NO2_LEVELS, EXPECTED_NotHisLat_CASES, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT, EXPECTED_CASES_NotHisLat_LOWER_CI, 
         EXPECTED_CASES_NotHisLat_UPPER_CI, TOTAL_ADULTS) %>% 
  filter(TOTAL_ADULTS > 0) %>%
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew),
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC_NotHisLat = PIF * EXPECTED_NotHisLat_CASES,
         AC_NotHisLat_LOWER_CI = PIF_LOWER_CI * EXPECTED_CASES_NotHisLat_LOWER_CI,
         AC_NotHisLat_UPPER_CI = PIF_UPPER_CI * EXPECTED_CASES_NotHisLat_UPPER_CI) %>%
  select(-ERF, -UNIT, -RRnew) %>%
  group_by(sum(NotHisLat_TOTAL)) %>%
  summarise(AttributableCases = sum(AC_NotHisLat, na.rm = T),
            LowerAttributableCases = sum(AC_NotHisLat_LOWER_CI, na.rm = T),
            UpperAttributableCases = sum(AC_NotHisLat_UPPER_CI, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0)

#Rename columns
colnames(burden_WHO_NotHisLat) <- c("TOTAL_ADULT_POP", "AttributableCases",
                                 "LowerAttributableCases", "UpperAttributableCases")
#View table
view(burden_WHO_NotHisLat)


#Combine race and Hispanic Latino tables
burden_WHO_RaceHisLat_combined <- rbind(burden_WHO_white, burden_WHO_black,
                                     burden_WHO_AIAN, burden_WHO_asian,
                                     burden_WHO_NHPI, burden_WHO_other,
                                     burden_WHO_twomore, burden_WHO_HisLat,
                                     burden_WHO_NotHisLat)
#Rename row names
rownames(burden_WHO_RaceHisLat_combined) <- c("WHITE_ALONE", "BLACK_ALONE", "AIAN_ALONE", 
                                           "ASIAN_ALONE", "NHPI_ALONE", "OTHER_ALONE",
                                           "TWO_MORE", "Hispanic_Latino",
                                           "Not_Hispanic_Latino")
#View table
view(burden_WHO_RaceHisLat_combined)


#Combine counterfactual and baseline AC by race & ethnicity tables
burden_counterfactual_race <- bind_cols(NO2_AttributableCases_RaceHisLat_combined,
                                      burden_WHO_RaceHisLat_combined)
burden_counterfactual_race <- burden_counterfactual_race[,-c(5)] #get rid of unnecessary column

#Rename columns
colnames(burden_counterfactual_race) <- c("TOTAL_ADULT_POP", "AttributableCases_baseline",
                                         "LowerAttributableCases_baseline",
                                         "UpperAttributableCases_baseline",
                                         "AttributableCases_counterfactual",
                                         "LowerAttributableCases_counterfactual",
                                         "UpperAttributableCases_counterfactual")
#Check table
view(burden_counterfactual_race)

#Find the difference between baseline and counterfactual ACs
burden_counterfactual_race <- burden_counterfactual_race %>%
  mutate(AttributableCases_difference = AttributableCases_baseline - AttributableCases_counterfactual,
         LowerAttributableCases_difference = LowerAttributableCases_baseline - LowerAttributableCases_counterfactual,
         UpperAttributableCases_difference = UpperAttributableCases_baseline - UpperAttributableCases_counterfactual)

#View table
view(burden_counterfactual_race)


#Save combined tables as csv file
write.csv(burden_counterfactual_race,
          "R_tables/07Counterfactual_tables/burden_counterfactual_race.csv")



##PART 7.3: COUNTERFACTUAL BY MEDIAN HOUSEHOLD INCOME ----

burden_WHO_income <- burden_clean_income %>%
  select(YEAR, INCOME_GROUP, NO2_LEVELS, EXPECTED_ALLCAUSE_CASES, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT, EXPECTED_ALLCAUSE_CASES_LOWER_CI,
         EXPECTED_ALLCAUSE_CASES_UPPER_CI) %>% 
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew), 
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC = PIF * EXPECTED_ALLCAUSE_CASES,
         AC_LOWER_CI = PIF_LOWER_CI * EXPECTED_ALLCAUSE_CASES_LOWER_CI,
         AC_UPPER_CI = PIF_UPPER_CI * EXPECTED_ALLCAUSE_CASES_UPPER_CI) %>%
  select(-ERF, -UNIT, -RRnew) %>%
  group_by(INCOME_GROUP) %>%
  summarise(AttributableCases = sum(AC, na.rm = T),
            LowerAttributableCases = sum(AC_LOWER_CI, na.rm = T),
            UpperAttributableCases = sum(AC_UPPER_CI, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  arrange(factor(INCOME_GROUP, levels = c("<$27,000", "$27,000 to <$52,000",
                                          "$52,000 to <$85,000",
                                          "$85,000 to <$141,000", ">=$141,000"))) %>%
  as.data.frame()

#Change column name
colnames(burden_WHO_income) <- c("TOTAL_ADULT_POP", "AttributableCases",
                              "LowerAttributableCases", "UpperAttributableCases")
#View table
view(burden_WHO_income)


#Combine counterfactual and baseline total adults pop tables
burden_counterfactual_income <- bind_cols(Table_AttributableCases_income, burden_WHO_income)
burden_counterfactual_income <- burden_counterfactual_income[,-c(5)] #get rid of unnecessary column

#Rename columns
colnames(burden_counterfactual_income) <- c("TOTAL_ADULT_POP", "AttributableCases_baseline",
                                            "LowerAttributableCases_baseline",
                                            "UpperAttributableCases_baseline",
                                            "AttributableCases_counterfactual",
                                            "LowerAttributableCases_counterfactual",
                                            "UpperAttributableCases_counterfactual")

#Find the difference between baseline and counterfactual ACs
burden_counterfactual_income <- burden_counterfactual_income %>%
  mutate(AttributableCases_difference = AttributableCases_baseline - AttributableCases_counterfactual,
         LowerAttributableCases_difference = LowerAttributableCases_baseline - LowerAttributableCases_counterfactual,
         UpperAttributableCases_difference = UpperAttributableCases_baseline - UpperAttributableCases_counterfactual)

#View table
view (burden_counterfactual_income)

#Save combined tables as csv file
write.csv(burden_counterfactual_income, 
          "R_tables/07Counterfactual_tables/burden_counterfactual_income.csv")



##PART 7.4: COUNTERFACTUAL BY LIVING LOCATION ----

burden_WHO_LivLoc <- burden_clean_livinglocation %>%
  select(YEAR, UrbanRural, NO2_LEVELS, EXPECTED_ALLCAUSE_CASES, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT, EXPECTED_ALLCAUSE_CASES_LOWER_CI,
         EXPECTED_ALLCAUSE_CASES_UPPER_CI) %>% 
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew), 
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC = PIF * EXPECTED_ALLCAUSE_CASES,
         AC_LOWER_CI = PIF_LOWER_CI * EXPECTED_ALLCAUSE_CASES_LOWER_CI,
         AC_UPPER_CI = PIF_UPPER_CI * EXPECTED_ALLCAUSE_CASES_UPPER_CI) %>%
  select(-ERF, -UNIT, -RRnew) %>%
  group_by(UrbanRural) %>%
  summarise(AttributableCases = sum(AC, na.rm = T),
            LowerAttributableCases = sum(AC_LOWER_CI, na.rm = T),
            UpperAttributableCases = sum(AC_UPPER_CI, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0)

#Change column name
colnames(burden_WHO_LivLoc) <- c("TOTAL_ADULT_POP", "AttributableCases",
                                 "LowerAttributableCases", "UpperAttributableCases")
#Check table
view(burden_WHO_LivLoc)

#Combine counterfactual and baseline total adults pop tables
burden_counterfactual_LivLoc <- bind_cols(Table_AttributableCases_LivLoc, burden_WHO_LivLoc)
burden_counterfactual_LivLoc <- burden_counterfactual_LivLoc[,-c(5)] #get rid of unnecessary column

#Rename columns
colnames(burden_counterfactual_LivLoc) <- c("TOTAL_ADULT_POP", "AttributableCases_baseline",
                                            "LowerAttributableCases_baseline",
                                            "UpperAttributableCases_baseline",
                                            "AttributableCases_counterfactual",
                                            "LowerAttributableCases_counterfactual",
                                            "UpperAttributableCases_counterfactual")

#Find the difference between baseline and counterfactual ACs
burden_counterfactual_LivLoc <- burden_counterfactual_LivLoc %>%
  mutate(AttributableCases_difference = AttributableCases_baseline - AttributableCases_counterfactual,
         LowerAttributableCases_difference = LowerAttributableCases_baseline - LowerAttributableCases_counterfactual,
         UpperAttributableCases_difference = UpperAttributableCases_baseline - UpperAttributableCases_counterfactual)

#View table
view(burden_counterfactual_LivLoc)

#Save combined tables as csv file
write.csv(burden_counterfactual_LivLoc, 
          "R_tables/07Counterfactual_tables/burden_counterfactual_LivLoc.csv")



##PART 7.5: COUNTERFACTUAL BY STATE ----

burden_WHO_state <- burden %>%
  select(YEAR, STATE.x, NO2_LEVELS, EXPECTED_ALLCAUSE_CASES, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT, EXPECTED_ALLCAUSE_CASES_LOWER_CI,
         EXPECTED_ALLCAUSE_CASES_UPPER_CI) %>% 
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew), 
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC = PIF * EXPECTED_ALLCAUSE_CASES,
         AC_LOWER_CI = PIF_LOWER_CI * EXPECTED_ALLCAUSE_CASES_LOWER_CI,
         AC_UPPER_CI = PIF_UPPER_CI * EXPECTED_ALLCAUSE_CASES_UPPER_CI) %>%
  select(-ERF, -UNIT, -RRnew) %>%
  group_by(STATE.x) %>%
  summarise(AttributableCases = sum(AC, na.rm = T),
            LowerAttributableCases = sum(AC_LOWER_CI, na.rm = T),
            UpperAttributableCases = sum(AC_UPPER_CI, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 0)

#Change column name
colnames(burden_WHO_state) <- c("State", "AttributableCases_counterfactual",
                                "LowerAttributableCases_counterfactual", 
                                "UpperAttributableCases_counterfactual")
#Check table
view(burden_WHO_state)


#Combine counterfactual and baseline total adults pop tables
burden_counterfactual_state <- bind_cols(Table_AttributableCases_state, burden_WHO_state)
burden_counterfactual_state <- burden_counterfactual_state[,-c(5)] #get rid of duplicated column

#Rename columns
colnames(burden_counterfactual_state) <- c("TOTAL_ADULT_POP", "AttributableCases_baseline",
                                           "LowerAttributableCases_baseline",
                                           "UpperAttributableCases_baseline",
                                           "AttributableCases_counterfactual",
                                           "LowerAttributableCases_counterfactual",
                                           "UpperAttributableCases_counterfactual")

#Find the difference between baseline and counterfactual ACs
burden_counterfactual_state <- burden_counterfactual_state %>%
  mutate(AttributableCases_difference = AttributableCases_baseline - AttributableCases_counterfactual,
         LowerAttributableCases_difference = LowerAttributableCases_baseline - LowerAttributableCases_counterfactual,
         UpperAttributableCases_difference = UpperAttributableCases_baseline - UpperAttributableCases_counterfactual)

#View table
view(burden_counterfactual_state)

#Save combined tables as csv file
write.csv(burden_counterfactual_state, 
          "R_tables/07Counterfactual_tables/burden_counterfactual_state.csv")




