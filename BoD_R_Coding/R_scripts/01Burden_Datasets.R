#------------------------------------------------------------------------------#
#Project : BoD U.S. adult-onset asthma and NO2 exposure
#Sub     : Pollutant estimate
#Part    : (01) Preparing datasets
#Purpose : Read in census data, NO2 concentration, race/ethnicity data,
#          income data living location, national incidence rate, and national
#          prevalence rate
#         Followed by joining the datasets 
#         Followed by estimating the burden
#Created by: PHS candidate 2547
#Date Created: 17-June-2022
#Last Updated: 23-July-2022
#------------------------------------------------------------------------------#



##PRELIMINARY STEPS: SETTING WORKSPACE ----

#Set working directory
setwd("~/BoD_R_Coding")

#Check working directory
getwd()


#Install packages relevant for subsequent analyses
my_packages <- c("readr", "tidyverse", "tidyr", "dplyr", "data.table")
for (p in my_packages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}



##PART 1: LOADING AND WRANGLING 2020 U.S. ADULT POPULATION DATA ----

#Read in total U.S. adult population (pop) count using race 18+ years pop dataset
census2020_path <- read_csv("R_imported_csv_datafiles/Race_2020.csv")

#Create general pop count variable including total pop column in dataset
census2020_var <- c("GEOID", "YEAR", "NAME", "STATE", "U7D001")

#Create table with GEOID, YEAR, tract and state names, and new variable "TOTAL_ADULTS"
census2020 <- fread("R_imported_csv_datafiles/Race_2020.csv", data.table = F, 
                    stringsAsFactors = F, verbose = F, select = census2020_var) %>% 
  mutate(TOTAL_ADULTS = U7D001) %>%
  select(GEOID, YEAR, NAME, STATE, TOTAL_ADULTS) %>%
  as_tibble()

#View table
view(census2020)



##PART 2: LOADING AND WRANGLING 2020 U.S. RACE POPULATION DATA ----

#Read in adult race pop data using previous census race dataset
race_path <- read_csv("R_imported_csv_datafiles/Race_2020.csv")


##PART 2A: White alone population
#Create white pop count variable constituted by relevant columns in dataset
white_var <- c("GEOID", "YEAR", "NAME", "STATE", "U7D003")

#Create table with GEOID, YEAR, tract and state names, and new variable of white pop
white_alone <- fread("R_imported_csv_datafiles/Race_2020.csv", data.table = F, 
                     stringsAsFactors = F, verbose = F, select = white_var) %>% 
  mutate(WHITE_ALONE = U7D003) %>%
  select(GEOID, YEAR, NAME, STATE, WHITE_ALONE) %>%
  as_tibble()

#View table
view(white_alone)


##PART 2B: Black alone population
#Create Black pop count variable constituted by relevant columns in dataset
black_var <- c("GEOID", "YEAR", "NAME", "STATE", "U7D004")

#Create table with GEOID, YEAR, tract and state names, and new variable of Black pop
black_alone <- fread("R_imported_csv_datafiles/Race_2020.csv", data.table = F,
                     stringsAsFactors = F, verbose = F, select = black_var) %>% 
  mutate(BLACK_ALONE = U7D004) %>%
  select(GEOID, YEAR, NAME, STATE, BLACK_ALONE) %>%
  as_tibble()

#View table
view(black_alone)


##PART 2C: American Indian Alaska Native (AIAN) alone population
#Create AIAN pop count variable constituted by relevant columns in dataset
AIAN_var <- c("GEOID", "YEAR", "NAME", "STATE", "U7D005")

#Create table with GEOID, YEAR, tract and state names, and new variable of AIAN pop
AIAN_alone <- fread("R_imported_csv_datafiles/Race_2020.csv", data.table = F, 
                    stringsAsFactors = F, verbose = F, select = AIAN_var) %>% 
  mutate(AIAN_ALONE = U7D005) %>%
  select(GEOID, YEAR, NAME, STATE, AIAN_ALONE) %>%
  as_tibble()

#View table
view(AIAN_alone)


##PART 2D: Asian alone population
#Create Asian pop count variable constituted by relevant columns in dataset
asian_var <- c("GEOID", "YEAR", "NAME", "STATE", "U7D006")

#Create table with GEOID, YEAR, tract and state names, and new variable of Asian pop
asian_alone <- fread("R_imported_csv_datafiles/Race_2020.csv", data.table = F, 
                     stringsAsFactors = F, verbose = F, select = asian_var) %>% 
  mutate(ASIAN_ALONE = U7D006) %>%
  select(GEOID, YEAR, NAME, STATE, ASIAN_ALONE) %>%
  as_tibble()

#View table
view(asian_alone)


##PART 2E: Native Hawaiian Pacific Islander (NHPI) alone population
#Create NHPI pop count variable constituted by relevant columns in dataset
NHPI_var <- c("GEOID", "YEAR", "NAME", "STATE", "U7D007")

#Create table with GEOID, YEAR, tract and state names, and new variable of NHPI pop
NHPI_alone <- fread("R_imported_csv_datafiles/Race_2020.csv", data.table = F,
                    stringsAsFactors = F, verbose = F, select = NHPI_var) %>% 
  mutate(NHPI_ALONE = U7D007) %>%
  select(GEOID, YEAR, NAME, STATE, NHPI_ALONE) %>%
  as_tibble()

#View table
view(NHPI_alone)


##PART 2F: Other race alone population
#Create other races pop count variable constituted by relevant columns in dataset
other_var <- c("GEOID", "YEAR", "NAME", "STATE", "U7D008")

#Create table with GEOID, YEAR, tract and state names, and new variable of Other pop
other_alone <- fread("R_imported_csv_datafiles/Race_2020.csv", data.table = F,
                     stringsAsFactors = F, verbose = F, select = other_var) %>% 
  mutate(OTHER_ALONE = U7D008) %>%
  select(GEOID, YEAR, NAME, STATE, OTHER_ALONE) %>%
  as_tibble()

#View table
view(other_alone)


##PART 2G: Two or more races population
#Create two or more races pop count variable constituted by relevant columns in dataset
two_more_var <- c("GEOID", "YEAR", "NAME", "STATE", "U7D009")

#Create table with GEOID, YEAR, tract and state names, and new variable of two or more races pop
two_more <- fread("R_imported_csv_datafiles/Race_2020.csv", data.table = F,
                  stringsAsFactors = F, verbose = F, select = two_more_var) %>% 
  mutate(TWO_MORE = U7D009) %>%
  select(GEOID, YEAR, NAME, STATE, TWO_MORE) %>%
  as_tibble()

#View table
view(two_more)


##PART 3: LOADING AND WRANGLING HISPANIC/LATINO DATA ----

#Read in Hispanic Latino pop count dataset
HisLat_path <- read_csv("R_imported_csv_datafiles/HispanicLatino_2020.csv")

#Create general Hispanic Latino pop count variable constituted by relevant columns in dataset
HisLat_var <- c("GEOID", "YEAR", "NAME", "STATE", "U7E002", "U7E003")

#Make new variable of Hispanic Latino pop count and rename relevant variables
HisLat <- fread("R_imported_csv_datafiles/HispanicLatino_2020.csv", data.table = F, 
                stringsAsFactors = F, verbose = F, select = HisLat_var) %>% 
  mutate(HisLat_TOTAL = U7E002, NotHisLat_TOTAL = U7E003) %>%
  select(GEOID, YEAR, NAME, STATE, HisLat_TOTAL, NotHisLat_TOTAL) %>%
  as_tibble()

#View table
view(HisLat)


##PART 4: LOADING AND WRANGLING MEDIAN HOUSEHOLD INCOME DATA ----

##Also, arrange ACSDT5Y2020.B19013_EDITED.csv so that median household incomes are in numerical order,
##smallest to largest. This sorting will help deal with mislabelling issues.

#Read in median household income pop count dataset
income_path <- read_csv("R_imported_csv_datafiles/MedianHouseholdIncome_EDITED.csv")

#Create median household income count variable made from each column in dataset
income_var <- c("GEOID", "YEAR", "NAME", "STATE", "MedHHIncome_B19013_001E")

#Create table with GEOID, YEAR, tract & state names, and new variables of "MH_INCOME" 
#and categorical groupings of median household incomes
income <- fread("R_imported_csv_datafiles/MedianHouseholdIncome_EDITED.csv",
                data.table = F, stringsAsFactors = F, verbose = F,  
                select = income_var) %>% 
  rename(MH_INCOME = MedHHIncome_B19013_001E) %>%
  mutate(INCOME_GROUP = case_when(
    MH_INCOME <27000 ~ "<$27,000", 
    between(MH_INCOME, 27000, 51999) ~ "$27,000 to <$52,000", 
    between(MH_INCOME, 52000, 84999) ~ "$52,000 to <$85,000", 
    between(MH_INCOME, 85000, 140999) ~ "$85,000 to <$141,000",
    MH_INCOME >=141000 ~ ">=$141,000",
    TRUE ~ "NA")) %>%
  select(GEOID, YEAR, NAME, STATE, MH_INCOME, INCOME_GROUP) %>%
  as_tibble()

#View table
view(income)

#Manual check shows output mislabeling, so these errors must be fixed
#Check number of data points in each income group with excel count
length(which(income$INCOME_GROUP == "<$27,000"))
length(which(income$INCOME_GROUP == "$27,000 to <$52,000"))
length(which(income$INCOME_GROUP == "$52,000 to <$85,000"))
length(which(income$INCOME_GROUP == "$85,000 to <$141,000"))
length(which(income$INCOME_GROUP == ">=$141,000"))
length(which(income$INCOME_GROUP == "NA"))

#Relabel errors
income[1:38, 6] = "<$27,000"
income[61601:79122, 6] = "$85,000 to <$141,000"
income[79123:82897, 6] = ">=$141,000"

#Double check number of data points for each income group
length(which(income$INCOME_GROUP == "<$27,000"))
length(which(income$INCOME_GROUP == "$27,000 to <$52,000"))
length(which(income$INCOME_GROUP == "$52,000 to <$85,000"))
length(which(income$INCOME_GROUP == "$85,000 to <$141,000"))
length(which(income$INCOME_GROUP == ">=$141,000"))
length(which(income$INCOME_GROUP == "NA"))

#View table with changes
view(income)



##PART 5: LOADING AND WRANGLING LIVING LOCATION DATA ----

##Before proceeding to next step, create new column "UrbanRural" in UrbanRural_2010_EDITED.csv.
##In this column, use logic statement "=IF([total urban cell] > [total rural cell], "Urban", "Rural")"
##to categorically label whether observed census tract has a majority urban or rural pop

#Read in living location pop count dataset
LivLoc_path <- read_csv("R_imported_csv_datafiles/UrbanRural_2010_EDITED.csv")

#Create living location pop count variable constituted by relevant columns
LivLoc_var <- c("GEOID", "YEAR", "NAME", "STATE", "UrbanRural", "Urban_H7W002",
                "Rural_H7W005")

#Create table with GEOID, YEAR, tract and state names, and "UrbanRural"
LivLoc <- fread("R_imported_csv_datafiles/UrbanRural_2010_EDITED.csv", data.table = F,
                stringsAsFactors = F, verbose = F, select = LivLoc_var) %>%
  rename(URBAN_POP = Urban_H7W002, RURAL_POP = Rural_H7W005) %>%
  select(GEOID, YEAR, NAME, STATE, UrbanRural, URBAN_POP, RURAL_POP) %>%
  as_tibble()

#View table
view(LivLoc)


##PART 6: LOADING AND WRANGLING NO2 CONCENTRATION DATA ----

##Before proceeding to next step, convert each state's attribute table into csv
##files; edit and combine tables accordingly with NO2 concentration data

#Read in NO2 concentration levels csv dataset
NO2_file <- read_csv("R_imported_csv_datafiles/NO2_2020_concentration.csv")

#Create NO2 concentration variable constituted by relevant columns dataset
NO2_var <- c("GEOID", "YEAR", "NAME", "STATE", "NO2_LEVELS")

#Create table with GEOID, YEAR, tract and state names, and NO2_LEVELS
NO2 <- fread("R_imported_csv_datafiles/NO2_2020_concentration.csv", data.table = F,
             stringsAsFactors = F, verbose = F, select = NO2_var) %>%
  select(GEOID, YEAR, NAME, STATE, NO2_LEVELS) %>% 
  as_tibble()

#View table
view(NO2)


##PART 7: JOINING DATASETS ----

#Join all census and NO2 concentration datasets by GEOID and YEAR; select for relevant variables
census2020_combined <- census2020 %>%
  left_join(white_alone, by = c("GEOID", "YEAR")) %>%
  left_join(black_alone, by = c("GEOID", "YEAR")) %>%
  left_join(AIAN_alone, by = c("GEOID", "YEAR")) %>%
  left_join(asian_alone, by = c("GEOID", "YEAR")) %>%
  left_join(NHPI_alone, by = c("GEOID", "YEAR")) %>%
  left_join(other_alone, by = c("GEOID", "YEAR")) %>%
  left_join(two_more, by = c("GEOID", "YEAR")) %>%
  left_join(HisLat, by = c("GEOID", "YEAR")) %>%
  left_join(income, by = c("GEOID", "YEAR")) %>% 
  left_join(LivLoc, by = c("GEOID", "YEAR")) %>%
  left_join(NO2, by = c("GEOID", "YEAR")) %>%
  select(GEOID, YEAR, NAME.x, STATE.x, TOTAL_ADULTS, WHITE_ALONE, BLACK_ALONE, 
         AIAN_ALONE, ASIAN_ALONE, NHPI_ALONE, OTHER_ALONE, TWO_MORE, HisLat_TOTAL, 
         NotHisLat_TOTAL, INCOME_GROUP, MH_INCOME, UrbanRural, URBAN_POP, RURAL_POP,
         NO2_LEVELS)

#View table
view(census2020_combined)

##The number of rows in 'census2020_combined' dataset does not match that in excel sheets;
##looks like it's due to duplicates; therefore, check for these by GEOID and Name
duplicate_check <- census2020_combined[which(duplicated(census2020_combined[, c("GEOID", "NAME.x")]) == T),]
view(duplicate_check)

##Remove the duplicates by their corresponding row number in the 'census2020_combined' dataset
census2020_combined <- census2020_combined[-c(1503, 1505, 1507, 1514, 1516, 1518, 
                                              1520, 1522, 1524, 3536, 3538, 3543,
                                              3545, 4597, 4599, 4612, 4614, 4617, 
                                              4621, 4623, 4629, 4635, 4637, 4640,
                                              4646, 4648, 4650, 4654, 4660, 4662,
                                              4664, 4666, 4668, 4671, 4673, 4675, 
                                              4677, 4679, 4681, 4683, 4685, 4688,
                                              4704, 4706, 4710, 4712, 9313),]
#Check for duplicates again
duplicate_check <- census2020_combined[which(duplicated(census2020_combined[, c("GEOID", "NAME.x")]) == T),]
view(duplicate_check)

#Check new 'census2020_combined' dataset
view(census2020_combined)



##PART 8: ESTIMATE BURDEN OF ADULT-ONSET ASTHMA ----

#Create object for national adult-onset asthma incidence rate
IR  <- 0.0038

#Perform calculations
burden <- census2020_combined %>% 
  mutate(PRV = 0.084, #prevalence rate
         ERF = 1.11, #exposure response function (ERF)
         ERF_LOWER_CI = 1.05, #ERF lower confidence interval (CI)
         ERF_UPPER_CI = 1.16, #ERF upper CI
         UNIT = 10, #calculations based on per 10 μg/m3 increase of NO2
         EXPECTED_ALLCAUSE_CASES = (TOTAL_ADULTS - (TOTAL_ADULTS * PRV)) * IR, #expected all-cause cases
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS), #relative risk (RR)
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS), #relative risk lower CI
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS), #relative risk upper CI
         PIF = (RRnew - 1)/(RRnew), #population impact fraction (PIF)
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI), #PIF lower CI
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI), #PIF upper CI
         PIF_percent = ((RRnew - 1)/(RRnew))*100, #PIF as %
         PIF_percent_LOWER_CI = ((RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI))*100, #PIF lower CI as %
         PIF_percent_UPPER_CI = ((RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI))*100, #PIF upper CI as %
         AC = PIF * EXPECTED_ALLCAUSE_CASES, #attributable cases (AC)
         AC_LOWER_CI = PIF_LOWER_CI * EXPECTED_ALLCAUSE_CASES, #AC lower CI
         AC_UPPER_CI = PIF_UPPER_CI * EXPECTED_ALLCAUSE_CASES) #AC upper CI
         

#View calculations
view(burden)




