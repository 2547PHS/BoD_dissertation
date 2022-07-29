##PRELIMINARY STEPS: SETTING UP THE WORKSPACE ----

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





##SECTION 1## ----
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





##SECTION 2## ----
#------------------------------------------------------------------------------#
#Project : BoD U.S. adult-onset asthma and NO2 exposure
#Sub     : Pollutant estimate
#Part    : (02) Plotting air pollution concentrations
#Purpose :  Plot NO2 concentrations
#Created by: PHS candidate 2547
#Date Created: 18-June-2022
#Last Updated: 23-July-2022
#------------------------------------------------------------------------------#


#NOTE:  Save boxplot in the 'R_figures' folder in the working directory using 'Export' 
#       function located above the displayed plot to the right of script; Save plot as 
#       pdf in 'US letter' size


##PART 2.1: SETTING FEATURES FOR PLOTS ----

#Prevent scientific notation whilst plotting data
options(scipen = 10000)

#Assign theme options without strip text features
theme_text2 <- theme(plot.title = element_text(size = 15, hjust = 0.5),
                     axis.title.x = element_text(size = 11),
                     axis.title.y = element_text(size = 11),
                     axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
                     axis.ticks.x = element_blank(),
                     strip.text = element_blank())

#Assign theme options without strip text features
theme_text3 <- theme(plot.title = element_text(size = 15, hjust = 0.5),
                     axis.title.x = element_text(size = 11),
                     axis.title.y = element_text(size = 11),
                     axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
                     axis.ticks.x = element_blank(),
                     strip.text = element_text(size = 6, 
                                               margin = margin(0.05, 0, 0.05, 0, "cm")))

#Assign theme options adjust x-axis label size
theme_text4 <- theme(plot.title = element_text(size = 15, hjust = 0.5),
                     axis.title.x = element_text(size = 11),
                     axis.title.y = element_text(size = 11),
                     axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1,
                                                size = 7.5),
                     axis.ticks.x = element_blank(),
                     strip.text = element_text(size = 6, 
                                               margin = margin(0.05, 0, 0.05, 0, "cm")))

#Set range for y-axis
breaks_m_y <- seq(0, max(burden$NO2_LEVELS, na.rm = T), 10)
scale_y_bod <- scale_y_continuous(breaks = breaks_m_y)

#Theme option for identifying mean in the boxplots
mean_dot <- stat_summary(fun = mean, geom = "point", shape = 20, size = 2, 
                         color = "red", fill = "red") 



##PART 2.2: PLOTTING NO2 CONCENTRATIONS ACROSS THE U.S. ----

#Make boxplots to display distributions across U.S. census tracts
burden %>%
  filter(TOTAL_ADULTS > 0) %>%
  select(YEAR, NO2_LEVELS) %>% 
  ggplot(aes(x = as.factor(YEAR), y = NO2_LEVELS)) +
  facet_wrap( ~ YEAR, nrow =  1) +
  geom_boxplot(show.legend = F, outlier.size = 0.1) +
  stat_boxplot(geom = "errorbar") + 
  mean_dot +
  theme_bw() +
  labs(title = "Spread of NO2 concentrations across adult-populated U.S. census tracts in 2020", 
       x = "Year", 
       y = "NO2 concentration (micrograms/cubic metre)") +
  theme_text2 +
  scale_y_bod +
  scale_fill_manual(values = c("#004e82", "#a5dbff"))


#Make boxplots to display distributions by state  
burden %>%
  select(STATE.x, YEAR, NO2_LEVELS) %>% 
  ggplot(aes(x = as.factor(YEAR), y = NO2_LEVELS)) + #make sure YEAR is factor class
  facet_wrap( ~ STATE.x, nrow =  6) +
  geom_boxplot(show.legend = F, outlier.size = 0.1) +
  stat_boxplot(geom = "errorbar") +
  mean_dot +
  theme_bw() +
  labs(title = "Spread of NO2 concentrations in each U.S. state during 2020", 
       x = "Year", 
       y = "NO2 concentration (micrograms/cubic metre)") +
  theme_text3 +
  scale_y_bod +
  scale_fill_manual(values = c("#004e82", "#a5dbff"))


#Supplemental plot: Make histogram of NO2 concentrations across U.S. census tracts
burden %>%
  filter(TOTAL_ADULTS > 0) %>%
  ggplot(aes(x = NO2_LEVELS)) +
  theme_bw() +
  geom_histogram(bins = 10, binwidth= 5, boundary = 0, closed = "left", colour = "darkblue", 
                 fill = "lightblue", ) +
  labs(title = "Distribution of NO2 concentrations across adult-populated U.S. census tracts in 2020", 
       x = "NO2 concentration (micrograms/cubic metre)",
       y = "Number of adult-populated U.S. census tracts") +
  theme(plot.title = element_text(size = 17, hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14), 
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11)) +
  scale_x_continuous(breaks = c(0, seq(0, 50, 5)))




##PART 2.3: PLOTTING NO2 CONCENTRATIONS BY RACE ----

##First create tables with population, state, year, and mean NO2 levels for each racial and ethnic group
#White pop
WHITE_ALONE_NO2plot <- burden %>%
  filter(WHITE_ALONE > 0) %>% 
  group_by(WHITE_ALONE, YEAR, STATE.x) %>%
  summarise(MEAN_NO2 = weighted.mean(NO2_LEVELS, WHITE_ALONE, na.rm = T)) %>%
  mutate(WHITE_ALONE_pop = "White")

#Rename columns
colnames(WHITE_ALONE_NO2plot) <- c("Adult_pop", "YEAR", "STATE", "MEAN_NO2", "Race")


#Black pop
BLACK_ALONE_NO2plot <- burden %>%
  filter(BLACK_ALONE > 0) %>% 
  group_by(BLACK_ALONE, YEAR, STATE.x) %>%
  summarise(MEAN_NO2 = weighted.mean(NO2_LEVELS, BLACK_ALONE, na.rm = T)) %>%
  mutate(BLACK_ALONE_pop = "Black")

#Rename columns
colnames(BLACK_ALONE_NO2plot) <- c("Adult_pop", "YEAR", "STATE", "MEAN_NO2", "Race")


#AIAN pop
AIAN_ALONE_NO2plot <- burden %>%
  filter(AIAN_ALONE > 0) %>% 
  group_by(AIAN_ALONE, YEAR, STATE.x) %>%
  summarise(MEAN_NO2 = weighted.mean(NO2_LEVELS, AIAN_ALONE, na.rm = T)) %>%
  mutate(AIAN_ALONE_pop = "AIAN")

#Rename columns
colnames(AIAN_ALONE_NO2plot) <- c("Adult_pop", "YEAR", "STATE", "MEAN_NO2", "Race")


#Asian pop
ASIAN_ALONE_NO2plot <- burden %>%
  filter(ASIAN_ALONE > 0) %>% 
  group_by(ASIAN_ALONE, YEAR, STATE.x) %>%
  summarise(MEAN_NO2 = weighted.mean(NO2_LEVELS, ASIAN_ALONE, na.rm = T)) %>%
  mutate(ASIAN_ALONE_pop = "Asian")

#Rename columns
colnames(ASIAN_ALONE_NO2plot) <- c("Adult_pop", "YEAR", "STATE", "MEAN_NO2", "Race")


#NHPI pop
NHPI_ALONE_NO2plot <- burden %>%
  filter(NHPI_ALONE > 0) %>% 
  group_by(NHPI_ALONE, YEAR, STATE.x) %>%
  summarise(MEAN_NO2 = weighted.mean(NO2_LEVELS, NHPI_ALONE, na.rm = T)) %>%
  mutate(NHPI_ALONE_pop = "NHPI")

#Rename columns
colnames(NHPI_ALONE_NO2plot) <- c("Adult_pop", "YEAR", "STATE", "MEAN_NO2", "Race")


#Other race pop
OTHER_ALONE_NO2plot <- burden %>%
  filter(OTHER_ALONE > 0) %>% 
  group_by(OTHER_ALONE, YEAR, STATE.x) %>%
  summarise(MEAN_NO2 = weighted.mean(NO2_LEVELS, OTHER_ALONE, na.rm = T)) %>%
  mutate(OTHER_ALONE_pop = "Other")

#Rename columns
colnames(OTHER_ALONE_NO2plot) <- c("Adult_pop", "YEAR", "STATE", "MEAN_NO2", "Race")


#Two or more races pop
TWO_MORE_NO2plot <- burden %>%
  filter(TWO_MORE > 0) %>% 
  group_by(TWO_MORE, YEAR, STATE.x) %>%
  summarise(MEAN_NO2 = weighted.mean(NO2_LEVELS, TWO_MORE, na.rm = T)) %>%
  mutate(TWO_MORE_pop = "Two or more")

#Rename columns
colnames(TWO_MORE_NO2plot) <- c("Adult_pop", "YEAR", "STATE", "MEAN_NO2", "Race")


#Hispanic or Latino pop
HisLat_NO2plot <- burden %>%
  filter(HisLat_TOTAL > 0) %>% 
  group_by(HisLat_TOTAL, YEAR, STATE.x) %>%
  summarise(MEAN_NO2 = weighted.mean(NO2_LEVELS, HisLat_TOTAL, na.rm = T)) %>%
  mutate(HisLat_pop = "Hispanic/Latino")

#Rename columns
colnames(HisLat_NO2plot) <- c("Adult_pop", "YEAR", "STATE", "MEAN_NO2", "Race")


#Non-Hispanic or Latino pop
NotHisLat_NO2plot <- burden %>%
  filter(NotHisLat_TOTAL > 0) %>% 
  group_by(NotHisLat_TOTAL, YEAR, STATE.x) %>%
  summarise(MEAN_NO2 = weighted.mean(NO2_LEVELS, NotHisLat_TOTAL, na.rm = T)) %>%
  mutate(HisLat_pop = "Not Hispanic/Latino")

#Rename columns
colnames(NotHisLat_NO2plot) <- c("Adult_pop", "YEAR", "STATE", "MEAN_NO2", "Race")


##Save each table as csv files to R_table directory 
write.csv(WHITE_ALONE_NO2plot, "R_tables/02Air_pollution_tables/WHITE_ALONE_NO2plot.csv")
write.csv(BLACK_ALONE_NO2plot, "R_tables/02Air_pollution_tables/BLACK_ALONE_NO2plot.csv")
write.csv(AIAN_ALONE_NO2plot, "R_tables/02Air_pollution_tables/AIAN_ALONE_NO2plot.csv")
write.csv(ASIAN_ALONE_NO2plot, "R_tables/02Air_pollution_tables/ASIAN_ALONE_NO2plot.csv")
write.csv(NHPI_ALONE_NO2plot, "R_tables/02Air_pollution_tables/NHPI_ALONE_NO2plot.csv")
write.csv(OTHER_ALONE_NO2plot, "R_tables/02Air_pollution_tables/OTHER_ALONE_NO2plot.csv")
write.csv(TWO_MORE_NO2plot, "R_tables/02Air_pollution_tables/TWO_MORE_NO2plot.csv")
write.csv(HisLat_NO2plot, "R_tables/02Air_pollution_tables/HisLat_NO2plot.csv")
write.csv(NotHisLat_NO2plot, "R_tables/02Air_pollution_tables/NotHisLat_NO2plot.csv")

##Afterwards, open a new Excel file and combine each racial/ethnic group's tables
##in this new file. Keep relevant columns; i.e. year, state, MEAN_NO2, and race.
##"NO2plot_RaceHisLat_combined.csv" is this new file (within the working directory)

#Upload new (combined) Excel file
NO2plot_RaceHisLat_combined <- read_csv("R_tables/02Air_pollution_tables/NO2plot_RaceHisLat_combined.csv")

#View table
view(NO2plot_RaceHisLat_combined)


##PLOT##

#First, reorder racial and ethnic groups
NO2plot_RaceHisLat_combined$Race <- factor(NO2plot_RaceHisLat_combined$Race, 
                                           c("White", "Black", "AIAN", "Asian",
                                             "NHPI", "Other", "Two or more", 
                                             "Hispanic/Latino", "Not Hispanic/Latino"))
#Graph
NO2plot_RaceHisLat_combined %>%
  select(Race, MEAN_NO2) %>%
  ggplot(aes(x = Race, y = MEAN_NO2)) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) +
  stat_boxplot(geom = "errorbar") +
  mean_dot +
  theme_bw() +
  labs(title = "Spread of NO2 concentrations for racial and ethnic groups across the U.S. in 2020", 
       x = "Racial and ethnic groups",
       y = "NO2 concentration (micrograms/cubic metre)") +
  theme_text3 +
  scale_y_bod +
  scale_colour_manual(values = c("black", "red"), aesthetics = "colour") +
  scale_fill_manual(values = c("white", "white", "white", "white", "white", 
                               "white", "white"))





##PART 2.3.1: PLOTTING NO2 CONCENTRATIONS BY RACE & STATE ----

NO2plot_RaceHisLat_combined %>%
  select(STATE, YEAR, Race, MEAN_NO2) %>% 
  ggplot(aes(x = Race, y = MEAN_NO2)) +
  facet_wrap( ~ STATE, nrow =  6) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) +
  stat_boxplot(geom = "errorbar") +
  mean_dot +
  theme_bw() +
  labs(title = "Spread of NO2 concentrations by racial and ethnic group for each U.S. state in 2020", 
       x = "Racial and ethnic groups",
       y = "NO2 concentration (micrograms/cubic metre)") +
  theme_text4 +
  scale_y_bod +
  scale_colour_manual(values = c("black", "red"), aesthetics = "colour") +
  scale_fill_manual(values = c("white", "white", "white", "white", "white", 
                               "white", "white"))




##PART 2.4: PLOTTING NO2 CONCENTRATIONS BY MEDIAN HOUSEHOLD INCOME ----

#Remove NA values for the income plot
burden_clean_income <- burden[!is.na(burden$INCOME_GROUP),]
burden_clean_income <- burden[!is.na(burden$MH_INCOME),]

#Reorder income groups from least to greatest
burden_clean_income$INCOME_GROUP <- factor(burden_clean_income$INCOME_GROUP, 
                                           c("<$27,000","$27,000 to <$52,000",
                                             "$52,000 to <$85,000","$85,000 to <$141,000", 
                                             ">=$141,000"))
#Plot
burden_clean_income %>%
  filter(TOTAL_ADULTS > 0) %>%
  select(YEAR, INCOME_GROUP, NO2_LEVELS) %>% 
  ggplot(aes(x = INCOME_GROUP, y = NO2_LEVELS)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) +
  stat_boxplot(geom = "errorbar") +
  mean_dot +
  theme_bw() +
  labs(title = "Spread of NO2 concentrations by median household income groups in 2020", 
       x = "Income Groups", 
       y = "NO2 concentration (micrograms/cubic metre)") +
  theme_text3 +
  scale_y_bod +
  mean_dot +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour") +
  scale_fill_manual(values=c("white", "white", "white", "white", "white", "white", "white"))



##PART 2.4.1: PLOTTING NO2 CONCENTRATIONS BY MEDIAN HOUSEHOLD INCOME & STATE ----

burden_clean_income %>%
  filter(TOTAL_ADULTS > 0) %>%
  select(STATE.x, YEAR, INCOME_GROUP, NO2_LEVELS) %>% 
  ggplot(aes(x = INCOME_GROUP, y = NO2_LEVELS)) +
  facet_wrap( ~ STATE.x, nrow =  6) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) +
  stat_boxplot(geom = "errorbar") +
  theme_bw() +
  labs(title = 
         "Spread of NO2 concentrations by median household income groups for each U.S. state in 2020", 
       x = "Income Groups", 
       y = "NO2 concentration (micrograms/cubic metre)") +
  theme_text4 +
  scale_y_bod +
  mean_dot +
  scale_colour_manual(values = c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values = c("white", "white", "white", "white", "white", "white", "white"))



##PART 2.5: PLOTTING NO2 CONCENTRATIONS BY LIVING LOCATION ----

#Remove NA values for the living location plot
burden_clean_livinglocation <- burden[!is.na(burden$UrbanRural),]

#Reorder living location types
burden_clean_livinglocation$UrbanRural <- factor(burden_clean_livinglocation$UrbanRural, 
                                                 c("Urban", "Rural"))

#Plot
burden_clean_livinglocation %>%
  filter(TOTAL_ADULTS > 0) %>%
  select(YEAR, UrbanRural, NO2_LEVELS) %>% 
  ggplot(aes(x = UrbanRural, y = NO2_LEVELS)) +
  stat_boxplot(geom = "errorbar") +
  facet_wrap( ~ YEAR, nrow =  1) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) +
  mean_dot +
  theme_bw() +
  labs(title = "Spread of NO2 concentrations across adult-populated U.S. census tracts by living location in 2020", 
       x = "Living Location", 
       y = "NO2 concentration (micrograms/cubic metre)") +
  theme_text2 +
  scale_y_bod +
  scale_colour_manual(values = c("black", "red"), aesthetics = "colour") +
  scale_fill_manual(values = c("white", "white", "white", "white", "white", "white", "white"))



##PART 2.5.1: PLOTTING NO2 CONCENTRATIONS BY LIVING LOCATION & STATE ----

#Plot
burden_clean_livinglocation %>%
  filter(TOTAL_ADULTS > 0) %>%
  select(STATE.x, YEAR, UrbanRural, NO2_LEVELS) %>% 
  ggplot(aes(x = UrbanRural, y = NO2_LEVELS)) +
  stat_boxplot(geom = "errorbar") +
  facet_wrap( ~ STATE.x, nrow =  6) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) +
  mean_dot +
  theme_bw() +
  labs(title = "Spread of NO2 concentrations by living location for each U.S. state in 2020", 
       x = "Living Location", 
       y = "NO2 concentration (micrograms/cubic metre)") +
  theme_text4 +
  scale_y_bod +
  scale_colour_manual(values = c("black", "red"), aesthetics = "colour") +
  scale_fill_manual(values = c("white", "white", "white", "white", "white", "white", "white"))






##SECTION 3## ----
#------------------------------------------------------------------------------#
#Project : BoD U.S. adult-onset asthma and NO2 exposure
#Sub     : Pollutant estimate
#Part    : (03)  Air pollution concentration tables
#Purpose : Create NO2 concentration tables
#Created by: PHS candidate 2547
#Date Created: 19-June-2022
#Last Updated: 23-July-2022
#------------------------------------------------------------------------------#



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





##SECTION 4## ----
#------------------------------------------------------------------------------#
#Project : BoD U.S. adult-onset asthma and NO2 exposure
#Sub     : Pollutant estimate
#Part    : (04)  Burden tables
#Purpose : Create burden tables
#Created by: PHS candidate 2547
#Date Created: 19-June-2022
#Last Updated: 23-July-2022
#------------------------------------------------------------------------------#



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





##SECTION 5## ----
#------------------------------------------------------------------------------#
#Project : BoD U.S. adult-onset asthma and NO2 exposure
#Sub     : Demographics
#Part    : (05)  Demographic tables
#Purpose : Create demographic tables
#Created by PHS candidate 2547
#Date Created: 19-June-2022
#Last Updated: 23-July-2022
#------------------------------------------------------------------------------#



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





##SECTION 6## ----
#------------------------------------------------------------------------------#
#Project : BoD U.S. adult-onset asthma and NO2 exposure
#Sub     : Geographical
#Part    : (06)  Geographic tables
#Purpose : Create geographic tables of census tracts
#Created by PHS Candidate 2547
#Date Created: 19-June-2022
#Last Updated: 23-July-2022
#------------------------------------------------------------------------------#



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





##SECTION 7## ----
#------------------------------------------------------------------------------#
#Project : BoD U.S. adult-onset asthma and NO2 exposure
#Sub     : Demographics
#Part    : (07)  Counterfactual scenarios
#Purpose : Create counterfactual scenarios and tables
#Created by PHS Candidate 2547
#Date Created: 19-June-2022
#Last Updated: 23-July-2022
#------------------------------------------------------------------------------#



##PART 7.1: COUNTERFACTUAL FOR U.S. ADULT POP ----

burden_WHO_ALL <- burden %>%
  select(YEAR, TOTAL_ADULTS, NO2_LEVELS, EXPECTED_ALLCAUSE_CASES, ERF, ERF_LOWER_CI, 
         ERF_UPPER_CI, UNIT) %>% 
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew), 
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC = PIF * EXPECTED_ALLCAUSE_CASES,
         AC_LOWER_CI = PIF_LOWER_CI * EXPECTED_ALLCAUSE_CASES,
         AC_UPPER_CI = PIF_UPPER_CI * EXPECTED_ALLCAUSE_CASES) %>%
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
#First, join 'EXPECTED_WHITE' & 'AC_WHITE' columns to 'burden' dataset for
#calculations. This time, without filtering for WHITE_ALONE > 0 to match row numbers
EXPECTED_WHITE <- burden %>% 
  summarise(EXPECTED_CASES_WHITE = (WHITE_ALONE - (WHITE_ALONE * PRV)) * IR) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_WHITE <- burden %>% 
  summarise(AC = PIF * EXPECTED_WHITE) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

burden <- burden %>%
  mutate(EXPECTED_WHITE = EXPECTED_WHITE, AC_WHITE = AC_WHITE)

#Calculate counterfactual scenario for white pop
burden_WHO_white <- burden %>%
  select(YEAR, WHITE_ALONE, NO2_LEVELS, EXPECTED_WHITE, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT) %>% 
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew),
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC_WHITE = PIF * EXPECTED_WHITE,
         AC_WHITE_LOWER_CI = PIF_LOWER_CI * EXPECTED_WHITE,
         AC_WHITE_UPPER_CI = PIF_UPPER_CI * EXPECTED_WHITE) %>%
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
  summarise(EXPECTED_CASES_BLACK = (BLACK_ALONE - (BLACK_ALONE * PRV)) * IR) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_BLACK <- burden %>% 
  summarise(AC = PIF * EXPECTED_BLACK) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

burden <- burden %>%
  mutate(EXPECTED_BLACK = EXPECTED_BLACK, AC_BLACK = AC_BLACK)

#Calculate counterfactual scenario for Black pop
burden_WHO_black <- burden %>%
  select(YEAR, BLACK_ALONE, NO2_LEVELS, EXPECTED_BLACK, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT) %>% 
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew),
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC_BLACK = PIF * EXPECTED_BLACK,
         AC_BLACK_LOWER_CI = PIF_LOWER_CI * EXPECTED_BLACK,
         AC_BLACK_UPPER_CI = PIF_UPPER_CI * EXPECTED_BLACK) %>%
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
  summarise(EXPECTED_CASES_AIAN = (AIAN_ALONE - (AIAN_ALONE * PRV)) * IR) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_AIAN <- burden %>% 
  summarise(AC = PIF * EXPECTED_AIAN) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

burden <- burden %>%
  mutate(EXPECTED_AIAN = EXPECTED_AIAN, AC_AIAN = AC_AIAN)

#Calculate counterfactual scenario for AIAN pop
burden_WHO_AIAN <- burden %>%
  select(YEAR, AIAN_ALONE, NO2_LEVELS, EXPECTED_AIAN, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT) %>% 
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew),
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC_AIAN = PIF * EXPECTED_AIAN,
         AC_AIAN_LOWER_CI = PIF_LOWER_CI * EXPECTED_AIAN,
         AC_AIAN_UPPER_CI = PIF_UPPER_CI * EXPECTED_AIAN) %>%
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
  summarise(EXPECTED_CASES_ASIAN = (ASIAN_ALONE - (ASIAN_ALONE * PRV)) * IR) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_ASIAN <- burden %>% 
  summarise(AC = PIF * EXPECTED_ASIAN) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

burden <- burden %>%
  mutate(EXPECTED_ASIAN = EXPECTED_ASIAN, AC_ASIAN = AC_ASIAN)

#Calculate counterfactual scenario for Asian pop
burden_WHO_asian <- burden %>%
  select(YEAR, ASIAN_ALONE, NO2_LEVELS, EXPECTED_ASIAN, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT) %>% 
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew),
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC_ASIAN = PIF * EXPECTED_ASIAN,
         AC_ASIAN_LOWER_CI = PIF_LOWER_CI * EXPECTED_ASIAN,
         AC_ASIAN_UPPER_CI = PIF_UPPER_CI * EXPECTED_ASIAN) %>%
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
  summarise(EXPECTED_CASES_NHPI = (NHPI_ALONE - (NHPI_ALONE * PRV)) * IR) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_NHPI <- burden %>% 
  summarise(AC = PIF * EXPECTED_NHPI) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

burden <- burden %>%
  mutate(EXPECTED_NHPI = EXPECTED_NHPI, AC_NHPI = AC_NHPI)

#Calculate counterfactual scenario for NHPI pop
burden_WHO_NHPI <- burden %>%
  select(YEAR, NHPI_ALONE, NO2_LEVELS, EXPECTED_NHPI, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT) %>% 
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew),
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC_NHPI = PIF * EXPECTED_NHPI,
         AC_NHPI_LOWER_CI = PIF_LOWER_CI * EXPECTED_NHPI,
         AC_NHPI_UPPER_CI = PIF_UPPER_CI * EXPECTED_NHPI) %>%
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
  summarise(EXPECTED_CASES_OTHER = (OTHER_ALONE - (OTHER_ALONE * PRV)) * IR) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_OTHER <- burden %>% 
  summarise(AC = PIF * EXPECTED_OTHER) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

burden <- burden %>%
  mutate(EXPECTED_OTHER = EXPECTED_OTHER, AC_OTHER = AC_OTHER)

#Calculate counterfactual scenario for Other pop
burden_WHO_other <- burden %>%
  select(YEAR, OTHER_ALONE, NO2_LEVELS, EXPECTED_OTHER, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT) %>% 
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew),
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC_OTHER = PIF * EXPECTED_OTHER,
         AC_OTHER_LOWER_CI = PIF_LOWER_CI * EXPECTED_OTHER,
         AC_OTHER_UPPER_CI = PIF_UPPER_CI * EXPECTED_OTHER) %>%
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
EXPECTED_TWOMORE <- burden %>% 
  summarise(EXPECTED_CASES_TWOMORE = (TWO_MORE - (TWO_MORE * PRV)) * IR) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_TWOMORE <- burden %>% 
  summarise(AC = PIF * EXPECTED_TWOMORE) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

burden <- burden %>%
  mutate(EXPECTED_TWOMORE = EXPECTED_TWOMORE, AC_TWOMORE = AC_TWOMORE)

#Calculate counterfactual scenario for Two or more races pop
burden_WHO_twomore <- burden %>%
  select(YEAR, TWO_MORE, NO2_LEVELS, EXPECTED_TWOMORE, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT) %>% 
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew),
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC_TWOMORE = PIF * EXPECTED_TWOMORE,
         AC_TWOMORE_LOWER_CI = PIF_LOWER_CI * EXPECTED_TWOMORE,
         AC_TWOMORE_UPPER_CI = PIF_UPPER_CI * EXPECTED_TWOMORE) %>%
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
  summarise(EXPECTED_CASES_HisLat = (HisLat_TOTAL - (HisLat_TOTAL * PRV)) * IR) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_HisLat <- burden %>% 
  summarise(AC = PIF * EXPECTED_HisLat) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

burden <- burden %>%
  mutate(EXPECTED_HisLat = EXPECTED_HisLat, AC_HisLat = AC_HisLat)

#Calculate counterfactual scenario for Hispanic Latino pop
burden_WHO_HisLat <- burden %>%
  select(YEAR, HisLat_TOTAL, NO2_LEVELS, EXPECTED_HisLat, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT) %>% 
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew),
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC_HisLat = PIF * EXPECTED_HisLat,
         AC_HisLat_LOWER_CI = PIF_LOWER_CI * EXPECTED_HisLat,
         AC_HisLat_UPPER_CI = PIF_UPPER_CI * EXPECTED_HisLat) %>%
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
  summarise(EXPECTED_CASES_NotHisLat = (NotHisLat_TOTAL - (NotHisLat_TOTAL * PRV)) * IR) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

AC_NotHisLat <- burden %>% 
  summarise(AC = PIF * EXPECTED_NotHisLat) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  as.data.frame()

burden <- burden %>%
  mutate(EXPECTED_NotHisLat = EXPECTED_NotHisLat, AC_NotHisLat = AC_NotHisLat)

#Calculate counterfactual scenario for Non-Hispanic Latino pop
burden_WHO_NotHisLat <- burden %>%
  select(YEAR, NotHisLat_TOTAL, NO2_LEVELS, EXPECTED_NotHisLat, ERF, ERF_LOWER_CI,
         ERF_UPPER_CI, UNIT) %>% 
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew),
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC_NotHisLat = PIF * EXPECTED_NotHisLat,
         AC_NotHisLat_LOWER_CI = PIF_LOWER_CI * EXPECTED_NotHisLat,
         AC_NotHisLat_UPPER_CI = PIF_UPPER_CI * EXPECTED_NotHisLat) %>%
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
         ERF_UPPER_CI, UNIT) %>% 
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew), 
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC = PIF * EXPECTED_ALLCAUSE_CASES,
         AC_LOWER_CI = PIF_LOWER_CI * EXPECTED_ALLCAUSE_CASES,
         AC_UPPER_CI = PIF_UPPER_CI * EXPECTED_ALLCAUSE_CASES) %>%
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
         ERF_UPPER_CI, UNIT) %>% 
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew), 
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC = PIF * EXPECTED_ALLCAUSE_CASES,
         AC_LOWER_CI = PIF_LOWER_CI * EXPECTED_ALLCAUSE_CASES,
         AC_UPPER_CI = PIF_UPPER_CI * EXPECTED_ALLCAUSE_CASES) %>%
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
         ERF_UPPER_CI, UNIT) %>% 
  mutate(NO2_LEVELS = ifelse(NO2_LEVELS > 10, 10, NO2_LEVELS),
         RRnew = exp((log(ERF)/UNIT) * NO2_LEVELS),
         RRnew_LOWER_CI = exp((log(ERF_LOWER_CI)/UNIT) * NO2_LEVELS),
         RRnew_UPPER_CI = exp((log(ERF_UPPER_CI)/UNIT) * NO2_LEVELS),
         PIF = (RRnew - 1)/(RRnew), 
         PIF_LOWER_CI = (RRnew_LOWER_CI - 1)/(RRnew_LOWER_CI),
         PIF_UPPER_CI = (RRnew_UPPER_CI - 1)/(RRnew_UPPER_CI),
         AC = PIF * EXPECTED_ALLCAUSE_CASES,
         AC_LOWER_CI = PIF_LOWER_CI * EXPECTED_ALLCAUSE_CASES,
         AC_UPPER_CI = PIF_UPPER_CI * EXPECTED_ALLCAUSE_CASES) %>%
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





##SECTION 8## ----
#------------------------------------------------------------------------------#
#Project : BoD U.S. adult-onset asthma and NO2 exposure
#Sub     : Pollutant estimate
#Part    : (08) Displaying burden estimates 
#Purpose : Plot and make tables of population impact fractions
#Created by PHS Candidate 2547
#Date Created: 19-June-2022
#Last Updated: 23-July-2022
#------------------------------------------------------------------------------#

#NOTE:  Save boxplot in the 'R_figures' folder in the working directory using 'Export' 
#       function located above the displayed plot to the right of script; Save plot as 
#       pdf in 'US letter' size


##PART 8.1: SETTING FEATURES FOR PLOTS ----

#Prevent scientific notations options
(scipen = 10000)

#Assign theme options without strip text features
theme_text2 <- theme(plot.title = element_text(size = 15, hjust = 0.5),
                     axis.title.x = element_text(size = 11),
                     axis.title.y = element_text(size = 11),
                     axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
                     axis.ticks.x = element_blank(),
                     strip.text = element_blank())

#Assign theme options without strip text features
theme_text3 <- theme(plot.title = element_text(size = 15, hjust = 0.5),
                     axis.title.x = element_text(size = 11),
                     axis.title.y = element_text(size = 11),
                     axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
                     axis.ticks.x = element_blank(),
                     strip.text = element_text(size = 6, 
                                               margin = margin(0.05, 0, 0.05, 0, "cm")))

#Assign theme options adjust x-axis label size
theme_text4 <- theme(plot.title = element_text(size = 15, hjust = 0.5),
                     axis.title.x = element_text(size = 11),
                     axis.title.y = element_text(size = 11),
                     axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1,
                                                size = 7.5),
                     axis.ticks.x = element_blank(),
                     strip.text = element_text(size = 6, 
                                               margin = margin(0.05, 0, 0.05, 0, "cm")))

#Set range for y-axis
breaks_m <- seq(0, max(burden$PIF_percent, na.rm = T), 5)
scale_y_bod <- scale_y_continuous(breaks = breaks_m)

#Theme option for identifying mean in the boxplots
mean_dot <- stat_summary(fun = mean, geom = "point", shape = 20, size = 2, 
                         color = "red", fill = "red") 



##PART 8.2: PLOTTING POPULATION IMPACT FRACTION (PIF) ACROSS THE U.S. ----

#Plot of NO2 PIF across U.S. census tracts
burden %>%
  select(YEAR, PIF_percent) %>% 
  ggplot(aes(x = as.factor(YEAR), y = PIF_percent)) +
  facet_wrap( ~ YEAR, nrow =  1) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) +
  stat_boxplot(geom = "errorbar") +
  mean_dot +
  theme_bw() +
  labs(title = "Population impact fraction of NO2 across adult-populated U.S. census tracts in 2020", 
       x = "Year", 
       y = "Population Impact Fraction (%)") +
  theme_text2 +
  scale_y_bod +
  scale_colour_manual(values = c("black", "red"), aesthetics = "colour") +
  scale_fill_manual(values = c("white", "white", "white", "white", "white", 
                               "white", "white"))

#Table of previous boxplot
StatSummary_PIF_national <- burden %>%
  filter(TOTAL_ADULTS > 0) %>%
  summarise(Mean = mean(PIF_percent, na.rm = T),
            Minimum = min(PIF_percent, na.rm = T), 
            Lower25 = quantile(PIF_percent, 0.25, na.rm = T),
            Median = median(PIF_percent, na.rm = T),
            Upper25 = quantile(PIF_percent, 0.75, na.rm = T),
            Maximum = max(PIF_percent, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  as.data.frame() %>% 
  t() %>%  
  as.data.frame()

#View table
view(StatSummary_PIF_national)

#Rename column
colnames(StatSummary_PIF_national) <- c("PIF (%)")

#Save file
write.csv(StatSummary_PIF_national, "R_tables/08PIF_tables/StatSummary_PIF_national.csv")



#NO2 PIF by state 
burden %>%
  select(STATE.x, YEAR, PIF_percent) %>% 
  ggplot(aes(x = as.factor(YEAR), y = PIF_percent)) +
  facet_wrap( ~ STATE.x, nrow =  6) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) +
  stat_boxplot(geom = "errorbar") +
  mean_dot +
  theme_bw() +
  labs(title = "Population impact fraction of NO2 by U.S. state in 2020", 
       x = "Year", 
       y = "Population Impact Fraction (%)") +
  theme_text3 +
  scale_y_bod +
  scale_colour_manual(values = c("black", "red"), aesthetics = "colour") +
  scale_fill_manual(values = c("white", "white", "white", "white", "white", 
                               "white", "white"))

#Table of previous boxplot
StatSummary_PIF_states <- burden %>%
  select(STATE.x, YEAR, PIF_percent) %>%
  group_by(STATE.x) %>%
  summarise(Mean = mean(PIF_percent, na.rm = T),
            Minimum = min(PIF_percent, na.rm = T), 
            Lower25 = quantile(PIF_percent, 0.25, na.rm = T),
            Median = median(PIF_percent, na.rm = T),
            Upper25 = quantile(PIF_percent, 0.75, na.rm = T),
            Maximum = max(PIF_percent, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  as.data.frame()

#View table
view(StatSummary_PIF_states)

#Save file
write.csv(StatSummary_PIF_states, "R_tables/08PIF_tables/StatSummary_PIF_states.csv")




##PART 8.3: TABLE OF PIF BY RACE ACROSS THE U.S. ----

##First create tables with population, state, year, and PIFs for each racial and ethnic group##

#White pop
WHITE_ALONE_PIFplot <- burden %>%
  filter(WHITE_ALONE > 0) %>% 
  group_by(WHITE_ALONE, STATE.x, YEAR) %>%
  summarise(PIF_WHITE = (PIF * ((WHITE_ALONE - (WHITE_ALONE * PRV)) * IR)) /
              ((WHITE_ALONE - (WHITE_ALONE * PRV)) * IR)) %>%
  mutate(WHITE_ALONE_pop = "White")

#Rename columns
colnames(WHITE_ALONE_PIFplot) <- c("Adult_pop", "STATE", "YEAR", "PIF", "Race")



#Black pop
BLACK_ALONE_PIFplot <- burden %>%
  filter(BLACK_ALONE > 0) %>% 
  group_by(BLACK_ALONE, STATE.x, YEAR) %>%
  summarise(PIF_black = (PIF * ((BLACK_ALONE - (BLACK_ALONE * PRV)) * IR)) /
              ((BLACK_ALONE - (BLACK_ALONE * PRV)) * IR)) %>%
  mutate(BLACK_ALONE_pop = "Black")

#Rename columns
colnames(BLACK_ALONE_PIFplot) <- c("Adult_pop", "STATE", "YEAR", "PIF", "Race")



#AIAN pop
AIAN_ALONE_PIFplot <- burden %>%
  filter(AIAN_ALONE > 0) %>% 
  group_by(AIAN_ALONE, STATE.x, YEAR) %>%
  summarise(PIF_AIAN = (PIF * ((AIAN_ALONE - (AIAN_ALONE * PRV)) * IR)) /
              ((AIAN_ALONE - (AIAN_ALONE * PRV)) * IR)) %>%
  mutate(AIAN_ALONE_pop = "AIAN")

#Rename columns
colnames(AIAN_ALONE_PIFplot) <- c("Adult_pop", "STATE", "YEAR", "PIF", "Race")



#Asian pop
ASIAN_ALONE_PIFplot <- burden %>%
  filter(ASIAN_ALONE > 0) %>% 
  group_by(ASIAN_ALONE, STATE.x, YEAR) %>%
  summarise(PIF_ASIAN = (PIF * ((ASIAN_ALONE - (ASIAN_ALONE * PRV)) * IR)) /
              ((ASIAN_ALONE - (ASIAN_ALONE * PRV)) * IR)) %>%
  mutate(ASIAN_ALONE_pop = "Asian")

#Rename columns
colnames(ASIAN_ALONE_PIFplot) <- c("Adult_pop", "STATE", "YEAR", "PIF", "Race")



#NHPI pop
NHPI_ALONE_PIFplot <- burden %>%
  filter(NHPI_ALONE > 0) %>% 
  group_by(NHPI_ALONE, STATE.x, YEAR) %>%
  summarise(PIF_NHPI = (PIF * ((NHPI_ALONE - (NHPI_ALONE * PRV)) * IR)) /
              ((NHPI_ALONE - (NHPI_ALONE * PRV)) * IR)) %>%
  mutate(NHPI_ALONE_pop = "NHPI")

#Rename columns
colnames(NHPI_ALONE_PIFplot) <- c("Adult_pop", "STATE", "YEAR", "PIF", "Race")



#Other race pop
OTHER_ALONE_PIFplot <- burden %>%
  filter(OTHER_ALONE > 0) %>% 
  group_by(OTHER_ALONE, STATE.x, YEAR) %>%
  summarise(PIF_OTHER = (PIF * ((OTHER_ALONE - (OTHER_ALONE * PRV)) * IR)) /
              ((OTHER_ALONE - (OTHER_ALONE * PRV)) * IR)) %>%
  mutate(OTHER_ALONE_pop = "Other")

#Rename columns
colnames(OTHER_ALONE_PIFplot) <- c("Adult_pop", "STATE", "YEAR", "PIF", "Race")



#Two or more races pop
TWO_MORE_PIFplot <- burden %>%
  filter(TWO_MORE > 0) %>% 
  group_by(TWO_MORE, STATE.x, YEAR) %>%
  summarise(PIF_TWOMORE = (PIF * ((TWO_MORE - (TWO_MORE * PRV)) * IR)) /
              ((TWO_MORE - (TWO_MORE * PRV)) * IR)) %>%
  mutate(TWO_MORE_pop = "Two or more")

#Rename columns
colnames(TWO_MORE_PIFplot) <- c("Adult_pop", "STATE", "YEAR", "PIF", "Race")



#Hispanic Latino pop
HisLat_PIFplot <- burden %>%
  filter(HisLat_TOTAL > 0) %>% 
  group_by(HisLat_TOTAL, STATE.x, YEAR) %>%
  summarise(PIF_HisLat = (PIF * ((HisLat_TOTAL - (HisLat_TOTAL * PRV)) * IR)) /
              ((HisLat_TOTAL - (HisLat_TOTAL * PRV)) * IR)) %>%
  mutate(HisLat_pop = "Hispanic/Latino")

#Rename columns
colnames(HisLat_PIFplot) <- c("Adult_pop", "STATE", "YEAR", "PIF", "Race")



#Non-Hispanic Latino pop
NotHisLat_PIFplot <- burden %>%
  filter(NotHisLat_TOTAL > 0) %>% 
  group_by(NotHisLat_TOTAL, STATE.x, YEAR) %>%
  summarise(PIF_NotHisLat = (PIF * ((NotHisLat_TOTAL - (NotHisLat_TOTAL * PRV)) * IR)) /
              ((NotHisLat_TOTAL - (NotHisLat_TOTAL * PRV)) * IR)) %>%
  mutate(NotHisLat_pop = "Not Hispanic/Latino")

#Rename columns
colnames(NotHisLat_PIFplot) <- c("Adult_pop", "STATE", "YEAR", "PIF", "Race")



##Next, save each table as csv files to R_table directory##
write.csv(WHITE_ALONE_PIFplot, "R_tables/08PIF_tables/WHITE_ALONE_PIFplot.csv")
write.csv(BLACK_ALONE_PIFplot, "R_tables/08PIF_tables/BLACK_ALONE_PIFplot.csv")
write.csv(AIAN_ALONE_PIFplot, "R_tables/08PIF_tables/AIAN_ALONE_PIFplot.csv")
write.csv(ASIAN_ALONE_PIFplot, "R_tables/08PIF_tables/ASIAN_ALONE_PIFplot.csv")
write.csv(NHPI_ALONE_PIFplot, "R_tables/08PIF_tables/NHPI_ALONE_PIFplot.csv")
write.csv(OTHER_ALONE_PIFplot, "R_tables/08PIF_tables/OTHER_ALONE_PIFplot.csv")
write.csv(TWO_MORE_PIFplot, "R_tables/08PIF_tables/TWO_MORE_PIFplot.csv")
write.csv(HisLat_PIFplot, "R_tables/08PIF_tables/HisLat_PIFplot.csv")
write.csv(NotHisLat_PIFplot, "R_tables/08PIF_tables/NotHisLat_PIFplot.csv")

##Afterwards, open a new Excel file and combine each racial/ethnic group's tables
##in this new file. Keep relevant columns; i.e. year, state, PIF, and race. Change PIF to %.
##"PIFplot_RaceHisLat_combined.csv" is this new file (within the same folder as the above file list)

#Upload new (combined) Excel file
PIFplot_RaceHisLat_combined <- 
  read_csv("R_tables/08PIF_tables/PIFplot_RaceHisLat_combined.csv")

#View table
view(PIFplot_RaceHisLat_combined)



##Then, create the combined PIF table by race and ethnicity##

#Reorder racial and ethnic groups
PIFplot_RaceHisLat_combined$Race <- factor(PIFplot_RaceHisLat_combined$Race, 
                                           c("White", "Black", "AIAN", "Asian",
                                             "NHPI", "Other", "Two or more", 
                                             "Hispanic/Latino", "Not Hispanic/Latino"))
#Create table
PIFtable_RaceHisLat <- PIFplot_RaceHisLat_combined %>% 
  group_by(Race) %>%
  summarise(Mean = mean(PIF, na.rm = T),
            Minimum = min(PIF, na.rm = T), 
            Lower25 = quantile(PIF, 0.25, na.rm = T),
            Median = median(PIF, na.rm = T),
            Upper25 = quantile(PIF, 0.75, na.rm = T),
            Maximum = max(PIF, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  as.data.frame() %>%
  t() %>%  
  as.data.frame()

#Rename columns
colnames(PIFtable_RaceHisLat) <- c("White", "Black", "AIAN", "Asian", "NHPI", 
                                   "Other", "Two or more", "Hispanic/Latino", 
                                   "Not Hispanic/Latino")
#Delete unnecessary row
PIFtable_RaceHisLat <- PIFtable_RaceHisLat[-1,]

#View table
view(PIFtable_RaceHisLat)

#Save file
write.csv(PIFtable_RaceHisLat, "R_tables/08PIF_tables/PIFtable_RaceHisLat.csv")




##PART 8.4: PLOTTING PIF BY MEDIAN HOUSEHOLD INCOME ----

#Reorder income groups from least to greatest
burden_clean_income$INCOME_GROUP <- factor(burden_clean_income$INCOME_GROUP, 
                                           c("<$27,000","$27,000 to <$52,000",
                                             "$52,000 to <$85,000","$85,000 to <$141,000", 
                                             ">=$141,000"))
#Plot
burden_clean_income %>%
  select(YEAR, INCOME_GROUP, PIF_percent) %>% 
  ggplot(aes(x = INCOME_GROUP, y = PIF_percent)) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) +
  stat_boxplot(geom = "errorbar") +
  mean_dot +
  theme_bw() +
  labs(title = "Population impact fraction of NO2 by median household income groups in 2020", 
       x = "Income Groups", 
       y = "Population Impact Fraction (%)") +
  theme_text3 +
  scale_y_bod +
  mean_dot +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour") +
  scale_fill_manual(values=c("white", "white", "white", "white", "white", "white", "white"))


##PART 8.4.1: PLOTTING PIF BY MEDIAN HOUSEHOLD INCOME AND STATE ----

burden_clean_income %>%
  select(STATE.x, YEAR, INCOME_GROUP, PIF_percent) %>% 
  ggplot(aes(x = INCOME_GROUP, y = PIF_percent)) +
  facet_wrap( ~ STATE.x, nrow =  6) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) +
  stat_boxplot(geom = "errorbar") +
  theme_bw() +
  labs(title = 
         "Population impact fraction by median household income groups for each U.S. state in 2020", 
       x = "Income Groups", 
       y = "Population Impact Fraction (%)") +
  theme_text4 +
  scale_y_bod +
  mean_dot +
  scale_colour_manual(values = c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values = c("white", "white", "white", "white", "white", "white", "white"))



##PART 8.5: PLOTTING PIF BY LIVING LOCATION ----

#Reorder living location types
burden_clean_livinglocation$UrbanRural <- factor(burden_clean_livinglocation$UrbanRural, 
                                                 c("Urban", "Rural"))

#Plot
burden_clean_livinglocation %>%
  select(YEAR, UrbanRural, PIF_percent) %>% 
  ggplot(aes(x = UrbanRural, y = PIF_percent)) +
  facet_wrap( ~ YEAR, nrow =  1) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) +
  stat_boxplot(geom = "errorbar") +
  mean_dot +
  theme_bw() +
  labs(title = "Population impact fraction of NO2 by living location in 2020", 
       x = "Living Location", 
       y = "Population Impact Fraction (%)") +
  theme_text2 +
  scale_y_bod +
  scale_colour_manual(values = c("black", "red"), aesthetics = "colour") +
  scale_fill_manual(values = c("white", "white", "white", "white", "white", "white", "white"))



##PART 8.5.1: PLOTTING PIF BY LIVING LOCATION AND STATE ----

burden_clean_livinglocation %>%
  select(STATE.x, YEAR, UrbanRural, PIF_percent) %>% 
  ggplot(aes(x = UrbanRural, y = PIF_percent)) +
  facet_wrap( ~ STATE.x, nrow =  6) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) +
  stat_boxplot(geom = "errorbar") +
  mean_dot +
  theme_bw() +
  labs(title = 
         "Population impact fraction by living location for each U.S. state in 2020", 
       x = "Living Location", 
       y = "Population Impact Fraction (%)") +
  theme_text4 +
  scale_y_bod +
  scale_colour_manual(values = c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values = c("white", "white", "white", "white", "white", "white", "white"))




