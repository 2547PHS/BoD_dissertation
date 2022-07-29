#------------------------------------------------------------------------------#
#Project : BoD U.S. adult-onset asthma and NO2 exposure
#Sub     : Pollutant estimate
#Part    : (02) Plotting air pollution concentrations
#Purpose :  Plot NO2 concentrations
#Created by: PHS candidate 2547
#Date Created: 18-June-2022
#Last Updated: 23-July-2022
#------------------------------------------------------------------------------#

##Note## 
#1. must run "01Burden_DataSet.R" script first
#2. Save boxplot in the 'R_figures' folder (specifically 02AirPollution_Plots) in
#   the working directory using 'Export' function located above the displayed plot
#   to the right of script; Save plot as pdf in 'US letter' size



##PRELIMINARY STEPS: SETTING WORKSPACE ----

#Set working directory
setwd("~/BoD_R_Coding")

#Check working directory
getwd()


#Install packages relevant for subsequent analyses
my_packages <- c("readr", "tidyverse", "tidyr", "dplyr", "data.table", "ggplot2")
for (p in my_packages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}



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





