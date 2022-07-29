#------------------------------------------------------------------------------#
#Project : BoD U.S. adult-onset asthma and NO2 exposure
#Sub     : Pollutant estimate
#Part    : (08) Displaying burden estimates 
#Purpose : Plot and make tables of population impact fractions
#Created by PHS Candidate 2547
#Date Created: 19-June-2022
#Last Updated: 23-July-2022
#------------------------------------------------------------------------------#

##Note## 
#1. must run "01Burden_DataSet.R" script first
#2. Save boxplot using 'Export' function located above the displayed plot to the
#   right of script; Save plot as pdf in 'US letter' size##



##PRELIMINARY STEPS: SETTING WORKSPACE ----

#Set working directory
setwd("~/BoD_R_Coding")

#Check working directory
getwd()


#Install packages relevant for subsequent analyses
my_packages <- c("readr", "tidyverse", "dplyr", "tidyr", "data.table", "ggplot2")
for (p in my_packages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}



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





