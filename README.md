'Traffic-Related Air Pollution NO2 and Adult-Onset Asthma in the United States of America: A Burden of Disease Assessment'

Aim: to estimate the number of incident adult asthma cases attributable to TRAP NO2 exposure across the U.S. in 2020

Objectives
1.	To estimate and analyse the number of adult-onset asthma cases attributable to TRAP NO2 across the U.S. in 2020 at the baseline and counterfactual scenario NO2 concentrations.
2.	To stratify and analyse the number of adult-onset asthma cases attributable to TRAP NO2 across the U.S. in 2020 based on three sociodemographic traits – race and ethnicity, median household income, and living location – at the baseline and counterfactual scenario NO2 concentrations.


This repository contains the necessary files for conducting GIS spatial analysis and burden of disease calculations for this dissertation. Each folder is explained below:
1.  'BoD_R_Coding' holds the R scripts for analysis, the tables necessary to run these codes, and the tables and figures produced by these scripts.
2.  'NO2 csv shapefiles' contain csv files of mean NO2 concentrations (measured in micrograms/cubic metre) for each U.S. state at the census tract level. These files are derived from each state's attribute table in ArcGIS Pro, and they were tidied for further usage in the R coding analysis.
3.  'NO2 files' includes two articles - (1) Anenberg et al.'s published article on how the NO2 global data were collected and analysed and (2) the WHO Air Quality Guildeline limits. Unfortunately, the NO2 global raster data files are too big to upload to Github.
4.  'Unzipped demographic files' contains the sociodemographic census files along with their metadata.
5.  'Zipped ArcGIS files' are in fact the unzipped shapefiles for each U.S. state and the entire U.S. Certain files for very large states (e.g., California and Texas) as too big to upload to Github.
     N.B.This folder was mistakenly named early in the data analysis process. Changing it now would disrupt the ArcGIS Pro analysis because of how the data are linked.
