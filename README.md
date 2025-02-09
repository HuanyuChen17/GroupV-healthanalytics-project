# Group V README File
Outline for Introduction to Health Analytics group project

## Project Overview
This project explores the impact of educational attainment on HPV vaccination rates, with a focus on understanding how this relationship differs across various population groups and  regions. Utilizing data from the IPUMS NHIS 2022 dataset, the analysis employs descriptive statistics, regression models, heterogeneity analysis, robustness checks, and visualizations to provide a comprehensive examination of the factors influencing vaccination rates. Results are systematically documented and exported as Word reports for further review and interpretation.

## Description of data
- **IPUMS series**: IPUMS NHIS (National Health Interview Survey)
- **Countries**: United States
- **Years**: 2022
- **How to access the data**: The specific dataset (nhis_00004.xml) contains variables.
Steps to Access Data:
1.	Visit https://nhis.ipums.org/nhis-action/variables/group
2.	Request access to the NHIS dataset.
3.	Select and download the relevant variables:
HPVACHAD: Ever received HPV vaccine
EDUC: Education attainment
AGE: Age
SEX: Sex
MARSTAT: Legal marital status
RACENEW: Self-reported Race (Post-1997 OMB standards)
REGION: Region of residence
4.	Ensure you have both the XML file (nhis_00004.xml) and the microdata file in the same directory as the scripts.

## Description of how to run the code
## Prerequisites
- **Software Requirements**
R (version 4.0.0 or later)
RStudio 
- **Required R Packages**
The following R packages are required to run the scripts. Install them using the command below:
install.packages(c("dplyr", "ipumsr", "ggplot2", "car", "flextable", "officer", "RColorBrewer", "broom"))
Files
*nhis_00004.xml: Metadata for the dataset.*
*Corresponding microdata file: Contains the raw data.*


