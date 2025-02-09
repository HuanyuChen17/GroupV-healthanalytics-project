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
The following R packages are required to run the scripts. Install them using the command below: *install.packages(c("dplyr", "ipumsr", "ggplot2", "car", "flextable", "officer", "RColorBrewer", "broom"))*

- **Files: nhis_00004.xml: Metadata for the dataset.**
- **Corresponding microdata file: Contains the raw data.**

## Workflow Overview
The project is divided into several components:
- **1.	Data Cleaning**
- **2.	Descriptive Statistics**
- **3.	Visualization**
- **4.	Main Regression Analysis**
- **5.	Heterogeneity Analysis and visualization**
- **6.	Robustness Checks**

## Code Instructions
- **1.	Data Cleaning**
This step preprocesses the raw data by:
-	Recoding variables.
-	Filtering for relevant observations (e.g., year = 2022).
-	Ensuring consistency in categorical variables.

- **2.	Descriptive Statistics**
-	Summarize categorical variables like education and vaccination rates.
-	Summarize continuous variables like age.
-	Export summary tables as a Word document (Descriptive_Statistics.docx).


- **3.	Visualization**
-	Group by education level and calculate the average vaccination rate.
-	Create bar charts with ggplot2.


- **4.	Main Regression Analysis**
- Run a linear probability model (LPM) with lm.
- Extract results and format them into a table.
-	Export regression results to Word (Main_Regression_Results.docx).


- **5.	Heterogeneity Analysis and visualization**
- Filter data for each region (Northeast, Midwest, South, West).
- Run regressions within each region.
- Extract coefficients and standard errors for education levels.
- Create a box chart showing regional differences in vaccination rates by education level.
- Export results to Word (Heterogeneity_Regression.docx).


- **6.	Robustness Checks**
- Run glm for Logit and Probit models.
- Extract regression results and format them into tables.
- Export results to Word (Robustness_Regression.docx).


## How to Run the Analysis
- **1.	Prepare the Environment:**
-	Place nhis_00004.xml and the corresponding microdata file in your working directory.
-	Install all required R packages.
- **2.	Run the Scripts:**
- Data Cleaning: Data cleaning and variable transformation.
-	Descriptive Statistics: Execute the descriptive statistics functions and export results to *Descriptive_Statistics.docx*.
-	Visualization: Generate the bar chart of HPV vaccination rates by education level.
-	Main Regression Analysis: Run the LPM regression script and export results to *Main_Regression_Results.docx*.
-	Heterogeneity Analysis and Visualization: Execute regional regressions and export results to *Heterogeneity_Regression.docx* and generate the box chart of HPV vaccination rates by education level among four different regions.
-	Robustness Checks: Run Logit and Probit models, and export results to *Robustness_Regression.docx*.

## Outputs
-	Descriptive Statistics Table: *Descriptive_Statistics.docx*
-	Main Regression Results: *Main_Regression_Results.docx*
-	Heterogeneity Analysis Results: *Heterogeneity_Regression.docx*
-	Robustness Check Results: *Robustness_Regression.docx*







