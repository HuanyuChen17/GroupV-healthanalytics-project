# Load necessary libraries
install.packages(c("dplyr", "ipumsr", "ggplot2", "car", "flextable", "officer", "RColorBrewer", "broom"))
library(dplyr)
library(ipumsr)
library(ggplot2)
library(car)
library(flextable)
library(officer)
library(RColorBrewer)
library(broom)

# Read data
ddi <- read_ipums_ddi("nhis_00004.xml")
data_health <- read_ipums_micro(ddi)

# Data cleaning and variable transformation 
data_clean <- data_health %>%
  select(EDUC, HPVACHAD, AGE, SEX, YEAR, MARSTAT, RACENEW, REGION) %>%
  mutate(
    HPVACHAD = case_when(HPVACHAD == 20 ~ 1, HPVACHAD == 10 ~ 0, TRUE ~ NA_real_),
    SEX = case_when(SEX == 1 ~ 1, SEX == 2 ~ 0, TRUE ~ NA_real_),
    EDUC_GROUP = case_when(
      EDUC %in% 100:116 ~ "No High School",
      EDUC %in% 200:202 ~ "High School Graduate",
      EDUC %in% c(300:303, 400) ~ "Some College or Bachelor",
      EDUC %in% 500:505 ~ "Higher",
      TRUE ~ NA_character_
), 
# Process age grouping 
AGE_GROUP = case_when(
    AGE <= 25 ~ 1,  # 26- group
    AGE > 25 ~ 0,   # 26+ group
TRUE ~ NA_real_
),
     # Generate marital status dummy variable (Married = 1, Others = 0)
    MARSTAT_DUMMY = case_when(
      MARSTAT %in% c(10, 11, 12, 13) ~ 1,  
      MARSTAT %in% c(20, 30, 40, 50) ~ 0,  
      TRUE ~ NA_real_
    ) %>% as.numeric(),
    
    # Process self-reported race variable
    RACENEW_GROUP = case_when(
      RACENEW == 100 ~ "White",
      RACENEW == 200 ~ "Black",
      RACENEW == 400 ~ "Asian",
      RACENEW %in% c(300, 510, 540) ~ "Other",
      TRUE ~ NA_character_
    ),

    # Process REGION variable (01: Northeast, 02: North Central/Midwest, 03: South, 04: West)
    REGION_GROUP = case_when(
      REGION == 01 ~ "Northeast",
      REGION == 02 ~ "Midwest",
      REGION == 03 ~ "South",
      REGION == 04 ~ "West",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    YEAR == 2022,  # Keep only 2022 data
    !is.na(HPVACHAD), 
    !is.na(EDUC_GROUP), 
    !is.na(AGE_GROUP), 
    !is.na(SEX), 
    !is.na(MARSTAT_DUMMY),  
    !is.na(RACENEW_GROUP),
    !is.na(REGION_GROUP)  # Keep only samples with valid REGION information
  )

# Convert categorical variables to factors
data_clean$EDUC_GROUP <- factor(data_clean$EDUC_GROUP, levels = c("No High School", "High School Graduate", "Some College or Bachelor", "Higher"))
data_clean$RACENEW_GROUP <- factor(data_clean$RACENEW_GROUP, levels = c("White", "Black", "Asian", "Other"))
data_clean$REGION_GROUP <- factor(data_clean$REGION_GROUP, levels = c("Northeast", "Midwest", "South", "West"))





# Visualization: Bar chart of Education Level vs HPV Vaccination Rate**
# Compute the average HPV vaccination rate for each education level
data_summary <- data_clean %>%
  group_by(EDUC_GROUP) %>%
  summarise(HPV_Rate = mean(HPVACHAD, na.rm = TRUE))

# Create a bar chart
ggplot(data_summary, aes(x = EDUC_GROUP, y = HPV_Rate, fill = EDUC_GROUP)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "HPV Vaccination Rate by Education Level",
       x = "Education Level",
       y = "HPV Vaccination Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")




# Visualization: Extract coefficients and standard errors for education levels
data_multilevel <- heterogeneity_results %>%
  filter(Variable %in% c("EDUC_GROUPHigh School Graduate", 
                         "EDUC_GROUPSome College or Bachelor", 
                         "EDUC_GROUPHigher")) %>%
  mutate(
    Education = case_when(
      Variable == "EDUC_GROUPHigh School Graduate" ~ "High School Graduate",
      Variable == "EDUC_GROUPSome College or Bachelor" ~ "Some College or Bachelor",
      Variable == "EDUC_GROUPHigher" ~ "Higher Education"
    )
  ) %>%
  select(Region, Education, Coefficient = Estimate, SE = `Std. Error`)

# Plot subfigures
ggplot(data_multilevel, aes(x = Region, y = Coefficient, fill = Education)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Coefficient - 1.96 * SE, ymax = Coefficient + 1.96 * SE), 
                position = position_dodge(0.9), width = 0.2) +
  facet_wrap(~ Education, ncol = 1) +
  labs(title = "Regional Differences in HPV Vaccination by Education Level",
       x = "Region", y = "Coefficient") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

