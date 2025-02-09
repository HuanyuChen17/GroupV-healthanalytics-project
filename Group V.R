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

# Descriptive Statistics
descriptive_stats <- function(data, var) {
  stats <- data %>%
    group_by(!!sym(var)) %>%
    summarise(n = n(), Percentage = round(n() / nrow(data) * 100, 3)) %>%
    mutate(Variable = paste(var, "-", as.character(!!sym(var)))) %>%
    select(Variable, n, Percentage) %>%
    mutate(across(everything(), as.character))
  return(stats)
}

age_overall_stats <- data_clean %>%
  summarise(n = n(), Mean = round(mean(AGE, na.rm = TRUE), 3), SD = round(sd(AGE, na.rm = TRUE), 3), Min = min(AGE, na.rm = TRUE), Max = max(AGE, na.rm = TRUE)) %>%
  mutate(Variable = "AGE - Overall") %>%
  select(Variable, n, Mean, SD, Min, Max) %>%
  mutate(across(everything(), as.character))

hpv_stats <- descriptive_stats(data_clean, "HPVACHAD")
age_group_stats <- descriptive_stats(data_clean, "AGE_GROUP")
educ_stats <- descriptive_stats(data_clean, "EDUC_GROUP")
sex_stats <- descriptive_stats(data_clean, "SEX")
marstat_stats <- descriptive_stats(data_clean, "MARSTAT_DUMMY")
race_stats <- descriptive_stats(data_clean, "RACENEW_GROUP")
region_stats <- descriptive_stats(data_clean, "REGION_GROUP")

categorical_table <- bind_rows(hpv_stats, age_group_stats, educ_stats, sex_stats, marstat_stats, race_stats, region_stats)

# Export Descriptive Statistics
table_categorical <- flextable(categorical_table) %>%
  theme_booktabs() %>%
  autofit() %>%
  set_caption("Table 1. Descriptive Statistics (Categorical Variables)")

table_age_overall <- flextable(age_overall_stats) %>%
  theme_booktabs() %>%
  autofit() %>%
  set_caption("Table 2. Descriptive Statistics (AGE - Overall)")

doc <- read_docx() %>%
  body_add_flextable(table_categorical) %>%
  body_add_flextable(table_age_overall)

print(doc, target = "Descriptive_Statistics.docx")


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


# Main Regression
# Function to format p-values exactly as displayed in R terminal
format_pvalue <- function(pval) {
  format.pval(pval, digits = 3, eps = .Machine$double.eps)
}

# Run Main Regression (LPM)
lpm_model <- lm(HPVACHAD ~ EDUC_GROUP + AGE_GROUP + SEX + MARSTAT_DUMMY + RACENEW_GROUP + REGION_GROUP, 
                data = data_clean)

# Print full regression results in R terminal
cat("----- Main Regression (LPM) Summary -----\n")
print(summary(lpm_model))

# Extract and format results
lpm_results <- broom::tidy(lpm_model) %>%
  mutate(
    Estimate = round(estimate, 3),
    `Std. Error` = round(std.error, 3),
    `t value` = round(statistic, 3),
    `Pr(>|t|)` = format_pvalue(p.value),  # Ensure exact p-value format
    Significance = case_when(
        p.value < 0.001 ~ "***",  
        p.value < 0.01 ~ "**",    
        p.value < 0.05 ~ "*",     
        p.value < 0.1 ~ ".", 
      TRUE ~ ""
    )
  ) %>%
  select(term, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`, Significance)

# Export Main Regression Results to Word
doc <- read_docx() %>%
  body_add_par("Table 1. Main Regression Results", style = "heading 1") %>%
  body_add_flextable(flextable(lpm_results) %>%
    theme_booktabs() %>%
    set_table_properties(width = 1.0, layout = "autofit") %>%
    fontsize(size = 10, part = "all") %>%
    width(j = 1, width = 2.5) %>%  
    width(j = 2:6, width = 1.5))

print(doc, target = "Main_Regression_Results.docx")


# Multicollinearity
# Calculate VIF for the linear regression model 
vif_results <- vif(lpm_model) 
# Print VIF results 
print("Variance Inflation Factor (VIF) Results:") 
print(vif_results)

# Heterogeneity Analysis
# Function to format p-values exactly as displayed in R terminal
format_pvalue <- function(pval) {
  format.pval(pval, digits = 3, eps = .Machine$double.eps)
}

# Function to run regional regressions
run_regression <- function(region) {
  subset_data <- data_clean %>% filter(REGION_GROUP == region)
  
  model <- lm(HPVACHAD ~ EDUC_GROUP + AGE_GROUP + SEX + MARSTAT_DUMMY + RACENEW_GROUP, data = subset_data)
  
  # Print full regression results in R terminal
  cat(paste0("\n----- Heterogeneity Analysis: ", region, " -----\n"))
  print(summary(model))
  
  result <- broom::tidy(model) %>%
    mutate(
      Estimate = round(estimate, 3),
      `Std. Error` = round(std.error, 3),
      `t value` = round(statistic, 3),
      `Pr(>|t|)` = format_pvalue(p.value),  # Ensure exact p-value format
      Significance = case_when(
        p.value < 0.001 ~ "***",  
        p.value < 0.01 ~ "**",    
        p.value < 0.05 ~ "*",     
        p.value < 0.1 ~ ".", 
        TRUE ~ ""
      ),
      Region = region
    ) %>%
    select(Region, term, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`, Significance) %>%
    rename(Variable = term)
  
  return(result)
}

# Run regressions for all regions
heterogeneity_results <- bind_rows(
  run_regression("Northeast"),
  run_regression("Midwest"),
  run_regression("South"),
  run_regression("West")
)

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

# Ensure each region appears only once in the Word output
heterogeneity_results$Region <- ifelse(duplicated(heterogeneity_results$Region), "", heterogeneity_results$Region)

# Export Heterogeneity Analysis Results to Word
doc <- read_docx() %>%
  body_add_par("Table 2. Heterogeneity Analysis by Region", style = "heading 1") %>%
  body_add_flextable(flextable(heterogeneity_results) %>%
    theme_booktabs() %>%
    set_table_properties(width = 1.0, layout = "autofit") %>%
    fontsize(size = 10, part = "all") %>%
    width(j = 1, width = 2) %>%
    width(j = 2:7, width = 1.5))

print(doc, target = "Heterogeneity_Regression.docx")


# Robustness Check
# Function to format p-values exactly as displayed in R terminal
format_pvalue <- function(pval) {
  format.pval(pval, digits = 3, eps = .Machine$double.eps)
}

# Run Logit and Probit Regressions
logit_model <- glm(HPVACHAD ~ EDUC_GROUP + AGE_GROUP + SEX + MARSTAT_DUMMY + RACENEW_GROUP + REGION_GROUP, 
                   family = binomial(link = "logit"), data = data_clean)

probit_model <- glm(HPVACHAD ~ EDUC_GROUP + AGE_GROUP + SEX + MARSTAT_DUMMY + RACENEW_GROUP + REGION_GROUP, 
                    family = binomial(link = "probit"), data = data_clean)

# Print full regression results in R terminal
cat("\n----- Logit Regression Results -----\n")
print(summary(logit_model))

cat("\n----- Probit Regression Results -----\n")
print(summary(probit_model))

# Function to extract and format regression results
extract_results <- function(model, model_name) {
  broom::tidy(model) %>%
    mutate(
      Estimate = round(estimate, 3),
      `Std. Error` = round(std.error, 3),
      `t value` = round(statistic, 3),
      `Pr(>|t|)` = format_pvalue(p.value),  
      Significance = case_when(
        p.value < 0.001 ~ "***",  
        p.value < 0.01 ~ "**",    
        p.value < 0.05 ~ "*",     
        p.value < 0.1 ~ ".", 
        TRUE ~ ""
      ),
      Model = model_name
    ) %>%
    select(Model, term, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`, Significance) %>%
    rename(Variable = term)
}

# Organize Logit and Probit Results
logit_results <- extract_results(logit_model, "Logit")
probit_results <- extract_results(probit_model, "Probit")

robustness_results <- bind_rows(logit_results, probit_results)

# Ensure Model column appears only once for Logit & Probit
robustness_results$Model <- ifelse(duplicated(robustness_results$Model), "", robustness_results$Model)

# Export Robustness Check Results to Word
doc <- read_docx() %>%
  body_add_par("Table 3. Robustness Check: Logit & Probit Models", style = "heading 1") %>%
  body_add_flextable(flextable(robustness_results) %>%
    theme_booktabs() %>%
    set_table_properties(width = 1.0, layout = "autofit") %>%
    fontsize(size = 10, part = "all") %>%
    width(j = 1, width = 2) %>%
    width(j = 2:7, width = 1.5))

print(doc, target = "Robustness_Regression.docx")

