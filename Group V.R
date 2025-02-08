




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

