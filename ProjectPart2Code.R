library(haven) 
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(gt)
library(rdrobust)
library(stargazer)
library(scales)
setwd("/Users/shreyachaturvedi/Downloads/Spring2024/ECON50/Project")

atlas <- read_dta("atlas.dta")
head(atlas)

# Chosen tract
state_id <- 17
county_id <- 31
tract_id <- 809400

# Compute the means for the full dataset and the specific tract
means_full <- atlas %>%
  summarize(
    Mean_Household_Income_2000 = mean(hhinc_mean2000, na.rm = TRUE),
    Fraction_with_College_Degree_or_More_2000 = mean(frac_coll_plus2000, na.rm = TRUE),
    Population_Density_2000 = mean(popdensity2000, na.rm = TRUE),
    Poverty_Share_2000 = mean(poor_share2000, na.rm = TRUE),
    Share_Black_2000 = mean(share_black2000, na.rm = TRUE),
    Share_White_2000 = mean(share_white2000, na.rm = TRUE),
    Employment_Rate_2000 = mean(emp2000, na.rm = TRUE),
    Population_Density_2010 = mean(popdensity2010, na.rm = TRUE),
    Kfr_Pooled_Pooled_25 = mean(kfr_pooled_pooled_p25, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Demographic Variable", values_to = "Mean for Full Dataset")

means_tract <- atlas %>%
  filter(state == state_id, county == county_id, tract == tract_id) %>%
  summarize(
    Mean_Household_Income_2000 = mean(hhinc_mean2000, na.rm = TRUE),
    Fraction_with_College_Degree_or_More_2000 = mean(frac_coll_plus2000, na.rm = TRUE),
    Population_Density_2000 = mean(popdensity2000, na.rm = TRUE),
    Poverty_Share_2000 = mean(poor_share2000, na.rm = TRUE),
    Share_Black_2000 = mean(share_black2000, na.rm = TRUE),
    Share_White_2000 = mean(share_white2000, na.rm = TRUE),
    Employment_Rate_2000 = mean(emp2000, na.rm = TRUE),
    Population_Density_2010 = mean(popdensity2010, na.rm = TRUE),
    Kfr_Pooled_Pooled_25 = mean(kfr_pooled_pooled_p25, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Demographic Variable", values_to = "Mean for Specific Tract")

# Merge the two datasets
demographic_comparison <- left_join(means_full, means_tract, by = "Demographic Variable")

# Create the gt table with formatted numbers
gt_table <- gt(demographic_comparison) %>%
  fmt_number(
    columns = vars(`Mean for Full Dataset`, `Mean for Specific Tract`),
    decimals = 2,
    scale_by = 1,
    suffixing = FALSE
  ) %>%
  cols_label(
    `Demographic Variable` = "Demographic Variable",
    `Mean for Full Dataset` = "Mean for Full Dataset",
    `Mean for Specific Tract` = "Mean for Specific Tract"
  )

# Print the gt table to view it
print(gt_table)

### Hypothesis 1A: Influence of Northwestern University: Higher Education Mechanism

atlas %>%
  filter(!is.na(frac_coll_plus2010)) %>%
  group_by(education_level = cut(frac_coll_plus2010, 
                                 breaks = quantile(frac_coll_plus2010, na.rm = TRUE, probs = 0:4/4),
                                 labels = c("First Quantile", "Second Quantile", "Third Quantile", "Fourth Quantile"),
                                 include.lowest = TRUE)) %>%
  summarize(mean_mobility = mean(kfr_pooled_pooled_p25, na.rm = TRUE)) %>%
  ggplot(aes(x = education_level, y = mean_mobility)) +
  geom_bar(stat = "identity", fill = "grey") +
  theme_minimal() +
  labs(title = "Upward Mobility by Education Level", x = "Education Level as Measured by Fraction with College Degree", y = "Mean Upward Mobility")

# Correlation Analysis
cor(atlas$frac_coll_plus2010, atlas$kfr_pooled_pooled_p25, use = "complete.obs")


# Calculate means for 2000
mean_educ2000_full <- mean(atlas$frac_coll_plus2000, na.rm = TRUE)
mean_educ2000_state <- mean(atlas$frac_coll_plus2000[atlas$state == 17], na.rm = TRUE)
mean_educ2000_county <- mean(atlas$frac_coll_plus2000[atlas$state == 17 & atlas$county == 31], na.rm = TRUE)
mean_educ2000_tract <- atlas$frac_coll_plus2000[atlas$state == 17 & atlas$county == 31 & atlas$tract == 809400]

# Calculate means for 2010
mean_educ2010_full <- mean(atlas$frac_coll_plus2010, na.rm = TRUE)
mean_educ2010_state <- mean(atlas$frac_coll_plus2010[atlas$state == 17], na.rm = TRUE)
mean_educ2010_county <- mean(atlas$frac_coll_plus2010[atlas$state == 17 & atlas$county == 31], na.rm = TRUE)
mean_educ2010_tract <- atlas$frac_coll_plus2010[atlas$state == 17 & atlas$county == 31 & atlas$tract == 809400]

# Prepare data for plotting
plot_data <- data.frame(
  Category = rep(c("Full US", "State Level", "County Level", "Evanston Tract"), 2),
  Mean = c(mean_educ2000_full, mean_educ2000_state, mean_educ2000_county, mean_educ2000_tract,
           mean_educ2010_full, mean_educ2010_state, mean_educ2010_county, mean_educ2010_tract),
  Year = rep(c("2000", "2010"), each=4)
)
plot_data$Category <- factor(plot_data$Category, levels = c("Full US", "State Level", "County Level", "Evanston Tract"))

# Plotting Bar Charts
ggplot(plot_data, aes(x=Category, y=Mean, fill=Year)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.7) +
  scale_y_continuous(limits = c(0, 1.2)) +  # Set y axis limits
  scale_fill_brewer(palette="Pastel1") +
  labs(title="Mean of Fraction with College Degree (2000 vs. 2010)",
       y="Mean Fraction with College Degree") +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "top")


ggplot(atlas, aes(x = frac_coll_plus2000, y = kfr_pooled_pooled_p25)) +
  stat_summary_bin(fun = "mean", bins = 20, geom = "line", color = "light grey") +  # Connected line for binned means
  stat_summary_bin(fun = "mean", bins = 20, geom = "point", color = "red") +  # Points for binned means
  stat_smooth(method = "lm", se = FALSE, color = "black",linetype = "dashed") +  # Linear best fit line
  labs(
    x = 'Fraction with College Degree (2000)',
    y = 'Mean Child Income Rank (kfr_pooled_pooled_p25)',
    title = 'Relationship between child income rank and college degree'
  ) +
  theme_minimal()

# Run the regression models
regression1a <- lm(kfr_pooled_pooled_p25 ~ frac_coll_plus2000, data = atlas)
regression1b <- lm(kfr_pooled_pooled_p25 ~ frac_coll_plus2000 + poor_share2000, data = atlas)
regression1c <- lm(kfr_pooled_pooled_p25 ~ frac_coll_plus2000 + share_black2000, data = atlas)
regression1d <- lm(kfr_pooled_pooled_p25 ~ frac_coll_plus2000 + share_black2000 + poor_share2000 + hhinc_mean2000, data = atlas)
regression1e <- lm(kfr_pooled_pooled_p25 ~ frac_coll_plus2000 + share_black2000 + poor_share2000 + hhinc_mean2000 + emp2000, data = atlas)

# Check the summaries of the models
summary(regression1a)
summary(regression1b)
summary(regression1c)
summary(regression1d)
summary(regression1e)

# Check if any model has NA coefficients and proceed if none are found
if(all(sapply(list(regression1a, regression1b, regression1c, regression1d), function(x) !any(is.na(coef(x)))))) {
  stargazer(regression1a, regression1b, regression1c, regression1d, type = "html",
            title = "Regression Results",
            align = TRUE,
            dep.var.labels.include = FALSE,
            model.numbers = FALSE,
            omit.stat = c("LL", "ser", "f"),
            column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
            covariate.labels = c("Fraction with College Degree or More in 2000", "Share Black in 2000", "Poverty Share in 2000", "Employed Share in 2000"),
            notes = c("Dependent variable: Absolute Mobility at the 25th Percentile using Household Income as Income Concept"),
            notes.align = "l",
            notes.append = FALSE,
            out = "regression_table1.html")
} else {
  cat("One or more regression models contain NA coefficients")
}



### Hypothesis 1B: Influence of Northwestern University: Test Score Mechanism 
# Calculate means for gsmn_math_g3_2013
mean_math_full <- mean(atlas$gsmn_math_g3_2013, na.rm = TRUE)
mean_math_state <- mean(atlas$gsmn_math_g3_2013[atlas$state == 17], na.rm = TRUE)
mean_math_county <- mean(atlas$gsmn_math_g3_2013[atlas$state == 17 & atlas$county == 31], na.rm = TRUE)
mean_math_tract <- atlas$gsmn_math_g3_2013[atlas$state == 17 & atlas$county == 31 & atlas$tract == 809400]

# Prepare data for plotting gsmn_math_g3_2013
plot_math_data <- data.frame(
  Category = c("Full US", "State Level", "County Level", "Evanston Tract"),
  Mean = c(mean_math_full, mean_math_state, mean_math_county, mean_math_tract)
)

plot_math_data$Category <- factor(plot_math_data$Category, levels = c("Full US", "State Level", "County Level", "Evanston Tract"))

# Plot for gsmn_math_g3_2013
ggplot(plot_math_data, aes(x=Category, y=Mean, fill=Category)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.7) +
  scale_y_continuous(limits = c(0, max(plot_math_data$Mean, na.rm = TRUE) * 1.2)) +  # Set y axis limits dynamically based on max mean
  scale_fill_brewer(palette="Pastel2") +
  labs(title="Mean 3rd Grade Math Test Scores (2013)",
       y="Mean Test Scores") +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "none")

ggplot(atlas, aes(x = gsmn_math_g3_2013, y = kfr_pooled_pooled_p25)) +
  stat_summary_bin(fun = "mean", bins = 20, geom = "line", color = "light grey") +  # Connected line for binned means
  stat_summary_bin(fun = "mean", bins = 20, geom = "point", color = "black") +  # Points for binned means
  stat_smooth(method = "lm", se = FALSE, color = "grey",linetype = "dashed") +  # Linear best fit line
  labs(
    x = 'Math Score',
    y = 'Mean Child Income Rank (kfr_pooled_pooled_p25)',
    title = 'Binned Scatter Plot with Best Fit Line'
  ) +
  theme_minimal()

cor(atlas$gsmn_math_g3_2013, atlas$kfr_pooled_pooled_p25, use = "complete.obs")

regression2 <- lm(kfr_pooled_pooled_p25 ~ gsmn_math_g3_2013, data = atlas)
summary(regression2) 

stargazer(regression2, type = "text",
          title = "Regression Results",
          dep.var.labels = "Absolute Mobility at the 25th Percentile",
          covariate.labels = c("Grade 3 Math Scores in 2013"),
          omit.stat = c("LL", "ser", "f"), # Omitting likelihood, standard error of regression, and F-statistic if desired
          out = "p2/regression_table2.html")

### Hypothesis 1C: Influence of Northwestern University: Higher Rent
# Calculate means for rent_twobed2015
mean_rent_full <- mean(atlas$rent_twobed2015, na.rm = TRUE)
mean_rent_state <- mean(atlas$rent_twobed2015[atlas$state == 17], na.rm = TRUE)
mean_rent_county <- mean(atlas$rent_twobed2015[atlas$state == 17 & atlas$county == 31], na.rm = TRUE)
mean_rent_tract <- atlas$rent_twobed2015[atlas$state == 17 & atlas$county == 31 & atlas$tract == 809400]

# Prepare data for plotting rent_twobed2015
plot_rent_data <- data.frame(
  Category = c("Full US", "State Level", "County Level", "Evanston Tract"),
  Mean = c(mean_rent_full, mean_rent_state, mean_rent_county, mean_rent_tract)
)

plot_rent_data$Category <- factor(plot_rent_data$Category, levels = c("Full US", "State Level", "County Level", "Evanston Tract"))

# Plot for rent_twobed2015
ggplot(plot_rent_data, aes(x=Category, y=Mean, fill=Category)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.7) +
  scale_y_continuous(limits = c(0, max(plot_rent_data$Mean) * 1.2)) +  # Set y axis limits dynamically based on max mean
  scale_fill_brewer(palette="Pastel1") +
  labs(title="Mean Rent for Two-Bedroom Apartment (2015)",
       y="Mean Rent") +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "none")

ggplot(atlas, aes(x = rent_twobed2015, y = kfr_pooled_pooled_p25)) +
  stat_summary_bin(fun = "mean", bins = 20, geom = "line", color = "light grey") +  # Connected line for binned means
  stat_summary_bin(fun = "mean", bins = 20, geom = "point", color = "light blue") +  # Points for binned means
  stat_smooth(method = "lm", se = FALSE, color = "black",linetype = "dashed") +  # Linear best fit line
  labs(
    x = 'Rent',
    y = 'Mean Child Income Rank (kfr_pooled_pooled_p25)',
    title = 'Binned Scatter Plot with Best Fit Line'
  ) +
  theme_minimal()

regression3 <- lm(kfr_pooled_pooled_p25 ~ rent_twobed2015, data = atlas)
summary(regression3) 

stargazer(regression3, type = "text",
          title = "Regression Results",
          dep.var.labels = "Absolute Mobility at the 25th Percentile",
          covariate.labels = c("Mean Rent of Two Bedroom in 2015"),
          omit.stat = c("LL", "ser", "f"), # Omitting likelihood, standard error of regression, and F-statistic if desired
          out = "p2/regression_table3.html")

####################
##### Hypothesis 2: Vegetation and Development
# Filter out NA values
atlas_clean <- atlas %>%
  filter(!is.na(vegetation), !is.na(developed), !is.na(kfr_pooled_pooled_p25))

# Create quantile-based categories for vegetation and developed variables
atlas_clean <- atlas_clean %>%
  mutate(
    vegetation_quantile = cut(vegetation, 
                              breaks = quantile(vegetation, na.rm = TRUE, probs = 0:4/4), 
                              labels = c("First Quantile", "Second Quantile", "Third Quantile", "Fourth Quantile"),
                              include.lowest = TRUE),
    developed_quantile = cut(developed, 
                             breaks = quantile(developed, na.rm = TRUE, probs = 0:4/4), 
                             labels = c("First Quantile", "Second Quantile", "Third Quantile", "Fourth Quantile"),
                             include.lowest = TRUE)
  )

# Summarize mean mobility within each quantile category for vegetation
mean_mobility_by_vegetation <- atlas_clean %>%
  group_by(vegetation_quantile) %>%
  summarize(mean_mobility = mean(kfr_pooled_pooled_p25, na.rm = TRUE))

# Summarize mean mobility within each quantile category for developed land
mean_mobility_by_developed <- atlas_clean %>%
  group_by(developed_quantile) %>%
  summarize(mean_mobility = mean(kfr_pooled_pooled_p25, na.rm = TRUE))

# Regression analysis with both vegetation and developed land as predictors
regression_model <- lm(kfr_pooled_pooled_p25 ~ vegetation + developed, data = atlas_clean)
summary(regression_model)

# Plotting the results for vegetation with updated y-axis limits
ggplot(mean_mobility_by_vegetation, aes(x = vegetation_quantile, y = mean_mobility)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Upward Mobility by Vegetation Quantile", x = "Vegetation Quantile", y = "Mean Upward Mobility") + 
  coord_cartesian(ylim = c(35, 50))

# Plotting the results for developed land with updated y-axis limits
ggplot(mean_mobility_by_developed, aes(x = developed_quantile, y = mean_mobility)) +
  geom_bar(stat = "identity", fill = "coral") +
  theme_minimal() +
  labs(title = "Upward Mobility by Developed Land Quantile", x = "Developed Land Quantile", y = "Mean Upward Mobility") +
  coord_cartesian(ylim = c(35, 50))


# Calculate means for vegetation
mean_veg_full <- mean(atlas$vegetation, na.rm = TRUE)
mean_veg_state <- mean(atlas$vegetation[atlas$state == 17], na.rm = TRUE)
mean_veg_county <- mean(atlas$vegetation[atlas$state == 17 & atlas$county == 31], na.rm = TRUE)
mean_veg_tract <- atlas$vegetation[atlas$state == 17 & atlas$county == 31 & atlas$tract == 809400]

# Means for the developed variable
mean_dev_full <- mean(atlas$developed, na.rm = TRUE)
mean_dev_state <- mean(atlas$developed[atlas$state == 17], na.rm = TRUE)
mean_dev_county <- mean(atlas$developed[atlas$state == 17 & atlas$county == 31], na.rm = TRUE)
mean_dev_tract <- atlas$developed[atlas$state == 17 & atlas$county == 31 & atlas$tract == 809400]

regression4a <- lm(kfr_pooled_pooled_p25 ~ vegetation, data = atlas)
summary(regression4a) 
regression4b <- lm(kfr_pooled_pooled_p25 ~ developed, data = atlas)
summary(regression4b) 
regression4c <- lm(kfr_pooled_pooled_p25 ~ vegetation + developed, data = atlas)
summary(regression4c) 

stargazer(regression4a, regression4b, regression4c, type = "html",
          title = "Summary of Linear Regression Analyses",
          model.names = FALSE, # Removes the model names (e.g., Model 1, Model 2) if you prefer a cleaner table
          dep.var.labels = "Absolute Mobility at the 25th Percentile",
          covariate.labels = c("Vegetation", "Developed", "Vegetation + Developed"),
          omit.stat = c("LL", "ser", "f"), # Customize this based on which stats you want to omit
          out = "p2/regression_table4.html")

####### Hypothesis 3
#### Incarceration and Influence of Crime in Chicago
# Calculate means for jail variables, now including the additional tract
jail_variables <- c("jail_pooled_pooled_p25", "jail_pooled_male_p25", "jail_pooled_female_p25", 
                    "jail_black_pooled_p25", "jail_white_pooled_p25")

# means_list <- lapply(jail_variables, function(var) {
#   full <- mean(atlas[[var]], na.rm = TRUE)
#   state <- mean(atlas[[var]][atlas$state == 17], na.rm = TRUE)
#   county <- mean(atlas[[var]][atlas$state == 17 & atlas$county == 31], na.rm = TRUE)
#   tract_809400 <- mean(atlas[[var]][atlas$state == 17 & atlas$county == 31 & atlas$tract == 809400], na.rm = TRUE)
#   tract_400400 <- mean(atlas[[var]][atlas$state == 17 & atlas$county == 31 & atlas$tract == 400400], na.rm = TRUE)
#   
#   # Return a named vector for each jail variable
#   c(full = full, state = state, county = county, tract_809400 = tract_809400, tract_400400 = tract_400400)
# })

# 
# plot_jail_data$Variable <- var_descriptions[plot_jail_data$Variable]
# 
# # Assign descriptive labels to the variable names
# var_descriptions <- setNames(c("Overall Incarceration Rate", "Male Incarceration Rate", "Female Incarceration Rate",
#                                "Black Incarceration Rate", "White Incarceration Rate"), 
#                              c("jail_pooled_pooled_p25", "jail_pooled_male_p25", "jail_pooled_female_p25", 
#                                "jail_black_pooled_p25", "jail_white_pooled_p25"))
# 
# plot_jail_data$Variable <- var_descriptions[plot_jail_data$Variable]

# # Plotting with updated names
# ggplot(plot_jail_data, aes(x = Category, y = Mean, fill = Variable)) +
#   geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
#   scale_y_continuous(limits = c(0, max(plot_jail_data$Mean, na.rm = TRUE) * 1.2)) +
#   scale_fill_brewer(palette = "Set3", name = "Incarceration Rate") +
#   labs(title = "Incarceration Rates by Group",
#        y = "Mean Incarceration Rate") +
#   theme_minimal() +
#   theme(legend.title = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1))

# Filter for non-missing values
atlas_filtered <- atlas %>%
  filter(!is.na(kfr_pooled_pooled_p25) & !is.na(jail_pooled_male_p25))

# Creating quintiles for the male incarceration rate and calculating mean mobility
incarceration_quintiles <- atlas_filtered %>%
  mutate(incarceration_quintile = ntile(jail_pooled_male_p25, 5)) %>%
  group_by(incarceration_quintile) %>%
  summarize(mean_mobility = mean(kfr_pooled_pooled_p25, na.rm = TRUE)) %>%
  ungroup()

# Define and apply labels for quintiles
quintile_labels <- c("1st Quintile", "2nd Quintile", "3rd Quintile", "4th Quintile", "5th Quintile")
incarceration_quintiles$incarceration_quintile <- factor(incarceration_quintiles$incarceration_quintile, 
                                                         levels = 1:5, labels = quintile_labels)

# Plotting mean upward mobility by quintiles of male incarceration rate
ggplot(incarceration_quintiles, aes(x = incarceration_quintile, y = mean_mobility)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Upward Mobility by Quintiles of Male Incarceration Rate", 
       x = "Quintiles of Male Incarceration Rate", 
       y = "Mean Upward Mobility (kfr_pooled_pooled_p25)")

# Run the regression models with incremental controls (assuming 'atlas' is correctly set up)
regression5a <- lm(kfr_pooled_pooled_p25 ~ jail_pooled_male_p25, data = atlas)
regression5b <- lm(kfr_pooled_pooled_p25 ~ jail_pooled_male_p25 + hhinc_mean2000, data = atlas)
regression5c <- lm(kfr_pooled_pooled_p25 ~ jail_pooled_male_p25 + hhinc_mean2000 + poor_share2000, data = atlas)
regression5d <- lm(kfr_pooled_pooled_p25 ~ jail_pooled_male_p25 + hhinc_mean2000 + poor_share2000 + share_white2000, data = atlas)


# Check if any model has NA coefficients and proceed if none are found
regression_list <- list(regression5a, regression5b, regression5c, regression5d)
if(all(sapply(regression_list, function(x) !any(is.na(coef(x)))))) {
  stargazer(regression5a, regression5b, regression5c, regression5d, type = "html",
            title = "Regression Results",
            align = TRUE,
            dep.var.labels = "Absolute Mobility at the 25th Percentile using Household Income as Income Concept",
            model.numbers = FALSE,
            omit.stat = c("LL", "ser", "f"),
            column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
            covariate.labels = c("Male Incarceration Rate", "Mean Household Income in 2000", "Poverty Share in 2000", "Share White in 2000"),
            notes.align = "l",
            notes.append = TRUE,
            out = "regression_table2.html")
} else {
  cat("One or more regression models contain NA coefficients")
}
