## HKS SUP 135: Using Big Data to Solve Economic and Social Problems
## Project Part 1: Code
## Author: Shreya Chaturvedi
## Date: March 7th, 2024

#importing necessary libraries
library(haven) 
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(gt)
library(rdrobust)

setwd("/Users/shreyachaturvedi/Downloads/Spring2024/ECON50/Project")

atlas <- read_dta("atlas.dta")
head(atlas)

## Question 3
#plotting histogram of kfr_pooled_pooled_p25
ggplot(atlas, aes(x = kfr_pooled_pooled_p25)) +
  geom_histogram(binwidth = 5, fill="skyblue", color="black") +
  geom_vline(aes(xintercept=mean(kfr_pooled_pooled_p25, na.rm = TRUE)),
             color="red", linetype="dashed", size=1) +
  labs(title="Histogram of kfr_pooled_pooled_p25",
       x="kfr_pooled_pooled_p25",
       y="Frequency") +
  theme_minimal() +
  annotate("text", x = mean(atlas$kfr_pooled_pooled_p25, na.rm = TRUE), 
           y = Inf, label = sprintf("Mean: %.2f", mean(atlas$kfr_pooled_pooled_p25, na.rm = TRUE)), 
           vjust=2, color="red", size=2.5) + 
  theme(plot.title = element_text(size = 9)) 

## Question 4
#Calculating summary statistics
summary(atlas$kfr_pooled_pooled_p25)
num_missing <- sum(is.na(atlas$kfr_pooled_pooled_p25))
mean_value <- mean(atlas$kfr_pooled_pooled_p25, na.rm = TRUE)
sd_value <- sd(atlas$kfr_pooled_pooled_p25, na.rm = TRUE)
min_value <- min(atlas$kfr_pooled_pooled_p25, na.rm = TRUE)
max_value <- max(atlas$kfr_pooled_pooled_p25, na.rm = TRUE)

## Question 6
#Calculating mean at different levels
mean_full <- mean(atlas$kfr_pooled_pooled_p25, na.rm = TRUE)
mean_state <- mean(atlas$kfr_pooled_pooled_p25[atlas$state == 17], na.rm = TRUE)
mean_county <- mean(atlas$kfr_pooled_pooled_p25[atlas$state == 17 & atlas$county == 31], na.rm = TRUE) 
mean_tract<- mean(atlas$kfr_pooled_pooled_p25[atlas$state == 17 & atlas$county == 31 & atlas$tract == 809400], na.rm = TRUE) 
means_df <- data.frame(
  Group = factor(c("Full Dataset", "State Level", "County Level", "Tract Level"), 
                 levels = c("Full Dataset", "State Level", "County Level", "Tract Level")),
  Mean = c(mean_full, mean_state, mean_county, mean_tract)
)

#Plotting the means
ggplot(means_df, aes(x=Group, y=Mean, fill=Group)) +
  geom_bar(stat="identity", show.legend = FALSE, width=.6) +
  scale_fill_manual(values=c("lightblue", "steelblue", "grey", "darkgrey")) +
  labs(y = "Mean kfr_pooled_pooled_p25", x = "", title = "Comparison of Mean Statistic at Different Levels") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8), 
        plot.title = element_text(size = 9), 
        axis.title.y = element_text(size = 9)) 

## Question 7
#Calculating SDs at different levels
sd_full <- sd(atlas$kfr_pooled_pooled_p25, na.rm = TRUE)
sd_state <- sd(atlas$kfr_pooled_pooled_p25[atlas$state == 17], na.rm = TRUE)
sd_county <- sd(atlas$kfr_pooled_pooled_p25[atlas$state == 17 & atlas$county == 31], na.rm = TRUE)


std_devs_df <- data.frame(
  Group = factor(c("Full Dataset", "State Level", "County Level"), 
                 levels = c("Full Dataset", "State Level", "County Level")),
  Std_Dev = c(sd_full, sd_state, sd_county)
)

#Plotting the SDs
ggplot(std_devs_df, aes(x=Group, y=Std_Dev, fill=Group)) +
  geom_bar(stat="identity", show.legend = FALSE, width=.6) +
  scale_fill_manual(values=c("lightblue", "steelblue", "grey")) +
  labs(y = "Standard Deviation of kfr_pooled_pooled_p25", x = "", title = "Comparison of SD Statistic at Different Levels") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        plot.title = element_text(size = 9), 
        axis.title = element_text(size = 9))

## Question 8
#Subsettong to keep particular county only and then particular tract only
subset_atlas <- atlas[atlas$state == 17 & atlas$county == 31, ]
highlight_point <- atlas[atlas$state == 17 & atlas$county == 31 & atlas$tract == 809400, ]

ggplot(subset_atlas, aes(x = rent_twobed2015, y = kfr_pooled_pooled_p25)) +
  geom_point(size = 0.1) +  # Scatter plot for all points
  geom_point(data = highlight_point, aes(x = rent_twobed2015, y = kfr_pooled_pooled_p25), colour = "blue", size = 2) + # Highlight specific point
  geom_smooth(method = "lm", col = "red") +  # Linear best fit line
  labs(x = "Rent for Two-Bedroom in 2015", y = "Mean Child Income Rank (kfr_pooled_pooled_p25)",
       title = "Rent vs. Child Income Rank") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10), 
        axis.title.x = element_text(size = 6), 
        axis.title.y = element_text(size = 6))

##Question 9
#Summarizing mean for key variables
table_df <- atlas %>%
  mutate(HOLC_grade = addNA(HOLC_grade, ifany = TRUE)) %>%
  mutate(HOLC_grade = ifelse(is.na(HOLC_grade), "NA", as.character(HOLC_grade))) %>%
  mutate(HOLC_grade = factor(HOLC_grade)) %>%
  group_by(HOLC_grade) %>%
  summarize(
    Percentage = n() / nrow(atlas),  # Calculate percentage of each grade
    Mean_kfr_pooled_p25 = mean(kfr_pooled_pooled_p25, na.rm = TRUE),  # Mean of kfr_pooled_pooled_p25
    Mean_share_black1990 = mean(share_black1990, na.rm = TRUE),  # Mean share of black population in 1990
    Mean_kfr_black_pooled_p25 = mean(kfr_black_pooled_p25, na.rm = TRUE),  # Mean of kfr_black_pooled_p25
    Mean_kfr_white_pooled_p25 = mean(kfr_white_pooled_p25, na.rm = TRUE),  # Mean of kfr_white_pooled_p25
    Mean_homeownership2010 = mean(homeownership2010, na.rm = TRUE),  # Mean homeownership rate in 2010
    Mean_vegetation = mean(vegetation, na.rm = TRUE),  # Mean vegetation index
    Mean_extreme_heat = mean(extreme_heat, na.rm = TRUE)  # Mean extreme heat index
  ) %>%
  ungroup()  # Remove grouping

# Print the table
print(table_df)

# Creating a printable table of summary values
gt_table <- gt(table_df) %>%
  tab_header(
    title = "Summary of HOLC Grades"
  ) %>%
  cols_label(
    HOLC_grade = "HOLC Grade",
    Percentage = "Percentage (%)",
    Mean_kfr_pooled_p25 = "Mean kfr_pooled_p25",
    Mean_share_black1990 = "Mean share_black1990 (%)",
    Mean_kfr_black_pooled_p25 = "Mean kfr_black_pooled_p25",
    Mean_kfr_white_pooled_p25 = "Mean kfr_white_pooled_p25",
    Mean_homeownership2010 = "Mean homeownership2010 (%)",
    Mean_vegetation = "Mean vegetation",
    Mean_extreme_heat = "Mean extreme_heat"
  ) %>%
  fmt_percent(
    columns = c(Percentage, Mean_share_black1990, Mean_homeownership2010),
    decimals = 2
  ) %>%
  fmt_number(
    columns = c(Mean_kfr_pooled_p25, Mean_kfr_black_pooled_p25, Mean_kfr_white_pooled_p25, Mean_vegetation, Mean_extreme_heat),
    decimals = 2
  )
gtsave(gt_table, filename = "tableQ9.png")

table_df$HOLC_grade <- factor(table_df$HOLC_grade, levels = c(1, 2, 3, 4, NA))

## Plot 1: Mean kfr_pooled_pooled_p25
ggplot(table_df, aes(x=HOLC_grade, y=Mean_kfr_pooled_p25, fill=HOLC_grade)) +
  geom_bar(stat="identity", show.legend = FALSE, width=.6) +
  scale_fill_manual(values=c("1" = "lightblue", "2" = "steelblue", "3" = "grey", "4" = "darkgrey", "NA" = "black")) +
  labs(y = "Mean kfr_pooled_pooled_p25", x = "HOLC Grade") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        plot.title = element_text(size = 9),
        axis.title = element_text(size = 9))
ggsave("Q9A.png", width = 8, height = 6, dpi = 300)

## Plot 2: Mean share_black1990
ggplot(table_df, aes(x=HOLC_grade, y=Mean_share_black1990,  fill=HOLC_grade)) +
  geom_bar(stat="identity", show.legend = FALSE, width=.6) +
  scale_fill_manual(values=c("1" = "lightblue", "2" = "steelblue", "3" = "grey", "4" = "darkgrey", "NA" = "black")) +
  labs(y = "Mean share_black1990, ", x = "HOLC Grade") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        plot.title = element_text(size = 9),
        axis.title = element_text(size = 9))
ggsave("Q9B.png", width = 8, height = 6, dpi = 300)

##Plot 3: kfr_black_pooled_p25, and kfr_white_pooled_p25
# Reshape the data for faceting
long_df <- table_df %>%
  select(HOLC_grade, Mean_kfr_black_pooled_p25, Mean_kfr_white_pooled_p25) %>%
  pivot_longer(-HOLC_grade, names_to = "Variable", values_to = "Value")

# Plot with faceting
ggplot(long_df, aes(x=HOLC_grade, y=Value, fill=HOLC_grade)) +
  geom_bar(stat="identity", show.legend = FALSE, width=.6) +
  facet_wrap(~Variable, scales = "free_y") +
  scale_fill_manual(values=c("1" = "lightblue", "2" = "steelblue", "3" = "grey", "4" = "darkgrey", "NA" = "black")) +
  labs(y = "Value", x = "HOLC Grade") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        plot.title = element_text(size = 9),
        axis.title = element_text(size = 9))
ggsave("Q9C.png", width = 8, height = 6, dpi = 300)

## Plot 4: homeownership2010, vegetation, and extreme_heat
# Reshape the data for faceting
env_df <- table_df %>%
  select(HOLC_grade, Mean_homeownership2010, Mean_vegetation, Mean_extreme_heat) %>%
  pivot_longer(-HOLC_grade, names_to = "Variable", values_to = "Value")

# Plot with faceting
ggplot(env_df, aes(x=HOLC_grade, y=Value, fill=HOLC_grade)) +
  geom_bar(stat="identity", show.legend = FALSE, width=.6) +
  facet_wrap(~Variable, scales = "free_y") +
  scale_fill_manual(values=c("1" = "lightblue", "2" = "steelblue", "3" = "grey", "4" = "darkgrey", "NA" = "black")) +
  labs(y = "Value", x = "HOLC Grade") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = NA),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        plot.title = element_text(size = 9),
        axis.title = element_text(size = 9))
ggsave("Q9D.png", width = 8, height = 6, dpi = 300)

##Question 10
# Part A
#subsetting fraction of Census Tracts above threshold
fractions <- atlas %>%
  select(tract, pm25_1982, pm25_1990, pm25_2010) %>%
  summarise_all(list(~mean(. > 12, na.rm = TRUE)))
print(fractions)

#Part B
#draw binned scatter plot with linear fit
ggplot(atlas, aes(x = pm25_1990, y = kfr_pooled_pooled_p25)) +
  stat_summary_bin(fun = "mean", bins = 20, geom = "line", color = "grey") +  # Connected line for binned means
  stat_summary_bin(fun = "mean", bins = 20, geom = "point", color = "green") +  # Points for binned means
  stat_smooth(method = "lm", se = FALSE, color = "dark grey") +  # Linear best fit line
  labs(
    x = 'PM2.5 Levels in 1990 (μg/m3)',
    y = 'Mean Child Income Rank (kfr_pooled_pooled_p25)',
    title = 'Binned Scatter Plot with Best Fit Line'
  ) +
  theme_minimal()
#Save graph
ggsave("Q10B.png", width = 8, height = 6, dpi = 300)

# Part C
#collapsing to county level
county_level_data <- atlas %>%
  group_by(state, county) %>%
  summarize(
    mean_kfr_pooled_pooled_p25 = mean(kfr_pooled_pooled_p25, na.rm = TRUE),
    mean_pm25_1990 = mean(pm25_1990, na.rm = TRUE)
  ) %>%
  ungroup()

# Compute the correlation coefficient
correlation_coefficient1 <- cor(
  county_level_data$mean_kfr_pooled_pooled_p25, 
  county_level_data$mean_pm25_1990,
  use = "complete.obs"  
)

print(correlation_coefficient1)

#Part D
correlation_coefficient2 <- cor(atlas$kfr_pooled_pooled_p25, atlas$pm25_1990, use = "complete.obs")

print(correlation_coefficient2)

##Question 11
#generating needed variable 
atlas$mobility_diff = atlas$kir_white_male_p25 - atlas$kir_black_male_p25

#fraction where white men have higher mobility
higher_mobility_white_men_fraction <- mean(atlas$mobility_diff > 0, na.rm = TRUE)
print(higher_mobility_white_men_fraction)

#plotting the relationship between mobility difference and poverty rate
ggplot(atlas, aes(x = poor_share1990, y = mobility_diff)) +
  geom_point(alpha = 0.1) +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(x = "Poverty Rate in 1990", y = "Mobility Difference (White - Black Men)", 
       title = "Relationship Between Mobility Difference and Poverty Rate in 1990") +
  theme_minimal()

ggsave("Q11C.png", width = 8, height = 6, dpi = 300)

## Question 12
zip_tracts_xwalk <- read_dta("zip_tracts_xwalk.dta")
social_capital <- read_dta("Social_capital_zip.dta")

# Merge atlas data set with the cross-walk data set
merged_data <- merge(atlas, zip_tracts_xwalk, by = c("tract", "county", "state"), all.x = TRUE)

# Collapse the data to get the weighted average of kfr_pooled_pooled_p25 for each zip code
zip_mobility <- merged_data %>%
  group_by(zip) %>%
  summarize(weighted_avg_kfr = weighted.mean(kfr_pooled_pooled_p25, zpoppct, na.rm = TRUE)) %>%
  ungroup()

# Merge the mobility data set with the social capital data set
final_data <- merge(zip_mobility, social_capital, by = "zip", all.x = TRUE)

# Calculate correlation coefficients
cor_kfr_ec <- cor(final_data$weighted_avg_kfr, final_data$ec_zip, use = "complete.obs")
cor_kfr_clustering <- cor(final_data$weighted_avg_kfr, final_data$clustering_zip, use = "complete.obs")
cor_kfr_civic <- cor(final_data$weighted_avg_kfr, final_data$civic_organizations_zip, use = "complete.obs")

# Print the correlation coefficients
print(cor_kfr_ec)
print(cor_kfr_clustering)
print(cor_kfr_civic)

