#Exam.2#
#Load the libraries####
library(tidyverse)
library(janitor)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
# 1. Load the UNICEF data set.csv####
unicef_data <- read_csv("unicef-u5mr.csv")
head(unicef_data)
str(unicef_data)
view(unicef_data)

# 2. tidy up the data#### 
unicef_tidy <- unicef_data %>%
  pivot_longer(
    cols = starts_with("U5MR."),
    names_to = "Year",
    values_to = "U5MR"
  ) %>%
  mutate(Year = as.numeric(str_replace(Year, "U5MR.", "")))
unicef_tidy_filtered <- unicef_tidy %>%
  filter(!is.na(U5MR))

# 3. Plot each country’s U5MR over time####
plot1 <- ggplot(unicef_tidy_filtered, aes(x = Year, y = U5MR, group = CountryName)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~ Continent) +
  labs(
    title = "Under-5 Mortality Rate by Country Over Time",
    subtitle = "Faceted by Continent",
    x = "Year",
    y = "Under-5 Mortality Rate (deaths per 1000 live births)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold")
  )
plot1
# 4. Save this plot####
ggsave("MCFARLANE_Plot_1.png", plot1, width = 12, height = 8, dpi = 300)

# 5. Create another plot showing mean U5MR by continent over time####
continent_means <- unicef_tidy_filtered %>%
  group_by(Continent, Year) %>%
  summarize(mean_U5MR = mean(U5MR, na.rm = TRUE), .groups = "drop")

plot2 <- ggplot(continent_means, aes(x = Year, y = mean_U5MR, color = Continent, group = Continent)) +
  geom_line(size = 1.2) +
  labs(
    title = "Mean Under-5 Mortality Rate by Continent Over Time",
    x = "Year",
    y = "Mean Under-5 Mortality Rate (deaths per 1000 live births)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  ) +
  scale_color_brewer(palette = "Set1")
plot2
# 6. Save that plot####
ggsave("MCFARLANE_Plot_2.png", plot2, width = 10, height = 6, dpi = 300)

# 7. Create three models of U5MR####
mod1 <- lm(U5MR ~ Year, data = unicef_tidy_filtered)
mod2 <- lm(U5MR ~ Year + Continent, data = unicef_tidy_filtered)
mod3 <- lm(U5MR ~ Year * Continent, data = unicef_tidy_filtered)

# 8. Compare the three models with respect to their performance####
model_comparison <- data.frame(
  Model = c("mod1", "mod2", "mod3"),
  R_squared = c(summary(mod1)$r.squared, summary(mod2)$r.squared, summary(mod3)$r.squared),
  Adj_R_squared = c(summary(mod1)$adj.r.squared, summary(mod2)$adj.r.squared, summary(mod3)$adj.r.squared),
  AIC = c(AIC(mod1), AIC(mod2), AIC(mod3)),
  BIC = c(BIC(mod1), BIC(mod2), BIC(mod3))
)
#I can't lie this part was rough, and took me a lot of time to figure out

# Print model comparison
print(model_comparison)

# Compare models with ANOVA for fun
anova_comp1_2 <- anova(mod1, mod2)
anova_comp2_3 <- anova(mod2, mod3)
print("Comparison between mod1 and mod2:")
print(anova_comp1_2)
print("Comparison between mod2 and mod3:")
print(anova_comp2_3)

# Comment on model selection#
# Based on the information above, I would probably choose model 3 
# This model accounts for both the overall trend and how that trend changes across continents

# 9. Plot the 3 models' predictions####
# Create a prediction dataset
pred_years <- seq(min(unicef_tidy_filtered$Year), max(unicef_tidy_filtered$Year), by = 1)
continents <- unique(unicef_tidy_filtered$Continent)

# Create a grid of all combinations
pred_data <- expand.grid(Year = pred_years, Continent = continents)

# Generate predictions for each model
pred_data$mod1_pred <- predict(mod1, newdata = pred_data)
pred_data$mod2_pred <- predict(mod2, newdata = pred_data)
pred_data$mod3_pred <- predict(mod3, newdata = pred_data)

# Convert to long format for plotting
pred_long <- pred_data %>%
  pivot_longer(
    cols = contains("_pred"),
    names_to = "Model",
    values_to = "Predicted_U5MR"
  ) %>%
  mutate(Model = factor(Model, levels = c("mod1_pred", "mod2_pred", "mod3_pred"),
                        labels = c("Year Only", "Year + Continent", "Year * Continent")))

# Sample points from data for clarification 
set.seed(123)
sampled_points <- unicef_tidy_filtered %>%
  group_by(Continent) %>%
  sample_frac(0.05) %>%
  ungroup()

# Plot the predictions
plot3 <- ggplot() +
  geom_point(data = sampled_points, aes(x = Year, y = U5MR, color = Continent), alpha = 0.2, size = 1) +
  geom_line(data = pred_long, aes(x = Year, y = Predicted_U5MR, color = Continent, linetype = Model), size = 1) +
  facet_wrap(~ Continent) +
  labs(
    title = "Model Predictions of Under-5 Mortality Rate",
    subtitle = "Comparing Three Models",
    x = "Year",
    y = "Under-5 Mortality Rate (deaths per 1000 live births)",
    linetype = "Model"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold")
  ) +
  scale_color_brewer(palette = "Set1")
plot3

# Save the model comparison plot 
ggsave("MCFARLANE_Plot_3.png", plot3, width = 14, height = 10, dpi = 300)

#10.Bonus####
# 10. BONUS - Predict Ecuador 2020
ecuador_data <- unicef_tidy_filtered %>%
  filter(CountryName == "Ecuador")

ecuador_continent <- ecuador_data$Continent[1]
cat("\nEcuador is in", ecuador_continent, "\n")

ecuador_2020 <- data.frame(
  CountryName = "Ecuador",
  Continent = ecuador_continent,
  Year = 2020
)

ecuador_pred_mod3 <- predict(mod3, newdata = ecuador_2020)

actual_value <- 13
difference <- ecuador_pred_mod3 - actual_value
percent_error <- (abs(difference) / actual_value) * 100


#extra#
ecuador_only <- unicef_tidy_filtered %>%
  filter(CountryName == "Ecuador")
# Plot Ecuador's data 
ggplot(ecuador_only, aes(x = Year, y = U5MR)) +
  geom_point() +
  geom_line() +
  labs(title = "Ecuador's U5MR Over Time")

pred_years <- seq(min(ecuador_only$Year), 2020, by = 1)
pred_data <- data.frame(Year = pred_years)

plot4 <- ggplot() +
  # Actual data points
  geom_point(data = ecuador_only, aes(x = Year, y = U5MR), color = "black") +
  geom_line(data = ecuador_only, aes(x = Year, y = U5MR), color = "black", linetype = "dashed") +
  # Model predictions
  geom_line(data = pred_long, aes(x = Year, y = Predicted_U5MR, color = Model), size = 1) +
  # Actual 2020 point
  geom_point(aes(x = 2020, y = actual_value), color = "red", size = 3) +
  geom_text(aes(x = 2020, y = actual_value, label = "Actual 2020"), color = "red", vjust = -1) +
  labs(
    title = "Ecuador's U5MR: Actual vs Predicted",
    subtitle = "Comparing Different Models for 2020 Prediction",
    x = "Year",
    y = "Under-5 Mortality Rate (deaths per 1000 live births)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = seq(1950, 2020, by = 10), limits = c(NA, 2022))

plot4

ggsave("MCFARLANE_Plot_Ecuador_Prediction.png", plot4, width = 10, height = 6, dpi = 300)
