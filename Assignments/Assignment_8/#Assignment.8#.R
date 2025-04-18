#Assignment.8#
library(ggplot2)
library(dplyr)
library(tidyverse)
# 1. Load the data####
mushroom_data <- read.csv("mushroom_growth.csv")
str(mushroom_data)
summary(mushroom_data)
view(mushroom_data)

# 2. Create exploratory plots####
# Plot 1: Relationship between Temperature and GrowthRate
ggplot(mushroom_data, aes(x = Temperature, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Growth Rate vs Temperature",
       x = "Temperature (°C)",
       y = "Growth Rate (mm/day)")

# Plot 2: Relationship between Humidity and GrowthRate
ggplot(mushroom_data, aes(x = Humidity, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Growth Rate vs Humidity",
       x = "Humidity (%)",
       y = "Growth Rate (mm/day)")

# Plot 3: Relationship between Light and GrowthRate
ggplot(mushroom_data, aes(x = Light, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Growth Rate vs Light",
       x = "Light Intensity (lux)",
       y = "Growth Rate (mm/day)")

# Plot 4: Relationship between Nitrogen and GrowthRate
ggplot(mushroom_data, aes(x = Nitrogen, y = GrowthRate)) +
  geom_boxplot() +
  labs(title = "Growth Rate by Nitrogen",
       x = "Nitrogen",
       y = "Growth Rate (mm/day)")

# Plot 5: Relationship between Species and GrowthRate
ggplot(mushroom_data, aes(x = Species, y = GrowthRate)) +
  geom_boxplot() +
  labs(title = "Growth Rate by Species",
       x = "Species",
       y = "Growth Rate (mm/day)")

# Plot 6: Plot to check for interactions between Temperature and Humidity
ggplot(mushroom_data, aes(x = Temperature, y = GrowthRate, color = Humidity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Growth Rate vs Temperature by Humidity",
       x = "Temperature (°C)",
       y = "Growth Rate (mm/day)")



# 3. Define at least 4 models for the dependent variable GrowthRate####
# Model 1: Basic continuous variables
model1 <- lm(GrowthRate ~ Light + Nitrogen + Temperature, data = mushroom_data)

# Model 2: Add Species
model2 <- lm(GrowthRate ~ Light + Nitrogen + Temperature + Species, data = mushroom_data)

# Model 3: Add Humidity and interaction
model3 <- lm(GrowthRate ~ Light * Nitrogen + Temperature + Humidity, data = mushroom_data)

# Model 4: Full model with all predictors and interactions
model4 <- lm(GrowthRate ~ Light * Nitrogen + Temperature + Humidity + Species, data = mushroom_data)


# 4. Calculate the mean squared error for each model####
# calclate mse model 1
calculate_mse <- function(model1, data) {
  predictions <- predict(model1, data)
  actual <- mushroom_data$GrowthRate
  mse <- mean((actual - predictions)^2)
  return(mse)
}
mse_model1 <- calculate_mse(model1, mushroom_data)
mse_model1

# calclate mse model 2
calculate_mse <- function(model2, data) {
  predictions <- predict(model2, data)
  actual <- mushroom_data$GrowthRate
  mse <- mean((actual - predictions)^2)
  return(mse)
}
mse_model2 <- calculate_mse(model2, mushroom_data)
mse_model2

# calclate mse model 3
calculate_mse <- function(model3, data) {
  predictions <- predict(model3, data)
  actual <- mushroom_data$GrowthRate
  mse <- mean((actual - predictions)^2)
  return(mse)
}
mse_model3 <- calculate_mse(model3, mushroom_data)
mse_model3

# calclate mse model 4
calculate_mse <- function(model4, data) {
  predictions <- predict(model4, data)
  actual <- mushroom_data$GrowthRate
  mse <- mean((actual - predictions)^2)
  return(mse)
}
mse_model4 <- calculate_mse(model4, mushroom_data)
mse_model4

#5. Find the best model####
mse_comparison <- data.frame(
  Model = c("Model 1 (All predictors)", 
            "Model 2 (Polynomial terms)", 
            "Model 3 (Interactions)", 
            "Model 4 (Simplified)"),
  MSE = c(mse_model1, mse_model2, mse_model3, mse_model4)
)

mse_comparison <- mse_comparison[order(mse_comparison$MSE), ]
print(mse_comparison)

best_model <- model4

# 6. Add predictions based on new hypothetical values####
mushroom_data$Humidity <- factor(mushroom_data$Humidity)
mushroom_data$Species <- factor(mushroom_data$Species)

humidity_levels <- levels(mushroom_data$Humidity)
species_levels <- levels(mushroom_data$Species)

hypothetical_data <- data.frame(
  Light = c(10, 20, 30),
  Nitrogen = c(5, 10, 15),
  Temperature = c(20, 25, 30),
  Humidity = factor(c("Low", "Low", "High"), levels = levels(mushroom_data$Humidity)),
  Species = factor(c("P.ostreotus", "P.ostreotus", "P.ostreotus"), levels = levels(mushroom_data$Species))
)

hypothetical_data$PredictedGrowthRate <- predict(model4, newdata = hypothetical_data)

print(hypothetical_data)


# 7. Plot the predictions alongside the real data####
ggplot() +
  geom_point(data = mushroom_data, aes(x = Light, y = GrowthRate, color = "Actual"), alpha = 0.6) +
  geom_point(data = hypothetical_data, aes(x = Light, y = PredictedGrowthRate, color = "Predicted"), size = 3, shape = 17) +
  facet_wrap(~ Humidity) +
  scale_color_manual(values = c("Actual" = "steelblue", "Predicted" = "pink")) +
  labs(
    title = "Actual vs Predicted Mushroom Growth Rates",
    x = "Light",
    y = "Growth Rate",
    color = "Data Type"
  ) +
  theme_minimal()



