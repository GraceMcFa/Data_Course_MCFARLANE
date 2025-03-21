#Assignment_6_ThisOne#
library(tidyverse)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)

dat <- read_csv("Data/BioLog_Plate_Data.csv")

# Clean data into tidy (long) form
dat_clean <- dat %>% 
  pivot_longer(cols = starts_with('Hr_'),
               names_to = 'Time',
               values_to = 'Absorbance') %>% 
  mutate(time = as.numeric(str_remove(Time, 'Hr_')))  

# Create a new column 
dat_clean_v2 <- dat_clean %>% 
  mutate(type = case_when(
    `Sample ID` == "Clear_Creek" ~ 'Water',  
    `Sample ID` == "Waste_Water" ~ 'Water',
    `Sample ID` %in% c('Soil_1', 'Soil_2') ~ 'Soil',
    TRUE ~ 'Other'
  ))

# Filter for dilution 0.1 for the first plot
dat_plot <- dat_clean_v2 %>% 
  filter(Dilution == 0.1)

# Create plot for dilution 0.1
dat_plot %>% 
  ggplot(aes(x = time, y = Absorbance, color = type)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~Substrate) +
  labs(title = "Just dilution 0.1",
       x = "Time",
       y = "Absorbance",
       color = "Type") +
  theme_minimal()

# Filter for just Itaconic Acid data
Itaconic_Acid <- dat_clean_v2 %>% 
  filter(Substrate == 'Itaconic Acid')

# Calculate mean absorbance for each sample/dilution/time combination
mean_abs <- Itaconic_Acid %>% 
  group_by(`Sample ID`, Dilution, time) %>%  
  summarise(mean_absorbance = mean(Absorbance), .groups = "drop")

#.groups = "drop" helps summarize the function so there hopefully will be no warnings

# Create animated plot
mean_abs %>% 
  ggplot(aes(x = time, y = mean_absorbance, color = `Sample ID`)) +  
  geom_line() +
  facet_wrap(~ Dilution) +
  labs(x = 'Time',
       y = 'Mean absorbance',
       color = 'Sample ID') + 
  theme_minimal() +
  transition_reveal(time)
