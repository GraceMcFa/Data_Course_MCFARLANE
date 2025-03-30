#!Assignment.7.ThisOne#
# Load required libraries####
library(tidyverse)
library(here)
#open data####
utah_religions <- read.csv("Utah_Religions_by_County.csv")
# view data 
head(utah_religions)
str(utah_religions)
view(utah_religions)
# Clean data#### 
#Step.1
glimpse(utah_religions) 
#Step.2
utah_tidy <- utah_religions %>% 
  pivot_longer(
    cols = -c(County, Pop_2010), # All columns except County and Population
    names_to = "Religion",
    values_to = "Proportion"
  )
#Step.3
head(utah_tidy)
summary(utah_tidy)
#Figures####
# Figure 1: Religious proportions across counties
Figure.1 <- ggplot(utah_tidy, aes(x = County, y = Proportion, fill = Religion)) +
  geom_bar(stat = "identity", position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Religious Proportions by County",
       x = "County", 
       y = "Proportion")
Figure.1

# Figure 2: Distribution of each religion across counties
Figure.2 <- ggplot(utah_tidy, aes(x = Religion, y = Proportion, color = Religion)) +
  geom_boxplot() +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Religious Proportions",
       x = "Religion", 
       y = "Proportion")
Figure.2

# Figure 3: Relationship between county population and proportions
Figure.3 <- ggplot(utah_tidy, aes(x = Pop_2010, y = Proportion, color = Religion)) +
  geom_point() +
  facet_wrap(~Religion) +
  scale_x_log10() +  # Log scale for population to handle skew
  labs(title = "Religious Proportion vs. County Population",
       x = "Population 2010 (log scale)", 
       y = "Proportion")
Figure.3

#Questions####
# Question 1: "Does population of a county correlate with the proportion of 
# any specific religious group in that county?"
correlations_with_pop <- utah_tidy %>%
  group_by(Religion) %>%
  summarize(correlation = cor(Pop_2010, Proportion, use = "complete.obs")) %>%
  arrange(desc(abs(correlation)))
# Results
correlations_with_pop

ggplot(correlations_with_pop, aes(x = reorder(Religion, -abs(correlation)), y = correlation, color = Religion)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")+
  labs(title = "Correlation between Population and Religious Proportion",
       x = "Religion", 
       y = "Correlation Coefficient")


# Question 2: "Does proportion of any specific religion in a given county 
# correlate with the proportion of non-religious people?"
nonreligious_data <- utah_tidy %>%
  filter(Religion == "Non.Religious") %>% 
  select(County, NonReligious_Prop = Proportion)

religion_vs_nonreligious <- utah_tidy %>%
  filter(Religion != "Non.Religious") %>%  
  left_join(nonreligious_data, by = "County")

correlations_with_nonreligious <- religion_vs_nonreligious %>%
  group_by(Religion) %>%
  summarize(correlation = cor(Proportion, NonReligious_Prop, use = "complete.obs")) %>%
  arrange(desc(abs(correlation)))

# Results
correlations_with_nonreligious
# View
ggplot(correlations_with_nonreligious, aes(x = reorder(Religion, -abs(correlation)), y = correlation, color = Religion)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")+
  labs(title = "Correlation between Religious Proportion and Non-religious Proportion",
       x = "Religion", 
       y = "Correlation with Non-religious Proportion")


#Extra####
# Making a scatter plot because I was curious
# Top correlated religion with non-religious proportion
top_religion <- correlations_with_nonreligious %>% 
  slice(1) %>% #selects just the first row of that data frame
  pull(Religion) #extracts just the value from the Religion column from that single row

ggplot(filter(religion_vs_nonreligious, Religion == top_religion), 
       aes(x = Proportion, y = NonReligious_Prop)) +
  geom_point() +
  geom_smooth(method = "lm", color = "purple") +
  labs(title = paste("Correlation between", top_religion, "and Non-religious Proportion"),
       x = paste(top_religion, "Proportion"), 
       y = "Non-religious Proportion")

# Save the cleaned dataset
write_csv(utah_tidy, here("utah_religions_tidy.csv"))