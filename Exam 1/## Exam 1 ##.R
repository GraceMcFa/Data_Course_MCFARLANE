## Exam 1 ##

---
  output: 
  html_document:
  toc: yes
toc_float:
  collapsed: false
pagetitle: Exam_1
---
  
  # Skills Test 1 (the real thing)
  
  ____

# Setup

Do a fresh "git pull" to get the skills test files.
The files you just got from your "git pull" are:
  
+ README.md (this text file with instructions)
+ README.html (fancy version of this file)
+ cleaned_covid_data.csv
+ prepare_data.R (the script I used to turn all the raw data files into cleaned_covid_data.csv - only for the curious)
+ data/ (directory containing all the raw data files - only for the curious)

# Data description

**cleaned_covid_data.csv is the main data file you will use for this skills test.**
  
  The columns in the cleaned_covid_data.csv file are as follows:
  
  | Column name              | Description                                                                   |
  | ------------------------ | ------------------------------------------------------------------------------|
  | "Province_State"         | State (or DC)                                                                 |
  | "Last_Update"            | Date of observation                                               |
  | "Confirmed"              | Cumulative number of confirmed COVID-19 cases as of the given date            |
  | "Deaths"                 | The date the DNA was originally extracted in the format YYYY-MM-DD            |
  | "Recovered"              | Total number of recovered cases as of the given date                          |
  | "Active"                 | Total number of active confirmed COVID-19 cases as of the given date          |
  | "Case_Fatality_Ratio"    | Percent of cases that resulted in death due to COVID-19                       |
  
  A glimpse of the data structure:
  ```{r echo=FALSE, message=FALSE, warning=FALSE}
library(tidyverse)
df <- read_csv("cleaned_covid_data.csv")
skimr::skim(df)
```

# YOUR TASKS:

#I.
  #Read the cleaned_covid_data.csv file into an R data frame. (20 pts)**
  
covid_data <- read.csv("cleaned_covid_data.csv")
view(covid_data)
head(covid_data)
str(covid_data)

#II.
  #Subset the data set to just show states that begin with "A" and save this as an object called A_states. (20 pts)
  
#Use the *tidyverse* suite of packages
#Selecting rows where the state starts with "A" is tricky (you can use the grepl() function or just a vector of those states if you prefer)

library(tidyverse)
A_states <- covid_data %>% 
  filter(Province_State %in% c("Alabama", "Alaska", "Arizona", "Arkansas"))
  view(A_states)



#III.
  #Create a plot _of that subset_ showing Deaths over time, with a separate facet for each state. (20 pts)**
#Create a scatterplot
#Add loess curves WITHOUT standard error shading
#Keep scales "free" in each facet
A_states %>% 
  ggplot(aes(x = Last_Update, y = Deaths, color = Province_State)) + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)+
  facet_wrap(~Province_State, scales = 'free')
  


##IV. (Back to the full dataset)
##Find the "peak" of Case_Fatality_Ratio for each state and save this as a new 
#data frame object called state_max_fatality_rate. (20 pts)
state_max_fatality_rate <- covid_data %>% 
  filter(!is.na(Case_Fatality_Ratio)) %>% 
  group_by(Province_State) %>% 
  summarise(Max_Case_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>% 
  arrange(desc(Max_Case_Fatality_Ratio))
view(state_max_fatality_rate)



#V.
#Use that new data frame from task IV to create another plot. (20 pts)
  #X-axis is Province_State
  #Y-axis is Maximum_Fatality_Ratio
  #bar plot
  #x-axis arranged in descending order, just like the data frame (make it a factor to accomplish this)
  #X-axis labels turned to 90 deg to be readable

state_max_fatality_rate %>% 
  ggplot(aes(x = Province_State, y = Max_Case_Fatality_Ratio)) +
  geom_bar(stat = "identity", fill = "pink") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Maximum Fatality Ratio by State",
       x = "Province/State",
       y = "Maximum Fatality Ratio")


#VI.(BONUS 10 pts)
#Using the FULL data set, plot cumulative deaths for the entire US over time
#You'll need to read ahead a bit and use the dplyr package functions group_by() and summarize() to accomplish this.
library(dplyr)
covid_deaths <- covid_data %>% 
  group_by(Last_Update) %>% 
  summarise(cumulative_death = sum(Deaths, na.rm = TRUE)) %>% 
  arrange(Last_Update)
view(covid_deaths)

covid_deaths %>% 
  ggplot(aes(x = Last_Update, y = cumulative_death))+
  geom_bar(stat = 'identity', color = 'darkgreen')+
  labs(title = 'US Cumulative Deaths Over Time', 
       x = 'Date', y = 'Cumulative Deaths')+
  theme_minimal()
