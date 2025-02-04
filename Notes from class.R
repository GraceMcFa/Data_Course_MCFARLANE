## load 'mtcars' dataset 
datasets::mtcars
View(mtcars)
## 1. What type of object is this? 

## 2. Find cars with an mpg greater than 20 and 4 cyl, 
## then save them to a new object. 
mtcars[]
data(mtcars)
dim(mtcars) #32 11 

data(mtcars) # reset everything (restore mtcars)

file <- read.csv()
file_1 <- 

  # option 1: two steps 
my_mtcars <- mtcars #to not mess up the orginial data
my_mtcars <- my_mtcars[my_mtcars$mpg > 20, ]
my_mtcars <- my_mtcars[my_mtcars$cyl == 4, ]

# option 2: one step 
my_mtccars <- my_mtcars[my_mtcars$mpg > 20 & my_mtcars$cyl == 4, ]
View(my_mtcars)


## 3. Convert mpg to a character data type.
as.character(my_mtcars$mpg)
my_mtcars$mpg <- as.character(my_mtcars$mpg)
my_mtcars$new_col <- as.numeric(my_mtcars$mpg)

my_mtcars... 

## 4. Convert the entire data frame to character data type. 
str(my_mtcars)
names(my_mtcars)

as.character(my_mtcars)
dat1 <- as.character(my_mtcars)
class(dat1)

my_mtcars$mpg... 


for (col in names(my_mtcars)) {
  #print(col)
  my_mtcars[, col] <- as.character(my_mtcars[, col])
}

apply(my_mtcars[1:3, ], 2, as.character)
apply(my_mtcars, 2, as.character)
apply(my_mtcars, 2, mean)
# (they have different functions) (row, col, function)
# apply functions over array margins 
# matrix, or an array? 
new_input <- mtcars[1:3, ]
new_dat_w_new_ipubt <- apply(new_input, 2, as.character)
#lapply
#sapply
#vapply
class(new_dat_w_new_ipubt)
new_dat <- as.data.frame(new_dat_w_new_ipubt) #converting back to a data frame
class(new_dat)

read.csv()
write.csv(new_dat, path ='class_practice_28Jan25.csv')



install.packages('qrcode')
install.packages('tidyverse')
Yes
library(tidyverse)
mtcars$mpg %>%
  mean()
mean(mtcars$mpg) #these two things are the same comand 


## lad 'mtcars' dataset 
datasets::mtcars
View(mtcars)
data = mtcars 
data_test = data[data$wt > 3 & data$cyl == 8,]
## 1. Find cars with an wt greater than 3 and 8 cyl, 
## then save them to a new object. 
data[data$wt > 3 & data$cyl == 8,]
data_test = data[data$wt > 3 & data$cyl == 8,]

## 2. Calculate the average mpg of the new object 
mean(data_test$mpg)

# 3. Create a new numeric vector object named "hp.cyl" 
# wich is calculated by dividing hp by cyl. 
names(data_test)
data_test$hp.cyl <- data_test$hp/data_test$cyl

# 4. Save this as a .csv file on your laptop. and open it. 
write.csv(data_test, 'data_test.csv')
write.csv(data_test, '/Users/gracemcfarlane/Desktop/Data_Course_MCFARLANE/data_test.csv')

# need to reload library(tidyverse) everytime you open R up again 
library(tidyverse)
mtcars %>% 
  filter(wt > 3 & cyl == 8) %>%
  mutate(hp.cyl = hp/cyl) %>%
  write_csv('test.csv')

mean(mtcars$mpg) # opt 1

mtcars$mpg %>% #pipe
  mean()


#control shift m ???
## 
install.packages('palmerpenguins')
library(palmerpenguins)

penguins 
view(penguins)

penguins %>% 
  names()

names(penguins)

dat_bill <- penguins %>% 
  filter(bill_length_mm > 40 & sex == 'female') %>% 
  View()

mean(dat_bill$body_mass_g) #to find the average 

dat_bill$body_mass_g %>% mean()

#function to calculate the average mean()
penguins %>% 
  filter(bill_length_mm > 40 & sex == 'female') %>% 
  pluck('body_mass_g') %>% #select a cyl
  mean()

filtered_penguins <- penguins %>% 
  filter(bill_length_mm > 40 & sex == 'female') %>% 

penguins %>% 
  filter(bill_length_mm > 40 & sex == 'female') %>% 
  group_by(species) %>% 
  summarize(mean_body_mass = mean(body_mass_g))

## summarize(mean_body_mass #cyl name = mean(body_mass_g) #what you are naming the new cyl)

penguins %>% 
  filter(bill_length_mm > 40 & sex == 'female') %>% 
  group_by(species, island) %>% #category you want 
  summarize(mean_body_mass = mean(body_mass_g), #what you want 
            max_body_mass = max(body_mass_g),
            count = n()) %>%
arrange(mean_body_mass)

#save object to .csv 
#write.csv(filename, 'path') #we can ignore file name here, it will know 
#write_csv('penguins_1.csv')
penguins %>% 
  filter(bill_length_mm > 40 & sex == 'female') %>% 
  group_by(species, island) %>% #category you want 
  summarize(mean_body_mass = mean(body_mass_g), #what you want 
            max_body_mass = max(body_mass_g),
            count = n()) %>%
  #write_csv('penguins_1.csv')

## %>% comand, shift, m to get this symbol 

mean(na.rm = T) #troubleshooting 

## 
vec <- c(1:10)

for (i in 1:10) { 
  print(i)
}

View(iris)

#change col to character 
iris$Sepal.Length <- as.character(iris$Sepal.Length)

names(iris)

for (col in names(iris)) { 
  iris[, col] >- as.character(iris [, col])
  }

iris[, 1]

str(iris)


#2/4 class note
## 1.1 find the fattie penguins (body_mass > 5000)
install.packages('tidyverse')
install.packages('palmerpenguins')
library(palmerpenguins)

View(penguins)
penguins %>% 
  filter(body_mass_g > 5000)

fat_pen <- penguins %>% 
  filter(body_mass_g > 5000) %>% 
  group_by(sex) %>% 
  summarise(count = n(), 
            fattest = max(body_mass_g))
max(penguins$body_mass_g, na.rm = T)
# or if using tidyverse 
penguins$body_mass_g %>% 
  max(na.rm = T)


## 1.2 count how many are male and how many are female 



## 1.3 return the max body mass for male and female 


## 2.1 add new column to penguins to dataset that says whether they're fat 
dat_peng <- penguins 

dat_peng$fat_state <- dat_peng$body_mass_g > 5000 
View(dat_peng)

penguins %>%  
  filter(body_mass_g > 5000) %>% #do not always need a filter 
  mutate(fat_state = body_mass_g > 5000) %>% 
  View()
#how to remove a column 
new_dat <- dat_peng[, c(1:8)]
View(new_dat)

penguins %>% 
  mutate(fat_or_not = case_when(body_mass_g > 5000 ~ 'fat',
                                body_mass_g <= 5000 & body_mass_g > 3000 ~ 'medium',
                                body_mass_g <= 3000 ~ 'skinny')) %>% 
  View()

## if condition is TRUE ~ do this 

Dat_peng <- penguins %>% 
  mutate(fat_or_not = case_when(body_mass_g > 5000 ~ 'fat',
                                body_mass_g <= 5000 & body_mass_g > 3000 ~ 'medium',
                                body_mass_g <= 3000 ~ 'skinny'))
plot(dat_peng$bill_length_mm, dat_peng$body_mass_g)

library(ggplot2)

ggplot(data = dat_peng)

dat_peng %>% 
  ggplot(aes(x = bill_length_mm,
             y = body_mass_g))



#aes = aesthetic 





## 
## Ghibli color 














