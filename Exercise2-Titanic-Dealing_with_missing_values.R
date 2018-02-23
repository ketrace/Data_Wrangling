#load packages
library(dplyr)
library(tidyr)

#read csv into data frame
titanic_clean <- read.csv("titanic_original.csv")

#replace missing values in embarked column with S for Southampton
titanic_clean$embarked <- gsub(pattern = " ", "S", titanic_clean$embarked)

#calculate mean passenger age, removing NA values
avg_age <- mean(titanic_clean$age, na.rm = TRUE)

#search age column for NA values and replace with average age
titanic_clean <- titanic_clean %>%
  mutate(age = ifelse(is.na(age), avg_age, age))

#search boat column for missing values and replace with NA 

titanic_clean <- titanic_clean %>%
  mutate(boat = ifelse(boat == "", NA, boat))

#create new column for has cabin number (indicative of survival)
titanic_clean <- titanic_clean %>%
  mutate(has_cabin_number = ifelse(cabin != "", 1, 0))

write.csv(titanic_clean, "titanic_clean.csv")