#Loading the required packages. 

library(tidyverse) #For core importation and data wrangling functions
library(lubridate) #For time and date functions
library(ggplot2)   #For data visualizations
getwd()            #Spells out my working directory
library(lubridate)
install.packages("rmarkdown")
installed.packages("knitr")
library(knitr)
library(rmarkdown)


#Importing the data into different data frames
library(readr)
CovidDeaths <- read_csv("C:/Users/Bright/Downloads/Compressed/covid/CovidDeaths.csv", 
                        col_types = cols(date = col_date(format = "%m/%d/%Y")))
View(CovidDeaths)

library(readr)
CovidVaccinations <- read_csv("C:/Users/Bright/Downloads/Compressed/covid/CovidVaccinations.csv", 
                              col_types = cols(date = col_date(format = "%m/%d/%Y")))
View(CovidVaccinations)

#EXPLORING THE DATA
#Quick Exploration

colnames(CovidVaccinations)
unique(CovidDeaths$weekly_hosp_admissions_per_million)
unique(CovidVaccinations$extreme_poverty)

head(CovidDeaths)
head(CovidVaccinations)
tail(CovidDeaths)
tail(CovidVaccinations)

glimpse(CovidDeaths)
glimpse(CovidVaccinations)

length(CovidDeaths$iso_code)
length(CovidDeaths$iso_code)
length(CovidVaccinations)
length(unique(CovidVaccinations$location)) 
#Returns the unique number of locations in the CovidVaccinations data frame

table(CovidVaccinations$continent)
table(CovidDeaths$continent)
table(CovidVaccinations$location) 
#This has exposed certain dislocated observations in the location variable. 
#e.g. Asia and Africa in the variable representing location (country). 


#EXPLORING THE COVIDDEATHS DATA FRAME ONLY...
boxplot(CovidDeaaths$reproduction_rate)
#Spotting an outlier from the above function. 

#Exploring all variables to find patterns, null values, etc. 
CovidDeaths %>% 
  select(total_cases_per_million) %>% 
  filter(!complete.cases(.)) %>% 
View()
#All relevant variables of the CovidDeaths data frame will be run through 
#the above code to find the number of rows (for that variable) with nulls.

#Some summary statistics 
sum(CovidDeaths$new_deaths, na.rm = TRUE)
summary(CovidDeaths$new_deaths, na.rm = TRUE)
max(CovidDeaths$total_deaths, na.rm = TRUE)

#EXPLORING THE COVIDVACCINATIONS DATA FRAME ONLY...
View(CovidVaccinations)

#Exploring all variables to find patterns, null values, etc. 
CovidVaccinations %>% 
  select(people_fully_vaccinated) %>% 
  filter(!complete.cases(.)) %>% 
  View() 
#All relevant variables of the CovidVaccinations data frame will be run through 
#the above code to find the number of rows (for that variable) with nulls.  

#Some summary statistics
sum(CovidVaccinations$new_vaccinations, na.rm = TRUE)
summary(CovidVaccinations$total_vaccinations, na.rm = TRUE)
max(CovidVaccinations$total_vaccinations, na.rm = TRUE)
mean(CovidVaccinations$female_smokers, na.rm = TRUE)

#The codes below dive deeper into the exploration phase of the project
CovidDeaths %>%
  select(total_deaths, location) %>%
  filter(complete.cases(.)) %>%
  group_by(location) %>%
  summarise(tds = max(total_deaths)) %>%
  arrange(-tds) %>% 
  View()
#The code above groups the total deaths by location in descending order by total
#deaths

CovidVaccinations %>% 
  select(people_fully_vaccinated, location) %>% 
  filter(complete.cases(.)) %>% 
  group_by(location) %>% 
  summarise(people_vaccinated = max(people_fully_vaccinated)) %>% 
  arrange(-people_vaccinated) %>%  
  View()
#The code above ranks the people vaccinated in descending order grouped by their
#locations

CovidVaccinations %>% 
  select(handwashing_facilities, location) %>% 
  filter(complete.cases(.)) %>% 
  group_by(location) %>% 
  summarise(max_hand_wash = mean(handwashing_facilities)) %>% 
  arrange(-max_hand_wash) %>% 
  View()
#The code above ranks in descending order the average number of hand washing
#facilities grouped by location


#FOLLOW TO THE DATA CLEANING and MANIPULATION FILE TO CONTINUE READING THIS
#STORY

