#FINDING AND ELIMINATING ODD VALUES 

table(CovidVaccinations$continent)
table(CovidDeaths$continent)

table(CovidVaccinations$location) 
table(CovidDeaths$location)
#This has exposed certain dislocated observations in the location variable. 
#e.g. Asia and Africa in the variable representing location (country). 

CovidVaccinations %>%
  select(location, iso_code) %>%
  filter(location %in% c("World",
                         "High income", 
                         "Upper middle income", 
                         "Europe", 
                         "North America", 
                         "Asia", 
                         "South America", 
                         "Lower middle income", 
                         "European Union")) %>%
  View()

#These are irrelevant observations in the location variable and they will be taken out.

CovidVaccinations <- CovidVaccinations[!(CovidVaccinations$location %in% c("World",
                                                                           "High income", 
                                                                           "Upper middle income", 
                                                                           "Europe", 
                                                                           "North America", 
                                                                           "Asia",
                                                                           "Africa",
                                                                           "Oceania", 
                                                                           "South America", 
                                                                           "Pitcairn", 
                                                                           "Reunion",
                                                                           "Lower middle income", 
                                                                           "South America", 
                                                                           "Western Sahara",
                                                                           "European Union", 
                                                                           "Low income")), ]


#Doing the same for the CovidDeaths Data frame. 

CovidDeaths <- CovidDeaths[!(CovidDeaths$location %in% c("World",
                                                         "High income", 
                                                         "Upper middle income", 
                                                         "Europe", 
                                                         "North America", 
                                                         "Asia",
                                                         "Africa",
                                                         "Oceania", 
                                                         "South America", 
                                                         "Pitcairn", 
                                                         "Reunion",
                                                         "Lower middle income", 
                                                         "South Anerica", 
                                                         "Western Sahara",
                                                         "European Union", 
                                                         "Low income")), ]


#The changes have been effected 
CovidDeaths %>%
  select(location, iso_code) %>%
  filter(location %in% c("World",
                         "High income", 
                         "Upper middle income", 
                         "Europe", 
                         "North America", 
                         "Asia", 
                         "South America", 
                         "Lower middle income", 
                         "European Union")) %>%
  View()


#Now, tackling and removing variables irrelevant to our analysis or those that
#have too many NA's to be useful. 

CovidDeaths <- subset(CovidDeaths, select = -c(reproduction_rate, 
                                               icu_patients, 
                                               icu_patients_per_million, 
                                               hosp_patients, 
                                               hosp_patients_per_million, 
                                               weekly_icu_admissions, 
                                               weekly_icu_admissions_per_million,
                                               weekly_hosp_admissions,
                                               weekly_hosp_admissions_per_million))

View(CovidDeaths)

#Doing same for the CovidVaccinations data frame

CovidVaccinations <- subset(CovidVaccinations, select = -c( 
                                                           new_tests_smoothed_per_thousand,
                                                           total_tests_per_thousand, 
                                                           new_tests_per_thousand,
                                                           new_tests_smoothed,
                                                           new_tests_per_thousand,
                                                           tests_per_case, 
                                                           tests_units, 
                                                           total_vaccinations, 
                                                           people_vaccinated,
                                                           total_boosters, 
                                                           new_vaccinations_smoothed,
                                                           total_vaccinations_per_hundred,
                                                           people_vaccinated_per_hundred, 
                                                           total_boosters_per_hundred, 
                                                           new_vaccinations_smoothed_per_million,
                                                           new_people_vaccinated_smoothed,
                                                           new_people_vaccinated_smoothed_per_hundred, 
                                                           excess_mortality_cumulative, 
                                                           excess_mortality_cumulative_per_million, 
                                                           excess_mortality_cumulative_absolute, 
                                                           excess_mortality))
View(CovidVaccinations)                                       

#Creating the year, month and day column from the date variable to enable a much
#detailed analysis

#We begin with the CovidDeaths data frame.
CovidDeaths$day <- format(as.Date(CovidDeaths$date), "%d")
CovidDeaths$year <- format(as.Date(CovidDeaths$date), "%Y")
CovidDeaths$day_of_week <- format(as.Date(CovidDeaths$date), "%A")
CovidDeaths$month <- month(as.Date(CovidDeaths$date), label = TRUE)

#Let's do the same for the CovidVaccinations data frame
CovidVaccinations$day <- format(as.Date(CovidVaccinations$date), "%d")
CovidVaccinations$year <- format(as.Date(CovidVaccinations$date), "%Y")
CovidVaccinations$day_of_week <- format(as.Date(CovidVaccinations$date), "%A")
CovidVaccinations$month <- month(as.Date(CovidVaccinations$date), label = TRUE)

#The code below selects all the columns but eliminates all NAs
Deaths_new <- CovidDeaths %>% 
  select(everything()) %>% 
  filter(complete.cases(.))

#However, I'm going to save the new CovidVaccinations data frame separately
#because it eliminates certain observations I need. 
#Then, create separate data frames for each discrete analysis and visualization that I'll make. 
gdp_pc_life_exp <- CovidVaccinations %>% 
  select(location, continent, gdp_per_capita, life_expectancy) %>% 
  filter(complete.cases(.))

vacc_per_country <- CovidVaccinations %>% 
  select(location, continent, new_vaccinations, people_fully_vaccinated) %>% 
  filter(complete.cases(.))

m_n_f_smokers <- CovidVaccinations %>% 
  select(location, continent, male_smokers, female_smokers, life_expectancy) %>% 
  filter(complete.cases(.))

cardio_smokers <- CovidVaccinations %>% 
  select(location, continent, cardiovasc_death_rate, male_smokers, female_smokers) %>% 
  filter(complete.cases(.))

handwash_positive <- CovidVaccinations %>% 
  select(location, continent, handwashing_facilities, positive_rate) %>% 
  filter(complete.cases(.))

positive_pop_density <- CovidVaccinations %>% 
  select(location, continent, population_density, positive_rate) %>% 
  filter(complete.cases(.))

#stringency_newcase <- CovidVaccinations %>% 
 # select(location, continent, new_case, stringency_index) %>% 
  #filter(complete.cases(.))

hdi_life_expec <- CovidVaccinations %>% 
  select(location, continent, human_development_index, life_expectancy) %>% 
  filter(complete.cases(.))
##Find the average of the new cases per day per country for the above analysis
#Use the merged df of Deaths_new and CovidVaccinations

string_positive_rate <- CovidVaccinations %>% 
  select(location, continent, positive_rate, stringency_index) %>% 
  filter(complete.cases(.)) %>%
  group_by(location, continent) %>% 
  summarise(average_string_index = round(mean(stringency_index),2),  
            average_positive_rate = round(mean(positive_rate), 2))

#FOLLOW TO THE ANALYZE FILE TO CONTINUE READING THIS STORY. 

