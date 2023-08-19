#TOTAL DEATHS OVERALL
Deaths_new %>% 
  summarise(total_deaths = sum(new_deaths)) %>% 
  View()

#TOTAL DEATHS PER YEAR
Deaths_new %>% 
  select(new_deaths, year) %>%
  group_by(year) %>% 
  summarise(total_deaths =  sum(new_deaths))%>% 
  arrange(-total_deaths) %>% 
  View()
  
#TOTAL DEATHS PER YEAR PER LOCATION
Deaths_new %>% 
  select(location, new_deaths, year) %>%
  group_by(location, year) %>% 
  summarise(total_deaths = sum(new_deaths)) %>% 
  arrange(-total_deaths) %>% 
  View()

#TOTAL DEATHS PER CONTINENT
Deaths_new %>% 
  select(continent, new_deaths, year) %>% 
  group_by(continent) %>% 
  summarise(total_deaths = sum(new_deaths)) %>% 
  arrange(-total_deaths) %>% 
  View()

#TOTAL AVERAGE NEW CASES PER CONTINENT PER DAY
Deaths_new %>% 
  select(new_cases, continent) %>% 
  group_by(continent) %>%
  summarise(average_new_case_per_day = round(mean(new_cases), 0)) %>%
  arrange(-average_new_case) %>% 
  View()

#FULLY VACCINATED PEOPLE BY LOCATION AND CONTINENT
fully_vaccinated <- CovidVaccinations %>% 
  select(location, people_fully_vaccinated, continent, new_vaccinations) %>% 
  filter(complete.cases(.)) %>% 
  group_by(location, continent) %>% 
  summarise(fully_vaccinated = last(people_fully_vaccinated)) %>% 
  arrange(-fully_vaccinated)

#The new df (fully_vaccinated) will then be used to find the total vaccinations
#overall


#TOTAL VACCINATIONS OVERALL
  sum(fully_vaccinated$fully_vaccinated) %>% 
    View()

#TOTAL VACCINATIONS PER CONTINENT
  CovidVaccinations %>% 
    select(location, people_fully_vaccinated, continent) %>% 
    filter(complete.cases(.)) %>% 
    group_by(continent) %>% 
    summarise(fully_vaccinated = max(people_fully_vaccinated)) %>% 
    arrange(-fully_vaccinated) %>% 
    View()

#TOTAL VACCINATIONS OVERALL
  sum(fully_vaccinated$fully_vaccinated) 

#TOTAL VACCINATIONS PER CONTINENT
  CovidVaccinations %>% 
    select(location, people_fully_vaccinated, continent) %>% 
    filter(complete.cases(.)) %>% 
    group_by(continent) %>% 
    summarise(fully_vaccinated = max(people_fully_vaccinated)) %>% 
    arrange(-fully_vaccinated) %>% 
    View()
  
#TOTAL TESTS PER COUNTRY 
  CovidVaccinations %>% 
    select(location, new_tests, continent) %>% 
    filter(complete.cases(.)) %>% 
    group_by(location) %>% 
    summarise(total_tests = sum(new_tests)) %>% 
    arrange(-total_tests) %>% 
    View()
  
#STRINGENCY INDEX PER COUNTRY
  CovidVaccinations %>% 
    select(location, continent, stringency_index) %>% 
    filter(complete.cases(.)) %>% 
    group_by(location) %>% 
    summarise(stringency_index = first(stringency_index)) %>% 
    arrange(-stringency_index) %>% 
    View()
  
#CALCULATING THE DEATH RATES FOR EACH OBSERVATION EVEN AS IT INCREASES OR
#DECREASES OVER TIME
Deaths_new <- Deaths_new %>% 
  mutate(death_rate = round((total_deaths / population) * 100, 2))
   
table(Deaths_new$death_rate)

#AVERAGE DEATH RATES PER CONTINENT
Deaths_new %>% 
  select(continent, death_rate) %>%
  group_by(continent) %>% 
  summarise(average_death_rate = mean(death_rate)) %>% 
  arrange(-average_death_rate) %>% 
  View()

#AVERAGE DEATH RATES PER LOCATION
Deaths_new %>% 
  select(location, death_rate) %>%
  group_by(location) %>% 
  summarise(average_death_rate = mean(death_rate)) %>% 
  arrange(-average_death_rate) %>% 
  View()

#HIGHEST SMOKERS PER COUNTRY AND CONTINENT
#MALES
CovidVaccinations %>%
  select(male_smokers, location, continent) %>% 
  group_by(location) %>%
  summarise(percentage_of_male_smokers = first(male_smokers)) %>% 
  arrange(-percentage_of_male_smokers) %>% 
  top_n(20, wt = percentage_of_male_smokers) %>% 
  View()
#The top_n(10, wt = percentage_of_male_smokers) clause limits the number or rows
#returned to 20 

#FEMALES
CovidVaccinations %>%
  select(female_smokers, location, continent) %>% 
  group_by(location) %>%
  summarise(percentage_of_female_smokers = first(female_smokers)) %>% 
  arrange(-percentage_of_female_smokers) %>% 
  top_n(20, wt = percentage_of_female_smokers) %>% 
  View()

#The top_n(20, wt = percentage_of_male_smokers) clause limits the number or rows
#returned to 20

#CARDIOVASCULAR DEATH RATE (TOP 50)
CovidVaccinations %>%
  select(location, continent, cardiovasc_death_rate) %>% 
  group_by(location) %>%
  summarise(cardiovascular_death_rate = first(cardiovasc_death_rate)) %>% 
  arrange(-cardiovascular_death_rate) %>% 
  top_n(50, wt = cardiovascular_death_rate) %>% 
  View()
#The top_n(50, wt = percentage_of_male_smokers) clause limits the number or rows
#returned to 50


#PLEASE FOLLOW TO THE VISUALIZATIONS FILE TO FOLLOW THIS STORY 