install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
library(stringr)
library(scales)


#THE RELATIONSHIP BETWEEN GDP PER CAPITA AND LIFE EXPECTANCY
gdp_pc_life_exp %>% 
  ggplot(aes(x = gdp_per_capita,
             y = life_expectancy,
             color = continent))+
  geom_jitter(size = 4, width = 0.2)+
  scale_color_brewer(palette = "Dark2")+
  theme_minimal()+
  labs(title = "GDP PER CAPITA AND LIFE EXPECTANCY",
       subtitle = paste0("Data from: ", death_min_date, " to ", death_max_date),
       caption = paste0("Data Source: ourworldindata.org","\n\n Analysis by Bright Sackey (Data Analyst)"), 
       x = "GDP Per Capita (USD)",
       y = "Life Expectancy(%)",
  color = str_to_title("Continent")) +
  theme(legend.title = element_text())

#The code below will help create a subtitle in each plot
death_max_date <- max(Deaths_new$year)
death_min_date <- min(Deaths_new$year)


#THE RELATIONSHIP BETWEEN MALE SMOKERS AND CARDIOVASCULAR DEATH RATES
cardio_smokers %>%
  ggplot(aes(x = cardiovasc_death_rate, 
             y = male_smokers, 
             color = continent))+
  geom_jitter(size = 4, width = 0.2)+
  scale_color_brewer(palette = "Dark2")+
  theme_minimal()+
  labs(title = "CARDIOVASCULAR DEATH RATES AND MALE SMOKERS",
       subtitle = paste0("Data from: ", death_min_date, " to ", death_max_date),
       caption = paste0("Data Source: ourworldindata.org", "\n\n Analysis by Bright Sackey (Data Analyst)"),
       x = "Cardiovascular Death Rate (Per 100,000 People)",
       y = "Male Smokers (%)",
       color = str_to_title("Continent")) +
  theme(legend.title = element_text())

 
#THE RELATIONSHIP BETWEEN FEMALE SMOKERS AND CARDIOVASCULAR DEATH RATES
cardio_smokers %>% 
  ggplot(aes(x = cardiovasc_death_rate, 
             y = female_smokers, 
             color = continent))+
  geom_jitter(size = 4, width = 0.2)+
  scale_color_brewer(palette = "Set1")+
  theme_minimal()+
  labs(title = "CARDIOVASCULAR DEATH RATES AND FEMALE SMOKERS",
       subtitle = paste0("Data from: ", death_min_date, " to ", death_max_date),
       caption = paste0("Data Source: ourworldindata.org", "\n\n Analysis by Bright Sackey (Data Analyst)"),
       x = "Cardiovascular Death Rate (Per 100,000 People)",
       y = "Female Smokers (%)",
       color = str_to_title("Continent")) +
  theme(legend.title = element_text())


#HUMAN DEVELOPMENT INDEX AND LIFE EXPECTANCY
hdi_life_expec_2 %>% 
  group_by(location) %>% 
  ggplot(aes(x = life_expectancy, y = human_development_index, color = continent)) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()+
  geom_jitter(size = 4, width = 5)+
  scale_y_continuous(name = "Human Development Index",
                     limits = c(0.35, 1),
                     breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(name = "Life Expectancy (At Birth)",
                     limits = c(50, 90),
                     breaks = seq(50, 90, by = 20)) +
  labs(title = "HDI AND LIFE EXPECTANCY",
       subtitle = paste0("Data from: ", death_min_date, " to ", death_max_date),
       caption = paste0("Data Source: ourworldindata.org","\n\n Analysis by Bright Sackey (Data Analyst)"),
       color = str_to_title("Continent")) +
  theme(legend.title = element_text())


#TOTAL DEATHS PER CONTINENT
Deaths_new %>%
  group_by(continent) %>% 
  summarise(total_deaths = sum(new_deaths)) %>% 
  arrange(-total_deaths) %>% 
  ggplot(aes(x = continent, y = total_deaths, fill = continent)) +
  geom_col() +
  geom_text(aes(label = scales::comma(total_deaths)),
            vjust = -0.3, size = 4.5, color = "black", na.rm = TRUE) +  # Add na.rm = TRUE
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(labels = comma_format(scale = 1e-6, big.mark = ","), 
                     name = "Total Deaths (Millions)",
                     limits = c(0, 2300000),
                     breaks = seq(0, 2300000, by = 500000)) +
  theme_minimal() +
  labs(title = "COVID-19 DEATHS PER CONTINENT", 
       subtitle = paste0("Data from: ", death_min_date, " to ", death_max_date),
       caption = paste0("Data Source: ourworldindata.org", "\n\n Analysis by Bright Sackey (Data Analyst)"),
       x = "Continent", 
       y = "Total Number of Deaths") +
  theme(legend.position = "none")


#DEATHS PER LOCATION (TOP 10)
Deaths_new %>%
  group_by(location) %>%
  summarise(total_deaths = sum(new_deaths)) %>%
  top_n(10, wt = total_deaths) %>%
  arrange(desc(total_deaths)) %>%
  ggplot(aes(x = reorder(location, -total_deaths), y = total_deaths, fill = location)) +
  geom_col() +
  geom_text(aes(label = scales::comma(total_deaths)), 
            vjust = -0.3, size = 3.5, color = "black") + # Add labels
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = comma_format(scale = 1e-6, big.mark = ","), 
                     name = "Total Deaths (Millions)",
                     limits = c(0, 1200000),
                     breaks = seq(0, 1200000, by = 300000)) +
  theme_minimal() +
  labs(title = "COVID-19 DEATHS PER COUNTRY (TOP 10)",
       subtitle = paste0("Data from: ", death_min_date, " to ", death_max_date),
       caption = paste0("Data Source: ourworldindata.org", "\n\n Analysis by Bright Sackey (Data Analyst)"),
       x = "Country", 
       y = "Total Number of Deaths") +
  theme(legend.position = "none") #Remove the legend


#HANDWASHING FACILITIES AND POSITIVE RATES
handwash_positive%>% 
  group_by(location) %>% 
  ggplot(aes(x = handwashing_facilities, y = positive_rate, color = continent)) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()+
  geom_jitter(size = 4, width = 5)+
  scale_y_continuous(name = "Positive Rates (Per Case)",
                     limits = c(0., 0.5),
                     breaks = seq(0, 0.5, by = 0.1)) +
  scale_x_continuous(name = "Handwashing Facilities",
                     limits = c(0, 100),
                     breaks = seq(0, 100, by = 25)) +
  labs(title = "COVID-19 POSITIVE RATES AND HAND-WASHING FACILITIES",
       subtitle = paste0("Data from: ", death_min_date, " to ", death_max_date),
       caption = paste0("Data Source: ourworldindata.org","\n\n Analysis by Bright Sackey (Data Analyst)"),
       color = str_to_title("Continent")) +
  theme(legend.title = element_text())

#DEATHS PER YEAR
Deaths_new %>%
  group_by(year, continent) %>% 
  summarise(total_deaths = sum(new_deaths)) %>% 
  ggplot(aes(x = continent, y = total_deaths, fill = continent)) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  geom_col(position = "dodge") +
  facet_wrap(vars(year), scales = "free_x", ncol = 1) +
  scale_y_continuous(name = "Total Deaths (Thousands)") +
  geom_text(aes(label = scales::comma(total_deaths)),
            vjust = -0.3, size = 3.5, color = "black", na.rm = TRUE) +
  scale_y_continuous(labels = comma_format(scale = 1e-6, big.mark = ","), 
                     name = "Total Deaths (Millions)",
                     limits = c(0, 1500000),
                     breaks = seq(0, 1500000, by = 500000)) +
  labs(title = "COVID-19 DEATHS FOR EACH YEAR", 
       subtitle = paste0("Data from: ", death_min_date, " to ", death_max_date),
       caption = paste0("Data Source: ourworldindata.org", "\n\n Analysis by Bright Sackey (Data Analyst)"),
       x = "Continent") +
  theme(legend.position = "none")

#VACCINATIONS PER COUNTRY 
fully_vaccinated %>%
  arrange(desc(fully_vaccinated)) %>%
  head(10) %>% 
  ggplot(aes(x = location, y = fully_vaccinated, fill = location)) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  geom_text(aes(label = scales::comma(fully_vaccinated)),
            vjust = -0.3, size = 3.5, color = "black", na.rm = TRUE) +  # Add na.rm = TRUE
  scale_y_continuous(labels = comma_format(scale = 1e-6, big.mark = ","), 
                     name = "Total Vaccinations (Millions)",
                     limits = c(0, 1300000000),
                     breaks = seq(0, 1300000000, by = 400000000)) +
  theme_minimal() +
  labs(title = "COVID-19 VACCINATIONS PER CONTINENT", 
       subtitle = paste0("Data from: ", death_min_date, " to ", death_max_date),
       caption = paste0("Data Source: ourworldindata.org", "\n\n Analysis by Bright Sackey (Data Analyst)"),
       x = "Location") +
  theme(legend.position = "none")

#STRINGENCY INDEX AND POSITIVE RATES
string_positive_rate %>% 
  ggplot(aes(x = average_string_index,
             y = average_positive_rate,
             color = continent))+
  geom_jitter(size = 4, width = 0.2)+
  scale_color_brewer(palette = "Dark2")+
  theme_minimal()+
  labs(title = "STRINGENCY INDEX AND POSITIVE RATES",
       subtitle = paste0("Data from: ", death_min_date, " to ", death_max_date),
       caption = paste0("Data Source: ourworldindata.org","\n\n Analysis by Bright Sackey (Data Analyst)"), 
       x = "Stringency Index",
       y = "Positive Rate (%)",
       color = str_to_title("Continent")) +
  theme(legend.title = element_text())

#KINDLY VISIT...... TO READ THE DATA DRIVEN STORY. 
