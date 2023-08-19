#Then we merge both the CovidVaccinations and CovidDeaths data frames based on
#the iso_code variable.

# Check the number of rows in each data frame
nrow_CovidDeaths <- nrow(CovidDeaths)
nrow_CovidVaccinations <- nrow(CovidVaccinations)

# Make sure the number of rows is the same for both data frames
if (nrow_CovidDeaths > nrow_CovidVaccinations) {
  CovidDeaths <- head(CovidDeaths, nrow_CovidVaccinations)
} else {
  CovidVaccinations <- head(CovidVaccinations, nrow_CovidDeaths)
}                           

#Now, let's merge!
Covid_merged <- bind_cols(Deaths_new, CovidVaccinations)

#Then, to make our analysis and code run faster, we create different data frames
#for each of the visualizations we want to create and the analysis we want to
#do.

#Also, any code that involves the Covid_merged data frame and also involves the
#use of the iso_code, location, continent, date, month, day and day of week
#variables will use these variables instead:

table(Covid_merged$iso_code...1)
table(Covid_merged$location...3)
table(Covid_merged$continent...2)
table(Covid_merged$date...4)
table(Covid_merged$month...21)
table(Covid_merged$day...18)
table(Covid_merged$day_of_week...20) 

rm(dt_deaths, dt_vaccinations)


#PLEASE FOLLOW TO THE ANALYSIS FILE TO CONTINUE READING THIS STORY

library(scales)

Deaths_new %>%
  group_by(location) %>%
  summarise(total_deaths = sum(new_deaths)) %>%
  top_n(10, wt = total_deaths) %>%
  arrange(desc(total_deaths)) %>%
  ggplot(aes(x = reorder(location, -total_deaths), y = total_deaths, fill = location)) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = comma_format()) +
  theme_minimal() +
  labs(title = "Deaths Per Country (Top 10)",
       subtitle = paste0("Data from: ", death_min_date, " to ", death_max_date),
       caption = paste0("Data Source: ourworldindata.org"),
       x = "Location",
       y = "Total Number of Deaths")



library(scales)  # Load the 'scales' library for comma_format()

Deaths_new %>%
  group_by(country) %>%
  summarise(total_deaths = sum(new_deaths)) %>%
  top_n(10, wt = total_deaths) %>%
  arrange(desc(total_deaths)) %>%
  ggplot(aes(x = reorder(country, -total_deaths), y = total_deaths, fill = country)) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(labels = comma_format()) +  # Format y-axis labels with commas
  theme_minimal() +
  labs(title = "Deaths Per Country (Top 10)",
       subtitle = paste0("Data from: ", death_min_date, " to ", death_max_date),
       caption = paste0("Data Source: ourworldindata.org"),
       x = "Country",
       y = "Total Number of Deaths")

  # Load the 'scales' library for comma_format()

Deaths_new %>%
  group_by(location) %>%
  summarise(total_deaths = sum(new_deaths)) %>%
  top_n(10, wt = total_deaths) %>%
  arrange(desc(total_deaths)) %>%
  ggplot(aes(x = reorder(location, -total_deaths), y = total_deaths, fill = location)) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = comma_format(scale = 1e-6, big.mark = ","), 
                     name = "Total Deaths (Millions)") +  # Format y-axis labels with commas and add custom label
  theme_minimal() +
  labs(title = "Deaths Per Location (Top 10)",
       subtitle = paste0("Data from: ", death_min_date, " to ", death_max_date),
       caption = paste0("Data Source: ourworldindata.org"),
       x = "Location",
       y = "Total Number of Deaths")


library(scales)  # Load the 'scales' library for comma_format()

library(scales)  # Load the 'scales' library for comma_format()

Deaths_new %>%
  group_by(location) %>%
  summarise(total_deaths = sum(new_deaths)) %>%
  top_n(10, wt = total_deaths) %>%
  arrange(desc(total_deaths)) %>%
  ggplot(aes(x = reorder(location, -total_deaths), y = total_deaths, fill = location)) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = comma_format(scale = 1e-6, big.mark = ","), 
                     name = "Total Deaths (Millions)",
                     limits = c(0, 1300000),  # Set custom y-axis limits
                     breaks = seq(0, 1300000, by = 300000)) +  # Specify the breaks
  theme_minimal() +
  labs(title = "Deaths Per Location (Top 10)",
       subtitle = paste0("Data from: ", death_min_date, " to ", death_max_date),
       caption = paste0("Data Source: ourworldindata.org"),
       x = "Location",
       y = "Total Number of Deaths")


library(scales)  # Load the 'scales' library for comma_format()

library(scales)  # Load the 'scales' library for comma_format()

Deaths_new %>%
  group_by(location) %>%
  summarise(total_deaths = sum(new_deaths)) %>%
  top_n(10, wt = total_deaths) %>%
  arrange(desc(total_deaths)) %>%
  ggplot(aes(x = reorder(location, -total_deaths), y = total_deaths, fill = location)) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = comma_format(scale = 1e-6, big.mark = ","), 
                     name = "Total Deaths (Millions)",
                     limits = c(0, 1200000),  # Set custom y-axis limits
                     breaks = seq(0, 1200000, by = 300000)) +  # Specify the breaks
  theme_minimal() +
  labs(title = "Deaths Per Location (Top 10)",
       subtitle = paste0("Data from: ", death_min_date, " to ", death_max_date),
       caption = paste0("Data Source: ourworldindata.org",  "\n\n Analysis by Bright Sackey (Data Analyst)"),
       x = "Location", 
       y = "Total Number of Deaths") +
  theme(legend.position = "none") #Remove the legend

library(scales)  # Load the 'scales' library for comma_format()

Deaths_new %>%
  group_by(location) %>%
  summarise(total_deaths = sum(new_deaths)) %>%
  top_n(10, wt = total_deaths) %>%
  arrange(desc(total_deaths)) %>%
  ggplot(aes(x = reorder(location, -total_deaths), y = total_deaths, fill = location)) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = comma_format(scale = 1e-6, big.mark = ","), 
                     name = "Total Deaths (Millions)",
                     limits = c(0, 1200000),  # Set custom y-axis limits
                     breaks = seq(0, 1200000, by = 200000)) +  # Specify the breaks
  theme_minimal() +
  labs(title = "Deaths Per Location (Top 10)",
       subtitle = paste0("Data from: ", death_min_date, " to ", death_max_date),
       caption = paste0("Data Source: ourworldindata.org"),
       x = "Location", 
       y = "Total Number of Deaths",
       caption = "Built by Bright Sackey (Data Analyst)") +  # Add the label
  theme(legend.position = "none")  # Remove the legend



library(scales)  # Load the 'scales' library for comma_format()

Deaths_new %>%
  group_by(location) %>%
  summarise(total_deaths = sum(new_deaths)) %>%
  top_n(10, wt = total_deaths) %>%
  arrange(desc(total_deaths)) %>%
  ggplot(aes(x = reorder(location, -total_deaths), y = total_deaths, fill = location)) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = comma_format(scale = 1e-6, big.mark = ","), 
                     name = "Total Deaths (Millions)",
                     limits = c(0, 1200000),  # Set custom y-axis limits
                     breaks = seq(0, 1200000, by = 200000)) +  # Specify the breaks
  theme_minimal() +
  labs(title = "Deaths Per Location (Top 10)",
       subtitle = paste0("Data from: ", death_min_date, " to ", death_max_date),
       x = NULL,  # Remove the x-axis label
       y = "Total Number of Deaths",
       caption = "Built by Bright Sackey (Data Analyst)") +  # Add the label
  theme(legend.position = "none")  # Remove the legend



library(scales)

library(scales)




#HOW DOES POPULATION DENSITY AFFECT POSITIVE TEST RATES?
positive_pop_density %>% 
  ggplot(aes(x = positive_rate,
             y = population_density,
             color = continent))+
  geom_jitter(size = 4, width = 0.2)+
  scale_color_brewer(palette = "Dark2")+
  theme_minimal()

#HOW DOES THE INSTALLATION OF HANDWASHING FACILITIES AFFECT THE RATE OF POSITIVE TEST RESULTS?
handwash_positive %>% 
  ggplot(aes(x = positive_rate, 
             y = handwashing_facilities,
             color = continent))+
  geom_jitter(size = 4, width = 0.2)+
  scale_color_brewer(palette = "Dark2")+
  theme_minimal()

# Check for missing values in the 'continent' and 'total_deaths' columns
sum(is.na(Deaths_new$continent))
sum(is.na(Deaths_new$total_deaths))

Deaths_new %>%
  group_by(continent) %>% 
  summarise(total_deaths = sum(new_deaths)) %>% 
  arrange(desc(total_deaths)) %>%
  View()

# Save the plot as a PNG file
ggsave("covid_deaths_per_continent.png", width = 10, height = 6)


hdi_life_expec_2 <- CovidVaccinations %>% 
  select(location, continent, life_expectancy, human_development_index) %>% 
  filter(complete.cases(.))%>%
  group_by(location, continent) %>% 
  summarise(life_expectancy = first(life_expectancy), human_development_index = first(human_development_index)) 


Deaths_new %>%
  group_by(location) %>%
  summarise(total_deaths = sum(new_deaths)) %>%
  top_n(10, wt = total_deaths) %>%
  arrange(desc(total_deaths)) %>%
  ggplot(aes(x = reorder(location, -total_deaths), y = total_deaths, fill = location)) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = comma_format(scale = 1e-6, big.mark = ","), 
                     name = "Total Deaths (Millions)",
                     limits = c(0, 1200000),  # Set custom y-axis limits
                     breaks = seq(0, 1200000, by = 200000)) +  # Specify the breaks
  theme_minimal() +
  labs(title = "Deaths Per Location (Top 10)",
       subtitle = paste0("Data from: ", death_min_date, " to ", death_max_date),
       x = NULL,  # Remove the x-axis label
       y = "Total Number of Deaths",
       caption = "Built by Bright Sackey (Data Analyst)") +  # Add the label
  theme(legend.position = "none")  # Remove the legend

positive_pop_density <- CovidVaccinations %>% 
  select(location, continent, population_density, positive_rate) %>% 
  filter(complete.cases(.)) %>% 
  group_by(location, continent) %>% 
  summarise(population_density = first(population_density), 
            average_positive_rate = round(mean(positive_rate), 2))

library(ggplot2)



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


fully_vaccinated %>%
  
  library(dplyr)

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
  labs(title = "COVID-19 VACCINATIONS PER COUNTRY", 
       subtitle = paste0("Data from: ", death_min_date, " to ", death_max_date),
       caption = paste0("Data Source: ourworldindata.org", "\n\n Analysis by Bright Sackey (Data Analyst)"),
       x = "Location") +
  theme(legend.position = "none")

 
 
 ggplot(poverty_map, aes(map_id = region, fill = `Taux de pauvreté`)) +
   geom_map(map = poverty_map,  color = "white") +
   expand_limits(x = poverty_map$long, y = poverty_map$lat) +
   scale_fill_viridis_c(option = "C") +
   theme_minimal()
 
 
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
  