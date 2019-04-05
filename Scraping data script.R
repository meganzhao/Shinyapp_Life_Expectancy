# Life expectancy table
life_tables<- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_life_expectancy") %>%
  html_nodes("table")
life_exp <- html_table(life_tables[[1]])
write_csv(life_exp, "data/life_exp.csv")

# Education variables
ed_tables <- read_html("https://en.wikipedia.org/wiki/Education_Index") %>%
  html_nodes("table")
education <- html_table(ed_tables[[2]])
write_csv(education, "data/education.csv")

# Traffic-related death rate table
traffic_tables <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_traffic-related_death_rate") %>%
  html_nodes("table")
traffic <- html_table(traffic_tables[[1]], fill = TRUE)
write_csv(traffic, "data/traffic.csv")

# Cigarette consumption table
cigarette_tables <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_cigarette_consumption_per_capita") %>%
  html_nodes("table")
cigarette <- html_table(cigarette_tables[[1]])
write_csv(cigarette, "data/cigarette.csv")

# Wealth per adult table
wealth_tables <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_wealth_per_adult") %>%
  html_nodes("table")
wealth_per_adult <- t(html_table(wealth_tables[[2]], fill = TRUE)[1,]) %>%
  as.tibble()
write_csv(wealth_per_adult, "data/wealth_per_adult.csv")

# Gas emission table
gas_emission_tables <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_greenhouse_gas_emissions") %>%
  html_nodes("table")
gas_emission <- html_table(gas_emission_tables[[1]])
write_csv(gas_emission, "data/gas_emission.csv")
