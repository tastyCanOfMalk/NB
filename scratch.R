library(tidyverse)
library(janitor)
library(Hmisc)
library(plotly)
library(lubridate)
library(DT)
theme_set(theme_minimal())

# IMPORT DATA ----

# suppressed = (1-9) deaths
# unreliable = < 20 deaths

## old import ----
TX_pop <- read_csv("data/TX_2021_total_pop.csv") %>% 
  janitor::clean_names() %>% 
  mutate(county = as.factor(gsub(" County", "",gsub(", TX", "", county))),
         county_code = as.factor(county_code),
         crude_rate_per_100k = as.numeric(gsub("Unreliable", NA, crude_rate)),
         annual_deaths = as.numeric(gsub("Suppressed", NA, deaths))) %>% 
  select(county_code, annual_deaths, population, crude_rate_per_100k) %>% 
  drop_na()

TX_day <- read_csv("data/TX_2021_deaths_weekday.csv") %>% 
  janitor::clean_names() %>% 
  mutate(month = as.factor(gsub("2021/", "", month_code)),
         month = month(as.numeric(month), label=TRUE),
         county = as.factor(gsub(" County", "",gsub(", TX", "", county))),
         county_code = as.factor(county_code),
         weekday = as.factor(weekday),
         daily_deaths = as.numeric(gsub("Suppressed", NA, deaths))) %>% 
  select(county, county_code, month, weekday, daily_deaths) %>% 
  filter(weekday != "Unknown")

TX_all <- TX_day %>% left_join(TX_pop, by="county_code")
 
## Functions ----

setup_data <- function(year, state){

  path_pop <- paste0("data/", state, "_", year, "_total_pop.csv")
  path_day <- paste0("data/", state, "_", year, "_deaths_weekday.csv")
  
  temp_pop <- read_csv(path_pop) %>% 
    janitor::clean_names() %>% 
    mutate(county = as.factor(gsub(" County", "",gsub(", TX", "", county))),
           county_code = as.factor(county_code),
           # crude_rate_per_100k = as.numeric(gsub("Unreliable", NA, crude_rate)),
           # crude_rate_per_100k = crude_rate,
           # annual_deaths = as.numeric(gsub("Suppressed", NA, deaths))
           annual_county_deaths = deaths,
           annual_county_population = population
           ) %>% 
    select(county_code, annual_county_deaths, annual_county_population) %>%
    drop_na()
  
  month_gsub = paste0(year,"/")
  temp_day <- read_csv(path_day) %>% 
    janitor::clean_names() %>% 
    mutate(state = as.factor(state),
           year = as.numeric(year),
           month = gsub(month_gsub, "", month_code),
           month = month(as.numeric(month), label=TRUE),
           county = as.factor(gsub(" County", "",gsub(", TX", "", county))),
           county_code = as.factor(county_code),
           weekday = as.factor(weekday),
           # daily_deaths = as.numeric(gsub("Suppressed", NA, deaths)),
           daily_county_deaths = deaths
           ) %>% 
    select(state, county, county_code, year, month, weekday, daily_county_deaths) %>% 
    filter(weekday != "Unknown")
  
  temp_all <- temp_day %>% left_join(temp_pop, by="county_code")
  
  return(temp_all)
}

join_data <- function(state){
  
  tmp_2018 <- setup_data(year = "2018", state = state)
  tmp_2019 <- setup_data(year = "2019", state = state)
  tmp_2020 <- setup_data(year = "2020", state = state)
  tmp_2021 <- setup_data(year = "2021", state = state)
  
  tmp_all <-  
    bind_rows(
      tmp_2018, 
      tmp_2019,
      tmp_2020,
      tmp_2021
    )
  
  return(tmp_all)
}

TX_all <- join_data(state = "TX")


## compare annual death to calculated ----

compare <- 
  left_join(TX_pop,
            TX_all %>% 
              select(county_code, daily_deaths) %>% 
              group_by(county_code) %>% 
              summarise(calc_annual_deaths = sum(daily_deaths, na.rm = TRUE))
            ) %>% 
  select(county_code, annual_deaths, calc_annual_deaths) %>% 
  mutate(lessThan = ifelse(calc_annual_deaths <= annual_deaths, TRUE, FALSE))
  
compare %>% 
  arrange(-annual_deaths)

compare %>% 
  filter(lessThan == FALSE)

# EDA ----

## monthly distribution ----

gg_1 <- 
  TX_all %>% 
    mutate_if(is.character, as.numeric) %>% 
    mutate(year = as.factor(year)) %>% 
    group_by(year, month) %>% 
    summarise(deaths = sum(daily_deaths, na.rm = TRUE)) %>% 
    # ungroup() %>% 
    ggplot(aes(x=month, y=deaths, color=year, group=year)) +
    geom_point() + 
    geom_line(alpha=.8) + 
    labs(
      title = "Deaths over time"
    ) + 
    scale_y_continuous(labels = comma) + 
    xlab("Month") +
    ylab("Deaths")

ggplotly(gg_1)

## normalized monthly distribution ----

# get TX population by year
TX_pop <- TX_all %>%
  select(county, year, county_population) %>% 
  distinct() %>% 
  group_by(year) %>% 
  summarise(TX_population = sum(county_population)) %>% 
  mutate(year = as.factor(year))

gg_2 <- TX_all %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(year = as.factor(year)) %>% 
  group_by(year, month) %>% 
  summarise(deaths = sum(daily_deaths, na.rm = TRUE)) %>% 
  left_join(TX_pop) %>%
  mutate(pct_pop_deaths = deaths/TX_population) %>% 
  ggplot(aes(x=month, y=pct_pop_deaths, color=year, group=year)) +
  geom_point() + 
  geom_line(alpha=.8) + 
  labs(
    title = "Deaths over time",
    subtitle = "Normalized for population"
  ) + 
  scale_y_continuous(labels = percent) + 
  xlab("Month") +
  ylab("Deaths (percent of state population")
ggplotly(gg_2)

## visualize covid spike ----

TX_all %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(year = as.factor(year)) %>%
  group_by(year, month) %>% 
  summarise(deaths = sum(daily_deaths, na.rm = TRUE)) %>% 
  left_join(TX_pop) %>%
  mutate(
    pct_pop_deaths = deaths/TX_population,
    date = ymd(paste0(year,"-", month, "-01")),
    isCovid = ifelse(year == 2018, TRUE, FALSE)
    ) %>% 
  ggplot(aes(x=date, y=pct_pop_deaths, color=year)) +
  geom_point()+
  geom_line(alpha=.8)+
  labs(
    title = "Deaths over time",
    subtitle = "Normalized for population"
  ) + 
  scale_y_continuous(labels = percent) + 
  scale_x_date(date_breaks = "4 months", date_labels = "%Y-%m") + 
  xlab("Month") +
  ylab("Deaths (percent of state population")+
  geom_rect(aes(xmin=ymd("2020-01-01"), xmax=ymd("2021-12-01"), ymin=0, ymax=.001), alpha=.005, fill='orange', color=NA) +
  theme(legend.position = "none")  


