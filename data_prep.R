rm(list = ls())

# IMPORT ----
# import with COVID information (to filter), and inpatient/outpatient/ER ONLY

## TX_2021_01.csv - COUNTY (County population and ALL deaths)
## TX_2021_02.csv - COUNTY (All Deaths EXCEPT at Decedents home / Other)

## TX_2021_03.csv - COUNTY > Month (All Deaths EXCEPT at Decedents home / Other)
## TX_2021_04.csv - COUNTY > Month (COVID Deaths EXCEPT at Decedents home / Other)

## TX_2021_05.csv - COUNTY > Month > Weekday (All Deaths EXCEPT at Decedents home / Other)
## TX_2021_06.csv - COUNTY > Month > Weekday (COVID Deaths EXCEPT at Decedents home / Other)

library(tidyverse)
library(janitor)
library(magrittr)
library(readr)

## Raw Code (for testing) ----

### Annual and Monthly ----

# state="TX"
# year="2021"
# 
# path1=paste0("data/", state, "_", year, "_01.csv")
# path2=paste0("data/", state, "_", year, "_02.csv")
# path3=paste0("data/", state, "_", year, "_03.csv")
# path4=paste0("data/", state, "_", year, "_04.csv")
# path5=paste0("data/", state, "_", year, "_05.csv")
# path6=paste0("data/", state, "_", year, "_06.csv")
# 
# d1 <- read_csv(path1) %>%
#   clean_names() %>%
#   select(county, county_code, population, deaths) %>%
#   set_colnames(c("county", "county_code", "annual_county_population", "annual_all_deaths")) %>%
#   drop_na()
# 
# d2 <- read_csv(path2) %>%
#   clean_names() %>%
#   select(county_code, deaths) %>%
#   set_colnames(c("county_code", "annual_hospital_deaths")) %>%
#   drop_na() %>%
#   left_join(d1) %>%
#   select(county, county_code, annual_county_population, annual_all_deaths, annual_hospital_deaths)
# 
# month_sub = paste0(year, "/")
# 
# d3 <- read_csv(path3) %>%
#   clean_names() %>%
#   select(county_code, month_code, deaths) %>%
#   mutate(
#     month = month(as.numeric(gsub(month_sub, "", month_code)), label=TRUE)
#   ) %>%
#   select(county_code, month, deaths) %>%
#   set_colnames(c("county_code", "month", "monthly_hospital_deaths")) %>%
#   drop_na() %>%
#   right_join(d2) %>%
#   select(county, county_code, annual_county_population, annual_all_deaths, annual_hospital_deaths, month, monthly_hospital_deaths)
# 
# d4 <- read_csv(path4) %>%
#   clean_names() %>%
#   select(county_code, month_code, deaths) %>%
#   mutate(
#     month = month(as.numeric(gsub(month_sub, "", month_code)), label=TRUE)
#   ) %>%
#   select(county_code, month, deaths) %>%
#   set_colnames(c("county_code", "month", "covid_monthly_hospital_deaths")) %>%
#   drop_na() %>%
#   right_join(d3) %>%
#   mutate(year = as.numeric(year),
#          state = as.factor(state),
#          county = as.factor(gsub(" County, TX", "", county))
#   ) %>%
#   select(year, state, county, county_code, annual_county_population, annual_all_deaths, annual_hospital_deaths, month, monthly_hospital_deaths, covid_monthly_hospital_deaths)
# 
# export_filename = paste0(state, "_", year, "_all")
# write_csv(d4, paste0("export/", export_filename, ".csv"))
# 
# ### Weekday ----
# 
# d5 <- read_csv(path5) %>%
#   clean_names() %>%
#   mutate(
#     year = year,
#     state = state,
#     month = month(as.numeric(gsub(month_sub, "", month_code)), label=TRUE)
#   ) %>%
#   select(state, year, county_code, month, weekday, deaths) %>%
#   set_colnames(c("county_code", "month", "weekday", "wkday_hospital_deaths")) %>%
#   drop_na()
# 
# d6 <- read_csv(path6) %>%
#   clean_names() %>%
#   mutate(
#     month = month(as.numeric(gsub(month_sub, "", month_code)), label=TRUE)
#   ) %>%
#   select(county_code, month, weekday, deaths) %>%
#   set_colnames(c("county_code", "month", "weekday", "covid_wkday_hospital_deaths")) %>%
#   drop_na()
# 
# d6 <- d5 %>% left_join(d6)
# 
# d6 %>%
#   arrange(-as.numeric(covid_wkday_hospital_deaths))

## Functions ----

make_monthly_dataset <- function(state, year, export){
  
  path1=paste0("data/", state, "_", year, "_01.csv")
  path2=paste0("data/", state, "_", year, "_02.csv")
  path3=paste0("data/", state, "_", year, "_03.csv")
  path4=paste0("data/", state, "_", year, "_04.csv")
  
  d1 <- read_csv(path1) %>% 
    clean_names() %>% 
    select(county, county_code, population, deaths) %>% 
    set_colnames(c("county", "county_code", "annual_county_population", "annual_all_deaths")) %>% 
    drop_na()
  
  d2 <- read_csv(path2) %>% 
    clean_names() %>%
    select(county_code, deaths) %>% 
    set_colnames(c("county_code", "annual_hospital_deaths")) %>% 
    drop_na() %>% 
    left_join(d1) %>% 
    mutate(
      annual_all_deaths = as.character(annual_all_deaths),
      annual_hospital_deaths = as.character(annual_hospital_deaths)
      ) %>% 
    select(county, county_code, annual_county_population, annual_all_deaths, annual_hospital_deaths)
  
  month_sub = paste0(year, "/")
  
  d3 <- read_csv(path3) %>% 
    clean_names() %>% 
    select(county_code, month_code, deaths) %>% 
    mutate(
      month = month(as.numeric(gsub(month_sub, "", month_code)), label=FALSE)
    ) %>% 
    select(county_code, month, deaths) %>% 
    set_colnames(c("county_code", "month", "monthly_hospital_deaths")) %>% 
    drop_na() %>% 
    right_join(d2) %>% 
    select(county, county_code, annual_county_population, annual_all_deaths, annual_hospital_deaths, month, monthly_hospital_deaths)
  
  county_sub = paste0(" County, ", state)
  
  d4 <- read_csv(path4) %>% 
    clean_names() %>% 
    select(county_code, month_code, deaths) %>% 
    mutate(
      month = month(as.numeric(gsub(month_sub, "", month_code)), label=FALSE)
    ) %>% 
    select(county_code, month, deaths) %>% 
    set_colnames(c("county_code", "month", "monthly_covid_hospital_deaths")) %>% 
    drop_na() %>% 
    right_join(d3) %>% 
    mutate(year = as.numeric(year),
           state = as.factor(state),
           county = as.factor(gsub(county_sub, "", county)),
           monthly_covid_hospital_deaths = as.character(monthly_covid_hospital_deaths)
    ) %>% 
    select(year, state, county, county_code, annual_county_population, annual_all_deaths, annual_hospital_deaths, month, monthly_hospital_deaths, monthly_covid_hospital_deaths)
  
  if(export){
    export_filename = paste0(state, "_", year, "_monthly")
    write_csv(d4, paste0("export/", export_filename, ".csv"))
  }
  
  return(d4)
}

make_wkday_dataset <- function(state, year, export){
  
  path5=paste0("data/", state, "_", year, "_05.csv")
  path6=paste0("data/", state, "_", year, "_06.csv")
  
  month_sub = paste0(year, "/")
  county_sub = paste0(" County, ", state)
  
  d5 <- read_csv(path5) %>% 
    clean_names() %>% 
    mutate(
      year = year,
      state = state,
      county = as.factor(gsub(county_sub, "", county)),
      month = month(as.numeric(gsub(month_sub, "", month_code)), label=FALSE)
    ) %>% 
    select(year, state, county, county_code, month, weekday, deaths) %>% 
    set_colnames(c("year", "state", "county", "county_code", "month", "weekday", "wkday_hospital_deaths")) %>% 
    drop_na()
  
  d6 <- read_csv(path6) %>% 
    clean_names() %>% 
    mutate(
      month = month(as.numeric(gsub(month_sub, "", month_code)), label=FALSE)
    ) %>% 
    select(county_code, month, weekday, deaths) %>% 
    set_colnames(c("county_code", "month", "weekday", "covid_wkday_hospital_deaths")) %>% 
    mutate(
      covid_wkday_hospital_deaths = as.character(covid_wkday_hospital_deaths)
    ) %>% 
    drop_na()
  
  d6 <- d5 %>% left_join(d6)
  
  if(export){
    export_filename = paste0(state, "_", year, "_wkday")
    write_csv(d6, paste0("export/", export_filename, ".csv"))
  }
 
  return(d6)
}

to_remove = list()

load_datasets <- function(state, year, export = FALSE){

  name_monthly = paste0(state, "_", year, "_monthly")
  assign(name_monthly, make_monthly_dataset(state = state, year = year, export = export), envir = parent.frame())

  name_wkday = paste0(state, "_", year, "_wkday")
  assign(name_wkday, make_wkday_dataset(state = state, year = year, export = export), envir = parent.frame())

  to_remove <<- append(to_remove, name_monthly)  
  to_remove <<- append(to_remove, name_wkday)  
}

export_datasets <- function(state, year, export = TRUE){
  make_monthly_dataset(state = state, year = year, export = export)
  make_wkday_dataset(state = state, year = year, export = export)
}

run_all_exports <- function(){
  export_datasets(state = "TX", year = "2021")
  export_datasets(state = "TX", year = "2020")
  export_datasets(state = "TX", year = "2019")
  export_datasets(state = "TX", year = "2018")
  
  export_datasets(state = "OK", year = "2021")
  export_datasets(state = "OK", year = "2020")
  export_datasets(state = "OK", year = "2019")
  export_datasets(state = "OK", year = "2018")
}
  
## Manual loading ----

load_datasets  (state = "TX", year = "2021")
load_datasets  (state = "TX", year = "2020")
load_datasets  (state = "TX", year = "2019")
load_datasets  (state = "TX", year = "2018")

load_datasets  (state = "OK", year = "2021")
load_datasets  (state = "OK", year = "2020")
load_datasets  (state = "OK", year = "2019")
load_datasets  (state = "OK", year = "2018")

OK_all_monthly <- bind_rows(list(OK_2018_monthly,
                                 OK_2019_monthly,
                                 OK_2020_monthly,
                                 OK_2021_monthly))
TX_all_monthly <- bind_rows(list(TX_2018_monthly,
                                 TX_2019_monthly,
                                 TX_2020_monthly,
                                 TX_2021_monthly))

OK_all_wkday <- bind_rows(list(OK_2018_wkday,
                               OK_2019_wkday,
                               OK_2020_wkday,
                               OK_2021_wkday))
TX_all_wkday <- bind_rows(list(TX_2018_wkday,
                               TX_2019_wkday,
                               TX_2020_wkday,
                               TX_2021_wkday))

export_all <- function(){
  write_csv(OK_all_monthly, paste0("export/OK_all_monthly.csv"))
  write_csv(TX_all_monthly, paste0("export/TX_all_monthly.csv"))
  write_csv(OK_all_wkday,   paste0("export/OK_all_wkday.csv"))
  write_csv(TX_all_wkday,   paste0("export/TX_all_wkday.csv"))
}

# export_all()

remove(list = to_remove %>% unlist())
remove(to_remove)