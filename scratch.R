# IMPORT ----
# import with COVID information (to filter), and inpatient/outpatient/ER ONLY

## TX_2021_01.csv - COUNTY (county population and ALL deaths)
## TX_2021_02.csv - COUNTY (All Deaths EXCEPT at Decedents home / Other)
## TX_2021_03.csv - COUNTY > Month (All Deaths EXCEPT at Decedents home / Other)
## TX_2021_04.csv - COUNTY > Month (COVID Deaths EXCEPT at Decedents home / Other)

library(tidyverse)
library(janitor)
library(magrittr)
library(readr)

## raw code to make data ----

state="TX"
year="2021"

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
  select(county, county_code, annual_county_population, annual_all_deaths, annual_hospital_deaths)

month_sub = paste0(year, "/")

d3 <- read_csv(path3) %>% 
  clean_names() %>% 
  select(county_code, month_code, deaths) %>% 
  mutate(
    month = month(as.numeric(gsub(month_sub, "", month_code)), label=TRUE)
  ) %>% 
  select(county_code, month, deaths) %>% 
  set_colnames(c("county_code", "month", "monthly_hospital_deaths")) %>% 
  drop_na() %>% 
  right_join(d2) %>% 
  select(county, county_code, annual_county_population, annual_all_deaths, annual_hospital_deaths, month, monthly_hospital_deaths)

d4 <- read_csv(path4) %>% 
  clean_names() %>% 
  select(county_code, month_code, deaths) %>% 
  mutate(
    month = month(as.numeric(gsub(month_sub, "", month_code)), label=TRUE)
  ) %>% 
  select(county_code, month, deaths) %>% 
  set_colnames(c("county_code", "month", "covid_monthly_hospital_deaths")) %>% 
  drop_na() %>% 
  right_join(d3) %>% 
  mutate(year = as.numeric(year),
         state = as.factor(state),
         county = as.factor(gsub(" County, TX", "", county))
  ) %>% 
  select(year, state, county, county_code, annual_county_population, annual_all_deaths, annual_hospital_deaths, month, monthly_hospital_deaths, covid_monthly_hospital_deaths)

export_filename = paste0(state, "_", year, "_all")
write_csv(d4, paste0("export/", export_filename, ".csv"))

## Function to do it ----

make_dataset <- function(state, year, export){
  
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
    select(county, county_code, annual_county_population, annual_all_deaths, annual_hospital_deaths)
  
  month_sub = paste0(year, "/")
  
  d3 <- read_csv(path3) %>% 
    clean_names() %>% 
    select(county_code, month_code, deaths) %>% 
    mutate(
      month = month(as.numeric(gsub(month_sub, "", month_code)), label=TRUE)
    ) %>% 
    select(county_code, month, deaths) %>% 
    set_colnames(c("county_code", "month", "monthly_hospital_deaths")) %>% 
    drop_na() %>% 
    right_join(d2) %>% 
    select(county, county_code, annual_county_population, annual_all_deaths, annual_hospital_deaths, month, monthly_hospital_deaths)
  
  d4 <- read_csv(path4) %>% 
    clean_names() %>% 
    select(county_code, month_code, deaths) %>% 
    mutate(
      month = month(as.numeric(gsub(month_sub, "", month_code)), label=TRUE)
    ) %>% 
    select(county_code, month, deaths) %>% 
    set_colnames(c("county_code", "month", "monthly_covid_hospital_deaths")) %>% 
    drop_na() %>% 
    right_join(d3) %>% 
    mutate(year = as.numeric(year),
           state = as.factor(state),
           county = as.factor(gsub(" County, TX", "", county))
    ) %>% 
    select(year, state, county, county_code, annual_county_population, annual_all_deaths, annual_hospital_deaths, month, monthly_hospital_deaths, monthly_covid_hospital_deaths)
  
  if(export){
    export_filename = paste0(state, "_", year, "_all")
    write_csv(d4, paste0("export/", export_filename, ".csv"))
  }
  
  return(d4)
}

make_dataset(state = "TX", year = "2021", export = FALSE)