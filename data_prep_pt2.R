# Continuation of data cleaning and formatting
# Substitute `Suppressed` with numeric value 5
# Keep only relevant death data (in-hospital AND non-COVID deaths)
# Break up datasets into annual/monthly/wkday datasets so they can be joined as needed

# Run initial data prep
source("data_prep.R")

# Join states ----
df_monthly <- bind_rows(OK_all_monthly, 
                        TX_all_monthly)

df_wkday <- bind_rows(OK_all_wkday, 
                      TX_all_wkday)

# Replace suppressed ----
df_monthly[df_monthly == "Suppressed"] <- "1"  
df_wkday[df_wkday == "Suppressed"] <- "1"  

# Create annual  DF ----
df_annual <- df_monthly %>% 
  left_join(
    df_monthly %>% 
      group_by(year, state, county, county_code) %>% 
      summarise(annual_covid_hospital_deaths = sum(as.numeric(monthly_covid_hospital_deaths)))
  ) %>%
  mutate(
    county_code = as.factor(county_code),
    annual_adj_deaths = as.numeric(annual_hospital_deaths) - as.numeric(annual_covid_hospital_deaths),
    annual_crude_rate_10k = annual_adj_deaths/annual_county_population*10000
    ) %>%
  ungroup() %>% 
  select(year, state, county, county_code, annual_county_population, annual_adj_deaths, annual_crude_rate_10k, annual_hospital_deaths, annual_covid_hospital_deaths) %>%
  set_colnames(c("year", "state", "county", "county_code", "county_population", "annual_adj_deaths", "annual_crude_rate_10k", "annual_hospital_deaths", "annual_covid_hospital_deaths")) %>% 
  # select(year, state, county, county_code, annual_county_population, annual_adj_deaths, annual_crude_rate_10k) %>%
  # set_colnames(c("year", "state", "county", "county_code", "county_population", "annual_adj_deaths", "annual_crude_rate_10k")) %>%
  unique()

## Remove negative rates ----
df_annual <- df_annual %>%
  mutate(
    annual_hospital_deaths = ifelse(annual_adj_deaths < 0, as.numeric(annual_hospital_deaths) + 3, annual_hospital_deaths),
    annual_adj_deaths = ifelse(annual_adj_deaths < 0, as.numeric(annual_hospital_deaths) - as.numeric(annual_covid_hospital_deaths), annual_adj_deaths),
    annual_crude_rate_10k = ifelse(annual_crude_rate_10k < 0, annual_adj_deaths / county_population * 10000, annual_crude_rate_10k)
  ) %>% 
  select(year, state, county, county_code, county_population, annual_adj_deaths, annual_crude_rate_10k)

# Create monthly DF ----
df_monthly <- df_monthly %>% 
  mutate(
    date = ymd(paste0(year, "-", month, "-01")),
    monthly_adj_deaths = as.numeric(monthly_hospital_deaths) - as.numeric(monthly_covid_hospital_deaths),
    monthly_crude_rate_10k = monthly_adj_deaths/annual_county_population*10000
  ) %>% 
  select(year, month, date, state, county, county_code, monthly_adj_deaths, monthly_crude_rate_10k)

# Create wkday DF ----
county_pop <- df_annual %>% select(year, state, county_code, county_population)

df_wkday <- df_wkday %>% 
  mutate(
    year = as.numeric(year),
    state = as.factor(state),
    county_code = as.factor(county_code)
  ) %>% 
  left_join(county_pop) %>% 
  mutate(
    wkday = as.factor(weekday),
    wkday_adj_deaths = as.numeric(wkday_hospital_deaths) - as.numeric(covid_wkday_hospital_deaths),
    wkday_crude_rate_10k = wkday_adj_deaths/county_population*10000
  ) %>% 
  select(year, month, wkday, state, county, county_code, county_population, wkday_adj_deaths, wkday_crude_rate_10k)

# Export ----

export_dfs <- function(){
  write_csv(df_wkday,   paste0("export2/df_wkday.csv"))
  write_csv(df_monthly, paste0("export2/df_monthly.csv"))
  write_csv(df_annual,  paste0("export2/df_annual.csv"))
  write_csv(county_pop, paste0("export2/df_county_pop.csv"))
}

export_dfs()


