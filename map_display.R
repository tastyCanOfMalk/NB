
# urbnmappr ----
## https://github.com/UrbanInstitute/urbnmapr

# devtools::install_github("UrbanInstitute/urbnmapr")

library(urbnmapr)
library(tidyverse)

counties_sf <- get_urbn_map("counties", sf = TRUE)

counties_sf %>% 
  filter(state_abbv == "OK" | state_abbv == "TX") %>% 
  select(state_abbv, county_fips, geometry) %>% 
  set_colnames(c("state", "county_code", "geometry")) %>% 
  ggplot() + 
  geom_sf(mapping = aes(fill = state),
          color = "white", size = .05)

  
  mutate_if(is.character, as.factor)

map_data %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=NA))

df_annual %>%
  mutate(county_code = as.factor(county_code)) %>%
  left_join(map_data) %>% 
  ggplot()+
  geom_sf(mapping = aes(fill=annual_adj_deaths), color="white", size=.5)

  
  
  
  

county_groups <- countydata %>% 
  mutate(cat_var = paste0("Group ",
                          sample(1:4, nrow(countydata), replace = TRUE)))

household_data <- left_join(counties_sf, county_groups, by = "county_fips")

household_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = cat_var),
          color = NA, size = 0.05) +
  labs(fill = "Categorical variable")


ggplotly(
  counties_sf %>% 
    mutate(test = ifelse(state_abbv == "OK" | state_abbv == "OK", "Group1", "Group2")) %>%
    filter(state_abbv == "OK" | state_abbv == "TX") %>% 
    ggplot() + 
    geom_sf(mapping = aes(fill = test),
            color = "white", size = .05)
)


counties_sf %>% 
  mutate(test = ifelse(state_abbv == "OK" | state_abbv == "TX", "Group1", "Group2")) %>%
  # left_join(df_annual)
  filter(state_abbv == "OK" | state_abbv == "TX") %>% 
  ggplot() + 
  geom_sf(mapping = aes(fill = test),
          color = "white", size = .05)


# tigris ----
## https://rpubs.com/walkerke/tigris01 

library(acs)
library(tigris)
library(leaflet)
library(sp)

api.key.install("698268df7541bd6eacdc66cfb3476cfd112c8ab2") # You can get your own API key from the Census Bureau

dfw <- tracts(state = 'TX', county = c('Dallas', 'Tarrant'))

income_data <- acs.fetch(endyear = 2012, 
                         geography = geo.make(state = "TX", 
                                              county = c(113, 439), 
                                              tract = "*"), 
                         variable = "B19013_001")

income_df <- data.frame(paste0(as.character(income_data@geography$state), 
                               as.character(income_data@geography$county), 
                               income_data@geography$tract), 
                        income_data@estimate)

colnames(income_df) <- c("GEOID", "hhincome")

dfw_merged <- geo_join(dfw, income_df, "GEOID", "GEOID")

pal <- colorQuantile("Greens", NULL, n = 6)

popup <- paste0("Median household income: ", as.character(dfw_merged$hhincome))

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = dfw_merged, 
              fillColor = ~pal(dfw_merged$hhincome), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = popup) %>%
  addLegend(pal = pal, 
            values = dfw_merged$hhincome, 
            position = "bottomright", 
            title = "Income in DFW")



a <- tracts(state='OK')

# annother ----

library(tigris)

# plot states
st <- states()
plot(st$geometry)

# plot counties
tx_counties <- counties(state="TX")
ok_counties <- counties(state="OK")

counties <- bind_rows(tx_counties, ok_counties)
 
plot(tx_counties$geometry)
plot(ok_counties$geometry)
plot(counties$geometry)

# join with death data
df_monthly %>% 
  filter(year == 2021 & state == "OK") %>% 
  select(county, county_code, monthly_crude_rate_10k) %>% 
  left_join(ok_counties, by)

# ggplot counties
ggplot(tx_counties) + 
  geom_sf() + 
  theme_void()

#using patchwork
library(patchwork)

tx <- ggplot(tx_counties) + 
  geom_sf() + 
  theme_void()

ok <- ggplot(ok_counties) + 
  geom_sf() + 
  theme_void()

tx / ok
tx + ok

# mapview
library(mapview)
library(sf)
mapview(ok_counties)

# crs
library(crsuggest)

tx_crs <- suggest_crs(tx_counties)
ok_crs <- suggest_crs(ok_counties)

tx_projected <- st_transform(tx_counties, crs=3085)
ok_projected <- st_transform(ok_counties, crs=2836)

options(scipen = 999)
# ggplot(tx_counties) + 
#   geom_sf() +
#   coord_sf(crs = 3085)
ggplot(tx_projected) + 
  geom_sf()

# testing LEAFLET ----
library(tigris)
library(leaflet)
tx_counties <- counties(state="TX")
ok_counties <- counties(state="OK")
counties <- bind_rows(tx_counties, ok_counties)

## OK ----
ok_test <- ok_counties %>% 
  # select(GEOID, geometry) %>% 
  left_join(
    df_annual %>% 
      filter(year == '2021' & state == 'OK') %>% 
      select(county, county_code, annual_crude_rate_10k, county_population) %>% 
      mutate(county_code = as.character(county_code),
             county = as.character(county),
             annual_crude_rate_10k = round(annual_crude_rate_10k, digits=1)) %>% 
      set_colnames(c("NAME", "GEOID", "rate", "population"))
  )

# ggplot(ok_test, aes(fill = rate)) + 
#   geom_sf()

# try leaflet
# pal_ok <- colorNumeric("Greens", domain=ok_test$rate)
# pal_ok <- colorNumeric(c("yellow", "orange", "red"), domain=ok_test$rate)
# pal_ok <- colorNumeric('plasma', domain=ok_test$rate)
# pal_ok <- colorQuantile(c("yellow", "orange", "red"), 
#                         domain=as.numeric(tx_test$rate),
#                         n=3)
pal_ok <- colorQuantile(palette = "plasma",
                        domain=as.numeric(ok_test$rate),
                        n=3)

popup <- paste0(paste0("<strong>", ok_test$NAME, 
                       "</strong><br/>Rate: ", ok_test$rate,
                       "</strong><br/>Population: ", format(ok_test$population, big.mark=",", scientific=FALSE)))

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = ok_test, 
              fillColor = ~pal_ok(ok_test$rate),
              fillOpacity = 0.8, 
              weight = .8, 
              # smoothFactor = 0.2,
              popup = ~popup,
              highlightOptions = highlightOptions(color = "white", weight = 5, bringToFront = TRUE)) %>% 
  addLegend(pal = pal_ok, 
            values = ok_test$rate,
            position = "bottomright", 
            title = "Crude death rate per 10k")

## TX ----

tx_test <- tx_counties %>% 
  left_join(
    df_annual %>% 
      filter(year == '2021' & state == 'TX') %>% 
      select(county, county_code, annual_crude_rate_10k, county_population) %>% 
      mutate(county_code = as.character(county_code),
             county = as.character(county),
             annual_crude_rate_10k = round(annual_crude_rate_10k, digits=1)) %>% 
      set_colnames(c("NAME", "GEOID", "rate", "population"))
  )
ggplot(tx_test, aes(fill = rate)) + 
  geom_sf()

library(viridis)
# pal_tx <- colorNumeric("viridis", domain=as.numeric(tx_test$rate))
pal_tx <- colorQuantile(c("yellow", "orange", "red"),
                        domain=as.numeric(tx_test$rate),
                        n=3)
# pal_tx <- colorQuantile(palette = "plasma",
#                         domain=as.numeric(tx_test$rate),
#                         n=3)

popup <- paste0(paste0("<strong>", tx_test$NAME, 
                       "</strong><br/>Rate: ", tx_test$rate,
                       "</strong><br/>Population: ", format(tx_test$population, big.mark=",", scientific=FALSE)))

# popup_sb <- paste0("<strong>", states_merged_sb_pc$NAME, 
#                    "</strong><br />Total: ", states_merged_sb_pc$total,
#                    "<br />Per capita: ", 
#                    as.character(states_merged_sb_pc$per_capita))
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = tx_test, 
              fillColor = ~pal_tx(tx_test$rate),
              fillOpacity = 0.9, 
              weight = 0.2, 
              smoothFactor = 0.2,
              popup = ~popup) %>% 
  addLegend(pal = pal_tx, 
            values = tx_test$rate,
            position = "bottomright", 
            title = "Crude death rate per 10k")
