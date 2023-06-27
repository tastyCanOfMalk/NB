
devtools::install_github("UrbanInstitute/urbnmapr")

library(urbnmapr)
library(tidyverse)

counties_sf <- get_urbn_map("counties", sf = TRUE)

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
  filter(state_abbv == "OK" | state_abbv == "TX") %>% 
  ggplot() + 
  geom_sf(mapping = aes(fill = test),
          color = "white", size = .05)

