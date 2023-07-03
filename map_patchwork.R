library(tigris)
library(leaflet)
library(patchwork)

# all counties
ok_counties <- counties(state="OK")
tx_counties <- counties(state="TX")

# Annual map data ----
## Oklahoma ----

# join crude rates with map data
df_ok_2021 <- ok_counties %>% 
  left_join(
    df_annual %>% 
      filter(year == '2021' & state == 'OK') %>% 
      select(county, county_code, annual_crude_rate_10k) %>% 
      mutate(county_code = as.character(county_code),
             county = as.character(county)) %>% 
      set_colnames(c("NAME", "GEOID", "rate"))
  )
df_ok_2020 <- ok_counties %>% 
  left_join(
    df_annual %>% 
      filter(year == '2020' & state == 'OK') %>% 
      select(county, county_code, annual_crude_rate_10k) %>% 
      mutate(county_code = as.character(county_code),
             county = as.character(county)) %>% 
      set_colnames(c("NAME", "GEOID", "rate"))
  )
df_ok_2019 <- ok_counties %>% 
  left_join(
    df_annual %>% 
      filter(year == '2019' & state == 'OK') %>% 
      select(county, county_code, annual_crude_rate_10k) %>% 
      mutate(county_code = as.character(county_code),
             county = as.character(county)) %>% 
      set_colnames(c("NAME", "GEOID", "rate"))
  )
df_ok_2018 <- ok_counties %>% 
  left_join(
    df_annual %>% 
      filter(year == '2018' & state == 'OK') %>% 
      select(county, county_code, annual_crude_rate_10k) %>% 
      mutate(county_code = as.character(county_code),
             county = as.character(county)) %>% 
      set_colnames(c("NAME", "GEOID", "rate"))
  )

# divide into equal groups by rate
df_ok_2021$group <- as.factor(cut_number(df_ok_2021$rate, 3))
df_ok_2020$group <- as.factor(cut_number(df_ok_2020$rate, 3))
df_ok_2019$group <- as.factor(cut_number(df_ok_2019$rate, 3))
df_ok_2018$group <- as.factor(cut_number(df_ok_2018$rate, 3))

# make plots
gg_ok_2021 <- df_ok_2021 %>% 
  ggplot(aes(fill=group))+
  geom_sf() + 
  theme_void() + 
  scale_fill_manual(values=c("#FCE8B2", "#FFB74D", "#E65100"))+
  ggtitle('2021')
gg_ok_2020 <- df_ok_2020 %>% 
  ggplot(aes(fill=group))+
  geom_sf() + 
  theme_void() + 
  scale_fill_manual(values=c("#FCE8B2", "#FFB74D", "#E65100"))+
  ggtitle('2020')
gg_ok_2019 <- df_ok_2019 %>% 
  ggplot(aes(fill=group))+
  geom_sf() + 
  theme_void() + 
  scale_fill_manual(values=c("#FCE8B2", "#FFB74D", "#E65100"))+
  ggtitle('2019')
gg_ok_2018 <- df_ok_2018 %>% 
  ggplot(aes(fill=group))+
  geom_sf() + 
  theme_void() + 
  scale_fill_manual(values=c("#FCE8B2", "#FFB74D", "#E65100"))+
  ggtitle('2018')

# display plots
gg_ok_2021 + gg_ok_2020 + gg_ok_2019 + gg_ok_2018

## Texas ----
# join crude rates with map data
df_tx_2021 <- tx_counties %>% 
  left_join(
    df_annual %>% 
      filter(year == '2021' & state == 'TX') %>% 
      select(county, county_code, annual_crude_rate_10k) %>% 
      mutate(county_code = as.character(county_code),
             county = as.character(county)) %>% 
      set_colnames(c("NAME", "GEOID", "rate"))
  )
df_tx_2020 <- tx_counties %>% 
  left_join(
    df_annual %>% 
      filter(year == '2020' & state == 'TX') %>% 
      select(county, county_code, annual_crude_rate_10k) %>% 
      mutate(county_code = as.character(county_code),
             county = as.character(county)) %>% 
      set_colnames(c("NAME", "GEOID", "rate"))
  )
df_tx_2019 <- tx_counties %>% 
  left_join(
    df_annual %>% 
      filter(year == '2019' & state == 'TX') %>% 
      select(county, county_code, annual_crude_rate_10k) %>% 
      mutate(county_code = as.character(county_code),
             county = as.character(county)) %>% 
      set_colnames(c("NAME", "GEOID", "rate"))
  )
df_tx_2018 <- tx_counties %>% 
  left_join(
    df_annual %>% 
      filter(year == '2018' & state == 'TX') %>% 
      select(county, county_code, annual_crude_rate_10k) %>% 
      mutate(county_code = as.character(county_code),
             county = as.character(county)) %>% 
      set_colnames(c("NAME", "GEOID", "rate"))
  )

# divide into equal groups by rate
df_tx_2021$group <- as.factor(cut_number(df_tx_2021$rate, 3))
df_tx_2020$group <- as.factor(cut_number(df_tx_2020$rate, 3))
df_tx_2019$group <- as.factor(cut_number(df_tx_2019$rate, 3))
df_tx_2018$group <- as.factor(cut_number(df_tx_2018$rate, 3))

# make plots
gg_tx_2021 <- df_tx_2021 %>% 
  ggplot(aes(fill=group))+
  geom_sf() + 
  theme_void() + 
  scale_fill_manual(values=c("#FCE8B2", "#FFB74D", "#E65100"))+
  ggtitle('2021')
gg_tx_2020 <- df_tx_2020 %>% 
  ggplot(aes(fill=group))+
  geom_sf() + 
  theme_void() + 
  scale_fill_manual(values=c("#FCE8B2", "#FFB74D", "#E65100"))+
  ggtitle('2020')
gg_tx_2019 <- df_tx_2019 %>% 
  ggplot(aes(fill=group))+
  geom_sf() + 
  theme_void() + 
  scale_fill_manual(values=c("#FCE8B2", "#FFB74D", "#E65100"))+
  ggtitle('2019')
gg_tx_2018 <- df_tx_2018 %>% 
  ggplot(aes(fill=group))+
  geom_sf() + 
  theme_void() + 
  scale_fill_manual(values=c("#FCE8B2", "#FFB74D", "#E65100"))+
  ggtitle('2018')

# display plots
gg_tx_2021 + gg_tx_2020 + gg_tx_2019 + gg_tx_2018

# get population heat maps ----
df_ok_2021_pop <- ok_counties %>% 
  left_join(
    df_annual %>% 
      filter(year == '2021' & state == 'OK') %>% 
      select(county, county_code, county_population) %>% 
      mutate(county_code = as.character(county_code),
             county = as.character(county)) %>% 
      set_colnames(c("NAME", "GEOID", "population"))
  )
df_tx_2021_pop <- tx_counties %>% 
  left_join(
    df_annual %>% 
      filter(year == '2021' & state == 'TX') %>% 
      select(county, county_code, county_population) %>% 
      mutate(county_code = as.character(county_code),
             county = as.character(county)) %>% 
      set_colnames(c("NAME", "GEOID", "population"))
  )

df_ok_2021_pop$group <- as.factor(cut_number(df_ok_2021_pop$population, 3))
df_tx_2021_pop$group <- as.factor(cut_number(df_tx_2021_pop$population, 3))

gg_ok_2021_pop <- df_ok_2021_pop %>% 
  ggplot(aes(fill=group))+
  geom_sf() + 
  theme_void() + 
  scale_fill_manual(values=c("#FCE8B2", "#FFB74D", "#E65100"))+
  ggtitle('2021 Population')
gg_tx_2021_pop <- df_tx_2021_pop %>% 
  ggplot(aes(fill=group))+
  geom_sf() + 
  theme_void() + 
  scale_fill_manual(values=c("#FCE8B2", "#FFB74D", "#E65100"))+
  ggtitle('2021 Population')

gg_tx_2021_pop + gg_ok_2021_pop

## display maps with pop ----

# display plots
gg_tx_2021_pop + gg_tx_2021 + gg_tx_2020 + gg_tx_2019 + gg_tx_2018
gg_ok_2021_pop + gg_ok_2021 + gg_ok_2020 + gg_ok_2019 + gg_ok_2018
