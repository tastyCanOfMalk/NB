library(tidyverse)
library(janitor)
library(magrittr)
library(readr)
library(DT)
library(plotly)
library(gridExtra)
library(tigris)
library(leaflet)
library(patchwork)

theme_set(theme_minimal())

rm(list = ls())

tx_counties <- counties(state="TX")
ok_counties <- counties(state="OK")

df_wkday   <- read_csv("export2/df_wkday.csv")   %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(year = as.factor(year))
df_monthly <- read_csv("export2/df_monthly.csv") %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(year = as.factor(year))
df_annual  <- read_csv("export2/df_annual.csv")  %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(year = as.factor(year))
df_annual_5  <- read_csv("example_data/df_annual_5.csv")  %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(year = as.factor(year))
df_annual_neg  <- read_csv("example_data/df_annual_neg.csv")  %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(year = as.factor(year))


# glimpsed ----
df_wkday %>% glimpse()
df_monthly %>% glimpse()
df_annual %>% glimpse()

# suppressed w 5 ----
df_annual_5 %>%
  select(state, county, county_population, annual_adj_deaths, annual_crude_rate_10k) %>%
  arrange(-annual_crude_rate_10k) %>% 
  slice(1:5)

# histograms comparing suppressed values ----
gg_a <- df_annual_5 %>% 
  ggplot(aes(x=annual_crude_rate_10k))+
  geom_histogram(bins=50, fill=NA, color="black")+
  geom_vline(xintercept = 0, color="red")+
  labs(
    title="Distribution of death rates",
    subtitle="Suppressed replaced with 5"
  )+
  xlab("Annual Crude Death Rate per 10k")

gg_b <- df_annual_neg %>%
  ggplot(aes(x=annual_crude_rate_10k))+
  geom_histogram(bins=50, fill=NA, color="black")+
  geom_vline(xintercept = 0, color="red")+
  labs(
    title="Better distribution of death rates",
    subtitle="Suppressed replaced with 1"
  )+
  xlab("Annual Crude Death Rate per 10k")

gg_c <- df_annual %>% 
  ggplot(aes(x=annual_crude_rate_10k))+
  geom_histogram(bins=50, fill=NA, color="black")+
  # geom_vline(xintercept = 0, color="red")+
  labs(
    title="Final distribution of death rates",
    subtitle="Negative values replaced with 0"
  )+
  xlab("Annual Crude Death Rate per 10k")

gg_a + gg_b + gg_c

# library(ggpubr)
# ggqqplot(df_annual$annual_crude_rate_10k)

# histogram faceted ----
df_annual %>% 
  ggplot(aes(x=annual_crude_rate_10k))+
  geom_histogram(bins=50, fill=NA, color="black")+
  # geom_vline(xintercept = 0, color="red")+
  labs(
    title="Final distribution of death rates"
  ) +
  xlab("Annual Crude Death Rate per 10k")+
  facet_wrap(~state+year, nrow = 2)

# annual heatmaps ----
## TX ----

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
  ggtitle('2021') + 
  theme(legend.position = "top")
gg_tx_2020 <- df_tx_2020 %>% 
  ggplot(aes(fill=group))+
  geom_sf() + 
  theme_void() + 
  scale_fill_manual(values=c("#FCE8B2", "#FFB74D", "#E65100"))+
  ggtitle('2020')+ 
  theme(legend.position = "top")
gg_tx_2019 <- df_tx_2019 %>% 
  ggplot(aes(fill=group))+
  geom_sf() + 
  theme_void() + 
  scale_fill_manual(values=c("#FCE8B2", "#FFB74D", "#E65100"))+
  ggtitle('2019')+ 
  theme(legend.position = "top")
gg_tx_2018 <- df_tx_2018 %>% 
  ggplot(aes(fill=group))+
  geom_sf() + 
  theme_void() + 
  scale_fill_manual(values=c("#FCE8B2", "#FFB74D", "#E65100"))+
  ggtitle('2018')+ 
  theme(legend.position = "top")

df_tx_2021_pop <- tx_counties %>% 
  left_join(
    df_annual %>% 
      filter(year == '2021' & state == 'TX') %>% 
      select(county, county_code, county_population) %>% 
      mutate(county_code = as.character(county_code),
             county = as.character(county)) %>% 
      set_colnames(c("NAME", "GEOID", "population"))
  )

df_tx_2021_pop$group <- cut_number(df_tx_2021_pop$population/10000, 3)

gg_tx_2021_pop <- df_tx_2021_pop %>% 
  ggplot(aes(fill=group))+
  geom_sf() + 
  theme_void() + 
  scale_fill_manual(values=c("#FCE8B2", "#FFB74D", "#E65100"))+
  # scale_fill_viridis_d()+
  ggtitle('2021 Population/10,000')+ 
  theme(legend.position = "top")

(gg_tx_2021_pop + gg_tx_2021) / (gg_tx_2020 + gg_tx_2019)

## OK ----
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
  ggtitle('2021') + 
  theme(legend.position = "top")
gg_ok_2020 <- df_ok_2020 %>% 
  ggplot(aes(fill=group))+
  geom_sf() + 
  theme_void() + 
  scale_fill_manual(values=c("#FCE8B2", "#FFB74D", "#E65100"))+
  ggtitle('2020') + 
  theme(legend.position = "top")
gg_ok_2019 <- df_ok_2019 %>% 
  ggplot(aes(fill=group))+
  geom_sf() + 
  theme_void() + 
  scale_fill_manual(values=c("#FCE8B2", "#FFB74D", "#E65100"))+
  ggtitle('2019') + 
  theme(legend.position = "top")
gg_ok_2018 <- df_ok_2018 %>% 
  ggplot(aes(fill=group))+
  geom_sf() + 
  theme_void() + 
  scale_fill_manual(values=c("#FCE8B2", "#FFB74D", "#E65100"))+
  ggtitle('2018') + 
  theme(legend.position = "top")

# populations
df_ok_2021_pop <- ok_counties %>% 
  left_join(
    df_annual %>% 
      filter(year == '2021' & state == 'OK') %>% 
      select(county, county_code, county_population) %>% 
      mutate(county_code = as.character(county_code),
             county = as.character(county)) %>% 
      set_colnames(c("NAME", "GEOID", "population"))
  )

df_ok_2021_pop$group <- as.factor(cut_number(df_ok_2021_pop$population/10000, 3))

gg_ok_2021_pop <- df_ok_2021_pop %>% 
  ggplot(aes(fill=group))+
  geom_sf() + 
  theme_void() + 
  scale_fill_manual(values=c("#FCE8B2", "#FFB74D", "#E65100"))+
  # scale_fill_viridis_d()+
  ggtitle('2021 Population/10,000') + 
  theme(legend.position = "top")

(gg_ok_2021_pop + gg_ok_2021) / (gg_ok_2020 + gg_ok_2019)

# monthly line charts ----
## take 2021, get high/med/low rate states and plot 3 of each
## color code the rates themselves to show when they are changing from low/med/high

## select some different counties based on annual crude death rates ----
numToShow = 3

# get high med low for states
OK_annual <- df_annual %>% filter(year == "2021" & state == "OK") 
OK_annual$annual_group <- cut_number(OK_annual$annual_crude_rate_10k,3)
OK_annual <- OK_annual %>%
  mutate(annual_group = as.factor(case_when(
    annual_group == levels(OK_annual$annual_group)[1] ~ "low",
    annual_group == levels(OK_annual$annual_group)[2] ~ "med",
    annual_group == levels(OK_annual$annual_group)[3] ~ "high"
  )))
# OK_low <- OK_annual %>% filter(annual_group == "low") %>% arrange(annual_crude_rate_10k) %>% slice(1:numToShow) %>% select(county_code)
OK_low <- OK_annual %>% filter(annual_group == "low") %>% slice(1:numToShow) %>% select(county_code)
OK_med <- OK_annual %>% filter(annual_group == "med") %>% slice(1:numToShow) %>% select(county_code)
OK_high <- OK_annual %>% filter(annual_group == "high") %>% arrange(-annual_crude_rate_10k) %>% slice(1:numToShow) %>% select(county_code)

TX_annual <- df_annual %>% filter(year == "2021" & state == "TX") 
TX_annual$annual_group <- cut_number(TX_annual$annual_crude_rate_10k,3)
TX_annual <- TX_annual %>%
  mutate(annual_group = as.factor(case_when(
    annual_group == levels(TX_annual$annual_group)[1] ~ "low",
    annual_group == levels(TX_annual$annual_group)[2] ~ "med",
    annual_group == levels(TX_annual$annual_group)[3] ~ "high"
  )))
# TX_low <- TX_annual %>% filter(annual_group == "low") %>% arrange(annual_crude_rate_10k) %>% slice(1:numToShow) %>% select(county_code)
TX_low <- TX_annual %>% filter(annual_group == "low") %>% slice(1:numToShow) %>% select(county_code)
TX_med <- TX_annual %>% filter(annual_group == "med") %>% slice(1:numToShow) %>% select(county_code)
# TX_high <- TX_annual %>% filter(annual_group == "high") %>% slice(1:numToShow) %>% select(county_code)
TX_high <- TX_annual %>% filter(annual_group == "high") %>% arrange(-annual_crude_rate_10k) %>% slice(1:numToShow) %>% select(county_code)

## OK plots ----
df_a <- df_monthly %>% 
  filter(state == "OK" & year == "2021") %>% 
  left_join(df_annual %>% select(county_code, year, county_population)) %>% 
  select(date, county, county_code, county_population, monthly_crude_rate_10k)

df_a$groups <- cut_number(df_a$monthly_crude_rate_10k, 3)

df_a <- df_a %>% 
  mutate(group = as.factor(case_when(
    groups == levels(df_a$groups)[1] ~ "low",
    groups == levels(df_a$groups)[2] ~ "med",
    groups == levels(df_a$groups)[3] ~ "high"
  )))

gg_OK_low <- df_a %>% 
  filter(county_code %in% OK_low$county_code) %>% 
  ggplot(aes(x=date, y=monthly_crude_rate_10k, group=county, fill=group, color=group)) +
  geom_line()+
  geom_point(size=4)+
  labs(
    title="OK-2021, Death Rates",
    subtitle="Counties with Lowest mean annual death rates"
  )+
  scale_x_date(date_breaks = "3 months", date_labels =  "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  ylab("Monthly Crude Death Rate per 10k")+
  xlab("Month")+
  facet_wrap(~county)

gg_OK_med <- df_a %>% 
  filter(county_code %in% OK_med$county_code) %>% 
  ggplot(aes(x=date, y=monthly_crude_rate_10k, group=county, fill=group, color=group)) +
  geom_line()+
  geom_point(size=4)+
  labs(
    title="OK-2021, Death Rates",
    subtitle="Counties with Medium mean annual death rates"
  )+
  scale_x_date(date_breaks = "3 months", date_labels =  "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  ylab("Monthly Crude Death Rate per 10k")+
  xlab("Month")+
  facet_wrap(~county)

gg_OK_high <- df_a %>% 
  filter(county_code %in% OK_high$county_code) %>% 
  ggplot(aes(x=date, y=monthly_crude_rate_10k, group=county, fill=group, color=group)) +
  geom_line()+
  geom_point(size=4)+
  labs(
    title="OK-2021, Death Rates",
    subtitle="Counties with Highest mean annual death rates"
  )+
  scale_x_date(date_breaks = "3 months", date_labels =  "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  ylab("Monthly Crude Death Rate per 10k")+
  xlab("Month")+
  facet_wrap(~county)

gg_OK_low / gg_OK_med / gg_OK_high

df_a %>% 
  filter(
    county_code %in% OK_low$county_code | 
    county_code %in% OK_med$county_code | 
    county_code %in% OK_high$county_code
      ) %>% 
  
  ggplot(aes(x=date, y=monthly_crude_rate_10k, group=county, fill=group, color=group)) +
  geom_line()+
  geom_point(size=3)+
  labs(
    title="OK-2021, Death Rates"
  )+
  scale_x_date(date_breaks = "1 months", date_labels =  "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  ylab("Monthly Crude Death Rate per 10k")+
  xlab("Month")+
  facet_wrap(~county)

## TX plots ----
df_b <- df_monthly %>% 
  filter(state == "TX" & year == "2021") %>% 
  left_join(df_annual %>% select(county_code, year, county_population)) %>% 
  select(date, county, county_code, county_population, monthly_crude_rate_10k)

df_b$groups <- cut_number(df_b$monthly_crude_rate_10k, 3)

df_b <- df_b %>% 
  mutate(group = as.factor(case_when(
    groups == levels(df_b$groups)[1] ~ "low",
    groups == levels(df_b$groups)[2] ~ "med",
    groups == levels(df_b$groups)[3] ~ "high"
  )))

gg_TX_low <- df_b %>% 
  filter(county_code %in% TX_low$county_code) %>% 
  ggplot(aes(x=date, y=monthly_crude_rate_10k, group=county, fill=group, color=group)) +
  geom_line()+
  geom_point(size=4)+
  labs(
    title="TX-2021, Death Rates",
    subtitle="Counties with Lowest mean annual death rates"
  )+
  scale_x_date(date_breaks = "3 months", date_labels =  "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  ylab("Monthly Crude Death Rate per 10k")+
  xlab("Month")+
  facet_wrap(~county)

gg_TX_med <- df_b %>% 
  filter(county_code %in% TX_med$county_code) %>% 
  ggplot(aes(x=date, y=monthly_crude_rate_10k, group=county, fill=group, color=group)) +
  geom_line()+
  geom_point(size=4)+
  labs(
    title="TX-2021, Death Rates",
    subtitle="Counties with Medium mean annual death rates"
  )+
  scale_x_date(date_breaks = "3 months", date_labels =  "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  ylab("Monthly Crude Death Rate per 10k")+
  xlab("Month")+
  facet_wrap(~county)

gg_TX_high <- df_b %>% 
  filter(county_code %in% TX_high$county_code) %>% 
  ggplot(aes(x=date, y=monthly_crude_rate_10k, group=county, fill=group, color=group)) +
  geom_line()+
  geom_point(size=4)+
  labs(
    title="TX-2021, Death Rates",
    subtitle="Counties with Highest mean annual death rates"
  )+
  scale_x_date(date_breaks = "3 months", date_labels =  "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  ylab("Monthly Crude Death Rate per 10k")+
  xlab("Month")+
  facet_wrap(~county)

gg_TX_low / gg_TX_med / gg_TX_high

df_b %>% 
  filter(
    county_code %in% TX_low$county_code | 
    county_code %in% TX_med$county_code | 
    county_code %in% TX_high$county_code
  ) %>% 
  ggplot(aes(x=date, y=monthly_crude_rate_10k, group=county, fill=group, color=group)) +
  geom_line()+
  geom_point(size=3)+
  labs(
    title="TX-2021, Death Rates"
  )+
  scale_x_date(date_breaks = "1 months", date_labels =  "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  ylab("Monthly Crude Death Rate per 10k")+
  xlab("Month")+
  # theme(legend.position = "bottom")+
  # scale_color_manual(values=c("low"="#FCE8B2", "med"="#FFB74D", "high"="#E65100"))+
  facet_wrap(~county)

# prediction plots ----


# show highest populated areas not necessarily 
