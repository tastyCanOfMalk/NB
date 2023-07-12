library(tidyverse)
library(magrittr)
library(DT)
library(lubridate)
theme_set(theme_minimal())

# get data ----

df_monthly <- read_csv("export2/df_monthly.csv") %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(year = as.factor(year))
df_annual  <- read_csv("export2/df_annual.csv")  %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(year = as.factor(year))

county_pop <- read_csv("export2/df_county_pop.csv") %>% 
  filter(year == 2021) %>% 
  select(county_code, county_population)

pred_arimax       <- read_csv("predictions/df_arimax_predicted.csv") %>% 
  set_colnames(c("county_code", "p_arimax"))
pred_exp          <- read_csv("predictions/df_exp_predicted.csv") %>% 
  set_colnames(c("county_code", "p_exp"))
pred_lstm_minmax  <- read_csv("predictions/df_lstm_predicted_minmax.csv") %>% 
  set_colnames(c("county_code", "p_lstm_minmax"))
pred_lstm_std     <- read_csv("predictions/df_lstm_predicted_standard.csv") %>% 
  set_colnames(c("county_code", "p_lstm_std"))

# join all predictions and actual values
preds <- pred_arimax %>% 
  left_join(pred_exp) %>% 
  left_join(pred_lstm_std) %>% 
  left_join(pred_lstm_minmax) %>% 
  left_join(county_pop) %>% 
  left_join(df_monthly %>% 
              filter(year == 2021 & month == 12) %>% 
              select(county_code, monthly_adj_deaths) %>% 
              set_colnames(c("county_code", "actual")))

# convert deaths to crude rate
preds <- preds %>% 
  mutate(p_arimax      = p_arimax/county_population*10000,
         p_exp         = p_exp/county_population*10000,
         p_lstm_std    = p_lstm_std/county_population*10000,
         p_lstm_minmax = p_lstm_minmax/county_population*10000,
         actual        = actual/county_population*10000
         ) %>% 
  select(county_code, actual, p_arimax:p_lstm_minmax)
  
# convert any negative values to zero
preds[preds < 0] <- 0

# create DT of MSE ----
mse <- tibble(
  arimax      = round(mean((preds$actual - preds$p_arimax)^2),0),
  exp         = round(mean((preds$actual - preds$p_exp)^2),0),
  lstm_std    = round(mean((preds$actual - preds$p_lstm_std)^2),0),
  lstm_minmax = round(mean((preds$actual - preds$p_lstm_minmax)^2,0))
)

mse %>% 
  t() %>% 
  set_colnames(c("mse")) %>% 
  datatable(
    options = list(
      order = list(list(1, 'asc'))
  ))

# plot it ----

## single county first ----

## plot top 6 counties from high/med/low rates 
## make actual and prediction the same date, create labels for actual and predicted for color coding

# join with populations
df_a <- df_monthly %>% 
  filter(state == "OK" & year == "2021") %>% 
  left_join(df_annual %>% select(county_code, year, county_population)) %>% 
  select(date, county, county_code, county_population, monthly_crude_rate_10k)

a <- preds %>% 
  mutate(date = as_date("2021-12-02")) %>% 
  pivot_longer(cols = starts_with("p_")) %>% 
  select(-actual) %>% 
  filter(county_code == 40001)

b <- df_a %>% 
  select(date, county_code, monthly_crude_rate_10k) %>% 
  mutate(value = monthly_crude_rate_10k) %>% 
  mutate(name = "actual") %>% 
  select(-monthly_crude_rate_10k) %>% 
  filter(county_code == 40001)

c <- bind_rows(a,b)

c %>% 
  ggplot(aes(x=date, y=value, group=name, color=name, shape=name)) +
  geom_line()+
  geom_point(size=4)+
  ggtitle("Crude death rate 2021, OK") +
  scale_x_date(date_breaks = "3 months", date_labels =  "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_color_viridis_d(option="H")

## filter top 6 low/med/high and then plot ----

### find top in each category, annually
numToKeep <- 3
#### OK
OK_annual <- df_annual %>% filter(year == "2021" & state == "OK") 
OK_annual$annual_group <- cut_number(OK_annual$annual_crude_rate_10k,3)
OK_annual <- OK_annual %>%
  mutate(annual_group = as.factor(case_when(
    annual_group == levels(OK_annual$annual_group)[1] ~ "low",
    annual_group == levels(OK_annual$annual_group)[2] ~ "med",
    annual_group == levels(OK_annual$annual_group)[3] ~ "high"
  )))
OK_low <- OK_annual %>% filter(annual_group == "low") %>% arrange(annual_crude_rate_10k) %>% slice(1:numToKeep) %>% select(county_code)
OK_med <- OK_annual %>% filter(annual_group == "med") %>% slice(1:numToKeep) %>% select(county_code)
OK_high <- OK_annual %>% filter(annual_group == "high") %>% arrange(-annual_crude_rate_10k) %>% slice(1:numToKeep) %>% select(county_code)

#### TX
TX_annual <- df_annual %>% filter(year == "2021" & state == "TX") 
TX_annual$annual_group <- cut_number(TX_annual$annual_crude_rate_10k,3)
TX_annual <- TX_annual %>%
  mutate(annual_group = as.factor(case_when(
    annual_group == levels(TX_annual$annual_group)[1] ~ "low",
    annual_group == levels(TX_annual$annual_group)[2] ~ "med",
    annual_group == levels(TX_annual$annual_group)[3] ~ "high"
  )))
TX_low <- TX_annual %>% filter(annual_group == "low") %>% arrange(annual_crude_rate_10k) %>% slice(1:numToKeep) %>% select(county_code)
TX_med <- TX_annual %>% filter(annual_group == "med") %>% slice(1:numToKeep) %>% select(county_code)
TX_high <- TX_annual %>% filter(annual_group == "high") %>% arrange(-annual_crude_rate_10k) %>% slice(1:numToKeep) %>% select(county_code)

### join all predictions with actual ----
df_a <- df_monthly %>% 
  filter(year == "2021") %>% 
  left_join(df_annual %>% select(county_code, year, county_population)) %>% 
  select(date, county, county_code, county_population, monthly_crude_rate_10k)

a <- preds %>% 
  mutate(date = as_date("2021-12-02")) %>% 
  pivot_longer(cols = starts_with("p_")) %>% 
  select(-actual)

b <- df_a %>% 
  select(date, county_code, monthly_crude_rate_10k) %>% 
  mutate(value = monthly_crude_rate_10k) %>% 
  mutate(name = "actual") %>% 
  select(-monthly_crude_rate_10k)

county_legend <- df_annual %>% filter(year == 2021) %>% select(state, county_code, county) %>% unique()

c <- bind_rows(a,b) %>% 
  left_join(county_legend)

### create the plots ----
gg_OK_low <- c %>% 
  filter(county_code %in% OK_low$county_code) %>% 
  ggplot(aes(x=date, y=value, group=name, color=name, shape=name)) +
  geom_line()+
  geom_point(size=4)+
  labs(title = "OK 2021, Crude death rates - low rates",
       subtitle = "Actual vs Predicted")+
  scale_x_date(date_breaks = "3 months", date_labels =  "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_color_viridis_d(option="H")+
  facet_wrap(~county, scales = "free_y")+  xlab("Month")+
  ylab("Crude Death Rate per 10k")

gg_OK_med <- c %>% 
  filter(county_code %in% OK_med$county_code) %>% 
  ggplot(aes(x=date, y=value, group=name, color=name, shape=name)) +
  geom_line()+
  geom_point(size=4)+
  labs(title = "OK 2021, Crude death rates - med rates",
       subtitle = "Actual vs Predicted")+
  scale_x_date(date_breaks = "3 months", date_labels =  "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_color_viridis_d(option="H")+
  facet_wrap(~county, scales = "free_y")+
  xlab("Month")+
  ylab("Crude Death Rate per 10k")

gg_OK_high <- c %>% 
  filter(county_code %in% OK_high$county_code) %>% 
  ggplot(aes(x=date, y=value, group=name, color=name, shape=name)) +
  geom_line()+
  geom_point(size=4)+
  labs(title = "OK 2021, Crude death rates - high rates",
       subtitle = "Actual vs Predicted")+
  scale_x_date(date_breaks = "3 months", date_labels =  "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_color_viridis_d(option="H")+
  facet_wrap(~county, scales = "free_y")+
  xlab("Month")+
  ylab("Crude Death Rate per 10k")


gg_OK_low
gg_OK_med
gg_OK_high

# few from each OK
c %>% 
  filter(
    county_code %in% OK_low$county_code | 
    county_code %in% OK_med$county_code | 
    county_code %in% OK_high$county_code
  ) %>% 
  ggplot(aes(x=date, y=value, group=name, color=name, shape=name)) +
  geom_line()+
  geom_point(size=3)+
  labs(
    title="OK-2021, Death Rates",
    subtitle="With predictions shown for Dec"
  )+
  scale_x_date(date_breaks = "1 months", date_labels =  "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  ylab("Monthly Crude Death Rate per 10k")+
  xlab("Month")+
  scale_color_viridis_d(option="H")+
  scale_shape_manual(values=c("actual"=1, "p_exp"=15, "p_arimax"=17,"p_lstm_minmax"=3, "p_lstm_std"=7))+
  # theme(legend.position = "bottom")+
  facet_wrap(~county, scales = "free_y")


# few from each TX
c %>% 
  filter(
    county_code %in% TX_low$county_code | 
    county_code %in% TX_med$county_code | 
    county_code %in% TX_high$county_code
  ) %>% 
  ggplot(aes(x=date, y=value, group=name, color=name, shape=name)) +
  geom_line()+
  geom_point(size=3)+
  labs(
    title="TX-2021, Death Rates",
    subtitle="With predictions shown for Dec"
  )+
  scale_x_date(date_breaks = "1 months", date_labels =  "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  ylab("Monthly Crude Death Rate per 10k")+
  xlab("Month")+
  scale_color_viridis_d(option="H")+
  scale_shape_manual(values=c("actual"=1, "p_exp"=15, "p_arimax"=17,"p_lstm_minmax"=3, "p_lstm_std"=7))+
  # theme(legend.position = "bottom")+
  facet_wrap(~county, scales = "free_y")

## remove bad models ----

c %>% 
  filter(
    county_code %in% TX_low$county_code | 
    county_code %in% TX_med$county_code | 
    county_code %in% TX_high$county_code
  ) %>% 
  filter(name != "p_lstm_std" & name != "p_lstm_minmax") %>% 
  ggplot(aes(x=date, y=value, group=name, color=name, shape=name)) +
  geom_line()+
  geom_point(size=3)+
  labs(
    title="TX-2021, Death Rates",
    subtitle="With predictions shown for Dec"
  )+
  scale_x_date(date_breaks = "1 months", date_labels =  "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  ylab("Monthly Crude Death Rate per 10k")+
  xlab("Month")+
  scale_color_viridis_d(option="H")+
  # theme(legend.position = "bottom")+
  facet_wrap(~county, scales = "free_y")

# select from OK and TX
zips <- c(48005,48021,40001,40005,40017,40057,48001,40009,40127)
c %>% 
  filter(
      county_code %in% zips
  ) %>% 
  filter(name != "p_lstm_std" & name != "p_lstm_minmax"  & name != "p_arimax") %>% 
  ggplot(aes(x=date, y=value, group=name, color=name, shape=name)) +
  geom_line()+
  geom_point(size=3)+
  labs(
    title="Death Rates from TX, OK",
    subtitle="With predictions shown for Dec"
  )+
  scale_x_date(date_breaks = "1 months", date_labels =  "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  ylab("Monthly Crude Death Rate per 10k")+
  xlab("Month")+
  scale_color_viridis_d(option="H")+
  # theme(legend.position = "bottom")+
  scale_shape_manual(values=c("actual"=1, "p_exp"=15))+
  scale_color_manual(values=c("actual"="black", "p_exp"="green"))+
  facet_wrap(~county, scales = "free_y")
  facet_wrap(~county)



# look into specific counties ----
  
  # join with populations
df_c <- df_monthly %>% 
  filter(state == "TX") %>% 
  left_join(df_annual %>% select(county_code, year, county_population)) %>% 
  select(date, county, county_code, county_population, monthly_crude_rate_10k)

a <- preds %>% 
  mutate(date = as_date("2021-12-02")) %>% 
  pivot_longer(cols = starts_with("p_")) %>% 
  select(-actual) 

b <- df_c %>% 
  select(date, county_code, monthly_crude_rate_10k) %>% 
  mutate(value = monthly_crude_rate_10k) %>% 
  mutate(name = "actual") %>% 
  select(-monthly_crude_rate_10k)

d <- bind_rows(a,b)

d <- d %>% left_join(county_legend)

d %>% 
  filter(county == "Motley" | 
           county == "Roberts" | 
           county == "Terrell") %>% 
  filter(name != "p_lstm_std" &
           name != "p_lstm_minmax") %>% 
  ggplot(aes(x=date, y=value, group=name, color=name, shape=name)) +
  geom_point(size=4, alpha=.5)+
  ggtitle("Crude death rate 2021, OK") +
  scale_x_date(date_breaks = "3 months", date_labels =  "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_color_viridis_d(option="H")+
  facet_wrap(~county)
  # facet_wrap(~county, scales = "free_y")



b$annual_group <- cut_number(b$value, 3)

# get mean death rate per county for later filtering
top_OK <- df_a %>% 
  group_by(county) %>% 
  summarise(mean_rate = mean(monthly_crude_rate_10k)) %>% 
  arrange(-mean_rate) %>% 
  slice(1:6)





















# classi
# df_monthly$groups <- cut_number(df_monthly$monthly_crude_rate_10k, 3)


c %>% 
  ggplot(aes(x=date, y=value, group=name, color=name, shape=name)) +
  geom_line()+
  geom_point(size=4)+
  ggtitle("Crude death rate 2021, OK") +
  scale_x_date(date_breaks = "3 months", date_labels =  "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_color_viridis_d(option="H")

# group based on annual death rate
# df_monthly$groups <- cut_number(df_monthly$monthly_crude_rate_10k, 3)

# join to counties
# df_a <- df_a %>% left_join(df_monthly)

# df_a <- df_a %>% 
#   mutate(group = as.factor(case_when(
#     groups == levels(df_a$groups)[1] ~ "low",
#     groups == levels(df_a$groups)[2] ~ "med",
#     groups == levels(df_a$groups)[3] ~ "high"
#   )))


# df_a %>% 
#   select(date, county_code, monthly_crude_rate_10k, group)



# get mean death rate per county for later filtering
top_OK <- df_a %>% 
  group_by(county) %>% 
  summarise(mean_rate = mean(monthly_crude_rate_10k)) %>% 
  arrange(-mean_rate) %>% 
  slice(1:num_counties)

top_OK


df_a %>% 
  filter(county %in% top_OK$county) %>% 
  ggplot(aes(x=date, y=monthly_crude_rate_10k, group=county, fill=group, color=group)) +
  geom_line()+
  geom_point()+
  ggtitle("Crude death rate 2021, OK") +
  scale_x_date(date_breaks = "3 months", date_labels =  "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  facet_wrap(~county)