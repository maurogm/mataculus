library(data.table)
library(tidyverse)
library(fpp3)
library(rethinking)

STATES_OF_INTEREST <- c("Texas", "Florida", "New York", "California", "Pennsylvania", "Arizona")

DATES_OF_INTEREST <- c(
  "2023-02-11",
  "2023-02-25",
  "2023-03-11",
  "2023-03-25",
  "2023-04-8",
  "2023-04-22",
  "2023-05-6",
  "2023-05-20"
) %>% ymd()
DATES_OF_INTEREST_df <- data.frame(date = DATES_OF_INTEREST, week = yearweek(DATES_OF_INTEREST))

data <- fread("https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/data-truth/truth-Incident%20Hospitalizations.csv") %>%
  .[, variation_perc := (value + 1) / lag(value + 1), location_name] %>%
  .[, fourier_1 := sin(2 * pi * week(date) / 52)] %>%
  .[, fourier_2 := cos(2 * pi * week(date) / 52)] %>%
  .[, fourier_3 := sin(4 * pi * week(date) / 52)] %>%
  .[, fourier_4 := cos(4 * pi * week(date) / 52)] %>%
  .[, lag1 := lag(value), location_name] %>%
  .[, lag2 := lag(value, 2), location_name] %>%
  .[, lag3 := lag(value, 3), location_name] %>%
  .[, lag4 := lag(value, 4), location_name] %>%
  .[date >= ymd("2020-11-01")]

data %>%
  .[location_name %in% c("US", STATES_OF_INTEREST)] %>%
  ggplot() +
  geom_line(aes(x = date, y = value, color = location_name)) +
  scale_y_log10()

data %>%
  .[location_name %in% c("US", STATES_OF_INTEREST)] %>%
  ggplot() +
  geom_density(aes(value, color = location_name)) +
  scale_x_log10()


data %>%
  .[location_name %in% c(STATES_OF_INTEREST)] %>%
  #  .[date > ymd("2022-12-01")] %>%
  ggplot(aes(x = date, y = value, color = location_name)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~location_name)


data %>%
  .[location_name %in% c(STATES_OF_INTEREST)] %>%
  ggplot(aes(x = date, y = variation_perc, color = location_name)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~location_name, scales = "free_y")


ts <- data %>%
  mutate(week = yearweek(date)) %>%
  as_tsibble(index = week, key = location_name)

ts_aggregated <- ts %>%
  filter(location_name != "US") %>%
  aggregate_key(location_name, value = sum(value))

ts %>%
  filter(location_name %in% c(STATES_OF_INTEREST)) %>%
  autoplot() +
  facet_wrap(~location_name, scales = "free_y")



ts %>%
  filter(location_name %in% c(STATES_OF_INTEREST)) %>%
  features(value, feat_acf)

ts %>%
  filter(location_name %in% "US") %>%
  gg_tsdisplay(difference(value, 12),
    plot_type = "partial", lag = 36
  ) +
  labs(title = "Seasonally differenced", y = "")

ts %>%
  filter(location_name %in% "US") %>%
  gg_tsdisplay(difference(value, 12) %>% difference(),
    plot_type = "partial", lag = 36
  )


fit_original <- ts_aggregated %>%
  model(
    # arima_auto = ARIMA(log(1+value), stepwise = FALSE, approx = FALSE),
    arima = ARIMA(log(1 + value)),
  )
fit <- fit_original %>%
  reconcile(
    MinT = min_trace(arima, method = "mint_shrink")
  )


fit %>%
  forecast(h = 8) %>%
  filter(location_name == "Texas" | location_name == "Florida" | location_name == "California" | location_name == "Arizona" | location_name == "New York" | location_name == "Pennsylvania" | is_aggregated(location_name)) %>%
  autoplot(ts_aggregated, alpha = 0.7, level = 50)

index = 1
fit %>%
  select(-arima) %>%
  filter(location_name == "Texas" | location_name == "Florida" | location_name == "California" | location_name == "Arizona" | location_name == "New York" | location_name == "Pennsylvania" | is_aggregated(location_name)) %>%
  #filter(location_name == STATES_OF_INTEREST[index]) %>%
  generate(h = 8, times = 1000) %>%
  select(location_name, week, model = .model, sim = .sim, rep = .rep) %>%
  group_by(location_name, model) %>%
  summarise(
    q_05 = quantile(sim, probs = 0.05),
    q_25 = quantile(sim, probs = 0.25),
    q_50 = quantile(sim, probs = 0.50),
    q_75 = quantile(sim, probs = 0.75),
    q_95 = quantile(sim, probs = 0.95)
  ) %>%
  merge(DATES_OF_INTEREST_df %>% filter(date == "2023-03-25"), by = "week", all.x = FALSE) %>%
  #merge(DATES_OF_INTEREST_df, by = "week", all.x = FALSE) %>%
  .[!is.na(date)]


ts %>%
  filter(location_name == "Texas" | location_name == "Florida" | location_name == "California" | location_name == "Arizona" | location_name == "New York" | location_name == "Pennsylvania" | is_aggregated(location_name)) %>%
  filter(date == max(date)) %>% 
  select(value)
