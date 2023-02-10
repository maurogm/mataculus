library(data.table)
library(tidyverse)
library(fpp3)
library(readxl)

#' Hay 2 fuentes de datos. Uno con un índice mensual desde 1900,
#' y otro con un índice diario desde 1985.
#' Bajo ambos y los cargo:

url <- "https://www.matteoiacoviello.com/gpr_files/data_gpr_export.xls"
url_daily <- "https://www.matteoiacoviello.com/gpr_files/data_gpr_daily_recent.xls"

download.file(url, destfile = "/tmp/data_gpr.xls")
download.file(url_daily, destfile = "/tmp/data_gpr_daily_recent.xls.xls")

ts_gpr <- read_excel("/tmp/data_gpr.xls", guess_max = 100000) %>%
  mutate(month = yearmonth(month)) %>%
  mutate(
    log_GPRH = log(GPRH + 1),
    log_GPR = log(GPR + 1)
  ) %>%
  as_tsibble(index = month)
ts_gpr_daily <- read_excel("/tmp/data_gpr_daily_recent.xls.xls", guess_max = 100000) %>%
  mutate(date = lubridate::ymd(date)) %>%
  mutate(log_GPRD = log(GPRD + 1)) %>%
  as_tsibble(index = date)

# Aux variables:
most_recent_month <- ts_gpr %>%
  filter(month == max(month)) %>%
  pull(month)
most_recent_date <- ts_gpr_daily %>%
  filter(date == max(date)) %>%
  pull(date)
current_year <- 2023
limit_date <- lubridate::ymd("2024-01-01")
n_remaining_months <- yearmonth(limit_date) - most_recent_month - 1
n_remaining_dates <- as.numeric(limit_date - most_recent_date - 1)

ts_gpr %>%
  autoplot(.vars = GPRH) + ggtitle("Indice mensual de riesgo geopolítico")

ts_gpr %>%
  filter(!is.na(GPR)) %>%
  autoplot(.vars = GPR) + ggtitle("Indice mensual de riesgo geopolítico (reciente)")

ts_gpr_daily %>%
  autoplot(.vars = GPRD) + ggtitle("Indice diario de riesgo geopolítico")

#' Claramente hay tendencias y poca estacionalidad.
#' Vamos qué distribución tendrían si las viéramos como vaiid:

ts_gpr %>% ggplot(aes(x = GPRH)) +
  geom_density()

ts_gpr_daily %>% ggplot(aes(x = GPRD)) +
  geom_density()


#' Una escala logarítmica parece apropiada. Notemos que la distrubución
#' queda más linda en el caso diario, dado que tiene más datos:

ts_gpr %>% ggplot(aes(x = log_GPRH)) +
  geom_density()

ts_gpr_daily %>% ggplot(aes(x = log_GPRD)) +
  geom_density()


#' La transformación logarítmica además nos va a ser útil porque
#' la forecastear queremos evitar que el índice sea negativo.


#' Fitteo modelos NAIVE para tener una estimación de los residuos:
fit_naive_monthly <- ts_gpr %>%
  model(naive = NAIVE(log_GPRH))
fit_naive_daily <- ts_gpr_daily %>%
  model(naive = NAIVE(log_GPRD))

#' Y para hacer forecasts, hago bootstrap sucesivos de los residuos:

fc_naive_monthly <- fit_naive_monthly %>%
  forecast(h = n_remaining_months, bootstrap = TRUE, times = 1000)
autoplot(fc_naive_monthly, ts_gpr, level = c(50)) +
  labs(title = "Monthly data - NAIVE forecast", y = "log_GPRH")

fc_naive_daily <- fit_naive_daily %>%
  forecast(h = n_remaining_dates, bootstrap = TRUE, times = 1000)
autoplot(fc_naive_daily, ts_gpr_daily, level = c(50)) +
  labs(title = "Daily data - NAIVE forecast", y = "log_GPRD")

#' El modelo mensual da valores razonables. Extrañamente, el modelo diario no.
#'
#' Ahora bien, si usamos un modelo ETS vemos que el resultado es mucho más
#' razonable:

fit_ets_daily <- ts_gpr_daily %>%
  model(
    # Holt = ETS(log_GPRD ~ error("A") + trend("Ad") + season("N"))
    arima = ARIMA(log_GPRD),
  )

fc_ets_daily <- fit_ets_daily %>%
  forecast(h = n_remaining_dates, bootstrap = TRUE, times = 1000)
autoplot(ts_gpr_daily, log_GPRD) +
  autolayer(fc_ets_daily, level = c(50, 90)) +
  labs(title = "Daily data - Exponential smoothing forecast", y = "log_GPRD")


#' Finalmente, un modelo ETS con los datos mensuales:

fit_ets_monthly <- ts_gpr %>%
  model(
    Holt = ETS(log_GPRH ~ error("A") + trend("Ad") + season("N"))
  )

fc_ets_monthly <- fit_ets_monthly %>%
  forecast(h = n_remaining_months, bootstrap = TRUE, times = 1000)
autoplot(ts_gpr, log_GPRH) +
  autolayer(fc_ets_monthly, level = c(50, 90)) +
  labs(title = "Monthly data - Exponential smoothing forecast", y = "log_GPRH")



#' El modelo que más me gustó fue el ETS diario.
#' Por lo tanto, de ahora en más lo voy a usar para simular las
#' trayectorias futuras del índice para lo que resta del año.


n_sims <- 10000

#' Simulo data futura para el log_GPRD, y vuelvo a la escala original:
simulated_data_daily <- fit_ets_daily %>%
  generate(h = n_remaining_dates, times = n_sims, bootstrap = TRUE) %>%
  mutate(
    sim_GPRD = exp(.sim) - 1,
    .rep = as.integer(.rep)
  ) %>%
  setDT()

#' Grafico una muestra de las posibles trayectorias que simulé:
n_sims_to_plot <- 3
ts_gpr_daily %>%
  filter(date >= ymd("2020-01-01")) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = GPRD)) +
  geom_line(aes(y = sim_GPRD, colour = as.factor(.rep)),
    data = simulated_data_daily[.rep <= n_sims_to_plot],
    alpha = 0.5
  ) +
  labs(title = "Geopolitical risk index", y = "Index") +
  guides(colour = "none")


#' A las simulaciones les agrego los datos de las fechas del año actual
#' para las que ya tengo los datos reales:
actual_data_current_year <- ts_gpr_daily %>%
  filter(year(date) == 2023) %>%
  select(date, GPRD) %>%
  setDT()
replicate_adding_column <- function(df, n) {
  cbind(df, rep = rep(1:n, each = nrow(df)))
}
sim_and_actual_daily <- rbind(
  replicate_adding_column(actual_data_current_year, n_sims),
  simulated_data_daily %>%
    select(date, GPRD = sim_GPRD, rep = .rep)
)

#' Para cada trayectoria que simulé, tomo el promedio del año.
#' Y luego calculo los cuantiles de esos promedios:
simulated_quantiles_from_daily <- sim_and_actual_daily %>%
  .[, .(mean_GPRD = mean(GPRD)), by = rep] %>%
  .[, .(
    year = current_year,
    q_05 = quantile(mean_GPRD, 0.05),
    q_25 = quantile(mean_GPRD, 0.25),
    q_50 = quantile(mean_GPRD, 0.50),
    q_75 = quantile(mean_GPRD, 0.75),
    q_95 = quantile(mean_GPRD, 0.95)
  )]
simulated_quantiles_from_daily

historic_yearly_means <- ts_gpr_daily %>%
  mutate(year = year(date)) %>%
  setDT() %>%
  .[year < current_year, .(GPRD = mean(GPRD)), year]



#' Desgraciadamente, al graficar estas estimaciones junto a la información
#' histórica, vemos que los intervalos me quedaron demasiado amplios:
simulated_quantiles_from_daily %>%
  ggplot(aes(x = year)) +
  geom_point(aes(y = q_50)) +
  geom_errorbar(
    aes(ymin = q_25, , ymax = q_75),
    alpha = 0.5
  ) +
  geom_errorbar(
    aes(ymin = q_05, ymax = q_95),
    alpha = 0.2
  ) +
  geom_line(data = historic_yearly_means, aes(y = GPRD)) +
  geom_point(data = historic_yearly_means, aes(y = GPRD)) +
  labs(title = "Forecasted yearly index", y = "GPRH")







#' Aunque pensé que el modelo ETS diario me iba a dar mejores resultados,
#' veo que pasa si uso el modelo ETS mensual:
ts_gpr_daily %>% tail(31) %>% as.data.table()
fit_ets_monthly_recent <- ts_gpr %>%
  filter(!is.na(GPR)) %>%
  model(
    # naive = NAIVE(log_GPR),
    arima = ARIMA(log_GPR),
    # Holt = ETS(log_GPR ~ error("A") + trend("Ad") + season("N"))
  )

fit_ets_monthly_recent %>%
  gg_tsresiduals()

fit_ets_monthly_recent %>%
  forecast(h = n_remaining_months, bootstrap = TRUE, times = 1000) %>%
  autoplot(filter(ts_gpr, !is.na(GPR)), level = c(50, 80, 95)) +
  labs(title = "Monthly data - NAIVE forecast", y = "log_GPRH")


#' Simulo data futura para el log_GPRD, y vuelvo a la escala original:
simulated_data_daily <- fit_ets_monthly_recent %>%
  generate(h = n_remaining_months, times = n_sims, bootstrap = TRUE) %>%
  mutate(
    sim_GPRD = exp(.sim) - 1,
    .rep = as.integer(.rep)
  ) %>%
  setDT()

#' Grafico una muestra de las posibles trayectorias que simulé:
n_sims_to_plot <- 100
ts_gpr %>%
  filter(month >= yearmonth("2000-01-01")) %>%
  ggplot(aes(x = month)) +
  geom_line(aes(y = GPR)) +
  geom_line(aes(y = sim_GPRD, colour = as.factor(.rep)),
    data = simulated_data_daily[.rep <= n_sims_to_plot],
    alpha = 0.5
  ) +
  labs(title = "Geopolitical risk index", y = "Index") +
  guides(colour = "none")


#' A las simulaciones les agrego los datos de las fechas del año actual
#' para las que ya tengo los datos reales:
actual_data_current_year <- ts_gpr %>%
  filter(year(month) == 2023) %>%
  select(month, GPR) %>%
  setDT()
replicate_adding_column <- function(df, n) {
  cbind(df, rep = rep(1:n, each = nrow(df)))
}
sim_and_actual_monthly <- rbind(
  replicate_adding_column(actual_data_current_year, n_sims),
  simulated_data_daily %>%
    select(month, GPR = sim_GPRD, rep = .rep)
)

#' Para cada trayectoria que simulé, tomo el promedio del año.
#' Y luego calculo los cuantiles de esos promedios:
simulated_quantiles_from_monthly <- sim_and_actual_monthly %>%
  .[, .(mean_GPR = mean(GPR)), by = rep] %>%
  .[, .(
    year = current_year,
    q_05 = quantile(mean_GPR, 0.05),
    q_25 = quantile(mean_GPR, 0.25),
    q_50 = quantile(mean_GPR, 0.50),
    q_75 = quantile(mean_GPR, 0.75),
    q_95 = quantile(mean_GPR, 0.95)
  )]
simulated_quantiles_from_monthly

historic_yearly_means <- ts_gpr %>%
  filter(!is.na(GPR)) %>%
  mutate(year = year(month)) %>%
  setDT() %>%
  .[year < current_year, .(GPR = mean(GPR)), year]

#' Desgraciadamente, al graficar estas estimaciones junto a la información
#' histórica, vemos que los intervalos me quedaron demasiado amplios:
simulated_quantiles_from_monthly %>%
  ggplot(aes(x = year)) +
  geom_point(aes(y = q_50)) +
  geom_errorbar(
    aes(ymin = q_25, , ymax = q_75),
    alpha = 0.5
  ) +
  geom_errorbar(
    aes(ymin = q_05, ymax = q_95),
    alpha = 0.2
  ) +
  geom_line(data = historic_yearly_means, aes(y = GPR)) +
  geom_point(data = historic_yearly_means, aes(y = GPR)) +
  labs(title = "Forecasted yearly index", y = "GPR")


simulated_quantiles_from_monthly

