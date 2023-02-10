# Must have installed rstan first

install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

cmdstanr::install_cmdstan()

renv::install(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
devtools::install_github("rmcelreath/rethinking")
