library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# compile stan model
test_model <- stan_model(here::here("test_setup", "stan_test.stan"))
