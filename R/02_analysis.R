#--------------------------------------------------------------------
# Where are the Moderates?
#--------------------------------------------------------------------
# Data Analysis
#--------------------------------------------------------------------
# Author: Jens Wiederspohn
#--------------------------------------------------------------------



rm(list = ls(all = TRUE))


# ----------------------------
# Load Packages
# ----------------------------

library(tidyverse)
library(haven)
library(cmdstanr)
library(posterior)

# Install CmdStan (the backend compiler)
# cmdstanr::install_cmdstan()



# ----------------------------
# Import Data
# ----------------------------

dat_long <- read_dta("data/CES 2020 - 2024 Panel Survey/recontact_prep.dta")

set.seed(1234)

dat_long <- dat_long |>
  distinct(respondent_id) |>
  arrange(respondent_id) |> 
  slice_sample(n = 500) |>
  inner_join(dat_long, by = "respondent_id")

# write_dta(dat_long, "data/CES 2020 - 2024 Panel Survey/recontact_prep_sample_500.dta")



# ----------------------------
# Prepare data for Stan
# ----------------------------

topics <- dat_long |>
  distinct(topic) |>
  arrange(topic) |>
  mutate(k = row_number())

items <- dat_long |>
  distinct(item, topic) |>
  arrange(topic, item) |>
  left_join(topics, by = "topic") |>
  mutate(i = row_number())

waves <- tibble(year = c(2020L, 2022L, 2024L)) |>
  filter(year %in% (dat_long |> distinct(year) |> pull(year))) |>
  mutate(t = row_number())

respondents <- dat_long |>
  distinct(respondent_id) |>
  arrange(respondent_id) |>
  mutate(j = row_number())

dat_indexed <- dat_long |>
  left_join(respondents, by = "respondent_id") |>
  left_join(items, by = c("item", "topic")) |>
  left_join(waves, by = "year") |>
  mutate(y = as.integer(y))



# ----------------------------
# Build (j,i) pair indexing for item-person random effects
# ----------------------------

pair_tbl <- dat_indexed |>
  distinct(j, i, k) |>
  arrange(j, i) |>
  mutate(
    pair_id = row_number(),
    pair_j = j,
    pair_i = i,
    pair_k = k
  )

dat_stan <- dat_indexed |>
  left_join(pair_tbl |> select(j, i, pair_id), by = c("j", "i")) |>
  arrange(j, t, k, i)

# Define respondents for joins
respondents <- dat_stan |>
  dplyr::distinct(j, respondent_id) |>
  dplyr::arrange(j)



# ----------------------------
# Create the list for Stan
# ----------------------------

stan_list <- list(
  N = nrow(dat_stan),
  y = dat_stan$y,
  J = nrow(respondents),
  I = nrow(items),
  K = nrow(topics),
  T = nrow(waves),
  
  jj = dat_stan$j,
  ii = dat_stan$i,
  kk = dat_stan$k,
  tt = dat_stan$t,
  
  P = nrow(pair_tbl),
  pair_id = dat_stan$pair_id,
  pair_j  = pair_tbl$pair_j,
  pair_i  = pair_tbl$pair_i,
  pair_k  = pair_tbl$pair_k
)

# Save Stan list
# saveRDS(stan_list, file = "Data/CES 2020 - 2024 Panel Survey/stan_list.rds")
# cmdstanr::write_stan_json(stan_list, "Data/CES 2020 - 2024 Panel Survey/stan_list.json")


# ----------------------------
# Quick summaries
# ----------------------------

dat_stan |>
  count(topic, year) |>
  arrange(topic, year)

dat_stan |>
  summarise(
    N = n(),
    share_y1 = mean(y),
    J = n_distinct(j),
    I = n_distinct(i),
    K = n_distinct(k),
    T = n_distinct(t),
    P = n_distinct(pair_id)
  )



# ----------------------------
# Fit model
# ----------------------------

# For execution on a local, multicore CPU with excess RAM
options(mc.cores = parallel::detectCores())

# Path to Stan code
stan_file <- "Stan/irt_model.stan"

mod <- cmdstanr::cmdstan_model(
  stan_file,
  cpp_options = list(stan_threads = FALSE)
)

init_fun <- function(chain_id) {
  list(
    alpha = rep(1, stan_list$I),
    delta = rep(0, stan_list$I),
    theta = rep(0, stan_list$J),
    sigma_topic = rep(0.3, stan_list$J),
    d_tilde = matrix(0, stan_list$J, stan_list$K),
    sigma_u = rep(0.3, stan_list$J),
    u_tilde = array(0, dim = c(stan_list$J, stan_list$K, stan_list$T)),
    nu_raw = rep(0, stan_list$P),
    sigma_nu_person = rep(0.3, stan_list$J),
    sigma_alpha = 0.5,
    sigma_delta = 0.5,
    sigma_sigma_u = 0.3,
    mu_sigma_topic = 0.3,
    sigma_sigma_topic = 0.3,
    mu_sigma_nu = 0.3,
    sigma_sigma_nu = 0.3,
    pi_raw = rep(0, stan_list$J)
  )
}

fit <- mod$sample(
  data = stan_list,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 500,
  seed = 1234,
  init = init_fun,
  adapt_delta = 0.95,
  max_treedepth = 12,
  refresh = 1,
  output_dir = "data/stan_output",
  output_basename = "irt_model"
)

# Save fit
# saveRDS(fit, "Data/stan_fits/fit_500.rds")

# Load fit
# fit <- readRDS("Data/stan_fits/fit_500.rds")

# Load fit from csvs
csv_files <- list.files(
  "data/stan_output",
  pattern = "irt_model.*\\.csv$",
  full.names = TRUE
)

fit <- as_cmdstan_fit(csv_files)

# Save draws
# draws <- fit$draws()
# saveRDS(draws, "Data/stan_fits/fit_500_draws.rds")

# Inspect posterior
draws <- fit$draws(variables = c("alpha", "delta", "theta", "pi", "d", 
                                 "sigma_topic", "sigma_u", "sigma_nu_person",
                                 "sigma_alpha", "sigma_delta", "mu_sigma_topic",
                                 "sigma_sigma_topic", "sigma_sigma_u", 
                                 "mu_sigma_nu", "sigma_sigma_nu"))

posterior::summarise_draws(draws)
posterior::rhat(draws)
posterior::ess_bulk(draws)



# ----------------------------
# Diagnostics / hyperparameters
# ----------------------------

hyper_pars <- c(
  "sigma_alpha", "sigma_delta",
  "mu_sigma_topic", "sigma_sigma_topic",
  "sigma_sigma_u",
  "mu_sigma_nu", "sigma_sigma_nu"
)

draws_hyp <- fit$draws(variables = hyper_pars)
mat <- posterior::as_draws_matrix(draws_hyp)

tab_hyp <- tibble::tibble(
  param = colnames(mat),
  mean  = colMeans(mat),
  sd    = apply(mat, 2, stats::sd),
  q05   = apply(mat, 2, stats::quantile, probs = 0.05, names = FALSE),
  q50   = apply(mat, 2, stats::quantile, probs = 0.50, names = FALSE),
  q95   = apply(mat, 2, stats::quantile, probs = 0.95, names = FALSE)
)

print(tab_hyp, n = Inf)



# ----------------------------
# Extract posterior draws (arrays)
# ----------------------------

draw_vars <- c(
  "theta", "d", "u", "nu",
  "alpha", "delta",
  "sigma_topic", "sigma_u", "sigma_nu_person",
  "pi",
  hyper_pars
)

draws <- fit$draws(variables = draw_vars)
# draws_df <- posterior::as_draws_df(draws)  # very large



# ----------------------------
# Posterior summaries (flat table)
# ----------------------------

draws_to_summ <- function(draws_obj, probs = c(0.05, 0.5, 0.95)) {
  mat <- posterior::as_draws_matrix(draws_obj)
  
  tibble::tibble(
    param = colnames(mat),
    mean  = colMeans(mat),
    sd    = apply(mat, 2, stats::sd),
    q05   = apply(mat, 2, stats::quantile, probs = probs[1], names = FALSE),
    q50   = apply(mat, 2, stats::quantile, probs = probs[2], names = FALSE),
    q95   = apply(mat, 2, stats::quantile, probs = probs[3], names = FALSE)
  )
}

summ <- draws_to_summ(draws)



# ----------------------------
# Add convergence diagnostics
# ----------------------------

add_diag <- function(fit, variables, summ_tbl) {
  dr <- fit$draws(variables = variables)
  rh <- posterior::rhat(dr)
  eb <- posterior::ess_bulk(dr)
  
  summ_tbl |>
    dplyr::mutate(
      rhat  = dplyr::if_else(param %in% names(rh), as.numeric(rh[param]), NA_real_),
      n_eff = dplyr::if_else(param %in% names(eb), as.numeric(eb[param]), NA_real_)
    )
}

summ <- add_diag(fit, hyper_pars, summ)



# ----------------------------
# Split summaries by parameter family
# ----------------------------

summ_theta <- summ |> dplyr::filter(stringr::str_starts(param, "theta\\["))
summ_d     <- summ |> dplyr::filter(stringr::str_starts(param, "d\\["))
summ_u     <- summ |> dplyr::filter(stringr::str_starts(param, "u\\["))
summ_nu    <- summ |> dplyr::filter(stringr::str_starts(param, "nu\\["))
summ_pi    <- summ |> dplyr::filter(stringr::str_starts(param, "pi\\["))

summ_alpha <- summ |> dplyr::filter(stringr::str_starts(param, "alpha\\["))
summ_delta <- summ |> dplyr::filter(stringr::str_starts(param, "delta\\["))



# ----------------------------
# Map summaries back to labels
# ----------------------------

has_diag <- all(c("n_eff", "rhat") %in% names(summ))

diag_cols <- if (has_diag) c("n_eff", "rhat") else character(0)

# theta[j]
theta_tbl <- summ_theta |>
  dplyr::mutate(j = readr::parse_number(param)) |>
  dplyr::left_join(respondents, by = "j") |>
  dplyr::transmute(
    respondent_id,
    mean, sd, q05, q50, q95,
    !!!rlang::syms(diag_cols)
  )

# d[j,k]
d_tbl <- summ_d |>
  tidyr::extract(
    param,
    into = c("j","k"),
    regex = "d\\[(\\d+),(\\d+)\\]",
    convert = TRUE
  ) |>
  dplyr::left_join(respondents, by = "j") |>
  dplyr::left_join(topics, by = "k") |>
  dplyr::transmute(
    respondent_id, topic,
    mean, sd, q05, q50, q95,
    !!!rlang::syms(diag_cols)
  )

# u[j,k,t]
u_tbl <- summ_u |>
  tidyr::extract(
    param,
    into = c("j","k","t"),
    regex = "u\\[(\\d+),(\\d+),(\\d+)\\]",
    convert = TRUE
  ) |>
  dplyr::left_join(respondents, by = "j") |>
  dplyr::left_join(topics, by = "k") |>
  dplyr::left_join(waves, by = "t") |>
  dplyr::transmute(
    respondent_id, topic, year,
    mean, sd, q05, q50, q95,
    !!!rlang::syms(diag_cols)
  )

# nu[p]  (p indexes unique (j,i) pairs)
nu_tbl <- summ_nu |>
  dplyr::mutate(p = readr::parse_number(param)) |>
  dplyr::left_join(
    pair_tbl |>
      dplyr::select(pair_id, pair_j, pair_i, pair_k) |>
      dplyr::rename(p = pair_id),
    by = "p"
  ) |>
  dplyr::left_join(respondents, by = c("pair_j" = "j")) |>
  dplyr::left_join(items, by = c("pair_i" = "i", "pair_k" = "k")) |>
  dplyr::transmute(
    respondent_id, topic, item,
    mean, sd, q05, q50, q95,
    !!!rlang::syms(diag_cols)
  )

# sigma_topic[j]
sigma_topic_tbl <- summ |>
  filter(startsWith(param, "sigma_topic[")) |>
  mutate(j = readr::parse_number(param)) |>
  left_join(respondents, by = "j") |>
  transmute(
    respondent_id,
    mean, sd, q05, q50, q95
  )

# sigma_u[j]
sigma_u_tbl <- summ |>
  filter(startsWith(param, "sigma_u[")) |>
  mutate(j = readr::parse_number(param)) |>
  left_join(respondents, by = "j") |>
  transmute(
    respondent_id,
    mean, sd, q05, q50, q95
  )

# sigma_nu_person[j]
sigma_nu_person_tbl <- summ |>
  filter(startsWith(param, "sigma_nu_person[")) |>
  mutate(j = readr::parse_number(param)) |>
  left_join(respondents, by = "j") |>
  transmute(
    respondent_id,
    mean, sd, q05, q50, q95
  )

# alpha[i]
alpha_tbl <- summ_alpha |>
  dplyr::mutate(i = readr::parse_number(param)) |>
  dplyr::left_join(items, by = "i") |>
  dplyr::transmute(topic, item, i, mean, sd, q05, q50, q95, !!!rlang::syms(diag_cols))

# delta[i]
delta_tbl <- summ_delta |>
  dplyr::mutate(i = readr::parse_number(param)) |>
  dplyr::left_join(items, by = "i") |>
  dplyr::transmute(topic, item, i, mean, sd, q05, q50, q95, !!!rlang::syms(diag_cols))

# pi[j] (random-answering mixture weight)
pi_tbl <- summ_pi |>
  dplyr::mutate(j = readr::parse_number(param)) |>
  dplyr::left_join(respondents, by = "j") |>
  dplyr::transmute(
    respondent_id,
    mean, sd, q05, q50, q95,
    !!!rlang::syms(diag_cols)
  )



# ----------------------------
# Bundle and save outputs
# ----------------------------

posteriors <- list(
  fit = fit,
  draws = draws,
  hyper = tab_hyp,
  theta = theta_tbl,
  d = d_tbl,
  u = u_tbl,
  nu = nu_tbl,
  sigma_topic = sigma_topic_tbl,
  sigma_u = sigma_u_tbl,
  sigma_nu_person = sigma_nu_person_tbl,
  alpha = alpha_tbl,
  delta = delta_tbl,
  pi = pi_tbl
)

# Build the combined individual-level posterior table
posterior_individual <- bind_rows(
  posteriors$theta |> mutate(parameter = "theta"),
  posteriors$pi |> mutate(parameter = "pi"),
  posteriors$sigma_topic |> mutate(parameter = "sigma_topic"),
  posteriors$sigma_u |> mutate(parameter = "sigma_u"),
  posteriors$sigma_nu_person |> mutate(parameter = "sigma_nu_person")
)

# Standardize column order
posterior_individual <- posterior_individual |>
  relocate(
    parameter,
    respondent_id,
    .before = mean
  ) |> 
  select(-n_eff, -rhat)

# Save lookup tables
saveRDS(
  list(
    respondents = respondents,
    topics = topics,
    items = items,
    waves = waves,
    pair_tbl = pair_tbl
  ),
  file = "data/outputs/stan_index_maps.rds"
)

# Save posterior
saveRDS(
  posteriors,
  file = "data/outputs/posteriors_full.rds"
)

# Save individual-level posterior table
saveRDS(
  posterior_individual,
  file = "data/outputs/posterior_individual_level.rds"
)

