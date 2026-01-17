#--------------------------------------------------------------------
# Where are the Moderates?
#--------------------------------------------------------------------
# Visualize Results
#--------------------------------------------------------------------
# Author: Jens Wiederspohn
#--------------------------------------------------------------------



rm(list = ls(all = TRUE))


# ----------------------------
# Load Packages
# ----------------------------

library(tidyverse)
library(kableExtra)



# ----------------------------
# Import Data
# ----------------------------

posterior_individual <- readRDS("data/outputs/posterior_individual_level.rds")



# ----------------------------
# Construct respondent-level latent summaries
# ----------------------------

# Global ideology
theta_j <- posterior_individual |>
  filter(parameter == "theta") |>
  transmute(respondent_id, theta = mean)

# Cross-topic dispersion
sigma_d_j <- posterior_individual |>
  filter(parameter == "sigma_topic") |>
  transmute(respondent_id, sigma_d = mean)

# Temporal instability
sigma_u_j <- posterior_individual |>
  filter(parameter == "sigma_u") |>
  transmute(respondent_id, sigma_u = mean)

# Stable idiosyncrasy
sigma_nu_j <- posterior_individual |>
  filter(parameter == "sigma_nu_person") |>
  transmute(respondent_id, sigma_nu = mean)

# Response quality
pi_j <- posterior_individual |>
  filter(parameter == "pi") |>
  transmute(respondent_id, pi = mean)

# Assemble respondent-level analysis dataset
respondent_latents <- theta_j |>
  left_join(sigma_d_j, by = "respondent_id") |>
  left_join(sigma_u_j, by = "respondent_id") |>
  left_join(sigma_nu_j, by = "respondent_id") |>
  left_join(pi_j, by = "respondent_id")



# ------------------------------------------------------------
# Summary statistics table
# ------------------------------------------------------------

summary_tbl <- tibble::tibble(
  Parameter = c(
    "$\\theta_j$",
    "$\\sigma_{d,j}$",
    "$\\sigma_{u,j}$",
    "$\\sigma_{\\nu,j}$",
    "$\\pi_j$"
  ),
  Meaning = c(
    "Global ideology",
    "Cross-topic dispersion",
    "Temporal instability",
    "Item-specific idiosyncrasy",
    "Random responses"
  ),
  Variable = c("theta", "sigma_d", "sigma_u", "sigma_nu", "pi")
) |>
  rowwise() |>
  mutate(
    `Min.`    = min(respondent_latents[[Variable]], na.rm = TRUE),
    `1st Qu.` = stats::quantile(respondent_latents[[Variable]], 0.25, na.rm = TRUE),
    `Median`  = stats::median(respondent_latents[[Variable]], na.rm = TRUE),
    `Mean`    = base::mean(respondent_latents[[Variable]], na.rm = TRUE),
    `3rd Qu.` = stats::quantile(respondent_latents[[Variable]], 0.75, na.rm = TRUE),
    `Max.`    = max(respondent_latents[[Variable]], na.rm = TRUE)
  ) |>
  ungroup() |>
  select(-Variable) |>
  mutate(across(c(`Min.`, `1st Qu.`, `Median`, `Mean`, `3rd Qu.`, `Max.`), ~ round(.x, 3)))

latex_table <- summary_tbl |>
  kable(
    format = "latex",
    booktabs = TRUE,
    align = "llrrrrrr",
    col.names = c("Parameter", "Substantive", "Min.", "1st Qu.", 
                  "Median", "Mean", "3rd Qu.", "Max."),
    caption = "Summary statistics of respondent-level latent parameters",
    escape = FALSE
  ) |>
  kableExtra::kable_styling(latex_options = c("hold_position")) |>
  kableExtra::column_spec(2, width = "4.3cm")

cat(latex_table)



# ----------------------------
# Define centrality and moderation dimensions
# ----------------------------

# Central ideology
theta_cut <- quantile(abs(respondent_latents$theta), probs = 0.5)

respondent_latents <- respondent_latents |>
  mutate(
    central_theta = abs(theta) <= theta_cut
  )

# Low dispersion / instability / noise
cuts <- respondent_latents |>
  summarise(
    d_cut  = quantile(sigma_d, 0.5),
    u_cut  = quantile(sigma_u, 0.5),
    nu_cut = quantile(sigma_nu, 0.5),
    pi_cut = quantile(pi, 0.5)
  )

respondent_latents <- respondent_latents |>
  mutate(
    low_dispersion = sigma_d <= cuts$d_cut,
    low_instability = sigma_u <= cuts$u_cut,
    low_idiosyncrasy = sigma_nu <= cuts$nu_cut,
    low_noise = pi <= cuts$pi_cut
  )



# ----------------------------
# Classify respondent types
# ----------------------------

respondent_latents <- respondent_latents |>
  mutate(
    type = case_when(
      central_theta & low_dispersion & low_instability & low_idiosyncrasy & low_noise
      ~ "Substantive moderate",
      
      !central_theta & low_dispersion & low_instability & low_idiosyncrasy & low_noise
      ~ "Constrained extremist",
      
      central_theta & !low_dispersion & low_instability & low_noise
      ~ "Cross-pressured",
      
      central_theta & !low_idiosyncrasy & low_noise
      ~ "Idiosyncratic unstable",
      
      central_theta & !low_noise
      ~ "Weakly constrained / noisy",
      
      TRUE ~ "Other"
    )
  )



# ----------------------------
# Initial empirical results
# ----------------------------

respondent_latents |>
  filter(central_theta) |>
  count(type) |>
  mutate(share = n / sum(n))

respondent_latents |>
  summarise(
    p_substantive = mean(type == "Substantive moderate")
  )



# ----------------------------
# Function: classify types given a quantile q for all cutpoints
# ----------------------------
classify_types_quantile <- function(df, q) {
  
  theta_cut <- quantile(abs(df$theta), probs = q, na.rm = TRUE)
  
  cuts <- df |>
    summarise(
      d_cut  = quantile(sigma_d,  probs = q, na.rm = TRUE),
      u_cut  = quantile(sigma_u,  probs = q, na.rm = TRUE),
      nu_cut = quantile(sigma_nu, probs = q, na.rm = TRUE),
      pi_cut = quantile(pi,       probs = q, na.rm = TRUE)
    )
  
  df |>
    mutate(
      central_theta    = abs(theta) <= theta_cut,
      low_dispersion   = sigma_d  <= cuts$d_cut,
      low_instability  = sigma_u  <= cuts$u_cut,
      low_idiosyncrasy = sigma_nu <= cuts$nu_cut,
      low_noise        = pi       <= cuts$pi_cut
    ) |>
    mutate(
      type = case_when(
        central_theta & low_dispersion & low_instability & low_idiosyncrasy & low_noise
        ~ "Substantive moderate",
        
        !central_theta & low_dispersion & low_instability & low_idiosyncrasy & low_noise
        ~ "Constrained extremist",
        
        central_theta & !low_dispersion & low_instability & low_noise
        ~ "Cross-pressured",
        
        central_theta & !low_idiosyncrasy & low_noise
        ~ "Idiosyncratic unstable",
        
        central_theta & !low_noise
        ~ "Weakly constrained / noisy",
        
        TRUE ~ "Other"
      ),
      q_def = q
    )
}



# ----------------------------
# Apply for q = 0.25, 0.50, 0.75 and compute type shares
# ----------------------------
qs <- c(0.25, 0.50, 0.75)

type_order <- c(
  "Substantive moderate",
  "Constrained extremist",
  "Cross-pressured",
  "Idiosyncratic unstable",
  "Weakly constrained / noisy",
  "Other"
)

shares_long <- 
  bind_rows(lapply(qs, \(q) classify_types_quantile(respondent_latents, q))) |>
  count(q_def, type) |>
  group_by(q_def) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  mutate(
    type = factor(type, levels = type_order),
    q_def = factor(q_def, levels = qs, labels = paste0("q = ", qs))
  )



# ----------------------------
# Create wide table
# ----------------------------
shares_wide <- shares_long |>
  select(q_def, type, share) |>
  tidyr::pivot_wider(names_from = q_def, values_from = share, values_fill = 0) |>
  arrange(type) |>
  mutate(across(starts_with("q ="), ~ round(.x, 3)))



# ----------------------------
# LaTeX table
# ----------------------------
kable(
  shares_wide,
  format = "latex",
  booktabs = TRUE,
  align = "lrrr",
  caption = "Share of respondent types under alternative quantile-based definitions. Classification is based on respondent-level posterior medians from the hierarchical topic-IRT model. Central ideology and low dispersion, instability, idiosyncrasy, and noise are defined as values below the q-th quantile of the corresponding empirical distributions. Types are assigned according to combinations of these criteria, distinguishing substantively moderate respondents from constrained extremists, cross-pressured, idiosyncratic, and weakly constrained or noisy respondents.") |> cat()



# ----------------------------
# Create joint plots
# ----------------------------

center_stat <- "mean"

# Build the respondent-level dataset
respondent_latents <- posterior_individual |> 
  filter(parameter %in% c("theta","sigma_topic","sigma_u","sigma_nu_person","pi")) |> 
  select(respondent_id, parameter, value = all_of(center_stat)) |> 
  pivot_wider(names_from = parameter, values_from = value) |> 
  rename(
    sigma_d  = sigma_topic,
    sigma_nu = sigma_nu_person
  ) |> 
  select(respondent_id, theta, sigma_d, sigma_u, sigma_nu, pi)

# Define highlight rules
cuts <- respondent_latents |> 
  summarise(
    d_cut = quantile(sigma_d, 0.5, na.rm = TRUE),
    u_cut = quantile(sigma_u, 0.5, na.rm = TRUE),
    nu_cut = quantile(sigma_nu, 0.5, na.rm = TRUE),
    pi_cut = quantile(pi, 0.5, na.rm = TRUE)
  )

respondent_latents <- respondent_latents |> 
  mutate(
    highlight = case_when(
      sigma_d > cuts$d_cut & sigma_u > cuts$u_cut & 
        sigma_nu > cuts$nu_cut & pi > cuts$pi_cut ~ "All",
      sigma_d <= cuts$d_cut & sigma_u <= cuts$u_cut & 
        sigma_nu <= cuts$nu_cut & pi <= cuts$pi_cut ~ "None",
      TRUE ~ "Mixed"
    )
  )

# Long format for faceting
plot_dat <- respondent_latents |> 
  filter(pi <= 0.3) |> 
  pivot_longer(
    cols = c(sigma_d, sigma_u, sigma_nu, pi),
    names_to = "parameter",
    values_to = "value"
  ) |> 
  mutate(
    parameter = factor(
      parameter,
      levels = c("sigma_d", "sigma_u", "sigma_nu", "pi")
    )
  )

param_labels <- c(
  sigma_d = "sigma[d,j]~'(topic-specific dispersion)'",
  sigma_u = "sigma[u,j]~'(temporal instability)'",
  sigma_nu = "sigma[nu,j]~'(item-level idiosyncrasy)'",
  pi = "pi[j]~'(random responses)'"
)


# Faceted plot
fig_faceted <- ggplot(plot_dat, aes(x = theta, y = value)) +
  geom_point(aes(alpha = highlight), size = 1.2) +
  scale_alpha_manual(
    values = c(
      "All" = 1,
      "Mixed" = 0.6,
      "None" = 0.3
    )
  ) +
  scale_x_continuous(limits = c(-1, 1),
                     breaks = c(-1, -0.5, 0, 0.5, 1)) +
  facet_wrap(
    ~ parameter,
    scales = "free_y",
    ncol = 2,
    labeller = as_labeller(param_labels, label_parsed)
  ) +
  labs(
    x = expression(theta[j]~"(global ideology)"),
    y = NULL,
    alpha = "Above average values",
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

fig_faceted

# Save figure
ggsave("figures/fig_faceted.png", fig_faceted, width = 7, height = 5, dpi = 300)


# Parallel coordinates
# Create quartile groups of |theta| on the respondent-level data
respondent_latents_q <- respondent_latents |> 
  filter(pi <= 0.3) |> 
  mutate(
    abs_theta = abs(theta),
    theta_q = ntile(abs_theta, 4),
    theta_q = factor(
      theta_q,
      levels = 1:4,
      labels = c("Q1 (most central)", "Q2", "Q3", "Q4 (most extreme)")
    )
  )

# Ensure dataset has theta_q and add direction
respondent_latents_q <- respondent_latents_q |> 
  mutate(
    abs_theta = abs(theta),
    theta_q = ntile(abs_theta, 4),
    theta_q = factor(
      theta_q,
      levels = 1:4,
      labels = c("Q1 (most central)", "Q2", "Q3", "Q4 (most extreme)")
    ),
    ideology_dir = if_else(theta < 0, "Left-leaning", "Right-leaning"),
    ideology_dir = factor(ideology_dir, levels = c("Left-leaning", "Right-leaning"))
  )

# Build long data for parallel coordinates (standardize within full sample)
pc_dat <- respondent_latents_q |>
  mutate(across(c(theta, sigma_d, sigma_u, sigma_nu, pi), ~ as.numeric(scale(.x)))) |>
  pivot_longer(
    cols = c(theta, sigma_d, sigma_u, sigma_nu, pi),
    names_to = "parameter",
    values_to = "z"
  ) |> 
  mutate(
    parameter = factor(parameter, levels = c("theta","sigma_d","sigma_u","sigma_nu","pi"))
  )

# Plotmath labels for x-axis
x_labs <- c(
  theta = expression(theta[j]),
  sigma_d = expression(sigma[d,j]),
  sigma_u = expression(sigma[u,j]),
  sigma_nu = expression(sigma[nu,j]),
  pi = expression(pi[j])
)

# Plot facet by |theta| quartiles
fig_parallel_faceted <- ggplot(
  pc_dat,
  aes(x = parameter, y = z, group = respondent_id, color = ideology_dir)
  ) +
  geom_line(linewidth = 0.3, alpha = 0.20) +
  scale_x_discrete(labels = x_labs) +
  scale_color_manual(
    values = c(
      "Left-leaning"  = "blue",
      "Right-leaning" = "red"
    )
  ) +
  facet_wrap(~ theta_q, ncol = 2) +
  labs(
    x = NULL,
    y = "Standardized value (z-score)",
    color = "Ideological direction",
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom"
  )

fig_parallel_faceted

# Save figures
ggsave("figures/fig_parallel_faceted.png", fig_parallel_faceted, width = 7, height = 5, dpi = 300)

