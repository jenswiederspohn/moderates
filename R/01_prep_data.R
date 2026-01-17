#--------------------------------------------------------------------
# Where are the Moderates?
#--------------------------------------------------------------------
# Data Preparation
#--------------------------------------------------------------------
# Author: Jens Wiederspohn
#--------------------------------------------------------------------



rm(list = ls(all = TRUE))


# ----------------------------
# Load Packages
# ----------------------------

library(tidyverse)
library(haven)
library(knitr)
library(kableExtra)



# ----------------------------
# Import Data
# ----------------------------

recontact_raw <- read_dta("data/CES 2020 - 2024 Panel Survey/merged_recontact_2024.dta")



# ----------------------------
# Define Helpers
# ----------------------------

# Convert items to a 0/1 "support" indicator:
# support = 1, oppose = 0 (NA if not a valid response)
to_support01 <- function(x) {
  xn <- suppressWarnings(as.integer(as.character(x)))
  case_when(
    xn == 1L ~ 1L,   # support
    xn == 2L ~ 0L,   # oppose
    TRUE ~ NA_integer_
  )
}

# Produce y (1 = conservative, 0 = liberal)
support_to_y <- function(support01, conservative_when_support) {
  case_when(
    is.na(support01) ~ NA_integer_,
    conservative_when_support ~ support01,
    !conservative_when_support ~ (1L - support01)
  )
}



# ----------------------------
# Declare topic–item specification
# ----------------------------

item_spec <- tribble(
  ~topic, ~item, ~conservative_when_support, ~v2024, ~v2022, ~v2020,
  
  # --- Abortion
  "Abortion", "Always allow abortion as a matter of choice",                
  FALSE, "RC24_332a", "CC22_332a", "CC20_332a",
  "Abortion", "Permit only rape/incest/life endangerment",                  
  TRUE, "RC24_332b", "CC22_332b", "CC20_332b",
  "Abortion", "Prohibit abortions after the 20th week",                     
  TRUE, "RC24_332c", "CC22_332c", "CC20_332c",
  "Abortion", "Allow employers to decline abortion coverage",               
  TRUE, "RC24_332d", "CC22_332d", "CC20_332d",
  "Abortion", "Prohibit federal funds for any abortion",                    
  TRUE, "RC24_332e", "CC22_332e", "CC20_332e",
  "Abortion", "Make abortions illegal in all circumstances",                
  TRUE, "RC24_332f", "CC22_332f", "CC20_332f",
  "Abortion", "Prohibit states from requiring hospital-only abortions",     
  FALSE, "RC24_332g", "CC22_332g", "CC20_332g",
  "Abortion","Expand access to abortion (2024 only)",                      
  FALSE, "CC24_324d", NA, NA,
  
  # --- Environment
  "Environment", "EPA regulate CO2 emissions",                              
  FALSE, "RC24_333a", "CC22_333a", "CC20_333a",
  "Environment", "State minimum renewables even if prices rise",           
  FALSE, "RC24_333b", "CC22_333b", "CC20_333b",
  "Environment", "Strengthen EPA enforcement even if it costs jobs",        
  FALSE, "RC24_333c", "CC22_333c", "CC20_333c",
  "Environment", "Raise fuel efficiency to 54.5 mpg by 2025",               
  FALSE, "RC24_333d", "CC22_333d", "CC20_333d",
  "Environment", "Increase fossil fuel production / boost LNG exports (2020 missing)",     
  TRUE, "RC24_333e", "CC22_333e", NA,
  "Environment", "Require 20% electricity from renewables (2024 only)",     
  FALSE, "CC24_326b", NA, NA,
  "Environment", "Halt new oil and gas leases on federal lands (2024 only)",
  FALSE, "CC24_326e", NA, NA,
  "Environment", "Prevent the government from banning gas stoves (2024 only)",
  TRUE,"CC24_326f", NA, NA,
  
  # --- Gun control
  "Gun control", "Prohibit publishing names/addresses of gun owners",       
  TRUE, "RC24_330a", "CC22_330a", "CC20_330a",
  "Gun control","Ban assault rifles",                                      
  FALSE, "RC24_330b", "CC22_330b", "CC20_330b",
  "Gun control", "Make it easier to obtain concealed-carry permit",         
  TRUE, "RC24_330c", "CC22_330c", "CC20_330c",
  "Gun control", "Federal funding for red-flag removal (2020 missing)",     
  FALSE, "RC24_330d", "CC22_330d", NA,
  "Gun control", "Improve background checks (juvenile/mental health) (2020 missing)", FALSE,
  "RC24_330e", "CC22_330e", NA,
  "Gun control", "Allow teachers/school officials to carry guns in schools", 
  TRUE, "RC24_330f", "CC22_330f", NA,
  "Gun control","Require background checks on all gun sales (2024 only)",  
  FALSE, "CC24_321c", NA, NA,
  
  # --- Health care
  "Health care", "Expand Medicare to single-payer for all",                 
  FALSE, "RC24_327a", "CC22_327a", "CC20_327a",
  "Health care", "Government negotiate drug prices (international cap)",    
  FALSE, "RC24_327b", "CC22_327b", "CC20_327b",
  "Health care", "Repeal the entire Affordable Care Act",                   
  TRUE, "RC24_327c", "CC22_327c", "CC20_327d",
  "Health care", "Allow states to import prescription drugs",               
  FALSE, "RC24_327d", "CC22_327d", "CC20_327f",
  "Health care", "Lower Medicare eligibility age from 65 to 50",                   
  FALSE, "CC20_327c_24", "CC20_327c_22", "CC20_327c_20",
  "Health care", "Restore ACA individual mandate",                          
  FALSE, "CC20_327e_24", "CC20_327e_22", "CC20_327e_20",
  
  # --- Immigration
  "Immigration", "Legal status for undocumented with 3y work/taxes, no felonies",
  FALSE, "CC24_331a", "CC22_331a", "CC20_331a",
  "Immigration", "Increase number of border patrols",                       
  TRUE, "CC24_331b", "CC22_331b", "CC20_331b",
  "Immigration", "Reduce legal immigration by 50% (end lottery/family)",     
  TRUE, "RC24_331c", "CC22_331c", "CC20_331d",
  "Immigration", "Increase border security spending + wall ($25B)",         
  TRUE, "RC24_331d", "CC22_331d", "CC20_331e",
  "Immigration", "Withhold funds from police not reporting undocumented",   
  TRUE, "CC20_331c_24", "CC20_331c_22", "CC20_331c_20",
  "Immigration", "Dreamers: permanent resident and pathway to citizenship",   
  FALSE, "CC20_350e_24", "CC20_350e_22", "CC20_350e_20",
  "Immigration", "Build a wall between U.S. and Mexico (2024 only)",        
  TRUE, "CC24_323c", NA, NA,
  
  # --- Policing policies
  "Policing policies", "Eliminate mandatory minimums for non-violent drug offenders",
  FALSE, "RC24_334a", "CC22_334a", "CC20_334a",
  "Policing policies", "Require police body cameras",                       
  FALSE, "RC24_334b", "CC22_334b", "CC20_334b",
  "Policing policies", "Increase police on street by 10% even if fewer services",
  TRUE, "RC24_334c", "CC22_334c", "CC20_334c",
  "Policing policies", "Decrease police on street by 10% + increase other services",
  FALSE, "RC24_334d", "CC22_334d", "CC20_334d",
  "Policing policies", "Ban police choke holds",                            
  FALSE, "RC24_334e", "CC22_334e", "CC20_334e",
  "Policing policies", "National registry of investigated/disciplined police",
  FALSE, "RC24_334f", "CC22_334f", "CC20_334f",
  "Policing policies", "End DoD surplus military equipment to police",       
  FALSE, "RC24_334g", "CC22_334g", "CC20_334g",
  "Policing policies", "Allow lawsuits if officer 'recklessly disregarded' rights",
  FALSE,"RC24_334h", "CC22_334h", "CC20_334h",
  
  # --- Tax rates (2024 only)
  "Tax rates", "Extend 2017 tax cuts",                                     
  TRUE, "CC24_341a", NA, NA,
  "Tax rates", "Raise corporate tax from 21% to 28%",                            
  FALSE, "CC24_341b", NA, NA,
  "Tax rates", "Raise top rate ($400k+) to 35%",
  FALSE, "CC24_341c", NA, NA,
  
  # --- Trade policy (2020 only)
  "Trade policy", "Tariffs on $200B of goods imported from China",          
  TRUE, NA, NA, "CC20_338a",
  "Trade policy", "25% steel / 10% aluminum tariffs EXCEPT Canada/Mexico",  
  TRUE, NA, NA, "CC20_338b",
  "Trade policy", "25% steel / 10% aluminum tariffs INCLUDING Canada/Mexico",
  TRUE, NA, NA, "CC20_338c",
  "Trade policy", "Increase tariffs on European aircraft/agricultural products", 
  TRUE, NA, NA, "CC20_338d"
)



# ----------------------------
# Wide to long, recode y, and create indices for Stan
# ----------------------------

id_var <- "caseid"

# Keep only required columns
all_vars <- item_spec |>
  select(v2024, v2022, v2020) |>
  pivot_longer(everything(), values_to = "var") |>
  filter(!is.na(var)) |>
  distinct(var) |>
  pull(var)

recontact <- recontact_raw |>
  select(all_of(id_var), any_of(all_vars))

# Pivot longer with a wave/year indicator
spec_long <- item_spec |>
  pivot_longer(
    cols = c(v2024, v2022, v2020),
    names_to = "wave_col",
    values_to = "var"
  ) |>
  filter(!is.na(var)) |>
  mutate(
    year = case_when(
      wave_col == "v2020" ~ 2020L,
      wave_col == "v2022" ~ 2022L,
      wave_col == "v2024" ~ 2024L
    )
  ) |>
  select(topic, item, conservative_when_support, year, var)

# Recode long data
dat_long <- recontact |>
  pivot_longer(
    cols = -all_of(id_var),
    names_to = "var",
    values_to = "raw"
  ) |>
  left_join(spec_long, by = "var") |>
  filter(!is.na(topic), !is.na(item), !is.na(year)) |>
  mutate(
    support01 = to_support01(raw),
    y = support_to_y(support01, conservative_when_support)
  ) |>
  filter(!is.na(y)) |>
  transmute(
    respondent_id = .data[[id_var]],
    year,
    topic,
    item,
    y = as.integer(y)
  )

# Save prepared data
write_dta(dat_long, "data/CES 2020 - 2024 Panel Survey/recontact_prep.dta")



# ---------------------------------
# Create topic-year summary LaTeX table
# ---------------------------------

# Item-level conservative shares
item_level <- dat_long |> 
  group_by(topic, year, item) |> 
  summarise(item_share = mean(y, na.rm = TRUE), .groups = "drop")

# Topic-year summary
topic_year_summary <- dat_long |> 
  group_by(topic, year) |> 
  summarise(
    n_items = n_distinct(item),
    share_conservative = mean(y, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    item_level %>%
      group_by(topic, year) |> 
      summarise(
        max_share_item = max(item_share, na.rm = TRUE),
        min_share_item = min(item_share, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("topic", "year")
  )

# Ensure all topic x year combos exist
all_years <- sort(unique(dat_long$year))

topic_year_long <- topic_year_summary |>
  complete(
    topic,
    year = all_years,
    fill = list(
      n_items = 0L,
      share_conservative = NA_real_,
      max_share_item = NA_real_,
      min_share_item = NA_real_
    )
  ) |>
  arrange(topic, year) |> 
  mutate(
    share_conservative = round(share_conservative, 3),
    max_share_item     = round(max_share_item, 3),
    min_share_item     = round(min_share_item, 3)
  ) |> 
  group_by(topic) |> 
  mutate(
    # show topic only once (middle row of the 3-year block)
    topic_display = if_else(row_number() == 2L, paste0("\\textbf{", topic, "}"), "")
  ) |> 
  ungroup()

# Final table data
tab_df <- topic_year_long |> 
  select(
    topic, topic_display, year, n_items,
    share_conservative, min_share_item, max_share_item
  )

# Build LaTeX table
kbl <- kable(
  tab_df |>  select(topic_display, year, n_items, share_conservative, min_share_item, max_share_item),
  format = "latex",
  booktabs = TRUE,
  linesep = "",         
  escape = FALSE,       
  align = c("l", "r", "r", "r", "r", "r"),
  # second header row
  col.names = c("Topic", "Year", "Items", "Mean", "Min", "Max"),
  caption = "Item Coverage and Conservative Response Shares by Topic and Year",
  digits = 3
  ) |> 
  # top header row
  add_header_above(c(" " = 3, "Share Conservative" = 3), escape = FALSE) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 9
  )

# Add horizontal separators between topics
# Compute last row index for each topic
topic_end_rows <- tab_df %>%
  group_by(topic) %>%
  summarise(last_row = dplyr::n(), .groups = "drop") %>%
  mutate(cum_last = cumsum(last_row)) %>%
  pull(cum_last)

# Add a midrule after each topic block except the last
for (r in head(topic_end_rows, -1)) {
  kbl <- kbl %>% row_spec(r, hline_after = TRUE)
}

kbl



# ------------------------------------------------------------
# LaTeX tables: item inventory by wave/year
# ------------------------------------------------------------

make_item_inventory_table <- function(item_spec, year_col, year_label) {
  
  spec_year <- item_spec |>
    select(topic, item, conservative_when_support, var = all_of(year_col)) |>
    filter(!is.na(var)) |>
    mutate(
      conservative_code = if_else(
        conservative_when_support,
        "Support",
        "Oppose"
      )
    ) |>
    arrange(topic, item) |>
    group_by(topic) |>
    mutate(
      topic_display = if_else(
        row_number() == 1L,
        paste0("\\textbf{", topic, "}"),
        ""
      )
    ) |>
    ungroup()
  
  tab_df <- spec_year |>
    select(topic, topic_display, item, conservative_code)
  
  kbl <- kable(
    tab_df |>
      select(topic_display, item, conservative_code),
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    escape = FALSE,
    align = c("l", "l", "l"),
    col.names = c("Topic", "Item summary", "Conservative"),
    caption =
      paste0(
        "CES policy item inventory (", year_label, "). The table lists each topic and item summary ",
        "and indicates whether the conservative policy position corresponds to supporting (1) or ",
        "opposing (2) the item."
      )
  ) |>
    kable_styling(
      latex_options = c("hold_position"),
      font_size = 9
    ) |>
    column_spec(1, width = "3cm") |>
    column_spec(2, width = "8.0cm") |>
    column_spec(3, width = "3.0cm")
  
  # Horizontal separators between topic blocks
  topic_end_rows <- tab_df |>
    count(topic) |>
    mutate(cum_last = cumsum(n)) |>
    pull(cum_last)
  
  for (r in head(topic_end_rows, -1)) {
    kbl <- kbl |> row_spec(r, hline_after = TRUE)
  }
  
  kbl
}


# Create the three LaTeX tables (2020 / 2022 / 2024)
tbl_items_2020 <- make_item_inventory_table(item_spec, "v2020", "2020")
tbl_items_2022 <- make_item_inventory_table(item_spec, "v2022", "2022")
tbl_items_2024 <- make_item_inventory_table(item_spec, "v2024", "2024")

tbl_items_2020
tbl_items_2022
tbl_items_2024

