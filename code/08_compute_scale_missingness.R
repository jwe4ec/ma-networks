# ---------------------------------------------------------------------------- #
# Compute Scale-Level Missing Data Rates
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./code/01a_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

net_dat_all_to_s6_wide <- read.csv(file = "./data/intermediate/net_dat_all_to_s6_wide.csv")

load("./results/descriptives/res_node_vars_itt_by_cond.RData")
load("./results/descriptives/res_node_vars_compl_by_cond.RData")

# ---------------------------------------------------------------------------- #
# Compute rates of scale-level missingness ----
# ---------------------------------------------------------------------------- #

# Compute number of ITT participants

(N <- length(net_dat_all_to_s6_wide$participant_id)) == 729

# TODO (create "all_item_missingness.txt", likely in "prepare_net_data.R"; see 
# https://github.com/jwe4ec/fy7e6/blob/develop/code/06_further_clean_other_data_and_compute_item_missingness.R
# from main outcomes paper): Define function to compute rate of scale-level missingness for given scale
# based on number of ITT participants (N) and number of time points (J). Note:
# Scale-level missingness can occur due to (a) attrition or (b) endorsing "prefer 
# not to answer" for all items (see "all_item_missingness.txt" for rates of this).





compute_scale_missingness_itt <- function(desc_tbl, scale, N, J) {
  n_cols <- c("n_POSITIVE", "n_FIFTY_FIFTY", "n_NEUTRAL")

  # Restrict to sample size columns for scale and convert to numeric
  
  dat <- desc_tbl[desc_tbl$Measure == scale, n_cols]
  dat <- sapply(dat, as.numeric)
  
  # Compute percentages of scale-level missingness
  
  prop <- 1 - sum(dat, na.rm = TRUE)/(N*J)
  percent <- prop*100
  
  cat(scale, ": ", percent, "%", "\n", sep = "")
}

# TODO (run for BBSIQ too): Run function and write results





missing_rates_path <- "./results/missing_rates/"
dir.create(missing_rates_path)

sink(file = paste0(missing_rates_path, "scale_missingness.txt"))

cat("Percentages of Scale-Level Missingness for Each Scale:", "\n\n")

compute_scale_missingness_itt(res_node_vars_itt_by_cond, "rr_ns_mean",     N, 3)
compute_scale_missingness_itt(res_node_vars_itt_by_cond, "rr_ps_mean_rev", N, 3)

sink()