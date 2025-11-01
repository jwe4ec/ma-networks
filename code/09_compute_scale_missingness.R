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
# Check correct R version and load packages ----
# ---------------------------------------------------------------------------- #

# Load custom functions

source(file.path("code", "01a_define_functions.R"))

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

# Overall network dataset

net_dat_all_to_s6 <- readRDS(file.path("data", "processed", "net_dat_all_to_s6.rds"))

# Descriptives table for ITT participants

desc_tbl_by_cond_itt_any_net <- readRDS(file.path("results", "descriptives", 
                                                  "desc_tbl_by_cond_itt_any_net.rds"))

# ---------------------------------------------------------------------------- #
# Compute rates of scale-level missingness ----
# ---------------------------------------------------------------------------- #

# Compute number of ITT participants

N <- length(unique(net_dat_all_to_s6$participant_id))
stopifnot(N == 807)

# Define function to compute rate of scale-level missingness for given scale 
# based on number of ITT participants (N) and number of time points (J)
# - Note: Scale-level missingness can occur when (a) the scale was not administered 
#   (e.g., due to attrition or study error), (b) the data were lost (e.g., due to a 
#   server issue), or (c) the participant selected “prefer not to answer” for all 
#   items (see "all_item_missingness.txt" for rates)

compute_scale_missingness_itt <- function(desc_tbl, scale, N, J) {
  n_cols <- c("n_POSITIVE", "n_FIFTY_FIFTY", "n_NEUTRAL")

  # Restrict to sample size columns for scale and convert to numeric
  
  dat <- desc_tbl[desc_tbl$Measure == scale, n_cols]
  dat <- sapply(dat, as.numeric)
  
  # Compute percentages of scale-level missingness
  
  prop <- 1 - sum(dat, na.rm = TRUE) / (N * J)
  perc <- prop * 100
  
  cat(scale, ": ", perc, "%", "\n", sep = "")
}

# Run function and write results

missing_rates_path <- file.path("results", "missing_rates")
dir.create(missing_rates_path)

sink(file.path(missing_rates_path, "scale_missingness.txt"))

cat("Percentages of Scale-Level Missingness for Each Scale:", "\n\n")

compute_scale_missingness_itt(desc_tbl_by_cond_itt_any_net, "rr_neg_thr_mean",     N, 3)
compute_scale_missingness_itt(desc_tbl_by_cond_itt_any_net, "rr_pos_thr_mean_rev", N, 3)
compute_scale_missingness_itt(desc_tbl_by_cond_itt_any_net, "bbsiq_neg_mean",      N, 3)

sink()