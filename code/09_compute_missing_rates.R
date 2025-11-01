# ---------------------------------------------------------------------------- #
# Compute Missing Data Rates
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

processed_path <- file.path("data", "processed")
desc_path      <- file.path("results", "descriptives")

# For item-level missingness, clean data and helper items list

cln_dat <- readRDS(file.path(processed_path, "cln_dat.rds"))

items <- readRDS(file.path("data", "helper", "items.rds"))

# For scale-level missingness, overall network dataset and descriptives table

net_dat_all_to_s6 <- readRDS(file.path(processed_path, "net_dat_all_to_s6.rds"))

desc_tbl_by_cond_itt_any_net <- readRDS(file.path(desc_path, "desc_tbl_by_cond_itt_any_net.rds"))

# ---------------------------------------------------------------------------- #
# Compute rates of item-level missingness for ITT participants ----
# ---------------------------------------------------------------------------- #

# Define function to compute percentage of scale scores computed with at least 
# one item missing for given outcome

compute_some_item_missingness <- function(dat, outcome, items, time_pts = NULL) {
  if (!is.null(time_pts)) dat <- dat[dat$session_only %in% time_pts, ]

  denom <- sum(!is.na(dat[[outcome]]))
  
  rows_at_least_one_item_na <- rowSums(is.na(dat[items])) > 0
  rows_all_items_na         <- rowSums(!is.na(dat[items])) == 0
  
  numer <- nrow(dat[rows_at_least_one_item_na & !rows_all_items_na, ])
  
  prop <- numer / denom
  perc <- prop * 100
  
  cat(outcome, " at ", paste(time_pts, collapse = ", "), ": ", 
      perc, "%", "\n", sep = "")
}

# Run function for ITT participants and write results

time_pts <- c("PRE", paste0("SESSION", c(3, 6)))

missing_rates_path <- file.path("results", "missing_rates")
dir.create(missing_rates_path)

sink(file.path(missing_rates_path, "some_item_missingness.txt"))

cat("Percentages of Scale Scores Computed With At Least One Item Missing:", "\n\n")

compute_some_item_missingness(cln_dat$rr,    "rr_neg_thr_mean",     items$rr_neg_thr, time_pts)
compute_some_item_missingness(cln_dat$rr,    "rr_pos_thr_mean_rev", items$rr_pos_thr, time_pts)
compute_some_item_missingness(cln_dat$bbsiq, "bbsiq_neg_mean",      items$bbsiq_neg,  time_pts)

sink()

# Define function to compute number of scale scores missing due to endorsements
# of "prefer not to answer" for all items

compute_all_item_missingness <- function(dat, outcome, items, time_pts = NULL) {
  if (!is.null(time_pts)) dat <- dat[dat$session_only %in% time_pts, ]
  
  rows_all_items_na     <- rowSums(!is.na(dat[items])) == 0
  num_rows_all_items_na <- sum(rows_all_items_na)
  
  num_missing_outcome <- sum(is.na(dat[[outcome]]))
  
  if (num_rows_all_items_na != num_missing_outcome) {
    warning(paste0("For ", outcome, ", ",
                   "number of rows with NA for all items does not equal ",
                   "number of scale scores with values of NA", "\n"))
  }
  
  cat(outcome, " at ", paste(time_pts, collapse = ", "), ": ",
      num_rows_all_items_na, "\n", sep = "")
  
  print(table(dat[rows_all_items_na, "session_only"]))
  cat("\n", "-----", "\n\n")
}

# Run function for ITT participants and write results

sink(file.path(missing_rates_path, "all_item_missingness.txt"))

cat("Number of Scale Scores Missing Due to 'Prefer Not to Answer' for All Items:", "\n\n")

compute_all_item_missingness(cln_dat$rr,    "rr_neg_thr_mean",     items$rr_neg_thr, time_pts)
compute_all_item_missingness(cln_dat$rr,    "rr_pos_thr_mean_rev", items$rr_pos_thr, time_pts)
compute_all_item_missingness(cln_dat$bbsiq, "bbsiq_neg_mean",      items$bbsiq_neg,  time_pts)

sink()

# ---------------------------------------------------------------------------- #
# Compute rates of scale-level missingness for ITT participants ----
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

sink(file.path(missing_rates_path, "scale_missingness.txt"))

cat("Percentages of Scale-Level Missingness for Each Scale:", "\n\n")

compute_scale_missingness_itt(desc_tbl_by_cond_itt_any_net, "rr_neg_thr_mean",     N, 3)
compute_scale_missingness_itt(desc_tbl_by_cond_itt_any_net, "rr_pos_thr_mean_rev", N, 3)
compute_scale_missingness_itt(desc_tbl_by_cond_itt_any_net, "bbsiq_neg_mean",      N, 3)

sink()