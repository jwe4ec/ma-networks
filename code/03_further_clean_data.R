# ---------------------------------------------------------------------------- #
# Further clean data
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

# Intermediate clean data

int_cln_dat <- readRDS(file.path(processed_path, "int_cln_dat.rds"))

# ---------------------------------------------------------------------------- #
# Define scale items ----
# ---------------------------------------------------------------------------- #

# OASIS

oa_items <- c("anxious_freq", "anxious_sev", "avoid", "interfere", "interfere_social")

stopifnot(
  length(oa_items) == 5,
  all(oa_items %in% names(int_cln_dat$oa))
)

# RR

rr_neg_thr_items <- names(int_cln_dat$rr)[grep("_NS$", names(int_cln_dat$rr))]
rr_neg_non_items <- names(int_cln_dat$rr)[grep("_NF$", names(int_cln_dat$rr))]
rr_pos_thr_items <- names(int_cln_dat$rr)[grep("_PS$", names(int_cln_dat$rr))]
rr_pos_non_items <- names(int_cln_dat$rr)[grep("_PF$", names(int_cln_dat$rr))]

rr_items <- c(rr_neg_thr_items, rr_neg_non_items, rr_pos_thr_items, rr_pos_non_items)

stopifnot(
  all(sapply(list(rr_neg_thr_items, rr_neg_non_items, rr_pos_thr_items, rr_pos_non_items), length) == 9),
  length(rr_items) == 36,
  all(rr_items %in% names(int_cln_dat$rr))
)

# BBSIQ

bbsiq_neg_int_items <- c("breath_suffocate", "vision_illness", "lightheaded_faint", "chest_heart",
                         "heart_wrong", "confused_outofmind", "dizzy_ill")
bbsiq_neg_ext_items <- c("visitors_bored", "shop_irritating", "smoke_house", "friend_incompetent",
                         "jolt_burglar", "party_boring", "urgent_died")
bbsiq_neg_items <- c(bbsiq_neg_int_items, bbsiq_neg_ext_items)

bbsiq_ben_int_items <- c("breath_flu", "breath_physically", "vision_glasses", "vision_strained",
                         "lightheaded_eat", "lightheaded_sleep", "chest_indigestion", "chest_sore",
                         "heart_active", "heart_excited", "confused_cold", "confused_work",
                         "dizzy_ate", "dizzy_overtired")
bbsiq_ben_ext_items <- c("visitors_engagement", "visitors_outstay", "shop_bored", "shop_concentrating",
                         "smoke_cig", "smoke_food", "friend_helpful", "friend_moreoften", "jolt_dream",
                         "jolt_wind", "party_hear", "party_preoccupied", "urgent_bill", "urgent_junk")
bbsiq_ben_items <- c(bbsiq_ben_int_items, bbsiq_ben_ext_items)

bbsiq_items <- c(bbsiq_neg_items, bbsiq_ben_items)

stopifnot(
  all(sapply(list(bbsiq_neg_int_items, bbsiq_neg_ext_items), length) == 7),
  length(bbsiq_neg_items) == 14,
  
  all(sapply(list(bbsiq_ben_int_items, bbsiq_ben_ext_items), length) == 14),
  length(bbsiq_ben_items) == 28,
  
  all(bbsiq_items %in% names(int_cln_dat$bbsiq))
)

# Credibility

credibility_items <- c("important", "logical", "recommendable")

stopifnot(
  length(credibility_items) == 3,
  all(credibility_items %in% names(int_cln_dat$credibility))
)

# Collect items in list

items <- list(oa            = oa_items, 
              rr            = rr_items, 
              rr_neg_thr    = rr_neg_thr_items, 
              rr_neg_non    = rr_neg_non_items, 
              rr_pos_thr    = rr_pos_thr_items, 
              rr_pos_non    = rr_pos_non_items, 
              bbsiq         = bbsiq_items, 
              bbsiq_neg     = bbsiq_neg_items, 
              bbsiq_neg_int = bbsiq_neg_int_items, 
              bbsiq_neg_ext = bbsiq_neg_ext_items, 
              bbsiq_ben     = bbsiq_ben_items, 
              bbsiq_ben_int = bbsiq_ben_int_items, 
              bbsiq_ben_ext = bbsiq_ben_ext_items)

# ---------------------------------------------------------------------------- #
# Compute average item scores for RR and BBSIQ ----
# ---------------------------------------------------------------------------- #

cln_dat <- int_cln_dat

# RR

cln_dat$rr$rr_neg_thr_mean <- rowMeans(cln_dat$rr[items$rr_neg_thr], na.rm = TRUE)
cln_dat$rr$rr_pos_thr_mean <- rowMeans(cln_dat$rr[items$rr_pos_thr], na.rm = TRUE)

## Reverse positive threat bias scale to reflect lack of positive threat bias

cln_dat$rr$rr_pos_thr_mean_rev <- 3 - cln_dat$rr$rr_pos_thr_mean

# BBSIQ

cln_dat$bbsiq$bbsiq_neg_mean <- rowMeans(cln_dat$bbsiq[bbsiq_neg_items], na.rm = TRUE)

# ---------------------------------------------------------------------------- #
# Order CBM-I condition levels ----
# ---------------------------------------------------------------------------- #

cln_dat$participant$cbmCondition <- factor(cln_dat$participant$cbmCondition,
  levels = c("POSITIVE", "FIFTY_FIFTY", "NEUTRAL"))

stopifnot(all(!is.na(cln_dat$participant$cbmCondition)))

# ---------------------------------------------------------------------------- #
# Export data and helper items list ----
# ---------------------------------------------------------------------------- #

saveRDS(cln_dat, file.path(processed_path, "cln_dat.rds"))

helper_path <- file.path("data", "helper")
dir.create(helper_path)

saveRDS(items, file.path(helper_path, "items.rds"))