# ---------------------------------------------------------------------------- #
# Prepare Network Data
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

groundhog_day <- version_control_temporal()

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

mrg_oa      <- read.csv("./data/intermediate/merge_oa_rest2.csv")
participant <- read.csv("./data/intermediate/participant_raw_rest2.csv")
mrg_rr      <- read.csv("./data/intermediate/merge_rr_rest2.csv")

mrg_dat <- list(oa = mrg_oa,
                participant = participant,
                rr = mrg_rr)

# ---------------------------------------------------------------------------- #
# Restrict data for analysis ----
# ---------------------------------------------------------------------------- #

# Identify columns that will be manifest nodes. Note: "rr_ns_mean" and "rr_ps_mean" 
# are mean negative and mean positive threat-related interpretation ratings

oa_node_cols <- c("anxious_freq", "anxious_sev", "avoid", "interfere", "interfere_social")
rr_node_cols <- c("rr_ns_mean", "rr_ps_mean")

# TODO: Confirm that clean data loaded in prior script contains only ITT participants





# Use full outer join to merge all node columns into one table

net_dat <- merge(mrg_dat$oa[, c("participant_id", "session_only", oa_node_cols)],
                 mrg_dat$rr[, c("participant_id", "session_only", rr_node_cols)],
                 by = c("participant_id", "session_only"),
                 all = TRUE)

row_all_nodes_na_participant_ids <- 
  net_dat[rowSums(is.na(net_dat[, c(oa_node_cols, rr_node_cols)])) == 
              ncol(net_dat[, c(oa_node_cols, rr_node_cols)]), "participant_id"]

# View(net_dat[net_dat$participant_id %in% row_all_nodes_na_participant_ids, ])

# TODO: Remove participant 583, who has no data on any nodes

net_dat <- net_dat[net_dat$participant_id != 583, ]





# Add condition

net_dat <- merge(net_dat, 
                 mrg_dat$participant[, c("participant_id", "cbmCondition", "prime")],
                 by = "participant_id",
                 all.x = TRUE)

# TODO: For now, remove participants with no condition

table(net_dat$cbmCondition, useNA = "always")
table(net_dat$prime, useNA = "always")

net_dat <- net_dat[!is.na(net_dat$cbmCondition), ]





# Restrict time points in another dataset to "PRE" through "SESSION6", given that 
# OASIS was assessed at every time point (excluding "SESSION7" and "SESSION8",
# following main outcomes paper; Ji et al., 2021)

net_dat_all_to_s6 <- net_dat[net_dat$session_only %in% c("PRE", paste0("SESSION", 1:6)), ]

# ---------------------------------------------------------------------------- #
# Create reverse-coded version of positive threat bias scale ----
# ---------------------------------------------------------------------------- #

net_dat_all_to_s6$rr_ps_mean_rev <- 3 - net_dat_all_to_s6$rr_ps_mean

# ---------------------------------------------------------------------------- #
# Restructure data ----
# ---------------------------------------------------------------------------- #

# Save long format data for search for auxiliary variables, etc.

save(net_dat_all_to_s6, file = "./data/intermediate/net_dat_all_to_s6.RData")

# Convert to wide format

net_dat_all_to_s6_wide <- reshape(net_dat_all_to_s6, 
                                  direction = "wide",
                                  idvar = "participant_id",
                                  timevar = "session_only",
                                  v.names = c(oa_node_cols, rr_node_cols, "rr_ps_mean_rev"))

# ---------------------------------------------------------------------------- #
# Export data ----
# ---------------------------------------------------------------------------- #

write.csv(net_dat_all_to_s6_wide, 
          file = "./data/intermediate/net_dat_all_to_s6_wide.csv",
          row.names = FALSE)