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
# Check correct R version and load packages ----
# ---------------------------------------------------------------------------- #

# Load custom functions

source(file.path("code", "01a_define_functions.R"))

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import data and helper items list ----
# ---------------------------------------------------------------------------- #

processed_path <- file.path("data", "processed")
helper_path    <- file.path("data", "helper")

cln_dat <- readRDS(file.path(processed_path, "cln_dat.rds"))

items <- readRDS(file.path(helper_path, "items.rds"))

# ---------------------------------------------------------------------------- #
# Define columns that will be manifest nodes and collect in list ----
# ---------------------------------------------------------------------------- #

oa_node_cols    <- items$oa
rr_node_cols    <- c("rr_neg_thr_mean", "rr_pos_thr_mean_rev")
bbsiq_node_cols <- "bbsiq_neg_mean"

nodes <- list(oa     = oa_node_cols,
              rr     = rr_node_cols,
              bbsiq  = bbsiq_node_cols,
              rr_net = c(oa_node_cols, rr_node_cols),
              bb_net = c(oa_node_cols, bbsiq_node_cols))

# ---------------------------------------------------------------------------- #
# Merge data ----
# ---------------------------------------------------------------------------- #

# Use full outer join to merge all node columns. Create separate datasets for RR 
# network analyses and BBSIQ network analyses and another overall dataset with 
# both RR and BBSIQ data for reporting raw means and SDs, etc.

index_cols <- c("participant_id", "session_only")

oa_node_dat <- cln_dat$oa[c(index_cols, oa_node_cols)]
rr_node_dat <- cln_dat$rr[c(index_cols, rr_node_cols)]
bb_node_dat <- cln_dat$bbsiq[c(index_cols, bbsiq_node_cols)]

net_dat_rr <- merge(oa_node_dat, rr_node_dat, by = index_cols, all = TRUE)
net_dat_bb <- merge(oa_node_dat, bb_node_dat, by = index_cols, all = TRUE)
net_dat    <- merge(net_dat_rr,  bb_node_dat, by = index_cols, all = TRUE)

# ---------------------------------------------------------------------------- #
# Add condition columns ----
# ---------------------------------------------------------------------------- #

condition_cols <- c("cbmCondition", "prime")
condition_dat  <- cln_dat$participant[c("participant_id", condition_cols)]

net_dat_rr <- merge(net_dat_rr, condition_dat, by = "participant_id", all.x = TRUE)
net_dat_bb <- merge(net_dat_bb, condition_dat, by = "participant_id", all.x = TRUE)
net_dat    <- merge(net_dat,    condition_dat, by = "participant_id", all.x = TRUE)

# ---------------------------------------------------------------------------- #
# Remove participants with no data on any nodes ----
# ---------------------------------------------------------------------------- #

# Define function to get participant IDs with NA for all nodes in at least one row

get_pids_any_row_na <- function(net_dat, node_cols) {
  unique(net_dat$participant_id[rowSums(is.na(net_dat[node_cols])) == length(node_cols)])
}

# Define function to get participant IDs with NA for all nodes in all rows

get_pids_all_rows_na <- function(net_dat, node_cols) {
  net_dat_split <- split(net_dat, net_dat$participant_id)
  
  all_rows_na_mask <- sapply(net_dat_split, function(part_net_dat) {
    all(is.na(part_net_dat[node_cols]))
  })
  
  pids_all_rows_na <- as.integer(names(net_dat_split)[all_rows_na_mask])
  
  return(pids_all_rows_na)
}

# Run functions for network-specific datasets

pids_any_row_na_net_dat_rr <- get_pids_any_row_na(net_dat_rr, nodes$rr_net)
pids_any_row_na_net_dat_bb <- get_pids_any_row_na(net_dat_bb, nodes$bb_net)

# View(net_dat_rr[net_dat_rr$participant_id %in% pids_any_row_na_net_dat_rr, ])
# View(net_dat_bb[net_dat_bb$participant_id %in% pids_any_row_na_net_dat_bb, ])

pids_all_rows_na_net_dat_rr <- get_pids_all_rows_na(net_dat_rr, nodes$rr_net)
pids_all_rows_na_net_dat_bb <- get_pids_all_rows_na(net_dat_bb, nodes$bb_net)

# View(net_dat_rr[net_dat_rr$participant_id %in% pids_all_rows_na_net_dat_rr, ])

stopifnot(
  pids_all_rows_na_net_dat_rr == 583,
  length(pids_all_rows_na_net_dat_bb) == 0
)

# Remove participant 583 from RR network dataset (no data on any nodes)

net_dat_rr <- net_dat_rr[net_dat_rr$participant_id != 583, ]

# ---------------------------------------------------------------------------- #
# Restrict to time points through Session 6 ----
# ---------------------------------------------------------------------------- #

# Restrict to time points "PRE" through "SESSION6" (excluding "SESSION7", "SESSION8",
# and "POST"; following main outcomes paper; Ji et al., 2021)

sessions_to_keep <- c("PRE", paste0("SESSION", 1:6))

net_dat_rr_all_to_s6 <- net_dat_rr[net_dat_rr$session_only %in% sessions_to_keep, ]
net_dat_bb_all_to_s6 <- net_dat_bb[net_dat_bb$session_only %in% sessions_to_keep, ]
net_dat_all_to_s6    <- net_dat[net_dat$session_only       %in% sessions_to_keep, ]

# ---------------------------------------------------------------------------- #
# Restructure network-specific datasets ----
# ---------------------------------------------------------------------------- #

# Define function to convert to wide format and check that columns for all nodes
# and time points are present

convert_to_wide <- function(net_dat_long, node_cols, sessions) {
  net_dat_wide <- reshape(net_dat_long,
                          direction = "wide",
                          idvar     = "participant_id",
                          timevar   = "session_only",
                          v.names   = node_cols)
  
  node_cols_wide <- paste0(rep(node_cols, each = length(sessions)), ".", sessions)
  
  stopifnot(all(node_cols_wide %in% names(net_dat_wide)))
  
  return(net_dat_wide)
}

# Run function

net_dat_rr_all_to_s6_wide <- convert_to_wide(net_dat_rr_all_to_s6, nodes$rr_net, sessions_to_keep)
net_dat_bb_all_to_s6_wide <- convert_to_wide(net_dat_bb_all_to_s6, nodes$bb_net, sessions_to_keep)

# ---------------------------------------------------------------------------- #
# Compute indicator of complete data across baseline, Session 3, and Session 6 ----
# ---------------------------------------------------------------------------- #

# Define function

compute_complete_bl_s3_s6 <- function(net_dat_wide, node_cols) {
  target_waves <- c("PRE", "SESSION3", "SESSION6")
  
  target_cols <- paste0(rep(node_cols, each = length(target_waves)), ".", target_waves)
  
  net_dat_wide$complete_bl_s3_s6 <- as.integer(complete.cases(net_dat_wide[target_cols]))
  
  return(net_dat_wide)
}

# Run function

net_dat_rr_all_to_s6_wide <- compute_complete_bl_s3_s6(net_dat_rr_all_to_s6_wide, nodes$rr_net)
net_dat_bb_all_to_s6_wide <- compute_complete_bl_s3_s6(net_dat_bb_all_to_s6_wide, nodes$bb_net)

# ---------------------------------------------------------------------------- #
# Add analysis sample indicators to long-format datasets ----
# ---------------------------------------------------------------------------- #

# Add indicators for RR and BBSIQ network ITT samples to overall dataset

net_dat_all_to_s6$itt_rr_net <- ifelse(net_dat_all_to_s6$participant_id %in% 
                                         net_dat_rr_all_to_s6_wide$participant_id, 1, 0)
net_dat_all_to_s6$itt_bb_net <- ifelse(net_dat_all_to_s6$participant_id %in% 
                                         net_dat_bb_all_to_s6_wide$participant_id, 1, 0)

# Add indicators for RR and BBSIQ network completer samples to specific and overall datasets

## Define function

add_complete_indicator <- function(net_dat_long, net_dat_wide, net_suffix = NULL) {
  complete_colname <- "complete_bl_s3_s6"
  
  indicator_dat <- net_dat_wide[c("participant_id", complete_colname)]
  
  if (!is.null(net_suffix)) {
    complete_colname_net <- paste0(complete_colname, net_suffix)
    
    names(indicator_dat)[names(indicator_dat) == complete_colname] <- complete_colname_net
  }
  
  net_dat_long <- merge(net_dat_long, indicator_dat, "participant_id", all.x = TRUE)
  
  return(net_dat_long)
}

## Run function for specific datasets

net_dat_rr_all_to_s6 <- add_complete_indicator(net_dat_rr_all_to_s6, net_dat_rr_all_to_s6_wide)
net_dat_bb_all_to_s6 <- add_complete_indicator(net_dat_bb_all_to_s6, net_dat_bb_all_to_s6_wide)

## Run function for overall dataset

net_dat_all_to_s6 <- add_complete_indicator(net_dat_all_to_s6, net_dat_rr_all_to_s6_wide, "_rr_net")
net_dat_all_to_s6 <- add_complete_indicator(net_dat_all_to_s6, net_dat_bb_all_to_s6_wide, "_bb_net")

### Recode NAs (if present) for indicators to 0

net_dat_all_to_s6$complete_bl_s3_s6_rr_net[is.na(net_dat_all_to_s6$complete_bl_s3_s6_rr_net)] <- 0
stopifnot(all(!is.na(net_dat_all_to_s6$complete_bl_s3_s6_bb_net)))

# ---------------------------------------------------------------------------- #
# Compute indicators of ITT participants and completers in RR or BBSIQ networks in overall dataset ----
# ---------------------------------------------------------------------------- #

net_dat_all_to_s6$itt_any_net <- as.integer(net_dat_all_to_s6$itt_rr_net == 1 | 
                                              net_dat_all_to_s6$itt_bb_net == 1)

net_dat_all_to_s6$complete_bl_s3_s6_any_net <- as.integer(net_dat_all_to_s6$complete_bl_s3_s6_rr_net == 1 |
                                                            net_dat_all_to_s6$complete_bl_s3_s6_bb_net == 1)

# ---------------------------------------------------------------------------- #
# Compute number of participants in each analysis sample ----
# ---------------------------------------------------------------------------- #

stopifnot(
  # RR and BBSIQ network datasets have 806 and 807 ITT participants and 105 and 
  # 108 completers, respectively
  
  nrow(net_dat_rr_all_to_s6_wide)                       == 806,
  nrow(net_dat_bb_all_to_s6_wide)                       == 807,
  
  sum(net_dat_rr_all_to_s6_wide$complete_bl_s3_s6 == 1) == 105,
  sum(net_dat_bb_all_to_s6_wide$complete_bl_s3_s6 == 1) == 108,
  
  # Overall, 807 ITT participants and 112 completers are in RR or BBSIQ network datasets
  
  length(unique(net_dat_all_to_s6$participant_id[net_dat_all_to_s6$itt_any_net == 1]))               == 807,
  length(unique(net_dat_all_to_s6$participant_id[net_dat_all_to_s6$complete_bl_s3_s6_any_net == 1])) == 112
)

# ---------------------------------------------------------------------------- #
# Export data and helper nodes list ----
# ---------------------------------------------------------------------------- #

# RR and BBSIQ network datasets (wide and long format)

saveRDS(net_dat_rr_all_to_s6_wide, file.path(processed_path, "net_dat_rr_all_to_s6_wide.rds"))
saveRDS(net_dat_bb_all_to_s6_wide, file.path(processed_path, "net_dat_bb_all_to_s6_wide.rds"))

saveRDS(net_dat_rr_all_to_s6, file.path(processed_path, "net_dat_rr_all_to_s6.rds"))
saveRDS(net_dat_bb_all_to_s6, file.path(processed_path, "net_dat_bb_all_to_s6.rds"))

# Overall network dataset with both RR and BBSIQ (long format)

saveRDS(net_dat_all_to_s6, file.path(processed_path, "net_dat_all_to_s6.rds"))

# Helper nodes list

saveRDS(nodes, file.path(helper_path, "nodes.rds"))