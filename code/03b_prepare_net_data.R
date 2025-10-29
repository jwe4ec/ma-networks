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

cln_dat <- readRDS(file.path(processed_path, "cln_dat.rds"))

items <- readRDS(file.path("data", "helper", "items.rds"))

# ---------------------------------------------------------------------------- #
# Merge data ----
# ---------------------------------------------------------------------------- #

# Define columns that will be manifest nodes and collect in list

oa_node_cols    <- items$oa
rr_node_cols    <- c("rr_neg_thr_mean", "rr_pos_thr_mean_rev")
bbsiq_node_cols <- "bbsiq_neg_mean"

nodes <- list(oa     = oa_node_cols,
              rr     = rr_node_cols,
              bbsiq  = bbsiq_node_cols,
              rr_net = c(oa_node_cols, rr_node_cols),
              bb_net = c(oa_node_cols, bbsiq_node_cols))

# Use full outer join to merge all node columns into one table

index_cols <- c("participant_id", "session_only")

net_dat_rr <- merge(cln_dat$oa[c(index_cols, oa_node_cols)],
                    cln_dat$rr[c(index_cols, rr_node_cols)],
                    by = index_cols,
                    all = TRUE)

net_dat_bb <- merge(cln_dat$oa[c(index_cols, oa_node_cols)],
                    cln_dat$bbsiq[c(index_cols, bbsiq_node_cols)],
                    by = index_cols,
                    all = TRUE)

# Add condition columns

condition_cols <- c("cbmCondition", "prime")
condition_dat  <- cln_dat$participant[c("participant_id", condition_cols)]

stopifnot(all(!is.na(condition_dat[condition_cols])))

net_dat_rr <- merge(net_dat_rr, condition_dat, by = "participant_id", all.x = TRUE)
net_dat_bb <- merge(net_dat_bb, condition_dat, by = "participant_id", all.x = TRUE)

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

# Run functions

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

# Remove participant 583 from RR analyses (no data on any nodes), leaving 806 
# participants in RR analyses and 807 in BBSIQ analyses

net_dat_rr <- net_dat_rr[net_dat_rr$participant_id != 583, ]

stopifnot(
  length(unique(net_dat_rr$participant_id)) == 806,
  length(unique(net_dat_bb$participant_id)) == 807
)

# ---------------------------------------------------------------------------- #
# Restrict to time points through Session 6 ----
# ---------------------------------------------------------------------------- #

# Restrict to time points "PRE" through "SESSION6" (excluding "SESSION7", "SESSION8",
# and "POST"; following main outcomes paper; Ji et al., 2021)

sessions_to_keep <- c("PRE", paste0("SESSION", 1:6))

net_dat_rr_all_to_s6 <- net_dat_rr[net_dat_rr$session_only %in% sessions_to_keep, ]
net_dat_bb_all_to_s6 <- net_dat_bb[net_dat_bb$session_only %in% sessions_to_keep, ]

# ---------------------------------------------------------------------------- #
# Restructure data ----
# ---------------------------------------------------------------------------- #

# Save long format data for search for auxiliary variables, etc.

saveRDS(net_dat_rr_all_to_s6, file.path(processed_path, "net_dat_rr_all_to_s6.rds"))
saveRDS(net_dat_bb_all_to_s6, file.path(processed_path, "net_dat_bb_all_to_s6.rds"))

# Convert to wide format

net_dat_rr_all_to_s6_wide <- reshape(net_dat_rr_all_to_s6,
                                     direction = "wide",
                                     idvar = "participant_id",
                                     timevar = "session_only",
                                     v.names = nodes$rr_net)

net_dat_bb_all_to_s6_wide <- reshape(net_dat_bb_all_to_s6,
                                     direction = "wide",
                                     idvar = "participant_id",
                                     timevar = "session_only",
                                     v.names = nodes$bb_net)

# Check that columns for all nodes and time points are present

node_cols_wide_net_dat_rr <- paste0(rep(nodes$rr_net, each = length(sessions_to_keep)), 
                                    ".", sessions_to_keep)
node_cols_wide_net_dat_bb <- paste0(rep(nodes$bb_net, each = length(sessions_to_keep)), 
                                    ".", sessions_to_keep)

stopifnot(
  all(node_cols_wide_net_dat_rr %in% names(net_dat_rr_all_to_s6_wide)),
  all(node_cols_wide_net_dat_bb %in% names(net_dat_bb_all_to_s6_wide))
)

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
# Export data and helper nodes list ----
# ---------------------------------------------------------------------------- #

saveRDS(net_dat_rr_all_to_s6_wide, file.path(processed_path, "net_dat_rr_all_to_s6_wide.rds"))
saveRDS(net_dat_bb_all_to_s6_wide, file.path(processed_path, "net_dat_bb_all_to_s6_wide.rds"))

saveRDS(nodes, file.path("data", "helper", "nodes.rds"))