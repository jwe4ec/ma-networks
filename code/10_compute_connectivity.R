# ---------------------------------------------------------------------------- #
# Compute Connectivity
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

# Load packages

pkgs <- c("dplyr", "psychonetrics")

groundhog.library(pkgs, groundhog_day)

# ---------------------------------------------------------------------------- #
# Import multigroup temporal network model ----
# ---------------------------------------------------------------------------- #

load("./results/panel_gvar/multi_grp/mg_model2_rev_nlminb.RData")

# ---------------------------------------------------------------------------- #
# Compute connectivity values in observed multigroup model ----
# ---------------------------------------------------------------------------- #

# Define function to compute connectivity values for temporal and contemporaneous 
# networks in each group of saturated multigroup model

compute_conn <- function(mg_mod) {
  # Extract temporal and contemporaneous partial correlations from multigroup model
  
  mg_tem_list <- getmatrix(mg_mod, "PDC")
  mg_con_list <- getmatrix(mg_mod, "omega_zeta_within")
  
  # Extract groups from multigroup model
  
  groups <- attr(mg_mod, "sample")@groups$label
  
  # Compute connectivity for each group
  
  conn_res <- vector("list", length(groups))
  names(conn_res) <- groups
  
  for (group in groups) {
    
    mg_tem <- mg_tem_list[[group]]
    mg_con <- mg_con_list[[group]]
    
    # Compute global strength for temporal network (inter-node connectivity based on
    # cross-lagged effects, intra-node connectivity based on autoregressive effects,
    # and overall connectivity based on all effects), including nonsignificant edges
    
    tem_gs_inter <- mean(abs(c(mg_tem[upper.tri(mg_tem)],
                               mg_tem[lower.tri(mg_tem)])))
    
    tem_gs_intra <- mean(abs(diag(mg_tem)))
    
    tem_gs_whole <- mean(abs(mg_tem))
    
    # Compute global strength for contemporaneous network (inter-node connectivity only)
    
    con_gs_inter <- mean(abs(mg_con[lower.tri(mg_con)]))
    
    # Compute global expected influence for temporal network
    
    tem_ge_inter <- mean(c(mg_tem[upper.tri(mg_tem)],
                           mg_tem[lower.tri(mg_tem)]))
    
    tem_ge_intra <- mean(diag(mg_tem))
    
    tem_ge_whole <- mean(mg_tem)
    
    # Compute global expected influence for contemporaneous network
    
    con_ge_inter <- mean(mg_con[lower.tri(mg_con)])
    
    # Collect results in list and convert to vector
    
    conn_res_group <- list(cbmCondition = group,
                           tem_gs_inter = tem_gs_inter,
                           tem_gs_intra = tem_gs_intra,
                           tem_gs_whole = tem_gs_whole,
                           con_gs_inter = con_gs_inter,
                           tem_ge_inter = tem_ge_inter,
                           tem_ge_intra = tem_ge_intra,
                           tem_ge_whole = tem_ge_whole,
                           con_ge_inter = con_ge_inter)
    
    conn_res[[group]] <- unlist(conn_res_group)
  }
  
  # Combine results in data frame
  
  conn_res <- as.data.frame(do.call(cbind, conn_res))
  
  conn_res <- conn_res[row.names(conn_res) != "cbmCondition", ]
  
  conn_res$metric <- row.names(conn_res)
  row.names(conn_res) <- 1:nrow(conn_res)
  conn_res <- conn_res[, c("metric", names(conn_res)[names(conn_res) != "metric"])]
  
  num_cols <- names(conn_res)[names(conn_res) != "metric"]
  
  conn_res[, num_cols] <- sapply(conn_res[, num_cols], as.numeric)
  
  # Compute group differences
  
  conn_res$pos_vs_neu <- conn_res$POSITIVE - conn_res$NEUTRAL
  conn_res$pos_vs_fif <- conn_res$POSITIVE - conn_res$FIFTY_FIFTY
  
  return(conn_res)
}

# Run function

conn_res_observed <- compute_conn(mg_model2_rev_nlminb)

# ---------------------------------------------------------------------------- #
# Format connectivity table ----
# ---------------------------------------------------------------------------- #

conn_res_tbl <- conn_res_observed[c("metric", "POSITIVE", "FIFTY_FIFTY", "NEUTRAL",
                                    "pos_vs_fif", "pos_vs_neu")]

round_2_cols <- c("POSITIVE", "FIFTY_FIFTY", "NEUTRAL", "pos_vs_fif", "pos_vs_neu")

conn_res_tbl[round_2_cols] <- round(conn_res_tbl[round_2_cols], 2)

con_metrics <- c("con_gs_inter", "con_ge_inter")
tem_metrics <- setdiff(conn_res_tbl$metric, con_metrics)

conn_res_tbl <- conn_res_tbl[match(c(tem_metrics, con_metrics), conn_res_tbl$metric), ]

# ---------------------------------------------------------------------------- #
# Save results ----
# ---------------------------------------------------------------------------- #

# Export results

write.csv(conn_res_tbl, file = "./results/panel_gvar/multi_grp/connectivity/conn_res_tbl.csv", row.names = FALSE)

save(conn_res_tbl, file = "./results/panel_gvar/multi_grp/connectivity/conn_res_tbl.RData")