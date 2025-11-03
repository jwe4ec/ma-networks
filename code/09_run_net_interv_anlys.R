# ---------------------------------------------------------------------------- #
# Run Network Intervention Analyses
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

# Load packages

groundhog.library("mgm", groundhog_day)

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

# Wide-format network datasets

processed_path <- file.path("data", "processed")

net_dat_rr_all_to_s6_wide <- readRDS(file.path(processed_path, "net_dat_rr_all_to_s6_wide.rds"))
net_dat_bb_all_to_s6_wide <- readRDS(file.path(processed_path, "net_dat_bb_all_to_s6_wide.rds"))

# Helper nodes list

nodes <- readRDS(file.path("data", "helper", "nodes.rds"))

# ---------------------------------------------------------------------------- #
# Create CBM-I condition contrasts ----
# ---------------------------------------------------------------------------- #

# Define function to create dummy-coded CBM-I condition variables for contrasting 
# positive CBM-I with (a) no-training and (b) 50-50 CBM-I

create_contrasts <- function(net_dat) {
  net_dat$positive_vs_neutral <- ifelse(net_dat$cbmCondition == "POSITIVE", 1, 
                                        ifelse(net_dat$cbmCondition == "NEUTRAL", 0, NA))
  
  net_dat$positive_vs_fifty_fifty <- ifelse(net_dat$cbmCondition == "POSITIVE", 1, 
                                            ifelse(net_dat$cbmCondition == "FIFTY_FIFTY", 0, NA))
  
  return(net_dat)
}

# Run function

net_dat_rr_all_to_s6_wide <- create_contrasts(net_dat_rr_all_to_s6_wide)
net_dat_bb_all_to_s6_wide <- create_contrasts(net_dat_bb_all_to_s6_wide)

# ---------------------------------------------------------------------------- #
# Compute zero-order correlations at baseline for ITT participants ----
# ---------------------------------------------------------------------------- #

# Define function to compute Pearson correlations (given that we are treating nodes 
# as continuous)

compute_cor_at_bl <- function(net_dat) {
  # Get baseline variables
  
  bl_vars <- grep(".PRE", names(net_dat), value = TRUE)
  
  # Compute correlations
  
  cor_res <- cor(net_dat[bl_vars], use = "pairwise.complete.obs", method = "pearson")
  
  # Format results
  
  cor_res <- round(cor_res, 2)
  
  labels <- bl_vars
  
  labels[labels == "anxious_freq.PRE"]        <- "Anx. Freq."
  labels[labels == "anxious_sev.PRE"]         <- "Anx. Sev."
  labels[labels == "avoid.PRE"]               <- "Sit. Avoid"
  labels[labels == "interfere.PRE"]           <- "Work Imp."
  labels[labels == "interfere_social.PRE"]    <- "Soc. Imp."
  labels[labels == "rr_neg_thr_mean.PRE"]     <- "Neg. Bias (RR)"
  labels[labels == "rr_pos_thr_mean_rev.PRE"] <- "Lack of Pos. Bias (RR)"
  labels[labels == "bbsiq_neg_mean.PRE"]      <- "Neg. Bias (BBSIQ)"
  
  rownames(cor_res) <- colnames(cor_res) <- labels
  
  cor_res[upper.tri(cor_res, diag = TRUE)] <- NA

  return(cor_res)
}

# Run function

cor_res_rr_net <- compute_cor_at_bl(net_dat_rr_all_to_s6_wide)
cor_res_bb_net <- compute_cor_at_bl(net_dat_bb_all_to_s6_wide)

# Compute ranges

stopifnot(range(cor_res_rr_net, na.rm = TRUE) == c(0.09, 0.58),
          range(cor_res_bb_net, na.rm = TRUE) == c(0.28, 0.58))

# Export results

bl_correlations_path <- file.path("results", "baseline_correlations")
dir.create(bl_correlations_path)

write.csv(cor_res_rr_net, file.path(bl_correlations_path, "cor_res_rr_net.csv"))
write.csv(cor_res_bb_net, file.path(bl_correlations_path, "cor_res_bb_net.csv"))

# ---------------------------------------------------------------------------- #
# Run analyses ----
# ---------------------------------------------------------------------------- #

# Define function to fit mixed graphical models at baseline, Session 3, and Session 6 
# for given dummy-coded condition contrast (using name of contrast column) and missing 
# data handling method (listwise deletion per wave or across waves)

fit_mgm <- function(net_dat_wide, contrast, missing, nodes) {
  x <- net_dat_wide
  
  # Restrict data to conditions defined in contrast
  
  x <- x[!is.na(x[[contrast]]), ]
  
  # If specified, restrict to rows with complete data at baseline, Session 3, and Session 6
  
  if (missing == "lw_across_waves") x <- x[x$complete_bl_s3_s6 == 1, ]
  
  # Prepare data and fit model at baseline, Session 3, and Session 6
  
  waves <- c("PRE", "SESSION3", "SESSION6")
  
  res <- vector("list", length(waves))
  names(res) <- waves
  
  for (i in 1:length(waves)) {
    wave <- waves[i]
    
    # Restrict to contrast column and node columns of given wave
    
    target_wave_cols <- paste0(nodes, ".", wave)
    
    target_cols <- c(contrast, target_wave_cols)
    
    dat <- x[target_cols]
    
    if (missing == "lw_per_wave") {
      # Restrict to rows with complete data for columns of given wave
      
      dat <- dat[complete.cases(dat[target_wave_cols]), ]
    }
    
    # Convert data to matrix and define column types and levels (assume that binary
    # condition contrast coded 0/1 is first column)
    
    dat_mat <- as.matrix(dat)
    
    num_nodes <- length(nodes)
    
    type  <- c("c", rep("g", num_nodes))
    level <- c(2,   rep(1,   num_nodes))
    
    # Fit saturated model without regularization, per Fried et al. (2020, https://doi.org/gg6378, 
    # "Network 4 (3b without regularization)" on Line 719 of "3.network_estimation.R" in supplement),
    # but also remove beta-min threshold (in contrast to Fried et al.), which is needed for network 
    # stability analyses to yield confidence intervals
    
    set.seed(1234)
    fit <- mgm(dat_mat, type, level, scale = TRUE, binarySign = TRUE, saveData = TRUE,
               lambdaSeq = 0, lambdaSel = "EBIC", lambdaGam = 0, threshold = "none")
    
    # Collect results for time point in list
    
    res[[wave]] <- list(vars = target_cols,
                        wave = wave,
                        fit  = fit)
  }

  return(res)
}

# Run function for RR and BBSIQ networks for two contrasts and two missing handling methods each

## For RR network

res_rr_pos_neu_lw_per_wave     <- fit_mgm(net_dat_rr_all_to_s6_wide, 
                                          "positive_vs_neutral",     "lw_per_wave",     nodes$rr_net)
res_rr_pos_neu_lw_across_waves <- fit_mgm(net_dat_rr_all_to_s6_wide, 
                                          "positive_vs_neutral",     "lw_across_waves", nodes$rr_net)

res_rr_pos_fif_lw_per_wave     <- fit_mgm(net_dat_rr_all_to_s6_wide, 
                                          "positive_vs_fifty_fifty", "lw_per_wave",     nodes$rr_net)
res_rr_pos_fif_lw_across_waves <- fit_mgm(net_dat_rr_all_to_s6_wide, 
                                          "positive_vs_fifty_fifty", "lw_across_waves", nodes$rr_net)

## For BBSIQ network

res_bb_pos_neu_lw_per_wave     <- fit_mgm(net_dat_bb_all_to_s6_wide, 
                                          "positive_vs_neutral",     "lw_per_wave",     nodes$bb_net)
res_bb_pos_neu_lw_across_waves <- fit_mgm(net_dat_bb_all_to_s6_wide, 
                                          "positive_vs_neutral",     "lw_across_waves", nodes$bb_net)

res_bb_pos_fif_lw_per_wave     <- fit_mgm(net_dat_bb_all_to_s6_wide, 
                                          "positive_vs_fifty_fifty", "lw_per_wave",     nodes$bb_net)
res_bb_pos_fif_lw_across_waves <- fit_mgm(net_dat_bb_all_to_s6_wide, 
                                          "positive_vs_fifty_fifty", "lw_across_waves", nodes$bb_net)

# ---------------------------------------------------------------------------- #
# Export results ----
# ---------------------------------------------------------------------------- #

net_interv_path <- file.path("results", "net_interv")
dir.create(net_interv_path)

# For RR network

saveRDS(res_rr_pos_neu_lw_per_wave,     file.path(net_interv_path, "res_rr_pos_neu_lw_per_wave.rds"))
saveRDS(res_rr_pos_neu_lw_across_waves, file.path(net_interv_path, "res_rr_pos_neu_lw_across_waves.rds"))

saveRDS(res_rr_pos_fif_lw_per_wave,     file.path(net_interv_path, "res_rr_pos_fif_lw_per_wave.rds"))
saveRDS(res_rr_pos_fif_lw_across_waves, file.path(net_interv_path, "res_rr_pos_fif_lw_across_waves.rds"))

# For BBSIQ network

saveRDS(res_bb_pos_neu_lw_per_wave,     file.path(net_interv_path, "res_bb_pos_neu_lw_per_wave.rds"))
saveRDS(res_bb_pos_neu_lw_across_waves, file.path(net_interv_path, "res_bb_pos_neu_lw_across_waves.rds"))

saveRDS(res_bb_pos_fif_lw_per_wave,     file.path(net_interv_path, "res_bb_pos_fif_lw_per_wave.rds"))
saveRDS(res_bb_pos_fif_lw_across_waves, file.path(net_interv_path, "res_bb_pos_fif_lw_across_waves.rds"))