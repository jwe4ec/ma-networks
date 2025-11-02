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

processed_path <- file.path("data", "processed")

net_dat_rr_all_to_s6_wide <- readRDS(file.path(processed_path, "net_dat_rr_all_to_s6_wide.rds"))
net_dat_bb_all_to_s6_wide <- readRDS(file.path(processed_path, "net_dat_bb_all_to_s6_wide.rds"))

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

# TODO: Continue revising below for new data





# Define function to fit mixed graphical models at baseline, Session 3, and Session 6 
# for given dummy-coded condition contrast (using name of contrast column) and missing 
# data handling method (listwise deletion per wave or across waves)

fit_mgm <- function(net_dat_wide, contrast, missing) {
  x <- net_dat_wide
  
  # Restrict data to conditions defined in contrast
  
  x <- x[!is.na(x[[contrast]]), ]
  
  # If specified, restrict to rows with complete data at baseline, Session 3, and Session 6
  
  if (missing == "listwise_across_waves") x <- x[x$complete_bl_s3_s6 == 1, ]
  
  # Prepare data and fit model at baseline, Session 3, and Session 6
  
  waves <- c("PRE", "SESSION3", "SESSION6")
  
  res <- vector("list", length(waves))
  names(res) <- waves
  
  for (i in 1:length(waves)) {
    wave <- waves[i]
    
    # Restrict to contrast column and columns of given wave
    
    target_wave_cols <- grep(paste0(".", wave), names(x), value = TRUE)
    
    target_cols <- c(contrast, target_wave_cols)
    
    dat <- x[target_cols]
    
    if (missing == "listwise_per_wave") {
      # Restrict to rows with complete data for columns of given wave
      
      dat <- dat[complete.cases(dat[target_wave_cols]), ]
    }
    
    # Convert data to matrix and define column types and levels (assume that binary
    # condition contrast coded 0/1 is first column)
    
    dat_mat <- as.matrix(dat)
    
    type  <- c("c", rep("g", 7))
    level <- c(2, rep(1, 7))
    
    # Fit with LASSO regularization with lambda selected via 10-fold cross-validation
    # and default beta-min threshold (seed seems to be needed for reproducibility)
    
    set.seed(1234)
    fit1 <- mgm(dat_mat, type, level, scale = TRUE, binarySign = TRUE, saveData = TRUE,
                lambdaSeq = NULL, lambdaSel = "CV", lambdaFolds = 10, threshold = "LW")
    
    # Fit with LASSO regularization with lambda that minimizes EBIC with default gamma 
    # of 0.25 and default beta-min threshold
    
    set.seed(1234)
    fit2 <- mgm(dat_mat, type, level, scale = TRUE, binarySign = TRUE, saveData = TRUE,
                lambdaSeq = NULL, lambdaSel = "EBIC", lambdaGam = 0.25, threshold = "LW")
    
    # Fit without regularization but retaining default beta-min threshold, per Fried 
    # et al. (2020, https://doi.org/gg6378, "Network 4 (3b without regularization)" 
    # on Line 719 of "3.network_estimation.R" in supplement)
    
    set.seed(1234)
    fit3 <- mgm(dat_mat, type, level, scale = TRUE, binarySign = TRUE, saveData = TRUE,
                lambdaSeq = 0, lambdaSel = "EBIC", lambdaGam = 0, threshold = "LW")
    
    # Fit saturated model without regularization (i.e., remove beta-min threshold), 
    # which is needed for network stability analyses to yield confidence intervals
    
    set.seed(1234)
    fit4 <- mgm(dat_mat, type, level, scale = TRUE, binarySign = TRUE, saveData = TRUE,
                lambdaSeq = 0, lambdaSel = "EBIC", lambdaGam = 0, threshold = "none")
    
    # Collect results for time point in list
    
    res[[wave]] <- list(vars = target_cols,
                        wave = wave,
                        fit1 = fit1,
                        fit2 = fit2,
                        fit3 = fit3,
                        fit4 = fit4)
  }

  return(res)
}

# Run function for two contrasts and two missing handling methods each

res_rev_pos_neu_lw_per_wave     <- fit_mgm(net_dat_all_to_s6_wide, "positive_vs_neutral",     "listwise_per_wave")
res_rev_pos_neu_lw_across_waves <- fit_mgm(net_dat_all_to_s6_wide, "positive_vs_neutral",     "listwise_across_waves")

res_rev_pos_fif_lw_per_wave     <- fit_mgm(net_dat_all_to_s6_wide, "positive_vs_fifty_fifty", "listwise_per_wave")
res_rev_pos_fif_lw_across_waves <- fit_mgm(net_dat_all_to_s6_wide, "positive_vs_fifty_fifty", "listwise_across_waves")

# Export results

net_interv_path <- "./results/net_interv/"

dir.create(net_interv_path, recursive = TRUE)

save(res_rev_pos_neu_lw_per_wave,     file = paste0(net_interv_path, "res_rev_pos_neu_lw_per_wave.RData"))
save(res_rev_pos_neu_lw_across_waves, file = paste0(net_interv_path, "res_rev_pos_neu_lw_across_waves.RData"))

save(res_rev_pos_fif_lw_per_wave,     file = paste0(net_interv_path, "res_rev_pos_fif_lw_per_wave.RData"))
save(res_rev_pos_fif_lw_across_waves, file = paste0(net_interv_path, "res_rev_pos_fif_lw_across_waves.RData"))