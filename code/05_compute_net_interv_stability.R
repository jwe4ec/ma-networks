# ---------------------------------------------------------------------------- #
# Compute Network Intervention Stability
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

groundhog.library("mgm", groundhog_day)

# ---------------------------------------------------------------------------- #
# Import results ----
# ---------------------------------------------------------------------------- #

net_interv_path <- "./results/net_interv/"

load(paste0(net_interv_path, "res_rev_pos_neu_lw_per_wave.RData"))
load(paste0(net_interv_path, "res_rev_pos_neu_lw_across_waves.RData"))

load(paste0(net_interv_path, "res_rev_pos_fif_lw_per_wave.RData"))
load(paste0(net_interv_path, "res_rev_pos_fif_lw_across_waves.RData"))

# ---------------------------------------------------------------------------- #
# Compute and export network stability ----
# ---------------------------------------------------------------------------- #

net_interv_stab_path <- paste0(net_interv_path, "stab/")

dir.create(net_interv_stab_path)

# Note: Use only Fit 4 based on saturated networks because this estimation method
# is the only one that yields true confidence intervals

# print("Starting Set 1")

set.seed(1234)
res_rev_pos_neu_lw_per_wave_pre_fit4_bs <- resample(res_rev_pos_neu_lw_per_wave$PRE$fit4, res_rev_pos_neu_lw_per_wave$PRE$fit4$call$data, 500)
save(res_rev_pos_neu_lw_per_wave_pre_fit4_bs, file = paste0(net_interv_stab_path, "res_rev_pos_neu_lw_per_wave_pre_fit4_bs.RData"))
set.seed(1234)
res_rev_pos_neu_lw_per_wave_session3_fit4_bs <- resample(res_rev_pos_neu_lw_per_wave$SESSION3$fit4, res_rev_pos_neu_lw_per_wave$SESSION3$fit4$call$data, 500)
save(res_rev_pos_neu_lw_per_wave_session3_fit4_bs, file = paste0(net_interv_stab_path, "res_rev_pos_neu_lw_per_wave_session3_fit4_bs.RData"))
set.seed(1234)
res_rev_pos_neu_lw_per_wave_SESSION6_fit4_bs <- resample(res_rev_pos_neu_lw_per_wave$SESSION6$fit4, res_rev_pos_neu_lw_per_wave$SESSION6$fit4$call$data, 500)
save(res_rev_pos_neu_lw_per_wave_SESSION6_fit4_bs, file = paste0(net_interv_stab_path, "res_rev_pos_neu_lw_per_wave_SESSION6_fit4_bs.RData"))

# print("Starting Set 2")

set.seed(1234)
res_rev_pos_neu_lw_across_waves_pre_fit4_bs <- resample(res_rev_pos_neu_lw_across_waves$PRE$fit4, res_rev_pos_neu_lw_across_waves$PRE$fit4$call$data, 500)
save(res_rev_pos_neu_lw_across_waves_pre_fit4_bs, file = paste0(net_interv_stab_path, "res_rev_pos_neu_lw_across_waves_pre_fit4_bs.RData"))

# Error below for "res_rev_pos_neu_lw_across_waves_session3_fit4_bs" with seed 1234 resolved by changing seed to 1235

  # Error in matrix(fit$a0[seq(lmu * nc)], nc, lmu, dimnames = list(classnames,  :
  #   length of 'dimnames' [2] not equal to array extent
  # In addition: Warning message:
  #   from glmnet C++ code (error code -1); Convergence for 1th lambda value not reached after maxit=100000 iterations; solutions for larger lambdas returned

set.seed(1235)
res_rev_pos_neu_lw_across_waves_session3_fit4_bs <- resample(res_rev_pos_neu_lw_across_waves$SESSION3$fit4, res_rev_pos_neu_lw_across_waves$SESSION3$fit4$call$data, 500)
save(res_rev_pos_neu_lw_across_waves_session3_fit4_bs, file = paste0(net_interv_stab_path, "res_rev_pos_neu_lw_across_waves_session3_fit4_bs.RData"))

set.seed(1234)
res_rev_pos_neu_lw_across_waves_SESSION6_fit4_bs <- resample(res_rev_pos_neu_lw_across_waves$SESSION6$fit4, res_rev_pos_neu_lw_across_waves$SESSION6$fit4$call$data, 500)
save(res_rev_pos_neu_lw_across_waves_SESSION6_fit4_bs, file = paste0(net_interv_stab_path, "res_rev_pos_neu_lw_across_waves_SESSION6_fit4_bs.RData"))

# print("Starting Set 3")

set.seed(1234)
res_rev_pos_fif_lw_per_wave_pre_fit4_bs <- resample(res_rev_pos_fif_lw_per_wave$PRE$fit4, res_rev_pos_fif_lw_per_wave$PRE$fit4$call$data, 500)
save(res_rev_pos_fif_lw_per_wave_pre_fit4_bs, file = paste0(net_interv_stab_path, "res_rev_pos_fif_lw_per_wave_pre_fit4_bs.RData"))
set.seed(1234)
res_rev_pos_fif_lw_per_wave_session3_fit4_bs <- resample(res_rev_pos_fif_lw_per_wave$SESSION3$fit4, res_rev_pos_fif_lw_per_wave$SESSION3$fit4$call$data, 500)
save(res_rev_pos_fif_lw_per_wave_session3_fit4_bs, file = paste0(net_interv_stab_path, "res_rev_pos_fif_lw_per_wave_session3_fit4_bs.RData"))
set.seed(1234)
res_rev_pos_fif_lw_per_wave_SESSION6_fit4_bs <- resample(res_rev_pos_fif_lw_per_wave$SESSION6$fit4, res_rev_pos_fif_lw_per_wave$SESSION6$fit4$call$data, 500)
save(res_rev_pos_fif_lw_per_wave_SESSION6_fit4_bs, file = paste0(net_interv_stab_path, "res_rev_pos_fif_lw_per_wave_SESSION6_fit4_bs.RData"))

# print("Starting Set 4")

set.seed(1234)
res_rev_pos_fif_lw_across_waves_pre_fit4_bs <- resample(res_rev_pos_fif_lw_across_waves$PRE$fit4, res_rev_pos_fif_lw_across_waves$PRE$fit4$call$data, 500)
save(res_rev_pos_fif_lw_across_waves_pre_fit4_bs, file = paste0(net_interv_stab_path, "res_rev_pos_fif_lw_across_waves_pre_fit4_bs.RData"))
set.seed(1234)
res_rev_pos_fif_lw_across_waves_session3_fit4_bs <- resample(res_rev_pos_fif_lw_across_waves$SESSION3$fit4, res_rev_pos_fif_lw_across_waves$SESSION3$fit4$call$data, 500)
save(res_rev_pos_fif_lw_across_waves_session3_fit4_bs, file = paste0(net_interv_stab_path, "res_rev_pos_fif_lw_across_waves_session3_fit4_bs.RData"))
set.seed(1234)
res_rev_pos_fif_lw_across_waves_SESSION6_fit4_bs <- resample(res_rev_pos_fif_lw_across_waves$SESSION6$fit4, res_rev_pos_fif_lw_across_waves$SESSION6$fit4$call$data, 500)
save(res_rev_pos_fif_lw_across_waves_SESSION6_fit4_bs, file = paste0(net_interv_stab_path, "res_rev_pos_fif_lw_across_waves_SESSION6_fit4_bs.RData"))