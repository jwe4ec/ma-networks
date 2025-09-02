# ---------------------------------------------------------------------------- #
# Compute Network Intervention Stability
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# SKIP: This script was based on original analyses using positive bias that was not
# reversed scored ("rr_ps_mean" instead of "rr_rev_ps_mean"). Per our preregistration
# and per theory, it should have been reverse scored. It also uses the three estimation 
# methods originally proposed (Fits 1-3) in addition to the final estimation method 
# based on saturated networks (Fit 4), but only Fit 4 yields true confidence intervals.
# Thus, skip this script and run the other "compute_net_interv_stability.R" instead.

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("../code/01a_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control_temporal()

# Load packages

groundhog.library("mgm", groundhog_day)

# ---------------------------------------------------------------------------- #
# Import results ----
# ---------------------------------------------------------------------------- #

net_interv_path <- "../results/net_interv/"
old_net_interv_path <- "../results/net_interv/old/"

load(paste0(old_net_interv_path, "res_pos_neu_lw_per_wave.RData"))
load(paste0(old_net_interv_path, "res_pos_neu_lw_across_waves.RData"))

load(paste0(old_net_interv_path, "res_pos_fif_lw_per_wave.RData"))
load(paste0(old_net_interv_path, "res_pos_fif_lw_across_waves.RData"))

# ---------------------------------------------------------------------------- #
# Compute and export network stability ----
# ---------------------------------------------------------------------------- #

old_net_interv_stab_path <- paste0(net_interv_path, "stab/old/")

dir.create(old_net_interv_stab_path)

# TODO (those indented and commented have already been run): Run individually in 
# case there are errors (ultimately use only fit4 because that is the only one
# in which the results truly represent confidence intervals)





     # print("Starting Set 1")
     # 
     # set.seed(1234)
     # res_pos_neu_lw_per_wave_pre_fit1_bs <- resample(res_pos_neu_lw_per_wave$PRE$fit1, res_pos_neu_lw_per_wave$PRE$fit1$call$data, 500)
     # save(res_pos_neu_lw_per_wave_pre_fit1_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_per_wave_pre_fit1_bs.RData"))
     # set.seed(1234)
     # res_pos_neu_lw_per_wave_pre_fit2_bs <- resample(res_pos_neu_lw_per_wave$PRE$fit2, res_pos_neu_lw_per_wave$PRE$fit2$call$data, 500)
     # save(res_pos_neu_lw_per_wave_pre_fit2_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_per_wave_pre_fit2_bs.RData"))
     # set.seed(1234)
     # res_pos_neu_lw_per_wave_pre_fit3_bs <- resample(res_pos_neu_lw_per_wave$PRE$fit3, res_pos_neu_lw_per_wave$PRE$fit3$call$data, 500)
     # save(res_pos_neu_lw_per_wave_pre_fit3_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_per_wave_pre_fit3_bs.RData"))
     # set.seed(1234)
     # res_pos_neu_lw_per_wave_pre_fit4_bs <- resample(res_pos_neu_lw_per_wave$PRE$fit4, res_pos_neu_lw_per_wave$PRE$fit4$call$data, 500)
     # save(res_pos_neu_lw_per_wave_pre_fit4_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_per_wave_pre_fit4_bs.RData"))
     # set.seed(1234)
     # res_pos_neu_lw_per_wave_session3_fit1_bs <- resample(res_pos_neu_lw_per_wave$SESSION3$fit1, res_pos_neu_lw_per_wave$SESSION3$fit1$call$data, 500)
     # save(res_pos_neu_lw_per_wave_session3_fit1_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_per_wave_session3_fit1_bs.RData"))
     # set.seed(1234)
     # res_pos_neu_lw_per_wave_session3_fit2_bs <- resample(res_pos_neu_lw_per_wave$SESSION3$fit2, res_pos_neu_lw_per_wave$SESSION3$fit2$call$data, 500)
     # save(res_pos_neu_lw_per_wave_session3_fit2_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_per_wave_session3_fit2_bs.RData"))
     # set.seed(1234)
     # res_pos_neu_lw_per_wave_session3_fit3_bs <- resample(res_pos_neu_lw_per_wave$SESSION3$fit3, res_pos_neu_lw_per_wave$SESSION3$fit3$call$data, 500)
     # save(res_pos_neu_lw_per_wave_session3_fit3_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_per_wave_session3_fit3_bs.RData"))
     # set.seed(1234)
     # res_pos_neu_lw_per_wave_session3_fit4_bs <- resample(res_pos_neu_lw_per_wave$SESSION3$fit4, res_pos_neu_lw_per_wave$SESSION3$fit4$call$data, 500)
     # save(res_pos_neu_lw_per_wave_session3_fit4_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_per_wave_session3_fit4_bs.RData"))
     # set.seed(1234)
     # res_pos_neu_lw_per_wave_SESSION6_fit1_bs <- resample(res_pos_neu_lw_per_wave$SESSION6$fit1, res_pos_neu_lw_per_wave$SESSION6$fit1$call$data, 500)
     # save(res_pos_neu_lw_per_wave_SESSION6_fit1_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_per_wave_SESSION6_fit1_bs.RData"))
     # set.seed(1234)
     # res_pos_neu_lw_per_wave_SESSION6_fit2_bs <- resample(res_pos_neu_lw_per_wave$SESSION6$fit2, res_pos_neu_lw_per_wave$SESSION6$fit2$call$data, 500)
     # save(res_pos_neu_lw_per_wave_SESSION6_fit2_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_per_wave_SESSION6_fit2_bs.RData"))
     # set.seed(1234)
     # res_pos_neu_lw_per_wave_SESSION6_fit3_bs <- resample(res_pos_neu_lw_per_wave$SESSION6$fit3, res_pos_neu_lw_per_wave$SESSION6$fit3$call$data, 500)
     # save(res_pos_neu_lw_per_wave_SESSION6_fit3_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_per_wave_SESSION6_fit3_bs.RData"))
     # set.seed(1234)
     # res_pos_neu_lw_per_wave_SESSION6_fit4_bs <- resample(res_pos_neu_lw_per_wave$SESSION6$fit4, res_pos_neu_lw_per_wave$SESSION6$fit4$call$data, 500)
     # save(res_pos_neu_lw_per_wave_SESSION6_fit4_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_per_wave_SESSION6_fit4_bs.RData"))
     # 
     # print("Starting Set 2")
     # 
     # set.seed(1234)
     # res_pos_neu_lw_across_waves_pre_fit1_bs <- resample(res_pos_neu_lw_across_waves$PRE$fit1, res_pos_neu_lw_across_waves$PRE$fit1$call$data, 500)
     # save(res_pos_neu_lw_across_waves_pre_fit1_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_across_waves_pre_fit1_bs.RData"))
     # set.seed(1234)
     # res_pos_neu_lw_across_waves_pre_fit2_bs <- resample(res_pos_neu_lw_across_waves$PRE$fit2, res_pos_neu_lw_across_waves$PRE$fit2$call$data, 500)
     # save(res_pos_neu_lw_across_waves_pre_fit2_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_across_waves_pre_fit2_bs.RData"))
     # set.seed(1234)
     # res_pos_neu_lw_across_waves_pre_fit3_bs <- resample(res_pos_neu_lw_across_waves$PRE$fit3, res_pos_neu_lw_across_waves$PRE$fit3$call$data, 500)
     # save(res_pos_neu_lw_across_waves_pre_fit3_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_across_waves_pre_fit3_bs.RData"))
     # set.seed(1234)
     # res_pos_neu_lw_across_waves_pre_fit4_bs <- resample(res_pos_neu_lw_across_waves$PRE$fit4, res_pos_neu_lw_across_waves$PRE$fit4$call$data, 500)
     # save(res_pos_neu_lw_across_waves_pre_fit4_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_across_waves_pre_fit4_bs.RData"))
     # set.seed(1234)
     # res_pos_neu_lw_across_waves_session3_fit1_bs <- resample(res_pos_neu_lw_across_waves$SESSION3$fit1, res_pos_neu_lw_across_waves$SESSION3$fit1$call$data, 500)
     # save(res_pos_neu_lw_across_waves_session3_fit1_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_across_waves_session3_fit1_bs.RData"))
     # set.seed(1234)
     # res_pos_neu_lw_across_waves_session3_fit2_bs <- resample(res_pos_neu_lw_across_waves$SESSION3$fit2, res_pos_neu_lw_across_waves$SESSION3$fit2$call$data, 500)
     # save(res_pos_neu_lw_across_waves_session3_fit2_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_across_waves_session3_fit2_bs.RData"))

     # Error below for "res_pos_neu_lw_across_waves_session3_fit3_bs" and "res_pos_neu_lw_across_waves_session3_fit4_bs"
     # (occurs at 79% of progress) with seed 1234 resolved by changing seed to 1235

       # Error in matrix(fit$a0[seq(lmu * nc)], nc, lmu, dimnames = list(classnames,  : 
       #   length of 'dimnames' [2] not equal to array extent
       # In addition: Warning message:
       # from glmnet C++ code (error code -1); Convergence for 1th lambda value not reached after maxit=100000 iterations; solutions for larger lambdas returned

     # set.seed(1235)
     # res_pos_neu_lw_across_waves_session3_fit3_bs <- resample(res_pos_neu_lw_across_waves$SESSION3$fit3, res_pos_neu_lw_across_waves$SESSION3$fit3$call$data, 500)
     # save(res_pos_neu_lw_across_waves_session3_fit3_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_across_waves_session3_fit3_bs.RData"))
     # set.seed(1235)
     # res_pos_neu_lw_across_waves_session3_fit4_bs <- resample(res_pos_neu_lw_across_waves$SESSION3$fit4, res_pos_neu_lw_across_waves$SESSION3$fit4$call$data, 500)
     # save(res_pos_neu_lw_across_waves_session3_fit4_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_across_waves_session3_fit4_bs.RData"))

     # set.seed(1234)
     # res_pos_neu_lw_across_waves_SESSION6_fit1_bs <- resample(res_pos_neu_lw_across_waves$SESSION6$fit1, res_pos_neu_lw_across_waves$SESSION6$fit1$call$data, 500)
     # save(res_pos_neu_lw_across_waves_SESSION6_fit1_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_across_waves_SESSION6_fit1_bs.RData"))
     # set.seed(1234)
     # res_pos_neu_lw_across_waves_SESSION6_fit2_bs <- resample(res_pos_neu_lw_across_waves$SESSION6$fit2, res_pos_neu_lw_across_waves$SESSION6$fit2$call$data, 500)
     # save(res_pos_neu_lw_across_waves_SESSION6_fit2_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_across_waves_SESSION6_fit2_bs.RData"))
# set.seed(1234)
# res_pos_neu_lw_across_waves_SESSION6_fit3_bs <- resample(res_pos_neu_lw_across_waves$SESSION6$fit3, res_pos_neu_lw_across_waves$SESSION6$fit3$call$data, 500)
# save(res_pos_neu_lw_across_waves_SESSION6_fit3_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_across_waves_SESSION6_fit3_bs.RData"))
     # set.seed(1234)
     # res_pos_neu_lw_across_waves_SESSION6_fit4_bs <- resample(res_pos_neu_lw_across_waves$SESSION6$fit4, res_pos_neu_lw_across_waves$SESSION6$fit4$call$data, 500)
     # save(res_pos_neu_lw_across_waves_SESSION6_fit4_bs, file = paste0(old_net_interv_stab_path, "res_pos_neu_lw_across_waves_SESSION6_fit4_bs.RData"))

     # print("Starting Set 3")
     # 
     # set.seed(1234)
     # res_pos_fif_lw_per_wave_pre_fit1_bs <- resample(res_pos_fif_lw_per_wave$PRE$fit1, res_pos_fif_lw_per_wave$PRE$fit1$call$data, 500)
     # save(res_pos_fif_lw_per_wave_pre_fit1_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_per_wave_pre_fit1_bs.RData"))
     # set.seed(1234)
     # res_pos_fif_lw_per_wave_pre_fit2_bs <- resample(res_pos_fif_lw_per_wave$PRE$fit2, res_pos_fif_lw_per_wave$PRE$fit2$call$data, 500)
     # save(res_pos_fif_lw_per_wave_pre_fit2_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_per_wave_pre_fit2_bs.RData"))
# set.seed(1234)
# res_pos_fif_lw_per_wave_pre_fit3_bs <- resample(res_pos_fif_lw_per_wave$PRE$fit3, res_pos_fif_lw_per_wave$PRE$fit3$call$data, 500)
# save(res_pos_fif_lw_per_wave_pre_fit3_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_per_wave_pre_fit3_bs.RData"))
     # set.seed(1234)
     # res_pos_fif_lw_per_wave_pre_fit4_bs <- resample(res_pos_fif_lw_per_wave$PRE$fit4, res_pos_fif_lw_per_wave$PRE$fit4$call$data, 500)
     # save(res_pos_fif_lw_per_wave_pre_fit4_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_per_wave_pre_fit4_bs.RData"))
     # set.seed(1234)
     # res_pos_fif_lw_per_wave_session3_fit1_bs <- resample(res_pos_fif_lw_per_wave$SESSION3$fit1, res_pos_fif_lw_per_wave$SESSION3$fit1$call$data, 500)
     # save(res_pos_fif_lw_per_wave_session3_fit1_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_per_wave_session3_fit1_bs.RData"))
     # set.seed(1234)
     # res_pos_fif_lw_per_wave_session3_fit2_bs <- resample(res_pos_fif_lw_per_wave$SESSION3$fit2, res_pos_fif_lw_per_wave$SESSION3$fit2$call$data, 500)
     # save(res_pos_fif_lw_per_wave_session3_fit2_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_per_wave_session3_fit2_bs.RData"))
# set.seed(1234)
# res_pos_fif_lw_per_wave_session3_fit3_bs <- resample(res_pos_fif_lw_per_wave$SESSION3$fit3, res_pos_fif_lw_per_wave$SESSION3$fit3$call$data, 500)
# save(res_pos_fif_lw_per_wave_session3_fit3_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_per_wave_session3_fit3_bs.RData"))
     # set.seed(1234)
     # res_pos_fif_lw_per_wave_session3_fit4_bs <- resample(res_pos_fif_lw_per_wave$SESSION3$fit4, res_pos_fif_lw_per_wave$SESSION3$fit4$call$data, 500)
     # save(res_pos_fif_lw_per_wave_session3_fit4_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_per_wave_session3_fit4_bs.RData"))
     # set.seed(1234)
     # res_pos_fif_lw_per_wave_SESSION6_fit1_bs <- resample(res_pos_fif_lw_per_wave$SESSION6$fit1, res_pos_fif_lw_per_wave$SESSION6$fit1$call$data, 500)
     # save(res_pos_fif_lw_per_wave_SESSION6_fit1_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_per_wave_SESSION6_fit1_bs.RData"))
     # set.seed(1234)
     # res_pos_fif_lw_per_wave_SESSION6_fit2_bs <- resample(res_pos_fif_lw_per_wave$SESSION6$fit2, res_pos_fif_lw_per_wave$SESSION6$fit2$call$data, 500)
     # save(res_pos_fif_lw_per_wave_SESSION6_fit2_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_per_wave_SESSION6_fit2_bs.RData"))
# set.seed(1234)
# res_pos_fif_lw_per_wave_SESSION6_fit3_bs <- resample(res_pos_fif_lw_per_wave$SESSION6$fit3, res_pos_fif_lw_per_wave$SESSION6$fit3$call$data, 500)
# save(res_pos_fif_lw_per_wave_SESSION6_fit3_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_per_wave_SESSION6_fit3_bs.RData"))
     # set.seed(1234)
     # res_pos_fif_lw_per_wave_SESSION6_fit4_bs <- resample(res_pos_fif_lw_per_wave$SESSION6$fit4, res_pos_fif_lw_per_wave$SESSION6$fit4$call$data, 500)
     # save(res_pos_fif_lw_per_wave_SESSION6_fit4_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_per_wave_SESSION6_fit4_bs.RData"))

     # print("Starting Set 4")
     # 
     # set.seed(1234)
     # res_pos_fif_lw_across_waves_pre_fit1_bs <- resample(res_pos_fif_lw_across_waves$PRE$fit1, res_pos_fif_lw_across_waves$PRE$fit1$call$data, 500)
     # save(res_pos_fif_lw_across_waves_pre_fit1_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_across_waves_pre_fit1_bs.RData"))
     # set.seed(1234)
     # res_pos_fif_lw_across_waves_pre_fit2_bs <- resample(res_pos_fif_lw_across_waves$PRE$fit2, res_pos_fif_lw_across_waves$PRE$fit2$call$data, 500)
     # save(res_pos_fif_lw_across_waves_pre_fit2_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_across_waves_pre_fit2_bs.RData"))
# set.seed(1234)
# res_pos_fif_lw_across_waves_pre_fit3_bs <- resample(res_pos_fif_lw_across_waves$PRE$fit3, res_pos_fif_lw_across_waves$PRE$fit3$call$data, 500)
# save(res_pos_fif_lw_across_waves_pre_fit3_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_across_waves_pre_fit3_bs.RData"))
     # set.seed(1234)
     # res_pos_fif_lw_across_waves_pre_fit4_bs <- resample(res_pos_fif_lw_across_waves$PRE$fit4, res_pos_fif_lw_across_waves$PRE$fit4$call$data, 500)
     # save(res_pos_fif_lw_across_waves_pre_fit4_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_across_waves_pre_fit4_bs.RData"))
     # set.seed(1234)
     # res_pos_fif_lw_across_waves_session3_fit1_bs <- resample(res_pos_fif_lw_across_waves$SESSION3$fit1, res_pos_fif_lw_across_waves$SESSION3$fit1$call$data, 500)
     # save(res_pos_fif_lw_across_waves_session3_fit1_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_across_waves_session3_fit1_bs.RData"))
     # set.seed(1234)
     # res_pos_fif_lw_across_waves_session3_fit2_bs <- resample(res_pos_fif_lw_across_waves$SESSION3$fit2, res_pos_fif_lw_across_waves$SESSION3$fit2$call$data, 500)
     # save(res_pos_fif_lw_across_waves_session3_fit2_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_across_waves_session3_fit2_bs.RData"))
# set.seed(1234)
# res_pos_fif_lw_across_waves_session3_fit3_bs <- resample(res_pos_fif_lw_across_waves$SESSION3$fit3, res_pos_fif_lw_across_waves$SESSION3$fit3$call$data, 500)
# save(res_pos_fif_lw_across_waves_session3_fit3_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_across_waves_session3_fit3_bs.RData"))
     # set.seed(1234)
     # res_pos_fif_lw_across_waves_session3_fit4_bs <- resample(res_pos_fif_lw_across_waves$SESSION3$fit4, res_pos_fif_lw_across_waves$SESSION3$fit4$call$data, 500)
     # save(res_pos_fif_lw_across_waves_session3_fit4_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_across_waves_session3_fit4_bs.RData"))
     # set.seed(1234)
     # res_pos_fif_lw_across_waves_SESSION6_fit1_bs <- resample(res_pos_fif_lw_across_waves$SESSION6$fit1, res_pos_fif_lw_across_waves$SESSION6$fit1$call$data, 500)
     # save(res_pos_fif_lw_across_waves_SESSION6_fit1_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_across_waves_SESSION6_fit1_bs.RData"))
     # set.seed(1234)
     # res_pos_fif_lw_across_waves_SESSION6_fit2_bs <- resample(res_pos_fif_lw_across_waves$SESSION6$fit2, res_pos_fif_lw_across_waves$SESSION6$fit2$call$data, 500)
     # save(res_pos_fif_lw_across_waves_SESSION6_fit2_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_across_waves_SESSION6_fit2_bs.RData"))
# set.seed(1234)
# res_pos_fif_lw_across_waves_SESSION6_fit3_bs <- resample(res_pos_fif_lw_across_waves$SESSION6$fit3, res_pos_fif_lw_across_waves$SESSION6$fit3$call$data, 500)
# save(res_pos_fif_lw_across_waves_SESSION6_fit3_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_across_waves_SESSION6_fit3_bs.RData"))
     # set.seed(1234)
     # res_pos_fif_lw_across_waves_SESSION6_fit4_bs <- resample(res_pos_fif_lw_across_waves$SESSION6$fit4, res_pos_fif_lw_across_waves$SESSION6$fit4$call$data, 500)
     # save(res_pos_fif_lw_across_waves_SESSION6_fit4_bs, file = paste0(old_net_interv_stab_path, "res_pos_fif_lw_across_waves_SESSION6_fit4_bs.RData"))