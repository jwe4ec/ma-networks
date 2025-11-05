# ---------------------------------------------------------------------------- #
# Create Network Intervention Stability Plots
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
# Import results ----
# ---------------------------------------------------------------------------- #

# Point estimates

net_interv_path <- file.path("results", "net_interv")

res_rr_files <- c("res_rr_pos_neu_lw_per_wave.rds",
                  "res_rr_pos_neu_lw_across_waves.rds",
                  "res_rr_pos_fif_lw_per_wave.rds",
                  "res_rr_pos_fif_lw_across_waves.rds")

res_bb_files <- c("res_bb_pos_neu_lw_per_wave.rds",
                  "res_bb_pos_neu_lw_across_waves.rds",
                  "res_bb_pos_fif_lw_per_wave.rds",
                  "res_bb_pos_fif_lw_across_waves.rds")
           
res_rr_net_ls <- lapply(res_rr_files, function(f) readRDS(file.path(net_interv_path, f)))
res_bb_net_ls <- lapply(res_bb_files, function(f) readRDS(file.path(net_interv_path, f)))

names(res_rr_net_ls) <- tools::file_path_sans_ext(res_rr_files)
names(res_bb_net_ls) <- tools::file_path_sans_ext(res_bb_files)

# Confidence intervals

net_interv_stab_path <- file.path(net_interv_path, "stab")

res_bs_rr_net_ls <- readRDS(file.path(net_interv_stab_path, "res_bs_rr_net_ls.rds"))
res_bs_bb_net_ls <- readRDS(file.path(net_interv_stab_path, "res_bs_bb_net_ls.rds"))

# ---------------------------------------------------------------------------- #
# Create plots ----
# ---------------------------------------------------------------------------- #

# Define function to plot (a) proportion of bootstrap samples in which each edge 
# was selected and (b) quantiles of edge bootstrap sampling distributions
# (for interpretive caveats, see Williams, 2021; https://doi.org/10.31234/osf.io/kjh2f )

create_stab_plot <- function(res_bs) {
  vars <- res_bs$vars
  wave <- res_bs$wave
  
  labels <- sub(paste0(".", wave), "", vars)
    
  pos_cbm_contrasts <- c("positive_vs_neutral", "positive_vs_fifty_fifty")
  
  labels[labels %in% pos_cbm_contrasts]   <- "Pos. CBM-I"
  labels[labels == "anxious_freq"]        <- "Anx. Freq."
  labels[labels == "anxious_sev"]         <- "Anx. Sev."
  labels[labels == "avoid"]               <- "Sit. Avoid"
  labels[labels == "interfere"]           <- "Work Imp."
  labels[labels == "interfere_social"]    <- "Soc. Imp."
  labels[labels == "rr_neg_thr_mean"]     <- "Neg. Bias"
  labels[labels == "rr_pos_thr_mean_rev"] <- "Lack of Pos. Bias"
  labels[labels == "bbsiq_neg_mean"]      <- "Neg. Bias"
  
  axis.ticks <- c(-1.25, -1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1, 1.25)
  
  # TODO: Finalize below
  
  
  
  
  
  # plotRes(res_bs, labels = labels, axis.ticks = axis.ticks)                            # alpha = .10 (default)
  # plotRes(res_bs, labels = labels, axis.ticks = axis.ticks, quantiles = c(.025, .975)) # alpha = .05
  plotRes(res_bs, labels = labels, axis.ticks = axis.ticks, quantiles = c(.005, .995))   # alpha = .01
}

# TODO (continue below): Run function

create_stab_plot(res_bs_rr_net_ls$res_rr_pos_neu_lw_per_wave_PRE_bs)
create_stab_plot(res_bs_rr_net_ls$res_rr_pos_neu_lw_per_wave_SESSION6_bs)


create_stab_plot(res_bs_bb_net_ls$res_bb_pos_neu_lw_per_wave_PRE_bs)





# TODO: Likely remove this script and instead compute CI plots in "create_net_interv_plots_tbls.R"




