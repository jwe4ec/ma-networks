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
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./code/01a_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control_temporal()

# Load packages

pkgs <- c("mgm", "ggplot2", "dplyr", "cowplot")

groundhog.library(pkgs, groundhog_day)

# ---------------------------------------------------------------------------- #
# Import stability results ----
# ---------------------------------------------------------------------------- #

# TODO: Change case of imported objects (and the saved objects they came from and
# the script "compute_net_interv_stability.R" that created them)





net_interv_stab_path <- "./results/net_interv/stab/"

load(paste0(net_interv_stab_path, "res_rev_pos_neu_lw_per_wave_pre_fit4_bs.RData"))
load(paste0(net_interv_stab_path, "res_rev_pos_neu_lw_per_wave_session3_fit4_bs.RData"))
load(paste0(net_interv_stab_path, "res_rev_pos_neu_lw_per_wave_SESSION6_fit4_bs.RData"))

load(paste0(net_interv_stab_path, "res_rev_pos_neu_lw_across_waves_pre_fit4_bs.RData"))
load(paste0(net_interv_stab_path, "res_rev_pos_neu_lw_across_waves_session3_fit4_bs.RData"))
load(paste0(net_interv_stab_path, "res_rev_pos_neu_lw_across_waves_SESSION6_fit4_bs.RData"))

load(paste0(net_interv_stab_path, "res_rev_pos_fif_lw_per_wave_pre_fit4_bs.RData"))
load(paste0(net_interv_stab_path, "res_rev_pos_fif_lw_per_wave_session3_fit4_bs.RData"))
load(paste0(net_interv_stab_path, "res_rev_pos_fif_lw_per_wave_SESSION6_fit4_bs.RData"))

load(paste0(net_interv_stab_path, "res_rev_pos_fif_lw_across_waves_pre_fit4_bs.RData"))
load(paste0(net_interv_stab_path, "res_rev_pos_fif_lw_across_waves_session3_fit4_bs.RData"))
load(paste0(net_interv_stab_path, "res_rev_pos_fif_lw_across_waves_SESSION6_fit4_bs.RData"))

# ---------------------------------------------------------------------------- #
# Create plots ----
# ---------------------------------------------------------------------------- #

# Plot summary of all sampling distributions 

labels <- c("Pos. CBM-I", "Anx. Freq.", "Anx. Sev.", "Sit. Avoid", "Work Imp.", 
            "Soc. Imp.", "Neg. Bias", "Lack of Pos. Bias")

# Define function to plot (a) proportion of bootstrap samples in which each edge 
# was selected and (b) quantiles of edge bootstrap sampling distributions
# (for interpretive caveats, see Williams, 2021; https://doi.org/10.31234/osf.io/kjh2f)

create_stab_plot <- function(res_bs, labels) {
  axis.ticks <- c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
  
  # plotRes(res_bs, labels = labels, axis.ticks = axis.ticks)                            # alpha = .10 (default)
  # plotRes(res_bs, labels = labels, axis.ticks = axis.ticks, quantiles = c(.025, .975)) # alpha = .05
  plotRes(res_bs, labels = labels, axis.ticks = axis.ticks, quantiles = c(.005, .995))   # alpha = .01
}

# Run function

create_stab_plot(res_rev_pos_neu_lw_per_wave_pre_fit4_bs, labels)
create_stab_plot(res_rev_pos_neu_lw_per_wave_session3_fit4_bs, labels)
create_stab_plot(res_rev_pos_neu_lw_per_wave_SESSION6_fit4_bs, labels)

create_stab_plot(res_rev_pos_neu_lw_across_waves_pre_fit4_bs, labels)
create_stab_plot(res_rev_pos_neu_lw_across_waves_session3_fit4_bs, labels)
create_stab_plot(res_rev_pos_neu_lw_across_waves_SESSION6_fit4_bs, labels)

create_stab_plot(res_rev_pos_fif_lw_per_wave_pre_fit4_bs, labels)
create_stab_plot(res_rev_pos_fif_lw_per_wave_session3_fit4_bs, labels)
create_stab_plot(res_rev_pos_fif_lw_per_wave_SESSION6_fit4_bs, labels)

create_stab_plot(res_rev_pos_fif_lw_across_waves_pre_fit4_bs, labels)
create_stab_plot(res_rev_pos_fif_lw_across_waves_session3_fit4_bs, labels)
create_stab_plot(res_rev_pos_fif_lw_across_waves_SESSION6_fit4_bs, labels)