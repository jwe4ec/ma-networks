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

net_interv_path <- file.path("results", "net_interv")

# For RR network

res_rr_pos_neu_lw_per_wave     <- readRDS(file.path(net_interv_path, "res_rr_pos_neu_lw_per_wave.rds"))
res_rr_pos_neu_lw_across_waves <- readRDS(file.path(net_interv_path, "res_rr_pos_neu_lw_across_waves.rds"))

res_rr_pos_fif_lw_per_wave     <- readRDS(file.path(net_interv_path, "res_rr_pos_fif_lw_per_wave.rds"))
res_rr_pos_fif_lw_across_waves <- readRDS(file.path(net_interv_path, "res_rr_pos_fif_lw_across_waves.rds"))

# For BBSIQ network

res_bb_pos_neu_lw_per_wave     <- readRDS(file.path(net_interv_path, "res_bb_pos_neu_lw_per_wave.rds"))
res_bb_pos_neu_lw_across_waves <- readRDS(file.path(net_interv_path, "res_bb_pos_neu_lw_across_waves.rds"))

res_bb_pos_fif_lw_per_wave     <- readRDS(file.path(net_interv_path, "res_bb_pos_fif_lw_per_wave.rds"))
res_bb_pos_fif_lw_across_waves <- readRDS(file.path(net_interv_path, "res_bb_pos_fif_lw_across_waves.rds"))

# ---------------------------------------------------------------------------- #
# Compute and export network stability ----
# ---------------------------------------------------------------------------- #

net_interv_stab_path <- file.path(net_interv_path, "stab")
dir.create(net_interv_stab_path)

# Define function to compute network stability at given wave and export

compute_export_net_stab <- function(res, wave) {
  # Bootstrap results at wave
  
  res_wave <- res[[wave]]
  
  set.seed(1234)
  res_bs <- resample(res_wave$fit, res_wave$fit$call$data, 500)
  
  # Create filename for bootstrapped results
  
  res_name  <- deparse(substitute(res))
  wave_name <- tolower(wave)
  
  res_bs_filename <- paste(res_name, wave_name, "bs.rds", sep = "_")
  
  saveRDS(res_bs, file.path(net_interv_stab_path, res_bs_filename))
}

# Run function

print("For RR network")

compute_export_net_stab(res_rr_pos_neu_lw_per_wave,     "PRE")
compute_export_net_stab(res_rr_pos_neu_lw_per_wave,     "SESSION3")
compute_export_net_stab(res_rr_pos_neu_lw_per_wave,     "SESSION6")

compute_export_net_stab(res_rr_pos_neu_lw_across_waves, "PRE")
compute_export_net_stab(res_rr_pos_neu_lw_across_waves, "SESSION3")
compute_export_net_stab(res_rr_pos_neu_lw_across_waves, "SESSION6")

compute_export_net_stab(res_rr_pos_fif_lw_per_wave,     "PRE")
compute_export_net_stab(res_rr_pos_fif_lw_per_wave,     "SESSION3")
compute_export_net_stab(res_rr_pos_fif_lw_per_wave,     "SESSION6")

compute_export_net_stab(res_rr_pos_fif_lw_across_waves, "PRE")
compute_export_net_stab(res_rr_pos_fif_lw_across_waves, "SESSION3")
compute_export_net_stab(res_rr_pos_fif_lw_across_waves, "SESSION6")

print("For BB network")

compute_export_net_stab(res_bb_pos_neu_lw_per_wave,     "PRE")
compute_export_net_stab(res_bb_pos_neu_lw_per_wave,     "SESSION3")
compute_export_net_stab(res_bb_pos_neu_lw_per_wave,     "SESSION6")

compute_export_net_stab(res_bb_pos_neu_lw_across_waves, "PRE")
compute_export_net_stab(res_bb_pos_neu_lw_across_waves, "SESSION3")
compute_export_net_stab(res_bb_pos_neu_lw_across_waves, "SESSION6")

compute_export_net_stab(res_bb_pos_fif_lw_per_wave,     "PRE")
compute_export_net_stab(res_bb_pos_fif_lw_per_wave,     "SESSION3")
compute_export_net_stab(res_bb_pos_fif_lw_per_wave,     "SESSION6")

compute_export_net_stab(res_bb_pos_fif_lw_across_waves, "PRE")
compute_export_net_stab(res_bb_pos_fif_lw_across_waves, "SESSION3")
compute_export_net_stab(res_bb_pos_fif_lw_across_waves, "SESSION6")