# ---------------------------------------------------------------------------- #
# Search for Auxiliary Variables
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
# Import data ----
# ---------------------------------------------------------------------------- #

processed_path <- file.path("data", "processed")

# Clean data (for credibility and demographics)

cln_dat <- readRDS(file.path(processed_path, "cln_dat.rds"))

# Overall network dataset

net_dat_all_to_s6 <- readRDS(file.path(processed_path, "net_dat_all_to_s6.rds"))

# ---------------------------------------------------------------------------- #
# Prepare data ----
# ---------------------------------------------------------------------------- #

# TODO




