# ---------------------------------------------------------------------------- #
# Further Clean Other Data
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

# Clean data

cln_dat <- readRDS(file.path("data", "processed", "cln_dat.rds"))

# ---------------------------------------------------------------------------- #
# Further clean credibility data ----
# ---------------------------------------------------------------------------- #

# Extract credibility data, which is available for 803 participants

cred_dat <- cln_dat$credibility

stopifnot(nrow(cred_dat) == 803)

# TODO (prepare data to search for auxiliary variables)




