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
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./code/01a_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

# Import data with final analysis sample

load("./data/intermediate/net_dat_all_to_s6.RData")

# Import restricted credibility data

  # TODO: Update with new "_rest.csv" file (instead of "_rest.csv")





credibility_raw_rest2 <- read.csv("./data/intermediate/credibility_raw_rest2.csv")

# ---------------------------------------------------------------------------- #
# Further clean credibility data ----
# ---------------------------------------------------------------------------- #

# TODO: Restrict to final analysis sample




