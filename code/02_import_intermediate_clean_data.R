# ---------------------------------------------------------------------------- #
# Import Intermediate Clean Data
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
# Import intermediate clean data tables and their POSIXct time zones ----
# ---------------------------------------------------------------------------- #

# Import intermediate clean files for 807 intent-to-treat participants from Public 
# Component ("public_v2.0.1.zip") of MindTrails Managing Anxiety study OSF project 
# - See Eberle et al., 2025, https://doi.org/10.5281/zenodo.17459153, or on GitHub:
# https://github.com/TeachmanLab/MT-Data-ManagingAnxietyStudy-Cleaning/tree/v2.0.1

## Import intermediate clean data tables into named list

int_cln_tbls_path      <- file.path("data", "intermediate_clean", "tables")
int_cln_tbls_filenames <- c("bbsiq.csv", "credibility.csv", "dass21_as.csv", "dass21_ds.csv", 
                            "demographic.csv", "oa.csv", "participant.csv", "rr.csv")
int_cln_tbls_paths     <- file.path(int_cln_tbls_path, int_cln_tbls_filenames)

int_cln_tbls        <- lapply(int_cln_tbls_paths, read.csv)
names(int_cln_tbls) <- tools::file_path_sans_ext(int_cln_tbls_filenames)

## Import time zones of tables' "_as_POSIXct" columns

POSIXct_time_zones <- read.csv(file.path("data", "intermediate_clean", "POSIXct_time_zones.csv"))

# ---------------------------------------------------------------------------- #
# Convert POSIXct columns to POSIXct with correct time zones ----
# ---------------------------------------------------------------------------- #

# Define function to convert a table's "_as_POSIXct" columns to POSIXct with time 
# zones specified in "POSIXct_time_zones"

convert_POSIXct_cols <- function(table, table_name, POSIXct_time_zones) {
  POSIXct_cols <- names(table)[grepl("_as_POSIXct", names(table))]
  
  for (col in POSIXct_cols) {
    tzone <- POSIXct_time_zones$tzone[POSIXct_time_zones$table == table_name &
                                        POSIXct_time_zones$col == col]
    
    table[[col]] <- as.POSIXct(table[[col]], tzone)
    
    cat("Converted", table_name, "column", col, "to POSIXct with time zone", tzone, "\n")
  }
  
  return(table)
}

# Run function for all tables in list

for (i in 1:length(int_cln_tbls)) {
  table_name <- names(int_cln_tbls)[i]
  table <- int_cln_tbls[[i]]
  
  table <- convert_POSIXct_cols(table, table_name, POSIXct_time_zones)
  
  int_cln_tbls[[i]] <- table
}

# ---------------------------------------------------------------------------- #
# Export data ----
# ---------------------------------------------------------------------------- #

processed_path <- file.path("data", "processed")
dir.create(processed_path)

saveRDS(int_cln_tbls, file.path(processed_path, "int_cln_dat.rds"))