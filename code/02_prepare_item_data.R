# ---------------------------------------------------------------------------- #
# Prepare Item-Level Data
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# The present script imports a few potential sources of data for the Managing Anxiety
# study and determines which source(s) to use

# 1. Clean data files ("R34_Cronbach.csv" and "R34_FinalData_New_v02.csv") from the 
#    OSF project (https://osf.io/3b67v) for the Managing Anxiety Study main outcomes 
#    paper (https://doi.org/g62s). "R34_Cronbach.csv" has item-level data at baseline, 
#    whereas "R34_FinalData_New_v02.csv" has scale-level data at multiple time points.

#    "R34_FinalData_New_v02.csv" is generated from "FinalData-28Feb20_v02.csv" using 
#    "Script1_DataPrep.R" from the OSF project. "FinalData-28Feb20_v02.csv", which is 
#    not on the OSF project, is a newer version of the data file "FinalData-28Feb20.csv" 
#    outputted by the script "R34.ipynb" in the study's Data Cleaning folder 
#    (https://bit.ly/3CLi5It) on GitHub. Sonia Baee indicated that the files in the 
#    Data Cleaning folder and the Main Outcomes folder (https://bit.ly/3FHRz4G) on 
#    GitHub are out of date given file losses when switching laptops. Code to generate 
#    "FinalData-28Feb20_v02.csv" is not presently available as a result.

#    Code used to generate "R34_Cronbach.csv", likely written by Sonia Baee, is not
#    available. However, the file was likely created by adjusting the script "R34.ipynb" 
#    in the study's Data Cleaning folder (https://bit.ly/3CLi5It) on GitHub.

# 2. 26 raw data files obtained from Sonia Baee on 9/3/2020, who stated on that 
#    date that they represent the latest version of the database on the R34 server 
#    and that she obtained them from Claudia Calicho-Mamani. These are Set A from
#    "public-v1.0.0.zip" on the Public Component (https://osf.io/2x3jq/) of the
#    OSF project for the Managing Anxiety study (https://osf.io/pvd67/).

# 3. A partial set of 20 raw data files obtained from Sonia Baee on 1/18/2023.
#    These are Set B from "public-v1.0.0.zip".

# It also imports "notes.csv" obtained from Sonia Baee on 11/24/2021. A "notes.csv" is
# loaded by the "R34.ipynb" script on GitHub (whose last commit "b3c370b" was 4/16/2020).

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
# Import clean data files from main outcomes paper ----
# ---------------------------------------------------------------------------- #

# Import baseline items and longitudinal scales from OSF project for Managing Anxiety 
# main outcomes paper (see Source 1 above for more info)

dat_main_bl_items  <- read.csv("./data/source/clean_from_main_paper/R34_Cronbach.csv")
dat_main_lg_scales <- read.csv("./data/source/clean_from_main_paper/R34_FinalData_New_v02.csv")

# ---------------------------------------------------------------------------- #
# Import raw data files from Sonia ----
# ---------------------------------------------------------------------------- #

# Obtain file names of Sets A and B of raw CSV data files from Sonia (see Sources
# 2 and 3 above for more info)

raw_data_dir_son_a <- paste0(wd_dir, "/data/source/raw_from_sonia_a/")
raw_data_dir_son_b <- paste0(wd_dir, "/data/source/raw_from_sonia_b/")

raw_filenames_son_a <- list.files(raw_data_dir_son_a, pattern = "\\.csv$")
raw_filenames_son_b <- list.files(raw_data_dir_son_b, pattern = "\\.csv$")

# Import raw data files and store them in list

raw_dat_son_a <- lapply(paste0(raw_data_dir_son_a, raw_filenames_son_a), read.csv)
raw_dat_son_b <- lapply(paste0(raw_data_dir_son_b, raw_filenames_son_b), read.csv)

# Name each raw data file in list

names(raw_dat_son_a) <- tools::file_path_sans_ext(raw_filenames_son_a)
names(raw_dat_son_b) <- tools::file_path_sans_ext(raw_filenames_son_b)

# ---------------------------------------------------------------------------- #
# Import "notes.csv" from Sonia ----
# ---------------------------------------------------------------------------- #

notes <- read.csv("./data/source/notes_from_sonia/notes.csv")

# ---------------------------------------------------------------------------- #
# Decide which data to use for network analyses ----
# ---------------------------------------------------------------------------- #

# TODO: Document decisions





cln_dat_bl <- dat_main_bl_items
cln_dat    <- dat_main_lg_scales

raw_dat   <- raw_dat_son_a
raw_dat_b <- raw_dat_son_b

# ---------------------------------------------------------------------------- #
# Clean "notes.csv" ----
# ---------------------------------------------------------------------------- #

# Rename columns (per "R34.ipynb" script)

names(notes)[names(notes) == "final4.participantID"]             <- "CL"
names(notes)[names(notes) == "Julie_participants.participantID"] <- "SB"

  # Also rename "participantID" (not done in "R34.ipynb")

names(notes)[names(notes) == "participantID"] <- "participant_id"

# Create test account column (per "R34.ipynb" script)

notes$test <- ifelse(notes$notes == "Test account found after original analyses were run", 1, 0)

# Identify server error IDs

note_server_error <- "Not included in original dataset due to server error"

server_error_pids <- notes$participant_id[is.na(notes$SB) & 
                                            (!is.na(notes$notes) & notes$notes == note_server_error)]
length(server_error_pids) == 36

# TODO: Continue reviewing "R34.ipynb"





# ---------------------------------------------------------------------------- #
# Extract clean longitudinal scale data into separate tables ----
# ---------------------------------------------------------------------------- #

# Remove renamed columns, computed columns (besides "score"-related columns), and 
# dummy-coded condition columns, which were all defined in "Script1_DataPrep.R", 
# which is on the OSF project for the main outcomes paper (https://osf.io/5wscm/)

renamed_cols <- c("RRNegative_PRE", "RRNegative_SESSION3", "RRNegative_SESSION6",
                  "RRPositive_PRE", "RRPositive_SESSION3", "RRPositive_SESSION6",
                  "OASISScore_PRE", "OASISScore_SESSION1", "OASISScore_SESSION2",
                  "OASISScore_SESSION3", "OASISScore_SESSION4", "OASISScore_SESSION5",
                  "OASISScore_SESSION6")
computed_cols <- c("negativeBBSIQ_PRE", "negativeBBSIQ_SESSION3", "negativeBBSIQ_SESSION6")
dummy_cols <- c("ANXIETY", "POSITIVE", "FIFTY_FIFTY", "POSITIVEANXIETY",
                "POSITIVENEUTRAL", "FIFTY_FIFTYANXIETY", "FIFTY_FIFTYNEUTRAL",
                "NONEANXIETY")

cln_dat <- cln_dat[, names(cln_dat)[!(names(cln_dat) %in% c(renamed_cols, computed_cols, dummy_cols))]]

# Remove dropout-related columns, which seem to have been created in some version
# of "R34.ipynb", which, albeit outdated, is in the Data Cleaning folder on GitHub
# (https://github.com/jwe4ec/MT-Data-ManagingAnxietyStudy/tree/master/Data%20Cleaning)

dropout_cols <- names(cln_dat)[grep("_dropout", names(cln_dat))]

cln_dat <- cln_dat[, names(cln_dat)[!(names(cln_dat) %in% dropout_cols)]]

# Rename "participantID" as "participant_id"

names(cln_dat)[names(cln_dat) == "participantID"] <- "participant_id"

# Extract column names for separate tables

bbsiq_cols       <- names(cln_dat)[grep("bbsiq_",       names(cln_dat), ignore.case = TRUE)]
dass_as_cols     <- names(cln_dat)[grep("dass_as_",     names(cln_dat))]
dass_ds_cols     <- names(cln_dat)[grep("dass_ds_",     names(cln_dat))]
demographic_cols <- names(cln_dat)[grep("demographic_", names(cln_dat))]
oa_cols          <- names(cln_dat)[grep("oasis_",       names(cln_dat), ignore.case = TRUE)]
participant_cols <- names(cln_dat)[grep("participant",  names(cln_dat))]
rr_cols          <- names(cln_dat)[grep("RR_",          names(cln_dat), ignore.case = TRUE)]

setdiff(names(cln_dat), c(bbsiq_cols, dass_as_cols, dass_ds_cols, demographic_cols,
                          oa_cols, participant_cols, rr_cols))

# Define function to extract data into separate tables

create_sep_dat <- function(dat, table_cols) {
  sep_dat <- vector("list", length = length(table_cols))
  
  for (i in 1:length(table_cols)) {
    if ("participant_id" %in% table_cols[[i]]) {
      sep_dat[[i]] <- dat[, table_cols[[i]]]
    } else {
      sep_dat[[i]] <- dat[, c("participant_id", table_cols[[i]])]
    }
  }
  
  names(sep_dat) <- names(table_cols)
  
  return(sep_dat)
}

# Run function

table_cols <- list(bbsiq       = bbsiq_cols,
                   dass_as     = dass_as_cols,
                   dass_ds     = dass_ds_cols,
                   demographic = demographic_cols,
                   oa          = oa_cols,
                   participant = participant_cols,
                   rr          = rr_cols)

sep_dat_wide <- create_sep_dat(cln_dat, table_cols)

# ---------------------------------------------------------------------------- #
# Convert clean longitudinal scales data tables into long format ----
# ---------------------------------------------------------------------------- #

# Alphabetize columns of each table and then sort them by time point, retaining
# "participant_id" as the first column

for (i in 1:length(sep_dat_wide)) {
  sep_dat_wide[[i]] <- sep_dat_wide[[i]][, sort(names(sep_dat_wide[[i]]))]
  
  if (names(sep_dat_wide[i]) %in% c("demographic", "participant")) {
    sep_dat_wide[[i]] <- sep_dat_wide[[i]]
  } else {
    sep_dat_wide[[i]] <- 
      sep_dat_wide[[i]][, c("participant_id",
                            names(sep_dat_wide[[i]])[grep("ELIGIBLE", names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("PRE",      names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("SESSION1", names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("SESSION2", names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("SESSION3", names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("SESSION4", names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("SESSION5", names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("SESSION6", names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("SESSION7", names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("SESSION8", names(sep_dat_wide[[i]]))],
                            names(sep_dat_wide[[i]])[grep("POST",     names(sep_dat_wide[[i]]))])]
  }
}

# Identify repeated-measures columns for each score

bbsiq_physical_score_cols <- names(sep_dat_wide$bbsiq)[grep("bbsiq_physical_score", names(sep_dat_wide$bbsiq))]
bbsiq_threat_score_cols   <- names(sep_dat_wide$bbsiq)[grep("bbsiq_threat_score",   names(sep_dat_wide$bbsiq))]

dass_as_score_cols        <- names(sep_dat_wide$dass_as)[grep("dass_as_score",      names(sep_dat_wide$dass_as))]

dass_ds_score_cols        <- names(sep_dat_wide$dass_ds)[grep("dass_ds_score",      names(sep_dat_wide$dass_ds))]

oasis_score_cols          <- names(sep_dat_wide$oa)[grep("oasis_score",             names(sep_dat_wide$oa))]

RR_negative_nf_score_cols <- names(sep_dat_wide$rr)[grep("RR_negative_nf_score",    names(sep_dat_wide$rr))]
RR_negative_ns_score_cols <- names(sep_dat_wide$rr)[grep("RR_negative_ns_score",    names(sep_dat_wide$rr))]
RR_positive_pf_score_cols <- names(sep_dat_wide$rr)[grep("RR_positive_pf_score",    names(sep_dat_wide$rr))]
RR_positive_ps_score_cols <- names(sep_dat_wide$rr)[grep("RR_positive_ps_score",    names(sep_dat_wide$rr))]

# Convert repeated-measures tables to long format

sep_dat <- vector("list", length = length(sep_dat_wide))
names(sep_dat) <- names(sep_dat_wide)

sep_dat$demographic <- sep_dat_wide$demographic
sep_dat$participant <- sep_dat_wide$participant

sep_dat$bbsiq <- reshape(sep_dat_wide$bbsiq, 
                         direction = "long",
                         idvar = "participant_id",
                         timevar = "session",
                         varying = list(bbsiq_physical_score = bbsiq_physical_score_cols,
                                        bbsiq_threat_score = bbsiq_threat_score_cols),
                         v.names = c("bbsiq_physical_score", "bbsiq_threat_score"),
                         times = c("PRE", "SESSION3", "SESSION6", "SESSION8", "POST"))

sep_dat$dass_as <- reshape(sep_dat_wide$dass_as, 
                           direction = "long",
                           idvar = "participant_id",
                           timevar = "session",
                           varying = list(dass_as_score = dass_as_score_cols),
                           v.names = "dass_as_score",
                           times = c("ELIGIBLE", "SESSION8", "POST"))

sep_dat$dass_ds <- reshape(sep_dat_wide$dass_ds, 
                           direction = "long",
                           idvar = "participant_id",
                           timevar = "session",
                           varying = list(dass_ds_score = dass_ds_score_cols),
                           v.names = "dass_ds_score",
                           times = c("PRE", "SESSION3", "SESSION6", "SESSION8", "POST"))

sep_dat$oa <- reshape(sep_dat_wide$oa, 
                      direction = "long",
                      idvar = "participant_id",
                      timevar = "session",
                      varying = list(oasis_score = oasis_score_cols),
                      v.names = "oasis_score",
                      times = c("PRE", "SESSION1", "SESSION2", "SESSION3", "SESSION4",
                                "SESSION5", "SESSION6", "SESSION7", "SESSION8", "POST"))

sep_dat$rr <- reshape(sep_dat_wide$rr, 
                      direction = "long",
                      idvar = "participant_id",
                      timevar = "session",
                      varying = list(RR_negative_nf_score = RR_negative_nf_score_cols,
                                     RR_negative_ns_score = RR_negative_ns_score_cols,
                                     RR_positive_pf_score = RR_positive_pf_score_cols,
                                     RR_positive_ps_score = RR_positive_ps_score_cols),
                      v.names = c("RR_negative_nf_score", "RR_negative_ns_score",
                                  "RR_positive_pf_score", "RR_positive_ps_score"),
                      times = c("PRE", "SESSION3", "SESSION6", "SESSION8", "POST"))

# Remove rows that have only NAs for all scores, as in most cases such rows would
# mean either that the assessment was not administered at that time point or that
# the participant did not complete the assessment at that time point. But retain
# known exceptions in which participants' responses would yield NA for all scores.

for (i in 1:length(sep_dat)) {
  df_name <- names(sep_dat[i])
  df <- sep_dat[[i]]
  
  if (!(df_name %in% c("demographic", "participant"))) {
    # Identify rows that have only NAs for all scores
    
    score_cols <- names(df)[grep("_score", names(df))]
    
    idx_all_scores_NA <- which(rowSums(is.na(df[score_cols])) == length(score_cols))
    
    # Specify exceptions leading to NA for all scores in a given table
    
    exceptions <- NULL
    
    if (df_name == "bbsiq") {
      # - P 481's items were all 0 at "PRE"
      # - P 1385's items were all "prefer not to answer" at Sessions 6 and 8
      
      exceptions <- c(which(df[["participant_id"]] == 481  & df[["session"]] == "PRE"),
                      which(df[["participant_id"]] == 1385 & df[["session"]] %in% paste0("SESSION", c(6, 8))))
    } else if (df_name == "rr") {
      # - P 1385's items were all "prefer not to answer" at Sessions 6 and 8
      
      exceptions <- which(df[["participant_id"]] == 1385 & df[["session"]] %in% paste0("SESSION", c(6, 8)))
    }
    
    # Remove rows
    
    idx_remove <- setdiff(idx_all_scores_NA, exceptions)

    sep_dat[[i]] <- df[-idx_remove, ]
  }
}

# Rename columns in "participant" table for comparability with Set A

names(sep_dat$participant)[names(sep_dat$participant) == "participant_cbm_condition"] <- "cbmCondition"
names(sep_dat$participant)[names(sep_dat$participant) == "participant_prime"]         <- "prime"

# ---------------------------------------------------------------------------- #
# Compute "session_only" in clean longitudinal scales data ----
# ---------------------------------------------------------------------------- #

# Given that "session" column in "dass21_as" table may contain both session information 
# and eligibility status (unlike original "session" column in Set A, which contains both 
# "ELIGIBLE" and "" entries, original "session" column in clean data has no "" entries), 
# create new column "session_only" with "ELIGIBLE" entries of original "session" column 
# recoded as "Eligibility" (to reflect that these were collected at eligibility screener).

for (i in 1:length(sep_dat)) {
  if (!(names(sep_dat[i]) %in% c("demographic", "participant"))) {
    if (names(sep_dat[i]) == "dass_as") {
      sep_dat[[i]][, "session_only"] <- sep_dat[[i]][, "session"]
      sep_dat[[i]][sep_dat[[i]][, "session_only"] == "ELIGIBLE", "session_only"] <- "Eligibility"
    } else {
      names(sep_dat[[i]])[names(sep_dat[[i]]) == "session"] <- "session_only"
    }
  }
}

# ---------------------------------------------------------------------------- #
# Investigate "FIXED" tables in Set A ----
# ---------------------------------------------------------------------------- #

# Three raw data files have "FIXED" in filenames ("CIHS_Feb_02_2019_FIXED.csv",
# "DASS21_DS_recovered_Feb_02_2019_FIXED.csv", "TaskLog_final_FIXED.csv"). Only
# "CIHS_Feb_02_2019_FIXED.csv" has a corresponding file without "FIXED" in the
# filename: "CIHS_recovered_Feb_02_2019.csv".

names(raw_dat)[grep("FIXED", names(raw_dat))]

# Compare "CIHS_recovered_Feb_02_2019" with "CIHS_Feb_02_2019_FIXED". All their
# shared columns have the same values, but "CIHS_Feb_02_2019_FIXED" includes two
# additional columns ("datetime" and "corrected_session"). Thus, use the "FIXED"
# version and remove "CIHS_recovered_Feb_02_2019" from the list.

shared_cihs_cols <- intersect(names(raw_dat$CIHS_recovered_Feb_02_2019), 
                              names(raw_dat$CIHS_Feb_02_2019_FIXED))
all(raw_dat$CIHS_recovered_Feb_02_2019[, shared_cihs_cols] == 
      raw_dat$CIHS_Feb_02_2019_FIXED[, shared_cihs_cols])

added_cihs_cols <- setdiff(names(raw_dat$CIHS_Feb_02_2019_FIXED), 
                           names(raw_dat$CIHS_recovered_Feb_02_2019))

head(raw_dat$CIHS_Feb_02_2019_FIXED[, c("date", "datetime")])

table(raw_dat$CIHS_Feb_02_2019_FIXED$session)
table(raw_dat$CIHS_Feb_02_2019_FIXED$corrected_session)

raw_dat$CIHS_recovered_Feb_02_2019 <- NULL

# ---------------------------------------------------------------------------- #
# Investigate other repeated tables in Set A ----
# ---------------------------------------------------------------------------- #

# Compare "DD_recovered_Feb_02_2019" with "DD_FU_recovered_Feb_02_2019". The "FU"
# version does not contain rows where "session" is "PRE" and does not contain the
# columns "average_amount" and "average_freq". After recoding "True" and "False"
# to "TRUE" and "FALSE" for columns "q1_noAns" and "q2_noAns" in the "FU" version,
# all entries on shared columns for rows where "session" is not "PRE" are identical.
# Thus, use the full version and remove "DD_FU_recovered_Feb_02_2019" from the list.

table(raw_dat$DD_recovered_Feb_02_2019$session, useNA = "always")
table(raw_dat$DD_FU_recovered_Feb_02_2019$session, useNA = "always")

raw_dat$DD_FU_recovered_Feb_02_2019$q1_noAns[raw_dat$DD_FU_recovered_Feb_02_2019$q1_noAns == "True"]  <- "TRUE"
raw_dat$DD_FU_recovered_Feb_02_2019$q1_noAns[raw_dat$DD_FU_recovered_Feb_02_2019$q1_noAns == "False"] <- "FALSE"
raw_dat$DD_FU_recovered_Feb_02_2019$q2_noAns[raw_dat$DD_FU_recovered_Feb_02_2019$q2_noAns == "True"]  <- "TRUE"
raw_dat$DD_FU_recovered_Feb_02_2019$q2_noAns[raw_dat$DD_FU_recovered_Feb_02_2019$q2_noAns == "False"] <- "FALSE"

shared_dd_cols <- intersect(names(raw_dat$DD_recovered_Feb_02_2019),
                            names(raw_dat$DD_FU_recovered_Feb_02_2019))

all(raw_dat$DD_recovered_Feb_02_2019[raw_dat$DD_recovered_Feb_02_2019$session != "PRE", shared_dd_cols] == 
      raw_dat$DD_FU_recovered_Feb_02_2019[, shared_dd_cols])

added_dd_cols <- setdiff(names(raw_dat$DD_recovered_Feb_02_2019), 
                         names(raw_dat$DD_FU_recovered_Feb_02_2019))

raw_dat$DD_FU_recovered_Feb_02_2019 <- NULL

# ---------------------------------------------------------------------------- #
# Investigate repeated tables in Set B ----
# ---------------------------------------------------------------------------- #

# Compare "DD_02_02_2019" with "DD_FU_02_02_2019". Same situation as the DD tables
# in Set A. Thus, use the full version and remove "DD_FU_02_02_2019" from the list.

table(raw_dat_b$DD_02_02_2019$session, useNA = "always")
table(raw_dat_b$DD_FU_02_02_2019$session, useNA = "always")

raw_dat_b$DD_FU_02_02_2019$q1_noAns[raw_dat_b$DD_FU_02_02_2019$q1_noAns == "True"]  <- "TRUE"
raw_dat_b$DD_FU_02_02_2019$q1_noAns[raw_dat_b$DD_FU_02_02_2019$q1_noAns == "False"] <- "FALSE"
raw_dat_b$DD_FU_02_02_2019$q2_noAns[raw_dat_b$DD_FU_02_02_2019$q2_noAns == "True"]  <- "TRUE"
raw_dat_b$DD_FU_02_02_2019$q2_noAns[raw_dat_b$DD_FU_02_02_2019$q2_noAns == "False"] <- "FALSE"

shared_dd_cols_b <- intersect(names(raw_dat_b$DD_02_02_2019),
                              names(raw_dat_b$DD_FU_02_02_2019))

all(raw_dat_b$DD_02_02_2019[raw_dat_b$DD_02_02_2019$session != "PRE", shared_dd_cols_b] == 
      raw_dat_b$DD_FU_02_02_2019[, shared_dd_cols_b])

added_dd_cols_b <- setdiff(names(raw_dat_b$DD_02_02_2019), 
                           names(raw_dat_b$DD_FU_02_02_2019))

raw_dat_b$DD_FU_02_02_2019 <- NULL

# ---------------------------------------------------------------------------- #
# Shorten table names in Set A ----
# ---------------------------------------------------------------------------- #

names(raw_dat)[names(raw_dat) == "AnxietyTriggers_recovered_Feb_02_2019"]       <- "anxiety_triggers"
names(raw_dat)[names(raw_dat) == "BBSIQ_recovered_Feb_02_2019"]                 <- "bbsiq"
names(raw_dat)[names(raw_dat) == "CC_recovered_Feb_02_2019"]                    <- "cc"
names(raw_dat)[names(raw_dat) == "CIHS_Feb_02_2019_FIXED"]                      <- "cihs"
names(raw_dat)[names(raw_dat) == "Credibility_recovered_Feb_02_2019"]           <- "credibility"
names(raw_dat)[names(raw_dat) == "DASS21_AS_recovered_Feb_02_2019"]             <- "dass21_as"
names(raw_dat)[names(raw_dat) == "DASS21_DS_recovered_Feb_02_2019_FIXED"]       <- "dass21_ds"
names(raw_dat)[names(raw_dat) == "DD_recovered_Feb_02_2019"]                    <- "dd"
names(raw_dat)[names(raw_dat) == "Demographic_recovered_Feb_02_2019"]           <- "demographic"
names(raw_dat)[names(raw_dat) == "EmailLogDAO_recovered_Feb_02_2019"]           <- "email_log"
names(raw_dat)[names(raw_dat) == "GiftLogDAO_recovered_Feb_02_2019_redacted"]   <- "gift_log"
names(raw_dat)[names(raw_dat) == "ImageryPrime_recovered_Feb_02_2019_redacted"] <- "imagery_prime"
names(raw_dat)[names(raw_dat) == "ImpactAnxiousImagery_recovered_Feb_02_2019"]  <- "impact_anxious_imagery"
names(raw_dat)[names(raw_dat) == "MentalHealthHxTx_recovered_Feb_02_2019"]      <- "mental_health_hx_tx"
names(raw_dat)[names(raw_dat) == "MultiUserExperience_recovered_Feb_02_2019"]   <- "multi_user_experience"
names(raw_dat)[names(raw_dat) == "OA_recovered_Feb_02_2019"]                    <- "oa"
names(raw_dat)[names(raw_dat) == "ParticipantExportDAO_recovered_Feb_02_2019"]  <- "participant_export_dao"
names(raw_dat)[names(raw_dat) == "QOL_recovered_Feb_02_2019"]                   <- "qol"
names(raw_dat)[names(raw_dat) == "ReturnIntention_recovered_Feb_02_2019"]       <- "return_intention"
names(raw_dat)[names(raw_dat) == "RR_recovered_Feb_02_2019"]                    <- "rr"
names(raw_dat)[names(raw_dat) == "SUDS_recovered_Feb_02_2019"]                  <- "suds"
names(raw_dat)[names(raw_dat) == "TaskLog_final_FIXED"]                         <- "task_log"
names(raw_dat)[names(raw_dat) == "TrialDAO_recovered_Feb_02_2019"]              <- "trial_dao"
names(raw_dat)[names(raw_dat) == "VisitDAO_recovered_Feb_02_2019"]              <- "visit_dao"

# ---------------------------------------------------------------------------- #
# Shorten table names in Set B ----
# ---------------------------------------------------------------------------- #

names(raw_dat_b)[names(raw_dat_b) == "AnxietyTriggers_02_02_2019"]       <- "anxiety_triggers"
names(raw_dat_b)[names(raw_dat_b) == "BBSIQ_02_02_2019"]                 <- "bbsiq"
names(raw_dat_b)[names(raw_dat_b) == "CC_02_02_2019"]                    <- "cc"
names(raw_dat_b)[names(raw_dat_b) == "CIHS_02_02_2019"]                  <- "cihs"
names(raw_dat_b)[names(raw_dat_b) == "Credibility_02_02_2019"]           <- "credibility"
names(raw_dat_b)[names(raw_dat_b) == "DASS21_AS_02_02_2019"]             <- "dass21_as"
names(raw_dat_b)[names(raw_dat_b) == "DASS21_DS_02_02_2019"]             <- "dass21_ds"
names(raw_dat_b)[names(raw_dat_b) == "DD_02_02_2019"]                    <- "dd"
names(raw_dat_b)[names(raw_dat_b) == "Demographics_02_02_2019"]          <- "demographic"
names(raw_dat_b)[names(raw_dat_b) == "ImageryPrime_02_02_2019_redacted"] <- "imagery_prime"
names(raw_dat_b)[names(raw_dat_b) == "ImpactAnxiousImagery_02_02_2019"]  <- "impact_anxious_imagery"
names(raw_dat_b)[names(raw_dat_b) == "MentalHealthHxTx_02_02_2019"]      <- "mental_health_hx_tx"
names(raw_dat_b)[names(raw_dat_b) == "MultiUserExperience_02_02_2019"]   <- "multi_user_experience"
names(raw_dat_b)[names(raw_dat_b) == "OA_02_02_2019"]                    <- "oa"
names(raw_dat_b)[names(raw_dat_b) == "QOL_02_02_2019"]                   <- "qol"
names(raw_dat_b)[names(raw_dat_b) == "ReturnIntention_02_02_2019"]       <- "return_intention"
names(raw_dat_b)[names(raw_dat_b) == "RR_02_02_2019"]                    <- "rr"
names(raw_dat_b)[names(raw_dat_b) == "SUDS_02_02_2019"]                  <- "suds"
names(raw_dat_b)[names(raw_dat_b) == "TrialDAO_02_02_2019"]              <- "trial_dao"

# ---------------------------------------------------------------------------- #
# Restrict raw data tables to those relevant to present analysis for Sets A and B ----
# ---------------------------------------------------------------------------- #

# For Set A

sel_tbls <- c("bbsiq", "credibility", "dass21_as", "dass21_ds", "dd", "demographic", "oa",
              "participant_export_dao", "qol", "rr", "task_log")
sel_dat <- raw_dat[names(raw_dat) %in% sel_tbls]

# For Set B

sel_tbls_b <- c("bbsiq", "credibility", "dass21_as", "dass21_ds", "dd", "demographic", "oa",
                "qol", "rr")
sel_dat_b <- raw_dat_b[names(raw_dat_b) %in% sel_tbls_b]

# ---------------------------------------------------------------------------- #
# Identify potential columns that index participants in Set A ----
# ---------------------------------------------------------------------------- #

# Manually inspect column names to identify those that may index participants

lapply(raw_dat, identify_cols, grep_pattern = "")

potential_id_cols <- c("^id$", "participantDAO", "participantdao_id", "participantRSA",
                       "participantId", "participant", "sessionId")
pattern <- paste(potential_id_cols, collapse = "|")
lapply(sel_dat, identify_cols, grep_pattern = pattern)

# Table "oa" has "participantDAO" and "participantRSA". The values are identical, 
# with 6 exceptions where "participantRSA" has 344-character values. Given that 
# "participantDAO" is present in these cases, use this to index participants.

nrow(sel_dat$oa[sel_dat$oa$participantDAO != sel_dat$oa$participantRSA, ])
table(nchar(sel_dat$oa$participantRSA))

# In table "dass21_as", at eligibility screening eligible participants receive
# a "participantDAO" and ineligible participants do not. Neither eligible nor 
# ineligible participants receive "participantRSA" at screening. At other time 
# points, "participantDAO" is identical to "participantRSA" with 1 exception:
# "participantDAO" 29 at "session" "POST" has a 344-character "participantRSA".
# Use "participantDAO" to index participants.

dass21_as_eligibility <- sel_dat$dass21_as[sel_dat$dass21_as$session %in% c("ELIGIBLE", ""), ]

nrow(dass21_as_eligibility) ==
  nrow(dass21_as_eligibility[dass21_as_eligibility$session == "ELIGIBLE" &
                               !is.na(dass21_as_eligibility$participantDAO), ]) +
  nrow(dass21_as_eligibility[dass21_as_eligibility$session == "" &
                               is.na(dass21_as_eligibility$participantDAO), ])

nrow(dass21_as_eligibility) ==
  nrow(dass21_as_eligibility[dass21_as_eligibility$participantRSA == "", ])

dass21_as_other <- sel_dat$dass21_as[!(sel_dat$dass21_as$session %in% c("ELIGIBLE", "")), ]

nrow(dass21_as_other[dass21_as_other$participantDAO != dass21_as_other$participantRSA, ])
dass21_as_other[dass21_as_other$participantDAO != dass21_as_other$participantRSA, "participantDAO"]

# Table "dass_21_as" also has "sessionId", but no other tables (not even raw tables)
# have this. It is blank until 6/8/2016 (starting with "id" 834), after which point 
# it has 32-character values where "session" is blank or "ELIGIBLE". It was likely 
# introduced partway through the study to tie screening attempts to participants using 
# a cookie, before those who are eligible and enroll are given a "participantDAO".

lapply(raw_dat, identify_cols, grep_pattern = "sessionId")
table(nchar(sel_dat$dass21_as$sessionId), sel_dat$dass21_as$session, useNA = "always")

min(sel_dat$dass21_as$id[sel_dat$dass21_as$sessionId != ""]) == 834
sel_dat$dass21_as$date[sel_dat$dass21_as$id == 834] == "Wed, 08 Jun 2016 16:15:30 -0500"

# Tables other than "oa" and "dass21_as" have some 344-character "participantRSA"
# values too. However, none of these 344-character values appears more than once 
# within or across these tables. It's unclear what the 344-character value means.

oa_participantRSA_chars          <- sel_dat$oa[nchar(sel_dat$oa$participantRSA)                   == 344, "participantRSA"]
dass21_as_participantRSA_chars   <- sel_dat$dass21_as[nchar(sel_dat$dass21_as$participantRSA)     == 344, "participantRSA"]

bbsiq_participantRSA_chars       <- sel_dat$bbsiq[nchar(sel_dat$bbsiq$participantRSA)             == 344, "participantRSA"]
credibility_participantRSA_chars <- sel_dat$credibility[nchar(sel_dat$credibility$participantRSA) == 344, "participantRSA"]
dass21_ds_participantRSA_chars   <- sel_dat$dass21_ds[nchar(sel_dat$dass21_ds$participantRSA)     == 344, "participantRSA"]
dd_participantRSA_chars          <- sel_dat$dd[nchar(sel_dat$dd$participantRSA)                   == 344, "participantRSA"]
demographic_participantRSA_chars <- sel_dat$demographic[nchar(sel_dat$demographic$participantRSA) == 344, "participantRSA"]
qol_participantRSA_chars         <- sel_dat$qol[nchar(sel_dat$qol$participantRSA)                 == 344, "participantRSA"]
rr_participantRSA_chars          <- sel_dat$rr[nchar(sel_dat$rr$participantRSA)                   == 344, "participantRSA"]

sum(table(c(oa_participantRSA_chars,
            credibility_participantRSA_chars,
            dass21_as_participantRSA_chars,
            bbsiq_participantRSA_chars,
            dass21_ds_participantRSA_chars,
            dd_participantRSA_chars,
            demographic_participantRSA_chars,
            qol_participantRSA_chars,
            rr_participantRSA_chars)) > 1)

# ---------------------------------------------------------------------------- #
# Correct selected "participantRSA" values in Set A ----
# ---------------------------------------------------------------------------- #

# Sonia Baee's code "R34_cleaning_script.R" corrects "participantRSA" for certain
# row numbers in some tables. Namely, it corrects "participantRSA" to 532 in Row 
# 68 of "bbsiq", "dass_ds", and "rr" tables and in Row 50 of "dd" table. It also
# corrects "participantRSA" to 534, 535, and 536 for Rows 81, 88, and 99 of "qol"
# table. Obtain the original "participantRSA" for each of these row numbers.

bbsiq_row68_participantRSA     <- sel_dat$bbsiq[68, ]$participantRSA
dass21_ds_row68_participantRSA <- sel_dat$dass21_ds[68, ]$participantRSA
rr_row68_participantRSA        <- sel_dat$rr[68, ]$participantRSA
dd_row50_participantRSA        <- sel_dat$dd[50, ]$participantRSA

qol_row81_participantRSA       <- sel_dat$qol[81, ]$participantRSA
qol_row88_participantRSA       <- sel_dat$qol[88, ]$participantRSA
qol_row89_participantRSA       <- sel_dat$qol[89, ]$participantRSA

# Explicitly name the relevant "participantRSA" values and subsequently refer to
# these rather than to row numbers

bbsiq_row68_participantRSA == 
  paste0("bkUHY+xsxxIbBEF2CHPAiUooa8HWC/zDEYhXS1ktfHTmWPmkHl8Cdw+XCQ+7V+4V+lfnTq",
         "+UnxHtwpqZ9nvDOaCYujSb/xr5NXbMs+B8MEqkTFi8XmX46SCXwfEoteuxi5bnK9tIxcWZ",
         "C1mQkyfhphR3Hw+uZTfetH0qOZu1QlsHPnAMIjZDuipAEJCddUPZG+fFhWrsQKFz99YY1a",
         "d88g37Mdy8wMSFNxDHybkIFQbY6o9clvaAH+F5nfCBPgqx978yjkSeGP8cq58N3PViF/ED",
         "sT1WgZePkyylPdezs8PorJrLhtbHscCqL15QEIOd4t6DQf48sps9IR3GzFE1kg==")
dass21_ds_row68_participantRSA ==
  paste0("nmm8V5qydpeqiJK3opOxycYiXr2jYvMmvaF+sTc3FPXiz4HL1VrP/OBI7+eURNleliPsiv",
         "QpfdyHK9eX3ddpTJq8i25yjL+Isj17YyetkJ/C0DPYMloZDvK5LqCqoHYYA7bohUFHxc+f",
         "6wTgnuK7u+igZlI2N+b72M50P2sgxltVVgKZNxgYbahF3bJX8bvgANsSg40HD6DZTjvABd",
         "+6NeL/wRIwKMCCFnn870X8xeQnq4c3m9DcTP19dHPMj3uThFbe0VRPnBWqvO0LKDERwAzT",
         "Jn5Xk0mRFoU3fIKxBycy4oILofwK9TaTJaQbR1z+wkdkZGntkp3waNkgSJDXtQ==")
rr_row68_participantRSA ==
  paste0("j8xMRa/5b5fAWDc9k3XmKa5gc70yFtOEQeDmhbdhV0PVXbkastBIXfZNmJB5dC0NA+2q/p",
         "VkQ9nbyUCVXTb4K28ZT1X7msw7o/DDVJoDeUtnFY44UXaMKb8grRK3wqc/2A7sGI22IS0h",
         "ZxkhBPDz7PLvg7WRicUrf+joAJzoUcR96U8Da8TskUNeJKnx5ExsDCXSVDz7juecNg7mG9",
         "+f+8dSg7J8MK8I+P/AAVd6UlGhTi/Y63dmyV04tIA+tTWFjwsNNtQO6x65jhd+2EGAqeW3",
         "VNPcF9McqbKzteJEKW1i1hPpcA5wOfNpOwpCcddNgpwNUa5UQpS4vCWiBj/FtQ==")
dd_row50_participantRSA ==
  paste0("JxkDrxWqMNiIBetFR/Mjs+K0wIZBoYFXt8/urWIXZCj+09Z0Nc0VrHPzHq1gc1xYnA3gZg",
         "YEpDv+RHOx/foAFE04wj/XWtKt53Eu2rMNmpl5SnFHh0WZQadmrtyDqAeIsyh8Pi/9zpba",
         "sLulygiCAG10Vja0biESqixbOAaSLX7vXU4MBAAGOFGSPr5q2/bX3V7zbwqLYsG4Z7/RjS",
         "eR9jjzGFJeXG1SXyHVavwZWOKT2lyic7gnpp+WrMBbn66frS3ekE1cFrNv2VrWtRHTanlr",
         "78T6eCDLO+IykOo589YjtFJ1WKv6LbzdPct23tAiOFTyAFq56IrovojfH9ecgQ==")

qol_row81_participantRSA ==
  paste0("wbEBZcHlnsFt7G9Nery6IGcDyekYMRdfF9JY+86NsApN+xq72V9YVux2UoxOLHQ32GM+pf",
         "DdILnnXgUsCZ7/627m/HQcG5IDuJH5NzoRFO0Keg7+n8tIEG1oasegk4eUcSqts/Sottvc",
         "qZUd07bgy62kFtt/r0ANtUCIK/lFtJrwOD3GnsbUNSQPml2YvOD4QDDwN9jhLOzCaFN+n+",
         "2AEAkgMSV7wBqvQUv1Nq1zMM86h3+9+zfgNM5g1fBEtU05QnrmnkqO/EW4kGb72BfUEywJ",
         "HunLBwNqY71sTCkPlYnsWNtsdGm5hrrVlI+OXvbaIVa49ufK5YhGc3wcuDcuuA==")
qol_row88_participantRSA ==
  paste0("hfuLJcIoHPV/fo4cwqrI0O1bLuy/V8Me5gNXRpu3lKlg4m/BC0F0GfIV85jn7XIvCKDmcP",
         "RSzeE/nz7MDSo9oMMj+7JHcwPBqThTvWiF0MB4mkawa2BKWOOp77f0emHfhaYsU0hVcIYA",
         "IgQFx2yM3sb5xC8MCOB4bX31/q8mh7fpJFR3YGRKDsChafZdrLi5vA7jda9f1Xw8I5FZFY",
         "Vf/BjCadtETdliIrD/l33431Tjs4AGPs24aGd3XPGEh8DOCB3wlxjGTk8IK/msG2xbkTGf",
         "BGSzfCTjGiPJ/hE28PgGoJZhdhaD457wxGKPJc4hoc/zVXNYZllrhCuKKLh7Ew==")
qol_row89_participantRSA ==
  paste0("MPDLN5nIJels9xMMcfpKALfJCaZO0V0OTMwfPW+0e250c44/iTKXZeL2mL6rM3kTQfetnv",
         "FFp+tp+Dfa/XsArlxktjPq7N3oGMdZAPOAqvmUuSEvc2ooNH1y/GLxiJpr+oAwNIlqPpWE",
         "NVeWhYIJ6Mz4n4NvAqBtfebxso8CoSjxJNzt2tnncy884tlA9C7t0scOKHynrDN0mpaURI",
         "wj08vnuqodfz8ctojDvt9NByFLjY6qc/M9q+KnRTxgxwaFjWuimjwe5nsWGRJXiWrIaF5a",
         "6XEAxk2RkcqRP65Z8CQ+7FElWjVqws/770zdwQQce6Z0/fRM7qkTCYNOlxMurw==")

# Correct these "participantRSA" values

sel_dat$bbsiq[sel_dat$bbsiq$participantRSA         == bbsiq_row68_participantRSA, "participantRSA"]     <- 532
sel_dat$dass21_ds[sel_dat$dass21_ds$participantRSA == dass21_ds_row68_participantRSA, "participantRSA"] <- 532
sel_dat$rr[sel_dat$rr$participantRSA               == rr_row68_participantRSA, "participantRSA"]        <- 532
sel_dat$dd[sel_dat$dd$participantRSA               == dd_row50_participantRSA, "participantRSA"]        <- 532

sel_dat$qol[sel_dat$qol$participantRSA             == qol_row81_participantRSA, "participantRSA"]       <- 534
sel_dat$qol[sel_dat$qol$participantRSA             == qol_row88_participantRSA, "participantRSA"]       <- 535
sel_dat$qol[sel_dat$qol$participantRSA             == qol_row89_participantRSA, "participantRSA"]       <- 536

# Correct other "participantRSA" values that are explicitly named in Sonia Baee's 
# code "R34_cleaning_script.R" for "demographic" table

demographic_participantRSA_NvorEv <-
  paste0("NvorEV/x1Rn9FC1yuqkfDjcDlSRj9b8Y3ji78LPNHI8vzrvOJdQjRTpAY+Mn/C8pToWmJ",
         "mY4luS8P3Rfcfk/hcZGxUwzWpbkJjuKA0q8E4Y6IPxJmLdiO2zFYh8BJKEr4j8pidKyMo",
         "oi2REL5hcNeJKPpkId4R0iBpLgPXfapA+XnRqHQftVZo+JL1TD8eP362JX7A0L90Oxpqk",
         "uOp3lnEce2ox6vcc4fwTsvoWy+utocR+Fp7Pgct/wBIdjvKhSLJvabMK7Ffglk9OrAIXu",
         "xj77+n+sbsW4PoGc9a2UZu5D18WKGrUW6kdLTBCDqE/DyWjcLXEGWCwCwlTdRtPtjQ==")
demographic_participantRSA_j5J6cs <-
  paste0("j5J6cs0Yromqio+zSNruW1qA1athdsLV4AQf6AnWnhVS2M+SYJ3bEgBG9GPQDZMLN4Ttq",
         "yk76HO+x1QXEdL/FtgkyyYy82aR4j5lYJ0IeamTIK5atLtT0MZJQqhhehLp5y97TvJjoo",
         "tnTj4OV+l0iaxAMKxzcIqadlZ7FzD5uWDTqJCzxX/Bny7V01irJfqHgEEpxNwwLmJttjg",
         "fr3ifWWv7JseWIgPPIH1spGFUWN127vCvKmZ3bmbYCLq119q5Nn11jKe+Oq2oSvv7HPcz",
         "Db/knU3Uu7Y8Js01nK+VtX6kFYAACzwZcu6GN+KGuUUexlxMJXCaLBQvEdU3Iy7lVQ==")
demographic_participantRSA_VS9LPK <-
  paste0("VS9LPKcqvwakVj2Orq/tFLN+JB9nmbaZ5kqg0gDXF7CQ4NKpey6kMSnEErlvr3UrnJKYg",
         "KGEAk3wi0zijUZDNS5+Nzys2L7ynDGXQoE61RO07m2dqMsxZGHgv8qadNtfIyKxpjYaNo",
         "blp0d3VJ8sSce08XBmVxMfr7nLbDwC60Zm1vsBajDqX073gc3U35cq+n4vNG5v1t1zPQF",
         "qeRvXsTHGc4naet1I8PeQEcLtQJN2daxdfgZplToaOzEW7KGhdzgYYRyTwWCvwh8wHUcd",
         "mV4dW7VG3oGLvz35wLYVkOlpZMgZ+5eAr9an4JDaXwR1e3CeHYJHRDxSGibqK3rl/Q==")

sel_dat$demographic[sel_dat$demographic$participantRSA == demographic_participantRSA_NvorEv, "participantRSA"] <- 534
sel_dat$demographic[sel_dat$demographic$participantRSA == demographic_participantRSA_j5J6cs, "participantRSA"] <- 535
sel_dat$demographic[sel_dat$demographic$participantRSA == demographic_participantRSA_VS9LPK, "participantRSA"] <- 536

# Note that even after correcting these "participantRSA" values, some 344-character
# values remain in all of these tables except "demographic"

# ---------------------------------------------------------------------------- #
# Define "participant_id" across tables in Set A ----
# ---------------------------------------------------------------------------- #

# The following decisions are also consistent with "R34.ipynb"

# In "participant_export_dao" table, rename "id" as "participant_id", which is
# the only column that can index participants

names(sel_dat$participant_export_dao)[names(sel_dat$participant_export_dao) == "id"] <- "participant_id"

# In "oa" and "dass21_as" tables, rename "participantDAO" as "participant_id"
# and remove "participantRSA"

names(sel_dat$oa)[names(sel_dat$oa)               == "participantDAO"] <- "participant_id"
names(sel_dat$dass21_as)[names(sel_dat$dass21_as) == "participantDAO"] <- "participant_id"

sel_dat$oa$participantRSA        <- NULL
sel_dat$dass21_as$participantRSA <- NULL

# In "task_log" table, rename "participantdao_id" as "participant_id", which is
# the only column that can index participants

names(sel_dat$task_log)[names(sel_dat$task_log) == "participantdao_id"] <- "participant_id"

# Rename "participantRSA" as "participant_id" in tables where "participantRSA" is
# the only column that can index participants

participantRSA_index_tbls <- c("bbsiq", "credibility", "dass21_ds", "dd", "demographic",
                               "qol", "rr")

for (i in 1:length(sel_dat)) {
  if (names(sel_dat)[i] %in% participantRSA_index_tbls) {
    names(sel_dat[[i]])[names(sel_dat[[i]]) == "participantRSA"] <- "participant_id" 
  }
}

# ---------------------------------------------------------------------------- #
# Identify potential columns that index participants in Set B ----
# ---------------------------------------------------------------------------- #

# Manually inspect column names to identify those that may index participants

lapply(raw_dat_b, identify_cols, grep_pattern = "")

potential_id_cols_b <- c("^id$", "participantRSA", "participant")
pattern <- paste(potential_id_cols_b, collapse = "|")
lapply(sel_dat_b, identify_cols, grep_pattern = pattern)

# "bbsiq" has 1 NA for "participantRSA", but "id" is complete and otherwise identical 
# to "participantRSA". In "credibility", "dass21_ds", "dd", "demographic", "qol", and 
# "rr", "id" is identical to "participantRSA". Use "id" to index participants.

all(sel_dat_b$bbsiq$id == sel_dat_b$bbsiq$participantRSA, na.rm = TRUE)
sum(is.na(sel_dat_b$bbsiq$id))

all(sel_dat_b$credibility$id == sel_dat_b$credibility$participantRSA, na.rm = TRUE)
all(sel_dat_b$dass21_ds$id   == sel_dat_b$dass21_ds$participantRSA,   na.rm = TRUE)
all(sel_dat_b$dd$id          == sel_dat_b$dd$participantRSA,          na.rm = TRUE)
all(sel_dat_b$demographic$id == sel_dat_b$demographic$participantRSA, na.rm = TRUE)
all(sel_dat_b$qol$id         == sel_dat_b$qol$participantRSA,         na.rm = TRUE)
all(sel_dat_b$rr$id          == sel_dat_b$rr$participantRSA,          na.rm = TRUE)

# TODO: Unlike in Set A, Set B table "dass21_as" contains fewer rows, has no
# blank "session" values, and does not contain "sessionId"

table(sel_dat_b$dass21_as$session, useNA = "always")
table(sel_dat$dass21_as$session, useNA = "always")





# Unlike in Set A, no Set B tables have 344-character values for "participantRSA"
# (if present, longest "participantRSA" is 4 characters)

sel_dat_b_participantRSA_max_nchar <- sapply(sel_dat_b, function(x) {
  if ("participantRSA" %in% names(x)) {
    max(nchar(x$participantRSA), na.rm = TRUE)
  } else {
    NA
  }
})

all(sel_dat_b_participantRSA_max_nchar == 4, na.rm = TRUE)

# ---------------------------------------------------------------------------- #
# Define "participant_id" across tables in Set B ----
# ---------------------------------------------------------------------------- #

# In "dass21_as" and "oa" tables, rename "id" as "participant_id", which is
# the only column that can index participants

names(sel_dat_b$dass21_as)[names(sel_dat_b$dass21_as) == "id"] <- "participant_id"
names(sel_dat_b$oa)[names(sel_dat_b$oa)               == "id"] <- "participant_id"

# In "bbsiq", "credibility", "dass21_ds", "dd", "demographic", "qol", and "rr" 
# tables, rename "id" as "participant_id" and remove "participantRSA"

names(sel_dat_b$bbsiq)[names(sel_dat_b$bbsiq)             == "id"] <- "participant_id"
names(sel_dat_b$credibility)[names(sel_dat_b$credibility) == "id"] <- "participant_id"
names(sel_dat_b$dass21_ds)[names(sel_dat_b$dass21_ds)     == "id"] <- "participant_id"
names(sel_dat_b$dd)[names(sel_dat_b$dd)                   == "id"] <- "participant_id"
names(sel_dat_b$demographic)[names(sel_dat_b$demographic) == "id"] <- "participant_id"
names(sel_dat_b$qol)[names(sel_dat_b$qol)                 == "id"] <- "participant_id"
names(sel_dat_b$rr)[names(sel_dat_b$rr)                   == "id"] <- "participant_id"

sel_dat_b$bbsiq$participantRSA       <- NULL
sel_dat_b$credibility$participantRSA <- NULL
sel_dat_b$dass21_ds$participantRSA   <- NULL
sel_dat_b$dd$participantRSA          <- NULL
sel_dat_b$demographic$participantRSA <- NULL
sel_dat_b$qol$participantRSA         <- NULL
sel_dat_b$rr$participantRSA          <- NULL

# ---------------------------------------------------------------------------- #
# Compare numbers of participants in Sets A and B and those counted in "R34.ipynb" ----
# ---------------------------------------------------------------------------- #

# Extract numbers of unique participants by table listed for raw data in "R34.ipynb" 
# and in tables in Sets A and B

r34.ipynb_Ns <- c(bbsiq = 1417, dass21_as = 1865, dass21_ds = 1356, demographic = 1741,
                  oa = 1413, participant = 1971, rr = 1382, task_log = 1390)

set_a_Ns <- sapply(sel_dat,   function(x) length(unique(x$participant_id)))
set_b_Ns <- sapply(sel_dat_b, function(x) length(unique(x$participant_id)))

# Define function to compare

compare_Ns <- function(Ns1, Ns2) {
  shared_tbls <- intersect(names(Ns1), names(Ns2))
  
  Ns1_comp <- Ns1[names(Ns1) %in% shared_tbls]
  Ns2_comp <- Ns2[names(Ns2) %in% shared_tbls]
  
  Ns1_comp_vs_Ns2_comp <- Ns1_comp - Ns2_comp
  
  return(Ns1_comp_vs_Ns2_comp)
}

# Run function

  # "R34.ipynb" lists more participants for all corresponding tables in Set A except 
  # for "demographic" and "task_log" tables

names(set_a_Ns)[names(set_a_Ns) == "participant_export_dao"] <- "participant"

(set_a_vs_r34.ipynb_Ns <- compare_Ns(set_a_Ns, r34.ipynb_Ns))

  # "R34.ipynb" lists far more participants for all corresponding tables in Set B

(set_b_vs_r34.ipynb_Ns <- compare_Ns(set_b_Ns, r34.ipynb_Ns))

  # Set A contains far more participants than Set B

(set_a_vs_set_b_Ns <- compare_Ns(set_a_Ns, set_b_Ns))

# ---------------------------------------------------------------------------- #
# Identify and recode time stamp and date columns in Set A ----
# ---------------------------------------------------------------------------- #

# Identify columns containing "date" in each table

lapply(sel_dat, identify_cols, grep_pattern = "date")

# Note: "task_log" table has multiple columns containing "date". "date_completed" 
# is identical to "datetime" but different from "corrected_datetime".

all(sel_dat$task_log$date_completed == sel_dat$task_log$datetime)

all(sel_dat$task_log$date_completed == sel_dat$task_log$corrected_datetime)
nrow(sel_dat$task_log[sel_dat$task_log$date_completed != sel_dat$task_log$corrected_datetime, ])

# View structure of columns containing specified date column in each table

view_date_str <- function(df, df_name, grep_pattern = "date") {
  cat('\nTable: "', df_name, '"\n\n', sep = "")
  
  date_cols <- names(df)[grepl(grep_pattern, names(df))]
  
  if (length(date_cols) != 0) {
    for (col in date_cols) {
      cat('"', col, '"\n', sep = "")
      str(df[[col]], vec.len = 3)
      cat("- Number NA:   ", sum(is.na(df[[col]])), "\n")
      
      if (is.character(df[[col]])) {
        cat("- Number blank:", sum(df[[col]] == "",  na.rm = TRUE), "\n")
        cat("- Number 555:  ", sum(df[[col]] == 555, na.rm = TRUE), "\n")
        cat("- Number of characters:\n")
        print(table(nchar(df[[col]])))
      } else if (class(df[[col]])[1] == "POSIXct") {
        cat("- Time zone:", attr(df[[col]], "tz"), "\n")
      } else {
        stop("Column's class is not character or POSIXct")
      }
      
      cat("\n")
    }
  } else {
    cat('No column names containing "', grep_pattern, '" found\n\n', sep = "")
  }
  
  cat("----------\n")
}

invisible(mapply(view_date_str, sel_dat, names(sel_dat)))

# TODO: The following columns across tables are system-generated date and time 
# stamps. For now, assume all of these are in EST time zone (note: EST, or UTC - 
# 5, all year, not "America/New York", which switches between EST and EDT). But
# check this with Sonia/Dan, especially for "task_log" columns.

system_date_time_cols <- c("date", "date_completed", "datetime", "corrected_datetime")





# Define function to reformat system-generated time stamps and add time zone

recode_date_time_timezone <- function(dat) {
  for (i in 1:length(dat)) {
    table_name <- names(dat[i])
    colnames <- names(dat[[i]])
    target_colnames <- colnames[colnames %in% system_date_time_cols]
    
    if (length(target_colnames) != 0) {
      for (j in 1:length(target_colnames)) {
        # Create new variable for POSIXct values. Recode blanks as NA.
        
        POSIXct_colname <- paste0(target_colnames[j], "_as_POSIXct")
        
        dat[[i]][, POSIXct_colname] <- dat[[i]][, target_colnames[j]]
        dat[[i]][dat[[i]][, POSIXct_colname] == "", POSIXct_colname] <- NA
        
        # Specify time zone as "EST" for all system-generated time stamps. Specify 
        # nonstandard formats to parse columns, which are not in standard formats.
        
        if (table_name == "task_log" & target_colnames[j] %in% 
            c("date_completed", "datetime", "corrected_datetime")) {
          dat[[i]][, POSIXct_colname] <- 
            as.POSIXct(dat[[i]][, POSIXct_colname],
                       tz = "EST", 
                       format = "%m/%d/%Y %H:%M") # Note: 4-digit year
        } else {
          dat[[i]][, POSIXct_colname] <-
            as.POSIXct(dat[[i]][, POSIXct_colname], 
                       tz = "EST",
                       format = "%a, %d %b %Y %H:%M:%S %z")
        }
      }
    }
  }
  
  return(dat)
}

# Run function

sel_dat <- recode_date_time_timezone(sel_dat)

# ---------------------------------------------------------------------------- #
# Identify and recode time stamp and date columns in Set B ----
# ---------------------------------------------------------------------------- #

# Identify columns containing "date" in each table

lapply(sel_dat_b, identify_cols, grep_pattern = "date")

# View structure of columns containing "date" in each table

invisible(mapply(view_date_str, sel_dat_b, names(sel_dat_b)))

# TODO (dates are now handled but think of what this means for the two datasets)
# Note: 58 and 42 participants in "dass21_as" and "oa" tables, respectively, have 
# dates that are 11:14 characters (and no dates with more characters), whereas all 
# other participants (and all other tables) have dates that are 31 characters. These 
# participants with shorter dates in "dass21_as" and "oa" tables are not in the Set A
# "dass21_as", "oa", or "participant" tables but include all 36 of the participants
# listed as "not included in original dataset due to server error" in "notes.csv"
# (who are also included in the clean data).





sel_dat_b$dass21_as$date[nchar(sel_dat_b$dass21_as$date) %in% 11:14]
sel_dat_b$oa$date[nchar(sel_dat_b$oa$date)               %in% 11:14]

short_date_pids_dass21_as <- unique(sel_dat_b$dass21_as$participant_id[nchar(sel_dat_b$dass21_as$date) %in% 11:14])
short_date_pids_oa        <- unique(sel_dat_b$oa$participant_id[nchar(sel_dat_b$oa$date)               %in% 11:14])

length(short_date_pids_dass21_as) == 58
length(short_date_pids_oa)        == 42

all(sort(unique(nchar(sel_dat_b$dass21_as$date[sel_dat_b$dass21_as$participant_id %in% short_date_pids_dass21_as]))) == 11:14)
all(sort(unique(nchar(sel_dat_b$oa$date[sel_dat_b$oa$participant_id               %in% short_date_pids_oa])))        == 11:14)

  # Comparison with Set A tables

sum(short_date_pids_dass21_as %in% sel_dat$dass21_as$participant_id)              == 0
sum(short_date_pids_oa        %in% sel_dat$oa$participant_id)                     == 0

sum(short_date_pids_dass21_as %in% sel_dat$participant_export_dao$participant_id) == 0
sum(short_date_pids_oa        %in% sel_dat$participant_export_dao$participant_id) == 0

  # Comparison with participants subject at one point to server error

all(server_error_pids %in% short_date_pids_dass21_as)
all(server_error_pids %in% short_date_pids_oa)

  # Comparison with clean data

sum(short_date_pids_dass21_as %in% sep_dat$participant$participant_id) == 36
sum(short_date_pids_oa        %in% sep_dat$participant$participant_id) == 36

setequal(intersect(short_date_pids_dass21_as, sep_dat$participant$participant_id), server_error_pids)
setequal(intersect(short_date_pids_oa,        sep_dat$participant$participant_id), server_error_pids)





# Except for the participants above with shorter dates in their "dass21_as" and 
# "oa" tables, the following columns across tables are system-generated date and 
# time stamps. For now, assume all of these are in EST time zone (note: EST, or 
# UTC - 5, all year, not "America/New York", which switches between EST and EDT). 
# Assume that the shorter dates are also in EST.

system_date_time_cols_b <- "date"

# Define function to reformat system-generated time stamps and add time zone in Set B, 
# handling participants above with shorter dates in their "dass21_as" and "oa" tables

recode_date_time_timezone_b <- function(dat) {
  for (i in 1:length(dat)) {
    table_name <- names(dat[i])
    colnames <- names(dat[[i]])
    target_colnames <- colnames[colnames %in% system_date_time_cols_b]
    
    if (length(target_colnames) != 0) {
      for (target_colname in target_colnames) {
        # Create new variable for POSIXct values
        
        POSIXct_colname <- paste0(target_colname, "_as_POSIXct")
        
        dat[[i]][[POSIXct_colname]] <- as.POSIXct(NA, tz = "EST")

        # Specify time zone as "EST" for all system-generated time stamps. Specify 
        # nonstandard formats to parse columns, which are not in standard formats.
        
        if (table_name %in% c("dass21_as", "oa") & target_colname == "date") {
          short_date_idx <- nchar(dat[[i]][[target_colname]]) %in% 11:14
          
          dat[[i]][[POSIXct_colname]][short_date_idx] <-
            as.POSIXct(dat[[i]][[target_colname]][short_date_idx],
                       tz = "EST",
                       format = "%m/%d/%y %H:%M") # Note: 2-digit year
          
          long_date_idx <- nchar(dat[[i]][[target_colname]]) == 31
          
          dat[[i]][[POSIXct_colname]][long_date_idx] <- 
            as.POSIXct(dat[[i]][[target_colname]][long_date_idx], 
                       tz = "EST",
                       format = "%a, %d %b %Y %H:%M:%S %z")
        } else {
          dat[[i]][[POSIXct_colname]] <- 
            as.POSIXct(dat[[i]][[target_colname]], 
                       tz = "EST",
                       format = "%a, %d %b %Y %H:%M:%S %z")
        } 
      }
    }
  }
  
  return(dat)
}

# Run function

sel_dat_b <- recode_date_time_timezone_b(sel_dat_b)

# ---------------------------------------------------------------------------- #
# Identify and rename session-related columns for Set A ----
# ---------------------------------------------------------------------------- #

# Identify columns containing "session" in each table

lapply(sel_dat, identify_cols, grep_pattern = "session", exclude_cols = "sessionId")

# View structure of columns containing "session" in each table

view_session_str <- function(dat, exclude_cols = NULL) {
  for (i in 1:length(dat)) {
    cat('\nTable: "', names(dat[i]), '"\n\n', sep = "")
    
    colnames <- names(dat[[i]])
    session_colnames <- colnames[grep("session", colnames, ignore.case = TRUE)]
    
    if (!is.null(exclude_cols)) {
      session_colnames <- setdiff(session_colnames, exclude_cols)
    }
    
    if (length(session_colnames) != 0) {
      for (j in 1:length(session_colnames)) {
        session_colname <- session_colnames[j]
        session_colname_class <- class(dat[[i]][, session_colname])
        
        cat('"', session_colname, '"\n', sep = "")
        cat("- Class:", session_colname_class, "\n")

        if (length(unique(dat[[i]][, session_colname])) > 20) {
          cat("- First 20 unique levels:\n")
          print(unique(dat[[i]][, session_colname])[1:20])
        } else {
          cat("- All unique levels:\n")
          print(unique(dat[[i]][, session_colname]))
        }
        
        cat("- Number NA:", sum(is.na(dat[[i]][, session_colname])), "\n")
        
        if (!("POSIXct" %in% session_colname_class)) {
          cat("- Number blank:", sum(dat[[i]][, session_colname] == ""), "\n")
          cat("- Number 555:",   sum(dat[[i]][, session_colname] == 555, na.rm = TRUE), "\n")
        }
        
        cat("\n")
      }
    } else {
      print('No columns containing "session" found.\n\n')
    }
    
    cat("----------\n")
  }
}

view_session_str(sel_dat, exclude_cols = "sessionId")

# Rename selected session-related columns to clarify conflated content of some
# columns and to enable consistent naming (i.e., "session_only") across tables
# for columns that contain only session information

# Given that "session" column in "dass21_as" table contains both session 
# information and eligibility status, rename column to reflect this.
# Also create new column "session_only" with "ELIGIBLE" and "" entries of
# original "session" column recoded as "Eligibility" (to reflect that these
# entries were collected at the eligibility screener time point).

table(sel_dat$dass21_as$session)

# Rename remaining "session_name" columns (in "task_log" table) and remaining 
# "session" columns to "session_only" to reflect that they contain only session 
# information. Do not rename "currentSession" column of "participant_export_dao" 
# table because "currentSession" does not index entries within participants; 
# rather, it reflects participants' current sessions.

for (i in 1:length(sel_dat)) {
  if (names(sel_dat[i]) == "dass21_as") {
    names(sel_dat[[i]])[names(sel_dat[[i]]) == "session"] <- "session_and_eligibility_status"
    
    sel_dat[[i]][, "session_only"] <- sel_dat[[i]][, "session_and_eligibility_status"]
    sel_dat[[i]][sel_dat[[i]][, "session_only"] %in% c("ELIGIBLE", ""), "session_only"] <- "Eligibility"
  } else if (names(sel_dat[i]) == "task_log") {
    names(sel_dat[[i]])[names(sel_dat[[i]]) == "session_name"] <- "session_only"
  } else if ("session" %in% names(sel_dat[[i]])) {
    names(sel_dat[[i]])[names(sel_dat[[i]]) == "session"] <- "session_only"
  }
}

# ---------------------------------------------------------------------------- #
# Identify and rename session-related columns for Set B ----
# ---------------------------------------------------------------------------- #

# Identify columns containing "session" in each table

lapply(sel_dat_b, identify_cols, grep_pattern = "session")

# View structure of columns containing "session" in each table

view_session_str(sel_dat_b)

# Given that "session" column in "dass21_as" table may contain both session information 
# and eligibility status (unlike original "session" column in Set A, which contains both 
# "ELIGIBLE" and "" entries, original "session" column in Set B contains no "" entries), 
# create new column "session_only" with "ELIGIBLE" entries of original "session" column 
# recoded as "Eligibility" (to reflect that these were collected at eligibility screener).

table(sel_dat_b$dass21_as$session)

# Rename remaining "session" columns to "session_only" to reflect that they contain only 
# session information

for (i in 1:length(sel_dat_b)) {
  if (names(sel_dat_b[i]) == "dass21_as") {
    sel_dat_b[[i]][, "session_only"] <- sel_dat_b[[i]][, "session"]
    sel_dat_b[[i]][sel_dat_b[[i]][, "session_only"] == "ELIGIBLE", "session_only"] <- "Eligibility"
  } else if ("session" %in% names(sel_dat_b[[i]])) {
    names(sel_dat_b[[i]])[names(sel_dat_b[[i]]) == "session"] <- "session_only"
  }
}

# ---------------------------------------------------------------------------- #
# Identify participant IDs in clean data ----
# ---------------------------------------------------------------------------- #

cln_participant_ids <- sep_dat$participant$participant_id

length(cln_participant_ids) == 807

# Confirm that none are test accounts

  # Test accounts manually identified per scripts "R34_cleaning_script.R" and 
  # "sonia-consort diagram.R" on "MT-Data-ManagingAnxietyStudy" GitHub

test_manual <- c(1, 2, 4, 5, 441, 450, 538, 540, 578, 610, 624, 718, 767, 753,
                 775, 847, 848, 926, 971, 1014, 1020:1026, 1031:1033, 1038, 1058, 
                 1187, 1213, 1215, 1220:1226, 1232, 1263, 1270, 1288, 1309, 1338, 
                 1407, 1486:1488, 1490, 1499, 1500, 1608, 1631, 1740, 1767, 
                 1817:1819, 1831, 1899, 1900, 1968, 1971)

  # Test accounts per "notes.csv", all of which are already in "test_consort"

test_notes <- notes$participant_id[notes$test %in% 1]

all(test_notes %in% test_manual)

  # Check for test accounts

sum(cln_participant_ids %in% c(test_manual, test_notes)) == 0

# ---------------------------------------------------------------------------- #
# Filter raw data in Sets A and B ----
# ---------------------------------------------------------------------------- #

# Define function to filter raw data based on participant_ids in clean data

filter_all_data <- function(dat, participant_ids) {
  output <- vector("list", length(dat))
  
  for (i in 1:length(dat)) {
    if ("participant_id" %in% names(dat[[i]])) {
      output[[i]] <- dat[[i]][dat[[i]][, "participant_id"] %in% participant_ids, ]
    } else {
      output[[i]] <- dat[[i]]
    }
  }
  
  names(output) <- names(dat)
  return(output)
}

# Run function for Sets A and B

flt_dat     <- filter_all_data(sel_dat,     cln_participant_ids)
flt_dat_b   <- filter_all_data(sel_dat_b,   cln_participant_ids)

# ---------------------------------------------------------------------------- #
# Check session and date values in Sets A and B ----
# ---------------------------------------------------------------------------- #

# In Set A, there are discrepancies between "session" and date-related values in 
# "task_log" table and "session" and "date" in OASIS table (e.g., P 623). This is
# because participants with multiple entries (whose values also skip a session) have 
# not yet been handled in the Set A OASIS table (they are handled in a section below).

# View(flt_dat$oa[flt_dat$oa$participant_id == 623, ])
# View(flt_dat$task_log[flt_dat$task_log$participant_id == 623 & flt_dat$task_log$task_name == "OA", ])
# View(flt_dat_b$oa[flt_dat_b$oa$participant_id == 623, ])
# View(sep_dat$oa[sep_dat$oa$participant_id     == 623, ])

# TODO: Continue documenting other differences in session and date values, including
# those identified in sections below





# ---------------------------------------------------------------------------- #
# Check for multiple entries in Set A ----
# ---------------------------------------------------------------------------- #

# For rows that have duplicated values on every meaningful column (i.e., every
# column except "X" and "id"), keep only the last row after sorting by "id" for
# tables that contain "id" (throw error if "participant_export_dao", which lacks
# "id", contains multiple rows per "participant_id", in which case it will need 
# to be sorted and have its rows consolidated).

flt_dat_nrow_before <- sapply(flt_dat, nrow)

for (i in 1:length(flt_dat)) {
  meaningful_cols <- names(flt_dat[[i]])[!(names(flt_dat[[i]]) %in% c("X", "id"))]
  
  if (names(flt_dat[i]) == "participant_export_dao") {
    if (nrow(flt_dat[[i]]) != length(unique(flt_dat[[i]][, "participant_id"]))) {
      error(paste0("Unexpectedly, table ", names(flt_dat[i]), 
                   "contains multiple rows for at least one participant_id"))
    }
  } else if ("id" %in% names(flt_dat[[i]])) {
    flt_dat[[i]] <- flt_dat[[i]][order(flt_dat[[i]][, "id"]), ]
    
    flt_dat[[i]] <- flt_dat[[i]][!duplicated(flt_dat[[i]][, meaningful_cols], fromLast = TRUE), ]
  } else {
    stop(paste0("Table ", names(flt_dat[i]), "needs to be checked for duplicates"))
  }
}

flt_dat_nrow_after <- sapply(flt_dat, nrow)

flt_dat_nrow_rm <- flt_dat_nrow_before - flt_dat_nrow_after
all(flt_dat_nrow_rm[names(flt_dat_nrow_rm) != "task_log"] == 0)  # No exact dups except
flt_dat_nrow_rm[names(flt_dat_nrow_rm)     == "task_log"] == 570 #   570 in "task_log"

# Define functions to report duplicated rows on target columns. "report_dups_df"
# is used within "report_dups_list".

report_dups_df <- function(df, df_name, target_cols, index_col) {
  duplicated_rows <- df[duplicated(df[, target_cols]), ]
  if (nrow(duplicated_rows) > 0) {
    cat(nrow(duplicated_rows), "duplicated rows for table:", df_name)
    cat("\n")
    cat("With these '", index_col, "': ", duplicated_rows[, index_col])
    cat("\n-------------------------\n")
  } else {
    cat("No duplicated rows for table:", df_name)
    cat("\n-------------------------\n")
  }
}

report_dups_list <- function(dat) {
  for (i in 1:length(dat)) {
    if (names(dat[i]) %in% "participant_export_dao") {
      report_dups_df(dat[[i]], 
                     names(dat[i]), 
                     "participant_id", 
                     "participant_id")
    } else if (names(dat[i]) == "dass21_as") {
      duplicated_rows_eligibility <- 
        dat[[i]][dat[[i]][, "session_only"] == "Eligibility" &
                   (duplicated(dat[[i]][, c("participant_id",
                                            "session_only",
                                            "sessionId")])), ]
      duplicated_rows_other <-
        dat[[i]][dat[[i]][, "session_only"] != "Eligibility" &
                   (duplicated(dat[[i]][, c("participant_id",
                                            "session_only")])), ]
      duplicated_rows <- rbind(duplicated_rows_eligibility, duplicated_rows_other)
      
      if (nrow(duplicated_rows) > 0) {
        p_ids <- duplicated_rows_eligibility[!is.na(duplicated_rows_eligibility$participant_id),
                                             "participant_id"]
        s_ids <- duplicated_rows_eligibility[is.na(duplicated_rows_eligibility$participant_id),
                                             "sessionId"]
        
        cat(nrow(duplicated_rows_eligibility), 
            "duplicated rows at Eligibility for table:", names(dat[i]))
        cat("\n")
        cat("With these ", length(p_ids), "'participant_id' (where available): ", p_ids)
        cat("\n")
        cat("And with ", length(s_ids), "'sessionId' (where 'participant_id' unavailable)")
        cat("\n")
        cat(nrow(duplicated_rows_other), 
            "duplicated rows at other time points for table:", names(dat[i]))
        if (nrow(duplicated_rows_other) > 0) {
          cat("\n")
          cat("With these 'participant_id': ", duplicated_rows_other$participant_id)
        }
        cat("\n-------------------------\n")
      } else {
        cat("No duplicated rows for table:", names(dat[i]))
        cat("\n-------------------------\n")
      }
    } else if (names(dat[i]) == "task_log") {
      report_dups_df(dat[[i]], 
                     names(dat[i]), 
                     c("participant_id", 
                       "session_only", 
                       "task_name"), 
                     "participant_id")
      
      duplicated_rows_dass21_as_eligibility <- 
        dat[[i]][duplicated(dat[[i]][, c("participant_id", 
                                         "session_only", 
                                         "task_name")]) &
                   dat[[i]][, "session_only"] == "Eligibility" &
                   dat[[i]][, "task_name"] == "DASS21_AS", ]
      duplicated_rows_other_suds <- 
        dat[[i]][duplicated(dat[[i]][, c("participant_id", 
                                         "session_only", 
                                         "task_name")]) &
                   !(dat[[i]][, "session_only"] == "Eligibility" &
                       dat[[i]][, "task_name"] == "DASS21_AS") &
                   (dat[[i]][, "task_name"] == "SUDS"), ]
      duplicated_rows_other_nonsuds <- 
        dat[[i]][duplicated(dat[[i]][, c("participant_id", 
                                         "session_only", 
                                         "task_name")]) &
                   !(dat[[i]][, "session_only"] == "Eligibility" &
                       dat[[i]][, "task_name"] == "DASS21_AS") &
                   !(dat[[i]][, "task_name"] == "SUDS"), ]
      if (nrow(duplicated_rows_dass21_as_eligibility) > 0 |
          nrow(duplicated_rows_other_suds) > 0 |
          nrow(duplicated_rows_other_nonsuds) > 0) {
        cat(nrow(duplicated_rows_dass21_as_eligibility),
            "duplicated rows for DASS21_AS at Eligibility in table:", names(dat[i]))
        cat("\n")
        cat("With these 'participant_id': ", duplicated_rows_dass21_as_eligibility$participant_id)
        cat("\n", "\n")
        cat(nrow(duplicated_rows_other_suds),
            "duplicated rows for other, SUDS tasks in table:", names(dat[i]))
        cat("\n")
        cat("With these 'participant_id': ", duplicated_rows_other_suds$participant_id)
        cat("\n", "\n")
        cat(nrow(duplicated_rows_other_nonsuds),
            "duplicated rows for other, non-SUDS tasks in table:", names(dat[i]))
        cat("\n")
        cat("With these 'participant_id': ", duplicated_rows_other_nonsuds$participant_id)
        cat("\n-------------------------\n")
      }
    } else {
      report_dups_df(dat[[i]],
                     names(dat[i]), 
                     c("participant_id", 
                       "session_only"), 
                     "participant_id")
    }
  }
}

# Run function

report_dups_list(flt_dat)

# ---------------------------------------------------------------------------- #
# Resolve multiple entries for "oa" table in Set A ----
# ---------------------------------------------------------------------------- #

# Investigation of multiple entries revealed that those in "oa" table were not
# resolved by keeping the most recent entry, but by sorting each participant's 
# entries chronologically and then recoding the session column so that it reflects 
# the expected session order for the number of entries present for that participant

  # TODO (use more robust approach to get these): Collect participants with multiple 
  # "oa" entries from "report_dups_list()" above

multiple_oa_entry_participant_ids <- 
  c(8, 14, 16, 17, 421, 425, 432, 435, 445, 485, 532, 539, 541, 552, 582, 590, 
    597, 598, 600, 620, 623, 625, 627, 640, 644, 659, 662, 669, 674, 683, 684, 
    687, 701, 708, 710, 712, 719, 723, 727, 731, 745)





# Define function to recode session in "oa" table for certain participants so that it 
# reflects the expected session order for number of entries present for each participant

recode_oa_session_to_expected_order <- function(oa_tbl, participant_ids) {
  # Sort by participant and then date
  
  oa_tbl <- oa_tbl[order(oa_tbl$participant_id, oa_tbl$date_as_POSIXct), ]
  
  # For specified participants, recode session so it reflects the expected session 
  # order for number of entries present for each participant
  
  oa_split <- split(oa_tbl, oa_tbl$participant_id)
  
  oa_split <- lapply(oa_split, function(x) {
    participant_id <- unique(x$participant_id)
    
    if (participant_id %in% participant_ids) {
      oa_sessions <- c("PRE", paste0("SESSION", 1:8), "POST")
      
      number_oa_sessions_done <- nrow(x)
      
      oa_sessions_done <- oa_sessions[1:number_oa_sessions_done]
      
      x$session_only <- oa_sessions_done
    }
    
    return(x)
  })
  oa_tbl <- do.call(rbind, oa_split)
  row.names(oa_tbl) <- 1:nrow(oa_tbl)

  return(oa_tbl)
}

# Run function for participants with multiple entries

flt_dat$oa <- recode_oa_session_to_expected_order(flt_dat$oa, multiple_oa_entry_participant_ids)

# Recheck for multiple unexpected entries in "oa" table

report_dups_list(flt_dat) # None

# ---------------------------------------------------------------------------- #
# Resolve multiple entries for other tables in Set A ----
# ---------------------------------------------------------------------------- #

# TODO: Describe what was done in R34 main outcomes paper. Script "R34.ipynb"
# seems to inadequately sort by "date" (does not first convert to "datetime") 
# when  keeping most recent entry, but see if it's an issue once the scale scores 
# are regenerated





# dass21_as:
# - "R34_cleaning_script.R" says all duplicates are multiple screening attempts, 
#   assumes entries are in order by date, and keeps the last row
# - Other script says "get the latest entry for each participant" (does so by 
#   most recent date for each session)
# 
# oa:
# - Despite what the following scripts say, the method above was ultimately used
#   - "R34_cleaning_script.R" says all duplicates are for scam/test accounts, but none 
#     of these are test accounts based on Sonia's manually identified list of test accounts
#   - Other script says "get the latest entry for each participant" (does so by most 
#     recent date for each session) and notes that 1767 has duplicated values at PRE 
#     (not above) and then takes the sum to generate the score
#     - ## Get the latest entry for each participant
#     - oasis_analysis = oasis_analysis.sort_values(by="date").groupby(['participantID','session']).tail(1)
# 
# task_log:
# - Multiple SUDS at Sessions 1, 3, 6, 8 with no "tag"
# - Other duplicates (likely due to repeating training)





# Sort tables chronologically. Given that "dd" table lacks unique "id" for each row, 
# sort tables on "date_as_POSIXct". Given that "task_log" table lacks "date_as_POSIXct", 
# sort it on "corrected_datetime_as_POSIXct". Given that "participant_export_dao" 
# table lacks date-related columns, sort it by "participant_id".

for (i in 1:length(flt_dat)) {
  if (names(flt_dat[i]) == "participant_export_dao") {
    flt_dat[[i]] <- flt_dat[[i]][order(flt_dat[[i]][, "participant_id"]), ]
  } else if (names(flt_dat[i]) == "task_log") {
    flt_dat[[i]] <- flt_dat[[i]][order(flt_dat[[i]][, "corrected_datetime_as_POSIXct"]), ]
  } else {
    flt_dat[[i]] <- flt_dat[[i]][order(flt_dat[[i]][, "date_as_POSIXct"]), ]
  }
}

# Define functions to keep the most recent entry where duplicated rows exist on 
# target columns. "keep_recent_entry_df" is used within "keep_recent_entry_list".

keep_recent_entry_df <- function(df, target_cols) {
  output <- df[!duplicated(df[, target_cols], fromLast = TRUE), ]
  
  return(output)
}

keep_recent_entry_list <- function(dat) {
  output <- vector("list", length(dat))
  
  for (i in 1:length(dat)) {
    if (names(dat[i]) %in% "participant_export_dao") {
      output[[i]] <- keep_recent_entry_df(dat[[i]],
                                          "participant_id")
    } else if (names(dat[i]) == "dass21_as") {
      recent_entry_eligibility <- 
        keep_recent_entry_df(dat[[i]][dat[[i]][, "session_only"] == "Eligibility", ],
                             c("participant_id",
                               "session_only",
                               "sessionId"))
      recent_entry_other <-
        keep_recent_entry_df(dat[[i]][dat[[i]][, "session_only"] != "Eligibility", ],
                             c("participant_id",
                               "session_only"))
      output[[i]] <- rbind(recent_entry_eligibility, recent_entry_other)
    } else if (names(dat[i]) == "task_log") {
      # Do not remove multiple entries from "task_log" table
      
      output[[i]] <- dat[[i]]
    } else {
      output[[i]] <- keep_recent_entry_df(dat[[i]],
                                          c("participant_id", 
                                            "session_only"))
    }
    rownames(output[[i]]) <- 1:nrow(output[[i]])
  }
  names(output) <- names(dat)
  
  return(output)
}

# Run function

flt_dat <- keep_recent_entry_list(flt_dat)

# ---------------------------------------------------------------------------- #
# Check for multiple entries in Set B ----
# ---------------------------------------------------------------------------- #

# For rows that have duplicated values on every meaningful column (i.e., every
# column except "X" [only in "dass21_ds"]; no Set B tables contain "id" at this 
# point), keep only the last row after sorting by "date_as_POSIXct"

flt_dat_b_nrow_before <- sapply(flt_dat_b, nrow)

for (i in 1:length(flt_dat_b)) {
  meaningful_cols <- names(flt_dat_b[[i]])[names(flt_dat_b[[i]]) != "X"]
  
  if ("date_as_POSIXct" %in% names(flt_dat_b[[i]])) {
    flt_dat_b[[i]] <- flt_dat_b[[i]][order(flt_dat_b[[i]][, "date_as_POSIXct"]), ]
    
    flt_dat_b[[i]] <- flt_dat_b[[i]][!duplicated(flt_dat_b[[i]][, meaningful_cols], fromLast = TRUE), ]
  } else {
    stop(paste0("Table ", names(flt_dat_b[i]), "needs to be checked for duplicates"))
  }
}

flt_dat_b_nrow_after <- sapply(flt_dat_b, nrow)

flt_dat_b_nrow_rm <- flt_dat_b_nrow_before - flt_dat_b_nrow_after
all(flt_dat_b_nrow_rm == 0) # No exact duplicates

# Define functions to report duplicated rows on target columns. "report_dups_df" 
# defined for Set A above is used within "report_dups_list_b" for Set B.

report_dups_list_b <- function(dat) {
  for (i in 1:length(dat)) {
    if (names(dat[i]) == "dass21_as") {
      duplicated_rows_eligibility <- 
        dat[[i]][dat[[i]][, "session_only"] == "Eligibility" &
                   (duplicated(dat[[i]][, c("participant_id", "session_only")])), ]
      duplicated_rows_other <-
        dat[[i]][dat[[i]][, "session_only"] != "Eligibility" &
                   (duplicated(dat[[i]][, c("participant_id", "session_only")])), ]
      duplicated_rows <- rbind(duplicated_rows_eligibility, duplicated_rows_other)
      
      if (nrow(duplicated_rows) > 0) {
        p_ids <- duplicated_rows_eligibility[!is.na(duplicated_rows_eligibility$participant_id),
                                             "participant_id"]
        
        cat(nrow(duplicated_rows_eligibility), 
            "duplicated rows at Eligibility for table:", names(dat[i]), "\n")
        cat("With these ", length(p_ids), "'participant_id': ", p_ids, "\n")
        cat(nrow(duplicated_rows_other), 
            "duplicated rows at other time points for table:", names(dat[i]))
        if (nrow(duplicated_rows_other) > 0) {
          cat("\nWith these 'participant_id': ", duplicated_rows_other$participant_id)
        }
        cat("\n-------------------------\n")
      } else {
        cat("No duplicated rows for table:", names(dat[i]))
        cat("\n-------------------------\n")
      }
    } else {
      report_dups_df(dat[[i]],
                     names(dat[i]), 
                     c("participant_id", "session_only"), 
                     "participant_id")
    }
  }
}

# Run function

report_dups_list_b(flt_dat_b) # No duplicates

# ---------------------------------------------------------------------------- #
# Rename and recode columns in clean item-level baseline data ----
# ---------------------------------------------------------------------------- #

# Rename "participantID" as "participant_id" and "session" as "session_only"

names(cln_dat_bl)[names(cln_dat_bl) == "participantID"] <- "participant_id"
names(cln_dat_bl)[names(cln_dat_bl) == "session"]       <- "session_only"

# Confirm all rows are at baseline

all(cln_dat_bl$session_only == "PRE")

# Remove "id" and "date", which are ambiguous

cln_dat_bl[c("id", "date")] <- NULL

# Remove prefixes from OA and DASS-21-AS columns

names(cln_dat_bl) <- sub("^OA_",    "", names(cln_dat_bl))
names(cln_dat_bl) <- sub("^DASSA_", "", names(cln_dat_bl))

# ---------------------------------------------------------------------------- #
# Define scale items in Sets A and B and in clean baseline item-level data ----
# ---------------------------------------------------------------------------- #

# OASIS

oa_items <- c("anxious_freq", "anxious_sev", "avoid", "interfere", "interfere_social")

length(oa_items) == 5

all(oa_items %in% names(flt_dat$oa))
all(oa_items %in% names(flt_dat_b$oa))
all(oa_items %in% names(cln_dat_bl))

# Recognition Ratings

rr_nf_items <- names(flt_dat$rr)[grep("_NF$", names(flt_dat$rr))] # Negative nonthreat
rr_ns_items <- names(flt_dat$rr)[grep("_NS$", names(flt_dat$rr))] # Negative threat
rr_pf_items <- names(flt_dat$rr)[grep("_PF$", names(flt_dat$rr))] # Positive nonthreat
rr_ps_items <- names(flt_dat$rr)[grep("_PS$", names(flt_dat$rr))] # Positive threat

rr_items <- c(rr_nf_items, rr_ns_items, rr_pf_items, rr_ps_items)

all(sapply(list(rr_nf_items, rr_ns_items, rr_pf_items, rr_ps_items), length) == 9)
length(rr_items) == 36

all(rr_items %in% names(flt_dat$rr))
all(rr_items %in% names(flt_dat_b$rr))
all(rr_items %in% names(cln_dat_bl))

# BBSIQ

bbsiq_neg_int_items <- c("breath_suffocate", "vision_illness", "lightheaded_faint", "chest_heart",
                         "heart_wrong", "confused_outofmind", "dizzy_ill")
bbsiq_neg_ext_items <- c("visitors_bored", "shop_irritating", "smoke_house", "friend_incompetent",
                         "jolt_burglar", "party_boring", "urgent_died")
bbsiq_neg_items <- c(bbsiq_neg_int_items, bbsiq_neg_ext_items)
 
bbsiq_ben_int_items <- c("breath_flu", "breath_physically", "vision_glasses", "vision_strained",
                         "lightheaded_eat", "lightheaded_sleep", "chest_indigestion", "chest_sore",
                         "heart_active", "heart_excited", "confused_cold", "confused_work",
                         "dizzy_ate", "dizzy_overtired")
bbsiq_ben_ext_items <- c("visitors_engagement", "visitors_outstay", "shop_bored", "shop_concentrating",
                         "smoke_cig", "smoke_food", "friend_helpful", "friend_moreoften", "jolt_dream",
                         "jolt_wind", "party_hear", "party_preoccupied", "urgent_bill", "urgent_junk")
bbsiq_ben_items <- c(bbsiq_ben_int_items, bbsiq_ben_ext_items)

bbsiq_items <- c(bbsiq_neg_items, bbsiq_ben_items)

all(sapply(list(bbsiq_neg_int_items, bbsiq_neg_ext_items), length) == 7)
length(bbsiq_neg_items) == 14

all(sapply(list(bbsiq_ben_int_items, bbsiq_ben_ext_items), length) == 14)
length(bbsiq_ben_items) == 28

all(bbsiq_items %in% names(flt_dat$bbsiq))
all(bbsiq_items %in% names(flt_dat_b$bbsiq))
all(bbsiq_items %in% names(cln_dat_bl))

# DASS-21-AS

dass21_as_items <- c("breathing", "dryness", "heart", "panic", "scared", "trembling", "worry")

length(dass21_as_items) == 7

all(dass21_as_items %in% names(flt_dat$dass21_as))
all(dass21_as_items %in% names(flt_dat_b$dass21_as))
all(dass21_as_items %in% names(cln_dat_bl))

# DASS-21-DS

dass21_ds_items <- c("blue", "difficult", "hopeless", "meaningless", "noenthusiastic", 
                     "nopositive", "noworth")

length(dass21_ds_items) == 7

  # Item-level data is only in Sets A and B

all(dass21_ds_items %in% names(flt_dat$dass21_ds))
all(dass21_ds_items %in% names(flt_dat_b$dass21_ds))

# ---------------------------------------------------------------------------- #
# Extract clean item-level baseline data into separate tables ----
# ---------------------------------------------------------------------------- #

setdiff(names(cln_dat_bl), c(bbsiq_items, dass21_as_items, oa_items, rr_items))

# Extract data into separate tables using function created for clean longitudinal
# scales data above

table_cols_bl <- list(bbsiq       = bbsiq_items,
                      dass_as     = dass21_as_items,
                      oa          = oa_items,
                      participant = "participant_id",
                      rr          = rr_items)

sep_dat_bl <- create_sep_dat(cln_dat_bl, table_cols_bl)
sep_dat_bl$participant <- as.data.frame(sep_dat_bl$participant)

# Alphabetize columns of each table, retaining "participant_id" as first column

for (i in 1:length(sep_dat_bl)) {
  if (names(sep_dat_bl[i]) == "participant") {
    sep_dat_bl[[i]] <- sep_dat_bl[[i]]
  } else {
    sep_dat_bl[[i]] <- sep_dat_bl[[i]][, c("participant_id", 
                                           setdiff(sort(names(sep_dat_bl[[i]])), "participant_id"))]
  }
}

# Add "session_only" reflecting baseline value for each table

for (i in 1:length(sep_dat_bl)) {
  if (names(sep_dat_bl[i]) == "participant") {
    sep_dat_bl[[i]] <- sep_dat_bl[[i]]
  } else if (names(sep_dat_bl[i]) == "dass_as") {
    sep_dat_bl[[i]][, "session_only"] <- "Eligibility"
  } else {
    sep_dat_bl[[i]][, "session_only"] <- "PRE"
  }
}

# Remove rows that contain only NAs for all items

for (i in 1:length(sep_dat_bl)) {
  if (names(sep_dat_bl[i]) == "participant") {
    sep_dat_bl[[i]] <- sep_dat_bl[[i]]
  } else {
    item_cols <- setdiff(names(sep_dat_bl[[i]]), c("participant_id", "session_only"))
    
    sep_dat_bl[[i]] <- sep_dat_bl[[i]][rowSums(is.na(sep_dat_bl[[i]][item_cols])) != length(item_cols), ]
  }
}

# ---------------------------------------------------------------------------- #
# Recode "prefer not to answer" values in Sets A and B and clean item-level baseline data ----
# ---------------------------------------------------------------------------- #

# "R34.ipynb" says that it recodes one user's DASS-21-AS values of -2 to -1. This
# only occurs in Set A (for two items for participant 1644).

sum(flt_dat$dass21_as[, dass21_as_items] == -2)

flt_dat$dass21_as[, dass21_as_items][flt_dat$dass21_as[, dass21_as_items] == -2]  <- -1

# In Set A, recode 555 and -1 ("prefer not to answer") as NA

sum(flt_dat$oa[, oa_items]               == 555)
sum(flt_dat$rr[, rr_items]               == -1)
sum(flt_dat$bbsiq[, bbsiq_items]         == 555)
sum(flt_dat$dass21_as[, dass21_as_items] == -1)
sum(flt_dat$dass21_ds[, dass21_ds_items] == -1)

flt_dat$oa[, oa_items][flt_dat$oa[, oa_items]                             == 555] <- NA
flt_dat$rr[, rr_items][flt_dat$rr[, rr_items]                             == -1]  <- NA
flt_dat$bbsiq[, bbsiq_items][flt_dat$bbsiq[, bbsiq_items]                 == 555] <- NA
flt_dat$dass21_as[, dass21_as_items][flt_dat$dass21_as[, dass21_as_items] == -1]  <- NA
flt_dat$dass21_ds[, dass21_ds_items][flt_dat$dass21_ds[, dass21_ds_items] == -1]  <- NA

# In Set B, no values in "oa", "rr", "bbsiq", "dass21_as", or "dass21_ds" tables 
# are 555 or -1 (already recoded as NA)

# In clean item-level baseline data, recode 555 and -1 as NA

sum(sep_dat_bl$oa[, oa_items]             == 555)
sum(sep_dat_bl$rr[, rr_items]             %in% c(-1, 555), na.rm = TRUE) == 0 # None
sum(sep_dat_bl$bbsiq[, bbsiq_items]       == 555)
sum(sep_dat_bl$dass_as[, dass21_as_items] %in% c(-1, 555), na.rm = TRUE) == 0 # None

sep_dat_bl$oa[, oa_items][sep_dat_bl$oa[, oa_items]             == 555] <- NA
sep_dat_bl$bbsiq[, bbsiq_items][sep_dat_bl$bbsiq[, bbsiq_items] == 555] <- NA

# ---------------------------------------------------------------------------- #
# Check response ranges in Sets A and B and clean item-level baseline data ----
# ---------------------------------------------------------------------------- #

# Note: In present Managing Anxiety study, RR response options were 0:3, whereas 
# in Calm Thinking study, they were 1:4

# For Set A

all(sort(unique(as.vector(as.matrix(flt_dat$oa[, oa_items]))))               %in% 0:4)
all(sort(unique(as.vector(as.matrix(flt_dat$rr[, rr_items]))))               %in% 0:3)
all(sort(unique(as.vector(as.matrix(flt_dat$bbsiq[, bbsiq_items]))))         %in% 0:4)
all(sort(unique(as.vector(as.matrix(flt_dat$dass21_as[, dass21_as_items])))) %in% 0:3)
all(sort(unique(as.vector(as.matrix(flt_dat$dass21_ds[, dass21_ds_items])))) %in% 0:3)

# For Set B

all(sort(unique(as.vector(as.matrix(flt_dat_b$oa[, oa_items]))))               %in% 0:4)
all(sort(unique(as.vector(as.matrix(flt_dat_b$rr[, rr_items]))))               %in% 0:3)
all(sort(unique(as.vector(as.matrix(flt_dat_b$bbsiq[, bbsiq_items]))))         %in% 0:4)
all(sort(unique(as.vector(as.matrix(flt_dat_b$dass21_as[, dass21_as_items])))) %in% 0:3)
all(sort(unique(as.vector(as.matrix(flt_dat_b$dass21_ds[, dass21_ds_items])))) %in% 0:3)

# For clean item-level baseline data

all(sort(unique(as.vector(as.matrix(sep_dat_bl$oa[, oa_items]))))             %in% 0:4)
all(sort(unique(as.vector(as.matrix(sep_dat_bl$rr[, rr_items]))))             %in% 0:3)
all(sort(unique(as.vector(as.matrix(sep_dat_bl$bbsiq[, bbsiq_items]))))       %in% 0:4)
all(sort(unique(as.vector(as.matrix(sep_dat_bl$dass_as[, dass21_as_items])))) %in% 0:3)

# ---------------------------------------------------------------------------- #
# Compute selected scores in Sets A and B and clean item-level baseline data ----
# ---------------------------------------------------------------------------- #

# OASIS

  # Note: The summation used for "oa_total" below is incorrect because it treats 
  # item-level values of NA as responses of 0, but this seems to be what was done 
  # in "R34.ipynb" cleaning script

  # For Set A

# View(flt_dat$oa[rowSums(is.na(flt_dat$oa[, oa_items])) > 0, ])
sum(rowSums(is.na(flt_dat$oa[, oa_items])) == ncol(flt_dat$oa[, oa_items]))

flt_dat$oa$oa_total <- rowSums(flt_dat$oa[, oa_items], na.rm = TRUE)

  # For Set B

# View(flt_dat_b$oa[rowSums(is.na(flt_dat_b$oa[, oa_items])) > 0, ])
sum(rowSums(is.na(flt_dat_b$oa[, oa_items])) == ncol(flt_dat_b$oa[, oa_items]))

flt_dat_b$oa$oa_total <- rowSums(flt_dat_b$oa[, oa_items], na.rm = TRUE)

  # For clean item-level baseline data

# View(sep_dat_bl$oa[rowSums(is.na(sep_dat_bl$oa[, oa_items])) > 0, ])
sum(rowSums(is.na(sep_dat_bl$oa[, oa_items])) == ncol(sep_dat_bl$oa[, oa_items]))

sep_dat_bl$oa$oa_total <- rowSums(sep_dat_bl$oa[, oa_items], na.rm = TRUE)

# RR

  # Define function to compute RR scores

compute_rr_scores <- function(dat) {
  dat$rr$rr_nf_mean <- rowMeans(dat$rr[, rr_nf_items], na.rm = TRUE)
  dat$rr$rr_ns_mean <- rowMeans(dat$rr[, rr_ns_items], na.rm = TRUE)
  dat$rr$rr_pf_mean <- rowMeans(dat$rr[, rr_pf_items], na.rm = TRUE)
  dat$rr$rr_ps_mean <- rowMeans(dat$rr[, rr_ps_items], na.rm = TRUE)
  
  return(dat)
}

  # Run function for Sets A and B and clean item-level baseline data

flt_dat    <- compute_rr_scores(flt_dat)
flt_dat_b  <- compute_rr_scores(flt_dat_b)
sep_dat_bl <- compute_rr_scores(sep_dat_bl)

# BBSIQ

  # Note: BBSIQ scores below are those per the Managing Anxiety main outcomes paper
  # - The computation of the scores for "bbsiq_int_ratio" (as the ratio of the mean 
  # for negative internal items to the mean for benign internal items) and for 
  # "bbsiq_ext_ratio" (as the ratio of the mean for negative external items to the 
  # mean for benign external items) is what was done in "R34.ipynb" cleaning script
  # - The computation of "bbsiq_ratio_mean" (as the mean of the ratios above) is what 
  # was done in the "Script1_DataPrep.R" script on the Managing Anxiety main outcomes 
  # paper OSF project (https://osf.io/3b67v)

  # Define function to compute BBSIQ scores

compute_bbsiq_scores <- function(dat) {
  dat$bbsiq$bbsiq_neg_int_mean <- rowMeans(dat$bbsiq[, bbsiq_neg_int_items], na.rm = TRUE)
  dat$bbsiq$bbsiq_ben_int_mean <- rowMeans(dat$bbsiq[, bbsiq_ben_int_items], na.rm = TRUE)
  dat$bbsiq$bbsiq_neg_ext_mean <- rowMeans(dat$bbsiq[, bbsiq_neg_ext_items], na.rm = TRUE)
  dat$bbsiq$bbsiq_ben_ext_mean <- rowMeans(dat$bbsiq[, bbsiq_ben_ext_items], na.rm = TRUE)
  
  dat$bbsiq$bbsiq_int_ratio <- dat$bbsiq$bbsiq_neg_int_mean / dat$bbsiq$bbsiq_ben_int_mean
  dat$bbsiq$bbsiq_ext_ratio <- dat$bbsiq$bbsiq_neg_ext_mean / dat$bbsiq$bbsiq_ben_ext_mean
  
  dat$bbsiq$bbsiq_ratio_mean <- rowMeans(dat$bbsiq[, c("bbsiq_int_ratio", "bbsiq_ext_ratio")], na.rm = TRUE)
  
  return(dat)
}

  # Run function for Sets A and B and clean item-level baseline data

flt_dat    <- compute_bbsiq_scores(flt_dat)
flt_dat_b  <- compute_bbsiq_scores(flt_dat_b)
sep_dat_bl <- compute_bbsiq_scores(sep_dat_bl)

# Note: The DASS-21-AS and DASS-21-DS scores below are those per the Managing Anxiety 
# main outcomes paper
# - Imputing item-level NAs with the median of the item's column was done in "R34.ipynb"
# - Multiplying the total score by 2 was also done in "R34.ipynb"

  # DASS-21-AS

    # Impute item-level NAs with median of the item's column in Set A (since column
    # median depends on the number of rows)

      # Compute item medians in Set A

flt_dat_dass21_as_item_medians <- sapply(dass21_as_items, function(x) {
  median(flt_dat$dass21_as[[x]], na.rm = TRUE)
})
names(flt_dat_dass21_as_item_medians) <- dass21_as_items

nrow(flt_dat$dass21_as) == 871
all(flt_dat_dass21_as_item_medians == c(1, 1, 2, 2, 2, 1, 2))

      # Define function to impute item-level NAs with specified item medians

impute_given_item_medians <- function(df, items, item_medians) {
  if (!all(items == names(item_medians))) stop("Items and medians have different names or order")
  
  for (item in items) {
    item_median <- item_medians[item]
    
    na_item_idx <- which(is.na(df[[item]]))
    
    df[[item]][na_item_idx] <- item_median
  }
  
  return(df)
}

      # Run function for Sets A and B and clean item-level baseline data

flt_dat$dass21_as   <- impute_given_item_medians(flt_dat$dass21_as,   dass21_as_items, flt_dat_dass21_as_item_medians)
flt_dat_b$dass21_as <- impute_given_item_medians(flt_dat_b$dass21_as, dass21_as_items, flt_dat_dass21_as_item_medians)
sep_dat_bl$dass_as  <- impute_given_item_medians(sep_dat_bl$dass_as,  dass21_as_items, flt_dat_dass21_as_item_medians)

    # Compute doubled total score for Sets A and B and clean item-level baseline data

flt_dat$dass21_as$dass21_as_total_dbl   <- rowSums(flt_dat$dass21_as[, dass21_as_items]) * 2
flt_dat_b$dass21_as$dass21_as_total_dbl <- rowSums(flt_dat_b$dass21_as[, dass21_as_items]) * 2
sep_dat_bl$dass_as$dass21_as_total_dbl  <- rowSums(sep_dat_bl$dass_as[, dass21_as_items]) * 2

  # DASS-21-DS

    # Impute item-level NAs in Sets A and B with median of the item's column in Set A

flt_dat_dass21_ds_item_medians <- sapply(dass21_ds_items, function(x) {
  median(flt_dat$dass21_ds[[x]], na.rm = TRUE)
})
names(flt_dat_dass21_ds_item_medians) <- dass21_ds_items

nrow(flt_dat$dass21_ds) == 1175
all(flt_dat_dass21_ds_item_medians == c(1, 2, 1, 1, 1, 1, 1))

flt_dat$dass21_ds   <- impute_given_item_medians(flt_dat$dass21_ds,   dass21_ds_items, flt_dat_dass21_ds_item_medians)
flt_dat_b$dass21_ds <- impute_given_item_medians(flt_dat_b$dass21_ds, dass21_ds_items, flt_dat_dass21_ds_item_medians)

    # Compute doubled total score for Sets A and B

flt_dat$dass21_ds$dass21_ds_total_dbl   <- rowSums(flt_dat$dass21_ds[, dass21_ds_items]) * 2
flt_dat_b$dass21_ds$dass21_ds_total_dbl <- rowSums(flt_dat_b$dass21_ds[, dass21_ds_items]) * 2

# ---------------------------------------------------------------------------- #
# Clean demographic table (with focus on Set A) and compare with clean data ----
# ---------------------------------------------------------------------------- #

# Given that the sections below ultimately base the item-level data on Set A and 
# that Set A is not missing any demographics data, the present section focuses on
# cleaning Set A. For a given item, if Sets A and B are identical, only Set A is
# cleaned and compared with clean data. If Sets A and B are not identical, then
# Sets A and B are cleaned until they are identical, and then Set A is compared
# with clean data. Once it is shown that Set A reproduces the clean data, is some
# cases additional cleaning is done that deviates from clean data (i.e., cleaning
# additional odd birth year values vs. recoding them as NA; cleaning additional
# odd education values; recoding blank values as missing due to a server issue).

# Other demographics cleaning and analysis scripts for reference:
# - On GitHub (https://github.com/TeachmanLab/MT-Data-ManagingAnxietyStudy/tree/master/Data%20Cleaning)
#   - "R34_cleaning_script.R"
#   - "R34.ipynb"
# - On MA main outcomes paper OSF project
#   - "Script0_Demographics.R" (https://osf.io/uv5jm)

# Order Sets A and B and clean data by participant and compare participant IDs

flt_dat$demographic$participant_id <- as.integer(flt_dat$demographic$participant_id)

  # Sort by participant in Sets A and B and in clean data

flt_dat$demographic   <- flt_dat$demographic[order(flt_dat$demographic$participant_id), ]
flt_dat_b$demographic <- flt_dat_b$demographic[order(flt_dat_b$demographic$participant_id), ]
sep_dat$demographic   <- sep_dat$demographic[order(sep_dat$demographic$participant_id), ]

row.names(flt_dat$demographic)   <- 1:nrow(flt_dat$demographic)
row.names(flt_dat_b$demographic) <- 1:nrow(flt_dat_b$demographic)
row.names(sep_dat$demographic)   <- 1:nrow(sep_dat$demographic)

  # Sets A and B and clean data all contain the same participants

identical(flt_dat$demographic$participant_id, flt_dat_b$demographic$participant_id,
          sep_dat$demographic$participant_id)

# For birth year:

  # Recode values of 0 or 2222 to NA in Sets A and B (which was done in both
  # "R34_cleaning_script.R" and "R34.ipynb")

sum(flt_dat$demographic$birthYear   %in% c(0, 2222)) == 2
sum(flt_dat_b$demographic$birthYear %in% c(0, 2222)) == 1

flt_dat$demographic$birthYear[flt_dat$demographic$birthYear     %in% c(0, 2222)] <- NA
flt_dat_b$demographic$birthYear[flt_dat_b$demographic$birthYear %in% c(0, 2222)] <- NA

  # Some values in Set A are only 2 digits or longer than 4 digits. These seem
  # to have been corrected in Set B, whose values are all 4 digits. However, the
  # clean data doesn't include all of the corrected values (treats some as NA).

odd_birth_year_pids <- flt_dat$demographic$participant_id[!is.na(flt_dat$demographic$birthYear) &
                                                            nchar(flt_dat$demographic$birthYear) != 4]

  # Clean the values in Set A in accordance with those from Set B, after which
  # Sets A and B contain the same non-NA values and same number of NAs but the clean
  # data contains more NAs (but the same values that are non-NA across datasets).
  # The clean data has NAs specifically for values > 2222, even though it seems
  # that "R34.ipynb" intended to make the same corrections below (see this issue:
  # https://github.com/TeachmanLab/MT-Data-ManagingAnxietyStudy/issues/9#issue-3457953028).
  # "R34_cleaning_script.R" also made these corrections.

flt_dat$demographic$birthYear[flt_dat$demographic$birthYear == 195100]                  <- 1951
flt_dat$demographic$birthYear[flt_dat$demographic$birthYear == 1900955]                 <- 1955
flt_dat$demographic$birthYear[flt_dat$demographic$birthYear %in% c(19001959, 19019590)] <- 1959
flt_dat$demographic$birthYear[flt_dat$demographic$birthYear == 61]                      <- 1961
flt_dat$demographic$birthYear[flt_dat$demographic$birthYear == 64]                      <- 1964
flt_dat$demographic$birthYear[flt_dat$demographic$birthYear == 1972900]                 <- 1972
flt_dat$demographic$birthYear[flt_dat$demographic$birthYear == 19001975]                <- 1975
flt_dat$demographic$birthYear[flt_dat$demographic$birthYear == 19001976]                <- 1976
flt_dat$demographic$birthYear[flt_dat$demographic$birthYear %in% c(77, 19001977)]       <- 1977
flt_dat$demographic$birthYear[flt_dat$demographic$birthYear == 80]                      <- 1980
flt_dat$demographic$birthYear[flt_dat$demographic$birthYear %in% c(81, 19001981)]       <- 1981
flt_dat$demographic$birthYear[flt_dat$demographic$birthYear == 82]                      <- 1982
flt_dat$demographic$birthYear[flt_dat$demographic$birthYear %in% c(87, 19001987)]       <- 1987
flt_dat$demographic$birthYear[flt_dat$demographic$birthYear %in% c(89, 19001989)]       <- 1989
flt_dat$demographic$birthYear[flt_dat$demographic$birthYear == 92]                      <- 1992
flt_dat$demographic$birthYear[flt_dat$demographic$birthYear == 19001997]                <- 1997

flt_dat$demographic$birthYear <- as.integer(flt_dat$demographic$birthYear)

identical(flt_dat$demographic$birthYear, flt_dat_b$demographic$birthYear)

all(flt_dat$demographic$birthYear == sep_dat$demographic$demographic_birthYear, na.rm = TRUE)
sum(is.na(flt_dat$demographic$birthYear))             == 2
sum(is.na(sep_dat$demographic$demographic_birthYear)) == 14

# Compute age in Set A

  # Define function to compute age from year of demographics data (per "R34.ipynb")

compute_age <- function(df) {
  date_year <- as.integer(format(df$date_as_POSIXct, "%Y"))
  
  df$age <- date_year - df$birthYear
  
  return(df)
}

  # Run function for Sets A and B

flt_dat$demographic   <- compute_age(flt_dat$demographic)
flt_dat_b$demographic <- compute_age(flt_dat_b$demographic)

  # Non-NA values match those in clean data

identical(flt_dat$demographic$age, flt_dat_b$demographic$age)

all(flt_dat$demographic$age == sep_dat$demographic$demographic_age, na.rm = TRUE)

# For gender, given that Sets A and B are identical, recode odd values in Set A 
# in ways (per "R34.ipynb" and "R34_cleaning_script.R") that match clean data

identical(flt_dat$demographic$gender, flt_dat_b$demographic$gender)

flt_dat$demographic$gender[flt_dat$demographic$gender == "?"]       <- ""
flt_dat$demographic$gender[flt_dat$demographic$gender == "Femmina"] <- "Female"
flt_dat$demographic$gender[flt_dat$demographic$gender == "Mâle"]    <- "Male"

identical(flt_dat$demographic$gender, sep_dat$demographic$demographic_gender)

  # But rather than retaining a blank value, recode it in Set A to reflect that 
  # it is missing due to an apparent server issue

missing_server_issue <- "Missing (server issue)"

flt_dat$demographic$gender[flt_dat$demographic$gender == ""] <- missing_server_issue

# For education, given that Sets A and B are identical, and nearly the same (except 
# for one odd level) as clean data, recode odd values in Set A (per "R34_cleaning_script.R";
# "R34.ipynb" did not clean education; see issue
# https://github.com/TeachmanLab/MT-Data-ManagingAnxietyStudy/issues/10#issue-3458115537)

identical(flt_dat$demographic$education, flt_dat_b$demographic$education)

identical(flt_dat$demographic$education[flt_dat$demographic$education != "Un lycée"],
          sep_dat$demographic$demographic_education[sep_dat$demographic$demographic_education != "Un lyc̩e"])

flt_dat$demographic$education[flt_dat$demographic$education %in% c("", "????")] <- missing_server_issue
flt_dat$demographic$education[flt_dat$demographic$education %in% 
                                c("Diploma di scuola superiore", "Un lycée")]   <- "High School Graduate"

# For ethnicity, given that Sets A and B are identical, recode odd values in Set A
# in ways (per "R34.ipynb" and "R34_cleaning_script.R") that match clean data

identical(flt_dat$demographic$ethnicity, flt_dat_b$demographic$ethnicity)

flt_dat$demographic$ethnicity[flt_dat$demographic$ethnicity == "??????????"]                  <- ""
flt_dat$demographic$ethnicity[flt_dat$demographic$ethnicity %in% c("Inconnu", "Sconosciuto")] <- "Unknown"

identical(flt_dat$demographic$ethnicity, sep_dat$demographic$demographic_ethnicity)

  # But recode blank value in Set A to reflect that it's missing due to server issue

flt_dat$demographic$ethnicity[flt_dat$demographic$ethnicity == ""] <- missing_server_issue

# For employment status, given that Sets A and B are identical, recode odd values
# in Set A in ways (per "R34.ipynb" and "R34_cleaning_script.R") that match clean data

identical(flt_dat$demographic$employmentStatus, flt_dat_b$demographic$employmentStatus)

flt_dat$demographic$employmentStatus[flt_dat$demographic$employmentStatus == "????"]             <- ""
flt_dat$demographic$employmentStatus[flt_dat$demographic$employmentStatus == "Étudiant"]         <- "Student"
flt_dat$demographic$employmentStatus[flt_dat$demographic$employmentStatus == "Lavoro part-time"] <- "Working part-time"

identical(flt_dat$demographic$employmentStatus, sep_dat$demographic$demographic_employmentStatus)

  # But recode blank value in Set A to reflect that it's missing due to server issue

flt_dat$demographic$employmentStatus[flt_dat$demographic$employmentStatus == ""] <- missing_server_issue

# For income, given that Sets A and B are identical, recode odd values in Set A 
# in ways (per "R34.ipynb" and "R34_cleaning_script.R") that match clean data

identical(flt_dat$demographic$income, flt_dat_b$demographic$income)

flt_dat$demographic$income[flt_dat$demographic$income == "Moins de 5 000 $"]   <- "Less than $5,000"
flt_dat$demographic$income[flt_dat$demographic$income == "$ 5,000 a $ 11999"]  <- "$5,000 through $11,999"
flt_dat$demographic$income[flt_dat$demographic$income == "$ 50,000??$ 74,999"] <- "$50,000 through $74,999"

identical(flt_dat$demographic$income, sep_dat$demographic$demographic_income)

  # But recode blank value in Set A to reflect that it's missing due to server issue

flt_dat$demographic$income[flt_dat$demographic$income == ""] <- missing_server_issue

# For marital status, given that Sets A and B are identical, recode odd values in Set A 
# in ways (per "R34.ipynb" and "R34_cleaning_script.R") that match clean data

identical(flt_dat$demographic$maritalStatus, flt_dat_b$demographic$maritalStatus)

flt_dat$demographic$maritalStatus[flt_dat$demographic$maritalStatus == "??"]     <- ""
flt_dat$demographic$maritalStatus[flt_dat$demographic$maritalStatus == 
                                    "Single, ma attualmente fidanzato"]          <- "Single, but currently engaged to be married"
flt_dat$demographic$maritalStatus[flt_dat$demographic$maritalStatus == "Unique"] <- "Single"

identical(flt_dat$demographic$maritalStatus, sep_dat$demographic$demographic_maritalStatus)

  # But recode blank value in Set A to reflect that it's missing due to server issue

flt_dat$demographic$maritalStatus[flt_dat$demographic$maritalStatus == ""] <- missing_server_issue

# For race, given that Sets A and B are identical, recode odd values in Set A 
# in ways (per "R34.ipynb" and "R34_cleaning_script.R") that match clean data

identical(flt_dat$demographic$race, flt_dat_b$demographic$race)

flt_dat$demographic$race[flt_dat$demographic$race == "?/????"]                          <- ""
flt_dat$demographic$race[flt_dat$demographic$race %in% 
                           c("Bianco / origine europea", "Blanc / origine européenne")] <- "White/European origin"

identical(flt_dat$demographic$race, sep_dat$demographic$demographic_race)

  # But recode blank value in Set A to reflect that it's missing due to server issue

flt_dat$demographic$race[flt_dat$demographic$race == ""] <- missing_server_issue

# For country, given that Sets A and B and clean data are identical, recode blank 
# value in Set A to reflect that it's missing due to server issue

identical(flt_dat$demographic$residenceCountry, flt_dat_b$demographic$residenceCountry)
identical(flt_dat$demographic$residenceCountry, sep_dat$demographic$demographic_residenceCountry)

flt_dat$demographic$residenceCountry[flt_dat$demographic$residenceCountry     == ""] <- missing_server_issue

# ---------------------------------------------------------------------------- #
# Compare clean data and Set A on non-demographic tables ----
# ---------------------------------------------------------------------------- #

# Define lists with corresponding tables

flt_dat_comp <- flt_dat[c("bbsiq", "dass21_as", "dass21_ds", "demographic", 
                          "oa", "participant_export_dao", "rr")]
sep_dat_comp <- sep_dat[c("bbsiq", "dass_as", "dass_ds", "demographic", 
                          "oa", "participant", "rr")]

# Compare participant IDs for each table. Set A tables have no participants not in 
# clean data tables. By contrast, "dass_as", "oa", and "participant" tables in clean
# data have 36, 37, and 36 more participants than those in Set A.

length_diff_participant_ids <- function(x, y) {
  length(setdiff(x$participant_id, y$participant_id))
}
all(mapply(length_diff_participant_ids, flt_dat_comp, sep_dat_comp) == 0)
mapply(length_diff_participant_ids, sep_dat_comp, flt_dat_comp)

diff_participant_ids <- function(x, y) {
  setdiff(x$participant_id, y$participant_id)
}
mapply(diff_participant_ids, flt_dat_comp, sep_dat_comp)
mapply(diff_participant_ids, sep_dat_comp, flt_dat_comp)

# Restrict to shared "participant_id"s in each table and confirm that is so

  # Define function to restrict to shared "participant_id"s in each table

restrict_to_shared_ps <- function(dat1, dat2) {
  dat1_rest <- vector("list", length(dat1))
  dat2_rest <- vector("list", length(dat2))
  
  names(dat1_rest) <- names(dat1)
  names(dat2_rest) <- names(dat2)
  
  for (i in 1:length(dat1)) {
    shared_participant_ids <- intersect(dat1[[i]][, "participant_id"], 
                                        dat2[[i]][, "participant_id"])
    dat1_rest[[i]] <- dat1[[i]][dat1[[i]][, "participant_id"] %in% shared_participant_ids, ]
    dat2_rest[[i]] <- dat2[[i]][dat2[[i]][, "participant_id"] %in% shared_participant_ids, ]
  }
  
  ls_rest <- list(dat1_rest, dat2_rest)
  
  return(ls_rest)
}

  # Run function

ls_rest <- restrict_to_shared_ps(flt_dat_comp, sep_dat_comp)
flt_dat_comp_rest <- ls_rest[[1]]
sep_dat_comp_rest <- ls_rest[[2]]

  # Confirm

all(mapply(length_diff_participant_ids, flt_dat_comp_rest, sep_dat_comp_rest) == 0)
all(mapply(length_diff_participant_ids, sep_dat_comp_rest, flt_dat_comp_rest) == 0)

# Sort tables by participant and then session

flt_dat_comp_rest <- sort_by_part_then_session(flt_dat_comp_rest, "participant_id", "session_only")
sep_dat_comp_rest <- sort_by_part_then_session(sep_dat_comp_rest, "participant_id", "session_only")

# Compare numbers of observations. They differ between datasets.

set_a_vs_cln_nrow <- data.frame(set_a = sapply(flt_dat_comp_rest, nrow),
                                clean = sapply(sep_dat_comp_rest, nrow))
set_a_vs_cln_nrow$diff <- set_a_vs_cln_nrow$set_a - set_a_vs_cln_nrow$clean

set_a_vs_cln_nrow

# Use natural join to restrict to shared time points for "oa" table. All scores
# are the same.

merge_oa <- merge(flt_dat_comp_rest$oa, 
                  sep_dat_comp_rest$oa,
                  by = c("participant_id", "session_only"),
                  all = FALSE)

all(merge_oa$oa_total == merge_oa$oasis_score)

  # Compare session dates between "oa" and "rr" tables in Set A, although "oa" 
  # was assessed at every time point and "rr" was assessed at fewer time points. 
  # The session dates are inconsistent for 30 participants between these tables.

    # Define function to identify participant IDs whose session dates between two
    # tables are unequal

get_unequal_session_dates_pids <- function(df1, df2,
                                           df1_subscript, df2_subscript,
                                           df1_date_as_POSIXct_col, df2_date_as_POSIXct_col) {
  df1_date_only_as_POSIXct_name <- paste0("date_only_as_POSIXct", df1_subscript)
  df2_date_only_as_POSIXct_name <- paste0("date_only_as_POSIXct", df2_subscript)
  
  fmt <- "%Y-%m-%d"
  
  df1[[df1_date_only_as_POSIXct_name]] <- as.Date(format(df1[[df1_date_as_POSIXct_col]], fmt))
  df2[[df2_date_only_as_POSIXct_name]] <- as.Date(format(df2[[df2_date_as_POSIXct_col]], fmt))
  
  mrg <- merge(df1, df2,
               by = c("participant_id", "session_only"),
               suffixes = c(df1_subscript, df2_subscript),
               all = FALSE)
  
  unequal_session_dates_idx <- which(mrg[[df1_date_only_as_POSIXct_name]] != 
                                       mrg[[df2_date_only_as_POSIXct_name]])

  unequal_session_dates_pids <- sort(unique(mrg$participant_id[unequal_session_dates_idx]))
  
  return(unequal_session_dates_pids)
}

    # Run function

flt_dat_comp_rest_oa_sessions <- flt_dat_comp_rest$oa[c("participant_id", "date_as_POSIXct", "session_only")]
flt_dat_comp_rest_rr_sessions <- flt_dat_comp_rest$rr[c("participant_id", "date_as_POSIXct", "session_only")]

set_a_unequal_oa_rr_session_dates_pids <- 
  get_unequal_session_dates_pids(flt_dat_comp_rest_oa_sessions, flt_dat_comp_rest_rr_sessions,
                                 "_oa", "_rr",
                                 "date_as_POSIXct", "date_as_POSIXct")

length(set_a_unequal_oa_rr_session_dates_pids) == 30

class(flt_dat_comp_rest_oa_sessions$participant_id) == "integer"
flt_dat_comp_rest_rr_sessions$participant_id <- as.integer(flt_dat_comp_rest_rr_sessions$participant_id)
flt_dat_comp_rest_rr_sessions <- 
  flt_dat_comp_rest_rr_sessions[order(flt_dat_comp_rest_rr_sessions$participant_id), ]

# View(flt_dat_comp_rest_oa_sessions[flt_dat_comp_rest_oa_sessions$participant_id %in%
#                                      set_a_unequal_oa_rr_session_dates_pids, ])
# View(flt_dat_comp_rest_rr_sessions[flt_dat_comp_rest_rr_sessions$participant_id %in%
#                                      set_a_unequal_oa_rr_session_dates_pids, ])

  # TODO (check this): Compare session dates between "oa" and "task_log" tables in Set A

flt_dat_task_log_oa_sessions <- flt_dat$task_log[flt_dat$task_log$task_name == "OA",
                                                 c("participant_id",
                                                   "corrected_datetime_as_POSIXct", 
                                                   "session_only")]
flt_dat_task_log_oa_sessions <- 
  flt_dat_task_log_oa_sessions[order(flt_dat_task_log_oa_sessions$participant_id,
                                     flt_dat_task_log_oa_sessions$corrected_datetime_as_POSIXct), ]

set_a_unequal_oa_tl_session_dates_pids <- 
  get_unequal_session_dates_pids(flt_dat_comp_rest_oa_sessions, flt_dat_task_log_oa_sessions,
                                 "_oa", "_tl",
                                 "date_as_POSIXct", "corrected_datetime_as_POSIXct")

length(set_a_unequal_oa_tl_session_dates_pids) == 60

# View(flt_dat_comp_rest_oa_sessions[flt_dat_comp_rest_oa_sessions$participant_id %in%
#                                      set_a_unequal_oa_tl_session_dates_pids, ])
# View(flt_dat_task_log_oa_sessions[flt_dat_task_log_oa_sessions$participant_id %in%
#                                     set_a_unequal_oa_tl_session_dates_pids, ])





  # Identify participants with sessions skipped in OASIS table in Set A

    # Define function to identify participant IDs with a given number of nonconsecutive 
    # expected "session_only" values (or any number, which is the default) in a table

identify_skipped_session_pids <- function(df, expected_session_order, n_sessions_skipped = "any") {
  df_split <- split(df, df$participant_id)
  
  skipped_session <- sapply(df_split, function(x) {
    obs_sessions <- x$session_only
    obs_sessions_idx <- sort(match(obs_sessions, expected_session_order))

    if (any(diff(obs_sessions_idx) == 0)) warning("At least one repeated 'session_only' value")
    
    obs_n_sessions_skipped <- sum(diff(obs_sessions_idx) > 1)

    if (n_sessions_skipped == "any") {
      obs_n_sessions_skipped >= 1
    } else {
      obs_n_sessions_skipped == n_sessions_skipped
    }
    
  })
  
  skipped_session_pids <- as.integer(names(skipped_session)[skipped_session])
  
  return(skipped_session_pids)
}

    # Run function. All skipped only one session.

possible_sessions_oa <- c("PRE", paste0("SESSION", 1:8), "POST")

skipped_any_oa_session_set_a_pids    <- identify_skipped_session_pids(flt_dat_comp_rest$oa, possible_sessions_oa, "any")
skipped_only_1_oa_session_set_a_pids <- identify_skipped_session_pids(flt_dat_comp_rest$oa, possible_sessions_oa, 1)

setequal(skipped_any_oa_session_set_a_pids, skipped_only_1_oa_session_set_a_pids)

length(skipped_only_1_oa_session_set_a_pids) == 111

  # Determine which session was skipped

    # Define function to identify participant IDs with a specific "session_only"
    # value skipped in a table (i.e., have an observed session after the specific
    # session but lack the specific session)

identify_skipped_specific_session_pids <- function(df, expected_session_order, specific_session) {
  specific_session_idx <- match(specific_session, expected_session_order)
  last_expected_session_idx <- length(expected_session_order)

  if (specific_session_idx == last_expected_session_idx) {
    stop("Can't 'skip' the last expected session. Change 'specific_session'.")
  }
  
  df_split <- split(df, df$participant_id)
  
  skipped_specific_session <- sapply(df_split, function(x) {
    obs_sessions <- x$session_only
    obs_sessions_idx <- sort(match(obs_sessions, expected_session_order))
    max_obs_session_idx <- max(obs_sessions_idx)

    if (max_obs_session_idx > specific_session_idx) {
      !(specific_session %in% obs_sessions)
    } else {
      FALSE
    }
    
  })
  
  skipped_specific_session_pids <- as.integer(names(skipped_specific_session)[skipped_specific_session])
  
  return(skipped_specific_session_pids)
}

    # Run function. Of the 111 with a session skipped, 109 have only S1 skipped
    # (12 of these are those whose OASIS dates don't match their RR dates for the
    # same session), and 2 have only S3 skipped

skipped_oa_S1_set_a_pids <- identify_skipped_specific_session_pids(flt_dat_comp_rest$oa, possible_sessions_oa, "SESSION1")
skipped_oa_S3_set_a_pids <- identify_skipped_specific_session_pids(flt_dat_comp_rest$oa, possible_sessions_oa, "SESSION3")

skipped_only_oa_S1_set_a_pids <- intersect(skipped_only_1_oa_session_set_a_pids, skipped_oa_S1_set_a_pids)
length(skipped_only_oa_S1_set_a_pids) == 109

length(intersect(set_a_unequal_oa_rr_session_dates_pids, skipped_oa_S1_set_a_pids)) == 12

skipped_only_oa_S3_set_a_pids <- intersect(skipped_only_1_oa_session_set_a_pids, skipped_oa_S3_set_a_pids)
length(skipped_only_oa_S3_set_a_pids) == 2

# Use natural join to restrict to shared time points for "rr" table. All scores
# are the same when rounded to 9 decimal places.

merge_rr <- merge(flt_dat_comp_rest$rr, 
                  sep_dat_comp_rest$rr,
                  by = c("participant_id", "session_only"),
                  all = FALSE)

  # Define function to confirm all RR scores are the same

confirm_all_rr_scores_same <- function(merge_rr, digits) {
  all(sum(round(merge_rr$rr_nf_mean, digits) != round(merge_rr$RR_negative_nf_score, digits), na.rm = TRUE) == 0,
      sum(round(merge_rr$rr_ns_mean, digits) != round(merge_rr$RR_negative_ns_score, digits), na.rm = TRUE) == 0,
      sum(round(merge_rr$rr_pf_mean, digits) != round(merge_rr$RR_positive_pf_score, digits), na.rm = TRUE) == 0,
      sum(round(merge_rr$rr_ps_mean, digits) != round(merge_rr$RR_positive_ps_score, digits), na.rm = TRUE) == 0,
      sum(which(is.na(merge_rr$rr_nf_mean))  != which(is.na(merge_rr$RR_negative_nf_score)))                == 0,
      sum(which(is.na(merge_rr$rr_ns_mean))  != which(is.na(merge_rr$RR_negative_ns_score)))                == 0,
      sum(which(is.na(merge_rr$rr_pf_mean))  != which(is.na(merge_rr$RR_positive_pf_score)))                == 0,
      sum(which(is.na(merge_rr$rr_ps_mean))  != which(is.na(merge_rr$RR_positive_ps_score)))                == 0)
}

  # Run function

confirm_all_rr_scores_same(merge_rr, 9) # TRUE

# Use natural join to restrict to shared time points for "bbsiq" table. All scores
# are the same when rounded to 7 decimal places.

merge_bbsiq <- merge(flt_dat_comp_rest$bbsiq, 
                     sep_dat_comp_rest$bbsiq,
                     by = c("participant_id", "session_only"),
                     all = FALSE)

  # Define function to confirm all BBSIQ scores are the same

confirm_all_bbsiq_scores_same <- function(merge_bbsiq, digits) {
  all(sum(round(merge_bbsiq$bbsiq_int_ratio, digits) != round(merge_bbsiq$bbsiq_physical_score, digits), na.rm = TRUE) == 0,
      sum(round(merge_bbsiq$bbsiq_ext_ratio, digits) != round(merge_bbsiq$bbsiq_threat_score,   digits), na.rm = TRUE) == 0,
      sum(which(is.na(merge_bbsiq$bbsiq_int_ratio))  != which(is.na(merge_bbsiq$bbsiq_physical_score)))                == 0,
      sum(which(is.na(merge_bbsiq$bbsiq_ext_ratio))  != which(is.na(merge_bbsiq$bbsiq_threat_score)))                  == 0)
}

  # Run function

confirm_all_bbsiq_scores_same(merge_bbsiq, 7) # TRUE

# Use natural join to restrict to shared time points for "dass21_as" table. All scores
# are the same.

merge_dass21_as <- merge(flt_dat_comp_rest$dass21_as, 
                         sep_dat_comp_rest$dass_as,
                         by = c("participant_id", "session_only"),
                         all = FALSE)

all(merge_dass21_as$dass21_as_total_dbl == merge_dass21_as$dass_as_score) # TRUE

# Use natural join to restrict to shared time points for "dass21_ds" table. All scores
# are the same.

merge_dass21_ds <- merge(flt_dat_comp_rest$dass21_ds, 
                         sep_dat_comp_rest$dass_ds,
                         by = c("participant_id", "session_only"),
                         all = FALSE)

all(merge_dass21_ds$dass21_ds_total_dbl == merge_dass21_ds$dass_ds_score) # TRUE

# ---------------------------------------------------------------------------- #
# Compare clean data and Set B on non-demographic tables ----
# ---------------------------------------------------------------------------- #

# Define lists with corresponding tables

flt_dat_comp_b <- flt_dat_b[c("bbsiq", "dass21_as", "dass21_ds", "demographic", "oa", "rr")]
sep_dat_comp_b <- sep_dat[c("bbsiq", "dass_as", "dass_ds", "demographic", "oa", "rr")]

# Using functions defined above for Set A, compare participant IDs for each table. 
# Set B tables have no participants not in clean data tables. By contrast, "oa" 
# table in clean data has 1 more participant (1866) than that in Set B.

all(mapply(length_diff_participant_ids, flt_dat_comp_b, sep_dat_comp_b) == 0)
mapply(length_diff_participant_ids, sep_dat_comp_b, flt_dat_comp_b)

mapply(diff_participant_ids, flt_dat_comp_b, sep_dat_comp_b)
mapply(diff_participant_ids, sep_dat_comp_b, flt_dat_comp_b)

# Restrict to shared participant_ids in each table and confirm that is so

  # Run function defined for Set A

ls_rest_b <- restrict_to_shared_ps(flt_dat_comp_b, sep_dat_comp_b)
flt_dat_comp_rest_b <- ls_rest_b[[1]]
sep_dat_comp_rest_b <- ls_rest_b[[2]]

  # Confirm

all(mapply(length_diff_participant_ids, flt_dat_comp_rest_b, sep_dat_comp_rest_b) == 0)
all(mapply(length_diff_participant_ids, sep_dat_comp_rest_b, flt_dat_comp_rest_b) == 0)

# Sort tables by participant and (if present) then session

flt_dat_comp_rest_b <- sort_by_part_then_session(flt_dat_comp_rest_b, "participant_id", "session_only")
sep_dat_comp_rest_b <- sort_by_part_then_session(sep_dat_comp_rest_b, "participant_id", "session_only")

# Compare numbers of observations. They differ between datasets.

set_b_vs_cln_nrow <- data.frame(set_b = sapply(flt_dat_comp_rest_b, nrow),
                                clean = sapply(sep_dat_comp_rest_b, nrow))
set_b_vs_cln_nrow$diff <- set_b_vs_cln_nrow$set_b - set_b_vs_cln_nrow$clean

set_b_vs_cln_nrow

# Use natural join to restrict to shared time points for "oa" table. For 
# each of the 24 participants with discrepancies, the scores are actually the 
# same but the session column in the clean data skips Session 1 (i.e., lists 
# Session 2 instead), whereas the session column in Set B lists all consecutive 
# sessions (i.e., lists Session 1 where the clean data lists Session 2)

merge_oa_b <- merge(flt_dat_comp_rest_b$oa, 
                    sep_dat_comp_rest_b$oa,
                    by = c("participant_id", "session_only"),
                    all = FALSE)

sum(merge_oa_b$oa_total == merge_oa_b$oasis_score) == 2512
sum(merge_oa_b$oa_total != merge_oa_b$oasis_score) == 38

discrep_ids_b <- unique(merge_oa_b[merge_oa_b$oa_total != merge_oa_b$oasis_score, ]$participant_id)
length(discrep_ids_b) == 24

  # All 24 participants are in Set A and lack discrepancies with clean data

all(discrep_ids_b %in% merge_oa$participant_id)

  # Only 10 of them are those in Set A with inconsistencies in session dates
  # between Set A OASIS and RR tables, but all of them are in the 109 in Set A
  # with skipped Session 1 value in "session_only" in OASIS table

length(intersect(discrep_ids_b, set_a_unequal_oa_rr_session_dates_pids)) == 10

all(discrep_ids_b %in% skipped_oa_S1_set_a_pids)

  # No participants have skipped sessions in Set B OASIS table

skipped_oa_session_set_b_pids <- identify_skipped_session_pids(flt_dat_comp_rest_b$oa, possible_sessions_oa, "any")

length(skipped_oa_session_set_b_pids) == 0

  # Session values of Set B are identical to Set A OASIS entries in "task_log".
  # Thus, it seems session was corrected in Set B at some point.

flt_dat_task_log_oa <- flt_dat$task_log[flt_dat$task_log$task_name == "OA", ]
flt_dat_task_log_oa <- flt_dat_task_log_oa[order(flt_dat_task_log_oa$participant_id,
                                                 flt_dat_task_log_oa$corrected_datetime_as_POSIXct), ]
flt_dat_task_log_oa$corrected_date_only_as_POSIXct <-
  as.Date(format(flt_dat_task_log_oa$corrected_datetime_as_POSIXct, "%Y-%m-%d"))

flt_dat_task_log_oa_test <- flt_dat_task_log_oa[flt_dat_task_log_oa$participant_id %in% skipped_oa_S1_set_a_pids, 
                                                c("participant_id", "session_only", "task_name", 
                                                  "corrected_datetime_as_POSIXct", "corrected_date_only_as_POSIXct")]
row.names(flt_dat_task_log_oa_test) <- 1:nrow(flt_dat_task_log_oa_test)

flt_dat_comp_rest_b_oa_test <- flt_dat_comp_rest_b$oa[flt_dat_comp_rest_b$oa$participant_id %in% 
                                                        skipped_oa_S1_set_a_pids, ]
row.names(flt_dat_comp_rest_b_oa_test) <- 1:nrow(flt_dat_comp_rest_b_oa_test)

identical(flt_dat_task_log_oa_test[c("participant_id", "session_only")], 
          flt_dat_comp_rest_b_oa_test[c("participant_id", "session_only")])

  # By contrast, session values of Set A OASIS table are different from Set A
  # OASIS entries in "task_log"

flt_dat_comp_rest_oa_test <- flt_dat_comp_rest$oa[flt_dat_comp_rest$oa$participant_id %in% 
                                                    skipped_oa_S1_set_a_pids, ]
flt_dat_comp_rest_oa_test <- sort_by_part_then_session(flt_dat_comp_rest_oa_test,
                                                       "participant_id", "session_only")
row.names(flt_dat_comp_rest_oa_test) <- 1:nrow(flt_dat_comp_rest_oa_test)

identical(flt_dat_task_log_oa_test[c("participant_id", "session_only")], 
          flt_dat_comp_rest_oa_test[c("participant_id", "session_only")]) == FALSE

    # But when session is recoded to expected order, session values of Set A OASIS
    # table match those from Set A OASIS entries in "task_log"

flt_dat_comp_rest_oa_test <- recode_oa_session_to_expected_order(flt_dat_comp_rest_oa_test, skipped_oa_S1_set_a_pids)

identical(flt_dat_task_log_oa_test[c("participant_id", "session_only")], 
          flt_dat_comp_rest_oa_test[c("participant_id", "session_only")])

  # Which session values make the most sense? Seems that recoding Set A OASIS sessions 
  # to be consecutive (as in Set B) would be most plausible. Indeed, for participants
  # whose session values in Set A skip Session 1, most of the dates are the same for
  # the baseline and Session 2 values, which doesn't make sense because whereas Session
  # 1 could be started right after baseline, Session 2 could not be started until 2 days
  # after Session 1 was completed. OASIS session values are recoded to be consecutive for 
  # participants whose values skip a session (Session 1 or 3) at the end of this script.

# View(flt_dat_comp_rest$oa[flt_dat_comp_rest$oa$participant_id %in% skipped_oa_S1_set_a_pids, ])
# View(flt_dat_comp_rest_b$oa[flt_dat_comp_rest_b$oa$participant_id %in% skipped_oa_S1_set_a_pids, ])
# View(sep_dat_comp_rest$oa[sep_dat_comp_rest_b$oa$participant_id %in% skipped_oa_S1_set_a_pids, ])

# View(flt_dat_comp_rest$oa[flt_dat_comp_rest$oa$participant_id %in% skipped_oa_S3_set_a_pids, ])
# View(flt_dat_comp_rest_b$oa[flt_dat_comp_rest_b$oa$participant_id %in% skipped_oa_S3_set_a_pids, ])
# View(sep_dat_comp_rest$oa[sep_dat_comp_rest_b$oa$participant_id %in% skipped_oa_S3_set_a_pids, ])

  # Define function to view "oa" table from relevant lists for given participant in Set B

view_oa_b <- function(participant_id) {
  View(sel_dat_b$oa[sel_dat_b$oa$participant_id == participant_id, ])
  View(flt_dat_b$oa[flt_dat_b$oa$participant_id == participant_id, ])
  View(sep_dat$oa[sep_dat$oa$participant_id     == participant_id, ])
  View(merge_oa_b[merge_oa_b$participant_id     == participant_id, ])
}

  # Run function

# view_oa_b(431) # Scores same but sessions mismatch
# view_oa_b(434) # Scores same but sessions mismatch
# view_oa_b(449) # Scores same but sessions mismatch
# view_oa_b(460) # Scores same but sessions mismatch
# view_oa_b(462) # Scores same but sessions mismatch
# view_oa_b(471) # Scores same but sessions mismatch
# view_oa_b(476) # Scores same but sessions mismatch
# view_oa_b(477) # Scores same but sessions mismatch
# view_oa_b(491) # Scores same but sessions mismatch
# view_oa_b(503) # Scores same but sessions mismatch
# view_oa_b(505) # Scores same but sessions mismatch
# view_oa_b(514) # Scores same but sessions mismatch
# view_oa_b(520) # Scores same but sessions mismatch
# view_oa_b(523) # Scores same but sessions mismatch
# view_oa_b(529) # Scores same but sessions mismatch
# view_oa_b(531) # Scores same but sessions mismatch
# view_oa_b(549) # Scores same but sessions mismatch
# view_oa_b(550) # Scores same but sessions mismatch
# view_oa_b(555) # Scores same but sessions mismatch
# view_oa_b(584) # Scores same but sessions mismatch
# view_oa_b(591) # Scores same but sessions mismatch
# view_oa_b(615) # Scores same but sessions mismatch
# view_oa_b(617) # Scores same but sessions mismatch
# view_oa_b(695) # Scores same but sessions mismatch

  # Compare session dates between "oa" and "rr" tables in Set B, although "oa" 
  # was assessed at every time point and "rr" was assessed at fewer time points.
  # The session dates are consistent across tables.

flt_dat_comp_rest_b_oa_sessions <- 
  flt_dat_comp_rest_b$oa[flt_dat_comp_rest_b$oa$participant_id %in% discrep_ids_b,
                         c("participant_id", "date_as_POSIXct", "session_only")]
flt_dat_comp_rest_b_rr_sessions <- 
  flt_dat_comp_rest_b$rr[flt_dat_comp_rest_b$rr$participant_id %in% discrep_ids_b, 
                         c("participant_id", "date_as_POSIXct", "session_only")]

# Use natural join to restrict to shared time points for "rr" table. All scores
# are the same when rounded to 9 decimal places.

merge_rr_b <- merge(flt_dat_comp_rest_b$rr, 
                    sep_dat_comp_rest_b$rr,
                    by = c("participant_id", "session_only"),
                    all = FALSE)

confirm_all_rr_scores_same(merge_rr_b, 9) # TRUE

# Use natural join to restrict to shared time points for "bbsiq" table. All scores
# are the same when rounded to 7 decimal places.

merge_bbsiq_b <- merge(flt_dat_comp_rest_b$bbsiq, 
                       sep_dat_comp_rest_b$bbsiq,
                       by = c("participant_id", "session_only"),
                       all = FALSE)

confirm_all_bbsiq_scores_same(merge_bbsiq_b, 7) # TRUE

# Use natural join to restrict to shared time points for "dass21_as" table. All scores
# are the same.

merge_dass21_as_b <- merge(flt_dat_comp_rest_b$dass21_as, 
                           sep_dat_comp_rest_b$dass_as,
                           by = c("participant_id", "session_only"),
                           all = FALSE)

all(merge_dass21_as_b$dass21_as_total_dbl == merge_dass21_as_b$dass_as_score) # TRUE

# Use natural join to restrict to shared time points for "dass21_ds" table. All scores
# are the same.

merge_dass21_ds_b <- merge(flt_dat_comp_rest_b$dass21_ds, 
                           sep_dat_comp_rest_b$dass_ds,
                           by = c("participant_id", "session_only"),
                           all = FALSE)

all(merge_dass21_ds_b$dass21_ds_total_dbl == merge_dass21_ds_b$dass_ds_score) # TRUE

# Although "imagery_prime" contains "prime" condition, no table in Set B seems to
# indicate CBM condition. Perhaps it could be derived from the "trial_dao" table,
# though, by computing the proportion of positive scenarios (see "positive" column)

# ---------------------------------------------------------------------------- #
# Look for non-demographic data missing for some participants in Set A ----
# ---------------------------------------------------------------------------- #

# 36 participants in clean data are missing from "participant_export_dao" table 
# in Set A. A "notes.csv" file obtained from Sonia Baee on 11/24/2021 lists these 
# participant IDs as "not included in original dataset due to server error." Asked
# Sonia about this on 11/24/21 (no response).

miss_ids <- setdiff(cln_participant_ids, flt_dat$participant_export_dao$participant_id)

setequal(miss_ids, server_error_pids)

# None of the missing participants have data in "dass21_as" or "oa" in Set A, but 
# most of them do have data in other tables. The participants do have "dass21_as" 
# and "oa" data in the raw data in Set B, and the same number of rows of data in
# other tables as in Set A. The participants also seem to have baseline data in 
# in the clean item-level baseline data file "R34_Cronbach.csv".

lapply(flt_dat,   function(x) sum(unique(x$participant_id) %in% miss_ids))
lapply(flt_dat_b, function(x) sum(unique(x$participant_id) %in% miss_ids))

lapply(flt_dat,   function(x) nrow(x[x$participant_id %in% miss_ids, ]))
lapply(flt_dat_b, function(x) nrow(x[x$participant_id %in% miss_ids, ]))

# Set B OASIS sessions match unique sessions for OASIS entries in Set A "task_log"

flt_dat_task_log_oa_miss_ids <- flt_dat$task_log[flt_dat$task_log$participant_id %in% miss_ids &
                                                   flt_dat$task_log$task_name == "OA", ]
flt_dat_task_log_oa_miss_ids <- sort_by_part_then_session(flt_dat_task_log_oa_miss_ids,
                                                          "participant_id", "session_only")
flt_dat_task_log_oa_miss_ids_sessions <- unique(flt_dat_task_log_oa_miss_ids[c("participant_id", "session_only")])
row.names(flt_dat_task_log_oa_miss_ids_sessions) <- 1:nrow(flt_dat_task_log_oa_miss_ids_sessions)

flt_dat_b_oa_miss_ids <- flt_dat_b$oa[flt_dat_b$oa$participant_id %in% miss_ids, ]
flt_dat_b_oa_miss_ids <- sort_by_part_then_session(flt_dat_b_oa_miss_ids,
                                                   "participant_id", "session_only")
flt_dat_b_oa_miss_ids_sessions <- flt_dat_b_oa_miss_ids[c("participant_id", "session_only")]
row.names(flt_dat_b_oa_miss_ids_sessions) <- 1:nrow(flt_dat_b_oa_miss_ids_sessions)

identical(flt_dat_task_log_oa_miss_ids_sessions, flt_dat_b_oa_miss_ids_sessions)

# Set B DASS-21-AS sessions after eligibility screening match sessions for DASS-21-AS 
# entries in Set A "task_log" (which has no DASS-21-AS entries at eligibility screening)

flt_dat_task_log_dass21_as_miss_ids <- flt_dat$task_log[flt_dat$task_log$participant_id %in% miss_ids &
                                                          flt_dat$task_log$task_name == "DASS21_AS", ]
flt_dat_task_log_dass21_as_miss_ids <- sort_by_part_then_session(flt_dat_task_log_dass21_as_miss_ids,
                                                                 "participant_id", "session_only")
flt_dat_task_log_dass21_as_miss_ids_sessions <- flt_dat_task_log_dass21_as_miss_ids[c("participant_id", "session_only")]
row.names(flt_dat_task_log_dass21_as_miss_ids_sessions) <- 1:nrow(flt_dat_task_log_dass21_as_miss_ids_sessions)

all(flt_dat_task_log_dass21_as_miss_ids_sessions$session_only != "Eligibility")

flt_dat_b_dass21_as_miss_ids <- flt_dat_b$dass21_as[flt_dat_b$dass21_as$participant_id %in% miss_ids, ]
flt_dat_b_dass21_as_miss_ids <- sort_by_part_then_session(flt_dat_b_dass21_as_miss_ids,
                                                          "participant_id", "session_only")
flt_dat_b_dass21_as_miss_ids_sessions <- flt_dat_b_dass21_as_miss_ids[c("participant_id", "session_only")]
flt_dat_b_dass21_as_miss_ids_sessions_after_elig <-
  flt_dat_b_dass21_as_miss_ids_sessions[flt_dat_b_dass21_as_miss_ids_sessions$session_only != "Eligibility", ]
row.names(flt_dat_b_dass21_as_miss_ids_sessions_after_elig) <- 1:nrow(flt_dat_b_dass21_as_miss_ids_sessions_after_elig)

identical(flt_dat_task_log_dass21_as_miss_ids_sessions, flt_dat_b_dass21_as_miss_ids_sessions_after_elig)

# The missing participants' baseline data for "oa" items in Set B is the same as 
# that in "R34_Cronbach.csv". This suggests that the data from Set B can be used 
# for "oa" items at other time points for the missing participants.

flt_dat_b_oa_miss_ids_bl <- flt_dat_b_oa_miss_ids[flt_dat_b_oa_miss_ids$session_only == "PRE", ]
row.names(flt_dat_b_oa_miss_ids_bl) <- 1:nrow(flt_dat_b_oa_miss_ids_bl)

sep_dat_bl_oa_miss_ids <- sep_dat_bl$oa[sep_dat_bl$oa$participant_id %in% miss_ids, ]
sep_dat_bl_oa_miss_ids <- sep_dat_bl_oa_miss_ids[order(sep_dat_bl_oa_miss_ids$participant_id), ]
row.names(sep_dat_bl_oa_miss_ids) <- 1:nrow(sep_dat_bl_oa_miss_ids)

identical(flt_dat_b_oa_miss_ids_bl[, c("participant_id", "session_only", oa_items)],
          sep_dat_bl_oa_miss_ids[,   c("participant_id", "session_only", oa_items)])

# Same for baseline "dass21_as" items, except that Sonia's "R34_Cronbach.csv" file
# has data for all 36 missing participants, whereas Set B has data for only 34

flt_dat_b_dass21_as_miss_ids_bl <- 
  flt_dat_b_dass21_as_miss_ids[flt_dat_b_dass21_as_miss_ids$session_only == "Eligibility", ]
row.names(flt_dat_b_dass21_as_miss_ids_bl) <- 1:nrow(flt_dat_b_dass21_as_miss_ids_bl)

sep_dat_bl_dass_as_miss_ids <- sep_dat_bl$dass_as[sep_dat_bl$dass_as$participant_id %in% miss_ids, ]
sep_dat_bl_dass_as_miss_ids <- sep_dat_bl_dass_as_miss_ids[order(sep_dat_bl_dass_as_miss_ids$participant_id), ]
row.names(sep_dat_bl_dass_as_miss_ids) <- 1:nrow(sep_dat_bl_dass_as_miss_ids)

setdiff(flt_dat_b_dass21_as_miss_ids_bl$participant_id, sep_dat_bl_dass_as_miss_ids$participant_id)
(two_more_miss_ids_in_sep_dat_bl <- setdiff(sep_dat_bl_dass_as_miss_ids$participant_id, 
                                            flt_dat_b_dass21_as_miss_ids_bl$participant_id)) == c(1929, 1932)

sep_dat_bl_dass_as_miss_ids_tmp <- 
  sep_dat_bl_dass_as_miss_ids[!(sep_dat_bl_dass_as_miss_ids$participant_id %in% two_more_miss_ids_in_sep_dat_bl), ]
row.names(sep_dat_bl_dass_as_miss_ids_tmp) <- 1:nrow(sep_dat_bl_dass_as_miss_ids_tmp)

identical(flt_dat_b_dass21_as_miss_ids_bl[, c("participant_id", "session_only", dass21_as_items)],
          sep_dat_bl_dass_as_miss_ids_tmp[, c("participant_id", "session_only", dass21_as_items)])

# ---------------------------------------------------------------------------- #
# Add non-demographic data missing for some participants in Set A ----
# ---------------------------------------------------------------------------- #

# Define function for labeling all rows of dataset with name of dataset's source

label_dataset <- function(dat, dataset_name) {
  dat <- lapply(dat, function(x) {
    x$dataset <- dataset_name
    
    return(x)
  })
  
  return(dat)
}

# Run function for Sets A and B and clean datasets

flt_dat    <- label_dataset(flt_dat,    "raw_dat_son_a")
flt_dat_b  <- label_dataset(flt_dat_b,  "raw_dat_son_b")
sep_dat    <- label_dataset(sep_dat,    "dat_main_lg_scales")
sep_dat_bl <- label_dataset(sep_dat_bl, "dat_main_bl_items")

# Add data for participants missing data in Set A due to server error

  # Add Set B OASIS and DASS-21-AS data

flt_dat_add <- flt_dat

flt_dat_add$oa        <- merge(flt_dat_add$oa,
                               flt_dat_b$oa[flt_dat_b$oa$participant_id %in% miss_ids, ],
                               all = TRUE, sort = FALSE)

flt_dat_add$dass21_as <- merge(flt_dat_add$dass21_as,
                               flt_dat_b$dass21_as[flt_dat_b$dass21_as$participant_id %in% miss_ids, ],
                               all = TRUE, sort = FALSE)

  # Add "R34_Cronbach.csv" baseline DASS-21-AS data for 2 additional participants,
  # for whom such data is not in Set B

flt_dat_add$dass21_as <- merge(flt_dat_add$dass21_as,
                               sep_dat_bl$dass_as[sep_dat_bl$dass_as$participant_id %in% 
                                                    two_more_miss_ids_in_sep_dat_bl, ],
                               all = TRUE, sort = FALSE)

  # Add these participants and their condition to "participant" table from clean data

names(flt_dat_add)[names(flt_dat_add) == "participant_export_dao"] <- "participant"

flt_dat_add$participant <- merge(flt_dat_add$participant,
                                 sep_dat$participant[sep_dat$participant$participant_id %in% miss_ids, ],
                                 all = TRUE, sort = FALSE)

# Add "R34_Cronbach.csv" baseline OASIS data for 1 participant (1866) missing OASIS
# data in Sets A and B (participant is already in Set A "participant" table)

flt_dat_add$oa <- merge(flt_dat_add$oa,
                        sep_dat_bl$oa[sep_dat_bl$oa$participant_id == 1866, ],
                        all = TRUE, sort = FALSE)

# ---------------------------------------------------------------------------- #
# Compare clean data and Set A with added data on non-demographic tables ----
# ---------------------------------------------------------------------------- #

# Define lists with corresponding tables

flt_dat_comp_add <- flt_dat_add[c("bbsiq", "dass21_as", "dass21_ds", "demographic", 
                                  "oa", "participant", "rr")]
sep_dat_comp_add <- sep_dat[c("bbsiq", "dass_as", "dass_ds", "demographic", 
                              "oa", "participant", "rr")]

# Using functions defined above for Set A, compare participant IDs for each table.
# Set A tables with added data and clean tables have the same participants.

all(mapply(length_diff_participant_ids, flt_dat_comp_add, sep_dat_comp_add) == 0)
all(mapply(length_diff_participant_ids, sep_dat_comp_add, flt_dat_comp_add) == 0)

# Sort tables by participant and (if present) then session

flt_dat_comp_add <- sort_by_part_then_session(flt_dat_comp_add, "participant_id", "session_only")
sep_dat_comp_add <- sort_by_part_then_session(sep_dat_comp_add, "participant_id", "session_only")

# Compare numbers of observations. They differ between datasets.

set_add_vs_cln_nrow <- data.frame(set_add = sapply(flt_dat_comp_add, nrow),
                                  clean   = sapply(sep_dat_comp_add, nrow))
set_add_vs_cln_nrow$diff <- set_add_vs_cln_nrow$set_add - set_add_vs_cln_nrow$clean

set_add_vs_cln_nrow
  
  # Inspect rows in Set A with added data not in clean data

key_cols <- c("participant_id", "session_only")

# View(diff_df1_not_in_df2(flt_dat_comp_add$bbsiq,     sep_dat_comp_add$bbsiq,   key_cols)) # P 532
# View(diff_df1_not_in_df2(flt_dat_comp_add$dass21_ds, sep_dat_comp_add$dass_ds, key_cols)) # P 532
# View(diff_df1_not_in_df2(flt_dat_comp_add$rr,        sep_dat_comp_add$rr,      key_cols)) # P 532

    # P 532 lacks clean data at "PRE" for BBSIQ, DASS-21-DS, and RR. "notes.csv" says that they were 
    # originally thought not to have RR data at "PRE" but do. Their "PRE" BBSIQ and RR data are the 
    # same as those in clean item-level baseline data. Retain these data.

flt_dat_comp_add_id532_bbsiq_pre <- flt_dat_comp_add$bbsiq[flt_dat_comp_add$bbsiq$participant_id == 532 &
                                                             flt_dat_comp_add$bbsiq$session_only == "PRE", ]
flt_dat_comp_add_id532_rr_pre    <- flt_dat_comp_add$rr[flt_dat_comp_add$rr$participant_id == 532 &
                                                          flt_dat_comp_add$rr$session_only == "PRE", ]

sep_dat_bl_id532_bbsiq <- sep_dat_bl$bbsiq[sep_dat_bl$bbsiq$participant_id == 532, ]
sep_dat_bl_id532_rr    <- sep_dat_bl$rr[sep_dat_bl$rr$participant_id       == 532, ]

all(flt_dat_comp_add_id532_bbsiq_pre[c("participant_id", "session_only", bbsiq_items)] ==
      sep_dat_bl_id532_bbsiq[c("participant_id", "session_only", bbsiq_items)])
all(flt_dat_comp_add_id532_rr_pre[c("participant_id", "session_only", rr_items)] ==
      sep_dat_bl_id532_rr[c("participant_id", "session_only", rr_items)])

  # Inspect rows in clean data not in Set A with added data. In all cases, the
  # clean data contains row(s) for session(s) beyond those available in Sets A
  # or B. Most likely, the clean data was derived from raw data dumped from the
  # server after Sets A and B were dumped from the server.

# View(diff_df1_not_in_df2(sep_dat_comp_add$dass_as, flt_dat_comp_add$dass21_as, key_cols)) # Ps       1687, 1806,       1883,       1910
# View(diff_df1_not_in_df2(sep_dat_comp_add$oa,      flt_dat_comp_add$oa,        key_cols)) # Ps 1273, 1687, 1806, 1866, 1883, 1889, 1910

# Use natural join to restrict to shared time points for "oa" table. All scores 
# are the same, but recode session in section below for participants who skipped 
# one session (see above for Sets A and B).

merge_oa_add <- merge(flt_dat_comp_add$oa, 
                      sep_dat_comp_add$oa,
                      by = c("participant_id", "session_only"),
                      all = FALSE)

all(merge_oa_add$oa_total == merge_oa_add$oasis_score)

# Use natural join to restrict to shared time points for "rr" table. All scores
# are the same when rounded to 9 decimal places.

merge_rr_add <- merge(flt_dat_comp_add$rr, 
                      sep_dat_comp_add$rr,
                      by = c("participant_id", "session_only"),
                      all = FALSE)

confirm_all_rr_scores_same(merge_rr_add, 9) # TRUE

# Use natural join to restrict to shared time points for "bbsiq" table. All scores
# are the same when rounded to 7 decimal places.

merge_bbsiq_add <- merge(flt_dat_comp_add$bbsiq, 
                         sep_dat_comp_add$bbsiq,
                         by = c("participant_id", "session_only"),
                         all = FALSE)

confirm_all_bbsiq_scores_same(merge_bbsiq_add, 7) # TRUE

# Use natural join to restrict to shared time points for "dass21_as" table. All scores
# are the same.

merge_dass21_as_add <- merge(flt_dat_comp_add$dass21_as, 
                             sep_dat_comp_add$dass_as,
                             by = c("participant_id", "session_only"),
                             all = FALSE)

all(merge_dass21_as_add$dass21_as_total_dbl == merge_dass21_as_add$dass_as_score) # TRUE

# Use natural join to restrict to shared time points for "dass21_ds" table. All scores
# are the same.

merge_dass21_ds_add <- merge(flt_dat_comp_add$dass21_ds, 
                             sep_dat_comp_add$dass_ds,
                             by = c("participant_id", "session_only"),
                             all = FALSE)

all(merge_dass21_ds_add$dass21_ds_total_dbl == merge_dass21_ds_add$dass_ds_score) # TRUE

# Compare CBM condition and anxiety imagery prime condition

all(flt_dat_comp_add$participant$cbmCondition == sep_dat_comp_add$participant$cbmCondition)
all(flt_dat_comp_add$participant$prime        == sep_dat_comp_add$participant$prime)

# ---------------------------------------------------------------------------- #
# Compare Sets A and B on credibility table ----
# ---------------------------------------------------------------------------- #

# Credibility table is not in clean data, but at least compare Sets A and B

# After sorting by participant ID, Sets A and B have the same participants

flt_dat$credibility   <- flt_dat$credibility[order(flt_dat$credibility$participant_id), ]
flt_dat_b$credibility <- flt_dat_b$credibility[order(flt_dat_b$credibility$participant_id), ]

row.names(flt_dat$credibility)   <- 1:nrow(flt_dat$credibility)
row.names(flt_dat_b$credibility) <- 1:nrow(flt_dat_b$credibility)

flt_dat$credibility$participant_id   <- as.integer(flt_dat$credibility$participant_id)
flt_dat_b$credibility$participant_id <- as.integer(flt_dat_b$credibility$participant_id)

# Sets A and B are identical on comparable columns

flt_dat$credibility <- flt_dat$credibility[c(names(flt_dat_b$credibility), "id")]

comparable_cols <- names(flt_dat$credibility)[!(names(flt_dat$credibility) %in% c("dataset", "id"))]

identical(flt_dat$credibility[comparable_cols], flt_dat_b$credibility[comparable_cols])

# ---------------------------------------------------------------------------- #
# Finalize item-level data ----
# ---------------------------------------------------------------------------- #

# TODO (update comment): Given that so far the OA, RR, BBSIQ, DASS-21-AS,
# and DASS-21-DS scores in the clean data used in the main outcomes paper have been 
# reproduced, restrict to those tables and to the participant and demographics tables. 
# All these tables have the same participants as the clean data, but some have different 
# numbers of rows (see section above comparing clean data and Set A with added data).

flt_dat_final <- flt_dat_comp_add





# Recode OASIS sessions to be consecutive for participants whose values skip one
# session (i.e., Session 1 or 3) because this is more plausible than the original 
# session values in Set A and in the clean data used for the main outcomes paper 
# (see sections above comparing clean data with Sets A and B)

flt_dat_final$oa <- recode_oa_session_to_expected_order(flt_dat_final$oa, skipped_only_1_oa_session_set_a_pids)

# TODO: Rename or remove scores computed via the methods of main outcomes paper
# because different scores should be used instead





# ---------------------------------------------------------------------------- #
# Extract relevant tables named to reflect their source ----
# ---------------------------------------------------------------------------- #

# TODO: Update this section to use final data and name more clearly





# merge_oa_add
# merge_rr_add
# merge_bbsiq_add

participant_raw_add <- flt_dat_comp_add$participant
participant_cln_add <- sep_dat_comp_add$participant

demographics_raw_add <- flt_dat_comp_add$demographic
demographics_cln_add <- sep_dat_comp_add$demographic

# TODO: Add credibility data here from above





# ---------------------------------------------------------------------------- #
# Export data ----
# ---------------------------------------------------------------------------- #

# TODO: Update below





dir.create("./data/intermediate/")

# Export merged, restricted item-level OA, RR, and BBSIQ data

write.csv(merge_oa_rest_add,    file = "./data/intermediate/merge_oa_rest_add.csv",    row.names = FALSE)
write.csv(merge_rr_rest_add,    file = "./data/intermediate/merge_rr_rest_add.csv",    row.names = FALSE)
write.csv(merge_bbsiq_rest_add, file = "./data/intermediate/merge_bbsiq_rest_add.csv", row.names = FALSE)

# Export raw restricted participant data (contains more columns than clean data)

write.csv(participant_raw_rest_add, file = "./data/intermediate/participant_raw_rest_add.csv", row.names = FALSE)

# Export clean restricted demographics data (cleaner than raw data)

write.csv(demographics_cln_rest_add, file = "./data/intermediate/demographics_cln_rest_add.csv", row.names = FALSE)

# Export raw restricted credibility data (not available in clean data)

write.csv(credibility_raw_rest, file = "./data/intermediate/credibility_raw_rest.csv", row.names = FALSE)