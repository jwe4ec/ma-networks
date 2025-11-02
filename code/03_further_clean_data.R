# ---------------------------------------------------------------------------- #
# Further clean data
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

# Intermediate clean data

int_cln_dat <- readRDS(file.path(processed_path, "int_cln_dat.rds"))

# ---------------------------------------------------------------------------- #
# Define scale items ----
# ---------------------------------------------------------------------------- #

# OASIS

oa_items <- c("anxious_freq", "anxious_sev", "avoid", "interfere", "interfere_social")

stopifnot(
  length(oa_items) == 5,
  all(oa_items %in% names(int_cln_dat$oa))
)

# RR

rr_neg_thr_items <- names(int_cln_dat$rr)[grep("_NS$", names(int_cln_dat$rr))]
rr_neg_non_items <- names(int_cln_dat$rr)[grep("_NF$", names(int_cln_dat$rr))]
rr_pos_thr_items <- names(int_cln_dat$rr)[grep("_PS$", names(int_cln_dat$rr))]
rr_pos_non_items <- names(int_cln_dat$rr)[grep("_PF$", names(int_cln_dat$rr))]

rr_items <- c(rr_neg_thr_items, rr_neg_non_items, rr_pos_thr_items, rr_pos_non_items)

stopifnot(
  all(sapply(list(rr_neg_thr_items, rr_neg_non_items, rr_pos_thr_items, rr_pos_non_items), length) == 9),
  length(rr_items) == 36,
  all(rr_items %in% names(int_cln_dat$rr))
)

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

stopifnot(
  all(sapply(list(bbsiq_neg_int_items, bbsiq_neg_ext_items), length) == 7),
  length(bbsiq_neg_items) == 14,
  
  all(sapply(list(bbsiq_ben_int_items, bbsiq_ben_ext_items), length) == 14),
  length(bbsiq_ben_items) == 28,
  
  all(bbsiq_items %in% names(int_cln_dat$bbsiq))
)

# Credibility
# - Note: In centralized data cleaning ("TeachmanLab/MT-Data-ManagingAnxietyStudy-Cleaning" 
#   repo), values were recoded to 1:5 (the displayed options; in contrast to present prereg)

cred_items <- c("important", "logical", "recommendable")

cred_conf_items <- c("logical", "recommendable")

stopifnot(
  length(cred_items)      == 3,
  length(cred_conf_items) == 2,
  
  all(cred_items %in% names(int_cln_dat$credibility))
)

# Collect items in list

items <- list(oa            = oa_items, 
              rr            = rr_items, 
              rr_neg_thr    = rr_neg_thr_items, 
              rr_neg_non    = rr_neg_non_items, 
              rr_pos_thr    = rr_pos_thr_items, 
              rr_pos_non    = rr_pos_non_items, 
              bbsiq         = bbsiq_items, 
              bbsiq_neg     = bbsiq_neg_items, 
              bbsiq_neg_int = bbsiq_neg_int_items, 
              bbsiq_neg_ext = bbsiq_neg_ext_items, 
              bbsiq_ben     = bbsiq_ben_items, 
              bbsiq_ben_int = bbsiq_ben_int_items, 
              bbsiq_ben_ext = bbsiq_ben_ext_items,
              cred          = cred_items,
              cred_conf     = cred_conf_items)

# ---------------------------------------------------------------------------- #
# Compute average item scores for RR, BBSIQ, and training confidence ----
# ---------------------------------------------------------------------------- #

cln_dat <- int_cln_dat

# RR

cln_dat$rr$rr_neg_thr_mean <- rowMeans(cln_dat$rr[items$rr_neg_thr], na.rm = TRUE)
cln_dat$rr$rr_pos_thr_mean <- rowMeans(cln_dat$rr[items$rr_pos_thr], na.rm = TRUE)

## Reverse positive threat bias scale to reflect lack of positive threat bias

cln_dat$rr$rr_pos_thr_mean_rev <- 3 - cln_dat$rr$rr_pos_thr_mean

# BBSIQ

cln_dat$bbsiq$bbsiq_neg_mean <- rowMeans(cln_dat$bbsiq[items$bbsiq_neg], na.rm = TRUE)

# Training confidence

cln_dat$credibility$cred_conf_mean <- rowMeans(cln_dat$credibility[items$cred_conf], na.rm = TRUE)

# ---------------------------------------------------------------------------- #
# Further clean demographics ----
# ---------------------------------------------------------------------------- #

# Temporarily extract data (reassign below)

dem_dat <- cln_dat$demographic

# Demographics are available for all 807 ITT participants

stopifnot(nrow(dem_dat) == 807)

# Age

stopifnot(
  # Range is from 18 to 91, which is reasonable
  
  range(dem_dat$age, na.rm = TRUE) == c(18, 91),
  
  # Per "TeachmanLab/MT-Data-ManagingAnxietyStudy-Cleaning" repo, weird "birthYear" 
  # values (0 or 2222) were recoded to NA
  
  sum(is.na(dem_dat$age)) == 2,
  
  # All NAs in "age" are due to "birthYear" of NA
  
  all(is.na(dem_dat$birthYear[is.na(dem_dat$age)]))
)

# Gender

dem_dat$gender <- factor(dem_dat$gender,
  levels = c("Female", "Male", "Transgender", "Other", "Prefer not to answer", 
             "Missing (server issue)"))

# Education

dem_dat$education <- factor(dem_dat$education,
  levels = c("Elementary School", "Junior High", "Some High School", "High School Graduate", 
             "Some College", "Associate's Degree", "Bachelor's Degree", "Some Graduate School", 
             "Master's Degree", "M.B.A.", "J.D.", "M.D.", "Ph.D.", "Other Advanced Degree", 
             "Prefer not to answer", "Missing (server issue)"))

# Ethnicity

dem_dat$ethnicity <- factor(dem_dat$ethnicity,
  levels = c("Hispanic or Latino", "Not Hispanic or Latino", "Unknown", 
             "Prefer not to answer", "Missing (server issue)"))

# Employment status

homemaker <- "Homemaker/keeping house or raising children full-time"
dem_dat$employmentStatus[dem_dat$employmentStatus == homemaker] <- "Homemaker"

dem_dat$employmentStatus <- factor(dem_dat$employmentStatus,
  levels = c("Student", "Homemaker", "Unemployed or laid off", "Looking for work",
             "Working part-time", "Working full-time", "Retired", "Other",
             "Prefer not to answer", "Missing (server issue)"))

# Income

dem_dat$income[dem_dat$income == "Don't know"] <- "Unknown"

dem_dat$income <- factor(dem_dat$income,
  levels = c("Less than $5,000", "$5,000 through $11,999", "$12,000 through $15,999", 
             "$16,000 through $24,999", "$25,000 through $34,999", "$35,000 through $49,999",
             "$50,000 through $74,999", "$75,000 through $99,999", "$100,000 through $149,999", 
             "$150,000 through $199,999", "$200,000 through $249,999", "$250,000 or greater",
             "Unknown", "Prefer not to answer", "Missing (server issue)"))

# Marital status

civil_union   <- "In a domestic or civil union"
dating        <- "Single, but casually dating"
engaged       <- "Single, but currently engaged to be married"
marriage_like <- "Single, but currently living with someone in a marriage-like relationship"

dem_dat$maritalStatus[dem_dat$maritalStatus == civil_union]   <- "In domestic or civil union"
dem_dat$maritalStatus[dem_dat$maritalStatus == dating]        <- "Dating"
dem_dat$maritalStatus[dem_dat$maritalStatus == engaged]       <- "Engaged"
dem_dat$maritalStatus[dem_dat$maritalStatus == marriage_like] <- "In marriage-like relationship"

dem_dat$maritalStatus <- factor(dem_dat$maritalStatus,
  levels = c("Single", "Dating", "Engaged", "In marriage-like relationship",
             "Married", "In domestic or civil union", "Separated", "Divorced", 
             "Widow/widower", "Other", "Prefer not to answer", "Missing (server issue)"))

# Race

dem_dat$race <- factor(dem_dat$race,
  levels = c("American Indian/Alaska Native", "Black/African origin", "East Asian",
             "Native Hawaiian/Pacific Islander", "South Asian", "White/European origin", 
             "Other or Unknown", "Prefer not to answer", "Missing (server issue)"))

# Country

pna <- "Prefer not to answer"
dem_dat$residenceCountry[dem_dat$residenceCountry == "NoAnswer"] <- pna

server_issue <- "Missing (server issue)"

## Define desired levels order (decreasing frequency ending with "Prefer not to answer"
## and "Missing (server issue)")

country_levels <- names(sort(table(dem_dat$residenceCountry), decreasing = TRUE))
end_levels     <- c(pna, server_issue)
country_levels <- c(setdiff(country_levels, end_levels), end_levels)

## Reorder levels

dem_dat$residenceCountry <- factor(dem_dat$residenceCountry, levels = country_levels)

## Define "country_col", collapsing countries with fewer than 10 participants into "Other"

top_countries <- names(table(dem_dat$residenceCountry))[table(dem_dat$residenceCountry) > 10]

stopifnot(top_countries == c("United States", "Canada", "United Kingdom"))

for (i in 1:nrow(dem_dat)) {
  if (is.na(dem_dat$residenceCountry[i])) {
    dem_dat$residenceCountry_col[i] <- NA
  } else if (as.character(dem_dat$residenceCountry)[i] %in% top_countries) {
    dem_dat$residenceCountry_col[i] <- as.character(dem_dat$residenceCountry)[i]
  } else if (as.character(dem_dat$residenceCountry)[i] == pna) {
    dem_dat$residenceCountry_col[i] <- pna
  } else if (as.character(dem_dat$residenceCountry)[i] == server_issue) {
    dem_dat$residenceCountry_col[i] <- server_issue
  } else {
    dem_dat$residenceCountry_col[i] <- "Other"
  }
}

## Reorder levels of "country_col"

dem_dat$residenceCountry_col <- factor(dem_dat$residenceCountry_col,
  levels = c(top_countries, "Other", pna, server_issue))

# Identify participants missing data due to server issue

get_pids_server_issue <- function(dem_dat, col) {
  dem_dat$participant_id[dem_dat[[col]] == server_issue]
}

stopifnot(
  get_pids_server_issue(dem_dat, "gender")           == 430,
  get_pids_server_issue(dem_dat, "education")        == c(430,      782),
  get_pids_server_issue(dem_dat, "ethnicity")        == c(430,      782, 792),
  get_pids_server_issue(dem_dat, "employmentStatus") == c(430, 555, 782),
  get_pids_server_issue(dem_dat, "income")           ==             782,
  get_pids_server_issue(dem_dat, "maritalStatus")    == c(430,      782,     900),
  get_pids_server_issue(dem_dat, "race")             == c(430,      782),
  get_pids_server_issue(dem_dat, "residenceCountry") ==             782
)

# Reassign data to list

cln_dat$demographic <- dem_dat

# ---------------------------------------------------------------------------- #
# Order CBM-I condition levels ----
# ---------------------------------------------------------------------------- #

cln_dat$participant$cbmCondition <- factor(cln_dat$participant$cbmCondition,
  levels = c("POSITIVE", "FIFTY_FIFTY", "NEUTRAL"))

stopifnot(all(!is.na(cln_dat$participant$cbmCondition)))

# ---------------------------------------------------------------------------- #
# Export data and helper items list ----
# ---------------------------------------------------------------------------- #

saveRDS(cln_dat, file.path(processed_path, "cln_dat.rds"))

helper_path <- file.path("data", "helper")
dir.create(helper_path)

saveRDS(items, file.path(helper_path, "items.rds"))