# ---------------------------------------------------------------------------- #
# Further Clean Demographic Data and Create Tables
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

pkgs <- c("flextable", "ftExtra", "officer")
groundhog.library(pkgs, groundhog_day)

# Set "flextable" package defaults

source(file.path("code", "01b_set_flextable_defaults.R"))

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

processed_path <- file.path("data", "processed")

# Clean data

cln_dat <- readRDS(file.path(processed_path, "cln_dat.rds"))

# Network data

net_dat_rr_all_to_s6_wide <- readRDS(file.path(processed_path, "net_dat_rr_all_to_s6_wide.rds"))
net_dat_bb_all_to_s6_wide <- readRDS(file.path(processed_path, "net_dat_bb_all_to_s6_wide.rds"))

# ---------------------------------------------------------------------------- #
# Extract demographics table ----
# ---------------------------------------------------------------------------- #

dem_dat <- cln_dat$demographic

# Demographics are available for all 807 ITT participants

stopifnot(nrow(dem_dat) == 807)

# ---------------------------------------------------------------------------- #
# Clean age ----
# ---------------------------------------------------------------------------- #

stopifnot(
  # Range is from 18 to 91, which is reasonable
  
  range(dem_dat$age, na.rm = TRUE) == c(18, 91),

  # Per "TeachmanLab/MT-Data-ManagingAnxietyStudy-Cleaning" repo, weird "birthYear" 
  # values (0 or 2222) were recoded to NA

  sum(is.na(dem_dat$age)) == 2,

  # All NAs in "age" are due to "birthYear" of NA

  all(is.na(dem_dat$birthYear[is.na(dem_dat$age)]))
)

# ---------------------------------------------------------------------------- #
# Clean gender ----
# ---------------------------------------------------------------------------- #

dem_dat$gender <- factor(dem_dat$gender,
  levels = c("Female", "Male", "Transgender", "Other", "Prefer not to answer", 
             "Missing (server issue)"))

# 1 participant is missing gender due to server issue

server_issue <- "Missing (server issue)"

stopifnot(dem_dat$participant_id[dem_dat$gender == server_issue] == 430)

# ---------------------------------------------------------------------------- #
# Clean education ----
# ---------------------------------------------------------------------------- #

dem_dat$education <- factor(dem_dat$education,
  levels = c("Elementary School", "Junior High", "Some High School", "High School Graduate", 
             "Some College", "Associate's Degree", "Bachelor's Degree", "Some Graduate School", 
             "Master's Degree", "M.B.A.", "J.D.", "M.D.", "Ph.D.", "Other Advanced Degree", 
             "Prefer not to answer", "Missing (server issue)"))

# 2 participants are missing education due to a server issue

stopifnot(dem_dat$participant_id[dem_dat$education == server_issue] == c(430, 782))

# ---------------------------------------------------------------------------- #
# Clean ethnicity ----
# ---------------------------------------------------------------------------- #

dem_dat$ethnicity <- factor(dem_dat$ethnicity,
  levels = c("Hispanic or Latino", "Not Hispanic or Latino", "Unknown", 
             "Prefer not to answer", "Missing (server issue)"))

# 3 participants are missing ethnicity due to a server issue

stopifnot(dem_dat$participant_id[dem_dat$ethnicity == server_issue] == c(430, 782, 792))

# ---------------------------------------------------------------------------- #
# Clean employment status ----
# ---------------------------------------------------------------------------- #

homemaker <- "Homemaker/keeping house or raising children full-time"
dem_dat$employmentStatus[dem_dat$employmentStatus == homemaker] <- "Homemaker"

dem_dat$employmentStatus <- factor(dem_dat$employmentStatus,
  levels = c("Student", "Homemaker", "Unemployed or laid off", "Looking for work",
             "Working part-time", "Working full-time", "Retired", "Other",
             "Prefer not to answer", "Missing (server issue)"))

# 3 participants are missing employment status due to a server issue

stopifnot(dem_dat$participant_id[dem_dat$employmentStatus == server_issue] == c(430, 555, 782))

# ---------------------------------------------------------------------------- #
# Clean income ----
# ---------------------------------------------------------------------------- #

dem_dat$income[dem_dat$income == "Don't know"] <- "Unknown"

dem_dat$income <- factor(dem_dat$income,
  levels = c("Less than $5,000", "$5,000 through $11,999", "$12,000 through $15,999", 
             "$16,000 through $24,999", "$25,000 through $34,999", "$35,000 through $49,999",
             "$50,000 through $74,999", "$75,000 through $99,999", "$100,000 through $149,999", 
             "$150,000 through $199,999", "$200,000 through $249,999", "$250,000 or greater",
             "Unknown", "Prefer not to answer", "Missing (server issue)"))

# 1 participant is missing income due to a server issue

stopifnot(dem_dat$participant_id[dem_dat$income == server_issue] == 782)

# ---------------------------------------------------------------------------- #
# Clean marital status ----
# ---------------------------------------------------------------------------- #

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

# 3 participants are missing marital status due to a server issue

stopifnot(dem_dat$participant_id[dem_dat$maritalStatus == server_issue] == c(430, 782, 900))

# ---------------------------------------------------------------------------- #
# Clean race ----
# ---------------------------------------------------------------------------- #

dem_dat$race <- factor(dem_dat$race,
  levels = c("American Indian/Alaska Native", "Black/African origin", "East Asian",
             "Native Hawaiian/Pacific Islander", "South Asian", "White/European origin", 
             "Other or Unknown", "Prefer not to answer", "Missing (server issue)"))

# 2 participants are missing race due to a server issue

stopifnot(dem_dat$participant_id[dem_dat$race == server_issue] == c(430, 782))

# ---------------------------------------------------------------------------- #
# Clean country ----
# ---------------------------------------------------------------------------- #

pna <- "Prefer not to answer"
dem_dat$residenceCountry[dem_dat$residenceCountry == "NoAnswer"] <- pna

# 1 participant is missing country due to a server issue

stopifnot(dem_dat$participant_id[dem_dat$residenceCountry == server_issue] == 782)

# Define desired levels order (decreasing frequency ending with "Prefer not to answer"
# and "Missing (server issue)")

country_levels <- names(sort(table(dem_dat$residenceCountry), decreasing = TRUE))
end_levels     <- c(pna, server_issue)
country_levels <- c(setdiff(country_levels, end_levels), end_levels)

# Reorder levels

dem_dat$residenceCountry <- factor(dem_dat$residenceCountry, levels = country_levels)

# Define "country_col", collapsing countries with fewer than 10 participants into "Other"

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

# Reorder levels of "country_col"

dem_dat$residenceCountry_col <- factor(dem_dat$residenceCountry_col,
  levels = c(top_countries, "Other", pna, server_issue))

# ---------------------------------------------------------------------------- #
# Create demographics tables ----
# ---------------------------------------------------------------------------- #

# Create two demographics tables: (a) one for all ITT participants and (b) one for 
# participants with complete data across target waves (baseline, Session 3, Session 
# 6) in the RR or BBSIQ network datasets

dem_tbl_itt <- dem_dat

# Add condition columns

dem_tbl_itt <- merge(dem_tbl_itt,
                     cln_dat$participant[c("participant_id", "cbmCondition", "prime")], 
                     "participant_id", all.x = TRUE)

# Order CBM-I condition levels

dem_tbl_itt$cbmCondition <- factor(dem_tbl_itt$cbmCondition,
                                   levels = c("POSITIVE", "FIFTY_FIFTY", "NEUTRAL"))

# Create table for 112 participants with complete data across target waves in any network dataset

complete_col_rr_net <- net_dat_rr_all_to_s6_wide[c("participant_id", "complete_bl_s3_s6")]
complete_col_bb_net <- net_dat_bb_all_to_s6_wide[c("participant_id", "complete_bl_s3_s6")]

names(complete_col_rr_net)[names(complete_col_rr_net) == "complete_bl_s3_s6"] <- "complete_bl_s3_s6_rr_net"
names(complete_col_bb_net)[names(complete_col_bb_net) == "complete_bl_s3_s6"] <- "complete_bl_s3_s6_bb_net"

dem_tbl_itt <- merge(dem_tbl_itt, complete_col_rr_net, "participant_id", all.x = TRUE)
dem_tbl_itt <- merge(dem_tbl_itt, complete_col_bb_net, "participant_id", all.x = TRUE)

dem_tbl_itt$complete_bl_s3_s6_any_net <- as.integer((!is.na(dem_tbl_itt$complete_bl_s3_s6_rr_net) & 
                                                       dem_tbl_itt$complete_bl_s3_s6_rr_net == 1) |
                                                       dem_tbl_itt$complete_bl_s3_s6_bb_net == 1)

dem_tbl_complete_bl_s3_s6_any_net <- dem_tbl_itt[dem_tbl_itt$complete_bl_s3_s6_any_net == 1, ]

stopifnot(nrow(dem_tbl_complete_bl_s3_s6_any_net) == 112)

# Define function to compute descriptives

compute_desc <- function(df) {
  # Compute sample size
  
  n <- data.frame(label = "n",
                  value = length(df$participant_id))
  
  # Compute mean and standard deviation for numeric variables
  
  num_res <- rbind(data.frame(label = "Age",
                              value = NA),
                   data.frame(label = "Years: M (SD)",
                              value = paste0(format(round(mean(df$age, na.rm = TRUE), 2),
                                                    nsmall = 2, trim = TRUE), 
                                             " (",
                                             format(round(sd(df$age, na.rm = TRUE), 2),
                                                    nsmall = 2, trim = TRUE),
                                             ")")))
  
  # Compute count and percentage "missing" for numeric variables
  
  num_res_na <- data.frame(label = "Missing: n (%)",
                           value = paste0(sum(is.na(df$age)),
                                          " (",
                                          format(round((sum(is.na(df$age)) / length(df$age)) * 100, 1),
                                                 nsmall = 1, trim = TRUE),
                                          ")"))
  
  # Compute count and percentage for factor variables
  
  vars <- c("gender", "race", "ethnicity", "residenceCountry_col", "education",
            "employmentStatus", "income", "maritalStatus")
  var_labels <- paste0(c("Gender", "Race", "Ethnicity", "Country", "Education",
                         "Employment Status", "Annual Income", "Marital Status"),
                       ": n (%)")
  
  fct_res <- data.frame()
  
  for (i in 1:length(vars)) {
    var       <- vars[i]
    var_label <- var_labels[i]
    
    tbl <- table(df[[var]])
    perc_tbl <- prop.table(tbl) * 100
    
    tbl_res <- rbind(data.frame(label = var_label,
                                value = NA),
                     data.frame(label = names(tbl),
                                value = paste0(as.numeric(tbl),
                                               " (", 
                                               format(round(as.numeric(perc_tbl), 1),
                                                      nsmall = 1, trim = TRUE),
                                               ")")))
    fct_res <- rbind(fct_res, tbl_res)
  }
  
  # Combine results
  
  res <- rbind(n, num_res, num_res_na, fct_res)
  
  return(res)
}

# Define function to compute descriptives by condition

compute_desc_by_cond <- function(df) {
  conditions <- levels(droplevels(df$cbmCondition))
  
  for (i in 1:length(conditions)) {
    condition <- conditions[i]
    
    df_cond <- df[df$cbmCondition == condition, ]
    
    cond_res <- compute_desc(df_cond)
    names(cond_res)[names(cond_res) == "value"] <- condition
    
    if (i == 1) {
      res_by_cond <- cond_res
    } else if (i > 1) {
      cond_res$label <- NULL
      
      res_by_cond <- cbind(res_by_cond, cond_res)
    }
  }
  
  return(res_by_cond)
}

# Compute descriptives across conditions for ITT sample

res_itt_across_cond <- compute_desc(dem_tbl_itt)

# Compute descriptives by condition for the ITT and completer samples

res_itt_by_cond <- compute_desc_by_cond(dem_tbl_itt)
res_complete_bl_s3_s6_any_net_by_cond <- compute_desc_by_cond(dem_tbl_complete_bl_s3_s6_any_net)

# Save tables to CSV

dem_path <- file.path("results", "demographics")
dir.create(dem_path)

write.csv(res_itt_across_cond,
          file.path(dem_path, "itt_across_cond.csv"), row.names = FALSE)
write.csv(res_itt_by_cond,
          file.path(dem_path, "itt_by_cond.csv"), row.names = FALSE)
write.csv(res_complete_bl_s3_s6_any_net_by_cond, 
          file.path(dem_path, "complete_bl_s3_s6_any_net_by_cond.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------- #
# Format demographics tables ----
# ---------------------------------------------------------------------------- #

# "flextable" defaults are set in "set_flextable_defaults.R" above

# Define function to format demographics tables

format_dem_tbl <- function(dem_tbl, gen_note, footnotes, title, sample = NULL) {
  # Format "label" column using Markdown
  
  dem_tbl$label_md <- dem_tbl$label
  
  rows_no_indent <- dem_tbl$label_md == "n" | 
    grepl("\\b(Age|Gender|Race|Ethnicity|Country|Education|Employment|Annual|Marital)\\b",
          dem_tbl$label_md)
  rows_indent <- !rows_no_indent
  
  indent_spaces <- "\\ \\ \\ \\ \\ "
  
  dem_tbl$label_md[rows_indent] <- paste0(indent_spaces, dem_tbl$label_md[rows_indent])
  
  dem_tbl$label_md[dem_tbl$label_md == "n"] <- "*n*"
  dem_tbl$label_md <- gsub("n \\(%\\)", "*n* \\(%\\)", dem_tbl$label_md)
  
  dem_tbl$label_md <- gsub("M \\(SD\\)", "*M* \\(*SD*\\)", dem_tbl$label_md)
  
  dem_tbl <- dem_tbl[c("label_md", names(dem_tbl)[names(dem_tbl) != "label_md"])]
  
  # Identify rows for footnotes
  
  age_missing_row_idx <- 4
  if (dem_tbl$label[age_missing_row_idx] != "Missing: n (%)") {
    stop("Row index for 'age' value of 'NA' is incorrect. Update 'age_missing_row_idx'.")
  }
  
  country_other_row_idx <- 32
  if (dem_tbl$label[country_other_row_idx] != "Other") {
    stop("Row index for 'country' value of 'Other' is incorrect. Update 'country_other_row_idx'.")
  }
  
  # Define columns
  
  left_align_body_cols <- "label_md"
  target_cols <- setdiff(names(dem_tbl), "label")
  
  # Create flextable
  
  dem_tbl_ft <- flextable(dem_tbl[target_cols]) |>
    set_table_properties(align = "left") |>
    
    set_caption(as_paragraph(as_i(title)), word_stylename = "heading 1",
                fp_p = fp_par(padding.left = 0, padding.right = 0),
                align_with_table = FALSE) |>
    
    align(align = "center", part = "header") |>
    align(align = "center", part = "body") |>
    align(j = left_align_body_cols, align = "left", part = "body") |>
    align(align = "left", part = "footer") |>
    
    valign(valign = "bottom", part = "header") |>
    
    set_header_labels(label_md    = "Characteristic",
                      POSITIVE    = "Positive CBM-I",
                      FIFTY_FIFTY = "50-50 CBM-I",
                      NEUTRAL     = "No-Training") |>

    colformat_md(j = "label_md", part = "body") |>
    
    add_footer_lines(gen_note)
  
  if (!is.null(sample) && sample == "itt") {
    dem_tbl_ft <- dem_tbl_ft |>
      footnote(i = age_missing_row_idx,
               j = 1,
               value = as_paragraph_md(footnotes$age_missing),
               ref_symbols = footnotes$age_missing_ref_symbol,
               part = "body")
  }
  
  dem_tbl_ft <- dem_tbl_ft |>
    footnote(i = country_other_row_idx,
             j = 1,
             value = as_paragraph_md(footnotes$country_other),
             ref_symbols = footnotes$country_other_ref_symbol,
             part = "body") |>
    
    autofit()
}

# Define notes

gen_note_itt <- as_paragraph_md("*Note.* CBM-I = cognitive bias modification for interpretation.")

gen_note_complete_bl_s3_s6_any_net <- as_paragraph_md(
  "*Note.* Characteristics are shown for participants with complete data at baseline, Session 3, and
  Session 6 for the Overall Anxiety Severity and Impairment Scale and for Recognition Ratings or the
  Brief Body Sensations Interpretations Questionnaire. CBM-I = cognitive bias modification for interpretation.")

country_other <- "\\ Countries with fewer than 10 participants were collapsed into Other."

footnotes_itt <- list(age_missing   = paste0("\\ Per Ji et al. (2021), ages were treated as missing for 
                                             participants whose birth years were reported as 0 or 2222."),
                      age_missing_ref_symbol = " a",
                      country_other = country_other,
                      country_other_ref_symbol = " b")

footnotes_complete_bl_s3_s6_any_net <- list(country_other = country_other,
                                            country_other_ref_symbol = " a")

# Run function

dem_tbl_itt_by_cond_ft <- 
  format_dem_tbl(res_itt_by_cond, 
                 gen_note_itt, 
                 footnotes_itt,
                 "Demographic Characteristics by Treatment Condition for Intent-To-Treat Sample",
                 "itt")

dem_tbl_complete_bl_s3_s6_any_net_by_cond_ft <-
  format_dem_tbl(res_complete_bl_s3_s6_any_net_by_cond, 
                 gen_note_complete_bl_s3_s6_any_net, 
                 footnotes_complete_bl_s3_s6_any_net,
                 "Demographic Characteristics by Treatment Condition for Completer Sample")

# ---------------------------------------------------------------------------- #
# Save flextables ----
# ---------------------------------------------------------------------------- #

saveRDS(dem_tbl_itt_by_cond_ft,
        file.path(dem_path, "dem_tbl_itt_by_cond_ft.RData"))
saveRDS(dem_tbl_complete_bl_s3_s6_any_net_by_cond_ft,
        file.path(dem_path, "dem_tbl_complete_bl_s3_s6_any_net_by_cond_ft.RData"))