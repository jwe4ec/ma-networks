# ---------------------------------------------------------------------------- #
# Compute Raw Means and Standard Deviations Over Time
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
# Import data and helper nodes list ----
# ---------------------------------------------------------------------------- #

processed_path <- file.path("data", "processed")

# Specific and overall long-format network datasets

net_dat_rr_all_to_s6 <- readRDS(file.path(processed_path, "net_dat_rr_all_to_s6.rds"))
net_dat_bb_all_to_s6 <- readRDS(file.path(processed_path, "net_dat_bb_all_to_s6.rds"))

net_dat_all_to_s6 <- readRDS(file.path(processed_path, "net_dat_all_to_s6.rds"))

# Helper nodes list

nodes <- readRDS(file.path("data", "helper", "nodes.rds"))

# ---------------------------------------------------------------------------- #
# Prepare data ----
# ---------------------------------------------------------------------------- #

# Make "session_only" a factor (for correct order when aggregating below)

ordered_levs <- c("PRE", paste0("SESSION", 1:6))

net_dat_rr_all_to_s6$session_only <- factor(net_dat_rr_all_to_s6$session_only, levels = ordered_levs)
net_dat_bb_all_to_s6$session_only <- factor(net_dat_bb_all_to_s6$session_only, levels = ordered_levs)
net_dat_all_to_s6$session_only    <- factor(net_dat_all_to_s6$session_only,    levels = ordered_levs)

# For overall dataset, restrict to ITT participants in RR or BBSIQ datasets

stopifnot(all(net_dat_all_to_s6$itt_any_net == 1))
net_dat_all_to_s6_itt <- net_dat_all_to_s6

# For specific datasets, restrict to completers

net_dat_rr_all_to_s6_compl <- net_dat_rr_all_to_s6[net_dat_rr_all_to_s6$complete_bl_s3_s6 == 1, ]
net_dat_bb_all_to_s6_compl <- net_dat_bb_all_to_s6[net_dat_bb_all_to_s6$complete_bl_s3_s6 == 1, ]

# ---------------------------------------------------------------------------- #
# Compute n, M, and SD for each raw node variable by condition over time ----
# ---------------------------------------------------------------------------- #

# Define function for computing n, M, and SD for each node variable over time

compute_desc_node_vars <- function(df, node_vars) {
  for (k in 1:length(node_vars)) {
    node_var <- node_vars[k]
    
    fml <- as.formula(paste0(node_var, "~ session_only"))
    
    ag  <- aggregate(fml, data = df, FUN = function(x) {
      n  <- length(x)
      m  <- format(round(mean(x), 2), nsmall = 2, trim = TRUE)
      sd <- format(round(sd(x),   2), nsmall = 2, trim = TRUE)
                       
      m_sd <- paste0(m, " (", sd, ")")
                       
      c(n = n, m_sd = m_sd)
    })
    
    ag <- do.call(data.frame, ag)
    
    node_var_output <- cbind(node_var, ag)
    names(node_var_output) <- c("Measure", "Assessment", "n", "m_sd")
    
    if (k == 1) {
      output <- node_var_output
    } else if (k > 1) {
      output <- rbind(output, node_var_output)
    } 
  }
  
  return(output)
}

# Define function for computing n, M, and SD for each score by condition over time

compute_desc_node_vars_by_cond <- function(df, node_vars, ordered_levs) {
  conditions <- levels(droplevels(df$cbmCondition))
  
  for (i in 1:length(conditions)) {
    condition <- conditions[i]
    
    df_cond <- df[df$cbmCondition == condition, ]
    
    # Compute descriptives for each node variable over time
    
    cond_res <- compute_desc_node_vars(df_cond, node_vars)
    
    # Create empty data frame with all node variables at all time points to store output 
    # (given that not all conditions will have observations at all time points)
    
    out <- data.frame(Measure    = rep(node_vars, each = length(ordered_levs)),
                      Assessment = rep(ordered_levs, length(node_vars)))
    
    # Format condition's output data frame
    
    cond_out <- merge(out, cond_res, by = c("Measure", "Assessment"), all.x = TRUE)
    
    cond_out$Measure    <- factor(cond_out$Measure,    levels = node_vars)
    cond_out$Assessment <- factor(cond_out$Assessment, levels = ordered_levs)
    
    cond_out <- cond_out[order(cond_out$Measure, cond_out$Assessment), ]
    
    names(cond_out)[names(cond_out) == "n"]    <- paste0("n_",    condition)
    names(cond_out)[names(cond_out) == "m_sd"] <- paste0("m_sd_", condition)
    
    # Add condition's output to overall output
    
    if (i == 1) {
      res_by_cond <- cond_out
    } else if (i > 1) {
      cond_out[c("Measure", "Assessment")] <- NULL
      
      res_by_cond <- cbind(res_by_cond, cond_out)
    }
  }
  
  # Exclude time points not assessed for certain measures
  
  nonserial_measures <- c("rr_neg_thr_mean", "rr_pos_thr_mean_rev", "bbsiq_neg_mean")
  levs_not_assessed <- paste0("SESSION", c(1:2, 4:5))
  
  res_by_cond <- res_by_cond[!(res_by_cond$Measure %in% nonserial_measures &
                                 res_by_cond$Assessment %in% levs_not_assessed), ]

  row.names(res_by_cond) <- 1:nrow(res_by_cond)
  
  return(res_by_cond)
}

# Compute descriptives by condition

## For ITT participants in RR or BBSIQ datasets
## - Note: Values for OASIS and RR are for ITT sample in RR network dataset, and 
##   values for OASIS and BBSIQ are for ITT sample in BBSIQ network dataset; the
##   datasets differ by 1 person, who lacks OASIS and RR data but not BBSIQ data

diff_mask <- net_dat_all_to_s6_itt$itt_rr_net != net_dat_all_to_s6_itt$itt_bb_net
diff_pid  <- unique(net_dat_all_to_s6_itt$participant_id[diff_mask])
stopifnot(diff_pid == 583)

node_vars_overall <- unique(c(nodes$rr_net, nodes$bb_net))

desc_tbl_by_cond_itt_any_net <- 
  compute_desc_node_vars_by_cond(net_dat_all_to_s6_itt, node_vars_overall, ordered_levs)

## For completers in each specific dataset

desc_tbl_by_cond_compl_rr_net <- 
  compute_desc_node_vars_by_cond(net_dat_rr_all_to_s6_compl, nodes$rr_net, ordered_levs)
desc_tbl_by_cond_compl_bb_net <- 
  compute_desc_node_vars_by_cond(net_dat_bb_all_to_s6_compl, nodes$bb_net, ordered_levs)

# Save objects for later use in computing rates of scale-level missingness

desc_path <- file.path("results", "descriptives")
dir.create(desc_path)

saveRDS(desc_tbl_by_cond_itt_any_net,  file.path(desc_path, "desc_tbl_by_cond_itt_any_net.rds"))
saveRDS(desc_tbl_by_cond_compl_rr_net, file.path(desc_path, "desc_tbl_by_cond_compl_rr_net.rds"))
saveRDS(desc_tbl_by_cond_compl_bb_net, file.path(desc_path, "desc_tbl_by_cond_compl_bb_net.rds"))

# ---------------------------------------------------------------------------- #
# Format descriptives tables ----
# ---------------------------------------------------------------------------- #

# "flextable" defaults are set in "set_flextable_defaults.R" above

# Define function to format descriptives tables

format_desc_tbl <- function(desc_tbl, gen_note, footnotes, title, sample = NULL) {
  # Define columns
  
  target_cols <- names(desc_tbl)
  left_align_body_cols <- c("Measure", "Assessment")
  merge_v_cols         <- "Measure"
  
  # Define column formats
  
  n_format    <- as_paragraph(as_i("n"))
  m_sd_format <- as_paragraph(as_i("M"), " (", as_i("SD"), ")")
  
  # Create flextable
  
  desc_tbl_ft <- flextable(desc_tbl[target_cols]) |>
    set_table_properties(align = "left") |>
    
    set_caption(as_paragraph(as_i(title)), word_stylename = "heading 1",
                fp_p = fp_par(padding.left = 0, padding.right = 0),
                align_with_table = FALSE) |>
    
    align(align = "center", part = "header") |>
    align(align = "center", part = "body") |>
    align(j = left_align_body_cols, align = "left", part = "body") |>
    align(align = "left", part = "footer") |>
    
    merge_v(j = merge_v_cols, part = "body") |>
    valign(j = merge_v_cols, valign = "top", part = "body") |>
    fix_border_issues(part = "body") |>
    
    valign(valign = "bottom", part = "header") |>
    
    compose(j = "n_POSITIVE",    part = "header", value = n_format) |>
    compose(j = "n_FIFTY_FIFTY", part = "header", value = n_format) |>
    compose(j = "n_NEUTRAL",     part = "header", value = n_format) |>

    compose(j = "m_sd_POSITIVE",    part = "header", value = m_sd_format) |>
    compose(j = "m_sd_FIFTY_FIFTY", part = "header", value = m_sd_format) |>
    compose(j = "m_sd_NEUTRAL",     part = "header", value = m_sd_format) |>
    
    add_header_row(values = as_paragraph_md(c("",
                                              "Positive CBM-I",
                                              "50-50 CBM-I",
                                              "No-Training")),
                   colwidths = rep(2, 4)) |>

    add_footer_lines(gen_note)
  
  if (!is.null(sample) && sample %in% c("itt", "compl_rr_net")) {
    lack_pos_bias_row_idx <- min(which(desc_tbl$Measure == "rr_pos_thr_mean_rev"))
    
    desc_tbl_ft <- desc_tbl_ft |>
      footnote(i = lack_pos_bias_row_idx, j = 1,
               value = as_paragraph_md(footnotes$lack_pos_bias),
               ref_symbols = " a",
               part = "body")
  }
  
  desc_tbl_ft <- desc_tbl_ft |>
    labelizor(part = "body",
              labels = c("anxious_freq"        = "Anxiety Frequency (OASIS)",
                         "anxious_sev"         = "Anxiety Severity (OASIS)",
                         "avoid"               = "Situational Avoidance (OASIS)",
                         "interfere"           = "Work Impairment (OASIS)",
                         "interfere_social"    = "Social Impairment (OASIS)",
                         "rr_neg_thr_mean"     = "Negative Bias (RR)",
                         "rr_pos_thr_mean_rev" = "Lack of Positive Bias (RR)",
                         "bbsiq_neg_mean"      = "Negative Bias (BBSIQ)",
                         "PRE"                 = "Baseline",
                         "SESSION1"            = "Session 1",
                         "SESSION2"            = "Session 2",
                         "SESSION3"            = "Session 3",
                         "SESSION4"            = "Session 4",
                         "SESSION5"            = "Session 5",
                         "SESSION6"            = "Session 6")) |>
    
    autofit()
}

# Define notes

cbm_abbr   <- "CBM-I = cognitive bias modification for interpretation"
oasis_abbr <- "OASIS = item from Overall Anxiety Severity and Impairment Scale"
rr_abbr    <- "RR = average item score from Recognition Ratings"
bbsiq_abbr <- "BBSIQ = average item score from Brief Body Sensations Interpretations Questionnaire"

gen_note_itt <- as_paragraph_md(paste0(
  "*Note.* Descriptives shown for OASIS and RR are for ITT sample in RR networks; ",
  "descriptives shown for OASIS and BBSIQ are for ITT sample in BBSIQ networks. ",
  paste(c(cbm_abbr, oasis_abbr, rr_abbr, bbsiq_abbr), collapse = "; "), "."))

gen_note_compl_rr_net <- as_paragraph_md(paste0(
  "*Note.* Descriptives are shown for participants with complete data for OASIS ",
  "and RR at baseline, Session 3, and Session 6. ",
  paste(c(cbm_abbr, oasis_abbr, rr_abbr), collapse = "; "), "."))

gen_note_compl_bb_net <- as_paragraph_md(paste0(
  "*Note.* Descriptives are shown for participants with complete data for OASIS ",
  "and BBSIQ at baseline, Session 3, and Session 6. ",
  paste(c(cbm_abbr, oasis_abbr, bbsiq_abbr), collapse = "; "), "."))

footnotes <- list(lack_pos_bias = "\\ Reverse-scored positive bias.")

# Run function

desc_tbl_by_cond_itt_any_net_ft <- 
  format_desc_tbl(desc_tbl_by_cond_itt_any_net, gen_note_itt, footnotes,
                  "Raw Means and Standard Deviations by Treatment Arm for Intent-To-Treat (ITT) Sample",
                  "itt")

desc_tbl_by_cond_compl_rr_net_ft <- 
  format_desc_tbl(desc_tbl_by_cond_compl_rr_net, gen_note_compl_rr_net, footnotes,
                  "Raw Means and Standard Deviations by Treatment Arm for Completer Sample in RR Networks",
                  "compl_rr_net")
desc_tbl_by_cond_compl_bb_net_ft <- 
  format_desc_tbl(desc_tbl_by_cond_compl_bb_net, gen_note_compl_bb_net, footnotes,
                  "Raw Means and Standard Deviations by Treatment Arm for Completer Sample in BBSIQ Networks")

# ---------------------------------------------------------------------------- #
# Save flextables ----
# ---------------------------------------------------------------------------- #

saveRDS(desc_tbl_by_cond_itt_any_net_ft,  file.path(desc_path, "desc_tbl_by_cond_itt_any_net_ft.rds"))
saveRDS(desc_tbl_by_cond_compl_rr_net_ft, file.path(desc_path, "desc_tbl_by_cond_compl_rr_net_ft.rds"))
saveRDS(desc_tbl_by_cond_compl_bb_net_ft, file.path(desc_path, "desc_tbl_by_cond_compl_bb_net_ft.rds"))