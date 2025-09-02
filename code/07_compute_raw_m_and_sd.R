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
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./code/01a_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# Load packages

pkgs <- c("flextable", "ftExtra", "officer")
groundhog.library(pkgs, groundhog_day)

# Set "flextable" package defaults

source("./code/01b_set_flextable_defaults.R")

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/intermediate/net_dat_all_to_s6.RData")
net_dat_all_to_s6_wide <- read.csv(file = "./data/intermediate/net_dat_all_to_s6_wide.csv")

# ---------------------------------------------------------------------------- #
# Prepare data ----
# ---------------------------------------------------------------------------- #

# Add completer sample indicator from "net_dat_all_to_s6_wide"

net_dat_all_to_s6 <- merge(net_dat_all_to_s6,
                           net_dat_all_to_s6_wide[, c("participant_id", "complete_bl_s3_s6")],
                           by = "participant_id", all.x = TRUE)

# Order condition levels

net_dat_all_to_s6$cbmCondition <- factor(net_dat_all_to_s6$cbmCondition,
                                         levels = c("POSITIVE", "FIFTY_FIFTY", "NEUTRAL"))

# TODO (is this needed?): Check sample sizes (divide by 7 time points to compute number of participants)

# table(anlys_df$condition_sep[anlys_df$itt_anlys == 1])/7
# table(anlys_df$condition_sep[anlys_df$s5_train_compl_anlys_uncorrected_c1 == 1])/7





# Make "session_only" a factor (for correct order when aggregating below)

ordered_levs <- c("PRE", paste0("SESSION", 1:6))

net_dat_all_to_s6$session_only <- factor(net_dat_all_to_s6$session_only, levels = ordered_levs)

# Restrict to ITT and completer samples

net_dat_all_to_s6_itt   <- net_dat_all_to_s6
net_dat_all_to_s6_compl <- net_dat_all_to_s6[net_dat_all_to_s6$complete_bl_s3_s6 == 1, ]

# ---------------------------------------------------------------------------- #
# Compute n, M, and SD for each raw node variable by condition over time ----
# ---------------------------------------------------------------------------- #

# TODO (add BBSIQ): Define node vars

oa_node_vars <- c("anxious_freq", "anxious_sev", "avoid", "interfere", "interfere_social")
rr_node_vars <- c("rr_ns_mean", "rr_ps_mean_rev")

node_vars <- c(oa_node_vars, rr_node_vars)





# Define function for computing n, M, and SD for each node variable over time

compute_desc_node_vars <- function(df, node_vars) {
  for (k in 1:length(node_vars)) {
    fml <- as.formula(paste0(node_vars[k], "~ session_only"))
    ag <- do.call(data.frame, aggregate(fml, 
                                        data = df,
                                        FUN = function(x) {
                                          c(n    = length(x), 
                                            m_sd = paste0(format(round(mean(x), 2), nsmall = 2, trim = TRUE), 
                                                          " (",
                                                          format(round(sd(x), 2), nsmall = 2, trim = TRUE),
                                                          ")"))
                                        }))
    
    node_var_output <- cbind(node_vars[k], ag)
    names(node_var_output) <- c("Measure", "Assessment", "n", "m_sd")
    
    if (k == 1) {
      output <- node_var_output
    } else if (k > 1) {
      output <- rbind(output, node_var_output)
    } 
  }
  
  return(output)
}

# Create empty data frame with all node variables at all time points to store output 
# (given that not all conditions will have observations at all time points)

out <- data.frame(Measure    = rep(node_vars, each = length(ordered_levs)),
                  Assessment = rep(ordered_levs, length(node_vars)))

# Define function for computing n, M, and SD for each score by condition over time

compute_desc_node_vars_by_cond <- function(df, out, node_vars, ordered_levs) {
  conditions <- levels(droplevels(df$cbmCondition))
  
  for (i in 1:length(conditions)) {
    df_cond <- df[df$cbmCondition == conditions[i], ]
    
    cond_res <- compute_desc_node_vars(df_cond, node_vars)
    
    cond_out <- merge(out, cond_res, by = c("Measure", "Assessment"), all.x = TRUE)
    
    cond_out$Measure    <- factor(cond_out$Measure,    levels = node_vars)
    cond_out$Assessment <- factor(cond_out$Assessment, levels = ordered_levs)
    
    cond_out <- cond_out[order(cond_out$Measure, cond_out$Assessment), ]
    
    names(cond_out)[names(cond_out) == "n"]     <- paste0("n_",    conditions[i])
    names(cond_out)[names(cond_out) == "m_sd"]  <- paste0("m_sd_", conditions[i])
    
    if (i == 1) {
      res_by_cond <- cond_out
    } else if (i > 1) {
      cond_out[, c("Measure", "Assessment")] <- NULL
      
      res_by_cond <- cbind(res_by_cond, cond_out)
    }
  }
  
  res_by_cond <- res_by_cond[res_by_cond$Measure %in% oa_node_vars |
                               
                               (res_by_cond$Measure %in% rr_node_vars &
                                  res_by_cond$Assessment %in% c("PRE", paste0("SESSION", c(3, 6)))), ]
  
  row.names(res_by_cond) <- 1:nrow(res_by_cond)
  
  return(res_by_cond)
}

# Compute descriptives by condition for the ITT and completer samples

res_node_vars_itt_by_cond <-
  compute_desc_node_vars_by_cond(net_dat_all_to_s6_itt,   out, node_vars, ordered_levs)
res_node_vars_compl_by_cond <- 
  compute_desc_node_vars_by_cond(net_dat_all_to_s6_compl, out, node_vars, ordered_levs)

# Save main outcomes objects for later use in computing rates of scale-level missingness

desc_path <- "./results/descriptives/"
dir.create(desc_path)

save(res_node_vars_itt_by_cond,   file = paste0(desc_path, "res_node_vars_itt_by_cond.RData"))
save(res_node_vars_compl_by_cond, file = paste0(desc_path, "res_node_vars_compl_by_cond.RData"))

# ---------------------------------------------------------------------------- #
# Format descriptives tables ----
# ---------------------------------------------------------------------------- #

# "flextable" defaults are set in "set_flextable_defaults.R" above

# Define function to format descriptives tables

format_desc_tbl <- function(desc_tbl, gen_note, footnotes, title) {
  # Identify rows for footnotes
  
  lack_pos_bias_row_idx <- min(which(desc_tbl$Measure == "rr_ps_mean_rev"))
  
  # Define columns
  
  target_cols <- names(desc_tbl)
  left_align_body_cols <- c("Measure", "Assessment")
  merge_v_cols         <- "Measure"
  
  # Define column formats
  
  n_format    <- as_paragraph(as_i("n"))
  m_sd_format <- as_paragraph(as_i("M"), " (", as_i("SD"), ")")
  
  # Create flextable
  
  desc_tbl_ft <- flextable(desc_tbl[, target_cols]) |>
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
    
    compose(j = "n_POSITIVE", part = "header", value = n_format) |>
    compose(j = "n_FIFTY_FIFTY", part = "header", value = n_format) |>
    compose(j = "n_NEUTRAL",    part = "header", value = n_format) |>

    compose(j = "m_sd_POSITIVE", part = "header", value = m_sd_format) |>
    compose(j = "m_sd_FIFTY_FIFTY", part = "header", value = m_sd_format) |>
    compose(j = "m_sd_NEUTRAL",    part = "header", value = m_sd_format) |>
    
    add_header_row(values = as_paragraph_md(c("",
                                              "Positive CBM-I",
                                              "50-50 CBM-I",
                                              "No-Training")),
                   colwidths = rep(2, 4)) |>

    add_footer_lines(gen_note) |>
    
    footnote(i = lack_pos_bias_row_idx, j = 1,
             value = as_paragraph_md(footnotes$lack_pos_bias),
             ref_symbols = " a",
             part = "body") |>
    
    labelizor(part = "body",
              labels = c("anxious_freq"     = "Anxiety Frequency (OASIS)",
                         "anxious_sev"      = "Anxiety Severity (OASIS)",
                         "avoid"            = "Situational Avoidance (OASIS)",
                         "interfere"        = "Work Impairment (OASIS)",
                         "interfere_social" = "Social Impairment (OASIS)",
                         "rr_ns_mean"       = "Negative Bias (RR)",
                         "rr_ps_mean_rev"   = "Lack of Positive Bias (RR)",
                         "PRE"              = "Baseline",
                         "SESSION1"         = "Session 1",
                         "SESSION2"         = "Session 2",
                         "SESSION3"         = "Session 3",
                         "SESSION4"         = "Session 4",
                         "SESSION5"         = "Session 5",
                         "SESSION6"         = "Session 6")) |>
    
    autofit()
}

# Define notes

gen_note <- as_paragraph_md("*Note.* CBM-I = cognitive bias modification for interpretation; OASIS = item from Overall Anxiety Severity and Impairment Scale; RR = average item score from Recognition Ratings.")

footnotes <- list(lack_pos_bias = "\\ Reverse-scored positive bias.")

# Run function

desc_tbl_itt_by_cond_ft <- 
  format_desc_tbl(res_node_vars_itt_by_cond,   gen_note, footnotes,
                  "Raw Means and Standard Deviations by Treatment Arm for Intent-To-Treat Sample")
desc_tbl_compl_by_cond_ft <- 
  format_desc_tbl(res_node_vars_compl_by_cond, gen_note, footnotes,
                  "Raw Means and Standard Deviations by Treatment Arm for Completer Sample")

# ---------------------------------------------------------------------------- #
# Save flextables ----
# ---------------------------------------------------------------------------- #

save(desc_tbl_itt_by_cond_ft,   file = paste0(desc_path, "desc_tbl_itt_by_cond_ft.RData"))
save(desc_tbl_compl_by_cond_ft, file = paste0(desc_path, "desc_tbl_compl_by_cond_ft.RData"))

# TODO: Clarify description of "completer sample"




