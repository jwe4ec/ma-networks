# ---------------------------------------------------------------------------- #
# Run Temporal Network Analyses
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

groundhog_day <- version_control_temporal()

# Load packages

pkgs <- c("dplyr", "psychonetrics")

groundhog.library(pkgs, groundhog_day)

# Set seed

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

net_dat_all_to_s6_wide <- read.csv(file = "./data/intermediate/net_dat_all_to_s6_wide.csv")

# ---------------------------------------------------------------------------- #
# Remove significant linear trends and standardize across waves and participants ----
# ---------------------------------------------------------------------------- #

# Convert to long format

potential_node_vars <- c("anxious_freq", "anxious_sev", "avoid", "interfere", "interfere_social",
                         "rr_ns_mean", "rr_ps_mean", "rr_ps_mean_rev")

waves <- c("PRE", paste0("SESSION", 1:6))

varying_vars <- paste0(rep(potential_node_vars, length(waves)), 
                       ".",
                       rep(waves, each = length(potential_node_vars)))

net_dat_long <- reshape(net_dat_all_to_s6_wide,
                        varying   = varying_vars,
                        v.names   = potential_node_vars,
                        timevar   = "wave",
                        times     = waves,
                        idvar     = "participant_id",
                        direction = "long")

net_dat_long <- net_dat_long[order(net_dat_long$participant_id,
                                   net_dat_long$wave), ]

# Recode wave

net_dat_long$wave_int <- NA

net_dat_long$wave_int[net_dat_long$wave == "PRE"] <- 0
net_dat_long$wave_int[net_dat_long$wave == "SESSION1"] <- 1
net_dat_long$wave_int[net_dat_long$wave == "SESSION2"] <- 2
net_dat_long$wave_int[net_dat_long$wave == "SESSION3"] <- 3
net_dat_long$wave_int[net_dat_long$wave == "SESSION4"] <- 4
net_dat_long$wave_int[net_dat_long$wave == "SESSION5"] <- 5
net_dat_long$wave_int[net_dat_long$wave == "SESSION6"] <- 6

# Remove significant linear trends for each variable separately in each condition
# (and output which trends were removed to TXT). For reference, see:
#   1. Freichel et al. (2023, https://doi.org/m3x9, "panel_network_models.R" script 
#      Lines 40-69 at https://www.tinyurl.com/4m7m78sm ), who commented out detrending 
#      code for panel GVAR, stating in paper it was not needed given good model fit; 
#      however, they still seem to standardize per variable across waves and participants, 
#      although this is not mentioned in their paper
#   2. Epskamp et al. (2018, https://doi.org/ggs3x7, "Supplementary1_Rcodes.R" script 
#      Lines 23-31 at https://osf.io/c8wjz ), who removed significant linear trends in an 
#      idiographic time series GVAR
#   3. Freichel (2023, https://doi.org/m3zb, "panel_analyses.R" script Lines 125-153 at
#      https://osf.io/vqzsy/?view_only=fe79b6f659b648d697389e9109ff7962 ), who removed
#      all linear and quadratic trends and standardized in a panel GVAR

detrend_path <- "./results/panel_gvar/detrend/"

dir.create(detrend_path, recursive = TRUE)

sink(file = paste0(detrend_path, "detrend.txt"))

conditions <- c("POSITIVE", "FIFTY_FIFTY", "NEUTRAL")

for (i in 1:length(potential_node_vars)) {
  # Create new variable for detrended values
  
  node_var_detrend <- paste0(potential_node_vars[i], "_detrend")
  
  net_dat_long[node_var_detrend] <- NA
  
  # Detrend if needed separately in each condition (otherwise use original value)
  
  for (j in 1:length(conditions)) {
    ff <- as.formula(paste0(potential_node_vars[i]," ~ wave_int"))
    
    fit <- lm(ff, net_dat_long[net_dat_long$cbmCondition == conditions[j], ])

    if (anova(fit)["wave_int", "Pr(>F)"] < .05) {
      net_dat_long[net_dat_long$cbmCondition == conditions[j] &
                     !is.na(net_dat_long[potential_node_vars[i]]), node_var_detrend] <- residuals(fit)

      print(paste("Removed sig. linear trend for", potential_node_vars[i], "in condition", conditions[j]))
    } else {
      net_dat_long[net_dat_long$cbmCondition == conditions[j], node_var_detrend] <-
        net_dat_long[net_dat_long$cbmCondition == conditions[j], potential_node_vars[i]]

      print(paste("No sig. linear trend for", potential_node_vars[i], "in condition", conditions[j]))
    }
  }
}

sink()

# Remove "detrend" from column names

net_dat_long_detrend <- net_dat_long[, !(names(net_dat_long) %in% potential_node_vars)]

names(net_dat_long_detrend) <- sub("_detrend", "", names(net_dat_long_detrend))

# Also create a dataset with each variable standardized across waves and participants.
# However, this does not seem to make a difference in results. Perhaps not needed.

net_dat_long_detrend_std <- net_dat_long_detrend

net_dat_long_detrend_std[, potential_node_vars] <- 
  as.data.frame(scale(net_dat_long_detrend_std[, potential_node_vars]))

# Convert to wide format

net_dat_all_to_s6_wide_detrend <- reshape(net_dat_long_detrend, 
                                          direction = "wide",
                                          idvar     = "participant_id",
                                          timevar   = "wave",
                                          v.names   = potential_node_vars,
                                          drop      = "wave_int")

net_dat_all_to_s6_wide_detrend_std <- reshape(net_dat_long_detrend_std, 
                                              direction = "wide",
                                              idvar     = "participant_id",
                                              timevar   = "wave",
                                              v.names   = potential_node_vars,
                                              drop      = "wave_int")

# Save detrended, unstandardized data for connectivity analyses

save(net_dat_all_to_s6_wide_detrend, file = "./data/intermediate/net_dat_all_to_s6_wide_detrend.RData")

# ---------------------------------------------------------------------------- #
# Notes on panel GVAR analyses ----
# ---------------------------------------------------------------------------- #

# Use "panelgvar" wrapper of "dvlm1" to run a multilevel GVAR model (with random 
# intercepts and fixed network parameters), which is what the panel-lvgvar model
# reduces to if all variables are treated as observed without measurement error 
# (see Epskamp, 2020, p. 227). The code draws on examples below from Sacha Epskamp
# and on "panel_lvgvar_example.R" in Epskamp (2020) supplement at https://doi.org/m337

# http://psychonetrics.org/files/PNAWS2020lecture.html#panel-data-gvar
# https://github.com/SachaEpskamp/SEM-code-examples/blob/master/psychonetrics/SHARE%20panel%20example/shareAnalysis.R

# TODO: If model fit issues, consider removing quadratic trends (perhaps inspecting
# trajectories first), as Freichel (2023) did





# TODO: Consider detrending and standardizing OASIS items just based on three waves 
# modeled. Also consider completer analysis based on complete data





# ---------------------------------------------------------------------------- #
# Specify design matrix ----
# ---------------------------------------------------------------------------- #

# Define function to specify design matrix. OASIS items are at every wave, but RR 
# items were given only at "PRE", "SESSION3", and "SESSION6" (specify NA at these 
# time points). Specify whether to use "rr_ps_mean" or "rr_ps_mean_rev".

node_vars_w_rr_ps_mean     <- setdiff(potential_node_vars, "rr_ps_mean_rev")
node_vars_w_rr_ps_mean_rev <- setdiff(potential_node_vars, "rr_ps_mean")

create_design_mat <- function(node_vars, waves) {
  design_mat <- matrix(c(paste0(node_vars, ".PRE"),
                         c(paste0(node_vars[1:5], ".SESSION1"), NA, NA),
                         c(paste0(node_vars[1:5], ".SESSION2"), NA, NA),
                         paste0(node_vars, ".SESSION3"),
                         c(paste0(node_vars[1:5], ".SESSION4"), NA, NA),
                         c(paste0(node_vars[1:5], ".SESSION5"), NA, NA),
                         paste0(node_vars, ".SESSION6")),
                       nrow = length(node_vars), 
                       ncol = length(waves),
                       dimnames = list(node_vars, waves))
}

design_mat_w_rr_ps_mean     <- create_design_mat(node_vars_w_rr_ps_mean,     waves)
design_mat_w_rr_ps_mean_rev <- create_design_mat(node_vars_w_rr_ps_mean_rev, waves)

# ---------------------------------------------------------------------------- #
# Specify single-group model ----
# ---------------------------------------------------------------------------- #

# Specify model using raw detrended data (instead of covariance matrix) and "FIML"
# estimator given missing data

#   Using full design matrix with all waves yields the following error for every
#   available optimizer, for both ML and FIML estimation, for raw, detrended, and
#   detrended/standardized data, and for both original and reverse-coded positive
#   bias. Perhaps RR nodes are missing at too many waves.

      # Error in eigen(prior_estimates[[g]]$beta_estimate) : 
      #   infinite or missing values in 'x'

# model <- panelgvar(net_dat_all_to_s6_wide,             vars = design_mat_w_rr_ps_mean, estimator = "FIML")
# model <- panelgvar(net_dat_all_to_s6_wide_detrend,     vars = design_mat_w_rr_ps_mean, estimator = "FIML")
# model <- panelgvar(net_dat_all_to_s6_wide_detrend_std, vars = design_mat_w_rr_ps_mean, estimator = "FIML")

# model_rev <- panelgvar(net_dat_all_to_s6_wide,             vars = design_mat_w_rr_ps_mean_rev, estimator = "FIML")
# model_rev <- panelgvar(net_dat_all_to_s6_wide_detrend,     vars = design_mat_w_rr_ps_mean_rev, estimator = "FIML")
# model_rev <- panelgvar(net_dat_all_to_s6_wide_detrend_std, vars = design_mat_w_rr_ps_mean_rev, estimator = "FIML")

#   Restricting design matrix to waves where all variables were administered works

full_waves <- c("PRE", "SESSION3", "SESSION6")

design_mat_w_rr_ps_mean_full     <- design_mat_w_rr_ps_mean[, full_waves]
design_mat_w_rr_ps_mean_rev_full <- design_mat_w_rr_ps_mean_rev[, full_waves]

model1         <- panelgvar(net_dat_all_to_s6_wide_detrend,     vars = design_mat_w_rr_ps_mean_full,     estimator = "FIML")
model1_std     <- panelgvar(net_dat_all_to_s6_wide_detrend_std, vars = design_mat_w_rr_ps_mean_full,     estimator = "FIML")

model1_rev     <- panelgvar(net_dat_all_to_s6_wide_detrend,     vars = design_mat_w_rr_ps_mean_rev_full, estimator = "FIML")
model1_rev_std <- panelgvar(net_dat_all_to_s6_wide_detrend_std, vars = design_mat_w_rr_ps_mean_rev_full, estimator = "FIML")

#   Save design matrix with reverse-scored positive bias for connectivity analyses

design_mat_path <- "./results/panel_gvar/design_mat/"

dir.create(design_mat_path)

save(design_mat_w_rr_ps_mean_rev_full, file = paste0(design_mat_path, "./design_mat_w_rr_ps_mean_rev_full.RData"))

#   Although full waves above are evenly spaced in intervening assessment points,
#   see if it matters whether intervening assessment points are deemed as NAs. This
#   yields the same error as above.

    # Error in eigen(prior_estimates[[g]]$beta_estimate) : 
    #   infinite or missing values in 'x'

design_mat_w_rr_ps_mean_rev_nas <- design_mat_w_rr_ps_mean_rev

na_waves <- paste0("SESSION", c(1, 2, 4, 5))

design_mat_w_rr_ps_mean_rev_nas[, na_waves] <- NA

# model1_rev_nas     <- panelgvar(net_dat_all_to_s6_wide_detrend,     vars = design_mat_w_rr_ps_mean_rev_nas, estimator = "FIML")
# model1_rev_nas_std <- panelgvar(net_dat_all_to_s6_wide_detrend_std, vars = design_mat_w_rr_ps_mean_rev_nas, estimator = "FIML")

# ---------------------------------------------------------------------------- #
# Run saturated single-group model ----
# ---------------------------------------------------------------------------- #

# TODO: Export all results below to TXT (not just parameters)





# Estimate saturated model using default optimizer "nlminb". Results are same
# when using "rr_ps_mean" and "rr_ps_mean_rev" with exception of opposite sign
# for nu, omega_zeta_within, beta, and omega_zeta_between. Use "rr_ps_mean_rev"
# per preregsitration.

model1_nlminb     <- model1     %>% runmodel
model1_std_nlminb <- model1_std %>% runmodel

model1_rev_nlminb     <- model1_rev     %>% runmodel
model1_rev_std_nlminb <- model1_rev_std %>% runmodel

# Inspect parameters
#   nu                 = intercepts of observed variables
#   lambda             = factor loadings (set to 1 by panelgvar)
#   omega_zeta_within  = within-subject contemporaneous effects
#   delta_zeta_within  = diagonal scaling matrix controlling variances
#   beta               = within-subject temporal effects
#   omega_zeta_between = between-subject effects
#   delta_zeta_between = diagonal scaling matrix controlling variances

parameters_path <- "./results/panel_gvar/single_grp/parameters/"

dir.create(parameters_path, recursive = TRUE)

sink(file = paste0(parameters_path, "model1_nlminb_parameters.txt"))
model1_nlminb %>% parameters
sink()
sink(file = paste0(parameters_path, "model1_std_nlminb_parameters.txt"))
model1_std_nlminb %>% parameters
sink()

sink(file = paste0(parameters_path, "model1_rev_nlminb_parameters.txt"))
model1_rev_nlminb %>% parameters
sink()
sink(file = paste0(parameters_path, "model1_rev_std_nlminb_parameters.txt"))
model1_rev_std_nlminb %>% parameters
sink()

# Standard errors for the between-person effects seem implausibly large. Given
# that Epskamp says panelgvar tends to struggle to estimate sensible between-person 
# effects and that if the between-person scaling parameters (delta_zeta_between)
# are not significant then the the partial correlations (omega_zeta_between) cannot
# really be interpreted, refit model using Cholesky decomposition to model between-
# person effects and do not interpret the between-person effects. Epskamp says that
# simulations suggest that doing so can still yield proper within-person effects (see 
# https://github.com/SachaEpskamp/psychonetrics/issues/25#issuecomment-1767748032).
# Indeed, the parameters for the within-person effects are nearly identical.

model2_rev     <- panelgvar(net_dat_all_to_s6_wide_detrend,     vars = design_mat_w_rr_ps_mean_rev_full, estimator = "FIML", 
                            between_latent = "chol")
model2_rev_std <- panelgvar(net_dat_all_to_s6_wide_detrend_std, vars = design_mat_w_rr_ps_mean_rev_full, estimator = "FIML", 
                            between_latent = "chol")

model2_rev_nlminb     <- model2_rev     %>% runmodel
model2_rev_std_nlminb <- model2_rev_std %>% runmodel

sink(file = paste0(parameters_path, "model2_rev_nlminb_parameters.txt"))
model2_rev_nlminb %>% parameters
sink()
sink(file = paste0(parameters_path, "model2_rev_std_nlminb_parameters.txt"))
model2_rev_std_nlminb %>% parameters
sink()

# TODO: Ask Epskamp if the lowertri_zeta_between estimate for "rr_ps_mean_rev" of 
# 0.00053 (SE = 126.40, p = 1.0) is a problem. Tried fixing it to zero, and all other 
# parameter estimates and model fit are nearly identical as those of "model2_rev". 
# Ignore this for now.

model2_rev_fix <- model2_rev %>%
  fixpar("lowertri_zeta_between", "rr_ps_mean_rev", "rr_ps_mean_rev")

model2_rev_fix_nlminb <- model2_rev_fix %>% runmodel

sink(file = paste0(parameters_path, "model2_rev_fix_nlminb_parameters.txt"))
model2_rev_fix_nlminb %>% parameters
sink()





# Check fit. Although chi-square is sig., RMSEA, CFI, and TLI show good fit

model1_nlminb %>% print                # Says relative convergence (4)
model1_nlminb %>% fit

model1_std_nlminb %>% print            # Says relative convergence (4)
model1_std_nlminb %>% fit

model1_rev_nlminb %>% print            # Says relative convergence (4)
model1_rev_nlminb %>% fit

model1_rev_std_nlminb %>% print        # Says relative convergence (4)
model1_rev_std_nlminb %>% fit

model2_rev_nlminb %>% print            # Says relative convergence (4)
model2_rev_nlminb %>% fit

model2_rev_std_nlminb %>% print        # Says relative convergence (4)
model2_rev_std_nlminb %>% fit

model2_rev_fix_nlminb %>% print        # Says relative convergence (4)
model2_rev_fix_nlminb %>% fit

# ---------------------------------------------------------------------------- #
# Consider pruning and model search for single-group model ----
# ---------------------------------------------------------------------------- #

# Specify estimation algorithm settings, using same as used for "dlvm1" model

alpha <- 0.01
adjust <- "none"
searchstrategy <- "modelsearch"

# Run estimation algorithm (prune step)

model2_rev_nlminb_prune <- model2_rev_nlminb %>% 
  prune(alpha = alpha, adjust = adjust, recursive = FALSE)

# Run model search strategy

if (searchstrategy == "stepup") {
  model2_rev_nlminb_prune <- model2_rev_nlminb_prune %>%  
    stepup(alpha = alpha, criterion = "bic")
} else if (searchstrategy == "modelsearch") {
  model2_rev_nlminb_prune <- model2_rev_nlminb_prune %>%  
    modelsearch(prunealpha = alpha, addalpha = alpha)
}

# Compare original and pruned models. Original model fits significantly better
# and is superior on AIC but worse on BIC.

comp_model2_rev_nlminb <- compare(original = model2_rev_nlminb,
                                  pruned   = model2_rev_nlminb_prune)

comp_model2_rev_nlminb$AIC[1] - comp_model2_rev_nlminb$AIC[2]
comp_model2_rev_nlminb$BIC[1] - comp_model2_rev_nlminb$BIC[2]

# Fit of pruned model is no longer within traditional guidelines per CFI and
# TLI (now .88 and .87, respectively). Retain original model.

model2_rev_nlminb_prune %>% print     # Says relative convergence (4)
model2_rev_nlminb_prune %>% fit

# Also, saturated model seems preferred for inferring significance of parameters
# to avoid model selection bias (Williams, 2021), so ultimately retain this model
# anyway. Can use thresholding (vs. pruning/model search and refitting) to achieve 
# more sparsity in plots, but saturated network needed for uncertainty estimates.

# ---------------------------------------------------------------------------- #
# Check factor loadings and residual variances of single-group model ----
# ---------------------------------------------------------------------------- #

# Confirm that factor loadings are set to identity matrix and that residual
# variances are set to 0

# Factor loadings

lambda_model2_rev_nlminb <- getmatrix(model2_rev_nlminb, "lambda")

# Residual variances

theta_model2_rev_nlminb  <- getmatrix(model2_rev_nlminb, "sigma_epsilon_within")

# Latent variance-covariance

psi_model2_rev_nlminb    <- getmatrix(model2_rev_nlminb, "sigma_zeta_within")

# ---------------------------------------------------------------------------- #
# Specify multigroup model ----
# ---------------------------------------------------------------------------- #

# Using full design matrix with all waves again yields following error

  # Error in eigen(prior_estimates[[g]]$beta_estimate) : 
  #   infinite or missing values in 'x'

# mg_model_rev <- panelgvar(net_dat_all_to_s6_wide_detrend, vars = design_mat_w_rr_ps_mean_rev, estimator = "FIML",
#                           groups = "cbmCondition")

# Restricting design matrix to waves where all variables were administered works

mg_model1_rev     <- panelgvar(net_dat_all_to_s6_wide_detrend,     vars = design_mat_w_rr_ps_mean_rev_full, estimator = "FIML",
                               groups = "cbmCondition")
mg_model1_rev_std <- panelgvar(net_dat_all_to_s6_wide_detrend_std, vars = design_mat_w_rr_ps_mean_rev_full, estimator = "FIML",
                               groups = "cbmCondition")

# ---------------------------------------------------------------------------- #
# Run saturated multigroup model ----
# ---------------------------------------------------------------------------- #

# TODO: Export all results below to TXT (not just parameters)





# Estimate saturated model using default optimizer "nlminb", which yields warning.
# Thus, try obtaining approximate SEs, which resolves the warning.

  # Warning message:
  # In addSEs_cpp(x, verbose = verbose, approximate_SEs = approximate_SEs) :
  #   Standard errors could not be obtained because the Fischer information matrix 
  #   could not be inverted. This may be a symptom of a non-identified model or due 
  #   to convergence issues. You can try to approximate standard errors by setting 
  #   approximate_SEs = TRUE at own risk.

mg_model1_rev_nlminb     <- mg_model1_rev     %>% runmodel(approximate_SEs = TRUE)
mg_model1_rev_std_nlminb <- mg_model1_rev_std %>% runmodel(approximate_SEs = TRUE)

# Inspect parameters

mg_parameters_path <- "./results/panel_gvar/multi_grp/parameters/"

dir.create(mg_parameters_path, recursive = TRUE)

sink(file = paste0(mg_parameters_path, "mg_model1_rev_nlminb_parameters.txt"))
mg_model1_rev_nlminb %>% parameters
sink()

# Some possibly odd parameters result. Given that all involve between-person
# effects, refit model using Cholesky decomposition to model such effects and do
# not interpret such effects.

  # For NEUTRAL
    # beta estimate > 1 (not odd because all corresponding PDCs are < 1; see below)
      # interfere        <- rr_ps_mean_rev  1.03  0.34   0.0025   4   7  81
    # omega_zeta_between estimate of -1 (odd because these are already partial correlations)
      # interfere_social -- anxious_freq    -1.0 0.014 < 0.0001   5   1  88
    # delta_zeta_between:
      # all estimates nonsignificant (but SEs not too big)

  # For FIFTY_FIFTY
    # omega_zeta_between estimate of -1:
      # interfere          -- anxious_sev   -1.0 0.032 < 0.0001   4   2 204
    # delta_zeta_between:
      # all estimates nonsignificant (but SEs not too big)
  
  # For POSITIVE
    # delta_zeta_between:
      # all estimates nonsignificant (but SEs not too big)

sum(abs(attr(mg_model1_rev_nlminb, "modelmatrices")$NEUTRAL$beta) > 1) == 1 # Unstandardized betas
sum(abs(attr(mg_model1_rev_nlminb, "modelmatrices")$NEUTRAL$PDC) > 1)  == 0 # Standardized to partial directed correlations

attr(mg_model1_rev_nlminb, "modelmatrices")$NEUTRAL$omega_zeta_between

sink(file = paste0(mg_parameters_path, "mg_model1_rev_std_nlminb_parameters.txt"))
mg_model1_rev_std_nlminb %>% parameters
sink()

# Some possibly odd parameters result, and again all involve between-person effects.
# Model such effects with Cholesky decomposition and do not interpret them.

# For NEUTRAL
  # omega_zeta_between estimate of -1 (odd because these are already partial correlations)
      # interfere_social --     anxious_freq   -1.0 0.013 < 0.0001   5   1  88
  # delta_zeta_between:
    # all estimates nonsignificant (but SEs not too big)

# For FIFTY_FIFTY
  # omega_zeta_between:
    # estimate of nearly -1
      # interfere        --      anxious_sev   -0.99 0.10 < 0.0001   4   2 204
    # many SEs greater than 1 (but not huge)
  # delta_zeta_between:
    # all estimates nonsignificant (but SEs not too big)

# For POSITIVE
  # delta_zeta_between:
    # all estimates nonsignificant (but SEs not too big)

attr(mg_model1_rev_std_nlminb, "modelmatrices")$FIFTY_FIFTY$omega_zeta_between

mg_model2_rev     <- panelgvar(net_dat_all_to_s6_wide_detrend,     vars = design_mat_w_rr_ps_mean_rev_full, estimator = "FIML", 
                               between_latent = "chol", groups = "cbmCondition")
mg_model2_rev_std <- panelgvar(net_dat_all_to_s6_wide_detrend_std, vars = design_mat_w_rr_ps_mean_rev_full, estimator = "FIML", 
                               between_latent = "chol", groups = "cbmCondition")

mg_model2_rev_nlminb     <- mg_model2_rev     %>% runmodel
mg_model2_rev_std_nlminb <- mg_model2_rev_std %>% runmodel

sink(file = paste0(mg_parameters_path, "mg_model2_rev_nlminb_parameters.txt"))
mg_model2_rev_nlminb %>% parameters
sink()

# Some possibly odd parameters result, but still only for between-person effects.
# Note: We did not need to approximate SEs when using Cholesky for between-person effects.

  # For NEUTRAL
    # beta estimate still > 1 (not odd because all corresponding PDCs are < 1; see below)
      # interfere            <- rr_ps_mean_rev        1.03     0.34   0.0025   4   7  81
    # lowertri_zeta_between SEs very large:   
      # rr_ns_mean       ~chol~ interfere_social     -0.25    35.84     0.99   6   5 108
      # rr_ps_mean_rev   ~chol~ interfere_social      0.27    37.19     0.99   7   5 109
      # rr_ns_mean       ~chol~ rr_ns_mean           0.023   379.13      1.0   6   6 110
      # rr_ps_mean_rev   ~chol~ rr_ns_mean          -0.025   402.27      1.0   7   6 111
      # rr_ps_mean_rev   ~chol~ rr_ps_mean_rev         ~ 0 17368.64      1.0   7   7 112

  # For FIFTY_FIFTY
    # lowertri_zeta_between SEs very large:
      # interfere_social ~chol~ interfere_social      -0.018  93.81      1.0   5   5 219
      # rr_ns_mean       ~chol~ interfere_social      -0.010 115.20      1.0   6   5 220
      # rr_ps_mean_rev   ~chol~ interfere_social     -0.0047  33.44      1.0   7   5 221
      # rr_ns_mean       ~chol~ rr_ns_mean           -0.0010 787.25      1.0   6   6 222
      # rr_ps_mean_rev   ~chol~ rr_ns_mean               ~ 0 388.39      1.0   7   6 223
      # rr_ps_mean_rev   ~chol~ rr_ps_mean_rev      -0.00016 544.83      1.0   7   7 224

  # For POSITIVE
    # lowertri_zeta_between SEs very large:
      # interfere_social   ~chol~        interfere     0.29   36.39     0.99   5   4 328
      # rr_ns_mean         ~chol~        interfere    -0.13   24.20      1.0   6   4 329
      # rr_ps_mean_rev     ~chol~        interfere     0.14    8.59     0.99   7   4 330
      # interfere_social   ~chol~ interfere_social    0.040  269.63      1.0   5   5 331
      # rr_ns_mean         ~chol~ interfere_social   -0.025  137.30      1.0   6   5 332
      # rr_ps_mean_rev     ~chol~ interfere_social    0.014   89.52      1.0   7   5 333
      # rr_ns_mean         ~chol~       rr_ns_mean -0.00023  741.00      1.0   6   6 334
      # rr_ps_mean_rev     ~chol~       rr_ns_mean  0.00010  911.82      1.0   7   6 335
      # rr_ps_mean_rev     ~chol~   rr_ps_mean_rev      ~ 0 6589.48      1.0   7   7 336

sum(abs(attr(mg_model2_rev_nlminb, "modelmatrices")$NEUTRAL$beta > 1)) == 1 # Unstandardized betas
sum(abs(attr(mg_model2_rev_nlminb, "modelmatrices")$NEUTRAL$PDC > 1))  == 0 # Standardized to partial directed correlations

sink(file = paste0(mg_parameters_path, "mg_model2_rev_std_nlminb_parameters.txt"))
mg_model2_rev_std_nlminb %>% parameters
sink()

# Some possibly odd parameters result, but still only for between-person effects.
# Note: We did not need to approximate SEs when using Cholesky for between-person effects.

  # For NEUTRAL
    # lowertri_zeta_between SEs very large:   
      # rr_ns_mean         ~chol~ interfere_social     -0.25      35.63     0.99   6   5 108
      # rr_ps_mean_rev     ~chol~ interfere_social      0.52      71.81     0.99   7   5 109
      # rr_ns_mean         ~chol~       rr_ns_mean    -0.033     265.66      1.0   6   6 110
      # rr_ps_mean_rev     ~chol~       rr_ns_mean     0.068     550.71      1.0   7   6 111
      # rr_ps_mean_rev     ~chol~   rr_ps_mean_rev       ~ 0  209858.52      1.0   7   7 112

  # For FIFTY_FIFTY
    # lowertri_zeta_between SEs very large:
      # interfere_social   ~chol~ interfere_social     0.053      32.42      1.0   5   5 219
      # rr_ns_mean         ~chol~ interfere_social     0.034      40.63      1.0   6   5 220
      # rr_ps_mean_rev     ~chol~ interfere_social     0.027      23.02      1.0   7   5 221
      # rr_ns_mean         ~chol~       rr_ns_mean   -0.0038     216.69      1.0   6   6 222
      # rr_ps_mean_rev     ~chol~       rr_ns_mean   -0.0015     169.46      1.0   7   6 223
      # rr_ps_mean_rev     ~chol~   rr_ps_mean_rev  -0.00020    1297.00      1.0   7   7 224

  # For POSITIVE
    # lowertri_zeta_between SEs very large:
      # interfere_social   ~chol~        interfere      0.27      31.00     0.99   5   4 328
      # rr_ns_mean         ~chol~        interfere     -0.12      21.29      1.0   6   4 329
      # rr_ps_mean_rev     ~chol~        interfere      0.26      14.41     0.99   7   4 330
      # interfere_social   ~chol~ interfere_social     0.093      91.21      1.0   5   5 331
      # rr_ns_mean         ~chol~ interfere_social    -0.062      46.07      1.0   6   5 332
      # rr_ps_mean_rev     ~chol~ interfere_social     0.065      60.73      1.0   7   5 333
      # rr_ns_mean         ~chol~       rr_ns_mean    0.0023      78.08      1.0   6   6 334
      # rr_ps_mean_rev     ~chol~       rr_ns_mean    0.0011     143.26      1.0   7   6 335
      # rr_ps_mean_rev     ~chol~   rr_ps_mean_rev       ~ 0 1300972.91      1.0   7   7 336

# TODO: Ask Epskamp if the lowertri_zeta_between estimates for "rr_ps_mean_rev" of 
# approximately 0 is a problem. Tried fixing it to zero for all groups, and all other 
# parameter estimates and model fit are nearly identical as those of "mg_model2_rev_std". 
# Ignore this for now.

mg_model2_rev_fix <- mg_model2_rev %>%
  fixpar("lowertri_zeta_between", "rr_ps_mean_rev", "rr_ps_mean_rev", group = 1) %>%
  fixpar("lowertri_zeta_between", "rr_ps_mean_rev", "rr_ps_mean_rev", group = 2) %>%
  fixpar("lowertri_zeta_between", "rr_ps_mean_rev", "rr_ps_mean_rev", group = 3)

mg_model2_rev_fix_nlminb <- mg_model2_rev_fix %>% runmodel

sink(file = paste0(mg_parameters_path, "mg_model2_rev_fix_nlminb_parameters.txt"))
mg_model2_rev_fix_nlminb %>% parameters
sink()





# Check fit. Chi-square sig and CFI and TLI below traditional guidelines, but
# RMSEA near traditional guidelines

mg_model1_rev_nlminb %>% print            # Says relative convergence (4)
mg_model1_rev_nlminb %>% fit

mg_model1_rev_std_nlminb %>% print        # Says relative convergence (4)
mg_model1_rev_std_nlminb %>% fit

mg_model2_rev_nlminb %>% print            # Says relative convergence (4)
mg_model2_rev_nlminb %>% fit

mg_model2_rev_std_nlminb %>% print        # Says relative convergence (4)
mg_model2_rev_std_nlminb %>% fit

mg_model2_rev_fix_nlminb %>% print        # Says relative convergence (4)
mg_model2_rev_fix_nlminb %>% fit

# ---------------------------------------------------------------------------- #
# Consider pruning and model search for multigroup model ----
# ---------------------------------------------------------------------------- #

# Specify estimation algorithm settings, using same as used for "dlvm1" model

alpha <- 0.01
adjust <- "none"
searchstrategy <- "modelsearch"

# Run estimation algorithm (prune step)

mg_model2_rev_nlminb_prune <- mg_model2_rev_nlminb %>% 
  prune(alpha = alpha, adjust = adjust, recursive = FALSE)

# Tried running model search strategy, but error says it is only implemented
# for single-group models at the moment

# if (searchstrategy == "stepup") {
#   mg_model2_rev_nlminb_prune <- mg_model2_rev_nlminb_prune %>%
#     stepup(alpha = alpha, criterion = "bic")
# } else if (searchstrategy == "modelsearch") {
#   mg_model2_rev_nlminb_prune <- mg_model2_rev_nlminb_prune %>%
#     modelsearch(prunealpha = alpha, addalpha = alpha)
# }

# Compare original and pruned models. Original model fits significantly better
# and is superior on AIC but worse on BIC.

comp_mg_model2_rev_nlminb <- compare(original = mg_model2_rev_nlminb,
                                     pruned   = mg_model2_rev_nlminb_prune)

comp_mg_model2_rev_nlminb$AIC[1] - comp_mg_model2_rev_nlminb$AIC[2]
comp_mg_model2_rev_nlminb$BIC[1] - comp_mg_model2_rev_nlminb$BIC[2]

# Fit of pruned model is even worse on traditional guidelines per RMSEA, CFI, and
# TLI (now .064, .74, and .70, respectively). Retain original model.

mg_model2_rev_nlminb_prune %>% print     # Says relative convergence (4)
mg_model2_rev_nlminb_prune %>% fit

# Also, saturated model is needed for uncertainty estimates (Williams, 2021). See
# comment for single-group model above for more on this.

# ---------------------------------------------------------------------------- #
# Check factor loadings and residual variances of multigroup model ----
# ---------------------------------------------------------------------------- #

# Confirm that factor loadings are set to identity matrix and that residual
# variances are set to 0

# Factor loadings

lambda_mg_model2_rev_nlminb <- getmatrix(mg_model2_rev_nlminb, "lambda")

# Residual variances

theta_mg_model2_rev_nlminb  <- getmatrix(mg_model2_rev_nlminb, "sigma_epsilon_within")

# Latent variance-covariance

psi_mg_model2_rev_nlminb    <- getmatrix(mg_model2_rev_nlminb, "sigma_zeta_within")

# ---------------------------------------------------------------------------- #
# Save models ----
# ---------------------------------------------------------------------------- #

save(model2_rev_nlminb,    file = "./results/panel_gvar/single_grp/model2_rev_nlminb.RData")
save(mg_model2_rev_nlminb, file = "./results/panel_gvar/multi_grp/mg_model2_rev_nlminb.RData")