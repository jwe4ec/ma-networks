# ---------------------------------------------------------------------------- #
# Plot Temporal Networks
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

pkgs <- c("psychonetrics", "qgraph")

groundhog.library(pkgs, groundhog_day)

# ---------------------------------------------------------------------------- #
# Import results ----
# ---------------------------------------------------------------------------- #

load("./results/panel_gvar/single_grp/model2_rev_nlminb.RData")
load("./results/panel_gvar/multi_grp/mg_model2_rev_nlminb.RData")

# ---------------------------------------------------------------------------- #
# Extract partial correlations ----
# ---------------------------------------------------------------------------- #

# Extract partial temporal, contemporaneous, and between-person correlations. Per prior 
# script, ignore between-person effects ("omega_zeta_between" was not modeled)

sg_tem <- getmatrix(model2_rev_nlminb, "PDC")                # Partial directed correlations
sg_con <- getmatrix(model2_rev_nlminb, "omega_zeta_within")  # Partial contemporaneous correlations
# sg_btw <- getmatrix(model2_rev_nlminb, "omega_zeta_between") # Partial between-person correlations

  # Note: Another way to get these matrices is below

# attr(model2_rev_nlminb, "modelmatrices")$fullsample$PDC
# attr(model2_rev_nlminb, "modelmatrices")$fullsample$omega_zeta_within
# attr(model2_rev_nlminb, "modelmatrices")$fullsample$omega_zeta_between

mg_tem <- getmatrix(mg_model2_rev_nlminb, "PDC")
mg_con <- getmatrix(mg_model2_rev_nlminb, "omega_zeta_within")
# mg_btw <- getmatrix(mg_model2_rev_nlminb, "omega_zeta_between")

# ---------------------------------------------------------------------------- #
# Extract thresholded partial correlations ----
# ---------------------------------------------------------------------------- #

# Threshold at .01 (Freichel et al., 2023; Isvoanu & Epskamp, 2021) and .05

sg_tem_thres_a05 <- getmatrix(model2_rev_nlminb,    "PDC",                threshold = TRUE, alpha = .05)
sg_con_thres_a05 <- getmatrix(model2_rev_nlminb,    "omega_zeta_within",  threshold = TRUE, alpha = .05)
# sg_btw_thres_a05 <- getmatrix(model2_rev_nlminb,    "omega_zeta_between", threshold = TRUE, alpha = .05)

mg_tem_thres_a05 <- getmatrix(mg_model2_rev_nlminb, "PDC",                threshold = TRUE, alpha = .05)
mg_con_thres_a05 <- getmatrix(mg_model2_rev_nlminb, "omega_zeta_within",  threshold = TRUE, alpha = .05)
# mg_btw_thres_a05 <- getmatrix(model2_rev_nlminb,    "omega_zeta_between", threshold = TRUE, alpha = .05)

sg_tem_thres_a01 <- getmatrix(model2_rev_nlminb,    "PDC",                threshold = TRUE, alpha = .01)
sg_con_thres_a01 <- getmatrix(model2_rev_nlminb,    "omega_zeta_within",  threshold = TRUE, alpha = .01)
# sg_btw_thres_a01 <- getmatrix(model2_rev_nlminb,    "omega_zeta_between", threshold = TRUE, alpha = .01)

mg_tem_thres_a01 <- getmatrix(mg_model2_rev_nlminb, "PDC",                threshold = TRUE, alpha = .01)
mg_con_thres_a01 <- getmatrix(mg_model2_rev_nlminb, "omega_zeta_within",  threshold = TRUE, alpha = .01)
# mg_btw_thres_a01 <- getmatrix(model2_rev_nlminb,    "omega_zeta_between", threshold = TRUE, alpha = .01)

# ---------------------------------------------------------------------------- #
# Plot partial correlations ----
# ---------------------------------------------------------------------------- #

# Compute max value (ignore between-person effects)

sg_max           <- max(c(abs(sg_tem),           abs(sg_con)))
sg_max_thres_a05 <- max(c(abs(sg_tem_thres_a05), abs(sg_con_thres_a05)))
sg_max_thres_a01 <- max(c(abs(sg_tem_thres_a01), abs(sg_con_thres_a01)))

round(sg_max,           2) == .37
round(sg_max_thres_a05, 2) == .37
round(sg_max_thres_a01, 2) == .37

  # For multigroup model, compute max value per group and across groups

mg_max           <- list(POSITIVE    = max(c(abs(mg_tem$POSITIVE), 
                                             abs(mg_con$POSITIVE))),
                         FIFTY_FIFTY = max(c(abs(mg_tem$FIFTY_FIFTY), 
                                             abs(mg_con$FIFTY_FIFTY))),
                         NEUTRAL     = max(c(abs(mg_tem$NEUTRAL), 
                                             abs(mg_con$NEUTRAL))))
mg_max_thres_a05 <- list(POSITIVE    = max(c(abs(mg_tem_thres_a05$POSITIVE), 
                                             abs(mg_con_thres_a05$POSITIVE))),
                         FIFTY_FIFTY = max(c(abs(mg_tem_thres_a05$FIFTY_FIFTY), 
                                             abs(mg_con_thres_a05$FIFTY_FIFTY))),
                         NEUTRAL     = max(c(abs(mg_tem_thres_a05$NEUTRAL), 
                                             abs(mg_con_thres_a05$NEUTRAL))))
mg_max_thres_a01 <- list(POSITIVE    = max(c(abs(mg_tem_thres_a01$POSITIVE), 
                                             abs(mg_con_thres_a01$POSITIVE))),
                         FIFTY_FIFTY = max(c(abs(mg_tem_thres_a01$FIFTY_FIFTY), 
                                             abs(mg_con_thres_a01$FIFTY_FIFTY))),
                         NEUTRAL     = max(c(abs(mg_tem_thres_a01$NEUTRAL), 
                                             abs(mg_con_thres_a01$NEUTRAL))))

mg_max$overall           <- max(unlist(mg_max))
mg_max_thres_a05$overall <- max(unlist(mg_max_thres_a05))
mg_max_thres_a01$overall <- max(unlist(mg_max_thres_a01))

round(mg_max$overall,           2) == .64
round(mg_max_thres_a05$overall, 2) == .64
round(mg_max_thres_a01$overall, 2) == .64

# Define function to plot networks for single-group model (sg) or for one group 
# of multigroup (mg) model (ignore between-person effects)

plot_networks <- function(model, model_type, tem, con, max, mg_max_type, mg_group, thres) {
  # Get model object name
  
  model_name <- deparse(substitute(model))

  # Get node variables and rename as labels
  
  labels <- row.names(attr(model, "extramatrices")$design)
  
  labels[labels == "anxious_freq"]     <- "Anx.\nFreq."
  labels[labels == "anxious_sev"]      <- "Anx.\nSev."
  labels[labels == "avoid"]            <- "Sit.\nAvoid"
  labels[labels == "interfere"]        <- "Work\nImp."
  labels[labels == "interfere_social"] <- "Soc.\nImp."
  labels[labels == "rr_ns_mean"]       <- "Neg.\nBias"
  labels[labels == "rr_ps_mean_rev"]   <- "Lack\nof Pos.\nBias"
  
  # Define plotting options
  
    # Include edge labels only for thresholded networks (too cluttered in saturated networks)
  
  if (thres == "full") {
    edge_labels <- FALSE
  } else {
    edge_labels <- TRUE
  }
  
  if (model_type == "sg") {
    tem_to_plot <- tem
    con_to_plot <- con
    # btw_to_plot <- btw
    
    max_for_plot <- max
    
    tem_title <- "Temporal"
    con_title <- "Contemporaneous"
    
    plots_path <- "./results/panel_gvar/single_grp/plots/"
    
    tem_plot_filename  <- paste0(plots_path, model_name, "_tem_plot_",  thres)
    con_plot_filename  <- paste0(plots_path, model_name, "_con_plot_",  thres)
    all_plots_filename <- paste0(plots_path, model_name, "_all_plots_", thres)
  } else if (model_type == "mg") {
    tem_to_plot <- tem[[mg_group]]
    con_to_plot <- con[[mg_group]]
    # btw_to_plot <- btw[[mg_group]]
    
    if (mg_max_type == "per_group") {
      max_for_plot <- max[[mg_group]]
    } else if (mg_max_type == "overall") {
      max_for_plot <- max[["overall"]]
    }
    
    if (mg_group == "POSITIVE") {
      mg_group_title <- "Positive CBM-I"
    } else if (mg_group == "FIFTY_FIFTY") {
      mg_group_title <- "50-50 CBM-I"
    } else if (mg_group == "NEUTRAL") {
      mg_group_title <- "No-Training"
    }
    
    tem_title <- paste0(mg_group_title, ": Temporal")
    con_title <- paste0(mg_group_title, ": Contemporaneous")
    
    plots_path <- "./results/panel_gvar/multi_grp/plots/"
    
    tem_plot_filename  <- paste0(plots_path, model_name, "_", mg_group, "_tem_plot_",  thres)
    con_plot_filename  <- paste0(plots_path, model_name, "_", mg_group, "_con_plot_",  thres)
    all_plots_filename <- paste0(plots_path, model_name, "_", mg_group, "_all_plots_", thres)
  }
  
  # Plot circle graphs (ignore between-person effects)
  
  tem_plot <- qgraph(tem_to_plot,
                     edge.labels = edge_labels,
                     edge.label.color = "black",
                     edge.label.margin = .01,
                     edge.label.cex = 1.6,
                     layout = "circle", 
                     labels = labels, 
                     theme = "colorblind",
                     asize = 7, 
                     vsize = 11, 
                     label.cex = c(rep(.8, 6), .7), 
                     mar = rep(6, 4), 
                     title = tem_title,
                     label.scale = FALSE,
                     maximum = max_for_plot,
                     esize = 10)
  
  con_plot <- qgraph(con_to_plot, 
                     edge.labels = edge_labels,
                     edge.label.color = "black",
                     edge.label.margin = .01,
                     edge.label.cex = 1.6,
                     layout = "circle", 
                     labels = labels, 
                     theme = "colorblind",
                     vsize = 11, 
                     label.cex = c(rep(.8, 6), .7), 
                     mar = rep(6, 4), 
                     title = con_title,
                     label.scale = FALSE,
                     maximum = max_for_plot,
                     esize = 10)
  
  # Export plots
  
  dir.create(plots_path, showWarnings = FALSE)
  
  # qgraph(tem_plot,
  #        filetype = "pdf",
  #        filename = tem_plot_filename)
  # qgraph(con_plot,
  #        filetype = "pdf",
  #        filename = con_plot_filename)
  
  pdf(paste0(all_plots_filename, ".pdf"), width = 8, height = 4)
  layout(t(1:2))
  qgraph(tem_plot)
  box("figure")
  qgraph(con_plot)
  box("figure")
  dev.off()
}

# Run function

plot_networks(model2_rev_nlminb,    "sg", sg_tem,           sg_con,           sg_max,           NULL,      NULL,          "full")
plot_networks(mg_model2_rev_nlminb, "mg", mg_tem,           mg_con,           mg_max,           "overall", "POSITIVE",    "full")
plot_networks(mg_model2_rev_nlminb, "mg", mg_tem,           mg_con,           mg_max,           "overall", "FIFTY_FIFTY", "full")
plot_networks(mg_model2_rev_nlminb, "mg", mg_tem,           mg_con,           mg_max,           "overall", "NEUTRAL",     "full")

plot_networks(model2_rev_nlminb,    "sg", sg_tem_thres_a05, sg_con_thres_a05, sg_max_thres_a05, NULL,      NULL,          "thres_a05")
plot_networks(mg_model2_rev_nlminb, "mg", mg_tem_thres_a05, mg_con_thres_a05, mg_max_thres_a05, "overall", "POSITIVE",    "thres_a05")
plot_networks(mg_model2_rev_nlminb, "mg", mg_tem_thres_a05, mg_con_thres_a05, mg_max_thres_a05, "overall", "FIFTY_FIFTY", "thres_a05")
plot_networks(mg_model2_rev_nlminb, "mg", mg_tem_thres_a05, mg_con_thres_a05, mg_max_thres_a05, "overall", "NEUTRAL",     "thres_a05")

plot_networks(model2_rev_nlminb,    "sg", sg_tem_thres_a01, sg_con_thres_a01, sg_max_thres_a01, NULL,      NULL,          "thres_a01")
plot_networks(mg_model2_rev_nlminb, "mg", mg_tem_thres_a01, mg_con_thres_a01, mg_max_thres_a01, "overall", "POSITIVE",    "thres_a01")
plot_networks(mg_model2_rev_nlminb, "mg", mg_tem_thres_a01, mg_con_thres_a01, mg_max_thres_a01, "overall", "FIFTY_FIFTY", "thres_a01")
plot_networks(mg_model2_rev_nlminb, "mg", mg_tem_thres_a01, mg_con_thres_a01, mg_max_thres_a01, "overall", "NEUTRAL",     "thres_a01")

# ---------------------------------------------------------------------------- #
# Create tables ----
# ---------------------------------------------------------------------------- #

# Define function to create table for single-group model (sg) or for one group 
# of multigroup (mg) model (ignore between-person effects)

create_tbls <- function(model, model_type, tem, con, mg_group) {
  # Get model object name
  
  model_name <- deparse(substitute(model))
  
  # Get node variables and rename as labels
  
  labels <- row.names(attr(model, "extramatrices")$design)
  
  labels[labels == "anxious_freq"]     <- "Anx. Freq."
  labels[labels == "anxious_sev"]      <- "Anx. Sev."
  labels[labels == "avoid"]            <- "Sit. Avoid"
  labels[labels == "interfere"]        <- "Work Imp."
  labels[labels == "interfere_social"] <- "Soc. Imp."
  labels[labels == "rr_ns_mean"]       <- "Neg. Bias"
  labels[labels == "rr_ps_mean_rev"]   <- "Lack of Pos. Bias"

  # Define partial correlation sources
  
  if (model_type == "sg") {
    tem_tbl <- sg_tem
    con_tbl <- sg_con
    # btw_tbl <- sg_btw
  } else if (model_type == "mg") {
    tem_tbl <- mg_tem[[mg_group]]
    con_tbl <- mg_con[[mg_group]]
    # btw_tbl <- mg_btw[[mg_group]]
  }
  
  # Create temporal table (partial directed correlations)
  
  tem_tbl[tem_tbl == 0] <- NA
  rownames(tem_tbl) <- colnames(tem_tbl) <- labels
  tem_tbl <- round(tem_tbl, 2)
  
  # Create contemporaneous table (partial correlations in lower tri, marginal correlations in upper tri)
  
  if (model_type == "sg") {
    con_marg_cors <- cov2cor(getmatrix(model, "sigma_zeta_within"))
  } else if (model_type == "mg") {
    con_marg_cors <- cov2cor(getmatrix(model, "sigma_zeta_within", mg_group))
  }
  
  con_tbl[upper.tri(con_tbl)] <- con_marg_cors[upper.tri(con_marg_cors)]
  
  con_tbl[con_tbl == 0] <- NA
  
  rownames(con_tbl) <- colnames(con_tbl) <- labels
  con_tbl <- round(con_tbl, 2)
  
  # Create between-person table (partial correlations in lower tri, marginal correlations in upper tri)
  
  # if (model_type == "sg") {
  #   btw_marg_cors <- cov2cor(getmatrix(model, "sigma_zeta_between"))
  # } else if (model_type == "mg") {
  #   btw_marg_cors <- cov2cor(getmatrix(model, "sigma_zeta_between", mg_group))
  # }
  # 
  # btw_tbl[upper.tri(btw_tbl)] <- btw_marg_cors[upper.tri(btw_marg_cors)]
  # 
  # btw_tbl[btw_tbl == 0] <- NA
  # 
  # rownames(btw_tbl) <- colnames(btw_tbl) <- labels
  # btw_tbl <- round(btw_tbl, 2)
  
  # Export tables to CSV (ignore between-person effects)
  
  if (model_type == "sg") {
    tbls_path <- "./results/panel_gvar/single_grp/tbls/"
    
    filename <- model_name
  } else if (model_type == "mg") {
    tbls_path <- "./results/panel_gvar/multi_grp/tbls/"
    
    filename <- paste0(model_name, "_", mg_group)
  }
  
  dir.create (tbls_path, showWarnings = FALSE)
  
  sink(file = paste0(tbls_path, filename, ".csv"))
  
  print(paste("Temporal (partial directed correlations: cross-lagged from row to column; autoregressive on diagonal):"))
  write.csv(tem_tbl)
  
  print(paste("Contemporaneous (partial correlations in lower; marginal correlations in upper):"))
  write.csv(con_tbl)
  
  print(paste("Between-Subjects (partial correlations in lower; marginal correlations in upper):"))
  print("Not interpreted due to estimation issue")
  
  sink()
}

# Run function

create_tbls(model2_rev_nlminb,    "sg", sg_tem, sg_con, NULL)
create_tbls(mg_model2_rev_nlminb, "mg", mg_tem, mg_con, "POSITIVE")
create_tbls(mg_model2_rev_nlminb, "mg", mg_tem, mg_con, "FIFTY_FIFTY")
create_tbls(mg_model2_rev_nlminb, "mg", mg_tem, mg_con, "NEUTRAL")

# ---------------------------------------------------------------------------- #
# Create confidence interval plots ----
# ---------------------------------------------------------------------------- #

# TODO: Resolve error for "PDC"

# CIplot(model2_rev_nlminb, "PDC")
# CIplot(model2_rev_nlminb, "omega_zeta_within")




# TODO: Does not seem to work for multigroup model automatically

# CIplot(mg_model2_rev_nlminb, "PDC")
# CIplot(mg_model2_rev_nlminb, "omega_zeta_within")




