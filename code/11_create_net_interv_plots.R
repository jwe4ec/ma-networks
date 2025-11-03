# ---------------------------------------------------------------------------- #
# Create Network Intervention Plots
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

groundhog.library("qgraph", groundhog_day)

# ---------------------------------------------------------------------------- #
# Import results ----
# ---------------------------------------------------------------------------- #

# TODO (are these needed?) Network results





net_interv_path <- file.path("results", "net_interv")

## For RR network

res_rr_pos_neu_lw_per_wave     <- readRDS(file.path(net_interv_path, "res_rr_pos_neu_lw_per_wave.rds"))
res_rr_pos_neu_lw_across_waves <- readRDS(file.path(net_interv_path, "res_rr_pos_neu_lw_across_waves.rds"))

res_rr_pos_fif_lw_per_wave     <- readRDS(file.path(net_interv_path, "res_rr_pos_fif_lw_per_wave.rds"))
res_rr_pos_fif_lw_across_waves <- readRDS(file.path(net_interv_path, "res_rr_pos_fif_lw_across_waves.rds"))

## For BBSIQ network

res_bb_pos_neu_lw_per_wave     <- readRDS(file.path(net_interv_path, "res_bb_pos_neu_lw_per_wave.rds"))
res_bb_pos_neu_lw_across_waves <- readRDS(file.path(net_interv_path, "res_bb_pos_neu_lw_across_waves.rds"))

res_bb_pos_fif_lw_per_wave     <- readRDS(file.path(net_interv_path, "res_bb_pos_fif_lw_per_wave.rds"))
res_bb_pos_fif_lw_across_waves <- readRDS(file.path(net_interv_path, "res_bb_pos_fif_lw_across_waves.rds"))

# Network stability results (i.e., nonparametric bootstrap samples)

net_interv_stab_path <- file.path(net_interv_path, "stab")

## For RR network

res_bs_rr_net_files <- c("res_rr_pos_neu_lw_per_wave_pre_bs.rds", 
                         "res_rr_pos_neu_lw_per_wave_session3_bs.rds", 
                         "res_rr_pos_neu_lw_per_wave_session6_bs.rds", 
                         "res_rr_pos_neu_lw_across_waves_pre_bs.rds", 
                         "res_rr_pos_neu_lw_across_waves_session3_bs.rds", 
                         "res_rr_pos_neu_lw_across_waves_session6_bs.rds", 
                         "res_rr_pos_fif_lw_per_wave_pre_bs.rds", 
                         "res_rr_pos_fif_lw_per_wave_session3_bs.rds", 
                         "res_rr_pos_fif_lw_per_wave_session6_bs.rds", 
                         "res_rr_pos_fif_lw_across_waves_pre_bs.rds", 
                         "res_rr_pos_fif_lw_across_waves_session3_bs.rds", 
                         "res_rr_pos_fif_lw_across_waves_session6_bs.rds")

res_bs_rr_net_ls <- lapply(res_bs_rr_net_files, function(f) readRDS(file.path(net_interv_stab_path, f)))
names(res_bs_rr_net_ls) <- tools::file_path_sans_ext(res_bs_rr_net_files)

## For BBSIQ network

res_bs_bb_net_files <- c("res_bb_pos_neu_lw_per_wave_pre_bs.rds", 
                         "res_bb_pos_neu_lw_per_wave_session3_bs.rds", 
                         "res_bb_pos_neu_lw_per_wave_session6_bs.rds", 
                         "res_bb_pos_neu_lw_across_waves_pre_bs.rds", 
                         "res_bb_pos_neu_lw_across_waves_session3_bs.rds", 
                         "res_bb_pos_neu_lw_across_waves_session6_bs.rds", 
                         "res_bb_pos_fif_lw_per_wave_pre_bs.rds", 
                         "res_bb_pos_fif_lw_per_wave_session3_bs.rds", 
                         "res_bb_pos_fif_lw_per_wave_session6_bs.rds", 
                         "res_bb_pos_fif_lw_across_waves_pre_bs.rds", 
                         "res_bb_pos_fif_lw_across_waves_session3_bs.rds", 
                         "res_bb_pos_fif_lw_across_waves_session6_bs.rds")

res_bs_bb_net_ls <- lapply(res_bs_bb_net_files, function(f) readRDS(file.path(net_interv_stab_path, f)))
names(res_bs_bb_net_ls) <- tools::file_path_sans_ext(res_bs_bb_net_files)

# ---------------------------------------------------------------------------- #
# Compute quantiles of bootstrap samples for alpha levels of .05 and .01 ----
# ---------------------------------------------------------------------------- #

# Define function to compute quantiles for alphas of .05 and .01. given that resample() 
# in "compute_net_interv_stability.R" computed quantiles for alpha of .10

compute_quantiles <- function(res_bs, alpha) {
  quantiles <- c(alpha / 2, 1 - alpha / 2)
  
  if (alpha == 0.10) {
    alpha_char <- "a10"
  } else if (alpha == 0.05) {
    alpha_char <- "a05"
  } else if (alpha == 0.01) {
    alpha_char <- "a01"
  }
  
  quantiles_array_name <- paste0("bootQuantiles_", alpha_char)
  
  num_nodes <- ncol(res_bs$call$data)
  
  res_bs[[quantiles_array_name]] <- array(NA, dim = c(num_nodes, num_nodes, 2))
  
  res_bs[[quantiles_array_name]][,,1] <- apply(res_bs$bootParameters, c(1, 2),
                                               function(ij_element) quantile(ij_element, probs = quantiles[1]))
  res_bs[[quantiles_array_name]][,,2] <- apply(res_bs$bootParameters, c(1, 2), 
                                               function(ij_element) quantile(ij_element, probs = quantiles[2]))
  
  return(res_bs)
}

# Run function

res_bs_rr_net_ls <- lapply(res_bs_rr_net_ls, compute_quantiles, 0.05)
res_bs_rr_net_ls <- lapply(res_bs_rr_net_ls, compute_quantiles, 0.01)

res_bs_bb_net_ls <- lapply(res_bs_bb_net_ls, compute_quantiles, 0.05)
res_bs_bb_net_ls <- lapply(res_bs_bb_net_ls, compute_quantiles, 0.01)

# ---------------------------------------------------------------------------- #
# Create edge inclusion matrices for alpha levels of .05 and .01  ----
# ---------------------------------------------------------------------------- #

# Define function to create matrix indicating which edges to retain after thresholding

create_edge_include_mat <- function(res_bs) {
  # Identify manually computed quantiles and number of nodes
  
  quantiles_array_names <- grep("bootQuantiles_a", names(res_bs), value = TRUE)
  
  num_nodes <- ncol(res_bs$call$data)
  
  for (a in 1:length(quantiles_array_names)) {
    quantiles_array_name <- quantiles_array_names[a]
    quantiles_array      <- res_bs[[quantiles_array_name]]
    
    # Create name for matrix of edges to include at given threshold
    
    edge_include_mat_name <- sub("bootQuantiles", "edge_include_thres", quantiles_array_name)
    
    # Create matrix
    
    mat <- matrix(NA, num_nodes, num_nodes)
    diag(mat) <- 0
    
    mat[quantiles_array[,,1] < 0 & quantiles_array[,,2] > 0]     <- 0
    
    mat[(quantiles_array[,,1] > 0 & quantiles_array[,,2] > 0) |
          (quantiles_array[,,1] < 0 & quantiles_array[,,2] < 0)] <- 1
    
    res_bs[[edge_include_mat_name]] <- mat
  }
  
  return(res_bs)
}

# Run function

res_bs_rr_net_ls <- lapply(res_bs_rr_net_ls, create_edge_include_mat)
res_bs_rr_net_ls <- lapply(res_bs_rr_net_ls, create_edge_include_mat)

res_bs_bb_net_ls <- lapply(res_bs_bb_net_ls, create_edge_include_mat)
res_bs_bb_net_ls <- lapply(res_bs_bb_net_ls, create_edge_include_mat)

# Define function to extract edge include matrices into list for each model type

extract_edge_include_mats <- function(res_bs_ls, model_type) {
  res_bs_pre <- res_bs_ls[[paste0(model_type, "_pre_bs")]]
  res_bs_s3  <- res_bs_ls[[paste0(model_type, "_session3_bs")]]
  res_bs_s6  <- res_bs_ls[[paste0(model_type, "_session6_bs")]]
  
  edge_include_mats_pre <- res_bs_pre[grep("edge_include_thres_", names(res_bs_pre), value = TRUE)]
  edge_include_mats_s3  <- res_bs_s3[grep("edge_include_thres_",  names(res_bs_s3),  value = TRUE)]
  edge_include_mats_s6  <- res_bs_s6[grep("edge_include_thres_",  names(res_bs_s6),  value = TRUE)]
  
  names(edge_include_mats_pre) <- sub("edge_include_", "", names(edge_include_mats_pre))
  names(edge_include_mats_s3)  <- sub("edge_include_", "", names(edge_include_mats_s3))
  names(edge_include_mats_s6)  <- sub("edge_include_", "", names(edge_include_mats_s6))
  
  ls <- list(PRE      = edge_include_mats_pre,
             SESSION3 = edge_include_mats_s3,
             SESSION6 = edge_include_mats_s6)
  
  return(ls)
}

# TODO (check below): Run function

rr_pos_neu_lw_per_wave_edge_include     <- extract_edge_include_mats(res_bs_rr_net_ls, "res_rr_pos_neu_lw_per_wave")
rr_pos_neu_lw_across_waves_edge_include <- extract_edge_include_mats(res_bs_rr_net_ls, "res_rr_pos_neu_lw_across_waves")
rr_pos_fif_lw_per_wave_edge_include     <- extract_edge_include_mats(res_bs_rr_net_ls, "res_rr_pos_fif_lw_per_wave")
rr_pos_fif_lw_across_waves_edge_include <- extract_edge_include_mats(res_bs_rr_net_ls, "res_rr_pos_fif_lw_across_waves")





# ---------------------------------------------------------------------------- #
# Create thresholded weighted adjacency matrices at alpha levels of .05 and .01  ----
# ---------------------------------------------------------------------------- #

# Define function to create thresholded weighted adjacency matrices

create_wadj_thres <- function(res, fit4_edge_include) {
  waves <- c("PRE", "SESSION3", "SESSION6")
  thres_names <- c("thres_a05", "thres_a01")
  
  for (wave in waves) {
    for (thres_name in thres_names) {
      res[[wave]][["fit4"]][["pairwise"]][[paste0("wadj_", thres_name)]] <-
        res[[wave]][["fit4"]][["pairwise"]][["wadj"]] * fit4_edge_include[[wave]][[thres_name]]
      
      res[[wave]][["fit4"]][["pairwise"]][[paste0("signs_", thres_name)]] <- res[[wave]][["fit4"]][["pairwise"]][["signs"]]
      
      res[[wave]][["fit4"]][["pairwise"]][[paste0("signs_", thres_name)]][fit4_edge_include[[wave]][[thres_name]] == 0] <- NA
    }
  }
  
  return(res)
}

# Run function

res_rev_pos_neu_lw_per_wave     <- create_wadj_thres(res_rev_pos_neu_lw_per_wave,     rev_pos_neu_lw_per_wave_fit4_edge_include)
res_rev_pos_neu_lw_across_waves <- create_wadj_thres(res_rev_pos_neu_lw_across_waves, rev_pos_neu_lw_across_waves_fit4_edge_include)
res_rev_pos_fif_lw_per_wave     <- create_wadj_thres(res_rev_pos_fif_lw_per_wave,     rev_pos_fif_lw_per_wave_fit4_edge_include)
res_rev_pos_fif_lw_across_waves <- create_wadj_thres(res_rev_pos_fif_lw_across_waves, rev_pos_fif_lw_across_waves_fit4_edge_include)

# ---------------------------------------------------------------------------- #
# Compute maximum edge weight across all network intervention analyses ----
# ---------------------------------------------------------------------------- #

fit4_max_overall <- max(abs(res_rev_pos_neu_lw_per_wave$PRE$fit4$pairwise$wadj),
                        abs(res_rev_pos_neu_lw_per_wave$SESSION3$fit4$pairwise$wadj),
                        abs(res_rev_pos_neu_lw_per_wave$SESSION6$fit4$pairwise$wadj),
                        abs(res_rev_pos_neu_lw_across_waves$PRE$fit4$pairwise$wadj),
                        abs(res_rev_pos_neu_lw_across_waves$SESSION3$fit4$pairwise$wadj),
                        abs(res_rev_pos_neu_lw_across_waves$SESSION6$fit4$pairwise$wadj),
                        abs(res_rev_pos_fif_lw_per_wave$PRE$fit4$pairwise$wadj),
                        abs(res_rev_pos_fif_lw_per_wave$SESSION3$fit4$pairwise$wadj),
                        abs(res_rev_pos_fif_lw_per_wave$SESSION6$fit4$pairwise$wadj),
                        abs(res_rev_pos_fif_lw_across_waves$PRE$fit4$pairwise$wadj),
                        abs(res_rev_pos_fif_lw_across_waves$SESSION3$fit4$pairwise$wadj),
                        abs(res_rev_pos_fif_lw_across_waves$SESSION6$fit4$pairwise$wadj))

round(fit4_max_overall, 2) == .71

# ---------------------------------------------------------------------------- #
# Create plots ----
# ---------------------------------------------------------------------------- #

# Define function to plot networks at baseline, Session 3, and Session 6

create_plots <- function(res, fit_name, thres_name, max_type, fit4_max_overall) {
  res_name <- deparse(substitute(res))
  
  if (max_type == "per_fit_across_waves") {
    # Compute maximum edge magnitude across waves
    
    fit_bl <- res$PRE[[fit_name]]
    fit_s3 <- res$SESSION3[[fit_name]]
    fit_s6 <- res$SESSION6[[fit_name]]
    
    if (fit_name != "fit4" | (fit_name == "fit4" & thres_name == "none")) {
      wadj_bl <- fit_bl$pairwise$wadj
      wadj_s3 <- fit_s3$pairwise$wadj
      wadj_s6 <- fit_s6$pairwise$wadj
    } else if (fit_name == "fit4" & thres_name == "thres_a05") {
      wadj_bl <- fit_bl$pairwise$wadj_thres_a05
      wadj_s3 <- fit_s3$pairwise$wadj_thres_a05
      wadj_s6 <- fit_s6$pairwise$wadj_thres_a05
    } else if (fit_name == "fit4" & thres_name == "thres_a01") {
      wadj_bl <- fit_bl$pairwise$wadj_thres_a01
      wadj_s3 <- fit_s3$pairwise$wadj_thres_a01
      wadj_s6 <- fit_s6$pairwise$wadj_thres_a01
    }
    
    max <- max(c(abs(wadj_bl), abs(wadj_s3), abs(wadj_s6)))
  } else if (max_type == "overall" & fit_name == "fit4") {
    # Use maximum edge weight across all network intervention analyses (see above)
    
    max <- fit4_max_overall
  } else if (max_type == "overall" & fit_name != "fit4") {
    stop(paste0("Overall maximum has been computed only for fit4, not for ", fit_name))
  }
  
  # Create plot for each wave
  
  waves       <- c("PRE", "SESSION3", "SESSION6")
  wave_titles <- c("Baseline", "Session 3", "Session 6")
  
  plots <- vector("list", length(waves))
  names(plots) <- waves
  
  for (i in 1:length(waves)) {
    res_wave <- res[[waves[i]]]
    
    vars <- res_wave$vars
    wave <- res_wave$wave
    wave_title <- wave_titles[i]
    
    fit  <- res_wave[[fit_name]]
    
    if (fit_name != "fit4" | (fit_name == "fit4" & thres_name == "none")) {
      wadj <- fit$pairwise$wadj
    } else if (fit_name == "fit4" & thres_name == "thres_a05") {
      wadj <- fit$pairwise$wadj_thres_a05
    } else if (fit_name == "fit4" & thres_name == "thres_a01") {
      wadj <- fit$pairwise$wadj_thres_a01
    }
    
    labels <- sub(paste0(".", wave), "", vars)
    
    labels[labels %in% c("positive_vs_neutral",
                         "positive_vs_fifty_fifty")] <- "Pos.\nCBM-I"
    labels[labels == "anxious_freq"]                 <- "Anx.\nFreq."
    labels[labels == "anxious_sev"]                  <- "Anx.\nSev."
    labels[labels == "avoid"]                        <- "Sit.\nAvoid"
    labels[labels == "interfere"]                    <- "Work\nImp."
    labels[labels == "interfere_social"]             <- "Soc.\nImp."
    labels[labels == "rr_ns_mean"]                   <- "Neg.\nBias"
    labels[labels == "rr_ps_mean_rev"]               <- "Lack\nof Pos.\nBias"
    
    # Change edge colors to match those in temporal plots (see first elements of vectors here:
    # https://github.com/SachaEpskamp/qgraph/blob/9b70cd438ee14b2c51e8030b3d5100d07a553c28/R/qgraph.R#L965)
    
    edge_colors <- fit$pairwise$edgecolor_cb
    edge_colors[edge_colors == "darkblue"] <- "#0000D5"
    edge_colors[edge_colors == "red"]      <- "#BF0000"
    
    # Include edge labels only for thresholded networks (too cluttered in saturated networks)
    
    if (thres_name == "none") {
      edge_labels <- FALSE
    } else {
      edge_labels <- TRUE
    }
    
    # Make edge labels for certain models smaller due to overlapping positions
    
    if (res_name == "res_rev_pos_fif_lw_across_waves" |
        (res_name == "res_rev_pos_fif_lw_per_wave" & thres_name == "thres_a05")) {
      edge_label_cex <- 1.6
    } else {
      edge_label_cex <- 2
    }
    
    # TODO: Find better way to make negative edges dashed ("lty" below makes them
    # dashed, but the spacing between dashes increases as the magnitude of the edge
    # weight increases, making the plot difficult to read)
    
    plots[[waves[i]]] <- 
      qgraph(wadj,
             edge.color = edge_colors,
             edge.labels = edge_labels,
             edge.label.color = "black",
             edge.label.margin = .01,
             edge.label.cex = edge_label_cex,
             # lty = fit$pairwise$edge_lty,     # TODO
             layout = "circle",
             labels = labels,
             theme = "colorblind",
             asize = 7, 
             vsize = 15, 
             shape = c("square", rep("circle", 7)),
             label.cex = c(rep(1.1, 7), .9), 
             mar = rep(4, 4), 
             title = bquote(paste(.(wave_title), " (", italic("n"), " = ", 
                                  .(fit$call$n), ")")),
             title.cex = 1.4,
             label.scale = FALSE,
             maximum = max)
  }
  
  return(plots)
}

# Run function

plots_rev_pos_neu_lw_per_wave_fit4               <- create_plots(res_rev_pos_neu_lw_per_wave,     "fit4", "none",      "overall", fit4_max_overall)
plots_rev_pos_neu_lw_per_wave_fit4_thres_a05     <- create_plots(res_rev_pos_neu_lw_per_wave,     "fit4", "thres_a05", "overall", fit4_max_overall)
plots_rev_pos_neu_lw_per_wave_fit4_thres_a01     <- create_plots(res_rev_pos_neu_lw_per_wave,     "fit4", "thres_a01", "overall", fit4_max_overall)

plots_rev_pos_neu_lw_across_waves_fit4           <- create_plots(res_rev_pos_neu_lw_across_waves, "fit4", "none",      "overall", fit4_max_overall)
plots_rev_pos_neu_lw_across_waves_fit4_thres_a05 <- create_plots(res_rev_pos_neu_lw_across_waves, "fit4", "thres_a05", "overall", fit4_max_overall)
plots_rev_pos_neu_lw_across_waves_fit4_thres_a01 <- create_plots(res_rev_pos_neu_lw_across_waves, "fit4", "thres_a01", "overall", fit4_max_overall)

plots_rev_pos_fif_lw_per_wave_fit4               <- create_plots(res_rev_pos_fif_lw_per_wave,     "fit4", "none",      "overall", fit4_max_overall)
plots_rev_pos_fif_lw_per_wave_fit4_thres_a05     <- create_plots(res_rev_pos_fif_lw_per_wave,     "fit4", "thres_a05", "overall", fit4_max_overall)
plots_rev_pos_fif_lw_per_wave_fit4_thres_a01     <- create_plots(res_rev_pos_fif_lw_per_wave,     "fit4", "thres_a01", "overall", fit4_max_overall)

plots_rev_pos_fif_lw_across_waves_fit4           <- create_plots(res_rev_pos_fif_lw_across_waves, "fit4", "none",      "overall", fit4_max_overall)
plots_rev_pos_fif_lw_across_waves_fit4_thres_a05 <- create_plots(res_rev_pos_fif_lw_across_waves, "fit4", "thres_a05", "overall", fit4_max_overall)
plots_rev_pos_fif_lw_across_waves_fit4_thres_a01 <- create_plots(res_rev_pos_fif_lw_across_waves, "fit4", "thres_a01", "overall", fit4_max_overall)

# Export plots

net_interv_plots_path <- paste0(net_interv_path, "plots/")

dir.create(net_interv_plots_path)

save(plots_rev_pos_neu_lw_per_wave_fit4,               file = paste0(net_interv_plots_path, "plots_rev_pos_neu_lw_per_wave_fit4.RData"))
save(plots_rev_pos_neu_lw_per_wave_fit4_thres_a05,     file = paste0(net_interv_plots_path, "plots_rev_pos_neu_lw_per_wave_fit4_thres_a05.RData"))
save(plots_rev_pos_neu_lw_per_wave_fit4_thres_a01,     file = paste0(net_interv_plots_path, "plots_rev_pos_neu_lw_per_wave_fit4_thres_a01.RData"))

save(plots_rev_pos_neu_lw_across_waves_fit4,           file = paste0(net_interv_plots_path, "plots_rev_pos_neu_lw_across_waves_fit4.RData"))
save(plots_rev_pos_neu_lw_across_waves_fit4_thres_a05, file = paste0(net_interv_plots_path, "plots_rev_pos_neu_lw_across_waves_fit4_thres_a05.RData"))
save(plots_rev_pos_neu_lw_across_waves_fit4_thres_a01, file = paste0(net_interv_plots_path, "plots_rev_pos_neu_lw_across_waves_fit4_thres_a01.RData"))

save(plots_rev_pos_fif_lw_per_wave_fit4,               file = paste0(net_interv_plots_path, "plots_rev_pos_fif_lw_per_wave_fit4.RData"))
save(plots_rev_pos_fif_lw_per_wave_fit4_thres_a05,     file = paste0(net_interv_plots_path, "plots_rev_pos_fif_lw_per_wave_fit4_thres_a05.RData"))
save(plots_rev_pos_fif_lw_per_wave_fit4_thres_a01,     file = paste0(net_interv_plots_path, "plots_rev_pos_fif_lw_per_wave_fit4_thres_a01.RData"))

save(plots_rev_pos_fif_lw_across_waves_fit4,           file = paste0(net_interv_plots_path, "plots_rev_pos_fif_lw_across_waves_fit4.RData"))
save(plots_rev_pos_fif_lw_across_waves_fit4_thres_a05, file = paste0(net_interv_plots_path, "plots_rev_pos_fif_lw_across_waves_fit4_thres_a05.RData"))
save(plots_rev_pos_fif_lw_across_waves_fit4_thres_a01, file = paste0(net_interv_plots_path, "plots_rev_pos_fif_lw_across_waves_fit4_thres_a01.RData"))

# ---------------------------------------------------------------------------- #
# Export multipanel plots ----
# ---------------------------------------------------------------------------- #

# Define function to export multipanel plots (for examples of using "layout()", see 
# https://stackoverflow.com/questions/14660372/common-main-title-of-a-figure-panel-compiled-with-parmfrow

export_multipanel <- function(plots, filename) {
  # pdf(paste0(net_interv_plots_path, filename, ".pdf"), width = 12, height = 4.5)  # To include filename as title, use this line instead
  pdf(paste0(net_interv_plots_path, filename, ".pdf"), width = 12, height = 4)
  
  # par(mar=c(0, 0, 0, 0))                                                          # To include filename as title, use these 3 lines instead
  # layout(matrix(c(1, 2, 1, 3, 1, 4), ncol = 3), heights = c(.5, 4, .5, 4, .5, 4))
  # plot.new()
  
  layout(t(1:3))                                                                    # To include filename, remove this line
  
  # text(0, .5, filename, pos = 4, cex = 2, font = 2, adj = 0)                      # To include filename as title, add this line
  
  qgraph(plots$PRE)
  box("figure")
  qgraph(plots$SESSION3)
  box("figure")
  qgraph(plots$SESSION6)
  box("figure")
  
  dev.off()
}

# Run function

export_multipanel(plots_rev_pos_neu_lw_per_wave_fit4,               "multi_rev_pos_neu_lw_per_wave_fit4")
export_multipanel(plots_rev_pos_neu_lw_per_wave_fit4_thres_a05,     "multi_rev_pos_neu_lw_per_wave_fit4_thres_a05")
export_multipanel(plots_rev_pos_neu_lw_per_wave_fit4_thres_a01,     "multi_rev_pos_neu_lw_per_wave_fit4_thres_a01")

export_multipanel(plots_rev_pos_neu_lw_across_waves_fit4,           "multi_rev_pos_neu_lw_across_waves_fit4")
export_multipanel(plots_rev_pos_neu_lw_across_waves_fit4_thres_a05, "multi_rev_pos_neu_lw_across_waves_fit4_thres_a05")
export_multipanel(plots_rev_pos_neu_lw_across_waves_fit4_thres_a01, "multi_rev_pos_neu_lw_across_waves_fit4_thres_a01")

export_multipanel(plots_rev_pos_fif_lw_per_wave_fit4,               "multi_rev_pos_fif_lw_per_wave_fit4")
export_multipanel(plots_rev_pos_fif_lw_per_wave_fit4_thres_a05,     "multi_rev_pos_fif_lw_per_wave_fit4_thres_a05")
export_multipanel(plots_rev_pos_fif_lw_per_wave_fit4_thres_a01,     "multi_rev_pos_fif_lw_per_wave_fit4_thres_a01")

export_multipanel(plots_rev_pos_fif_lw_across_waves_fit4,           "multi_rev_pos_fif_lw_across_waves_fit4")
export_multipanel(plots_rev_pos_fif_lw_across_waves_fit4_thres_a05, "multi_rev_pos_fif_lw_across_waves_fit4_thres_a05")
export_multipanel(plots_rev_pos_fif_lw_across_waves_fit4_thres_a01, "multi_rev_pos_fif_lw_across_waves_fit4_thres_a01")

# ---------------------------------------------------------------------------- #
# Create tables ----
# ---------------------------------------------------------------------------- #

# Define function to create table for saturated model (fit4) across time points

create_net_interv_tbls <- function(res, fit4_edge_include) {
  # Get results object name
  
  res_name <- deparse(substitute(res))
  
  # Create table for each wave
  
  waves       <- c("PRE", "SESSION3", "SESSION6")

  tbls <- vector("list", length(waves))
  names(tbls) <- waves
  
  for (i in 1:length(waves)) {
    res_wave <- res[[waves[i]]]
    
    vars <- res_wave$vars
    wave <- res_wave$wave

    fit4  <- res_wave$fit4
    
    labels <- sub(paste0(".", wave), "", vars)
    
    labels[labels %in% c("positive_vs_neutral",
                         "positive_vs_fifty_fifty")] <- "Pos. CBM-I"
    labels[labels == "anxious_freq"]                 <- "Anx. Freq."
    labels[labels == "anxious_sev"]                  <- "Anx. Sev."
    labels[labels == "avoid"]                        <- "Sit. Avoid"
    labels[labels == "interfere"]                    <- "Work Imp."
    labels[labels == "interfere_social"]             <- "Soc. Imp."
    labels[labels == "rr_ns_mean"]                   <- "Neg. Bias"
    labels[labels == "rr_ps_mean_rev"]               <- "Lack of Pos. Bias"
  
    wadj  <- fit4$pairwise$wadj
    signs <- fit4$pairwise$signs
    
    tbl <- wadj * signs
    
    rownames(tbl) <- colnames(tbl) <- labels
    
    tbl[upper.tri(tbl, diag = TRUE)] <- NA
    tbl <- round(tbl, 2)
    
    tbls[[wave]] <- tbl
  }
  
  # Export tables to CSV
  
  tbls_path <- "./results/net_interv/tbls/"
  
  filename <- sub("res_", "", res_name)
  
  dir.create (tbls_path, showWarnings = FALSE)
  
  sink(file = paste0(tbls_path, filename, ".csv"))
  
  print("Baseline:")
  write.csv(tbls[["PRE"]])
  
  print("Session 3:")
  write.csv(tbls[["SESSION3"]])
  
  print("Session 6:")
  write.csv(tbls[["SESSION6"]])
  
  sink()
}

# Run function

create_net_interv_tbls(res_rev_pos_neu_lw_per_wave,     rev_pos_neu_lw_per_wave_fit4_edge_include)
create_net_interv_tbls(res_rev_pos_neu_lw_across_waves, rev_pos_neu_lw_across_waves_fit4_edge_include)
create_net_interv_tbls(res_rev_pos_fif_lw_per_wave,     rev_pos_fif_lw_per_wave_fit4_edge_include)
create_net_interv_tbls(res_rev_pos_fif_lw_across_waves, rev_pos_fif_lw_across_waves_fit4_edge_include)