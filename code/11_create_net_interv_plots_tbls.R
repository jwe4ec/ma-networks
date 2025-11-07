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

groundhog.library(c("qgraph", "dplyr", "ggplot2", "cowplot"), groundhog_day)

# ---------------------------------------------------------------------------- #
# Import results ----
# ---------------------------------------------------------------------------- #

# Network results

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

res_bs_rr_net_ls <- readRDS(file.path(net_interv_stab_path, "res_bs_rr_net_ls.rds"))
res_bs_bb_net_ls <- readRDS(file.path(net_interv_stab_path, "res_bs_bb_net_ls.rds"))

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
  waves <- c("PRE", "SESSION3", "SESSION6")

  ls <- lapply(waves, function(wave) {
    res_bs <- res_bs_ls[[paste0(model_type, "_", wave, "_bs")]]
    
    edge_include_mats <- res_bs[grep("edge_include_thres_", names(res_bs), value = TRUE)]
    names(edge_include_mats) <- sub("edge_include_", "", names(edge_include_mats))
    
    edge_include_mats
  })
  
  names(ls) <- waves
  
  return(ls)
}

# Run function

rr_pos_neu_lw_per_wave_edge_include     <- extract_edge_include_mats(res_bs_rr_net_ls, "res_rr_pos_neu_lw_per_wave")
rr_pos_neu_lw_across_waves_edge_include <- extract_edge_include_mats(res_bs_rr_net_ls, "res_rr_pos_neu_lw_across_waves")
rr_pos_fif_lw_per_wave_edge_include     <- extract_edge_include_mats(res_bs_rr_net_ls, "res_rr_pos_fif_lw_per_wave")
rr_pos_fif_lw_across_waves_edge_include <- extract_edge_include_mats(res_bs_rr_net_ls, "res_rr_pos_fif_lw_across_waves")

bb_pos_neu_lw_per_wave_edge_include     <- extract_edge_include_mats(res_bs_bb_net_ls, "res_bb_pos_neu_lw_per_wave")
bb_pos_neu_lw_across_waves_edge_include <- extract_edge_include_mats(res_bs_bb_net_ls, "res_bb_pos_neu_lw_across_waves")
bb_pos_fif_lw_per_wave_edge_include     <- extract_edge_include_mats(res_bs_bb_net_ls, "res_bb_pos_fif_lw_per_wave")
bb_pos_fif_lw_across_waves_edge_include <- extract_edge_include_mats(res_bs_bb_net_ls, "res_bb_pos_fif_lw_across_waves")

# ---------------------------------------------------------------------------- #
# Create thresholded weighted adjacency matrices at alpha levels of .05 and .01  ----
# ---------------------------------------------------------------------------- #

# Define function

create_wadj_thres <- function(res, edge_include) {
  waves <- c("PRE", "SESSION3", "SESSION6")
  thres_names <- c("thres_a05", "thres_a01")
  
  for (wave in waves) {
    fit <- res[[wave]]$fit
    pw  <- fit$pairwise
    
    for (thres_name in thres_names) {
      mask <- edge_include[[wave]][[thres_name]]
      
      wadj_name  <- paste0("wadj_",  thres_name)
      signs_name <- paste0("signs_", thres_name)
      
      pw[[wadj_name]]  <- pw$wadj * mask
      
      pw[[signs_name]] <- pw$signs
      pw[[signs_name]][mask == 0] <- NA
    }
    
    res[[wave]]$fit$pairwise <- pw
  }
  
  return(res)
}

# Run function

res_rr_pos_neu_lw_per_wave     <- create_wadj_thres(res_rr_pos_neu_lw_per_wave,     rr_pos_neu_lw_per_wave_edge_include)
res_rr_pos_neu_lw_across_waves <- create_wadj_thres(res_rr_pos_neu_lw_across_waves, rr_pos_neu_lw_across_waves_edge_include)
res_rr_pos_fif_lw_per_wave     <- create_wadj_thres(res_rr_pos_fif_lw_per_wave,     rr_pos_fif_lw_per_wave_edge_include)
res_rr_pos_fif_lw_across_waves <- create_wadj_thres(res_rr_pos_fif_lw_across_waves, rr_pos_fif_lw_across_waves_edge_include)

res_bb_pos_neu_lw_per_wave     <- create_wadj_thres(res_bb_pos_neu_lw_per_wave,     bb_pos_neu_lw_per_wave_edge_include)
res_bb_pos_neu_lw_across_waves <- create_wadj_thres(res_bb_pos_neu_lw_across_waves, bb_pos_neu_lw_across_waves_edge_include)
res_bb_pos_fif_lw_per_wave     <- create_wadj_thres(res_bb_pos_fif_lw_per_wave,     bb_pos_fif_lw_per_wave_edge_include)
res_bb_pos_fif_lw_across_waves <- create_wadj_thres(res_bb_pos_fif_lw_across_waves, bb_pos_fif_lw_across_waves_edge_include)

# ---------------------------------------------------------------------------- #
# Create data frames for confidence interval plots ----
# ---------------------------------------------------------------------------- #

# Define function

create_ci_dfs <- function(res, res_bs_ls, label_length = "s") {
  res_name <- deparse(substitute(res))
  
  waves <- c("PRE", "SESSION3", "SESSION6")
  
  el <- vector("list", length(waves))
  names(el) <- waves
  
  for (wave in waves) {
    # Extract results for given wave to get variables, weighted adjacency matrix,
    # and signs for saturated network
    
    res_wave <- res[[wave]]
    
    vars  <- res_wave$vars
    
    pw    <- res_wave$fit$pairwise
    wadj  <- pw$wadj
    signs <- pw$signs
    
    # Extract bootstrap results at given wave to get quantiles and edge inclusion 
    # matrices for alphas of .05 and .01
    
    res_wave_bs_name <- paste(res_name, wave, "bs", sep = "_")
    res_wave_bs      <- res_bs_ls[[res_wave_bs_name]]
    
    bootQuantiles_a05 <- res_wave_bs$bootQuantiles_a05
    bootQuantiles_a01 <- res_wave_bs$bootQuantiles_a01
    
    edge_include_thres_a05 <- res_wave_bs$edge_include_thres_a05
    edge_include_thres_a01 <- res_wave_bs$edge_include_thres_a01
    
    # Create edge list with results from matrices (using upper tri to obtain
    # same edge labels below as "mgm::plotRes()")
    
    inds <- which(upper.tri(wadj), arr.ind = TRUE)
    
    el_wave <- data.frame(row        = inds[, "row"],
                          col        = inds[, "col"],
                          abs_weight = wadj[inds],
                          sign       = signs[inds],
                          ci_ll_a05  = bootQuantiles_a05[, , 1][inds],
                          ci_ul_a05  = bootQuantiles_a05[, , 2][inds],
                          sig_a05    = edge_include_thres_a05[inds],
                          ci_ll_a01  = bootQuantiles_a01[, , 1][inds],
                          ci_ul_a01  = bootQuantiles_a01[, , 2][inds],
                          sig_a01    = edge_include_thres_a01[inds])
    
    # Compute signed weight and order edge list on it
    
    el_wave$weight <- el_wave$abs_weight * el_wave$sign
    
    el_wave <- el_wave[order(el_wave$weight, decreasing = TRUE), ]
    
    # Create node labels
    
    labels <- sub(paste0(".", wave), "", vars)
    
    pos_cbm_contrasts <- c("positive_vs_neutral", "positive_vs_fifty_fifty")
    
    if (label_length == "s") {
      labels[labels %in% pos_cbm_contrasts]   <- "P.CBM"
      labels[labels == "anxious_freq"]        <- "AF"
      labels[labels == "anxious_sev"]         <- "AS"
      labels[labels == "avoid"]               <- "SA"
      labels[labels == "interfere"]           <- "WI"
      labels[labels == "interfere_social"]    <- "SI"
      labels[labels == "rr_neg_thr_mean"]     <- "NB"
      labels[labels == "rr_pos_thr_mean_rev"] <- "LPB"
      labels[labels == "bbsiq_neg_mean"]      <- "NB"
    } else if (label_length == "l") {
      labels[labels %in% pos_cbm_contrasts]   <- "Pos. CBM-I"
      labels[labels == "anxious_freq"]        <- "Anx. Freq."
      labels[labels == "anxious_sev"]         <- "Anx. Sev."
      labels[labels == "avoid"]               <- "Sit. Avoid"
      labels[labels == "interfere"]           <- "Work Imp."
      labels[labels == "interfere_social"]    <- "Soc. Imp."
      labels[labels == "rr_neg_thr_mean"]     <- "Neg. Bias"
      labels[labels == "rr_pos_thr_mean_rev"] <- "Lack Pos. Bias"
      labels[labels == "bbsiq_neg_mean"]      <- "Neg. Bias"
    }
    
    # Create edge labels and index
    
    el_wave$label <- paste(labels[el_wave$row], "-", labels[el_wave$col])
    
    row.names(el_wave) <- NULL
    el_wave$index <- as.integer(row.names(el_wave))
    
    el[[wave]] <- el_wave
  }
  
  return(el)
}

# Run function

rr_pos_neu_lw_per_wave_el     <- create_ci_dfs(res_rr_pos_neu_lw_per_wave,     res_bs_rr_net_ls)
rr_pos_neu_lw_across_waves_el <- create_ci_dfs(res_rr_pos_neu_lw_across_waves, res_bs_rr_net_ls)
rr_pos_fif_lw_per_wave_el     <- create_ci_dfs(res_rr_pos_fif_lw_per_wave,     res_bs_rr_net_ls)
rr_pos_fif_lw_across_waves_el <- create_ci_dfs(res_rr_pos_fif_lw_across_waves, res_bs_rr_net_ls)

bb_pos_neu_lw_per_wave_el     <- create_ci_dfs(res_bb_pos_neu_lw_per_wave,     res_bs_bb_net_ls)
bb_pos_neu_lw_across_waves_el <- create_ci_dfs(res_bb_pos_neu_lw_across_waves, res_bs_bb_net_ls)
bb_pos_fif_lw_per_wave_el     <- create_ci_dfs(res_bb_pos_fif_lw_per_wave,     res_bs_bb_net_ls)
bb_pos_fif_lw_across_waves_el <- create_ci_dfs(res_bb_pos_fif_lw_across_waves, res_bs_bb_net_ls)

# ---------------------------------------------------------------------------- #
# Create confidence interval plots ----
# ---------------------------------------------------------------------------- #

# Define function to create multipanel plot

create_ci_plot <- function(el, label_length = "s") {
  waves       <- c("PRE", "SESSION3", "SESSION6")
  wave_labels <- c("Baseline", "Session 3", "Session 6")
  
  # Determine number of 0.5 x-axis increments needed for each wave (include
  # padding at left for edge labels), to ensure physical distance of each 
  # increment is same across waves using "rel_widths" in "plot_grid()"
  
  pad <- c(s = 1, l = 2.5)[[label_length]]

  x_axis_ls <- lapply(el, function(el_wave) {
    x_min <- floor(min(el_wave$ci_ll_a01) * 2) / 2 - pad
    x_max <- ceiling(max(el_wave$ci_ul_a01) * 2) / 2
    
    increments <- (x_max - x_min) / 0.5
    
    list(x_min = x_min, x_max = x_max, increments = increments)
  })
  
  x_increments <- sapply(x_axis_ls, function(x) x$increments)
  
  # Create plots
  
  ci_p_ls <- vector("list", length(waves))
  names(ci_p_ls) <- waves
  
  for (i in 1:length(waves)) {
    wave       <- waves[i]
    wave_label <- wave_labels[i]
    el_wave    <- el[[wave]]
    
    x_axis_wave <- x_axis_ls[[wave]]
    x_min <- x_axis_wave$x_min
    x_max <- x_axis_wave$x_max

    # Determine positions for edge and wave labels
    
    offset <- 0.05

    x_edge_label <- x_min + pad - offset
    
    x_wave_label <- x_max - offset
    y_wave_label <- max(el_wave$index)
    
    # Create plot
    
    ci_p_ls[[wave]] <- ggplot(el_wave, aes(x = weight, y = index)) +
      # X-axis at top and bottom
      
      scale_x_continuous(limits = c(x_min, x_max),
                         breaks = seq(x_min + pad, x_max, by = 0.5),
                         expand = c(0, 0),
                         sec.axis = dup_axis()) +
      
      # Y-axis (line hidden in theme)
      
      scale_y_reverse() +
      
      # Edge labels inside plot area
      
      geom_text(aes(label = label), size = 3,
                x = x_edge_label,
                hjust = 1, vjust = 0.5) +
      
      # Vertical lines at first x-axis tick and at 0
      
      geom_vline(xintercept = x_min + pad) +
      geom_vline(xintercept = 0, linetype = "dotted") +
      
      # Error bars (95% CIs overlaid on 99% CIs) and points (sample estimates)
      
      geom_errorbar(aes(xmin = ci_ll_a01, xmax = ci_ul_a01,
                        color = case_when(sig_a01 == 1 & weight > 0 ~ "pos_sig",
                                          sig_a01 == 1 & weight < 0 ~ "neg_sig",
                                          TRUE ~ "ns")),
                    width = 0) +
      geom_errorbar(aes(xmin = ci_ll_a05, xmax = ci_ul_a05,
                        color = case_when(sig_a05 == 1 & weight > 0 ~ "pos_sig",
                                          sig_a05 == 1 & weight < 0 ~ "neg_sig",
                                          TRUE ~ "ns")),
                    width = 0.5) +
      geom_point(size = 1.5) +
      
      # Theme
      
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.x = element_line(color = "black"),
            axis.text.x = element_text(color = "black"),
            axis.ticks.x = element_line(color = "black"),
            legend.position = "none") +
      scale_color_manual(values = c("pos_sig" = "#0000D5",
                                    "neg_sig" = "#BF0000",
                                    "ns"      = "#00B3B3")) +
      
      # Wave labels inside plot area
      
      annotate("text", label = wave_label, size = 3.5, fontface = "bold",
               x = x_wave_label, y = y_wave_label,
               hjust = 1, vjust = 1)
  }
  
  # Create multipanel plot
  
  ci_multi_p <- plot_grid(ci_p_ls$PRE, ci_p_ls$SESSION3, ci_p_ls$SESSION6, 
                          nrow = 1, rel_widths = x_increments)
  
  return(ci_multi_p)
}

# Run function

ci_p_ls <- list()

ci_p_ls[["rr_pos_neu_lw_per_wave_el_multi"]]     <- create_ci_plot(rr_pos_neu_lw_per_wave_el)
ci_p_ls[["rr_pos_neu_lw_across_waves_el_multi"]] <- create_ci_plot(rr_pos_neu_lw_across_waves_el)
ci_p_ls[["rr_pos_fif_lw_per_wave_el_multi"]]     <- create_ci_plot(rr_pos_fif_lw_per_wave_el)
ci_p_ls[["rr_pos_fif_lw_across_waves_el_multi"]] <- create_ci_plot(rr_pos_fif_lw_across_waves_el)

ci_p_ls[["bb_pos_neu_lw_per_wave_el_multi"]]     <- create_ci_plot(bb_pos_neu_lw_per_wave_el)
ci_p_ls[["bb_pos_neu_lw_across_waves_el_multi"]] <- create_ci_plot(bb_pos_neu_lw_across_waves_el)
ci_p_ls[["bb_pos_fif_lw_per_wave_el_multi"]]     <- create_ci_plot(bb_pos_fif_lw_per_wave_el)
ci_p_ls[["bb_pos_fif_lw_across_waves_el_multi"]] <- create_ci_plot(bb_pos_fif_lw_across_waves_el)

# ---------------------------------------------------------------------------- #
# Export confidence interval plots ----
# ---------------------------------------------------------------------------- #

# Export multipanel CI plots to PDF

ci_plots_path <- file.path(net_interv_path, "ci_plots")
dir.create(ci_plots_path)

for (name in names(ci_p_ls)) {
  save_plot(file.path(ci_plots_path, paste0(name, ".pdf")),
            ci_p_ls[[name]],
            base_height = 5,
            base_width = 12)
}

# ---------------------------------------------------------------------------- #
# Compute maximum edge weight across all network intervention analyses ----
# ---------------------------------------------------------------------------- #

res_all_ls <- list(res_rr_pos_neu_lw_per_wave     = res_rr_pos_neu_lw_per_wave,
                   res_rr_pos_neu_lw_across_waves = res_rr_pos_neu_lw_across_waves,
                   res_rr_pos_fif_lw_per_wave     = res_rr_pos_fif_lw_per_wave,
                   res_rr_pos_fif_lw_across_waves = res_rr_pos_fif_lw_across_waves,
                   
                   res_bb_pos_neu_lw_per_wave     = res_bb_pos_neu_lw_per_wave,
                   res_bb_pos_neu_lw_across_waves = res_bb_pos_neu_lw_across_waves,
                   res_bb_pos_fif_lw_per_wave     = res_bb_pos_fif_lw_per_wave,
                   res_bb_pos_fif_lw_across_waves = res_bb_pos_fif_lw_across_waves)

waves <- c("PRE", "SESSION3", "SESSION6")

abs_wadj_ls <- lapply(names(res_all_ls), function(res_name) {
  res <- res_all_ls[[res_name]]
  
  abs_wadj_mats <- lapply(waves, function(w) abs(res[[w]]$fit$pairwise$wadj))
  names(abs_wadj_mats) <- waves
  
  abs_wadj_mats
})
names(abs_wadj_ls) <- names(res_all_ls)

max_overall <- max(unlist(abs_wadj_ls))

stopifnot(round(max_overall, 2) == .76)

# ---------------------------------------------------------------------------- #
# Create network plots ----
# ---------------------------------------------------------------------------- #

# Define function to plot networks at baseline, Session 3, and Session 6, using
# maximum edge weight across all network intervention analyses (see above)

create_plots <- function(res, thres_name, max_overall) {
  res_name <- deparse(substitute(res))
  
  # Create plot for each wave
  
  waves       <- c("PRE", "SESSION3", "SESSION6")
  wave_titles <- c("Baseline", "Session 3", "Session 6")
  
  plots <- vector("list", length(waves))
  names(plots) <- waves
  
  for (i in 1:length(waves)) {
    wave       <- waves[i]
    wave_title <- wave_titles[i]
    res_wave   <- res[[wave]]
    
    vars             <- res_wave$vars
    num_vars         <- length(vars)
    num_noncond_vars <- num_vars - 1
    
    fit <- res_wave$fit
    pw  <- fit$pairwise

    if (thres_name == "none") {
      wadj <- pw$wadj
    } else if (thres_name == "thres_a05") {
      wadj <- pw$wadj_thres_a05
    } else if (thres_name == "thres_a01") {
      wadj <- pw$wadj_thres_a01
    }
    
    labels <- sub(paste0(".", wave), "", vars)
    
    pos_cbm_contrasts <- c("positive_vs_neutral", "positive_vs_fifty_fifty")
    lack_pos_bias     <- "Lack\nof Pos.\nBias"
    
    labels[labels %in% pos_cbm_contrasts]   <- "Pos.\nCBM-I"
    labels[labels == "anxious_freq"]        <- "Anx.\nFreq."
    labels[labels == "anxious_sev"]         <- "Anx.\nSev."
    labels[labels == "avoid"]               <- "Sit.\nAvoid"
    labels[labels == "interfere"]           <- "Work\nImp."
    labels[labels == "interfere_social"]    <- "Soc.\nImp."
    labels[labels == "rr_neg_thr_mean"]     <- "Neg.\nBias"
    labels[labels == "rr_pos_thr_mean_rev"] <- lack_pos_bias
    labels[labels == "bbsiq_neg_mean"]      <- "Neg.\nBias"
    
    label_cex <- ifelse(labels == lack_pos_bias, 0.9, 1.1)
    
    # Change edge colors to match those in temporal plots (see first elements of vectors here:
    # https://github.com/SachaEpskamp/qgraph/blob/9b70cd438ee14b2c51e8030b3d5100d07a553c28/R/qgraph.R#L965)
    
    edge_colors <- pw$edgecolor_cb
    edge_colors[edge_colors == "darkblue"] <- "#0000D5"
    edge_colors[edge_colors == "red"]      <- "#BF0000"
    
    # Include edge labels only for thresholded networks (too cluttered in saturated networks)
    
    edge_labels <- thres_name != "none"
    
    # If needed, edit this to make edge labels for certain models (i.e., certain "res_name" 
    # and "thres_name" values) smaller (e.g., 1.6) due to overlapping positions
    
    edge_label_cex <- 2
    
    # TODO: Find better way to make negative edges dashed ("lty" below makes them
    # dashed, but the spacing between dashes increases as the magnitude of the edge
    # weight increases, making the plot difficult to read)
    
    plots[[wave]] <- qgraph(wadj,
                            edge.color = edge_colors,
                            edge.labels = edge_labels,
                            edge.label.color = "black",
                            edge.label.margin = .01,
                            edge.label.cex = edge_label_cex,
                            # lty = pw$edge_lty,   # TODO
                            layout = "circle",
                            labels = labels,
                            theme = "colorblind",
                            asize = 7,
                            vsize = 15,
                            shape = c("square", rep("circle", num_noncond_vars)),
                            label.cex = label_cex,
                            mar = rep(4, 4),
                            title = bquote(paste(.(wave_title), " (", italic("n"), " = ", 
                                                 .(fit$call$n), ")")),
                            title.cex = 1.4,
                            label.scale = FALSE,
                            maximum = max_overall)
  }
  
  return(plots)
}

# Run function

p_ls <- list()

## For RR network

p_ls[["plots_rr_pos_neu_lw_per_wave"]]               <- create_plots(res_rr_pos_neu_lw_per_wave,     "none",      max_overall)
p_ls[["plots_rr_pos_neu_lw_per_wave_thres_a05"]]     <- create_plots(res_rr_pos_neu_lw_per_wave,     "thres_a05", max_overall)
p_ls[["plots_rr_pos_neu_lw_per_wave_thres_a01"]]     <- create_plots(res_rr_pos_neu_lw_per_wave,     "thres_a01", max_overall)

p_ls[["plots_rr_pos_neu_lw_across_waves"]]           <- create_plots(res_rr_pos_neu_lw_across_waves, "none",      max_overall)
p_ls[["plots_rr_pos_neu_lw_across_waves_thres_a05"]] <- create_plots(res_rr_pos_neu_lw_across_waves, "thres_a05", max_overall)
p_ls[["plots_rr_pos_neu_lw_across_waves_thres_a01"]] <- create_plots(res_rr_pos_neu_lw_across_waves, "thres_a01", max_overall)

p_ls[["plots_rr_pos_fif_lw_per_wave"]]               <- create_plots(res_rr_pos_fif_lw_per_wave,     "none",      max_overall)
p_ls[["plots_rr_pos_fif_lw_per_wave_thres_a05"]]     <- create_plots(res_rr_pos_fif_lw_per_wave,     "thres_a05", max_overall)
p_ls[["plots_rr_pos_fif_lw_per_wave_thres_a01"]]     <- create_plots(res_rr_pos_fif_lw_per_wave,     "thres_a01", max_overall)

p_ls[["plots_rr_pos_fif_lw_across_waves"]]           <- create_plots(res_rr_pos_fif_lw_across_waves, "none",      max_overall)
p_ls[["plots_rr_pos_fif_lw_across_waves_thres_a05"]] <- create_plots(res_rr_pos_fif_lw_across_waves, "thres_a05", max_overall)
p_ls[["plots_rr_pos_fif_lw_across_waves_thres_a01"]] <- create_plots(res_rr_pos_fif_lw_across_waves, "thres_a01", max_overall)

## For BBSIQ network

p_ls[["plots_bb_pos_neu_lw_per_wave"]]               <- create_plots(res_bb_pos_neu_lw_per_wave,     "none",      max_overall)
p_ls[["plots_bb_pos_neu_lw_per_wave_thres_a05"]]     <- create_plots(res_bb_pos_neu_lw_per_wave,     "thres_a05", max_overall)
p_ls[["plots_bb_pos_neu_lw_per_wave_thres_a01"]]     <- create_plots(res_bb_pos_neu_lw_per_wave,     "thres_a01", max_overall)

p_ls[["plots_bb_pos_neu_lw_across_waves"]]           <- create_plots(res_bb_pos_neu_lw_across_waves, "none",      max_overall)
p_ls[["plots_bb_pos_neu_lw_across_waves_thres_a05"]] <- create_plots(res_bb_pos_neu_lw_across_waves, "thres_a05", max_overall)
p_ls[["plots_bb_pos_neu_lw_across_waves_thres_a01"]] <- create_plots(res_bb_pos_neu_lw_across_waves, "thres_a01", max_overall)

p_ls[["plots_bb_pos_fif_lw_per_wave"]]               <- create_plots(res_bb_pos_fif_lw_per_wave,     "none",      max_overall)
p_ls[["plots_bb_pos_fif_lw_per_wave_thres_a05"]]     <- create_plots(res_bb_pos_fif_lw_per_wave,     "thres_a05", max_overall)
p_ls[["plots_bb_pos_fif_lw_per_wave_thres_a01"]]     <- create_plots(res_bb_pos_fif_lw_per_wave,     "thres_a01", max_overall)

p_ls[["plots_bb_pos_fif_lw_across_waves"]]           <- create_plots(res_bb_pos_fif_lw_across_waves, "none",      max_overall)
p_ls[["plots_bb_pos_fif_lw_across_waves_thres_a05"]] <- create_plots(res_bb_pos_fif_lw_across_waves, "thres_a05", max_overall)
p_ls[["plots_bb_pos_fif_lw_across_waves_thres_a01"]] <- create_plots(res_bb_pos_fif_lw_across_waves, "thres_a01", max_overall)

# ---------------------------------------------------------------------------- #
# Export network plots ----
# ---------------------------------------------------------------------------- #

# Export plots objects to RDS

net_interv_plots_path <- file.path(net_interv_path, "plots")
dir.create(net_interv_plots_path)

for (plots_name in names(p_ls)) {
  plots <- p_ls[[plots_name]]
  
  saveRDS(plots, file.path(net_interv_plots_path, paste0(plots_name, ".rds")))
}

# Export multipanel plots to PDF

for (plots_name in names(p_ls)) {
  plots <- p_ls[[plots_name]]
  
  multi_name <- sub("plots_", "multi_", plots_name)
  
  pdf(file.path(net_interv_plots_path, paste0(multi_name, ".pdf")), width = 12, height = 4)
  
  layout(t(1:3))
  
  qgraph(plots$PRE)
  box("figure")
  qgraph(plots$SESSION3)
  box("figure")
  qgraph(plots$SESSION6)
  box("figure")
  
  dev.off()
}

# ---------------------------------------------------------------------------- #
# Create saturated model tables ----
# ---------------------------------------------------------------------------- #

# Define function to create table for saturated model across time points

create_net_interv_tbls <- function(res, edge_include) {
  # Create table for each wave
  
  waves <- c("PRE", "SESSION3", "SESSION6")

  tbls <- vector("list", length(waves))
  names(tbls) <- waves
  
  for (i in 1:length(waves)) {
    wave     <- waves[i]
    res_wave <- res[[wave]]
    
    vars <- res_wave$vars
    
    fit <- res_wave$fit
    pw  <- fit$pairwise
    
    labels <- sub(paste0(".", wave), "", vars)
    
    pos_cbm_contrasts <- c("positive_vs_neutral", "positive_vs_fifty_fifty")
    
    labels[labels %in% pos_cbm_contrasts]   <- "Pos. CBM-I"
    labels[labels == "anxious_freq"]        <- "Anx. Freq."
    labels[labels == "anxious_sev"]         <- "Anx. Sev."
    labels[labels == "avoid"]               <- "Sit. Avoid"
    labels[labels == "interfere"]           <- "Work Imp."
    labels[labels == "interfere_social"]    <- "Soc. Imp."
    labels[labels == "rr_neg_thr_mean"]     <- "Neg. Bias"
    labels[labels == "rr_pos_thr_mean_rev"] <- "Lack of Pos. Bias"
    labels[labels == "bbsiq_neg_mean"]      <- "Neg. Bias"
    
    wadj  <- pw$wadj
    signs <- pw$signs
    
    tbl <- wadj * signs
    
    rownames(tbl) <- colnames(tbl) <- labels
    
    tbl[upper.tri(tbl, diag = TRUE)] <- NA
    tbl <- round(tbl, 2)
    
    tbls[[wave]] <- tbl
  }
  
  return(tbls)
}

# Run function

t_ls <- list()

t_ls[["tbls_rr_pos_neu_lw_per_wave"]]     <- create_net_interv_tbls(res_rr_pos_neu_lw_per_wave,     rr_pos_neu_lw_per_wave_edge_include)
t_ls[["tbls_rr_pos_neu_lw_across_waves"]] <- create_net_interv_tbls(res_rr_pos_neu_lw_across_waves, rr_pos_neu_lw_across_waves_edge_include)
t_ls[["tbls_rr_pos_fif_lw_per_wave"]]     <- create_net_interv_tbls(res_rr_pos_fif_lw_per_wave,     rr_pos_fif_lw_per_wave_edge_include)
t_ls[["tbls_rr_pos_fif_lw_across_waves"]] <- create_net_interv_tbls(res_rr_pos_fif_lw_across_waves, rr_pos_fif_lw_across_waves_edge_include)

t_ls[["tbls_bb_pos_neu_lw_per_wave"]]     <- create_net_interv_tbls(res_bb_pos_neu_lw_per_wave,     bb_pos_neu_lw_per_wave_edge_include)
t_ls[["tbls_bb_pos_neu_lw_across_waves"]] <- create_net_interv_tbls(res_bb_pos_neu_lw_across_waves, bb_pos_neu_lw_across_waves_edge_include)
t_ls[["tbls_bb_pos_fif_lw_per_wave"]]     <- create_net_interv_tbls(res_bb_pos_fif_lw_per_wave,     bb_pos_fif_lw_per_wave_edge_include)
t_ls[["tbls_bb_pos_fif_lw_across_waves"]] <- create_net_interv_tbls(res_bb_pos_fif_lw_across_waves, bb_pos_fif_lw_across_waves_edge_include)

# ---------------------------------------------------------------------------- #
# Export saturated model tables ----
# ---------------------------------------------------------------------------- #

# Export tables to CSV

tbls_path <- file.path(net_interv_path, "tbls")
dir.create(tbls_path)

for (tbls_name in names(t_ls)) {
  tbls <- t_ls[[tbls_name]]
  
  multi_name <- sub("tbls_", "multi_", tbls_name)
  
  sink(file = file.path(tbls_path, paste0(multi_name, ".csv")))
  
  print("Baseline:")
  write.csv(tbls$PRE)
  
  print("Session 3:")
  write.csv(tbls$SESSION3)
  
  print("Session 6:")
  write.csv(tbls$SESSION6)
  
  sink()
}

# TODO: Format tables




