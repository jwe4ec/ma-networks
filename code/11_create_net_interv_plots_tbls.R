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
  wave_suffixes <- c(PRE = "pre_bs", SESSION3 = "session3_bs", SESSION6 = "session6_bs")
  
  ls <- lapply(wave_suffixes, function(suffix) {
    res_bs <- res_bs_ls[[paste0(model_type, "_", suffix)]]
    
    edge_include_mats <- res_bs[grep("edge_include_thres_", names(res_bs), value = TRUE)]
    names(edge_include_mats) <- sub("edge_include_", "", names(edge_include_mats))
    
    edge_include_mats
  })
  
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
# Create plots ----
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
# Export plots ----
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
# Create tables ----
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
# Export tables ----
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




