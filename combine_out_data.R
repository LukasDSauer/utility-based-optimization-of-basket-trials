################################################################################
## COMBINE OUTPUT DATA FROM SIMULATION STUDY
## PART IV OF COMPARISON STUDY: EXPLORATORY ANALYSES
##
## Author: LD Sauer, IMBI Heidelberg, Germany
################################################################################

# SETUP
source("setup.R")
setup_study(baby_setup = FALSE)
check_empty_dirs("data")

# output file prefices
# (This was introduced in the beginning because I generated datasets in several
# portions which I denoted by different prefices such as "berlin" or "atlanta")
prefices <- list("final_")

# PART 1 - INVESTIGATION OF OPTIMIZATION ALGORITHMS
# Combine and save datasets
part1_efficiency <-
  lapply(X = prefices, FUN = function(prefix) {
    list_out_files(part = 1, type = "efficiency", file_prefix = prefix)
  }) %>%
  unlist %>%
  map_dfr(readRDS)
part1_opt_res <- lapply(X = prefices, FUN = function(prefix) {
  list_out_files(part = 1, type = "opt", file_prefix = prefix)
}) %>%
  unlist %>%
  map_dfr(function(x) readRDS(x)$res)
part1_opt_trace <- lapply(X = prefices, FUN = function(prefix) {
  list_out_files(part = 1, type = "opt", file_prefix = prefix)
}) %>%
  unlist %>%
  map_dfr(function(x) readRDS(x)$trace) %>%
  group_by(alg, u, id_run) %>%
  mutate(it = row_number()) %>%
  ungroup
# Mutating data sets
part1_opt_res %<>%
  mutate(
    epsilon_rounded = round_to_closest(
      epsilon,
      alg_params[["alg_grd"]]$axes$epsilon
    ),
    lambda_rounded = round_to_closest(
      lambda,
      alg_params[["alg_grd"]]$axes$lambda
    ),
    alg_fac = factor(alg, alg_lvls, alg_Labs)
  )
part1_efficiency %<>%
  mutate(
    alg_fac = factor(alg, alg_lvls, alg_Labs),
    user.self = user.self / 60,
    sys.self = sys.self / 60,
    elapsed = elapsed / 60,
    mem.total = mem.total / 1024
  )
part1_opt_trace %<>%
  mutate(alg_fac = factor(alg, alg_lvls, alg_Labs))
part1_ext_consistency <- part1_opt_res %>%
  filter(alg == "alg_grd") %>%
  select(u, lambda, epsilon, tau, value) %>%
  rename_with(
    ~ paste0(.x, "_grd"),
    all_of(c("lambda", "epsilon", "tau", "value"))
  ) %>%
  #cbind(filter(x, alg != "alg_grd"), .) %>%
  left_join(part1_opt_res, ., by = "u") %>%
  mutate(
    alg_fac = factor(alg, alg_lvls, alg_Labs),
    success_gte_grd = value >= value_grd,
    value_diff_grd = value - value_grd,
    value_absdiff_grd = abs(value_diff_grd),
    params_diff_grd = sqrt(
      (epsilon - epsilon_grd)^2 +
        (tau - tau_grd)^2 +
        (lambda - lambda_grd)^2
    )
  )
saveRDS(part1_efficiency, paste0("data/df_part1_efficiency.RDS"))
saveRDS(part1_opt_res, paste0("data/df_part1_opt_res.RDS"))
saveRDS(part1_opt_trace, paste0("data/df_part1_opt_trace.RDS"))
saveRDS(part1_ext_consistency, paste0("data/df_part1_ext_consistency.RDS"))


# PART 2 AND 3 - INVESTIGATION OF UTILITY FUNCTIONS AND FUJIKAWA'S DEFAULT
#                PARAMETERS

# - Helper functions for combining data sets

#' Combine the data concerning the performance measures of the utility functions
#' in the real scenario in part 2 and 3 of the comparison study
#'
#' @param sc A string denoting the scenario.
#' @returns A data frame
#' @examples get_df_part2_perf_realp1_freq("sc_fuj03")
get_df_part2_perf_realp1_freq <- function(sc) {
  part2_perf_realp1 <- lapply(X = prefices, FUN = function(prefix) {
    list.files(
      path = "./out",
      pattern = paste0(prefix, "part2_perform_p1real_*", sc, ".*(?:.RDS)"),
      full.names = TRUE
    )
  }) %>%
    unlist %>%
    map_dfr(readRDS)
  part2_perf_frequentist <- lapply(X = prefices, FUN = function(prefix) {
    list.files(
      path = "./out",
      pattern = paste0(prefix, "part2_perform_frequentist_*", sc, ".*(?:.RDS)"),
      full.names = TRUE
    )
  }) %>%
    unlist %>%
    map_dfr(readRDS)
  return(bind_rows(part2_perf_realp1, part2_perf_frequentist))
}

#' Combine the data concerning the performance measures of the utility functions
#' in part 2 and 3 of the comparison study
#'
#' @param sc A string denoting the scenario.
#' @returns A data frame
#' @examples get_df_part2_perf("sc_fuj03")
get_df_part2_perf <- function(sc) {
  # Load data on performance measures
  part2_perf <- lapply(X = prefices, FUN = function(prefix) {
    list.files(
      path = "./out",
      pattern = paste0(prefix, "part2_perform_u_.*", sc, ".*(?:.RDS)"),
      full.names = TRUE
    )
  }) %>%
    unlist %>%
    map_dfr(readRDS)
  # Load performance in real scenarios
  part2_perf_realp1 <- get_df_part2_perf_realp1_freq(sc)
  part2_perf <- bind_rows(part2_perf, part2_perf_realp1)
  # Check incoming data
  if (nrow(part2_perf) == 0) {
    stop(
      "No observations found while reading part 2. Perhaps you specified\n",
      " the wrong out file prefix."
    )
  }
  # Formatting scenario labels
  part2_perf %<>%
    mutate(
      u_type = factor(
        as.character(u_labs_type[u]),
        levels = unique(as.character(u_labs_type))
      ),
      u_family = as.character(u_labs_family[u]),
    ) %>%
    format_scenario_labels()
  if (sc %in% sc_keys_sim) {
    part2_perf %<>%
      mutate(
        FWER_mcse = if_else(u != "freq", FWER_SE, 0),
        # equal to mcse_rate(FWER, n_sim = n_iter_part2_final_perf)
        EWP_mcse = if_else(u != "freq", EWP_SE, 0),
        # equal to mcse_rate(EWP, n_sim = n_iter_part2_final_perf)
        ECD_mcse = if_else(u != "freq", ECD_SE, 0)
      )
  }
  return(part2_perf)
}

#' Combine the data concerning the optimal parameters
#'
#' @param sc A string denoting the scenario.
#' @returns A data frame
#' @examples get_df_part2_perf("sc_fuj03")
get_df_part2_opt <- function(sc) {
  part2_opt_res <- lapply(
    X = prefices,
    FUN = function(prefix) {
      list.files(
        path = "./out",
        pattern = paste0(prefices[[1]], "part2_opt.*", sc, ".*(?:.RDS)"),
        full.names = TRUE
      )
    }
  ) %>%
    unlist %>%
    map_dfr(function(x) readRDS(x)$res) |>
    mutate(param_label = paste0("(", epsilon, ", ", tau, ") for ", u)) # |>
  # distinct(u, epsilon, tau, param_label)z
  return(part2_opt_res)
}

#' Combine the data concerning the trace of the optimization algorithm in the
#' parameter space
#'
#' @param sc A string denoting the scenario.
#' @returns A data frame
#' @examples get_df_part2_trace("sc_fuj03")
get_df_part2_trace <- function(sc) {
  part2_opt_trace <- lapply(
    X = prefices,
    FUN = function(prefix) {
      list.files(
        path = "./out",
        pattern = paste0(prefices[[1]], "part2_opt.*", sc, ".*(?:.RDS)"),
        full.names = TRUE
      )
    }
  ) %>%
    unlist %>%
    map_dfr(function(x) readRDS(x)$trace)
  return(part2_opt_trace)
}

#' Get an example data sets with weights for one stratum
#'
#' @param sc A string denoting the scenario.
#' @returns A data frame
#' @examples get_df_part2_weight_example("sc_fuj03", df_part2_opt[["sc_fuj03"]])
get_df_part2_weight_example <- function(sc, part2_opt_res) {
  design <- setup_fujikawa_x(
    k = scenarios[[sc]]$k,
    p0 = scenarios[[sc]]$p0,
    backend = "exact"
  )
  part2_opt_res %<>%
    bind_rows(
      .,
      data.frame(
        u = "fuj1",
        epsilon = 2,
        tau = 0,
        param_label = "(2, 0) suggestion fuj1 by Fujikawa et al."
      ),
      data.frame(
        u = "fuj2",
        epsilon = 2,
        tau = 0.5,
        param_label = "(2, 0.5) suggestion fuj2 by Fujikawa et al."
      )
    )
  r1 <- round(scenarios[[sc]]$n * 0.4)
  dat_weights <-
    mapply(
      function(u, epsilon, tau, param_label, ...) {
        weight_mat <- weights_jsd(epsilon, tau, ...)
        # For tau = 1, weight_jsd will return 0.
        # This means that no information is shared between different
        # strata. However, both
        # basksim and baskexact will subsequently manually set the
        # "sharing within the basket" to 1, i.e. the usual beta-binomial
        # posterior without borrowing.
        return(cbind(
          u = u,
          epsilon = epsilon,
          tau = tau,
          param_label = param_label,
          r = (0:scenarios[[sc]]$n),
          weight = weight_mat[r1 + 1, ]
        ))
      },
      u = part2_opt_res[, "u"],
      epsilon = part2_opt_res[, "epsilon"],
      tau = part2_opt_res[, "tau"],
      param_label = part2_opt_res[, "param_label"],
      MoreArgs = list(
        design = design,
        n = scenarios[[sc]]$n,
        logbase = detail_params_exact$logbase,
        lambda = NULL
      ),
      SIMPLIFY = F
    ) %>%
    do.call(rbind, .) %>%
    as.data.frame %>%
    mutate(r = as.integer(r), weight = as.numeric(weight))
  attr(dat_weights, "r1") <- r1
  dat_weights <- lapply(
    X = c("u_ewp", "u_ecd", "u_2ewp", "u_2pow", "fuj1", "fuj2"),
    FUN = function(u_select) {
      if (u_select != "fuj1" & u_select != "fuj2") {
        avg_and_pen_equal <- is_avg_and_pen_equal(part2_opt_res, u_select)
      } else {
        avg_and_pen_equal <- FALSE
      }
      part2_perf_select <- filter(dat_weights, str_detect(u, u_select)) %>%
        mutate(u_family = u_select)
      if (avg_and_pen_equal) {
        part2_perf_select <- filter_avg_pen(part2_perf_select, u_select)
      }
      part2_perf_select <- part2_perf_select %>%
        mutate(
          u_family = if_else(
            u_family == "fuj1" | u_family == "fuj2",
            "fuj1/2",
            u_family
          )
        )
      return(part2_perf_select)
    }
  )
  dat_weights <- do.call(bind_rows, dat_weights)
  return(dat_weights)
}

# - Combine and save data sets
df_part2_perf <- list(
  sc_fuj03 = get_df_part2_perf("sc_fuj03"),
  sc_lrg03 = get_df_part2_perf("sc_lrg03"),
  sc_bau04 = get_df_part2_perf("sc_bau04"),
  sc_med04 = get_df_part2_perf("sc_med04"),
  sc_kra08 = get_df_part2_perf("sc_kra08"),
  sc_med09 = get_df_part2_perf("sc_med09"),
  sc_lrg20 = get_df_part2_perf("sc_lrg20")
)
saveRDS(df_part2_perf, paste0("data/df_part2_perf.RDS"))
df_part2_opt <- list(
  sc_fuj03 = get_df_part2_opt("sc_fuj03"),
  sc_lrg03 = get_df_part2_opt("sc_lrg03"),
  sc_bau04 = get_df_part2_opt("sc_bau04"),
  sc_med04 = get_df_part2_opt("sc_med04"),
  sc_kra08 = get_df_part2_opt("sc_kra08"),
  sc_med09 = get_df_part2_opt("sc_med09"),
  sc_lrg20 = get_df_part2_opt("sc_lrg20")
)
saveRDS(df_part2_opt, paste0("data/df_part2_opt.RDS"))
df_part2_weight_examples <-
  list(
    sc_fuj03 = get_df_part2_weight_example(
      "sc_fuj03",
      df_part2_opt[["sc_fuj03"]]
    ),
    sc_lrg03 = get_df_part2_weight_example(
      "sc_lrg03",
      df_part2_opt[["sc_lrg03"]]
    ),
    sc_bau04 = get_df_part2_weight_example(
      "sc_bau04",
      df_part2_opt[["sc_bau04"]]
    ),
    sc_med04 = get_df_part2_weight_example(
      "sc_med04",
      df_part2_opt[["sc_med04"]]
    ),
    sc_kra08 = get_df_part2_weight_example(
      "sc_kra08",
      df_part2_opt[["sc_kra08"]]
    ),
    sc_med09 = get_df_part2_weight_example(
      "sc_med09",
      df_part2_opt[["sc_med09"]]
    ),
    sc_lrg20 = get_df_part2_weight_example(
      "sc_lrg20",
      df_part2_opt[["sc_lrg20"]]
    )
  )
saveRDS(df_part2_weight_examples, paste0("data/df_part2_weight_examples.RDS"))
df_part2_trace <-
  list(
    sc_fuj03 = get_df_part2_trace("sc_fuj03"),
    sc_lrg03 = get_df_part2_trace("sc_lrg03"),
    sc_bau04 = get_df_part2_trace("sc_bau04"),
    sc_med04 = get_df_part2_trace("sc_med04"),
    sc_kra08 = get_df_part2_trace("sc_kra08"),
    sc_med09 = get_df_part2_trace("sc_med09"),
    sc_lrg20 = get_df_part2_trace("sc_lrg20")
  )
saveRDS(df_part2_trace, paste0("data/df_part2_trace.RDS"))


# PART 4 - FURTHER ANALYSES

#' Turn divergence measure variable to factor
#'
#' @param x A character variable
factor_hld <- function(x) {
  return(factor(x, levels = c("hld", "jsd"), labels = c("HLD", "JSD")))
}

# - Comparison of JSD vs. HLD
part4_jsd_vs_hld_opt <- lapply(
  X = prefices,
  FUN = function(prefix) {
    list_out_files(
      part = 4,
      type = "opt_alg_grd_jsd_vs_hld",
      file_prefix = prefix
    )
  }
) %>%
  unlist %>%
  map_dfr(function(x) readRDS(x)$res) %>%
  mutate(id_run = factor_hld(id_run))
part4_jsd_vs_hld_efficiency_weights_run <- lapply(
  X = prefices,
  FUN = function(prefix) {
    list_out_files(
      part = 4,
      type = "efficiency_alg_grd_jsd_vs_hld",
      file_prefix = prefix
    )
  }
) %>%
  unlist %>%
  map_dfr(readRDS) |>
  mutate(id_run = factor_hld(id_run))
part4_jsd_vs_hld_efficiency_weights_mat <- lapply(
  X = prefices,
  FUN = function(prefix) {
    list_out_files(
      part = 4,
      type = "efficiency_weights_mat_alg_grd_jsd_vs_hld",
      file_prefix = prefix
    )
  }
) %>%
  unlist %>%
  map_dfr(function(x) {
    mutate(readRDS(x), sc = sc_keys[sapply(sc_keys, function(sc) grepl(sc, x))])
  }) %>%
  mutate(weights = factor_hld(weights))
part4_jsd_vs_hld_perf <- lapply(
  X = prefices,
  FUN = function(prefix) {
    list_out_files(
      part = 4,
      type = "perform_alg_grd_jsd_vs_hld",
      file_prefix = prefix
    )
  }
) %>%
  unlist %>%
  map_dfr(function(x) format_scenario_labels(readRDS(x))) |>
  mutate(id_run = factor_hld(id_run))

# - Effect of information sharing on TOER
part4_reject02 <- lapply(
  X = prefices,
  FUN = function(prefix) {
    list_out_files(part = 4, type = "two_basket_reject", file_prefix = prefix)
  }
) %>%
  unlist %>%
  map_dfr(readRDS)

saveRDS(part4_jsd_vs_hld_opt, paste0("data/df_part4_jsd_vs_hld_opt.RDS"))
saveRDS(
  part4_jsd_vs_hld_efficiency_weights_run,
  paste0("data/df_part4_jsd_vs_hld_efficiency_weights_run.RDS")
)
saveRDS(
  part4_jsd_vs_hld_efficiency_weights_mat,
  paste0("data/df_part4_jsd_vs_hld_efficiency_weights_mat.RDS")
)
saveRDS(part4_jsd_vs_hld_perf, paste0("data/df_part4_jsd_vs_hld_perf.RDS"))
saveRDS(part4_reject02, paste0("data/df_part4_reject02.RDS"))
