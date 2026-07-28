################################################################################
## UTILITY-BASED OPTIMIZATION OF FUJIKAWA'S BASKET TRIAL DESIGN
## PART II AND III OF COMPARISON STUDY: INVESTIGATING PERFORMANCE IN REAL
## SCENARIOS
##
## Author: LD Sauer, IMBI Heidelberg, Germany
################################################################################

# sc_keys_real <- c("sc_med09", "sc_lrg20", "sc_med04", "sc_lrg03")
run_part2n3_real_scenarios <- function(sc, file_prefix) {
  res_df <- NULL
  options_future <- list(seed = TRUE, packages = c("foreach"))
  # Load setup data
  source(here::here("setup.R"))
  setup_study(baby_setup = FALSE)
  # Calculate performance for the real scenario for all utility functions
  # and Fujikawa's default parameters
  pars_fuj <- list(
    fuj1 = c(lambda = 0.99, epsilon = 2, tau = 0),
    fuj2 = c(lambda = 0.99, epsilon = 2, tau = 0.5)
  )
  for (u in c(u_keys, names(pars_fuj))) {
    if (u %in% u_keys) {
      # retrieve optimized parameters from data set out/final_part2_opt_...
      file_type <- paste0("opt_", u, "_", sc)
      file <- list_out_files(part = 2,
                             type = file_type,
                             file_prefix = file_prefix)
      if(length(file) > 1){
        stop(paste0("I found too many output files of type ",
                    file_type, " in the ./out/ folder."))
      } else if (length(file) < 1){
        stop(paste0("I found no output files of type ",
                    file_type, " in the ./out/ folder."))
      }
      df_res_opt <- readRDS(file)
      opt_par_df <- df_res_opt$res[
        which(df_res_opt$res$u == u),
        c("lambda", "epsilon", "tau")
      ]
      opt_par <- as.numeric(opt_par_df)
      names(opt_par) <- names(opt_par_df)
    } else if (u %in% names(pars_fuj)) {
      # retrieve Fujikawa's parameters from pars_fuj (see above)
      opt_par <- pars_fuj[[u]]
    } else {
      stop("Something is rotten in this state.")
    }
    set.seed(06022026, kind = "L'Ecuyer-CMRG")
    # - Setting up Fujikawa's design, scenario, utility
    scenario <- scenarios[[sc]]
    utility <- utilities[[u]]
    utility_params <- add_sc_to_u_params(
      u = u,
      utility_params = u_params[[u]],
      scenario = scenario
    )
    # - Scenarios where baskexact will be used as a backend
    if (sc %in% sc_keys_exact) {
      design <- baskwrap::setup_fujikawa_x(
        k = scenario$k,
        shape1 = shape1,
        shape2 = shape2,
        p0 = scenario$p0,
        backend = "exact"
      )
      detail_params <- detail_params_exact
      detail_params$n <- scenario$n
      # - Precalculating weights for speed-up
      weight_mat_vanilla <- baskwrap::weights_jsd_vanilla(
        design = design,
        n = scenario$n,
        logbase = detail_params$logbase
      )
      weights_from_save <- function(epsilon, tau, ...) {
        return(baskwrap::weights_fujikawa_tuned(
          weight_mat_vanilla,
          epsilon = epsilon,
          tau = tau
        ))
      }
      detail_params$weight_fun <- weights_from_save
      # - Options for futures (foreach and future_apply)
      globals_for_future <- structure(TRUE, add = c("weight_mat_vanilla"))

      options_future <- c(options_future, globals = globals_for_future)
      algorithm_params_part2$control$future_apply_options <-
        c(
          algorithm_params_part2$control$future_apply_options,
          future.globals = globals_for_future
        )
    } else if (sc %in% sc_keys_sim) {
      # - Scenarios where basksim will be used as a backend
      design <- baskwrap::setup_fujikawa_x(
        k = scenario$k,
        shape1 = shape1,
        shape2 = shape2,
        p0 = scenario$p0,
        backend = "sim"
      )
      detail_params <- detail_params_sim
      detail_params$n <- scenario$n
    } else {
      stop("Scenario is invalid!")
    }
    p1s <- list(
      "1" = if_else(scenario$p1_real > design$p0, scenario$p1_real, design$p0)
    )
    pb_message <- paste0(
      "Calculating performance measures in the 'real scenario' for optimal",
      " parameters found by utility function ",
      u,
      " in the scenario ",
      sc
    )
    performance_measures <-
      get_performance_measures(
        design = design,
        detail_params = detail_params,
        p1s = p1s,
        res_par = opt_par,
        u_keys = "u_ewp", # We don't need any utility values here,
        # but I don't want to change the code, so I just picked one which is
        # fast to calculate, such as u_ewp.
        utilities = utilities,
        u_params = u_params,
        scenario = scenario,
        options_future = options_future,
        pb_message = pb_message
      )
    perf_row <-
      data.frame(
        u = u,
        sc = sc,
        id_run = "real_p1",
        t(opt_par),
        Rejection_Probabilities = t(
          performance_measures[[1]]$Rejection_Probabilities
        ),
        FWER = performance_measures[[1]]$FWER,
        EWP = performance_measures[[1]]$EWP,
        ECD = performance_measures[[1]]$ECD,
        p0 = design$p0,
        p1 = t(p1s[[1]]),
        Rejection_Probabilities_SE = t(
          performance_measures[[1]]$Rejection_Probabilities_SE
        ),
        FWER_SE = performance_measures[[1]]$FWER_SE,
        EWP_SE = performance_measures[[1]]$EWP_SE,
        ECD_SE = performance_measures[[1]]$ECD_SE
      )
    if (!is.null(res_df)) {
      res_df <- rbind(res_df, perf_row)
    } else {
      res_df <- perf_row
    }
  }
  saveRDS(
    object = res_df,
    file = out_file(
      part = 2,
      "perform",
      fname1 = "p1real",
      fname2 = sc,
      "RDS",
      file_prefix = file_prefix
    )
  )
  return(NULL)
}
