################################################################################
## UTILITY-BASED OPTIMIZATION OF FUJIKAWA'S BASKET TRIAL DESIGN
## PART II OF COMPARISON STUDY: COMPARISON OF UTILITY FUNCTIONS
## PART III OF COMPARISON STUDY: COMPARISON WITH FUJIKAWA'S PARAMETERS
##
## Author: LD Sauer, IMBI Heidelberg, Germany
################################################################################

#' Run part II and III of the comparison study
#'
#' @inheritParams run_part1
#' @param file_prefix A character for quickly identifying different runs.
#' @param part3 Should I also calculate the performance measures for part III,
#' i.e. for the default parameters suggested by Fujikawa et al.
#'
#' @return `NULL`, results are saved in the `./out/` folder.
run_part2n3 <- function(file_prefix,
                        baby_setup = FALSE,
                        u_keys_override = NULL,
                        sc_keys_override = NULL,
                        workers = 20,
                        tmp = "tmp",
                        part3 = TRUE,
                        strategy = ifelse(Sys.info()["sysname"] == "Linux",
                                          "multicore", "multisession")){
  # GENERAL SETUP
  source(here::here("setup.R"))
  # - Defining global variables, setting seed and parallelization
  setup_study(baby_setup = baby_setup)
  check_output_dirs(c("./out", paste0("./out/", tmp)))
  check_empty_dirs(paste0("./out/", tmp))
  u_keys_select <- u_keys
  if(!is.null(u_keys_override)){
    u_keys_select <- u_keys_override
  }
  if(!is.null(sc_keys_override)){
    sc_keys <- intersect(sc_keys, sc_keys_override)
  }
  if(strategy == "sequential"){
    plan(strategy = "sequential")
  } else {
    message("Planning ", strategy, " session with ", workers," workers.")
    plan(strategy = strategy, workers = workers) 
  }
  options_future <- list(seed = TRUE,
                         packages = c("foreach"))
  # SETUP FOR PART II
  # - Manually selecting the best algorithm from part I
  alg_part2 <- "alg_grd"
  algorithm_part2 <- algorithms[[alg_part2]]
  algorithm_params_part2 <- alg_params[[alg_part2]]
  algorithm_params_part2$control <-
    c(algorithm_params_part2$control,
      list(future_apply_options =
             list(future.seed = options_future$seed,
                  future.packages = options_future$packages)))
  fn_name_part2 <- alg_fn_names[[alg_part2]]
  # EXECUTE PART II
  for(sc in sc_keys){
    for(u in u_keys_select){
      # Setting the same seed for every scenario-utility combination.
      # Of course, the seeds need to vary in the Monte-Carlo draws of basksim.
      set.seed(899, kind = "L'Ecuyer-CMRG")
      status_message(part = 2, list(u = u, sc = sc))
      # - Setting up Fujikawa's design, scenario, utility
      scenario <- scenarios[[sc]]
      p1s <- split(scenario$p1s, row(scenario$p1s))
      utility <- utilities[[u]]
      utility_params <- add_sc_to_u_params(u = u,
                                           utility_params = u_params[[u]],
                                           scenario = scenario)
      # - Scenarios where baskexact will be used as a backend
      if(sc %in% sc_keys_exact){
        design <- baskwrap::setup_fujikawa_x(k = scenario$k,
                                             shape1 = shape1, shape2 = shape2,
                                             p0 = scenario$p0,
                                             backend = "exact")
        detail_params <- detail_params_exact
        detail_params$n <- scenario$n
        # - Precalculating weights for speed-up
        weight_mat_vanilla <- baskwrap::weights_jsd_vanilla(
          design = design, n = scenario$n,
          logbase = detail_params$logbase)
        weights_from_save <- function(epsilon,
                                      tau,
                                      ...) {
          return(baskwrap::weights_fujikawa_tuned(weight_mat_vanilla,
                                                  epsilon = epsilon,
                                                  tau = tau))
        }
        detail_params$weight_fun <- weights_from_save
        # - Options for futures (foreach and future_apply)
        globals_for_future <- structure(TRUE,
                                        add = c("weight_mat_vanilla"))

        options_future <- c(options_future, globals = globals_for_future)
        algorithm_params_part2$control$future_apply_options <-
          c(algorithm_params_part2$control$future_apply_options,
            future.globals = globals_for_future)
      } else if(sc %in% sc_keys_sim){
        # - Scenarios where basksim will be used as a backend
        design <- baskwrap::setup_fujikawa_x(k = scenario$k,
                                             shape1 = shape1, shape2 = shape2,
                                             p0 = scenario$p0,
                                             backend = "sim")
        detail_params <- detail_params_sim
        detail_params$n <- scenario$n
      } else {
        stop("Scenario is invalid!")
      }
      # - Record the trace
      trace <- out_file(2, "trace", sc, u, "RDS",
                        file_prefix = paste0(tmp, "/", file_prefix))
      # - Run the optimization
      time_opt <- system.time(res_opt <- opt_design_gen(design = design,
                                                        utility = utility,
                                                        algorithm =
                                                          algorithm_part2,
                                                        detail_params =
                                                          detail_params,
                                                        utility_params =
                                                          utility_params,
                                                        algorithm_params =
                                                          algorithm_params_part2,
                                                        x_names = x_names,
                                                        fn_name =
                                                          fn_name_part2,
                                                        trace = trace,
                                                        trace_options = 
                                                          list(robust = TRUE),
                                                        final_details =
                                                          FALSE))
      # - Calculate performance measures in all scenarios for optimal result
      pb_message <- paste0("Calculating performance measures for optimal parameters found ",
                           "by utility function ", u, " in the scenario ", sc)
      message(pb_message)
      res_named <- res_opt$par
      names(res_named) <- x_names
      performance_measures <- 
        get_performance_measures(design = design,
                                 detail_params = detail_params,
                                 p1s = p1s, 
                                 res_par = res_named,
                                 u_keys = u_keys,
                                 utilities = utilities,
                                 u_params = u_params,
                                 scenario = scenario,
                                 options_future = options_future,
                                 pb_message = pb_message)
      res_list <- list(id = sc, alg = alg_part2, u = u, sc = sc,
                       time_opt = time_opt,
                       res_opt = res_opt,
                       scenario = scenario,
                       performance_measures = performance_measures,
                       memory = FALSE)
      save_res_list(data = res_list, 
                    file_prefix = paste0(tmp, "/", file_prefix), 
                    part = 2)
    }
    if(part3){
      # CALCULATE PERFORMANCE MEASURES FOR PART III
      pars_fuj <- list(fuj1 = c(lambda = 0.99, epsilon = 2, tau = 0),
                       fuj2 = c(lambda = 0.99, epsilon = 2, tau = 0.5))
      for(tag in c("fuj1", "fuj2")){
        pb_message <- paste0(
          "Calculating performance measures for the parameters",
          " suggested by Fujikawa et al. (", tag, ")")
        message(pb_message)
        par_fuj <- pars_fuj[[tag]]
        performance_measures <- 
          get_performance_measures(design = design,
                                   detail_params = detail_params,
                                   p1s = p1s, 
                                   res_par = par_fuj,
                                   u_keys = u_keys,
                                   utilities = utilities,
                                   u_params = u_params,
                                   scenario = scenario,
                                   options_future = options_future,
                                   pb_message = pb_message)
        
        res_list <- list(id = sc, alg = "", u = tag, sc = sc,
                         time_opt = c(user.self = NA_real_,
                                      sys.self = NA_real_,
                                      elapsed = NA_real_,
                                      user.child = NA_real_,
                                      sys.child = NA_real_),
                         res_opt = list(par = par_fuj, 
                                        value = NA_real_,
                                        trace = data.frame(lambda = NA_real_,
                                                           epsilon = NA_real_,
                                                           tau = NA_real_,
                                                           fn = NA_real_,
                                                           seed1 = NA_real_,
                                                           seed2 = NA_real_,
                                                           seed3 = NA_real_,
                                                           seed4 = NA_real_,
                                                           seed5 = NA_real_,
                                                           seed6 = NA_real_,
                                                           seed7 = NA_real_)),
                         scenario = scenario,
                         performance_measures = performance_measures,
                         memory = FALSE)
        save_res_list(data = res_list, 
                      file_prefix = paste0(tmp, "/", file_prefix), 
                      part = 2)
      }
    }
    
    # LOAD RESULTS
    results <- list_out_files(2, paste0("res_list_raw_", sc),
                              file_prefix, path = paste0("./out/", tmp)) %>%
      map(readRDS)
    # COMBINE AND SAVE RESULTS
    dat_opt <- combine_opt_output(results)
    dat_eff <- combine_alg_efficiency(results, algorithm_params_part2)
    dat_perf <- combine_utility_performance_measures(results)
    save_alg_efficiency(data = dat_eff, time_opt = NULL, alg = alg_part2,
                        memory = FALSE, file_prefix = file_prefix, part = 2,
                        fname1 = u, fname2 = sc)
    save_opt_output(data = dat_opt, res_opt = NULL, alg = alg_part2,
                    file_prefix = file_prefix, part = 2, fname1 = u, 
                    fname2 = sc)
    # save utility performance measures (no function needed, we only do it here)
    saveRDS(object = dat_perf,
            file = out_file(part = 2, "perform", fname1 = u, fname2 = sc, "RDS",
                            file_prefix = file_prefix))
  }
  # CLEANUP
  plan(sequential)
  return(NULL)
}
