################################################################################
## UTILITY-BASED OPTIMIZATION OF FUJIKAWA'S BASKET TRIAL DESIGN
## PART I OF COMPARISON STUDY: COMPARISON OF OPTIMIZATION ALGORITHMS
##
## Author: LD Sauer, IMBI Heidelberg, Germany
################################################################################

#' Run part I of the comparison study
#'
#' @param file_prefix A character for quickly identifying different runs.
#' @param baby_setup A logical, should the complete setup be used or a reduced
#' version for testing (smaller sample sizes, smaller grid, less runs etc.)?
#' @param baby_run A logical, should less algorithms be investigated?
#' @param alg_keys_override A numeric, default is `NULL`. If not `NULL`, this
#' manually overrides which algorithms will be investigated. Only works
#' if `baby_run == FALSE`, as this also overrides algorithm selection.
#' @param n_alg_runs_override A numeric, default is `NULL`. If not `NULL`, this
#' overrides `n_alg_runs` from `setup.R`, i.e. the number of times
#' that stochastic algorithms will be executed.
#' @param u_keys_override A numeric, default is `NULL`. If not `NULL`, this
#' manually overrides which utility functions will be investigated.
#' @param workers Number of workers for parallelization
#' @param tmp Name of the temporary directory
#' @param combine_only Should I only take existing simulation results from the
#' temporary directory and combine them instead of running a new simulation?
#' @param strategy Parallelization strategy (argument of `plan()` function).
#'
#' @return `NULL`; results are saved in the `./out/` folder.
run_part1 <- function(file_prefix,
                      baby_setup = FALSE,
                      baby_run = FALSE,
                      alg_keys_override = NULL,
                      n_alg_runs_override = NULL,
                      u_keys_override = NULL,
                      workers = 20,
                      tmp = "tmp",
                      combine_only = FALSE,
                      strategy = ifelse(Sys.info()["sysname"] == "Linux",
                                        "multicore", "multisession")){
  # GENERAL SETUP
  # - Loading setup function
  source(here::here("setup.R"))
  # - Recording total time
  runtimes_overall <- add_runtimes()
  start_overall <- add_start_time("part 1 in total")
  # - Defining global variables, setting seed and parallelization
  setup_study(baby_setup = baby_setup)
  if(!combine_only){
    check_output_dirs(c("./out", "./out/profiles", paste0("./out/", tmp)))
    check_empty_dirs(paste0("./out/", tmp))
  }
  run_label <- "complete"
  if(baby_setup | baby_run | !is.null(n_alg_runs_override) |
     !is.null(alg_keys_override) | !is.null(u_keys_override)){
    run_label <- paste(c("restricted",
                         alg_keys_override,
                         u_keys_override), collapse = "_")
  }
  if(!combine_only){
    if(alg_keys_override == "alg_cby"){
      message("Planning sequential session because alg_cby doesn't need parallelization.")
      plan(sequential)
    } else if(strategy == "multisession"){
      message("Planning multisession with ", workers," workers.")
      plan(multisession, workers = workers)
    }
    else if(strategy == "multicore"){
      if(!parallelly::supportsMulticore()){
        stop(paste("Trying to run multicore on Linux, but multicore is not",
                   "supported. Perhaps you are executing this script from",
                   "RStudio."))
      }
      message("Planning multicore with ", workers," workers.")
      plan(multicore, workers = workers)
    } else if (strategy == "cluster"){
      message("Planning cluster session with ", workers," workers.")
      plan(cluster, workers = workers)
    } else {
      message("Planning sequential session.")
      plan(sequential)
    }
  }
  # SETUP FOR PART I
  # - Overwrite n_alg_runs
  if(!is.null(n_alg_runs_override)){
    n_alg_runs <- n_alg_runs_override
  }
  # - Selection of utility functions and scenarios according to simulation
  #   protocol
  if(baby_run){
    u_keys_part1 <- u_keys_part1[[1]]
  } else if (!is.null(u_keys_override)){
    u_keys_part1 <- u_keys_override
  }
  sc_part1 <- "sc_bau04"
  scenario_part1 <- scenarios[[sc_part1]]
  design_part1 <- baskwrap::setup_fujikawa_x(k = scenario_part1$k,
                                             shape1 = shape1, shape2 = shape2,
                                             p0 = scenario_part1$p0,
                                             backend = "exact")
  detail_params_part1 <- detail_params_exact
  detail_params_part1$n <- scenario_part1$n

  # - Precalculating weights for speed-up
  weight_mat_vanilla <- baskwrap::weights_jsd_vanilla(
    design = design_part1, n = scenario_part1$n,
    logbase = detail_params_part1$logbase)
  weights_from_save <- function(epsilon,
                                tau,
                                ...) {
    return(baskwrap::weights_fujikawa_tuned(weight_mat_vanilla,
                                            epsilon = epsilon,
                                            tau = tau))
  }
  detail_params_part1$weight_fun <- weights_from_save
  # - Options for futures (foreach and future_apply)
  globals_for_future <- structure(TRUE,
                                  add = c("weight_mat_vanilla"))
  options_future <- list(seed = TRUE,
                         globals = globals_for_future)


  # - Algorithm execution function
  execute_algorithm <- function(id, utility, utility_params,
                                algorithm, algorithm_params,
                                trace = FALSE,
                                trace_options = list(robust = FALSE),
                                format_result = NULL,
                                progress_bar = NULL){
    memory <- FALSE
    memory_file_name <- NULL
    if(id %in% seq(from = 1, to = n_alg_runs, by = 5)){
      memory_file_name <- out_file(1, "profile", alg, u, "out",
                                   file_prefix = paste0("profiles/",
                                                        file_prefix),
                                   file_suffix = paste0("_", id))
      # Default profiling interval is 0.02, but this results in profile file
      # of several GB on Linux.
      Rprof(memory_file_name, interval = 2,
            memory.profiling=TRUE)
    }
    if(trace){
      trace <- out_file(1, "trace", alg, u, "RDS",
                             file_prefix = paste0(tmp, "/", file_prefix),
                             file_suffix = paste0("_", id))
    }
    time_opt <- system.time(res_opt <- opt_design_gen(design = design_part1,
                                                      utility = utility,
                                                      algorithm = algorithm,
                                                      detail_params =
                                                        detail_params_part1,
                                                      utility_params =
                                                        utility_params,
                                                      algorithm_params =
                                                        algorithm_params,
                                                      x_names = x_names,
                                                      fn_name = fn_name,
                                                      trace = trace,
                                                      trace_options =
                                                        trace_options,
                                                      format_result =
                                                        format_result,
                                                      progress_bar = progress_bar
                                                      ))
    if(id %in% seq(from = 1, to = n_alg_runs, by = 5)){
      Rprof(NULL)
      memory <- TRUE
    }
    res_list <- list(id = id, alg = alg, u = u, sc = sc_part1,
                     time_opt = time_opt, res_opt = res_opt,
                     scenario = scenario_part1,
                     memory = memory,
                     memory_file_name = memory_file_name)
    save_res_list(data = res_list,
                  file_prefix = paste0(tmp, "/", file_prefix),
                  part = 1)
    return(res_list)
  }

  execute_parallel_algorithm_runs <- function(utility, utility_params,
                                              alg, algorithm, algorithm_params,
                                              n_alg_runs){
      pb <- progressr::progressor(steps = n_alg_runs*(n_maxit + 60))
      pb(paste0("Executing ", n_alg_runs, " runs of ", alg, "."),
         class = "sticky", amount = 0)
      format_stoch_out = NULL
      if(!is.null(alg_format_out[[alg]])){
        format_stoch_out = function(res) {return(alg_format_out[[alg]](
          res = res,
          utility = utility,
          utility_params = utility_params,
          design = design_part1,
          detail_params = detail_params_part1
        ))}
      }
      # The simulated annealing algorithm optimizr::simann is also sending status
      # updates, but these are not escaping the foreach call.
      foreach(id=(1:n_alg_runs),
             .options.future = options_future) %dofuture% {
               result <- execute_algorithm(id = id, utility,
                                           utility_params,
                                           algorithm, algorithm_params,
                                           trace =
                                             TRUE, #alg_trace_manual[[alg]],
                                           format_result = format_stoch_out,
                                           progress_bar = pb)
               result
             }
  }

  # EXECUTE PART I
  # - Stochastic optimization algorithms
  if(baby_run){
    alg_keys_stoch <- c("alg_sa10")
    alg_keys_determ <- c("alg_grd")
  } else if(!is.null(alg_keys_override)){
    alg_keys_stoch <- intersect(alg_keys_override, alg_keys_stoch)
    alg_keys_determ <- intersect(alg_keys_override, alg_keys_determ)
  }
  for(alg in alg_keys_stoch){
    start_time <- add_start_time(alg)
    # The same seed is used for every algorithm, but of course not for every
    # algorithm run.
    set.seed(1856, kind = "L'Ecuyer-CMRG")

    # Loading algorithm parameters
    algorithm <- algorithms[[alg]]
    algorithm_params <- alg_params[[alg]]
    fn_name <- alg_fn_names[[alg]]

    for(u in u_keys_part1){
      # Loading utility parameters
      status_message(part = 1, list(u = u, alg = alg))
      utility <- utilities[[u]]
      utility_params <- u_params[[u]]
      utility_params$p1s <- scenario_part1$p1s
      if(!combine_only){
        # Run, algorithm, run!
        execute_parallel_algorithm_runs(utility, utility_params, alg,
                                        algorithm, algorithm_params,
                                        n_alg_runs)
      }
      results <- list_out_files(1,
                                paste("res_list_raw", alg, u, sep = "_"),
                                file_prefix,
                                path = paste0("./out/", tmp)) %>%
        map(readRDS)
      # Combining and saving results
      dat_eff <- combine_alg_efficiency(results, algorithm_params)
      dat_opt <- combine_opt_output(results)
      save_alg_efficiency(data = dat_eff, time_opt = NULL, alg = alg,
                          u = u, sc_part1, memory = FALSE, file_prefix = file_prefix)
      save_opt_output(data = dat_opt, res_opt = NULL, alg = alg,
                      u = u, sc_part1, file_prefix = file_prefix)
    }
    runtimes_overall <- add_runtimes(start_time, alg, runtimes_overall)
  }

  # - Stochastic unbounded optimization algorithms
  if(ifelse(is.null(alg_keys_override),
            TRUE, "alg_saun" %in% alg_keys_override)){
    start_time <- add_start_time(alg)
    # Setting seed as before
    set.seed(1856, kind = "L'Ecuyer-CMRG")

    # Loading algorithm parameters
    alg <- alg_keys_unb[[1]]
    algorithm <- algorithms[[alg]]
    algorithm_params <- alg_params[[alg]]
    fn_name <- alg_fn_names[[alg]]
    for(u in u_keys_part1){
      # Report status
      status_message(part = 1, list(u = u, alg = alg))

      # Loading utility parameters
      utility <- utilities_bnd[[u]]
      utility_params <- u_params_bnd[[u]]
      utility_params$utility_params$p1s <- scenario_part1$p1s

      # Running the algorithms
      if(!combine_only){
        execute_parallel_algorithm_runs(utility, utility_params, alg,
                                        algorithm, algorithm_params,
                                        n_alg_runs)
      }
      # Combining and saving results
      results <- list_out_files(1,
                                paste("res_list_raw", alg, u, sep = "_"),
                                file_prefix,
                                path = paste0("./out/", tmp)) %>%
        map(readRDS)
      dat_eff <- combine_alg_efficiency(results, algorithm_params)
      dat_opt <- combine_opt_output(results)
      save_alg_efficiency(data = dat_eff, time_opt = NULL, alg = alg,
                          u = u, sc_part1, memory = FALSE, file_prefix = file_prefix)
      save_opt_output(data = dat_opt, res_opt = NULL, alg = alg,
                      u = u, sc_part1, file_prefix = file_prefix)
    }
    runtimes_overall <- add_runtimes(start_time, alg, runtimes_overall)
  }


  # - Deterministic optimization algorithms
  for(alg in alg_keys_determ){
    start_time <- add_start_time(alg)
    set.seed(1856, kind = "L'Ecuyer-CMRG")
    # Loading algorithm parameters
    algorithm <- algorithms[[alg]]
    algorithm_params <- alg_params[[alg]]
    if(alg == "alg_grd"){
      algorithm_params$control <-
        c(algorithm_params$control,
          list(future_apply_options =
            list(future.seed = TRUE,
                 future.globals = globals_for_future)))
      trace_options <- list(robust = TRUE)
    } else if (alg == "alg_cby") {
      trace_options <- list(robust = FALSE)
    }
    fn_name <- alg_fn_names[[alg]]

    for(u in u_keys_part1){
      # Report status
      status_message(part = 1, list(u = u, alg = alg))

      # Loading utility parameters
      utility <- utilities[[u]]
      utility_params <- u_params[[u]]
      utility_params$p1s <- scenario_part1$p1s

      format_det_out = NULL
      if(!is.null(alg_format_out[[alg]])){
        format_det_out = function(res) {return(alg_format_out[[alg]](
          res = res,
          utility = utility,
          utility_params = utility_params,
          design = design_part1,
          detail_params = detail_params_part1
        ))}
      }

      pb <- NULL
      if(alg == "alg_cby"){
        pb <- progressr::progressor(steps = n_maxit + 60)
        pb(paste0("Executing one run of ", alg, "."),
           class = "sticky", amount = 0)
      }

      # Running the algorithm
      if(!combine_only){
        result <- execute_algorithm(id = 1,
                                    utility,
                                    utility_params,
                                    algorithm,
                                    algorithm_params,
                                    trace =
                                      TRUE, #alg_trace_manual[[alg]],
                                    trace_options =
                                      trace_options,
                                    format_result = format_det_out,
                                    progress_bar = pb)
      }
      else {
        # Combining and saving results
        result <- list_out_files(1,
                                  paste("res_list_raw", alg, u, sep = "_"),
                                  file_prefix,
                                  path = paste0("./out/", tmp)) %>%
          map(readRDS)
      }
      # Combining and saving results
      save_alg_efficiency(data = NULL, result$time_opt, alg, u, sc_part1,
                          memory = TRUE,
                          algorithm_params = algorithm_params,
                          scenario = scenario_part1,
                          file_prefix = file_prefix)
      save_opt_output(data = NULL, result$res_opt, alg, u, sc_part1,
                      file_prefix = file_prefix)
    }
    runtimes_overall <- add_runtimes(start_time, alg, runtimes_overall)
  }

  # CLEANUP
  plan(sequential)
  runtimes_overall <- add_runtimes(start_overall,
                                   "part 1 in total",
                                   runtimes_overall)
  attr(runtimes_overall, "baby_setup") <- baby_setup
  attr(runtimes_overall, "baby_run") <- baby_run
  attr(runtimes_overall, "n_alg_runs_override") <- n_alg_runs_override
  attr(runtimes_overall, "u_keys_override") <- u_keys_override
  saveRDS(runtimes_overall,
          out_file(1, "runtimes", "overall", run_label, "RDS",
                   file_prefix = file_prefix))
  return(NULL)
}

# # Look at the memory profile of id 1
# profvis::profvis(prof_input = out_file(1, "profile", alg, u, "out",
#                                        file_prefix = paste0("profiles/",
#                                                             file_prefix),
#                                        file_suffix = "_1")
