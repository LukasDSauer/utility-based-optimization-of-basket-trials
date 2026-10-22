################################################################################
## UTILITY-BASED OPTIMIZATION OF FUJIKAWA'S BASKET TRIAL DESIGN
## PART IV OF COMPARISON STUDY: EXPLORATORY ANALYSES
##
## Author: LD Sauer, IMBI Heidelberg, Germany
################################################################################

#' Run the exploratory analyses of the comparison study ("part IV")
#' 
#' By default, the comparison of HLD and JSD is run for `"sc_bau04"`, but other
#' scenario sets can be specified via `sc_keys_override`
#'
#' @inheritParams run_part1
#' 
#' @return `NULL`, results are saved in the `./out/` folder.
run_part4 <- function(file_prefix,
                      baby_setup = FALSE,
                      sc_keys_override = NULL,
                      workers = 20,
                      tmp = "tmp",
                      strategy = ifelse(Sys.info()["sysname"] == "Linux",
                                        "multicore", "multisession")){
  # SETUP
  source(here::here("setup.R"))
  setup_study(baby_setup = baby_setup)
  check_output_dirs(c("./out", paste0("./out/", tmp)))
  check_empty_dirs(paste0("./out/", tmp))
  # Load algorithm, scenario and utility function
  if(!is.null(sc_keys_override)){
    if(length(sc_keys_override) > 1){
      stop("For part 4, sc_keys_override can only contain one element. (This could be adapted in the future.)")
    }
    # Default of sc_explo comes from setup_study()
    sc_explo <- sc_keys_override
  }
  algorithm <- algorithms[[alg_explo]]
  
  algorithm_params <- alg_params[[alg_explo]]
  scenario <- scenarios[[sc_explo]]
  p1s <- split(scenario$p1s, row(scenario$p1s))
  utility <- utilities[[u_explo]]
  utility_params <- add_sc_to_u_params(u = u_explo,
                                       utility_params = u_params[[u_explo]],
                                       scenario = scenario)
  set.seed(20250610, kind = "L'Ecuyer-CMRG")
  future::plan(strategy = strategy, workers = workers)
  
  # QUESTION 1:
  # Can the Jensen-Shannon divergence be replaced by the Hellinger distance?
  # How does this change the performance measures?
  message("Part 4: Exploratory analysis - Comparison of JSD and HLD")
  explo1_tag <- paste0("jsd_vs_hld_", sc_explo)
  if(sc_explo %in% sc_keys_exact){
    # - Scenarios where baskexact will be used as a backend
    design <- baskwrap::setup_fujikawa_x(k = scenario$k,
                                         shape1 = shape1, shape2 = shape2,
                                         p0 = scenario$p0,
                                         backend = "exact")
    detail_params <- detail_params_exact
  } else if(sc_explo %in% sc_keys_sim){
    # - Scenarios where basksim will be used as a backend
    design <- baskwrap::setup_fujikawa_x(k = scenario$k,
                                         shape1 = shape1, shape2 = shape2,
                                         p0 = scenario$p0,
                                         backend = "sim")
    detail_params <- detail_params_sim
  } else {
    stop("Scenario is invalid!")
  }
  detail_params$n <- scenario$n
  # Optimize design with custom weight matrix
  opt_with_weight_custom <- function(weight_custom, id){
    # Tune loaded weights
    weights_from_save <- function(epsilon,
                                      tau,
                                      ...) {
      return(baskwrap::weights_fujikawa_tuned(weight_custom,
                                              epsilon = epsilon,
                                              tau = tau))
    }
    detail_params$weight_fun <- weights_from_save
    # - Options for futures (foreach and future_apply)
    globals_for_future <- structure(TRUE,
                                    add = c("weight_custom"))
    options_future <- list(seed = TRUE,
                           globals = globals_for_future,
                           packages = c("foreach"))
    algorithm_params$control <-
      c(algorithm_params$control,
        list(future_apply_options =
               list(future.globals = globals_for_future,
                    future.seed = options_future$seed,
                    future.packages = options_future$packages)))
    time_opt <- system.time(res_opt <- opt_design_gen(design = design,
                              utility = utility,
                              algorithm =
                                algorithm,
                              detail_params =
                                detail_params,
                              utility_params =
                                utility_params,
                              algorithm_params =
                                algorithm_params,
                              x_names = x_names,
                              trace = "none",
                              final_details =
                                FALSE))
    # - Calculate performance measures in all scenarios for optimal result
    pb_message <- paste0("Calculating performance measures for optimal",
                         " parameters found ",
                         "by utility function ", u_explo, " in the scenario ", 
                         sc_explo)
    message(pb_message)
    res_named <- res_opt$par
    names(res_named) <- x_names
    # A hack that I don't entirely understand. Calling doFuture the second time,
    # it doesn't automatically, remember the weight_custom. But we can remind it.
    weight_custom <- weight_custom
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
    # - Save results
    res_list <- list(id = id,
                     alg = alg_explo, u = u_explo, 
                     sc = sc_explo,
                     time_opt = time_opt,
                     res_opt = res_opt,
                     scenario = scenario,
                     performance_measures = performance_measures,
                     memory = FALSE)
    save_res_list(data = res_list, 
                  file_prefix = paste0(tmp, "/", file_prefix), 
                  part = 4)
    return(list(res_list))
  }
  
  # Precalculating weights for speed-up, then optimize with the different weight 
  # matrices
  eff_weights_mat <- microbenchmark::microbenchmark(
    baskwrap::weights_jsd_vanilla(design = design, n = scenario$n, logbase = detail_params$logbase),
    baskwrap::weights_hld_vanilla(design = design, n = scenario$n), 
    times = 100)
  time_mat_jsd <- system.time(weight_custom <- baskwrap::weights_jsd_vanilla(
    design = design, n = scenario$n, logbase = detail_params$logbase))
  res_jsd <- opt_with_weight_custom(weight_custom = weight_custom, id = "jsd")
  time_mat_hld <- system.time(weight_custom <- baskwrap::weights_hld_vanilla(
    design = design, n = scenario$n))
  res_hld <- opt_with_weight_custom(weight_custom = weight_custom, id = "hld")
  # LOAD RESULTS
  results <- list_out_files(4, paste0("res_list_raw_", sc_explo),
                            file_prefix, path = paste0("./out/", tmp)) %>%
    map(readRDS)
  # COMBINE AND SAVE RESULTS
  dat_opt <- combine_opt_output(results)
  dat_eff <- combine_alg_efficiency(results, algorithm_params)
  dat_perf <- combine_utility_performance_measures(results)
  # Efficiency of weight matrices
  dat_eff_weights_mat <- cbind(weights = c("jsd", "hld"), summary(eff_weights_mat))
  dat_eff_weights_run <- cbind(weights = c("jsd", "hld"),
                           as.data.frame(rbind(t(time_mat_jsd),
                                               t(time_mat_hld))))
  save_alg_efficiency(data = dat_eff, time_opt = NULL, alg = alg_explo,
                      memory = FALSE, file_prefix = file_prefix,
                      part = 4, fname2 = explo1_tag)
  save_opt_output(data = dat_opt, res_opt = NULL, alg = alg_explo,
                  file_prefix = file_prefix, part = 4, fname2 = explo1_tag)
  # save utility performance measures
  saveRDS(object = dat_perf,
          file = out_file(part = 4, "perform",
                          fname1 = alg_explo, fname2 = explo1_tag, "RDS",
                          file_prefix = file_prefix))
  saveRDS(object = dat_eff_weights_mat,
          file = out_file(part = 4, "efficiency_weights_mat",
                          fname1 = alg_explo, fname2 = explo1_tag, "RDS",
                          file_prefix = file_prefix))
  saveRDS(object = dat_eff_weights_run,
          file = out_file(part = 4, "efficiency_weights_run",
                          fname1 = alg_explo, fname2 = explo1_tag, "RDS",
                          file_prefix = file_prefix))
  # Visualize different weights
  fig_jsd <- plot_weights(design = design, n = scenario$n, r1 = floor(scenario$n/3), 
               weight_fun = weights_jsd,
               weight_params = list(tau = res_jsd[[1]]$res_opt$par["tau"], 
                                    epsilon = res_jsd[[1]]$res_opt$par["epsilon"],
                                    logbase = detail_params$logbase))
  saveRDS(object = fig_jsd,
          file = out_file(part = 4, "weights",
                          fname1 = alg_explo, fname2 = paste0("jsd_", sc_explo), "RDS",
                          file_prefix = file_prefix))
  fig_hld <- plot_weights(design = design, n = scenario$n, r1 = floor(scenario$n/3), 
               weight_fun = weights_hld,
               weight_params = list(tau = res_hld[[1]]$res_opt$par["tau"], 
                                    epsilon = res_hld[[1]]$res_opt$par["epsilon"],
                                    logbase = detail_params$logbase))
  saveRDS(object = fig_hld,
          file = out_file(part = 4, "weights",
                          fname1 = alg_explo, fname2 = paste0("hld_", sc_explo), "RDS",
                          file_prefix = file_prefix))
  
  if(sc_explo == "sc_bau04"){
    # QUESTION 2:
    # How does borrowing affect the maximal TOER?
    message("Part 4: Exploratory analysis - Effect of borrowing on TOER")
    design_02 <- baskwrap::setup_fujikawa_x(k = 2,
                                         shape1 = shape1, shape2 = shape2,
                                         p0 = 0.2,
                                         backend = "exact")
    p0 <- 0.2
    p1s <- seq(p0, 1, by = 0.05)
    dat_explo_reject02 <- data.frame(expand.grid(p1 = p1s, 
                                             epsilon = 
                                               alg_params$alg_grd$axes$epsilon, 
                                             tau = alg_params$alg_grd$axes$tau))
    dat_explo_reject02 <- foreach(i=(1:nrow(dat_explo_reject02)),
                              .combine = rbind) %dofuture% {
      p1 <- dat_explo_reject02$p1[i]
      epsilon <- dat_explo_reject02$epsilon[i]
      tau <- dat_explo_reject02$tau[i]
      det <- get_details(
        design_02,
        n = 15,
        p1 = c(p0, p1),
        lambda = 0.95,
        epsilon = epsilon,
        tau = tau,
        logbase = detail_params_exact$logbase,
        weight_fun = detail_params_exact$weight_fun,
        which_details = "Rejection_Probabilities",
        verbose = FALSE)
      return(data.frame(p1 = p1, epsilon = epsilon, tau = tau,
                        toer01 = det$Rejection_Probabilities[1], 
                        power02 = det$Rejection_Probabilities[2]))
    }
    saveRDS(object = dat_explo_reject02,
            file = out_file(part = 4, "two",
                            fname1 = "basket", fname2 = "reject", "RDS",
                            file_prefix = file_prefix))
  }
}

