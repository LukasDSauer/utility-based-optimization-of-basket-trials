################################################################################
## PROJECT SETUP
##
## Author: LD Sauer, IMBI Heidelberg, Germany
################################################################################

# LIBRARIES
library(baskwrap)
library(baskoptr)
library(doFuture)
library(foreach)
library(tidyverse)
library(progressr)
library(microbenchmark)

# SETUP FUNCTION
#' @param baby_setup A logical. For trouble shooting and development, use
#' a setup that runs a lot faster than the actual simulation study.
setup_study <- function(baby_setup = FALSE){
  # PRIOR PARAMETERS for Fujikawa's basket trial design
  shape1 <- 1
  shape2 <- 1
  logbase <- exp(1)
  # PARAMETER SPACE
  # - Start value
  x_start <- c(lambda = 0.2,
                 epsilon = 0.5,
                 tau = 0)
  # - Lower bound of parameter space
  x_lower <- c(lambda = 2.2250738585072014e-308,
             epsilon = 0,
             tau = 0)
  # - Upper bound of parameter space
  x_upper <- c(lambda = 0.9999999999,
             epsilon = 25,
             tau = 1)
  # - Axes of the parameter space for grid search
  if(baby_setup){
    x_axes <- list(lambda =  c(0.2, 0.9),
                   epsilon = c(0.5, 2),
                   tau = c(0.0, 0.5))
  } else {
    x_axes <- list(lambda =  c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99,
                               0.999),
                   epsilon = c(0.0, 0.5, 1.0, 1.5, 2.0, 5.0, 10.0, 15.0, 20.0,
                               25.0),
                   tau = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0))
  }
  # - Names of parameters
  x_names <- names(x_start)

  # DETAIL PARAMS
  detail_params_exact <- list(p1 = NULL, # placeholder, filled in later
                              n = NULL, # placeholder
                              weight_fun = baskexact::weights_fujikawa,
                              logbase = logbase,
                              verbose = FALSE)
  detail_params_sim <- list(p1 = NULL, # placeholder, filled in later
                            n = NULL, # placeholder
                            iter = 1000,
                            logbase = logbase,
                            use_future = FALSE)

  # OUTCOME SCENARIOS
  sc_keys <- c("sc_fuj03",
               "sc_bau04",
               "sc_kra08",
               "sc_med09",
               "sc_lrg20",
               "sc_med04",
               "sc_lrg03")
  sc_keys_exact <- c("sc_fuj03",
                     "sc_bau04")
  sc_keys_sim <- c("sc_kra08",
                   "sc_med09",
                   "sc_lrg20",
                   "sc_med04",
                   "sc_lrg03")
  sc_fuj03 <- c(n = 24,
                baskoptr::get_scenarios(k = 3, p0 = 0.2, p1 = 0.5),
                list(p1_single = c(0.2, 0.5, 0.5)))
  sc_bau04 <- c(n = 20,
                baskoptr::get_scenarios(k = 4, p0 = 0.15, p1 = 0.4),
                list(p1_single = c(0.15, 0.15, 0.4, 0.4)))
  sc_bau04$p1s <- rbind(sc_bau04$p1s,
                        c(0.4, 0.4, 0.3, 0.5),
                        c(0.15, 0.25, 0.35, 0.45))
  sc_bau04_levels <- c("0 active", "1 active", "2 active", 
                       "3 active", "4 active", "1 i. t. middle", 
                       "linear")
  sc_bau04_levels_short <- c("0", "1", "2", "3", "4", "4(1m)", "3(l)")
  sc_kra08 <- c(n = 15,
                baskoptr::get_scenarios(k = 8, p0 = 0.15, p1 = 0.45),
                list(p1_single = c(rep(0.15, 4), rep(0.45, 4))))
  sc_med09 <- c(n = 23,
                baskoptr::get_scenarios(k = 9, p0 = 0.01, p1 = 0.1),
                list(
                  p1_single = c(rep(0.01, 4), rep(0.1, 5)),
                  p1_real = c(0.056, 0.000, 0.113, 0.143, 0.043,
                            0.000, 0.286, 0.065, 0.362)))
  sc_lrg20 <- c(n = 24,
                baskoptr::get_scenarios(k = 20, p0 = 0.1, p1 = 0.35, by = 2),
                list(
                  p1_single = c(rep(0.1, 10), rep(0.35, 10)),
                  p1_real = c(0.160, 0.174, 0.120, 0.120, 0.167,
                            0.043, 0.130, 0.304, 0.080, 0.042,
                            0.200, 0.259, 0.063, 0.115, 0.000,
                            0.174, 0.115, 0.333, 0.091, 0.056)))
  sc_med04 <- c(n = 36,
                baskoptr::get_scenarios(k = 4, p0 = 0.1, p1 = 0.35),
                list(
                  p1_single = c(0.1, 0.1, 0.35, 0.35),
                  p1_real = c(0.156, 0.167, 0.212, 0.205)))
  sc_lrg03 <- c(n = 54,
                baskoptr::get_scenarios(k = 3, p0 = 0.15, p1 = 0.3),
                list(p1_single = c(0.15, 0.3, 0.3),
                     p1_real = c(0.289, 0.315, 0.333)))

  if(baby_setup){
    sc_fuj03$n <- 10
    sc_bau04$n <- 10
    sc_kra08$n <- 10
    sc_med09$n <- 10
    sc_lrg20$n <- 10
    sc_med04$n <- 10
    sc_lrg03$n <- 10
  }
  scenarios <- list(sc_fuj03 = sc_fuj03,
                    sc_bau04= sc_bau04,
                    sc_kra08 = sc_kra08,
                    sc_med09 = sc_med09,
                    sc_lrg20 = sc_lrg20,
                    sc_med04 = sc_med04,
                    sc_lrg03 = sc_lrg03)
  # UTILITY FUNCTIONS
  u_keys <- c("u_ewp", "u_ecd", "u_2ewp", "u_2pow",
              "u_ewp_avg", "u_ecd_avg", "u_2ewp_avg", "u_2pow_avg",
              "u_ewp_avg_pen", "u_ecd_avg_pen", "u_2ewp_avg_pen",
              "u_2pow_avg_pen")
  u_keys_single <- c("u_ewp", "u_ecd", "u_2ewp", "u_2pow")
  u_keys_avg <- c("u_ewp_avg", "u_ecd_avg", "u_2ewp_avg", "u_2pow_avg")
  u_keys_avg_pen <- c("u_ewp_avg_pen", "u_ecd_avg_pen", "u_2ewp_avg_pen",
                      "u_2pow_avg_pen")
  u_keys_part1 <- c("u_ecd_avg", "u_2pow_avg")
  utilities <- list(u_ewp = baskoptr::u_ewp,
                    u_ecd = baskoptr::u_ecd,
                    u_2ewp = baskoptr::u_2ewp,
                    u_2pow = baskoptr::u_2pow,
                    u_ewp_avg = baskoptr::u_avg,
                    u_ecd_avg = baskoptr::u_avg,
                    u_2ewp_avg = baskoptr::u_avg,
                    u_2pow_avg = baskoptr::u_avg,
                    u_ewp_avg_pen = baskoptr::u_avg,
                    u_ecd_avg_pen = baskoptr::u_avg,
                    u_2ewp_avg_pen = baskoptr::u_avg,
                    u_2pow_avg_pen = baskoptr::u_avg)
  # - Labels for the utility functions
  u_labs_type <- list(freq = "separate frequentist\ntests",
                      fuj1 = "fuj1/2",
                      fuj2 = "fuj1/2",
                      u_ewp = "single-scenario", 
                      u_ecd = "single-scenario", 
                      u_2ewp = "single-scenario", 
                      u_2pow = "single-scenario",
                      u_ewp_avg = "scenario-averaged",
                      u_ecd_avg = "scenario-averaged", 
                      u_2ewp_avg = "scenario-averaged", 
                      u_2pow_avg = "scenario-averaged",
                      u_ewp_avg_pen = "penalized and\nsc.-averaged",
                      u_ecd_avg_pen = "penalized and\nsc.-averaged", 
                      u_2ewp_avg_pen = "penalized and\nsc.-averaged", 
                      u_2pow_avg_pen = "penalized and\nsc.-averaged")
  u_labs_family <- list(freq = "freq",
                        fuj1 = "fuj1",
                        fuj2 = "fuj2",
                        u_ewp = "u_ewp", 
                        u_ecd = "u_ecd", 
                        u_2ewp = "u_2ewp", 
                        u_2pow = "u_2pow",
                        u_ewp_avg = "u_ewp",
                        u_ecd_avg = "u_ecd", 
                        u_2ewp_avg = "u_2ewp", 
                        u_2pow_avg = "u_2pow",
                        u_ewp_avg_pen = "u_ewp",
                        u_ecd_avg_pen = "u_ecd", 
                        u_2ewp_avg_pen = "u_2ewp", 
                        u_2pow_avg_pen = "u_2pow")
  # - Parameters for the utility functions
  #   - Penalty xi_1, xi_2 = 1
  #   - Threshold eta_1 = 0.05, eta_2 = 0.1 as defined in the comparison
  #     protocol
  u_prm_ewp <- list(penalty = 1, thresh = 0.05)
  u_prm_ecd <- u_prm_ewp
  u_prm_2ewp <- list(penalty1 = 1, penalty2 = 1, threshold = 0.1)
  u_prm_2pow <- u_prm_2ewp
  u_prm_ewp_avg <-list(utility = baskoptr::u_ewp,
                       utility_params = u_prm_ewp,
                       p1s = NULL,
                       use_future = FALSE)
  u_prm_ecd_avg <-list(utility = baskoptr::u_ecd,
                       utility_params = u_prm_ecd,
                       p1s = NULL,
                       use_future = FALSE)
  u_prm_2ewp_avg <-list(utility = baskoptr::u_2ewp,
                        utility_params = u_prm_2ewp,
                        p1s = NULL,
                        use_future = FALSE)
  u_prm_2pow_avg <-list(utility = baskoptr::u_2pow,
                        utility_params = u_prm_2pow,
                        p1s = NULL,
                        use_future = FALSE)
  u_prm_maxtoer <- list(penalty_maxtoer = 1000,
                        threshold_maxtoer = 0.2)
  reduce_calculations_false <- function(utility_params){
    utility_params$utility_params$reduce_calculations <- FALSE
    return(utility_params)
  }
  u_prm_ewp_avg_pen <- c(reduce_calculations_false(u_prm_ewp_avg),
                         u_prm_maxtoer)
  u_prm_ecd_avg_pen <- c(reduce_calculations_false(u_prm_ecd_avg),
                         u_prm_maxtoer)
  u_prm_2ewp_avg_pen <- c(u_prm_2ewp_avg, u_prm_maxtoer)
  u_prm_2pow_avg_pen <- c(u_prm_2pow_avg, u_prm_maxtoer)
  u_params <- list(u_ewp = u_prm_ewp,
                   u_ecd = u_prm_ecd,
                   u_2ewp = u_prm_2ewp,
                   u_2pow = u_prm_2pow,
                   u_ewp_avg = u_prm_ewp_avg,
                   u_ecd_avg = u_prm_ecd_avg,
                   u_2ewp_avg = u_prm_2ewp_avg,
                   u_2pow_avg = u_prm_2pow_avg,
                   u_ewp_avg_pen = u_prm_ewp_avg_pen,
                   u_ecd_avg_pen = u_prm_ecd_avg_pen,
                   u_2ewp_avg_pen = u_prm_2ewp_avg_pen,
                   u_2pow_avg_pen = u_prm_2pow_avg_pen)
  # - Bounded utility functions and their parameters
  utilities_bnd <- rep(list(baskoptr::u_bnd), 12)
  names(utilities_bnd) <- u_keys
  u_params_bnd <- mapply(FUN = function(utility, utility_params){
    return(list(utility = utility,
                utility_params = utility_params,
                lower = x_lower,
                upper = x_upper))
    },
    utilities,
    u_params,
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE)


  # OPTIMIZATION ALGORITHMS
  # - All algorithms, stochastic/unbounded/deterministic algorithms
  #   We removed "alg_de" from alg_keys and alg_keys_stoch, see explanation
  #   in results.qmd.
  alg_keys <- c("alg_sa100", "alg_sa10", "alg_sa1", "alg_saun",
                "alg_gwo", "alg_cby", "alg_grd")
  alg_keys_stoch <- c("alg_sa100", "alg_sa10", "alg_sa1", "alg_gwo")
  alg_keys_unb <- c("alg_saun")
  alg_keys_determ <- c("alg_grd", "alg_cby")
  # Labels for algorithms
  alg_lvls <- c("alg_grd", "alg_cby", "alg_sa100", "alg_sa10", "alg_sa1", 
                "alg_saun", "alg_gwo")
  alg_Labs <- c(alg_grd = "Grid search", 
                alg_cby = "COBYLA", 
                alg_sa100 = "Sim. ann. (T=100, bnd.)",
                alg_sa10 = "Sim. ann. (T=10, bnd.)",
                alg_sa1 = "Sim. ann. (T=1, bnd.)", 
                alg_saun = "Sim. ann. (T=10, unb.)", 
                alg_gwo = 'PSO ("grey wolf")')
  alg_labs <- c(alg_grd = "grid search", 
                alg_cby = "COBYLA", 
                alg_sa100 = "sim. ann. (T=100, bnd.)",
                alg_sa10 = "sim. ann. (T=10, bnd.)",
                alg_sa1 = "sim. ann. (T=1, bnd.)", 
                alg_saun = "sim. ann. (T=10, unb.)", 
                alg_gwo = 'PSO ("grey wolf")')
  # - Function names in the optimization algorithms
  alg_fn_names <- list(alg_sa100 = "fn",
                     alg_sa10 = "fn",
                     alg_sa1 = "fn",
                     alg_saun = "fn",
                     alg_de = "FUN",
                     alg_gwo = "FUN",
                     alg_cby = "fn",
                     alg_grd = "fn")
  if(baby_setup){
    n_maxit <- 100 # Maximal number of algorithm iterations
    n_alg_runs <- 2 # Number of runs for testing stochastic algorithms
  } else {
    n_maxit <- 1000 # Maximal number of algorithm iterations
    n_alg_runs <- 50 # Number of runs for testing stochastic algorithms
  }

  # - Manually defining COBYLA algorithm with convenience adjustment for
  #   maximization.
  cobyla_max <- function(x0,
                         fn,
                         lower = NULL,
                         upper = NULL,
                         hin = NULL,
                         nl.info = FALSE,
                         control = list(),
                         ...){
    return(nloptr::cobyla(x0 = x0,
                          fn = function(x) (-1)*fn(x),
                          lower = lower,
                          upper = upper,
                          hin = hin,
                          nl.info = nl.info,
                          control = control,
                          ...))
  }
  # - Optimization algorithms
  algorithms <- list(alg_sa100 = optimizr::simann,
                     alg_sa10 = optimizr::simann,
                     alg_sa1 = optimizr::simann,
                     alg_saun = stats::optim,
                     alg_de = metaheuristicOpt::DE,
                     alg_gwo = metaheuristicOpt::GWO,
                     alg_cby = cobyla_max,
                     alg_grd = optimizr::gridsearch)
  # - Parameters for simulated annealing with high starting temperature
  alg_prm_sa100 <- list(par = x_start,
                        lower = x_lower,
                        upper = x_upper,
                        control = list(maxit = n_maxit,
                                       temp = 100,
                                       tmax = 1,
                                       fnscale = -1,
                                       REPORT = 10))
  # - Parameters for simulated annealing with medium starting temperature
  alg_prm_sa10 <- alg_prm_sa100
  alg_prm_sa10$control$temp <- 10
  # - Parameters for simulated annealing with low starting temperature
  alg_prm_sa1 <- alg_prm_sa100
  alg_prm_sa1$control$temp <- 1
  # - Parameters for the differential evolution algorithm
  n_population <- 10
  alg_prm_de <- list(optimType = "MAX",
                     numVar = length(x_start),
                     numPopulation = n_population,
                     maxIter = round(n_maxit/n_population),
                     rangeVar = rbind(x_lower, x_upper),
                     scalingVector = 0.01)
  # - Parameters for the grey wolf optimizer
  alg_prm_gwo <- alg_prm_de
  alg_prm_gwo$scalingVector <- NULL
  # - Parameters for the COBYLA algorithm
  alg_prm_cby <- list(x0 = x_start,
                      lower = x_lower,
                      upper = x_upper,
                      nl.info = TRUE,
                      control = list(maxeval = n_maxit))
  # - Parameters for the grid search
  #   As we only conduct one run of gridsearch (no parallel runs, we can
  #   execute this run using parallelization, i.e. use_future = TRUE).
  alg_prm_grd <- list(axes = x_axes,
                      control = list(fnscale = -1,
                                     REPORT = 1,
                                     use_future = TRUE))
  # - Parameters for "unbounded" simulated annealing
  alg_params_saun <- list(par = x_start,
                          method = "SANN",
                          control = list(maxit = n_maxit,
                                         temp = 10,
                                         tmax = 1,
                                         fnscale = -1))
  # - List of parameters for optimization algorithms using unbounded utility
  #   functions
  alg_params <- list(alg_sa100 = alg_prm_sa100,
                     alg_sa10 = alg_prm_sa10,
                     alg_sa1 = alg_prm_sa1,
                     alg_saun = alg_params_saun,
                     alg_de = alg_prm_de,
                     alg_gwo = alg_prm_gwo,
                     alg_cby = alg_prm_cby,
                     alg_grd = alg_prm_grd)
  # - Formatting functions and trace recording for the output of
  #   metaheuristicOpt and COBYLA algorithms, as they only return the parameter
  #   vector
  format_cby_out <- function(res,
                            utility,
                            utility_params,
                            design,
                            detail_params){
    x <- res$par
    names(x) <- x_names
    # fn <- do.call(utility, c(design = list(design),
    #                          x = list(x),
    #                          detail_params = list(detail_params),
    #                          utility_params))
    return(list(par = x,
                value = (-1)*res$value )) # value = fn))
  }
  format_meta_out <- function(res,
                              utility,
                              utility_params,
                              design,
                              detail_params){
    names(res) <- x_names
    fn <- do.call(utility, c(design = list(design),
                             x = list(res),
                             detail_params = list(detail_params),
                             utility_params))
    return(list(par = res,
                value = fn))
  }
  alg_format_out <- list(alg_sa100 = NULL,
                         alg_sa10 = NULL,
                         alg_sa1 = NULL,
                         alg_saun = NULL,
                         alg_de = format_meta_out,
                         alg_gwo = format_meta_out,
                         alg_cby = format_cby_out,
                         alg_grd = NULL)
  alg_trace_manual <- list(alg_sa100 = FALSE,
                           alg_sa10 = FALSE,
                           alg_sa1 = FALSE,
                           alg_saun = FALSE,
                           alg_de = TRUE,
                           alg_gwo = TRUE,
                           alg_cby = TRUE,
                           alg_grd = FALSE)
  # - Exploratory analyses
  alg_explo <- "alg_grd"
  sc_explo <- "sc_bau04"
  u_explo <- "u_2pow_avg_pen"
  # - List of all study-relevant parameters
  setup_list <- list(x_names = x_names,
                     shape1 = shape1,
                     shape2 = shape2,
                     n_alg_runs = n_alg_runs,
                     n_maxit = n_maxit,
                     detail_params_exact = detail_params_exact,
                     detail_params_sim = detail_params_sim,
                     sc_keys = sc_keys,
                     sc_keys_exact = sc_keys_exact,
                     sc_keys_sim = sc_keys_sim,
                     sc_explo = sc_explo,
                     sc_bau04_levels = sc_bau04_levels,
                     sc_bau04_levels_short = sc_bau04_levels_short,
                     scenarios = scenarios,
                     u_keys = u_keys,
                     u_keys_single = u_keys_single,
                     u_keys_avg = u_keys_avg,
                     u_keys_avg_pen = u_keys_avg_pen,
                     u_keys_part1 = u_keys_part1,
                     u_labs_type = u_labs_type,
                     u_labs_family = u_labs_family,
                     u_explo = u_explo,
                     utilities = utilities,
                     u_params = u_params,
                     utilities_bnd = utilities_bnd,
                     u_params_bnd = u_params_bnd,
                     alg_keys = alg_keys,
                     alg_keys_stoch = alg_keys_stoch,
                     alg_keys_unb = alg_keys_unb,
                     alg_keys_determ = alg_keys_determ,
                     alg_explo = alg_explo,
                     alg_fn_names = alg_fn_names,
                     algorithms = algorithms,
                     alg_params = alg_params,
                     alg_format_out = alg_format_out,
                     alg_trace_manual = alg_trace_manual,
                     alg_lvls = alg_lvls,
                     alg_Labs = alg_Labs,
                     alg_labs = alg_labs)
  # Assign parameters to global environment and return list
  list2env(setup_list, envir = .GlobalEnv)
  message("Study parameters assigned to global environment.")
  return(NULL)
}

# LOAD UTILS
source(here::here("helpers.R"))
