################################################################################
## HELPER FUNCTIONS
##
## Author: LD Sauer, IMBI Heidelberg, Germany
################################################################################

#' Check whether directories for simulation output exist
#'
#' @param dirs A character vector of directory names
#'
#' @returns No return, called for side effects.
check_output_dirs <- function(dirs = c("./out/")) {
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      stop(paste("The directory", dir, "is required but does not exist."))
    }
  }
  message("All necessary folders were found.")
}

#' Check whether directories for simulation output are empty
#'
#' @inheritParams dirs check_output_dirs
#' @inherit check_output_dirs return
check_empty_dirs <- function(dirs) {
  for (dir in dirs) {
    if (length(list.files(dir)) > 0) {
      stop(paste0("The directory ", dir, "/ must be empty."))
    }
  }
  message("All necessary directories were found empty.")
}

#' Generate a file name (and relative path) for an output file
#'
#' Concatenates all parts and automatically adds the current date to the file
#' name.
#'
#' @param part The part of the comparison study, 1, 2 or 4
#' @param file What is saved in the file, e.g. `"efficiency"`.
#' @param fname1 Additional part of the filename, e.g the algorithm key.
#' @param fname2 Additional part of the filename, e.g. the utility function key.
#' @param type The ending of the filename, e.g. `".RDS"`
#' @param file_prefix Additional prefix.
#' @param file_suffix Additional suffix.
#'
#' @return The desired file name as a character string.
out_file <- function(
  part,
  file = "",
  fname1,
  fname2,
  type,
  file_prefix = "",
  file_suffix = ""
) {
  return(paste0(
    paste(
      paste0("./out/", file_prefix, "part", part),
      file,
      fname1,
      fname2,
      gsub("-", "", Sys.Date()),
      sep = "_"
    ),
    file_suffix,
    ".",
    type
  ))
}

#' Return a list of files in a folder which have the same prefix, the same
#' part and the same file type.
#'
#' @inheritParams out_file
#' @param path The folder to search
#'
#' @return A list of available files
list_out_files <- function(part, type, file_prefix = "", path = "./out") {
  if (str_length(file_prefix) > 0) {
    file_prefix <- paste0("(?:^", file_prefix, ")", "(?:part", part, ")")
  } else {
    file_prefix <- paste0("(?:^part", part, ")")
  }
  pattern <- paste0(file_prefix, "(.*)(?:", type, ").*(?:.RDS)")
  list.files(path = path, pattern = pattern, full.names = TRUE) %>%
    return(.)
}

#' Save one step of the comparison study to an RDS file
#'
#' @param data A data frame containing the results of the calculation
#' @inheritParams out_file
#'
#' @inherit check_output_dirs return
save_res_list <- function(data, file_prefix = "", part = 1) {
  if (part == 1) {
    file <- paste0("res_list_raw_", data$alg)
    fname1 <- data$u
    fname2 <- data$id
  } else if (part == 2) {
    file <- paste0("res_list_raw_", data$sc)
    fname1 <- data$alg
    fname2 <- data$u
  } else if (part == 4) {
    file <- paste0("res_list_raw_", data$sc)
    fname1 <- data$id
    fname2 <- data$u
  }
  saveRDS(
    object = data,
    file = out_file(
      part,
      file,
      fname1,
      fname2,
      "RDS",
      file_prefix = file_prefix
    )
  )
}

#' Prepare the data on efficiency of one step of the comparison study
#'
#' This function is called by `combine_alg_efficiency()`.
#'
#' @param time_opt An object containing the run time of the step.
#' @param alg The algorithm used in that step.
#' @param u The utility function considered in that step.
#' @param sc A character, the key of the scenario considered in that step.
#' @param id An identifier for that step, e.g. a number in part 1.
#' @param memory A boolean that tells you whether memory usage was recorded in
#' that step.
#' @param algorithm_params A list of parameters used for the algorithm run.
#' @param scenario A list, the precise scenario considered in that step.
#' @param file_prefix A character prefix of the file name of the memory file.
#' @param memory_file_name The complete memory file name.
#'
#' @returns A one-row data frame.
prepare_alg_efficiency_row <- function(
  time_opt,
  alg,
  u,
  sc,
  id,
  memory,
  algorithm_params,
  scenario,
  file_prefix = "",
  memory_file_name = out_file(
    1,
    "profile",
    alg,
    u,
    "out",
    file_prefix,
    file_suffix = paste0("_", id)
  )
) {
  mem.total <- NA_real_
  if (memory) {
    mem.total <- summaryRprof(memory_file_name, memory = "both")$by.total[[
      "\"opt_design_gen\"",
      "mem.total"
    ]]
  }
  n_maxit <- NA_real_
  if (!is.null(algorithm_params$control$maxit)) {
    n_maxit <- algorithm_params$control$maxit
  } else if (!is.null(algorithm_params$maxIter)) {
    n_maxit <- algorithm_params$maxIter
  } else if (!is.null(algorithm_params$control$maxeval)) {
    n_maxit <- algorithm_params$control$maxeval
  } else if (!is.null(algorithm_params$axes)) {
    n_maxit <- length(algorithm_params$axes$lambda) *
      length(algorithm_params$axes$epsilon) *
      length(algorithm_params$axes$tau)
  }
  return(data.frame(
    alg = alg,
    u = u,
    sc = sc,
    id_run = id,
    user.self = time_opt["user.self"],
    sys.self = time_opt["sys.self"],
    elapsed = time_opt["elapsed"],
    mem.total = mem.total,
    n = scenario$n,
    k = scenario$k,
    p1s_length = nrow(scenario$p1s),
    n_maxit = n_maxit
  ))
}

#' Combine data on efficiency of the comparison study's steps into one large
#' data frame
#'
#' Takes a list of the results of each step, extracts the necessary information
#' using `prepare_alg_efficiency_row()` and then combines these results
#'
#' @param results A list, each entry contains the results of one step of the
#' comparison study.
#' @inheritParams prepare_alg_efficiency_row
#'
#' @returns A data frame
combine_alg_efficiency <- function(results, algorithm_params) {
  data <- do.call(
    rbind,
    lapply(results, function(x) {
      return(prepare_alg_efficiency_row(
        time_opt = x$time_opt,
        alg = x$alg,
        u = x$u,
        sc = x$sc,
        id = x$id,
        memory = x$memory,
        algorithm_params = algorithm_params,
        scenario = x$scenario,
        memory_file_name = x$memory_file_name
      ))
    })
  )
  row.names(data) <- NULL
  return(data)
}

#' Save the combined data on efficiency to an RDS file
#'
#' @param data A data frame, as returned by `combine_alg_efficiency()`
#' @inheritParams prepare_alg_efficiency_row
#' @inheritParams out_file
#'
#' @inherit check_output_dirs return
save_alg_efficiency <- function(
  data,
  time_opt,
  alg,
  u,
  sc,
  memory,
  algorithm_params = NULL,
  scenario = NULL,
  file_prefix = "",
  part = 1,
  fname1 = alg,
  fname2 = u
) {
  if (is.null(data)) {
    data <- prepare_alg_efficiency_row(
      time_opt,
      alg,
      u,
      sc,
      id = 1,
      memory,
      algorithm_params,
      scenario,
      file_prefix = paste0("profiles/", file_prefix)
    )
  }
  rownames(data) <- NULL
  saveRDS(
    object = data,
    file = out_file(
      part,
      "efficiency",
      fname1,
      fname2,
      "RDS",
      file_prefix = file_prefix
    )
  )
}

#' Prepare the data on optimal parameters of one step of the comparison study
#'
#' This function is called by `combine_opt_output()`.
#'
#' @inheritParams prepare_alg_efficiency_row
#'
#' @returns A one-row data frame.
prepare_opt_output_res_row <- function(res_opt, alg, u, sc, id) {
  return(cbind(
    alg = alg,
    u = u,
    sc = sc,
    id_run = id,
    as.data.frame(t(res_opt$par)),
    value = res_opt$value
  ))
}

#' Prepare the trace of one step of the comparison study
#'
#' This function is called by `combine_opt_output()`.
#'
#' @inheritParams prepare_alg_efficiency_row
#'
#' @returns A data frame.
prepare_opt_output_trace_row <- function(res_opt, alg, u, sc, id) {
  return(cbind(alg = alg, u = u, sc = sc, id_run = id, res_opt$trace))
}

#' Combine data on optimal parameters and trace of the comparison study's steps
#' into two data frames
#'
#' Takes a list of the results of each step, extracts the necessary information
#' using `prepare_opt_output_res_row()` and `prepare_opt_output_trace_row()`
#' and then combines these results into two data frames
#'
#' @inheritParams combine_alg_efficiency
#'
#' @returns A list of two data frames
combine_opt_output <- function(results) {
  data_res <- do.call(
    rbind,
    lapply(results, function(x) {
      return(prepare_opt_output_res_row(
        res_opt = x$res_opt,
        alg = x$alg,
        u = x$u,
        sc = x$sc,
        id = x$id
      ))
    })
  )
  data_trace <- NULL
  if (!is.null(results[[1]]$res_opt$trace)) {
    data_trace <- do.call(
      rbind,
      lapply(results, function(x) {
        return(prepare_opt_output_trace_row(
          res_opt = x$res_opt,
          alg = x$alg,
          u = x$u,
          sc = x$sc,
          id = x$id
        ))
      })
    )
    row.names(data_trace) <- NULL
  }
  row.names(data_res) <- NULL
  return(list(res = data_res, trace = data_trace))
}

#' Save the combined data on optimal parameters and trace to an RDS file
#'
#' @param data A data frame, as returned by `combine_opt_output()`
#' @inheritParams out_file
#'
#' @inherit check_output_dirs return
save_opt_output <- function(
  data,
  res_opt,
  alg,
  u,
  sc,
  file_prefix = "",
  part = 1,
  fname1 = alg,
  fname2 = u
) {
  if (is.null(data)) {
    data <- list(
      res = prepare_opt_output_res_row(res_opt, alg, u, sc, id = 1),
      trace = prepare_opt_output_trace_row(res_opt, alg, u, sc, id = 1)
    )
  }
  saveRDS(
    object = data,
    file = out_file(
      part,
      "opt",
      fname1,
      fname2,
      "RDS",
      file_prefix = file_prefix
    )
  )
}

#' Runs a simulation for utility function performance measures
#'
#' Takes the optimal result found by the utility functions and calculates the
#' performance measures from part II, i.e. stratum-wise power and TOER, EWP and
#' FWER, ECD and utility function values.
#'
#' @param design The design object, constructed by `baskwrap`
#' @param detail_params The detail parameters to be supplied the utility
#' function
#' @param p1s A matrix, a set of stratum-wise true response rates
#' @param res_par The optimal parameters for the utility functions found by
#' optimization
#' @param u_keys A character vector, the utility functions' identifying keys
#' @param utilities The list of all utility functions, indexed by the `u_keys`
#' @param u_params The list of necessary parameters for the utility function.
#' @param scenario Further details on the considered scenario (sample size,
#' number of strata, null response rate,)
#' @param options_future Options for the `doFuture` call to `foreach`
#' @param pb_message A character string, which message should be displayed in
#' the progress bar.
#'
#' @returns A list, each row containing performance measures for one utility
#' functions
get_performance_measures <- function(
  design,
  detail_params,
  p1s,
  res_par,
  u_keys,
  utilities,
  u_params,
  scenario,
  options_future,
  pb_message
) {
  pb <- progressor(
    steps = length(p1s) + length(u_keys),
    message = paste(pb_message)
  )
  pb(amount = 0)
  if (design$backend == "sim") {
    detail_params$iter <- 10000
  }
  final_detail_params <- detail_params
  final_detail_params$which_details = "all"
  detail_params$p1 <- NULL
  u_vals <- foreach(
    u = u_keys,
    .options.future = options_future,
    .combine = c
  ) %dofuture%
    {
      utility <- utilities[[u]]
      utility_params <- add_sc_to_u_params(
        u = u,
        utility_params = u_params[[u]],
        scenario = scenario
      )
      fn <- do.call(
        utility,
        c(
          design = list(design),
          x = list(res_par),
          detail_params = list(detail_params),
          utility_params
        )
      )
      pb()
      names(fn) <- u
      fn
    }
  performance_measures <- foreach(
    p1 = p1s,
    .options.future = options_future
  ) %dofuture%
    {
      final_detail_params$p1 <- p1
      res_perf <- do.call(
        baskwrap::get_details,
        c(design = list(design), as.list(res_par), final_detail_params)
      )
      res_perf[["u_vals"]] <- u_vals
      pb()
      res_perf
    }
  return(performance_measures)
}

#' Prepare the one block of performance measures of a utility function
#'
#' This function is called by `combine_utility_performance_measures()`.
#'
#' @param A list of performance measures
#' @inheritParams prepare_alg_efficiency_row
#'
#' @returns A data frame.
prepare_utility_performance_measures_block <- function(
  performance_measures_list,
  res_opt,
  alg,
  u,
  sc,
  id
) {
  return(do.call(
    rbind,
    lapply(performance_measures_list, function(x) {
      row <- cbind(
        alg = alg,
        u = u,
        sc = sc,
        id_run = id,
        as.data.frame(t(res_opt$par)),
        value = res_opt$value,
        FWER = x$FWER,
        EWP = x$EWP,
        ECD = x$ECD,
        Rejection_Probabilities = t(x$Rejection_Probabilities),
        p0 = x$p0,
        p1 = t(x$p1),
        as.data.frame(t(x$u_vals))
      )
      if (sc %in% sc_keys_sim) {
        row <- cbind(
          row,
          FWER_SE = x$FWER_SE,
          EWP_SE = x$EWP_SE,
          ECD_SE = x$ECD_SE,
          Rejection_Probabilities_SE = t(x$Rejection_Probabilities_SE)
        )
      }
      return(row)
    })
  ))
}

#' Combine data on performance measures of the utility functions
#'
#' Takes a list of the results of each combination of scenario and
#' utility function, extracts the necessary information
#' using `prepare_utility_performance_measures_block()`
#' and then combines these results into a data frame
#'
#' @inheritParams combine_alg_efficiency
#'
#' @returns A data frame
combine_utility_performance_measures <- function(results) {
  data_res <- do.call(
    rbind,
    lapply(results, function(x) {
      return(prepare_utility_performance_measures_block(
        performance_measures_list = x$performance_measures,
        res_opt = x$res_opt,
        alg = x$alg,
        u = x$u,
        sc = x$sc,
        id = x$id
      ))
    })
  )
  row.names(data_res) <- NULL
  return(data_res)
}

#' Status messages for the current state of the calculations for the comparison
#'study
#'
#' @param part An integer. What part of the comparison study is the current
#' calculation? `1` means part I, `2` means part II or III.
#' @param info_list A list containing info on the current step. These are
#' algorithm and utility function for part I, scenario and utility function for
#' part II.
#'
#' @returns No explicit return value, called for side effects.
status_message <- function(part, info_list) {
  if (part == 1) {
    message(paste0(
      "Running algorithm ",
      info_list$alg,
      " for the utility function ",
      info_list$u,
      "."
    ))
  }
  if (part == 2) {
    message(paste0(
      "Optimizing the utility function ",
      info_list$u,
      " for the scenario ",
      info_list$sc,
      "."
    ))
  }
}


#' Add a scenario to a list of utility parameters.
#'
#' Takes a list of utility parameters for a utility function and updates
#' its `p1` argument for single-scenario utility functions, or its `p1s`
#' argument for scenario-averaged utility functions with the `p1` from the
#' respective scenario.
#'
#' @param u A string, denoting a key notifying a utility function, e.g. `"u_ewp"`.
#' @param utility_params A list of utility parameters.
#' @param scenario A list of parameters for an outcome scenario.
#'
#' @returns The updated list of utility parameters.
add_sc_to_u_params <- function(u, utility_params, scenario) {
  if (u %in% u_keys_single) {
    utility_params$p1 <- scenario$p1_single
  }
  if (u %in% u_keys_avg) {
    utility_params$p1s <- scenario$p1s
  }
  if (u %in% u_keys_avg_pen) {
    utility_params$p1s <- scenario$p1s
  }
  return(utility_params)
}


#' Replace a vector by the value that is the closest in a list of vectors.
#'
#' @param x A numeric vector.
#' @param target A list of numeric vectors nearby.
#'
#' @returns A numeric vector, the vector in `target` that is the closest to `x`.
round_to_closest <- function(x, target) {
  distances <- outer(x, target, "-")
  closest_cell <- apply(distances, 1, function(x) which.min(abs(x)))
  return(target[closest_cell])
}


#' Return the current time and optionally display a message of the current time.
#'
#' @param label An optional string describing the current moment in time.
#'
#' @returns A `Sys.time()` object.
add_start_time <- function(label = NULL) {
  start_time <- Sys.time()
  if (!is.null(label)) {
    message("Starting run(s) of ", label, " at ", start_time, ".")
  }
  return(start_time)
}
#' Add the current time to a data frame of times and calculate the
#' difference to a start time.
#'
#' @inheritParams add_start_time
#' @param start_time A `Sys.time()` object denoting the start time of the run.
#' Used for calculating a duration.
#'
#' @returns A data frame of times and their labels.
add_runtimes <- function(start_time, label, data = NULL) {
  if (is.null(data)) {
    return(data.frame(alg = character(), time = numeric()))
  }
  stop_time <- Sys.time()
  diff_time <- stop_time - start_time
  message(
    "Finished run(s) of ",
    label,
    " after ",
    diff_time,
    " ",
    units(diff_time),
    "."
  )
  return(rbind(data, data.frame(label = label, time = diff_time)))
}

#' Helper function for converting centers of rectangles into their corners'
#' coordinates in order to to be supplied to a geom_tile ggplot object.
#'
#'
#' @param data A data frame.
#' @param xname A string, which column in the data frame is the x axis of the rectangle centers?
#' @param yname A string, which column in the data frame is the y axis of the rectangle centers?
#' @param xaxis A numeric vector, the x axis ticks of the rectangle centers
#' @param yaxis A numeric vector, the y axis ticks of the rectangle centers
#'
#' @returns The data frame appended with four columns `paste0(xname, "min")`,
#' `paste0(xname, "max")` defining the four corners of the rectangle.
get_rect_width_and_height <- function(data, xname, yname, xaxis, yaxis) {
  helper <- function(x, vec) {
    pos <- which(vec == x)
    left <- (vec[pos] - vec[pos - 1]) / 2
    right <- (vec[pos + 1] - vec[pos]) / 2
    if (length(left) == 0) {
      left <- right
    } else if (is.na(right)) {
      right <- left
    }
    return(data.frame(bmin = x - left, bmax = x + right))
  }
  xbounds <- bind_rows(lapply(data[[xname]], function(x) {
    helper(x, vec = xaxis)
  }))
  ybounds <- bind_rows(lapply(data[[yname]], function(x) {
    helper(x, vec = yaxis)
  }))
  data[[paste0(xname, "min")]] <- xbounds$bmin
  data[[paste0(xname, "max")]] <- xbounds$bmax
  data[[paste0(yname, "min")]] <- ybounds$bmin
  data[[paste0(yname, "max")]] <- ybounds$bmax
  return(data)
}


#' The Monte Carlo standard error of a rate estimate
#'
#' @param rate A numeric, the rate estimate
#' @param n_sim A numeric, the number of simulation
#'
#' @returns The Monte Carlo standard error of the rate estimate
mcse_rate <- function(rate, n_sim) {
  return(sqrt(rate * (1 - rate) / n_sim))
}

#' The Monte Carlo standard error of a mean estimate
#'
#' This uses an unbiased estimator for the standard deviation, not the classic
#' sample standard deviation
#'
#' @param x A numeric vector for which to estimate the MCSE
#' @param n_sim A numeric, the number of simulations
#' @param na.rm A logical, whether to remove NAs from x
#'
#' @returns The Monte Carlo standard error of the mean estimate
mcse_mean <- function(x, n_sim, na.rm = FALSE) {
  return(sd_unb(x, na.rm = na.rm) / sqrt(n_sim))
}

#' The Monte Carlo standard error of the standard deviation estimator
#'
#' This uses an unbiased estimator for the standard deviation, not the classic
#' sample standard deviation in order to estimate the standard error of the
#' standard deviation (sic).
#'
#' @param x A numeric vector for which to estimate the MCSE
#' @param na.rm A logical, whether to remove NAs from x
#'
#' @returns The Monte Carlo standard error of the mean estimate
mcse_sd_unb <- function(x, na.rm = FALSE) {
  n <- length(x)
  c_n <- sd_correct(n)
  return(c_n * sd(x, na.rm = na.rm) * sqrt(1 - 1 / c_n^2))
}

#' Unbiased standard deviation
#'
#' Unbiased estimator for the standard deviation of a vector `x`
#'
#' @param x A numeric vector
#' @param na.rm A logical, whether to remove NAs from x
#'
#' @returns A numeric
sd_unb <- function(x, na.rm = FALSE) {
  n <- length(x)
  c_n <- sd_correct(n)
  return(c_n * sd(x, na.rm = na.rm))
}

#' Correction factor for the bias of the standard deviation
#'
#' @param n Sample size
#'
#' @returns A numeric
sd_correct <- function(n) {
  if (n == 1) {
    return(NaN)
  }
  return(gamma((n - 1) / 2) / gamma(n / 2) * sqrt((n - 1) / 2))
}


#' Formats the data frame columns starting with "p1." to obtain different styles
#' of labels for the respective scenario, e.g. a character string of the whole
#' vector, the number of active strata, information which is the "real"
#' scenario.
#'
#' @param data A data frame containing columns whose names start with "p1."
format_scenario_labels <- function(data){
  data %<>% 
    unite(col = "p1_lab", starts_with("p1."), remove = FALSE, sep = ", ") %>%
    mutate(p1_lab_num = factor(rowSums(across(starts_with("p1."), ~ .x > p0))),
           p1_lab_str = as.character(p1_lab_num),
           p1_lab_sc_bau04 =
             factor(case_when(p1_lab == "0.15, 0.15, 0.15, 0.15" ~ "0 active",
                              p1_lab == "0.15, 0.15, 0.15, 0.4" ~ "1 active",
                              p1_lab == "0.15, 0.15, 0.4, 0.4" ~ "2 active",
                              p1_lab == "0.15, 0.4, 0.4, 0.4" ~ "3 active",
                              p1_lab == "0.4, 0.4, 0.4, 0.4" ~ "4 active",
                              p1_lab == "0.4, 0.4, 0.3, 0.5" ~ "1 i. t. middle",
                              p1_lab == "0.15, 0.25, 0.35, 0.45" ~ "linear",
                              TRUE ~ ""),
                    levels = sc_bau04_levels),
           p1_lab_sc_bau04_short =
             factor(case_when(p1_lab == "0.15, 0.15, 0.15, 0.15" ~ "0",
                              p1_lab == "0.15, 0.15, 0.15, 0.4" ~ "1",
                              p1_lab == "0.15, 0.15, 0.4, 0.4" ~ "2",
                              p1_lab == "0.15, 0.4, 0.4, 0.4" ~ "3",
                              p1_lab == "0.4, 0.4, 0.4, 0.4" ~ "4",
                              p1_lab == "0.4, 0.4, 0.3, 0.5" ~ "4(1m)",
                              p1_lab == "0.15, 0.25, 0.35, 0.45" ~ "3(l)",
                              TRUE ~ ""),
                    levels = sc_bau04_levels_short)
    )
  if(any(data$id_run == "real_p1")){
    p1_lab_str_levels <- c(as.character(unique(data$p1_lab_str[which(data$id_run != "real_p1")])), 
                           paste(data$p1_lab_str[which(data$id_run == "real_p1")][1], "(real p1)"))
    data %<>% 
      mutate(p1_lab_str = if_else(id_run == "real_p1",
                                  paste(p1_lab_str, "(real p1)"), p1_lab_str),
             p1_lab_str = factor(p1_lab_str,
                                 levels = p1_lab_str_levels))
  }
  return(data)
}

#' Are the parameters found by the utility function u_..._avg and u_..._avg_pen
#' identical?
#'
#' @param data A data frame
#' @param u_select A string denoting the utility function's key
is_avg_and_pen_equal <- function(data, u_select){
  all(data[which(data$u == paste0(u_select, "_avg"))[1],
           c("lambda", "epsilon", "tau")] ==
        data[which(data$u == paste0(u_select, "_avg_pen"))[1],
             c("lambda", "epsilon", "tau")])
}

#' Remove line on u_..._avg from data set if the parameters found by the utility
#' function u_..._avg and u_..._avg_pen are identical
#'
#' @param data A data frame
#' @param u_select A string denoting the utility function's key
filter_avg_pen <- function(data, u_select){
  data %>% 
    filter(u != paste0(u_select, "_avg")) %>% 
    mutate(u = if_else(u == paste0(u_select, "_avg_pen"),
                       paste0(u_select, "_avg and _avg_pen"),
                       u))
}