################################################################################
## UTILITY-BASED OPTIMIZATION OF FUJIKAWA'S BASKET TRIAL DESIGN
## PART II AND III OF COMPARISON STUDY: INVESTIGATING PERFORMANCE IN REAL
## SCENARIOS
##
## Author: LD Sauer, IMBI Heidelberg, Germany
################################################################################

# sc_keys_real <- c("sc_med09", "sc_lrg20", "sc_med04", "sc_lrg03")
# part2n3_date <- 20251002
run_part2n3_frequentist_design <- function(file_prefix, sc_keys_real) {
  # Load setup data
  source(here::here("setup.R"))
  setup_study(baby_setup = FALSE)
  options_future <- list(seed = TRUE, packages = c("foreach"))
  alpha <- 0.025
  for (sc in sc_keys) {
    # - Setting up Fujikawa's design, scenario, utility
    scenario <- scenarios[[sc]]
    design <- basksim::setup_binomial(
      k = scenario$k,
      p0 = scenario$p0,
    )
    p1s <- scenario$p1s
    p1s <- split(p1s, row(p1s))
    if (sc %in% sc_keys_real) {
      p1s[["real_p1"]] <- if_else(
        scenario$p1_real > design$p0,
        scenario$p1_real,
        design$p0
      )
    }
    pb_message <- paste0(
      "Calculating performance measures for the frequentist binomial design",
      " in the scenario ",
      sc
    )
    performance_measures <- foreach(
      i = (1:length(p1s)),
      .options.future = options_future,
      .combine = rbind
    ) %dofuture%
      {
        p1 <- p1s[[i]]
        res_perf <- basksim::get_details(
          design = design,
          n = scenario$n,
          p1 = p1,
          alpha = alpha
        )
        res_df <-
          data.frame(
            sc = sc,
            id_run = if_else(names(p1s)[i] == "real_p1", "real_p1", sc),
            u = "freq",
            epsilon = NA_real_,
            tau = NA_real_,
            lambda = NA_real_,
            alpha = alpha,
            Rejection_Probabilities = t(
              res_perf$Rejection_Probabilities
            ),
            FWER = res_perf$FWER,
            EWP = res_perf$EWP,
            ECD = res_perf$ECD,
            p0 = design$p0,
            p1 = t(p1)
          )
      }

    saveRDS(
      object = performance_measures,
      file = out_file(
        part = 2,
        "perform",
        fname1 = "frequentist",
        fname2 = sc,
        "RDS",
        file_prefix = file_prefix
      )
    )
  }

  return(NULL)
}
