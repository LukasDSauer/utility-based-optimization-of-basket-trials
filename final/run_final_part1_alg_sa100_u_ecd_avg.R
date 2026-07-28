# GENERATED AUTOMATICALLY BY gen_run_final_files.R, DO NOT EDIT BY HAND!

# LIBRARIES
require(renv)
library(progressr)

# SETUP
# Sandboxing is deactivated, as it heavily slows down the parallelized system.
Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = FALSE)
# Sandbox path could be set with the following line.
# Sys.setenv(RENV_PATHS_SANDBOX = "./renv-sandbox/")
renv::load()
setwd(here::here())
source("run_part1.R")
options(progressr.enable = TRUE)

# SLURM PROGRESS BAR,
handlers(global = TRUE)
handlers(handler_debug(interval = 5))

# RUN
print(paste("Start time:", Sys.time()))
run_part1(file_prefix = "final_",
          baby_setup = FALSE,
          alg_keys_override = "alg_sa100",
          u_keys_override = "u_ecd_avg",
          n_alg_runs_override = 50,
          workers = 50,
          tmp = "tmp_part1_alg_sa100_u_ecd_avg",
          strategy = "cluster")

# LOG SESSION INFORMATION
print(sessionInfo())
