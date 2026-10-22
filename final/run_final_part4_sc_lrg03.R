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
source("run_part4.R")
options(progressr.enable = TRUE)

# SLURM PROGRESS BAR,
handlers(global = TRUE)
handlers(handler_debug(interval = 5))

# RUN
print(paste("Start time:", Sys.time()))
run_part4(file_prefix = "final_",
          baby_setup = FALSE,
          sc_keys_override = "sc_lrg03",
          workers = 50,
          tmp = "tmp_part4_sc_lrg03",
          strategy = "cluster")
          
# LOG SESSION INFORMATION
print(sessionInfo())