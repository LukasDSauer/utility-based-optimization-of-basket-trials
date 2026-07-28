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
source("run_part2n3.R")
options(progressr.enable = TRUE)

# SLURM PROGRESS BAR,
handlers(global = TRUE)
handlers(handler_debug(interval = 5))

# RUN
print(paste("Start time:", Sys.time()))
run_part2n3(file_prefix = "final_",
            baby_setup = FALSE,
            u_keys_override = "u_ewp_avg",
            sc_keys_override = "sc_fuj03",
            workers = 50,
            tmp = "tmp_part2n3_u_ewp_avg_sc_fuj03",
            part3 = FALSE,
            strategy = "cluster")
          
# LOG SESSION INFORMATION
print(sessionInfo())