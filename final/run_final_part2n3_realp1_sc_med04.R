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
source("run_part2n3_real_scenarios.R")
options(progressr.enable = TRUE)

# SLURM PROGRESS BAR,
handlers(global = TRUE)
handlers(handler_debug(interval = 5))

# RUN
print(paste("Start time:", Sys.time()))
run_part2n3_real_scenarios(sc = "sc_med04", file_prefix = "final_")
print(paste("Stop time:", Sys.time()))
          
# LOG SESSION INFORMATION
print(sessionInfo())