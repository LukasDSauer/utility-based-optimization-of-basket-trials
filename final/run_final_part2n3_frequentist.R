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
source("run_part2n3_frequentist_design.R")

# RUN
print(paste("Start time:", Sys.time()))
run_part2n3_frequentist_design(
    file_prefix = "final_", sc_keys_real = c('sc_med09', 'sc_lrg20', 'sc_med04', 'sc_lrg03'))
print(paste("Stop time:", Sys.time()))
          
# LOG SESSION INFORMATION
print(sessionInfo())