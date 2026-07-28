# GENERATE BASH SCRIPTS AND R SCRIPTS FOR THE FINAL SIMULATION OF PART I
# ON A SLURM CLUSTER

# SETUP
setwd(here::here())
nworkers <- 50
n_alg_runs_override <- 50
ntasks <- nworkers * 2 # Twice the number of workers due to hyperthreading
memsize <- ntasks * 4 # Allocated RAM in GB
source("setup.R")
setup_study()
disclaimer_auto <- "#!/bin/bash\n# GENERATED AUTOMATICALLY BY gen_run_final_files.R, DO NOT EDIT BY HAND.\n\n"
master_run_file_path <- here::here("final", "master_run_final_part1.sh")
master_mktmpdir_path <- here::here("final", "master_mktmpdir_part1.sh")
cat(disclaimer_auto, file = master_run_file_path)
cat(disclaimer_auto, file = master_mktmpdir_path)

# BASH FILES
bash_string <- function(part, jobname, ntasks, memsize) {
  filepath <- "final/run_final_"
  return(paste0(
    '#!/bin/bash

# GENERATED AUTOMATICALLY BY gen_run_final_files.R, DO NOT EDIT BY HAND!

# SLURM SETTINGS
#SBATCH --nodes=1
#SBATCH --ntasks=',
    ntasks,
    '
#SBATCH --time=9-12
#SBATCH --mem=',
    memsize,
    'g
#SBATCH --job-name=',
    job_name,
    '
#SBATCH --partition=compute
#SBATCH --output="out/final_',
    job_name,
    '_slurm-%j.out"

# SESSION INFO
echo "Comparison study: Utility-based optimization of Fujikawa’s basket trial design"
echo "Log of job execution on bwForCluster BinAC 2"
echo "Job name: $SLURM_JOB_NAME"
echo "Temporary directory: $TMPDIR"
echo "Date:"
date
echo "Node details:"
uname -a
echo ""
echo ""

# LOAD MODULES
echo "--------------------------------------------------------------------------------"
echo "Loading modules:"
module load math/R/4.6.0-mkl-2025.0-gnu-14.2
module load bio/jags/4.3.2
echo ""
echo ""

# RUN R RSCRIPT
echo "--------------------------------------------------------------------------------"
echo "Rscript output:"
Rscript  ',
    filepath,
    job_name,
    '.R
'
  ))
}

# PART I
for (a in alg_keys) {
  for (u in u_keys_part1) {
    job_name <- paste("part1", a, u, sep = "_")
    tmp_file_name <- paste0("tmp_", job_name)
    bash_file_name <- paste0("run_final_", job_name, ".sh")
    if (!dir.exists(here::here("out", tmp_file_name))) {
      dir.create(here::here("out", tmp_file_name))
    }
    # GENERATING R SCRIPT
    cat(
      paste0(
        '# GENERATED AUTOMATICALLY BY gen_run_final_files.R, DO NOT EDIT BY HAND!

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
          alg_keys_override = "',
        a,
        '",
          u_keys_override = "',
        u,
        '",
          n_alg_runs_override = ',
        n_alg_runs_override,
        ',
          workers = ',
        nworkers,
        ',
          tmp = "',
        tmp_file_name,
        '",
          strategy = "cluster")

# LOG SESSION INFORMATION
print(sessionInfo())
'
      ),
      file = here::here("final", paste0("run_final_", job_name, ".R"))
    )
    # GENERATING BASH SCRIPT
    cat(
      bash_string(part = "part1", jobname, ntasks, memsize),
      file = here::here("final", bash_file_name)
    )
    # ADD COMMAND TO BASH FILE
    cat(
      text = paste0("sbatch final/", bash_file_name, "\n"),
      file = master_run_file_path,
      append = TRUE
    )
    cat(
      text = paste0("mkdir out/", tmp_file_name, "\n"),
      file = master_mktmpdir_path,
      append = TRUE
    )
  }
}

# PART II AND III
master_run_file_path <- here::here("final", "master_run_final_part2n3.sh")
master_mktmpdir_path <- here::here("final", "master_mktmpdir_part2n3.sh")
cat(disclaimer_auto, file = master_run_file_path)
cat(disclaimer_auto, file = master_mktmpdir_path)
for (sc in sc_keys) {
  for (u in u_keys) {
    job_name <- paste("part2n3", u, sc, sep = "_")
    tmp_file_name <- paste0("tmp_", job_name)
    bash_file_name <- paste0("run_final_", job_name, ".sh")
    if (!dir.exists(here::here("out", tmp_file_name))) {
      dir.create(here::here("out", tmp_file_name))
    }
    part3 <- u == "u_ewp"
    # GENERATING R SCRIPT
    cat(
      paste0(
        '# LIBRARIES
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
            u_keys_override = "',
        u,
        '",
            sc_keys_override = "',
        sc,
        '",
            workers = ',
        nworkers,
        ',
            tmp = "',
        tmp_file_name,
        '",
            part3 = ',
        part3,
        ',
            strategy = "cluster")
          
# LOG SESSION INFORMATION
print(sessionInfo())'
      ),
      file = here::here("final", paste0("run_final_", job_name, ".R"))
    )
    # GENERATING BASH SCRIPT
    cat(
      bash_string(part = "part2n3", jobname, ntasks, memsize),
      file = here::here("final", bash_file_name)
    )
    # ADD COMMAND TO BASH FILE
    cat(
      text = paste0("sbatch final/", bash_file_name, "\n"),
      file = master_run_file_path,
      append = TRUE
    )
    cat(
      text = paste0("mkdir out/", tmp_file_name, "\n"),
      file = master_mktmpdir_path,
      append = TRUE
    )
  }
}


# PART II and III REAL P1 SCENARIOS
master_run_file_path <- here::here(
  "final",
  "master_run_final_part2n3_realp1_frequentist.sh"
)
cat(disclaimer_auto, file = master_run_file_path)
sc_keys_real <- c("sc_med09", "sc_lrg20", "sc_med04", "sc_lrg03")
for (sc in sc_keys_real) {
  job_name <- paste0("part2n3_realp1_", sc)
  bash_file_name <- paste0("run_final_", job_name, ".sh")
  # GENERATING R SCRIPT
  cat(
    paste0(
      '# LIBRARIES
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
run_part2n3_real_scenarios(sc = "',
      sc,
      '", file_prefix = "final_")
print(paste("Stop time:", Sys.time()))
          
# LOG SESSION INFORMATION
print(sessionInfo())'
    ),
    file = here::here("final", paste0("run_final_", job_name, ".R"))
  )
  # GENERATING BASH SCRIPT
  cat(
    bash_string(part = "part2n3", jobname, ntasks, memsize),
    file = here::here("final", bash_file_name)
  )
  # ADD COMMAND TO BASH FILE
  cat(
    text = paste0("sbatch final/", bash_file_name, "\n"),
    file = master_run_file_path,
    append = TRUE
  )
}

# PART II FREQUENTIST COMPARATOR
job_name <- "part2n3_frequentist"
bash_file_name <- paste0("run_final_", job_name, ".sh")
# GENERATING R SCRIPT
cat(
  paste0(
    '# LIBRARIES
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
    file_prefix = "final_", sc_keys_real = ',
    'c(',
    paste(shQuote(sc_keys_real, type = 'sh'), collapse = ', '),
    ')',
    ')
print(paste("Stop time:", Sys.time()))
          
# LOG SESSION INFORMATION
print(sessionInfo())'
  ),
  file = here::here("final", paste0("run_final_", job_name, ".R"))
)
# GENERATING BASH SCRIPT
cat(
  bash_string(part = "part2n3", jobname, 4 * 2, 20),
  file = here::here("final", bash_file_name)
)
# ADD COMMAND TO BASH FILE
cat(
  text = paste0("sbatch final/", bash_file_name, "\n"),
  file = master_run_file_path,
  append = TRUE
)

# PART IV
master_run_file_path <- here::here("final", "master_run_final_part4.sh")
master_mktmpdir_path <- here::here("final", "master_mktmpdir_part4.sh")
cat(disclaimer_auto, file = master_run_file_path)
cat(disclaimer_auto, file = master_mktmpdir_path)
job_name <- "part4"
tmp_file_name <- paste0("tmp_", job_name)
bash_file_name <- paste0("run_final_", job_name, ".sh")
if (!dir.exists(here::here("out", tmp_file_name))) {
  dir.create(here::here("out", tmp_file_name))
}
# GENERATING R SCRIPT
cat(
  paste0(
    '# LIBRARIES
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
          workers = ',
    nworkers,
    ',
          tmp = "',
    tmp_file_name,
    '",
          strategy = "cluster")
          
# LOG SESSION INFORMATION
print(sessionInfo())'
  ),
  file = here::here("final", paste0("run_final_", job_name, ".R"))
)
# GENERATING BASH SCRIPT
cat(
  bash_string(part = "part4", jobname, ntasks, memsize),
  file = here::here("final", bash_file_name)
)
# ADD COMMAND TO BASH FILE
cat(
  text = paste0("sbatch final/", bash_file_name, "\n"),
  file = master_run_file_path,
  append = TRUE
)
cat(
  text = paste0("mkdir out/", tmp_file_name, "\n"),
  file = master_mktmpdir_path,
  append = TRUE
)
