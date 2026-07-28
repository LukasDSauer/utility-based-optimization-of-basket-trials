# Utility-based optimization of Fujikawa’s basket trial design - Program code of the comparison study

This comparison study looks at different optimization algorithms and utility 
functions for the optimization of information borrowing in one-stage basket 
trial designs. The exemplary basket trial design studied in this project is the 
beta-binomial design by [Fujikawa et al. 2019](https://onlinelibrary.wiley.com/doi/10.1002/bimj.201800404). 
In order to increase objectivity, the comparison study was prespecified in a 
study protocol ([Sauer et al. 2024](https://doi.org/10.1371/journal.pone.0323097)).

## Reproducing the results

The final files for executing the simulation study are the master bash files

* `final/master_run_final_part1.sh`,
* `final/master_run_final_part2n3.sh`, and
* `final/master_run_final_part4.sh`.

These were set up to run R scripts on the high 
performance cluster *BinAC2*
of the University of Tübingen, Germany (part of the *bwForCluster* of the
State of Baden-Württemberg). For execution, one needs to follow the steps below:

1. **Go to the project folder** in a command line tool of your choice.
2. **Make sure that R and JAGS are available:** On the cluster, we used
   `module load math/R/4.6.0-mkl-2025.0-gnu-14.2; module load bio/jags/4.3.2`.
3. **Restore the R virtual environment:** Run an R session on your system using
   `R`. In that R session, execute `renv::restore()`.
4. **Create the temp folders:** You can use the three bash scripts 
   - `final/master_mktmpdir_part1.sh`,
   - `final/master_mktmpdir_part2n3.sh`, and
   - `final/master_mktmpdir_part4.sh`
   
   to create the necessary temp files folders.
   You might need to set them to executable (e.g. using `chmod` on Linux).
5. **Execute the simulation files (first part):**
   
   - **Option A:** On a high performance cluster with SLURM such as the bwForCluster, one 
      needs to execute the `sbatch` commands in the master bash files
     - `final/master_run_final_part1.sh`,
     - `final/master_run_final_part2n3.sh`, and
     - `final/master_run_final_part4.sh`.
   
   - **Option B:** On a "regular" Linux server or a Windows machine, one can directly execute
      the R files in the terminal using `Rscript final/run_final_part<filename>.R`. Note that
      one needs to adapt the term `strategy = "cluster"` to `strategy = "multicore"` on Linux or  
      `strategy = "multisession"` on Windows.
    
   The main output files are saved in the folder `out/`. To monitor the process
   during the run, execute
   `squeue --format="%i %9P %60j %10u %.2t %.10M %.6D %.4C %10R" --user=$USER`
   (the format is optional) or look at the log files with `nano out/*.out`.
6. **Execute the simulation files (second part):** After successfully running 
   the files above, execute the file 
   `final/master_run_final_part2n3_realp1_frequentist.sh` (performance measures on the
   real scenarios and for the separate frequentist design).
7. **Combine output:** Finally, create an empty folder `data/`. Then, combine 
   output files by running `Rscript combine_out_data.R`.
