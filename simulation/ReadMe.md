# Simulation ReadMe

### Files:

- `simulation/simulation_prep.R` generates the folder structure and missingness equation(s) for a set of simulations
- `simulation/simulation_sample_gen.R` generates the samples for a given simulation (separate from the prep so we can re-generate/generate additional samples without altering the missingness mechanism)
- `simulation/simulation_run.R` actually runs the simulation
- `simulation/simulation_analysis.R` concatenates the results from all simulation runs across different proportions of missingness and generates aggregated summaries that can be moved off of rescomp
- `simulation/simulation_plots.R` uses the summary data to generate result plots
- `weighting/weighting_functions.R` - this file is sourced by a few of the simulation-specific files (mainly `simulation_run.R`) and contains the 6 weighting functions as well as a function for running all at once for a single iteration of the simulation


### How to run a single simulation

1. SSH into rescomp
`ssh biobank`

2. *PREP* Generate the folder structure and missingness mechanisms. Need to set the number of equations (`n_equations`) and number of samples to generate PER EQUATION (`n_samples`) in the script.  These are used

 You can run using `qsub`, or manually (it doesn't take that long).

From anywhere on rescomp:
```qsub /well/nichols/users/bwj567/mini-project-1/simulation/simulation_prep.R```

3. *GEN SAMPLES* Must run from the directory of the specific simulation that you want to generate samples for:
i.e. 
```cd /well/nichols/users/bwj567/simulation/sim_1_5000_1```
```qsub /well/nichols/users/bwj567/mini-project-1/simulation/simulation_sample_gen.R```


4. *RUN SIM* Navigate to the directory for the set of samples that you want to run analysis for
i.e. `/well/nichols/users/bwj567/simulation/sim_1_5000_1/samples/prop_0.02`
and run
```qsub /well/nichols/users/bwj567/mini-project-1/simulation/simulation_run.R```
Note that you have to do this separately for each proportion missing

5. *GENERATE SUMMARY* Once results are finished, navigate to the simulation directory
```cd /well/nichols/users/bwj567/simulation/sim_1_5000_1```
and run the following R code:
```/well/nichols/users/bwj567/mini-project-1/simulation/simulation_analysis.R```
This generates a `results_summary.rda` object that can be pulled off of rescomp. 


6. *PLOT RESULTS* In the simulation directory 
```cd /well/nichols/users/bwj567/simulation/sim_1_5000_1```
run 
```/well/nichols/users/bwj567/mini-project-1/simulation/simulation_plots.R```
This will generate a series of result plots that are saved in a `plots/` subdirectory



### Other Notes

To check on job status, `qstat`
To delete job, `qdel JOBID`
