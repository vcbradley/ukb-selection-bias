#### Simulation ReadMe

Files:

- `simulation_prep.R` generates the folder structure and missingness equation(s) for a set of simulations
- `simulation_sample_gen.R` generates the samples for a given simulation
- `simulation_run.R` actually runs the simulation


## How to run

1. `ssh biobank`
2. Can run from anywhere:
`qsub /well/nichols/users/bwj567/mini-project-1/simulation/simulation_prep.R`
3. Must run from the directory of the specific simulation that you want to generate samples for:
i.e. 
`cd /well/nichols/users/bwj567/simulation/sim_1_5000_1`
`qsub /well/nichols/users/bwj567/mini-project-1/simulation/simulation_sample_gen.R`
4. `qsub /well/nichols/users/bwj567/mini-project-1/simulation/simulation_run.R`

To check on job status, `qstat`
To delete job, `qdel JOBID`
