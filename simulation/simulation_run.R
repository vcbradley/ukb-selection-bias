#!/apps/well/R/3.4.3/bin/Rscript                                                                                                                                                               
#$ -t 1:5000                                                                                                                                                                                   
#$ -cwd
### -j option combines output and error messages
#$ -o $HOME/logs                                                                                                                                                                               
#$ -e $HOME/logs   

library(data.table)


#JobId = as.numeric(Sys.getenv("SGE_TASK_ID"))
JobId = 1

# load sample
sample = read.csv(sprintf("samples/sample_%05d.csv",JobId))

# load data
load(file = 'data.rda')
data = ukbdata[1:5000,] #limit for now
rm(ukbdata)


# run simulation draft
source('/well/nichols/users/bwj567/mini-project-1/simulation/simulation_draft.R')


# resulting object is all_weights

