#!/apps/well/R/3.4.3/bin/Rscript       
#$ -N sim_run
#$ -t 1:10
#$ -tc 50                                                                                                                         
#$ -cwd
#$ -o ../../logs                                                                                                                                                                               
#$ -e ../../logs   
### request maximum of 72 hours of compute time
#$ -l s_rt=24:00:00
#$ -l h_rt=24:00:00
#$ -l h_vmem=6G



library(data.table)
library(stringr)
library(knitr)


JobId = as.numeric(Sys.getenv("SGE_TASK_ID"))
#JobId = 3

dir = getwd()

prop = str_split(dir, '/')[[1]]
prop = prop[grepl('prop', prop)]
prop = gsub('prop_', '', prop)

sim_dir = str_split(dir, '/samples/')[[1]][1]

# load sample
sample = read.csv(sprintf("sample_%05d.csv",JobId))[,1]

# load data
load(file = paste0(sim_dir,'/data.rda'))
data = ukbdata[1:length(sample),]#limit for now
rm(ukbdata)

sessionInfo()