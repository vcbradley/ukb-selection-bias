#!/apps/well/R/3.4.3/bin/Rscript                                                                                                                                                               
#$ -t 1:5000                                                                                                                                                                                   
#$ -cwd
### -j option combines output and error messages
#$ -o $HOME/logs                                                                                                                                                                               
#$ -e $HOME/logs   


JobId = as.numeric(Sys.getenv("SGE_TASK_ID"))

sample = fread(sprintf("sample_%03d.csv",JobId))

print(sample)