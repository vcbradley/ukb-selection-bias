#!/apps/well/R/3.4.3/bin/Rscript                                                                                                                                                               
#$ -t 1:10                                                                                                                                                                                     
#$ -cwd                                                                                                                                                                                        
#$ -o $HOME/logs                                                                                                                                                                               
#$ -e $HOME/logs   


JobId=as.numeric(Sys.getenv("SGE_TASK_ID"))

print(sprintf("Run%03d.txt",JobId))

write(JobId,file=sprintf("Run%03d.txt",JobId))

