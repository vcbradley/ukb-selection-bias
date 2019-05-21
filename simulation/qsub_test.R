#!/usr/bin/env Rscript
#$ -S /usr/local/packages/R-3.5.1/bin/Rscript
#$ -t 1:10
#$ -cwd   # this is so that you stay in the directory that you launched from
#$ -o $HOME/logs   #redirecting std out and std error
#$ -e $HOME/logs

JobId=as.numeric(Sys.getenv("SGE_TASK_ID"))

mkdir('test')
write(JobId,file=sprintf("test/Run%03d.txt",JobId))

