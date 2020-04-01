
## IN SIMULATION GIT DIRECTORY
##cd /well/nichols/users/bwj567/mini-project-1/simulation/results/sim_1_5000_new_v1

ls apoe_reg_coef_prop_* -l

##### COEFS
rm apoe_reg_coef_all.csv
head -1 apoe_reg_coef_prop_0.01.csv > apoe_reg_coef_all.csv
for filename in $(ls apoe_reg_coef_prop_*); do sed 1d $filename >> apoe_reg_coef_all.csv; done


head apoe_reg_coef_all.csv
tail apoe_reg_coef_all.csv

##### PVALS
ls apoe_reg_pval_prop_*

rm apoe_reg_pval_all.csv
head -1 apoe_reg_pval_prop_0.01.csv > apoe_reg_pval_all.csv
for filename in $(ls apoe_reg_pval_prop_*); do sed 1d $filename >> apoe_reg_pval_all.csv; done


head apoe_reg_pval_all.csv