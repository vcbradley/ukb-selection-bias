
cd /well/nichols/users/bwj567/mini-project-1/simulation/results/sim_1_5000_old_v3

ls apoe_reg_coef_prop_*

##### COEFS
head -1 apoe_reg_coef_prop_0.01.rda > apoe_reg_coef_all.csv
for filename in $(ls apoe_reg_coef_prop_*); do sed 1d $filename >> apoe_reg_coef_all.csv; done


tail apoe_reg_coef_all.csv

##### PVALS
ls apoe_reg_pval_prop_*

head -1 apoe_reg_pval_prop_0.01.rda > apoe_reg_pval_all.csv
for filename in $(ls apoe_reg_pval_prop_*); do sed 1d $filename >> apoe_reg_pval_all.csv; done


tail apoe_reg_pval_all.csv