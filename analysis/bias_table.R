
library(data.table)
library(stringr)
library(knitr)
library(kableExtra)

setwd('~/github/mini-project-1/analysis')

xtabs = fread('summary_full.csv')


bias_table = xtabs[!is.na(count_hse16) & level != '40 to 44' & !(var %in% c('health_alc_freq', 'health_bp_cat', 'health_bp_meds_current', 'demo_white'))
                   , .(var, level
                       , count_hse16 = round(count_hse16, 0)
                       , count_ukb_img_atimg
                       , dist_hse16 = round(dist_hse16 * 100, 1)
                       , dist_ukb_img_atimg = round(dist_ukb_img_atimg * 100, 1)
                       , diff = round((dist_ukb_img_atimg - dist_hse16) * 100, 1)
                       )]

bias_table[, .N, var]

cat(paste(paste0("data.frame(var = '",bias_table[, .(var = unique(var))][order(var)]$var, "', label = 'LABEL')")), sep = '\n,')

var_reformat = list(
  data.frame(var = 'demo_sex', label = 'Sex')
  ,data.frame(var = 'demo_age_bucket', label = 'Age Bucket')
  ,data.frame(var = 'demo_educ_highest', label = 'Highest Education')
  ,data.frame(var = 'demo_empl_disabled', label = 'Disabled')
  ,data.frame(var = 'demo_empl_employed', label = 'Employed')
  ,data.frame(var = 'demo_empl_homemaker', label = 'Homemaker')
  ,data.frame(var = 'demo_empl_retired', label = 'Retired')
  ,data.frame(var = 'demo_empl_student', label = 'Student')
  ,data.frame(var = 'demo_empl_unemployed', label = 'Unemployed')
  ,data.frame(var = 'demo_empl_volunteer', label = 'Volunteer')
  ,data.frame(var = 'demo_ethnicity_4way', label = 'Ethnicity')
  ,data.frame(var = 'demo_hh_ownrent', label = 'Own/Rent House')
  ,data.frame(var = 'demo_income_bucket', label = 'Income')
  ,data.frame(var = 'demo_occupation', label = 'Occupation')
  
  ,data.frame(var = 'health_smoking_status', label = 'Smoking status')
  ,data.frame(var = 'health_BMI_bucket', label = 'BMI Bucket')
  ,data.frame(var = 'health_bp_high_ever', label = 'Ever diagnosed high BP')
  ,data.frame(var = 'health_diabetes', label = 'Ever diagnosed diabetes')
  
  ,data.frame(var = 'health_alc_freq', label = 'Alcohol consumption')
  ,data.frame(var = 'health_bp_cat', label = 'Blood Pressure Category')
  ,data.frame(var = 'health_bp_meds_current', label = 'Currently taking BP meds')
  
)


lapply(1:length(var_reformat), function(v_ind){
  v = var_reformat[[v_ind]]
  
  var_name = v$var
  
  label = v$label
  label = paste0(str_pad(v_ind, pad = '0', side = 'left', width = 2),'-',label)
  
  bias_table[var == var_name, var := label]
  
  return(var_name)
})

bias_table

# recode yes/no vars
bias_table[level == 1, level := '01-Yes']
bias_table[level == 0, level := '02-No']


# replace NAs with strings
bias_table[is.na(bias_table)] <- '-'


setnames(bias_table, c("Variable", "Level", "HSE", "UKB", "% of HSE", "% of UKB", "% UKB-% HSE"))

bias_table_kable = kable(bias_table[order(Variable, Level)][, -c('Variable'), with = F], format = 'latex'
      , longtable = T
      , caption = "Selection bias in the UK Biobank imaging cohort relative to the 2016 HSE nurse interview subsample.\\label{tab:selection-bias}"
      , booktabs = T
      )


tablevars = sort(unique(bias_table$Variable))


for(i in 1:length(tablevars)){
  varname = tablevars[i]
  
  rows = which(bias_table[order(Variable, Level)]$Variable == tablevars[i])
  
  bias_table_kable = bias_table_kable %>% 
    group_rows(varname, min(rows), max(rows))
}

bias_table_kable %>% add_header_above(c(" " = 1, "Count" = 2, "% of sample" = 2, " "=1)) %>% kable_styling(font_size = 10)

