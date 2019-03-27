## data recode
library('data.table')
setwd('/well/nichols/projects/UKB/SMS')

# set location of full data file
full_data_file = 'ukb25120.csv'

# read in data header to get all names
data_head = names(fread(full_data_file, nrows = 0))

# read in list of variables
variables = fread('/well/nichols/users/bwj567/mini-project-1/variable_codings.csv')
variables = unique(variables[, .(cat, var, `biobank code`)])
setnames(variables, old = 'biobank code', new = 'biobank_code')

all_parent_codes = variables[, biobank_code]

# grab list of actual variable names in the data (at baseline)
vars_baseline = lapply(all_parent_codes, function(c){
    data_head[grepl(paste0('^',c,'-0'), data_head)]
    })
names(vars_baseline) = variables[, var]
vars_baseline = unlist(vars_baseline)
vars_baseline = data.table(var = names(vars_baseline), code = vars_baseline)


# grab list of actual variable names in the data (at imaging)
vars_imaging = lapply(all_parent_codes, function(c){
    data_head[grepl(paste0('^',c,'-2'), data_head)]
    })
names(vars_imaging) = variables[, var]
vars_imaging = unlist(vars_imaging)
vars_imaging = data.table(var = names(vars_imaging), code = vars_imaging)



# read in ALL the imaging data
data_imaging = fread(full_data_file
    , nrows = 5000     #for testing
    , select = c('eid', vars_imaging$code)
    , col.names = c('eid', vars_imaging$var)
    , na.strings = ''
    )
data_imaging

# check imaging variables
data_imaging[, .N, is.na(brain_vol)]
data_imaging[, .N, .(has_brain_vol = !is.na(brain_vol), MRI_completed == '1')]
data_imaging[, .N, .(has_brain_vol = !is.na(brain_vol), MRI_completed == '1')]

data_imaging[, .N, assessment_center]



# read in ALL the imaging data
data_baseline = fread(full_data_file
    , nrows = 5000     #for testing
    , select = c('eid', vars_baseline$code)
    , col.names = c('eid', vars_baseline$var)
    , na.strings = ''
    )
data_baseline

data_baseline[, .N, .(year(assessment_date))][order(year)]


###########
# RECODES #
###########

# GENDER
data_baseline[, gender_male := as.numeric(sex)]
data_baseline[, demo_sex := ifelse(sex == 0, 'M', 'F')]

# AGE
data_baseline[, age]

# ethnicity
data_baseline[, demo_ethnicity := substr(ethnicity, 1,1)]
data_baseline[, .N, demo_ethnicity]
data_baseline[, demo_white := 'Non-white']
data_baseline[demo_ethnicity == 1, demo_white := 'White'

# employment status
data_baseline[, .N, .(job_status1, job_status2, job_status3)][order(job_status1, job_status2, job_status3)]
data_baseline[, employed := as.numeric(job_status1 == 1)]
data_baseline[, retired := 0]
data_baseline[job_status1 == 2 | job_status2 == 2, retired := 1]
data_baseline[, homemaker := 0]
data_baseline[job_status1 == 3 | job_status2 == 3 | job_status3 == 3, homemaker := 1]
data_baseline[, disabled := 0]
data_baseline[job_status1 == 4 | job_status2 == 4 | job_status3 == 4 | job_status4 == 4, disabled := 1]
data_baseline[, unemployed:= 0]
data_baseline[job_status1 == 5 | job_status2 == 5 | job_status3 == 5 | job_status4 == 5 | job_status5 == 5, unemployed := 1]
data_baseline[, volunteer := 0]
data_baseline[job_status1 == 6 | job_status2 == 6 | job_status3 == 6 | job_status4 == 6 | job_status5 == 6 | job_status6 == 6, volunteer := 1]
data_baseline[, student := 0]
data_baseline[job_status1 == 7 | job_status2 == 7 | job_status3 == 7 | job_status4 == 7 | job_status5 == 7 | job_status6 == 7 | job_status7 == 7, student := 1]

# check overlap
data_baseline[, .N, .(employed, retired, homemaker, disabled, unemployed, volunteer, student)]

# OCCUPATION
table(unlist(lapply(data_baseline[, job_code], length))) # all the same lemgth
data_baseline[, job_topcat := substr(job_code,1,1)]
data_baseline[, .N, .(is.na(job_topcat), employed)]
data_baseline[, .N, job_topcat]

data_baseline[, demo_occupation := '10-unemployed']
data_baseline[job_topcat == 0, demo_occupation := '00-military']
data_baseline[job_topcat == 1, demo_occupation := '01-manager']
data_baseline[job_topcat == 2, demo_occupation := '02-professional']
data_baseline[job_topcat == 3, demo_occupation := '03-assoc professional']
data_baseline[job_topcat == 4, demo_occupation := '04-admin']
data_baseline[job_topcat == 5, demo_occupation := '05-skilled trades']
data_baseline[job_topcat == 6, demo_occupation := '06-personal service']
data_baseline[job_topcat == 7, demo_occupation := '07-sales customer service']
data_baseline[job_topcat == 8, demo_occupation := '08-industrial']
data_baseline[job_topcat == 9, demo_occupation := '09-elementary']

data_baseline[, .N/nrow(data_baseline), demo_occupation]
# education

# income

# location

# imaging data
data_baseline[, ]



## VAR-INSTANCE.MEASUREMENT
data[, .(.N,.N/nrow(data)), by = `6138-0.0`][order(`6138-0.0`)]
data[, .(.N,.N/nrow(data)), by = `6138-0.1`][order(`6138-0.1`)]
data[, .(.N,.N/nrow(data)), by = `6138-0.2`][order(`6138-0.2`)]
data[, .(.N,.N/nrow(data)), by = `6138-0.3`][order(`6138-0.3`)]

data[, .(.N,.N/nrow(data)), by = .(`6138-0.0`, `6138-0.1`, `6138-0.2`)][order(`6138-0.0`)]



data = fread('ukb25120.csv'
	, nrows = 10000 	#for testing
	, select = c("132-0.0", "132-0.1","132-0.2","132-0.3","132-0.4","132-0.5")
	, col.names = c('job1', 'job2', 'job3', 'job4', 'job5', 'job6')
	)

data[, job1 := `132-0.0`]
data[, substr(job1, 1,1)]
data[job1 != '' & job1 != 0, .(.N), by = .(jobtop = substr(job1, 1,1))][order(jobtop)][, .(jobtop, N, N/sum(N))]
