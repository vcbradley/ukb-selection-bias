# ssh biobank
# R

library('data.table')
setwd('/well/nichols/projects/UKB/SMS')
          


### Test R Script
#/well/nichols/projects/UKB

#non-imaging, non-genetics data
list.files('/well/nichols/projects/UKB/SMS')


# translations between subsets of the data?
fread('bridge_34077_8107.csv', nrows = 10)
fread('bridge_8107_34077.csv', nrows = 10)


# ALL the data. 16k variables
data = fread('ukb25120.csv', nrows = 10)
data[, 1:5]
#         eid 3-0.0 3-1.0 3-2.0 4-0.0
#  1: 4994265   530               364
#  2: 3963128   283               409
#  3: 2273239   378         470   512
#  4: 2265400   347   395         445
#  5: 1902845   700               488
#  6: 4478296   564               371
#  7: 2817518   671               861
#  8: 3244647   710   656         963
#  9: 3978907   412               647
# 10: 3553200   360               299


# a list of all the fields in the data - these correspond to the columns in data
fread('ukb25120_fields.txt')
# f.eid
# f.3.0.0
# f.3.1.0
# f.3.2.0
# f.4.0.0
# f.4.1.0


# specifying subsets of the variables that correspond to CVR data
fread('vars_CVR.txt')
 #      V1
 # 1: 21003
 # 2: 20403
 # 3: 20161
 # 4:  3436
 # 5:  2867
 # 6:  6194
 # 7:  2897
 # 8:  6183

 # specifying subsets of the variables that correspond to alc data
fread('vars_alc.txt')
#       V1
# 1:  1558
# 2:    31
# 3: 21003
# 4:   189
# 5:   845
# 6:  6138



# config file for ukbparse - the Python module?
readLines('ukbparse_config.txt')
# [1] "# Template ukbparse configuration file"                                        
# [2] "variable_file /well/nichols/projects/UKB/SMS/ukbparse_config/variables.tsv"    
# [3] "datacoding_file /well/nichols/projects/UKB/SMS/ukbparse_config/datacodings.tsv"
# [4] "category_file /well/nichols/projects/UKB/SMS/ukbparse_config/categories.tsv"   
# [5] "type_file /well/nichols/projects/UKB/SMS/ukbparse_config/types.tsv"            
# [6] "processing_file /well/nichols/projects/UKB/SMS/ukbparse_config/processing.tsv" 
# [7] "icd10_file /well/nichols/projects/UKB/SMS/ukbparse_config/icd10.tsv"           
# [8] "#"                                                                             
# [9] "# Using this requires that 25734 is included"                                  
#[10] "#subject \"v25734 != na\""  


variables = fread('ukbparse_config/variables.tsv')
variables

variables[grep('age ', Description),]
variables[ID == 31,] 	#sex
variables[ID == 34,]	#YOB


# location
variables[grepl(2270, ID)]
# > variables[grepl(2270, ID)]
#        ID                 Type                                 Description
# 1:  22700                 Date             Date first recorded at location
# 2:  22702              Integer  Home location - east co-ordinate (rounded)
# 3:  22704              Integer Home location - north co-ordinate (rounded)

# data codings
datacodings = fread('/well/nichols/projects/UKB/SMS/ukbparse_config/datacodings.tsv')
datacodings[ID == 31,]
datacodings[ID == 34,]

datacodings[ID == 132,]


readLines('/well/nichols/projects/UKB/SMS/ukbparse_config/categories.tsv')
categories = fread('ukbparse_config/categories.tsv')
categories


# Python code to process data -- only 7 rows
processing = fread('/well/nichols/projects/UKB/SMS/ukbparse_config/processing.tsv')
processing


variables[grepl('Qualifications', Description)]




#######################
# READ IN USEFUL DATA #
#######################


# format = {vid}{value}-{visit}
# names based on
names(data)
names(data)[grepl('132', names(data))]


# 132 = job code

list(data.frame(code = '132-0.0', var = 'job_code')
	, data.frame(code = '31-0.0', var = 'sex')
	, data.frame(code = ''))

data[, `31-0.0`] #sex male = 1
data[, `34-0.0`] #YOB

# read in only some columns
# HAVE DATA ON ALL 500k!!
data_agesex = fread('ukb25120.csv',
 select = c('eid','31-0.0', '34-0.0', '139')
 , col.names = c('eid', 'sex', 'yob', 'townsend')
 , nrow = 1000)



data_agesex[, .N, yob][order(yob)]
data_agesex[, .N, sex][order(sex)]



