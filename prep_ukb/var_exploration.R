# Variable inventory/exploration
library('data.table')
setwd('/well/nichols/projects/UKB/SMS')

# ALL the data. 16k variables
data = fread('ukb25120.csv', nrows = 10)
data[, 1:5]

names(data)[grepl('6138-0', names(data))]

variables = fread('ukbparse_config/variables.tsv')
variables

categories = fread('ukbparse_config/categories.tsv')
categories

# age and sex
variables[grepl('age', Description) & !grepl('Average', Description), .(Description, ID)]
variables[ID == 31,] 	#sex
variables[ID == 34,]	#YOB
variables[ID == 55,] 	#MOB
variables[ID == 6138,] 

#Average total household income before tax     100294


# location
variables[grepl(2270, ID)]
# > variables[grepl(2270, ID)]
#        ID                 Type                                 Description
# 1:  22700                 Date             Date first recorded at location
# 2:  22702              Integer  Home location - east co-ordinate (rounded)
# 3:  22704              Integer Home location - north co-ordinate (rounded)


variables[ID < 200,]

variables[ID >= 52 & ID < 64]

#Townsend Index
variables[ID == 189, ]

#other deprivation metrics
variables[grepl('crime|Crime', Description),]

variables[grepl('^264', ID)]


# REASON LOST TO FOLLOW UP
variables[ID == 190]


variables[ID == 738]
data[, `738-0.1`]

names(data)[grepl('738', names(data))]

data[, `4674-0.0`]
qlogin


## Ethnicity
data_eth = fread('ukb25120.csv', select = c('21000-0.0'), nrow = 2000)
data_eth[, .N, .(ethnicity = `21000-0.0`)][order(ethnicity)]
names(data)[grepl('21000', names(data))]





