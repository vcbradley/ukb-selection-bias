# ssh biobank
# R

library('data.table')

          


### Test R Script
#/well/nichols/projects/UKB

#non-imaging, non-genetics data
list.files('/well/nichols/projects/UKB/SMS')


setwd('/well/nichols/projects/UKB/SMS')


# ALL the data. 16k variables
data = fread('ukb25120.csv', nrows = 10)
data[, 1:20]



# a list of all the fields in the data
fread('ukb25120_fields.txt')
# f.eid
# f.3.0.0
# f.3.1.0
# f.3.2.0
# f.4.0.0
# f.4.1.0



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


readLines('/well/nichols/projects/UKB/SMS/ukbparse_config/variables.tsv', n = 10)
 # [1] "ID\tType\tDescription\tDataCoding\tNAValues\tRawLevels\tNewLevels\tClean\tParentValues\tChildValues"
 # [2] "0\tSequence\tEncoded anonymised participant ID\t\t\t\t\t\t\t"                                       
 # [3] "3\tInteger\tVerbal interview duration\t\t\t\t\t\t\t"                                                
 # [4] "4\tInteger\tBiometrics duration\t\t\t\t\t\t\t"                                                      
 # [5] "5\tInteger\tSample collection duration\t\t\t\t\t\t\t"  


fields = fread('ukbparse_config/variables.tsv')
fields

fields[grep('age ', Description),]
fields[ID == 31,] 	#sex
fields[ID == 34,]	#YOB

readLines('/well/nichols/projects/UKB/SMS/ukbparse_config/datacodings.tsv')



readLines('/well/nichols/projects/UKB/SMS/ukbparse_config/categories.tsv', n = 5)
categories = fread('ukbparse_config/categories.tsv')
head(categories)

#Field Desc
#21022	Age at recruitment
#33	Date of birth
#52	Month of birth
#34	Year of birth
#31	Sex
#189	Townsend deprivation index at recruitment


