##data_extract.sh

#################
# Script to generate basefiles from ukbiobank data
# Extracts relevant variables and subjects 
#################


cd /well/nichols/users/bwj567/

# SET PARAMS
INSTANCE='imaging'    #'baseline' or 'imaging'
FILE_IN='/well/nichols/projects/UKB/SMS/ukb25120/ukb25120.csv'
#FILE_IN='/well/nichols/users/bwj567/data/ukb25120_sample5k.csv' ##TESTING
FILE_OUT='/well/nichols/users/bwj567/data/ukb25120_raw'
if [ "$INSTANCE" != "baseline" ]
then 
	FILE_OUT=$FILE_OUT'_imaging.tsv'
	VISIT=2
else
	FILE_OUT=$FILE_OUT'_baseline.tsv'
	VISIT=0
fi
echo $FILE_OUT
echo $FILE_IN





# CREATE VARIABLE LIST

# grab list of variables and cat to new file
head mini-project-1/variable_codings.csv
cat mini-project-1/variable_codings.csv | cut -d ',' -f3 > data/vars.csv

# remove header
sed -i '1d' data/vars.csv
sed -i '1d' data/vars.csv

# remove blank rows
sed -i '/^$/d' data/vars.csv

cat data/vars.csv


##### USING UKBparse
# --variables/-v = which vars to select.  can select with a file vars.csv
# --subject/-s = the IDs of the subjects to select.  can select with ranges (i.e. 1:5), from a file (i.e. subj.txt) or by variable value (i.e. "v1 > 10 && v2 < 70")
# --visit/-vi = the visit to select. select by number (i.e. 1) or with "last"
# -nb = don't use UKB recodes
# -q = quiet
# -ow = force overwrite existing out file

# select only sec


ukbparse -nb --log_file /well/nichols/users/bwj567/log.txt -v data/vars.csv $FILE_OUT $FILE_IN 

