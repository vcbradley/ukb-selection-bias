##data_extract.sh

#################
# Script to generate basefiles from ukbiobank data
# Extracts relevant variables and subjects 
#################


cd /well/nichols/users/bwj567/

# SET PARAMS
INSTANCE='baseline'    #'baseline' or 'imaging'
FILE_IN='/well/nichols/projects/UKB/SMS/ukb40280/ukb40280.csv'
#FILE_IN='/well/nichols/users/bwj567/data/ukb25120_sample5k.csv' ##TESTING
FILE_OUT='/well/nichols/users/bwj567/data/ukb40280_raw_cogdecline'
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




##### USING UKBparse
# --variables/-v = which vars to select.  can select with a file vars.csv
# --subject/-s = the IDs of the subjects to select.  can select with ranges (i.e. 1:5), from a file (i.e. subj.txt) or by variable value (i.e. "v1 > 10 && v2 < 70")
# --visit/-vi = the visit to select. select by number (i.e. 1) or with "last"
# -nb = don't use UKB recodes
# -q = quiet
# -ow = force overwrite existing out file

# select only sec


funpack -nb --log_file /well/nichols/users/bwj567/log.txt -v 20023 $FILE_OUT $FILE_IN 
