#load in the data
apoe_gen = read.table("ukb_ApoE_chr19_v3.gen")
apoe_sample = read.table("ukb_ApoE_chr19_v3.sample")

#remove the first two rows - headers that we do not need
apoe_sample_ids = matrix(apoe_sample$V1[-c(1,2)], ncol=1)

#the first six entries per row look like that 

#19 rs429358 rs429358 45411941 T C 
#19 rs7412 rs7412 45412079 C T

# and they are
#1. Chromosome number
#2. rs ID of SNP
#3. rs ID of SNP
#4. Base-pair coordinate
#5. Allele 1 (usually minor)
#6. Allele 2 (usually major)
#after those 6 entries, the data is in triples for each subject
#and the triples will be the probability of Allele 1-1, Allele 1-2, Allele 2-2


#the combinations of alleles we are interested in are based on ApoE e3/e4 and e4/e4
#you can repeat the same steps if you want to calculate other combinations
#https://www.snpedia.com/index.php/APOE

#the combination which leads to ~11 times increased Alzheimer's risk is e4/e4


#we start from 7th entry for rs7412 since Allele 1 is C and we are interested in CC
apoe_rs7412_seq_CC = seq(7, ncol(apoe_gen), by = 3)
apoe_rs7412_CC = matrix(apoe_gen[2, apoe_rs7412_seq_CC], ncol=1)

#we start from 9th entry for rs429358 since Allele 2 is C and we are interested in CC
apoe_rs429358_seq_CC = seq(9, ncol(apoe_gen), by=3)
apoe_rs429358_CC = matrix(apoe_gen[1, apoe_rs429358_seq_CC], ncol=1)

#we start from 8th entry for rs429358 since we are interested in TC as well
apoe_rs429358_seq_TC = seq(8, ncol(apoe_gen), by=3)
apoe_rs429358_TC = matrix(apoe_gen[1, apoe_rs429358_seq_TC], ncol=1)

#create a data set with two indicators of e3/e4 and e4/e4
apoe_indicators = matrix(rep(0, 2*nrow(apoe_sample_ids)), ncol=2)

#e3/e4
apoe_indicators[which(apoe_rs429358_TC==1 & apoe_rs7412_CC==1), 1] = 1
#e4/e4
apoe_indicators[which(apoe_rs429358_CC==1 & apoe_rs7412_CC==1), 2] = 1

table(apoe_indicators)

apoe_indicators = as.data.frame(apoe_indicators)
names(apoe_indicators) = c("e3/e4", "e4/e4")

apoe_indicators$id_34077 = apoe_sample_ids
names(apoe_indicators)

write.table(apoe_indicators,
            file="/well/nichols/users/kindalov/ApoE_Extract/ApoE.dat",
            row.names=FALSE, sep="\t", quote=FALSE)