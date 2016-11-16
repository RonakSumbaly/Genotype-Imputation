############################################################################
# Project Title: Genotype Imputation
# Done By: Ronak Sumbaly
# Description: simulate testing data by adding random missing values (1000 genome project)
############################################################################

##### REDUCE DATA #####

snp.consider = round(num.snps) # number to SNPs to consider
snp.consider = 100000
columns = sample(1:ncol(genome.1000), snp.consider, replace = TRUE)
imputation.train = genome.1000[,columns]
imputation.train.out = imputation.train

mask.snps = 0.40 * (num.individuals * snp.consider) # number of SNPs to mask

snp.rows = sample(1:nrow(imputation.train), mask.snps, replace = TRUE)
snp.columns = sample(1:ncol(imputation.train), mask.snps, replace = TRUE)

imputation.train[unique(cbind(snp.rows,snp.columns))] = NA
rm(columns)
rm(snp.rows)
rm(snp.columns)
