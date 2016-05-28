############################################################################
# Project Title: Genotype Imputation
# Done By: Ronak Sumbaly
# Description: simulate testing data by adding random missing values
############################################################################

##### REDUCE DATA #####

snp.consider = round(snps.count) # number to SNPs to consider
reference.data = subset(imputation.train, select=colnames(imputation.train)[1:snp.consider])  # consider 1:n SNPs only for time being
ref.snps.count = snp.consider

##### SIMULATE TESTING DATA #####

test.size = round(individuals.count / 2)  # change to increase test data size
no.mask.col = as.integer(runif(1, 1, ref.snps.count)) # number of columns to mask
no.mask.rows = as.integer(runif(1, 1, test.size)) # number of rows to mask

test.data = reference.data[sample(nrow(reference.data), test.size),]  # get random size diploid test data 
random.col = unique(sample(1:ref.snps.count, no.mask.col))  # mask random columns in test data 

# Illumina method 
# missing data along entire column
# add missing values to the panel
i.missing.data = test.data
i.missing.data[,random.col] = NA 

# Sequencing method
# random values in the matrix masked 
mask.rows = unique(sample(1:test.size, no.mask.rows, replace = TRUE))
mask.columns = unique(sample(1:ref.snps.count, no.mask.col, replace = TRUE))

s.missing.data = test.data
s.missing.data[mask.rows, mask.columns] = NA

