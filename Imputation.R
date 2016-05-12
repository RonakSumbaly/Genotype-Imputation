############################################################################
# Project Title: Genotype Imputation
#
# Done By: Ronak Sumbaly
#
# Description: statistical inference of unobserved genotypes using *FILL ALGORITHMS* 
############################################################################

library(data.table)
library(stringr)

setwd("~/Documents/Imputation Project/")
set.seed(891992)

##### INITIALIZING DATA #####

# read in 1000 genome data
haploid = t(fread("1000genomes/chr-22.geno.reduced.csv", header = FALSE))
individuals = fread("1000genomes/chr-22.ind", header = FALSE)
snps = fread("1000genomes/chr-22.snp", header = FALSE)

individuals.count = dim(individuals)[1] # haploid chromosomes
snps.count = dim(haploid)[2] # number of snps 

# construct diploid data
diploid = haploid[seq(1,individuals.count,2),] + haploid[seq(2,individuals.count,2),] # add consecutive haploid rows 

# add row names and col names to diploid data
rownames(diploid) = individuals$V1[seq(1,individuals.count,2)]
colnames(diploid) = paste("snp", str_split_fixed(snps$V1, ":", 2)[c(1:snps.count),2],sep = "")

# update individual count
individuals.count = dim(diploid)[1] # diploid chromosomes

##### SIMULATE TESTING DATA #####

test.size = 100  # change to increase test data size
no.mask.col = as.integer(runif(1, 1, snps.count)) # number of columns to mask
no.mask.rows = as.integer(runif(1, 1, test.size)) # number of rows to mask

test.data = diploid[sample(nrow(diploid), test.size),]  # get random size diploid test data 
random.col = unique(sample(1:snps.count, no.mask.col))  # mask random columns in test data 

# Illumina method 
# missing data along entire column
# add missing values to the panel
i.missing.data = test.data
i.missing.data[,random.col] = NA 

# Sequencing method
# random values in the matrix masked 
mask.rows = sample(1:test.size, no.mask.rows, replace = TRUE)
mask.columns = sample(1:snps.count, no.mask.col, replace = TRUE)
s.missing.data = test.data
s.missing.data[mask.rows, mask.columns] = NA


##### BASELINE METHOD #####


# accuracy measure
i.actual = i.missing.data[which(i.missing.data == -1)]
s.actual = s.missing.data[which(s.missing.data == -1)]
i.predicted = i.imputed.data[which(i.missing.data == -1)]
s.predicted = s.imputed.data[which(s.missing.data == -1)]
i.accuracy = length(which(i.actual == i.predicted))/length(i.actual)
s.accuracy = length(which(s.actual == s.predicted))/length(s.actual)

print("Baseline Illumina Method Accuracy = ", i.accuracy)
print("Baseline Sequencing Method Accuracy = ", s.accuracy)

##### IMPROVED METHOD #####