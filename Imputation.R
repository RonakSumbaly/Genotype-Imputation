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

test.size = 300  # change to increase test data size
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
## Simple Random Imputation ## 

simple.random.imp = function (data){
  missing = is.na(data)
  n.missing = sum(missing)
  data.obs = data[!missing]
  imputed = data
  imputed[missing] = sample (data.obs, n.missing, replace=TRUE)
  return (imputed)
}

i.imputed.data = simple.random.imp(i.missing.data)
s.imputed.data = simple.random.imp(s.missing.data)

# accuracy measure
i.actual = test.data[which(is.na(i.missing.data) == TRUE)]
s.actual = test.data[which(is.na(s.missing.data) == TRUE)]
i.predicted = i.imputed.data[which(is.na(i.missing.data) == TRUE)]
s.predicted = s.imputed.data[which(is.na(s.missing.data) == TRUE)]
i.accuracy = length(which(i.actual == i.predicted))/length(i.actual)
s.accuracy = length(which(s.actual == s.predicted))/length(s.actual)

cat("Baseline Illumina Method Accuracy = ", i.accuracy * 100, "\n")
cat("Baseline Sequencing Method Accuracy = ", s.accuracy * 100, "\n")

##### IMPROVED METHOD #####