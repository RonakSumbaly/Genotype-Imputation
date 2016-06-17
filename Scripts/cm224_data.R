############################################################################
# Project Title: Genotype Imputation
# Done By: Ronak Sumbaly
# Description: read project data
############################################################################

library(data.table)
library(doParallel)
library(doMC)
registerDoParallel(8)
library(stringr)
library(pdist)
library(MASS)

options(warn = -1)
setwd("~/Documents/Genotype Imputation/")

##### INITIALIZING DATA #####

work.set = 2  # 1 - easy, 2 - medium, 3 - hard 
train.loc = c("Final_7/easy/example/input/raw_genotypes.txt", "Final_7/medium/example/input/raw_genotypes.txt", "Final_7/hard/example/input/raw_genotypes.txt")
train.out.loc = c("Final_7/easy/example/output/imputed_genotypes.txt", "Final_7/medium/example/output/imputed_genotypes.txt", "Final_7/hard/example/output/imputed_genotypes.txt")
test.loc = c("Final_7/easy/test/input/raw_genotypes.txt", "Final_7/medium/test/input/raw_genotypes.txt", "Final_7/hard/test/input/raw_genotypes.txt")

##### READ DATA #####

train.data = data.frame(t(read.table(train.loc[work.set])))
train.out.data = data.frame(t(read.table(train.out.loc[work.set])))
test.data =  data.frame(t(read.table(test.loc[work.set])))

# imputation data details
num.individuals = dim(train.data)[1]
num.snps = dim(train.data)[2]
snp.consider = num.snps

train.data[train.data == -1] = NA  # replace -1 with NAs
test.data[test.data == -1] = NA  # replace -1 with NAs

# share same variable name over both data
imputation.train = data.frame(train.data)
imputation.train.out = data.frame(train.out.data)

cat("Number of Individuals = ", num.individuals, "\n")
cat("Number of SNPs = ", num.snps, "\n")
