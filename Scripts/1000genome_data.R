############################################################################
# Project Title: Genotype Imputation
# Done By: Ronak Sumbaly
# Description: read 1000 genome data 
############################################################################

library(data.table)
library(stringr)
library(pdist)
library(doParallel)

setwd("~/Documents/Genotype Imputation/")

##### INITIALIZING DATA #####

# read in 1000 genome data
haploid = t(fread("1000genomes/chr-22.geno.reduced.csv", header = FALSE, data.table = TRUE))
individuals = fread("1000genomes/chr-22.ind", header = FALSE,  data.table = TRUE)
snps = fread("1000genomes/chr-22.snp", header = FALSE, data.table = TRUE)

num.individuals = dim(individuals)[1] # haploid chromosomes
num.snps = dim(haploid)[2] # number of snps 

# construct diploid data
diploid = haploid[seq(1,num.individuals,2),] + haploid[seq(2,num.individuals,2),] # add consecutive haploid rows 

# add row names and col names to diploid data
rownames(diploid) = individuals$V1[seq(1,num.individuals,2)]
colnames(diploid) = paste("snp", str_split_fixed(snps$V1, ":", 2)[c(1:num.snps),2],sep = "")

# update individual count
num.individuals = dim(diploid)[1] # diploid chromosomes

# share same variable name over both data
genome.1000 = data.frame(diploid)

rm(diploid)
rm(haploid)
rm(individuals)
rm(snps)