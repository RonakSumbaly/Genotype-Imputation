############################################################################
# Project Title: Genotype Imputation
# Done By: Ronak Sumbaly
# Description: read project data
############################################################################

library(data.table)
library(stringr)
library(pdist)

options(warn = -1)
setwd("~/Documents/Imputation Project/")

##### INITIALIZING DATA #####

# read CM224 provided data 
main.imputation.train = data.frame(t(fread("Data/imputation_training.txt", header = TRUE)))
main.imputation.test = data.frame(t(fread("Data/imputation_test.txt", header = TRUE))) ## submit this file's output
imputation.train = main.imputation.train

# individual count
individuals.count = dim(imputation.train)[1] 

# snps count
snps.count = dim(imputation.train)[2] 


