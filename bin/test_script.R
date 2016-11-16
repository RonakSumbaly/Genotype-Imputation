############################################################################
# Project Title: Genotype Imputation
# Done By: Ronak Sumbaly
# Description: run main test data
############################################################################

library(data.table)
library(stringr)
library(pdist)

options(warn = -1)
setwd("~/Documents/Imputation Project/")

##### INITIALIZING DATA #####

# read CM224 provided data 
main.imputation.test = data.frame(t(fread("Data/imputation_test.txt", header = TRUE))) ## submit this file's output

main.imputed.sr.test = simple.random.imp(main.imputation.test)
main.imputed.knn.test = knn.imp(main.imputation.test)
main.imputed.svd.test = svd.imp(main.imputation.test)

final.test.output = main.imputation.test

for (i in 1:dim(main.imputation.test)[1]){
  svd.i = unlist(main.imputed.sr.test[i,])
  knn.i = unlist(main.imputed.knn.test[i,])
  sr.i = unlist(main.imputed.svd.test[i,])
  ensemble_output = rbind(svd.i, knn.i, sr.i)
  final.test.output[i,] = apply(ensemble_output, 2, cal.mode)
}

write.table(t(final.test.output),file = "imputation_test_604591897.txt", row.names = FALSE, sep = " ", quote = FALSE)
