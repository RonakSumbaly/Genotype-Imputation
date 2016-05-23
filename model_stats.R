############################################################################
# Project Title: Genotype Imputation
# Done By: Ronak Sumbaly
# Description: calculate model statistics
############################################################################

# accuracy function
accuracy = function(actual, predicted){
  accuracy.measure = length(which(actual == predicted))/length(actual)
  return(accuracy.measure)
}

print.details = function() {
  cat("Number of Individuals = ", test.size, "\n")
  cat("Number of SNPs Considered = ", snp.consider, "\n")
  cat("Number of Illumina SNPs Masked = ", length(which(is.na(i.missing.data) == TRUE)), "\n")
  cat("Number of Sequencing SNPs Masked = ", length(which(is.na(s.missing.data) == TRUE)), "\n")
}