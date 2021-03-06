############################################################################
# Project Title: Genotype Imputation
# Done By: Ronak Sumbaly
# Description: supplementary functions
############################################################################

# accuracy function
accuracy = function(actual, predicted){
  accuracy.measure = length(which(actual == predicted))/length(actual)
  return(accuracy.measure)
}

# print details of data
print.details = function() {
  cat("Number of Individuals = ", num.individuals, "\n")
  cat("Number of SNPs Considered = ", snp.consider, "\n")
  cat("Number of Genotype Data = ", num.individuals * snp.consider, "\n")
  cat("Number of SNPs Masked = ", length(which(is.na(imputation.train) == TRUE)), "\n")
}

# calculate mode 
cal.mode = function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# get details about the data - missing value locations
impute.details = function(x, byrow = F) {
  missing.matrix = is.na(x)
  missing.count = sum(missing.matrix) 
  
  if(missing.count == 0) {
    return ( list (missing.matrix = missing.matrix,
                   missing.count = missing.count,
                   missing.rows.indices = NULL,
                   missing.cols.indices = NULL,
                   x.missing = NULL) )
  }
  
  missing.rows.indices = which(apply(missing.matrix, 1, function(i) {
    any(i)
  }))
  
  missing.cols.indices = which(apply(missing.matrix, 2, function(i) {
    any(i)
  }))
  
  if (byrow) x.missing = cbind(1:nrow(x),x)[missing.rows.indices,]
  else x.missing = rbind(1:ncol(x),x)[,missing.cols.indices]
  
  return ( list (missing.matrix = missing.matrix,
                 missing.count = missing.count,
                 missing.rows.indices = missing.rows.indices,
                 missing.cols.indices = missing.cols.indices,
                 x.missing = x.missing) )
}