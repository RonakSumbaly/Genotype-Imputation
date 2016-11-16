############################################################################
# Project Title: Genotype Imputation
# Done By: Ronak Sumbaly
# Description: baseline method (random imputation and kNN imputation)
############################################################################

## Simple Random Imputation ## 

simple.random.imp = function (data){
  missing = is.na(data)
  n.missing = sum(missing)
  data.obs = data[!missing]
  imputed = data
  imputed[missing] = sample (data.obs, n.missing, replace=TRUE)
  return (imputed)
}

mode.imp = function(data){
  column.mode = foreach(i = 1:ncol(data)) %do% cal.mode(data[,i][!is.na(data[,i])])
  column.mode = unlist(column.mode)
  for (i in 1:ncol(data)){
    data[is.na(data[,i]),i] = column.mode[i]
  }
  return(data)
}

random.imputed.data = simple.random.imp(imputation.train)
mode.imputed.data = mode.imp(imputation.train)
print.details()
cat("Baseline Simple Random Imputation Accuracy = ", accuracy(imputation.train.out[is.na(imputation.train)], random.imputed.data[is.na(imputation.train)])  * 100, "%\n")
cat("Baseline Mode Imputation Accuracy = ", accuracy(imputation.train.out[is.na(imputation.train)], mode.imputed.data[is.na(imputation.train)])  * 100, "%\n")

## kNN Imputation ## 

knn.imp = function (data, k = 15){
  for (j in 1:dim(data)[1]) {
    j.data = data[j, ]
    complete.data = j.data[!is.na(j.data)]
    if (length(complete.data) == dim(data)[2]) next
    reference.data = data[,!is.na(j.data)]
    
    if (length(which(is.na(reference.data))) > 0) warning("reference data has NA values") 
    
    k.distance = matrix(pdist(complete.data, reference.data)@dist, nrow = 1)
    knn.data = t(apply(k.distance, 1, order)[1:k,])
    knn.data = knn.data[!(knn.data == j)]
    check.data = data[knn.data,is.na(j.data)]
    
    ref.chk.data = c()
    for (x in 1:dim(check.data)[1]) {
      if (length(which(is.na(check.data[x,]))) != 0) ref.chk.data = c(ref.chk.data, FALSE)
      else ref.chk.data = c(ref.chk.data, TRUE)
    }
    
    check.data = check.data[ref.chk.data, ]
    closet.reference = apply(check.data, 2, cal.mode)
    data[j, is.na(j.data)] = closet.reference
  }
  return(data)
}

knn.imputed.data = knn.imp(imputation.train)
cat("Baseline kNN Imputation Accuracy = ", accuracy(imputation.train.out[is.na(imputation.train)], knn.imputed.data[is.na(imputation.train)])  * 100, "%\n")