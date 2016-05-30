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

s.imputed.sr.data = simple.random.imp(s.missing.data)

print.details()
cat("Baseline Simple Random Imputation Accuracy = ", accuracy(test.data[is.na(s.missing.data) == TRUE], s.imputed.sr.data[is.na(s.missing.data) == TRUE])  * 100, "\n")

## kNN Imputation ## 

cal.mode = function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

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

s.imputed.knn.data = knn.imp(s.missing.data)
s.imputed.knn.data = simple.random.imp(s.imputed.knn.data) # incase of missing values

print.details()
cat("Baseline kNN Imputation Accuracy = ", accuracy(test.data[is.na(s.missing.data)], s.imputed.knn.data[is.na(s.missing.data)])  * 100, "\n")

