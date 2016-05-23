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

i.imputed.sr.data = simple.random.imp(i.missing.data)
s.imputed.sr.data = simple.random.imp(s.missing.data)

print.details()
cat("Baseline Simple Random Imputation Illumina Method Accuracy = ", accuracy(test.data[which(is.na(i.missing.data) == TRUE)], i.imputed.sr.data[which(is.na(i.missing.data) == TRUE)]) * 100, "\n")
cat("Baseline Simple Random Imputation Sequencing Method Accuracy = ", accuracy(test.data[which(is.na(s.missing.data) == TRUE)], s.imputed.sr.data[which(is.na(s.missing.data) == TRUE)])  * 100, "\n")

## kNN Imputation ## 

cal.mode = function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

knn.imp = function (data, method, k = 5){
  if (method == "i") {
    complete.data = data[, -random.col]
    complete.reference.data = reference.data[, -random.col]
    k.distance = matrix(pdist(complete.data, complete.reference.data)@dist, nrow = test.size)
    knn.data = t(apply(k.distance, 1, order)[1:k,])
    for (i in 1:test.size) {
      closet.reference = apply(reference.data[knn.data[i,],], 2, cal.mode)
      data[i, random.col] = closet.reference[random.col]
    }
  }
  else {
    for (j in 1:test.size) {
      j.data = data[j, ]
      complete.data = j.data[which(is.na(j.data) == FALSE)]
      if (length(complete.data) == snp.consider) next
      complete.reference.data = reference.data[, which(is.na(j.data) == FALSE)]
      k.distance = matrix(pdist(complete.data, complete.reference.data)@dist, nrow = 1)
      knn.data = t(apply(k.distance, 1, order)[1:k,])
      closet.reference = apply(reference.data[knn.data,], 2, cal.mode)
      data[j, which(is.na(j.data) == TRUE)] = closet.reference[which(is.na(j.data) == TRUE)]
    }
  }
  return(data)
}

i.imputed.knn.data = knn.imp(i.missing.data, "i")
s.imputed.knn.data = knn.imp(s.missing.data, "s")

print.details()
cat("Baseline k-Nearest Neighbor Illumina Method Accuracy = ", accuracy(test.data[which(is.na(i.missing.data) == TRUE)], i.imputed.knn.data[which(is.na(i.missing.data) == TRUE)]) * 100, "\n")
cat("Baseline Simple Random Imputation Sequencing Method Accuracy = ", accuracy(test.data[which(is.na(s.missing.data) == TRUE)], s.imputed.knn.data[which(is.na(s.missing.data) == TRUE)])  * 100, "\n")

