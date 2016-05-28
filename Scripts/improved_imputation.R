############################################################################
# Project Title: Genotype Imputation
# Done By: Ronak Sumbaly
# Description: improved method (SVD & PCA & SVT imputation)
############################################################################

## SVD Imputation ##

rank.approx = function(x, k) {
  x.svd = svd(x, nu=k, nv=k)
  x.svd$u %*% diag(x.svd$d[1:k],nrow=k,ncol=k) %*% t(x.svd$v)
}

svd.imp = function(x, k = 5, num.iters = 10) {
  data.details = impute.details(x)
  
  if (data.details$missing.count == 0) return (x)
  
  missing.matrix = data.details$missing.matrix
  data.matrix = data.details$x.missing
  col.indices = data.details$missing.cols.indices
  colnames(data.matrix) = 1:dim(data.matrix)[2] 
  rownames(data.matrix) = 1:dim(data.matrix)[1] 
  
  # initialize missing values with mode
  data.matrix.imp = apply(data.matrix, 2, function(j){
    col.index = as.numeric(j[1])
    col.original = as.numeric(j[-1])
    missing.rows = which(missing.matrix[,col.index])
        
    if(length(missing.rows) == nrow(x))
      warning(paste("Column",col.index,"is completely missing",sep=" ") )
    
    col.original[missing.rows] = cal.mode(col.original[-missing.rows])
    
    round(col.original)
  })
  
  x[,col.indices] = data.matrix.imp
  x[is.na(x)] = 0

  for (i in 1:num.iters) {
    print(paste("Running SVD iteration", i, sep=" "))
    x.svd = rank.approx(x, k)
    x[missing.matrix] = x.svd[missing.matrix]
  }
  
  print("Finished SVD Imputation")
  return(x)
}

# combine the reference and test dataset to perform SVD
i.combined.data = rbind(reference.data, i.missing.data)
s.combined.data = rbind(reference.data, s.missing.data)

# perform SVD and get only the test data back 
i.imputed.svd.data = round(svd.imp(i.combined.data)[dim(reference.data)[1]:(dim(reference.data)[1] + test.size - 1),])
s.imputed.svd.data = round(svd.imp(s.combined.data)[dim(reference.data)[1]:(dim(reference.data)[1] + test.size - 1),])

print.details()
cat("SVD k-Nearest Neighbor Illumina Method Accuracy = ", accuracy(test.data[is.na(i.missing.data)], i.imputed.svd.data[is.na(i.missing.data)]) * 100, "\n")
cat("SVD Simple Random Imputation Sequencing Method Accuracy = ", accuracy(test.data[is.na(s.missing.data)], s.imputed.svd.data[is.na(s.missing.data)])  * 100, "\n")

