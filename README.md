# Genotype Imputation
Statistical Inference of Unobserved Genotypes - CM224 Computational Genetics Project

##Background

A large problem in bioinformatics is the inability to get complete data. Since it is a data driven field, this creates many issues. In sequencing data aspect of bioinformatics, we often find difficulties in generating genotypes of individuals for a variety of reasons. Genotype imputation is the term used to describe the process of predicting or imputing genotypes 
that are not directly assayed in a sample of individuals. This can be as simple (and naive) as choosing a random value from the domain of the genetic data, or as complicated as analyzing all existing values to determine the most likely value for missing data points.

##Contents
+ 1000genome_data.R - load 1000 genome chr-22 data
+ cm224_data.R - load cm224 datasets
+ simulate_data.R - simulate missing values in test data
+ baseline_imputation.R - baseline imputation code
+ improved_imputation.R - improved imputation code
+ utils.R - supplementary functions

##Baseline Method

The baseline method for this project is random imputation. It is the most simplest and naïve approach to imputation, replacing all the missing values with random values of 0, 1 or 2. 

##Improved Method

The improved method for this project is SVD imputation. SVD Imputation is a popular concept that was applied on the Netflix challenge of predicting user ratings of movies using already observed data of other users. A similar approach is applied but with a couple of modifications to the logic of the algorithm. SVD basically applies the concept of Low Rank matrix factorization which is applied iteratively to solve the imputation problem. 

##Benchmark

###F1 Score

F1 score is a measure of precision and recall. We see an overall increase in F1 score for the improved method over the baseline method. F1 score drops as the number of SNP's to impute increases.

##Concluding Remarks

Imputation is not an easy process to do with high accuracy. Even though random imputation worked in certain scenarios like in the 1000 Genome simulation it clearly failed on the project data. Even though SVD does provide a solution it does require improvement in terms of accounting for high correlation between SNPs. Lastly since everything was written in R porting to Python would surely provide faster results.

