# ------ Finding the mean rate -------------
find_mean_rate <- function(A,
                           C_hat
) {
  N_red <- max(dim(C_hat)) # matlab was length(C_hat): For arrays with more dimensions, the length is max(size(X)).
  
  E <- matlab::ones(N_red, 1) # using ones(N_red, N_red) instead of ones(N_red, 1) (original) to facilitate multiplication with A
  M <- A %*% E 
  
  R_c <- 1 / (t(M) %*% M) %*% (t(M) %*% C_hat)
  R_mean <- as.numeric(R_c) * matlab::ones(N_red, 1) # to perform multiplication of single-value R_c with each element of the ones matrix R_c must be numeric
  
  
  C_mean <- A %*% R_mean
  
  # return
  list("R_mean" = R_mean,
       "C_mean" = C_mean)
}

# expected output: [R_mean,C_mean]
