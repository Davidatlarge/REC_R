# ------ Finding the mean rate -------------
find_mean_rate <- function(A,
                           C_hat
) {
  N_red <- max(dim(C_hat)) # matlab was length(C_hat): For arrays with more dimensions, the length is max(size(X)).
  
  E <- matlab::ones(N_red, 1)
  M <- A %*% E # A is a square matrix, E is a 1-column matrix, so M is basically rowsums of A
  
  R_c <- 1 / (t(M) %*% M) %*% (t(M) %*% C_hat)
  R_mean <- as.numeric(R_c) * matlab::ones(N_red, 1) # to perform multiplication of single-value R_c with each element of the ones matrix R_c must be numeric
  # in matlab "if one of A or B is a scalar, then the scalar is combined with each element of the other array"
  
  C_mean <- A %*% R_mean # A is a square matrix, R_mean is a 1-column matrix, C_mean is a 1-column matrix
  
  # return
  mean_rate <- list("R_mean" = R_mean, # R_mean is a 1-column matrix
       "C_mean" = C_mean) # C_mean is a 1-column matrix
  
  return(mean_rate)
}

# expected output: [R_mean,C_mean]
