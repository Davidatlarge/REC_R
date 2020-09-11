# ------ Finding the mean rate -------------
find_mean_rate <- function(A,
                           C_hat
) {
  N_red <- length(C_hat)
  
  E <- matlab::ones(N_red, 1)
  M <- A %*% E 
  
  R_c <- 1 / (t(M) %*% M) %*% (t(M) %*% C_hat)
  R_mean <- R_c %*% matlab::ones(N_red, 1)
  
  
  C_mean <- A %*% R_mean
  
  # return
  list("R_mean" = R_mean,
       "C_mean" = C_mean)
}

# expected output: [R_mean,C_mean]