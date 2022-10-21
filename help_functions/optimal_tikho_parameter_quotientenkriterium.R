optimal_tikho_parameter_quotientenkriterium <- function(R_tilde,
                                                        C_tilde,
                                                        B,
                                                        A,
                                                        alpha
){
  R_tilde <- make_column_vector(R_tilde)
  
  # ---- determine derivative ---- 
  H <- solve(t(A) %*% A + alpha * B)
  d <- -B %*% R_tilde 
  ablei <- H %*% d
  
  # ---- determine residuum ---- 
  res <- A %*% R_tilde - C_tilde
  
  quot <- norm( A%*%(alpha*ablei)-res, type =  "2" ) / norm( res, type = "2" ) 
  # using type = "2" specifies the “spectral” or 2-norm, which is the largest singular value (svd) of x. 
  # in matlab norm() returns the 2-norm or maximum singular value of matrix X, which is approximately max(svd(X))
  return(quot)
}
