# Finding the best solution with Tikhonov regularization
# First, a mean rate is estimated.
# Only the ratio criterion is used for estimating the optimal alpha.

calculate_con_rates_lin_sys_Tikhonov_mean_rate_2 <- function(
  C_water,      
  bnd_cond,     
  alpha_tikho,  
  l_0,         
  l_1,          
  l_2,         
  z_c,          
  C_c,          
  omega,        
  D_total,      
  beta,         
  phi          
) {
  
  # ---- scaling the data ---- 
  C_scal  <- max(C_c)
  C_c     <- C_c / C_scal
  C_water <- C_water / C_scal
  bnd_cond$C_z_min <- bnd_cond$C_z_min / C_scal 
  bnd_cond$C_z_max <- bnd_cond$C_z_max / C_scal 
  
  cat("Starting Tikhonov regularization\n")
  # ---- some pre-calculations ---- 
  z_c_red <- z_c[2:(length(z_c)-1)]
  C_c_red <- C_c[2:(length(z_c)-1)]
  N_red   <- length(z_c_red)
  C_c_red <- make_column_vector(C_c_red)
  delta_z <- z_c[2] - z_c[1]
  
  # ---- Calculate the necessary matrices ---- 
  A_e_F_d <- calculate_diff_operator_matrix_aequi_dist_grid_variable_coeff(z_c, D_total, omega, beta, phi, C_water, bnd_cond)
  A <- A_e_F_d$A # a matrix
  e <- A_e_F_d$e
  
  #F <- A_e_F_d$Diff_op # a matrix # not used again in this function
  #d <- A_e_F_d$d # not used again in this function
  
  A_ad <- t(A) # a matrix
  
  B_L0_L1_L2 <- calculate_B_matrix(l_0, l_1, l_2, N_red, z_c_red)
  B <- B_L0_L1_L2$B
  #L0 <- B_L0_L1_L2$L0 # not used again in this function
  #L1 <- B_L0_L1_L2$L1 # not used again in this function
  #L2 <- B_L0_L1_L2$L2 # not used again in this function
  
  cat("ready with Tikhonov - Matrices: A and B\n")
  
  C_hat <- C_c_red + e # C_c_red is a column vector, e is a matrix
  
  
  # ---- finding the mean rate and respective concentration ---- 
  mean_rate <- find_mean_rate(A, C_hat)
  R_mean    <- mean_rate$R_mean # a 1-column matrix
  C_mean    <- mean_rate$C_mean # a 1-column matrix
  C_tilde   <- C_hat - C_mean   # C_hat is a matrix, C_mean is a 1-column matrix
  
  
  # ---- Estimating the best alpha Parameter ---- 
  for(k in 1:length(alpha_tikho)) {
    
    alpha <- alpha_tikho[k]
    
    # finding the Tikhonov solution	
    R_tilde <- pracma::inv(A_ad %*% A + alpha * B) %*% A_ad %*% C_tilde	
    # A_ad is a matrix, A is a matrix, alpha is a scalar, B is a matrix, C_tilde is a 1-column matrix
    
    # constructing the final solution
    con_rate <- determine_con_and_rate(delta_z, R_tilde, A, e, R_mean, C_mean, bnd_cond)
    if(k == 1) { # binding rows
      R_tich_c_m <- con_rate$R_c
      C_tich_c_m <- con_rate$C_c
    } else if(k > 1){
      R_tich_c_m <- rbind(R_tich_c_m, con_rate$R_c)
      C_tich_c_m <- rbind(C_tich_c_m, con_rate$C_c)
    } 
    # matlab code: [R_tich_c_m(k,:),C_tich_c_m(k,:)] = determine_con_and_rate(delta_z,R_tilde,A,e,R_mean,C_mean,bnd_cond)
    
    # calculate Tikhonov-optimal parameter function
    OTPQ <- optimal_tikho_parameter_quotientenkriterium(R_tilde, C_tilde, B, A, alpha)
    
    if(k == 1) { # combining scalars to vector
      quot_crit <- OTPQ
    } else if(k > 1){
      quot_crit <- append(quot_crit, OTPQ)
    } 
    # matlab code: quot_crit(k) = optimal_tikho_parameter_quotientenkriterium(R_tilde,C_tilde,B,A,alpha)
  }
  
  # ---- finding the minima of the criteria functions ----      
  # locmin <- find_local_minimum_with_smallest_x(-quot_crit, alpha_tikho)
  # #mini <- locmin$x_min # not used again in this function
  # index <- locmin$ind_min
  # found <- locmin$found
  # 
  # if(found == 0) {# no local minimum
  #   #mini <- min(-quot_crit) # not used again in this function
  #   print("No local minimum of the Tikhonov criterion found. Using absolute minimum. But you can change alpha limits to look for a local minimum.")
  #   index <- which.min(-quot_crit)
  # } 
  index <- find_local_minimum(values = -quot_crit)
  
  cat("finding optimal alpha with ratio criterion\n")        
  alpha_opt <- alpha_tikho[index]
  cat(paste("optimal alpha for Tikhonov regularization:", alpha_opt, "\n"))
  
  R_out <- R_tich_c_m[index,]
  C_out <- C_tich_c_m[index,]
  
  # ---- re-scaling the data ---- 
  C_out <- C_out * C_scal # C_out is a Vector, C_scal is a Scalar
  R_out <- R_out * C_scal # R_out is a Vector, C_scal is a Scalar
  
  # ---- return ---- 
  con_rate_mean <- list(R_out       = R_out,
                        C_out       = C_out,
                        alpha_opt   = alpha_opt,
                        quot_crit   = quot_crit,
                        alpha_tikho = alpha_tikho)
  
  return(con_rate_mean)
  
}
