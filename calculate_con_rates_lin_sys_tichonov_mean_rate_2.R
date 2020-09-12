# Finding the best solution with Tikhonov-Regularization
# First, a mean rate is estimated.
# Only the ratio criterion is used for estimating the optimal alpha.

calculate_con_rates_lin_sys_tichonov_mean_rate_2 <- function(
  C_water,      # from top fun
  #bnd_cond,    # OBSOLETE
  alpha_ticho,  # from top fun
  l_0,          # from top fun
  l_1,          # from top fun
  l_2,          # from top fun
  z_c,          # from reading input files
  C_c,          # from reading input files
  omega,        # from reading input files
  D_total,      # from reading input files
  beta,         # from reading input files
  phi           # from reading input files
) {
  # -------- scaling the data ------------
  C_scal <- max(C_c)
  C_c <- C_c / C_scal
  C_water <- C_water / C_scal
  bnd_cond.C_z_min <- bnd_cond.C_z_min / C_scal
  bnd_cond.C_z_max <- bnd_cond.C_z_max / C_scal
  
  print('Starting Tichonov - Regularization')
  # ------- some pre-calculations ------
  z_c_red <- z_c[2:(length(z_c)-1)]
  C_c_red <- C_c[2:(length(z_c)-1)]
  N_red <- length(z_c_red)
  C_c_red <- make_column_vector(C_c_red)
  delta_z <- z_c[2] - z_c[1]
  
  # ----- Calculate the neccessary matrices ------
  A_e_F_d <- calculate_diff_operator_matrix_aequi_dist_grid_variable_coeff(z_c, D_total, omega, beta, phi, C_water#, 
                                                                           #bnd_cond # OBSOLETE
                                                                           )
  A <- A_e_F_d$A
  e <- A_e_F_d$e
  F <- A_e_F_d$Diff_op
  d <- A_e_F_d$d
  
  A_ad <- t(A)

  B_L0_L1_L2 <- calculate_B_matrix(l_0, l_1, l_2, N_red, z_c_red)
  B <- B_L0_L1_L2$B
  L0 <- B_L0_L1_L2$L0
  L1 <- B_L0_L1_L2$L1
  L2 <- B_L0_L1_L2$L2
  
  print ('ready with Tichonov - Matrices: A and B')
  
  C_hat <- C_c_red + e
  # ----------------------------------------------
  
  
  # --------- finding the mean rate and respective concentration ------
  mean_rate <- find_mean_rate(A, C_hat)
  R_mean <- mean_rate$R_mean
  C_mean <- mean_rate$C_mean
  C_tilde <- C_hat - C_mean
  # --------------------------------------------------------------------
  
  # -------- Estimating the best alpha Parameter -----------------------
  for(k in 1:length(alpha_ticho)) {
    
    alpha <- alpha_ticho[k]
    
    # ------- finding the tichonov solution --------  	
    R_tilde <- pracma::inv(A_ad %*% A + alpha * B) %*% A_ad %*% C_tilde	
    # ----------------------------------------------
    
    # ------- constructing the final solution -------
    con_rate <- determine_con_and_rate(delta_z, R_tilde, A, e, R_mean, C_mean
                                       #, bnd_cond # OBSOLETE
    )
    if(k == 1) {
      R_tich_c_m <- con_rate$R_c
      C_tich_c_m <- con_rate$C_c
    } else if(k > 1){
      R_tich_c_m <- rbind(R_tich_c_m, con_rate$R_c)
      C_tich_c_m <- rbind(C_tich_c_m, con_rate$C_c)
    } 
    # matlab code: [R_tich_c_m(k,:),C_tich_c_m(k,:)] = determine_con_and_rate(delta_z,R_tilde,A,e,R_mean,C_mean,bnd_cond);
    # -----------------------------------------------
    
    # -------- calculate Tichonov-optimal parameter function -------
    OTPQ <- optimal_ticho_parameter_quotientenkriterium(R_tilde, C_tilde, B, A, alpha)
    
    if(k == 1) {
      quot_crit <- OTPQ
    } else if(k > 1){
      quot_crit <- append(quot_crit, OTPQ)
    } 
    # matlab code: quot_crit(k) = optimal_ticho_parameter_quotientenkriterium(R_tilde,C_tilde,B,A,alpha)
  }
  # ---------------------------------------------------------------------
  
  # ------ finding the minima of the criteria functions --        
  locmin <- find_local_minimum_with_smallest_x(-quot_crit, alpha_ticho)
  mini <- locmin$x_min
  index <- locmin$ind_min
  found <- locmin$found
  
  if(found == 0) {# no local minimum
    mini <- min(-quot_crit)
    index <- which.min(-quot_crit)
  } 
  
  print('finding optimal alpha with ratio criterion')        
  alpha_opt <- alpha_ticho[index]
  print(paste('optimal alpha for Tichonov - Regularization:\n', alpha_opt))
  
  R_out <- R_tich_c_m[index,]
  C_out <- C_tich_c_m[index,]
  # ------------------------------------------------------------------------
  
  # -------- re-scaling the data ------------
  C_out <- C_out * C_scal
  R_out <- R_out * C_scal
  # -------------------------------------
  
  # return
  list("R_out" = R_out,
       "C_out" = C_out,
       "alpha_opt" = alpha_opt)
  
}

## expected output: [R_out,C_out,alpha_opt]