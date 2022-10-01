
rec <- function(
  original_data,
  N_c,                 # Number of computational grid points
  C_water,             # Nutrient concentration in water column (only important for irrigation)
  # parameters for Tikhonov regularization
  lambda,              # 'smoothing' parameter lambda
  alpha_min,           # lowest alpha value for Tikhonov regularisation and ratio criterion ( actually log_10(alpha_min) )
  alpha_max,           # largest alpha value for Tikhonov regularisation and ratio criterion ( actually log_10(alpha_max) )
  N_alpha,             # Number of ratio criterion evaluations in the alpha interval, to find the minimum
  # setting the boundary conditions for the nutrient concentration
  bnd_cond_type_z_min, # type of boundary condition at the top: 1: for concentration / 2: for derivative
  bnd_cond_C_z_min,    # value of nutrient concentration or derivative at top
  bnd_cond_type_z_max, # type of boundary condition at the bottom: 1: for concentration / 2: for derivative
  bnd_cond_C_z_max     # value of nutrient concentration or derivative at bottom
  
) {
  # ---- load packages and helping functions ---- 
  suppressMessages(library(matlab)) 
  suppressWarnings(suppressMessages(library(pracma)))
  suppressMessages(library(scales)) 
  # source helper functions in a for loop because the argument local = TRUE is not effective in apply
  for(i in list.files(paste0(getwd(), "/help_functions"), pattern = ".R$", full.names = TRUE)) { source(i, local = TRUE) }
  
  # ---- some pre - operations ---- 
  z_data  <- original_data$z
  z_c     <- matlab::linspace(z_data[1], z_data[length(z_data)], N_c)
  
  C_c     <- operate_property(original_data, "C", z_c)$f_c
  phi     <- operate_property(original_data, "phi", z_c)$f_c
  omega   <- operate_property(original_data, "omega", z_c)$f_c
  beta    <- operate_property(original_data, "beta", z_c)$f_c
  D       <- operate_property(original_data, "D", z_c)$f_c
  D_b     <- operate_property(original_data, "Db", z_c)$f_c
  D_total <- D + D_b         # total diffusivity
  
  # store the boundary conditions into list
  bnd_cond <- list(type_z_min = bnd_cond_type_z_min,
                   C_z_min    = bnd_cond_C_z_min,
                   type_z_max = bnd_cond_type_z_max,
                   C_z_max    = bnd_cond_C_z_max)
  
  # for Tikhonov regularization
  l_0 <- 1
  l_1 <- lambda
  l_2 <- lambda
  alpha_tikho <- matlab::logspace(alpha_min, alpha_max, N_alpha)
  
  # ---- Determine Concentration and Rates ---- 
  con_rate <- calculate_con_rates_lin_sys_Tikhonov_mean_rate_2(C_water,
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
                                                               phi)
  
  # ---- combine results in an object ---- 
  rec_out <- list(input_data = original_data,
                  input_pars = list(N_c=N_c, C_water=C_water, 
                                    lambda=lambda, alpha_min=alpha_min, alpha_max=alpha_max, N_alpha=N_alpha, 
                                    bnd_cond_type_z_min=bnd_cond_type_z_min, bnd_cond_C_z_min=bnd_cond_C_z_min, 
                                    bnd_cond_type_z_max=bnd_cond_type_z_max, bnd_cond_C_z_max=bnd_cond_C_z_max), 
                  interpol_data = data.frame(C_c,
                                             phi,
                                             omega,
                                             beta,
                                             D,
                                             D_b,
                                             D_total),
                  output_data = data.frame(z = z_c, 
                                           conc = con_rate$C_out, 
                                           rate = con_rate$R_out),
                  alpha_opt = con_rate$alpha_opt,
                  alpha = con_rate$alpha_tikho,
                  Tikhonov_criterium = con_rate$quot_crit)
  
  return(rec_out)
}
