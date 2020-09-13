
####################################################
####################### args #######################
####################################################

  # enter data
  # ------ read concentrations ----------------
  z_data <- read.table("REC_v3.1_kit/matlab_code_no_GUI/test_case_2_data_delta/test_case_2_data_delta_C.txt")[,1]
  C_data <- read.table("REC_v3.1_kit/matlab_code_no_GUI/test_case_2_data_delta/test_case_2_data_delta_C.txt")[,2]
  # ---------- porosity ---------------------
  #z_phi <- read.table("REC_v3.1_kit/matlab_code_no_GUI/test_case_2_data_delta/test_case_2_data_delta_phi.txt")[,1]
  phi <- read.table("REC_v3.1_kit/matlab_code_no_GUI/test_case_2_data_delta/test_case_2_data_delta_phi.txt")[,2]
  #error
  ## ---------- vertikal velocity ---------------------
  #z_omega <- read.table("REC_v3.1_kit/matlab_code_no_GUI/test_case_2_data_delta/test_case_2_data_delta_omega.txt")[,1]
  omega <- read.table("REC_v3.1_kit/matlab_code_no_GUI/test_case_2_data_delta/test_case_2_data_delta_omega.txt")[,2]
  #error
  ## ---------- Bioirigation ---------------------
  #z_beta <- read.table("REC_v3.1_kit/matlab_code_no_GUI/test_case_2_data_delta/test_case_2_data_delta_beta.txt")[,1]
  beta <- read.table("REC_v3.1_kit/matlab_code_no_GUI/test_case_2_data_delta/test_case_2_data_delta_beta.txt")[,2]
  #error
  ## ---------- D ---------------------
  #z_D <- read.table("REC_v3.1_kit/matlab_code_no_GUI/test_case_2_data_delta/test_case_2_data_delta_D.txt")[,1]
  D <- read.table("REC_v3.1_kit/matlab_code_no_GUI/test_case_2_data_delta/test_case_2_data_delta_D.txt")[,2]
  #error
  ## ---------- D_b ---------------------
  #z_D_b <- read.table("REC_v3.1_kit/matlab_code_no_GUI/test_case_2_data_delta/test_case_2_data_delta_Db.txt")[,1]
  D_b <- read.table("REC_v3.1_kit/matlab_code_no_GUI/test_case_2_data_delta/test_case_2_data_delta_Db.txt")[,2]
  #error
  ###################
  #setup_name = 'test_case_2_data_delta', # REQUIRED FOR READING IN DATA; REPLACE WITH DIRECT DATA VARIABLE INPUT # Name of setup and name of the data folder
  N_c = 101      # Number of computational grid points
  C_water = 25e3 # Nutrient concentration in water column (only important for irrigation)
  # parameters for tikhonov regularisation
  lambda = 1     # 'smoothing' parameter lambda
  alpha_min = 4 # lowest alpha value for Tikhonov regularisation and ratio criterion ( actually log_10(alpha_min) )
  alpha_max = 15 # largest alpha value for Tikhonov regularisation and ratio criterion ( actually log_10(alpha_max) )
  N_alpha = 501  # Number of ratio criterion evaluations in the alpha interval, to find the minimum
  # setting the boundary conditions for the nutrient concentration
  bnd_cond.type_z_min = 1    # type of boundary condition at the top: 1: for concentration / 2: for derivative
  bnd_cond.C_z_min = 25.0e3  # value of nutrient concentration or derivative at top
  bnd_cond.type_z_max = 1    # type of boundary condition at the bottom: 1: for concentration / 2: for derivative
  bnd_cond.C_z_max = 5.0e3   # value of nutrient concentration or derivative at bottom
  
  integrate_rates_afterwards = 1  # if you want to integrate the obtained rate over a choosen interval
  # 0: no  / 1: yes
  
####################################################
####################### fun start ##################
####################################################
  
  # ========= define required functions ===============
  source("calculate_con_rates_lin_sys_tichonov_mean_rate_2.R")
  source("make_column_vector.R")
  source("make_row_vector.R")
  source("calculate_diff_operator_matrix_aequi_dist_grid_variable_coeff.R")
  source("read_bnd_cond_structure.R")
  source("abl_1.R")
  source("calculate_B_matrix.R")
  source("find_mean_rate.R")
  source("determine_con_and_rate.R")
  source("optimal_ticho_parameter_quotientenkriterium.R")
  source("abl_1_non_aequi.R")
  source("find_local_minimum_with_smallest_x.R")
  # =========================================================================
  
  # =========== some pre - operations ======================================
  l_0 <- 1
  l_1 <- lambda
  l_2 <- lambda
  alpha_ticho <- matlab::logspace(alpha_min, alpha_max, N_alpha) 
  # =========================================================================
  
  # =========  extending Input Data =======================================
  z_c     <- matlab::linspace(z_data[1], z_data[length(z_data)], N_c)
  
  C_c     <- pracma::interp1(z_data, C_data, xi = z_c, method = "linear")
  phi     <- pracma::interp1(z_data, phi, xi = z_c, method = "linear")
  omega   <- pracma::interp1(z_data, omega, xi = z_c, method = "linear")
  beta    <- pracma::interp1(z_data, beta, xi = z_c, method = "linear")
  D       <- pracma::interp1(z_data, D, xi = z_c, method = "linear")
  D_b     <- pracma::interp1(z_data, D_b, xi = z_c, method = "linear")
  D_total <- D + D_b # total diffusivity
  # =========================================================================
    
  # ========= Determine Concentration and Rates =============================
  con_rate <- calculate_con_rates_lin_sys_tichonov_mean_rate_2(C_water,
                                                               #bnd_cond, #OBSOLETE
                                                               alpha_ticho,
                                                               l_0,
                                                               l_1,
                                                               l_2,
                                                               z_c,
                                                               C_c,
                                                               omega,
                                                               D_total,
                                                               beta,
                                                               phi)
  R_out <- con_rate$R_out
  C_out <- con_rate$C_out
  alpha_opt <- con_rate$alpha_opt
    
  # =========================================================================
  
  # ======= graphical output of the results ===========
  # printing the figure to the setup folder
  # i.e. saving a file of the plot (jpg)
  # OBSOLETE
  library(tidyverse)
  ggplot() +
    geom_point(aes(x = z_data, y = C_data)) +
    geom_line(aes(x = z_c, y = C_out), size =1.5, col = "green") +
    coord_flip() + scale_x_reverse(name = "sediment depth") + ylab("concentration") + theme_bw()
  
  # ========= write to output file ======================
  # OBSOLETE results should be put into an r object, probably a list of results, for direct work in r
  # define write_results()?
  # of setup_name, # obsolete, but a name should be definable somewhere
  # R_out, 
  # C_out, 
  # z_c # where it this? - goes into calculate_con_rates_lin_sys_tichonov_mean_rate_2()
  # =====================================================
  
  # ========= calculate fluxes about the interval interfaces =====
  write_fluxes(setup_name,C_out,z_c,omega,D_total,phi) # this calculates fluxes but writes them directly to file
  # ===============================================================
  
  # ======== integrate the rate function =======
  if(integrate_rates_afterwards == 1) {
    integrate_rates(R_out, z_c, setup_name) # this writes the fluxes directly to a file 
  }
  # ============================================
  
  ####################################################
  ####################### fun end ####################
  ####################################################
