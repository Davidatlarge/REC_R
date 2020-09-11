REC <- function(
  setup_name = 'test_case_2_data_delta', # REQUIRED FOR READING IN DATA; REPLACE WITH DIRECT DATA VARIABLE INPUT # Name of setup and name of the data folder
  N_c = 101,      # Number of computational grid points
  C_water = 25e3, # Nutrient concentration in water column (only important for irrigation)
  
  # parameters for tikhonov regularisation
  lambda = 1,     # 'smoothing' parameter lambda
  alpha_min = 4, # lowest alpha value for Tikhonov regularisation and ratio criterion ( actually log_10(alpha_min) )
  alpha_max = 15, # largest alpha value for Tikhonov regularisation and ratio criterion ( actually log_10(alpha_max) )
  N_alpha = 501,  # Number of ratio criterion evaluations in the alpha interval, to find the minimum
  
  # setting the boundary conditions for the nutrient concentration
  bnd_cond.type_z_min = 1,    # type of boundary condition at the top: 1: for concentration / 2: for derivative
  bnd_cond.C_z_min = 25.0e3,  # value of nutrient concentration or derivative at top
  
  bnd_cond.type_z_max = 1,    # type of boundary condition at the bottom: 1: for concentration / 2: for derivative
  bnd_cond.C_z_max = 5.0e3,   # value of nutrient concentration or derivative at bottom
  
  integrate_rates_afterwards = 1  # if you want to integrate the obtained rate over a choosen interval
  # 0: no  / 1: yes
) {
  
  # ========= define required functions ===============
  source()
  # =========================================================================
  
  
  # =========== some pre - operations ======================================
  l_0 <- 1
  l_1 <- lambda
  l_2 <- lambda
  alpha_ticho <- logspace(alpha_min, alpha_max, N_alpha) # logspace in r is the same as in matlab
  # =========================================================================
  
  # ========= Determine Concentration and Rates =============================
  con_rate <- calculate_con_rates_lin_sys_tichonov_mean_rate_2(C_water,
                                                               bnd_cond,
                                                               alpha_ticho,
                                                               l_0,
                                                               l_1,
                                                               l_2,
                                                               z_c,
                                                               C_c,
                                                               omega,
                                                               D_total,
                                                               beta,phi)
  R_out <- con_rate$R_out
  C_out <- con_rate$C_out
  alpha_opt <- con_rate$alpha_opt
    
  # =========================================================================
  
  # ======= graphical output of the results ===========
  # to be done
  
  # printing the figure to the setup folder
  # i.e. saving a file of the plot (jpg)
  # OBSOLETE
  
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
}