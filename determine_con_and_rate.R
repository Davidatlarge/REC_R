# R_c : total rate from linear system on computational grid
# C_c : total nutrient concentration from linear system on computational grid
determine_con_and_rate <- function(delta_z,
                                   R_tilde,
                                   A,
                                   e,
                                   R_mean,
                                   C_mean#,
                                   #bnd_cond # OBSOLETE
) {
  # ------ reading the boundary conditions structure ----
  ### Comment Marko: there seems to be no need for such an overcomplicated way of renaming a variable...
  #bnd_cond_str <- read_bnd_cond_structure() ### Comment Marko: This function is useless?!
  # C_z_min <- bnd_cond_str$C_z_min
  # C_z_max <- bnd_cond_str$C_z_max
  # type_z_min <- bnd_cond_str$type_z_min
  # type_z_max <- bnd_cond_str$type_z_max
  
  C_z_min <- bnd_cond.C_z_min
  C_z_max <- bnd_cond.C_z_max
  type_z_min <- bnd_cond.type_z_min
  type_z_max <- bnd_cond.type_z_max
  # -----------------------------------------------------
  
  # ------ finding total nutrient concentration  ---
  C_tilde <- A %*% R_tilde
  C_hat_r <- C_tilde + C_mean
  C_r <- C_hat_r - e
  
  # top boundary condition
  if(type_z_min == 1) { # Dirichlet conditions
    bnd_z_min <- C_z_min
  } else if(type_z_min == 2) { # von Neumann conditions
    m_z_min <- C_z_min
    C_2 <- C_r[1]
    C_3 <- C_r[2]
    bnd_z_min <- 4/3*C_2 - 1/3*C_3 - 2/3*delta_z*m_z_min
  }
  
  # bottom boundary condition
  if(type_z_max == 1) { # Dirichlet conditions
    bnd_z_max <- C_z_max
  } else if(type_z_max == 2) { # von Neumann conditions
  m_z_max <- C_z_max
  C_Nminus1 <- C_r[length(C_r)]
  C_Nminus2 <- C_r[length(C_r)-1]
  bnd_z_max <- 4/3*C_Nminus1 - 1/3*C_Nminus2 + 2/3*delta_z*m_z_max
  }    

  C_c <- matrix(c(bnd_z_min, C_r, bnd_z_max)) # matlab code: C_c = [bnd_z_min; C_r; bnd_z_max]
  # -------------------------------------------------
  
  # -- finding total rate --------------------------
  R_red <- R_tilde + R_mean 
  R_c <- matrix( c(R_red[1], R_red, R_red[length(R_red)]) ) # matlab code: R_c = [R_red(1) ; R_red ; R_red(end)]
  # ------------------------------------------------
  
  # ------ making row-vectors ----------------
  R_c <- make_row_vector(R_c)
  C_c <- make_row_vector(C_c)
  # ------------------------------------------
  
  #return
  con_rate <- list("R_c" = R_c,
       "C_c" = C_c)
  
  return(con_rate)
}

# expected output: R_c, C_c 