# Function to construct the discrete inverse Differential Operator A for Berg problem
# on an aequidistant grid in z direction.
# It is A = inv(Diff_op).
#
# The coefficient functions por, D, omega and beta are z-dependend.
#
# For the first derivative term, a blended scheme according to Boudreau is
# used. For the second derivative term, a central difference scheme is used.

calculate_diff_operator_matrix_aequi_dist_grid_variable_coeff <- function(z, # PROBALY z_c
                                                                          D,
                                                                          omega,
                                                                          beta,
                                                                          por, # MAYBE phi ??????
                                                                          C_water#,
                                                                          #bnd_cond # OBSOLETE
) {
  N_grid <- length(z)
  N <- N_grid - 2  # Dimension of Matrix: N = N_grid - 2
  
  # --------- reading the bnd_con structure ----- # requires defining read_bnd_cond_structure()
  C_z_min <- read_bnd_cond_structure()$C_z_min
  C_z_max <- read_bnd_cond_structure()$C_z_max
  type_z_min <- read_bnd_cond_structure()$type_z_min
  type_z_max <- read_bnd_cond_structure()$type_z_max
  # ---------------------------------------------
  
  # ----- make row vectors -------------- # requires defining make_row_vector()
  z <- make_row_vector(z) 
  D <- make_row_vector(D)
  omega <- make_row_vector(omega)
  beta <- make_row_vector(beta)
  por <- make_row_vector(por)
  # -------------------------------------
  
  # ------- determine grid spacings -------
  delta_z <- z[2] - z[1]
  # ---------------------------------------
  
  # ------ construct coefficient vectors r(z) and s(z) -----
  omega2 <- omega * por # element-wise multiplication is via "*" in r but ".*" in matlab
  D2 <- D * por
  beta2 <- beta * por
  
  D2_abl <- abl_1(D2, z) # requires defining abl_1()
  omega2_abl <- abl_1(omega2, z) # requires defining abl_1()
  
  r <- omega2_abl + beta2
  s <- omega2 - D2_abl
  # -----------------------------------------------------
  
  # ----- construct blending factor sigma ---------------
  for(i in 1:N_grid ) {
    if(!exists("sig")) {sig <- numeric(0)}
    if(s[i] == 0) {
      sig[i] <- 0
    } else {
      eta <- s[i] * delta_z / ( 2 * D2[i] )
      sig[i] <- pracma::coth(eta) - 1/eta
    }
  }
  # -------------------------------------------------------
  
  # -------- construct the matrix coefficients ------------
  aa <- -(1+sig) * s/(2*delta_z) - D2/(delta_z)^2
  bb <- r + s * sig/delta_z + 2*D2/(delta_z)^2
  cc <- (1-sig) * s/(2*delta_z) - D2/(delta_z)^2
  # -------------------------------------------------------
  
  # ----- Construction of the forward Diff_op -------------------------
  Diff_op <- matlab::zeros(N, N)
  for(i in 2:N-1) {  
    Diff_op[i, i-1] <- aa[i+1]
    Diff_op[i,i]   <- bb[i+1]
    Diff_op[i,i+1] <- cc[i+1]
  }
  
  # first line
  if(type_z_min == 1) { # Dirichlet conditions
    print('Using Dirichlet boundary conditions for top boundary')
    Diff_op[1,1] <- bb[2]
    Diff_op[1,2] <- cc[2]
  } else if(type_z_min == 2) { # von Neumann conditions
    print('Using von Neumann boundary conditions for top boundary')
    Diff_op[1,1] <- bb[2] + 4/3*aa[2]
    Diff_op[1,2] <- cc[2] - 1/3*aa[2]
  }
  
  # N th line
  if(type_z_max == 1) { # Dirichlet conditions
    print('Using Dirichlet boundary conditions for bottom boundary')
    Diff_op[N,N-1] <- aa[N_grid-1]
    Diff_op[N,N] <- bb[N_grid-1]
  } else if(type_z_max == 2) { # von Neumann conditions
    print('Using von Neuman boundary conditions for bottom boundary')
    Diff_op[N,N-1] = aa[N_grid-1] - 1/3*cc[N_grid-1]
    Diff_op[N,N] = bb[N_grid-1] + 4/3*cc[N_grid-1]
  }
  # -------------------------------------------------------
  
  # ------ construction of A -------------------------
  #Diff_op
  A <- pracma::inv(Diff_op)
  # -------------------------------------------------
  
  # ------- Constructing e -----------------
  d <- -C_water * t(beta2[2:(N_grid-1)])  # results in a [1:N] matrix

  # first line of d 
  if(type_z_min == 1) {                   # Dirichlet conditions
    d[1] <- d[1] + aa[2]*C_z_min  
  } else if(type_z_min == 2) {            # von Neumann conditions
    m_z_min <- C_z_min                    # remember, m_z_min=C_z_min
    d[1] <- d[1] - 2/3*delta_z*aa[2]*m_z_min
  }
  
  # last line of d
  if(type_z_max == 1) {                   # Dirichlet conditions
    d[N] <- d[N] + cc[N_grid-1]*C_z_max
  } else if(type_z_max == 2) {            # von Neumann conditions
    m_z_max <- C_z_max                    # remember, m_z_max=C_z_max
    d[N] <- d[N] + 2/3*delta_z*cc[N_grid-1]*m_z_max
  }
  
  e <- A %*% matrix(d, nrow=length(d), ncol=length(d), byrow = FALSE) # repeat the [1:N] matrix 'C_c_red' N times to make it an [N:N] matrix
  # ----------------------------------------
  
  # return
  list(
    "A" = A,
    "e" = e,
    "Diff_op" = Diff_op,
    "d" = d
  )
  # -----------------------------------------
  
}
