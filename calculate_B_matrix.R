# Matrix B for sympathy functional
# ----------- calculate B-Matrix -------------------
calculate_B_matrix <- function(lam_0,
                               lam_1,
                               lam_2,
                               N_red,
                               z_c
) {
  delta_z <- z_c[2] - z_c[1]
  
  # ------ L0 -----------------
  L0 <- matlab::eye(N_red)
  # ---------------------------
  
  # ------ L1 / 1st Derivative -----------------
  N_L1 <- N_red - 1
  L1 <- matlab::zeros(N_L1, N_red)
  
  for(i in 1:N_L1) {
    L1[i,i] <- -1
    L1[i,i+1] <- 1
  }
  
  L1 <- 1/delta_z * L1
  # ------------------------------------------------
  
  # ------ L2 / 2nd Derivative -----------------
  N_L2 <- N_red - 2
  L2 <- matlab::zeros(N_L2, N_red)
  
  for(i in 1:N_L2) {
    L2[i,i] <- 1
    L2[i,i+1] <- -2
    L2[i,i+2] <- 1
  }
  
  L2 <- 1/(delta_z)^2 * L2
  # ------------------------------------------------
  
  # ------- B -------------------------------------
  B <- lam_0*t(L0)%*%L0 + lam_1*t(L1)%*%L1 + lam_2*t(L2)%*%L2
  # -----------------------------------------------
  # return
  list(
    "B" = B,
    "L0" = L0,
    "L1" = L1,
    "L2" = L2
  )
}

# expected output:  [B,L0,L1,L2]