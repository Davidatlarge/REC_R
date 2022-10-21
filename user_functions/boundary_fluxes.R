# this function calculates the diffusive and advective fluxes
# across any depth horizon.
# By default fluxes across the top and bottom boundary will be calculated.
boundary_fluxes <- function(rec_out, z = c(z_c[1], length(z_c)), explain = TRUE) {
  source("help_functions/abl_1.R", local = TRUE)
  
  C_out <- rec_out$output_data$conc
  z_c   <- rec_out$output_data$z
  phi   <- rec_out$interpol_data$phi
  D_total <- rec_out$interpol_data$D_total
  omega <- rec_out$interpol_data$omega
  
  z1 <- unlist(lapply(z, function(x) which.min(abs(z_c-x))))
  
  # ---- calculate diffusive fluxes ---- 
  C_out_abl <- abl_1(C_out, z_c)
  
  diff_fluxes <- NULL
  for(i in 1:length(z1)) {
    diff_fluxes[i] <- -phi[z1[i]] * D_total[z1[i]] * C_out_abl[z1[i]]
  }
  # flux_diff_top <- -phi[1] * D_total[1] * C_out_abl[1]
  # flux_diff_bottom <- -phi[length(phi)] * D_total[length(D_total)] * C_out_abl[length(C_out_abl)]
  
  # ---- calculate advective fluxes ---- 
  adv_fluxes <- NULL
  for(i in 1:length(z1)) {
    adv_fluxes[i] <- phi[z1[i]] * omega[z1[i]] * C_out[z1[i]]
  }
  # flux_adv_top <- phi[1] * omega[1] * C_out[1]
  # flux_adv_bottom <- phi[length(phi)] * omega[length(omega)] * C_out[length(C_out)]
  
  # ---- return actual depth of calculated flux ---- 
  z <- z_c[z1]
  
  if(explain) {
    cat("A flux is positive, if it is directed into positive z-direction (from top to bottom). \n")
    cat("The diffusive flux is defined as: -phi * (D_effective + D_bio) * dC/dz \n")
    cat("The advective flux is defined as: phi * omega * C \n")
    cat("The total flux is the sum of the diffusive and the advective  flux. \n")
    cat("All fluxes are given in: [cm * muM / s] \n")
    cat("Or in related units, if not the standard unit system is used. \n\n")
  }
  
  # ---- return ---- 
  return(
    data.frame(z = z, 
               diffusive = diff_fluxes, 
               advective = adv_fluxes, 
               total = diff_fluxes+adv_fluxes)
    # data.frame(boundary  = c("top", "bottom"),
    #            z         = c(z_c[1], z_c[length(z_c)]),
    #            diffusive = c(flux_diff_top, flux_diff_bottom),
    #            advective = c(flux_adv_top, flux_adv_bottom),
    #            total     = c(flux_diff_top+flux_adv_top, flux_diff_bottom+flux_adv_bottom))
  )
}



