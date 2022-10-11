# this function calculates the diffusive and advective fluxes
# across the interfaces of the considered depth interval
boundary_fluxes <- function(rec_out, explain = TRUE) {
  source("help_functions/abl_1.R", local = TRUE)
  
  C_out <- rec_out$output_data$conc
  z_c   <- rec_out$output_data$z
  phi   <- rec_out$interpol_data$phi
  D_total <- rec_out$interpol_data$D_total
  omega <- rec_out$interpol_data$omega
  
  # ---- calculate diffusive fluxes ---- 
  C_out_abl <- abl_1(C_out, z_c)
  
  flux_diff_top <- -phi[1] * D_total[1] * C_out_abl[1]
  flux_diff_bottom <- -phi[length(phi)] * D_total[length(D_total)] * C_out_abl[length(C_out_abl)]
  
  # ---- calculate advective fluxes ---- 
  flux_adv_top <- phi[1] * omega[1] * C_out[1]
  flux_adv_bottom <- phi[length(phi)] * omega[length(omega)] * C_out[length(C_out)]
  
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
    data.frame(boundary  = c("top", "bottom"),
               z         = c(z_c[1], z_c[length(z_c)]),
               diffusive = c(flux_diff_top, flux_diff_bottom),
               advective = c(flux_adv_top, flux_adv_bottom),
               total     = c(flux_diff_top+flux_adv_top, flux_diff_bottom+flux_adv_bottom))
  )
}



