# function for integrating the estimated rates
# over a chosen depth interval
# using Simpson´s rule
integrate_rates <- function(rec_out, 
                            z_min   = NULL, # define integration boundaries;
                            z_max   = NULL, # this was originally done with graphic input (i.e. clicking on a graph)
                            explain = TRUE) {
  
  R <- rec_out$output_data$rate
  z_c <- rec_out$output_data$z
  if(is.null(z_min)) z_min <- min(z_c)
  if(is.null(z_max)) z_max <- max(z_c) 
  z_int <- sort(c(z_min, z_max))  # must be increasing order
  
  # ---- interpolate values ---- 
  z_int_spl <- seq(from = z_int[1], to = z_int[2], length.out = 5000)
  R_int_spl <- spline(x = z_c, y = R, method = "natural", xout = z_int_spl)$y

  # do the integration via Simpsons rule
  # It is important that the z-grid is equidistant.
  # step-wise
  h <- z_int_spl[2] - z_int_spl[1]
  
  # ---- weights ---- 
  a <- c(3/8, 7/6, 23/24)
  b <- matrix(data = 1, nrow = 1, ncol = length(R_int_spl)-6)
  c <- c(23/24, 7/6, 3/8)
  weights <- c(a, b, c)
  
  if(explain) {
    cat("The depth integration is done by Simpson's Rule on the equidistant z-grid.  \n")
    cat("The unit of the integrated rate is: [nmol/(cm^2 s)] \n")
    cat("Or in related units, if not the standard unit system is used. \n")
    
  }
  integral <- h * sum(R_int_spl * weights)
  
  # ---- return ---- 
  return(integral)
}


