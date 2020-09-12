# calculates the first derivative of function f(s) 
# on a non equidistant grid
abl_1_non_aequi <- function(f = -quot_crit,
                            s = alpha_ticho
) {
  n <- length(f)
  
  # ------- determine grid spacings -------
  # delta is vector of grid spacings
  # delta = [0, s(2) - s(1), s(3)-s(2), ... , s(N)-s(N-1)]
  if(matlab::size(s, 1) > 1) { # this is the same as make_row_vector()
    s <- t(s)
  } 
  
  delta <- c(0, diff(s))
  # ---------------------------------------
  
  #------------ interior grid points ---------------------
  for(i in 2:(n-1)) {
    if(!exists("f_abl")) {f_abl <- numeric(0)}
    a <- -delta[i+1] / ( delta[i] * (delta[i] + delta[i+1]) )
    b <- (delta[i+1] - delta[i]) / (delta[i] * delta[i+1])
    c <- delta[i] / ( delta[i+1] * (delta[i] + delta[i+1]) )
    f_abl[i] <- a*f[i-1] + b*f[i] + c*f[i+1]
  } 
  
  # ------ left boundary --------
  a <-  -(2*delta[2] + delta[3] ) / ( delta[2] * (delta[2]+delta[3]) )
  b <- (delta[2] + delta[3]) / (delta[2] * delta[3])
  c <- -delta[2] / ( delta[3] * (delta[2]+delta[3]) )
  
  f_abl[1] <- a*f[1] + b*f[2] + c*f[3]
  
  # ------ right boundary --------
  a <- delta[n] / delta[n-1] / (delta[n] + delta[n-1])
  b <- -(delta[n] + delta[n-1]) / delta[n] / delta[n-1]
  c <- (2*delta[n] + delta[n-1]) / delta[n] / (delta[n] + delta[n-1])
  
  f_abl[n] <- a*f[n-2] + b*f[n-1] + c*f[n]
  
  return(f_abl)
}

# expected output:  f_abl