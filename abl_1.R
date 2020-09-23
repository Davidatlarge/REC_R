# Calculates the first derivative of the function f(s)
# by finite differences. 
abl_1 <- function(
  f,
  s
){
  delta <- s[2] - s[1]   # Spacing of s
  n <- length(f)
  
  #------------ central 4th order -------------------------------------
  for(i in 2:(n-1)) {
    if(!exists("f_abl")) {f_abl <- numeric(0)}
    if(i == 2) {
      f_abl[i] <- (-2*f[i-1] - 3*f[i] + 6*f[i+1] - f[i+2]) / (6*delta)
    }
    if(i >= 3 & i <= n-2) {
      f_abl[i] <- (-f[i+2] + 8*f[i+1] - 8*f[i-1] + f[i-2]) /(12*delta)
    }
    if(i == n-1) {
      f_abl[i] <- (2*f[i+1] + 3*f[i] - 6*f[i-1] + f[i-2]) / (6*delta)
    }
  }
  
  f_abl[1] <- (-3*f[1] + 4*f[2] - f[3]) / (2*delta)
  f_abl[n] <- (3*f[n] - 4*f[n-1] + f[n-2]) / (2*delta)
  #--------------------------------------------------------------------
}

# expected ouput: f_abl