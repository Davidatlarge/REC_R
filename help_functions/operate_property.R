# now operates on a df in the environment instead of re/opening a file
# accepts a column name on that df in the environment instead of a file path/name
operate_property <- function(original_data,
                             parameter, # c("C", "phi", "omega", "beta", "D", "Db") 
                             z_c) {
  # ---- identify the data ---- 
  z_data <- original_data$z
  f_data <- original_data[,parameter]
  
  if(z_data[1] <= z_c[1] & 
     z_data[length(z_data)] >= z_c[length(z_c)]
     ) {
    z_data <- make_column_vector(z_data)
    f_data <- make_column_vector(f_data)
    z_c    <- make_column_vector(z_c)
    
    # do the linear interpolation to the computational grid
    f_c <- pracma::interp1(z_data,f_data,  xi = z_c, method = "linear")
    cat(paste("Data interval for", parameter,"is correct.\n"))
  } else {
    cat("The z-coordinate at the end points of the interval do not match the z-coordinates of the concentration data\n")
  }
  
  # ---- return ---- 
  return(f_c)
}
