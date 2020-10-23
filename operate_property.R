# ---------------------------------------------
operate_property <- function(file_name_in,
                             z_c,
                             title_name
) {
  error <- 0
  
  z_data <- read.table(file_name_in)[,1]
  f_data <- read.table(file_name_in)[,2]
  
  if(z_data[1] == z_c[1] & z_data[length(z_data)] == z_c[length[z_c]]) {
    z_data <- make_column_vector(z_data)
    f_data <- make_column_vector(f_data)
    z_c <- make_column_vector(z_c)
    
    f_c <- pracma::interp1(z_data,f_data,z_c,'linear')
    
    error <- 0.0
    print('data are correct')
  } else {
    print('The z-coordinate at the end points do not match the z-coordinates of the concentration data')
    error <- 1.0
  }
  
  props <- list(z_c, f_c, error)
  
  return(props)
}

# expected output: [z_c,f_c,error]