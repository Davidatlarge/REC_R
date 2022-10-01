# function to guarantee that z is a column vector afterwards
make_column_vector <- function(z) {

  if (NCOL(z) > 1) z = t(z) 
  
  return(z)    
  
  # return(matrix(z, ncol = 1))
}
