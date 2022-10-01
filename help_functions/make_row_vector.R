# function to guarantee that z is a row vector afterwards
make_row_vector <- function(z) {
  
  if (NROW(z) > 1) z = t(z)
    
  return(z)
  
  #return(matrix(z, nrow = 1)) 
}
