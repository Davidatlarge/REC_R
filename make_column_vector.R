# function to guaranty that z is a column vector afterwards
# ------ make row vector -------
make_column_vector <- function(z) {
   return(matrix(z, ncol = 1))
}
