# function to guaranty that z is a row vector afterwards
# ------ make row vector -------
make_row_vector <- function(z) {
   matrix(z, nrow = 1)
}
