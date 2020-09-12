# Function to find local minima
# It returns that local minimum with the smallest x 

find_local_minimum_with_smallest_x <- function (f = -quot_crit,
                                                x = alpha_ticho
) {
  # ------ estimation of first derivative ----
  f_abl <- abl_1_non_aequi(f, x)
  # -------------------------------------------
  
  signs <- sign(f_abl) # sign() is the same in r and matlab
  sign_old <- signs[1]
  
  counter = 0
  
  for(i in 2:length(signs)) {
    if(!exists("x_min_vec")) {x_min_vec <- numeric(0)}
    if(!exists("index_vec")) {index_vec <- numeric(0)}
    sign_new <- signs[i]
    if(sign_new != sign_old & sign_old == -1) {
      counter <- counter + 1
      x_min_vec[counter] <- x[i] 
      index_vec[counter] <- i
    } 
    sign_old <- sign_new
  } 
  
  if(counter >= 1) { # at least one local minimum was found by the preceding for loop
    x_min <- min(x_min_vec)
    ind <- which.min(x_min_vec)
    # matlab code: "[x_min, ind] = min(x_min_vec)"
    x_min <- max(x_min_vec)
    ind <- which.max(x_min_vec)
    # matlab code:  "[x_min, ind] = max(x_min_vec) "
    ind_min <- index_vec[ind]
    found <- 1 # TRUE
  } else { # no local minimum was found
    x_min <- NaN
    ind_min <- matrix()
    found <- 0 # FALSE
  }
  
  list("x_min" = x_min,
       "ind_min" = ind_min,
       "found" = found)
}

# expected output: [x_min,ind_min,found]