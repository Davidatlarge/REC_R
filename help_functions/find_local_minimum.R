# find local minima to index the location of optimal alpha values
find_local_minimum <- function(values, 
                               which.loc.min = "last" # c("first", "lowest", "last", "all")
                               ) {
  # ---- find local minima ---- 
  locmin <- NULL
  for(i in 1:length(values)) {
    if(i == 1 | i == length(values)) {
      locmin[i] <- FALSE # even if the extremes are minima, they are not local minima
    } else { # check if the middle one in a group of 3 values is the lowest, i.e. a local minimum
      locmin[i] <- values[i] == min(values[(i-1):(i+1)])
    }
  }
  
  # ---- define the index for best alpha ---- 
  if(sum(locmin)>0) { # if there is at least one local minimum
    if(which.loc.min == "all") {
      index <- which(locmin)
    } else if(which.loc.min == "first") {
      index <- which(locmin)[1] # use the first local minimum
    } else if(which.loc.min == "lowest") {
      index <- which(locmin)[which.min(values[which(locmin)])] # use lowest local minimum
    } else if(which.loc.min == "last") {
      index <- which(locmin)[length(which(locmin))] # use the last local minimum
    } else {
      print("which.loc.min must be one of 'first', 'lowest', 'last', or 'all'")
    }
  } else { # if there is no local minimum
    cat("No local minimum of the Tikhonov criterion found. Using absolute minimum. \nBut you can change alpha limits to look for a local minimum.\n")
    index <- which.min(values)
  }
  
  # ---- return ---- 
  return(index)
}

# set.seed(2022)
# v <- runif(20)
# plot(v, type="l")
# points(find_local_minimum(v, "all"), v[find_local_minimum(v, "all")], pch = 4)
# points(find_local_minimum(v), v[find_local_minimum(v)], col = "green")
# points(find_local_minimum(v, "first"), v[find_local_minimum(v, "first")], col = "red")
# points(find_local_minimum(v, "lowest"), v[find_local_minimum(v, "lowest")], col = "blue")

