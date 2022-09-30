# find local minima to index the location of optimal alpha values
find_local_minimum <- function(values, 
                               which.loc.min = "first" # c("first", "lowest")
                               ) {
  
  locmin <- NULL
  for(i in 1:length(values)) {
    if(i == 1 | i == length(values)) {
      locmin[i] <- FALSE # even if the extremes are minima, they are not local minima
    } else { # check if the middle one in a group of 3 values is the lowest, i.e. a local minimum
      locmin[i] <- values[i] == min(values[(i-1):(i+1)])
    }
  }
  
  if(sum(locmin)>0) { # if there is at least one local minimum
    if(which.loc.min == "first") {
      index <- which(locmin)[1] # use the first local minimum
    } else if(which.loc.min == "lowest") {
      index <- which(locmin)[which.min(values[which(locmin)])] # use lowest local minimum
    } else {
      print("which.loc.min must be one of 'first' or 'lowest'")
    }
    #
  } else { # if there is no local minimum
    print("No local minimum of the Tikhonov criterion found. Using absolute minimum. But you can change alpha limits to look for a local minimum.")
    index <- which.min(values)
  }
  
  return(index)
}
