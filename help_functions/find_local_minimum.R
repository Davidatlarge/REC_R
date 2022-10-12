# find local minima to index the location of optimal alpha values
find_local_minimum <- function(values, 
                               which.loc.min = "last" # c("first", "lowest", "last", "all")
) {
  # ---- find local minima ---- 
  diffs <- c(0, sign(diff(values))) # the sign of the difference shows if values are decreasing or increasing
  diff.diffs <- diff(diffs)# at a local min decrease changes to increase and the sign of the difference changes from -1 to 1, so the difference of the sign difference is 2
  
  locmin <- FALSE
  for(i in 2:length(diff.diffs)) {
    locmin[i] <- ifelse(diff.diffs[i]==2, TRUE, FALSE)
  }
  locmin[length(values)] <- FALSE
  
  # ---- define the output index ---- 
  if(sum(locmin)>0) { # if there is at least one local minimum
    if(which.loc.min == "all") { 
      index <- which(locmin) # use all local minima
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
# # v <- c(10,10,10,10,9,8,7,6,5,4,5,6,6,6,7,8,7,6,5,5,5,5,4,3,2,2,2,3,4,5,6,7,7,7,6,5,4,3,2,1,1,1,1,1,1,1,1)
# plot(v, type="l")
# points(find_local_minimum(v, "all"), v[find_local_minimum(v, "all")], pch = 4)
# points(find_local_minimum(v, "last"), v[find_local_minimum(v, "last")], col = "green")
# points(find_local_minimum(v, "first"), v[find_local_minimum(v, "first")], col = "red")
# points(find_local_minimum(v, "lowest"), v[find_local_minimum(v, "lowest")], col = "blue")

