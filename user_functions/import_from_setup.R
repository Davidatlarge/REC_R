# import data from files in a setup
# this is to support the use of original setups

import_from_setup <- function(setup_path # path to the setup
) {
  data.files <- list.files(setup_path, 
                           pattern = "_C.txt|_phi.txt|_omega.txt|_beta.txt|_D.txt|_Db.txt", 
                           full.names = TRUE)
  for(i in data.files) {
    if(i == data.files[1]) {
      # read the first file
      data <- setNames(read.table(i), c("z", sub(".*_(.*).txt", "\\1", i)))
    } else {
      # add subsequent files
      data <- merge(data, 
                    setNames(read.table(i), c("z", sub(".*_(.*).txt", "\\1", i))), 
                    by = "z")
    }
  }
  # sort the columns (not strictly necessary)
  data <- data[,c("z", "C", "phi", "omega", "beta", "D", "Db")]
  
  return(data)
}


