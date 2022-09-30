# ---------------------------------------------
# import data from files in a setup
# this is to support the use of orignal setups
import_from_setup <- function(setup_path) {
  data.files <- list.files(path_data, 
                           pattern = "_C.txt|_phi.txt|_omega.txt|_beta.txt|_D.txt|_Db.txt", 
                           full.names = TRUE)
  for(i in data.files) {
    if(i == data.files[1]) {
      original_data <- setNames(read.table(i), c("z", sub(".*_(.*).txt", "\\1", i)))
    } else {
      original_data <- merge(original_data, 
                             setNames(read.table(i), c("z", sub(".*_(.*).txt", "\\1", i))), 
                             by = "z")
    }
  }
  original_data <- original_data[,c("z", "C", "phi", "omega", "beta","D","Db")]
  return(original_data)
}


