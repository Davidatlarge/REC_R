# Function to read the input Data from the different input files
# The data in these files need not be on the same z-values
# This is achieved by linear interpolation afterwards

read_input_data_func <- function(path_data,setup_name,N_c) {

  print('======== reading the input data ========================')

  # ------ read concentrations --------------------------
  # concentrations have a special treatment
  file_data  <- paste(path_data, setup_name ,"_C.txt", sep="")
  print(' ');
  print('reading concentrations from file: ')
  print(file_data)
  z_data     <- read.table( file_data )[,1]
  z_c        <- matlab::linspace(z_data[1], z_data[length(z_data)], N_c)
  data       <- operate_property(file_data, z_c)
  print(data$fc)
  
  C_c        <- data$f_c
  print(C_c)
  # ----------------------------------------------------
  
  # ---------- porosity --------------------------------
  file_data  <- paste(path_data, setup_name ,"_phi.txt", sep="")
  print(' ');
  print('reading porosity from file: ')
  print(file_data)
  data       <- operate_property(file_data, z_c)
  phi        <- data$f_c
  # ---------------------------------------------------
  
  # ---------- vertical velocity ----------------------
  file_data  <- paste(path_data, setup_name ,"_omega.txt", sep="")
  print(' ');
  print('reading vertical velocity from file: ')
  print(file_data)
  data       <- operate_property(file_data, z_c)
  omega      <- data$f_c
  # ------------------------------------------------- 
  
  # ---------- Bioirigation ------------------------
  file_data  <- paste(path_data, setup_name ,"_beta.txt", sep="")
  print(' ');
  print('reading bioirigation from file: ')
  print(file_data)
  data       <- operate_property(file_data, z_c)
  beta       <- data$f_c
  # ------------------------------------------------
  
  # ---------- D -----------------------------------
  file_data  <- paste(path_data, setup_name ,"_D.txt", sep="")
  print(' ');
  print('reading D from file: ')
  print(file_data)
  data       <- operate_property(file_data, z_c)
  D          <- data$f_c
  # ------------------------------------------------
  
  # ---------- D_b ---------------------------------
  file_data  <- paste(path_data, setup_name ,"_Db.txt", sep="")
  print(' ');
  print('reading D_b from file: ')
  print(file_data)
  data       <- operate_property(file_data, z_c)
  D_b        <- data$f_c
  # -------------------------------------------------
  
  # ------- return the data ------------------------
  input_data <- list("z_c"  = z_c,
                     "C_c"  = C_c,
                     "phi"  = phi,
                     "omega"= omega,
                     "beta" = beta,
                     "D"    = D,
                     "D_b"  = D_b)
  
  return(input_data)
  # ------------------------------------------------
}

# expected output: [z_c,C_c,phi,omega,beta,D,D_b]
