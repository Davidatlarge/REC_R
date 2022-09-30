
# ---- cleaning the memory and open graphics----
rm(list=ls())
graphics.off()
# ----------------------------------------------

# ----- load some packages and helping routines -------
library(matlab)
library(pracma)
library(ggplot2)
library(scales)
source( paste(getwd(), "helping_routines", "AA_list_helping_R_routines.R", sep=.Platform$file.sep) )
source(paste(getwd(), "helping_routines", "import_from_setup.R", sep=.Platform$file.sep))
# ----------------------------------------------------

####################################################
####################### args #######################
####################################################

# information for input data
path_data   <- "test_case_BEIBU/diffusive fluxes/diffusive fluxes by REC/Spring/dry_SYW_NO2/"
#setup_name  <-"dry_SYW_NO2"    # REQUIRED FOR READING IN DATA; REPLACE WITH DIRECT DATA VARIABLE INPUT # Name of setup and name of the data folder

# make one df of input data (now from setup files)
# I place this here because if data is passed to the function then it would be an argument input to the parent function
# this import can also work as an option in a later function so that the user either supplies data or a path to the setup
original_data <- import_from_setup(path_data)

N_c         <- 100   # Number of computational grid points
C_water     <- 0.5   # Nutrient concentration in water column (only important for irrigation)

# parameters for Tikhonov regularization
lambda      <- 3     # 'smoothing' parameter lambda
alpha_min   <- 15    # lowest alpha value for Tikhonov regularisation and ratio criterion ( actually log_10(alpha_min) )
alpha_max   <- 22    # largest alpha value for Tikhonov regularisation and ratio criterion ( actually log_10(alpha_max) )
N_alpha     <- 400   # Number of ratio criterion evaluations in the alpha interval, to find the minimum

# setting the boundary conditions for the nutrient concentration
bnd_cond_type_z_min <-  1    # type of boundary condition at the top: 1: for concentration / 2: for derivative
bnd_cond_C_z_min    <-  0.5  # value of nutrient concentration or derivative at top
bnd_cond_type_z_max <-  1    # type of boundary condition at the bottom: 1: for concentration / 2: for derivative
bnd_cond_C_z_max    <-  0.2  # value of nutrient concentration or derivative at bottom

integrate_rates_afterwards <- FALSE  # if you want to integrate the obtained rate over a choosen interval

# =============== end of parameter input =======================================


# =============== read the input data ==========================================
#input_data <- read_input_data_func(path_data,setup_name,N_c) # ORIGINAL 

# make z_c as done by read_input_data_func()
# the step of making an input_data object can be skipped because it seems input_data is not required anywhere else?!
# are the list elements .$z_c and .$error used anywhere?
z_data     <- original_data$z
z_c        <- matlab::linspace(z_data[1], z_data[length(z_data)], N_c)

C_c        <- operate_property("C", z_c)$f_c
phi        <- operate_property("phi", z_c)$f_c
omega      <- operate_property("omega", z_c)$f_c
beta       <- operate_property("beta", z_c)$f_c
D          <- operate_property("D", z_c)$f_c
D_b        <- operate_property("Db", z_c)$f_c
D_total    <- D + D_b         # total diffusivity
# ==============================================================================



# 
# ####################################################
# ####################### fun start ##################
# ####################################################
# 


# =========== some pre - operations ======================================
# store the boundary conditions into list
bnd_cond <- list( "type_z_min" = bnd_cond_type_z_min,
                  "C_z_min"    = bnd_cond_C_z_min,
                  "type_z_max" = bnd_cond_type_z_max,
                  "C_z_max"    = bnd_cond_C_z_max)

# for Tickhonov regularization
l_0 <- 1
l_1 <- lambda
l_2 <- lambda
alpha_ticho <- matlab::logspace(alpha_min, alpha_max, N_alpha)
# =========================================================================



# ========= Determine Concentration and Rates =============================
con_rate <- calculate_con_rates_lin_sys_tichonov_mean_rate_2(C_water,
                                                             bnd_cond, 
                                                             alpha_ticho,
                                                             l_0,
                                                             l_1,
                                                             l_2,
                                                             z_c,
                                                             C_c,
                                                             omega,
                                                             D_total,
                                                             beta,
                                                             phi)
R_out     <- con_rate$R_out
C_out     <- con_rate$C_out
alpha_opt <- con_rate$alpha_opt
# =========================================================================


# ======= do a simple plot of the rates ==================================
par(mfrow=c(1,2))

plot(C_out, z_c, col="green", type='l',
ylab = 'z', ylim = rev(range(z_c)),
xlab = 'C(z)')

plot(R_out, z_c, col="red", type='l',
ylab ='z', ylim = rev(range(z_c)),
xlab = 'R(z)')

dev.off()
# ========================================================================



# # ======= graphical output of the results ===========
# # printing the figure to the setup folder
# # i.e. saving a file of the plot (jpg)
# # OBSOLETE
# library(tidyverse)
ggplot() +
  geom_point(aes(x = z_c, y = C_c)) +
  geom_line(aes(x = z_c, y = C_out), size = 1, col = "green") +
  coord_flip() + scale_x_reverse(name = "sediment depth") + ylab("rate") + theme_bw()

ggplot() +
  geom_line(aes(x = z_c, y = R_out), size = 1, col = "red") +
  coord_flip() + scale_x_reverse(name = "sediment depth") + ylab("concentration") + theme_bw()

# ========= write to output file ======================
# of setup_name, R_out, C_out, z_c
# OBSOLETE results should be put into an r object, probably a list of results, for direct work in r
# write_results() is defined but obsolete
# =====================================================

# ========= calculate fluxes about the interval interfaces =====
# this calculates fluxes but writes them directly to file
# write_fluxes(setup_name,C_out,z_c,omega,D_total,phi) 
# ===============================================================

# ======== integrate the rate function =======
# this writes the fluxes directly to a file
if(integrate_rates_afterwards) {
  integrate_rates(R_out, z_c, setup_name)  
}
# ============================================

####################################################
####################### fun end ####################
####################################################
