# load data
rm(list = ls())
dev.off()
source("user_functions/import_from_setup.R")
setup_path <- "test_case_2_data_delta/"
df <- import_from_setup(setup_path)
head(df)

# get arguments for rec() as dataframe
file <- list.files(setup_path, pattern = "*_recargs.txt", full.names = TRUE)
recargs <- read.csv(file)
recargs

#sapply(list.files("1help_functions/", full.names = T, pattern = ".R$"), source)
# run rec
source("user_functions/rec.R")
test <- rec(original_data = df,
            #recargs = recargs,       # data frame containing values for the input arguments; takes precedent over individual arguments
            N_c = 101,               # Number of computational grid points
            C_water = 25e3,          # Nutrient concentration in water column (only important for irrigation)
            # parameters for Tikhonov regularization
            lambda = 1,              # 'smoothing' parameter lambda
            alpha_min = 8,           # lowest alpha value for Tikhonov regularisation and ratio criterion ( actually log_10(alpha_min) )
            alpha_max = 25,          # largest alpha value for Tikhonov regularisation and ratio criterion ( actually log_10(alpha_max) )
            N_alpha = 301,           # Number of ratio criterion evaluations in the alpha interval, to find the minimum
            # setting the boundary conditions for the nutrient concentration
            bnd_cond_type_z_min = 1, # type of boundary condition at the top: 1: for concentration / 2: for derivative
            bnd_cond_C_z_min = 25e3, # value of nutrient concentration or derivative at top
            bnd_cond_type_z_max = 1, # type of boundary condition at the bottom: 1: for concentration / 2: for derivative
            bnd_cond_C_z_max = 5e3   # value of nutrient concentration or derivative at bottom
            )

# plot results
source("user_functions/plot_rec.R")
plot_rec(test)
plot_rec(test, type = "localmin")
plot_rec(test, type = "input")
#which(test$alpha==test$alpha_opt)
#which.min(-test$Tikhonov_criterium)

# calculate boundary fluxes
source("user_functions/boundary_fluxes.R")
boundary_fluxes(test)
boundary_fluxes(test, z = c(0, 12.1, 30.5, 51), explain = FALSE)

# calculate integrated rates
source("user_functions/integrate_rates.R")
plot(test$output_data$z~test$output_data$rate, 
     ylim = rev(range(test$output_data$z)), ylab = "z", xlab = "rate", type = "l", col = "red")
abline(h = c(0,30))
integrate_rates(test)
integrate_rates(test, 0, 30, explain = FALSE)
