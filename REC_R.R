## example
# load data
rm(list = ls())
source("user_functions/import_from_setup.R")
df <- import_from_setup("test_case_2_data_delta/")
head(df)

# run rec
source("user_functions/rec.R")
test <- rec(original_data = df,
            N_c = 101,               # Number of computational grid points
            C_water = 25e3,          # Nutrient concentration in water column (only important for irrigation)
            # parameters for Tikhonov regularization
            lambda = 1,              # 'smoothing' parameter lambda
            alpha_min = 8,           # lowest alpha value for Tikhonov regularisation and ratio criterion ( actually log_10(alpha_min) )
            alpha_max = 15,          # largest alpha value for Tikhonov regularisation and ratio criterion ( actually log_10(alpha_max) )
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
#plot_rec(test, type = "input")

# calculate boundary fluxes
source("user_functions/boundary_fluxes.R")
boundary_fluxes(test)

# calculate integrated rates
source("user_functions/integrate_rates.R")
plot(test$output_data$z~test$output_data$rate, 
     ylim = rev(range(test$output_data$z)), ylab = "z", xlab = "rate", type = "l", col = "red")
abline(h = c(0,30))
integrate_rates(test)
integrate_rates(test, 0, 30, explain = FALSE)
