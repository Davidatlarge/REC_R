# plot the output of rec()
plot_rec <- function(rec_out,
                     type = "results" # c("input", "results", "localmin")
) {
  
  if(type == "input") {
    # ---- plot input data ---- 
    old <- par()$mfrow
    par(mfrow = c(2,3))
    for(i in 2:7) {
      plot(rec_out$input_data[,1] ~ rec_out$input_data[,i], 
           ylim = rev(range(rec_out$input_data[,1])), ylab = "z", 
           xlab = colnames(rec_out$input_data)[i])
    }
    par(mfrow = old)
     
  } else if(type == "results") {
    # ---- plot concentrations and rates ----
    old <- par()$mfrow
    par(mfrow = c(1,2))
    plot(rec_out$output_data$z ~ rec_out$output_data$conc, 
         type = "l", col = "green",
         ylim = rev(range(rec_out$output_data$z)), ylab = "z", 
         xlim = range(c(rec_out$output_data$conc, rec_out$input_data$C)),
         xlab = colnames(rec_out$output_data)[2])
    points(rec_out$input_data$C, rec_out$input_data$z)
    plot(rec_out$output_data$z ~ rec_out$output_data$rate, 
         type = "l", col = "red",
         ylim = rev(range(rec_out$output_data$z)), ylab = "z", 
         xlab = colnames(rec_out$output_data)[3])
    par(mfrow = old)
   
  } else if(type == "localmin") {
    # ---- plot Tikhonov criterion as function of alpha ---- 
    old <- par()$mfrow
    par(mfrow = c(1,1))
    plot(rec_out$alpha, 
         -rec_out$Tikhonov_criterium, # y is neg because the local min in calculate_con_rates_lin_sys_Tikhonov_mean_rate_2() is found using `-quot_crit`
         log = "x", type = "l",
         ylab = "T[alpha]", xlab = "alpha")
    points(rec_out$alpha_opt, -rec_out$Tikhonov_criterium[which(rec_out$alpha==rec_out$alpha_opt)], col = "green")
    par(mfrow = old)
    
  } else {
    stop('type needs to be one of "input", "results" or "localmin"')
  }
}
