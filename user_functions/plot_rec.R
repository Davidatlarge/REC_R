# plot the output of rec
plot_rec <- function(rec_out,
                     type = "results" # c("input", "results", "localmin")
) {
  if(type == "input") {
    
    old <- par()$mfrow
    par(mfrow = c(2,3))
    for(i in 2:7) {
      plot(rec_out$input_data[,1]~rec_out$input_data[,i], 
           ylim = rev(range(rec_out$input_data[,1])), ylab = "z", 
           xlab = colnames(rec_out$input_data)[i])
    }
    par(mfrow=old)
    
  } else if(type == "results") { # concentrations ad rates
    
    old <- par()$mfrow
    par(mfrow=c(1,2))
    for(i in 2:3) {
      plot(rec_out$output_data[,1]~rec_out$output_data[,i], 
           type = "l", col = ifelse(i == 2, "green", "red"),
           ylim = rev(range(rec_out$output_data[,1])), ylab = "z", 
           xlab = colnames(rec_out$output_data)[i])
      if(i==2) points(rec_out$input_data$C, rec_out$input_data$z)
    }
    par(mfrow=old)
    
  } else if(type == "localmin") { # Tichonov criterion as function of alpha
    
    old <- par()$mfrow
    par(mfrow = c(1,1))
    plot(rec_out$alpha, 
         -rec_out$Tichonov_criterium, # y is neg because the local min in calculate_con_rates_lin_sys_tichonov_mean_rate_2() is found by `which.min(-quot_crit)`
         log = "x", type = "l",
         ylab = "T[alpha]", xlab = "alpha")
    points(rec_out$alpha_opt, -rec_out$Tichonov_criterium[which.min(-rec_out$Tichonov_criterium)], col = "green")
    par(mfrow=old)
    
  } else {
    stop('type needs to be one of "input", "results" or "localmin"')
  }
}
