# plot the output of rec
plot_rec <- function(rec_out,
                     type = "results" # c("results", "localmin")
) {
  if(type == "results") { # concentrations ad rates
    
    old <- par()$mfrow
    par(mfrow=c(1,2))
    
    plot(rec_out$output_data$conc, rec_out$output_data$z, 
         col="green", type='l',
         ylab = 'z', ylim = rev(range(rec_out$output_data$z)),
         xlab = 'C(z)', xlim = c(min(c(rec_out$input_data$C, rec_out$output_data$conc)), 
                                 max(c(rec_out$input_data$C, rec_out$output_data$conc))) )
    points(rec_out$input_data$C, rec_out$input_data$z)
    
    plot(rec_out$output_data$rate, rec_out$output_data$z, 
         col="red", type='l',
         ylab ='z', ylim = rev(range(rec_out$output_data$z)),
         xlab = 'R(z)')
    
    par(mfrow=old)
    
  } else if(type == "localmin") { # Tichonov criterion as function of alpha
    
    plot(rec_out$alpha, 
         -rec_out$Tichonov_criterium, # y is neg because the local min in calculate_con_rates_lin_sys_tichonov_mean_rate_2() is found by `which.min(-quot_crit)`
         log = "x", type = "l",
         ylab = "T[alpha]", xlab = "alpha")
    points(rec_out$alpha_opt, -rec_out$Tichonov_criterium[which.min(-rec_out$Tichonov_criterium)], col = "green")
    
  } else {
    stop('type needs to be one of "results" or "localmin"')
  }
}

#plot_rec(rec_out, type = "results")
#plot_rec(rec_out, type = "localmin")