write_results <- function(setup_name = setup_name,
                          R_out = R_out,
                          C_out = C_out,
                          z_out = z_c
) {
  cat('Writing Results to output file')
  
  current_dir <- getwd()
  setup_name <- paste(current_dir, setup_name, setup_name, sep = "/")
  output_file <- paste(setup_name, "output.txt", sep = "_")
  
  cat(paste('Output File: \n', output_file))
  
  A <- data.frame("z [cm]" = z_out,
                  "C [muM]" = C_out,
                  "Rates [nmol/(cm^3 s)]" = R_out)
  
  write.table(A, file = output_file)
  # # I think this is only a print of the results to the console
  # for(i in 1:dim(A)[1]) {
  #   fprintf(fid, '%6.3e %6.3e %6.3e\n', A[i,])
  # } 
  print(A)
  
}
