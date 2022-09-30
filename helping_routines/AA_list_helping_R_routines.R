# This script includes the the helping routines to the search path
#
#
# ------- set Path of helping routines ------------
#sep_str2  <- .Platform$file.sep  # get file separator for operating system
#path_help <- paste(getwd(), "helping_routines", sep=sep_str2)

path_help <- paste(getwd(), "helping_routines", sep=.Platform$file.sep)
# ------------------------------------------------

# --------  add the helping routines -------------
source( paste(path_help, "abl_1_non_aequi.R"                                              , sep=.Platform$file.sep) )
source( paste(path_help, "abl_1.R"                                                        , sep=.Platform$file.sep) )
source( paste(path_help, "make_column_vector.R"                                           , sep=.Platform$file.sep) )
source( paste(path_help, "make_row_vector.R"                                              , sep=.Platform$file.sep) )
source( paste(path_help, "calculate_B_matrix.R"                                           , sep=.Platform$file.sep) )
source( paste(path_help, "find_mean_rate.R"                                               , sep=.Platform$file.sep) )
source( paste(path_help, "determine_con_and_rate.R"                                       , sep=.Platform$file.sep) )
source( paste(path_help, "operate_property.R"                                             , sep=.Platform$file.sep) )
source( paste(path_help, "read_input_data_func.R"                                         , sep=.Platform$file.sep) )
source( paste(path_help, "find_local_minimum_with_smallest_x.R"                           , sep=.Platform$file.sep) )
source( paste(path_help, "calculate_diff_operator_matrix_aequi_dist_grid_variable_coeff.R", sep=.Platform$file.sep) )
source( paste(path_help, "optimal_ticho_parameter_quotientenkriterium.R"                  , sep=.Platform$file.sep) )
source( paste(path_help, "calculate_con_rates_lin_sys_tichonov_mean_rate_2.R"             , sep=.Platform$file.sep) )


# ======= not used presently ==============================
# source( paste(path_help, "integrate_rates_MATLAB.R", sep=.Platform$file.sep) )
# source( paste(path_help, "plot_data_MATLAB.R", sep=.Platform$file.sep) )
# source( paste(path_help, "plot_parameter_kriteria_MATLAB.R", sep=.Platform$file.sep) )
# source( paste(path_help, "write_fluxes_MATLAB.R", sep=.Platform$file.sep) )
# source( paste(path_help, "write_results_OBSOLETE.R", sep=.Platform$file.sep) )


# ======= not used any more ============================
# source( paste(path_help, "operate_property_MATLAB.R", sep=.Platform$file.sep) )
# source( paste(path_help, "read_bnd_cond_structure.R"                                      , sep=.Platform$file.sep) )


