# function to read the boundary conditions structure
read_bnd_cond_structure <- function(
  bnd_cond
) {
  type_z_min <- bnd_cond.type_z_min
  type_z_max <- bnd_cond.type_z_max
  C_z_min <- bnd_cond.C_z_min
  C_z_max <- bnd_cond.C_z_max
  list("C_z_min" = C_z_min, 
       "C_z_max" = C_z_max, 
       "type_z_min" = type_z_min, 
       "type_z_max" = type_z_max)
}
# expected ouput: C_z_min, C_z_max, type_z_min, type_z_max

