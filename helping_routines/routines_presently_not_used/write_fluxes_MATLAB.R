# ==================================================================
function write_fluxes(setup_name,C_out,z_c,omega,D_total,phi)
# this function calcualtes the total diffusive and advective fluxes
# across the interfaces of the considered depth interval


dis_hor_sep
disp('                Writing diffusive and advective fluxes to output file');
disp(' ');

current_dir = pwd;
if (isunix == 1) 
  setup_name = [current_dir,'/',setup_name,'/',setup_name];
else
  setup_name = [current_dir,'\\',setup_name,'\\',setup_name];
end

disp('Flux-Output File:');
output_file = [setup_name,'_output_fluxes.txt']


# ---- calculate diffusive fluxes ---------
C_out_abl = abl_1(C_out,z_c);

flux_diff_top = - phi(1)*D_total(1)*C_out_abl(1);
flux_diff_bottom = - phi(end)*D_total(end)*C_out_abl(end);
# ------------------------------------------

# ----- calculate advective fluxes ---------
flux_adv_top = phi(1)*omega(1)*C_out(1);
flux_adv_bottom = phi(end)*omega(end)*C_out(end);
# ------------------------------------------

# ------- total flux -----------------------
flux_total_top = flux_diff_top + flux_adv_top;
flux_total_bottom = flux_diff_bottom + flux_adv_bottom;
# --------------------------------------------

# ------ write the data to output file -------
fid = fopen(output_file, 'wt');
fprintf(fid, 'Output file for flux calculations.  \n');
fprintf(fid, 'A flux is positive, if it is directed into positive z-direction (from top to bottom).  \n');
fprintf(fid, '  \n');
fprintf(fid, 'The diffusive flux is defined as: -phi * (D_effective + D_bio) * dC/dz \n');
fprintf(fid, 'The advective flux is defined as: phi * omega * C \n');
fprintf(fid, 'The total flux is the sum of the diffusive and the advective  flux. \n');
fprintf(fid, '  \n');
fprintf(fid, 'All fluxes are given in:    [cm * muM / s] \n');
fprintf(fid, 'Or in related units, if not the standard unit system is used. \n');
fprintf(fid, '  \n');
fprintf(fid, '         diffusive flux    advective flux    total flux \n');
fprintf(fid, '------------------------------------------------------------- \n');
fprintf(fid, ['top    : ',num2str(flux_diff_top,'#7.4e'),'       ',num2str(flux_adv_top,'#7.4e'),'       ',num2str(flux_total_top,'#7.4e'),'\n']);
fprintf(fid, ['bottom : ',num2str(flux_diff_bottom,'#7.4e'),'       ',num2str(flux_adv_bottom,'#7.4e'),'       ',num2str(flux_total_bottom,'#7.4e'),'\n']);

fclose(fid);

return