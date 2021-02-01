# Function to read the input Data

function [z_data,C_data,z_c,C_c,omega,D_total,beta,phi,error] = read_input_data_func(setup_name,N_c)
# function for reading the input data from the different files
#

# the right location of the data files
if (isunix == 1) 
    setup_name = [setup_name,'/',setup_name];
else
    setup_name = [setup_name,'\\',setup_name];
end
# -------------------------------------

error = 0.0;

dis_hor_sep
disp('                Reading input data');
disp(' ');

figure(1); clf; hold on;

# ------ read concentrations ----------------
file_in = [setup_name,'_C.txt'];
disp('---------------------------------------');
disp(['reading concentrations from file: ']);
disp(file_in);

[z_data,C_data] = read_data_from_file( file_in );
z_c = linspace(z_data(1),z_data(end),N_c);
#C_c = spline(z_data,C_data,z_c); # spline interpolation
#!!!!!!!! This is important and new !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Linear interpolation to avoid to strong oscillatiosn due to the 
# interpolation process
C_c = interp1(z_data,C_data,z_c,'linear'); # spline interpolation
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subplot(1,5,1); hold on;
plot_data(z_data,C_data,'bo','C [\muM]');
plot_data(z_c,C_c,'r+-','C [\muM]');
# ------------------------------------------

# ---------- porosity ---------------------
file_in = [setup_name,'_phi.txt'];
disp('---------------------------------------');
disp(['reading porosity from file: ']);

subplot(1,3,2);
[z_c,phi,error] = operate_property(file_in,z_c,'\phi');
if error == 1
    return
end
# ----------------------------------------

# ---------- vertikal velocity ---------------------
file_in = [setup_name,'_omega.txt'];
disp('---------------------------------------');
disp(['reading vertical velocity from file: ']);

subplot(1,3,3);
[z_c,omega,error] = operate_property(file_in,z_c,'\omega [cm/s]');
if error == 1
    return
end
# ----------------------------------------

figure(2); clf; hold on;

# ---------- Bioirigation ---------------------
file_in = [setup_name,'_beta.txt'];
disp('---------------------------------------');
disp(['reading bioirigation parameter from file: ']);

subplot(1,3,1);
[z_c,beta,error] = operate_property(file_in,z_c,'\beta [1/s]');
if error == 1
    return
end
# ----------------------------------------

# ---------- D ---------------------
file_in = [setup_name,'_D.txt'];
disp('---------------------------------------');
disp(['reading effective diffusivity from file: '])

subplot(1,3,2);
[z_c,D,error] = operate_property(file_in,z_c,'D [cm^2/s]');
if error == 1
    return
end
# ----------------------------------------

# ---------- D_b ---------------------
file_in = [setup_name,'_Db.txt'];
disp('---------------------------------------');
disp(['reading bioturbation from file: ']);

subplot(1,3,3);
[z_c,D_b,error] = operate_property(file_in,z_c,'D_b [cm^2/s]');
if error == 1
    return
end
# -------------------------------------

dis_hor_sep
disp(' ');

# ------ total diffusivity ----
D_total = D + D_b;
# -----------------------------

return


