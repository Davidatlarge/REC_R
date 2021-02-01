# ---------------------------------------------
function [z_c,f_c,error] = operate_property(file_name_in,z_c,title_name)

error = 0;

disp(file_name_in);

[z_data,f_data] = read_data_from_file(file_name_in);

if ( z_data(1) == z_c(1) ) & ( z_data(end) == z_c(end) )

z_data = make_column_vector(z_data);
f_data = make_column_vector(f_data);
z_c = make_column_vector(z_c);

f_c = interp1(z_data,f_data,z_c,'linear');  

hold on;
plot_data(z_data,f_data,'bo',title_name);
plot_data(z_c,f_c,'r+-',title_name);

f_max = max(f_data);  # Axis limits
f_min = min(f_data);     
if (f_max == 0) & (f_min == 0)
f_min = -1e-12;
f_max =  1e-12;         
end
set(gca,'xlim',[0.95*f_min 1.05*f_max]);

error = 0.0;
disp('data are correct');


else
  
  disp('The z-coordinate at the end points do not match')
disp('the z-coordinates of the concentration data')

error = 1.0;
return
end

return
# ----------------------------------------------
