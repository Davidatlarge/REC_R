function write_results(setup_name,R_out,C_out,z_out)

dis_hor_sep
disp('                Writing Results to output file');
disp(' ');

current_dir = pwd;
if (isunix == 1) 
  setup_name = [current_dir,'/',setup_name,'/',setup_name];
else
  setup_name = [current_dir,'\\',setup_name,'\\',setup_name];
end

disp('Output File:');
output_file = [setup_name,'_output.txt']

A(:,1) = make_column_vector(z_out);
A(:,2) = make_column_vector(C_out);
A(:,3) = make_column_vector(R_out);

fid = fopen(output_file, 'wt');
fprintf(fid, 'z [cm]       C [muM]     Rates [nmol/(cm^3 s)]\n');
for i=1:size(A,1)
fprintf(fid, '#6.3e #6.3e #6.3e\n', A(i,:));
end
fclose(fid);

return