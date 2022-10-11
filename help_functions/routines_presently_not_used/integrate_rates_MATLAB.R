function integrate_rates(R,z_c,setup_name)
% function for integrating the estimated rates
% over a chosen depth interval



% ------ ckeck, if rates have been estimated ------
  disp(' ');
dis_hor_sep;
disp('Integrate estimated consump./produc. rates over a selected depth interval.');

if length(R) < 1
disp('The rates have not been estimated, yet.');
disp('Therefore, an integration over a depth interval is not possible.');
return
end
% -------------------------------------------------    
  
  
  % -------------------------------------------------
  % determine integration boundaries
figure(4); hold on;
subplot(1,2,2);

for i=1:2
[y,z] = ginput(1);
plot([min(R) max(R)],[z z],'k');
[mini,int(i)] =min(abs(z_c-z));
z_int(i)=z;
end
z_int = sort(z_int);  % must be increasing order
% --------------------------------
  
  % -----do the integration-----
  z_int_spl = linspace(z_int(1),z_int(2),5000);
R_int_spl = spline(z_c,R,z_int_spl);

integral = simpson(z_int_spl,R_int_spl);
% ----------------------------
  
  
  % ---- write the data to the output file ---------------------
  write_results_integration(z_int,integral,setup_name);
% ------------------------------------------------------------
  
  return


function write_results_integration(z_int,integral,setup_name)
% This function writes the data to the output file



% ---------- choosing the output file --------------------
  disp('Writing Results of rate integration to output file');
disp(' ');

current_dir = pwd;
if (isunix == 1) 
  setup_name = [current_dir,'/',setup_name,'/',setup_name];
else
  setup_name = [current_dir,'\',setup_name,'\',setup_name];
end

disp('Output File:');
output_file = [setup_name,'_output_rate_integration.txt']
% ---------------------------------------------------------


% ------- writing the data to output file -----------------
fid = fopen(output_file, 'wt');

fprintf(fid, 'Output file for depth integrated rates.  \n');
fprintf(fid, 'The depth integration is done by Simpsons Rule on the equidistant z-grid.  \n');
fprintf(fid, '  \n');
fprintf(fid, '  \n');
fprintf(fid, 'The unit of the integrated rate is:    [nmol/(cm^2 s)] \n');
fprintf(fid, 'Or in related units, if not the standard unit system is used. \n');
fprintf(fid, '  \n');
fprintf(fid, '  \n');
fprintf(fid, ['z_start [cm]                     :  ',num2str(z_int(1),'%7.4e'),' \n']);
fprintf(fid, ['z_end   [cm]                     :  ',num2str(z_int(2),'%7.4e'),' \n']);
fprintf(fid, ['integrated rate  [nmol/(cm^2 s)] :  ',num2str(integral,'%7.4e'),' \n']);

fclose(fid);
% -------------------------------------------------------------

return






function I=simpson(x,f)
% do the integration via Simpsons rule
% It is important that the z-grid is equidistant.

%-------Schrittweite--------------------
h = x(2)-x(1);
%-----------------------------------

%---------Gewichte-------------------
a=[3/8 7/6 23/24];
b=ones(1,length(f)-6);
c=[23/24 7/6 3/8];
gewichte=[a b c];
%----------------------------------

I=h*sum(f.*gewichte);

return