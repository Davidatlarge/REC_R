#--------------------------------------------
function [z,f] = read_data_from_file(input_file)

fid = fopen(input_file, 'r');
ende=0;
i=0;

while ende~=-1
ende=fgetl(fid);

if ende~=-1
i=i+1;
[n1, remain] = strtok(ende);
[n2, remain] = strtok(remain);

z(i) = str2num(n1);
f(i) = str2num(n2);        

end

end
fclose(fid);

return
#--------------------------------------------------