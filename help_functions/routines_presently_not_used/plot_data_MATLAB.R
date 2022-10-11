# ------------------------------------------------
function plot_data(z,f,style,title_name)

plot(f,z,style);
set(gca,'YDir','reverse');
title(title_name);
ylabel('depth [cm]');
axis tight;


return