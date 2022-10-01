# ---------------------------------------------------
 plot_parameter_kriteria <- function(alpha_ticho,
                                     quot_crit,
                                     index
                                     ) {
   figure(3); clf; hold on;

set(gca,'Fontsize',14);
plot(alpha_ticho,-quot_crit,'k-','Linewidth',2);
plot(alpha_ticho(index),-quot_crit(index),'ko','Linewidth',2);
#set(gca,'xscale','log','yscale','log');
set(gca,'xscale','log');
ylabel('T(\alpha)');
xlabel('\alpha');
box on;
title('ratio criterion for finding the optimal \alpha');
axis tight;
# ---------------------------------------------------

pause(1);

return
 }

