close all
clear all
clc 
BD=csvread('BDMatlab.csv');

up_long=BD(:,1);
up_lat=BD(:,2);
off_long=BD(:,3);
off_lat=BD(:,4);


plot(up_long,up_lat,'r*')
hold on 
plot(off_long,off_lat,'b*')
xlabel('longitud')
ylabel('latitud')
legend('pick up','drop off')
xlim([-74.5 -73.4])
ylim([40.2,41.3])
hold off


 a=zeros(11);
 
 
 %contamos cuantos puntos de drop off hay en la imagen dividendolos en sectores. 
 for k=1:11
 for j=1:11
 for i=1:length(off_long)
     if  off_long(i,1)<-74.4+.1*(j-1) && off_long(i,1)>-74.5+.1*(j-1)...
             && off_lat(i,1)<41.3-.1*(k-1) && off_lat(i,1)>41.2-.1*(k-1)
         a(k,j)=a(k,j)+1;
     end 
 end 
 end
 end
 
 b=zeros(11);
 %contamos cuantos puntos de drop off hay en la imagen dividendolos en sectores. 
 for k=1:11
 for j=1:11
 for i=1:length(up_long)
     if  up_long(i,1)<-74.4+.1*(j-1) && up_long(i,1)>-74.5+.1*(j-1)...
             && up_lat(i,1)<41.3-.1*(k-1) && up_lat(i,1)>41.2-.1*(k-1)
         b(k,j)=b(k,j)+1;
     end 
 end 
 end
 end
 
 