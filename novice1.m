dj1=load('tequilas.txt');
T=load('dj70.txt');
A=load('2af.txt'); %max=274 nm
B=load('5hmf.txt'); %max=284 nm
C=load('fur.txt'); %max=277 nm
D=load('5mf.txt'); %max= nm
x=T(:,1);
y=T(:,4);

avg_af=mean(A); %promedio
avg_avg=mean(avg_af); %promedio de promedios
avg_af=mean(A,1);

%%%%%%%%%%%%%%%%%%%%%%%% Don Julio 70 %%%%%%%%%%%%%%%%%
 z=1*A(:,3)+1.1*C(:,6)+1.2*D(:,3)+1*B(:,2);
 a1=1*A(:,4);
 a2=1.1*C(:,6);
 a3=1.2*D(:,3);
 a4=1*B(:,2);
%%%%%%%%%%%%%%%%%%%%%%%% Tequila Blanco %%%%%%%%%%%%%%%%%%%
 
% z=1*A(:,3)+1.1*C(:,3)+1.2*D(:,3);
% a1=1*A(:,3);
% a2=1.1*C(:,3);
% a3=1.2*D(:,3);


%subplot(1,2,1)
P1=plot(A(:,1),A(:,3),'b',C(:,1),1.1*C(:,6),'k',D(:,1),1.2*D(:,3),'g',B(:,1),1*B(:,2),'c');

P2=line(x,y,'LineStyle','-','color','k');
P3=line(x,z,'LineStyle','-','color','r');

xlabel('Longitud de onda (nm)')
ylabel('Absorbancia (ua)')
legend('Ajuste')
legend('tequila')
axis([245 325 -0.02 1.5])
set(P1,'linewidth',2);
set(P2,'linewidth',5);
set(P3,'linewidth',5);
figure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subplot(1,2,1) 
P1=plot(A(:,1),A(:,2),'b',A(:,1),A(:,3),'b',A(:,1),A(:,4),'b',A(:,1),A(:,5),'b',...
    A(:,1),A(:,6),'b',A(:,1),A(:,7),'b',A(:,1),A(:,8),'b',A(:,1),A(:,9),'b',...
    A(:,1),A(:,10),'b',A(:,1),A(:,11),'b');
xlabel('Longitud de onda (nm)')
ylabel('Absorbancia (ua)')
legend('2-AF')
axis([245 325 -0.02 1.5])
set(P1,'linewidth',2);




a=zeros(10,1);
for i=1:10
a(i)=find(A(:,i+1)==max(A(:,i+1)));
end
am=round(mean(a));


 for j=2:11
 max(A(:,j));
 end
MaxA=[max(A(:,2)) max(A(:,3)) max(A(:,4)) max(A(:,5)) max(A(:,6))...
    max(A(:,7)) max(A(:,8)) max(A(:,9)) max(A(:,10)) max(A(:,11))]';
C2af=[0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0]';



FIT1=polyfit(C2af,MaxA,1);
CON=0:1.5;
yp=polyval(FIT1,CON);
subplot(1,2,2)
C1=plot(C2af,MaxA,'r+',CON, yp,'k');
xlabel('Concentración mg/100 mL')
ylabel('Absorbancia (ua)')
title('2 acetil furfural')
axis ([ 0 1.2 0 1.2 ])
set(C1,'linewidth',2);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

AT=[max(a1) max(a2) max(a3) max(a4)];
A1=max(a1); A2=max(a2); A3=max(a3); A4=max(a4);

disp('Máximos de absorción de cada componente furanico')
str = fprintf('2AF   %f  (ua)  \n', A1);
str = fprintf('FUR   %f  (ua)  \n', A2);
str = fprintf('5MF   %f  (ua)  \n', A3);
str = fprintf('5hMF   %f  (ua)  \n', A3);


C1=(max(a1)+FIT1(:,2))/FIT1(:,1);
C2=(max(a2)+0.0035)/1.3185;
C3=(max(a3)+0.0108)/1.5725;
C4=(max(a4)+0.0328)/1.6909;

disp('Concentración de cada componente furanico')
str = fprintf('2AF   %f  (mg/100mL)  \n', C1);
str = fprintf('FUR   %f  (mg/100mL)  \n', C2);
str = fprintf('5MF   %f  (mg/100mL)  \n', C3);
str = fprintf('5hMF  %f  (mg/100mL)  \n', C3);

