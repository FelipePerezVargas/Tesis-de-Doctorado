%%%%%% Programa para concentraciones de furfurales %%%%%

clc
clear all
%%%%%%%%%%%%%%%%%%%%%%%%% Muestras y componentes %%%%%%%%%%%%%%%%%%%%%%%%%
A=load('5mf.txt'); %max=274 nm
B=load('fur.txt'); %max=284 nm
C=load('5hmf.txt'); %max=277 nm
D=load('2af.txt'); %max= 292 nm

%%%%%%%%%%%%%%%%%%%%%%%%%% Concentraciones %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Co=[0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0]';
w=[ones(size(Co)) Co]; %X
Mw=w'*w; %X'*X

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Ruido %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

z=A(end,2:end);
ywA=w'*z';
rA=Mw^-1*ywA;
ruA=rA(1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% A=>2af %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for n=2:11
A(:,n)=smooth(A(:,n)); %y
end

subplot(1,2,1) 
 P1=plot(A(:,1),A(:,2)-ruA,'b',A(:,1),A(:,3)-ruA,'b',A(:,1),A(:,4)-ruA,'b',...
     A(:,1),A(:,5)-ruA,'b',A(:,1),A(:,6)-ruA,'b',A(:,1),A(:,7)-ruA,'b',...
     A(:,1),A(:,8)-ruA,'b',A(:,1),A(:,9)-ruA,'b',A(:,1),A(:,10)-ruA,'b',...
     A(:,1),A(:,11)-ruA,'b');
 xlabel('Longitud de onda (nm)')
 ylabel('Absorbancia (ua)')
 legend('2-AF');
 title('2 Acetil Furan')
 %gtext('a)')
 axis([245 325 0 1.5])
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


subplot(1,2,2)
 FIT1=polyfit(C2af,MaxA,1);
 CON=0:1.5;
 yp=polyval(FIT1,CON);
 subplot(1,2,2)
 C1=plot(C2af,MaxA,'r+',CON, yp,'k');
 xlabel('Concentración mg/100 mL')
 ylabel('Absorbancia (ua)')
 title('2 acetil furfural')
 legend('Concentraciones','Ajuste')
 %gtext('b)')
 axis([0 1.2 0 1.2])
 set(C1,'linewidth',2);
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% B=>5hmf %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Ruido %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%figure

z1=B(end,2:end);
ywB=w'*z1';
rB=Mw^-1*ywB;
ruB=rB(1);


for n=2:11
B(:,n)=smooth(B(:,n)); %y
end

subplot(1,2,1) 
 P2=plot(B(:,1),B(:,2)-ruB,'b',B(:,1),B(:,3)-ruB,'b',B(:,1),B(:,4)-ruB,'b',...
     B(:,1),B(:,5)-ruB,'b',B(:,1),B(:,6)-ruB,'b',B(:,1),B(:,7)-ruB,'b',...
     B(:,1),B(:,8)-ruB,'b',B(:,1),B(:,9)-ruB,'b',B(:,1),B(:,10)-ruB,'b',...
     B(:,1),B(:,11)-ruB,'b');
 xlabel('Longitud de onda (nm)')
 ylabel('Absorbancia (ua)')
 legend('5-HMF');
 title('2 Hidroximetil furfural')
 %gtext('a)')
 axis([245 325 0 1.5])
 set(P2,'linewidth',2);

 b=zeros(10,1);
for i=1:10
b(i)=find(B(:,i+1)==max(B(:,i+1)));
end
bm=round(mean(b));

for j=2:11
 max(B(:,j));
 end
MaxB=[max(B(:,2)) max(B(:,3)) max(B(:,4)) max(B(:,5)) max(B(:,6))...
    max(B(:,7)) max(B(:,8)) max(B(:,9)) max(B(:,10)) max(B(:,11))]';
C5hmf=[0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0]';


subplot(1,2,2)
 FIT2=polyfit(C5hmf,MaxB,1);
 CON=0:1.5;
 yp=polyval(FIT2,CON);
 subplot(1,2,2)
 C2=plot(C5hmf,MaxB,'r+',CON, yp,'k');
 xlabel('Concentración mg/100 mL')
 ylabel('Absorbancia (ua)')
 title('5 hidroximetil furfural')
 legend('Concentraciones','Ajuste')
 %gtext('b)')
 axis([0 1.2 0 1.2])
 set(C2,'linewidth',2);

 
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% C=>Furfural %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Ruido %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%figure

z2=C(end,2:end);
ywC=w'*z2';
rC=Mw^-1*ywC;
ruC=rC(1);


for n=2:11
C(:,n)=smooth(C(:,n)); %y
end

subplot(1,2,1) 
 P3=plot(C(:,1),C(:,2)-ruC,'b',C(:,1),C(:,3)-ruC,'b',C(:,1),C(:,4)-ruC,'b',...
     C(:,1),C(:,5)-ruC,'b',C(:,1),C(:,6)-ruC,'b',C(:,1),C(:,7)-ruC,'b',...
     C(:,1),C(:,8)-ruC,'b',C(:,1),C(:,9)-ruC,'b',C(:,1),C(:,10)-ruC,'b',...
     C(:,1),C(:,11)-ruC,'b');
 xlabel('Longitud de onda (nm)')
 ylabel('Absorbancia (ua)')
 legend('Furfural');
 title('Furfural')
 %gtext('a)')
 axis([245 325 0 1.5])
 set(P3,'linewidth',2);

 c=zeros(10,1);
for i=1:10
c(i)=find(C(:,i+1)==max(C(:,i+1)));
end
cm=round(mean(c));

for j=2:11
 max(C(:,j));
 end
MaxC=[max(C(:,2)) max(C(:,3)) max(C(:,4)) max(C(:,5)) max(C(:,6))...
    max(C(:,7)) max(C(:,8)) max(C(:,9)) max(C(:,10)) max(C(:,11))]';
Cfur=[0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0]';


subplot(1,2,2)
 FIT3=polyfit(Cfur,MaxC,1);
 CON=0:1.5;
 yp=polyval(FIT3,CON);
 subplot(1,2,2)
 C3=plot(Cfur,MaxC,'r+',CON, yp,'k');
 xlabel('Concentración mg/100 mL')
 ylabel('Absorbancia (ua)')
 title('Furfural')
 legend('Concentraciones','Ajuste')
 %gtext('b)')
 axis([0 1.2 0 1.2])
 set(C3,'linewidth',2);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Ruido %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

z=A(end,2:end);
ywA=w'*z';
rA=Mw^-1*ywA;
ruA=rA(1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% A=>2af %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for n=2:11
A(:,n)=smooth(A(:,n)); %y
end

subplot(1,2,1) 
 P1=plot(A(:,1),A(:,2)-ruA,'b',A(:,1),A(:,3)-ruA,'b',A(:,1),A(:,4)-ruA,'b',...
     A(:,1),A(:,5)-ruA,'b',A(:,1),A(:,6)-ruA,'b',A(:,1),A(:,7)-ruA,'b',...
     A(:,1),A(:,8)-ruA,'b',A(:,1),A(:,9)-ruA,'b',A(:,1),A(:,10)-ruA,'b',...
     A(:,1),A(:,11)-ruA,'b');
 xlabel('Longitud de onda (nm)')
 ylabel('Absorbancia (ua)')
 legend('Furfural');
 title('Furfural')
 %gtext('a)')
 axis([245 325 0 1.5])
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


subplot(1,2,2)
 FIT1=polyfit(C2af,MaxA,1);
 CON=0:1.5;
 yp=polyval(FIT1,CON);
 subplot(1,2,2)
 C1=plot(C2af,MaxA,'r+',CON, yp,'k');
 xlabel('Concentración mg/100 mL')
 ylabel('Absorbancia (ua)')
 title('Furfural')
 legend('Concentraciones','Ajuste')
 %gtext('b)')
 axis([0 1.2 0 1.2])
 set(C1,'linewidth',2);
 
 
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% C=>5 Metil Furfural %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Ruido %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%figure

z3=D(end,2:end);
ywD=w'*z3';
rD=Mw^-1*ywD;
ruD=rD(1);


for n=2:11
D(:,n)=smooth(D(:,n)); %y
end

subplot(1,2,1) 
 P4=plot(D(:,1),D(:,2)-ruD,'b',D(:,1),D(:,3)-ruD,'b',D(:,1),D(:,4)-ruD,'b',...
     D(:,1),D(:,5)-ruD,'b',D(:,1),D(:,6)-ruD,'b',D(:,1),D(:,7)-ruD,'b',...
     D(:,1),D(:,8)-ruD,'b',D(:,1),D(:,9)-ruD,'b',D(:,1),D(:,10)-ruD,'b',...
     D(:,1),D(:,11)-ruD,'b');
 xlabel('Longitud de onda (nm)')
 ylabel('Absorbancia (ua)')
 legend('5 Metil Furfural');
 title('5 Metil Furfural')
 %gtext('a)')
 axis([245 325 0 1.5])
 set(P4,'linewidth',2);

 c=zeros(10,1);
for i=1:10
d(i)=find(D(:,i+1)==max(D(:,i+1)));
end
dm=round(mean(d));

for j=2:11
 max(D(:,j));
 end
MaxD=[max(D(:,2)) max(D(:,3)) max(D(:,4)) max(D(:,5)) max(D(:,6))...
    max(D(:,7)) max(D(:,8)) max(D(:,9)) max(D(:,10)) max(D(:,11))]';
D5mf=[0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0]';


subplot(1,2,2)
 FIT4=polyfit(D5mf,MaxD,1);
 CON=0:1.5;
 yp=polyval(FIT4,CON);
 subplot(1,2,2)
 C4=plot(D5mf,MaxD,'r+',CON, yp,'k');
 xlabel('Concentración mg/100 mL')
 ylabel('Absorbancia (ua)')
 title('5 Metil Furfural')
 legend('Concentraciones','Ajuste')
 %gtext('b)')
 axis([0 1.2 0 1.2])
 set(C4,'linewidth',2);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% Coeficientes de extinción %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%% 2 af %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%figure

A(:,2:end)=A(:,2:end)-ruA;
epA=A(:,2:end)*Co./(Co'*Co); %(X'*X)^-1*(y'*X)
%epA1=(Co'*Co)^-1*A(:,2:end)*Co;
C2af=plot(A(:,1),epA,'g');
xlabel('Longitud de onda (nm)')
ylabel('Absortividad molar')
title('2 Acetil-Furan')
legend('Coeficiente de absorción 2AF')
axis ([ 250 325 0 1.5 ])
set(C2af,'linewidth',2);


%%%%%%%%%%%%%%%%%%%%%%%%%%%% 5hmf %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%figure

B(:,2:end)=B(:,2:end)-ruB;
epB=B(:,2:end)*Co./(Co'*Co); %(X'*X)^-1*(y'*X)
%epA1=(Co'*Co)^-1*A(:,2:end)*Co;
C5hmf=plot(B(:,1),epB,'g');
xlabel('Longitud de onda (nm)')
ylabel('Absortividad molar')
title('2 Acetil-Furan')
legend('Coeficiente de absorción 2AF')
axis ([ 250 325 0 1.5 ])
set(C5hmf,'linewidth',2);

%%%%%%%%%%%%%%%%%%%%%%%%%%%% furfural %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%figure

C(:,2:end)=C(:,2:end)-ruC;
epC=C(:,2:end)*Co./(Co'*Co); %(X'*X)^-1*(y'*X)
%epA1=(Co'*Co)^-1*A(:,2:end)*Co;
Cfur=plot(C(:,1),epC,'g');
xlabel('Longitud de onda (nm)')
ylabel('Absortividad molar')
title('2 Acetil-Furan')
legend('Coeficiente de absorción 2AF')
axis ([ 250 325 0 1.5 ])
set(Cfur,'linewidth',2);


%%%%%%%%%%%%%%%%%%%%%%%%%%%% 5 MF %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%figure

D(:,2:end)=D(:,2:end)-ruD;
epD=D(:,2:end)*Co./(Co'*Co); %(X'*X)^-1*(y'*X)
%epA1=(Co'*Co)^-1*A(:,2:end)*Co;
C5mf=plot(D(:,1),epD,'g');
xlabel('Longitud de onda (nm)')
ylabel('Absortividad molar')
title('2 Acetil-Furan')
legend('Coeficiente de absorción 2AF')
axis ([ 250 325 0 1.5 ])
set(C5mf,'linewidth',2);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%% Espectros de absorción %%%%%%%%%%%%%%%%%%%%%%%%%%%%

%figure

TO=plot(A(:,1),epA,'g',A(:,1),epB,'c',A(:,1),epC,'k',A(:,1),epD,'b');
xlabel('Longitud de onda (nm)')
ylabel('Absorbancia (ua)')
title('Coeficientes de absorción')
legend('2AF','5HMF', 'FUR','5MF')
axis([245 325 0 1.8])
set(TO,'linewidth',2);

%%%%%%%%%%%%%%% encuentra máximos %%%%%%%%%%%%%%%%%%%%%%%
nA=find(epA==max(epA));
nB=find(epB==max(epB));
nC=find(epC==max(epC));
nD=find(epD==max(epD)); 
lA=A(nA,1);
lB=B(nB,1);
lC=C(nC,1);
lD=D(nD,1);
MAF=[lA lB lC lD];

disp('Máximos de absorción de cada componente furanico')
str = fprintf('2AF   %3.0f  (nm)  \n', lA);
str = fprintf('5HMF  %3.0f  (nm)  \n', lB);
str = fprintf('FUR   %3.0f  (nm)  \n', lC);
str = fprintf('5MF  %3.0f  (nm)  \n', lD);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% Muestras de Tequila %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

figure 
T=load('teq190515.txt');
% zT=T(end,2:end);
% ywT=w'*zT';
% rT=Mw^-1*ywT;
% ruT=rT(1);
% 
% plot(T(:,1),T(:,2)-ruT,T(:,1),T(:,3)-ruT,T(:,1),Ts(:,2)-ruT,T(:,1),Ts(:,3)-ruT);

for n=2:2
Ts(:,n)=smooth(T(:,n));
nT(:,n)=find(T(:,n)==max(T(:,n)));
TT(:,n)=T(nT(:,n),1);
end

f1=1;
R1=abs(epC(1:find(epC==max(epC)))-max(epC)*f1);
li=find(R1==min(R1));


f2=0.0;
R2=abs(epD(find(epD==max(epD)):end)-max(epD)*f2);
lf=find(R2==min(R2))+find(epD==max(epD))-1;


for n=2:2
Te=T(:,n);
%Te=T(:,n);
epA2=epA(li:lf);
epB2=epB(li:lf);
epC2=epC(li:lf);
epD2=epD(li:lf);
T1=Te(li:lf);

x=[ones(size(epC)) epA epB epC epD];
X=[ones(size(epC2)) epA2 epB2 epC2 epD2];
M=X'*X;
FY=X'*T1;
CT1=M^-1*FY
rt=x*CT1;



%figure 

 FIN=plot(T(:,1),Te-CT1(1).*ones(size(epC)),'--k',T(:,1),rt-CT1(1).*ones(size(epC)),':r',...
     T(:,1),CT1(2).*epA,'g',T(:,1),CT1(3).*epB,'c',...
     T(:,1),CT1(4).*epC,'k',T(:,1),CT1(5).*epD,'b');
 xlabel('Longitud de onda (nm)')
 ylabel('Absorbancia (ua)')
 legend('Tequila','Ajuste','2AF','5HMF', 'FUR','5MF')
 title('Muestra de Tequila')
 axis([245 325 0 2])
 set(FIN,'linewidth',2);
% 
% 
 AT=[max(CT1(2)) max(CT1(3)) max(CT1(4)) max(CT1(5))];
 A1=max(CT1(2)); A2=max(CT1(3)); A3=max(CT1(4)); A4=max(CT1(5));
% 
disp('Máximos de absorción de cada componente furanico')
str = fprintf('2AF   %f  (ua)  \n', A1);
str = fprintf('5HMF   %f  (ua)  \n', A2);
str = fprintf('FUR   %f  (ua)  \n', A3);
str = fprintf('5MF   %f  (ua)  \n', A4);

C1=(max(CT1(2))+FIT1(:,2))/FIT1(:,1);
C2=(max(CT1(3))+FIT2(:,2))/FIT2(:,1);
C3=(max(CT1(4))+FIT3(:,2))/FIT3(:,1);
C4=(max(CT1(5))+FIT4(:,2))/FIT4(:,1);
% 
disp('Concentración de cada componente furanico')
str = fprintf('2AF   %f  (mg/mL)  \n', C1);
str = fprintf('5HMF   %f  (mg/mL)  \n', C2);
str = fprintf('FUR   %f  (mg/mL)  \n', C3);
str = fprintf('5MF  %f  (mg/mL)  \n', C4);
% 
figure
dk=Te(li:lf)-rt(li:lf);
dc=Te(:,1)-rt;
S2=sum(dk.^2);
subplot(1,2,2)
F2=plot(T(li:lf,1),dk,'-k');
xlabel('Longitud de onda (nm)')
ylabel('Absorbancia (ua)')
legend('Residuales de Ajuste ancho de banda')
set(F2,'linewidth',2);
subplot(1,2,1)
F3=plot(T(:,1),dc,'-k');
xlabel('Longitud de onda (nm)')
ylabel('Absorbancia (ua)')
legend('Residuales de Ajuste completo')
set(F3,'linewidth',2);
end

