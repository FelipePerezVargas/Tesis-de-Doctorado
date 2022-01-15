%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% Programa para Concentraciones de Compuestos %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% Autor: Felipe P�rez Vargas %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% Universidad de Guanajuato %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clc
clear all
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Compuestos                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
A=load('5mf.txt'); %max=274 nm                                           %%   
B=load('fur.txt'); %max=284 nm                                           %%
C=load('5hmf.txt'); %max=277 nm                                          %% 
D=load('2af.txt'); %max= 292 nm                                          %%
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%% Concentraciones %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
Co=[0.001 0.002 0.003 0.004 0.005 0.006 0.007 0.008 0.009 0.01]';        %%
w=[ones(size(Co)) Co]; %X                                                %%
Mw=w'*w; %X'*X                                                           %%
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        Primer Compuesto                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Ruido %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
z1=A(end,2:end);                                                         %%
ywA=w'*z1';                                                              %%
rA=Mw^-1*ywA;                                                            %%
ruA=rA(1);                                                               %%
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Suavizado %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
for n=2:11                                                               %%
A(:,n)=smooth(A(:,n)); %y                                                %% 
end                                                                      %% 
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%% Espectros de Absorci�n %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
subplot(1,2,1)                                                           %% 
P1=plot(A(:,1),A(:,2)-ruA,'b',A(:,1),A(:,3)-ruA,'b',...                  %%
    A(:,1),A(:,4)-ruA,'b', A(:,1),A(:,5)-ruA,'b',...                     %%
    A(:,1),A(:,6)-ruA,'b',A(:,1),A(:,7)-ruA,'b',...                      %%
     A(:,1),A(:,8)-ruA,'b',A(:,1),A(:,9)-ruA,'b',...                     %%
     A(:,1),A(:,10)-ruA,'b',A(:,1),A(:,11)-ruA,'b');                     %%  
 xlabel('Longitud de onda (nm)')                                         %%
 ylabel('Absorbancia (ua)')                                              %%
 legend('5-MF');                                                         %%
 title('5 Metil Furfural')                                               %%
 axis([245 325 0 1.7])                                                   %%
 set(P1,'linewidth',2);                                                  %%
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         Curva de Calibraci�n                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
 a=zeros(10,1);                                                          %%
for i=1:10                                                               %%
a(i)=find(A(:,i+1)==max(A(:,i+1)));                                      %%
end                                                                      %%
am=round(mean(a));                                                       %%
%                                                                        %%
for j=2:11                                                               %%
 max(A(:,j));                                                            %%
end                                                                      %%
MaxA=[max(A(:,2)) max(A(:,3)) max(A(:,4)) max(A(:,5)) max(A(:,6))...     %%
    max(A(:,7)) max(A(:,8)) max(A(:,9)) max(A(:,10)) max(A(:,11))]';     %%
C5mf=[0.001 0.002 0.003 0.004 0.005 0.006 0.007 0.008 0.009 0.01]';      %%
%                                                                        %%
%                                                                        %%
subplot(1,2,2)                                                           %% 
 FIT1=polyfit(C5mf,MaxA,1);                                              %%
 CON=0:0.001:0.01;                                                       %%
 yp=polyval(FIT1,CON);                                                   %%
 subplot(1,2,2)                                                          %%
 C1=plot(C5mf,MaxA,'r+',CON, yp,'k');                                    %%
 xlabel('Concentraci�n mg/100 mL')                                       %%
 ylabel('Absorbancia (ua)')                                              %% 
 title('5 Metil Furan')                                                  %% 
 legend('Concentraciones','Ajuste')                                      %%
 axis([0 0.01 0 1.7])                                                    %%
 set(C1,'linewidth',2);                                                  %%
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              Coeficiente de extinci�n molar  2AF                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%                                                                        %% 
figure                                                                   %%
A(:,2:end)=A(:,2:end)-ruA;                                               %%
epA=A(:,2:end)*Co./(Co'*Co); %(X'*X)^-1*(y'*X)                           %%
%epA1=(Co'*Co)^-1*A(:,2:end)*Co;                                         %%
C2af=plot(A(:,1),epA,'g');                                               %%
xlabel('Longitud de onda (nm)')                                          %%
ylabel('Absortividad molar')                                             %%  
title('5 Metil-Furan')                                                   %%
legend('Coeficiente de absorci�n 5MF')                                   %%
axis ([ 250 325 0 190 ])                                                 %% 
set(C2af,'linewidth',2);                                                 %%
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        Segundo Compuesto                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Ruido %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
z2=B(end,2:end);                                                         %%
ywB=w'*z2';                                                              %%
rB=Mw^-1*ywB;                                                            %%
ruB=rB(1);                                                               %%
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Suavizado %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
for n=2:11                                                               %%
B(:,n)=smooth(B(:,n)); %y                                                %% 
end                                                                      %% 
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%% Espectros de Absorci�n %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
figure                                                                   %%
subplot(1,2,1)                                                           %% 
P2=plot(B(:,1),B(:,2)-ruB,'b',B(:,1),B(:,3)-ruB,'b',...                  %%
    B(:,1),B(:,4)-ruB,'b', B(:,1),B(:,5)-ruB,'b',...                     %%
    B(:,1),B(:,6)-ruB,'b',B(:,1),B(:,7)-ruB,'b',...                      %%
    B(:,1),B(:,8)-ruB,'b',B(:,1),B(:,9)-ruB,'b',...                      %%
    B(:,1),B(:,10)-ruB,'b',B(:,1),B(:,11)-ruB,'b');                      %%  
 xlabel('Longitud de onda (nm)')                                         %%
 ylabel('Absorbancia (ua)')                                              %%
 legend('Furfural');                                                     %%
 title('Furural')                                                        %%
 axis([245 325 0 1.7])                                                   %%
 set(P2,'linewidth',2);                                                  %%
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         Curva de Calibraci�n                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
 b=zeros(10,1);                                                          %%
for i=1:10                                                               %%
b(i)=find(B(:,i+1)==max(B(:,i+1)));                                      %%
end                                                                      %%
bm=round(mean(b));                                                       %%
%                                                                        %%
for j=2:11                                                               %%
 max(B(:,j));                                                            %%
end                                                                      %%
MaxB=[max(B(:,2)) max(B(:,3)) max(B(:,4)) max(B(:,5)) max(B(:,6))...     %%
    max(B(:,7)) max(B(:,8)) max(B(:,9)) max(B(:,10)) max(B(:,11))]';     %%
Cfur=[0.001 0.002 0.003 0.004 0.005 0.006 0.007 0.008 0.009 0.01]';      %%
%                                                                        %%
%                                                                        %%
subplot(1,2,2)                                                           %% 
 FIT2=polyfit(Cfur,MaxB,1);                                              %%
 CON=0:0.001:0.01;                                                       %%
 yp=polyval(FIT2,CON);                                                   %%
 subplot(1,2,2)                                                          %%
 C2=plot(Cfur,MaxB,'r+',CON, yp,'k');                                    %%
 xlabel('Concentraci�n mg/100 mL')                                       %%
 ylabel('Absorbancia (ua)')                                              %% 
 title('Furfural')                                                       %% 
 legend('Concentraciones','Ajuste')                                      %%
 axis([0 0.01 0 1.7])                                                    %%
 set(C2,'linewidth',2);                                                  %%
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              Coeficiente de extinci�n molar  2AF                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%                                                                        %% 
figure                                                                   %%
B(:,2:end)=B(:,2:end)-ruB;                                               %%
epB=B(:,2:end)*Co./(Co'*Co); %(X'*X)^-1*(y'*X)                           %%
%epA1=(Co'*Co)^-1*A(:,2:end)*Co;                                         %%
Cfur=plot(B(:,1),epB,'g');                                               %%
xlabel('Longitud de onda (nm)')                                          %%
ylabel('Absortividad molar')                                             %%  
title('Furfural')                                                        %%
legend('Coeficiente de absorci�n Fur')                                   %%
axis ([ 250 325 0 190 ])                                                 %% 
set(Cfur,'linewidth',2);                                                 %%
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          Tercer Compuesto                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Ruido %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
z3=C(end,2:end);                                                         %%
ywC=w'*z3';                                                              %%
rC=Mw^-1*ywC;                                                            %%
ruC=rC(1);                                                               %%
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Suavizado %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
for n=2:11                                                               %%
C(:,n)=smooth(C(:,n)); %y                                                %% 
end                                                                      %% 
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%% Espectros de Absorci�n %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
figure                                                                   %%
subplot(1,2,1)                                                           %% 
P3=plot(C(:,1),C(:,2)-ruC,'b',C(:,1),C(:,3)-ruC,'b',...                  %%
    C(:,1),C(:,4)-ruC,'b', C(:,1),C(:,5)-ruC,'b',...                     %%
    C(:,1),C(:,6)-ruC,'b',C(:,1),C(:,7)-ruC,'b',...                      %%
    C(:,1),C(:,8)-ruC,'b',C(:,1),C(:,9)-ruC,'b',...                      %%
    C(:,1),C(:,10)-ruC,'b',C(:,1),C(:,11)-ruC,'b');                      %%  
 xlabel('Longitud de onda (nm)')                                         %%
 ylabel('Absorbancia (ua)')                                              %%
 legend('5 Hidroximetil Furfural');                                      %%
 title('5 Hidrocimetil Furural')                                         %%
 axis([245 325 0 1.7])                                                   %%
 set(P3,'linewidth',2);                                                  %%
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         Curva de Calibraci�n                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
 c=zeros(10,1);                                                          %%
for i=1:10                                                               %%
c(i)=find(C(:,i+1)==max(C(:,i+1)));                                      %%
end                                                                      %%
cm=round(mean(c));                                                       %%
%                                                                        %%
for j=2:11                                                               %%
 max(C(:,j));                                                            %%
end                                                                      %%
MaxC=[max(C(:,2)) max(C(:,3)) max(C(:,4)) max(C(:,5)) max(C(:,6))...     %%
      max(C(:,7)) max(C(:,8)) max(C(:,9)) max(C(:,10)) max(C(:,11))]';   %%
C5hmf=[0.001 0.002 0.003 0.004 0.005 0.006 0.007 0.008 0.009 0.01]';     %%
%                                                                        %%
%                                                                        %%
subplot(1,2,2)                                                           %% 
 FIT3=polyfit(C5hmf,MaxC,1);                                             %%
 CON=0:0.001:0.01;                                                       %%
 yp=polyval(FIT3,CON);                                                   %%
 subplot(1,2,2)                                                          %%
 C3=plot(C5hmf,MaxC,'r+',CON, yp,'k');                                   %%
 xlabel('Concentraci�n mg/100 mL')                                       %%
 ylabel('Absorbancia (ua)')                                              %% 
 title('5 Hidroximetil Furfural')                                        %% 
 legend('Concentraciones','Ajuste')                                      %%
 axis([0 0.01 0 1.7])                                                    %%
 set(C3,'linewidth',2);                                                  %%
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                 Coeficiente de extinci�n molar                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%                                                                        %% 
figure                                                                   %%
C(:,2:end)=C(:,2:end)-ruC;                                               %%
epC=C(:,2:end)*Co./(Co'*Co); %(X'*X)^-1*(y'*X)                           %%
%epA1=(Co'*Co)^-1*A(:,2:end)*Co;                                         %%
C5hmf=plot(C(:,1),epC,'g');                                              %%
xlabel('Longitud de onda (nm)')                                          %%
ylabel('Absortividad molar')                                             %%  
title('5 Hidroximetil Furfural')                                         %%
legend('Coeficiente de absorci�n 5HMF')                                  %%
axis ([ 250 325 0 190 ])                                                 %% 
set(C5hmf,'linewidth',2);                                                 %%
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          Cuarto Compuesto                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Ruido %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
z4=D(end,2:end);                                                         %%
ywD=w'*z4';                                                              %%
rD=Mw^-1*ywD;                                                            %%
ruD=rD(1);                                                               %%
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Suavizado %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
for n=2:11                                                               %%
D(:,n)=smooth(D(:,n)); %y                                                %% 
end                                                                      %% 
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%% Espectros de Absorci�n %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
figure                                                                   %%
subplot(1,2,1)                                                           %% 
P4=plot(D(:,1),D(:,2)-ruD,'b',D(:,1),D(:,3)-ruD,'b',...                  %%
    D(:,1),D(:,4)-ruD,'b', D(:,1),D(:,5)-ruD,'b',...                     %%
    D(:,1),D(:,6)-ruD,'b',D(:,1),D(:,7)-ruD,'b',...                      %%
    D(:,1),D(:,8)-ruD,'b',D(:,1),D(:,9)-ruD,'b',...                      %%
    D(:,1),D(:,10)-ruD,'b',D(:,1),D(:,11)-ruD,'b');                      %%  
 xlabel('Longitud de onda (nm)')                                         %%
 ylabel('Absorbancia (ua)')                                              %%
 legend('2 Acetil Furfural');                                            %%
 title('2 Acetil Furural')                                               %%
 axis([245 325 0 1.7])                                                   %%
 set(P4,'linewidth',2);                                                  %%
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         Curva de Calibraci�n                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
 d=zeros(10,1);                                                          %%
for i=1:10                                                               %%
d(i)=find(D(:,i+1)==max(D(:,i+1)));                                      %%
end                                                                      %%
dm=round(mean(d));                                                       %%
%                                                                        %%
for j=2:11                                                               %%
 max(D(:,j));                                                            %%
end                                                                      %%
MaxD=[max(D(:,2)) max(D(:,3)) max(D(:,4)) max(D(:,5)) max(D(:,6))...     %%
      max(D(:,7)) max(D(:,8)) max(D(:,9)) max(D(:,10)) max(D(:,11))]';   %%
C2af=[0.001 0.002 0.003 0.004 0.005 0.006 0.007 0.008 0.009 0.01]';      %%
%                                                                        %%
%                                                                        %%
subplot(1,2,2)                                                           %% 
 FIT4=polyfit(C2af,MaxD,1);                                              %%
 CON=0:0.001:0.01;                                                       %%
 yp=polyval(FIT4,CON);                                                   %%
 subplot(1,2,2)                                                          %%
 C4=plot(C2af,MaxD,'r+',CON, yp,'k');                                    %%
 xlabel('Concentraci�n mg/100 mL')                                       %%
 ylabel('Absorbancia (ua)')                                              %% 
 title('2 Acetil Furfural')                                              %% 
 legend('Concentraciones','Ajuste')                                      %%
 axis([0 0.01 0 1.7])                                                    %%
 set(C4,'linewidth',2);                                                  %%
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                 Coeficiente de extinci�n molar                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%                                                                        %% 
figure                                                                   %%
D(:,2:end)=D(:,2:end)-ruD;                                               %%
epD=D(:,2:end)*Co./(Co'*Co); %(X'*X)^-1*(y'*X)                           %%
%epA1=(Co'*Co)^-1*A(:,2:end)*Co;                                         %%
C2af=plot(D(:,1),epD,'g');                                               %%
xlabel('Longitud de onda (nm)')                                          %%
ylabel('Absortividad molar')                                             %%  
title('2 Acetil Furfural')                                               %%
legend('Coeficiente de absorci�n 2AF')                                   %%
axis ([ 250 325 0 190 ])                                                 %% 
set(C2af,'linewidth',2);                                                 %%
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                M�ximos  del Coeficiente de Absorci�n                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
%                                                                        %%
nA=find(epA==max(epA));                                                  %%
nB=find(epB==max(epB));                                                  %%
nC=find(epC==max(epC));                                                  %%
nD=find(epD==max(epD));                                                  %%
lA=A(nA,1);                                                              %%
lB=B(nB,1);                                                              %%
lC=C(nC,1);                                                              %%
lD=D(nD,1);                                                              %%
MAF=[lA lB lC lD];                                                       %%
disp('M�ximos de absorci�n de cada componente furanico')                 %%
str = fprintf('2AF   %3.0f  (nm)  \n', lA);                              %%
str = fprintf('5HMF  %3.0f  (nm)  \n', lB);                              %%
str = fprintf('FUR   %3.0f  (nm)  \n', lC);                              %%
str = fprintf('5MF  %3.0f  (nm)   \n', lD);                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% Muestras de Tequila %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %%
%figure                                                                   %%
T=load('tdic.txt');                                                 %%
%                                                                        %%
T1=figure                                                                %%
%subplot(1,2,1)                                                          %% 
All=plot(T(:,1),T(:,2),'b',T(:,1),T(:,3),'b',...                          %%
        T(:,1),T(:,4),'b', T(:,1),T(:,5),'b',...                         %%
        T(:,1),T(:,6),'b')                                               %%
 %   ,T(:,1),T(:,7)-ruT,'b')                                             %%
 %   D(:,1),D(:,8)-ruD,'b',D(:,1),D(:,9)-ruD,'b',...                     %%
 %   D(:,1),D(:,10)-ruD,'b',D(:,1),D(:,11)-ruD,'b');                     %%  
 xlabel('Longitud de onda (nm)')                                         %%
 ylabel('Absorbancia (ua)')                                              %%
 legend('2 Acetil Furfural');                                            %%
 title('2 Acetil Furural')                                               %%
 axis([245 325 0 1.7])                                                   %%
 set(All,'linewidth',2);                                                  %%
%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%% Espectros de absorci�n %%%%%%%%%%%%%%%%%%%%%%%%%%%%

figure

TO=plot(A(:,1),epA,'g',A(:,1),epB,'c',A(:,1),epC,'k',A(:,1),epD,'b');
xlabel('Longitud de onda (nm)')
ylabel('Absorbancia (ua)')
title('Coeficientes de absorci�n')
legend('2AF','5HMF', 'FUR','5MF')
axis([245 325 0 170])
set(TO,'linewidth',2);

%%%%%%%%%%%%%%% encuentra m�ximos %%%%%%%%%%%%%%%%%%%%%%%
nA=find(epA==max(epA));
nB=find(epB==max(epB));
nC=find(epC==max(epC));
nD=find(epD==max(epD)); 
lA=A(nA,1);
lB=B(nB,1);
lC=C(nC,1);
lD=D(nD,1);
MAF=[lA lB lC lD];

disp('M�ximos de absorci�n de cada componente furanico')
str = fprintf('2AF   %3.0f  (nm)  \n', lA);
str = fprintf('5HMF  %3.0f  (nm)  \n', lB);
str = fprintf('FUR   %3.0f  (nm)  \n', lC);
str = fprintf('5MF  %3.0f  (nm)  \n', lD);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% Muestras de Tequila %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

figure 
T=load('teq190515.txt');
%T=load('tdic.txt');
for n=2:7
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


for n=2:7
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
disp('M�ximos de absorci�n de cada componente furanico')
str = fprintf('2AF   %f  (ua)  \n', A1);
str = fprintf('5HMF   %f  (ua)  \n', A2);
str = fprintf('FUR   %f  (ua)  \n', A3);
str = fprintf('5MF   %f  (ua)  \n', A4);

C1=(max(CT1(2))-FIT1(:,2))/FIT1(:,1);
C2=(max(CT1(3))-FIT2(:,2))/FIT2(:,1);
C3=(max(CT1(4))-FIT3(:,2))/FIT3(:,1);
C4=(max(CT1(5))-FIT4(:,2))/FIT4(:,1);
% 
disp('Concentraci�n de cada componente furanico')
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




