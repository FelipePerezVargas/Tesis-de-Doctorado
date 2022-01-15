%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% Ejercicios Sobre Estadística Multivariante %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clc 
clear all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%% Capítulo 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% Álgebra Matricial Básica %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% u=[1 2]';  
% v=[-2 3]'; 
% w=[3, -5]';
% 
% % a) (u-2v)*W  b) ||u+v+w|| c) ||u|+||v||+||w|| d) (u-v)*(v-w)
% 
% a=(u-2*v)'*w
% b1=u+v+w
% b=sqrt(b1'*b1)
% b2=norm(u+v+w)
% c=sqrt(u'*u)+sqrt(v'*v)+sqrt(w'*w)
% c1=norm(u)+norm(v)+norm(w)
% d1=u-v
% d2=v-w
% d=d1'*d2

%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% a) u=(8,3) a=(4,-5)
% b) u=(2,1,-4) a=(-5,3,11)

% ua=[8;3];
% aa=[4;-5];
% 
% va=((ua'*aa)/(norm(aa)^2))*aa
% 
% ub=[2;1;-4];
% ab=[-5;3;11];
% 
% vb=((ub'*ab)/(norm(ab)^2))*ab


%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% syms k
% eq=k^2-57.9643*k+29.9952 == 0
% S=solve(eq, k)

%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 4 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A=[1 0 0;
%    1/3 4 0;
%    1/2 3 2];
% B=[9 1 0 0;
%    0 8 -2 0;
%    0 0 7 -3;
%    0 0 0 6];
% 
% da=det(A);
% db=det(B);
% 
% iA=inv(A);
% iB=inv(B);

%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 5 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A=[4 4.001;
%     4.001 4];
% 
% AA=[4 4.001;
%     4.001 4.001001];
% 
% iA=inv(A)
% iAA=inv(AA)
% 
% d=iA-iAA


%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 6 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ecuación característica
% Pendiente de como calcular los eigenvalores e eigenvectores
% A1=[1 2; 2 -2];
% A2=[-2 0 3; 2 4 0; 1 0 0];
% A3=[2 2 2; 1 1 1; 1 1 1];
% eA2=eig(A2);
% eA3=eig(A3)

%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 7 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Generese una matriz X de tamañan 4x3 y un vector u de 4x1, ambos de
%números aleatrios y constrúyanse las matrices simétricas A=XX' y B=uu'
%a) calcule la traza y la determinandte de A y B 
%b)Obtengase los autovalores y autovectores de de A y B
%c) Compruebese que la traza y el determinante coinicden respectivamente
%con la con la suma y el prodcuto de los autovalores de A
%d) Obtengase los rangos de A y B y compruébese que cinciden
%respectivamente, con el número de vectores no nulos de A y B

% A=rand(4,3)
% B=rand(4,1)
% 
% %a) matrices simétricas
%  
% As=A'*A;
% Bs=B*B';
% 
% detA=det(As);
% detB=det(Bs);
% tA=trace(As);
% tB=trace(Bs);
% 
% %b)
% 
% [Ta Da]=eig(As);
% [Tb Db]=eig(Bs);
% 
% % Calculando los autovalores  y autovectores podemos calcular la
% % descomposción espectral de A=TDT donde se cumple que T'T=TT'=1
% WW=Ta*Ta';
% 
% %c)
% 
% pAs=sum(diag(Da));
% pBs=sum(diag(Db));
% pDa=prod(diag(Da));
% pDb=prod(diag(Db));
% re=[detA tA pAs pDa detB tB  pBs pDb];
% 
% %d)
% 
% rA=rank(A)
% rB=rank(B)

%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 8 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Considere las matrices siguientes:

% A=[2 1 4; -1 4 1; 2 -1 4];
%  B=[1 1 -1; 0 1 0; -1 0 1];
% rank(B)
% trace(B)

 % C=[2 1 1; 1 2 -1; -1 -1 2];
% 
% %a) Son idempotentes?
% 
% 
% 
% %b) Calcúlese su determinante
% da=det(A);
% db=det(B);
% dc=det(C);
% 
% %c) Son definidas positivas?
% ta=eig(A)
% tb=eig(B)
% tc=eig(C)
% 
% %d) Son ortogonales?
% 
% oA=A'*A
% oA1=A*A'
% oB=B'*B
% oB1=B*B'
% oC=C'*C
% oC1=C*C'

%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 9 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Calcula la desconposición espectral de  X=U*S*V'

% A=[3 2 2; 2 3 2; 2 2 3];
% [vec val]=eig(A);
% dv=vec*val*vec';
% %s=svd(A);
% X =[1 2;3 4;5 6;7 8]
% s=svd(X)
% [U,S,V] = svd(X)

%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 10 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% de la matriz 

% A=[3 2 0;
%    2 3 0;
%    0 0 3];
% %a) calcula los autovalores de A^2 y A^-1
% 
% [T lambda]=eig(A)
% lambda2=eig(A*A)
% lambdainv=eig(inv(A))
% s=svd(A*A)
% 
% r2=T*diag(lambda2)*T'
% rinv=T*diag(lambdainv)*T'

%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 11 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% de la matriz 

%A=[2 a ; a 2]


%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 12 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% de la matriz 

% A=[6 10; 10 6; 1 5];
% 
% %a) Encuentre la atriz generalizada de Penrose-Moore A^-, de A.
% % A=UD^1/2V' que es la descomposición singular es decir A^-=VD^-1/2U'
% 
% [U D V]=svd(A,0)
% 
% AA=V*inv(D)*U'

%b) Compruébese que se cumple la porpiedad AA^-A=A
% B=pinv(A)
% 
% C=A*AA*A

%c) Compruébese que se cumplen las propiedades 
   % (i) A^-AA^-=A^-
   % (ii) A^-A es simétrica 
   % (iii) AA^- es simétrica
   
% D=AA*A*AA
% E=AA*A
% F=A*AA

%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 13 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Calcúlese la matriz simétrica asociada a cada una de las siguientes formas 
%cuadráticas y detemínese si esta definida positiva.

%a)Q(x1,x2)=2x1^2-3x1x2+3x2^2

% A=[2 -3/2;
%    -3/2 3]
% 
% As=eig(A)
% 
% %b) Q(x1,x2,x3)=x1^2+x1x3+0.25x3^2+1.6x1x2+0.6x2^2+0.8x2x3
% 
% B=[1 0.8 0.5;
%    0.8 0.6 0.4;
%    0.5 0.4 0.25]
% Bs=eig(B)

%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 14 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sea Q(x1,x2)=mx2^2-4x1x2+x1^2
%a)Determínese la matriz simétrica asociada a Q(x1,x2)
%syms m
%A=[1 -2;-2 m]
%dA=det(A)


%b) Deterníbese los valores de m para que A sea definida positiva
%dA=det(A)

% m>=4 para que sean positivas

%c) Hállense los autvalores y los autovectores asociados a A en el caso de 
% que m=-2

% A=[1 -2;-2 -2]
% A1=eig(A)

%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 15 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Considérense la siguientes matrices simétricas de dimensión de 3x3

% A=[3 1 0; 1 3 0; 0 0 3]
% B=[0 0 0; 0 0 0; 0 0 2]
% 
% %a) Decídase el signo de la forma cuadrática q(x')=x'Ax donde c E R^3
% 
% eA=eig(A)
% sA=svd(A)
% 
% %b)
% 
% 
% eB=eig(B)
% sB=svd(B)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% Capítulo 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% Estadísticos Descriptivos %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Se define una matriz de centrado de dimensión n como H=I-1/n11', donde I
% es la matriz identidad de nxn y 1 es el vector de nx1 de unos. La
% utilidad de esta matriz H radica en que, como su nombre lo indica se usa 
% para centrar configuraciones de datos de dimensión nxp. Si x es una matriz 
% de datos nxp, entonces HX es una matriz cuyas columnas tiene media cero. 
% Utilícese Matlab para comprobar las dos siguientes propiedades de la matriz 
% de centrado (Tamndo como ejemplo n=5)

%a) H es idenpotente 
% 
% n=5;
% H=eye(n)-ones(n,n)/n;
% H2=H*H;
% Hi=H-H2;
% 
% %b) Rango de H= traza de H= n-1
% 
% 
% rH=rank(H)
% tH=trace(H)
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%La tabla de datos corresponden a Chalets construidos por diez promotoras
%que operan a lo largo de la costa española.

%a) Dibújese el diagrama de dispersión múltiple y coméntese el aspecto
% del gráfico.

% X=load('pca1.txt');
% X1=[X(:,1) X(:,2)];
% X2=[X(:,1) X(:,3)];
% X3=[X(:,2) X(:,3)];
% 
% %figure
% subplot(2,2,1)
% plotmatrix(X)
% subplot(2,2,2)
% plotmatrix(X1)
% subplot(2,2,3)
% plotmatrix(X2)
% subplot(2,2,4)
% plotmatrix(X3)
% 
% %b) Para X1 y X2 calcúlense, respectivamente mas medias muestrales, las 
% % varianzas muestrales s1, s2, la covarianza entre X1 y X2, s12 y la correlación 
% %entre ambas r12, interprete el valor obtenido de r12.
% [n,p]=size(X);
% 
% %Medias
% m1=mean(X(:,1));
% m2=mean(X(:,2));
% m3=mean(X(:,3));
% 
% Mean=[m1 m2 m3];
% 
% %Varianzas
% 
% v1=var(X(:,1),1);
% v2=var(X(:,2),1);
% v3=var(X(:,3),1);
% v12=(sum(X(:,1).*X(:,2)))/n - m1*m2;
% v13=(sum(X(:,1).*X(:,3)))/n - m1*m3;
% v23=(sum(X(:,2).*X(:,3)))/n - m1*m2;
% 
% 
% V=[v1 v2 v3 v12 v13 v23]
% 
% %Correlación 
% 
% r12=v12/sqrt(v1*v2);
% r13=v13/sqrt(v1*v3);
% r23=v23/sqrt(v2*v3);
% 
% R=[r12 r13 r23]
% 
% %c) Utilizando la matriz de datos X y la de centrado H definida en el
% %problema 2.1, calcúlense el vector de medias muestrales x y la matriz de 
% %covarianzas muestrales S. A partir de esta obtengase la matriz de 
% % correlación R=diag(S)*S*diag(S)  
% 
% %Medias muestrales
% m=X'*ones(n,1)/n;
% %Matriz de covariancias
% H=eye(n,n)-ones(n,n)/n;
% S=(X'*H*X)/n;
% %Matriz de correlación 
% D=diag(S).^-0.5
% R=diag(D)'*S*diag(D)
% S1=cov(X,1)
% R1=corrcoef(S1)
%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%La pblación por mercurio de peces de agua dulce comestrible es una amenaza
%directa contra nuestra salud. Entre 1990 y 1991 se llevo acab un estudio
%de 53 lagos de Florida con el fin de examina   los factores que influían en 
% el nivel de contaminación por mercurio. Las variables que se midieron
% fueron

%X1 número de identificación ,
%X2 nombre del lago ,
%X3 alcalinidad (mg/l de carbonato de calcio),
%X4 pH (mg/l),
%X5 Calcio (mg/l),
%X6 clorofila (mg/l),
%X7Concentración media de mercurio (ppm) en el tejido múscular del grupo 
%   de peces estudiados en cada lago,
%X8 número de peces estudiado por lago, 
%X9 mínimo de la concentración en cada grupo de peces,
%X10 máximo de la concentración en cada grupo de peces, 
%X11 Estimación (mediate regresión) de la concentración de mercurio en un
%pez de 3 años (o promedio cuando la edad no esta disponible),
%X12 indicador de la edad de los peces.

%a) Represéntense de forma conjunta las variables X3, X6 y X7 y véase cómo 
% se modifica su dispersión cuando se porducen (Transformaciones lineales y
%no lineales) sobre las variables. Considérense como medidas de dispersión 
% globla la traza y el determinante de la matriz de covarianzas

% M=dlmread('mercurio.txt', '\t', [0 2 52 11]);
% 
% % XA=[M(:,1) M(:,4:5)]
% % detM=det(cov(XA,1))
% % tM=trace(cov(XA,1))
% % plotmatrix(XA)
% % figure
% % % transformando de g/l en lugar de  mg/l entonces 
% % YA=[M(:,1)/1000 M(:,4)/1000  M(:,5)]
% % detM1=det(cov(YA,1))
% % tM1=trace(cov(YA,1))
% % plotmatrix(YA)
% % figure
% % % Transformación log-log-sqrt
% % WA=[log(M(:,1))  log(M(:,4))  sqrt(M(:,5))]
% % detM2=det(cov(WA,1))
% % tM2=trace(cov(WA,1))
% % plotmatrix(WA)
% % 
% 
% %b) Dibújese el histograma trimensional correspondiente a X3 y X7, elíjanse 
% %sendas transformaciones no lineales para estas variable de entre las utilzadas 
% % en el apartado anterior y dibújese el histrograma de las variables 
% % transformadas
% 
% Xb=M(:,[5,1]);
% figure(1)
% hist3(Xb)
% ylabel('X_3=Alcalinidad')
% xlabel('X_7=Mercurio')
% view(50,50)
% 
% Xc=[sqrt(Xb(:,1)) log(Xb(:,2))]
% figure(2) 
% hist3(Xc)
% ylabel('log(X_3)')
% xlabel('X_7^{1/2}')
% view(50,50)

%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 4 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 5 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %Considérese la matriz de datos 
% 
% A=[-2 1 4; 3 0 -1; 5 1 2; -1 3 6; 2 -7 4; -1 0 -1];
% 
% % que recoge n=6 mediciones de un vector aleatorio X=(X1,X2,X3)'
% %a) calcule la matriz de medias x y varianzas muestrales Sx 
% 
% [n p]=size(A);
% mA=mean(A);
% cA=cov(A,1);
% 
% %b) Calcúlese la matriz de covarianza muestrales de los datos estandarizados 
% % a media cero 
% 
% H=eye(n)-ones(n,n)/n;
% 
% d=sqrt(diag(cA));
% std=ones(n,1)*d'
% Xo=(H*A)./std;
% ccA1=cov(Xo,1);
% 
% %c) Sea ek vector aleatorio Y=(Y1,Y2), donde Y1=-X1+2X2-X3 y Y2=X1+X2.
% % Calcúlense los vectores de medias y y la matriz de covarianzas muestrales
% % de Sy y Y. Calcúlese la matriz de observaciones de Y mediante una
% % operación matricial en la que aparezca la matiz de datos X.
% 
% 
% Y=[-1 2 -1; 1 1 0];
% 
% Y1=A*Y';
% mY=mA*Y';
% sY=Y*cA*Y';
% 
% %d) Calcúlese la matriz de covarianzas del vector aleatorio Z=(Z1,Z2), donde
% % Z1=Y1/sqrt(6) y Z2=Y2/sqrt(2)
% 
% D=[-1/sqrt(6) 2/sqrt(6) -1/sqrt(6); 1/sqrt(2) 1/sqrt(2) 0];
% 
% Z=A*D'
% sZ=D*cA*D'
% 
% %e)Calcúlense las matrices de correlación de X, Y y Z y de la mariz de datos        
% %obtenidad en el jercicio 2.2 
% 
%  
% dx=(diag(cA)).^(-0.5)
% Rx=diag(dx)*cA*diag(dx)
% 
% X=load('pca1.txt');
% [n p]=size(X)
% H=eye(n)-ones(n,n)/n
% Sx=X'*H*X/n
% dx0=(diag(Sx)).^(-0.5)
% Rx0=diag(dx0)*cA*diag(dx0)
% 
% dy=(diag(sY)).^(-0.5)
% Ry=diag(dy)*sY*diag(dy)
% 
% dz=(diag(sZ)).^(-0.5)
% Rz=diag(dz)*sZ*diag(dz)
% 
% coX=corrcoef(A)

%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 6 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Considere as n=5 observaciones,

% X=[1 6; 3 8;-2 7; 5 -3; 2 0];
% b=[-2 1]'; c=[-1 3]';
% 
% B=X*b
% C=X*c
% 
% mX=mean(X)
% mB=mean(B)
% mC=mean(C)
% 
% BC=[B C]
% [n p]=size(BC)
% vB=var(B,1)
% vC=var(C,1)
% cBC=cov(vB,vC,1)
% cX=cov(X,1)
% 


%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejercicio 7 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Un Biologo recoge medidas (en mm) de dos craneos de las especies, A y B,
% de ratones. Concretamente observa tres variables X1, X2 Y X3 en un conjunto
% de los cuales na=50 son de la especia A y los restantes nb=60 son de la 
% especie B.

%a) Denotemos por XA la matriz de datos observados en las especies A. Si
% nA=50
% 
% datoA=[25.5; 14.1;11.3];
% %y 
% datoB=[40.2 10.9 15.6; 10.9 13.7 14.5; 15.6 14.5 20.1];

%Calcúlese el vector de medias xA y la matriz de covarianza de SA
%correspondientes a esta epecie.

% MediaA=datoA/50 
% SA=datoB/50-MediaA*MediaA'
% 
% 
% %b) Denotemos por XB la matriz de observaciones de B. Si
% nB=60 
% datosB=[26.3; 15.5; 10];
%  datosXBXB=[50.7 32.6 24.8
%        32.6 29.0 12.6
%        24.8 12.6 35.8]
%    
%  MdatosB=datosB/nB
%  SB=datosXBXB/nB-MdatosB*MdatosB'
%    
 %c) 
 
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%%% Ensayo Regresión Multiple %%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
% load carsmall
%X = [Weight,Horsepower,Acceleration];
T=load('tdic.txt')
Y=T(:,2)
X=[epA epB epC epD]
%XY = [Weight,Horsepower,Acceleration Y]
 
 
 lm = fitlm(X,Y,'linear')

 
%%%%%%%%%%%%%%%%%%%%%%% Intento PCA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% M=load('pca1.txt');
% [n k]=size(M);
% 
% 
% %%%%%%%%%%%%%% 
% plotmatrix(M)%
% %%%%%%%%%%%%%%
% 
% %%%%%%%%%%%
% r1=M(:,1);%
% r2=M(:,2);%
% r3=M(:,3);%
% %%%%%%%%%%%
% 
% 
% 
% 
% %%%%%%%%%%%%%
% M1=mean(r1);%
% M2=mean(r2);%
% M3=mean(r3);%
% %%%%%%%%%%%%%
% 
% %%%%%%%%%%%%%%%
% v11=var(r1,1);%
% v22=var(r2,1);%
% v33=var(r3,1);%%%%%%%%%%%%%
% v12=sum(r1.*r2)/n - M1*M2;%
% v23=sum(r2.*r3)/n-M2*M3;%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% 
% %%%%%%%%%%%%%%%%%%%%%%%
% r11=v11/sqrt(v11*v22);%
% r22=v22/sqrt(v11*v22);%
% r12=v12/sqrt(v11*v22);%
% %%%%%%%%%%%%%%%%%%%%%%%
% 
% 
% M1=[r1 r2];
% %m=M1'*ones(n,1)/n;
% H=eye(n)-ones(n,n)/n;
% S=M1'*H*M1/n;
% [vectores valores]=eig(S);
% d=diag(S).^(-0.5);
% R=diag(d)*S*diag(d);
% 
% 
% X=[3 1 1;
%    1 3 1;
%    1 1 5];
% 
% 
% [vec val]=eig(X)

% % Animacion de la solucion de la ecuacion del transporte
% % U_t + 2 U_x = 0
% % con condicion inicial U(x,0)=exp(-xˆ2)
% x=-2:0.1:10; t=0:0.05:6; set(gca,'nextplot','replacechildren');
% axis([-2 10 0 1]); for i=1:length(t)
% u=exp(-(x-2*t(i)).^2);
% plot(x,u);
% M(i)=getframe;
% end


% Animacion de la solucion de la ecuacion del calor 2d
% U_t = 1/8(U_xx + U_yy)
% en el dominio 0 < x, y < 1
% condicion inicial u(x,y,0)=Sin(pix) Sin(piy)
% para 0<t<3
% [x y]=meshgrid(0:.01:1,0:.01:1);
% %define una malla para [0,1]x[0,1] con incremento en x y y de 0.01
% title('Animacion de la temperatura');
% set(gca,'nextplot','replacechildren'); caxis manual;
% % permite que todos los gr´aficos usen los mismos l´?mites en colores.
% caxis([-1 1]);
% % define los valores maximos para los limites como -1 y 1
% axis equal;
% %usa la misma escala para los ejes x y
% t=0:0.05:3;
% %vector de valores para los diferentes tiempos
% for j=1:length(t) z=exp(-1/8*pi^2.*t(j)).*sin(2*pi*x).*sin(2*pi*y);
% %evaluacion de la funcion de dos variables
% axis off;
% %remueve los ejes
% pcolor(x,y,z);
% %grafica en dos dimensiones
% shading interp;
% %interpolacion de colores
% colorbar;
% % agrega la barra de colores
% M(j) = getframe(gcf);
% % captura los graficos y los guarda en la matriz M
% end

% 
% [x,y,z] = meshgrid(0:.05:1,0:0.05:1,0:.05:1);
% set(gca,'nextplot','replacechildren'); caxis manual; caxis([0,1]);
% t=0:0.01:0.5; for j=1:length(t)
% v=exp(-pi*pi*t(j)).*sin(pi*x).*sin(pi*y).*sin(pi*z);
% h = slice(x,y,z,v,[0.25 .5 .75],[],[0]);
% caxis([0,1]); alpha('color')
% set(h,'EdgeColor','none','FaceColor','interp',...
%     'FaceAlpha','interp')
% alphamap('rampdown') alphamap('increase',.1) colormap(hsv);
% colorbar; F(j)=getframe(gcf); end;
