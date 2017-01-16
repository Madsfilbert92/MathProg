$eolcom //
option iterlim=999999999;// avoid limit on iterations
option reslim=300; // timelimit for solver in
option optcr=0.0; // gap tolerance
option solprint=ON; // include solution print in
option limrow=100; // limit number of rows in
option limcol=100; // limit number of columns
//-----------------------------------------------------------------
SETS
        Products   'Products'  / MAS, KUS, KOS, KUV, KOV, HSEL, LSEL, PAP/
        Regions   'Regions'            /EU, IE, PA, KI/
        Timber    'Types of timber' /MAT, KUT, KOT, MAK, KUK, KOK/
        SawMillProducts(Products) 'Products produced at the sawmill' /MAS, KUS, KOS/
        PlywoodMillProducts(Products) 'Products prodcued at plywoodmill' /KUV, KOV/
        FuelProducts(Products) 'Products producing fuel' /MAS, KUS, KOS, KUV, KOV/
        DemandParameters 'The demand parameters table 4' /Gamma, Delta/
        CostParameters 'The cost parameters table 1' /Alpha, Beta/
        Quantities 'Possible Quantities to be sold' /1*150/;
        ;

ALIAS(Products,i);
ALIAS(Regions,j);
ALIAS(Timber,k);
ALIAS(SawMillProducts,sm);
ALIAS(PlywoodMillProducts, pm);
ALIAS(FuelProducts, fp);
ALIAS(DemandParameters, dp);
ALIAS(CostParameters, cp);
ALIAS(Quantities, q);

Table ProductReq(i,k) 'Amount of timber needed for each product'
        MAT  KUT  KOT  MAK  KUK  KOK 
    
    MAS 2.0           -0.8           
    
    KUS      2.0            -0.8     
    
    KOS           2.0           -0.8 
    
    KUV      2.8            -1.6     
    
    KOV           2.8           -1.6 
    
    HSEL               4.8 
    
    LSEL                         4.8
    
    PAP                      1.0    ;               

Parameters
    c(i) 'cost of making product'
    /MAS 550,
     KUS 500,
     KOS 450,
     KUV 2500,
     KOV 2600, 
     HSEL 820,
     LSEL 800,
     PAP 1700
     /

    qu(q) 'Quantities in 10000';

qu(q) = ord(q);

Table demand(i,j,dp) 'The demand parameters of the products for different markets'
            Gamma   Delta
    MAS.EU  1600    4.0
    MAS.IE  1300    10.0
    MAS.PA  1400    12.0
    MAS.KI  1500    15.0
    KUS.EU  1400    4.0
    KUS.IE  1200    10.0
    KUS.PA  1300    12.0
    KUS.KI  1400    15.0
    KOS.EU  1300    14.0
    KOS.IE  1400    20.0
    KOS.PA  1500    22.0
    KOS.KI  1600    25.0
    KUV.EU  4400    4.0
    KUV.IE  3800    10.0
    KUV.PA  3600    12.0
    KUV.KI  3500    15.0
    KOV.EU  4300    4.0
    KOV.IE  4100    10.0
    KOV.PA  3900    12.0
    KOV.KI  3800    15.0
    HSEL.EU 2300    2.0
    HSEL.IE 2500    4.0
    HSEL.PA 2300    5.0
    HSEL.KI 2600    6.0
    LSEL.EU 2500    3.0
    LSEL.IE 2800    2.0
    LSEL.PA 2300    5.0
    LSEL.KI 2500    7.0
    PAP.EU  4500    4.0
    PAP.IE  4700    10.0
    PAP.PA  4300    12.0
    PAP.KI  4800    15.0   
    ;    
Table cost(k,cp)  'The timber assortment cost parameters'  
            Alpha   Beta
    MAT     190     1.0
    KUT     150     0.5
    KOT     120     3.0
    MAK     180     0.2
    KUK     150     0.3
    KOK     150     0.2    
    ;    

parameter price(i,j,q);

price(i,j,q) = demand(i,j, 'Gamma')-demand(i,j,'Delta')*qu(q)*10

parameter purchase(k,q);

purchase(k,q) = cost(k, 'Alpha')+cost(k,'Beta')*qu(q)*10;

variable
    z 'max profit'
    ;

integer variables
     x(i)     'Produced of product i in 1000';

variables
     s(k)    'Material surplus timber k in 1000';

binary variable
    y(k) 'if there is a surplus of material k'
    sol(i,j,q) 'sold product i in region j in 10000'
    t(k,q)    'Timber assortment for timber k in 10000';


equations
        profit 			'objective function'
        materialReq     ''
        sawMillCap 		''
        plywoodMillCap 	''
        line1Cap		''
        line2Cap		''
        paperMillCap	''
        surPlus(k)		''
        SoldLessThanProduced ''
        HSELToSell      ''
        LSELToSell      ''
        IsThereSurplus  ''
        NotMoreThanOneQuan ''
        NotMoreThanOneT ''
        ;

		profit .. 			z =e= sum((i,j,q), price(i,j,q)*sol(i,j,q)*ord(q)*10) - sum(i, c(i)*x(i)) -
                                  sum((k,q), purchase(k,q)*t(k,q)*ord(q)*10) +
                                  sum(fp, 0.2*x(fp)*40) +
                                  sum(k, cost(k,'Alpha')*y(k)) 
                                  ;
		sawMillCap.. 		sum((sm), x(sm)) =l= 200; 
 		plywoodMillCap.. 	sum((pm), x(pm)) =l= 90;
 		line1Cap..			x('HSEL') =l= 220;
 		line2Cap..			x('LSEL') =l= 180;
 		paperMillCap..		x('PAP') =l= 80;
        NotMoreThanOneT(k) .. sum(q, t(k,q)) =l= 1;
        surPlus(k)..        sum(q, t(k,q)*ord(q)*10) - sum(i, x(i)*ProductReq(i,k)) =e= s(k);
 		materialReq(k) ..   sum(i, ProductReq(i,k)*x(i)) =l= sum(q, t(k,q)*ord(q)*10); 
        SoldLessThanProduced(i) .. sum((j,q), sol(i,j,q)*ord(q)) =l=  x(i)/10;
        NotMoreThanOneQuan(i,j) .. sum(q, sol(i,j,q)) =l= 1; 
        HSELToSell .. sum((j,q), sol('HSEL', j,q)*ord(q)) =l= (x('HSEL')-0.2*x('PAP'))/10;
        LSELToSell .. sum((j,q), sol('LSEL', j, q)*ord(q)) =l= (x('LSEL')-0.2*x('PAP'))/10;
        IsThereSurplus(k) .. s(k) =g= y(k); 

model aStaticModel /all/ ;

solve aStaticModel using mip maximizing z;

Display x.L, t.L, sol.L, x.M, s.L, y.L;
