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
        Quantities 'Possible Quantities to be sold' /1*150/
        Year 'Year in which the product is sold' /T1*T3/
        ;

ALIAS(Products,i);
ALIAS(Regions,j);
ALIAS(Timber,k);
ALIAS(Year, a);
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
    
    LSEL                         4.2
    
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

     coef(i) 'coeficcient to be added'
    /
    MAS 1.010,
    KUS 1.008,
    KOS 1.015,
    KUV 1.015,
    KOV 1.020,
    HSEL 1.025,
    LSEL 1.030,
    PAP 1.0350
    /

    FixCost(i) 'Cost of added capacity'
    /
     MAS 100,
     KUS 100,
     KOS 100,
     KUV 300,
     KOV 300, 
     HSEL 500,
     LSEL 500,
     PAP 700
     /

    years(a) 'years in numbers'
    qu(q) 'Quantities in 10000';

qu(q) = ord(q);
years(a) = ord(a)-1;


Table demand(i,j, dp) 'The demand parameters of the products for different markets'
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
    yz(a) 'profit per year'
    ;

integer variables
     x(i,a)     'Produced of product i in 1000'
     ;

variables 
    s(k,a)      'surplus of timber k'
    cap(i,a)      'Slackvariable for extra capacity'
    AddCap(i,a) 'accumulated addition until a' ;

binary variable
    sol(i,j,a,q) 'sold product i in region j in 10000'
    t(k,a,q)    'Timber assortment for timber k in 10000';


equations
        profit          'objective function'
        YearlyProfit    'profitperyear'
        materialReq     ''
        sawMillCap      ''
        plywoodMillCap  ''
        line1Cap        ''
        line2Cap        ''
        paperMillCap    ''
        surPlus      ''
        SoldLessThanProduced ''
        HSELToSell      ''
        LSELToSell      ''
        NotMoreThanOneQuan ''
        NotMoreThanOneT ''
        DemandInYear ''
        SlackFirstYear ''
        MaxCapAdd1 ''
        MaxCapAdd2 ''
        MaxCapAdd3 ''
        MaxCapAdd4 ''
        MaxCapAdd5 ''
        Accu ''
        ;

        profit ..           z =e= sum(a, power(0.95,years(a))*yz(a)) 
                                  ;
        YearlyProfit(a) ..     yz(a) =e=  sum((i,j,q), price(i,j,q)*sol(i,j,a,q)*ord(q)*10) - sum(i, c(i)*x(i,a)) -
                                          sum((k,q), purchase(k,q)*t(k,a,q)*ord(q)*10) +
                                          sum(fp, 0.2*x(fp,a)*40) +
                                          sum(k, cost(k,'Alpha')*s(k,a))
                                          - sum(i, FixCost(i)*cap(i, a+1));                                 
        sawMillCap(a)..        sum((sm), x(sm,a)) =l= 100 + sum(sm, AddCap(sm,a)); 
        plywoodMillCap(a)..    sum((pm), x(pm,a)) =l= 90 + sum(pm,  AddCap(pm,a)); ;
        line1Cap(a)..          x('HSEL', a) =l= 100 +  AddCap('HSEL',a); 
        line2Cap(a)..          x('LSEL', a) =l= 150 +  AddCap('LSEL',a);
        paperMillCap(a)..      x('PAP', a) =l= 80 + AddCap('PAP',a);
        NotMoreThanOneT(a,k) .. sum(q, t(k,a,q)) =l= 1;
        surPlus(a,k)..        sum(q, t(k,a,q)*ord(q)*10) - sum(i, x(i,a)*ProductReq(i,k)) =e= s(k,a);
        materialReq(a,k) ..   sum(i, ProductReq(i,k)*x(i,a)) =l= sum(q, t(k,a,q)*ord(q)*10); 
        SoldLessThanProduced(a,i) .. sum((j,q), sol(i,j,a,q)*ord(q)) =l=  x(i,a)/10;
        NotMoreThanOneQuan(a,i,j) .. sum(q, sol(i,j,a,q)) =l= 1; 
        HSELToSell(a) .. sum((j,q), sol('HSEL', j, a, q)*ord(q)) =l= (x('HSEL',a)-0.2*x('PAP',a))/10;
        LSELToSell(a) .. sum((j,q), sol('LSEL', j, a, q)*ord(q)) =l= (x('LSEL',a)-0.2*x('PAP',a))/10; 
        DemandInYear(a,i,j,q)$(ord(a)>1) .. sol(i,j,a,q)*ord(q) =e= power(coef(i), years(a))*sol(i,j,a-1,q)*ord(q); 
        SlackFirstYear(i) .. cap(i,'T1') =e= 0;
        MaxCapAdd1 .. sum((a, sm), AddCap(sm,a)) =l= 100*1.5-100;
        MaxCapAdd2 .. sum((a, pm), AddCap(pm,a)) =l= 90*1.5-90;
        MaxCapAdd3 .. sum(a, AddCap('HSEL',a)) =l= 100*2-100;
        MaxCapAdd4 .. sum(a, AddCap('LSEL', a)) =l= 150*2-150;
        MaxCapAdd5 .. sum(a, AddCap('PAP', a)) =l= 80*2-80;
        Accu(i,a) .. AddCap(i,a) =e= AddCap(i,a-1)+cap(i,a);

model aStaticModel /all/ ;

solve aStaticModel using mip maximizing z;

Display x.L, t.L, sol.L, x.M, s.L, yz.L;