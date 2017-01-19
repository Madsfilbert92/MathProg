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
        Year 'Year in which the product is sold' /T0*T2/
        Scenarios   'Scenarios'                     /S1*S4/
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
ALIAS(Scenarios,sc);

Table Rho(sc,a)
        T0      T1      T2
    S1  1.00    1.05    1.07
    S2  1.00    1.05    0.95
    S3  1.00    0.95    1.05
    S4  1.00    0.95    0.93
;

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

parameter price(i,j,a,q);

price(i,j,a,q) = demand(i,j, 'Gamma')-(demand(i,j,'Delta')*ord(q)*10)/power(coef(i), years(a))

parameter purchase(k,q);

purchase(k,q) = cost(k, 'Alpha')+cost(k,'Beta')*ord(q)*10;

variable
    z 'max profit'
    yz(a,sc) 'profit per year' 
    s(k,a,sc)      'surplus of timber k'
    x(i,a,sc)     'Produced of product i in 1000'
    
;
positive variables
     cap(i,a,sc)      'Slackvariable for extra capacity'
     AccCap(i,a,sc) 'accumulated capacity until a'; 

binary variable
    sol(i,j,a,sc,q) 'sold product i in region j in 10000'
    t(k,a,sc,q)    'Timber assortment for timber k in 10000';


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
        SlackFirstYear ''
        MaxCapAdd1 ''
        MaxCapAdd2 ''
        MaxCapAdd3 ''
        MaxCapAdd4 ''
        MaxCapAdd5 ''
        AccuCap ''
        SCap1   ''
        SCap2   ''
        SCap3   ''
        SCap4   ''
        SCap5   ''
        ScenarioSolControl1 ''
        ScenarioSolControl2 ''
        ScenarioSolControl3 ''
        ;

        profit ..           z =e= sum((sc,a), yz(a,sc)); 
                                  ;
        YearlyProfit(a,sc) ..     yz(a,sc) =e=  power(0.95,years(a))*0.25*(sum((i,j,q), Rho(sc,a)*price(i,j,a,q)*sol(i,j,a,sc,q)*ord(q)*10) - sum(i, c(i)*x(i,a,sc)) -
                                          sum((k,q), purchase(k,q)*t(k,a,sc,q)*ord(q)*10) +
                                          sum(fp, 0.2*x(fp,a,sc)*40) +
                                          sum(k, cost(k,'Alpha')*s(k,a,sc))
                                          - sum(i, FixCost(i)*(cap(i, a+1,sc)+AccCap(i,a,sc))));                                 
        sawMillCap(a,sc)..        sum((sm), x(sm,a,sc)) =l=  sum(sm, AccCap(sm,a,sc)); 
        plywoodMillCap(a,sc)..    sum((pm), x(pm,a,sc)) =l= sum(pm,  AccCap(pm,a,sc)); 
        line1Cap(a,sc)..          x('HSEL', a,sc) =l= AccCap('HSEL',a,sc); 
        line2Cap(a,sc)..          x('LSEL', a,sc) =l= AccCap('LSEL',a,sc);
        paperMillCap(a,sc)..      x('PAP', a,sc) =l= AccCap('PAP',a,sc);
        NotMoreThanOneT(a,k,sc) .. sum(q, t(k,a,sc,q)) =l= 1;
        surPlus(a,k,sc)..        sum(q, t(k,a,sc,q)*ord(q)*10) - sum(i, x(i,a,sc)*ProductReq(i,k)) =e= s(k,a,sc);
        materialReq(a,k,sc) ..   sum(i, ProductReq(i,k)*x(i,a,sc)) =l= sum(q, t(k,a,sc,q)*ord(q)*10); 
        SoldLessThanProduced(a,i,sc) .. sum((j,q), sol(i,j,a,sc,q)*ord(q)) =l=  x(i,a,sc)/10;
        NotMoreThanOneQuan(a,i,j,sc) .. sum(q, sol(i,j,a,sc,q)) =l= 1; 
        HSELToSell(a,sc) .. sum((j,q), sol('HSEL', j, a,sc, q)*ord(q)) =l= (x('HSEL',a,sc)-0.2*x('PAP',a,sc))/10;
        LSELToSell(a,sc) .. sum((j,q), sol('LSEL', j, a,sc, q)*ord(q)) =l= (x('LSEL',a,sc)-0.2*x('PAP',a,sc))/10; 
        SlackFirstYear(i,sc) .. cap(i,'T0',sc) =e= 0;
        MaxCapAdd1(a,sc) .. sum(sm, AccCap(sm,a,sc)) =l= 100*1.5;
        MaxCapAdd2(a,sc) .. sum(pm, AccCap(pm,a,sc)) =l= 90*1.5;
        MaxCapAdd3(a,sc) .. AccCap('HSEL',a,sc) =l= 100*2;
        MaxCapAdd4(a,sc) .. AccCap('LSEL', a,sc) =l= 150*2;
        MaxCapAdd5(a,sc) .. AccCap('PAP', a,sc) =l= 80*2;
        AccuCap(i,a,sc)$(ord(a)>1) .. AccCap(i,a,sc) =e= AccCap(i,a-1,sc)+cap(i,a,sc);
        SCap1(sc) .. sum(sm, AccCap(sm, 'T0',sc)) =e= 100;
        SCap2(sc) .. sum(pm, AccCap(pm, 'T0',sc)) =e= 90;  
        SCap3(sc) .. AccCap('HSEL', 'T0',sc) =e= 100; 
        SCap4(sc) .. AccCap('LSEL', 'T0',sc) =e= 150;   
        SCap5(sc) .. AccCap('PAP', 'T0',sc) =e= 80;  
        ScenarioSolControl1(i,j,q, sc).. sol(i, j, 'T0',sc,q) =e= sol(i, j, 'T0',sc++1,q);
        ScenarioSolControl2(i,j,q).. sol(i, j, 'T1','S1',q) =e= sol(i, j, 'T1','S2',q);
        ScenarioSolControl3(i,j,q).. sol(i, j, 'T1','S3',q) =e= sol(i, j, 'T1','S4',q);
        

model aStaticModel /all/ ;

solve aStaticModel using mip maximizing z;

Display x.L, t.L, sol.L, x.M, s.L, yz.L, AccCap.L, cap.L;
