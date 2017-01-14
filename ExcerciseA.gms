$eolcom //
option iterlim=999999999;// avoid limit on iterations
option reslim=300; // timelimit for solver in
option optcr=0.0; // gap tolerance
option solprint=ON; // include solution print in
option limrow=100; // limit number of rows in
option limcol=100; // limit number of columns
//-----------------------------------------------------------------
SETS
        Material   'All Materials'     / MAS, KUS, KOS, KUV, KOV, HSEL, LSEL, PAP,MAT, KUT, KOT, MAK, KUK, KOK, FUEL/
        Products(Material)   'Products'  / MAS, KUS, KOS, KUV, KOV, HSEL, LSEL, PAP/
        Regions   'Regions'            /EU, IE, PA, KI/
        ProductionMaterials(Material) 'Materials used for other products' /MAT,KUT,KOT,MAK,KUK,KOK,HSEL,LSEL/
        Timber(Material)    'Types of timber' /MAT, KUT, KOT, MAK, KUK, KOK/
        SawMillProducts(Material) 'Products produced at the sawmill' /MAS, KUS, KOS/
        PlywoodMillProducts(Material) 'Products prodcued at plywoodmill' /KUV, KOV/
        FuelProducts(Material) 'Products producing fuel' /MAS, KUS, KOS, KUV, KOV/
        PulpMillProducts(Material) 'Products produced at pulpmill' /HSEL, LSEL/
        DemandParameters 'The demand parameters table 1' /Gamma, Delta/;
        ;

ALIAS(Products,i);
ALIAS(Regions,j);
ALIAS(Timber,k);
ALIAS(ProductionMaterials,ProM);
ALIAS(SawMillProducts,sm);
ALIAS(PlywoodMillProducts, pm);
ALIAS(FuelProducts, fp);
ALIAS(PulpMillProducts, pmp);
ALIAS(DemandParameters, dp);

Table ProductReq(i,ProM) 'Amount of timber needed for each product'
        MAT  KUT  KOT  MAK  KUK  KOK HSEL LSEL
    
    MAS 2.0           -0.8           
    
    KUS      2.0            -0.8     
    
    KOS           2.0           -0.8 
    
    KUV      2.8            -1.6     
    
    KOV           2.8           -1.6 
    
    HSEL               4.8 
    
    LSEL                         4.8
    
    PAP                     1.0       0.2  0.2;           

Parameters
    fuel(sm) 'fuel generated when producing products' /-0.2/

    c(i) 'cost of making product'
    /MAS 550,
     KUS 500,
     KOS 450,
     KUV 2500,
     KOV 2600, 
     HSEL 820,
     LSEL 800,
     PAP 1700
     /;

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

variable
    z 'max profit'
    ;
positive variables
    x(i,j)  'Product i for region j'
    t(k)    'Timber assortment for timber k'
    s(k)    'Material surplus timber k'
    ; 
        
equations
        profit 			'objective function'
        sawMillCap 		''
        plywoodMillCap 	''
        line1Cap		''
        line2Cap		''
        paperMillCap	''
        surPlus(j)		''
        MASproduction	''
        KUSKUVproduction	''
        KOSKOVproduction	''
        HSELproduction	''
        LSELproduction	''
        PAPproduction	''
        ;

		profit .. 			z =e= sum((i,j), x(i,j)*((g(i,j)-d(i,j)*x(i,j)) - c(i)) -
                                  sum(k, t(k)*(a(k)+b(k)*t(k))) +
                                  sum((fp,j), 0.2*x(fp,j)*40) +
                                  sum(k,s(k)*a(k)) 
                                  ;
							
		sawMillCap.. 		sum((sm,j), x(sm,j)) =l= 200000; 
 		plywoodMillCap.. 	sum((pm,j), x(pm,j)) =l= 90000;
 		line1Cap..			sum((pmp,j), x('HSEL',j)) =l= 220000;
 		line2Cap..			sum((pmp,j), x('LSEL',j)) =l= 180000;
 		paperMillCap..		sum((i,j), x('8',j)) =l= 80000;
 		surPlus(j)..		sum(i, t(i)-x(i,j)*u(i,j)) =e= s(i);
 		MASproduction..		sum(j, 2*x('1',j)) =l= t('1');
        KUSKUVproduction..	sum(j, 2*x('2',j) + 2.8*x('4',j)) =l= t('2');
        KOSKOVproduction..	sum(j, 2*x('3',j) + 2.8*x('5',j)) =l= t('3');
        HSELproduction..	sum(j, 4.8*x('6',j) - 0.8*x('1',j)) =l= t('4');	
        LSELproduction.. 	sum(j, 4.2*x('7',j) - 0.8*x('3',j) - 1.6*x('5',j)) =l= t('6');
        PAPproduction..		sum(j, x('8',j) - 0.8*x('2',j) - 1.6*x('4',j)) =l= 
        										t('5') + 0.2 * sum(j,x('6',j) + x('7',j));




model aStaticModel /all/ ;

solve aStaticModel using mip maximizing z;


