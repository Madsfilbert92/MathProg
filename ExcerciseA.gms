$eolcom //
option iterlim=999999999;// avoid limit on iterations
option reslim=300; // timelimit for solver in
option optcr=0.0; // gap tolerance
option solprint=ON; // include solution print in
option limrow=100; // limit number of rows in
option limcol=100; // limit number of columns
//-----------------------------------------------------------------
SETS
        Products   'Products'            / MAS, KUS, KOS, KUV, KOV, HSEL, LSEL, PAP/
        Regions   'Regions'            /EU, IE, PA, KI/
        Timber    'Types of timber' /MAT, KUT, KOT, MAK, KUK, KOK/
        SawMillProducts(Products) 'Products produced at the sawmill' /MAS, KUS, KOS/
        PlywoodMillProducts(Products) 'Products prodcued at plywoodmill' /KUV, KOV/
        FuelProducts(Products) 'Products producing fuel' /MAS, KUS, KOS, KUV, KOV/
        PulpMillProducts(Products) 'Products produced at pulpmill' /HSEL, LSEL/;
        ;

ALIAS(Products,i);
ALIAS(Regions,j);
ALIAS(Timber,k);
ALIAS(SawMillProducts,sm);
ALIAS(PlywoodMillProducts, pm);
ALIAS(FuelProducts, fp);
ALIAS(PulpMillProducts, pmp);

Table ProductReq(i,k) 'Amount of timber needed for each product'
        MAT  KUT  KOT  MAK  KUK  KOK  FUEL HSEL  LSEL
    
    MAS 2.0           -0.8
    
    KUS
    
    KOS
    
    KUV
    
    KOV
    
    HSEL
    
    LSEL
    
    PAP;
        
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

		profit .. 			z =e= sum((i,j), x(i,j));
							//OBS skal afhænge af subsets istedet for i,j
		sawMillCap.. 		sum((i,j), x(i,j)) =l= 200000; 
 		plywoodMillCap.. 	sum((i,j), x(i,j)) =l= 90000;
 		line1Cap..			sum((i,j), x(i,j)) =l= 220000;
 		line2Cap..			sum((i,j), x(i,j)) =l= 180000;
 		paperMillCap..		sum((i,j), x(i,j)) =l= 80000;
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


