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
    
    PAP


