$eolcom //
option iterlim=999999999;// avoid limit on iterations
option reslim=300; // timelimit for solver in
option optcr=0.0; // gap tolerance
option solprint=ON; // include solution print in
option limrow=100; // limit number of rows in
option limcol=100; // limit number of columns
//-----------------------------------------------------------------
SETS
        Ports   'ports'            / BR,LA,SE,SI,SH,HK,TO,VL,MA,AU/
        Ships   'ships'            /emma, grethe, lone, marianne/
        Days    'days in schedule' /0*21/;
        ;

ALIAS(Ports,i);
ALIAS(Ships,j);
ALIAS(Days,k);

parameters
         capacity(j) 'capacity of each ship'
         /       emma     10000
                 grethe   10000
                 lone     10000
                 marianne 10000   /
         startfuel(j) 'fuel at the beginning'
         /       emma     3322
                 grethe   1977
                 lone     2394
                 marianne 3681 /;

Table PortPrices(k,i) 'price in $ pr. ton at day k for port i'
       BR   LA    SE    SI    SH    HK    TO    VL    MA    AU
0     487   553   551   433   378   461   602   351   421   511
1     489   560   539   425   381   469   604   353   420   503
2     490   571   511   414   388   473   603   355   433   491
3     495   595   503   409   391   472   611   356   444   487
4     499   601   497   413   398   474   615   354   457   467
5     507   621   489   399   401   481   621   357   469   451
6     503   630   477   387   399   477   634   366   483   446
7     501   628   472   381   402   481   645   372   497   439
8     515   625   473   385   400   482   638   378   502   432
9     525   632   472   386   388   493   635   384   511   427
10    560   631   465   385   389   502   631   387   519   421
11    558   633   459   402   397   504   625   389   523   413
12    549   619   449   404   404   507   621   394   544   399
13    511   615   442   395   414   515   614   392   564   404
14    505   618   443   391   417   527   603   391   573   409
15    507   611   447   399   425   533   598   394   581   414
16    487   602   442   406   433   535   592   395   591   429
17    457   607   457   411   444   531   588   401   602   433
18    451   599   468   413   451   545   582   403   589   432
19    472   593   477   417   471   501   578   414   577   424
20    475   590   481   419   480   477   572   417   565   437
21    481   589   485   426   485   452   565   421   561   436 ;



TABLE ShipSpeed(k,j) 'speed for ship j at day k'
       Emma   Grethe   Lone   Marianne
0      0      0        0      0
1      0      0        0      0
2      0      0        0      0
3      22     22       22     25
4      22     22       22     25
5      22     22       22     25
6      22     22       22     25
7      22     18       22     25
8      22     18       22     25
9      22     18       22     25
10     22     18       22     25
11     22     17       22     25
12     15     17       25     25
13     25     17       25     25
14     25     25       25     17
15     25     25       22     25
16     25     25       22     25
17     25     25       22     25
18     25     25       0      25
19     25     0        0      25
20      0     0        0      25
21      0     0        0      25   ;






PARAMETER AtPort(j,k,i)     '1 if ship j visit port i at day k';
AtPort('Emma','2','SH')=1; AtPort('Emma','4','TO')=1;
AtPort('Emma','11','SE')=1; AtPort('Emma','12','LA')=1;
AtPort('Emma','19','HK')=1; AtPort('Grethe','2','BR')=1;
AtPort('Grethe','6','MA')=1; AtPort('Grethe','10','SI')=1;
AtPort('Grethe','13','HK')=1; AtPort('Grethe','18','VL')=1;
AtPort('Lone','2','LA')=1; AtPort('Lone','11','TO')=1;
AtPort('Lone','14','SH')=1; AtPort('Lone','17','HK')=1;
AtPort('Marianne','2','AU')=1; AtPort('Marianne','13','LA')=1;
AtPort('Marianne','14','SE')=1; AtPort('Marianne','21','VL')=1;


variable
         z 'max profit'
         ;

positive variables
         x(i,j)   	'Product i for region j'
         t(i) 		'Timer assortment i'
         s(i)		'Material surplus i'
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


display x.L , t.L, s.L;

