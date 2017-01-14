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
         z 'cost'
         rebate  'HK rebate';

positive variables
         x(j,k)   'how much oil does ship j have in reserve at day k (at the end of the day)'
         y(i,j,k) 'how much oil ship j buys in port i at day k'
         w(j,k)   'how much oil does ship j use on day k';// could've been a parameter

binary variable
         u(i,j,k) 'denotes whether or not ship j buys oil in port i'
         h       'hong kong variable' ;

equations
         cost 'objective function'
         maxcap 'maximum capacity of each ship'
         portoil 'every ship may not leave port without this amout of fuel'
         minbuy  'minimum purchase of 200 tonnes'
         forcebuy 'only buy oil, if u(i,j,k) <> 0'
         balance  'fuel balance'
         fuel    'fuel consumption'
         hongkong 'hong kong oil purchase'
         r       'rebate constraints'
         con     'rebate constraints'
         ;

         cost .. z =e= sum((i,j,k),portprices(k,i)*y(i,j,k))
                 - rebate;
         maxcap(j,k) .. x(j,k) =l= capacity(j);
         portoil(j,k)$(sum(i, AtPort(j,k,i))=1) .. x(j,k) =g=  600;
         minbuy(i,j,k) .. u(i,j,k)*200 =l= y(i,j,k);
         forcebuy(i,j,k) .. y(i,j,k) =l= u(i,j,k)*capacity(j);
         balance(j,k)$(ord(k) >= 3) .. x(j,k-1) + sum(i,y(i,j,k))-w(j,k)=e= x(j,k);
         fuel(j,k) .. power(shipspeed(k,j)/25,3)*200*0.96 =e= w(j,k);
         hongkong ..  sum((j,k), y('HK',j,k)) =g= h*500 ;
         r ..    rebate =e= sum((j,k),portprices(k,'HK')*0.35*y('HK',j,k));
         con .. rebate =l= h*1000000000;

// part 3. cost 8.000.000, cost savings: 134.000*52 = 6.986.800
// net loss of 1.013.200


model shipbunkering /all/ ;

x.FX(j,k)$(ord(k) = 2) = startfuel(j);
x.LO(j,'21') = 800;
u.UP(i,j,k) = AtPort(j,k,i);


solve shipbunkering using mip minimizing z;


display x.L , y.L, u.L;

