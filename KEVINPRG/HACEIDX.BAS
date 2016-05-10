'ordenador de archivos primitivo
DEFINT A-Z
KILL "F:\kevinprg\kevindat\new.dbf": CLS
OPEN "F:\kevinprg\kevindat\maestro.dbf" FOR RANDOM AS #1 LEN = 135
FIELD #1, 6 AS codigo$, 2 AS mini1$, 2 AS mini2$, 2 AS maxi1$, 2 AS maxi2$, 2 AS ubic$, 35 AS nomb$, 20 AS obser$, 1 AS acti$, 2 AS exi1$, 2 AS exi2$, 2 AS exi3$, 4 AS pv1$, 4 AS pv2$, 4 AS pv3$, 4 AS cos1$, 4 AS cos2$, 4 AS cos3$, 1 AS iv1$, 1 AS iv2$, 1 AS iv3$, 1 AS of1$, 1 AS of2$, 1 AS of3$, 2 AS labo$, 1 AS re$, 2 AS ped$, 1 AS medi$, 1 AS misc$, 1 AS quim$, 3 AS compo1$, 3 AS compo2$, 3 AS compo3$, 3 AS compo4$, 3 AS compo5$, 3 AS compo6$, 1 AS preg$
max1% = LOF(1) / 135
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM AS #2 LEN = 200
FIELD #2, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 18 AS reserva$
FOR k = 1 TO max1%
GET #1, k
'PRINT codigo$; "-"; CVI(mini1$); "-"; CVI(mini2$); "-"; CVI(maxi1$); "-"; CVI(maxi2$); "-"; "ubicacion:"; ubic$; "-"; nomb$
'PRINT obser$; "-"; acti$; "-EXIS>"; CVI(exi1$); "-"; CVI(exi2$); "-"; CVI(exi3$)
'PRINT "-Precio>";
'PRINT USING "#########,.##"; CVS(pv1$); CVS(pv2$); CVS(pv3$)
'PRINT USING "#########,.##"; CVS(cos1$); CVS(cos2$); CVS(cos3$)
'PRINT "-"; iv1$; "-"; iv2$; "-"; iv3$;
'PRINT "Laboratorio:"; labo$; "-"; re$; "-"; ped$; "-"; medi$; "-"; misc$; "-"; quim$; "-"; compo1$; "-"; compo2$; "-"; compo3$; "-"; compo4$; "-"; compo5$; "-"; compo6$; "-"; preg$
'PRINT : SLEEP 3
LSET codigo2$ = codigo$
LSET mini12$ = mini1$
LSET mini22$ = mini2$
LSET maxi12$ = maxi1$
LSET maxi22$ = maxi2$
LSET ubic2$ = MKI$(VAL(ubic$))
LSET nomb2$ = nomb$
LSET obser2$ = obser$
LSET acti2$ = acti$
LSET exi12$ = exi1$
LSET exi22$ = exi2$
LSET exi32$ = exi3$
LSET exi42$ = MKI$(0)
LSET pv12$ = MKD$(CVS(pv1$))
LSET pv22$ = MKD$(CVS(pv2$))
LSET pv32$ = MKD$(CVS(pv3$))
LSET pv42$ = MKD$(0)
LSET cos12$ = MKD$(CVS(cos1$))
LSET cos22$ = MKD$(CVS(cos2$))
LSET cos32$ = MKD$(CVS(cos3$))
LSET cos42$ = MKD$(0)
LSET iv12$ = iv1$
LSET iv22$ = iv2$
LSET iv32$ = iv3$
LSET iv42$ = " "
LSET of12$ = of1$
LSET of22$ = of2$
LSET of32$ = of3$
LSET labo2$ = MKI$(VAL(labo$))
LSET re2$ = re$
LSET ped2$ = MKI$(0)
LSET medi2$ = medi$
LSET misc2$ = misc$
LSET quim2$ = quim$
LSET compo12$ = MKI$(VAL(compo1$))
LSET compo22$ = MKI$(VAL(compo2$))
LSET compo32$ = MKI$(VAL(compo3$))
LSET compo42$ = MKI$(VAL(compo4$))
LSET compo52$ = MKI$(VAL(compo5$))
LSET compo62$ = MKI$(VAL(compo6$))
LSET preg2$ = preg$
LSET saldo$ = MKS$(0)
LSET upc$ = MKD$(0)
LSET reserva$ = SPACE$(18)
PUT #2, k
NEXT k
CLOSE
TYPE NewMaster
Ubicacion AS STRING * 2
Nombre AS STRING * 35
Codi AS STRING * 6
Mix1 AS STRING * 2
Lab AS STRING * 2
Oferta1 AS STRING * 1
Oferta2 AS STRING * 1
Oferta3 AS STRING * 1
Comp1 AS STRING * 2
Comp2 AS STRING * 2
Comp3 AS STRING * 2
Comp4 AS STRING * 2
Comp5 AS STRING * 2
Comp6 AS STRING * 2
END TYPE
DIM Xname(max1%) AS NewMaster
KILL "F:\kevinprg\kevindat\*.idx"

' indexado de nombre
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 18 AS reserva$
max1% = LOF(1) / 200
FOR k = 1 TO max1%: GET #1, k
Xname(k).Ubicacion = MKI$(k): Xname(k).Nombre = nomb2$
NEXT k: CLOSE
OPEN "F:\kevinprg\kevindat\nombre.idx" FOR RANDOM AS #1 LEN = 8
FIELD #1, 6 AS codigo$, 2 AS mini1$: CLS
FOR k = 1 TO max1%
LSET codigo$ = MID$(Xname(k).Nombre, 1, 6): LSET mini1$ = Xname(k).Ubicacion: PUT #1, k
NEXT k: CLOSE

' indexado de codigo
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 18 AS reserva$
max1% = LOF(1) / 200
FOR k = 1 TO max1%: GET #1, k
Xname(k).Ubicacion = MKI$(k): Xname(k).Codi = codigo2$
NEXT k: CLOSE :
FOR a% = 1 TO max1% - 1: FOR b% = a% + 1 TO max1%: LOCATE 14, 45: PRINT a%
IF (Xname(b%).Codi) < (Xname(a%).Codi) THEN
SWAP Xname(a%).Ubicacion, Xname(b%).Ubicacion
SWAP Xname(a%).Codi, Xname(b%).Codi
END IF
NEXT b%: NEXT a%
OPEN "F:\kevinprg\kevindat\codigo.idx" FOR RANDOM AS #1 LEN = 8
FIELD #1, 6 AS codigo$, 2 AS mini1$: CLS
FOR k = 1 TO max1%
LSET codigo$ = Xname(k).Codi: LSET mini1$ = Xname(k).Ubicacion: PUT #1, k
NEXT k: CLOSE


' indexado de laboratorio
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 18 AS reserva$
max1% = LOF(1) / 200
FOR k = 1 TO max1%: GET #1, k
Xname(k).Mix1 = MKI$(k): Xname(k).Nombre = nomb2$: Xname(k).Lab = labo2$
'PRINT MID$(STR$(CVI(Xname(k).Lab)), 2, 2): SLEEP
NEXT k: CLOSE
FOR a% = 1 TO max1% - 1: FOR b% = a% + 1 TO max1%: LOCATE 14, 45: PRINT a%
IF (MID$(STR$(CVI(Xname(b%).Lab)), 2, 2) + Xname(b%).Nombre) < (MID$(STR$(CVI(Xname(a%).Lab)), 2, 2) + Xname(a%).Nombre) THEN
SWAP Xname(a%).Lab, Xname(b%).Lab
SWAP Xname(a%).Mix1, Xname(b%).Mix1
SWAP Xname(a%).Nombre, Xname(b%).Nombre
END IF
NEXT b%: NEXT a%
OPEN "F:\kevinprg\kevindat\lab.idx" FOR RANDOM AS #1 LEN = 4
FIELD #1, 2 AS codigo$, 2 AS mini1$: CLS
FOR k = 1 TO max1%
LSET codigo$ = Xname(k).Lab: LSET mini1$ = Xname(k).Mix1: PUT #1, k
NEXT k: CLOSE

' indexado por ubicacion
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 18 AS reserva$
max1% = LOF(1) / 200
FOR k = 1 TO max1%: GET #1, k
Xname(k).Mix1 = MKI$(k): Xname(k).Nombre = nomb2$: Xname(k).Ubicacion = ubic2$
NEXT k: CLOSE :
FOR a% = 1 TO max1% - 1: FOR b% = a% + 1 TO max1%: LOCATE 14, 45: PRINT a%
IF (MID$(STR$(CVI(Xname(b%).Ubicacion)), 2, 2) + Xname(b%).Nombre) < (MID$(STR$(CVI(Xname(a%).Ubicacion)), 2, 2) + Xname(a%).Nombre) THEN
SWAP Xname(a%).Ubicacion, Xname(b%).Ubicacion
SWAP Xname(a%).Mix1, Xname(b%).Mix1
SWAP Xname(a%).Nombre, Xname(b%).Nombre
END IF
NEXT b%: NEXT a%
OPEN "F:\kevinprg\kevindat\ubica.idx" FOR RANDOM AS #1 LEN = 4
FIELD #1, 2 AS codigo$, 2 AS mini1$: CLS
FOR k = 1 TO max1%
LSET codigo$ = Xname(k).Ubicacion: LSET mini1$ = Xname(k).Mix1: PUT #1, k
NEXT k: CLOSE

' indexado por oferta
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 18 AS reserva$
max1% = LOF(1) / 200
FOR k = 1 TO max1%: GET #1, k
Xname(k).Mix1 = MKI$(k): Xname(k).Oferta1 = of12$: Xname(k).Oferta2 = of22$: Xname(k).Oferta3 = of32$
NEXT k: CLOSE
OPEN "F:\kevinprg\kevindat\oferta.idx" FOR RANDOM AS #1 LEN = 5
FIELD #1, 3 AS codigo$, 2 AS mini1$: CLS
le = 1
FOR k = 1 TO max1%
IF Xname(k).Oferta1 = "@" OR Xname(k).Oferta2 = "#" OR Xname(k).Oferta3 = "$" THEN
LSET codigo$ = Xname(k).Oferta1 + Xname(k).Oferta2 + Xname(k).Oferta3: LSET mini1$ = MKI$(k): PUT #1, le: le = le + 1
END IF
NEXT k: CLOSE

' indexado por composicion
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 18 AS reserva$
max1% = LOF(1) / 200
FOR k = 1 TO max1%: GET #1, k
Xname(k).Mix1 = MKI$(k): Xname(k).Comp1 = compo12$: Xname(k).Comp2 = compo22$: Xname(k).Comp3 = compo32$: Xname(k).Comp4 = compo42$: Xname(k).Comp5 = compo52$: Xname(k).Comp6 = compo62$
NEXT k: CLOSE
OPEN "F:\kevinprg\kevindat\compos.idx" FOR RANDOM AS #1 LEN = 2
FIELD #1, 2 AS mini1$: CLS
le = 1
FOR k = 1 TO max1%
IF CVI(Xname(k).Comp1) <> 1 OR CVI(Xname(k).Comp2) <> 1 OR CVI(Xname(k).Comp3) <> 1 OR CVI(Xname(k).Comp4) <> 1 OR CVI(Xname(k).Comp5) <> 1 OR CVI(Xname(k).Comp6) <> 1 THEN
LSET mini1$ = Xname(k).Mix1: PUT #1, le: le = le + 1
END IF
NEXT k: CLOSE
END

