'IF num3% = 1 THEN
'Box 4, 4, 16, 76: IF Fun(12) = "si" THEN Center 9, " Reescribiendo el Archivo Maestro. " ELSE Center 9, " Organizando , Respaldando y Reescribiendo el Archivo Maestro. "
'Center 10, " Por Favor Espere SIN PULSAR Ninguna Tecla ... ": LOCATE 14, 27: PRINT " Accesando el Registro:       de "; num2%
'END IF
'IF num3% = 2 THEN Center 25, " Salvando, Por Favor Espere . . . "
'OPEN "A:\maestro.dbf" FOR RANDOM AS #1 LEN = 200: FIELD #1, 6 AS codigo$, 2 AS mini1$, 2 AS MINI2$, 4 AS MAXI1$, 4 AS maxi2$, 2 AS ubic$, 35 AS NOMB$, 20 AS OBSER$, 1 AS ACTI$, 4 AS EXI1$, 4 AS EXI2$, 4 AS EXI3$, 10 AS PV1$, 10 AS PV2$, 10 AS PV3$, 10 AS COS1$, 10 AS COS2$, 10 AS COS3$, 8 AS VEN1$, 8 AS VEN2$, 8 AS VEN3$, 1 AS IV1$, 1 AS IV2$, 1 AS IV3$, 1 AS OF1$, 1 AS OF2$, 1 AS OF3$, 2 AS LABO$, 1 AS re$, 2 AS PED$, 1 AS MEDI$, 1 AS MISC$, 1 AS QUIM$, 2 AS COMPO1$, 2 AS COMPO2$, 2 AS COMPO3$, 2 AS COMPO4$, 2 AS COMPO5$, 2 AS COMPO6$, 1 AS preg$
'FOR k = num1% TO num2%: IF num3% = 1 THEN LOCATE 14, 51: PRINT k
'IF Fun(12) = "si" THEN GOTO coloca
'IF VAL(Xname(k).Exis1) = 0 THEN Xname(k).Vence1 = ""
'IF VAL(Xname(k).Exis2) = 0 THEN Xname(k).Vence2 = ""
'IF VAL(Xname(k).Exis3) = 0 THEN Xname(k).Vence3 = ""
'IF VAL(Xname(k).Pvp1) > VAL(Xname(k).Pvp2) AND VAL(Xname(k).Pvp1) < VAL(Xname(k).Pvp3) THEN GOSUB Cambia1y2: GOTO coloca
'IF VAL(Xname(k).Pvp1) < VAL(Xname(k).Pvp2) AND VAL(Xname(k).Pvp2) < VAL(Xname(k).Pvp3) THEN GOTO coloca
'IF VAL(Xname(k).Pvp1) < VAL(Xname(k).Pvp2) AND VAL(Xname(k).Pvp1) > VAL(Xname(k).Pvp3) THEN GOSUB Cambia1y2: GOSUB Cambia1y3: GOTO coloca
'IF VAL(Xname(k).Pvp1) > VAL(Xname(k).Pvp2) AND VAL(Xname(k).Pvp2) < VAL(Xname(k).Pvp3) THEN GOSUB Cambia1y2: GOSUB Cambia2y3: GOTO coloca
'IF VAL(Xname(k).Pvp1) < VAL(Xname(k).Pvp2) AND VAL(Xname(k).Pvp2) > VAL(Xname(k).Pvp3) THEN GOSUB Cambia2y3: GOTO coloca
'IF VAL(Xname(k).Pvp1) > VAL(Xname(k).Pvp2) AND VAL(Xname(k).Pvp2) > VAL(Xname(k).Pvp3) THEN GOSUB Cambia1y3: GOTO coloca
'IF VAL(Xname(k).Pvp1) = VAL(Xname(k).Pvp2) AND VAL(Xname(k).Pvp2) < VAL(Xname(k).Pvp3) THEN GOTO coloca
'IF VAL(Xname(k).Pvp1) = VAL(Xname(k).Pvp3) AND VAL(Xname(k).Pvp2) > VAL(Xname(k).Pvp3) THEN GOSUB Cambia2y3: GOTO coloca
'IF VAL(Xname(k).Pvp1) > VAL(Xname(k).Pvp2) AND VAL(Xname(k).Pvp2) = VAL(Xname(k).Pvp3) THEN GOSUB Cambia1y3: GOTO coloca
'IF VAL(Xname(k).Pvp1) = VAL(Xname(k).Pvp2) AND VAL(Xname(k).Pvp2) > VAL(Xname(k).Pvp3) THEN GOSUB Cambia1y3: GOTO coloca
'IF VAL(Xname(k).Pvp1) = VAL(Xname(k).Pvp3) AND VAL(Xname(k).Pvp2) < VAL(Xname(k).Pvp3) THEN GOSUB Cambia1y2: GOTO coloca
'IF VAL(Xname(k).Pvp2) = VAL(Xname(k).Pvp3) AND VAL(Xname(k).Pvp1) < VAL(Xname(k).Pvp3) THEN GOTO coloca
'coloca: LSET codigo$ = Xname(k).Codi: RSET mini1$ = Xname(k).Min1: RSET MINI2$ = Xname(k).Min2: RSET MAXI1$ = Xname(k).max1: RSET maxi2$ = Xname(k).max2: RSET ubic$ = Xname(k).Ubicacion: LSET NOMB$ = Xname(k).Nombre: LSET OBSER$ = Xname(k).Obsv: RSET ACTI$ = Xname(k).Activo: RSET EXI1$ = Xname(k).Exis1: RSET EXI2$ = Xname(k).Exis2: RSET EXI3$ = Xname(k).Exis3: RSET PV1$ = Xname(k).Pvp1: RSET PV2$ = Xname(k).Pvp2: RSET PV3$ = Xname(k).Pvp3: RSET COS1$ = Xname(k).Costo1: RSET COS2$ = Xname(k).Costo2: RSET COS3$ = Xname(k).Costo3: LSET VEN1$ = Xname(k).Vence1: LSET VEN2$ = Xname(k).Vence2: LSET VEN3$ = Xname(k).Vence3: RSET IV1$ = Xname(k).Iva1: RSET IV2$ = Xname(k).Iva2: RSET IV3$ = Xname(k).Iva3: RSET OF1$ = Xname(k).Oferta1: RSET OF2$ = Xname(k).Oferta2: RSET OF3$ = Xname(k).Oferta3: RSET LABO$ = Xname(k).Lab: RSET re$ = Xname(k).ReFalla: RSET PED$ = Xname(k).Pedido: RSET MEDI$ = Xname(k).Med: RSET MISC$ = Xname(k).Mis: RSET QUIM$ = Xname(k).Qui: RSET COMPO1$ = Xname(k).Comp1: RSET COMPO2$ = Xname(k).Comp2: RSET COMPO3$ = Xname(k).Comp3: RSET COMPO4$ = Xname(k).Comp4: RSET COMPO5$ = Xname(k).Comp5: RSET COMPO6$ = Xname(k).Comp6: RSET preg$ = Xname(k).PreVe: PUT #1, k
'NEXT k: Fun(12) = "no": CLOSE #1: OPEN "A:\kevindat\maximo.dat" FOR OUTPUT AS #3: PRINT #3, MaxArt: PRINT #3, Prov1: PRINT #3, Prov2: PRINT #3, Prov3: CLOSE #3: EXIT SUB
'Cambia1y2: Auxi$ = Xname(k).Pvp1: Xname(k).Pvp1 = Xname(k).Pvp2: Xname(k).Pvp2 = Auxi$: Auxi$ = Xname(k).Costo1: Xname(k).Costo1 = Xname(k).Costo2: Xname(k).Costo2 = Auxi$: Auxi$ = Xname(k).Iva1: Xname(k).Iva1 = Xname(k).Iva2: Xname(k).Iva2 = Auxi$: Auxi$ = Xname(k).Vence1: Xname(k).Vence1 = Xname(k).Vence2: Xname(k).Vence2 = Auxi$: Auxi$ = Xname(k).Exis1: Xname(k).Exis1 = Xname(k).Exis2: Xname(k).Exis2 = Auxi$: RETURN
'Cambia1y3: Auxi$ = Xname(k).Pvp1: Xname(k).Pvp1 = Xname(k).Pvp3: Xname(k).Pvp3 = Auxi$: Auxi$ = Xname(k).Costo1: Xname(k).Costo1 = Xname(k).Costo3: Xname(k).Costo3 = Auxi$: Auxi$ = Xname(k).Iva1: Xname(k).Iva1 = Xname(k).Iva3: Xname(k).Iva3 = Auxi$: Auxi$ = Xname(k).Vence1: Xname(k).Vence1 = Xname(k).Vence3: Xname(k).Vence3 = Auxi$: Auxi$ = Xname(k).Exis1: Xname(k).Exis1 = Xname(k).Exis3: Xname(k).Exis3 = Auxi$: RETURN
'Cambia2y3: Auxi$ = Xname(k).Pvp2: Xname(k).Pvp2 = Xname(k).Pvp3: Xname(k).Pvp3 = Auxi$: Auxi$ = Xname(k).Costo2: Xname(k).Costo2 = Xname(k).Costo3: Xname(k).Costo3 = Auxi$: Auxi$ = Xname(k).Iva2: Xname(k).Iva2 = Xname(k).Iva3: Xname(k).Iva3 = Auxi$: Auxi$ = Xname(k).Vence2: Xname(k).Vence2 = Xname(k).Vence3: Xname(k).Vence3 = Auxi$: Auxi$ = Xname(k).Exis2: Xname(k).Exis2 = Xname(k).Exis3: Xname(k).Exis3 = Auxi$: RETURN

