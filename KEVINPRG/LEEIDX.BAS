DEFINT A-Z
OPEN "a:\kevindat\maestro.dbf" FOR RANDOM AS #1 LEN = 135
FIELD #1, 6 AS CODIGO$, 2 AS MINI1$, 2 AS MINI2$, 2 AS MAXI1$, 2 AS MAXI2$, 2 AS Ubic$, 35 AS NOMB$, 20 AS OBSER$, 1 AS ACTI$, 2 AS EXI1$, 2 AS EXI2$, 2 AS EXI3$, 4 AS PV1$, 4 AS PV2$, 4 AS PV3$, 4 AS COS1$, 4 AS COS2$, 4 AS COS3$, 1 AS IV1$, 1 AS IV2$, 1 AS IV3$, 1 AS OF1$, 1 AS OF2$, 1 AS OF3$, 2 AS LABO$, 1 AS RE$, 2 AS PED$, 1 AS MEDI$, 1 AS MISC$, 1 AS QUIM$, 3 AS COMPO1$, 3 AS COMPO2$, 3 AS COMPO3$, 3 AS COMPO4$, 3 AS COMPO5$, 3 AS COMPO6$, 1 AS PREG$
FOR k = 1 TO 47: CLS : GET #1, k
PRINT CODIGO$;
PRINT USING "+##"; CVI(MINI1$); CVI(MINI2$)
PRINT Ubic$; NOMB$; OBSER$; ACTI$: LOCATE 4, 50
PRINT USING "+####"; CVI(MAXI1$); CVI(MAXI2$)
PRINT "Existencias:"; : PRINT USING "####"; CVI(EXI1$); CVI(EXI2$); CVI(EXI3$)
PRINT USING "+#######,.##"; CVS(PV1$); CVS(PV2$); CVS(PV3$)
PRINT USING "+#######,.##"; CVS(COS1$); CVS(COS2$); CVS(COS3$)
PRINT IV1$; IV2$; IV3$
PRINT OF1$; OF2$; OF3$; LABO$; RE$; PED$; MEDI$; MISC$; QUIM$
PRINT COMPO1$; COMPO2$; COMPO3$; COMPO4$; COMPO5$; COMPO6$; PREG$
DO WHILE INKEY$ = "": LOOP
NEXT k

