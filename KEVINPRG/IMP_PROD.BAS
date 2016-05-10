OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
max% = LOF(1) / 200: CLS
OPEN "lpt1" FOR OUTPUT AS #10
WIDTH #10, 255
FOR k = 1 TO max%
   LOCATE 1, 1: PRINT k
   GET #1, k
   IF CVD(pv12$) = CVD(pv22$) AND CVD(pv12$) <> 0 THEN
     LOCATE 2 + van%, 1
     PRINT "³"; nomb2$; "³"; k; " * 1=2 * "; "Exis: "; (CVI(exi12$) + CVI(exi22$) + CVI(exi32$))
     PRINT #10, "³"; nomb2$; "³"; k; " * 1=2 * "; "Exis: "; (CVI(exi12$) + CVI(exi22$) + CVI(exi32$))
     van% = van% + 1: iguales% = iguales% + 1
     IF van% = 20 THEN SLEEP: CLS : van% = 0
     GOTO Res
   END IF
   IF CVD(pv12$) = CVD(pv32$) AND CVD(pv12$) <> 0 THEN
     LOCATE 2 + van%, 1
     PRINT "³"; nomb2$; "³"; k; " * 1=3 * "; "Exis: "; (CVI(exi12$) + CVI(exi22$) + CVI(exi32$))
     PRINT #10, "³"; nomb2$; "³"; k; " * 1=2 * "; "Exis: "; (CVI(exi12$) + CVI(exi22$) + CVI(exi32$))
     van% = van% + 1: iguales% = iguales% + 1
     IF van% = 20 THEN SLEEP: CLS : van% = 0
   END IF
Res:
NEXT k
CLOSE
END

