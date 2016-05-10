DECLARE FUNCTION CalculaMes% (mes$)
CONST true = 1: false = NOT true
CLOSE
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
OPEN "f:\kevinprg\kevindat\ubica.idx" FOR RANDOM SHARED AS #2 LEN = 10
FIELD #2, 8 AS codigo$, 2 AS sitio$: max% = LOF(2) / 10: CLS
OPEN "lpt1" FOR RANDOM AS #10
WIDTH #10, 255
FOR k = 1 TO max%
     GET #2, k: GET #1, CVI(sitio$): imprimir = false: LOCATE 1, 1: PRINT k; nomb2$
     venci1% = CalculaMes%(ven11$ + ven12$): venci2% = CalculaMes%(ven21$ + ven22$): venci3% = CalculaMes%(ven31$ + ven32$): venci4% = CalculaMes%(ven41$ + ven42$)
     LOCATE 1, 40: PRINT venci1%; venci2%; venci3%; venci4%
     IF venci1% >= 113 AND venci1% <= 116 THEN imprimir = true
     IF venci2% >= 113 AND venci2% <= 116 THEN imprimir = true
     IF venci3% >= 113 AND venci3% <= 116 THEN imprimir = true
     IF venci4% >= 113 AND venci4% <= 116 THEN imprimir = true
     IF (CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$)) = 0 THEN imprimir = false
     IF imprimir = true THEN
          LOCATE 20, 20: PRINT k; nomb2$; CVI(ven12$); CVI(ven22$); CVI(ven32$); CVI(ven42$)
          PRINT #10, nomb2$; : PRINT #10, CHR$(10);
     END IF
NEXT k
CLOSE

FUNCTION CalculaMes% (mes$)
CalculaMes% = 0
'LOCATE 20, 20: PRINT MID$(mes$, 1, 1); CVI(MID$(mes$, 2, 2)): SLEEP
IF MID$(mes$, 1, 1) = "A" THEN CalculaMes% = 10 + 12 * (CVI(MID$(mes$, 2, 2)) - 1990): EXIT FUNCTION
IF MID$(mes$, 1, 1) = "B" THEN CalculaMes% = 11 + 12 * (CVI(MID$(mes$, 2, 2)) - 1990): EXIT FUNCTION
IF MID$(mes$, 1, 1) = "C" THEN CalculaMes% = 12 + 12 * (CVI(MID$(mes$, 2, 2)) - 1990): EXIT FUNCTION
IF VAL(MID$(mes$, 1, 1)) > 0 AND VAL(MID$(mes$, 1, 1)) < 10 THEN
     CalculaMes% = VAL(MID$(mes$, 1, 1)) + 12 * (CVI(MID$(mes$, 2, 2)) - 1990): EXIT FUNCTION
END IF
EXIT FUNCTION
END FUNCTION

