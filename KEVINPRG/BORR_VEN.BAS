DEFINT A-Z
DECLARE FUNCTION CalculaMes% (mes$)
CONST true = 1: false = NOT true
CLOSE
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
max% = LOF(1) / 200
FOR k = 1 TO max%
     GET #1, k
     LOCATE 1, 1: PRINT k
     RSET ven11$ = " "
     RSET ven12$ = MKI$(0)
     RSET ven21$ = " "
     RSET ven22$ = MKI$(0)
     RSET ven31$ = " "
     RSET ven32$ = MKI$(0)
     RSET ven41$ = " "
     RSET ven42$ = MKI$(0)
     PUT #1, k
NEXT k
CLOSE

DEFSNG A-Z
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

