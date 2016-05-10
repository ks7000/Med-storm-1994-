CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
max% = LOF(1) / 200: CLS
FOR k = 1 TO max%
     GET #1, k: LOCATE 1, 1: PRINT k
     'LOCATE 2, 1: PRINT CVI(ven12$); CVI(ven22$); CVI(ven32$); CVI(ven42$); SPACE$(40)
     'IF CVI(ven12$) = 1999 OR CVI(ven22$) = 1999 OR CVI(ven32$) = 1999 OR CVI(ven42$) = 1999 THEN
     IF CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$) = 0 THEN
          RSET ven12$ = "  ": RSET ven22$ = "  ": RSET ven32$ = "  ": RSET ven42$ = "  "
          RSET ven11$ = " ": RSET ven21$ = " ": RSET ven31$ = " ": RSET ven41$ = " "
          RSET preg$ = " ": PUT #1, k
     END IF
NEXT k
CLOSE

