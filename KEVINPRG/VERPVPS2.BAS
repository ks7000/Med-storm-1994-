CLOSE
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
max% = LOF(1) / 200
CLS : l = 0
DIM total AS DOUBLE
total = 0
totpvp$ = MKD$(0): totcos$ = MKD$(0): totuni$ = MKD$(0)
FOR k = 1 TO max%
     GET #1, k: GOSUB imprime
        'IF CVD(pv12$) >= 5050 AND CVD(pv12$) <= 5150 THEN GOSUB imprime
        'IF CVD(pv22$) >= 5050 AND CVD(pv22$) <= 5150 THEN GOSUB imprime
        'IF CVD(pv32$) >= 5050 AND CVD(pv32$) <= 5150 THEN GOSUB imprime
        'IF CVD(pv42$) >= 5050 AND CVD(pv42$) <= 5150 THEN GOSUB imprime
        'IF CVD(pv12$) = 1100 THEN PRINT nomb2$
        'IF CVD(pv22$) = 1100 THEN PRINT nomb2$
        'IF CVD(pv32$) = 1100 THEN PRINT nomb2$
        'IF CVD(pv42$) = 1100 THEN PRINT nomb2$
NEXT k
      PRINT
      PRINT "Total";
      PRINT USING "#################,.##"; total
      PRINT USING "#################,.##"; CVD(totpvp$)
      PRINT USING "#################,.##"; CVD(totcos$)
      PRINT USING "#################,.##"; CVL(totuni$)
CLOSE

END
imprime:
    PRINT nomb2$, (CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$))
    IF CVI(exi12$) > 0 THEN
      'PRINT CVD(pv12$);
      total = total + CVI(exi12$) * CVD(pv12$)
    END IF
    IF CVI(exi22$) > 0 THEN
      'PRINT CVD(pv22$);
      total = total + CVI(exi22$) * CVD(pv22$)
    END IF
    IF (CVI(exi32$) + CVI(exi42$)) > 0 THEN
      'PRINT CVD(pv32$);
      total = total + (CVI(exi32$) + CVI(exi42$)) * CVD(pv32$)
    END IF
    l = l + 1
    totpvp$ = MKD$(CVD(totpvp$) + (CVI(exi12$) * CVD(pv12$)) + (CVI(exi22$) * CVD(pv22$)) + (CVI(exi32$) * CVD(pv32$)) + (CVI(exi42$) * CVD(pv32$)))
    totcos$ = MKD$(CVD(totcos$) + (CVI(exi12$) * CVD(cos12$)) + (CVI(exi22$) * CVD(cos22$)) + (CVI(exi32$) * CVD(cos32$)) + (CVI(exi42$) * CVD(cos42$)))
    totuni$ = MKL$(CVL(totuni$) + CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$))
   IF l > 20 THEN
      PRINT
      PRINT "Total";
      PRINT USING "#################,.##"; total
      SLEEP
      CLS : l = 0
   END IF

   SELECT CASE CVI(ubic2$)
      CASE 6
      CASE 99
      CASE 17
      CASE 16
      CASE 2
      CASE 3
      CASE 20
      CASE 98
      CASE 10
      CASE 96
      CASE 21
      CASE 1
      CASE 19
      CASE 4
      CASE 5
      CASE 18
      CASE 7
      CASE 22
      CASE 97
      CASE 8
      CASE 9
      CASE 11
      CASE 12
      CASE 13
      CASE 14
      CASE 15
      CASE ELSE
         PRINT CVI(ubic2$): SLEEP
   END SELECT
RETURN

