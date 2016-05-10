CLOSE
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
OPEN "f:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
FIELD #2, 35 AS codigo$, 2 AS sitio$: max% = LOF(2) / 37
OPEN "lpt1" FOR RANDOM AS #10
WIDTH #10, 255
PRINT #10, STRING$(60, "²"); CHR$(10); CHR$(10);
FOR k = 1 TO max%
     GET #2, k: GET #1, CVI(sitio$): LOCATE 1, 1: PRINT k
     IF MID$(nomb2$, 1, 3) = "AD." THEN
          PRINT nomb2$; k;
          PRINT #10, nomb2$;
          PRINT #10, USING "#####,.##"; CVD(pv32$); : PRINT "     ";
          PRINT #10, USING "#####,.##"; CVD(pv42$); : PRINT "     ";
          PRINT #10, USING "#####,.##"; CVD(cos42$); : PRINT "     ";
         
          PRINT #10, CHR$(10);
     END IF
NEXT k
PRINT #10, STRING$(60, "²"); CHR$(10); CHR$(10);
PRINT #10, CHR$(12);

