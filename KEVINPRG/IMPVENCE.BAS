'visualiza y/o imprime vencimientos
CLOSE
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
OPEN "f:\kevinprg\kevindat\ubica.idx" FOR RANDOM SHARED AS #2 LEN = 10
FIELD #2, 8 AS codigo$, 2 AS sitio$
max% = LOF(2) / 10: CLS : l = 0
OPEN "LPT1:" FOR RANDOM AS #10
WIDTH #10, 255
FOR k = 1 TO max%
     GET #2, k: GET #1, CVI(sitio$): LOCATE 1, 1: PRINT k
     IF preg$ = "*" AND MID$(nomb2$, 1, 1) <> "-" THEN
          IF CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$) > 0 THEN
               'PRINT nomb2$; " V1="; ven11$; CVI(ven12$);
               'PRINT " V2="; ven21$; CVI(ven22$);
               'PRINT " V3="; ven31$; CVI(ven32$);
               'PRINT " V4="; ven41$; CVI(ven42$): SLEEP 1
               PRINT #10, nomb2$; " V1="; ven11$; CVI(ven12$);
               PRINT #10, " V2="; ven21$; CVI(ven22$);
               PRINT #10, " V3="; ven31$; CVI(ven32$);
               PRINT #10, " V4="; ven41$; CVI(ven42$); : PRINT #10, CHR$(10);
               l = l + 1: IF l > 50 THEN PRINT #10, CHR$(12); : l = 0
          END IF
     END IF
NEXT k
CLOSE

