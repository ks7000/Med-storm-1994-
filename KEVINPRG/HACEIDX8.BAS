DEFINT A-Z
'indexado de ofertas
KILL "f:\kevinprg\kevindat\oferta*.idx"

REM---------------------------------------------oferta1
OPEN "f:\kevinprg\kevindat\new.dbf" FOR RANDOM AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
max1% = LOF(1) / 200
OPEN "f:\kevinprg\kevindat\oferta1.idx" FOR RANDOM AS #2 LEN = 8
FIELD #2, 6 AS codigo$, 2 AS sitio$: lleva = 0
FOR k = 1 TO max1%: GET #1, k
IF acti2$ = " " AND of12$ = "@" THEN
     lleva = lleva + 1: RSET sitio$ = MKI$(k): RSET codigo$ = MID$(nomb2$, 1, 6): PUT #2, lleva
     LOCATE 1, 1: PRINT k
END IF
NEXT k: CLOSE : CLS : LOCATE 10, 1: PRINT TIMER
OPEN "f:\kevinprg\kevindat\oferta1.idx" FOR RANDOM AS #1 LEN = 8
FIELD #1, 6 AS codigo$, 2 AS sitio$: N = LOF(1) / 8: Salto = N
REM $DYNAMIC
DIM xa(N) AS STRING * 6, xb(N) AS STRING * 2
FOR k = 1 TO N: GET #1, k: xa(k) = codigo$: xb(k) = sitio$: NEXT

WHILE Salto > 1
     LOCATE 1, 1: PRINT Salto
     Salto = Salto / 2
     DO
          Sw = 1
          FOR J = 1 TO N - Salto
               I = J + Salto: LOCATE 3, 1: PRINT J
               IF xa(J) > xa(I) THEN
                    SWAP xa(J), xa(I): SWAP xb(J), xb(I): Sw = 0
               END IF
          NEXT J
     LOOP UNTIL Sw = 1
WEND
LOCATE 11, 1: PRINT TIMER

FOR k = 1 TO N: RSET codigo$ = xa(k): RSET sitio$ = xb(k): PUT #1, k: PRINT "³"; codigo$; "³"
NEXT k: CLOSE : SLEEP


REM---------------------------------------------oferta2
OPEN "f:\kevinprg\kevindat\new.dbf" FOR RANDOM AS #1 LEN = 200: CLS
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
max1% = LOF(1) / 200
OPEN "f:\kevinprg\kevindat\oferta2.idx" FOR RANDOM AS #2 LEN = 8
FIELD #2, 6 AS codigo$, 2 AS sitio$: lleva = 0
FOR k = 1 TO max1%: GET #1, k
IF acti2$ = " " AND of22$ = "#" THEN
     lleva = lleva + 1: RSET sitio$ = MKI$(k): RSET codigo$ = MID$(nomb2$, 1, 6): PUT #2, lleva
     LOCATE 1, 1: PRINT k
END IF
NEXT k: CLOSE : CLS : LOCATE 10, 1: PRINT TIMER
OPEN "f:\kevinprg\kevindat\oferta2.idx" FOR RANDOM AS #1 LEN = 8
FIELD #1, 6 AS codigo$, 2 AS sitio$: N = LOF(1) / 8: Salto = N
ERASE xa, xb: DIM xa(N) AS STRING * 6, xb(N) AS STRING * 2
FOR k = 1 TO N: GET #1, k: xa(k) = codigo$: xb(k) = sitio$: NEXT

WHILE Salto > 1
     LOCATE 1, 1: PRINT Salto
     Salto = Salto / 2
     DO
          Sw = 1
          FOR J = 1 TO N - Salto
               I = J + Salto: LOCATE 3, 1: PRINT J
               IF xa(J) > xa(I) THEN
                    SWAP xa(J), xa(I): SWAP xb(J), xb(I): Sw = 0
               END IF
          NEXT J
     LOOP UNTIL Sw = 1
WEND
LOCATE 11, 1: PRINT TIMER
FOR k = 1 TO N: RSET codigo$ = xa(k): RSET sitio$ = xb(k): PUT #1, k: PRINT "³"; codigo$; "³"
NEXT k: CLOSE : SLEEP



REM---------------------------------------------oferta3
OPEN "f:\kevinprg\kevindat\new.dbf" FOR RANDOM AS #1 LEN = 200: CLS
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
max1% = LOF(1) / 200
OPEN "f:\kevinprg\kevindat\oferta3.idx" FOR RANDOM AS #2 LEN = 8
FIELD #2, 6 AS codigo$, 2 AS sitio$: lleva = 0
FOR k = 1 TO max1%: GET #1, k
IF acti2$ = " " AND of32$ = "$" THEN
     lleva = lleva + 1: RSET sitio$ = MKI$(k): RSET codigo$ = MID$(nomb2$, 1, 6): PUT #2, lleva
     LOCATE 1, 1: PRINT k
END IF
NEXT k: CLOSE : CLS : LOCATE 10, 1: PRINT TIMER
OPEN "f:\kevinprg\kevindat\oferta3.idx" FOR RANDOM AS #1 LEN = 8
FIELD #1, 6 AS codigo$, 2 AS sitio$: N = LOF(1) / 8: Salto = N
ERASE xa, xb: DIM xa(N) AS STRING * 6, xb(N) AS STRING * 2
FOR k = 1 TO N: GET #1, k: xa(k) = codigo$: xb(k) = sitio$: NEXT

WHILE Salto > 1
     LOCATE 1, 1: PRINT Salto
     Salto = Salto / 2
     DO
          Sw = 1
          FOR J = 1 TO N - Salto
               I = J + Salto: LOCATE 3, 1: PRINT J
               IF xa(J) > xa(I) THEN
                    SWAP xa(J), xa(I): SWAP xb(J), xb(I): Sw = 0
               END IF
          NEXT J
     LOOP UNTIL Sw = 1
WEND
LOCATE 11, 1: PRINT TIMER
FOR k = 1 TO N: RSET codigo$ = xa(k): RSET sitio$ = xb(k): PUT #1, k: PRINT "³"; codigo$; "³"
NEXT k: CLOSE : SLEEP: END

