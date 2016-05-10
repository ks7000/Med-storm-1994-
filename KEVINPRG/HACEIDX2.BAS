DEFINT A-Z
CLS
'indexado de ubicacion
KILL "F:\kevinprg\kevindat\ubica.idx"
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$
max1% = LOF(1) / 200
OPEN "F:\kevinprg\kevindat\ubica.idx" FOR RANDOM AS #2 LEN = 10
FIELD #2, 8 AS codigo$, 2 AS sitio$: lleva = 0
FOR k = 1 TO max1%: GET #1, k
IF acti2$ = " " THEN
     lleva = lleva + 1: RSET sitio$ = MKI$(k): RSET codigo$ = ubic2$ + MID$(nomb2$, 1, 6): PUT #2, lleva
     LOCATE 1, 1: PRINT k
END IF
NEXT k: CLOSE : CLS : LOCATE 10, 1: PRINT TIMER
OPEN "F:\kevinprg\kevindat\ubica.idx" FOR RANDOM AS #1 LEN = 10
FIELD #1, 8 AS codigo$, 2 AS sitio$: N = LOF(1) / 10: Salto = N
DIM xa(N) AS STRING * 8, xb(N) AS STRING * 2
FOR k = 1 TO N: GET #1, k: xa(k) = codigo$: xb(k) = sitio$: NEXT

WHILE Salto > 1
     LOCATE 1, 1: PRINT Salto
     Salto = Salto / 2
     DO
          Sw = 1
          FOR J = 1 TO N - Salto
               I = J + Salto: LOCATE 3, 1: PRINT J
               IF xa(J) > xa(I) THEN
                    SWAP xa(J), xa(I)
                    SWAP xb(J), xb(I)
                    Sw = 0
               END IF
          NEXT J
     LOOP UNTIL Sw = 1
WEND
LOCATE 11, 1: PRINT TIMER: SLEEP

FOR k = 1 TO N
RSET codigo$ = xa(k): RSET sitio$ = xb(k)
PUT #1, k: PRINT "³"; codigo$; "³"
NEXT k: CLOSE : END

