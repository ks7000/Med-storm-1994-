DEFINT A-Z
' indexado de nombre
KILL "F:\kevinprg\kevindat\nombre.idx"
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$
max1% = LOF(1) / 200
OPEN "F:\kevinprg\kevindat\nombre.idx" FOR RANDOM AS #2 LEN = 37
FIELD #2, 35 AS codigo$, 2 AS sitio$: lleva = 0
FOR k = 1 TO max1%: GET #1, k
IF acti2$ = " " THEN
     lleva = lleva + 1: RSET sitio$ = MKI$(k): RSET codigo$ = nomb2$: PUT #2, lleva
     LOCATE 1, 1: PRINT k
END IF
NEXT k: CLOSE : CLS : LOCATE 10, 1: PRINT TIMER
DIM au1 AS STRING * 35, au2 AS STRING * 35, au3 AS STRING * 2, au4 AS STRING * 2
OPEN "F:\kevinprg\kevindat\nombre.idx" FOR RANDOM AS #1 LEN = 37
FIELD #1, 35 AS codigo$, 2 AS sitio$
N = LOF(1) / 37: Salto = N

WHILE Salto > 1
     LOCATE 1, 1: PRINT Salto
     Salto = Salto / 2
     DO
          Sw = 1
          FOR J = 1 TO N - Salto
               I = J + Salto
               GET #1, J: au1 = codigo$: au3 = sitio$
               GET #1, I: au2 = codigo$: au4 = sitio$
               IF au1 > au2 THEN
                    RSET codigo$ = au2: RSET sitio$ = au4: PUT #1, J
                    RSET codigo$ = au1: RSET sitio$ = au3: PUT #1, I
                    Sw = 0
               END IF
          NEXT J
     LOOP UNTIL Sw = 1
WEND
CLOSE : END
LOCATE 11, 1: PRINT TIMER: SLEEP

FOR k = 1 TO max1%
GET #1, k: PRINT "³"; codigo$; "³"
NEXT k: CLOSE
END

