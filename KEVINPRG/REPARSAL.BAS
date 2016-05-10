'repara registros saldos.psi
DEFINT A-Z
CLOSE
CLS

OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
max1% = LOF(1) / 200

OPEN "F:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
FIELD #2, 35 AS nomshort$, 2 AS num$
max2% = LOF(2) / 37

OPEN "f:\kevinprg\kevindat\saldos.dat" FOR RANDOM SHARED AS #3 LEN = 45
FIELD #3, 6 AS codipsi$, 35 AS nombpsi$, 2 AS saldopsi$, 2 AS sitiopsi$
max3% = LOF(3) / 45

FOR k = 1 TO max2%
     GET #2, k: GET #1, CVI(num$)
     IF CVI(ubic2$) = 1 THEN
          encontrado% = 0
          FOR l = 1 TO max3%
               GET #3, l
               IF codipsi$ = codigo2$ THEN
                    encontrado% = 1: CLS
                    LOCATE 1, 1: PRINT k: PRINT nomb2$; obser2$
                    INPUT "Desea borrar (S/N)"; sino$: sino$ = UCASE$(sino$)
                    IF sino$ = "S" THEN
                         RSET nombpsi$ = "BORRADO" + SPACE$(28): PUT #3, l
                    ELSE
                         RSET nombpsi$ = nomb2$: PUT #3, l
                    END IF
               END IF
          NEXT l
          IF encontrado% = 0 THEN
                    CLS : LOCATE 1, 1: PRINT k: PRINT nomb2$; obser2$
                    INPUT "Desea agregar (S/N)"; sino$: sino$ = UCASE$(sino$)
                    IF sino$ = "S" THEN
                         max3% = max3% + 1
                         RSET codipsi$ = codigo2$
                         RSET nombpsi$ = nomb2$
                         RSET sitiopsi$ = num$
                         RSET saldopsi$ = MKI$(0)
                         PUT #3, max3%
                     END IF
          END IF
     END IF
NEXT k
FOR k = 1 TO max3%
     GET #3, k: GET #1, CVI(sitiopsi$)
     CLS
     LOCATE 1, 1: PRINT nomb2$; CVI(ubic2$)
     LOCATE 2, 1: PRINT nombpsi$;
     INPUT "Desea borrar (S/N)"; sino$: sino$ = UCASE$(sino$)
     IF sino$ = "S" THEN
          RSET nombpsi$ = "BORRADO" + SPACE$(28): PUT #3, k
     END IF
NEXT k

CLOSE

OPEN "f:\kevinprg\kevindat\saldos.dat" FOR RANDOM SHARED AS #3 LEN = 45
FIELD #3, 6 AS codipsi1$, 35 AS nombpsi1$, 2 AS saldopsi1$, 2 AS sitiopsi1$
max3% = LOF(3) / 45

OPEN "f:\kevinprg\kevindat\saldos.ver" FOR RANDOM SHARED AS #4 LEN = 45
FIELD #4, 6 AS codipsi2$, 35 AS nombpsi2$, 2 AS saldopsi2$, 2 AS sitiopsi2$
max4% = 0

FOR k = 1 TO max3%
     GET #3, k
     IF nombpsi1$ <> "BORRADO" + SPACE$(28) THEN
          RSET codipsi2$ = codipsi1$
          RSET nombpsi2$ = nombpsi1$
          RSET sitiopsi2$ = sitiopsi1$
          RSET saldopsi2$ = saldopsi1$
          max4% = max4% + 1: PUT #4, max4%
     END IF
NEXT k
CLOSE
KILL "f:\kevinprg\kevindat\saldos.dat"
NAME "f:\kevinprg\kevindat\saldos.ver" AS "f:\kevinprg\kevindat\saldos.dat"








DIM au1 AS STRING * 6, au2    AS STRING * 6
DIM au3 AS STRING * 35, au4   AS STRING * 35
DIM au5 AS STRING * 2, au6    AS STRING * 2
DIM au7 AS STRING * 2, au8    AS STRING * 2

OPEN "f:\kevinprg\kevindat\saldos.dat" FOR RANDOM SHARED AS #1 LEN = 45
FIELD #1, 6 AS codipsi$, 35 AS nombpsi$, 2 AS saldopsi$, 2 AS sitiopsi$
N = LOF(1) / 45: salto = N

WHILE salto > 1
     salto = salto / 2
     DO
          Sw = 1
          FOR J = 1 TO N - salto
               I = J + salto
               GET #1, J: au1 = codipsi$: au3 = nombpsi$: au5 = saldopsi$: au7 = sitiopsi$
               GET #1, I: au2 = codipsi$: au4 = nombpsi$: au6 = saldopsi$: au8 = sitiopsi$
               IF au3 > au4 THEN
                    RSET codipsi$ = au2: RSET nombpsi$ = au4: RSET saldopsi$ = au6: RSET sitiopsi$ = au8
                    PUT #1, J
                    RSET codipsi$ = au1: RSET nombpsi$ = au3: RSET saldopsi$ = au5: RSET sitiopsi$ = au7
                    PUT #1, I
                    Sw = 0
               END IF
          NEXT J
     LOOP UNTIL Sw = 1
WEND
CLOSE

OPEN "f:\kevinprg\kevindat\saldos.dat" FOR RANDOM SHARED AS #1 LEN = 45
FIELD #1, 6 AS codipsi$, 35 AS nombpsi$, 2 AS saldopsi$, 2 AS sitiopsi$
N = LOF(1) / 45: salto = 0: CLS
FOR k = 1 TO N
     GET #1, k
     PRINT codipsi$; " - "; nombpsi$;
     PRINT USING "  ########"; CVI(saldopsi$);
     PRINT USING "  ########"; CVI(sitiopsi$)
     salto = salto + 1: IF salto >= 24 THEN SLEEP: CLS : salto = 0
NEXT k
CLOSE

