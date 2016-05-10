DEFINT A-Z
CONST true = -1: CONST false = NOT true: CLS
DIM nombre1 AS STRING * 35, nombre2 AS STRING * 35, cadena1 AS STRING * 45, cadena2 AS STRING * 45
agregado = false: modificado = false: encontrado = false
'verifica psicotr¢picos nuevos, actualiza y busca repetidos
CLOSE

OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
max1% = LOF(1) / 200

OPEN "f:\kevinprg\kevindat\saldos.dat" FOR RANDOM SHARED AS #3 LEN = 45
FIELD #3, 6 AS codipsi$, 35 AS nombpsi$, 2 AS saldopsi$, 2 AS sitiopsi$
max3% = LOF(3) / 45

FOR k = 1 TO max1%
   GET #1, k
   IF CVI(ubic2$) = 1 THEN
      encontrado = false
      FOR l = 1 TO max3%
         GET #3, l
         IF codigo2$ = codipsi$ THEN
            encontrado = true
            IF nombpsi$ <> nomb2$ THEN
               RSET nombpsi$ = nomb2$: modificado = true: PUT #3, l
            END IF
         END IF
      NEXT l
      IF encontrado = false THEN
         IF re2$ <> "*" AND (CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$)) <> 0 THEN
            max3% = max3% + 1: agregado = true
            LOCATE 1, 1: PRINT codigo2$, codipsi$
            RSET codipsi$ = codigo2$
            RSET nombpsi$ = nomb2$
            RSET sitiopsi$ = num$
            RSET saldopsi$ = MKI$(0)
            PUT #3, max3%
         END IF
      END IF
   END IF
NEXT k

IF modificado = true OR agregado = true THEN
   Salto = max3%: N = Salto
   WHILE Salto > 1
        Salto = Salto / 2
        DO
             Sw = 1
             FOR J = 1 TO N - Salto
                  I = J + Salto
                  GET #3, J: nombre1 = nombpsi$: cadena1 = codipsi$ + nombpsi$ + saldopsi$ + sitiopsi$
                  GET #3, I: nombre2 = nombpsi$: cadena2 = codipsi$ + nombpsi$ + saldopsi$ + sitiopsi$
                  IF nombre1 > nombre2 THEN
                       RSET codipsi$ = MID$(cadena2, 1, 6)
                       RSET nombpsi$ = MID$(cadena2, 7, 35)
                       RSET saldopsi$ = MID$(cadena2, 42, 2)
                       RSET sitiopsi$ = MID$(cadena2, 44, 2)
                       PUT #3, J
                       RSET codipsi$ = MID$(cadena1, 1, 6)
                       RSET nombpsi$ = MID$(cadena1, 7, 35)
                       RSET saldopsi$ = MID$(cadena1, 42, 2)
                       RSET sitiopsi$ = MID$(cadena1, 44, 2)
                       PUT #3, I
                       Sw = 0
                  END IF
             NEXT J
        LOOP UNTIL Sw = 1
   WEND
   END
   GET #3, 1: code$ = codipsi$
   FOR k = 2 TO max3%
      GET #3, k
      IF code$ = codipsi$ THEN
         FOR l = k + 1 TO max3%
            GET #3, l: PUT #3, l - 1
         NEXT l
         max3% = max3% - 1
      END IF
   NEXT k

   CLOSE
   KILL "f:\kevinprg\kevindat\saldas.dat"
   OPEN "f:\kevinprg\kevindat\saldos.dat" FOR RANDOM SHARED AS #3 LEN = 45
   FIELD #3, 45 AS cade1$
   OPEN "f:\kevinprg\kevindat\saldas.dat" FOR RANDOM SHARED AS #4 LEN = 45
   FIELD #3, 45 AS cade2$
   FOR k = 1 TO max3%
      GET #3, k: RSET cade2$ = cade1$: PUT #4, k
   NEXT k
   CLOSE
   KILL "f:\kevinprg\kevindat\saldos.dat"
   NAME "f:\kevinprg\kevindat\saldas.dat" AS "f:\kevinprg\kevindat\saldos.dat"
END IF

