DEFINT A-Z
DECLARE SUB Termometro (va%, total%, edo%, text$, fila%, columna%)
DECLARE SUB Box2 (Row1, Col1, Row2, Col2)
DECLARE FUNCTION Verify (ubi%, text$)
CONST true = -1: CONST false = NOT true
COMMON SHARED guia, colorpref AS INTEGER
TYPE ubicacion
   cab AS STRING * 8
   col AS STRING * 2
END TYPE
   DIM ao1 AS STRING * 35, ao2 AS STRING * 35, ao3 AS STRING * 2, ao4 AS STRING * 2
   DIM au1 AS STRING * 17, au2 AS STRING * 17, au3 AS STRING * 2, au4 AS STRING * 2, au5 AS STRING * 2, au6 AS STRING * 2
   DIM alu1 AS ubicacion, alu2 AS ubicacion
res% = Verify(13, "¨Desea organizar todo")
IF res% = true THEN
   LOCATE 1, 1: PRINT TIME$
   COLOR 0, 15: Termometro 0, 12, 0, " Estado General Organizacion  ", 3, 19


   ' indexado de nombre---------------------------------------------------------
   ON ERROR GOTO ErrorTrap
   Termometro 1, 12, 1, " Estado General Organizacion  ", 3, 19
   CLOSE
   KILL "f:\kevinprg\kevindat\nombre.idx"
   KILL "f:\kevinprg\kevindat\ubica.idx"
   KILL "f:\kevinprg\kevindat\codigo.idx"
   KILL "f:\kevinprg\kevindat\codigold.idx"
   KILL "f:\kevinprg\kevindat\lab.idx"
   KILL "f:\kevinprg\kevindat\compos.idx"
   KILL "f:\kevinprg\kevindat\borrados.idx"
   KILL "f:\kevinprg\kevindat\oferta*.idx"

   '========apertura de base de datos
   OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
   FIELD #1, 6 AS codigo$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$ _
, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2  _
AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
   max1% = LOF(1) / 200


   '--------apertura de los idx's
   OPEN "f:\kevinprg\kevindat\nombre.idx" FOR RANDOM AS #2 LEN = 37
   FIELD #2, 35 AS codigo2$, 2 AS sitio2$: lleva2 = 0
   OPEN "f:\kevinprg\kevindat\ubica.idx" FOR RANDOM AS #3 LEN = 10
   FIELD #3, 8 AS codigo3$, 2 AS sitio3$: lleva3 = 0
   OPEN "f:\kevinprg\kevindat\codigo.idx" FOR RANDOM AS #4 LEN = 8
   FIELD #4, 6 AS codigo4$, 2 AS sitio4$: lleva4 = 0
   OPEN "f:\kevinprg\kevindat\codigold.idx" FOR RANDOM AS #5 LEN = 8
   FIELD #5, 6 AS codigo5$, 2 AS sitio5$: lleva5 = 0
   OPEN "f:\kevinprg\kevindat\lab.idx" FOR RANDOM AS #6 LEN = 10
   FIELD #6, 8 AS codigo6$, 2 AS sitio6$: lleva6 = 0
   OPEN "f:\kevinprg\kevindat\compos.idx" FOR RANDOM AS #7 LEN = 4
   FIELD #7, 2 AS codigo7$, 2 AS sitio7$: lleva7 = 0
   OPEN "f:\kevinprg\kevindat\borrados.idx" FOR RANDOM AS #8 LEN = 37
   FIELD #8, 35 AS codigo8$, 2 AS sitio8$: lleva8 = 0
   OPEN "f:\kevinprg\kevindat\oferta1.idx" FOR RANDOM AS #9 LEN = 8
   FIELD #9, 6 AS codigo9$, 2 AS sitio9$: lleva9 = 0
   OPEN "f:\kevinprg\kevindat\oferta2.idx" FOR RANDOM AS #10 LEN = 8
   FIELD #10, 6 AS codigo10$, 2 AS sitio10$: lleva10 = 0
   OPEN "f:\kevinprg\kevindat\oferta3.idx" FOR RANDOM AS #11 LEN = 8
   FIELD #11, 6 AS codigo11$, 2 AS sitio11$: lleva11 = 0

   Termometro 0, max1%, 0, "Generando indexados ... ", 13, 19

   FOR k = 1 TO max1%: GET #1, k
   Termometro k, max1%, 1, "Generando indexados ... ", 13, 19
   IF acti2$ = " " THEN
        IF CVD(pv12$) > 0 AND CVI(exi12$) <> 0 THEN IF CVD(cos12$) = 0 THEN RSET cos12$ = MKD$(.7 * CVD(pv12$))
        IF CVD(pv22$) > 0 AND CVI(exi22$) <> 0 THEN IF CVD(cos22$) = 0 THEN RSET cos22$ = MKD$(.7 * CVD(pv22$))
        IF CVD(pv32$) > 0 AND CVI(exi32$) <> 0 THEN IF CVD(cos32$) = 0 THEN RSET cos32$ = MKD$(.7 * CVD(pv32$))
        IF CVD(pv42$) > 0 AND CVI(exi42$) <> 0 THEN IF CVD(cos42$) = 0 THEN RSET cos42$ = MKD$(.7 * CVD(pv42$))
       
        '--------copiado a nombre.idx archivo #2
        lleva2 = lleva2 + 1: RSET sitio2$ = MKI$(k): RSET codigo2$ = UCASE$(nomb2$): PUT #2, lleva2
        sitio1% = INSTR(1, obser2$, "("): sitio2% = INSTR(1, obser2$, ")")
        IF sitio2% > sitio1% AND sitio1% <> 0 THEN
             lleva2 = lleva2 + 1: RSET sitio2$ = MKI$(k)
             LSET codigo2$ = MID$(obser2$, (sitio1% + 1), (sitio2% - sitio1% - 1))
             LSET codigo2$ = MID$(codigo2$, 1, 34) + "þ": PUT #2, lleva2
        END IF
       
        '--------copiado a ubica.idx  archivo #3
        lleva3 = lleva3 + 1: RSET sitio3$ = MKI$(k): RSET codigo3$ = ubic2$ + MID$(nomb2$, 1, 6): PUT #3, lleva3

        '--------copiado a codigo.idx archivo #4
        lleva4 = lleva4 + 1: RSET sitio4$ = MKI$(k): RSET codigo4$ = codigo$: PUT #4, lleva4

        '--------copiado a lab.idx    archivo #6
        lleva6 = lleva6 + 1: RSET sitio6$ = MKI$(k): LSET codigo6$ = labo2$ + MID$(nomb2$, 1, 6): PUT #6, lleva6
       
        '--------copiado a compos.idx archivo #7
        IF (CVI(compo12$) + CVI(compo22$) + CVI(compo32$) + CVI(compo42$) + CVI(compo52$) + CVI(compo62$)) > 6 THEN
             clave$ = compo12$ + compo22$ + compo32$ + compo42$ + compo52$ + compo62$
             FOR l = 1 TO 12 STEP 2
                  IF CVI(MID$(clave$, l, 2)) > 1 THEN
                       lleva7 = lleva7 + 1: RSET sitio7$ = MKI$(k)
                       RSET codigo7$ = MID$(clave$, l, 2): PUT #7, lleva7
                  END IF
             NEXT l
        END IF
       
        '--------copiado a oferta1.idx archivo #9
        IF of12$ = "@" THEN
             lleva9 = lleva9 + 1: RSET sitio9$ = MKI$(k): RSET codigo9$ = MID$(nomb2$, 1, 6): PUT #9, lleva9
        END IF
       
        '--------copiado a oferta2.idx archivo #10
        IF of22$ = "#" THEN
             lleva10 = lleva10 + 1: RSET sitio10$ = MKI$(k): RSET codigo10$ = MID$(nomb2$, 1, 6): PUT #10, lleva10
        END IF
       
        '--------copiado a oferta3.idx archivo #11
        IF of32$ = "$" THEN
             lleva11 = lleva11 + 1: RSET sitio11$ = MKI$(k): RSET codigo11$ = MID$(nomb2$, 1, 6): PUT #11, lleva11
        END IF
   ELSE
        '--------copiado a codigold.idx archivo #5
        lleva5 = lleva5 + 1: RSET sitio5$ = MKI$(k): RSET codigo5$ = codigo$: PUT #5, lleva5
       
        '--------copiado a borrados.idx archivo #8
        lleva8 = lleva8 + 1: RSET sitio8$ = MKI$(k): RSET codigo8$ = nomb2$: PUT #8, lleva8
   END IF
   NEXT k: CLOSE

   'ordenado de indexado por nombre
   Termometro 2, 12, 0, " Estado General Organizacion  ", 3, 19
   OPEN "f:\kevinprg\kevindat\nombre.idx" FOR RANDOM AS #1 LEN = 37
   FIELD #1, 35 AS codigo$, 2 AS sitio$: N = LOF(1) / 37: Salto = N
   Termometro 0, N, 0, "Ordenando Nombres    ...", 13, 19

   WHILE Salto > 1
        Termometro (N - Salto), N, 1, "Ordenando Nombres    ...", 13, 19
        Salto = Salto / 2
        DO
             Sw = 1
             FOR J = 1 TO N - Salto
                  I = J + Salto
                  GET #1, J: ao1 = codigo$: ao3 = sitio$
                  GET #1, I: ao2 = codigo$: ao4 = sitio$
                  IF ao1 > ao2 THEN
                       RSET codigo$ = ao2: RSET sitio$ = ao4: PUT #1, J
                       RSET codigo$ = ao1: RSET sitio$ = ao3: PUT #1, I
                       Sw = 0
                  END IF
             NEXT J
        LOOP UNTIL Sw = 1
   WEND: CLOSE
'***************************************************************************
'***************************************************************************
   'ordenado de indexado por ubicacion
   Termometro 3, 12, 1, " Estado General Organizacion  ", 3, 19
   OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #2 LEN = 200
   FIELD #2, 6 AS codigo$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$ _
, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2  _
AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
   OPEN "f:\kevinprg\kevindat\ubica.idx" FOR RANDOM AS #1 LEN = 10
   N = LOF(1) / 10: Salto = N
   Termometro 0, N, 0, "Ordenando Ubicaci¢n  ...", 13, 19

   WHILE Salto > 1
        Termometro (N - Salto), N, 1, "Ordenando Ubicaci¢n  ...", 13, 19
        Salto = Salto / 2
        DO
             Sw = 1
             FOR J = 1 TO N - Salto
                  I = J + Salto
                  GET #1, J, alu1: GET #2, CVI(alu1.col): ao1 = MID$(alu1.cab, 1, 2) + nomb2$
                  GET #1, I, alu2: GET #2, CVI(alu2.col): ao2 = MID$(alu2.cab, 1, 2) + nomb2$
                  IF ao1 > ao2 THEN
                     PUT #1, J, alu2
                     PUT #1, I, alu1
                     Sw = 0
                  END IF
             NEXT J
        LOOP UNTIL Sw = 1
   WEND: CLOSE

   'ordenado de indexado de codigo de activos
   Termometro 4, 12, 1, " Estado General Organizacion  ", 3, 19
   OPEN "f:\kevinprg\kevindat\codigo.idx" FOR RANDOM AS #1 LEN = 8
   FIELD #1, 6 AS codigo$, 2 AS sitio$: N = LOF(1) / 8: Salto = N
   Termometro 0, N, 0, "Indexando C¢digos 1 ...", 13, 19

   WHILE Salto > 1
        Termometro (N - Salto), N, 1, "Ordenando C¢digos 1  ...", 13, 19
        Salto = Salto / 2
        DO
             Sw = 1
             FOR J = 1 TO N - Salto
                  I = J + Salto
                  GET #1, J: au1 = codigo$: au3 = sitio$
                  GET #1, I: au2 = codigo$: au4 = sitio$
                  IF au1 > au2 THEN
                       RSET codigo$ = MID$(au2, 1, 6): RSET sitio$ = au4: PUT #1, J
                       RSET codigo$ = MID$(au1, 1, 6): RSET sitio$ = au3: PUT #1, I
                       Sw = 0
                  END IF
             NEXT J
        LOOP UNTIL Sw = 1
   WEND: CLOSE

   'ordenado de indexado de codigo viejos
   Termometro 5, 12, 1, " Estado General Organizacion  ", 3, 19
   OPEN "f:\kevinprg\kevindat\codigold.idx" FOR RANDOM AS #1 LEN = 8
   FIELD #1, 6 AS codigo$, 2 AS sitio$: N = LOF(1) / 8: Salto = N
   Termometro 0, N, 0, "Indexando C¢digos 2 ...", 13, 19

   IF N > 1 THEN
   WHILE Salto > 1
        Termometro (N - Salto), N, 1, "Ordenando C¢digos 2  ...", 13, 19
        Salto = Salto / 2
        DO
             Sw = 1
             FOR J = 1 TO N - Salto
                  I = J + Salto
                  GET #1, J: au1 = codigo$: au3 = sitio$
                  GET #1, I: au2 = codigo$: au4 = sitio$
                  IF au1 > au2 THEN
                       RSET codigo$ = MID$(au2, 1, 6): RSET sitio$ = au4: PUT #1, J
                       RSET codigo$ = MID$(au1, 1, 6): RSET sitio$ = au3: PUT #1, I
                       Sw = 0
                  END IF
             NEXT J
        LOOP UNTIL Sw = 1
   WEND: CLOSE
   END IF

   'ordenado de indexado por laboratorio
   Termometro 6, 12, 1, " Estado General Organizacion  ", 3, 19
   OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #2 LEN = 200
   FIELD #2, 6 AS codigo$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$ _
, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2  _
AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
   OPEN "f:\kevinprg\kevindat\lab.idx" FOR RANDOM AS #1 LEN = 10
   FIELD #1, 8 AS codigo$, 2 AS sitio$: N = LOF(1) / 10: Salto = N
   Termometro 0, N, 0, "Indexando Laboratorios   ...", 13, 19

   IF N > 1 THEN
   WHILE Salto > 1
        Termometro (N - Salto), N, 1, "Indexando Laboratorios..", 13, 19
        Salto = Salto / 2
        DO
             Sw = 1
             FOR J = 1 TO N - Salto
                  I = J + Salto
                  GET #1, J: GET #2, CVI(sitio$): au1 = labo2$ + nomb2$: au3 = sitio$
                  GET #1, I: GET #2, CVI(sitio$): au2 = labo2$ + nomb2$: au4 = sitio$
                  IF au1 > au2 THEN
                       LSET codigo$ = MID$(au2, 1, 6): RSET sitio$ = au4: PUT #1, J
                       LSET codigo$ = MID$(au1, 1, 6): RSET sitio$ = au3: PUT #1, I
                       Sw = 0
                  END IF
             NEXT J
        LOOP UNTIL Sw = 1
   WEND: CLOSE
   END IF

   'ordenado de indexado por composicion
   Termometro 7, 12, 0, " Estado General Organizacion  ", 3, 19
   OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #2 LEN = 200
   FIELD #2, 6 AS codigo$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$ _
, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2  _
AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
   OPEN "f:\kevinprg\kevindat\compos.idx" FOR RANDOM AS #1 LEN = 4
   FIELD #1, 2 AS codigo$, 2 AS sitio$: N = LOF(1) / 4: Salto = N
   Termometro 0, N, 0, "Indexando Composici¢n   ...", 13, 19

   IF N > 1 THEN
   WHILE Salto > 1
        Termometro (N - Salto), N, 1, "Indexando Composici¢n   ...", 13, 19
        Salto = Salto / 2
        DO
             Sw = 1
             FOR J = 1 TO N - Salto
                  I = J + Salto
                  GET #1, J: GET #2, CVI(sitio$): au1 = codigo$ + nomb2$: au3 = sitio$
                  GET #1, I: GET #2, CVI(sitio$): au2 = codigo$ + nomb2$: au4 = sitio$
                  IF au1 > au2 THEN
                       RSET codigo$ = MID$(au2, 1, 2): RSET sitio$ = au4: PUT #1, J
                       RSET codigo$ = MID$(au1, 1, 2): RSET sitio$ = au3: PUT #1, I
                       Sw = 0
                  END IF
             NEXT J
        LOOP UNTIL Sw = 1
   WEND: CLOSE
   END IF

   'ordenado de indexado de borrados
   Termometro 8, 12, 1, " Estado General Organizacion  ", 3, 19
   OPEN "f:\kevinprg\kevindat\borrados.idx" FOR RANDOM AS #1 LEN = 37
   FIELD #1, 35 AS codigo$, 2 AS sitio$: N = LOF(1) / 37: Salto = N
   IF N > 1 THEN
   Termometro 0, N, 0, "Indexando Borrados   ...", 13, 19
   WHILE Salto > 1
        Salto = Salto / 2: Termometro (N - Salto), N, 1, "Indexando Borrados  ...", 13, 19
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
   END IF: CLOSE

   'ordenado de indexado de ofertas1
   Termometro 9, 12, 1, " Estado General Organizacion  ", 3, 19
   OPEN "f:\kevinprg\kevindat\oferta1.idx" FOR RANDOM AS #1 LEN = 8
   FIELD #1, 6 AS codigo$, 2 AS sitio$: N = LOF(1) / 8: Salto = N

   IF N > 1 THEN
   Termometro 0, N, 0, "Indexando Ofertas #1...", 13, 19
   WHILE Salto > 1
        Salto = Salto / 2: Termometro (N - Salto), N, 1, "Indexando Ofertas #1...", 13, 19
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
   END IF: CLOSE

   'ordenado de indexado de ofertas2
   Termometro 10, 12, 1, " Estado General Organizaci¢n  ", 3, 19
   OPEN "f:\kevinprg\kevindat\oferta2.idx" FOR RANDOM AS #1 LEN = 8
   FIELD #1, 6 AS codigo$, 2 AS sitio$: N = LOF(1) / 8: Salto = N

   IF N > 1 THEN
   Termometro 0, N, 0, "Indexando Ofertas #2...", 13, 19
   WHILE Salto > 1
        Salto = Salto / 2: Termometro (N - Salto), N, 1, "Indexando Ofertas #2...", 13, 19
        DO
             Sw = 1
             FOR J = 1 TO N - Salto
                  I = J + Salto
                  GET #1, J: au1 = codigo$: au3 = sitio$
                  GET #1, I: au2 = codigo$: au4 = sitio$
                  IF au1 > au2 THEN
                       RSET codigo$ = MID$(au2, 1, 6): RSET sitio$ = au4: PUT #1, J
                       RSET codigo$ = MID$(au1, 1, 6): RSET sitio$ = au3: PUT #1, I
                       Sw = 0
                  END IF
             NEXT J
        LOOP UNTIL Sw = 1
   WEND
   END IF: CLOSE

   Termometro 12, 12, 1, " Estado General Organizaci¢n  ", 3, 19
   OPEN "F:\kevinprg\kevindat\ubica.dat" FOR RANDOM AS #2 LEN = 18
   FIELD #2, 3 AS codigo$, 15 AS descripcion$
   OPEN "f:\kevinprg\kevindat\ordubica.dat" FOR RANDOM SHARED AS #3 LEN = 15
   FIELD #3, 15 AS desc$
   Termometro 0, 199, 0, "Ordenando Ubicaciones..", 13, 19
   FOR k = 1 TO 199
        Termometro k, 199, 1, "Ordenando Ubicaciones..", 13, 19
        GET #2, k: RSET desc$ = descripcion$
        PUT #3, VAL(codigo$)
   NEXT k
   CLOSE
  
   Termometro 0, 199, 0, "Ordenando Composiciones", 13, 19
   OPEN "F:\kevinprg\kevindat\componen.dat" FOR RANDOM AS #2 LEN = 18
   FIELD #2, 3 AS codigo$, 15 AS descripcion$
   OPEN "f:\kevinprg\kevindat\ordcompo.dat" FOR RANDOM AS #3 LEN = 15
   FIELD #3, 15 AS desc$
   FOR k = 1 TO 199
        Termometro k, 199, 1, "Ordenando Composiciones", 13, 19
        GET #2, k: RSET desc$ = descripcion$
        PUT #3, VAL(codigo$)
   NEXT k
   CLOSE
   Termometro 0, 199, 0, "Ordenando Laboratorios ", 13, 19
   OPEN "F:\kevinprg\kevindat\lab.dat" FOR RANDOM AS #2 LEN = 18
   FIELD #2, 3 AS codigo$, 15 AS descripcion$
   OPEN "f:\kevinprg\kevindat\ordlab.dat" FOR RANDOM AS #3 LEN = 15
   FIELD #3, 15 AS desc$
   FOR k = 1 TO 199
        Termometro k, 199, 1, "Ordenando Laboratorios ", 13, 19
        GET #2, k: RSET desc$ = descripcion$
        PUT #3, VAL(codigo$)
   NEXT k
   CLOSE

   Termometro 12, 12, 1, " Estado General Organizaci¢n  ", 3, 19
   SOUND 100, 500
   ON ERROR GOTO 0
   CLOSE
   LOCATE 2, 1: PRINT TIME$
   CHAIN "f:\kevinprg\kevin01.bas"
ELSE
   res% = Verify(13, "¨Desea organizar nombres y c¢digos")
   IF res% = true THEN
      LOCATE 1, 1: PRINT TIME$
      COLOR 0, 15: Termometro 0, 12, 0, " Estado General Organizacion  ", 3, 19


      ' indexado de nombre---------------------------------------------------------
      ON ERROR GOTO ErrorTrap
      Termometro 1, 12, 1, " Estado General Organizacion  ", 3, 19
      CLOSE
      KILL "f:\kevinprg\kevindat\nombre.idx"
      KILL "f:\kevinprg\kevindat\codigo.idx"

      '========apertura de base de datos
      OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
      FIELD #1, 6 AS codigo$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS  _
cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
      max1% = LOF(1) / 200


      '--------apertura de los idx's
      OPEN "f:\kevinprg\kevindat\nombre.idx" FOR RANDOM AS #2 LEN = 37
      FIELD #2, 35 AS codigo2$, 2 AS sitio2$: lleva2 = 0
      OPEN "f:\kevinprg\kevindat\codigo.idx" FOR RANDOM AS #4 LEN = 8
      FIELD #4, 6 AS codigo4$, 2 AS sitio4$: lleva4 = 0

      Termometro 0, max1%, 0, "Generando indexados ... ", 13, 19

      FOR k = 1 TO max1%: GET #1, k
      Termometro k, max1%, 1, "Generando indexados ... ", 13, 19
      IF acti2$ = " " THEN
           IF CVD(pv12$) > 0 AND CVI(exi12$) <> 0 THEN IF CVD(cos12$) = 0 THEN RSET cos12$ = MKD$(.7 * CVD(pv12$))
           IF CVD(pv22$) > 0 AND CVI(exi22$) <> 0 THEN IF CVD(cos22$) = 0 THEN RSET cos22$ = MKD$(.7 * CVD(pv22$))
           IF CVD(pv32$) > 0 AND CVI(exi32$) <> 0 THEN IF CVD(cos32$) = 0 THEN RSET cos32$ = MKD$(.7 * CVD(pv32$))
           IF CVD(pv42$) > 0 AND CVI(exi42$) <> 0 THEN IF CVD(cos42$) = 0 THEN RSET cos42$ = MKD$(.7 * CVD(pv42$))
         
           '--------copiado a nombre.idx archivo #2
           lleva2 = lleva2 + 1: RSET sitio2$ = MKI$(k): RSET codigo2$ = UCASE$(nomb2$): PUT #2, lleva2
           sitio1% = INSTR(1, obser2$, "("): sitio2% = INSTR(1, obser2$, ")")
           IF sitio2% > sitio1% AND sitio1% <> 0 THEN
                lleva2 = lleva2 + 1: RSET sitio2$ = MKI$(k)
                LSET codigo2$ = MID$(obser2$, (sitio1% + 1), (sitio2% - sitio1% - 1))
                LSET codigo2$ = MID$(codigo2$, 1, 34) + "þ": PUT #2, lleva2
           END IF
         
           '--------copiado a codigo.idx archivo #4
           lleva4 = lleva4 + 1: RSET sitio4$ = MKI$(k): RSET codigo4$ = codigo$: PUT #4, lleva4
      END IF
      NEXT k: CLOSE

      'ordenado de indexado por nombre
      Termometro 2, 12, 0, " Estado General Organizacion  ", 3, 19
      OPEN "f:\kevinprg\kevindat\nombre.idx" FOR RANDOM AS #1 LEN = 37
      FIELD #1, 35 AS codigo$, 2 AS sitio$: N = LOF(1) / 37: Salto = N
      Termometro 0, N, 0, "Ordenando Nombres    ...", 13, 19

      WHILE Salto > 1
           Termometro (N - Salto), N, 1, "Ordenando Nombres    ...", 13, 19
           Salto = Salto / 2
           DO
                Sw = 1
                FOR J = 1 TO N - Salto
                     I = J + Salto
                     GET #1, J: ao1 = codigo$: ao3 = sitio$
                     GET #1, I: ao2 = codigo$: ao4 = sitio$
                     IF ao1 > ao2 THEN
                          RSET codigo$ = ao2: RSET sitio$ = ao4: PUT #1, J
                          RSET codigo$ = ao1: RSET sitio$ = ao3: PUT #1, I
                          Sw = 0
                     END IF
                NEXT J
           LOOP UNTIL Sw = 1
      WEND: CLOSE

      'ordenado de indexado de codigo de activos
      Termometro 4, 12, 1, " Estado General Organizacion  ", 3, 19
      OPEN "f:\kevinprg\kevindat\codigo.idx" FOR RANDOM AS #1 LEN = 8
      FIELD #1, 6 AS codigo$, 2 AS sitio$: N = LOF(1) / 8: Salto = N
      Termometro 0, N, 0, "Indexando C¢digos 1 ...", 13, 19

      WHILE Salto > 1
           Termometro (N - Salto), N, 1, "Ordenando C¢digos 1  ...", 13, 19
           Salto = Salto / 2
           DO
                Sw = 1
                FOR J = 1 TO N - Salto
                     I = J + Salto
                     GET #1, J: au1 = codigo$: au3 = sitio$
                     GET #1, I: au2 = codigo$: au4 = sitio$
                     IF au1 > au2 THEN
                          RSET codigo$ = MID$(au2, 1, 6): RSET sitio$ = au4: PUT #1, J
                          RSET codigo$ = MID$(au1, 1, 6): RSET sitio$ = au3: PUT #1, I
                          Sw = 0
                     END IF
                NEXT J
           LOOP UNTIL Sw = 1
      WEND: CLOSE

      Termometro 12, 12, 1, " Estado General Organizaci¢n  ", 3, 19
      SOUND 100, 500
      ON ERROR GOTO 0
      CLOSE
      LOCATE 2, 1: PRINT TIME$
   END IF
END IF
CHAIN "f:\kevinprg\kevin01.bas"
END

ErrorTrap:
SELECT CASE ERR
     CASE 51, 55
          ' OPEN fx FOR RANDOM AS #7 LEN = 4: FIELD #7, 4 AS MONTI$: RSET MONTI$ = MKS$(0): PUT #7, 1: CLOSE #7: RESUME
     CASE 53
          RESUME NEXT
     CASE 57
          RESUME NEXT
     CASE 25, 27, 57, 68
          Box2 10, 10, 20, 70: LOCATE 15, 20: PRINT "La Impresora No Responde:Pulse Esc Para Salir"
          kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
          IF kbd$ = CHR$(27) THEN
               devolver = true: RESUME NEXT
          ELSE
               RESUME
          END IF
     CASE 70
          LOCATE 25, 15: PRINT "Esperando Acceso a La Red , Espere Por Favor . . ."; : SLEEP 1: LOCATE 25, 15: PRINT SPACE$(50); : RESUME
     CASE ELSE
END SELECT
RESUME NEXT

SUB Box2 (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "²"; STRING$(BoxWidth - 2, "²"); "²"; : FOR a = Row1 + 1 TO Row2 - 1: LOCATE a, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Termometro (va%, total%, edo%, text$, fila%, columna%)
IF edo% = 0 THEN Box2 fila%, columna%, fila% + 6, columna% + 31
LOCATE fila% + 1, columna% + 1: PRINT "0%                        100%"
IF edo% = 0 THEN LOCATE fila% + 2, columna% + 1: PRINT STRING$(30, "±")
LOCATE fila% + 2, columna% + 1: PRINT STRING$(INT((va% / total%) * 30), "Û")
LOCATE fila% + 3, columna% + 1: PRINT "Van"; va%; " de "; total%;
LOCATE fila% + 4, columna% + 9: PRINT USING "###.## % Hecho "; ((va% / total%) * 100)
LOCATE fila% + 5, columna% + 1: PRINT text$
END SUB

