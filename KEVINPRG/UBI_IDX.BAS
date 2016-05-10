  TYPE ubicacion
     cab AS STRING * 8
     col AS STRING * 2
  END TYPE
   DIM ao1 AS STRING * 37, ao2 AS STRING * 37
   DIM alu1 AS ubicacion, alu2 AS ubicacion
  
   OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
   FIELD #1, 6 AS codigo$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$ _
, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2  _
AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
   max1% = LOF(1) / 200
   OPEN "f:\kevinprg\kevindat\ubica.idx" FOR RANDOM AS #4 LEN = 10
   FIELD #4, 8 AS codigo4$, 2 AS sitio4$: lleva4 = 0
   FOR k = 1 TO max1%
        GET #1, k
        '--------copiado a ubica.idx  archivo #3
        lleva4 = lleva4 + 1: RSET sitio4$ = MKI$(k)
        RSET codigo4$ = ubic2$ + MID$(nomb2$, 1, 6)
        PUT #4, lleva4
  NEXT k
  CLOSE

   'ordenado de indexado por ubicacion
   OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #2 LEN = 200
   FIELD #2, 6 AS codigo$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$ _
, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2  _
AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
   OPEN "f:\kevinprg\kevindat\ubica.idx" FOR RANDOM AS #1 LEN = 10
   N = LOF(1) / 10: Salto = N

   WHILE Salto > 1
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

