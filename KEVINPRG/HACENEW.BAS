'convierte archivos .dbf de dbase a archivo .dbf de Medstorm
OPEN "F:\kevinprg\kevindat\newinv.dbf" FOR RANDOM AS #1 LEN = 524
FIELD #1, 1 AS activo$, 7 AS codigo$, 30 AS nomb$, 3 AS vacio1$, 8 AS pv1$, 8 AS pv2$, 8 AS pv3$, 11 AS vacio2$, 4 AS mini1$, 4 AS maxi1$, 2 AS ubic$, 84 AS vacio3$, 4 AS exi1$, 4 AS exi2$, 4 AS exi3$, 181 AS vacio4$, 1 AS iva$, 152 AS vacio5$, 8 AS vence$
max1% = LOF(1) / 524
'FOR k = 1 TO max1%: GET #1, k: PRINT vence$: NEXT k: END
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM AS #2 LEN = 200
FIELD #2, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
FOR k = 1 TO max1%: LOCATE 1, 1: PRINT k
GET #1, k
'PRINT codigo$; "-"; CVI(mini1$); "-"; CVI(mini2$); "-"; CVI(maxi1$); "-"; CVI(maxi2$); "-"; "ubicacion:"; ubic$; "-"; nomb$
'PRINT obser$; "-"; acti$; "-EXIS>"; CVI(exi1$); "-"; CVI(exi2$); "-"; CVI(exi3$)
'PRINT "-Precio>";
'PRINT USING "#########,.##"; CVS(pv1$); CVS(pv2$); CVS(pv3$)
'PRINT USING "#########,.##"; CVS(cos1$); CVS(cos2$); CVS(cos3$)
'PRINT "-"; iv1$; "-"; iv2$; "-"; iv3$;
'PRINT "Laboratorio:"; labo$; "-"; re$; "-"; ped$; "-"; medi$; "-"; misc$; "-"; quim$; "-"; compo1$; "-"; compo2$; "-"; compo3$; "-"; compo4$; "-"; compo5$; "-"; compo6$; "-"; preg$
'PRINT : SLEEP 3
LSET codigo2$ = MID$(codigo$, 1, 6)
LSET mini12$ = MKI$(VAL(mini1$))
LSET mini22$ = MKI$(0)
LSET maxi12$ = MKI$(0)
LSET maxi22$ = MKI$(0)
LSET ubic2$ = MKI$(VAL(ubic$))
LSET nomb2$ = MID$(nomb$, 1, 30)
LSET obser2$ = SPACE$(20)
LSET acti2$ = " "
LSET exi12$ = MKI$(VAL(exi1$))
LSET exi22$ = MKI$(VAL(exi2$))
LSET exi32$ = MKI$(VAL(exi3$))
LSET exi42$ = MKI$(0)
LSET pv12$ = MKD$(VAL(pv1$))
LSET pv22$ = MKD$(VAL(pv2$))
LSET pv32$ = MKD$(VAL(pv3$))
LSET pv42$ = MKD$(0)
LSET cos12$ = MKD$(FIX(.7 * VAL(pv1$)))
LSET cos22$ = MKD$(FIX(.7 * VAL(pv2$)))
LSET cos32$ = MKD$(FIX(.7 * VAL(pv3$)))
LSET cos42$ = MKD$(0)
IF iva$ = "S" THEN LSET iv12$ = "*" ELSE LSET iv12$ = " "
LSET iv22$ = " "
LSET iv32$ = " "
LSET iv42$ = " "
LSET of12$ = " "
LSET of22$ = " "
LSET of32$ = " "
LSET labo2$ = MKI$(1)
LSET re2$ = " "
LSET ped2$ = MKI$(0)
LSET medi2$ = "*"
LSET misc2$ = " "
LSET quim2$ = " "
LSET compo12$ = MKI$(1)
LSET compo22$ = MKI$(1)
LSET compo32$ = MKI$(1)
LSET compo42$ = MKI$(1)
LSET compo52$ = MKI$(1)
LSET compo62$ = MKI$(1)
LSET preg2$ = " "
LSET saldo$ = MKI$(VAL(exi1$) + VAL(exi2$) + VAL(exi3$))
LSET upc$ = MKD$(0)
LSET ven11$ = " "
LSET ven12$ = MKI$(0)
LSET ven21$ = " "
LSET ven22$ = MKI$(0)
IF LEN(vence$) > 0 THEN
     IF VAL(MID$(vence$, 5, 2)) < 10 THEN
          LSET ven31$ = MID$(vence$, 6, 1)
     ELSE
          IF VAL(MID$(vence$, 5, 2)) = 10 THEN LSET ven31$ = "A"
          IF VAL(MID$(vence$, 5, 2)) = 11 THEN LSET ven31$ = "B"
          IF VAL(MID$(vence$, 5, 2)) = 12 THEN LSET ven31$ = "C"
     END IF
END IF
IF LEN(vence$) > 0 THEN
     IF VAL(MID$(vence$, 3, 2)) > 90 THEN
          LSET ven32$ = MKI$(1900 + VAL(MID$(vence$, 3, 2)))
     ELSE
          LSET ven32$ = MKI$(2000 + VAL(MID$(vence$, 3, 2)))
     END IF
ELSE
     LSET ven32$ = MKI$(0)
END IF
LSET ven41$ = " "
LSET ven42$ = MKI$(0)
PUT #2, k
NEXT k
CLOSE

