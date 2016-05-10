DEFINT A-Z
CLOSE
TYPE descuentos
   desc1 AS DOUBLE
   desc2 AS DOUBLE
   desc3 AS DOUBLE
END TYPE
DIM SHARED dcto AS descuentos
OPEN "f:\kevinprg\kevindat\desc.dat" FOR RANDOM SHARED AS #48 LEN = LEN(dcto)

OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
max% = LOF(1) / 200

CLS : van% = 0: iguales% = 0: CONST true = -1: CONST false = 0

FOR k = 1 TO max%
   GET #1, k: GET #48, k, dcto
   LOCATE 1, 1: PRINT k
IF (CVI(exi12$) + CVI(exi22$) + CVI(exi32$)) > 0 THEN
   IF CVD(pv12$) = CVD(pv22$) AND CVD(pv12$) <> 0 THEN
      IF dcto.desc1 = dcto.desc2 THEN
         LOCATE 2 + van%, 1: GOSUB Une1y2
         PRINT "³"; nomb2$; "³"; k; " * 1=2 * "
         van% = van% + 1: iguales% = iguales% + 1
         IF van% = 20 THEN CLS : van% = 0
         GOTO Res
      END IF
   END IF
   IF CVD(pv12$) = CVD(pv32$) AND CVD(pv12$) <> 0 THEN
      IF dcto.desc1 = dcto.desc3 THEN
         LOCATE 2 + van%, 1: GOSUB Une1y3
         PRINT "³"; nomb2$; "³"; k; " * 1=3 * "
         van% = van% + 1: iguales% = iguales% + 1
         IF van% = 20 THEN CLS : van% = 0
         GOTO Res
      END IF
   END IF
   IF CVD(pv22$) = CVD(pv32$) AND CVD(pv22$) <> 0 THEN
      IF dcto.desc2 = dcto.desc3 THEN
         LOCATE 2 + van%, 1: GOSUB Une2y3
         PRINT "³"; nomb2$; "³"; k; " * 2=3 * "
         van% = van% + 1: iguales% = iguales% + 1
         IF van% = 20 THEN CLS : van% = 0
      END IF
   END IF
Res:
   IF CVI(exi12$) = 0 THEN
        RSET pv12$ = MKD$(0): RSET cos12$ = MKD$(0)
        RSET ven11$ = " ": RSET ven12$ = "  "
   END IF
   IF CVI(exi22$) = 0 THEN
        RSET pv22$ = MKD$(0): RSET cos22$ = MKD$(0)
        RSET ven21$ = " ": RSET ven22$ = "  "
   END IF
   IF CVI(exi32$) = 0 THEN
        RSET pv32$ = MKD$(0): RSET cos32$ = MKD$(0)
        RSET ven31$ = " ": RSET ven32$ = "  "
   END IF
   PUT #1, k
END IF
NEXT k
CLS : PRINT "Total de iguales:"; iguales%
CLOSE
END

Une1y2:
RSET cos22$ = MKD$((CVI(exi12$) * CVD(cos12$) + CVI(exi22$) * CVD(cos22$)) / (CVI(exi12$) + CVI(exi22$)))
RSET exi22$ = MKI$(CVI(exi12$) + CVI(exi22$))
RSET exi12$ = MKI$(0)
RETURN

Une1y3:
RSET cos32$ = MKD$((CVI(exi12$) * CVD(cos12$) + CVI(exi22$) * CVD(cos22$)) / (CVI(exi12$) + CVI(exi32$)))
RSET exi32$ = MKI$(CVI(exi12$) + CVI(exi32$))
RSET exi12$ = MKI$(0)
RETURN

Une2y3:
RSET cos32$ = MKD$((CVI(exi22$) * CVD(cos22$) + CVI(exi32$) * CVD(cos32$)) / (CVI(exi22$) + CVI(exi32$)))
RSET exi32$ = MKI$(CVI(exi22$) + CVI(exi32$))
RSET exi22$ = MKI$(0)
RETURN

