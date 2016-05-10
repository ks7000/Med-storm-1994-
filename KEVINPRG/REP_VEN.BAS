DEFINT A-Z
DECLARE FUNCTION Trim$ (x$)
CLOSE
TYPE descuentos
   desc1 AS DOUBLE
   desc2 AS DOUBLE
   desc3 AS DOUBLE
END TYPE
DIM SHARED dcto AS descuentos
DIM num2 AS STRING * 33
OPEN "f:\kevinprg\kevindat\desc.dat" FOR RANDOM SHARED AS #48 LEN = LEN(dcto)

OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
max% = LOF(1) / 200

OPEN "F:\kevinprg\kevindat\master.fct" FOR RANDOM SHARED AS #27 LEN = 70
FIELD #27, 70 AS linea$: final% = (LOF(27) / 70): selecc% = 0

OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #7 LEN = 18
FIELD #7, 3 AS codigo$, 15 AS descripcion$: max7% = LOF(7) / 18

CLS : van% = 0: iguales% = 0: CONST true = -1: CONST false = 0

FOR k = 1 TO max%
   GET #1, k: GET #48, k, dcto
  
   LOCATE 1, 1: PRINT k
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
 
     sitio = true
     IF CVD(pv12$) > CVD(pv22$) AND CVD(pv12$) < CVD(pv32$) THEN GOSUB Cambia1y2: sitio = false
     IF sitio = true THEN IF CVD(pv12$) < CVD(pv22$) AND CVD(pv22$) < CVD(pv32$) THEN sitio = false
     IF sitio = true THEN IF CVD(pv12$) < CVD(pv22$) AND CVD(pv12$) > CVD(pv32$) THEN GOSUB Cambia1y2: GOSUB Cambia1y3: sitio = false
     IF sitio = true THEN IF CVD(pv12$) > CVD(pv22$) AND CVD(pv22$) < CVD(pv32$) THEN GOSUB Cambia1y2: GOSUB Cambia2y3: sitio = false
     IF sitio = true THEN IF CVD(pv12$) < CVD(pv22$) AND CVD(pv22$) > CVD(pv32$) THEN GOSUB Cambia2y3: sitio = false
     IF sitio = true THEN IF CVD(pv12$) > CVD(pv22$) AND CVD(pv22$) > CVD(pv32$) THEN GOSUB Cambia1y3: sitio = false
     IF sitio = true THEN IF CVD(pv12$) = CVD(pv22$) AND CVD(pv22$) < CVD(pv32$) THEN sitio = false
     IF sitio = true THEN IF CVD(pv12$) = CVD(pv32$) AND CVD(pv22$) > CVD(pv32$) THEN GOSUB Cambia2y3: sitio = false
     IF sitio = true THEN IF CVD(pv12$) > CVD(pv22$) AND CVD(pv22$) = CVD(pv32$) THEN GOSUB Cambia1y3: sitio = false
     IF sitio = true THEN IF CVD(pv12$) = CVD(pv22$) AND CVD(pv22$) > CVD(pv32$) THEN GOSUB Cambia1y3: sitio = false
     IF sitio = true THEN IF CVD(pv12$) = CVD(pv32$) AND CVD(pv22$) < CVD(pv32$) THEN GOSUB Cambia1y2: sitio = false
     IF sitio = true THEN IF CVD(pv22$) = CVD(pv32$) AND CVD(pv12$) < CVD(pv32$) THEN sitio = false

   IF CVD(pv12$) = CVD(pv22$) AND CVD(pv12$) <> 0 THEN
     LOCATE 2 + van%, 1
     PRINT "³"; nomb2$; "³"; k; " * 1=2 * "
     van% = van% + 1: iguales% = iguales% + 1
     IF van% = 20 THEN SLEEP: CLS : van% = 0
     GOTO Res
   END IF
   IF CVD(pv12$) = CVD(pv32$) AND CVD(pv12$) <> 0 THEN
     LOCATE 2 + van%, 1
     PRINT "³"; nomb2$; "³"; k; " * 1=3 * "
     van% = van% + 1: iguales% = iguales% + 1
     IF van% = 20 THEN SLEEP: CLS : van% = 0
   END IF
Res:
   PUT #1, k
NEXT k
SLEEP
CLS : PRINT "Total de iguales:"; iguales%
CLS
FOR k = 1 TO final%
   GET #27, k
   IF MID$(linea$, 1, 1) = " " THEN
      FOR l = 1 TO max7%
         GET #7, l
         IF CVI(MID$(linea$, 24, 2)) = VAL(codigo$) THEN
            provex$ = codigo$: provix$ = descripcion$: EXIT FOR
         END IF
      NEXT l
    
      'linea$ 26 credito o contado   59 actualizada si/no  62 cancelada si/no
      num2 = STR$(CVL(MID$(linea$, 2, 4))): num2 = Trim$(MID$(num2, 2, 10)): larg% = 0
      FOR l = 1 TO 33: IF MID$(num2, l, 1) <> " " THEN larg% = larg% + 1
      NEXT l
      num2 = "F:\kevinprg\kevindat\" + STRING$(8 - larg%, "0") + MID$(num2, 1, larg%) + "." + provex$
    
      CLOSE #2: OPEN num2 FOR RANDOM AS #2 LEN = 40
      FIELD #2, 6 AS odigo$, 2 AS antid$, 8 AS recio$, 8 AS osto$, 8 AS ferta$, 2 AS oferantid$, 1 AS va$, 2 AS sitio$, 3 AS venci$
      maxx2% = LOF(2) / 40
      FOR l = 1 TO maxx2%
         LOCATE 1, 1: PRINT k, l
         GET #2, l: GET #1, CVI(sitio$)
         IF venci$ <> "   " THEN
            IF pv12$ = recio$ THEN RSET ven11$ = MID$(venci$, 1, 1): RSET ven12$ = MID$(venci$, 2, 2)
            IF pv22$ = recio$ THEN RSET ven21$ = MID$(venci$, 1, 1): RSET ven22$ = MID$(venci$, 2, 2)
            IF pv32$ = recio$ THEN RSET ven31$ = MID$(venci$, 1, 1): RSET ven32$ = MID$(venci$, 2, 2)
            PUT #1, CVI(sitio$)
         END IF
      NEXT l
   END IF
NEXT k
CLOSE
END


Cambia1y2:
auxi$ = pv12$: RSET pv12$ = pv22$: RSET pv22$ = auxi$
auxi$ = cos12$: RSET cos12$ = cos22$: RSET cos22$ = auxi$
auxi$ = exi12$: RSET exi12$ = exi22$: RSET exi22$ = auxi$
auxi$ = ven11$: RSET ven11$ = ven21$: RSET ven21$ = auxi$
auxi$ = ven12$: RSET ven12$ = ven22$: RSET ven22$ = auxi$
auxi$ = MKD$(dcto.desc1): dcto.desc1 = dcto.desc2: dcto.desc2 = CVD(auxi$)
RETURN

Cambia1y3:
auxi$ = pv12$: RSET pv12$ = pv32$: RSET pv32$ = auxi$
auxi$ = cos12$: RSET cos12$ = cos32$: RSET cos32$ = auxi$
auxi$ = exi12$: RSET exi12$ = exi32$: RSET exi32$ = auxi$
auxi$ = ven11$: RSET ven11$ = ven31$: RSET ven31$ = auxi$
auxi$ = ven12$: RSET ven12$ = ven32$: RSET ven32$ = auxi$
auxi$ = MKD$(dcto.desc1): dcto.desc1 = dcto.desc3: dcto.desc3 = CVD(auxi$)
RETURN

Cambia2y3:
auxi$ = pv22$: RSET pv22$ = pv32$: RSET pv32$ = auxi$
auxi$ = cos22$: RSET cos22$ = cos32$: RSET cos32$ = auxi$
auxi$ = exi22$: RSET exi22$ = exi32$: RSET exi32$ = auxi$
auxi$ = ven21$: RSET ven21$ = ven31$: RSET ven31$ = auxi$
auxi$ = ven22$: RSET ven22$ = ven32$: RSET ven32$ = auxi$
auxi$ = MKD$(dcto.desc2): dcto.desc2 = dcto.desc3: dcto.desc3 = CVD(auxi$)
RETURN

FUNCTION Trim$ (x$)
IF x$ = "" THEN
   Trim$ = ""
ELSE
lastChar = 0
FOR a = 1 TO LEN(x$): y$ = MID$(x$, a, 1): IF y$ <> CHR$(0) AND y$ <> " " THEN lastChar = a
NEXT a: Trim$ = LEFT$(x$, lastChar)
END IF
END FUNCTION

