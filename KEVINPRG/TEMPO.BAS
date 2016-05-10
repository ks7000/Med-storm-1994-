DECLARE FUNCTION Trim$ (x$)
DEFINT A-Z
comienzo% = 0
CLOSE
OPEN "F:\kevinprg\kevindat\master.fct" FOR RANDOM SHARED AS #27 LEN = 70
FIELD #27, 70 AS linea$
final% = (LOF(27) / 70)
selecc% = 0: CLS
FOR k = final% TO 1 STEP -1
  GET #27, k
  ' numfact(selecc%) = MID$(linea$, 2, 4)
  ' provee(selecc%) = MID$(linea$, 24, 2)
  ' fechafact(selecc%) = MID$(linea$, 51, 8)
  fechafact$ = MID$(linea$, 51, 8)
  num4$ = MID$(linea$, 24, 2): num4$ = STR$(CVI(num4$))
  num4$ = Trim$(num4$)
  IF VAL(num4$) < 10 THEN
     num4$ = "00" + MID$(num4$, 2, 1)
  END IF
  IF VAL(num4$) < 100 THEN
     num4$ = "0" + MID$(num4$, 2, 2)
  END IF
  
  num2$ = STR$(CVL(MID$(linea$, 2, 4)))
  num2$ = Trim$(MID$(num2$, 2, 10)):  larg% = LEN(num2$)
  num2$ = "F:\kevinprg\kevindat\" + STRING$(8 - larg%, "0") + MID$(num2$, 1, larg%) + "." + MID$(num4$, 1, 3)
  CLOSE #2
  OPEN num2$ FOR RANDOM AS #2 LEN = 40
  FIELD #2, 6 AS odigo$, 2 AS antid$, 8 AS recio$, 8 AS osto$, 8 AS ferta$, 2 AS oferantid$, 1 AS va$, 2 AS sitio$, 3 AS venci$
  lleped% = LOF(2) / 40
  FOR l% = 1 TO lleped%
    GET #2, l%: LOCATE 1, 1: PRINT k, l%
    IF odigo$ = "29067 " THEN PRINT fechafact$
  NEXT l
  ' numfact(selecc%) = MID$(linea$, 2, 4)
  ' provee(selecc%) = MID$(linea$, 24, 2)
  ' edofact(selecc%) = MID$(linea$, 26, 1) + MID$(linea$, 59, 1) + MID$(linea$, 62, 1)
  ' fechaacc(selecc%) = MID$(linea$, 6, 8)
  ' transpor(selecc%) = MID$(linea$, 14, 10)
  ' fechafact(selecc%) = MID$(linea$, 51, 8)
  ' ubifact(selecc%) = MKI$(k)
  ' bruto(selecc%) = MID$(linea$, 27, 8)
  ' neto(selecc%) = MID$(linea$, 35, 8)
  ' ivafact(selecc%) = MID$(linea$, 43, 8)
NEXT k

FUNCTION Trim$ (x$)
IF x$ = "" THEN
  Trim$ = ""
ELSE
lastChar = 0
FOR A = 1 TO LEN(x$): y$ = MID$(x$, A, 1): IF y$ <> CHR$(0) AND y$ <> " " THEN lastChar = A
NEXT A: Trim$ = LEFT$(x$, lastChar)
END IF
END FUNCTION

