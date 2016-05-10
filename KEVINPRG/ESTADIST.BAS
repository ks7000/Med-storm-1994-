DEFINT A-Z
DECLARE SUB Suma1dia (dat$, caso%)
DECLARE SUB GringoAEspanol (dat$)
DECLARE FUNCTION QuitaBarra$ (dat$)
DECLARE FUNCTION DateToJulian& (dat$, caso%)
DECLARE FUNCTION TrataFechas$ (dat$, fechaentra$, fechasale$)
CONST true = -1, false = 0
DIM SHARED fx AS STRING * 33

OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
max1% = LOF(1) / 200
 
OPEN "F:\kevinprg\kevindat\codigo.idx" FOR RANDOM SHARED AS #2 LEN = 8
FIELD #2, 6 AS nomshort$, 2 AS num$
max2% = LOF(2) / 8

OPEN "F:\kevinprg\kevinvnt\estadist.dat" FOR RANDOM SHARED AS #3 LEN = 20
FIELD #3, 4 AS unid$, 8 AS primera$, 8 AS ultima$
IF LOF(3) / 20 = 0 THEN max3% = max1%
IF LOF(3) / 20 <> max1% THEN
   FOR k = max1% TO max3%
      RSET unid$ = MKL$(0)
      RSET primera$ = "        "
      RSET ultima$ = "        "
      PUT #3, k
   NEXT k
   max3% = max1%
END IF

'archivo de control: £ltima fecha de actualizaci¢n
CLOSE #20
OPEN "F:\kevinprg\kevinvnt\control.dat" FOR RANDOM SHARED AS #20 LEN = 10
FIELD #20, 10 AS cadena$
die$ = DATE$: CALL GringoAEspanol(die$)
IF LOF(20) / 10 = 0 THEN
   die$ = "01/10/1998"
   RSET cadena$ = "01/10/1998"
   PUT #20, 1
   FOR k = 1 TO max1%
      RSET unid$ = MKL$(0)
      RSET primera$ = "        "
      PUT #3, k
      LOCATE 1, 1: PRINT k
   NEXT k: CLS
ELSE
   GET #20, 1
   die$ = cadena$
END IF
'LINE INPUT "¨Hasta que fecha desea? (ddmmaaaa) ", hasta$
hasta$ = TrataFechas(DATE$, "mm/dd/aaaa", "ddmmaaaa")
hast& = (DateToJulian&(hasta$, 1) - 1)
IF DateToJulian&(die$, 2) <= hast& THEN
   DO      'ciclo principal
      fx = "f:\kevinprg\kevinvnt\" + QuitaBarra(die$) + ".vnt"
      CLOSE #4
      OPEN fx FOR RANDOM AS #4 LEN = 30
      FIELD #4, 30 AS all$
      max4% = LOF(4) / 30
     
      IF max4% >= 4 THEN
                    
            salir = false
            puntero% = 2
            DO WHILE NOT salir
               GET #4, puntero%
               IF MID$(all$, 1, 1) <> "J" THEN PRINT "Archivo Corrupto": EXIT DO
               otropuntero% = puntero% + CVI(MID$(all$, 2, 2))
              
               FOR k = puntero% + 1 TO otropuntero%
                  GET #4, k
                  encontrado = false
                  primero = 1
                  ultimo = max2%
                  Prod$ = MID$(all$, 2, 6)
                  WHILE NOT encontrado AND primero <= ultimo
                     central = (primero + ultimo) / 2
                     GET #2, central
                     IF Prod$ = nomshort$ THEN
                             encontrado = true
                     ELSE
                        IF nomshort$ > Prod$ THEN
                             ultimo = central - 1
                        ELSE
                             primero = central + 1
                        END IF
                     END IF
                  WEND
                  GET #2, central
                  IF encontrado = false THEN PRINT "un borrado"
                  'ya tengo cvi(num$) y lo registro en estadist.dat
                  IF encontrado = true THEN
                       GET #3, CVI(num$)
                       IF primera$ = SPACE$(8) THEN
                            aux$ = die$: aux$ = QuitaBarra(aux$)
                            RSET primera$ = aux$
                            RSET ultima$ = aux$
                            RSET unid$ = MKL$(CVI(MID$(all$, 8, 2)))
                            PUT #3, CVI(num$)
                       ELSE
                            aux$ = die$: aux$ = QuitaBarra(aux$)
                            RSET ultima$ = aux$
                            RSET unid$ = MKL$(CVL(unid$) + CVI(MID$(all$, 8, 2)))
                            PUT #3, CVI(num$)
                       END IF
                  END IF
               NEXT k
              
               'baja factura
               SWAP puntero%, otropuntero%
               puntero% = puntero% + 1
               IF puntero% >= max4% THEN EXIT DO
            LOOP
      END IF
      CALL Suma1dia(die$, 2)
      IF DateToJulian&(die$, 2) > DateToJulian&(hasta$, 1) THEN EXIT DO
   LOOP
END IF
RSET cadena$ = die$
PUT #20, 1
CLOSE
END

FUNCTION DateToJulian& (dat$, caso%)
' reparado el error para fechas mayores que 29 de Febrero de 2000
SELECT CASE caso%
   CASE 1
      ' formato ddmmaaaa
      day% = VAL(MID$(dat$, 1, 2))
      month% = VAL(MID$(dat$, 3, 2))
      year% = VAL(MID$(dat$, 5, 4))
      mes% = VAL(MID$(dat$, 3, 2))
      ano% = VAL(MID$(dat$, 5, 4))
   CASE 2
      ' formato dd/mm/aaaa
      day% = VAL(MID$(dat$, 1, 2))
      month% = VAL(MID$(dat$, 4, 2))
      year% = VAL(MID$(dat$, 7, 4))
      mes% = VAL(MID$(dat$, 4, 2))
      ano% = VAL(MID$(dat$, 7, 4))
   CASE 3
      ' formato mm/dd/aaaa
      day% = VAL(MID$(dat$, 4, 2))
      month% = VAL(MID$(dat$, 1, 2))
      year% = VAL(MID$(dat$, 7, 4))
      mes% = VAL(MID$(dat$, 1, 2))
      ano% = VAL(MID$(dat$, 7, 4))
   CASE 4
      ' formato mmddaaaa
      day% = VAL(MID$(dat$, 3, 2))
      month% = VAL(MID$(dat$, 1, 2))
      year% = VAL(MID$(dat$, 5, 4))
      mes% = VAL(MID$(dat$, 1, 2))
      ano% = VAL(MID$(dat$, 5, 4))
END SELECT
      SELECT CASE month%
          CASE IS > 2
               month% = month% - 3
          CASE IS <= 2
               month% = month% + 9
               year% = year% - 1
      END SELECT
      A& = 146097# * FIX(FIX(year% / 100) / 4)
      B& = FIX(1461# * (year% MOD 100) / 4)
      C& = FIX((153# * month% + 2) / 5) + day% + 1721119
      D& = A& + B& + C&
      SELECT CASE ano%
         CASE 2000
            IF mes% > 2 THEN
               D& = D& - 109573
            END IF
         CASE IS > 2000
               D& = D& - 109573
         CASE ELSE
      END SELECT
      DateToJulian = D&
END FUNCTION

SUB GringoAEspanol (dat$)
'de formato mm/dd/aaaa a dd/mm/aaaa
aux$ = MID$(dat$, 4, 2) + "/" + MID$(dat$, 1, 2) + "/" + MID$(dat$, 7, 4)
dat$ = aux$
END SUB

FUNCTION QuitaBarra$ (dat$)
aux$ = MID$(dat$, 1, 2) + MID$(dat$, 4, 2) + MID$(dat$, 7, 4)
QuitaBarra$ = aux$
END FUNCTION

SUB Suma1dia (dat$, caso%)
SELECT CASE caso%
   CASE 1
      ' formato ddmmaaaa
      day% = VAL(MID$(dat$, 1, 2))
      month% = VAL(MID$(dat$, 3, 2))
      year% = VAL(MID$(dat$, 5, 4))
   CASE 2
      ' formato dd/mm/aaaa
      day% = VAL(MID$(dat$, 1, 2))
      month% = VAL(MID$(dat$, 4, 2))
      year% = VAL(MID$(dat$, 7, 4))
   CASE 3
      ' formato mm/dd/aaaa
      day% = VAL(MID$(dat$, 4, 2))
      month% = VAL(MID$(dat$, 1, 2))
      year% = VAL(MID$(dat$, 7, 4))
END SELECT
day% = day% + 1
SELECT CASE month%
   CASE 4, 6, 9, 11
      IF day% > 30 THEN day% = 1: month% = month% + 1
   CASE 2
     IF year% = 2000 THEN
          IF day% > 29 THEN day% = 1: month% = 3
     ELSE
          IF day% > 28 THEN day% = 1: month% = 3
     END IF
   CASE 12
     IF day% > 31 THEN day% = 1: month% = 1: year% = year% + 1
   CASE ELSE
     IF day% > 31 THEN day% = 1: month% = month% + 1
END SELECT
IF day% < 10 THEN
     dia$ = "0" + MID$(STR$(day%), 2, 1)
ELSE
     dia$ = MID$(STR$(day%), 2, 2)
END IF
IF month% < 10 THEN
     mes$ = "0" + MID$(STR$(month%), 2, 1)
ELSE
     mes$ = MID$(STR$(month%), 2, 2)
END IF
ano$ = MID$(STR$(year%), 2, 4)
SELECT CASE caso%
     CASE 1
       ' formato ddmmaaaa
       dat$ = dia$ + mes$ + ano$
     CASE 2
       ' formato dd/mm/aaaa
       dat$ = dia$ + "/" + mes$ + "/" + ano$
     CASE 3
       ' formato mm/dd/aaaa
       dat$ = mes$ + "/" + dia$ + "/" + ano$
END SELECT
END SUB

FUNCTION TrataFechas$ (dat$, fechaentra$, fechasale$)
fechaentra$ = LCASE$(fechaentra$)
fechasale$ = LCASE$(fechasale$)
tomadia% = false: tomames% = false: tomaano% = false
IF INSTR(1, fechaentra$, "d") > 0 THEN
   IF INSTR(1, fechaentra$, "dd") > 0 AND NOT tomadia% THEN
      dia% = VAL(MID$(dat$, INSTR(1, fechaentra$, "dd"), 2))
      tomadia% = true
   END IF
   IF INSTR(1, fechaentra$, "d") > 0 AND NOT tomadia% THEN
      dia% = VAL(MID$(dat$, INSTR(1, fechaentra$, "d"), 1))
      tomadia% = true
   END IF
END IF
IF INSTR(1, fechaentra$, "m") > 0 THEN
   IF INSTR(1, fechaentra$, "mm") > 0 AND NOT tomames% THEN
      mes% = VAL(MID$(dat$, INSTR(1, fechaentra$, "mm"), 2))
      tomames% = true
   END IF
   IF INSTR(1, fechaentra$, "m") > 0 AND NOT tomames% THEN
      mes% = VAL(MID$(dat$, INSTR(1, fechaentra$, "m"), 1))
      tomames% = true
   END IF
END IF
IF INSTR(1, fechaentra$, "a") > 0 THEN
   IF INSTR(1, fechaentra$, "aaaa") > 0 AND NOT tomaano% THEN
      ano% = VAL(MID$(dat$, INSTR(1, fechaentra$, "aaaa"), 4))
      tomaano% = true
   END IF
   IF INSTR(1, fechaentra$, "aa") > 0 AND NOT tomaano% THEN
      ano% = VAL(MID$(dat$, INSTR(1, fechaentra$, "aa"), 2))
      tomaano% = true
   END IF
END IF
IF dia% < 10 THEN
   dia2$ = "0" + MID$(STR$(dia%), 2, 1)
ELSE
   dia2$ = MID$(STR$(dia%), 2, 2)
END IF
IF mes% < 10 THEN
   mes2$ = "0" + MID$(STR$(mes%), 2, 1)
ELSE
   mes2$ = MID$(STR$(mes%), 2, 2)
END IF
IF INSTR(fechasale$, "a") > 0 THEN
   IF INSTR(fechasale$, "aaaa") > 0 THEN
      IF ano% >= 80 AND ano% < 100 THEN
         ano2$ = "19" + MID$(STR$(ano%), 2, 2)
      ELSEIF ano% >= 0 AND ano% < 80 THEN
         IF ano% < 10 THEN
            ano2$ = "200" + MID$(STR$(ano%), 2, 1)
         ELSE
            ano2$ = "20" + MID$(STR$(ano%), 2, 1)
         END IF
      ELSE
         ano2$ = MID$(STR$(ano%), 2, 4)
      END IF
      MID$(fechasale$, INSTR(fechasale$, "aaaa"), 4) = ano2$
   ELSE
      IF ano% < 100 THEN
         ano2$ = MID$(STR$(ano%), 2, 2)
      ELSE
         ano2$ = MID$(STR$(ano%), 4, 2)
      END IF
      MID$(fechasale$, INSTR(fechasale$, "aa"), 2) = ano2$
   END IF
END IF
IF INSTR(fechasale$, "d") > 0 THEN
   MID$(fechasale$, INSTR(fechasale$, "dd"), 2) = dia2$
END IF
IF INSTR(fechasale$, "m") > 0 THEN
   MID$(fechasale$, INSTR(fechasale$, "mm"), 2) = mes2$
END IF
TrataFechas$ = fechasale$
END FUNCTION

