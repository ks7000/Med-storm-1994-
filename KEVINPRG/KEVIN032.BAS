'      meter factura
DEFINT A-Z
DECLARE SUB Espera ()
DECLARE SUB LeeEstado ()
DECLARE SUB Center (row, text$)
DECLARE SUB Box (Row1%, Col1%, Row2%, Col%)
DECLARE SUB BorraBox (Row1%, Col1%, Row2%, Col%)
DECLARE SUB MeteFact ()
DECLARE SUB Termometro (va%, total%, edo%, text$, fila%, columna%)
DECLARE SUB PopUp (ka%, la%, ma%, na%, pa%, ra%, ta%, sa%)
DECLARE FUNCTION CalculaMeses% (caden$)
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$)
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Escoge% (fila%, edo%, fila2%, Prod$)
DECLARE FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%, est%)
DECLARE FUNCTION CheqDate (caden$, op%)
DECLARE FUNCTION Trim$ (x$)
DECLARE FUNCTION DateToJulian& (dat$, caso%)
DECLARE FUNCTION Verify (ubi%, text$)
DECLARE FUNCTION Descuentos$ (cadena$)
COMMON SHARED guia, colorpref: IF colorpref = 0 THEN colorpref = 1
DIM SHARED turno AS STRING * 1, devolver, neto AS STRING * 8, bruto AS STRING * 8, Iva AS STRING * 8, renglones AS STRING * 2, unidades AS STRING * 4, razon AS STRING * 8, oldbruto AS STRING * 8
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, Impuesto AS STRING * 6, NombFarma AS STRING * 25, Direccion AS STRING * 30, Rif     AS STRING * 25, Nit      AS STRING * 25, Telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2
DIM SHARED Fun(17) AS STRING * 2, salir, Turna, terminar, sitio, busqueda, cspedido, repag, avpag
DIM SHARED Contenido(199) AS STRING * 15, Code(199) AS STRING * 3, porc AS STRING * 8
DIM SHARED Nombra(5) AS STRING * 35, Pvp1a(5) AS STRING * 8, Pvp2a(5) AS STRING * 8, Pvp3a(5) AS STRING * 8, Pvp4a(5) AS STRING * 8, Numa(5) AS STRING * 2, Observa(5) AS STRING * 20, Exis1a(5) AS STRING * 2, Exis2a(5) AS STRING * 2, Exis3a(5) AS  _
STRING * 2, Exis4a(5) AS STRING * 2
DIM SHARED Cant1a(5) AS STRING * 2, Cant2a(5) AS STRING * 2, Cant3a(5) AS STRING * 2, Cant4a(5) AS STRING * 2, oldiva AS STRING * 8, errNum AS INTEGER, grabGav AS STRING * 1
TYPE Descuentos
   desc1 AS DOUBLE
   desc2 AS DOUBLE
   desc3 AS DOUBLE
END TYPE
DIM SHARED dcto AS Descuentos
CONST true = -1: CONST false = NOT true: busqueda = 0: errNum = 0
ON ERROR GOTO ErrorTrap
ON TIMER(5) GOSUB AbreGav
TIMER ON
CALL LeeEstado: CALL MeteFact
CLOSE : OPEN "f:\kevinprg\kevindat\compras.tkt" FOR RANDOM AS #1 LEN = 2: FIELD #1, 2 AS ticket$
GET #1, 1: RSET ticket$ = "si": PUT #1, 1: CLOSE
res% = Verify(10, "¨Desea hacer reclamos en este momento")
IF res% = true THEN
   COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... ": COLOR colors(7, colorpref), colors(4, colorpref)
   guia = 1: CHAIN "F:\kevinprg\kevin037.bas"
ELSE
   COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... ": COLOR colors(7, colorpref), colors(4, colorpref)
   CHAIN "F:\kevinprg\kevin01.bas"
END IF
END
ErrorTrap:
    SELECT CASE ERR
        CASE 53 'no se encontro archivo ...
            errNum = 53: RESUME NEXT
        CASE 70
            Center 25, " Esperando Acceso a La Red   ...": SLEEP 1: Center 25, SPACE$(35): RESUME
        CASE ELSE
REM            CLS : Box 10, 10, 20, 70: Center 13, "Error Inesperado Numero:" + STR$(ERR) + " (Anote Este Numero)": Center 14, "DESCONECTANDOSE DE LA RED ...": LOCATE 21, 1: SYSTEM
    END SELECT
    RESUME NEXT
END
DATA 0,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  2,     0,     1,     15,     0,     7,      0
DATA 3,    15,   13,  1,     14,    3,     15,     0,     7,      0
DATA 7,    12,   15,  4,     14,    0,     15,     15,    1,      0

'ATENCION AbreGav utiliza #29 y #30 en vez de #27 y #28
AbreGav:
'  CLOSE #29
'  OPEN "f:\kevinprg\kevindat\gaveta.sem" FOR RANDOM SHARED AS #29 LEN = 1
'  GET #29, 1, grabGav
'  SELECT CASE grabGav
'    CASE "1"
'      CLOSE #30: OPEN "com1" FOR OUTPUT AS #30: PRINT #30, "A": PRINT #30, "A": CLOSE #30
'      grabGav = "0": PUT #27, 1, grabGav
'    CASE "2"
'      CLOSE #30: OPEN "com1" FOR OUTPUT AS #30: PRINT #30, "A": PRINT #30, "A": CLOSE #30
'      grabGav = "0": PUT #27, 1, grabGav
'    CASE ELSE
'      'LOCATE 1, 1: PRINT grabGav
'  END SELECT
'  CLOSE #29
RETURN

SUB BorraBox (Row1%, Col1%, Row2%, Col2%) STATIC
BoxWidth = Col2% - Col1% + 1: FOR a = Row1% TO Row2%: LOCATE a, Col1%: PRINT SPACE$(BoxWidth); : NEXT a
END SUB

SUB Box (Row1%, Col1%, Row2%, Col2%) STATIC
BoxWidth = Col2% - Col1% + 1: LOCATE Row1%, Col1%: PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿"; : FOR a = Row1% + 1 TO Row2% - 1: LOCATE a, Col1%: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2%, Col1%: PRINT "À"; STRING$(BoxWidth - 2,  _
"Ä"); "Ù";
END SUB

FUNCTION CalculaMeses% (caden$)
caden$ = UCASE$(caden$)
mes$ = MID$(caden$, 1, 1): ano$ = MID$(caden$, 2, 2)
IF mes$ = " " THEN CalculaMeses% = 0: EXIT FUNCTION
SELECT CASE aux$
   CASE "A"
     CalculaMeses% = 10 + (12 * (CVI(ano$) - 1990)): EXIT FUNCTION
   CASE "B"
     CalculaMeses% = 11 + (12 * (CVI(ano$) - 1990)): EXIT FUNCTION
   CASE "C"
     CalculaMeses% = 12 + (12 * (CVI(ano$) - 1990)): EXIT FUNCTION
   CASE ELSE
     CalculaMeses% = VAL(mes$) + (12 * (CVI(ano$) - 1990)): EXIT FUNCTION
END SELECT
CalculaMeses% = 32500
END FUNCTION

SUB Center (row, text$)
IF row = 25 THEN COLOR colors(5, colorpref), colors(4, colorpref): LOCATE 25, 1: PRINT SPACE$(80);
LOCATE row, 41 - LEN(text$) / 2: PRINT text$;
END SUB

FUNCTION CheqDate (caden$, op%)
CheqDate = true: IF caden$ = "        " THEN EXIT FUNCTION
dd% = VAL(MID$(caden$, 1, 2)): mm% = VAL(MID$(caden$, 4, 2)): aa% = VAL(MID$(caden$, 7, 4))
dd2% = VAL(MID$(DATE$, 4, 2)): mm2% = VAL(MID$(DATE$, 1, 2)): aa2% = VAL(MID$(DATE$, 7, 4))
IF dd% < 1 THEN CheqDate = false: EXIT FUNCTION
IF mm% < 1 OR mm% > 12 THEN CheqDate = false: EXIT FUNCTION
IF aa% < 1996 OR aa% > 2020 THEN CheqDate = false: EXIT FUNCTION
IF mm% = 2 AND dd% > 29 THEN CheqDate = false: EXIT FUNCTION
IF mm% = 1 OR mm% = 3 OR mm% = 5 OR mm% = 7 OR mm% = 8 OR mm% = 10 OR mm% = 12 THEN IF dd% > 31 THEN CheqDate = false: EXIT FUNCTION
IF mm% = 4 OR mm% = 6 OR mm% = 9 OR mm% = 11 THEN IF dd% > 30 THEN CheqDate = false: EXIT FUNCTION
IF op% = 2 THEN
     IF aa% < aa2% THEN CheqDate = false: EXIT FUNCTION
     IF aa% = aa2% THEN IF mm% < mm2% THEN CheqDate = false: EXIT FUNCTION
     IF aa% = aa2% THEN IF mm% = mm2% THEN IF dd% < dd2% THEN CheqDate = false: EXIT FUNCTION
END IF
END FUNCTION

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
      a& = 146097# * FIX(FIX(year% / 100) / 4)
      B& = FIX(1461# * (year% MOD 100) / 4)
      C& = FIX((153# * month% + 2) / 5) + day% + 1721119
      D& = a& + B& + C&
      SELECT CASE ano%
         CASE 2000
            IF mes% > 2 THEN
               D& = D& - 109573
            END IF
         CASE IS > 2000
               D& = D& - 109573
         CASE ELSE
      END SELECT
      DateToJulian& = D&

END FUNCTION

FUNCTION Descuentos$ (cadena$)
  DIM ka AS INTEGER, maxLen AS INTEGER, auxCad AS STRING
  DIM totDcto AS DOUBLE
  maxLen = LEN(cadena$): totDcto = 1
  IF MID$(cadena$, maxLen, 1) <> "+" THEN cadena$ = cadena$ + "+"
  FOR k = 1 TO maxLen
    IF MID$(cadena$, k, 1) = "+" THEN
      IF VAL(auxCad) > 0 THEN
        totDcto = totDcto * (1 - VAL(auxCad) / 100): auxCad = ""
      END IF
    ELSE
      auxCad = auxCad + MID$(cadena$, k, 1)
    END IF
  NEXT k
  IF VAL(auxCad) > 0 THEN
    totDcto = totDcto * (1 - VAL(auxCad) / 100): auxCad = ""
  END IF
  totDcto = 100 * (1 - totDcto)
  Descuentos$ = STR$(totDcto)
END FUNCTION

FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%, est%)
COLOR colors(4, colorpref), colors(7, colorpref): Box fila%, columna%, fila% + 16, columna% + 16: selecc% = 199
IF est% = 0 THEN Center 25, "[F1]Ayuda  [F5]Editar o Adicionar Linea  [Enter]Tomar Opcion" ELSE Center 25, "[F1]Ayuda  [Enter]Tomar Opcion"
COLOR colors(4, colorpref), colors(7, colorpref)
SELECT CASE estesi%
     CASE 1
          LOCATE fila%, columna + 2: PRINT "Ubicacion"
          CLOSE #2: OPEN "F:\kevinprg\kevindat\ubica.dat" FOR RANDOM AS #2 LEN = 18
          FIELD #2, 3 AS codigo$, 15 AS descripcion$
     CASE 2
          LOCATE fila%, columna% + 1: PRINT "Laboratorio"
          CLOSE #2: OPEN "F:\kevinprg\kevindat\lab.dat" FOR RANDOM AS #2 LEN = 18
          FIELD #2, 3 AS codigo$, 15 AS descripcion$
     CASE 4
          LOCATE fila%, columna% + 1: PRINT "Componente "; ingrediente%
          CLOSE #2: OPEN "F:\kevinprg\kevindat\componen.dat" FOR RANDOM AS #2 LEN = 18
          FIELD #2, 3 AS codigo$, 15 AS descripcion$
     CASE 5
          LOCATE fila%, columna% + 1: PRINT " Proveedor "
          CLOSE #2: OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #2 LEN = 18
          FIELD #2, 3 AS codigo$, 15 AS descripcion$
END SELECT
FOR k = 1 TO 199: GET #2, k: Code(k) = codigo$: Contenido(k) = descripcion$: NEXT k: CLOSE #2
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila% + 1, columna% + 1: PRINT Contenido(1): COLOR colors(4, colorpref), colors(7, colorpref)
FOR k = 2 TO 15: LOCATE fila% + k, columna% + 1: PRINT Contenido(k): NEXT k
resal% = 1: up% = 1: terminar = false
DO WHILE NOT terminar
kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
        SELECT CASE kbd$
            CASE CHR$(0) + "K": terminar = true: resal% = 0: up% = 0: devolver = true
            CASE CHR$(0) + "M": terminar = true: resal% = 0: up% = 0: devolver = false
            CASE CHR$(0) + "H"
               resal% = resal% - 1: IF resal% < 1 THEN resal% = 1: up% = up% - 1
               IF up% < 1 THEN up% = 1: terminar = true: devolver = true: resal% = 0: up% = 0
            CASE CHR$(0) + "P"
               resal% = resal% + 1: IF resal% > 15 THEN resal% = 15: up% = up% + 1
               IF up% + 14 > selecc% THEN terminar = true: resal% = 0: up% = 0: devolver = false
            CASE CHR$(0) + CHR$(73)
               up% = up% - 14: IF up% < 1 THEN up% = 1: resal% = 1
            CASE CHR$(0) + CHR$(81)
               up% = up% + 14: IF up% + 14 > selecc% THEN up% = selecc% - 14: resal% = 15
            CASE CHR$(13): terminar = true: devolver = false
            CASE CHR$(27): terminar = true: resal% = 0: up% = 0: devolver = true
            CASE CHR$(0) + CHR$(63)
               Contenido(up% + resal% - 1) = MeteDatoLetra(resal% + 3, columna%, 15, (Contenido(up% + resal% - 1))): Center 25, "Organizando , Espere ...": GOSUB sortYname
               COLOR colors(4, colorpref), colors(7, colorpref): Box fila%, columna%, fila% + 16, columna% + 16
            CASE CHR$(0) + CHR$(66): terminar = true: Fun(8) = "si": sitio = resal%
            CASE ELSE:  BEEP
        END SELECT
          IF up% = 0 THEN
               FOR k = 0 TO 14: LOCATE 8 + fila%, columna% + 1: PRINT SPACE$(15): NEXT k
               Elije% = up% + resal% - 1
               EXIT FUNCTION
          END IF
          l = 0
          FOR k = up% TO up% + 14: l = l + 1
          IF resal% = l THEN COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila2%, columna2% - 1: PRINT USING "²###²"; VAL(Code(k)); : PRINT Contenido(k)
          LOCATE fila% + l, columna% + 1: PRINT Contenido(k): IF resal% = l THEN COLOR colors(4, colorpref), colors(7, colorpref)
          NEXT k
LOOP
Elije% = up% + resal% - 1: EXIT FUNCTION
sortYname:
Center 25, " Sorteando Los Campos De Menor A Mayor. Espere Por Favor . . . "
FOR a% = 1 TO 198: FOR B% = a% + 1 TO 199
IF Contenido(a%) > Contenido(B%) THEN SWAP Contenido(a%), Contenido(B%): SWAP Code(a%), Code(B%)
NEXT B%: NEXT a%:
IF estesi% = 1 THEN OPEN "F:\kevinprg\kevindat\ubica.dat" FOR RANDOM AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$
IF estesi% = 2 THEN OPEN "F:\kevinprg\kevindat\lab.dat" FOR RANDOM AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$
IF estesi% = 4 THEN OPEN "F:\kevinprg\kevindat\componen.dat" FOR RANDOM AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$
IF estesi% = 5 THEN OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$
FOR k = 1 TO 199: RSET codigo$ = Code(k): RSET descripcion$ = Contenido(k): PUT #2, k: NEXT k: CLOSE #2
Center 25, "[F1]Ayuda  [F5]Editar o Adicionar Linea  [Enter]Tomar Opcion"
RETURN
END FUNCTION

FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
IF LEN(caden$) > largo% THEN caden$ = MID$(caden$, 1, largo%)
LOCATE fila%, columna%: PRINT CHR$(178) + caden$ + SPACE$(largo% - LEN(caden$)): LOCATE fila%, columna% + largo% + 1: PRINT CHR$(178)
REDIM eleccion(0 TO largo% + 1) AS STRING * 1: llevasitio% = 2: repag = false: avpag = false
FOR k = 1 TO largo%: eleccion(k) = MID$(caden$, k, 1): NEXT k: LOCATE fila%, (columna% + llevasitio%): COLOR colors(4, colorpref) + 16, colors(7, colorpref): IF eleccion(2) = " " THEN PRINT "_";  ELSE PRINT eleccion(2)
DO
      llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
          SELECT CASE llave$
               CASE CHR$(0) + "H": devolver = true: Fun(16) = "si": EXIT DO
               CASE CHR$(0) + "P": Fun(16) = "si": EXIT DO
               CASE CHR$(0) + CHR$(73): : devolver = true: Fun(16) = "si": repag = true: EXIT DO
               CASE CHR$(0) + CHR$(81): avpag = true: : Fun(16) = "si": EXIT DO
               CASE CHR$(0) + "K"
                  llevasitio% = llevasitio% - 1: IF llevasitio% = 0 THEN GOSUB Sumar2: devolver = true: EXIT DO
               CASE CHR$(0) + "M"
                  llevasitio% = llevasitio% + 1: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE CHR$(8)
                  llevasitio% = llevasitio% - 1: IF llevasitio% = 0 THEN GOSUB Sumar2: EXIT DO
                  FOR k = llevasitio% TO largo% - 1: eleccion(k) = eleccion(k + 1)
                  NEXT k: eleccion(largo%) = " "
               CASE CHR$(0) + CHR$(83)
                  FOR k = llevasitio% TO largo%: eleccion(k) = eleccion(k + 1): NEXT k
                  eleccion(largo%) = " "
               CASE CHR$(13): EXIT DO
               CASE CHR$(27): devolver = true: EXIT DO
               'CASE CHR$(0) + CHR$(59): Fun(1) = "si": EXIT DO
               CASE CHR$(0) + CHR$(60): Fun(2) = "si": EXIT DO
               CASE CHR$(0) + CHR$(61): Fun(3) = "si": EXIT DO
               CASE CHR$(0) + CHR$(62): Fun(4) = "si": EXIT DO
               CASE CHR$(0) + CHR$(63): Fun(5) = "si": EXIT DO
               CASE CHR$(0) + CHR$(64): Fun(6) = "si": EXIT DO
               CASE CHR$(0) + CHR$(65): Fun(7) = "si": EXIT DO
               CASE CHR$(0) + CHR$(66): Fun(8) = "si": EXIT DO
               CASE CHR$(0) + CHR$(67): Fun(9) = "si": EXIT DO
               CASE CHR$(0) + CHR$(68): Fun(10) = "si": EXIT DO
               CASE CHR$(0) + CHR$(133): Fun(11) = "si": EXIT DO
               CASE CHR$(0) + CHR$(134): Fun(12) = "si": EXIT DO
               CASE CHR$(0) + CHR$(71): llevasitio% = 1
               CASE CHR$(0) + CHR$(79): llevasitio% = largo%
               CASE " ": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "1": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "2": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "3": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "4": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "5": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "6": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "7": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "8": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "9": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "0": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "+": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "-": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE ".": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE ELSE: IF sonido = "si" THEN BEEP
          END SELECT
          FOR k = 1 TO largo%
          IF k = llevasitio% THEN COLOR colors(4, colorpref) + 16, colors(7, colorpref)
          LOCATE fila%, (columna% + k)
          IF k = llevasitio% THEN
                    IF eleccion(k) <> " " THEN PRINT eleccion(k);  ELSE PRINT "_";
          ELSE
                    PRINT eleccion(k);
          END IF
          IF k = llevasitio% THEN COLOR colors(7, colorpref), colors(4, colorpref)
          NEXT k
LOOP
eleccion$ = "": eleccion(1) = UCASE$(eleccion(1)): FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna% + 1: PRINT eleccion$
IF entero% = 1 THEN eleccion$ = STR$(FIX(VAL(eleccion$)))
EntraDatoNumero$ = eleccion$: IF sonido = "si" THEN BEEP
EXIT FUNCTION
EntraNumero:
eleccion(llevasitio%) = llave$: llevasitio% = llevasitio% + 1
RETURN
Sumar2:
FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna%: PRINT "²"; eleccion$; "²"
RETURN
END FUNCTION

FUNCTION Escoge% (fila%, edo%, fila2%, Prod$)
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila% - 1, 1: PRINT STRING$(80, "Ä"): LOCATE fila% + 5, 1
PRINT STRING$(80, "Ä"); : FOR k = fila% TO fila% + 4: LOCATE k, 1: PRINT SPACE$(80); : NEXT k
STATIC max2%, Prud AS STRING * 6
IF edo% = 1 THEN
CALL Center(25, "[F1] Ayuda [F9] Salir SIN Escoger [Enter] Toma Articulo [A-Z] Ir a")
ELSE
CALL Center(25, "[F1] Ayuda [F9] Salir SIN Escoger [Enter] Toma Articulo")
END IF
IF edo% = 1 OR edo% = 6 THEN
     Prod$ = "      "
     DO WHILE Prod$ = "      "
     LOCATE fila2%, 5: PRINT "Introduzca Orden de Busqueda:"; SPACE$(15)
     IF Fun(17) = "op" THEN Prod$ = MeteDatoLetra(fila2%, 34, 6, "") ELSE Prod$ = MeteDatoLetra(fila2%, 34, 6, "")
     Prud = Prod$: Prod$ = Trim$(UCASE$(Prod$))
     IF Prod$ = "" THEN : Escoge% = -1: EXIT FUNCTION
     LOOP
END IF: selecc% = 0
CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
SELECT CASE edo%
     CASE 1 'por nombre
          OPEN "F:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
          FIELD #2, 35 AS nomshort$, 2 AS num$: max2% = LOF(2) / 37
          GOSUB Parche1
     CASE 2 'por  laboratorio
          OPEN "F:\kevinprg\kevindat\lab.idx" FOR RANDOM SHARED AS #2 LEN = 10
          FIELD #2, 8 AS nomshort$, 2 AS num$: max2% = LOF(2) / 10
          IF max2% = 0 THEN LOCATE fila% + 5, 5: PRINT STRING$(5, "Ä"); "² NO Hay Datos ²"; STRING$(7, "Ä"); : Espera: Escoge% = 0: EXIT FUNCTION
          GOSUB Parche1
     CASE 4  'por oferta
          SELECT CASE Prod$
               CASE "1": OPEN "F:\kevinprg\kevindat\oferta1.idx" FOR RANDOM SHARED AS #2 LEN = 8
               CASE "2": OPEN "F:\kevinprg\kevindat\oferta2.idx" FOR RANDOM SHARED AS #2 LEN = 8
               CASE "3": OPEN "F:\kevinprg\kevindat\oferta3.idx" FOR RANDOM SHARED AS #2 LEN = 8
          END SELECT
          FIELD #2, 6 AS nomshort$, 2 AS num$: max2% = LOF(2) / 8
          IF max2% = 0 THEN LOCATE fila% + 5, 5: PRINT STRING$(5, "Ä"); "² NO Hay Datos ²"; STRING$(7, "Ä"); : Espera: Escoge% = 0: EXIT FUNCTION
          Prod$ = "      "
          DO WHILE Prod$ = "      "
               LOCATE fila2%, 5: PRINT "Introduzca Orden de Busqueda:"; SPACE$(15)
               Prod$ = MeteDatoLetra(fila2%, 34, 6, "")
               Prud = Prod$: Prod$ = Trim$(Prod$)
               IF Fun(9) = "si" OR Prod$ = "" THEN : Escoge% = -1: EXIT FUNCTION
          LOOP
          GOSUB Parche1
     CASE 5  'por composicion
          OPEN "F:\kevinprg\kevindat\compos.idx" FOR RANDOM SHARED AS #2 LEN = 4
          FIELD #2, 2 AS nomshort$, 2 AS num$: selecc% = 0: max2% = LOF(2) / 4
          IF max2% = 0 THEN LOCATE fila% + 5, 5: PRINT STRING$(5, "Ä"); "² NO Hay Datos ²"; STRING$(7, "Ä"); : CALL Center(25, "Presione Cualquier Tecla Para Continuar"): Espera: Escoge% = 0: EXIT FUNCTION
          GOSUB Parche1
     CASE 6  'por codigo de aqui sale de funcion en este caso
          OPEN "F:\kevinprg\kevindat\codigo.idx" FOR RANDOM SHARED AS #2 LEN = 8
          FIELD #2, 6 AS nomshort$, 2 AS num$: max2% = LOF(2) / 8
          IF max2% = 0 THEN LOCATE fila% + 5, 5: PRINT STRING$(5, "Ä"); "² NO Hay Datos ²"; STRING$(7, "Ä"); : CALL Center(25, "Presione Cualquier Tecla Para Continuar"): Espera: Escoge% = 0: EXIT FUNCTION
          encontrado = false: primero = 1: ultimo = max2%
          WHILE NOT encontrado AND primero <= ultimo
               central = (primero + ultimo) / 2: GET #2, central
               IF Prud = nomshort$ THEN
                    encontrado = true
               ELSE
                    IF nomshort$ > Prud THEN
                         ultimo = central - 1
                    ELSE
                         primero = central + 1
                    END IF
               END IF
          WEND: salir = false
          IF encontrado = false THEN
               LOCATE fila% + 5, 5: PRINT STRING$(5, "Ä"); "² NO Se Encontro ²"; STRING$(5, "Ä");
               CALL Espera: Escoge% = 0: EXIT FUNCTION
          ELSE
               Escoge% = CVI(num$): EXIT FUNCTION
          END IF
     CASE ELSE
END SELECT
GOSUB Parche2
DO
     kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND: kbd$ = UCASE$(kbd$)
     SELECT CASE kbd$
          CASE CHR$(0) + "H": GOSUB Arriba
          CASE CHR$(0) + "P": GOSUB Abajo
          CASE CHR$(0) + CHR$(73): GOSUB Apgup
          CASE CHR$(0) + CHR$(81): GOSUB Apgdn
          CASE CHR$(13): EXIT DO
          CASE CHR$(27): resal% = 0: EXIT DO
          CASE CHR$(0) + CHR$(67): resal% = 0: Fun(3) = "si": EXIT DO
REM           CASE IS >= CHR$(65)
REM               IF kbd$ <= CHR$(90) AND edo% = 1 THEN
REM                    Prod$ = kbd$: OPEN "F:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
REM                    FIELD #2, 35 AS nomshort$, 2 AS num$: max2% = LOF(2) / 37: GOSUB Parche1: GOSUB Parche2
REM               END IF
          CASE ELSE:  BEEP
     END SELECT
     IF up% = 0 THEN
          FOR k = 0 TO 4: LOCATE fila% + k, 1: PRINT SPACE$(80); : NEXT k
          EXIT DO
     END IF
     FOR l = 1 TO 5
          IF resal% = l THEN LOCATE fila% - 2, 50: PRINT "En Oferta:"; : PRINT USING "#####, "; CVI(Cant4a(l)); : PRINT " a Bs."; : PRINT USING "######,"; CVD(Pvp4a(l))
          IF resal% = l THEN COLOR colors(7, colorpref), colors(4, colorpref)
          IF l <= selecc% THEN
               LOCATE fila% + l - 1, 1: PRINT Nombra(l); "³"; : LOCATE fila% + l - 1, 37: PRINT USING "#####, "; CVI(Cant1a(l)); : LOCATE fila% + l - 1, 51: PRINT "³"; : PRINT USING "#####, "; CVI(Cant2a(l));
               LOCATE fila% + l - 1, 66: PRINT "³"; : PRINT USING "#####, "; CVI(Cant3a(l)): LOCATE fila% + l - 1, 44: PRINT USING "######,"; CVD(Pvp1a(l)): LOCATE fila% + l - 1, 59: PRINT USING "######,"; CVD(Pvp2a(l))
               LOCATE fila% + l - 1, 74: PRINT USING "######,"; CVD(Pvp3a(l))
          ELSE
               LOCATE fila% + l - 1, 1: PRINT SPACE$(80)
          END IF
          IF resal% = l THEN COLOR colors(4, colorpref), colors(7, colorpref)
     NEXT l
LOOP
IF resal% > 0 THEN Escoge% = CVI(Numa(resal%)) ELSE Escoge% = -1
EXIT FUNCTION
Arriba:
     resal% = resal% - 1
     IF resal% < 1 THEN
          resal% = 1: up% = up% - 1: IF up% = 0 THEN up% = 1
          IF sonido = "si" THEN BEEP
          GET #2, up%: IF Prod$ <> MID$(nomshort$, 1, LEN(Prod$)) THEN up% = up% + 1
          GOSUB LeeCinco
     END IF
RETURN
Abajo:
     resal% = resal% + 1
     IF resal% > selecc% THEN
          resal% = selecc%: up% = up% + 1
          IF up% > max2% THEN up% = max2%
          IF sonido = "si" THEN BEEP
          GET #2, (up% + 4): IF Prod$ <> MID$(nomshort$, 1, LEN(Prod$)) THEN up% = up% - 1
          GOSUB LeeCinco
     END IF
RETURN
Apgup:
up% = up% - 4: IF up% < 1 THEN up% = 1: IF sonido = "si" THEN BEEP
DO
GET #2, up%: IF Prod$ = MID$(nomshort$, 1, LEN(Prod$)) THEN EXIT DO ELSE up% = up% + 1
LOOP
resal% = 1: GOSUB LeeCinco
RETURN
Apgdn:
up% = up% + 4: IF up% > max2% THEN up% = max2%: IF sonido = "si" THEN BEEP
DO
GET #2, up%: IF Prod$ = MID$(nomshort$, 1, LEN(Prod$)) THEN EXIT DO ELSE up% = up% - 1
LOOP
resal% = 1: GOSUB LeeCinco
RETURN
LeeCinco:
selecc% = 0
FOR k = up% TO up% + 4
     IF k > max2% THEN EXIT FOR
     GET #2, k
     IF Prod$ = MID$(nomshort$, 1, LEN(Prod$)) THEN
          GET #1, CVI(num$): selecc% = selecc% + 1
          Nombra(selecc%) = nomb2$: Cant1a(selecc%) = exi12$: Cant2a(selecc%) = exi22$: Cant3a(selecc%) = exi32$: Cant4a(selecc%) = exi42$: Pvp1a(selecc%) = pv12$: Pvp2a(selecc%) = pv22$: Pvp3a(selecc%) = pv32$: Pvp4a(selecc%) = pv42$:  Numa(selecc% _
) = num$
     ELSE
          EXIT FOR
     END IF
NEXT k
RETURN
Parche1:
          encontrado = false: primero = 1: ultimo = max2%
          WHILE NOT encontrado AND primero <= ultimo
               central = (primero + ultimo) / 2: GET #2, central
               IF Prod$ = MID$(nomshort$, 1, LEN(Prod$)) THEN
                    encontrado = true
               ELSE
                    IF MID$(nomshort$, 1, LEN(Prod$)) > Prod$ THEN
                         ultimo = central - 1
                    ELSE
                         primero = central + 1
                    END IF
               END IF
          WEND: salir = false
          DO WHILE salir = false
               central = central - 1: IF central < 1 THEN salir = true: central = 1
               GET #2, central
               IF Prod$ <> MID$(nomshort$, 1, LEN(Prod$)) THEN salir = true: central = central + 1
          LOOP
RETURN
Parche2:
IF encontrado = false THEN LOCATE fila% + 5, 5: PRINT STRING$(5, "Ä"); "²No se encontr¢²"; STRING$(7, "Ä"); : Espera: Escoge% = 0: EXIT FUNCTION
salir = false: selecc% = 0: up% = central
FOR k = central TO central + 4
     IF k > max2% THEN EXIT FOR
     GET #2, k
     IF Prod$ = MID$(nomshort$, 1, LEN(Prod$)) THEN
          GET #1, CVI(num$): selecc% = selecc% + 1
          Nombra(selecc%) = nomb2$: Cant1a(selecc%) = exi12$: Cant2a(selecc%) = exi22$: Cant3a(selecc%) = exi32$: Cant4a(selecc%) = exi42$: Pvp1a(selecc%) = pv12$: Pvp2a(selecc%) = pv22$: Pvp3a(selecc%) = pv32$: Pvp4a(selecc%) = pv42$:  Numa(selecc% _
) = num$
     ELSE
          EXIT FOR
     END IF
NEXT k
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila% - 2, 1: PRINT SPACE$(70): COLOR colors(4, colorpref), colors(7, colorpref)
wer$ = STRING$(36, "Ä") + "CAN1ÄÄÄÄÄÄPVP1ÂCAN2" + STRING$(6, "Ä") + "PVP2ÂCAN3" + STRING$(5, "Ä") + "PVP3" + "Ä"
LOCATE fila% - 1, 1: PRINT wer$;
wer$ = STRING$(80, "Ä"): LOCATE fila% + 5, 1: PRINT wer$; : COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE fila%, 1: PRINT Nombra(1); "³"; : LOCATE fila%, 37: PRINT USING "#####, "; CVI(Cant1a(1)); : LOCATE fila%, 51: PRINT "³"; : PRINT USING "#####, "; CVI(Cant2a(1)); : LOCATE fila%, 66: PRINT "³"; : PRINT USING "#####, "; CVI(Cant3a(1))
LOCATE fila%, 44: PRINT USING "######,"; CVD(Pvp1a(1)): LOCATE fila%, 59: PRINT USING "######,"; CVD(Pvp2a(1)): LOCATE fila%, 74: PRINT USING "######,"; CVD(Pvp3a(1)): COLOR colors(4, colorpref), colors(7, colorpref)
LOCATE fila% - 2, 50: PRINT "En Oferta:"; : PRINT USING "#####, "; CVI(Cant4a(1)); : PRINT " a Bs."; : PRINT USING "######,"; CVD(Pvp4a(1))
FOR k = 2 TO 5
     IF k <= selecc% THEN
          LOCATE fila% + k - 1, 1: PRINT Nombra(k); "³"; : LOCATE fila% + k - 1, 37: PRINT USING "#####, "; CVI(Cant1a(k)); : LOCATE fila% + k - 1, 51: PRINT "³"; : PRINT USING "#####, "; CVI(Cant2a(k));
          LOCATE fila% + k - 1, 66: PRINT "³"; : PRINT USING "#####, "; CVI(Cant3a(k)): LOCATE fila% + k - 1, 44: PRINT USING "######,"; CVD(Pvp1a(k)): LOCATE fila% + k - 1, 59: PRINT USING "######,"; CVD(Pvp2a(k))
          LOCATE fila% + k - 1, 74: PRINT USING "######,"; CVD(Pvp3a(k))
     ELSE
          LOCATE fila% + k - 1, 1: PRINT SPACE$(80)
     END IF
NEXT k: resal% = 1
RETURN
END FUNCTION

SUB Espera
Center 25, " Presione Cualquier Tecla Para Continuar ... "
DO: LOOP WHILE INKEY$ = ""
END SUB

SUB LeeEstado
CLOSE : OPEN "F:\kevinprg\kevindat\kaos.dat" FOR INPUT AS #2
INPUT #2, sonido: INPUT #2, Impuesto: INPUT #2, NombFarma: INPUT #2, Direccion
INPUT #2, Rif: INPUT #2, Nit: INPUT #2, Telefono: INPUT #2, contri
INPUT #2, contra: INPUT #2, Stock: INPUT #2, Blanq: INPUT #2, LargoH
INPUT #2, TipoL: INPUT #2, Mensaje1: INPUT #2, Mensaje2: INPUT #2, Mensaje3
INPUT #2, Redondeo: INPUT #2, Desc: INPUT #2, DescMax: INPUT #2, Exactitud
INPUT #2, Prov1: INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo
CLOSE
END SUB

FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$)
LOCATE fila%, columna%: PRINT CHR$(178) + caden$: LOCATE fila%, columna% + largo% + 1: PRINT CHR$(178): REDIM eleccion(0 TO largo% + 1) AS STRING * 1: tr$ = "0": llevasitio% = 1
FOR k = 1 TO largo%: eleccion(k) = MID$(caden$, k, 1): NEXT k: LOCATE fila%, (columna% + llevasitio%): COLOR colors(7, colorpref) + 16, colors(4, colorpref): IF eleccion(1 + VAL(tr$)) = " " THEN PRINT "_";  ELSE PRINT eleccion(1 + VAL(tr$))
DO
      llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
          SELECT CASE llave$
               CASE CHR$(0) + "H": devolver = true: EXIT DO
               CASE CHR$(0) + "P": EXIT DO
               CASE CHR$(0) + "K"
                  llevasitio% = llevasitio% - 1: IF llevasitio% = 0 THEN GOSUB Sumar: devolver = true: EXIT DO
               CASE CHR$(0) + "M"
                  llevasitio% = llevasitio% + 1: IF llevasitio% > largo% THEN GOSUB Sumar: EXIT DO
               CASE CHR$(8)
                  llevasitio% = llevasitio% - 1: IF llevasitio% = 0 THEN GOSUB Sumar: EXIT DO
                  FOR k = llevasitio% TO largo% - 1: eleccion(k) = eleccion(k + 1)
                  NEXT k: eleccion(largo%) = " "
               CASE CHR$(0) + CHR$(83)
                  FOR k = llevasitio% TO largo%: eleccion(k) = eleccion(k + 1): NEXT k
                  eleccion(largo%) = " "
               CASE CHR$(13): EXIT DO
               CASE CHR$(27): devolver = true: EXIT DO
               'CASE CHR$(0) + CHR$(59): Fun(1) = "si": EXIT DO
               CASE CHR$(0) + CHR$(60): Fun(2) = "si": EXIT DO
               CASE CHR$(0) + CHR$(61): Fun(3) = "si": EXIT DO
               CASE CHR$(0) + CHR$(62): Fun(4) = "si": EXIT DO
               CASE CHR$(0) + CHR$(63): Fun(5) = "si": EXIT DO
               CASE CHR$(0) + CHR$(64): Fun(6) = "si": EXIT DO
               CASE CHR$(0) + CHR$(65): Fun(7) = "si": EXIT DO
               CASE CHR$(0) + CHR$(66): Fun(8) = "si": EXIT DO
               CASE CHR$(0) + CHR$(67): Fun(9) = "si": EXIT DO
               CASE CHR$(0) + CHR$(68): Fun(10) = "si": EXIT DO
               CASE CHR$(0) + CHR$(71): llevasitio% = 1
               CASE CHR$(0) + CHR$(79): llevasitio% = largo%
               CASE ELSE
                  eleccion(llevasitio%) = llave$
                  llevasitio% = llevasitio% + 1
                  IF llevasitio% > largo% THEN GOSUB Sumar: EXIT DO
          END SELECT
          FOR k = 1 TO largo%
          IF k = llevasitio% THEN COLOR colors(4, colorpref) + 16, colors(7, colorpref)
          LOCATE fila%, (columna% + k)
          IF k = llevasitio% THEN
                    IF eleccion(k) <> " " THEN PRINT eleccion(k);  ELSE PRINT "_";
          ELSE
                    PRINT eleccion(k);
          END IF
          IF k = llevasitio% THEN COLOR colors(7, colorpref), colors(4, colorpref)
          NEXT k
LOOP
eleccion$ = "": FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR colors(5, colorpref), colors(4, colorpref): LOCATE fila%, columna% + 1: PRINT eleccion$
MeteDatoLetra$ = eleccion$: IF sonido = "si" THEN BEEP
EXIT FUNCTION
Sumar:
FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna%: PRINT "²"; eleccion$; "²"
RETURN
END FUNCTION

SUB MeteFact
REM si guia=1 , factura de contado ; si guia=2 , factura a credito
DIM congela AS SINGLE, miDesc AS Descuentos

CLOSE #77: porcentaje$ = "30 +   +   +  6 "
OPEN "F:\kevinprg\kevindat\congela.dat" FOR INPUT AS #77
IF errNum = 0 THEN
   res% = Verify%(10, "Hay una factura de compra congelada ¨DESEA CARGARLA")
   IF res% = false THEN EXIT SUB
   COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box 1, 1, 3, 80: Center 2, "Recuperaci¢n de una factura congelada"
   INPUT #77, num2$: CLOSE #77
   CLOSE : OPEN "F:\kevinprg\kevindat\master.fct" FOR RANDOM AS #27 LEN = 70
   FIELD #27, 1 AS acti$, 4 AS numfact$, 8 AS fechaacc$, 10 AS vendedor$, 2 AS prove$, 1 AS contacredi$, 8 AS brut$, 8 AS net$, 8 AS ivafact$, 8 AS fechafact$, 1 AS actual$, 4 AS numped$, 1 AS cancelada$, 6 AS vacio$
   final% = LOF(27) / 70: GET #27, final%: oldbruto = brut$: oldiva = ivafact$
   razon = MKD$(CVD(net$) / CVD(brut$))
   OPEN num2$ FOR RANDOM AS #2 LEN = 40: FIELD #2, 6 AS odigo$, 2 AS antid$, 8 AS recio$, 8 AS osto$, 8 AS ferta$, 2 AS oferantid$, 1 AS va$, 2 AS sitio$, 3 AS venci$
   CLOSE #1: OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
   FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS  _
cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
   seguir = true: Fun(16) = "no"
   neto$ = MKD$(0)
   bruto = MKD$(0)
   Iva = MKD$(0): lleped% = LOF(2) / 40
   FOR k = 1 TO lleped%: GET #2, k
      IF odigo$ <> "BORRA " THEN
        bruto = MKD$(CVD(osto$) + CVD(bruto)): unidades = MKL$(CVL(unidades) + CVI(oferantid$) + CVI(antid$)):
        IF va$ = "I" THEN Iva = MKD$(CVD(Iva) + (CVD(osto$) * VAL(Impuesto) / 100))
        renglones = MKI$(CVI(renglones) + 1)
      END IF
   NEXT k
   IF CVI(renglones) > 0 THEN
      apu% = 1: lleped% = CVI(renglones)
   ELSE
      lleped% = 0: Fun(7) = "si": sit% = 20: apu% = lleped%
   END IF
ELSE
   '----------

   COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box 1, 1, 3, 80: CLOSE
   IF guia = 1 THEN Center 2, "Introduccion de Facturas de Contado." ELSE Center 2, "Introduccion de Facturas a Credito."
   OPEN "F:\kevinprg\kevindat\master.pdd" FOR RANDOM SHARED AS #7 LEN = 67: salir = false: cspedido = false
   FIELD #7, 1 AS viva$, 4 AS numped$, 2 AS numprov$, 5 AS hora$, 10 AS fecha$, 4 AS cedven$, 40 AS obse$, 1 AS tipo$: selecc% = (LOF(7) / 67)
   IF selecc% < 2 THEN
        DO WHILE salir = false
             Box 10, 10, 20, 70: Center 12, "No Hay Solicitudes de Pedido Pendientes!"
             Center 14, "Desea Introducir Factura Sin Pedido Previo [S/N]?": Center 25, "Pulse - S -  ¢  - N -"
             res$ = MeteDatoLetra(14, 65, 1, "N"): res$ = UCASE$(res$): Box 10, 5, 15, 75: Center 12, "INTRODUZCA BIEN LOS DATOS , POR FAVOR ..."
             IF res$ = "S" THEN salir = true: cspedido = true: eleg% = 1: Fun(7) = "si": sit% = 0: lleped% = 0
             IF res$ = "N" THEN CLOSE : EXIT SUB
        LOOP
   END IF
   IF cspedido = false THEN
        otrsel% = 1
        FOR k = 2 TO selecc%: GET #7, k
             IF viva$ = " " THEN otrsel% = otrsel% + 1
        NEXT k: SWAP selecc%, otrsel%: l = 2
        DIM pedido(selecc%) AS STRING * 4, provee(selecc%) AS STRING * 18, tiempo(selecc%) AS STRING * 16, pedven(selecc%) AS STRING * 24, ojo(selecc%) AS STRING * 40, tipe(selecc%) AS STRING * 1
        FOR k = 2 TO otrsel%: GET #7, k
             IF viva$ = " " THEN
             pedido(l) = numped$: provee(l) = numprov$: tiempo(l) = hora$ + " " + MID$(fecha$, 4, 2) + "/" + MID$(fecha$, 1, 2) + "/" + MID$(fecha$, 7, 4)
             pedven(l) = cedven$: ojo(l) = obse$: tipe(l) = tipo$: l = l + 1
             END IF
        NEXT k: fila% = 6
        pedido(1) = MKL$(0): provee(1) = MKI$(1) + " Ninguno": tiempo(1) = "SIN Pedido      "
        pedven(1) = MKL$(0) + SPACE$(20): ojo(1) = "Tome Esta Si NO Hay Solicitud de Pedido"
        OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$
        FOR k = 1 TO 199: GET #2, k: Code(k) = codigo$: Contenido(k) = descripcion$: NEXT k: CLOSE #2
        FOR k = 2 TO selecc%: FOR l = 1 TO 199
             IF CVI(MID$(provee(k), 1, 3)) = VAL(Code(l)) THEN provee(k) = Code(l) + Contenido(l): l = 199
        NEXT l: NEXT k
        OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #1 LEN = 100
        FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS Cong$, 1 AS gaveta$, 19 AS vac$
        nursaler = LOF(1) / 100
        FOR k = 2 TO selecc%: FOR l = 1 TO nursaler
        GET #1, l: IF MID$(pedven(k), 1, 4) = cedu$ THEN MID$(pedven(k), 5, 20) = nomb$: l = nursaler
        NEXT l: NEXT k: CLOSE #1
        COLOR colors(7, colorpref), colors(4, colorpref): Box 4, 5, 16, 68: Center 4, "Pedidos Registrados": LOCATE 5, 6: PRINT "#Pedido: Proveedor:      Hora y Fecha:    Pedido Por:"
        Box 16, 5, 19, 68
        Center 25, "Use Las Teclas de Direccion y Enter ; Presione Escape Para Salir": COLOR colors(7, colorpref), colors(4, colorpref)
        LOCATE fila%, 6: PRINT USING "#######,³"; CVL(pedido(1)); : PRINT MID$(provee(1), 4, 15); "³"; tiempo(1); "³";
        PRINT MID$(pedven(1), 5, 20): LOCATE 17, 20: PRINT ojo(1);
        COLOR colors(4, colorpref), colors(7, colorpref)
        FOR k = 2 TO 10
             IF k <= selecc% THEN
                  LOCATE fila% + k - 1, 6: PRINT USING "#######,³"; CVL(pedido(k)); : PRINT MID$(provee(k), 4, 15); "³"; tiempo(k); "³";
                  PRINT MID$(pedven(k), 5, 20);
             ELSE
                  LOCATE fila% + k - 1, 6: PRINT SPACE$(62)
             END IF
        NEXT k
        resal% = 1: up% = 1: IF selecc% > 10 THEN ventana% = 10 ELSE ventana% = selecc%
        DO
             kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
             SELECT CASE kbd$
                  CASE CHR$(0) + "H": GOSUB Arri
                  CASE CHR$(0) + "P": GOSUB Abaj
                  CASE CHR$(0) + CHR$(73): IF selecc% > 10 THEN GOSUB Agup ELSE BEEP
                  CASE CHR$(0) + CHR$(81): IF selecc% > 10 THEN GOSUB Agdn ELSE BEEP
                  CASE CHR$(13)
                       eleg% = (up% + resal% - 1): IF eleg% > 1 THEN cspedido = true
                       EXIT DO
                  CASE CHR$(27): resal% = 0: up% = 1: EXIT SUB
                  CASE ELSE:  BEEP
             END SELECT
        l = 0
        FOR k = up% TO up% + 9: l = l + 1   ': LOCATE 1, 1: PRINT "k="; k, "up%="; up%, "l="; l, "resal%="; resal%, "selecc%="; selecc%: SLEEP 1
        IF resal% = l THEN
             COLOR colors(7, colorpref), colors(4, colorpref)
             LOCATE 17, 20: PRINT ojo(k); : LOCATE 18, 25
             IF tipe(k) = "1" THEN PRINT "***  Pedido de Medicinas   ***"
             IF tipe(k) = "2" THEN PRINT "***  Pedido de Miscelaneas ***"
             IF tipe(k) = "3" THEN PRINT "***  Pedido de Quimicos    ***"
             FOR t = 20 TO 24: Center t, SPACE$(35): NEXT t
             CLOSE #99: CLOSE #100
             OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #100 LEN = 200
             FIELD #100, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$ _
, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS _
 compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
             soliC$ = STR$(CVL(pedido(k)))
             soliC$ = Trim$(MID$(soliC$, 2, 8))
             soliC$ = "F:\kevinprg\kevindat\" + STRING$(8 - LEN(soliC$), "0") + soliC$ + ".pdd"
             OPEN soliC$ FOR RANDOM SHARED AS #99 LEN = 19
             FIELD #99, 19 AS todo$: max99% = LOF(99) / 19
             FOR t = 1 TO 5
               IF t <= max99% THEN
                  GET #99, t: GET #100, CVI(MID$(todo$, 18, 2))
                  Center 19 + t, nomb2$
               ELSE
                  EXIT FOR
               END IF
             NEXT t
             CLOSE #99: CLOSE 100
        END IF
             IF k <= selecc% THEN
                  LOCATE fila% + l - 1, 6: PRINT USING "#######,³"; CVL(pedido(k)); : PRINT MID$(provee(k), 4, 15); "³"; tiempo(k); "³";
                  PRINT MID$(pedven(k), 5, 20);
             ELSE
                  LOCATE fila% + l - 1, 6: PRINT SPACE$(62)
             END IF
        IF resal% = l THEN COLOR colors(4, colorpref), colors(7, colorpref)
        NEXT k
        LOOP: salir = false
   ELSE
        DIM pedido(1) AS STRING * 4, provee(1) AS STRING * 18, tiempo(1) AS STRING * 16, pedven(1) AS STRING * 24, ojo(1) AS STRING * 40
   END IF
   salir = false
   DO WHILE NOT salir
        COLOR colors(7, colorpref), colors(4, colorpref): CLS : Center 25, "[F1]Ayuda  [F9]Salir Sin Salvar": control% = 0
        IF guia = 1 THEN Center 1, "Introduccion de Facturas de Contado." ELSE Center 1, "Introduccion de Facturas a Credito."
        IF eleg% = 1 THEN
             DO WHILE control% = 0
                  COLOR colors(5, colorpref), colors(4, colorpref): LOCATE 2, 1: PRINT " Escoja el Proveedor:²00 ²Ninguno     "
                  control% = Elije%(5, 3, 22, 2, 23, 0, 0): IF control% > 99 OR control% < 1 THEN control% = 0
                  IF VAL(Code(control%)) = 1 OR control% = 0 THEN control% = 0: Center 24, "Proveedor No valido En Este Modulo de Compra.": Espera
                  eleg% = 1: provee(1) = Code(control%) + Contenido(control%): COLOR colors(7, colorpref), colors(4, colorpref): FOR k = 2 TO 25: Center k, SPACE$(80): NEXT k
             LOOP
        ELSE
             OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$
             FOR k = 1 TO 99: GET #2, k: Code(k) = codigo$: Contenido(k) = descripcion$: NEXT k: CLOSE #2
        END IF
        COLOR colors(5, colorpref), colors(4, colorpref): LOCATE 2, 1: PRINT "Proveedor:²"; MID$(provee(eleg%), 1, 3); "²"; MID$(provee(eleg%), 4, 15); "²"
        LOCATE 2, 31: PRINT "#Factura:²        ²": LOCATE 2, 50: PRINT "Fecha:²          ²": Center 25, "[F9] Salir"
        Prud1$ = EntraDatoNumero(2, 40, 8, " 0", 1)
        IF Fun(9) = "si" THEN Fun(9) = "no": EXIT SUB
        Prud2$ = MeteDatoLetra(2, 56, 10, "  /  /    ")
        IF Fun(9) = "si" THEN Fun(9) = "no": EXIT SUB
        Fun(10) = "no": neto = MKD$(0): bruto = MKD$(0): Iva = MKD$(0): Box 10, 5, 15, 75: Center 11, "Tipee Los Montos CON EXACTITUD Y REVISE SI SON CORRECTOS": Center 25, "[F1] Ayuda [F9] Salir SIN Procesar [F10] Aceptar los Montos A Memoria"
        LOCATE 12, 6: PRINT "Total (PVP - % de ganancia):": LOCATE 13, 6: PRINT "Total Impuesto (PVP - % de ganancia x % alicuota):": LOCATE 14, 6: PRINT "Total Neto (PVP - % de ganancia - % adicional):"
        DO WHILE Fun(10) = "no"
             bruto = MKD$(VAL(EntraDatoNumero(12, 58, 14, STR$((FIX(CVD(bruto) * 100)) / 100), 0))): neto = bruto: oldbruto = bruto
             IF Fun(9) = "si" THEN Fun(9) = "no": EXIT SUB
             IF Fun(10) = "si" THEN EXIT DO
             LOCATE 12, 59: PRINT USING "##########,.##"; CVD(bruto): LOCATE 14, 59: PRINT USING "##########,.##"; CVD(neto)
             Iva = MKD$(VAL(EntraDatoNumero(13, 58, 14, STR$((FIX(CVD(Iva) * 100)) / 100), 0))): IF Fun(9) = "si" THEN Fun(9) = "no": EXIT SUB
             IF Fun(10) = "si" THEN EXIT DO
             LOCATE 13, 59: PRINT USING "##########,.##"; CVD(Iva)
             neto = MKD$(VAL(EntraDatoNumero(14, 58, 14, STR$((FIX(CVD(neto) * 100)) / 100), 0))): IF Fun(9) = "si" THEN Fun(9) = "no": EXIT SUB
             LOCATE 14, 59: PRINT USING "##########,.##"; CVD(neto)
        LOOP: oldiva = Iva
        OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #4 LEN = 100: ClaveLarga$ = ""
        FIELD #4, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS Cong$, 1 AS gaveta$, 19 AS vac$: nursaler = LOF(4) / 100
        Box 10, 5, 15, 75: Center 11, "Pulse Ahora las Tres Letras de Su Codigo de Vendedor (ClaveCorta)": Center 25, "Introduzca Su Clave Corta De Vendedor , Por Favor ..."
        DO WHILE ClaveLarga$ = ""
             eleccion$ = MeteDatoLetra$(12, 40, 3, "   ")
             FOR k = 1 TO nursaler: GET #4, k
             IF eleccion$ = clco$ AND acti$ = " " THEN ClaveLarga$ = clla$: cedula$ = cedu$: EXIT FOR
             NEXT k
             IF ClaveLarga$ = "" THEN
                  Center 23, "Introduzca un Codigo de Vendedor Valido": Center 24, "No Hay Ningun Vendedor Con Esa Clave o Esta Borrado": Espera
                  Center 23, "                                       ": Center 24, "                                                   ": Center 25, SPACE$(80)
             END IF
        LOOP: CLOSE #4
        Box 10, 5, 15, 75: Center 11, "Desea Procesar En Este Momento la FACTURA [S/N]?": Center 12, "Atencion:Si Responde Que SI,Se Grabara Factura En Edo. de Cuenta": Center 25, "Pulse - S (si) - ¢ - N (no) -"
        res$ = MeteDatoLetra(11, 65, 1, "N"): res$ = UCASE$(res$): Box 10, 5, 15, 75: Center 12, "INTRODUZCA BIEN LOS DATOS , POR FAVOR ..."
        IF res$ = "S" THEN salir = true
        IF res$ = "N" THEN CLOSE : EXIT SUB
        IF VAL(Prud1$) < 1 THEN salir = false: Center 24, "El Numero de Factura Debe ser MAYOR Que Cero": Espera
        IF CheqDate(Prud2$, 1) = false THEN salir = false: Center 24, "Introdujo Una Fecha Incorrecta:" + Prud2$: Espera
        IF CVD(bruto) = 0 THEN salir = false: Center 24, "El Total De La Factura NO PUEDE ser Igual a Cero (No es Gratis)"
        IF CVD(neto) = 0 OR CVD(neto) > CVD(bruto) THEN salir = false: Center 24, "El (Total-Descuento Adicional) NO Debe Ser Mayor al Total ¢ Ser Igual a Cero": Espera
   LOOP: Box 10, 5, 15, 75: Center 11, "Procesando , Espere Por Favor ... ": razon = MKD$(CVD(neto) / CVD(bruto))
   OPEN "F:\kevinprg\kevindat\master.fct" FOR RANDOM AS #27 LEN = 70
   FIELD #27, 1 AS acti$, 4 AS numfact$, 8 AS fechaacc$, 10 AS vendedor$, 2 AS prove$, 1 AS contacredi$, 8 AS brut$, 8 AS net$, 8 AS ivafact$, 8 AS fechafact$, 1 AS actual$, 4 AS numped$, 1 AS cancelada$, 6 AS vacio$
   final% = (LOF(27) / 70) + 1
   FOR k = 1 TO final% - 1: GET #27, k
   IF acti$ = " " THEN
     IF CVL(numfact$) = VAL(Prud1$) AND VAL(MID$(provee(eleg%), 1, 3)) = CVI(prove$) THEN
       IF (DateToJulian(DATE$, 3) - DateToJulian(fechafact$, 4)) < 60 THEN
        Box 10, 5, 17, 75: Center 11, "Esta Factura Ya ESTA Registrada en Memoria:": Center 12, "Accesada el " + MID$(fechaacc$, 1, 2) + "/" + MID$(fechaacc$, 3, 2) + "/" + MID$(fechaacc$, 5, 4)
        Center 13, "Transcrita por: " + vendedor$
        LOCATE 14, 25: PRINT USING "Monto Bruto Bs ###########,.##"; CVD(brut$)
        LOCATE 15, 25: PRINT USING "Monto Neto  Bs ###########,.##"; CVD(net$)
        LOCATE 16, 25: PRINT USING "Monto ICSVM Bs ###########,.##"; CVD(ivafact$)
        CALL Espera: EXIT SUB
       END IF
     END IF
   END IF
   NEXT k
   RSET acti$ = " "
   RSET numfact$ = MKL$(VAL(Prud1$))
   LSET fechaacc$ = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4)
   RSET vendedor$ = ClaveLarga$
   RSET prove$ = MKI$(VAL(MID$(provee(eleg%), 1, 3)))
   'LOCATE 1, 1: PRINT VAL(MID$(provee(eleg%), 1, 3)): Espera
   LSET brut$ = bruto
   LSET net$ = neto
   LSET ivafact$ = Iva
   LSET fechafact$ = MID$(Prud2$, 4, 2) + MID$(Prud2$, 1, 2) + MID$(Prud2$, 7, 4)
   LSET actual$ = " "
   IF guia = 1 THEN
        IF cspedido = true THEN
             LSET contacredi$ = " ": LSET numped$ = pedido(eleg%): LSET cancelada$ = "*"
        ELSE
             LSET contacredi$ = " ": LSET numped$ = MKL$(0): LSET cancelada$ = "*"
        END IF
        PUT #27, final%: CLOSE
        num3$ = "F:\kevinprg\kevindat\master." + MID$(provee(eleg%), 1, 3)
        OPEN num3$ FOR RANDOM AS #27 LEN = 50: FIELD #27, 50 AS linea$: final% = (LOF(27) / 50) + 1
        LSET linea$ = "FCT" + Prud1$ + "Fct. de Contado" + neto + Iva: PUT #27, final%: CLOSE
   ELSE
        IF cspedido = true THEN
             LSET contacredi$ = "*": LSET numped$ = pedido(eleg%): LSET cancelada$ = " "
        ELSE
             LSET contacredi$ = "*": LSET numped$ = MKL$(0): LSET cancelada$ = " "
        END IF
        PUT #27, final%: CLOSE
   END IF
   IF eleg% > 1 THEN
        num3$ = STR$(CVL(pedido(eleg%))): num3$ = Trim$(MID$(num3$, 2, 8)): num3$ = "F:\kevinprg\kevindat\" + STRING$(8 - LEN(num3$), "0") + num3$ + ".pdd"
        OPEN num3$ FOR RANDOM AS #3 LEN = 19: FIELD #3, 6 AS codi7$, 2 AS cant7$, 8 AS pvp37$, 1 AS paf7$, 2 AS sit7$: maxped% = LOF(3) / 19: lleped% = 0
        num2$ = STR$(VAL(Prud1$)): num2$ = Trim$(MID$(num2$, 2, 8)): num2$ = "F:\kevinprg\kevindat\" + STRING$(8 - LEN(num2$), "0") + num2$ + "." + MID$(provee(eleg%), 1, 3)
        OPEN num2$ FOR RANDOM AS #2 LEN = 40: FIELD #2, 6 AS odigo$, 2 AS antid$, 8 AS recio$, 8 AS osto$, 8 AS ferta$, 2 AS oferantid$, 1 AS va$, 2 AS sitio$, 3 AS venci$
        FOR k = 1 TO maxped%: GET #3, k
             'IF paf7$ = " " THEN
                  'LOCATE 1, 1: PRINT codi7$: Espera
                  RSET odigo$ = codi7$: RSET antid$ = cant7$: RSET venci$ = "   "
                  RSET recio$ = pvp37$: RSET oferantid$ = MKI$(0): RSET va$ = " ": RSET sitio$ = "  "
                  unidades = MKL$(CVL(unidades) + CVI(antid$)): lleped% = lleped% + 1: PUT #2, lleped%: PUT #3, k
             'END IF
        NEXT k: CLOSE
   ELSE
        num2$ = STR$(VAL(Prud1$)): num2$ = Trim$(MID$(num2$, 2, 8)): num2$ = "F:\kevinprg\kevindat\" + STRING$(8 - LEN(num2$), "0") + num2$ + "." + MID$(provee(eleg%), 1, 3)
        'LOCATE 1, 1: PRINT num2$: Espera
        OPEN num2$ FOR RANDOM AS #2 LEN = 40: FIELD #2, 6 AS odigo$, 2 AS antid$, 8 AS recio$, 8 AS osto$, 8 AS ferta$, 2 AS oferantid$, 1 AS va$, 2 AS sitio$, 3 AS venci$
   END IF

   CLOSE : OPEN "F:\kevinprg\kevindat\codigo.idx" FOR RANDOM SHARED AS #3 LEN = 8
   FIELD #3, 6 AS nomshort$, 2 AS num$: max2% = LOF(3) / 8
   OPEN "f:\kevinprg\kevindat\congela.dat" FOR OUTPUT AS #2
   PRINT #2, num2$: CLOSE #2
   OPEN num2$ FOR RANDOM AS #2 LEN = 40: FIELD #2, 6 AS odigo$, 2 AS antid$, 8 AS recio$, 8 AS osto$, 8 AS ferta$, 2 AS oferantid$, 1 AS va$, 2 AS sitio$, 3 AS venci$: lleped% = LOF(2) / 40

   IF eleg% > 1 THEN
   Termometro 0, lleped%, 0, "Descomprimiendo Archivo", 10, 10
        FOR k = 1 TO lleped%: GET #2, k: Termometro k, lleped%, 1, "Descomprimiendo Archivo", 10, 10
             encontrado = false: primero = 1: ultimo = max2%: Prod$ = odigo$    ': LOCATE 1, 1: PRINT odigo$, Prod$: Espera
             WHILE NOT encontrado AND primero <= ultimo
                  central = (primero + ultimo) / 2: GET #3, central
                  IF Prod$ = nomshort$ THEN
                       encontrado = true
                  ELSE
                       IF nomshort$ > Prod$ THEN
                            ultimo = central - 1
                       ELSE
                            primero = central + 1
                       END IF
                  END IF
             WEND: GET #3, central: RSET sitio$ = num$: RSET ferta$ = MKD$(VAL(Desc)): PUT #2, k
        NEXT k
   END IF
   CLOSE #1: OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
   FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS  _
cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
   seguir = true: neto$ = MKD$(0): bruto = MKD$(0): Iva = MKD$(0): renglones = MKI$(lleped%)
   IF eleg% > 1 OR cspedido = true THEN apu% = 1 ELSE lleped% = 0: Fun(7) = "si": sit% = 20: apu% = lleped%

   '-------------
END IF

LOCATE 5, 1: PRINT "ÄDescripci¢nÄÄÄÄÄÄÄÄÄÄÄÄÂ% GanÂCantÄÄÂOferÄÂÄPVP ÄÄÄÄÄÂdesc.publ.ÂÄÄCostoÄÄÄÄÄÂÄ";
LOCATE 17, 1: PRINT "ÄDescripci¢nÄÄÄÄÄÄÄÄÄÄÄÄÁ%ÄGanÁCantÄÄÁOferÄÁÄPVP ÄÄÄÄÄÁdesc.publ.ÁÄÄCostoÄÄÄÄÄÁÄ"; : Center 11, "Proceso Hecho,Continue Por Favor ... "

DO WHILE seguir
'------------------------------------Impresionable:
IF lleped% > 0 THEN
     Center 25, "F2=Dsc F3=Venc F4=Ir F5=Iva F6=Nuev F7=Ins F8=Borr F10=Salv F11=Abor F12=Cng"
     FOR k = 1 TO 12: Fun(k) = "no": NEXT k: sit% = 2: COLOR colors(5, colorpref), colors(4, colorpref): l = 5
     FOR k = apu% - 5 TO apu% + 5
          IF k = apu% THEN COLOR colors(4, colorpref), colors(7, colorpref) ELSE COLOR colors(7, colorpref), colors(4, colorpref)
          l = l + 1: LOCATE l, 1: PRINT SPACE$(80): LOCATE l, 1: porc = MKD$(0)
          IF k > 0 AND k <= lleped% THEN
               GET #2, k: GET #1, CVI(sitio$)
               'OPEN "f:\kevinprg\kevindat\desc.dat" FOR RANDOM SHARED AS #38 LEN = LEN(miDesc)
               'GET #38, CVI(sitio$), miDesc: CLOSE #38
               LOCATE l, 1: PRINT MID$(nomb2$, 1, 24); "³";
               IF CVD(osto$) > 0 THEN
                    IF va$ = "I" THEN
                        porc = MKD$(100 - (((CVD(osto$) / (CVI(antid$)))) / (CVD(recio$) / (VAL(Impuesto) / 100 + 1))) * 100)
                    ELSE
                        porc = MKD$(100 - (((CVD(osto$) / (CVI(antid$)))) / CVD(recio$)) * 100)
                    END IF
                    IF CVD(porc) < 29.95 THEN COLOR colors(4, colorpref) + 16, colors(7, colorpref)
                    'LOCATE 1, 1: PRINT CVD(porc)
                    IF CVD(porc) >= 0 AND CVD(porc) < 99 THEN
                      PRINT USING "##.##"; CVD(porc);
                    ELSE
                      PRINT "- - -";
                    END IF
               ELSE
                    COLOR colors(4, colorpref) + 16, colors(7, colorpref): PRINT USING "##.##"; 0;
               END IF
               IF k = apu% THEN COLOR colors(4, colorpref), colors(7, colorpref) ELSE COLOR colors(7, colorpref), colors(4, colorpref)
               PRINT USING "³######³"; CVI(antid$); : PRINT USING "#####³"; CVI(oferantid$); : PRINT USING "######,.##³"; CVD(recio$); : PRINT USING "######,.##³"; CVD(ferta$); : PRINT USING "########,.##³"; CVD(osto$); : PRINT va$;
          ELSE
               LOCATE l, 1: PRINT "                        ³     ³      ³     ³          ³          ³            ³ ";
          END IF
          IF k = apu% THEN COLOR colors(7, colorpref), colors(4, colorpref)
     NEXT k
       GET #2, apu%: GET #1, CVI(sitio$): LOCK #1, CVI(sitio$)
       meaux% = CVI(sitio$): GOSUB Mostrar
END IF
     DO
     IF lleped% > 0 THEN
     COLOR colors(7, colorpref), colors(4, colorpref): LOCATE 3, 1: PRINT USING "Total=##########,.##"; CVD(bruto); : PRINT USING " Impuesto=##########,.##"; CVD(Iva) * CVD(razon); : PRINT USING " Renglones=#####,"; CVI(renglones); : PRINT USING  _
" Unidades=########,"; CVL(unidades);
     LOCATE 4, 1: PRINT "(Totales verdaderos:"; : PRINT USING "##########,.## "; CVD(oldbruto); : PRINT USING "Impuesto ##########,.## "; CVD(oldiva); : PRINT ")"; 'CVD(razon);
     LOCATE 17, 1: PRINT "ÄDescripci¢nÄÄÄÄÄÄÄÄÄÄÄÄÁ%ÄGanÁCantÄÄÁOferÄÁÄPVP ÄÄÄÄÄÁÄPVPOferÄÄÁÄÄCostoÄÄÄÄÄÁÄ";
               IF CVD(osto$) > 0 AND CVD(recio$) THEN
                    IF va$ = "I" THEN
                              porc = MKD$(100 - (((CVD(osto$) / (CVI(antid$)))) / (CVD(recio$) / (VAL(Impuesto) / 100 + 1))) * 100)
                    ELSE
                              porc = MKD$(100 - (((CVD(osto$) / (CVI(antid$)))) / CVD(recio$)) * 100)
                    END IF
               ELSE
                    porc = MKD$(0)
               END IF
          COLOR colors(4, colorpref), colors(7, colorpref): LOCATE 11, 1: PRINT MID$(nomb2$, 1, 24); "³"; : IF CVD(porc) < 30 THEN COLOR colors(4, colorpref) + 16, colors(7, colorpref)
          IF CVD(porc) >= 0 AND CVD(porc) < 99 THEN
            PRINT USING "##.##"; CVD(porc);
          ELSE
            PRINT "- - -";
          END IF
          COLOR colors(4, colorpref), colors(7, colorpref): devolver = false: Fun(16) = "no"
          PRINT USING "³######³"; CVI(antid$); : PRINT USING "#####³"; CVI(oferantid$); : PRINT USING "######,.##³"; CVD(recio$); : PRINT USING "######,.##³"; CVD(ferta$); : PRINT USING "########,.##³"; CVD(osto$); : PRINT va$;
          IF sit% = 20 THEN
               sit% = 20
          ELSE
               IF CVD(osto$) = 0 THEN sit% = 6
          END IF
          SELECT CASE sit%
          CASE 1   'porcentaje
               auxiliar$ = "0"
               DO WHILE VAL(auxiliar$) <= 0 OR VAL(auxiliar$) >= 100
               auxiliar$ = EntraDatoNumero$(11, 25, 5, MID$(STR$((FIX(CVD(porc) * 100)) / 100), 2, 5), 0)
               LOOP
               IF VAL(auxiliar$) <> CVD(porc) THEN
                    porc = MKD$(VAL(auxiliar$)): auxiliar$ = STR$(1 - (CVD(porc) / 100))
                    IF va$ = "I" THEN
                        RSET recio$ = MKD$(((CVD(osto$) / (CVI(antid$)) / VAL(auxiliar$)) * (VAL(Impuesto) / 100 + 1)))
                    ELSE
                        RSET recio$ = MKD$(((CVD(osto$) / (CVI(antid$)) / VAL(auxiliar$))))
                    END IF
               END IF
          CASE 2   'cantidad
               auxiliar$ = EntraDatoNumero$(11, 31, 6, STR$(CVI(antid$)), 1)
               LSET antid$ = MKI$(VAL(auxiliar$))
          CASE 3   'oferta cantidad 12+1,3+1,etc.
               auxiliar$ = EntraDatoNumero$(11, 38, 5, STR$(CVI(oferantid$)), 1)
               RSET oferantid$ = MKI$(VAL(auxiliar$))
          CASE 4   'pvp normal
               auxiliar$ = EntraDatoNumero$(11, 44, 10, STR$((FIX(CVD(recio$) * 100)) / 100), 0)
               RSET recio$ = MKD$(VAL(auxiliar$))
          CASE 5   'descuento al p£blico
               auxiliar$ = "100"
               DO WHILE ABS(VAL(auxiliar$)) > 99
                  auxiliar$ = EntraDatoNumero$(11, 55, 10, STR$((FIX(CVD(ferta$) * 100)) / 100), 0)
               LOOP
               RSET ferta$ = MKD$(VAL(auxiliar$))
          CASE 6   'costo sub-total ojo=( pvp - %ganancia * cantidad )
               auxiliar$ = EntraDatoNumero$(11, 66, 12, STR$((FIX(CVD(osto$) * 100)) / 100), 0)
               RSET osto$ = MKD$(VAL(auxiliar$))
          CASE ELSE
          END SELECT
     'descuentos dronena
    IF Fun(2) = "si" THEN
      Fun(2) = "no"
      CALL PopUp(12, 10, 5, 61, 112, 1, 0, 0)
      Box 12, 10, 16, 70: Center 13, "Introduzca los descuentos separados por signos de suma(+):": Center 15, "( por ejemplo 30+10+5 ¢ 25 + 5 + 4 + 3.5  "
      auxiliar$ = "0"
      DO WHILE VAL(auxiliar$) <= 0 OR VAL(auxiliar$) >= 100
        auxiliar$ = EntraDatoNumero$(14, 25, 40, porcentaje$, 0): porcentaje$ = auxiliar$
        auxiliar$ = Descuentos$(auxiliar$)
      LOOP
      IF VAL(auxiliar$) <> CVD(porc) THEN
           porc = MKD$(VAL(auxiliar$)): auxiliar$ = STR$(1 - (CVD(porc) / 100))
           IF va$ = "I" THEN
              RSET recio$ = MKD$(((CVD(osto$) / (CVI(antid$)) / VAL(auxiliar$)) * (VAL(Impuesto) / 100 + 1)))
           ELSE
              RSET recio$ = MKD$(((CVD(osto$) / (CVI(antid$)) / VAL(auxiliar$))))
           END IF
      END IF
      CALL ShutUp
    END IF
     IF Fun(3) = "si" THEN 'Vencimiento
      RSET preg$ = "*": PUT #1, CVI(sitio$): GOSUB PreguntaVenci: Fun(3) = "no"
     ELSE
      IF Fun(8) = "no" THEN
        IF preg$ = "*" AND venci$ = "   " THEN GOSUB PreguntaVenci
      END IF
     END IF
     IF Fun(5) = "si" THEN 'Iva
               Fun(5) = "no"
               IF va$ = " " THEN RSET va$ = "I" ELSE RSET va$ = " "
     END IF: PUT #1, CVI(sitio$): PUT #2, apu%
     bruto = MKD$(0): Iva = MKD$(0): unidades = MKD$(0)
     FOR k = 1 TO lleped%: GET #2, k
               bruto = MKD$(CVD(osto$) + CVD(bruto)): unidades = MKL$(CVL(unidades) + CVI(oferantid$) + CVI(antid$)):
               IF va$ = "I" THEN Iva = MKD$(CVD(Iva) + (CVD(osto$) * VAL(Impuesto) / 100))
     NEXT k: GET #2, apu%
     IF Fun(8) = "si" THEN 'borrado de una linea
          'if  lleped%=1 entonces se anula la factura ojo
          IF lleped% > 1 THEN
               renglones = MKI$(CVI(renglones) - 1): Fun(8) = "no"
               Box 14, 10, 16, 70: Center 15, "Borrando , Espere ..."
               FOR m = apu% TO (lleped% - 1): GET #2, (m + 1): PUT #2, m: NEXT m: LSET odigo$ = "BORRA ": PUT #2, lleped%: lleped% = lleped% - 1
               IF apu% > lleped% THEN apu% = lleped%
               EXIT DO
          END IF
     END IF
     END IF
     IF Fun(7) = "si" THEN   'adicion de una linea
          Box 14, 10, 16, 70: Center 15, "Adicionando , Espere ...": Fun(8) = "no"
          IF apu% <> lleped% THEN
               FOR m = lleped% TO apu% STEP -1: GET #2, m: PUT #2, m + 1: NEXT m
               lleped% = lleped% + 1: m = 0
          ELSE
               lleped% = lleped% + 1: m = 0: apu% = lleped%
          END IF: Center 15, "Adici¢n Hecha,Continue . . ."
          DO WHILE m <= 0: m = Escoge(18, 1, 19, ""): LOOP: CLOSE
          OPEN num2$ FOR RANDOM AS #2 LEN = 40: FIELD #2, 6 AS odigo$, 2 AS antid$, 8 AS recio$, 8 AS osto$, 8 AS ferta$, 2 AS oferantid$, 1 AS va$, 2 AS sitio$, 3 AS venci$
          OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
          FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8  _
AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
          meaux% = m: LOCK #1, meaux%
          GET #1, m: RSET odigo$ = codigo2$: RSET antid$ = MKI$(0): RSET venci$ = "   "
          RSET ferta$ = MKD$(VAL(Desc)): RSET recio$ = MKD$(0): RSET oferantid$ = MKI$(0): RSET va$ = " ": RSET osto$ = MKD$(0): RSET venci$ = "   "
          RSET sitio$ = MKI$(m): PUT #2, apu%: renglones = MKI$(CVI(renglones) + 1)
          IF preg$ = "*" THEN GOSUB PreguntaVenci
          EXIT DO
     END IF
     IF devolver = true AND Fun(16) = "si" THEN
      IF repag = false THEN
         apu% = apu% - 1: IF apu% < 1 THEN apu% = 1
      ELSE
         apu% = apu% - 5: IF apu% < 1 THEN apu% = 1
      END IF
      EXIT DO
     END IF
     IF devolver = true AND Fun(16) = "no" THEN
          sit% = sit% - 1: IF sit% < 1 THEN sit% = 6
     END IF
     IF devolver = false AND Fun(16) = "si" THEN
          IF avpag = false THEN
            apu% = apu% + 1
          ELSE
            apu% = apu% + 5
          END IF
          IF apu% > lleped% THEN
             Fun(7) = "si": sit% = 20: apu% = lleped%
          ELSE
             EXIT DO
          END IF
     END IF
     IF devolver = false AND Fun(16) = "no" THEN
          sit% = sit% + 1: IF sit% > 6 THEN sit% = 2
     END IF
     IF Fun(4) = "si" THEN
          Box 8, 10, 11, 70: Center 9, "Introduzca las tres primeras letras a buscar": auxiliar$ = ""
          DO
             auxiliar$ = MeteDatoLetra(10, 40, 3, "   ")
             auxiliar$ = Trim(auxiliar$): auxiliar$ = UCASE$(auxiliar$)
             IF LEN(auxiliar$) > 0 THEN EXIT DO
          LOOP: Fun(4) = "no"
          FOR s% = 1 TO lleped%
               GET #2, s%: GET #1, CVI(sitio$)
               IF MID$(nomb2$, 1, LEN(auxiliar$)) = auxiliar$ THEN
                  apu% = s%: EXIT FOR
               END IF
          NEXT s%: EXIT DO
     END IF
     IF Fun(6) = "si" THEN   'crea un articulo
          COLOR colors(4, colorpref), colors(7, colorpref): Ofr$ = "r": Box 13, 20, 15, 70: Fun(6) = "no"
          LOCATE 14, 21: PRINT "Desea CREAR UN ARTICULO   o Regresar?[S/R]": Ofr$ = MeteDatoLetra$(14, 65, 1, Ofr$)
          IF Ofr$ = "s" OR Ofr$ = "S" THEN
               Box 13, 1, 16, 80: Center 15, "Adicionando , Espere ...": Fun(6) = "no"
               IF apu% <> lleped% THEN
                    FOR m = lleped% TO apu% STEP -1: GET #2, m: PUT #2, m + 1: NEXT m
                    lleped% = lleped% + 1: m = 0
               ELSE
                    lleped% = lleped% + 1: m = 0: apu% = lleped%
               END IF: salir = false: m = -1
               DO WHILE m <= 0
                    DO WHILE NOT salir
                         Center 15, "Ahora puede crear el articulo...": DIM nuevocod AS STRING * 6
                         LOCATE 14, 2: PRINT "Introduzca el Nuevo Codigo o E.F. a Usar :"
                         nuevocod = MeteDatoLetra(14, 44, 6, "      ")
                         IF LEN(nuevocod) > 0 THEN salir = true
                         IF nuevocod = "NOTADE" THEN salir = false
                         IF nuevocod = "      " THEN salir = false
                         IF salir = false THEN
                         CLOSE : OPEN "f:\kevinprg\kevindat\codigold.idx" FOR RANDOM SHARED AS #1 LEN = 8
                         FIELD #1, 6 AS codigo$, 2 AS mini1$: max1% = LOF(1) / 8: seguir = true
                         FOR k = 1 TO max1%: GET #1, k
                         IF codigo$ = nuevocod THEN seguir = false
                         NEXT k: CLOSE
                         OPEN "f:\kevinprg\kevindat\codigo.idx" FOR RANDOM SHARED AS #2 LEN = 8
                         FIELD #2, 6 AS nomshort$, 2 AS num$: max2% = LOF(2) / 8
                         encontrado = false: primero = 1: ultimo = max2%
                         WHILE NOT encontrado AND primero <= ultimo
                              central = (primero + ultimo) / 2: GET #2, central
                              IF nuevocod = nomshort$ THEN
                                   encontrado = true
                              ELSE
                                   IF nomshort$ > nuevocod THEN
                                        ultimo = central - 1
                                   ELSE
                                        primero = central + 1
                                   END IF
                              END IF
                         WEND: salir = false
                         IF encontrado = true THEN seguir = false
                         IF seguir = false THEN Center 15, " Este Codigo Ya Esta En Uso, Utilize Otro Diferente": Espera ELSE salir = true
                         END IF
                    LOOP
                    CLOSE : OPEN "f:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200: Box 13, 1, 16, 80
                    FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS  _
pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$ _
, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
                    ElegMod% = (LOF(1) / 200) + 1: Prod$ = SPACE$(35)
                    DO WHILE Prod$ = SPACE$(35)
                         LOCATE 14, 5: PRINT "Introduzca nombre de Articulo:"; SPACE$(35)
                         Prod$ = MeteDatoLetra(14, 35, 35, Prod$): Prod$ = UCASE$(Prod$)
                    LOOP: seguir = false
                    DO
                         Center 15, SPACE$(78): Center 15, "¨Este Articulo NUEVO es Psicotropico [S/N]?"
                         kbd$ = "": DO WHILE kbd$ = "": kbd$ = INKEY$: LOOP: kbd$ = UCASE$(kbd$)
                         IF kbd$ = "S" THEN
                              Center 15, "ATENCION,SE LE DARA UBICACION DE PSICOTROPICO:¨DE ACUERDO[S/N]?"
                              kbd$ = "": DO WHILE kbd$ = "": kbd$ = INKEY$: LOOP: kbd$ = UCASE$(kbd$)
                              IF kbd$ = "S" THEN RSET ubic2$ = MKI$(1): seguir = true
                         ELSE
                              Center 15, "ATENCION,NO se le dara ubicacion en especial :¨DE ACUERDO[S/N]?"
                              kbd$ = "": DO WHILE kbd$ = "": kbd$ = INKEY$: LOOP: kbd$ = UCASE$(kbd$)
                              IF kbd$ = "S" THEN RSET ubic2$ = MKI$(4): seguir = true
                         END IF
                         IF seguir = true THEN EXIT DO
                    LOOP
                    Center 15, SPACE$(78): Center 15, "¨Desea marcarlo como MEDICINA        [S/N]?"
                    kbd$ = "": DO WHILE kbd$ = "": kbd$ = INKEY$: LOOP: kbd$ = UCASE$(kbd$)
                    IF kbd$ = "S" THEN
                      RSET medi2$ = "*": RSET misc2$ = " "
                    ELSE
                      RSET medi2$ = " ": RSET misc2$ = "*"
                    END IF
                    LSET nomb2$ = Prod$: RSET codigo2$ = nuevocod
                    RSET mini12$ = MKI$(1): RSET mini22$ = MKI$(1): RSET maxi12$ = MKI$(0): RSET maxi22$ = MKI$(0): LSET obser2$ = SPACE$(35): RSET acti2$ = " ": RSET exi12$ = MKI$(0): RSET exi22$ = MKI$(0)
                    RSET exi32$ = MKI$(0): RSET exi42$ = MKI$(0): RSET pv12$ = MKD$(0): RSET pv22$ = MKD$(0): RSET pv32$ = MKD$(10): RSET pv42$ = MKD$(0): RSET cos12$ = MKD$(0): RSET cos22$ = MKD$(0): RSET cos32$ = MKD$(0): RSET cos42$ = MKD$(0): _
                                                                                                     RSET iv$ = " "
                    RSET of12$ = " ": RSET of22$ = " ": RSET of32$ = " ": RSET labo2$ = MKI$(1): RSET re2$ = " ": RSET ped2$ = MKI$(0): RSET quim2$ = " ": RSET compo12$ = MKI$(1): RSET compo22$ = MKI$(1): RSET compo32$ = MKI$(1)
                    RSET compo42$ = MKI$(1): RSET compo52$ = MKI$(1): RSET compo62$ = MKI$(1): RSET preg$ = " ": RSET saldo$ = MKI$(0): RSET upc$ = MKD$(0): RSET ven11$ = " ": RSET ven12$ = MKI$(0): RSET ven21$ = " ": RSET ven22$ = MKI$(0): RSET  _
ven31$ = " ": RSET ven32$ = MKI$(0): RSET ven41$ = " ": RSET ven42$ = MKI$(0)
                    PUT #1, ElegMod%: LOCK #1, ElegMod%
                    OPEN "f:\kevinprg\kevindat\bitacora.dat" FOR RANDOM LOCK READ WRITE AS #17 LEN = 40: FIELD #17, 40 AS tod$: max% = LOF(17) / 40
                    LSET tod$ = codigo2$ + MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4) + MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MKI$(final%) + "10"
                    max% = max% + 1: PUT #17, max%: CLOSE #17
                    CLOSE #2: OPEN "f:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
                    FIELD #2, 35 AS nomshort$, 2 AS num$: max% = (LOF(2) / 37) + 1: central = max%
                    LSET nomshort$ = nomb2$: LSET num$ = MKI$(ElegMod%): PUT #2, max%: GET #2, (central - 1)
                    IF nomb2$ < nomshort$ THEN
                         DO WHILE nomb2$ < nomshort$
                              LOCK #2, central
                              GET #2, (central - 1): PUT #2, central: UNLOCK #2, central
                              central = central - 1 ': LOCATE 1, 1: PRINT central
                              IF central <= 1 THEN central = 1: EXIT DO
                         LOOP
                         LOCK #2, central
                         GET #2, central
                         IF nomb2$ > nomshort$ THEN
                              UNLOCK #2, central
                              central = central + 1
                              LOCK #2, central
                         END IF
                         LSET nomshort$ = nomb2$: LSET num$ = MKI$(ElegMod%): PUT #2, central
                         UNLOCK #2, central
                    END IF
                    IF CVI(ubic2$) = 1 THEN
                         CLOSE #3: OPEN "f:\kevinprg\kevindat\saldos.dat" FOR RANDOM SHARED AS #3 LEN = 45
                         FIELD #3, 6 AS codipsi$, 35 AS nombpsi$, 2 AS saldopsi$, 2 AS sitiopsi$: maxpsi% = (LOF(3) / 45) + 1
                         LSET codipsi$ = codigo2$: LSET nombpsi$ = nomb2$: LSET saldopsi$ = MKI$(0): LSET sitiopsi$ = MKI$(ElegMod%): PUT #3, maxpsi%: CLOSE #3
                    END IF
                    m = ElegMod%
               LOOP
               CLOSE : OPEN num2$ FOR RANDOM AS #2 LEN = 40: FIELD #2, 6 AS odigo$, 2 AS antid$, 8 AS recio$, 8 AS osto$, 8 AS ferta$, 2 AS oferantid$, 1 AS va$, 2 AS sitio$, 3 AS venci$
               OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
               FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$ _
, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS _
 compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
               meaux% = m: LOCK #1, meaux%
               GET #1, m: RSET odigo$ = codigo2$: RSET antid$ = MKI$(0): RSET venci$ = "   "
               RSET ferta$ = MKD$(VAL(Desc)): RSET recio$ = MKD$(0): RSET oferantid$ = MKI$(0): RSET va$ = " ": RSET osto$ = MKD$(0): RSET venci$ = "   "
               RSET sitio$ = MKI$(m): PUT #2, apu%: renglones = MKI$(CVI(renglones) + 1)
               EXIT DO
          END IF: Box 13, 20, 15, 70: Center 14, " Continue Editando la Factura ..."
     END IF
     IF Fun(10) = "si" THEN  'actualizar factura y salir
          COLOR colors(4, colorpref), colors(7, colorpref): Ofr$ = "r": Box 13, 20, 15, 70: Fun(10) = "no"
          LOCATE 14, 21: PRINT "Desea Salvar esta Factura o Regresar?[S/R]": Ofr$ = MeteDatoLetra$(14, 65, 1, Ofr$)
          IF Ofr$ = "s" OR Ofr$ = "S" THEN
            GOSUB VerifSalv
          END IF: Box 13, 20, 15, 70: Center 14, " Continue Editando la Factura ...": Espera: EXIT DO
     END IF
     IF Fun(11) = "si" THEN  'abortar factura
          COLOR colors(4, colorpref), colors(7, colorpref): Ofr$ = "r": Box 13, 20, 15, 70: Fun(10) = "no"
          LOCATE 14, 21: PRINT "DESEA ABORTAR esta Factura o Regresar?[S/R]": Ofr$ = MeteDatoLetra$(14, 66, 1, Ofr$)
          IF Ofr$ = "s" OR Ofr$ = "S" THEN
               CLOSE : OPEN "F:\kevinprg\kevindat\master.fct" FOR RANDOM AS #27 LEN = 70
               FIELD #27, 1 AS acti$, 4 AS numfact$, 8 AS fechaacc$, 10 AS vendedor$, 2 AS prove$, 1 AS contacredi$, 8 AS brut$, 8 AS net$, 8 AS ivafact$, 8 AS fechafact$, 1 AS actual$, 4 AS numped$, 1 AS cancelada$, 6 AS vacio$
               final% = LOF(27) / 70: RSET acti$ = "*": PUT 27, final%: CLOSE : KILL num2$
               KILL "f:\kevinprg\kevindat\congela.dat": EXIT SUB
          END IF: Box 13, 20, 15, 70: Center 14, " Continue Editando la Factura ...": Espera: EXIT DO
     END IF
     IF Fun(12) = "si" THEN  'congela factura
         EXIT SUB
     END IF
     LOOP
     UNLOCK #1, meaux% 'ojo  meaux% = CVI(sitio$)
LOOP
CLOSE : EXIT SUB

Arri:
     resal% = resal% - 1
     IF selecc% < 6 THEN
          IF resal% < 1 THEN resal% = 1: IF sonido = "si" THEN BEEP
     ELSE
          IF resal% < 1 THEN resal% = 1: up% = up% - 1
     END IF
     IF up% < 1 THEN up% = 1: IF sonido = "si" THEN BEEP
RETURN
Abaj: resal% = resal% + 1
     IF selecc% < 10 THEN
          IF resal% > ventana% THEN resal% = ventana%: IF sonido = "si" THEN BEEP
     ELSE
          IF resal% > 10 THEN resal% = 10: up% = up% + 1
     END IF
     IF selecc% > 10 THEN
          IF up% + 10 > selecc% THEN up% = selecc% - 9: IF sonido = "si" THEN BEEP
     END IF
RETURN
Agup: up% = up% - 9: IF up% < 1 THEN up% = 1: resal% = 1: IF sonido = "si" THEN BEEP
RETURN
Agdn: up% = up% + 9: IF up% + 9 > selecc% THEN up% = selecc% - 9: resal% = 10: IF sonido = "si" THEN BEEP
RETURN
PreguntaVenci:
    IF MID$(venci$, 1, 1) = "A" OR MID$(venci$, 1, 1) = "B" OR MID$(venci$, 1, 1) = "C" THEN
      IF MID$(venci$, 1, 1) = "A" THEN auxiliar$ = "10/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4)
      IF MID$(venci$, 1, 1) = "B" THEN auxiliar$ = "11/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4)
      IF MID$(venci$, 1, 1) = "C" THEN auxiliar$ = "12/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4)
    ELSE
      IF MID$(venci$, 1, 1) <> " " THEN auxiliar$ = "0" + MID$(venci$, 1, 1) + "/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4) ELSE auxiliar$ = "  /    "
    END IF
    CALL PopUp(12, 10, 5, 61, 112, 1, 0, 0)
    Box 12, 10, 16, 70: Center 13, "Introduzca solo Mes y A¤o de Vencimiento de Este Articulo:": Center 15, "Atenci¢n: SOLO EMPLEE EL FORMATO ²mm/aaaa²"
    DO
      auxiliar$ = MeteDatoLetra(14, 40, 7, auxiliar$)
      IF VAL(MID$(auxiliar$, 1, 2)) > 0 AND VAL(MID$(auxiliar$, 1, 2)) < 13 THEN
        IF VAL(MID$(auxiliar$, 4, 4)) > 1990 AND VAL(MID$(auxiliar$, 4, 4)) < 2050 THEN
          Auxi% = VAL(MID$(auxiliar$, 1, 2))
          SELECT CASE Auxi%
            CASE 1: RSET venci$ = "1" + MKI$(VAL(MID$(auxiliar$, 4, 4)))
            CASE 2: RSET venci$ = "2" + MKI$(VAL(MID$(auxiliar$, 4, 4)))
            CASE 3: RSET venci$ = "3" + MKI$(VAL(MID$(auxiliar$, 4, 4)))
            CASE 4: RSET venci$ = "4" + MKI$(VAL(MID$(auxiliar$, 4, 4)))
            CASE 5: RSET venci$ = "5" + MKI$(VAL(MID$(auxiliar$, 4, 4)))
            CASE 6: RSET venci$ = "6" + MKI$(VAL(MID$(auxiliar$, 4, 4)))
            CASE 7: RSET venci$ = "7" + MKI$(VAL(MID$(auxiliar$, 4, 4)))
            CASE 8: RSET venci$ = "8" + MKI$(VAL(MID$(auxiliar$, 4, 4)))
            CASE 9: RSET venci$ = "9" + MKI$(VAL(MID$(auxiliar$, 4, 4)))
            CASE 10: RSET venci$ = "A" + MKI$(VAL(MID$(auxiliar$, 4, 4)))
            CASE 11: RSET venci$ = "B" + MKI$(VAL(MID$(auxiliar$, 4, 4)))
            CASE 12: RSET venci$ = "C" + MKI$(VAL(MID$(auxiliar$, 4, 4)))
          END SELECT: EXIT DO
        END IF
      END IF
    LOOP
    PUT #2, apu%
    CALL ShutUp
RETURN

Actualiza:
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
OPEN num2$ FOR RANDOM AS #2 LEN = 40
FIELD #2, 6 AS odigo$, 2 AS antid$, 8 AS recio$, 8 AS osto$, 8 AS ferta$, 2 AS oferantid$, 1 AS va$, 2 AS sitio$, 3 AS venci$
lleped% = LOF(2) / 40: Box 13, 20, 15, 70: Center 14, "Actualizando , espere por favor ...": Termometro 0, lleped%, 0, "Incorporando al Inventario", 6, 10
OPEN "f:\kevinprg\kevindat\desc.dat" FOR RANDOM SHARED AS #48 LEN = LEN(dcto)
FOR k = 1 TO lleped%
     GET #2, k: Termometro k, lleped%, 1, "Incorporando al Inventario", 6, 10
     GET #1, CVI(sitio$)
     GET #48, CVI(sitio$), dcto
     LOCK #1, CVI(sitio$)
     devolver = false
     IF CVI(ubic2$) = 1 THEN
          OPEN "F:\kevinprg\kevindat\movpsico.dat" FOR RANDOM LOCK READ WRITE AS #17 LEN = 40: FIELD #17, 40 AS todo$: max% = LOF(17) / 40: max% = max% + 1
          LSET todo$ = "COMPRA" + MID$(num2$, 22, 12) + Prud2$ + codigo2$ + MKI$(CVI(antid$) + CVI(oferantid$)): PUT #17, max%: CLOSE #17
     END IF
     IF va$ = "I" THEN RSET osto$ = MKD$(CVD(razon) * CVD(osto$) * ((VAL(Impuesto) / 100) + 1)) ELSE RSET osto$ = MKD$(CVD(razon) * CVD(osto$))
     RSET upc$ = SPACE$(8)
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
          'LOCATE 1, 1: PRINT venci$
          'comienza a actualizar
    IF CVI(exi12$) < 0 OR CVI(exi22$) < 0 OR CVI(exi32$) < 0 THEN
      IF pv32$ = recio$ AND CVI(exi32$) < 0 THEN
        dcto.desc3 = CVD(ferta$)
        RSET cos32$ = MKD$(CVD(osto$) / (CVI(antid$) + CVI(oferantid$)))
        RSET exi32$ = MKI$(CVI(exi32$) + CVI(antid$) + CVI(oferantid$)): devolver = true
      END IF
      IF pv22$ = recio$ AND CVI(exi22$) < 0 AND devolver = false THEN
        dcto.desc2 = CVD(ferta$)
        RSET cos22$ = MKD$(CVD(osto$) / (CVI(antid$) + CVI(oferantid$)))
        RSET exi22$ = MKI$(CVI(exi22$) + CVI(antid$) + CVI(oferantid$)): devolver = true
      END IF
      IF pv12$ = recio$ AND CVI(exi12$) < 0 AND devolver = false THEN
        dcto.desc1 = CVD(ferta$)
        RSET cos12$ = MKD$(CVD(osto$) / (CVI(antid$) + CVI(oferantid$)))
        RSET exi12$ = MKI$(CVI(exi12$) + CVI(antid$) + CVI(oferantid$)): devolver = true
      END IF
      'si a£n no ha actualizado entonces busca en cero
      IF CVI(exi12$) = 0 AND devolver = false THEN
        dcto.desc1 = CVD(ferta$)
        RSET cos12$ = MKD$(CVD(osto$) / (CVI(antid$) + CVI(oferantid$)))
        RSET exi12$ = MKI$(CVI(exi12$) + CVI(antid$) + CVI(oferantid$)): devolver = true
      END IF
      IF CVI(exi22$) = 0 AND devolver = false THEN
        dcto.desc2 = CVD(ferta$)
        RSET cos22$ = MKD$(CVD(osto$) / (CVI(antid$) + CVI(oferantid$)))
        RSET exi22$ = MKI$(CVI(exi22$) + CVI(antid$) + CVI(oferantid$)): devolver = true
      END IF
      IF CVI(exi32$) = 0 AND devolver = false THEN
        dcto.desc3 = CVD(ferta$)
        RSET cos32$ = MKD$(CVD(osto$) / (CVI(antid$) + CVI(oferantid$)))
        RSET exi32$ = MKI$(CVI(exi32$) + CVI(antid$) + CVI(oferantid$)): devolver = true
      END IF
    ELSE
          IF pv32$ = recio$ AND CVD(ferta$) = dcto.desc3 THEN
               IF CVI(exi32$) = 0 THEN
                    GOSUB Carga3
                    IF preg$ = "*" THEN RSET ven31$ = MID$(venci$, 1, 1): RSET ven32$ = MID$(venci$, 2, 2)
               ELSE
                    IF preg$ = "*" THEN
                         IF CalculaMeses%(ven31$ + ven32$) = 0 THEN
                              GOSUB Carga3: RSET ven31$ = MID$(venci$, 1, 1): RSET ven32$ = MID$(venci$, 2, 2)
                         ELSE
                              IF CalculaMeses%(ven31$ + ven32$) = CalculaMeses%(venci$) THEN GOSUB Carga3
                         END IF
                    ELSE
                         GOSUB Carga3
                    END IF
               END IF
          END IF
          IF pv22$ = recio$ AND devolver = false THEN
               IF CVD(ferta$) = dcto.desc2 THEN
                  IF CVI(exi22$) = 0 THEN
                       GOSUB Carga2
                       IF preg$ = "*" THEN RSET ven21$ = MID$(venci$, 1, 1): RSET ven22$ = MID$(venci$, 2, 2)
                  ELSE
                       IF preg$ = "*" THEN
                            IF CalculaMeses%(ven21$ + ven22$) > 5000 THEN
                                 GOSUB Carga2: RSET ven21$ = MID$(venci$, 1, 1): RSET ven22$ = MID$(venci$, 2, 2)
                            ELSE
                                 IF CalculaMeses%(ven21$ + ven22$) = CalculaMeses%(venci$) THEN GOSUB Carga2
                            END IF
                       ELSE
                            GOSUB Carga2
                       END IF
                  END IF
               END IF
          END IF
          IF pv12$ = recio$ AND devolver = false THEN
               IF CVD(ferta$) = dcto.desc1 THEN
                  IF CVI(exi12$) = 0 THEN
                       GOSUB Carga1: IF preg$ = "*" THEN RSET ven11$ = MID$(venci$, 1, 1): RSET ven12$ = MID$(venci$, 2, 2)
                  ELSE
                       IF preg$ = "*" THEN
                            IF CalculaMeses%(ven11$ + ven12$) > 5000 THEN
                                 GOSUB Carga1: RSET ven11$ = MID$(venci$, 1, 1): RSET ven12$ = MID$(venci$, 2, 2)
                            ELSE
                                 IF CalculaMeses%(ven11$ + ven12$) = CalculaMeses%(venci$) THEN GOSUB Carga1
                            END IF
                       ELSE
                            GOSUB Carga1
                       END IF
                  END IF
               END IF
          END IF
          IF CVI(exi12$) = 0 AND devolver = false THEN
               GOSUB Carga1: IF preg$ = "*" THEN RSET ven11$ = MID$(venci$, 1, 1): RSET ven12$ = MID$(venci$, 2, 2)
               dcto.desc1 = CVD(ferta$)
          END IF
          IF CVI(exi22$) = 0 AND devolver = false THEN
               GOSUB Carga2: IF preg$ = "*" THEN RSET ven21$ = MID$(venci$, 1, 1): RSET ven22$ = MID$(venci$, 2, 2)
               dcto.desc2 = CVD(ferta$)
          END IF
          IF CVI(exi32$) = 0 AND devolver = false THEN
               GOSUB Carga3: IF preg$ = "*" THEN RSET ven31$ = MID$(venci$, 1, 1): RSET ven32$ = MID$(venci$, 2, 2)
               dcto.desc3 = CVD(ferta$)
          END IF
          IF devolver = false THEN
               GOSUB Carga3: IF preg$ = "*" THEN RSET ven31$ = MID$(venci$, 1, 1): RSET ven32$ = MID$(venci$, 2, 2)
               IF CVD(ferta$) < dcto.desc3 THEN dct.desc3 = CVD(ferta$)
          END IF
     'END IF
    END IF
    IF va$ <> " " THEN RSET iv$ = "*"
    RSET ped2$ = MKI$(CVI(ped2$) - CVI(antid$))
    IF CVI(ped2$) < 0 THEN RSET ped2$ = MKI$(0)
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
    PUT #1, CVI(sitio$): PUT #48, CVI(sitio$), dcto: UNLOCK #1, CVI(sitio$)
NEXT k
KILL "f:\kevinprg\kevindat\congela.dat"
RETURN

Cambia1y2:
Auxi$ = pv12$: RSET pv12$ = pv22$: RSET pv22$ = Auxi$
Auxi$ = cos12$: RSET cos12$ = cos22$: RSET cos22$ = Auxi$
Auxi$ = exi12$: RSET exi12$ = exi22$: RSET exi22$ = Auxi$
Auxi$ = ven11$: RSET ven11$ = ven21$: RSET ven21$ = Auxi$
Auxi$ = ven12$: RSET ven12$ = ven22$: RSET ven22$ = Auxi$
Auxi$ = MKD$(dcto.desc1): dcto.desc1 = dcto.desc2: dcto.desc2 = CVD(Auxi$)
RETURN

Cambia1y3:
Auxi$ = pv12$: RSET pv12$ = pv32$: RSET pv32$ = Auxi$
Auxi$ = cos12$: RSET cos12$ = cos32$: RSET cos32$ = Auxi$
Auxi$ = exi12$: RSET exi12$ = exi32$: RSET exi32$ = Auxi$
Auxi$ = ven11$: RSET ven11$ = ven31$: RSET ven31$ = Auxi$
Auxi$ = ven12$: RSET ven12$ = ven32$: RSET ven32$ = Auxi$
Auxi$ = MKD$(dcto.desc1): dcto.desc1 = dcto.desc3: dcto.desc3 = CVD(Auxi$)
RETURN

Cambia2y3:
Auxi$ = pv22$: RSET pv22$ = pv32$: RSET pv32$ = Auxi$
Auxi$ = cos22$: RSET cos22$ = cos32$: RSET cos32$ = Auxi$
Auxi$ = exi22$: RSET exi22$ = exi32$: RSET exi32$ = Auxi$
Auxi$ = ven21$: RSET ven21$ = ven31$: RSET ven31$ = Auxi$
Auxi$ = ven22$: RSET ven22$ = ven32$: RSET ven32$ = Auxi$
Auxi$ = MKD$(dcto.desc2): dcto.desc2 = dcto.desc3: dcto.desc3 = CVD(Auxi$)
RETURN

Carga1:
RSET pv12$ = recio$
' promedia el costo, existencia nueva y vieja
RSET cos12$ = MKD$(((CVI(exi12$) * CVD(cos12$)) + CVD(osto$)) / (CVI(exi12$) + CVI(antid$) + CVI(oferantid$)))
RSET exi12$ = MKI$(CVI(exi12$) + CVI(antid$) + CVI(oferantid$)): devolver = true
RETURN

Carga2:
RSET pv22$ = recio$
' promedia el costo, existencia nueva y vieja
RSET cos22$ = MKD$(((CVI(exi22$) * CVD(cos22$)) + CVD(osto$)) / (CVI(exi22$) + CVI(antid$) + CVI(oferantid$)))
RSET exi22$ = MKI$(CVI(exi22$) + CVI(antid$) + CVI(oferantid$)): devolver = true
RETURN

Carga3:
RSET pv32$ = recio$
' promedia el costo, existencia nueva y vieja
RSET cos32$ = MKD$(((CVI(exi32$) * CVD(cos32$)) + CVD(osto$)) / (CVI(exi32$) + CVI(antid$) + CVI(oferantid$)))
RSET exi32$ = MKI$(CVI(exi32$) + CVI(antid$) + CVI(oferantid$)): devolver = true
RETURN

Mostrar:
LOCATE 18, 1: PRINT "Codigo Nombre                             Ofertas Observaciones"; SPACE$(17)
LOCATE 19, 1: PRINT codigo2$; " ";
COLOR colors(7, colorpref) + 16, colors(4, colorpref): PRINT nomb2$; : COLOR colors(4, colorpref), colors(7, colorpref)
'PRINT " "; of12$; " "; of22$; " "; of32$; obser2$; SPACE$(12);
OPEN "f:\kevinprg\kevindat\desc.dat" FOR RANDOM SHARED AS #38 LEN = LEN(miDesc)
GET #38, CVI(sitio$), miDesc: CLOSE #38
PRINT USING "####.##"; miDesc.desc1; : PRINT SPACE$(8);
PRINT USING "####.##"; miDesc.desc2; : PRINT SPACE$(8);
PRINT USING "####.##"; miDesc.desc3;
Box 20, 1, 23, 80: wer$ = "En Oferta:" + STRING$(24, "Ä") + "CAN1ÄÄÄÄÄÄPVP1ÂCAN2" + STRING$(6, "Ä") + "PVP2ÂCAN3" + STRING$(5, "Ä") + "PVP3"
LOCATE 20, 2: PRINT wer$; : LOCATE 22, 2: PRINT "Costos:"
LOCATE 21, 36: PRINT USING "####"; CVI(exi12$); : LOCATE 21, 50: PRINT "³"; : PRINT USING "####"; CVI(exi22$); : LOCATE 21, 65: PRINT "³"; : PRINT USING "####"; CVI(exi32$)
LOCATE 21, 40: PRINT USING "######,.##"; CVD(pv12$): LOCATE 21, 55: PRINT USING "######,.##"; CVD(pv22$): LOCATE 21, 70: PRINT USING "######,.##"; CVD(pv32$)
LOCATE 21, 2: PRINT USING "####"; CVI(exi42$); : PRINT " a Bs."; : PRINT USING "######,.##"; CVD(pv42$)
LOCATE 22, 40: PRINT USING "######,.##"; CVD(cos12$); : PRINT "³"; : LOCATE 22, 55: PRINT USING "######,.##"; CVD(cos22$); : PRINT "³"; : LOCATE 22, 70: PRINT USING "######,.##"; CVD(cos32$); : PRINT "³";
LOCATE 22, 12: PRINT USING "######,.##"; CVD(cos42$): LOCATE 23, 49: PRINT "ÄÁ"; : LOCATE 23, 64: PRINT "ÄÁ";
LOCATE 24, 1: PRINT "[Minimo1="; : PRINT USING "####"; CVI(mini12$); : PRINT "] [Minimo2="; : PRINT USING "####"; CVI(mini22$); : PRINT "] [Maximo1="; : PRINT USING "####"; CVI(maxi12$); : PRINT "] [Maximo2="; : PRINT USING "####"; CVI(maxi22$); : _
                                                                                                     PRINT "]";
RETURN

VerifSalv:
  salir = true
  IF ABS(CVD(oldbruto) - CVD(bruto)) > 10 THEN
    Box 13, 5, 16, 75: Center 14, "LOS MONTOS NO CONCUERDAN CON LOS INTRODUCIDOS AL PRINCIPIO": LOCATE 15, 10: PRINT "Monto Inicial: Bs"; : PRINT USING "##########,.## "; CVD(oldbruto); : PRINT "monto en curso: Bs"; : PRINT USING "##########,.## ";  _
CVD(bruto)
    CALL Espera: RETURN
  END IF
  FOR k = 1 TO lleped%
    GET #2, k: IF (CVI(antid$) + CVI(oferantid$)) = 0 THEN apu% = k: Ofr$ = "N": Box 13, 20, 15, 70: Center 14, "NO DEBEN HABER Renglones SIN Cantidad": Espera: salir = false: EXIT FOR
    IF CVD(recio$) = 0 THEN apu% = k: Box 13, 20, 15, 70: Center 14, "NO DEBEN HABER Renglones SIN Precio": Espera: salir = false: EXIT FOR
    IF CVD(osto$) = 0 THEN apu% = k: Box 13, 20, 15, 70: Center 14, "NO DEBEN HABER Renglones SIN Costo": Espera: salir = false: EXIT FOR
    IF va$ = "I" THEN
      porc = MKD$(100 - (((CVD(osto$) / (CVI(antid$)))) / (CVD(recio$) / (VAL(Impuesto) / 100 + 1))) * 100)
    ELSE
      porc = MKD$(100 - (((CVD(osto$) / (CVI(antid$)))) / CVD(recio$)) * 100)
    END IF
    IF CVD(porc) <= CVD(ferta$) THEN apu% = k: Box 13, 20, 15, 70: Center 14, "NO HAY GANANCIA en este articulo  ": Espera: salir = false: EXIT FOR
  NEXT k
  IF salir = true THEN
    CLOSE : KILL "F:\kevinprg\kevindat\factura.tmp"
    OPEN "F:\kevinprg\kevindat\factura.tmp" FOR RANDOM AS #1 LEN = 40: FIELD #1, 40 AS all$
    OPEN num2$ FOR RANDOM AS #2 LEN = 40: FIELD #2, 40 AS todo$
    FOR k = 1 TO lleped%: GET #2, k: RSET all$ = todo$: PUT #1, k: NEXT k: CLOSE
    KILL num2$: NAME "F:\kevinprg\kevindat\factura.tmp" AS num2$: GOSUB Actualiza
    EXIT SUB
  END IF
RETURN
END SUB

SUB Termometro (va%, total%, edo%, text$, fila%, columna%)
IF edo% = 0 THEN Box fila%, columna%, fila% + 6, columna% + 31
LOCATE fila% + 1, columna% + 1: PRINT "0%                        100%"
IF edo% = 0 THEN LOCATE fila% + 2, columna% + 1: PRINT STRING$(30, "±")
LOCATE fila% + 2, columna% + 1: PRINT STRING$(INT((va% / total%) * 30), "Û")
LOCATE fila% + 3, columna% + 1: PRINT "Van"; va%; " de "; total%;
LOCATE fila% + 4, columna% + 9: PRINT USING "##.## % Hecho "; ((va% / total%) * 100)
LOCATE fila% + 5, columna% + 1: PRINT text$
END SUB

FUNCTION Trim$ (x$)
IF x$ = "" THEN
     Trim$ = ""
ELSE
lastChar = 0
FOR a = 1 TO LEN(x$): Y$ = MID$(x$, a, 1): IF Y$ <> CHR$(0) AND Y$ <> " " THEN lastChar = a
NEXT a: Trim$ = LEFT$(x$, lastChar)
END IF
END FUNCTION

