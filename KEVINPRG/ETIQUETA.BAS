DEFINT A-Z
DECLARE SUB Espera ()
DECLARE SUB LeeEstado ()
DECLARE SUB Center (row, text$)
DECLARE SUB Box (Row1%, Col1%, Row2%, Col%)
DECLARE SUB Box2 (Row1, Col1, Row2, Col2)
DECLARE SUB Etiquetas ()
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$)
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Trim$ (x$)
COMMON SHARED guia, colorpref
colorpref = 3
DIM SHARED turno AS STRING * 1, devolver
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, impuesto AS STRING * 6, nombfarma AS STRING * 25, direccion AS STRING * 30, rif     AS STRING * 25, nit      AS STRING * 25, telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2
DIM SHARED Fun(17) AS STRING * 2, salir, Turna, terminar, sitio
CONST true = -1: CONST false = NOT true
ON ERROR GOTO ErrorTrap
CALL LeeEstado
CALL Etiquetas
'COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... "
'CHAIN "F:\kevinprg\kevin01.bas"
END
ErrorTrap:
    SELECT CASE ERR
        CASE 70
            Center 25, " Esperando Acceso a Registro ...": SLEEP 1: Center 25, SPACE$(35): RESUME
        CASE 53
          RESUME
        CASE ELSE
            LOCATE 1, 1: PRINT ERR
    END SELECT
    RESUME NEXT
END
DATA 0,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  2,     0,     1,     15,     0,     7,      0
DATA 3,    15,   13,  1,     14,    3,     15,     0,     7,      0
DATA 7,    12,   15,  4,     14,    0,     15,     15,    1,      0

SUB Box (Row1%, Col1%, Row2%, Col2%) STATIC
BoxWidth = Col2% - Col1% + 1: LOCATE Row1%, Col1%
PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿";
FOR a = Row1% + 1 TO Row2% - 1
  LOCATE a, Col1%: PRINT "³"; SPACE$(BoxWidth - 2); "³";
NEXT a
LOCATE Row2%, Col1%: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù";
END SUB

SUB Box2 (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "²"; STRING$(BoxWidth - 2, "²"); "²"; : FOR a = Row1 + 1 TO Row2 - 1: LOCATE a, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Center (row, text$)
IF row = 25 THEN COLOR colors(5, colorpref), colors(4, colorpref): LOCATE 25, 1: PRINT SPACE$(80);
LOCATE row, 41 - LEN(text$) / 2: PRINT text$;
END SUB

FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
LOCATE fila%, columna%: PRINT CHR$(178) + caden$ + SPACE$(largo% - LEN(caden$)): LOCATE fila%, columna% + largo% + 1: PRINT CHR$(178)
REDIM eleccion(0 TO largo% + 1) AS STRING * 1: llevasitio% = 2
FOR k = 1 TO largo%: eleccion(k) = MID$(caden$, k, 1): NEXT k: LOCATE fila%, (columna% + llevasitio%): COLOR colors(4, colorpref) + 16, colors(7, colorpref): IF eleccion(2) = " " THEN PRINT "_";  ELSE PRINT eleccion(2)
DO
      llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
          SELECT CASE llave$
               CASE CHR$(0) + "H": devolver = true: EXIT DO
               CASE CHR$(0) + "P": EXIT DO
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
               CASE CHR$(0) + CHR$(71): llevasitio% = 1
               CASE CHR$(0) + CHR$(79): llevasitio% = largo%
               CASE " ", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "+", "-", "."
                    GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
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

SUB Espera
Center 25, " Presione Cualquier Tecla Para Continuar ... "
DO: LOOP WHILE INKEY$ = ""
END SUB

SUB Etiquetas
DIM Linea(7)  AS STRING * 25, aux AS STRING * 120
FOR k = 1 TO 7: Linea(k) = SPACE$(25): NEXT k
COLOR colors(7, colorpref), colors(4, colorpref): CLS
CALL Box2(1, 1, 24, 80)
CALL Box(6, 26, 15, 52): Center 4, "Impresi¢n de Etiquetas"
Linea(1) = "FARMACIA " + Trim$(nombfarma)
Linea(2) = direccion
Linea(3) = telefono
Linea(4) = "Valencia -Estado Carabobo"
LOCATE 7, 27: PRINT Linea(1)
LOCATE 8, 27: PRINT Linea(2)
LOCATE 9, 27: PRINT Linea(3)
LOCATE 10, 27: PRINT Linea(4)
LOCATE 11, 26: PRINT "Ã"; STRING$(25, "Ä"); "´";
sit% = 1
DO
  devolver = false: Center 25, "[F9]Abortar      [F10]Imprimir etiquetas": Fun(9) = "no": Fun(10) = "no"
  SELECT CASE sit%
    CASE 1
      Linea(5) = MeteDatoLetra$(12, 26, 25, Linea(5))
    CASE 2
      Linea(6) = MeteDatoLetra$(13, 26, 25, Linea(6))
    CASE 3
      Linea(7) = MeteDatoLetra$(14, 26, 25, Linea(7))
  END SELECT
  IF Fun(9) = "si" THEN EXIT SUB
  IF Fun(10) = "si" THEN EXIT DO
  IF devolver = true THEN
    sit% = sit% - 1: IF sit% < 1 THEN sit% = 3
  ELSE
    sit% = sit% + 1: IF sit% > 3 THEN sit% = 1
  END IF
LOOP
COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 12, 26: PRINT "³"; Linea(5); "³";
LOCATE 13, 26: PRINT "³"; Linea(6); "³";
LOCATE 14, 26: PRINT "³"; Linea(7); "³";
CALL Box(16, 5, 18, 75): Center 17, "¨Cu ntas etiquetas desea imprimir?"
cant% = VAL(EntraDatoNumero$(17, 60, 3, " 0 ", 1))
CALL Box(16, 5, 18, 75): Center 17, "Imprimiendo, por favor espere...  "
IF cant% > 0 THEN
  OPEN "c:\etiqueta.txt" FOR OUTPUT AS #10
  WIDTH #10, 255: lineas% = 0
  cant2% = FIX(cant% / 4): resto% = cant% - cant2% * 4
  FOR k = 1 TO cant2%
    lineas% = lineas% + 10
    aux = SPACE$(120): aux = STRING$(113, "-")
    PRINT #10, aux
    aux = SPACE$(120): aux = "|Ú" + STRING$(25, "Ä") + "¿" + "|Ú" + STRING$(25, "Ä") + "¿" + "|Ú" + STRING$(25, "Ä") + "¿" + "|Ú" + STRING$(25, "Ä") + "¿|"
    PRINT #10, aux
    FOR l = 1 TO 4
      aux = SPACE$(120): aux = "|³" + Linea(l) + "³" + "|³" + Linea(l) + "³" + "|³" + Linea(l) + "³" + "|³" + Linea(l) + "³|"
      PRINT #10, aux
    NEXT l
    aux = SPACE$(120): aux = "|Ã" + STRING$(25, "Ä") + "´" + "|Ã" + STRING$(25, "Ä") + "´" + "|Ã" + STRING$(25, "Ä") + "´" + "|Ã" + STRING$(25, "Ä") + "´|"
    PRINT #10, aux
    FOR l = 5 TO 7
      aux = SPACE$(120): aux = "|³" + Linea(l) + "³" + "|³" + Linea(l) + "³" + "|³" + Linea(l) + "³" + "|³" + Linea(l) + "³|"
      PRINT #10, aux
    NEXT l
    aux = SPACE$(120): aux = "|À" + STRING$(25, "Ä") + "Ù" + "|À" + STRING$(25, "Ä") + "Ù" + "|À" + STRING$(25, "Ä") + "Ù" + "|À" + STRING$(25, "Ä") + "Ù|"
    PRINT #10, aux
  NEXT k
  FOR k = 1 TO resto%
  NEXT k
  CLOSE
ELSE
  EXIT SUB
END IF
END SUB

SUB LeeEstado
CLOSE : OPEN "F:\kevinprg\kevindat\kaos.dat" FOR INPUT SHARED AS #2
INPUT #2, sonido: INPUT #2, impuesto: INPUT #2, nombfarma: INPUT #2, direccion: INPUT #2, rif: INPUT #2, nit: INPUT #2, telefono: INPUT #2, contri: INPUT #2, contra: INPUT #2, Stock: INPUT #2, Blanq
INPUT #2, LargoH: INPUT #2, TipoL: INPUT #2, Mensaje1: INPUT #2, Mensaje2: INPUT #2, Mensaje3: INPUT #2, Redondeo: INPUT #2, Desc: INPUT #2, DescMax: INPUT #2, Exactitud: INPUT #2, Prov1: INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo: CLOSE
END SUB

FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$)
LOCATE fila%, columna%: PRINT CHR$(178) + caden$: LOCATE fila%, columna% + largo% + 1: PRINT CHR$(178)
REDIM eleccion(0 TO largo% + 1) AS STRING * 1: llevasitio% = 1: ins% = true
FOR k = 1 TO largo%: eleccion(k) = MID$(caden$, k, 1): NEXT k
LOCATE fila%, (columna% + llevasitio%): COLOR colors(7, colorpref) + 16, colors(4, colorpref)
IF eleccion(1) = " " THEN PRINT "_";  ELSE PRINT eleccion(1)
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
               'CASE CHR$(0) + CHR$(60): Fun(2) = "si": EXIT DO
               'CASE CHR$(0) + CHR$(61): Fun(3) = "si": EXIT DO
               'CASE CHR$(0) + CHR$(62): Fun(4) = "si": EXIT DO
               'CASE CHR$(0) + CHR$(63): Fun(5) = "si": EXIT DO
               'CASE CHR$(0) + CHR$(64): Fun(6) = "si": EXIT DO
               'CASE CHR$(0) + CHR$(65): Fun(7) = "si": EXIT DO
               'CASE CHR$(0) + CHR$(66): Fun(8) = "si": EXIT DO
               CASE CHR$(0) + CHR$(67): Fun(9) = "si": EXIT DO
               CASE CHR$(0) + CHR$(68): Fun(10) = "si": EXIT DO
               CASE CHR$(0) + CHR$(71): llevasitio% = 1
               CASE CHR$(0) + CHR$(79): llevasitio% = largo%
               CASE CHR$(0) + CHR$(82)
                  IF ins% = true THEN ins% = false ELSE ins% = true
               CASE ELSE
                  IF ins% = true THEN
                    FOR k = largo% TO llevasitio% + 1 STEP -1
                      eleccion(k) = eleccion(k - 1)
                    NEXT k
                    eleccion(llevasitio%) = llave$: llevasitio% = llevasitio% + 1
                  ELSE
                    eleccion(llevasitio%) = llave$
                    llevasitio% = llevasitio% + 1
                  END IF
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

FUNCTION Trim$ (x$)
IF x$ = "" THEN
     Trim$ = ""
ELSE
lastChar = 0
FOR a = 1 TO LEN(x$): Y$ = MID$(x$, a, 1): IF Y$ <> CHR$(0) AND Y$ <> " " THEN lastChar = a
NEXT a: Trim$ = LEFT$(x$, lastChar)
END IF
END FUNCTION

