' relacion de psicotropicos
DEFINT A-Z
DECLARE SUB Espera ()
DECLARE SUB LeeEstado ()
DECLARE SUB Center (row, text$)
DECLARE SUB Box (Row1%, Col1%, Row2%, Col%)
DECLARE SUB Box2 (Row1, Col1, Row2, Col2)
DECLARE SUB Borrabox (Row1%, Col1%, Row2%, Col%)
DECLARE SUB Termometro (va%, total%, edo%, text$, fila%, columna%)
DECLARE SUB Psicotropicos ()
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$)
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Escoge% (fila%, edo%, fila2%, Prod$)
DECLARE FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%, est%)
DECLARE FUNCTION CheqDate (caden$, op%)
DECLARE FUNCTION Trim$ (x$)
COMMON SHARED guia, colorpref
DIM SHARED turno AS STRING * 1, devolver
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED Sonido  AS STRING * 2, Impuesto AS STRING * 6, NombFarma AS STRING * 25, Direccion AS STRING * 30, Rif     AS STRING * 25, Nit      AS STRING * 25, Telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2, num2 AS STRING * 33
DIM SHARED Fun(17) AS STRING * 2, salir, Turna, terminar, sitio, eleg
DIM SHARED Contenido(199) AS STRING * 15, code(199) AS STRING * 3
DIM SHARED numfact(10) AS STRING * 4, provee(10) AS STRING * 17, fechafact(10) AS STRING * 8, fechaacc(10) AS STRING * 8, bruto(10) AS STRING * 8, neto(10) AS STRING * 8, ivafact(10) AS STRING * 8, transpor(10) AS STRING * 10, edofact(10) AS STRING  _
* 3, ubifact(10) AS STRING * 2
REM $DYNAMIC
DIM SHARED Nombra(50) AS STRING * 35, Pvp1a(50) AS STRING * 8, Pvp2a(50) AS STRING * 8, Pvp3a(50) AS STRING * 8, Pvp4a(50) AS STRING * 8, Numa(50) AS STRING * 2, observa(50) AS STRING * 20, Exis1a(50) AS STRING * 2, Exis2a(50) AS STRING * 2, Exis3a( _
50) AS STRING * 2, Exis4a(50) AS STRING * 2
DIM SHARED Cant1a(50) AS STRING * 2, Cant2a(50) AS STRING * 2, Cant3a(50) AS STRING * 2, Cant4a(50) AS STRING * 2
DIM SHARED Xa(10) AS STRING * 6, Xb(10) AS STRING * 2, grabGav AS STRING * 1
CONST true = -1: CONST false = NOT true: busqueda = 0
ON ERROR GOTO ErrorTrap
ON TIMER(5) GOSUB AbreGav
TIMER ON
CALL LeeEstado: CALL Psicotropicos: CLOSE
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... ": COLOR colors(7, colorpref), colors(4, colorpref)
CHAIN "F:\kevinprg\kevin01.bas"
END

ErrorTrap:
    SELECT CASE ERR
        CASE 9
            Box 7, 20, 9, 60: Center 8, "No se pueden Adicionar Articulos Hasta Reiniciar el Sistema"
            SOUND 800, 2: Center 25, "No se pueden crear Mas Articulos Hasta Reiniciar el Sistema": Espera: Fun(11) = "si"
            RESUME NEXT
        CASE 24, 25
            SOUND 600, 2: Center 25, "La impresora no responde ... (revise si esta encendida,on-line o falta papel)": Espera
            RESUME
        CASE 53 'no se encontro archivo ...
            RESUME NEXT
        CASE 70
            Center 25, " Esperando Acceso a La Red   ...": SLEEP 1: Center 25, SPACE$(35): RESUME
        CASE ELSE
REM            CLS : Box 10, 10, 20, 70: Center 13, "Error Inesperado Numero:" + STR$(ERR) + " (Anote Este Numero)": Center 14, "DESCONECTANDOSE DE LA RED ...": LOCATE 21, 1: SYSTEM
    END SELECT
    RESUME NEXT
END
DATA 0,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  3,     0,     1,     15,     0,     7,      0
DATA 3,    15,   13,  1,     14,    3,     15,     0,     7,      0
DATA 7,    12,   15,  4,     14,    0,     15,     15,    1,      0
'Atributo     Valor pre-       Color pre-        Valor pre-    Color pre-
'de color     determinado(a)   sentado           determinado   sentado
'0            0                Negro             0          Desactivado
'1            1                Azul                            Subrayado(d)
'2            2                Verde             1          Activado(d)
'3            3                Azul-verdoso      1          Activado(d)
'4            4                Rojo              1          Activado(d)
'5            5                Magenta           1          Activado(d)
'6            6                Marr¢n            1          Activado(d)
'7            7                Blanco            1          Activado(d)
'8            8                Gris              0          Desactivado
'9            9                Azul claro                      Alta intensidad
'                                                              Subrayado
'10           10               Verde claro       2          Alta intensidad
'11           11               Azul-verdoso claro2          Alta intensidad
'12           12               Rojo claro        2          Alta intensidad
'13           13               Magenta claro     2          Alta intensidad
'14           14               Amarillo          2          Alta intensidad
'15           15               Blanco de alta    0          Desactivado
'                              intensidad
'

'ATENCION AbreGav utiliza #29 y #30 en vez de #27 y #28
AbreGav:
  CLOSE #29
  OPEN "f:\kevinprg\kevindat\gaveta.sem" FOR RANDOM SHARED AS #29 LEN = 1
  GET #29, 1, grabGav
  SELECT CASE grabGav
    CASE "1"
      CLOSE #30: OPEN "com1" FOR OUTPUT AS #30: PRINT #30, "A": PRINT #30, "A": CLOSE #30
      grabGav = "0": PUT #29, 1, grabGav
    CASE "2"
      CLOSE #30: OPEN "com2" FOR OUTPUT AS #30: PRINT #30, "A": PRINT #30, "A": CLOSE #30
      grabGav = "0": PUT #29, 1, grabGav
    CASE ELSE
      'LOCATE 1, 1: PRINT grabGav
  END SELECT
  CLOSE #29
RETURN

REM $STATIC
SUB Borrabox (Row1%, Col1%, Row2%, Col2%) STATIC
BoxWidth = Col2% - Col1% + 1: FOR a = Row1% TO Row2%: LOCATE a, Col1%: PRINT SPACE$(BoxWidth); : NEXT a
END SUB

SUB Box (Row1%, Col1%, Row2%, Col2%) STATIC
BoxWidth = Col2% - Col1% + 1: LOCATE Row1%, Col1%: PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿"; : FOR a = Row1% + 1 TO Row2% - 1: LOCATE a, Col1%: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2%, Col1%: PRINT "À"; STRING$(BoxWidth - 2,  _
"Ä"); "Ù";
END SUB

SUB Box2 (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "²"; STRING$(BoxWidth - 2, "²"); "²"; : FOR a = Row1 + 1 TO Row2 - 1: LOCATE a, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

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
IF mm% = 2 AND dd% > 28 THEN CheqDate = false: EXIT FUNCTION
IF mm% = 1 OR mm% = 3 OR mm% = 5 OR mm% = 7 OR mm% = 8 OR mm% = 10 OR mm% = 12 THEN IF dd% > 31 THEN CheqDate = false: EXIT FUNCTION
IF mm% = 4 OR mm% = 6 OR mm% = 9 OR mm% = 11 THEN IF dd% > 30 THEN CheqDate = false: EXIT FUNCTION
IF op% = 2 THEN
     IF aa% < aa2% THEN CheqDate = false: EXIT FUNCTION
     IF aa% = aa2% THEN IF mm% < mm2% THEN CheqDate = false: EXIT FUNCTION
     IF aa% = aa2% THEN IF mm% = mm2% THEN IF dd% < dd2% THEN CheqDate = false: EXIT FUNCTION
END IF
END FUNCTION

FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%, est%)
COLOR colors(4, colorpref), colors(7, colorpref): selecc% = 199
IF estesi% = 5 THEN Box fila%, columna%, fila% + 16, columna% + 16 ELSE Box fila%, columna%, fila% + 16, columna% + 31
IF est% = 0 THEN Center 25, "[F1]Ayuda  [F5]Editar o Adicionar Linea  [Enter]Tomar Opcion" ELSE Center 25, "[F1]Ayuda  [Enter]Tomar Opcion"
COLOR colors(4, colorpref), colors(7, colorpref)
SELECT CASE estesi%
     CASE 5
          LOCATE fila%, columna% + 1: PRINT " Proveedor "
          CLOSE #2: OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #2 LEN = 18
          FIELD #2, 3 AS codigo$, 15 AS descripcion$
     CASE 6
          LOCATE fila%, columna% + 1: PRINT "Causas de Reclamo"
          CLOSE #2: OPEN "F:\kevinprg\kevindat\causas.dat" FOR RANDOM AS #2 LEN = 33
          FIELD #2, 3 AS codigo$, 30 AS descripcion$, 1 AS edo$
END SELECT
FOR k = 1 TO 199: GET #2, k: code(k) = codigo$: Contenido(k) = descripcion$
NEXT k: CLOSE #2
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE (fila% + 1), (columna% + 1): IF estesi% = 5 THEN PRINT MID$(Contenido(1), 1, 15) ELSE PRINT Contenido(1)
COLOR colors(4, colorpref), colors(7, colorpref)
FOR k = 2 TO 15: LOCATE (fila% + k), (columna% + 1)
IF estesi% = 5 THEN PRINT MID$(Contenido(k), 1, 15) ELSE PRINT Contenido(k)
NEXT k: resal% = 1: up% = 1: terminar = false
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
               IF est% = 0 THEN
                    IF estesi% = 5 THEN
                         Contenido(up% + resal% - 1) = MeteDatoLetra(resal% + 3, columna%, 15, MID$(Contenido(up% + resal% - 1), 1, 15)): Center 25, "Organizando , Espere ...": GOSUB SortYname
                         COLOR colors(4, colorpref), colors(7, colorpref): Box fila%, columna%, fila% + 16, columna% + 16
                    ELSE
                         Contenido(up% + resal% - 1) = MeteDatoLetra(resal% + 4, columna%, 30, (Contenido(up% + resal% - 1)))
                         Center 25, "Seleccione la accion:0 no altera , 1 suma , 2 resta": LOCATE (resal% + 4), (columna% + 1): PRINT "¨ Accion a tomar ?            "
                         DO
                              auxe$ = MeteDatoLetra(resal% + 4, (columna% + 25), 1, " ")
                              IF auxe$ = "0" THEN EXIT DO
                              IF auxe$ = "1" THEN EXIT DO
                              IF auxe$ = "2" THEN EXIT DO
                         LOOP
                         Center 25, "Organizando , Espere ...": GOSUB SortYname
                         IF est% = 0 THEN Center 25, "[F1]Ayuda  [F5]Editar o Adicionar Linea  [Enter]Tomar Opcion" ELSE Center 25, "[F1]Ayuda  [Enter]Tomar Opcion"
                         COLOR colors(4, colorpref), colors(7, colorpref): Box fila%, columna%, fila% + 16, columna% + 31
                    END IF
               ELSE
                    BEEP
               END IF
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
               IF resal% = l THEN
                    COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila2%, columna2% - 1: PRINT USING "²###²"; VAL(code(k));
                    IF estesi% = 5 THEN PRINT MID$(Contenido(k), 1, 15) ELSE PRINT Contenido(k)
               END IF
               LOCATE fila% + l, columna% + 1: IF estesi% = 5 THEN PRINT MID$(Contenido(k), 1, 15) ELSE PRINT Contenido(k)
               IF resal% = l THEN COLOR colors(4, colorpref), colors(7, colorpref)
          NEXT k
LOOP
Elije% = up% + resal% - 1: EXIT FUNCTION
SortYname:
Center 25, " Sorteando Los Campos De Menor A Mayor. Espere Por Favor . . . "
FOR a% = 1 TO 198: FOR b% = a% + 1 TO 199
IF Contenido(a%) > Contenido(b%) THEN SWAP Contenido(a%), Contenido(b%): SWAP code(a%), code(b%)
NEXT b%: NEXT a%:
IF estesi% = 5 THEN OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #2 LEN = 18: FIELD #2, 2 AS codigo$, 15 AS descripcion$, 1 AS edo$
IF estesi% = 6 THEN OPEN "F:\kevinprg\kevindat\causas.dat" FOR RANDOM AS #2 LEN = 33: FIELD #2, 2 AS codigo$, 30 AS descripcion$, 1 AS edo$
FOR k = 1 TO 199: RSET codigo$ = code(k): RSET descripcion$ = Contenido(k): PUT #2, k: NEXT k: CLOSE #2
RETURN
END FUNCTION

FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
IF LEN(caden$) > largo% THEN caden$ = MID$(caden$, 1, largo%)
LOCATE fila%, columna%: PRINT CHR$(178) + caden$ + SPACE$(largo% - LEN(caden$)): LOCATE fila%, columna% + largo% + 1: PRINT CHR$(178)
REDIM eleccion(0 TO largo% + 1) AS STRING * 1: llevasitio% = 2
FOR k = 1 TO largo%: eleccion(k) = MID$(caden$, k, 1): NEXT k: LOCATE fila%, (columna% + llevasitio%): COLOR colors(4, colorpref) + 16, colors(7, colorpref): IF eleccion(2) = " " THEN PRINT "_";  ELSE PRINT eleccion(2)
DO
      llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
          SELECT CASE llave$
               CASE CHR$(0) + "H": devolver = true: Fun(12) = "si": EXIT DO
               CASE CHR$(0) + "P": Fun(12) = "si": EXIT DO
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
               'CASE CHR$(0) + CHR$(60): Fun(2) = "si": EXIT DO
               'CASE CHR$(0) + CHR$(61): Fun(3) = "si": EXIT DO
               'CASE CHR$(0) + CHR$(62): Fun(4) = "si": EXIT DO
               CASE CHR$(0) + CHR$(63): Fun(5) = "si": EXIT DO
               CASE CHR$(0) + CHR$(64): Fun(6) = "si": EXIT DO
               CASE CHR$(0) + CHR$(65): Fun(7) = "si": EXIT DO
               CASE CHR$(0) + CHR$(66): Fun(8) = "si": EXIT DO
               CASE CHR$(0) + CHR$(67): Fun(9) = "si": EXIT DO
               CASE CHR$(0) + CHR$(68): Fun(10) = "si": EXIT DO
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
               CASE ELSE: IF Sonido = "si" THEN BEEP
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
EntraDatoNumero$ = eleccion$: IF Sonido = "si" THEN BEEP
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
CALL Center(25, "[F1] Ayuda [F9] Salir SIN Escoger [Enter] Toma Articulo")
LOCATE fila2%, 5: PRINT "Introduzca Orden de Busqueda:"; SPACE$(15)
Prod$ = UCASE$(MeteDatoLetra(fila2%, 34, 6, "      "))
Prud = Prod$: Prod$ = Trim$(Prod$)
IF Fun(9) = "si" OR Prod$ = "" THEN Fun(9) = "no": Escoge% = -1: EXIT FUNCTION
selecc% = 0: CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
OPEN "F:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
FIELD #2, 35 AS nomshort$, 2 AS num$: max2% = LOF(2) / 37
GOSUB Parche1: GOSUB Parche2
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
          IF Sonido = "si" THEN BEEP
          GET #2, up%: IF Prod$ <> MID$(nomshort$, 1, LEN(Prod$)) THEN up% = up% + 1
          GOSUB LeeCinco
     END IF
RETURN
Abajo:
     resal% = resal% + 1
     IF resal% > selecc% THEN
          resal% = selecc%: up% = up% + 1
          IF up% > max2% THEN up% = max2%
          IF Sonido = "si" THEN BEEP
          GET #2, (up% + 4): IF Prod$ <> MID$(nomshort$, 1, LEN(Prod$)) THEN up% = up% - 1
          GOSUB LeeCinco
     END IF
RETURN
Apgup:
up% = up% - 4: IF up% < 1 THEN up% = 1: IF Sonido = "si" THEN BEEP
DO
GET #2, up%: IF Prod$ = MID$(nomshort$, 1, LEN(Prod$)) THEN EXIT DO ELSE up% = up% + 1
LOOP
resal% = 1: GOSUB LeeCinco
RETURN
Apgdn:
up% = up% + 4: IF up% > max2% THEN up% = max2%: IF Sonido = "si" THEN BEEP
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
          GET #1, CVI(num$)
          IF acti2$ = " " THEN
          selecc% = selecc% + 1
          Nombra(selecc%) = nomb2$: Cant1a(selecc%) = exi12$: Cant2a(selecc%) = exi22$: Cant3a(selecc%) = exi32$: Cant4a(selecc%) = exi42$: Pvp1a(selecc%) = pv12$: Pvp2a(selecc%) = pv22$: Pvp3a(selecc%) = pv32$: Pvp4a(selecc%) = pv42$:  Numa(selecc% _
) = num$
          END IF
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
INPUT #2, Sonido: INPUT #2, Impuesto: INPUT #2, NombFarma: INPUT #2, Direccion
INPUT #2, Rif: INPUT #2, Nit: INPUT #2, Telefono: INPUT #2, contri: INPUT #2, contra
INPUT #2, Stock: INPUT #2, Blanq: INPUT #2, LargoH: INPUT #2, TipoL: INPUT #2, Mensaje1
INPUT #2, Mensaje2: INPUT #2, Mensaje3: INPUT #2, Redondeo: INPUT #2, Desc
INPUT #2, DescMax: INPUT #2, Exactitud: INPUT #2, Prov1
INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo
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
                  llevasitio% = llevasitio% - 1: IF llevasitio% = 0 THEN GOSUB sumar: devolver = true: EXIT DO
               CASE CHR$(0) + "M"
                  llevasitio% = llevasitio% + 1: IF llevasitio% > largo% THEN GOSUB sumar: EXIT DO
               CASE CHR$(8)
                  llevasitio% = llevasitio% - 1: IF llevasitio% = 0 THEN GOSUB sumar: EXIT DO
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
                  IF llevasitio% > largo% THEN GOSUB sumar: EXIT DO
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
MeteDatoLetra$ = eleccion$: IF Sonido = "si" THEN BEEP
EXIT FUNCTION
sumar:
FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna%: PRINT "²"; eleccion$; "²"
RETURN
END FUNCTION

SUB Psicotropicos
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Organizando datos. Espere Por Favor ...": COLOR colors(7, colorpref), colors(4, colorpref)

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

CLS : Box2 1, 5, 3, 75: Center 2, "Relaci¢n de psicotr¢picos de un mes (Tlf. de ayuda:0416-841.68.64)"
Box2 4, 5, 22, 75: Center 5, "Introduzca mes y a¤o": salir = false
DO WHILE NOT salir
     Center 25, "Emplee el formato mm/aaaa , Pulse [F9] para abortar"
     dia$ = MeteDatoLetra$(6, 35, 7, "  /    "): IF Fun(9) = "si" THEN EXIT SUB
     IF VAL(MID$(dia$, 1, 2)) > 0 AND VAL(MID$(dia$, 1, 2)) < 13 THEN
          IF VAL(MID$(dia$, 4, 4)) > 1997 AND VAL(MID$(dia$, 4, 4)) < 2017 THEN salir = true ELSE Center 7, "Teclee bien la fecha , Por Favor ...": Espera: Center 7, "                                    "
     ELSE
          Center 7, "Teclee bien la fecha , por favor ...": Espera: Center 7, "                                    "
     END IF
LOOP
Center 8, "¨ Desea Grabar Saldos para el proximo mes ? Pulse S o N": llave$ = ""
Center 9, "(no se recomienda,a menos de que no haya diferencias de inventario)"
Center 25, "Pulse [S] , [N] o [Esc] para abortar proceso"
DO: llave$ = INKEY$
     IF UCASE$(llave$) = "S" THEN grabasaldo = true: EXIT DO
     IF UCASE$(llave$) = "N" THEN grabasaldo = false: EXIT DO
     IF llave$ = CHR$(27) THEN EXIT SUB
     BEEP
LOOP
IF grabasaldo = true THEN Center 13, "Se escogio la opcion de GRABAR saldos para el proximo mes." ELSE Center 13, "Se escogio la opcion de NO GRABAR saldos para el proximo mes."
Center 9, "                                                                   "
Center 11, "¨ Desea imprimir en papel la relaci¢n ? Pulse S o N": llave$ = ""
Center 25, "Pulse [S] , [N] o [Esc] para abortar proceso": llave$ = ""
DO: llave$ = INKEY$
     IF UCASE$(llave$) = "S" THEN imprimir = true: EXIT DO
     IF UCASE$(llave$) = "N" THEN imprimir = false: EXIT DO
     IF llave$ = CHR$(27) THEN EXIT SUB
     BEEP
LOOP
IF imprimir = true THEN Center 14, "Prepare la impresora con suficiente papel y pulse ON LINE" ELSE Center 14, "Se escogio la opcion de NO IMPRIMIR reporte de Psicotropicos"
CALL Borrabox(7, 1, 11, 80): CALL Termometro(0, 3, 0, "Paso 1:Blancos...", 6, 1)
die$ = MID$(dia$, 1, 2) + MID$(dia$, 4, 4) + ".PSI": archie$ = "f:\kevinprg\kevindat\" + die$
OPEN archie$ FOR OUTPUT AS #7: auxiliar$ = "": mes% = VAL(MID$(dia$, 1, 2))
auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
auxiliar$ = "Valencia , 1ø de ": : ano% = VAL(MID$(dia$, 4, 4))
SELECT CASE mes%
     CASE 1: mos$ = "Febrero": mes$ = "Enero"
     CASE 2: mos$ = "Marzo": mes$ = "Febrero"
     CASE 3: mos$ = "Abril": mes$ = "Marzo"
     CASE 4: mos$ = "Mayo": mes$ = "Abril"
     CASE 5: mos$ = "Junio": mes$ = "Mayo"
     CASE 6: mos$ = "Julio": mes$ = "Junio"
     CASE 7: mos$ = "Agosto": mes$ = "Julio"
     CASE 8: mos$ = "Septiembre": mes$ = "Agosto"
     CASE 9: mos$ = "Octubre": mes$ = "Septiembre"
     CASE 10: mos$ = "Noviembre": mes$ = "Octubre"
     CASE 11: mos$ = "Diciembre": mes$ = "Noviembre"
     CASE 12: mos$ = "Enero": mes$ = "Diciembre"
END SELECT
auxiliar$ = auxiliar$ + mos$: IF mes% = 12 THEN auxiliar$ = auxiliar$ + " de " + STR$(VAL(MID$(dia$, 4, 4)) + 1) + "." ELSE auxiliar$ = auxiliar$ + " de " + MID$(dia$, 4, 4) + "."
auxiliar$ = SPACE$(80 - LEN(auxiliar$)) + auxiliar$: PRINT #7, auxiliar$
auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
auxiliar$ = "CIUDADANO": PRINT #7, auxiliar$
auxiliar$ = "JEFE DE LA DIVISION DE FARMACIA": PRINT #7, auxiliar$
auxiliar$ = "MINISTERIO DE SALUD Y DESARROLLO SOCIAL": PRINT #7, auxiliar$
auxiliar$ = "DIVISION DE DROGAS Y COSMETICOS": PRINT #7, auxiliar$
auxiliar$ = SPACE$(80): PRINT #7, auxiliar$: auxiliar$ = SPACE$(80): PRINT #7, auxiliar$: auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #2 LEN = 100
FIELD #2, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 4 AS msas$, 4 AS colfar$, 4 AS inprefar$, 7 AS vac$
nursaler = LOF(2) / 100: eleg% = 0: IF nursaler = 0 THEN Box2 10, 5, 12, 75: Center 11, "No hay Vendedores Registrados": Espera: CLOSE : EXIT SUB
FOR k = 1 TO nursaler: GET #2, k
IF acti$ = " " AND carg$ = "Farmaceutico Regente" THEN eleg% = k  ': PRINT k
NEXT k
IF eleg% = 0 THEN Box 5, 1, 7, 80: Center 6, "ATENCION:NO HAY FARMACEUTICO REGENTE,POR FAVOR DEFINA UNO": Espera: CLOSE : KILL archie$: EXIT SUB
GET #2, eleg%: auxiliar$ = ""
auxiliar$ = "Yo " + nomb$ + " "
auxiliar$ = auxiliar$ + apel$ + " Regente del establecimiento farma-"
PRINT #7, auxiliar$
auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
auxiliar$ = "c‚utico denominado Farmacia " + NombFarma + ", direcci¢n " + MID$(Direccion, 1, 16): PRINT #7, auxiliar$
auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
auxiliar$ = MID$(Direccion, 17, 14) + ", remite a Ud. la correspondiente n¢mina de farmac‚uticos , auxi -": PRINT #7, auxiliar$
auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
auxiliar$ = "liares y aprendices , mes de " + mes$: PRINT #7, auxiliar$
auxiliar$ = SPACE$(80): PRINT #7, auxiliar$: simpri% = false
FOR k = 1 TO nursaler: GET #2, k
IF simpri% = false THEN
     IF acti$ = " " AND carg$ = "Farmaceutico        " THEN
          auxiliar$ = "FARMACEUTICOS" + SPACE$(47) + "No. CREDENCIAL": PRINT #7, auxiliar$
          auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
     END IF
END IF
IF acti$ = " " AND carg$ = "Farmaceutico        " THEN auxiliar$ = Trim$(nomb$) + " " + Trim$(apel$): auxiliar$ = auxiliar$ + SPACE$(59 - LEN(auxiliar$)) + STR$(CVL(msas$)): PRINT #7, auxiliar$: auxiliar$ = SPACE$(80): PRINT #7, auxiliar$: simpri%  _
= true
NEXT k
auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
auxiliar$ = "AUXILIARES" + SPACE$(50) + "No. CREDENCIAL": PRINT #7, auxiliar$
auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
FOR k = 1 TO nursaler: GET #2, k
IF acti$ = " " AND carg$ = "Auxiliar            " THEN auxiliar$ = Trim$(nomb$) + " " + Trim$(apel$): auxiliar$ = auxiliar$ + SPACE$(59 - LEN(auxiliar$)) + STR$(CVL(msas$)): PRINT #7, auxiliar$: auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
NEXT k
auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
auxiliar$ = "APRENDICES" + SPACE$(50) + "No. CREDENCIAL": PRINT #7, auxiliar$
auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
FOR k = 1 TO nursaler: GET #2, k
IF acti$ = " " AND carg$ = "Aprendiz            " THEN auxiliar$ = Trim$(nomb$) + " " + Trim$(apel$): auxiliar$ = auxiliar$ + SPACE$(59 - LEN(auxiliar$)) + STR$(CVL(msas$)): PRINT #7, auxiliar$: auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
NEXT k
auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
FOR k = 1 TO nursaler: GET #2, k
IF acti$ = " " AND carg$ = "Farmaceutico Regente" THEN
     auxiliar$ = Trim$(nomb$) + " " + Trim$(apel$): nuk% = LEN(auxiliar$)
     auxiliar$ = SPACE$((80 - nuk%) / 2) + auxiliar$: PRINT #7, auxiliar$
     auxiliar$ = "M.S.A.S:" + STR$(CVL(msas$)): auxiliar$ = SPACE$((80 - LEN(auxiliar$)) / 2) + auxiliar$: PRINT #7, auxiliar$
     auxiliar$ = "COL.FAR.:" + STR$(CVL(colfar$)): auxiliar$ = SPACE$((80 - LEN(auxiliar$)) / 2) + auxiliar$: PRINT #7, auxiliar$
     auxiliar$ = "IN.PRE.FAR.:" + STR$(CVL(inprefar$)): auxiliar$ = SPACE$((80 - LEN(auxiliar$)) / 2) + auxiliar$: PRINT #7, auxiliar$
END IF
NEXT k
auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
auxiliar$ = SPACE$(30) + "SELLO DE LA FARMACIA": PRINT #7, auxiliar$
auxiliar$ = CHR$(12): PRINT #7, auxiliar$: guia = 1: GOSUB Encabezado
'#1 new.dbf
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
'#2 saler.dat
'#3 saldos.dat
OPEN "f:\kevinprg\kevindat\saldos.dat" FOR RANDOM SHARED AS #3 LEN = 45
FIELD #3, 6 AS codipsi$, 35 AS nombpsi$, 2 AS saldopsi$, 2 AS sitiopsi$: maxpsi% = LOF(3) / 45
'#4 movpsico.dat
OPEN "F:\kevinprg\kevindat\movpsico.dat" FOR RANDOM AS #4 LEN = 40
FIELD #4, 40 AS todo$: maxmov% = LOF(4) / 40
'#20 proveedo.dat
OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #20 LEN = 18: FIELD #20, 3 AS codigo$, 15 AS descripcion$
FOR k = 1 TO 199: GET #20, k: code(k) = codigo$: Contenido(k) = descripcion$: NEXT k: CLOSE #20

'RSET todo$ = "VENTA " + fct + DATE$ + TIME$ + code1$(k) + cant1(k) + Cedula$
'RSET todo$ = "CREDIT" + notaentrega + DATE$ + TIME$ + code1$(k) + cant1(k) + Cedula$: PUT #1, max%: max% = max% + 1
'RSET todo$ = "COMPRA" + num2$ + Prud2$ + codigo2$ + MKI$(CVI(antid$) + CVI(oferantid$)): PUT #17, max%: CLOSE #17
'RSET todo$ = "DEVCRE" + DATE$ + TIME$ + codigo2$ + MKI$(VAL(auxo$)): PUT #17, max%: CLOSE #17
'RSET todo$ = "DEVOLU" + notac + DATE$ + TIME$ + code1$(k) + cant1(k) + Cedula$: PUT #1, nursaler: nursaler = nursaler + 1
CALL Termometro(0, maxpsi%, 0, "Relacionando...", 6, 48): van% = 0
FOR k = 1 TO maxpsi%
     GET #3, k: GET #1, CVI(sitiopsi$): Termometro k, maxpsi%, 1, "Relacionando...", 6, 48
     saldo% = 0: entra% = 0: sale% = 0
     'LOCATE 10, 1: PRINT k, maxpsi%, codipsi$, nombpsi$
     IF codigo2$ <> codipsi$ THEN BEEP: Espera
     IF UCASE$(MID$(obser2$, 2, 1)) = "B" THEN
          auxiliar$ = STRING$(30, "Ä") + "ÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄ"
          PRINT #7, auxiliar$
          multiplica% = VAL(MID$(obser2$, 3, 3)): saldo% = CVI(saldopsi$)
          FOR l = 1 TO maxmov%
               GET #4, l ': LOCATE 20, 1: PRINT l; , maxmov%
               IF MID$(todo$, 1, 6) = "VENTA " OR MID$(todo$, 1, 6) = "CREDIT" THEN
                    IF ano% = VAL(MID$(todo$, 17, 4)) THEN
                         IF VAL(MID$(todo$, 11, 2)) = mes% AND codipsi$ = MID$(todo$, 29, 6) THEN
                              sale% = sale% + (multiplica% * CVI(MID$(todo$, 35, 2)))
                         END IF
                    END IF
               END IF
               'LOCATE 1, 1: PRINT MID$(todo$, 1, 6); " COMPRA   a¤o:"; ano%; VAL(MID$(todo$, 25, 4))
               'LOCATE 2, 1: PRINT "mes:"; VAL(MID$(todo$, 22, 2)); mes%; " codigo:"; codipsi$; " "; MID$(todo$, 29, 6)
               IF MID$(todo$, 1, 6) = "COMPRA" AND ano% = VAL(MID$(todo$, 25, 4)) THEN
                    IF VAL(MID$(todo$, 22, 2)) = mes% AND codipsi$ = MID$(todo$, 29, 6) THEN
                         entra% = entra% + (multiplica% * CVI(MID$(todo$, 35, 2)))
                    END IF
               END IF
               IF MID$(todo$, 1, 6) = "DEVCRE" AND ano% = VAL(MID$(todo$, 13, 4)) THEN
                    IF VAL(MID$(todo$, 7, 2)) = mes% AND codipsi$ = MID$(todo$, 25, 6) THEN
                         entra% = entra% + (multiplica% * CVI(MID$(todo$, 31, 2)))
                    END IF
               END IF
               IF MID$(todo$, 1, 6) = "DEVOLU" AND ano% = VAL(MID$(todo$, 17, 4)) THEN
                    IF VAL(MID$(todo$, 11, 2)) = mes% AND codipsi$ = MID$(todo$, 29, 6) THEN
                         entra% = entra% + (multiplica% * CVI(MID$(todo$, 35, 2)))
                    END IF
               END IF
          NEXT l
          corrije = false: sit% = 1: Center 25, " Corrija las cantidades necesarias(LAS COMPRAS NO)  [F10]Continuar ": Box 15, 5, 24, 75
          Center 16, ("Articulo:" + nombpsi$ + "(cada caja tiene" + STR$(multiplica%) + ")")
          LOCATE 17, 10: PRINT "Saldo Anterior (# pastillas o ampollas):  "; : PRINT USING "²######²"; saldo%
          LOCATE 19, 10: PRINT "Entradas de mes(# pastillas o ampollas):  "; : PRINT USING "²######²"; entra%
          LOCATE 21, 10: PRINT "Salidas del mes(# pastillas o ampollas):  "; : PRINT USING "²######²"; sale%
          LOCATE 23, 10: PRINT "Saldo Pr¢ximo  (# pastillas o ampollas):  "; : PRINT USING "²######²"; (saldo% + entra% - sale%)
          DO WHILE corrije = false
               SELECT CASE sit%
                    CASE 1
                         corri% = saldo%: saldo% = VAL(EntraDatoNumero(17, 52, 6, STR$(corri%), 1)): sit% = 2: LOCATE 23, 52: PRINT USING "²######²"; (saldo% + entra% - sale%)
                    CASE 2
                         corri% = sale%: sale% = VAL(EntraDatoNumero(21, 52, 6, STR$(corri%), 1)): sit% = 1: LOCATE 23, 52: PRINT USING "²######²"; (saldo% + entra% - sale%)
               END SELECT
               IF Fun(10) = "si" THEN Fun(10) = "no": corrije = true
          LOOP: Box 15, 5, 24, 75: Center 19, " Procesando ... ": Center 25, "Espere..."
          auxiliar$ = MID$(nombpsi$, 1, 30) + "³"
          auxiliar3$ = SPACE$(9 - LEN(STR$(saldo%))) + STR$(saldo%)
          auxiliar$ = auxiliar$ + auxiliar3$ + "³"
          auxiliar3$ = SPACE$(9 - LEN(STR$(entra%))) + STR$(entra%)
          auxiliar$ = auxiliar$ + auxiliar3$ + "³"
          suma% = saldo% + entra%
          auxiliar3$ = SPACE$(9 - LEN(STR$(suma%))) + STR$(suma%)
          auxiliar$ = auxiliar$ + auxiliar3$ + "³"
          auxiliar3$ = SPACE$(9 - LEN(STR$(sale%))) + STR$(sale%)
          auxiliar$ = auxiliar$ + auxiliar3$ + "³"
          suma% = suma% - sale%
          auxiliar3$ = SPACE$(9 - LEN(STR$(suma%))) + STR$(suma%)
          auxiliar$ = auxiliar$ + auxiliar3$
          PRINT #7, auxiliar$: van% = van% + 1
          IF van% > 18 THEN
               auxiliar$ = STRING$(30, "Ä") + "ÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄ"
               PRINT #7, auxiliar$: van% = 0
               auxiliar$ = CHR$(12): PRINT #7, auxiliar$: guia = 1: GOSUB Encabezado
          END IF
          IF grabasaldo = true THEN LSET saldopsi$ = MKI$(suma%): PUT #3, k
     END IF
NEXT k

auxiliar$ = STRING$(30, "Ä") + "ÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄ"
PRINT #7, auxiliar$: van% = 0
auxiliar$ = CHR$(12): PRINT #7, auxiliar$: guia = 2: GOSUB Encabezado
CALL Termometro(1, 3, 0, "Paso 2:Violeta...", 6, 1)
CALL Termometro(0, maxpsi%, 0, "Relacionando...", 6, 48)
FOR k = 1 TO maxpsi%
     GET #3, k: GET #1, CVI(sitiopsi$): Termometro k, maxpsi%, 1, "Relacionando...", 6, 48
     saldo% = 0: entra% = 0: sale% = 0
     'LOCATE 10, 1: PRINT k, maxpsi%, codipsi$, nombpsi$
     IF codigo2$ <> codipsi$ THEN BEEP: Espera
     IF UCASE$(MID$(obser2$, 2, 1)) = "V" THEN
          auxiliar$ = STRING$(30, "Ä") + "ÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄ"
          PRINT #7, auxiliar$
          multiplica% = VAL(MID$(obser2$, 3, 3))
          saldo% = CVI(saldopsi$)
          FOR l = 1 TO maxmov%
               GET #4, l
               IF MID$(todo$, 1, 6) = "VENTA " OR MID$(todo$, 1, 6) = "CREDIT" THEN
                    IF ano% = VAL(MID$(todo$, 25, 4)) THEN
                         IF VAL(MID$(todo$, 11, 2)) = mes% AND codipsi$ = MID$(todo$, 29, 6) THEN
                              sale% = sale% + (multiplica% * CVI(MID$(todo$, 35, 2)))
                         END IF
                    END IF
               END IF
               'LOCATE 1, 1: PRINT MID$(todo$, 1, 6); " COMPRA   a¤o:"; ano%; VAL(MID$(todo$, 25, 4))
               'LOCATE 2, 1: PRINT "mes:"; VAL(MID$(todo$, 22, 2)); mes%; " codigo:"; codipsi$; " "; MID$(todo$, 29, 6)
               IF MID$(todo$, 1, 6) = "COMPRA" AND ano% = VAL(MID$(todo$, 25, 4)) THEN
                    IF VAL(MID$(todo$, 22, 2)) = mes% AND codipsi$ = MID$(todo$, 29, 6) THEN
                         entra% = entra% + (multiplica% * CVI(MID$(todo$, 35, 2)))
                    END IF
               END IF
               IF MID$(todo$, 1, 6) = "DEVCRE" AND ano% = VAL(MID$(todo$, 13, 4)) THEN
                    IF VAL(MID$(todo$, 7, 2)) = mes% AND codipsi$ = MID$(todo$, 25, 6) THEN
                         entra% = entra% + (multiplica% * CVI(MID$(todo$, 31, 2)))
                    END IF
               END IF
               IF MID$(todo$, 1, 6) = "DEVOLU" AND ano% = VAL(MID$(todo$, 17, 4)) THEN
                    IF VAL(MID$(todo$, 11, 2)) = mes% AND codipsi$ = MID$(todo$, 29, 6) THEN
                         entra% = entra% + (multiplica% * CVI(MID$(todo$, 35, 2)))
                    END IF
               END IF
          NEXT l
          corrije = false: sit% = 1: Center 25, " Corrija las cantidades necesarias(LAS COMPRAS NO)  [F10]Continuar ": Box 15, 5, 24, 75
          Center 16, ("Articulo:" + nombpsi$ + "(cada caja tiene" + STR$(multiplica%) + ")")
          LOCATE 17, 10: PRINT "Saldo Anterior (# pastillas o ampollas):  "; : PRINT USING "²######²"; saldo%
          LOCATE 19, 10: PRINT "Entradas de mes(# pastillas o ampollas):  "; : PRINT USING "²######²"; entra%
          LOCATE 21, 10: PRINT "Salidas del mes(# pastillas o ampollas):  "; : PRINT USING "²######²"; sale%
          LOCATE 23, 10: PRINT "Saldo Pr¢ximo  (# pastillas o ampollas):  "; : PRINT USING "²######²"; (saldo% + entra% - sale%)
          DO WHILE corrije = false
               SELECT CASE sit%
                    CASE 1
                         corri% = saldo%: saldo% = VAL(EntraDatoNumero(17, 52, 6, STR$(corri%), 1)): sit% = 2: LOCATE 23, 52: PRINT USING "²######²"; (saldo% + entra% - sale%)
                    CASE 2
                         corri% = sale%: sale% = VAL(EntraDatoNumero(21, 52, 6, STR$(corri%), 1)): sit% = 1: LOCATE 23, 52: PRINT USING "²######²"; (saldo% + entra% - sale%)
               END SELECT
               IF Fun(10) = "si" THEN Fun(10) = "no": corrije = true
          LOOP: Box 15, 5, 24, 75: Center 19, " Procesando ... ": Center 25, "Espere..."
          auxiliar$ = MID$(nombpsi$, 1, 30) + "³"
          auxiliar3$ = SPACE$(9 - LEN(STR$(saldo%))) + STR$(saldo%)
          auxiliar$ = auxiliar$ + auxiliar3$ + "³"
          auxiliar3$ = SPACE$(9 - LEN(STR$(entra%))) + STR$(entra%)
          auxiliar$ = auxiliar$ + auxiliar3$ + "³"
          suma% = saldo% + entra%
          auxiliar3$ = SPACE$(9 - LEN(STR$(suma%))) + STR$(suma%)
          auxiliar$ = auxiliar$ + auxiliar3$ + "³"
          auxiliar3$ = SPACE$(9 - LEN(STR$(sale%))) + STR$(sale%)
          auxiliar$ = auxiliar$ + auxiliar3$ + "³"
          suma% = suma% - sale%
          auxiliar3$ = SPACE$(9 - LEN(STR$(suma%))) + STR$(suma%)
          auxiliar$ = auxiliar$ + auxiliar3$
          PRINT #7, auxiliar$: van% = van% + 1
          IF van% > 18 THEN
               auxiliar$ = STRING$(30, "Ä") + "ÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄ"
               PRINT #7, auxiliar$: van% = 0
               auxiliar$ = CHR$(12): PRINT #7, auxiliar$: guia = 1: GOSUB Encabezado
          END IF
          IF grabasaldo = true THEN LSET saldopsi$ = MKI$(suma%): PUT #3, k
     END IF
NEXT k
auxiliar$ = STRING$(30, "Ä") + "ÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄ"
PRINT #7, auxiliar$: van% = 0
auxiliar$ = CHR$(12): PRINT #7, auxiliar$
auxiliar$ = "     RELACION DE ENTRADAS DE PSICOTROPICOS Y ESTUPEFACTIVOS DE LA FARMACIA:": PRINT #7, auxiliar$
auxiliar$ = "     " + NombFarma + " mes de " + mes$: PRINT #7, auxiliar$
auxiliar$ = "     ÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄ": PRINT #7, auxiliar$
auxiliar$ = "     Dia³Nombre del Producto                ³Proveedor      ³Factura ³Cant": PRINT #7, auxiliar$
CALL Termometro(2, 3, 0, "Paso 3:Compras...", 6, 1)
CALL Termometro(0, maxmov%, 0, "Relacionando...", 6, 48)
FOR l = 1 TO maxmov%
     GET #4, l: Termometro l, maxmov%, 1, "Relacionando...", 6, 48
     IF MID$(todo$, 1, 6) = "COMPRA" AND ano% = VAL(MID$(todo$, 25, 4)) THEN
          IF VAL(MID$(todo$, 22, 2)) = mes% THEN
               FOR k = 1 TO maxpsi%
                    GET #3, k: GET #1, CVI(sitiopsi$)
                    IF codipsi$ = MID$(todo$, 29, 6) THEN
                         auxiliar$ = "     ÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÅÄÄÄÄ": PRINT #7, auxiliar$
                         multiplica% = VAL(MID$(obser2$, 3, 3))
                         auxiliar$ = "      " + MID$(todo$, 19, 2) + "³" + nomb2$ + "³"
                         FOR m = 1 TO 199
                                IF VAL(code(m)) = VAL(MID$(todo$, 16, 3)) THEN auxiliar$ = auxiliar$ + Contenido(m) + "³": EXIT FOR
                         NEXT m
                         auxiliar$ = auxiliar$ + MID$(todo$, 7, 8) + "³"
                         auxiliar$ = auxiliar$ + MID$(STR$(multiplica% * CVI(MID$(todo$, 35, 2))), 2, 4): PRINT #7, auxiliar$
                         van% = van% + 1
                         IF van% > 18 THEN
                              auxiliar$ = "     ÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄ": PRINT #7, auxiliar$
                              auxiliar$ = CHR$(12): PRINT #7, auxiliar$
                              auxiliar$ = "     RELACION DE ENTRADAS DE PSICOTROPICOS Y ESTUPEFACTIVOS DE LA FARMACIA:": PRINT #7, auxiliar$
                              auxiliar$ = "     " + NombFarma + " mes de " + mes$: PRINT #7, auxiliar$: van% = 0
                              auxiliar$ = "     ÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄ": PRINT #7, auxiliar$
                              auxiliar$ = "     Dia³Nombre del Producto                ³Proveedor      ³Factura ³Cant": PRINT #7, auxiliar$
                         END IF
                    END IF
               NEXT k
          END IF
     END IF
NEXT l
auxiliar$ = "     ÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄ": PRINT #7, auxiliar$
auxiliar$ = CHR$(12): PRINT #7, auxiliar$
CALL Termometro(3, 3, 0, "Paso 3:Compras...", 6, 1)
CLOSE
EXIT SUB

Encabezado:
auxiliar$ = "Valencia , 1ø de " + mos$: IF mes% = 12 THEN auxiliar$ = auxiliar$ + " de " + STR$(VAL(MID$(dia$, 4, 4)) + 1) + "." ELSE auxiliar$ = auxiliar$ + " de " + MID$(dia$, 4, 4) + "."
auxiliar$ = SPACE$(80 - LEN(auxiliar$)) + auxiliar$: PRINT #7, auxiliar$
auxiliar$ = "Movimiento de las Especialidades Farmaceuticas que contienen sustancias  Psico -": PRINT #7, auxiliar$
IF guia = 1 THEN auxiliar$ = "tropicas para ser expendidas con RECIPE CORRIENTE,de la FARMACIA:" + MID$(NombFarma, 1, 14) + "-"
IF guia = 2 THEN auxiliar$ = "tropicas para ser expendidas con RECIPE VIOLETA , de la FARMACIA:" + MID$(NombFarma, 1, 14) + "-"
PRINT #7, auxiliar$
auxiliar$ = MID$(NombFarma, 15, 11) + " durante el mes de " + mes$: PRINT #7, auxiliar$
auxiliar$ = SPACE$(80): PRINT #7, auxiliar$: auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
FOR J = 1 TO nursaler: GET #2, J
IF acti$ = " " AND carg$ = "Farmaceutico Regente" THEN
     auxiliar$ = "Farmac‚utico Regente Dr.(a):" + Trim$(nomb$) + " " + Trim$(apel$)
     PRINT #7, auxiliar$
     auxiliar$ = "M.S.A.S:" + STR$(CVL(msas$))
     PRINT #7, auxiliar$
     auxiliar$ = "COL.FAR.:" + STR$(CVL(colfar$)) + SPACE$(10) + STRING$(19, "_")
     PRINT #7, auxiliar$
     auxiliar$ = "IN.PRE.FAR.:" + STR$(CVL(inprefar$)) + SPACE$(12) + "Firma" + SPACE$(25) + "Sello de la Farmacia"
     PRINT #7, auxiliar$
END IF
NEXT J
auxiliar$ = SPACE$(80): PRINT #7, auxiliar$
auxiliar$ = STRING$(30, "Ä") + "ÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄ": PRINT #7, auxiliar$
auxiliar$ = "   Nombre del Producto        ³saldo ant³entradas ³disponibi³ salidas ³saldo act": PRINT #7, auxiliar$
RETURN
END SUB

SUB Termometro (va%, total%, edo%, text$, fila%, columna%)
IF edo% = 0 THEN Box fila%, columna%, fila% + 6, columna% + 31
LOCATE fila% + 1, columna% + 1: PRINT "0%                        100%"
IF edo% = 0 THEN LOCATE fila% + 2, columna% + 1: PRINT STRING$(30, "±")
LOCATE fila% + 2, columna% + 1: PRINT STRING$(INT((va% / total%) * 30), "Û")
LOCATE fila% + 3, columna% + 1: PRINT "Van"; va%; " de "; total%;
LOCATE fila% + 4, columna% + 9: PRINT USING "###.## % Hecho "; ((va% / total%) * 100)
LOCATE fila% + 5, columna% + 1: PRINT text$
END SUB

FUNCTION Trim$ (x$)
IF x$ = "" THEN
     Trim$ = ""
ELSE
lastChar = 0
FOR a = 1 TO LEN(x$): y$ = MID$(x$, a, 1): IF y$ <> CHR$(0) AND y$ <> " " THEN lastChar = a
NEXT a: Trim$ = LEFT$(x$, lastChar)
END IF
END FUNCTION

