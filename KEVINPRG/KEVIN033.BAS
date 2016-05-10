' visualiza facturas de compra
DEFINT A-Z
DECLARE SUB Espera ()
DECLARE SUB LeeEstado ()
DECLARE SUB Center (row, text$)
DECLARE SUB Box (Row1%, Col1%, Row2%, Col%)
DECLARE SUB BorraBox (Row1%, Col1%, Row2%, Col%)
DECLARE SUB VerFactCompras ()
DECLARE SUB Termometro (va%, total%, edo%, text$, fila%, columna%)
DECLARE SUB PopUp (ka%, la%, ma%, na%, pa%, ra%, ta%, sa%)
DECLARE SUB ShutUp
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$)
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Escoge% (fila%, edo%, fila2%, Prod$)
DECLARE FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%, est%)
DECLARE FUNCTION CheqDate (caden$, op%)
DECLARE FUNCTION Trim$ (x$)
DECLARE FUNCTION CalculaDias% (caden$, op%)
DECLARE FUNCTION Verify (ubi%, text$)
COMMON SHARED guia, colorpref
DIM SHARED turno AS STRING * 1, devolver, neto AS STRING * 8, bruto AS STRING * 8, iva AS STRING * 8, renglones AS STRING * 2, unidades AS STRING * 4, exisfact AS INTEGER, fctorig AS STRING * 4
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED Sonido  AS STRING * 2, Impuesto AS STRING * 6, NombFarma AS STRING * 25, Direccion AS STRING * 30, Rif     AS STRING * 25, Nit      AS STRING * 25, Telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2, num1 AS STRING * 33, num2 AS STRING * 33
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2, acumulado AS STRING * 8
DIM SHARED Fun(17) AS STRING * 2, salir, Turna, terminar, sitio, busqueda, cspedido
DIM SHARED Contenido(199) AS STRING * 15, Code(199) AS STRING * 3, porc AS STRING * 8
DIM SHARED numfact(10) AS STRING * 4, provee(10) AS STRING * 18, fechafact(10) AS STRING * 8, fechaacc(10) AS STRING * 8, bruto(10) AS STRING * 8, neto(10) AS STRING * 8, ivafact(10) AS STRING * 8, transpor(10) AS STRING * 10, edofact(10) AS STRING  _
* 3, ubifact(10) AS STRING * 2, imprimir(10) AS STRING * 1
REM $DYNAMIC
DIM SHARED Nombra(50) AS STRING * 35, Pvp1a(50) AS STRING * 8, Pvp2a(50) AS STRING * 8, Pvp3a(50) AS STRING * 8, Pvp4a(50) AS STRING * 8, Numa(50) AS STRING * 2, observa(50) AS STRING * 20, Exis1a(50) AS STRING * 2, Exis2a(50) AS STRING * 2, Exis3a( _
50) AS STRING * 2, Exis4a(50) AS STRING * 2
DIM SHARED Cant1a(50) AS STRING * 2, Cant2a(50) AS STRING * 2, Cant3a(50) AS STRING * 2, Cant4a(50) AS STRING * 2
DIM SHARED Xa(10) AS STRING * 6, Xb(10) AS STRING * 2, grabGav AS STRING * 1
CONST true = -1: CONST false = NOT true: busqueda = 0
ON ERROR GOTO ErrorTrap
ON TIMER(5) GOSUB AbreGav
TIMER ON
LeeEstado
VerFactCompras
CLOSE : OPEN "f:\kevinprg\kevindat\compras.tkt" FOR RANDOM AS #1 LEN = 2: FIELD #1, 2 AS ticket$
GET #1, 1: RSET ticket$ = "si": PUT #1, 1: CLOSE
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
            exisfact = false: RESUME NEXT
        CASE 70
            Center 25, " Esperando Acceso a La Red   ...": SLEEP 1: Center 25, SPACE$(35): BEEP: RESUME
        CASE ELSE
REM            CLS : Box 10, 10, 20, 70: Center 13, "Error Inesperado Numero:" + STR$(ERR) + " (Anote Este Numero)": Center 14, "DESCONECTANDOSE DE LA RED ...": LOCATE 21, 1: SYSTEM
    PRINT ERR
    END SELECT
    RESUME NEXT
END
DATA 0,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  2,     0,     1,     15,     0,     7,      0
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
SUB BorraBox (Row1%, Col1%, Row2%, Col2%) STATIC
BoxWidth = Col2% - Col1% + 1: FOR a = Row1% TO Row2%: LOCATE a, Col1%: PRINT SPACE$(BoxWidth); : NEXT a
END SUB

SUB Box (Row1%, Col1%, Row2%, Col2%) STATIC
BoxWidth = Col2% - Col1% + 1
LOCATE Row1%, Col1%
PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿";
FOR a% = Row1% + 1 TO Row2% - 1
   LOCATE a%, Col1%: PRINT "³"; SPACE$(BoxWidth - 2); "³";
NEXT a%: LOCATE Row2%, Col1%: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù";
END SUB

FUNCTION CalculaDias% (caden$, op%)
IF op% = 1 THEN CalculaDias% = VAL(MID$(caden$, 1, 2)) + (VAL(MID$(caden$, 4, 2)) * 30) + (VAL(MID$(caden$, 7, 4)) - 1990) * 365
IF op% = 2 THEN CalculaDias% = VAL(MID$(caden$, 3, 2)) + (VAL(MID$(caden$, 1, 2)) * 30) + (VAL(MID$(caden$, 5, 4)) - 1990) * 365
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
FOR a% = 1 TO 198: FOR b% = a% + 1 TO 199
IF Contenido(a%) > Contenido(b%) THEN SWAP Contenido(a%), Contenido(b%): SWAP Code(a%), Code(b%)
NEXT b%: NEXT a%:
IF estesi% = 1 THEN OPEN "F:\kevinprg\kevindat\ubica.dat" FOR RANDOM AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$
IF estesi% = 2 THEN OPEN "F:\kevinprg\kevindat\lab.dat" FOR RANDOM AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$
IF estesi% = 4 THEN OPEN "F:\kevinprg\kevindat\componen.dat" FOR RANDOM AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$
IF estesi% = 5 THEN OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$
FOR k = 1 TO 199: RSET codigo$ = Code(k): RSET descripcion$ = Contenido(k): PUT #2, k: NEXT k: CLOSE #2
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
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila% - 1, 1: PRINT STRING$(80, "Ä"): LOCATE fila% + 5, 1: PRINT STRING$(80, "Ä");
FOR k = fila% TO fila% + 4: LOCATE k, 1: PRINT SPACE$(80); : NEXT k
CALL Center(25, "[F1] Ayuda [F9] Salir SIN Escoger [Enter] Toma Articulo"): Prod$ = ""
DO WHILE LEN(Prod$) < 3
     LOCATE fila2%, 1: PRINT SPACE$(5); "Introduzca Orden de Busqueda:"; SPACE$(40); : Center fila2% + 1, "Teclee , MINIMO , las tres (3) Primeras Letras . . ."
     Prod$ = MeteDatoLetra(fila2%, 35, 6, "      ")
     Prod$ = Trim$(UCASE$(Prod$)): IF Fun(9) = "si" OR Prod$ = "" THEN : Escoge% = -1: EXIT FUNCTION
LOOP: selecc% = 0
LOCATE fila% - 2, 5: PRINT " Se Est  Buscando , Espere Por Favor . . . "
CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS Saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
IF busqueda = 0 THEN
     OPEN "F:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #7 LEN = 37
     FIELD #7, 35 AS nomshort$, 2 AS num$: max2% = LOF(7) / 37: busqueda = max2%
     ERASE Xa, Xb: REDIM Xa(max2%) AS STRING * 6, Xb(max2%) AS STRING * 2
     FOR k = 1 TO max2%: GET #7, k: Xa(k) = MID$(nomshort$, 1, 6): Xb(k) = num$: NEXT k: CLOSE #7
ELSE
     max2% = busqueda
END IF
          encontrado = false: primero = 1: ultimo = max2%
          WHILE NOT encontrado AND primero <= ultimo
               central = (primero + ultimo) / 2
               'LOCATE 1, 1: PRINT Prod$; "--"; central
               IF Prod$ = MID$(Xa(central), 1, LEN(Prod$)) THEN
                    encontrado = true
               ELSE
                    IF MID$(Xa(central), 1, LEN(Prod$)) > Prod$ THEN
                         ultimo = central - 1
                    ELSE
                         primero = central + 1
                    END IF
               END IF
          WEND: salir = false
          DO WHILE salir = false
               central = central - 1: IF central < 1 THEN salir = true: central = 1
               IF Prod$ <> MID$(Xa(central), 1, LEN(Prod$)) THEN salir = true: central = central + 1
          LOOP
          FOR k = central TO max2%
               IF Prod$ = MID$(Xa(k), 1, LEN(Prod$)) THEN
                    GET #1, CVI(Xb(k)): selecc% = selecc% + 1
                    Nombra(selecc%) = nomb2$: Cant1a(selecc%) = exi12$: Cant2a(selecc%) = exi22$: Cant3a(selecc%) = exi32$: Cant4a(selecc%) = exi42$: Pvp1a(selecc%) = pv12$: Pvp2a(selecc%) = pv22$: Pvp3a(selecc%) = pv32$: Pvp4a(selecc%) = pv42$: _
                                                     observa(selecc%) = obser2$: Numa(selecc%) = Xb(k)
               ELSE
                    EXIT FOR
               END IF
          NEXT k
IF selecc% = 0 THEN LOCATE fila% + 5, 5: PRINT STRING$(5, "Ä"); "²No se encontr¢²"; STRING$(7, "Ä"); : LOCATE fila% - 2, 5: PRINT SPACE$(45): Espera: Escoge% = 0: EXIT FUNCTION
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila% - 2, 1: PRINT SPACE$(70): COLOR colors(4, colorpref), colors(7, colorpref)
wer$ = STRING$(36, "Ä") + "CAN1ÄÄÄÄÄÄPVP1ÂCAN2" + STRING$(6, "Ä") + "PVP2ÂCAN3" + STRING$(5, "Ä") + "PVP3" + "Ä"
LOCATE fila% - 1, 1: PRINT wer$;
wer$ = "ÄÄÄÄ" + STR$(selecc%) + " articulo(s) para escoger" + STRING$(50 - LEN(STR$(selecc%)), "Ä"): LOCATE fila% + 5, 1: PRINT wer$; : COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE fila%, 1: PRINT Nombra(1); "³"; : LOCATE fila%, 37: PRINT USING "####"; CVI(Cant1a(1)); : LOCATE fila%, 51: PRINT "³"; : PRINT USING "####"; CVI(Cant2a(1)); : LOCATE fila%, 66: PRINT "³"; : PRINT USING "####"; CVI(Cant3a(1))
LOCATE fila%, 41: PRINT USING "######,.##"; CVD(Pvp1a(1)): LOCATE fila%, 56: PRINT USING "######,.##"; CVD(Pvp2a(1)): LOCATE fila%, 71: PRINT USING "######,.##"; CVD(Pvp3a(1)): COLOR colors(4, colorpref), colors(7, colorpref)
LOCATE fila% - 2, 50: PRINT "En Oferta:"; : PRINT USING "####"; CVI(Cant4a(1)); : PRINT " a Bs."; : PRINT USING "######,.##"; CVD(Pvp4a(1))
FOR k = 2 TO 5
     IF k <= selecc% THEN
     LOCATE fila% + k - 1, 1: PRINT Nombra(k); "³"; : LOCATE fila% + k - 1, 37: PRINT USING "####"; CVI(Cant1a(k)); : LOCATE fila% + k - 1, 51: PRINT "³"; : PRINT USING "####"; CVI(Cant2a(k));
     LOCATE fila% + k - 1, 66: PRINT "³"; : PRINT USING "####"; CVI(Cant3a(k)): LOCATE fila% + k - 1, 41: PRINT USING "######,.##"; CVD(Pvp1a(k)): LOCATE fila% + k - 1, 56: PRINT USING "######,.##"; CVD(Pvp2a(k))
     LOCATE fila% + k - 1, 71: PRINT USING "######,.##"; CVD(Pvp3a(k))
     ELSE
     LOCATE fila% + k - 1, 1: PRINT SPACE$(80)
     END IF
NEXT k
resal% = 1: up% = 1: IF selecc% > 5 THEN ventana% = 5 ELSE ventana% = selecc%
DO
     kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
     SELECT CASE kbd$
          CASE CHR$(0) + "H": GOSUB Arriba
          CASE CHR$(0) + "P": GOSUB Abajo
          CASE CHR$(0) + CHR$(73): IF selecc% > 5 THEN GOSUB Apgup ELSE BEEP
          CASE CHR$(0) + CHR$(81): IF selecc% > 5 THEN GOSUB Apgdn ELSE BEEP
          CASE CHR$(13): EXIT DO
          CASE CHR$(27): resal% = 0: up% = 1: EXIT DO
          CASE CHR$(0) + CHR$(67): resal% = 0: up% = 1: Fun(3) = "si": EXIT DO
          CASE ELSE:  BEEP
     END SELECT
IF up% = 0 THEN
     FOR k = 0 TO 4
     LOCATE fila% + k, 1: PRINT SPACE$(80);
     NEXT k
     EXIT DO
END IF
l = 0
FOR k = up% TO up% + 4
l = l + 1
LOCATE fila% - 2, 50: PRINT "En Oferta:"; : PRINT USING "####"; CVI(Cant4a(k)); : PRINT " a Bs."; : PRINT USING "######,.##"; CVD(Pvp4a(k))
IF resal% = l THEN COLOR colors(7, colorpref), colors(4, colorpref)
IF l <= selecc% THEN
     LOCATE fila% + l - 1, 1: PRINT Nombra(k); "³"; : LOCATE fila% + l - 1, 37: PRINT USING "####"; CVI(Cant1a(k)); : LOCATE fila% + l - 1, 51: PRINT "³"; : PRINT USING "####"; CVI(Cant2a(k));
     LOCATE fila% + l - 1, 66: PRINT "³"; : PRINT USING "####"; CVI(Cant3a(k)): LOCATE fila% + l - 1, 41: PRINT USING "######,.##"; CVD(Pvp1a(k)): LOCATE fila% + l - 1, 56: PRINT USING "######,.##"; CVD(Pvp2a(k))
     LOCATE fila% + l - 1, 71: PRINT USING "######,.##"; CVD(Pvp3a(k))
END IF
IF resal% = l THEN COLOR colors(4, colorpref), colors(7, colorpref)
NEXT k
LOOP
Escoge% = CVI(Numa(up% + resal% - 1))
EXIT FUNCTION
Arriba:
     resal% = resal% - 1
     IF selecc% < 6 THEN
          IF resal% < 1 THEN resal% = 1: IF Sonido = "si" THEN BEEP
     ELSE
          IF resal% < 1 THEN resal% = 1: up% = up% - 1
     END IF
     IF up% < 1 THEN up% = 1: IF Sonido = "si" THEN BEEP
RETURN
Abajo: resal% = resal% + 1
     IF selecc% < 6 THEN
          IF resal% > ventana% THEN resal% = ventana%: IF Sonido = "si" THEN BEEP
     ELSE
          IF resal% > 5 THEN resal% = 5: up% = up% + 1
     END IF
     IF selecc% > 5 THEN
          IF up% + 4 > selecc% THEN up% = selecc% - 4: IF Sonido = "si" THEN BEEP
     END IF
RETURN
Apgup: up% = up% - 4: IF up% < 1 THEN up% = 1: resal% = 1: IF Sonido = "si" THEN BEEP
RETURN
Apgdn: up% = up% + 4: IF up% + 4 > selecc% THEN up% = selecc% - 4: resal% = 5: IF Sonido = "si" THEN BEEP
RETURN
END FUNCTION

SUB Espera
Center 25, " Presione Cualquier Tecla Para Continuar ... "
DO: LOOP WHILE INKEY$ = ""
END SUB

SUB LeeEstado
OPEN "F:\kevinprg\kevindat\kaos.dat" FOR INPUT AS #2
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
MeteDatoLetra$ = eleccion$: IF Sonido = "si" THEN BEEP
EXIT FUNCTION
Sumar:
FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna%: PRINT "²"; eleccion$; "²"
RETURN
END FUNCTION

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

SUB VerFactCompras
COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box 1, 1, 3, 80
Center 2, "Consulta de una Factura de Compra.": comienzo% = 0
CLOSE : OPEN "F:\kevinprg\kevindat\master.fct" FOR RANDOM SHARED AS #27 LEN = 70: FIELD #27, 70 AS linea$: final% = (LOF(27) / 70): selecc% = 0    ': selecc% = final%
IF final% = 0 THEN Box 5, 1, 7, 80: Center 6, "NO Hay NINGUNA Factura Para Visualizar": Espera: EXIT SUB
FOR k = final% TO 1 STEP -1
     GET #27, k: IF MID$(linea$, 1, 1) = " " THEN comienzo% = comienzo% + 1
     IF comienzo% > 9 THEN comienzo% = k: EXIT FOR
NEXT k
FOR k = comienzo% TO final%
IF k > 0 THEN
     GET #27, k
     IF MID$(linea$, 1, 1) = " " THEN
          selecc% = selecc% + 1: numfact(selecc%) = MID$(linea$, 2, 4): provee(selecc%) = MID$(linea$, 24, 2): edofact(selecc%) = MID$(linea$, 26, 1) + MID$(linea$, 59, 1) + MID$(linea$, 62, 1)
          fechaacc(selecc%) = MID$(linea$, 6, 8): transpor(selecc%) = MID$(linea$, 14, 10): fechafact(selecc%) = MID$(linea$, 51, 8): ubifact(selecc%) = MKI$(k)
          bruto(selecc%) = MID$(linea$, 27, 8): neto(selecc%) = MID$(linea$, 35, 8): ivafact(selecc%) = MID$(linea$, 43, 8)
     END IF
END IF
NEXT k
IF selecc% = 0 THEN Box 5, 1, 7, 80: Center 6, "NO Hay NINGUNA Factura Para Visualizar": Espera: EXIT SUB
OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$
FOR k = 1 TO 199: GET #2, k: Code(k) = codigo$: Contenido(k) = descripcion$: NEXT k: CLOSE #2
FOR k = 1 TO selecc%
     FOR l = 1 TO 199
          IF CVI(MID$(provee(k), 1, 2)) = VAL(Code(l)) THEN
               provee(k) = Code(l) + Contenido(l): l = 199
          END IF
     NEXT l
NEXT k: resal% = 0
DO
         COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box 1, 1, 3, 80
         Center 2, "Consulta de una Factura de Compra."
         COLOR colors(7, colorpref), colors(4, colorpref): Box 4, 1, 16, 80: Center 4, "Facturas Registradas": LOCATE 5, 2: PRINT "Factura:         Proveedor:       FechaFact:  FechaAcce:  Transcrita: F.Pago: "
         Center 24, "[F3]Ir a fecha [F4]Suma montos seg£n fecha [F5]Edita [F8]Borra facturas"
         Center 25, "Use Las Teclas de Direccion y Enter ; Presione Escape Para Salir "
         IF resal% = 0 THEN
             COLOR colors(7, colorpref), colors(4, colorpref)
             LOCATE 6, 2: PRINT USING " ##############,³"; CVL(numfact(1)); : PRINT MID$(provee(1), 4, 15); "³ "; MID$(fechafact(1), 3, 2); "/"; MID$(fechafact(1), 1, 2); "/"; MID$(fechafact(1), 5, 4); "³ "; MID$(fechaacc(1), 1, 2); "/"; MID$( _
fechaacc(1), 3, 2); "/"; MID$(fechaacc(1), 5, 4); "³ "; transpor(1);
             IF MID$(edofact(1), 1, 1) = "*" THEN PRINT " ³Contado ";  ELSE PRINT " ³Credito ";
             'IF MID$(edofact(1), 2, 1) = "*" THEN PRINT "   SI   ";  ELSE PRINT "   NO   ";
             LOCATE 18, 4: PRINT USING "Bruto Bs. ########,.##"; CVD(bruto(1)); : PRINT USING "   Impuesto Bs. ########,.##"; CVD(ivafact(1)); : PRINT USING "   Neto Bs. ########,.##"; CVD(neto(1));
             COLOR colors(4, colorpref), colors(7, colorpref)
         ELSE
             COLOR colors(4, colorpref), colors(7, colorpref)
             LOCATE 6, 2: PRINT USING " ##############,³"; CVL(numfact(1)); : PRINT MID$(provee(1), 4, 15); "³ "; MID$(fechafact(1), 3, 2); "/"; MID$(fechafact(1), 1, 2); "/"; MID$(fechafact(1), 5, 4); "³ "; MID$(fechaacc(1), 1, 2); "/"; MID$( _
fechaacc(1), 3, 2); "/"; MID$(fechaacc(1), 5, 4); "³ "; transpor(1);
             IF MID$(edofact(1), 1, 1) = "*" THEN PRINT " ³Contado ";  ELSE PRINT " ³Credito ";
             'IF MID$(edofact(1), 2, 1) = "*" THEN PRINT "   SI   ";  ELSE PRINT "   NO   ";
             LOCATE 18, 4: PRINT USING "Bruto Bs. ########,.##"; CVD(bruto(1)); : PRINT USING "   Impuesto Bs. ########,.##"; CVD(ivafact(1)); : PRINT USING "   Neto Bs. ########,.##"; CVD(neto(1));
             COLOR colors(4, colorpref), colors(7, colorpref)
         END IF
         FOR k = 2 TO 10
              IF resal% = k THEN COLOR colors(7, colorpref), colors(4, colorpref)
              IF k <= selecc% THEN
                   LOCATE 5 + k, 2: PRINT USING "###############,³"; CVL(numfact(k)); : PRINT MID$(provee(k), 4, 15); "³ "; MID$(fechafact(k), 3, 2); "/"; MID$(fechafact(k), 1, 2); "/"; MID$(fechafact(k), 5, 4); "³ "; MID$(fechaacc(k), 1, 2); "/";  _
MID$(fechaacc(k), 3, 2); "/"; MID$(fechaacc(k), 5, 4); "³ "; transpor(k);
                   IF MID$(edofact(k), 1, 1) = "*" THEN PRINT " ³Contado ";  ELSE PRINT " ³Credito ";
                   'IF MID$(edofact(k), 2, 1) = "*" THEN PRINT "   SI   ";  ELSE PRINT "   NO   ";
              ELSE
                   LOCATE 5 + k, 2: PRINT SPACE$(78)
              END IF
              IF resal% = k THEN COLOR colors(4, colorpref), colors(7, colorpref)
         NEXT k
         IF resal% = 0 THEN resal% = 1
         ventana% = selecc%: up% = final% - 10
         IF up% < 1 THEN up% = 1
         DO
              kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
              SELECT CASE kbd$
                   CASE CHR$(0) + "H": GOSUB Arri
                   CASE CHR$(0) + "P": GOSUB Abaj
                   CASE CHR$(0) + CHR$(73): GOSUB Agup
                   CASE CHR$(0) + CHR$(81): GOSUB Agdn
                   CASE CHR$(0) + CHR$(61): GOSUB CalculaMontos: EXIT SUB
                   CASE CHR$(0) + CHR$(62): GOSUB CalculaMontos: EXIT SUB
                   CASE CHR$(0) + CHR$(63): eleg = resal%: GOSUB ModificaFactura
                   CASE CHR$(0) + CHR$(66)
                        Box 10, 5, 12, 75: Center 11, "¨SEGURO DESEA BORRAR ESTA FACTURA? Pulse S o N "
                        llave$ = "": DO WHILE llave$ = "": llave$ = INKEY$: LOOP: llave$ = UCASE$(llave$)
                        IF llave$ = "S" THEN
                             eleg% = resal%
                             CLOSE : OPEN "F:\kevinprg\kevindat\master.fct" FOR RANDOM SHARED AS #27 LEN = 70: FIELD #27, 70 AS linea$
                             GET #27, CVI(ubifact(eleg%))
                             IF numfact(eleg%) = MID$(linea$, 2, 4) THEN
                                  RSET linea$ = "*" + MID$(linea$, 2, 69): PUT #27, CVI(ubifact(eleg%))
                             END IF: CLOSE : EXIT SUB
                        END IF
                   CASE CHR$(13): eleg = resal%: EXIT DO
                   CASE CHR$(27): resal% = 0: up% = 1: EXIT SUB
                   CASE ELSE:  BEEP
              END SELECT
            l = 0
            FOR k = 1 TO 10: l = l + 1   ': LOCATE 1, 1: PRINT "k="; k, "up%="; up%, "l="; l, "resal%="; resal%, "selecc%="; selecc%
               IF resal% = l THEN COLOR colors(7, colorpref), colors(4, colorpref)
               IF k <= selecc% THEN
                    LOCATE 5 + l, 2: PRINT USING "###############,³"; CVL(numfact(k)); : PRINT MID$(provee(k), 4, 15); "³ "; MID$(fechafact(k), 3, 2); "/"; MID$(fechafact(k), 1, 2); "/"; MID$(fechafact(k), 5, 4); "³ "; MID$(fechaacc(k), 1, 2); "/";  _
MID$(fechaacc(k), 3, 2); "/"; MID$(fechaacc(k), 5, 4); "³ "; transpor(k);
                    IF MID$(edofact(k), 1, 1) = "*" THEN PRINT " ³Contado ";  ELSE PRINT " ³Credito ";
                    'IF MID$(edofact(k), 2, 1) = "*" THEN PRINT "   SI   ";  ELSE PRINT "   NO   ";
               ELSE
                    LOCATE 5 + l, 2: PRINT SPACE$(78)
               END IF
               IF resal% = l THEN LOCATE 18, 4: PRINT USING "Bruto Bs. ########,.##"; CVD(bruto(k)); : PRINT USING "   Impuesto Bs. ########,.##"; CVD(ivafact(k)); : PRINT USING "   Neto Bs. ########,.##"; CVD(neto(k));
               IF resal% = l THEN COLOR colors(4, colorpref), colors(7, colorpref)
            NEXT k
         LOOP
    salir = false
    'linea$ 26 credito o contado   59 actualizada si/no  62 cancelada si/no
    num2 = STR$(CVL(numfact(eleg))): num2 = Trim$(MID$(num2, 2, 10)): larg% = 0
    FOR k = 1 TO 33: IF MID$(num2, k, 1) <> " " THEN larg% = larg% + 1
    NEXT k
    num2 = "F:\kevinprg\kevindat\" + STRING$(8 - larg%, "0") + MID$(num2, 1, larg%) + "." + MID$(provee(eleg), 1, 3)
    CLOSE #2: OPEN num2 FOR RANDOM AS #2 LEN = 40: FIELD #2, 6 AS odigo$, 2 AS antid$, 8 AS recio$, 8 AS osto$, 8 AS ferta$, 2 AS oferantid$, 1 AS va$, 2 AS sitio$, 3 AS venci$: lleped% = LOF(2) / 40
    CLOSE #1: OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
    FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS  _
cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$ _
, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS Saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
    seguir = true: apu% = 1: Box 4, 1, 6, 80
    LOCATE 4, 2: PRINT "Factura:         Proveedor:       FechaFact:  FechaAcce:  Transcrita: F.Pago: "
    LOCATE 5, 2: PRINT USING "###############,³"; CVL(numfact(eleg)); : PRINT MID$(provee(eleg), 3, 15); "³ "; MID$(fechafact(eleg), 3, 2); "/"; MID$(fechafact(eleg), 1, 2); "/"; MID$(fechafact(eleg), 5, 4); "³ "; MID$(fechaacc(eleg), 1, 2); "/";  _
MID$(fechaacc(eleg), 3, 2); "/"; MID$(fechaacc(eleg), 5, 4); "³ "; transpor(eleg);
    IF MID$(edofact(eleg), 1, 1) = "*" THEN PRINT " ³Contado ";  ELSE PRINT " ³Credito ";
    'IF MID$(edofact(eleg), 2, 1) = "*" THEN PRINT "   SI   ";  ELSE PRINT "   NO   ";
    LOCATE 6, 4: PRINT USING "Bruto Bs. ########,.##"; CVD(bruto(eleg)); : PRINT USING "   Impuesto Bs. ########,.##"; CVD(ivafact(eleg)); : PRINT USING "   Neto Bs. ########,.##"; CVD(neto(eleg));
    LOCATE 7, 1: PRINT "ÄDescripci¢nÄÄÄÄÄÄÄÄÄÄÄÄÄÂ% GanÂCantÄÂOferÄÂÄPVP ÄÄÄÄÄÂdesc.p£bl.ÂÄÄCostoÄÄÄÄÄÂÄ";
    LOCATE 23, 1: PRINT "ÄDescripci¢nÄÄÄÄÄÄÄÄÄÄÄÄÄÁ%ÄGanÁCantÄÁOferÄÁÄPVP ÄÄÄÄÄÁdesc.p£bl.ÁÄÄCostoÄÄÄÄÄÁÄ";
    Center 25, "[Esc]Salir [F9]Imprimir [F10]Salir  -Use las Teclas de Direccion-"

    DO WHILE seguir
         COLOR colors(7, colorpref), colors(4, colorpref): l = 7
         FOR k = apu% - 7 TO apu% + 7
              IF k = apu% THEN COLOR colors(4, colorpref), colors(7, colorpref)
              l = l + 1: LOCATE l, 1: PRINT SPACE$(80); : LOCATE l, 1: porc = MKD$(0)
              IF k > 0 AND k <= lleped% THEN
                   GET #2, k: GET #1, CVI(sitio$): PRINT MID$(nomb2$, 1, 25); "³";
                   IF CVD(osto$) > 0 THEN
                        IF va$ = "I" THEN
                                  porc = MKD$(100 - (((CVD(osto$) / (CVI(antid$)))) / (CVD(recio$) / (VAL(Impuesto) / 100 + 1))) * 100)
                        ELSE
                                  porc = MKD$(100 - (((CVD(osto$) / (CVI(antid$)))) / CVD(recio$)) * 100)
                        END IF
                        IF CVD(porc) < 29.95 THEN COLOR colors(4, colorpref) + 16, colors(7, colorpref)
                        PRINT USING "##.##"; CVD(porc);
                   ELSE
                        COLOR colors(4, colorpref) + 16, colors(7, colorpref): PRINT USING "##.##"; 0;
                   END IF
                   IF k = apu% THEN COLOR colors(4, colorpref), colors(7, colorpref) ELSE COLOR colors(7, colorpref), colors(4, colorpref)
                   PRINT USING "³#####³"; CVI(antid$); : PRINT USING "#####³"; CVI(oferantid$); : PRINT USING "######,.##³"; CVD(recio$); : PRINT USING "######,.##³"; CVD(ferta$); : PRINT USING "########,.##³"; CVD(osto$); : PRINT va$;
              ELSE
                   PRINT SPACE$(25); "³     ³     ³     ³          ³          ³            ³ ";
              END IF
              IF k = apu% THEN COLOR colors(7, colorpref), colors(4, colorpref)
         NEXT k
              GET #2, apu%: GET #1, CVI(sitio$): LOCATE 24, 1: PRINT nomb2$; "  Fecha de Vencimiento:"; : auxiliar$ = ""
              IF venci$ <> "   " THEN
                   IF MID$(venci$, 1, 1) = "A" OR MID$(venci$, 1, 1) = "B" OR MID$(venci$, 1, 1) = "C" THEN
                        IF MID$(venci$, 1, 1) = "A" THEN auxiliar$ = "10/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4)
                        IF MID$(venci$, 1, 1) = "B" THEN auxiliar$ = "11/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4)
                        IF MID$(venci$, 1, 1) = "C" THEN auxiliar$ = "12/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4)
                   ELSE
                        auxiliar$ = "0" + MID$(venci$, 1, 1) + "/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4)
                   END IF
                   PRINT auxiliar$
              ELSE
                   PRINT "  /         ";
              END IF
              kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
              SELECT CASE kbd$
                   CASE CHR$(0) + "H": apu% = apu% - 1: IF apu% < 1 THEN apu% = 1
                   CASE CHR$(0) + "P": apu% = apu% + 1: IF apu% > lleped% THEN apu% = lleped%
                   CASE CHR$(0) + CHR$(81): apu% = apu% + 14: IF apu% > lleped% THEN apu% = lleped%
                   CASE CHR$(0) + CHR$(73): apu% = apu% - 14: IF apu% < 1 THEN apu% = 1
                   CASE CHR$(0) + CHR$(67): Box 10, 10, 12, 70: Center 11, "Imprimiendo , espere por favor ...": GOSUB ImprimeFact
                   CASE CHR$(0) + CHR$(61)
                     kbd$ = "": DO WHILE kbd$ = "": kbd$ = INKEY$: LOOP
                     kbd$ = UCASE$(kbd$)
                     FOR k = (apu% + 1) TO lleped%
                        GET #2, k: GET #1, CVI(sitio$)
                        IF MID$(nomb2$, 1, 1) = kbd$ THEN
                           apu% = k
                           EXIT FOR
                        END IF
                     NEXT k
                   CASE CHR$(0) + CHR$(68): EXIT DO
                   CASE CHR$(27): EXIT DO
                   CASE ELSE:  BEEP
              END SELECT
    LOOP
LOOP
CLOSE
EXIT SUB

Arri:
     lee% = 0: resal% = resal% - 1: IF resal% < 1 THEN resal% = 1: up% = up% - 1: lee% = 1
     IF up% < 1 THEN up% = 1: IF Sonido = "si" THEN BEEP
     IF lee% = 1 THEN GOSUB LeeDiez: GOSUB BuscActivo
RETURN
Abaj:
     lee% = 0: resal% = resal% + 1: IF resal% > selecc% THEN up% = up% + 1: lee% = 2
     IF up% > final% THEN up% = final%: IF Sonido = "si" THEN BEEP
     IF lee% = 2 THEN GOSUB LeeDiez: GOSUB BuscActivo
RETURN
Agup:
     up% = up% - 9: lee% = 1
     IF up% < 1 THEN up% = 1: resal% = 1: IF Sonido = "si" THEN BEEP
     GOSUB LeeDiez: GOSUB BuscActivo
RETURN
Agdn:
     up% = up% + 9: lee% = 1
     IF up% > final% THEN up% = final%: resal% = 1: IF Sonido = "si" THEN BEEP
     GOSUB LeeDiez: GOSUB BuscActivo
RETURN

ImprimeFact:
res% = Verify%(13, "¨Desea imprimir las existencias actuales de cada producto")
ras% = Verify%(11, "¨Desea que se imprima por orden de ubicaci¢n y alfabeticamente")
IF ras% = true THEN
   Center 25, "Espere por favor..."
   COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... ": COLOR colors(7, colorpref), colors(4, colorpref)
   RANDOMIZE TIMER
   ahor$ = STR$(INT(RND * 7000)): ahor$ = MID$(ahor$, 2, LEN(ahor$) - 1)
   ahor$ = "F:\kevinprg\kevindat\" + ahor$ + ".ver"
   CLOSE #17: OPEN ahor$ FOR RANDOM AS #17 LEN = 4: FIELD #17, 4 AS nume$
   FOR k = 1 TO lleped%
      RSET nume$ = MKL$(k): PUT #17, k
   NEXT k
   N = lleped%: Salto = N
   WHILE Salto > 1
      Salto = Salto / 2
      DO
         Sw = 1
         FOR J = 1 TO N - Salto
            I = J + Salto
            GET #17, I: GET #2, CVL(nume$): GET #1, CVI(sitio$): auxx1$ = ubic2$ + nomb2$: ax1$ = nume$
            GET #17, J: GET #2, CVL(nume$): GET #1, CVI(sitio$): auxx2$ = ubic2$ + nomb2$: ax2$ = nume$
            IF auxx1$ < auxx2$ THEN
               ar$ = ax1$: ax1$ = ax2$: ax2$ = ar$
               RSET nume$ = ax1$: PUT #17, I
               RSET nume$ = ax2$: PUT #17, J: Sw = 0
            END IF
         NEXT J
      LOOP UNTIL Sw = 1
   WEND
END IF
CLOSE #10: OPEN "lpt1" FOR RANDOM AS #10: WIDTH #10, 255: l = 0
PRINT #10, CHR$(27); "C"; CHR$(0); CHR$(9); ' largo de 9 pulgadas
'PRINT #10, CHR$(27); "G"; ' double-strike
     FOR k = 1 TO lleped%
               IF l = 0 THEN
                    PRINT #10, "    *** Reporte impreso de una factura de Compra ***": PRINT #10, CHR$(10);
                    PRINT #10, "Factura:         Proveedor:       FechaFact:  FechaAcce:  Transcrita: F.Pago:"; : PRINT #10, CHR$(10);
                    PRINT #10, USING "###############,³"; CVL(numfact(eleg)); : PRINT #10, MID$(provee(eleg), 4, 15); "³ "; MID$(fechafact(eleg), 3, 2); "/"; MID$(fechafact(eleg), 1, 2); "/"; MID$(fechafact(eleg), 5, 4); "³ "; MID$(fechaacc(eleg), 1 _
, 2); "/"; MID$(fechaacc(eleg), 3, 2); "/"; MID$(fechaacc(eleg), 5, 4); "³ "; transpor(eleg);
                    IF MID$(edofact(eleg), 1, 1) = "*" THEN PRINT #10, " ³Contado ";  ELSE PRINT #10, " ³Credito ";
                    'IF MID$(edofact(eleg), 2, 1) = "*" THEN PRINT #10, "   SI   ";  ELSE PRINT #10, "   NO   ";
                    PRINT #10, CHR$(10); : PRINT #10, USING "    Bruto Bs. ########,.##"; CVD(bruto(eleg)); : PRINT #10, USING "   Impuesto Bs. ########,.##"; CVD(ivafact(eleg)); : PRINT #10, USING "   Neto Bs. ########,.##"; CVD(neto(eleg)); : _
                                                     PRINT #10, CHR$(10);
                    PRINT #10, "ÄDescripci¢nÄÄÄÄÄÄÄÄÄÄÄÄÄÂ% GanÂCantÄÂOferÄÂÄPVP ÄÄÄÄÄÂdesc.p£bl.ÂÄÄCostoÄÄÄÄÄÂÄ"; : PRINT #10, CHR$(10);
               END IF
               IF ras% = true THEN
                  GET #17, k: GET #2, CVL(nume$): GET #1, CVI(sitio$): PRINT #10, MID$(nomb2$, 1, 25); "³"; : l = l + 1: porc = MKD$(0)
               ELSE
                  GET #2, k: GET #1, CVI(sitio$): PRINT #10, MID$(nomb2$, 1, 25); "³"; : l = l + 1: porc = MKD$(0)
               END IF
               IF CVD(osto$) > 0 THEN
                    IF va$ = "I" THEN
                              porc = MKD$(100 - (((CVD(osto$) / (CVI(antid$)))) / (CVD(recio$) / (VAL(Impuesto) / 100 + 1))) * 100)
                    ELSE
                              porc = MKD$(100 - (((CVD(osto$) / (CVI(antid$)))) / CVD(recio$)) * 100)
                    END IF
                    PRINT #10, USING "##.##"; CVD(porc);
               ELSE
                    PRINT #10, USING "##.##"; 0;
               END IF
               PRINT #10, USING "³#####³"; CVI(antid$); : PRINT #10, USING "#####³"; CVI(oferantid$); : PRINT #10, USING "######,.##³"; CVD(recio$); : PRINT #10, USING "######,.##³"; CVD(ferta$); : PRINT #10, USING "########,.##³"; CVD(osto$);
               IF res% = true THEN
                  PRINT #10, va$; " Existencias:"; : pr% = true
                  IF CVI(exi12$) > 0 THEN PRINT #10, USING "#####,"; CVI(exi12$); : PRINT #10, " a"; : PRINT #10, USING "########,.##"; CVD(pv12$); : pr% = false
                  IF CVI(exi22$) > 0 THEN PRINT #10, USING "#####,"; CVI(exi22$); : PRINT #10, " a"; : PRINT #10, USING "########,.##"; CVD(pv22$); : pr% = false
                  IF CVI(exi32$) > 0 THEN PRINT #10, USING "#####,"; CVI(exi32$); : PRINT #10, " a"; : PRINT #10, USING "########,.##"; CVD(pv32$); : pr% = false
                  IF pr% = true THEN
                     PRINT #10, " 0";
                  END IF
               END IF
               PRINT #10, CHR$(10);
               IF l > 50 THEN
                    PRINT #10, "ÄDescripci¢nÄÄÄÄÄÄÄÄÄÄÄÄÄÁ%ÄGanÁCantÄÁOferÄÁÄPVP ÄÄÄÄÄÁdesc.p£bl.ÁÄÄCostoÄÄÄÄÄÁÄ";
                    PRINT #10, CHR$(27); "H"; ' without double-strike
                    PRINT #10, CHR$(12); : l = 0
               END IF
     NEXT k
IF l <> 0 THEN
     PRINT #10, "ÄDescripci¢nÄÄÄÄÄÄÄÄÄÄÄÄÄÁ%ÄGanÁCantÄÁOferÄÁÄPVP ÄÄÄÄÄÁdesc.p£bl.ÁÄÄCostoÄÄÄÄÄÁÄ"; CHR$(10);
     PRINT #10, " <<< FIN DEL REPORTE >>> "; CHR$(10);
     PRINT #10, CHR$(27); "H"; ' without double-strike
     PRINT #10, CHR$(12); : l = 0
END IF
CLOSE #10
Center 25, "[Esc]Salir [F9]Imprimir [F10]Salir  -Use las Teclas de Direccion-"
RETURN

LeeDiez:
selecc% = 0
FOR k = up% TO final%
IF k > 0 AND k <= final% THEN
     GET #27, k
     IF MID$(linea$, 1, 1) = " " THEN
          selecc% = selecc% + 1
          numfact(selecc%) = MID$(linea$, 2, 4)
          fechaacc(selecc%) = MID$(linea$, 6, 8)
          transpor(selecc%) = MID$(linea$, 14, 10)
          provee(selecc%) = MID$(linea$, 24, 2)
          edofact(selecc%) = MID$(linea$, 26, 1) + MID$(linea$, 59, 1) + MID$(linea$, 62, 1)
          bruto(selecc%) = MID$(linea$, 27, 8)
          neto(selecc%) = MID$(linea$, 35, 8)
          ivafact(selecc%) = MID$(linea$, 43, 8)
          fechafact(selecc%) = MID$(linea$, 51, 8)
          ubifact(selecc%) = MKI$(k)
          IF selecc% = 10 THEN EXIT FOR
     END IF
END IF
NEXT k
FOR k = 1 TO selecc%: FOR l = 1 TO 199
IF CVI(MID$(provee(k), 1, 2)) = VAL(Code(l)) THEN provee(k) = Code(l) + Contenido(l): l = 199
NEXT l: NEXT k: IF lee% = 2 THEN resal% = selecc%
RETURN

BuscActivo:
encontrado = false: GET #27, up%: IF MID$(linea$, 1, 1) = " " THEN encontrado = true
DO WHILE encontrado = false
     IF lee% = 1 THEN
          up% = up% - 1
          IF up% < 1 THEN
               FOR k = 1 TO final%
                    GET #27, k: IF MID$(linea$, 1, 1) = " " THEN up% = k: EXIT DO
               NEXT k
          END IF
     END IF
     IF lee% = 2 THEN
          up% = up% + 1
          IF up% > final% THEN
               FOR k = final% TO 1 STEP -1
                    GET #27, k: IF MID$(linea$, 1, 1) = " " THEN up% = k: EXIT DO
               NEXT k
          END IF
     END IF
     GET #27, up%: IF MID$(linea$, 1, 1) = " " THEN encontrado = true
LOOP
RETURN

CalculaMontos:
COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box 1, 1, 3, 80
Center 2, "Consulta de una Factura de Compra : calculo de un estado de cuenta"
COLOR colors(5, colorpref), colors(4, colorpref)
LOCATE 4, 1: PRINT " Escoja el Proveedor:²   ²             "
control% = Elije%(5, 5, 22, 4, 23, 0, 1)
COLOR colors(5, colorpref), colors(4, colorpref)
Box 9, 5, 13, 70: LOCATE 10, 12: PRINT "Introduzca desde que fecha :"
Center 24, "Introduzca DESDE que fecha desea el calculo: use el formato dd/mm/aaaa     "
DO
     Center 25, "[Enter]Aceptar [F9]Abortar"
     desde$ = MeteDatoLetra(10, 40, 10, "  /  /    ")
     IF Fun(9) = "si" THEN Fun(9) = "no": RETURN
     IF CheqDate(desde$, 1) = true THEN EXIT DO ELSE Center 25, "INTRODUZCA UNA FECHA VALIDA POR FAVOR": SLEEP 2
LOOP
Center 24, "Introduzca HASTA que fecha desea el calculo: use el formato dd/mm/aaaa     "
LOCATE 11, 12: PRINT "Introduzca hasta que fecha :"
DO
     Center 25, "[Enter]Aceptar [F9]Abortar"
     hasta$ = MeteDatoLetra(11, 40, 10, "  /  /    ")
     IF Fun(9) = "si" THEN Fun(9) = "no": RETURN
     IF CheqDate(hasta$, 1) = true THEN EXIT DO ELSE Center 25, "INTRODUZCA UNA FECHA VALIDA POR FAVOR": SLEEP 2
LOOP
desde1% = CalculaDias(desde$, 1): hasta1% = CalculaDias(hasta$, 1)
IF desde1% > hasta1% THEN Box 9, 5, 13, 70: Center 10, "Desde " + desde$ + " hasta " + hasta$: Center 11, "La fecha final debe ser mayor que la inicial": SLEEP 2
Box 9, 5, 13, 70: Center 11, "Calculando , espere por favor ... "
CLOSE #27: OPEN "F:\kevinprg\kevindat\master.fct" FOR RANDOM AS #27 LEN = 70
FIELD #27, 1 AS acti$, 4 AS numfact$, 8 AS fechaacc$, 10 AS vendedor$, 2 AS prove$, 1 AS contacredi$, 8 AS brut$, 8 AS net$, 8 AS ivafact$, 8 AS fechafact$, 1 AS actual$, 4 AS numped$, 1 AS cancelada$, 6 AS vacio$
final27% = LOF(27) / 70: acumulado = MKD$(0)
ahora$ = "F:\kevinprg\kevindat\" + Trim$(MID$(STR$(TIMER * 100), 2, 10)) + ".ver"
OPEN ahora$ FOR RANDOM AS #5 LEN = 32
FIELD #5, 10 AS repfecha$, 8 AS repneto$, 8 AS repimpuesto$, 4 AS repnumero$, 2 AS reporden$: reporte% = 1
FOR k = 1 TO final27%
     GET #27, k
     IF acti$ = " " THEN
          IF CVI(prove$) = VAL(Code(control%)) THEN
               IF CalculaDias(fechafact$, 2) >= desde1% THEN
                    IF CalculaDias(fechafact$, 2) <= hasta1% THEN
                         acumulado = MKD$(CVD(acumulado) + CVD(net$))
                         RSET reporden$ = MKI$(CalculaDias(fechafact$, 2))
                         RSET repfecha$ = MID$(fechafact$, 3, 2) + "/" + MID$(fechafact$, 1, 2) + "/" + MID$(fechafact$, 5, 4)
                         RSET repnumero$ = numfact$: RSET repneto$ = net$: RSET repimpuesto$ = ivafact$: PUT #5, reporte%: reporte% = reporte% + 1
                    END IF
               END IF
          END IF
     END IF
NEXT k: CLOSE #27: CLOSE #5
Center 24, "                                                                           ": Center 24, "Desde " + desde$ + " hasta " + hasta$
Box 9, 5, 13, 70: LOCATE 11, 20: PRINT " Acumulados :"; : PRINT USING "###########,.##"; CVD(acumulado): Espera
Box 9, 5, 13, 70: Center 11, "¨ Desea un reporte escrito ampliado ? Pulse S para imprimir": Center 25, "Pulse cualquier otra tecla para salir y NO imprimir"
kdb$ = "": DO WHILE kdb$ = "": kdb$ = INKEY$: LOOP: kdb$ = UCASE$(kdb$)
IF kdb$ = "S" THEN
     Box 9, 5, 13, 70: Center 11, "Espere por favor , colocando en cola de impresion..."
     OPEN ahora$ FOR RANDOM AS #5 LEN = 32
     FIELD #5, 26 AS todo$, 4 AS repnumero$, 2 AS reporden$: N = LOF(5) / 32: Salto = N
     DIM au1 AS STRING * 26, au2 AS STRING * 26, au5 AS STRING * 4, au6 AS STRING * 4
     IF N > 0 THEN
          WHILE Salto > 1
               Salto = Salto / 2
               DO
                    Sw = 1
                    FOR J = 1 TO N - Salto
                         I = J + Salto
                         GET #5, J: au1 = todo$: au3% = CVI(reporden$): au5 = repnumero$
                         GET #5, I: au2 = todo$: au4% = CVI(reporden$): au6 = repnumero$
                         IF (au3% + CVL(au5)) > (au4% + CVL(au6)) THEN
                              RSET todo$ = au2: RSET reporden$ = MKI$(au4%): RSET repnumero$ = au6: PUT #5, J
                              RSET todo$ = au1: RSET reporden$ = MKI$(au3%): RSET repnumero$ = au5: PUT #5, I: Sw = 0
                         END IF
                    NEXT J
               LOOP UNTIL Sw = 1
          WEND: CLOSE
          OPEN ahora$ FOR RANDOM AS #5 LEN = 32: saldito$ = MKD$(0): impuestito$ = MKD$(0)
          FIELD #5, 10 AS repfecha$, 8 AS repneto$, 8 AS repimpuesto$, 4 AS repnumero$, 2 AS reporden$
          reporte% = LOF(5) / 32: OPEN "LPT1:" FOR RANDOM AS #10: WIDTH #10, 255: GOSUB Cabeza: van% = 0: GET #5, 1: linadi$ = MID$(repfecha$, 1, 2)
          FOR k = 1 TO reporte%
               GET #5, k: IF linadi$ <> MID$(repfecha$, 1, 2) THEN PRINT #10, "     ³          ³            ³              ³              ³              ³"; CHR$(10); : van% = van% + 1
               saldito$ = MKD$(CVD(saldito$) + CVD(repneto$) + CVD(repimpuesto$)): impuestito$ = MKD$(CVD(impuestito$) + CVD(repimpuesto$))
               PRINT #10, "     ³"; repfecha$; "³"; : linadi$ = MID$(repfecha$, 1, 2)
               PRINT #10, USING "###########,"; CVL(repnumero$); : PRINT #10, "³";
               PRINT #10, USING "##########,.##"; CVD(repneto$); : PRINT #10, "³";
               PRINT #10, USING "##########,.##"; CVD(repimpuesto$); : PRINT #10, "³";
               PRINT #10, USING "##########,.##"; CVD(saldito$);
               PRINT #10, "³"; CHR$(10); : van% = van% + 1
               IF van% > 50 THEN GOSUB Cola: PRINT #10, CHR$(12); : GOSUB Cabeza: van% = 0
          NEXT k
          PRINT #10, "     ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"; CHR$(10);
          PRINT #10, "                             ³"; : PRINT #10, USING "##########,.##"; (CVD(saldito$) - CVD(impuestito$)); : PRINT #10, "³"; : PRINT #10, USING "##########,.##"; CVD(impuestito$); : PRINT #10, "³"; : PRINT #10, USING  _
"##########,.##"; CVD(saldito$); : PRINT #10, "³"; CHR$(10);
          PRINT #10, "                             ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"; CHR$(10);
          PRINT #10, "                              ---<<< Fin del reporte >>>---": PRINT #10, CHR$(12); : CLOSE
     END IF
END IF
RETURN

Cabeza:
     PRINT #10, "  Reporte de facturas de compras  desde " + desde$ + " hasta " + hasta$ + " inclusive."; CHR$(10);
     PRINT #10, "  Proveedor seleccionado:"; Contenido(control%); "            Fecha: "; MID$(DATE$, 4, 2); "/"; MID$(DATE$, 1, 2); "/"; MID$(DATE$, 7, 4); " "; TIME$; CHR$(10);
     PRINT #10, "     ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"; CHR$(10);
     PRINT #10, "     ³  Fecha   ³   Numero   ³    Monto     ³   Impuesto   ³    Saldo     ³"; CHR$(10);
     PRINT #10, "     ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"; CHR$(10);
RETURN

Cola:
     PRINT #10, "     ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"; CHR$(10);
RETURN

ModificaFactura:
fctorig = numfact(eleg): exisfact = true
CALL Center(24, SPACE$(78)): Center 25, "[F9]Salvar y salir [F10]Abortar modificaci¢n"
CALL PopUp(6, 8, 12, 62, 112, 1, 0, 0)
salir = false: sit% = 1: GOSUB DibujaCaja
DO WHILE salir = false
   SELECT CASE sit%
      CASE 1
         aux$ = STR$(CVL(numfact(eleg)))
         aux$ = MID$(aux$, 2, LEN(aux$) - 1)
         LOCATE 1, 1: PRINT aux$
         aux$ = EntraDatoNumero$(10, 40, 8, aux$, 0)
         numfact(eleg) = MKL$(VAL(aux$))
      CASE 2
         aux$ = MID$(fechafact(eleg), 3, 2) + "/" + MID$(fechafact(eleg), 1, 2) + "/" + MID$(fechafact(eleg), 5, 4)
         aux$ = MeteDatoLetra$(11, 40, 10, aux$)
         fechafact(eleg) = MID$(aux$, 4, 2) + MID$(aux$, 1, 2) + MID$(aux$, 7, 4)
      CASE 4
         aux$ = STR$(CVD(bruto(eleg)))
         aux$ = EntraDatoNumero$(12, 40, 16, aux$, 0)
         bruto(eleg) = MKD$(VAL(aux$))
      CASE 5
         aux$ = STR$(CVD(neto(eleg)))
         aux$ = EntraDatoNumero$(13, 40, 16, aux$, 0)
         neto(eleg) = MKD$(VAL(aux$))
      CASE 6
         aux$ = STR$(CVD(ivafact(eleg)))
         aux$ = EntraDatoNumero$(14, 40, 16, aux$, 0)
         ivafact(eleg) = MKD$(VAL(aux$))
   END SELECT
   GOSUB DibujaCaja
   IF devolver = true THEN
      devolver = false: sit% = sit% - 1
      IF sit% < 1 THEN sit% = 6
   ELSE
      devolver = false: sit% = sit% + 1
      IF sit% > 6 THEN sit% = 1
   END IF
   IF Fun(9) = "si" THEN
      IF CVL(fctorig) <> CVL(numfact(eleg)) THEN
         'revisa el n£mero de factura
         num2 = STR$(CVL(numfact(eleg)))
         num2 = Trim$(MID$(num2, 2, 10)): larg% = 0
         FOR k = 1 TO 33
            IF MID$(num2, k, 1) <> " " THEN larg% = larg% + 1
          NEXT k
          num2 = "F:\kevinprg\kevindat\" + STRING$(8 - larg%, "0") + MID$(num2, 1, larg%) + "." + MID$(provee(eleg), 1, 3)
          num1 = STR$(CVL(fctorig)): num1 = Trim$(MID$(num1, 2, 10)): larg% = 0
          FOR k = 1 TO 33
            IF MID$(num1, k, 1) <> " " THEN larg% = larg% + 1
          NEXT k
          num1 = "F:\kevinprg\kevindat\" + STRING$(8 - larg%, "0") + MID$(num1, 1, larg%) + "." + MID$(provee(eleg), 1, 3)
          LOCATE 25, 23: FILES num2
          'OPEN "F:\kevinprg\kevindat\master.fct" FOR RANDOM AS #27 LEN = 70
          'FIELD #27, 1 AS acti$, 4 AS numfact$, 8 AS fechaacc$, 10 AS vendedor$, 2 AS prove$, 1 AS contacredi$, 8 AS brut$, 8 AS net$, 8 AS ivafact$, 8 AS fechafact$, 1 AS actual$, 4 AS numped$, 1 AS cancelada$, 6 AS vacio$
          'IF exisfact = false THEN
          '  NAME num1 AS num2
          'rset linea$=" "+ numfact(eleg)+fechaacc(eleg)
          '+transpor(eleg)+provee(eleg)+mid$(edofact(eleg),1,1)
          '+bruto(eleg)+neto(eleg)+ivafact(eleg)+fechafact(eleg)
          '+
          'numfact(selecc%) = MID$(linea$, 2, 4)
          'fechaacc(selecc%) = MID$(linea$, 6, 8)
          'transpor(selecc%) = MID$(linea$, 14, 10)
          'provee(selecc%) = MID$(linea$, 24, 2)
          'edofact(selecc%) = MID$(linea$, 26, 1) + MID$(linea$, 59, 1) + MID$(linea$, 62, 1)
          'bruto(selecc%) = MID$(linea$, 27, 8)
          'neto(selecc%) = MID$(linea$, 35, 8)
          'ivafact(selecc%) = MID$(linea$, 43, 8)
          'fechafact(selecc%) = MID$(linea$, 51, 8)
          'ELSE
          'END IF
          salir = true: Fun(9) = "no"
      ELSE
         
      END IF
   END IF
   IF Fun(10) = "si" THEN salir = true: Fun(10) = "no"
LOOP
CALL ShutUp
COLOR colors(4, colorpref), colors(7, colorpref)
Center 24, "[F3]Ir a fecha [F4]Suma montos seg£n fecha [F5]Edita [F8]Borra facturas"
Center 25, "Use Las Teclas de Direccion y Enter ; Presione Escape Para Salir "
COLOR colors(4, colorpref), colors(7, colorpref)
RETURN

DibujaCaja:
COLOR colors(4, colorpref), colors(7, colorpref)
LOCATE 10, 10: PRINT "N£mero de factura:"
LOCATE 10, 41: PRINT USING "########"; CVL(numfact(eleg));
LOCATE 11, 10: PRINT "Fecha de factura:"
LOCATE 11, 41: PRINT MID$(fechafact(eleg), 3, 2); "/"; MID$(fechafact(eleg), 1, 2); "/"; MID$(fechafact(eleg), 5, 4)
LOCATE 12, 10: PRINT "Monto bruto:"
LOCATE 12, 41: PRINT USING "############,.##"; CVD(bruto(eleg));
LOCATE 13, 10: PRINT "Monto neto:"
LOCATE 13, 41: PRINT USING "############,.##"; CVD(neto(eleg));
LOCATE 14, 10: PRINT "Monto de impuesto:"
LOCATE 14, 41: PRINT USING "############,.##"; CVD(ivafact(eleg));
RETURN
END SUB

