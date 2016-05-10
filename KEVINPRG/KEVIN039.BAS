'     imprime solicitudes de pedido
DEFINT A-Z
DECLARE SUB Espera ()
DECLARE SUB LeeEstado ()
DECLARE SUB Center (row, text$)
DECLARE SUB Box (Row1%, Col1%, Row2%, Col%)
DECLARE SUB BorraBox (Row1%, Col1%, Row2%, Col%)
DECLARE SUB Termometro (va%, total%, edo%, text$, fila%, columna%)
DECLARE SUB ImprimePedido ()
DECLARE FUNCTION CalculaMeses% (caden$)
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$)
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Escoge% (fila%, edo%, fila2%, Prod$)
DECLARE FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%, est%)
DECLARE FUNCTION CheqDate (caden$, op%)
DECLARE FUNCTION Trim$ (x$)
DECLARE FUNCTION Verify (cs%, ct$)
COMMON SHARED guia, colorpref
DIM SHARED turno AS STRING * 1, devolver, neto AS STRING * 8, bruto AS STRING * 8, iva AS STRING * 8, renglones AS STRING * 2, unidades AS STRING * 4, razon AS STRING * 8, oldbruto AS STRING * 8
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, ct): NEXT x: NEXT ct
DIM SHARED sonido  AS STRING * 2, Impuesto AS STRING * 6, NombFarma AS STRING * 25, Direccion AS STRING * 30, Rif     AS STRING * 25, Nit      AS STRING * 25, Telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2
DIM SHARED Fun(17) AS STRING * 2, salir, Turna, terminar, sitio, busqueda, cspedido
DIM SHARED Contenido(199) AS STRING * 15, Code(199) AS STRING * 3, porc AS STRING * 8
DIM SHARED Nombra(5) AS STRING * 35, Pvp1a(5) AS STRING * 8, Pvp2a(5) AS STRING * 8, Pvp3a(5) AS STRING * 8, Pvp4a(5) AS STRING * 8, Numa(5) AS STRING * 2, Observa(5) AS STRING * 20, Exis1a(5) AS STRING * 2, Exis2a(5) AS STRING * 2, Exis3a(5) AS  _
STRING * 2, Exis4a(5) AS STRING * 2
DIM SHARED Cant1a(5) AS STRING * 2, Cant2a(5) AS STRING * 2, Cant3a(5) AS STRING * 2, Cant4a(5) AS STRING * 2, grabGav AS STRING * 1
CONST true = -1: CONST false = NOT true: busqueda = 0
ON ERROR GOTO ErrorTrap
ON TIMER(5) GOSUB AbreGav
TIMER ON
CALL LeeEstado: CALL ImprimePedido
CLOSE : OPEN "f:\kevinprg\kevindat\compras.tkt" FOR RANDOM AS #1 LEN = 2: FIELD #1, 2 AS ticket$
GET #1, 1: RSET ticket$ = "si": PUT #1, 1: CLOSE
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... ": COLOR colors(7, colorpref), colors(4, colorpref)
CHAIN "F:\kevinprg\kevin01.bas"
END
ErrorTrap:
    SELECT CASE ERR
        CASE 53 'no se encontro archivo ...
            RESUME NEXT
        CASE 70
            Center 25, " Esperando Acceso a La Red   ...": SLEEP 1: Center 25, SPACE$(35): RESUME
        CASE 71, 76
          Box 10, 10, 20, 70: Center 15, "Introduzca un diskette en el drive A": Espera
          Center 15, " Abriendo acceso al Diskette , Espere ...       ": RESUME
        CASE ELSE
            CLS : Box 10, 10, 20, 70: Center 13, "Error Inesperado Numero:" + STR$(ERR) + " (Anote Este Numero)": Espera
            Center 14, "DESCONECTANDOSE DE LA RED ...": LOCATE 21, 1
            SYSTEM
    END SELECT
    RESUME NEXT
END
DATA 0,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  2,     0,     1,     15,     0,     7,      0
DATA 3,    15,   13,  1,     14,    3,     15,     0,     7,      0
DATA 7,    12,   15,  4,     14,    0,     15,     15,    1,      0


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

SUB BorraBox (Row1%, Col1%, Row2%, Col2%) STATIC
BoxWidth = Col2% - Col1% + 1: FOR a = Row1% TO Row2%: LOCATE a, Col1%: PRINT SPACE$(BoxWidth); : NEXT a
END SUB

SUB Box (Row1%, Col1%, Row2%, Col2%) STATIC
BoxWidth = Col2% - Col1% + 1: LOCATE Row1%, Col1%: PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿"; : FOR a = Row1% + 1 TO Row2% - 1: LOCATE a, Col1%: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2%, Col1%: PRINT "À"; STRING$(BoxWidth - 2,  _
"Ä"); "Ù";
END SUB

FUNCTION CalculaMeses% (caden$)
caden$ = UCASE$(caden$)
IF MID$(caden$, 1, 1) = " " THEN CalculaMeses% = 32500: EXIT FUNCTION
IF MID$(caden$, 1, 1) = "A" OR MID$(caden$, 1, 1) = "B" OR MID$(caden$, 1, 1) = "B" THEN
     IF MID$(caden$, 1, 1) = "A" THEN CalculaMeses% = 10 + 12 * (CVI(MID$(caden$, 2, 2)) - 1990): EXIT FUNCTION
     IF MID$(caden$, 1, 1) = "B" THEN CalculaMeses% = 11 + 12 * (CVI(MID$(caden$, 2, 2)) - 1990): EXIT FUNCTION
     IF MID$(caden$, 1, 1) = "C" THEN CalculaMeses% = 12 + 12 * (CVI(MID$(caden$, 2, 2)) - 1990): EXIT FUNCTION
ELSE
     CalculaMeses% = VAL(MID$(caden$, 1, 1)) + 12 * (CVI(MID$(caden$, 2, 2)) - 1990): EXIT FUNCTION
END IF
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
Center 25, "[F1]Ayuda  [F5]Editar o Adicionar Linea  [Enter]Tomar Opcion"
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
               CASE CHR$(0) + CHR$(133): Fun(11) = "si": EXIT DO
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
          CASE IS >= CHR$(65)
               IF kbd$ <= CHR$(90) AND edo% = 1 THEN
                    Prod$ = kbd$: OPEN "F:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
                    FIELD #2, 35 AS nomshort$, 2 AS num$: max2% = LOF(2) / 37: GOSUB Parche1: GOSUB Parche2
               END IF
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

SUB ImprimePedido
COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box 1, 1, 3, 80
Center 2, "Imprimir UNA solicitud de pedido."
OPEN "F:\kevinprg\kevindat\master.pdd" FOR RANDOM SHARED AS #7 LEN = 67: salir = false: cspedido = false
FIELD #7, 1 AS viva$, 4 AS numped$, 2 AS numprov$, 5 AS hora$, 10 AS fecha$, 4 AS cedven$, 40 AS obse$, 1 AS tipo$
selecc% = (LOF(7) / 67)
IF selecc% < 2 THEN
          Box 10, 10, 20, 70: Center 12, "No Hay Solicitudes de Pedido Pendientes!": Espera: EXIT SUB
END IF
otrsel% = 1
FOR k = 2 TO selecc%: GET #7, k
     IF viva$ = " " THEN otrsel% = otrsel% + 1
NEXT k: SWAP selecc%, otrsel%: l = 1
DIM pedido(selecc%) AS STRING * 4, provee(selecc%) AS STRING * 18, tiempo(selecc%) AS STRING * 16, pedven(selecc%) AS STRING * 24, ojo(selecc%) AS STRING * 40, tipe(selecc%) AS STRING * 1
FOR k = 2 TO selecc%: GET #7, k
     IF viva$ = " " THEN
          pedido(l) = numped$: provee(l) = numprov$: tiempo(l) = hora$ + " " + MID$(fecha$, 4, 2) + "/" + MID$(fecha$, 1, 2) + "/" + MID$(fecha$, 7, 4)
          pedven(l) = cedven$: ojo(l) = obse$: tipe(l) = tipo$: l = l + 1
     END IF
NEXT k: fila% = 6: selecc% = selecc% - 1
OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$
FOR k = 1 TO 199: GET #2, k: Code(k) = codigo$: Contenido(k) = descripcion$: NEXT k: CLOSE #2
FOR k = 1 TO selecc%: FOR l = 1 TO 199
 IF CVI(MID$(provee(k), 1, 3)) = VAL(Code(l)) THEN provee(k) = Code(l) + Contenido(l): l = 199
NEXT l: NEXT k
OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #1 LEN = 100
FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
nursaler = LOF(1) / 100
FOR k = 1 TO selecc%: FOR l = 1 TO nursaler
GET #1, l: IF MID$(pedven(k), 1, 4) = cedu$ THEN MID$(pedven(k), 5, 20) = nomb$: l = nursaler
NEXT l: NEXT k: CLOSE #1
COLOR colors(7, colorpref), colors(4, colorpref): Box 4, 5, 16, 68: Center 4, "Pedidos Registrados": LOCATE 5, 6: PRINT "#Pedido: Proveedor:      Hora y Fecha:    Pedido Por:": Box 19, 5, 22, 75
Center 25, "Use Las Teclas de Direccion y Enter ; Presione Escape Para Salir": COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE fila%, 6: PRINT USING "#######,³"; CVL(pedido(1)); : PRINT MID$(provee(1), 4, 15); "³"; tiempo(1); "³";
PRINT MID$(pedven(1), 5, 20): LOCATE 20, 20: PRINT ojo(1);
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
     DO
          kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
          SELECT CASE kbd$
               CASE CHR$(0) + "H": GOSUB Arri
               CASE CHR$(0) + "P": GOSUB Abaj
               CASE CHR$(0) + CHR$(73): IF selecc% > 10 THEN GOSUB Agup ELSE BEEP
               CASE CHR$(0) + CHR$(81): IF selecc% > 10 THEN GOSUB Agdn ELSE BEEP
               CASE CHR$(13)
                    eleg% = (up% + resal% - 1): IF eleg% > 0 THEN cspedido = true
                    EXIT DO
               CASE CHR$(27): resal% = 0: up% = 1: EXIT SUB
               CASE ELSE:  BEEP
          END SELECT
     l = 0
     FOR k = up% TO up% + 9: l = l + 1   ': LOCATE 1, 1: PRINT "k="; k, "up%="; up%, "l="; l, "resal%="; resal%, "selecc%="; selecc%: SLEEP 1
     IF resal% = l THEN
          COLOR colors(7, colorpref), colors(4, colorpref): LOCATE 20, 20: PRINT ojo(k); : LOCATE 21, 25
          IF tipe(k) = "1" THEN PRINT "***  Pedido de Medicinas   ***"
          IF tipe(k) = "2" THEN PRINT "***  Pedido de Miscelaneas ***"
          IF tipe(k) = "3" THEN PRINT "***  Pedido de Quimicos    ***"
     END IF
          IF k <= selecc% THEN
               LOCATE fila% + l - 1, 6: PRINT USING "#######,³"; CVL(pedido(k)); : PRINT MID$(provee(k), 4, 15); "³"; tiempo(k); "³";
               PRINT MID$(pedven(k), 5, 20);
          ELSE
               LOCATE fila% + l - 1, 6: PRINT SPACE$(62)
          END IF
     IF resal% = l THEN COLOR colors(4, colorpref), colors(7, colorpref)
     NEXT k
     LOOP
     IF cspedido = true THEN
        res% = Verify(13, "¨Desea imprimir los fabricantes de cada producto")
        res2% = Verify(13, "¨Desea imprimir nuestros c¢digos de productos")
        salir = false: cspedido = false
        CLOSE #1: OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
        FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS  _
cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
        num3$ = STR$(CVL(pedido(eleg%))): fil$ = "c:\pedido.fll"
        num3$ = Trim$(MID$(num3$, 2, 8)): num3$ = "F:\kevinprg\kevindat\" + STRING$(8 - LEN(num3$), "0") + num3$ + ".pdd"
        CLOSE #3: OPEN num3$ FOR RANDOM AS #3 LEN = 19: FIELD #3, 6 AS codi7$, 2 AS cant7$, 8 AS pvp37$, 1 AS paf7$, 2 AS sit7$: maxped% = LOF(3) / 19
        CLOSE #7: OPEN "f:\kevinprg\kevindat\ordlab.dat" FOR RANDOM SHARED AS #7 LEN = 15
        FIELD #7, 15 AS labo$
        Center 13, "Imprimiendo..."
        res3% = Verify(13, "¨Desea 'imprimirlo' en un diskette")
        IF res3% = true THEN
            CLOSE #10: OPEN fil$ FOR OUTPUT AS #10: WIDTH #10, 255: l = 0
        ELSE
            CLOSE #10: OPEN "lpt1" FOR RANDOM AS #10: WIDTH #10, 255: l = 0
        END IF
        DIM totalpvp AS DOUBLE
        totalpvp = 0
        IF res3% = false THEN
         PRINT #10, CHR$(27); "C"; CHR$(0); CHR$(9); ' largo de 9 pulgadas
         'PRINT #10, CHR$(27); "G"; ' double-strike
        END IF
        FOR k = 1 TO maxped%
             GET #3, k: GET #1, CVI(sit7$)
                  IF l = 0 THEN
                       PRINT #10, "    *** Reporte impreso de una solicitud de pedido  ***": PRINT #10, CHR$(10);
                       PRINT #10, "Pedido: *Proveedor:    *Hora:*Fecha:    *Vendedor:"; : PRINT #10, CHR$(10);
                       PRINT #10, USING "#######,*"; CVL(pedido(eleg%));
                       PRINT #10, MID$(provee(eleg%), 4, 15); "*"; tiempo(eleg%); "*"; : PRINT #10, MID$(pedven(eleg%), 5, 20); : PRINT #10, CHR$(10);
                       IF res2% = true THEN PRINT #10, "Codigo*";
                       PRINT #10, "Descripcion*************************Cant*******PVP*********"; : PRINT #10, CHR$(10);
                   END IF
                   IF res2% = true THEN PRINT #10, codi7$; " ";
                   PRINT #10, nomb2$;
                   PRINT #10, USING " ########,"; CVI(cant7$);
                   PRINT #10, USING " #########,.##"; CVD(pvp37$);
                   IF res% = true THEN
                       IF res3% = true THEN PRINT #10, CHR$(10);
                       GET #7, CVI(labo2$): PRINT #10, " "; labo$;
                   END IF
                   PRINT #10, CHR$(10); : l = l + 1
                   totalpvp = totalpvp + CVI(cant7$) * CVD(pvp37$)
                   IF l > 50 THEN
                        IF res2% = true THEN PRINT #10, "Codigo*";
                        PRINT #10, "Descripcion*************************Cant*******PVP*********"; : PRINT #10, CHR$(10);
                        PRINT #10, CHR$(12); : l = 0
                   END IF
        NEXT k
        IF res2% = true THEN PRINT #10, "Codigo*";
        PRINT #10, "Descripcion*************************Cant*******PVP*********"; : PRINT #10, CHR$(10);
        PRINT #10, "Total (cantidad por precio de venta al publico) ----->>>";
        IF res3% = true THEN PRINT #10, CHR$(10);
        PRINT #10, USING "  ###############,.##"; totalpvp
        PRINT #10, "Total neto (total menos 30% de descuento normal)----->>>";
        IF res3% = true THEN PRINT #10, CHR$(10);
        PRINT #10, USING "  ###############,.##"; totalpvp * .7
        IF res3% = false THEN PRINT #10, CHR$(12); : l = 0
        Center 13, "              "
   END IF
LOOP
EXIT SUB

Arri:
     resal% = resal% - 1
     IF selecc% < 10 THEN
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

