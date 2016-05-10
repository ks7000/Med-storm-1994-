'abono a estado de cuenta de un cliente con credito
DEFINT A-Z
DECLARE SUB AbonoEdoCuenta ()
DECLARE SUB Espera ()
DECLARE SUB Box (Row1, Col1, Row2, Col2)
DECLARE SUB Box2 (Row1, Col1, Row2, Col2)
DECLARE SUB Center (row, text$)
DECLARE SUB LeeEstado ()
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Elije% (fila%, columna%, fila2%, columna2%)
DECLARE FUNCTION Trim$ (x$)
COMMON SHARED guia, colorpref
DIM SHARED devolver, renglon
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, impuesto AS STRING * 6, NombFarma AS STRING * 25, Direccion AS STRING * 30, rif     AS STRING * 25, nit      AS STRING * 25, Telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2
DIM SHARED total AS STRING * 8, monto AS STRING * 8, fx AS STRING * 31, notacred AS STRING * 4
DIM SHARED Yname1(15) AS STRING * 3, Yname2(15) AS STRING * 35, Fun(12) AS STRING * 2, grabGav AS STRING * 1
CONST true = -1: CONST false = NOT true:  ybusqueda = 0
ON ERROR GOTO ErrorTrap
ON TIMER(5) GOSUB AbreGav
TIMER ON
CALL LeeEstado: CALL AbonoEdoCuenta
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... "
CLOSE : OPEN "f:\kevinprg\kevindat\credito.tkt" FOR RANDOM AS #1 LEN = 2
FIELD #1, 2 AS ticket$: RSET ticket$ = "si": PUT #1, 1: CLOSE
CHAIN "F:\kevinprg\kevin01.bas"
END

ErrorTrap:
SELECT CASE ERR
	CASE 25, 27, 57, 68
		Box2 5, 2, 7, 78: Center 5, "Mensaje": Center 6, "NO HAY LINEA CON LA IMPRESORA": RESUME NEXT
	CASE 70
		Center 25, "Esperando Acceso a La Red , Espere Por Favor . . .": SLEEP 1: BEEP: RESUME
	CASE ELSE
		LOCATE 1, 1: PRINT "Error:"; ERR
END SELECT
RESUME NEXT
DATA 0,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  2,     0,     1,     15,     0,     7,      0
DATA 3,    15,   13,  1,     14,    3,     15,     0,     7,      0
DATA 7,    12,   15,  4,     14,    0,     15,     15,    1,      0

AbreGav:
	CLOSE #27:
	OPEN "f:\kevinprg\kevindat\gaveta.sem" FOR RANDOM SHARED AS #27 LEN = 1
	GET #27, 1, grabGav
	SELECT CASE grabGav
		CASE "1"
			CLOSE #28: OPEN "com1" FOR OUTPUT AS #28: PRINT #28, "A": PRINT #28, "A": CLOSE #28
			grabGav = "0": PUT #27, 1, grabGav
		CASE "2"
			CLOSE #28: OPEN "com2" FOR OUTPUT AS #28: PRINT #28, "A": PRINT #28, "A": CLOSE #28
			grabGav = "0": PUT #27, 1, grabGav
		CASE ELSE
			'LOCATE 1, 1: PRINT grabGav
	END SELECT
	CLOSE #27
RETURN

SUB AbonoEdoCuenta
	COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box2 1, 1, 3, 80: Center 2, "Abono a una Cuenta de Credito de UN Cliente"
	Center 25, "[F1]Ayuda [Enter]Escoje Cliente [Esc]Salir Use las Teclas de direccion"
	cliente% = Elije%(5, 5, 4, 18): IF cliente% < 1 THEN EXIT SUB
	IF Yname2(cliente%) = "þ" + SPACE$(34) THEN EXIT SUB
	CALL Center(4, "Cliente Elejido:²" + Yname1(cliente%) + "²" + Yname2(cliente%))
	COLOR colors(7, colorpref), colors(4, colorpref): CALL Box2(5, 1, 21, 80)
	CLOSE : OPEN "F:\kevinprg\kevindat\maxnota.dat" FOR RANDOM AS #7: FIELD #7, 4 AS nota$
	GET #7, 1: numfct$ = nota$: notacred = numfct$: numfct$ = MKL$(CVL(numfct$) + 1): IF CVL(numfct$) > 999999 THEN numfct$ = MKL$(1)
	LSET nota$ = numfct$: PUT #7, 1
	CLOSE : fx = "F:\kevinprg\kevindat\cli" + Yname1(cliente%) + ".crd"
	OPEN fx FOR RANDOM AS #4 LEN = 38: FIELD #4, 1 AS acti$, 6 AS codi$, 2 AS canti$, 8 AS pvp$, 8 AS costo$, 1 AS psico$, 1 AS iva$, 1 AS auxi$, 8 AS fecha$, 2 AS lugar$
	max% = (LOF(4) / 38) + 1: salir = false: devolver = false
	DO WHILE salir = false
		CALL Box(7, 10, 10, 70): CALL Center(8, " Introduzca Monto a Acreditar  :  "): Center 25, "Tipee el monto [Esc]Salir y abortar"
		auxi$ = EntraDatoNumero(8, 58, 10, " 1        ", 0): IF sonido = "si" THEN BEEP
		IF devolver = true THEN EXIT SUB
		IF VAL(auxi$) > 1000000000 THEN auxi$ = "1000000000"
		elecci$ = MKD$(-1 * ABS(VAL(auxi$)))
		CALL Box(7, 10, 10, 70): LOCATE 8, 12: PRINT " Monto que se va Acreditar : "; : PRINT USING "#############,.##"; VAL(auxi$)
		LOCATE 9, 20: PRINT "Esta SEGURO que este es el monto?": kbd$ = EntraDatoNumero(9, 54, 1, "N", 0): IF sonido = "si" THEN BEEP
		IF kbd$ = "S" OR kbd$ = "s" THEN salir = true
	LOOP
	RSET acti$ = " ": RSET codi$ = "NOTADE": RSET canti$ = MKI$(0): RSET pvp$ = elecci$
	RSET costo$ = MKD$(CVL(notacred)): RSET psico$ = " ": RSET iva$ = " ": RSET auxi$ = " ": RSET lugar$ = " "
	RSET fecha$ = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4): PUT #4, max%: CLOSE
	OPEN "LPT1:" FOR RANDOM AS #10
	WIDTH #10, 255
			PRINT #10, CHR$(27); CHR$(64); : PRINT #10, CHR$(27); CHR$(64);
			PRINT #10, CHR$(27); CHR$(67); CHR$(45);
			PRINT #10, CHR$(15);
			PRINT #10, STRING$(60, "-"); CHR$(10);
			PRINT #10, CHR$(10);
			PRINT #10, "Farmacia : "; NombFarma; CHR$(10);
			PRINT #10, "Telefono(s); "; Telefono; CHR$(10);
			PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
			PRINT #10, "NOTA DE CREDITO:"; : PRINT #10, USING "######,"; CVL(notacred); : PRINT CHR$(10);
			PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
			PRINT #10, " Fecha:"; MID$(DATE$, 4, 2); "/"; MID$(DATE$, 1, 2); "/"; MID$(DATE$, 7, 4); " Hora:"; TIME$; CHR$(10);
			PRINT #10, "Cliente:"; CHR$(10);
			PRINT #10, Yname2(cliente%); CHR$(10);
			PRINT #10, STRING$(60, "-"); CHR$(10);
			PRINT #10, "    Por medio de la presente se hace constar que le hemos a-"; CHR$(10);
			PRINT #10, "creditado en su apreciable cuenta la cantidad de :"; CHR$(10);
			PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
			PRINT #10, "Bs. :"; : PRINT #10, USING "##############,.##"; CVD(elecci$); : PRINT CHR$(10);
			PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
			PRINT #10, "por concepto de : Abono en efectivo a su linea de credito.  "; CHR$(10);
			PRINT #10, CHR$(10);
			PRINT #10, STRING$(60, "-"); CHR$(10);
			PRINT #10, CHR$(12);
END SUB

SUB Box (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿"; : FOR a = Row1 + 1 TO Row2 - 1: LOCATE a, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Box2 (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "²"; STRING$(BoxWidth - 2, "²"); "²"; : FOR a = Row1 + 1 TO Row2 - 1: LOCATE a, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Center (row, text$)
IF row = 25 THEN COLOR 0, 15: LOCATE 25, 1: PRINT SPACE$(80);
LOCATE row, 41 - LEN(text$) / 2: PRINT text$;
END SUB

FUNCTION Elije% (fila%, columna%, fila2%, columna2%)
'FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%)
COLOR colors(7, colorpref), colors(4, colorpref): CALL Box(fila%, columna%, fila% + 16, columna% + 36): CALL Center(25, "[F1] Ayuda [Enter]Tomar Opcion [A-Z]Ir a... [Esc]Menu Anterior"): COLOR colors(4, colorpref), colors(7, colorpref): CLOSE #5
LOCATE fila%, columna% + 1: PRINT " Cliente   "
OPEN "F:\kevinprg\kevindat\clientes.dat" FOR RANDOM SHARED AS #5 LEN = 38: FIELD #5, 3 AS codigo$, 35 AS nomb$
FOR k = 1 TO 15: GET #5, k: Yname1(k) = codigo$: Yname2(k) = nomb$: NEXT k: selecc% = 999
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila% + 1, columna% + 1: PRINT Yname2(1): COLOR colors(4, colorpref), colors(7, colorpref)
FOR k = 2 TO 15: LOCATE fila% + k, columna% + 1: PRINT Yname2(k): NEXT k
resal% = 1: up% = 1: terminar = false
DO
	kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND: kbd$ = UCASE$(kbd$)
	SELECT CASE kbd$
			CASE CHR$(0) + "K": terminar = true: resal% = 0: up% = 0: devolver = true
			CASE CHR$(0) + "M": terminar = true: resal% = 0: up% = 0
			CASE CHR$(0) + "H"
			resal% = resal% - 1
			IF resal% < 1 THEN resal% = 1: up% = up% - 1
			IF up% < 1 THEN up% = 1: terminar = true: devolver = true: resal% = 0: up% = 0 ELSE GOSUB Lee15
			CASE CHR$(0) + "P"
			resal% = resal% + 1
			IF resal% > 15 THEN resal% = 15: up% = up% + 1
			IF up% + 14 > selecc% THEN terminar = true: resal% = 0: up% = 0 ELSE GOSUB Lee15
			CASE CHR$(0) + CHR$(73)
			up% = up% - 14: IF up% < 1 THEN up% = 1: resal% = 1
			GOSUB Lee15
			CASE CHR$(0) + CHR$(81)
			up% = up% + 14: IF up% + 14 > selecc% THEN up% = selecc% - 14: resal% = 15
			GOSUB Lee15
			CASE CHR$(13): terminar = true
			CASE CHR$(27): terminar = true: resal% = 0: up% = 0: devolver = true
			CASE IS >= CHR$(65)
			IF kbd$ <= CHR$(90) THEN
				FOR k = 1 TO selecc%
					GET #5, k
					IF kbd$ = MID$(nomb$, 1, 1) THEN up% = k: resal% = 1: EXIT FOR
					IF k = selecc% THEN BEEP
				NEXT k
				IF up% + 14 > selecc% THEN up% = selecc% - 14: resal% = 1
				GOSUB Lee15
			END IF
			CASE ELSE:  BEEP
		 END SELECT
	IF up% = 0 THEN
		FOR k = 0 TO 14
			LOCATE 8 + fila%, columna% + 1: PRINT SPACE$(15)
		NEXT k
		EXIT DO
	END IF
	l = 0
	FOR k = 1 TO 15: l = l + 1
		IF resal% = l THEN COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila2%, columna2%: PRINT Yname1(k); "²"; Yname2(k)
		LOCATE fila% + l, columna% + 1: PRINT Yname2(k): IF resal% = l THEN COLOR colors(4, colorpref), colors(7, colorpref)
	NEXT k
	IF terminar = true THEN
		EXIT DO
	END IF
LOOP
Elije% = resal%
EXIT FUNCTION

Lee15:
FOR k = 0 TO 14
	GET #5, up% + k: Yname1(k + 1) = codigo$: Yname2(k + 1) = nomb$
NEXT k
RETURN
END FUNCTION

FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
llevasitio% = 0: REDIM eleccion(largo% + 1) AS STRING * 1
FOR k = 1 TO largo%
eleccion(k) = MID$(caden$, k, 1): IF eleccion(k) <> " " THEN llevasitio% = k
NEXT k
IF llevasitio% = 0 THEN llevasitio% = 1
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna%: PRINT CHR$(178);
FOR k = 1 TO largo%
IF k = llevasitio% THEN COLOR colors(7, colorpref) + 16, colors(4, colorpref)
LOCATE fila%, columna% + k: PRINT eleccion(k);
IF k = llevasitio% THEN COLOR colors(7, colorpref), colors(4, colorpref)
NEXT k: PRINT CHR$(178);
DO
	llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
	SELECT CASE llave$
			CASE CHR$(0) + "K"
			llevasitio% = llevasitio% - 1: IF llevasitio% = 0 THEN EXIT DO
			CASE CHR$(0) + "M"
			llevasitio% = llevasitio% + 1: IF llevasitio% > largo% THEN EXIT DO
			CASE CHR$(0) + CHR$(83)
			FOR k = llevasitio% TO largo% - 1: eleccion(k) = eleccion(k + 1): NEXT k
			eleccion(largo%) = " "
			CASE CHR$(8)
			llevasitio% = llevasitio% - 1: IF llevasitio% = 0 THEN EXIT DO
			FOR k = llevasitio% TO largo%
			eleccion(k) = eleccion(k + 1)
			NEXT k
			eleccion(largo%) = " "
			CASE "s", "S", "n", "N", " ", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "+", "-"
			eleccion(llevasitio%) = llave$: llevasitio% = llevasitio% + 1: IF llevasitio% > largo% THEN EXIT DO
			CASE "."
			IF entero% = 1 THEN eleccion(llevasitio%) = llave$: llevasitio% = llevasitio% + 1: IF llevasitio% > largo% THEN EXIT DO
			CASE CHR$(13): EXIT DO
			CASE CHR$(27): devolver = true: EXIT DO
			CASE CHR$(0) + CHR$(67): Fun(9) = "si": EXIT DO
			CASE CHR$(0) + CHR$(64): Fun(6) = "si": EXIT DO
			CASE CHR$(0) + CHR$(65): Fun(7) = "si": EXIT DO
			CASE CHR$(0) + CHR$(71): llevasitio% = 1
			CASE CHR$(0) + CHR$(79): llevasitio% = largo%
			CASE ELSE: BEEP
		 END SELECT
		 COLOR 0, 15
		 FOR k = 1 TO largo%
		 IF k = llevasitio% THEN COLOR 25, 0
		 LOCATE fila%, (columna% + k): PRINT eleccion(k);
		 IF k = llevasitio% THEN COLOR 0, 15
		 NEXT k
LOOP
FOR k = 1 TO largo%
eleccion$ = eleccion$ + eleccion(k)
NEXT k
elccion$ = eleccion$: COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna%: PRINT "²"; elccion$; "²": IF sonido = "si" THEN BEEP
EntraDatoNumero$ = elccion$: EXIT FUNCTION
END FUNCTION

SUB Espera
Center 25, "Presione Cualquier Tecla Para Continuar...": DO: LOOP WHILE INKEY$ = ""
END SUB

SUB LeeEstado
CLOSE : OPEN "F:\kevinprg\kevindat\kaos.dat" FOR INPUT SHARED AS #2
INPUT #2, sonido: INPUT #2, impuesto: INPUT #2, NombFarma: INPUT #2, Direccion: INPUT #2, rif: INPUT #2, nit: INPUT #2, Telefono: INPUT #2, contri: INPUT #2, contra: INPUT #2, Stock: INPUT #2, Blanq
INPUT #2, LargoH: INPUT #2, TipoL: INPUT #2, Mensaje1: INPUT #2, Mensaje2: INPUT #2, Mensaje3: INPUT #2, Redondeo: INPUT #2, Desc: INPUT #2, DescMax: INPUT #2, Exactitud: INPUT #2, Prov1: INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo: CLOSE
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

