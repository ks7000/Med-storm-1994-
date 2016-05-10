'modifica datos de clientes a credito
DEFINT A-Z
DECLARE SUB ModificaCliente ()
DECLARE SUB Espera ()
DECLARE SUB Box (Row1, Col1, Row2, Col2)
DECLARE SUB Box2 (Row1, Col1, Row2, Col2)
DECLARE SUB Center (row, text$)
DECLARE SUB FancyCls (dots%, Background%)
DECLARE SUB LeeEstado ()
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$, tr$)
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Elije% (fila%, columna%, fila2%, columna2%)
DECLARE FUNCTION Trim$ (x$)
COMMON SHARED guia, colorpref
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER
RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, impuesto AS STRING * 6, Nombfarma AS STRING * 25, Direccion AS STRING * 30, rif     AS STRING * 25, nit      AS STRING * 25, telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2
DIM SHARED PrintErr AS INTEGER, devolver AS INTEGER, Fun(12) AS STRING * 2, salir AS INTEGER
DIM SHARED Yname1(15) AS STRING * 3, Yname2(15) AS STRING * 35, sitio AS INTEGER, fx AS STRING * 31, grabGav AS STRING * 1
CONST true = -1: CONST false = NOT true: guia = 1
REM ON ERROR GOTO ErrorTrap
ON TIMER(5) GOSUB AbreGav
TIMER ON
CALL LeeEstado: CALL ModificaCliente
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... "
CLOSE : OPEN "f:\kevinprg\kevindat\credito.tkt" FOR RANDOM AS #1 LEN = 2
FIELD #1, 2 AS ticket$: RSET ticket$ = "si": PUT #1, 1: CLOSE
CHAIN "F:\kevinprg\kevin01.bas"
END

ErrorTrap:
SELECT CASE ERR
	CASE 25, 27, 57, 68
		Box2 5, 2, 7, 78: Center 5, "Mensaje": Center 6, "No Se Detect¢ Gavetero;NO DEBE Usar Esta Estacion Para Ventas de Contado": RESUME NEXT
	CASE 70
		Center 25, "Esperando Acceso a La Red , Espere Por Favor . . .": SLEEP 1: BEEP: RESUME
	CASE ELSE
		Center 25, STR$(ERR)
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
COLOR colors(7, colorpref), colors(4, colorpref): CALL Box(fila%, columna%, fila% + 16, columna% + 36): CALL Center(25, "[F5]Edita/adiciona [F8]Borra [Enter]Tomar Opcion [A-Z]Ir a... [Esc]Salir"): COLOR colors(4, colorpref), colors(7, colorpref):  _
CLOSE #5
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
			CASE CHR$(0) + CHR$(63): Fun(5) = "si": sitio = resal%: EXIT DO
			CASE CHR$(0) + CHR$(66): Fun(8) = "si": sitio = resal%: EXIT DO
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
			CASE CHR$(0) + "H": devolver = true: EXIT DO
			CASE CHR$(0) + "P": EXIT DO
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
			CASE " ", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "%", "+", "-"
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
IF eleccion(k) = "%" THEN eleccion(k) = " ": ojo$ = "si"
eleccion$ = eleccion$ + eleccion(k)
NEXT k
elccion$ = eleccion$: IF ojo$ = "si" THEN ojo$ = "": elccion$ = MID$(elccion$, 2, largo% - 1) + "%"
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna%: PRINT "²"; elccion$; "²": IF sonido = "si" THEN BEEP
EntraDatoNumero$ = elccion$: EXIT FUNCTION
END FUNCTION

SUB Espera
Center 25, "Presione Cualquier Tecla Para Continuar...": DO: LOOP WHILE INKEY$ = ""
END SUB

SUB LeeEstado
CLOSE : OPEN "F:\kevinprg\kevindat\kaos.dat" FOR INPUT SHARED AS #2
INPUT #2, sonido: INPUT #2, impuesto: INPUT #2, Nombfarma: INPUT #2, Direccion: INPUT #2, rif: INPUT #2, nit: INPUT #2, telefono: INPUT #2, contri: INPUT #2, contra: INPUT #2, Stock: INPUT #2, Blanq
INPUT #2, LargoH: INPUT #2, TipoL: INPUT #2, Mensaje1: INPUT #2, Mensaje2: INPUT #2, Mensaje3: INPUT #2, Redondeo: INPUT #2, Desc: INPUT #2, DescMax: INPUT #2, Exactitud: INPUT #2, Prov1: INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo: CLOSE
END SUB

FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$, tr$)
IF tr$ = "1" THEN llevasitio% = 2 ELSE llevasitio% = 1
LOCATE fila%, columna%: PRINT CHR$(178) + caden$: LOCATE fila%, columna% + largo% + 1: PRINT CHR$(178): REDIM eleccion(0 TO largo% + 1) AS STRING * 1
FOR k = 1 TO largo%: eleccion(k) = MID$(caden$, k, 1): NEXT k: LOCATE fila%, (columna% + llevasitio%): COLOR 31, 0: IF eleccion(1 + VAL(tr$)) = " " THEN PRINT "_";  ELSE PRINT eleccion(1 + VAL(tr$))
DO
	 llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
		SELECT CASE llave$
			CASE CHR$(0) + "H": devolver = true: EXIT DO
			CASE CHR$(0) + "P": EXIT DO
			CASE CHR$(0) + "K"
				 llevasitio% = llevasitio% - 1: IF llevasitio% = 0 THEN GOSUB Sumar: EXIT DO
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
			CASE CHR$(27): salida = true: devolver = true: EXIT DO
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
				 FOR k = largo% TO llevasitio% + 1 STEP -1: eleccion(k) = eleccion(k - 1): NEXT k
				 eleccion(llevasitio%) = llave$: llevasitio% = llevasitio% + 1
				 IF llevasitio% > largo% THEN GOSUB Sumar: EXIT DO
		END SELECT
		FOR k = 1 TO largo%
		IF k = llevasitio% THEN COLOR 31, 0
		LOCATE fila%, (columna% + k)
		IF k = llevasitio% THEN
				IF eleccion(k) <> " " THEN PRINT eleccion(k);  ELSE PRINT "_";
		ELSE
				PRINT eleccion(k);
		END IF
		IF k = llevasitio% THEN COLOR colors(5, colorpref), colors(4, colorpref)
		NEXT k
LOOP
eleccion$ = "": FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna% + 1: PRINT eleccion$
MeteDatoLetra$ = eleccion$: IF sonido = "si" THEN BEEP
EXIT FUNCTION
Sumar:
FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR 0, 15: LOCATE fila%, columna%: PRINT "²"; eleccion$; "²"
RETURN
END FUNCTION

SUB ModificaCliente
DIM ao1 AS STRING * 35, ao2 AS STRING * 35, ao3 AS STRING * 3, ao4 AS STRING * 3, cuando AS STRING * 10
DO WHILE guia = 1
COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box2 2, 2, 24, 79: Center 2, "Modificacion/Adicion de Clientes"
Center 25, "[F1]Ayuda [F5]Edita/Agrega Nombre [F8]Borra [Enter]Edita Ficha [Esc]Salir"
mjfox% = Elije%(5, 5, 4, 18): IF mjfox% < 1 THEN EXIT DO
IF Fun(8) = "si" THEN
	Box 10, 5, 20, 75: Center 13, " * * * ATENCION * * *": Center 15, "¨ Esta realmente seguro de BORRAR a este cliente ? Pulse -S- o -N-"
	Center 17, Yname1(mjfox%) + "-" + Yname2(mjfox%)
	llave$ = "": DO WHILE llave$ = "": llave$ = INKEY$: LOOP: llave$ = UCASE$(llave$)
	IF llave$ = "S" THEN
		fx = "F:\kevinprg\kevindat\cli" + Yname1(mjfox%) + ".crd"
		OPEN fx FOR RANDOM AS #4 LEN = 38: FIELD #4, 1 AS acti$, 6 AS codi$, 2 AS canti$, 8 AS pvp$, 8 AS costo$, 1 AS psico$, 1 AS iva$, 1 AS auxi$, 8 AS fecha$, 2 AS lugar$
		max% = LOF(4) / 38: total$ = MKD$(0)
		IF max% > 0 THEN
			FOR k = 1 TO max%: GET #4, k
				IF acti$ = " " THEN total$ = MKD$(CVD(total$) + CVI(canti$) * CVD(pvp$))
			NEXT k
		END IF
		IF CVD(total$) = 0 THEN
			CLOSE #4: KILL fx: fx = "F:\kevinprg\kevindat\cli" + Yname1(mjfox%) + ".htr"
			OPEN fx FOR RANDOM AS #4 LEN = 28: FIELD #4, 1 AS acti$, 6 AS codi$, 2 AS canti$, 8 AS pvp$, 8 AS costo$, 1 AS psico$, 1 AS iva$, 1 AS auxi$: CLOSE #4: KILL fx
			LOCATE sitio + 5, 6: PRINT "þ" + SPACE$(34): Fun(8) = "no": Yname2(mjfox%) = "þ" + SPACE$(34): donde% = mjfox%: GOSUB SortYname: CLOSE #5
			OPEN "f:\kevinprg\kevindat\fichacli.ent" FOR RANDOM SHARED AS #5 LEN = 326
			FIELD #5, 20 AS rif5$, 20 AS nit5$, 2 AS diacred5$, 4 AS monmax5$, 60 AS direc15$, 60 AS direc25$, 20 AS tele5$, 1 AS edo15$, 2 AS dia15$, 1 AS edo25$, 2 AS dia25$, 1 AS edo35$, 2 AS dia35$, 1 AS edo45$, 2 AS dia45$, 30 AS autor15$, 30 AS  _
autor25$, 30 AS autor35$, 30 AS autor45$, 8 AS desde$
			LSET rif5$ = SPACE$(20): LSET nit5$ = SPACE$(20)
			LSET diacred5$ = MKI$(0): LSET monmax5$ = MKL$(0)
			LSET direc15$ = SPACE$(60): LSET direc25$ = SPACE$(60)
			LSET tele5$ = SPACE$(20): LSET edo15$ = " "
			LSET dia15$ = MKI$(0): LSET edo25$ = " "
			LSET dia25$ = MKI$(0): LSET edo35$ = " "
			LSET dia35$ = MKI$(0): LSET edo45$ = " "
			LSET dia45$ = MKI$(0): LSET autor15$ = SPACE$(30)
			LSET autor25$ = SPACE$(30): LSET autor35$ = SPACE$(30)
			LSET autor45$ = SPACE$(30): LSET desde5$ = SPACE$(8)
			PUT #5, VAL(Yname1(mjfox%)): CLOSE : EXIT SUB
		ELSE
			Box 10, 5, 20, 75: Center 13, " * * * ATENCION * * *": Center 15, "Este Cliente NO se puede borrar , ya que tiene un saldo de:": LOCATE 16, 35: PRINT USING " Bs ##############,.##"; CVD(total$): Espera: CLOSE : EXIT SUB
		END IF
	ELSE
		CLOSE : EXIT SUB
	END IF
END IF
IF Fun(5) = "si" THEN
		IF MID$(Yname2(mjfox%), 1, 1) = "þ" THEN cuando = DATE$
		Yname2(mjfox%) = MeteDatoLetra(sitio + 5, 5, 35, Yname2(mjfox%), "0")
		'fox% = VAL(Yname1(mjfox%))
		donde% = mjfox%
		GOSUB SortYname
		'CLOSE #5
		'OPEN "F:\kevinprg\kevindat\clientes.dat" FOR RANDOM AS #5 LEN = 38
		'FIELD #5, 3 AS codigo$, 35 AS nomb$
		'Fun(5) = "no"
		'FOR k = 1 TO 999
		'      GET #5, k
		'      IF VAL(codigo$) = fox% THEN mjfox% = k
		'NEXT k: CLOSE
END IF
IF Yname2(mjfox%) = "þ" + SPACE$(34) THEN EXIT SUB
CLOSE #5: OPEN "f:\kevinprg\kevindat\fichacli.ent" FOR RANDOM SHARED AS #5 LEN = 326
FIELD #5, 20 AS rif5$, 20 AS nit5$, 2 AS diacred5$, 4 AS monmax5$, 60 AS direc15$, 60 AS direc25$, 20 AS tele5$, 1 AS edo15$, 2 AS dia15$, 1 AS edo25$, 2 AS dia25$, 1 AS edo35$, 2 AS dia35$, 1 AS edo45$, 2 AS dia45$, 30 AS autor15$, 30 AS autor25$,  _
30 AS autor35$, 30 AS autor45$, 8 AS desde$
GET #5, VAL(Yname1(mjfox%)): sit% = 1: COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box2 1, 1, 24, 80: Center 1, "Modificando al Cliente " + Yname2(mjfox%)  ': salir = false
IF desde$ = SPACE$(8) THEN RSET desde$ = MID$(cuando, 4, 2) + MID$(cuando, 1, 2) + MID$(cuando, 7, 4)
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE 3, 2: PRINT "R.I.F. ¢ C.I.:²"; rif5$; "²": LOCATE 3, 50: PRINT "Cliente desde "; MID$(desde$, 1, 2); "/"; MID$(desde$, 3, 2); "/"; MID$(desde$, 5, 4);
LOCATE 5, 2: PRINT "N.I.T.       :²"; nit5$; "²"
LOCATE 7, 2: PRINT "D¡as Credito :"; : PRINT USING "²###"; CVI(diacred5$); : PRINT "²"
LOCATE 9, 2: PRINT "Monto Credito:"; : PRINT USING "²###############,"; CVL(monmax5$); : PRINT "²"
LOCATE 11, 2: PRINT "Direcci¢n    :²"; direc15$; "²": LOCATE 12, 16: PRINT "²"; direc25$; "²"
LOCATE 13, 2: PRINT "Telefonos    :²"; tele5$; "²"
LOCATE 14, 2: PRINT "D¡as de cobro de cada mes": LOCATE 15, 16: PRINT USING "²###"; CVI(dia15$); : PRINT "²"
LOCATE 15, 26: PRINT USING "²###"; CVI(dia25$); : PRINT "²": LOCATE 14, 46: PRINT "% Descuento"
LOCATE 15, 36: PRINT USING "²###"; CVI(dia35$); : PRINT "²": Center 25, "[F1]Ayuda [F9]Salir y Salvar - Use las Teclas de Direccion y Enter"
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE 15, 46: PRINT USING "²###"; CVI(dia45$); : PRINT "²": salir = false
LOCATE 17, 2: PRINT "Autorizado #1:²"; autor15$; "²": LOCATE 19, 2: PRINT "Autorizado #2:²"; autor25$; "²"
LOCATE 21, 2: PRINT "Autorizado #3:²"; autor35$; "²": LOCATE 23, 2: PRINT "Autorizado #4:²"; autor45$; "²"
	DO WHILE salir = false
		 SELECT CASE sit%
		 CASE 1
		auxi$ = MeteDatoLetra(3, 16, 20, rif5$, "0"): LSET rif5$ = auxi$
		IF devolver = true THEN devolver = false: sit% = 15 ELSE sit% = 2
		 CASE 2
		auxi$ = MeteDatoLetra(5, 16, 20, nit5$, "0"): LSET nit5$ = auxi$
		IF devolver = true THEN devolver = false: sit% = 1 ELSE sit% = 3
		 CASE 3
		auxi$ = EntraDatoNumero(7, 16, 3, STR$(CVI(diacred5$)), 0): RSET diacred5$ = MKI$(VAL(auxi$))
		IF devolver = true THEN devolver = false: sit% = 2 ELSE sit% = 4
		 CASE 4
		auxi$ = EntraDatoNumero(9, 16, 16, STR$(CVL(monmax5$)), 0): IF VAL(auxi$) < 2000000000 THEN RSET monmax5$ = MKL$(ABS(VAL(auxi$)))
		LOCATE 9, 16: PRINT USING "²###############,"; CVL(monmax5$); : PRINT "²"
		IF devolver = true THEN devolver = false: sit% = 3 ELSE sit% = 5
		 CASE 5
		auxi$ = MeteDatoLetra(11, 16, 60, direc15$, "0"): LSET direc15$ = auxi$
		IF devolver = true THEN devolver = false: sit% = 4 ELSE sit% = 6
		 CASE 6
		auxi$ = MeteDatoLetra(12, 16, 60, direc25$, "0"): LSET direc25$ = auxi$
		IF devolver = true THEN devolver = false: sit% = 5 ELSE sit% = 7
		 CASE 7
		auxi$ = MeteDatoLetra(13, 16, 20, tele5$, "0"): LSET tele5$ = auxi$
		IF devolver = true THEN devolver = false: sit% = 6 ELSE sit% = 8
		 CASE 8
		auxi$ = EntraDatoNumero(15, 16, 3, STR$(CVI(dia15$)), 0): RSET dia15$ = MKI$(VAL(auxi$))
		IF devolver = true THEN devolver = false: sit% = 7 ELSE sit% = 9
		IF CVI(dia15$) < 0 OR CVI(dia15$) > 31 THEN sit% = 8
		 CASE 9
		auxi$ = EntraDatoNumero(15, 26, 3, STR$(CVI(dia25$)), 0): RSET dia25$ = MKI$(VAL(auxi$))
		IF devolver = true THEN devolver = false: sit% = 8 ELSE sit% = 10
		IF CVI(dia25$) < 0 OR CVI(dia25$) > 31 THEN sit% = 9
		 CASE 10
		auxi$ = EntraDatoNumero(15, 36, 3, STR$(CVI(dia35$)), 0): RSET dia35$ = MKI$(VAL(auxi$))
		IF devolver = true THEN devolver = false: sit% = 9 ELSE sit% = 11
		IF CVI(dia35$) < 0 OR CVI(dia35$) > 31 THEN sit% = 10
		 CASE 11
		auxi$ = EntraDatoNumero(15, 46, 3, STR$(CVI(dia45$)), 0): RSET dia45$ = MKI$(VAL(auxi$))
		IF devolver = true THEN devolver = false: sit% = 10 ELSE sit% = 12
		IF CVI(dia45$) < 0 OR CVI(dia45$) > 31 THEN sit% = 11
		 CASE 12
		auxi$ = MeteDatoLetra(17, 16, 30, autor15$, "0"): LSET autor15$ = auxi$
		IF devolver = true THEN devolver = false: sit% = 11 ELSE sit% = 13
		 CASE 13
		auxi$ = MeteDatoLetra(19, 16, 30, autor25$, "0"): LSET autor25$ = auxi$
		IF devolver = true THEN devolver = false: sit% = 12 ELSE sit% = 14
		 CASE 14
		auxi$ = MeteDatoLetra(21, 16, 30, autor35$, "0"): LSET autor35$ = auxi$
		IF devolver = true THEN devolver = false: sit% = 13 ELSE sit% = 15
		 CASE 15
		auxi$ = MeteDatoLetra(23, 16, 30, autor45$, "0"): LSET autor45$ = auxi$
		IF devolver = true THEN devolver = false: sit% = 14 ELSE sit% = 1
		 END SELECT
		 IF Fun(9) = "si" THEN salir = true
	LOOP
PUT #5, VAL(Yname1(mjfox%)): guia = 0
LOOP
EXIT SUB

SortYname:
Center 25, " Sorteando Los Campos De Menor A Mayor. Espere Por Favor . . . ": selecc% = 999
CLOSE #5: OPEN "F:\kevinprg\kevindat\clientes.dat" FOR RANDOM SHARED AS #5 LEN = 38: FIELD #5, 3 AS codigo$, 35 AS descripcion$
FOR k = 1 TO selecc%
	GET #5, k
	IF VAL(codigo$) = VAL(Yname1(donde%)) THEN
		RSET codigo$ = Yname1(donde%): RSET descripcion$ = Yname2(donde%)
		PUT #5, k
		EXIT FOR
	END IF
NEXT k
N = selecc%: Salto = N
WHILE Salto > 1
	Salto = Salto / 2
	DO
		Sw = 1
		FOR J = 1 TO N - Salto
			I = J + Salto
			GET #5, J: ao1 = descripcion$: ao3 = codigo$
			GET #5, I: ao2 = descripcion$: ao4 = codigo$
			IF ao1 > ao2 THEN
				RSET descripcion$ = ao2: RSET codigo$ = ao4: PUT #5, J
				RSET descripcion$ = ao1: RSET codigo$ = ao3: PUT #5, I
				Sw = 0
			END IF
		NEXT J
	LOOP UNTIL Sw = 1
WEND: CLOSE #5
RETURN
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

