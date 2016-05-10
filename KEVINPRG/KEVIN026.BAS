' estado de cuenta de clientes con credito
DEFINT A-Z
DECLARE SUB EstadodeCuenta ()
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
DIM SHARED total AS STRING * 8, credit AS STRING * 8, fx AS STRING * 31, Costa AS STRING * 8, Descger AS STRING * 8, neto AS STRING * 8
DIM SHARED Yname1(15) AS STRING * 3, Yname2(15) AS STRING * 35, Fun(12) AS STRING * 2, grabGav AS STRING * 1
CONST true = -1: CONST false = NOT true:  ybusqueda = 0
ON ERROR GOTO ErrorTrap
ON TIMER(5) GOSUB AbreGav
TIMER ON
CALL LeeEstado: CALL EstadodeCuenta
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






COLOR colors(7, colorpref), colors(4, colorpref): CALL Box(fila%, columna%, fila% + 16, columna% + 36): selecc% = 199: CALL Center(25, "[F1] Ayuda [Enter]Tomar Opcion [A-Z]Ir a... [Esc]Menu Anterior"): COLOR colors(4, colorpref), colors(7, colorpref _
)
LOCATE fila%, columna% + 1: PRINT " Cliente   "
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila% + 1, columna% + 1: PRINT Yname2(1): LOCATE fila2%, columna2%: PRINT Yname1(1); "²"; Yname2(1)
COLOR colors(4, colorpref), colors(7, colorpref)
FOR k = 2 TO 15: LOCATE fila% + k, columna% + 1: PRINT Yname2(k): NEXT k
resal% = 1: up% = 1: terminar = false
DO
	kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND: kbd$ = UCASE$(kbd$)
	SELECT CASE kbd$
			CASE CHR$(0) + "K": terminar = true: resal% = 0: up% = 0: devolver = true
			CASE CHR$(0) + "M": terminar = true: resal% = 0: up% = 0
			CASE CHR$(0) + "H"
			resal% = resal% - 1: IF resal% < 1 THEN resal% = 1: up% = up% - 1
			IF up% < 1 THEN up% = 1: terminar = true: devolver = true: resal% = 0: up% = 0
			CASE CHR$(0) + "P"
			resal% = resal% + 1: IF resal% > 15 THEN resal% = 15: up% = up% + 1
			IF up% + 14 > selecc% THEN terminar = true: resal% = 0: up% = 0
			CASE CHR$(0) + CHR$(73)
			up% = up% - 14: IF up% < 1 THEN up% = 1: resal% = 1
			CASE CHR$(0) + CHR$(81)
			up% = up% + 14: IF up% + 14 > selecc% THEN up% = selecc% - 14: resal% = 15
			CASE CHR$(13): terminar = true
			CASE CHR$(27): terminar = true: resal% = 0: up% = 0: devolver = true
			CASE IS >= CHR$(65)
			IF kbd$ <= CHR$(90) THEN
				FOR k = 1 TO selecc%: IF kbd$ = MID$(Yname2(k), 1, 1) THEN up% = k: resal% = 1: EXIT FOR
					IF k = selecc% THEN BEEP
				NEXT k
				IF up% + 14 > selecc% THEN up% = selecc% - 14: resal% = 1
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
	FOR k = up% TO up% + 14: l = l + 1
		IF resal% = l THEN COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila2%, columna2%: PRINT Yname1(k); "²"; Yname2(k)
		LOCATE fila% + l, columna% + 1: PRINT Yname2(k): IF resal% = l THEN COLOR colors(4, colorpref), colors(7, colorpref)
	NEXT k
	IF terminar = true THEN
		EXIT DO
	END IF
LOOP
Elije% = up% + resal% - 1
EXIT FUNCTION
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

SUB EstadodeCuenta
DO
	CLOSE
	COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box2 1, 1, 3, 80: Center 2, "Estado de Cuenta de UN Cliente"
	Center 25, "[F1]Ayuda [Enter]Escoje Cliente [Esc]Salir Use las Teclas de direccion"
	cliente% = Elije%(5, 5, 4, 18): IF cliente% < 1 THEN EXIT SUB
	IF Yname2(cliente%) = "þ" + SPACE$(34) THEN EXIT SUB
	DO
	CALL Center(4, "Cliente Elejido:²" + Yname1(cliente%) + "²" + Yname2(cliente%))
	COLOR colors(7, colorpref), colors(4, colorpref): CALL Box2(5, 1, 21, 80): LOCATE 6, 2: PRINT "Codigo ³ Descripcion                       ³Cantidad³ P.V.P.         ³Fecha  "
	CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
	FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$ _
, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2  _
AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
	fx = "F:\kevinprg\kevindat\cli" + Yname1(cliente%) + ".crd"
	OPEN fx FOR RANDOM AS #4 LEN = 38: FIELD #4, 1 AS acti$, 6 AS codi$, 2 AS canti$, 8 AS pvp$, 8 AS costo$, 1 AS psico$, 1 AS iva$, 1 AS auxi$, 8 AS fecha$, 2 AS lugar$
	max% = LOF(4) / 38: sitio% = 7: renglon = 0: Costa = MKD$(0)
	Descger = MKD$(0): total = MKD$(0): credit = MKD$(0): neto = MKD$(0)
	OPEN "f:\kevinprg\kevindat\fichacli.ent" FOR RANDOM SHARED AS #5 LEN = 326
	FIELD #5, 20 AS rif5$, 20 AS nit5$, 2 AS diacred5$, 4 AS monmax5$, 60 AS direc15$, 60 AS direc25$, 20 AS tele5$, 1 AS edo15$, 2 AS dia15$, 1 AS edo25$, 2 AS dia25$, 1 AS edo35$, 2 AS dia35$, 1 AS edo45$, 2 AS dia45$, 30 AS autor15$, 30 AS autor25$ _
, 30 AS autor35$, 30 AS autor45$, 8 AS desde$
	GET #5, VAL(Yname1(cliente%))
	IF max% > 0 THEN
		FOR k = 1 TO max%: GET #4, k
			IF acti$ = " " THEN
				IF codi$ = "NOTADE" THEN
					credit = MKD$(CVD(credit) + CVD(pvp$))
					LOCATE sitio%, 2: PRINT "NOTA   ³   Nota numero:";
					PRINT USING "#########,"; CVD(costo$); : PRINT "          ³";
					PRINT USING "#######,³"; 1; : sitio% = sitio% + 1
					PRINT USING "############,.##³"; CVD(pvp$);
					PRINT MID$(fecha$, 1, 2); "/"; MID$(fecha$, 3, 2); "/"; MID$(fecha$, 7, 2)
				ELSE
					total = MKD$(CVD(total) + CVI(canti$) * CVD(pvp$))
					Costa = MKD$(CVD(Costa) + CVI(canti$) * CVD(costo$))
					LOCATE sitio%, 2: PRINT codi$; " ³"; : GET #1, CVI(lugar$)
					PRINT nomb2$; "³";
					PRINT USING "#######,³"; CVI(canti$); : sitio% = sitio% + 1
					PRINT USING "############,.##³"; CVD(pvp$);
					PRINT MID$(fecha$, 1, 2); "/"; MID$(fecha$, 3, 2); "/"; MID$(fecha$, 7, 2)
				END IF
				IF sitio% = 21 THEN
					CALL Espera: sitio% = 7: COLOR colors(7, colorpref), colors(4, colorpref)
					CALL Box2(5, 1, 21, 80): LOCATE 6, 2: PRINT "Codigo ³ Descripcion                       ³Cantidad³ P.V.P.         ³Fecha  "
				END IF
			END IF
		NEXT k
	END IF
	IF CVD(total) = 0 THEN Box2 10, 10, 12, 70: Center 11, " Este cliente NO Adeuda ": Espera: EXIT DO
	CALL Espera: Descger = MKD$(CVD(total) * ((CVI(dia45$)) / 100)): neto = MKD$(CVD(total) - CVD(Descger))
	IF VAL(Redondeo) > 0 THEN
		IF CVD(neto) > 100 THEN
			neto = MKD$(VAL(Redondeo) * (FIX(CVD(neto) / VAL(Redondeo))))
			Descger = MKD$(CVD(total) - CVD(neto))
		END IF
	END IF
	IF CVD(neto) < CVD(Costa) THEN neto = Costa: Descger = MKD$(CVD(total) - CVD(neto))
	Box2 10, 10, 15, 70
	LOCATE 11, 12: PRINT " Total Adeudado    :"; : PRINT USING "###########,.##"; CVD(total);
	LOCATE 12, 12: PRINT " Total Descuento   :"; : PRINT USING "###########,.##"; CVD(Descger);
	LOCATE 13, 12: PRINT " Total Abonado     :"; : PRINT USING "###########,.##"; CVD(credit);
	LOCATE 14, 12: PRINT " Total Neto a Pagar:"; : PRINT USING "###########,.##"; (CVD(total) - CVD(Descger) + CVD(credit));
	IF CVD(total$) = 0 THEN EXIT DO
	CALL Box(16, 10, 18, 70): CALL Center(17, "Introduzca # de Copias a imprimir:"): van% = 1: neaux$ = MKD$(0): Center 25, "Suministre un valor de 0 a 9 , Por Favor ..."
	eleccion$ = EntraDatoNumero(17, 58, 1, "0", 0): IF sonido = "si" THEN BEEP
	IF VAL(eleccion$) > 5 THEN eleccion$ = "1"
	OPEN "LPT1:" FOR RANDOM AS #10
	WIDTH #10, 255
		FOR k = 1 TO VAL(eleccion$)
				PRINT #10, CHR$(27); CHR$(64); : PRINT #10, CHR$(27); CHR$(64);
				PRINT #10, CHR$(27); CHR$(67); CHR$(45); : neaux$ = MKD$(0)
				PRINT #10, CHR$(15);
				PRINT #10, STRING$(67, "-"); CHR$(10);
				PRINT #10, "Farmacia : "; NombFarma; CHR$(10);
				PRINT #10, "Telefono(s); "; Telefono; CHR$(10);
				PRINT #10, "Estado de Cuenta Fecha:"; MID$(DATE$, 4, 2); "/"; MID$(DATE$, 1, 2); "/"; MID$(DATE$, 7, 4); " Hora:"; TIME$; CHR$(10);
				PRINT #10, "Cliente:"; CHR$(10);
				PRINT #10, Yname2(cliente%); CHR$(10);
				PRINT #10, STRING$(67, "-"); CHR$(10);
				PRINT #10, "³Descripcion                        ³Cantidad³Precio Venta³Fecha"; CHR$(10);
				FOR l = 1 TO max%: GET #4, l
					IF acti$ = " " THEN
						IF codi$ = "NOTADE" THEN
							PRINT #10, "³   Nota numero:";
							PRINT #10, USING "#########,"; CVD(costo$); : PRINT #10, "          ³";
							PRINT #10, USING "#######,³"; 1;
							PRINT #10, USING "########,.##³"; CVD(pvp$); : PRINT #10, MID$(fecha$, 1, 2); "/"; MID$(fecha$, 3, 2); "/"; MID$(fecha$, 6, 2); : PRINT #10, CHR$(10);
							neaux$ = MKD$(CVD(neaux$) + CVD(pvp$)): van% = van% + 1
						ELSE
							GET #1, CVI(lugar$)
							PRINT #10, "³"; nomb2$; "³";
							PRINT #10, USING "#######,³"; CVI(canti$);
							PRINT #10, USING "########,.##³"; CVD(pvp$); : PRINT #10, MID$(fecha$, 1, 2); "/"; MID$(fecha$, 3, 2); "/"; MID$(fecha$, 6, 2); : PRINT #10, CHR$(10);
							neaux$ = MKD$(CVD(neaux$) + ((CVI(canti$) * (CVD(pvp$))))): van% = van% + 1
						END IF
						IF van% > 20 THEN
							van% = 1
							PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
							PRINT #10, "VAN --> Bs "; : PRINT #10, USING "##########,.##"; CVD(neaux$); : PRINT CHR$(10);
							PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
							PRINT #10, STRING$(67, "-"); CHR$(10); : PRINT #10, CHR$(12); : PRINT #10, STRING$(67, "-"); CHR$(10);
							PRINT #10, "Farmacia : "; NombFarma; CHR$(10);
							PRINT #10, "Telefono(s); "; Telefono; CHR$(10);
							PRINT #10, "Estado de Cuenta Fecha:"; MID$(DATE$, 4, 2); "/"; MID$(DATE$, 1, 2); "/"; MID$(DATE$, 7, 4); " Hora:"; TIME$; CHR$(10);
							PRINT #10, "Cliente:"; CHR$(10);
							PRINT #10, Yname2(cliente%); CHR$(10);
							PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
							PRINT #10, "VIENEN->Bs "; : PRINT #10, USING "##########,.##"; CVD(neaux$); : PRINT CHR$(10);
							PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
							PRINT #10, STRING$(67, "-"); CHR$(10);
							PRINT #10, "³Descripcion                        ³Cantidad³Precio Venta³Fecha"; CHR$(10);
						END IF
					END IF
				NEXT l
				PRINT #10, SPACE$(38); STRING$(15, "="); CHR$(10);
				PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
				PRINT #10, "Total Cuenta "; : IF VAL(Redondeo) > 0 THEN tote$ = MKD$((FIX(CVD(neaux$) / VAL(Redondeo))) * VAL(Redondeo)) ELSE tote$ = neaux$
				PRINT #10, USING "##########,.## Bs"; CVD(tote$); : PRINT #10, CHR$(10);
				PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
				PRINT #10, SPACE$(6); "*Cancelando al dia conserva su credito*"; CHR$(10);
				PRINT #10, SPACE$(6); "*  Se entregara Factura  al Cancelar  *"; CHR$(10);
				PRINT #10, SPACE$(6); "******** NO VALIDO COMO FACTURA *******"; CHR$(10);
				PRINT #10, STRING$(67, "-"); CHR$(10);
				PRINT #10, CHR$(12); : van% = 1
		NEXT k
		EXIT DO
	LOOP
LOOP
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

