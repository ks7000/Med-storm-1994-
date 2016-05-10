'venta de productos a credito
DEFINT A-Z
DECLARE SUB VentaCredito ()
DECLARE SUB Espera ()
DECLARE SUB Box (Row1, Col1, Row2, Col2)
DECLARE SUB Box2 (Row1, Col1, Row2, Col2)
DECLARE SUB Center (row, text$)
DECLARE SUB FancyCls (dots%, Background%)
DECLARE SUB LeeEstado ()
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$, tr$)
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Escoge% (fila%, edo%, fila2%, Prod$)
DECLARE FUNCTION Elije% (fila%, columna%, fila2%, columna2%)
DECLARE FUNCTION Trim$ (x$)
DECLARE FUNCTION DateToJulian& (Dat$, caso%)
COMMON SHARED guia, colorpref
DIM SHARED nursaler: guia = 1
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, impuesto AS STRING * 6, NombFarma AS STRING * 25, Direccion AS STRING * 30, Rif     AS STRING * 25, Nit      AS STRING * 25, telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2
DIM SHARED regi1(26) AS STRING * 2, regi2(26) AS STRING * 1, desc1(26) AS STRING * 35
DIM SHARED cant1(26) AS STRING * 2, cost1(26) AS STRING * 8, ivaf1(26) AS STRING * 1
DIM SHARED prec1(26) AS STRING * 8, code1(26) AS STRING * 6, psic1(26) AS STRING * 1
DIM SHARED PrintErr AS INTEGER, devolver, Fun(17) AS STRING * 2, sitio, salir
DIM SHARED renglon AS INTEGER, total AS STRING * 8, monto AS STRING * 8
DIM SHARED keya AS STRING * 6, fx AS STRING * 31, autorizado AS STRING * 30, encabezado AS STRING * 2
DIM SHARED notaentrega AS STRING * 4, conda AS STRING * 8, Yname1(15) AS STRING * 3, Yname2(15) AS STRING * 35
REM $DYNAMIC
DIM SHARED Nombra(5) AS STRING * 35, Pvp1a(5) AS STRING * 8, Pvp2a(5) AS STRING * 8, Pvp3a(5) AS STRING * 8, Pvp4a(5) AS STRING * 8, Numa(5) AS STRING * 2, Exis1a(5) AS STRING * 2, Exis2a(5) AS STRING * 2, Exis3a(5) AS STRING * 2, Exis4a(5) AS  _
STRING * 2
DIM SHARED Cant1a(5) AS STRING * 2, Cant2a(5) AS STRING * 2, Cant3a(5) AS STRING * 2, Cant4a(5) AS STRING * 2, grabGav AS STRING * 1
CONST true = -1: CONST false = NOT true:  ybusqueda = 0
ON TIMER(5) GOSUB AbreGav
TIMER ON
ON ERROR GOTO ErrorTrap
CALL LeeEstado: CALL VentaCredito
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

REM $STATIC
SUB Box (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿"; : FOR A = Row1 + 1 TO Row2 - 1: LOCATE A, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT A: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Box2 (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "²"; STRING$(BoxWidth - 2, "²"); "²"; : FOR A = Row1 + 1 TO Row2 - 1: LOCATE A, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT A: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Center (row, text$)
IF row = 25 THEN COLOR 0, 15: LOCATE 25, 1: PRINT SPACE$(80);
LOCATE row, 41 - LEN(text$) / 2: PRINT text$;
END SUB

FUNCTION DateToJulian& (Dat$, caso%)
IF caso% = 1 THEN   ' formato ddmmaaaa
		Day% = VAL(MID$(Dat$, 1, 2))
		Month% = VAL(MID$(Dat$, 3, 2))
		Year% = VAL(MID$(Dat$, 5, 4))
ELSE                ' formato mm/dd/aaaa
		Day% = VAL(MID$(Dat$, 4, 2))
		Month% = VAL(MID$(Dat$, 1, 2))
		Year% = VAL(MID$(Dat$, 7, 4))
END IF
		SELECT CASE Month%
			 CASE IS > 2
					Month% = Month% - 3
			 CASE IS <= 2
					Month% = Month% + 9
					Year% = Year% - 1
		END SELECT
		A& = 146097# * FIX(FIX(Year% / 100) / 4)
		B& = FIX(1461# * (Year% MOD 100) / 4)
		C& = FIX((153# * Month% + 2) / 5) + Day% + 1721119
		DateToJulian& = A& + B& + C&

END FUNCTION

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
	IF Fun(17) = "op" THEN Prod$ = UCASE$(MeteDatoLetra(fila2%, 34, 6, keya, "1")) ELSE Prod$ = UCASE$(MeteDatoLetra(fila2%, 34, 6, keya, "0"))
	Prud = Prod$: Prod$ = Trim$(Prod$)
	IF Fun(9) = "si" OR Prod$ = "" THEN : Escoge% = -1: EXIT FUNCTION
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
			Prod$ = UCASE$(MeteDatoLetra(fila2%, 34, 6, keya, "0"))
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
		'CASE IS >= CHR$(65)
		'     IF kbd$ <= CHR$(90) AND edo% = 1 THEN
		'          Prod$ = kbd$: OPEN "F:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
		'          FIELD #2, 35 AS nomshort$, 2 AS num$: max2% = LOF(2) / 37: GOSUB Parche1: GOSUB Parche2
		'     END IF
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
		GET #1, CVI(num$)
		IF acti2$ = " " THEN
		selecc% = selecc% + 1
		Nombra(selecc%) = nomb2$: Cant1a(selecc%) = exi12$: Cant2a(selecc%) = exi22$: Cant3a(selecc%) = exi32$: Cant4a(selecc%) = exi42$: Pvp1a(selecc%) = pv12$: Pvp2a(selecc%) = pv22$: Pvp3a(selecc%) = pv32$: Pvp4a(selecc%) = pv42$:  Numa(selecc%) =  _
num$
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
		Nombra(selecc%) = nomb2$: Cant1a(selecc%) = exi12$: Cant2a(selecc%) = exi22$: Cant3a(selecc%) = exi32$: Cant4a(selecc%) = exi42$: Pvp1a(selecc%) = pv12$: Pvp2a(selecc%) = pv22$: Pvp3a(selecc%) = pv32$: Pvp4a(selecc%) = pv42$:  Numa(selecc%) =  _
num$
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
Center 25, "Presione Cualquier Tecla Para Continuar...": DO: LOOP WHILE INKEY$ = ""
END SUB

SUB LeeEstado
CLOSE : OPEN "F:\kevinprg\kevindat\kaos.dat" FOR INPUT SHARED AS #2
INPUT #2, sonido: INPUT #2, impuesto: INPUT #2, NombFarma: INPUT #2, Direccion: INPUT #2, Rif: INPUT #2, Nit: INPUT #2, telefono: INPUT #2, contri: INPUT #2, contra: INPUT #2, Stock: INPUT #2, Blanq
INPUT #2, LargoH: INPUT #2, TipoL: INPUT #2, Mensaje1: INPUT #2, Mensaje2: INPUT #2, Mensaje3: INPUT #2, Redondeo: INPUT #2, Desc: INPUT #2, DescMax: INPUT #2, Exactitud: INPUT #2, Prov1: INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo: CLOSE
OPEN "f:\kevinprg\kevindat\encabeza.dat" FOR INPUT SHARED AS #2: INPUT #2, encabezado: CLOSE
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
			CASE CHR$(27): salida = true: EXIT DO
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
COLOR colors(5, colorpref), colors(4, colorpref): LOCATE fila%, columna% + 1: PRINT eleccion$
MeteDatoLetra$ = eleccion$: IF sonido = "si" THEN BEEP
EXIT FUNCTION
Sumar:
FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR 0, 15: LOCATE fila%, columna%: PRINT "²"; eleccion$; "²"
RETURN
END FUNCTION

FUNCTION Trim$ (x$)
IF x$ = "" THEN
	Trim$ = ""
ELSE
lastChar = 0
FOR A = 1 TO LEN(x$): y$ = MID$(x$, A, 1): IF y$ <> CHR$(0) AND y$ <> " " THEN lastChar = A
NEXT A: Trim$ = LEFT$(x$, lastChar)
END IF
END FUNCTION

SUB VentaCredito
DO
	CLOSE : OPEN "F:\kevinprg\kevindat\clientes.dat" FOR RANDOM AS #5 LEN = 38: FIELD #5, 3 AS codigo$, 35 AS nomb$
	FOR k = 1 TO 199: GET #5, k: Yname1(k) = codigo$: Yname2(k) = nomb$: NEXT k: CLOSE
	COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box2 1, 1, 3, 80: Center 2, "Venta a Clientes con Credito"
	Center 25, "[F1]Ayuda [Enter]Escoje Cliente [Esc]Salir Use las Teclas de direccion"
	cliente% = Elije%(5, 5, 4, 18): IF cliente% < 1 THEN EXIT SUB
	IF Yname2(cliente%) = "þ" + SPACE$(34) THEN EXIT SUB
	CLOSE : fx = "F:\kevinprg\kevindat\cli" + Yname1(cliente%) + ".crd"
	OPEN fx FOR RANDOM AS #4 LEN = 38: FIELD #4, 1 AS acti$, 6 AS codi$, 2 AS canti$, 8 AS pvp$, 8 AS costo$, 1 AS psico$, 1 AS iva$, 1 AS auxi$, 8 AS fecha$, 2 AS lugar$
	max& = LOF(4) / 38: total = MKD$(0): masviejo& = 0: bandera% = 0
	IF max& > 0 THEN
		FOR k = 1 TO max&: GET #4, k
		'LOCATE 1, 1: PRINT "acti"; acti$; "codi"; codi$; "canti"; CVI(canti$); "pvp"; CVD(pvp$); "costo"; CVD(costo$); "psico"; psico$; "fecha"; fecha$: Espera
		IF acti$ = " " THEN total = MKD$(CVD(total) + CVI(canti$) * CVD(pvp$))
		IF acti$ = " " AND bandera% = 0 THEN
			masviejo& = DateToJulian(DATE$, 2) - DateToJulian(fecha$, 1): bandera% = 1
		END IF
		NEXT k
	END IF: CLOSE : salir = false
	OPEN "f:\kevinprg\kevindat\fichacli.ent" FOR RANDOM SHARED AS #5 LEN = 326
	FIELD #5, 20 AS rif5$, 20 AS nit5$, 2 AS diacred5$, 4 AS monmax5$, 60 AS direc15$, 60 AS direc25$, 20 AS tele5$, 1 AS edo15$, 2 AS dia15$, 1 AS edo25$, 2 AS dia25$, 1 AS edo35$, 2 AS dia35$, 1 AS edo45$, 2 AS dia45$, 30 AS autor15$, 30 AS autor25$ _
, 30 AS autor35$, 30 AS autor45$, 8 AS desde$
	GET #5, VAL(Yname1(cliente%)): monto = MKD$(CVL(monmax5$))
	IF CVI(diacred5$) = 0 THEN Box2 10, 10, 12, 70: Center 11, "Este cliente tiene cero dias de credito": Espera: salir = true: EXIT SUB
	IF CVD(total) >= CVL(monmax5$) THEN Box2 10, 10, 12, 70: Center 11, "Este cliente sobrepaso su limite de credito:" + STR$(CVL(monmax5$)): Espera: salir = true: EXIT SUB
	IF masviejo& > CVI(diacred5$) THEN
		Box2 10, 10, 12, 70: Center 11, "Este cliente sobrepaso su limite de tiempo:" + STR$(CVI(diacred5$)) + " dias": Espera: salir = true
		EXIT SUB
	END IF
	CALL Box(22, 10, 24, 70): LOCATE 23, 11: PRINT "Deuda Actual : "; : PRINT USING "##############,.##"; CVD(total$)
	CALL Box(5, 10, 12, 70): CALL Center(6, "Introduzca # de Autorizado:"): LOCATE 7, 11: PRINT "# 1.-"; Yname2(cliente%)
	LOCATE 8, 11: PRINT "# 2.-"; autor15$: LOCATE 9, 11: PRINT "# 3.-"; autor25$
	LOCATE 10, 11: PRINT "# 4.-"; autor35$: LOCATE 11, 11: PRINT "# 5.-"; autor45$: Center 25, "Indique quien esta recibiendo los productos. [Esc]Salir y abortar"
	eleccion$ = EntraDatoNumero(6, 58, 1, "1", 0): IF devolver = true THEN EXIT SUB
	IF sonido = "si" THEN BEEP
	IF VAL(eleccion$) > 5 OR VAL(eleccion$) = 0 THEN eleccion$ = "1"
	IF VAL(eleccion$) = 1 THEN autorizado = Yname2(cliente%)
	IF VAL(eleccion$) = 2 THEN autorizado = autor15$
	IF VAL(eleccion$) = 3 THEN autorizado = autor25$
	IF VAL(eleccion$) = 4 THEN autorizado = autor35$
	IF VAL(eleccion$) = 5 THEN autorizado = autor45$
	CALL Box(5, 10, 12, 70): FOR k = 1 TO 17: Fun(k) = "no": NEXT k: seguir = true: devolver = false: keya = "      "
	CALL Box(5, 10, 7, 70): CALL Center(6, " Introduzca Clave de Vendedor:²   ²")
	eleccion$ = "": eleccion$ = MeteDatoLetra(6, 54, 3, "   ", "0"): IF sonido = "si" THEN BEEP
	COLOR colors(7, colorpref), colors(4, colorpref): elegsal% = 0
	CLOSE : OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #1 LEN = 100: FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
	nursaler = LOF(1) / 100: ClaveLarga$ = "": si$ = " "
	FOR k = 1 TO nursaler: GET #1, k
		IF eleccion$ = clco$ AND acti$ = " " AND inout$ = " " THEN
			ClaveLarga$ = clla$: Cedula$ = cedu$: RSET inout$ = "*"
			PUT #1, k: EXIT FOR
		END IF
	NEXT k: CLOSE
	IF ClaveLarga$ = "" THEN CALL Center(8, "No Hay Ningun Vendedor Con Esa Clave o Esta Fuera El Vendedor"): Espera: EXIT SUB
	COLOR colors(7, colorpref), colors(4, colorpref): CLS
	LOCATE 1, 1: PRINT "Vendedor(a):"; ClaveLarga$: CALL Box(2, 1, 16, 80)
	salir = false: total = MKD$(0): renglon = 1
	OPEN "F:\kevinprg\kevindat\maxentre.dat" FOR RANDOM AS #7: FIELD #7, 4 AS nota$
	GET #7, 1: numfct$ = nota$: notaentrega = numfct$: numfct$ = MKL$(CVL(numfct$) + 1)
	IF CVL(numfct$) > 999999 THEN numfct$ = MKL$(1)
	LSET nota$ = numfct$: PUT #7, 1: CLOSE #7
	LOCATE 1, 44: PRINT "Nota de Entrega Numero  :²"; : PRINT USING "**####,"; CVL(notaentrega); : PRINT "²": Fun(17) = "op"
	'AdicProd: ------------------===============---------------------------------
	DO
		IF Fun(17) = "no" AND renglon > 26 THEN
			DO
			GOSUB Limpio: Fun(9) = "si": GOSUB Cierre: IF salir = true THEN EXIT DO
			LOOP: salir = false: EXIT DO
		END IF
		IF Fun(17) = "no" THEN EXIT DO
		CALL Center(25, "[1]Ayuda [6]Cambio [7]Lim [10]Cerrar y Salir"): COLOR colors(7, colorpref), colors(4, colorpref)
		CALL Box(2, 1, 16, 80)
		seguir = true: mjfox% = 0: LOCATE 16, 1: PRINT "À" + STRING$(78, "Ä") + "Ù"
		LOCATE 2, 24: PRINT "Â": LOCATE 2, 40: PRINT "ÂÂ": LOCATE 2, 29: PRINT "Â": LOCATE 2, 64: PRINT "Â": LOCATE 2, 69: PRINT "Â": LOCATE 16, 24: PRINT "Á": LOCATE 16, 29: PRINT "Á": LOCATE 16, 40: PRINT "ÁÁ"
		FOR k = 1 TO 13
		LOCATE k + 2, 1: PRINT "³"; MID$(desc1(k), 1, 22); "³"
		LOCATE k + 2, 40: PRINT "³³"; MID$(desc1(k + 13), 1, 22); "³"
		LOCATE k + 2, 25: IF CVI(cant1(k)) = 0 THEN PRINT "    ³" ELSE PRINT USING "####"; CVI(cant1(k)); : PRINT "³"
		LOCATE k + 2, 65: IF CVI(cant1(k + 13)) = 0 THEN PRINT "    ³" ELSE PRINT USING "####"; CVI(cant1(k + 13)); : PRINT "³"
		LOCATE k + 2, 30: IF CVD(prec1(k)) = 0 THEN PRINT "          ³" ELSE PRINT USING "######,.##"; CVD(prec1(k));
		LOCATE k + 2, 70: IF CVD(prec1(k + 13)) = 0 THEN PRINT "          ³" ELSE PRINT USING "######,.##"; CVD(prec1(k + 13)); : PRINT "³"
		NEXT k
		COLOR colors(7, colorpref), colors(4, colorpref): LOCATE 18, 1: PRINT STRING$(80, "Ä"): COLOR colors(7, colorpref), colors(4, colorpref): LOCATE 17, 1: PRINT SPACE$(4); "Introduzca Nombre o Funcion: ²      ²"; SPACE$(29)
		COLOR colors(7, colorpref) + 16, colors(4, colorpref): LOCATE 16, 45: PRINT "** SUB - TOTAL ="; : PRINT USING "########,.##"; CVD(total); : PRINT " Bs. **"
		COLOR colors(7, colorpref), colors(4, colorpref)
			llave$ = "": DO WHILE llave$ = "": llave$ = INKEY$: LOOP
			SELECT CASE llave$
				CASE CHR$(0) + CHR$(64): GOSUB Cambiar
				CASE CHR$(0) + CHR$(65): GOSUB Limpio: GOSUB Anula
				CASE CHR$(0) + CHR$(68): GOSUB Limpio: Fun(10) = "si": GOSUB Cierre
				CASE ELSE: keya = llave$: GOSUB BuscaOrden
			END SELECT
	LOOP
LOOP
'   GOTO Pantalla  '-------------------===============--------------------------

BuscaOrden:
CALL Center(17, SPACE$(80))
mjfox% = Escoge%(19, 1, 17, keya)
IF mjfox% < 1 THEN devolver = false: GOSUB Limpio: RETURN
GOSUB Mostrar
IF Fun(17) = "op" THEN GOSUB EscPvp
RETURN

Cierre: '       CIERRE-----CIERRE------CIERRE-----
CALL Box2(17, 15, 24, 65): LOCATE 20, 20: PRINT "¨Desea CERRAR el proceso ?  "
keya$ = UCASE$(MeteDatoLetra$(20, 48, 1, "N", "0"))
IF MID$(keya$, 1, 1) = "N" OR MID$(keya$, 1, 1) = "n" THEN devolver = false: Fun(8) = "no": Fun(9) = "no": Fun(10) = "no": GOSUB Limpio: RETURN
CLOSE : fx = "F:\kevinprg\kevindat\cli" + Yname1(cliente%) + ".crd"
OPEN fx FOR RANDOM AS #4 LEN = 38: FIELD #4, 1 AS acti$, 6 AS codi$, 2 AS canti$, 8 AS pvp$, 8 AS costo$, 1 AS psico$, 1 AS iva$, 1 AS auxi$, 8 AS fecha$, 2 AS lugar$
max& = LOF(4) / 38
FOR k = 1 TO renglon - 1
		RSET acti$ = " ": RSET codi$ = code1(k): RSET canti$ = cant1(k)
		RSET pvp$ = prec1(k): RSET costo$ = cost1(k): RSET psico$ = psic1(k): RSET lugar$ = regi1(k)
		RSET iva$ = ivaf1(k): RSET fecha$ = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4)
		PUT #4, (max& + k): IF psic1(k) = "*" THEN salir = true
NEXT k: CLOSE
IF salir = true THEN
	OPEN "F:\kevinprg\kevindat\movpsico.dat" FOR RANDOM AS #1 LEN = 40: FIELD #1, 40 AS todo$: max& = LOF(1) / 40: max& = max& + 1
		FOR k = 1 TO renglon - 1
			IF psic1(k) = "*" THEN RSET todo$ = "CREDIT" + notaentrega + DATE$ + TIME$ + code1$(k) + cant1(k) + Cedula$: PUT #1, max&: max& = max& + 1
		NEXT k: CLOSE
END IF
		CLOSE : OPEN "F:\kevinprg\kevindat\bitacora.dat" FOR RANDOM AS #17 LEN = 40: FIELD #17, 40 AS tod$: max& = LOF(17) / 40
		FOR k = 1 TO renglon - 1
		LSET tod$ = code1$(k) + MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4) + MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + cant1(k) + "11" + notaentrega + Cedula$ + prec1(k)
		max& = max& + 1: PUT #17, max&: NEXT k: CLOSE
		OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #1 LEN = 100
		FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
		nursaler = LOF(1) / 100
		FOR k = 1 TO nursaler: GET #1, k
		IF Cedula$ = cedu$ THEN RSET inout$ = " ": Cedula$ = clla$: PUT #1, k: EXIT FOR
		NEXT k: CLOSE : devolver = false
		OPEN "f:\kevinprg\kevindat\fichacli.ent" FOR RANDOM SHARED AS #5 LEN = 326
		FIELD #5, 20 AS rif5$, 20 AS nit5$, 2 AS diacred5$, 4 AS monmax5$, 60 AS direc15$, 60 AS direc25$, 20 AS tele5$, 140 AS nada5$
		GET #5, VAL(Yname1(cliente%)): nombre$ = Yname2(cliente%): domicilio1$ = direc15$: domicilio2$ = direc25$
		rifo$ = rif5$: nito$ = nit5$: tlf$ = tele5$: keya = "0"
		CALL Box(19, 17, 21, 60): LOCATE 20, 18: PRINT "¨Cuantas Copias desea?": keya = EntraDatoNumero$(20, 48, 2, " 0", 1)
		OPEN "LPT1:" FOR RANDOM AS #10
		WIDTH #10, 255
		FOR l% = 1 TO VAL(keya)
			PRINT #10, CHR$(27); CHR$(64); : PRINT #10, CHR$(27); CHR$(64);
			PRINT #10, CHR$(27); CHR$(67); CHR$(45); : neaux$ = MKD$(0)
			PRINT #10, CHR$(10); : PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
			PRINT #10, CHR$(15);
			PRINT #10, STRING$(60, "-"); CHR$(10);
			IF encabezado = "si" THEN
				PRINT #10, "Farmacia:"; NombFarma; CHR$(10);
				PRINT #10, "RIF"; Rif; " NIT:"; Nit; CHR$(10);
				PRINT #10, "Telefono(s):"; telefono; CHR$(10);
			END IF
			PRINT #10, USING "Entrega:######,"; CVL(notaentrega); : PRINT #10, " Fecha:"; MID$(DATE$, 4, 2); "/"; MID$(DATE$, 1, 2); "/"; MID$(DATE$, 7, 4); " Hora:"; TIME$;
			PRINT #10, " Pagina:1 de "; : IF (renglon - 1) <= 11 THEN PRINT #10, "1"; CHR$(10);  ELSE PRINT #10, "2"; CHR$(10);
			PRINT #10, "Cliente:                               Vendedor:"; Cedula$; CHR$(10);
			PRINT #10, nombre$; CHR$(10);
			PRINT #10, "R.I.F.:"; rifo$; SPACE$(20 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(20 - LEN(rifo$)); CHR$(10);
			PRINT #10, "Domicilio Fiscal:"; CHR$(10);
			PRINT #10, domicilio1$; CHR$(10); : PRINT #10, domicilio2$; CHR$(10);
			PRINT #10, "Telefonos:"; tlf$; SPACE$(20 - LEN(rifo$)); " Forma de Pago: Credito"; CHR$(10);
			PRINT #10, STRING$(60, "-"); CHR$(10); : renglon = renglon - 1
			PRINT #10, "³Descripcion                        ³Cantidad³Precio Venta³"; CHR$(10);
			SELECT CASE renglon
				CASE 1 TO 12
					FOR k = 1 TO renglon
						PRINT #10, "³"; desc1(k); "³";
						PRINT #10, USING "#######,³"; CVI(cant1(k)); : neaux$ = MKD$(CVD(neaux$) + ((CVI(cant1(k)) * (CVD(prec1(k))))))
						PRINT #10, USING "########,.##³"; CVD(prec1(k)); : PRINT #10, CHR$(10);
					NEXT k
				CASE 13 TO 21
					FOR k = 1 TO renglon
						PRINT #10, "³"; desc1(k); "³";
						PRINT #10, USING "#######,³"; CVI(cant1(k));
						PRINT #10, USING "########,.##³"; CVD(prec1(k));
						PRINT #10, CHR$(10); : neaux$ = MKD$(CVD(neaux$) + ((CVI(cant1(k)) * (CVD(prec1(k))))))
					NEXT k
					FOR k = 1 TO (21 - renglon): PRINT #10, SPACE$(k + 5); "\"; SPACE$(20); "\"; CHR$(10); : NEXT k
					PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
					PRINT #10, "VAN --> Bs "; : PRINT #10, USING "##########,.##"; CVD(neaux$); : PRINT CHR$(10);
					PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
					PRINT #10, CHR$(12); : PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
					PRINT #10, STRING$(60, "-"); CHR$(10);
					PRINT #10, USING "Entrega:######,"; CVL(notaentrega); : PRINT #10, " Fecha:"; MID$(DATE$, 4, 2); "/"; MID$(DATE$, 1, 2); "/"; MID$(DATE$, 7, 4); " Hora:"; TIME$;
					PRINT #10, " Pagina:2 de 2"; CHR$(10); : PRINT #10, "Cliente:                               Vendedor:"; Cedula$; CHR$(10);
					PRINT #10, nombre$; CHR$(10);
					PRINT #10, "R.I.F.:"; rifo$; SPACE$(20 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(20 - LEN(rifo$)); CHR$(10);
					PRINT #10, "Domicilio Fiscal:"; CHR$(10);
					PRINT #10, domicilio1$; CHR$(10); : PRINT #10, domicilio2$; CHR$(10);
					PRINT #10, "Telefonos:"; tlf$; SPACE$(20 - LEN(rifo$)); " Forma de Pago: Credito"; CHR$(10);
					PRINT #10, STRING$(60, "-"); CHR$(10);
					PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
					PRINT #10, "VIENEN->Bs "; : PRINT #10, USING "##########,.##"; CVD(neaux$); : PRINT CHR$(10);
					PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
				CASE 22 TO 26
					FOR k = 1 TO 21
						PRINT #10, "³"; desc1(k); "³";
						PRINT #10, USING "#######,³"; CVI(cant1(k));
						PRINT #10, USING "########,.##³"; CVD(prec1(k));
						PRINT #10, CHR$(10); : neaux$ = MKD$(CVD(neaux$) + ((CVI(cant1(k)) * (CVD(prec1(k))))))
					NEXT k
					PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
					PRINT #10, "VAN --> Bs "; : PRINT #10, USING "##########,.##"; CVD(neaux$); : PRINT CHR$(10);
					PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
					PRINT #10, CHR$(12); : PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
					PRINT #10, STRING$(60, "-"); CHR$(10);
					PRINT #10, USING "Entrega:######,"; CVL(notaentrega); : PRINT #10, " Fecha:"; MID$(DATE$, 4, 2); "/"; MID$(DATE$, 1, 2); "/"; MID$(DATE$, 7, 4); " Hora:"; TIME$;
					PRINT #10, " Pagina:2 de 2"; CHR$(10); : PRINT #10, "Cliente:                               Vendedor:"; Cedula$; CHR$(10);
					PRINT #10, nombre$; CHR$(10);
					PRINT #10, "R.I.F.:"; rifo$; " N.I.T.:"; nito$; CHR$(10);
					PRINT #10, "Domicilio Fiscal:"; CHR$(10);
					PRINT #10, domicilio1$; CHR$(10);
					PRINT #10, domicilio2$; CHR$(10);
					PRINT #10, "Telefonos:"; tlf$; " Forma de Pago: Credito"; CHR$(10);
					PRINT #10, STRING$(60, "-"); CHR$(10);
					PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
					PRINT #10, "VIENEN->Bs "; : PRINT #10, USING "##########,.##"; CVD(neaux$); : PRINT CHR$(10);
					PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
					FOR k = 22 TO renglon
						PRINT #10, "³"; desc1(k); "³"; : PRINT #10, USING "#######,³"; CVI(cant1(k));
						PRINT #10, USING "########,.##³"; CVD(prec1(k)); : PRINT #10, CHR$(10); : neaux$ = MKD$(CVD(neaux$) + ((CVI(cant1(k)) * (CVD(prec1(k))))))
					NEXT k
			END SELECT
			PRINT #10, SPACE$(38); STRING$(15, "="); CHR$(10);
			PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
			PRINT #10, "Total Entrega"; : PRINT #10, USING "##########,.## Bs"; CVD(neaux$); : PRINT #10, CHR$(10);
			PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
			PRINT #10, "Acepto y Recibo Conforme:"; autorizado; CHR$(10);
			PRINT #10, CHR$(10); : PRINT #10, CHR$(10); : PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
			PRINT #10, SPACE$(10); "*Se entregara Factura  al Cancelar*"; CHR$(10);
			PRINT #10, SPACE$(10); "****** NO VALIDO COMO FACTURA *****"; CHR$(10);
			PRINT #10, STRING$(60, "-"); CHR$(10);
			PRINT #10, CHR$(12); : renglon = renglon + 1
		NEXT l%
EXIT SUB
RETURN    ' FIN CIERRE-----FIN CIERRE-----FIN CIERRE------
Limpia:
COLOR colors(7, colorpref), colors(4, colorpref): FOR k = 5 TO 24: LOCATE k, 1: PRINT SPACE$(80); : NEXT k
RETURN
Limpio:
COLOR colors(7, colorpref), colors(4, colorpref): FOR k = 17 TO 24: LOCATE k, 1: PRINT SPACE$(80); : NEXT k
RETURN
EscPvp:              ' EscPvp---------EscPvp----------EscPvp---------
GOSUB Mostrar: LOCK #1, mjfox%
IF Fun(3) = "si" THEN
		CALL Center(25, "1]Ayuda 2]Lab 3]Cod 4]Ofe 5]Com 6]Cambio 7]Lim 8]SIN 9]CON 10]COM 11]CHEQ")
		COLOR colors(7, colorpref), colors(4, colorpref): CALL Box(2, 1, 16, 80)
		LOCATE 16, 1: PRINT "À" + STRING$(78, "Ä") + "Ù"
		LOCATE 2, 24: PRINT "Â": LOCATE 2, 40: PRINT "ÂÂ": LOCATE 2, 29: PRINT "Â": LOCATE 2, 64: PRINT "Â": LOCATE 2, 69: PRINT "Â": LOCATE 16, 24: PRINT "Á": LOCATE 16, 29: PRINT "Á": LOCATE 16, 40: PRINT "ÁÁ"
		FOR k = 1 TO 13
		LOCATE k + 2, 1: PRINT "³"; MID$(desc1(k), 1, 22); "³"
		LOCATE k + 2, 40: PRINT "³³"; MID$(desc1(k + 13), 1, 22); "³"
		LOCATE k + 2, 25: IF CVI(cant1(k)) = 0 THEN PRINT "    ³" ELSE PRINT USING "####"; CVI(cant1(k)); : PRINT "³"
		LOCATE k + 2, 65: IF CVI(cant1(k + 13)) = 0 THEN PRINT "    ³" ELSE PRINT USING "####"; CVI(cant1(k + 13)); : PRINT "³"
		LOCATE k + 2, 30: IF CVD(prec1(k)) = 0 THEN PRINT "          ³" ELSE PRINT USING "######,.##"; CVD(prec1(k));
		LOCATE k + 2, 70: IF CVD(prec1(k + 13)) = 0 THEN PRINT "          ³" ELSE PRINT USING "######,.##"; CVD(prec1(k + 13)); : PRINT "³"
		NEXT k
		COLOR colors(7, colorpref) + 16, colors(4, colorpref): LOCATE 2, 45
		PRINT "** SUB - TOTAL ="; : PRINT USING "########,.##"; CVD(total); : PRINT " Bs. **"
		COLOR colors(7, colorpref), colors(4, colorpref)
END IF: sit% = 0
IF preg$ = "*" THEN
	venzo& = 1000000: lado% = 0: auxvenzo& = 0
	IF ven12$ <> "  " AND CVI(exi12$) <> 0 THEN
		auxvenzo& = 12 * CVI(ven12$): IF ven11$ = "A" THEN auxvenzo& = auxvenzo& + 10
		IF ven11$ = "B" THEN auxvenzo& = auxvenzo& + 11
		IF ven11$ = "C" THEN auxvenzo& = auxvenzo& + 12 ELSE auxvenzo& = auxvenzo& + VAL(ven11$)
		IF auxvenzo& < venzo& THEN venzo& = auxvenzo&: lado$ = "uno (1)": sit% = 1: condi% = CVI(exi12$)
	END IF: auxvenzo& = 0
	IF ven22$ <> "  " AND CVI(exi22$) <> 0 THEN
		auxvenzo& = 12 * CVI(ven22$): IF ven21$ = "A" THEN auxvenzo& = auxvenzo& + 10
		IF ven21$ = "B" THEN auxvenzo& = auxvenzo& + 11
		IF ven21$ = "C" THEN auxvenzo& = auxvenzo& + 12 ELSE auxvenzo& = auxvenzo& + VAL(ven21$)
		IF auxvenzo& < venzo& THEN venzo& = auxvenzo&: lado$ = "dos (2)": sit% = 2: condi% = CVI(exi22$)
	END IF: auxvenzo& = 0
	IF ven32$ <> "  " AND CVI(exi32$) <> 0 THEN
		auxvenzo& = 12 * CVI(ven32$): IF ven31$ = "A" THEN auxvenzo& = auxvenzo& + 10
		IF ven31$ = "B" THEN auxvenzo& = auxvenzo& + 11
		IF ven31$ = "C" THEN auxvenzo& = auxvenzo& + 12 ELSE auxvenzo& = auxvenzo& + VAL(ven31$)
		IF auxvenzo& < venzo& THEN venzo& = auxvenzo&: lado$ = "tres (3)": sit% = 3: condi% = CVI(exi32$)
	END IF: auxvenzo& = 0
	IF ven42$ <> "  " AND CVI(exi42$) <> 0 THEN
		auxvenzo& = 12 * CVI(ven42$): IF ven41$ = "A" THEN auxvenzo& = auxvenzo& + 10
		IF ven41$ = "B" THEN auxvenzo& = auxvenzo& + 11
		IF ven41$ = "C" THEN auxvenzo& = auxvenzo& + 12 ELSE auxvenzo& = auxvenzo& + VAL(ven41$)
		IF auxvenzo& < venzo& THEN venzo& = auxvenzo&: lado$ = "cuatro (4)": sit% = 4: condi% = CVI(exi42$)
	END IF: auxvenzo& = 0
	IF venzo& < 1000000 THEN
		COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 17, 1, 24, 80
		LOCATE 18, 25: PRINT "* * *  A T E N C I O N  * * *": COLOR colors(7, colorpref), colors(4, colorpref)
		Center 20, "A Este Articulo se le vigila el Vencimiento , Usted DEBE": Center 21, "vender del PVP Numero " + lado$: Espera
		GOSUB Mostrar: LOCK #1, mjfox%
	END IF
END IF
IF sit% = 0 THEN
	COLOR colors(7, colorpref), colors(4, colorpref): LOCATE 20, 70: PRINT USING "######,.##"; CVD(pv32$); : sit% = 3: conda = pv32$: condi% = CVI(exi32$)
	DO
		DO
		COLOR colors(4, colorpref), colors(7, colorpref)
		LOCATE 22, 2: PRINT STRING$(33, "Ä"): Center 25, " Utilize Teclas de Direccion y Escoja PVP con [Enter];salga con [Esc] "
		llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
		SELECT CASE llave$
			CASE CHR$(0) + "K": old% = sit%: sit% = sit% - 1: IF sit% < 1 THEN sit% = 4
			CASE CHR$(0) + "M": old% = sit%: sit% = sit% + 1: IF sit% > 4 THEN sit% = 1
			CASE CHR$(27)
				GOSUB Limpio: UNLOCK #1, mjfox% :
				mjfox% = 0: RETURN
			CASE CHR$(13)
			IF CVD(conda) > 0 THEN EXIT DO ELSE LOCATE 22, 3: PRINT "El PVP Debe Ser MAYOR Que 0": Espera
			CASE ELSE: IF sonido = "si" THEN BEEP
		END SELECT
		COLOR colors(4, colorpref), colors(7, colorpref)
		IF old% = 1 THEN LOCATE 20, 32: PRINT USING "######,.##"; CVD(pv12$)
		IF old% = 2 THEN LOCATE 20, 51: PRINT USING "######,.##"; CVD(pv22$)
		IF old% = 3 THEN LOCATE 20, 70: PRINT USING "######,.##"; CVD(pv32$)
		IF old% = 4 THEN LOCATE 20, 14: PRINT USING "######,.##"; CVD(pv42$)
		COLOR colors(7, colorpref), colors(4, colorpref)
		IF sit% = 1 THEN LOCATE 20, 32: PRINT USING "######,.##"; CVD(pv12$): condi% = CVI(exi12$): conda = pv12$
		IF sit% = 2 THEN LOCATE 20, 51: PRINT USING "######,.##"; CVD(pv22$): condi% = CVI(exi22$): conda = pv22$
		IF sit% = 3 THEN LOCATE 20, 70: PRINT USING "######,.##"; CVD(pv32$): condi% = CVI(exi32$): conda = pv32$
		IF sit% = 4 THEN LOCATE 20, 14: PRINT USING "######,.##"; CVD(pv42$): condi% = CVI(exi42$): conda = pv42$
		LOOP
	IF negativo = "no" THEN
		IF condi% > 0 THEN
			EXIT DO
		ELSE
			LOCATE 22, 3: PRINT "NO Hay Existencia en Ese Precio": Espera
		END IF
	ELSE
		EXIT DO
	END IF
	LOOP
END IF
LOCATE 20, 24: PRINT "³"; : PRINT USING "#####,"; CVI(exi12$); : LOCATE 20, 42: PRINT "³"; : PRINT USING "#####,"; CVI(exi22$);
LOCATE 20, 61: PRINT "³"; : PRINT USING "#####,"; CVI(exi32$): LOCATE 20, 32: PRINT USING "######,.##"; CVD(pv12$)
LOCATE 20, 51: PRINT USING "######,.##"; CVD(pv22$): LOCATE 20, 70: PRINT USING "######,.##"; CVD(pv32$)
CALL Center(25, "Introduzca el N£mero de Unidades y Presione [Enter] cuando est‚ listo.")
Box 21, 3, 23, 50: LOCATE 22, 5: PRINT "¨ Cantidad a vender ? ": salir = false
IF negativo = "si" THEN
	keya = EntraDatoNumero(22, 25, 6, "1     ", 1)
ELSE
	DO WHILE NOT salir
	keya = EntraDatoNumero(22, 25, 6, "1     ", 1): IF VAL(keya) <= condi% THEN salir = true
	Center 25, " La Cantidad NO debe ser MAYOR que la Existencia "
	LOOP
END IF: cant1(renglon) = MKI$(VAL(keya))
IF CVI(cant1(renglon)) = 0 THEN mjfox% = 0: desc1(renglon) = "": code1(renglon) = "": cant1(renglon) = MKI$(0): prec1(renglon) = MKD$(0): cost1(renglon) = MKD$(0): GOSUB Limpio: RETURN
IF sit% = 1 THEN
	IF (CVD(total) + VAL(keya) * CVD(pv12$)) > CVD(monto) THEN mjfox% = 0: desc1(renglon) = "": code1(renglon) = "": cant1(renglon) = MKI$(0): prec1(renglon) = MKD$(0): cost1(renglon) = MKD$(0): LOCATE 22, 5: PRINT "Sobrepasa el limite de Credito": _
      Espera: GOSUB Limpio: RETURN
END IF
IF sit% = 2 THEN
	IF (CVD(total) + VAL(keya) * CVD(pv22$)) > CVD(monto) THEN mjfox% = 0: desc1(renglon) = "": code1(renglon) = "": cant1(renglon) = MKI$(0): prec1(renglon) = MKD$(0): cost1(renglon) = MKD$(0): LOCATE 22, 5: PRINT "Sobrepasa el limite de Credito": _
      Espera: GOSUB Limpio: RETURN
END IF
IF sit% = 3 THEN
	IF (CVD(total) + VAL(keya) * CVD(pv32$)) > CVD(monto) THEN mjfox% = 0: desc1(renglon) = "": code1(renglon) = "": cant1(renglon) = MKI$(0): prec1(renglon) = MKD$(0): cost1(renglon) = MKD$(0): LOCATE 22, 5: PRINT "Sobrepasa el limite de Credito": _
      Espera: GOSUB Limpio: RETURN
END IF
IF sit% = 4 THEN
	IF (CVD(total) + VAL(keya) * CVD(pv42$)) > CVD(monto) THEN mjfox% = 0: desc1(renglon) = "": code1(renglon) = "": cant1(renglon) = MKI$(0): prec1(renglon) = MKD$(0): cost1(renglon) = MKD$(0): LOCATE 22, 5: PRINT "Sobrepasa el limite de Credito": _
      Espera: GOSUB Limpio: RETURN
END IF
tour% = 1
IF renglon > 1 THEN
FOR k = 1 TO renglon - 1
IF codigo2$ = code1(k) THEN
	IF sit% = 1 THEN
		IF CVD(prec1(k)) = CVD(pv12$) THEN
				IF CVD(cost1(k)) = CVD(cos12$) THEN
					cant1(k) = MKI$(CVI(cant1(k)) + CVI(cant1(renglon))): total = MKD$((CVD(prec1(k)) * CVI(cant1(renglon))) + (CVD(total)))
					tour% = 2: k = renglon: RSET exi12$ = MKI$(CVI(exi12$) - CVI(cant1(renglon))): cant1(renglon) = MKI$(0)
					PUT #1, mjfox: CLOSE
				END IF
		END IF
	END IF
	IF sit% = 2 THEN
		IF CVD(prec1(k)) = CVD(pv22$) THEN
				IF CVD(cost1(k)) = CVD(cos22$) THEN
					cant1(k) = MKI$(CVI(cant1(k)) + CVI(cant1(renglon))): total = MKD$((CVD(prec1(k)) * CVI(cant1(renglon))) + (CVD(total)))
					tour% = 2: k = renglon: RSET exi22$ = MKI$(CVI(exi22$) - CVI(cant1(renglon))): cant1(renglon) = MKI$(0)
					PUT #1, mjfox: CLOSE
				END IF
		END IF
	END IF
	IF sit% = 3 THEN
		IF CVD(prec1(k)) = CVD(pv32$) THEN
				IF CVD(cost1(k)) = CVD(cos32$) THEN
					cant1(k) = MKI$(CVI(cant1(k)) + CVI(cant1(renglon))): total = MKD$((CVD(prec1(k)) * CVI(cant1(renglon))) + (CVD(total)))
					tour% = 2: k = renglon: RSET exi32$ = MKI$(CVI(exi32$) - CVI(cant1(renglon))): cant1(renglon) = MKI$(0)
					PUT #1, mjfox: CLOSE
				END IF
		END IF
	END IF
	IF sit% = 4 THEN
		IF CVD(prec1(k)) = CVD(pv32$) THEN
				IF CVD(cost1(k)) = CVD(cos42$) THEN
					cant1(k) = MKI$(CVI(cant1(k)) + CVI(cant1(renglon))): total = MKD$((CVD(prec1(k)) * CVI(cant1(renglon))) + (CVD(total)))
					tour% = 2: k = renglon: RSET exi42$ = MKI$(CVI(exi42$) - CVI(cant1(renglon))): cant1(renglon) = MKI$(0)
					PUT #1, mjfox: CLOSE
				END IF
		END IF
	END IF
END IF
NEXT k
END IF
SELECT CASE tour%
	CASE 1
		IF sit% = 1 THEN prec1(renglon) = pv12$: RSET exi12$ = MKI$(CVI(exi12$) - CVI(cant1(renglon))): ivaf1(renglon) = iv$: cost1(renglon) = cos12$
		IF sit% = 2 THEN prec1(renglon) = pv22$: RSET exi22$ = MKI$(CVI(exi22$) - CVI(cant1(renglon))): ivaf1(renglon) = iv$: cost1(renglon) = cos22$
		IF sit% = 3 THEN prec1(renglon) = pv32$: RSET exi32$ = MKI$(CVI(exi32$) - CVI(cant1(renglon))): ivaf1(renglon) = iv$: cost1(renglon) = cos32$
		IF sit% = 4 THEN
			prec1(renglon) = pv32$: RSET exi42$ = MKI$(CVI(exi42$) - CVI(cant1(renglon)))
			ivaf1(renglon) = iv$: cost1(renglon) = cos42$
		END IF
		regi2(renglon) = MID$(STR$(sit%), 2, 1): regi1(renglon) = MKI$(mjfox%): LOCATE 1, 1: IF CVI(ubic2$) = 1 THEN psic1(renglon) = "*"
REM        'LOCATE 1, 1: PRINT USING "(####)"; VAL(regi2(renglon)); renglon; mjfox%; CVI(regi1(renglon))
		code1(renglon) = codigo2$: desc1(renglon) = nomb2$
		total = MKD$(CVD(total) + CVD(prec1(renglon)) * CVI(cant1(renglon)))
		PUT #1, mjfox: CLOSE
	CASE 2
		renglon = renglon - 1
END SELECT
GOSUB Limpio: renglon = renglon + 1: IF renglon > 26 THEN Fun(17) = "no"
RETURN

Anula:
Box2 17, 10, 24, 70: LOCATE 20, 20: PRINT "¨ Desea LIMPIAR esta Nota ? "
keya$ = UCASE$(MeteDatoLetra$(20, 48, 1, "N", "0"))
IF MID$(keya$, 1, 1) = "s" OR MID$(keya$, 1, 1) = "S" THEN
	CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200: renglon = renglon - 1
	FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$ _
, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2  _
AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
	FOR k = 1 TO renglon
	LOCK #1, CVI(regi1(k))
	GET #1, CVI(regi1(k)): salir = false
	IF regi2(k) = "1" THEN
		IF CVD(prec1(k)) = CVD(pv12$) THEN
			IF ivaf1(k) = iv$ THEN
				IF CVD(cost1(k)) = CVD(cos12$) THEN
					RSET exi12$ = MKI$(CVI(exi12$) + CVI(cant1(k))): salir = true
				END IF
			END IF
		END IF
	END IF
	IF regi2(k) = "2" THEN
		IF CVD(prec1(k)) = CVD(pv22$) THEN
			IF ivaf1(k) = iv$ THEN
				IF CVD(cost1(k)) = CVD(cos22$) THEN
					RSET exi22$ = MKI$(CVI(exi22$) + CVI(cant1(k))): salir = true
				END IF
			END IF
		END IF
	END IF
	IF regi2(k) = "3" THEN
		IF CVD(prec1(k)) = CVD(pv32$) THEN
			IF ivaf1(k) = iv$ THEN
				IF CVD(cost1(k)) = CVD(cos32$) THEN
					RSET exi32$ = MKI$(CVI(exi32$) + CVI(cant1(k))): salir = true
				END IF
			END IF
		END IF
	END IF
	IF regi2(k) = "4" THEN
		IF CVD(prec1(k)) = CVD(pv32$) THEN
			IF ivaf1(k) = iv$ THEN
				IF CVD(cost1(k)) = CVD(cos42$) THEN
					RSET exi42$ = MKI$(CVI(exi42$) + CVI(cant1(k))): salir = true
				END IF
			END IF
		END IF
	END IF
	IF salir = true THEN
		PUT #1, CVI(regi1(k)): UNLOCK #1, CVI(regi1(k))
	ELSE
		PRINT "Problema con:"; desc1(k): Espera
	END IF
	NEXT k
	FOR k = 1 TO 26
		regi1(k) = MKI$(0): regi2(k) = " ": desc1(k) = "": prec1(k) = MKD$(0)
		cant1(k) = MKI$(0): cost1(k) = MKD$(0): ivaf1(k) = " ": psic1(k) = " "
	NEXT k
	CLOSE : total = MKD$(0): renglon = 1
END IF
GOSUB Limpia
RETURN

Cambiar:
	GOSUB Limpio: CALL Center(25, "Utilize las Teclas de Direccion y Presione [Enter] para Elegir"): sit% = 1: Center 20, "*** Proceso de Cambiar Articulos ***"
	COLOR colors(4, colorpref), colors(7, colorpref): CALL Box(2, 1, 16, 80)
	LOCATE 2, 24: PRINT "Â": LOCATE 2, 40: PRINT "ÂÂ": LOCATE 2, 29: PRINT "Â": LOCATE 2, 64: PRINT "Â": LOCATE 2, 69: PRINT "Â": LOCATE 16, 24: PRINT "Á": LOCATE 16, 29: PRINT "Á": LOCATE 16, 40: PRINT "ÁÁ"
	FOR k = 1 TO 13
	LOCATE k + 2, 1: PRINT "³"; MID$(desc1(k), 1, 22); "³"
	LOCATE k + 2, 40: PRINT "³³"; MID$(desc1(k + 13), 1, 22); "³"
	LOCATE k + 2, 25: IF CVI(cant1(k)) = 0 THEN PRINT "    ³" ELSE PRINT USING "####"; CVI(cant1(k)); : PRINT "³"
	LOCATE k + 2, 65: IF CVI(cant1(k + 13)) = 0 THEN PRINT "    ³" ELSE PRINT USING "####"; CVI(cant1(k + 13)); : PRINT "³"
	LOCATE k + 2, 30: IF CVD(prec1(k)) = 0 THEN PRINT "          ³" ELSE PRINT USING "######,.##"; CVD(prec1(k));
	LOCATE k + 2, 70: IF CVD(prec1(k + 13)) = 0 THEN PRINT "          ³" ELSE PRINT USING "######,.##"; CVD(prec1(k + 13)); : PRINT "³"
	NEXT k
	COLOR colors(7, colorpref), colors(4, colorpref): LOCATE 18, 1: PRINT STRING$(80, "Ä"): COLOR colors(7, colorpref), colors(4, colorpref)
	COLOR colors(7, colorpref) + 16, colors(4, colorpref): LOCATE 2, 45
	PRINT "** SUB - TOTAL ="; : PRINT USING "########,.##"; CVD(total); : PRINT " Bs. **"
	COLOR colors(7, colorpref), colors(4, colorpref)
DO
	IF old% < 14 THEN LOCATE old% + 2, 2 ELSE LOCATE old% - 11, 42
	COLOR colors(4, colorpref), colors(7, colorpref): PRINT MID$(desc1(old%), 1, 22); "³"; : PRINT USING "####"; CVI(cant1(old%)); : PRINT "³"; : PRINT USING "######,.##"; CVD(prec1(old%));
	IF sit% < 14 THEN LOCATE sit% + 2, 2 ELSE LOCATE sit% - 11, 42
	COLOR colors(7, colorpref), colors(4, colorpref): PRINT MID$(desc1(sit%), 1, 22); "³"; : PRINT USING "####"; CVI(cant1(sit%)); : PRINT "³"; : PRINT USING "######,.##"; CVD(prec1(sit%));
	llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
	SELECT CASE llave$
		CASE CHR$(0) + "H": old% = sit%: sit% = sit% - 1: IF sit% < 1 THEN sit% = 1: IF sonido = "si" THEN BEEP
		CASE CHR$(0) + "P": old% = sit%: sit% = sit% + 1: IF sit% > (renglon - 1) THEN sit% = renglon - 1: IF sonido = "si" THEN BEEP
		CASE CHR$(13): EXIT DO
		CASE CHR$(27): GOSUB Limpia: RETURN
		CASE ELSE: IF sonido = "si" THEN BEEP
	END SELECT
LOOP
salir = false: provi$ = cant1(sit%): canti$ = "0"
Center 24, "Introduzca La Cantidad Que Desea Vender , NO LA QUE VA A DEVOLVER ."
DO WHILE NOT salir
IF sit% < 14 THEN
canti$ = EntraDatoNumero$(sit% + 2, 24, 4, STR$(CVI(provi$)), 0)
ELSE
canti$ = EntraDatoNumero$(sit% - 11, 64, 4, STR$(CVI(provi$)), 0)
END IF
IF ABS(VAL(canti$)) > CVI(provi$) THEN Center 20, "La Cantidad DEBE Ser Menor o Igual a Lo Que Esta Facturado:" + STR$(CVI(provi$)): Espera ELSE salir = true
LOOP
canti$ = STR$((CVI(cant1(sit%)) - ABS(VAL(canti$))))
IF VAL(canti$) = 0 THEN GOSUB Limpio: RETURN
CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
LOCK #1, CVI(regi1(sit%)) :
GET #1, CVI(regi1(sit%)): salir = false
	IF regi2(sit%) = "1" THEN
		IF CVD(prec1(sit%)) = CVD(pv12$) THEN
			IF ivaf1(sit%) = iv$ THEN
				IF CVD(cost1(sit%)) = CVD(cos12$) THEN
					RSET exi12$ = MKI$(CVI(exi12$) + VAL(canti$)): salir = true
				END IF
			END IF
		END IF
	END IF
	IF regi2(sit%) = "2" THEN
		IF CVD(prec1(sit%)) = CVD(pv22$) THEN
			IF ivaf1(sit%) = iv$ THEN
				IF CVD(cost1(sit%)) = CVD(cos22$) THEN
					RSET exi22$ = MKI$(CVI(exi22$) + VAL(canti$)): salir = true
				END IF
			END IF
		END IF
	END IF
	IF regi2(sit%) = "3" THEN
		IF CVD(prec1(sit%)) = CVD(pv32$) THEN
			IF ivaf1(sit%) = iv$ THEN
				IF CVD(cost1(sit%)) = CVD(cos32$) THEN
					RSET exi32$ = MKI$(CVI(exi32$) + VAL(canti$)): salir = true
				END IF
			END IF
		END IF
	END IF
	IF regi2(sit%) = "4" THEN
		IF CVD(prec1(sit%)) = CVD(pv32$) THEN
			IF ivaf1(sit%) = iv$ THEN
				IF CVD(cost1(sit%)) = CVD(cos42$) THEN
					RSET exi42$ = MKI$(CVI(exi42$) + VAL(canti$)): salir = true
				END IF
			END IF
		END IF
	END IF
	IF salir = true THEN
		PUT #1, CVI(regi1(sit%)): UNLOCK #1, CVI(regi1(sit%))
	ELSE
		PRINT "Problema con:"; desc1(sit%): Espera
	END IF: CLOSE
total = MKD$(CVD(total) - CVD(prec1(sit%)) * VAL(canti$))
IF VAL(canti$) = CVI(cant1(sit%)) THEN
	renglon = renglon - 1
	FOR k = sit% TO renglon
	regi1(k) = regi1(k + 1): regi2(k) = regi2(k + 1): desc1(k) = desc1(k + 1): psic1(k) = psic1(k + 1)
	cant1(k) = cant1(k + 1): cost1(k) = cost1(k + 1): ivaf1(k) = ivaf1(k + 1): prec1(k) = prec1(k + 1)
	NEXT k
ELSE
cant1(sit%) = MKI$((CVI(cant1(sit%)) - (VAL(canti$))))
END IF
GOSUB Limpio
RETURN

Mostrar:
COLOR colors(4, colorpref), colors(7, colorpref): GOSUB Limpio
CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
GET #1, mjfox%
LOCATE 17, 1: PRINT "Codigo Nombre                             Ofertas Observaciones"
LOCATE 18, 1: PRINT codigo2$; " ";
COLOR colors(7, colorpref) + 16, colors(4, colorpref)
PRINT nomb2$; : COLOR colors(4, colorpref), colors(7, colorpref)
PRINT " "; of12$; " "; of22$; " "; of32$; obser2$
Box 19, 1, 22, 80: wer$ = "En Oferta:" + STRING$(12, "Ä") + "ÂCAN1ÄÄÄÄÄÄÄÄÄPVP1ÂCAN2" + STRING$(10, "Ä") + "PVP2ÂCAN3" + STRING$(9, "Ä") + "PVP3"
LOCATE 19, 2: PRINT wer$; : LOCATE 21, 2: PRINT "Costos:"
LOCATE 20, 24: PRINT "³"; : PRINT USING "#####,"; CVI(exi12$);
LOCATE 20, 42: PRINT "³"; : PRINT USING "#####,"; CVI(exi22$);
LOCATE 20, 61: PRINT "³"; : PRINT USING "#####,"; CVI(exi32$)
LOCATE 20, 32: PRINT USING "######,.##"; CVD(pv12$): LOCATE 20, 51: PRINT USING "######,.##"; CVD(pv22$)
LOCATE 20, 70: PRINT USING "######,.##"; CVD(pv32$)
LOCATE 20, 2: PRINT USING "#####,"; CVI(exi42$); : PRINT " a Bs.";
PRINT USING "######,.##"; CVD(pv42$)
LOCATE 21, 32: PRINT USING "######,.##"; CVD(cos12$); : PRINT "³";
LOCATE 21, 51: PRINT USING "######,.##"; CVD(cos22$); : PRINT "³";
LOCATE 21, 70: PRINT USING "######,.##"; CVD(cos32$); : PRINT "³";
LOCATE 21, 14: PRINT USING "######,.##"; CVD(cos42$); : PRINT "³";
LOCATE 22, 23: PRINT "ÄÁ"; : LOCATE 22, 41: PRINT "ÄÁ"; : LOCATE 22, 60: PRINT "ÄÁ";
LOCATE 23, 1: PRINT "[Minimo1="; : PRINT USING "####"; CVI(mini12$);
PRINT "] [Minimo2="; : PRINT USING "####"; CVI(mini22$);
PRINT "] [Maximo1="; : PRINT USING "####"; CVI(maxi12$);
PRINT "] [Maximo2="; : PRINT USING "####"; CVI(maxi22$); : PRINT "]";
IF iv$ = "*" THEN PRINT " Paga I.C.S.V.M." ELSE PRINT " Producto Exento"
IF Fun(17) = "no" THEN Espera
RETURN
END SUB

