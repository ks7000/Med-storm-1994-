'devolucion de productos de contado
DEFINT A-Z
DECLARE SUB NotadeCredito ()
DECLARE SUB Espera ()
DECLARE SUB Box (Row1, Col1, Row2, Col2)
DECLARE SUB Box2 (Row1, Col1, Row2, Col2)
DECLARE SUB Center (row, text$)
DECLARE SUB FancyCls (dots%, Background%)
DECLARE SUB LeeEstado ()
DECLARE SUB Imprimir (cedu0$, nombre0$, rifo0$, nito0$, domicilio10$, domicilio20$, tlf0$, dia$)
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$, tr$)
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Escoge% (fila%, edo%, fila2%, Prod$)
DECLARE FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%)
DECLARE FUNCTION Trim$ (x$)
DECLARE FUNCTION CheqDate (caden$, op%)
DECLARE FUNCTION Verify% (ubi%, text$)
COMMON SHARED guia, colorpref
DIM SHARED nursaler, ybusqueda
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, impuesto AS STRING * 6, NombFarma AS STRING * 25, Direccion AS STRING * 30, Rif     AS STRING * 25, Nit      AS STRING * 25, telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2
DIM SHARED regi1(26) AS STRING * 2, regi2(26) AS STRING * 1, desc1(26) AS STRING * 35
DIM SHARED cant1(26) AS STRING * 2, cost1(26) AS STRING * 8, ivaf1(26) AS STRING * 1
DIM SHARED prec1(26) AS STRING * 8, code1(26) AS STRING * 6, psic1(26) AS STRING * 1, cobrado1(26) AS STRING * 8
DIM SHARED PrintErr AS INTEGER, devolver, Fun(17) AS STRING * 2, sitio, salir
DIM SHARED renglon AS INTEGER, total AS STRING * 8, keya AS STRING * 6, NuevoIva AS STRING * 2, ShowPerc AS STRING * 2
DIM SHARED fx AS STRING * 33, fx2 AS STRING * 33, notac AS STRING * 4, encabezado AS STRING * 2
DIM SHARED Yname1(15) AS STRING * 3, Yname2(15) AS STRING * 15
DIM SHARED Nombra(5) AS STRING * 35, Pvp1a(5) AS STRING * 8, Pvp2a(5) AS STRING * 8, Pvp3a(5) AS STRING * 8, Pvp4a(5) AS STRING * 8, Numa(5) AS STRING * 2, Exis1a(5) AS STRING * 2, Exis2a(5) AS STRING * 2, Exis3a(5) AS STRING * 2, Exis4a(5) AS  _
STRING * 2
DIM SHARED Cant1a(5) AS STRING * 2, Cant2a(5) AS STRING * 2, Cant3a(5) AS STRING * 2, Cant4a(5) AS STRING * 2, grabGav AS STRING * 1
CONST true = -1: CONST false = NOT true:  ybusqueda = 0
ON TIMER(5) GOSUB AbreGav
ON ERROR GOTO ErrorTrap
CALL LeeEstado: CALL NotadeCredito
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... "
CHAIN "F:\kevinprg\kevin01.bas"
END

ErrorTrap:
SELECT CASE ERR
	CASE 25, 27, 57, 68
		Box2 5, 2, 7, 78: Center 5, "Mensaje": Center 6, "No Se Detect¢ Gavetero;NO DEBE Usar Esta Estacion Para Ventas de Contado": RESUME NEXT
	CASE 70
		Center 25, "Esperando Acceso a La Red , Espere Por Favor . . .": SLEEP 1: BEEP: RESUME
	CASE ELSE
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

FUNCTION CheqDate (caden$, op%)
CheqDate = true: IF caden$ = "          " THEN EXIT FUNCTION
'formato dd/mm/aaaa
dd% = VAL(MID$(caden$, 1, 2))
mm% = VAL(MID$(caden$, 4, 2))
aa% = VAL(MID$(caden$, 7, 4))
dd2% = VAL(MID$(DATE$, 4, 2))
mm2% = VAL(MID$(DATE$, 1, 2))
aa2% = VAL(MID$(DATE$, 7, 4))
IF dd% < 1 THEN CheqDate = false: EXIT FUNCTION
IF mm% < 1 OR mm% > 12 THEN CheqDate = false: EXIT FUNCTION
IF aa% < 1996 OR aa% > 2020 THEN CheqDate = false: EXIT FUNCTION
IF mm% = 2 AND dd% > 29 THEN CheqDate = false: EXIT FUNCTION
SELECT CASE mm%
	CASE 1, 3, 5, 7, 8, 10, 12
		IF dd% > 31 THEN CheqDate = false: EXIT FUNCTION
	CASE 4, 6, 9, 11
		IF dd% > 30 THEN CheqDate = false: EXIT FUNCTION
END SELECT
IF op% = 2 THEN
		IF aa% < aa2% THEN CheqDate = false: EXIT FUNCTION
		IF aa% = aa2% THEN IF mm% < mm2% THEN CheqDate = false: EXIT FUNCTION
		IF aa% = aa2% THEN IF mm% = mm2% THEN IF dd% < dd2% THEN CheqDate = false: EXIT FUNCTION
END IF
END FUNCTION

FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%)
COLOR colors(7, colorpref), colors(4, colorpref): CALL Box(fila%, columna%, fila% + 16, columna% + 16): CALL Center(25, "[F1] Ayuda [Enter]Tomar Opcion [A-Z]Ir a... [Esc]Menu Anterior"): COLOR colors(4, colorpref), colors(7, colorpref): CLOSE #5
SELECT CASE estesi%
	CASE 1
		LOCATE fila%, columna% + 2: PRINT "Ubicacion"
		OPEN "F:\kevinprg\kevindat\ubica.dat" FOR RANDOM SHARED AS #5 LEN = 18: FIELD #5, 3 AS codigo$, 15 AS nomb$
		FOR k = 1 TO 15: GET #5, k: Yname1(k) = codigo$: Yname2(k) = nomb$: NEXT k: selecc% = 199
	CASE 2
		LOCATE fila%, columna% + 1: PRINT "Laboratorio"
		OPEN "F:\kevinprg\kevindat\lab.dat" FOR RANDOM SHARED AS #5 LEN = 18: FIELD #5, 3 AS codigo$, 15 AS nomb$
		FOR k = 1 TO 15: GET #5, k: Yname1(k) = codigo$: Yname2(k) = nomb$: NEXT k: selecc% = 999
	CASE 4
		LOCATE fila%, columna% + 1: PRINT "Componente "
		OPEN "F:\kevinprg\kevindat\componen.dat" FOR RANDOM SHARED AS #5 LEN = 18: FIELD #5, 3 AS codigo$, 15 AS nomb$
		FOR k = 1 TO 15: GET #5, k: Yname1(k) = codigo$: Yname2(k) = nomb$: NEXT k: selecc% = 999
	CASE 5
		LOCATE fila%, columna% + 1: PRINT " Proveedor "
		OPEN "F:\kevinprg\kevindat\lab.dat" FOR RANDOM SHARED AS #5 LEN = 18: FIELD #5, 3 AS codigo$, 15 AS nomb$
		FOR k = 1 TO 15: GET #5, k: Yname1(k) = codigo$: Yname2(k) = nomb$: NEXT k: selecc% = 199
	CASE 6
		LOCATE fila%, columna% + 1: PRINT " Banco     "
		OPEN "F:\kevinprg\kevindat\bank.dat" FOR RANDOM SHARED AS #5 LEN = 18: FIELD #5, 3 AS codigo$, 15 AS nomb$
		FOR k = 1 TO 15: GET #5, k: Yname1(k) = codigo$: Yname2(k) = nomb$: NEXT k: selecc% = 199
	CASE 7
		LOCATE fila%, columna% + 1: PRINT " Cliente   "
		OPEN "F:\kevinprg\kevindat\clientes.dat" FOR RANDOM SHARED AS #5 LEN = 38: FIELD #5, 3 AS codigo$, 35 AS nomb$
		FOR k = 1 TO 15: GET #5, k: Yname1(k) = codigo$: Yname2(k) = nomb$: NEXT k: selecc% = 999
END SELECT
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
	FOR k = 1 TO up% + 15: l = l + 1
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
FOR k = 1 TO largo%   'si entero%=1 entonces se esta metiendo decimal
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
STATIC max2%, Prud AS STRING * 6: CALL Center(25, "[F1] Ayuda [F9] Salir SIN Escoger [Enter] Toma Articulo [A-Z] Ir a")
Prod$ = "      ": Fun(9) = "no"
DO WHILE Prod$ = "      "
	LOCATE fila2%, 5: PRINT "Introduzca Orden de Busqueda:"; SPACE$(15)
	IF Fun(17) = "op" THEN
		Prod$ = MeteDatoLetra(fila2%, 34, 6, keya, "1")
	ELSE
		Prod$ = MeteDatoLetra(fila2%, 34, 6, keya, "0")
	END IF
	Prud = Prod$: Prod$ = Trim$(Prod$)
	IF Fun(9) = "si" OR Prod$ = "" THEN : Escoge% = -1: EXIT FUNCTION
LOOP: selecc% = 0
CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
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

SUB Imprimir (cedu0$, nombre0$, rifo0$, nito0$, domicilio10$, domicilio20$, tlf0$, dia$)
OPEN "lpt1:" FOR OUTPUT AS #10
WIDTH #10, 255
PRINT #10, CHR$(27); CHR$(64); : PRINT #10, CHR$(27); CHR$(64);
PRINT #10, CHR$(15);
					
PRINT #10, USING "      NotaCre:######,"; CVL(notac); : PRINT #10, " Fecha:"; dia$; " Hora:"; TIME$;
PRINT #10, " Pagina:1 de ";
aex% = renglon - 1
SELECT CASE aex%
	 CASE 1, 2, 3, 4, 5, 6, 7
			PRINT #10, "1"; SPACE$(9);
	 CASE 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21
			PRINT #10, "2"; SPACE$(9);
	 CASE 22, 23, 24, 25, 25
			PRINT #10, "3"; SPACE$(9);
END SELECT
PRINT #10, USING "NotaCre:######,"; CVL(notac); : PRINT #10, " Fecha:"; dia$; " Hora:"; TIME$;
PRINT #10, " Pagina:1 de ";
SELECT CASE aex%
	 CASE 1, 2, 3, 4, 5, 6, 7
			PRINT #10, "1";
	 CASE 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21
			PRINT #10, "2";
	 CASE 22, 23, 24, 25, 26
			PRINT #10, "3";
END SELECT
PRINT #10, CHR$(10);
					 
PRINT #10, "      Cliente:                               Vendedor:"; cedu0$; SPACE$(11);
PRINT #10, "Cliente:                               Vendedor:"; cedu0$; CHR$(10);

PRINT #10, "      "; nombre0$; SPACE$(29);
PRINT #10, nombre0$; CHR$(10);

rifo0$ = Trim$(rifo0$): nito0$ = Trim$(nito0$)
PRINT #10, "      R.I.F.:"; rifo0$; SPACE$(22 - LEN(rifo0$)); " N.I.T.:"; nito0$; SPACE$(23 - LEN(nito0$)); SPACE$(9);
PRINT #10, "R.I.F.:"; rifo0$; SPACE$(22 - LEN(rifo0$)); " N.I.T.:"; nito0$; SPACE$(23 - LEN(nito0$)); CHR$(10);

domicilio10$ = Trim$(domicilio10$): domicilio20$ = Trim$(domicilio20$): tlf0$ = Trim$(tlf0$)
PRINT #10, "      Domicilio Fiscal:"; SPACE$(52);
PRINT #10, "Domicilio Fiscal:"; CHR$(10);
					 
PRINT #10, "      "; domicilio10$; SPACE$(60 - LEN(domicilio10$)); SPACE$(9);
PRINT #10, domicilio10$; CHR$(10);
					 
PRINT #10, "      "; domicilio20$; SPACE$(60 - LEN(domicilio20$)); SPACE$(9);
PRINT #10, domicilio20$; CHR$(10);
					 
PRINT #10, "      Telefonos:"; tlf0$; SPACE$(20 - LEN(tlf0$)); " Forma de Pago: Contado       "; SPACE$(9);
PRINT #10, "Telefonos:"; tlf0$; SPACE$(20 - LEN(tlf0$)); " Forma de Pago: Contado"; CHR$(10);

PRINT #10, "      "; STRING$(60, "-"); SPACE$(9);
PRINT #10, STRING$(60, "-"); CHR$(10);
					 
renglon = renglon - 1: auxe$ = MKD$(0)
PRINT #10, "      ³Descripcion                        ³Cantidad³PrecioCobrad³ "; SPACE$(9);
PRINT #10, "³Descripcion                        ³Cantidad³PrecioCobrad³ "; CHR$(10);

SELECT CASE renglon
		 CASE 1 TO 7
					FOR k = 1 TO renglon
							 PRINT #10, "      ³"; desc1(k); "³";
							 PRINT #10, USING "#######,³"; CVI(cant1(k));
							 IF ivaf1(k) = "*" THEN
										IF NuevoIva = "si" THEN
												 PRINT #10, USING "########,.##³"; (CVD(cobrado1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
												 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(cobrado1(k)) / (1 + (VAL(impuesto) / 100)))))
										ELSE
												 PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
												 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(cobrado1(k)))))
										END IF
							 ELSE
										PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
										auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(cobrado1(k))))
							 END IF
													
							 PRINT #10, SPACE$(9); "³"; desc1(k); "³";
							 PRINT #10, USING "#######,³"; CVI(cant1(k));
							 IF ivaf1(k) = "*" THEN
										IF NuevoIva = "si" THEN
												 PRINT #10, USING "########,.##³"; (CVD(cobrado1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
										ELSE
												 PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
										END IF
							 ELSE
										PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
							 END IF
							 PRINT #10, CHR$(10);
					NEXT k
		 CASE 8 TO 21
					ler% = 1
					FOR k = 1 TO 15
							 IF k <= renglon THEN
									PRINT #10, "      ³"; desc1(k); "³";
									PRINT #10, USING "#######,³"; CVI(cant1(k));
									IF ivaf1(k) = "*" THEN
											 IF NuevoIva = "si" THEN
														PRINT #10, USING "########,.##³"; (CVD(cobrado1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
														auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(cobrado1(k)) / (1 + (VAL(impuesto) / 100)))))
											 ELSE
														PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
														auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(cobrado1(k)))))
											 END IF
									ELSE
											 PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
											 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(cobrado1(k))))
									END IF
														
									PRINT #10, SPACE$(9); "³"; desc1(k); "³";
									PRINT #10, USING "#######,³"; CVI(cant1(k));
									IF ivaf1(k) = "*" THEN
											 IF NuevoIva = "si" THEN
														PRINT #10, USING "########,.##³"; (CVD(cobrado1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
											 ELSE
														PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
											 END IF
									ELSE
											 PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
									END IF
									PRINT #10, CHR$(10);
							 ELSE
									PRINT #10, SPACE$(6); SPACE$(ler%); " \ \ \ \"; SPACE$(60 - ler% - 8); SPACE$(9);
									PRINT #10, SPACE$(ler%); " \ \ \ \"; CHR$(10); : ler% = ler% + 1
							 END IF
					NEXT k
											
					PRINT #10, SPACE$(6); SPACE$(35); "VAN --> "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); : PRINT #10, SPACE$(9);
					PRINT #10, SPACE$(35); "VAN --> "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); CHR$(10);
										 
										 
					PRINT #10, USING "      NotaCre:######,"; CVL(notac); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
					PRINT #10, " Pagina:2 de 2"; SPACE$(9);
					PRINT #10, USING "NotaCre:######,"; CVL(notac); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
					PRINT #10, " Pagina:2 de 2"; CHR$(10);
											
					PRINT #10, "      Cliente:                               Vendedor:"; cedu0$; SPACE$(11);
					PRINT #10, "Cliente:                               Vendedor:"; cedu0$; CHR$(10);

					PRINT #10, "      "; nombre0$; SPACE$(29);
					PRINT #10, nombre0$; CHR$(10);

					rifo0$ = Trim$(rifo0$): nito0$ = Trim$(nito0$)
					PRINT #10, "      R.I.F.:"; rifo0$; SPACE$(22 - LEN(rifo0$)); " N.I.T.:"; nito0$; SPACE$(23 - LEN(nito0$)); SPACE$(9);
					PRINT #10, "R.I.F.:"; rifo0$; SPACE$(22 - LEN(rifo0$)); " N.I.T.:"; nito0$; SPACE$(23 - LEN(nito0$)); CHR$(10);
										 
					domicilio10$ = Trim$(domicilio10$): domicilio20$ = Trim$(domicilio20$)
					PRINT #10, "      Domicilio Fiscal:"; SPACE$(52);
					PRINT #10, "Domicilio Fiscal:"; CHR$(10);
											
					PRINT #10, "      "; domicilio10$; SPACE$(60 - LEN(domicilio10$)); SPACE$(9);
					PRINT #10, domicilio10$; CHR$(10);
											
					PRINT #10, "      "; domicilio20$; SPACE$(60 - LEN(domicilio20$)); SPACE$(9);
					PRINT #10, domicilio20$; CHR$(10);
															 
					PRINT #10, "      Telefonos:"; tlf0$; SPACE$(20 - LEN(tlf0$)); " Forma de Pago: Contado       "; SPACE$(9);
					PRINT #10, "Telefonos:"; tlf0$; SPACE$(20 - LEN(tlf0$)); " Forma de Pago: Contado"; CHR$(10);
										 
					PRINT #10, "      "; STRING$(60, "-"); SPACE$(9);
					PRINT #10, STRING$(60, "-"); CHR$(10);
											
					PRINT #10, "      ³Descripcion                        ³Cantidad³PrecioCobrad³ "; SPACE$(9);
					PRINT #10, "³Descripcion                        ³Cantidad³PrecioCobrad³ "; CHR$(10);

					PRINT #10, SPACE$(6); SPACE$(35); "VIENEN->"; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$);
					PRINT #10, SPACE$(9); SPACE$(35); "VIENEN->"; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$);
					PRINT #10, CHR$(10);

					FOR k = 16 TO 21
							 IF k <= renglon THEN
									PRINT #10, "      ³"; desc1(k); "³";
									PRINT #10, USING "#######,³"; CVI(cant1(k));
									IF ivaf1(k) = "*" THEN
											 IF NuevoIva = "si" THEN
														PRINT #10, USING "########,.##³"; (CVD(cobrado1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
														auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(cobrado1(k)) / (1 + (VAL(impuesto) / 100)))))
											 ELSE
														PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
														auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(cobrado1(k)))))
											 END IF
									ELSE
											 PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
											 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(cobrado1(k))))
									END IF
													 
									PRINT #10, SPACE$(9); "³"; desc1(k); "³";
									PRINT #10, USING "#######,³"; CVI(cant1(k));
									IF ivaf1(k) = "*" THEN
											 IF NuevoIva = "si" THEN
														PRINT #10, USING "########,.##³"; (CVD(cobrado1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
											 ELSE
														PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
											 END IF
									ELSE
											 PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
									END IF
									PRINT #10, CHR$(10);
							 END IF
					NEXT k

		 CASE 22 TO 26
					ler% = 1
					FOR k = 1 TO 15
							 IF k <= renglon THEN
									PRINT #10, "      ³"; desc1(k); "³";
									PRINT #10, USING "#######,³"; CVI(cant1(k));
									IF ivaf1(k) = "*" THEN
											 IF NuevoIva = "si" THEN
														PRINT #10, USING "########,.##³"; (CVD(cobrado1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
														auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(cobrado1(k)) / (1 + (VAL(impuesto) / 100)))))
											 ELSE
														PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
														auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(cobrado1(k)))))
											 END IF
									ELSE
											 PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
											 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(cobrado1(k))))
									END IF
													 
									PRINT #10, SPACE$(9); "³"; desc1(k); "³";
									PRINT #10, USING "#######,³"; CVI(cant1(k));
									IF ivaf1(k) = "*" THEN
											 IF NuevoIva = "si" THEN
														PRINT #10, USING "########,.##³"; (CVD(cobrado1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
											 ELSE
														PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
											 END IF
									ELSE
											 PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
									END IF
									PRINT #10, CHR$(10);
							 ELSE
									PRINT #10, SPACE$(6); SPACE$(ler%); " \ \ \ \"; SPACE$(60 - ler% - 8); SPACE$(9);
									PRINT #10, SPACE$(ler%); " \ \ \ \"; CHR$(10); : ler% = ler% + 1
							 END IF
					NEXT k
										 
					PRINT #10, SPACE$(6); SPACE$(35); "VAN --> "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); : PRINT #10, SPACE$(9);
					PRINT #10, SPACE$(35); "VAN --> "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); CHR$(10);
										
					FOR mrw% = 1 TO 9: PRINT #10, CHR$(10); : NEXT mrw%
					PRINT #10, USING "      NotaCre:######,"; CVL(notac); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
					PRINT #10, " Pagina:2 de 3"; SPACE$(9);
					PRINT #10, USING "NotaCre:######,"; CVL(notac); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
					PRINT #10, " Pagina:2 de 3"; CHR$(10);
										 
					PRINT #10, "      Cliente:                               Vendedor:"; cedu0$; SPACE$(11);
					PRINT #10, "Cliente:                               Vendedor:"; cedu0$; CHR$(10);

					PRINT #10, "      "; nombre0$; SPACE$(29);
					PRINT #10, nombre0$; CHR$(10);

					rifo0$ = Trim$(rifo0$): nito0$ = Trim$(nito0$)
					PRINT #10, "      R.I.F.:"; rifo0$; SPACE$(22 - LEN(rifo0$)); " N.I.T.:"; nito0$; SPACE$(23 - LEN(nito0$)); SPACE$(9);
					PRINT #10, "R.I.F.:"; rifo0$; SPACE$(22 - LEN(rifo0$)); " N.I.T.:"; nito0$; SPACE$(23 - LEN(nito0$)); CHR$(10);
										
					domicilio10$ = Trim$(domicilio10$): domicilio20$ = Trim$(domicilio20$)
					PRINT #10, "      Domicilio Fiscal:"; SPACE$(52);
					PRINT #10, "Domicilio Fiscal:"; CHR$(10);
										 
					PRINT #10, "      "; domicilio10$; SPACE$(60 - LEN(domicilio10$)); SPACE$(9);
					PRINT #10, domicilio10$; CHR$(10);
										 
					PRINT #10, "      "; domicilio20$; SPACE$(60 - LEN(domicilio20$)); SPACE$(9);
					PRINT #10, domicilio20$; CHR$(10);
															
					PRINT #10, "      Telefonos:"; tlf0$; SPACE$(20 - LEN(tlf0$)); " Forma de Pago: Contado       "; SPACE$(9);
					PRINT #10, "Telefonos:"; tlf0$; SPACE$(20 - LEN(tlf0$)); " Forma de Pago: Contado"; CHR$(10);
										
					PRINT #10, "      "; STRING$(60, "-"); SPACE$(9);
					PRINT #10, STRING$(60, "-"); CHR$(10);
										 
					PRINT #10, "      ³Descripcion                        ³Cantidad³PrecioCobrad³ "; SPACE$(9);
					PRINT #10, "³Descripcion                        ³Cantidad³PrecioCobrad³ "; CHR$(10);

					PRINT #10, SPACE$(6); SPACE$(35); "VIENEN->";
					PRINT #10, USING "##########,.## Bs"; CVD(auxe$);
					PRINT #10, SPACE$(9); SPACE$(35); "VIENEN->";
					PRINT #10, USING "##########,.## Bs"; CVD(auxe$);
					PRINT #10, CHR$(10);
										 
					ler% = 1
					FOR k = 16 TO 26
							 IF k <= renglon THEN
									PRINT #10, "      ³"; desc1(k); "³";
									PRINT #10, USING "#######,³"; CVI(cant1(k));
									IF ivaf1(k) = "*" THEN
											 IF NuevoIva = "si" THEN
														PRINT #10, USING "########,.##³"; (CVD(cobrado1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
														auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(cobrado1(k)) / (1 + (VAL(impuesto) / 100)))))
											 ELSE
														PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
														auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(cobrado1(k)))))
											 END IF
									ELSE
											 PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
											 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(cobrado1(k))))
									END IF
													
									PRINT #10, SPACE$(9); "³"; desc1(k); "³";
									PRINT #10, USING "#######,³"; CVI(cant1(k));
									IF ivaf1(k) = "*" THEN
											 IF NuevoIva = "si" THEN
														PRINT #10, USING "########,.##³"; (CVD(cobrado1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
											 ELSE
														PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
											 END IF
									ELSE
											 PRINT #10, USING "########,.##³ "; CVD(cobrado1(k));
									END IF
									PRINT #10, CHR$(10);
							 ELSE
									PRINT #10, SPACE$(6); SPACE$(ler%); " \ \ \ \"; SPACE$(60 - ler% - 8); SPACE$(9);
									PRINT #10, SPACE$(ler%); " \ \ \ \"; CHR$(10); : ler% = ler% + 1
							 END IF
					NEXT k
										
					PRINT #10, SPACE$(6); SPACE$(35); "VAN --> "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); : PRINT #10, SPACE$(9);
					PRINT #10, SPACE$(35); "VAN --> "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); CHR$(10);
									 
					PRINT #10, CHR$(12);

					PRINT #10, USING "      NotaCre:######,"; CVL(notac); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
					PRINT #10, " Pagina:3 de 3"; SPACE$(9);
					PRINT #10, USING "NotaCre:######,"; CVL(notac); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
					PRINT #10, " Pagina:3 de 3"; CHR$(10);
										
					PRINT #10, "      Cliente:                               Vendedor:"; cedu0$; SPACE$(11);
					PRINT #10, "Cliente:                               Vendedor:"; cedu0$; CHR$(10);

					PRINT #10, "      "; nombre0$; SPACE$(29);
					PRINT #10, nombre0$; CHR$(10);

					rifo0$ = Trim$(rifo0$): nito0$ = Trim$(nito0$)
					PRINT #10, "      R.I.F.:"; rifo0$; SPACE$(22 - LEN(rifo0$)); " N.I.T.:"; nito0$; SPACE$(23 - LEN(nito0$)); SPACE$(9);
					PRINT #10, "R.I.F.:"; rifo0$; SPACE$(22 - LEN(rifo0$)); " N.I.T.:"; nito0$; SPACE$(23 - LEN(nito0$)); CHR$(10);
									 
					domicilio10$ = Trim$(domicilio10$): domicilio20$ = Trim$(domicilio20$)
					PRINT #10, "      Domicilio Fiscal:"; SPACE$(52);
					PRINT #10, "Domicilio Fiscal:"; CHR$(10);
										
					PRINT #10, "      "; domicilio10$; SPACE$(60 - LEN(domicilio10$)); SPACE$(9);
					PRINT #10, domicilio10$; CHR$(10);
										
					PRINT #10, "      "; domicilio20$; SPACE$(60 - LEN(domicilio20$)); SPACE$(9);
					PRINT #10, domicilio20$; CHR$(10);
														 
					PRINT #10, "      Telefonos:"; tlf0$; SPACE$(20 - LEN(tlf0$)); " Forma de Pago: Contado       "; SPACE$(9);
					PRINT #10, "Telefonos:"; tlf0$; SPACE$(20 - LEN(tlf0$)); " Forma de Pago: Contado"; CHR$(10);
									 
					PRINT #10, "      "; STRING$(60, "-"); SPACE$(9);
					PRINT #10, STRING$(60, "-"); CHR$(10);
										
					PRINT #10, "      ³Descripcion                        ³Cantidad³PrecioCobrad³ "; SPACE$(9);
					PRINT #10, "³Descripcion                        ³Cantidad³PrecioCobrad³ "; CHR$(10);

					PRINT #10, SPACE$(6); SPACE$(35); "VIENEN->"; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$);
					PRINT #10, SPACE$(9); SPACE$(35); "VIENEN->"; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$);
					PRINT CHR$(10);

END SELECT
						
'-- 1
PRINT #10, "      "; SPACE$(45); STRING$(15, "="); SPACE$(9);
PRINT #10, SPACE$(45); STRING$(15, "="); CHR$(10);
						
'-- 2
PRINT #10, "      "; SPACE$(30); "Sub - Total: "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); : PRINT #10, SPACE$(9);
PRINT #10, SPACE$(30); "Sub - Total: "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); : PRINT #10, CHR$(10);
						
'-- 3
PRINT #10, CHR$(10);
						
'-- 4
PRINT #10, CHR$(10);
						
'-- 5
IF NuevoIva = "si" THEN
		 PRINT #10, SPACE$(6); SPACE$(24); "+Impuesto("; impuesto; "%):";
		 PRINT #10, USING "##########,.## Bs"; CVD(total) - CVD(auxe$);
		 PRINT #10, SPACE$(9);
END IF
IF NuevoIva = "si" THEN
		 PRINT #10, SPACE$(24); "+Impuesto("; impuesto; "%):";
		 PRINT #10, USING "##########,.## Bs"; CVD(total) - CVD(auxe$);
		 PRINT #10, CHR$(10);
ELSE
		 PRINT #10, CHR$(10);
END IF
						
'-- 6
PRINT #10, SPACE$(6); SPACE$(30); "Neto NotaCre:"; : PRINT #10, USING "##########,.## Bs"; CVD(total); : PRINT #10, SPACE$(9);
PRINT #10, SPACE$(30); "Neto NotaCre:"; : PRINT #10, USING "##########,.## Bs"; CVD(total); : PRINT #10, CHR$(10);
						
'-- 7
PRINT #10, "      "; STRING$(60, "-"); SPACE$(9);
PRINT #10, STRING$(60, "-"); CHR$(10);

'-- 8        
PRINT #10, "      "; SPACE$(22); "NOTA DE  CREDITO"; SPACE$(31);
PRINT #10, SPACE$(22); "NOTA DE  CREDITO"; SPACE$(31); CHR$(10);

'-- 9 y 10
FOR mrw% = 1 TO 14: PRINT #10, CHR$(10); : NEXT mrw%
						
'PRINT #10, CHR$(12);
CLOSE #10
END SUB

SUB LeeEstado
CLOSE : OPEN "F:\kevinprg\kevindat\kaos.dat" FOR INPUT SHARED AS #2
INPUT #2, sonido: INPUT #2, impuesto: INPUT #2, NombFarma: INPUT #2, Direccion: INPUT #2, Rif: INPUT #2, Nit: INPUT #2, telefono: INPUT #2, contri: INPUT #2, contra: INPUT #2, Stock: INPUT #2, Blanq
INPUT #2, LargoH: INPUT #2, TipoL: INPUT #2, Mensaje1: INPUT #2, Mensaje2: INPUT #2, Mensaje3: INPUT #2, Redondeo: INPUT #2, Desc: INPUT #2, DescMax: INPUT #2, Exactitud: INPUT #2, Prov1: INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo
INPUT #2, NuevoIva: INPUT #2, ShowPerc: CLOSE
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
		IF k = llevasitio% THEN COLOR 31, 0 ELSE COLOR 0, 15
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
COLOR 0, 15: LOCATE fila%, columna% + 1: PRINT eleccion$
MeteDatoLetra$ = UCASE$(eleccion$): IF sonido = "si" THEN BEEP
EXIT FUNCTION
Sumar:
FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR 0, 15: LOCATE fila%, columna%: PRINT "²"; eleccion$; "²"
RETURN
END FUNCTION

SUB NotadeCredito
COLOR colors(7, colorpref), colors(4, colorpref): CLS
DO
	FOR k = 1 TO 17: Fun(k) = "no": NEXT k
	DO 'Pantalla:
		FOR k = 1 TO 12: Fun(k) = "no": NEXT k
		seguir = true: devolver = false: llave$ = "": keya = "      "
		COLOR colors(7, colorpref), colors(4, colorpref): CLS : CALL Box(1, 1, 4, 80): CALL Center(2, "Farmacia:" + NombFarma + " Fecha:" + MID$(DATE$, 4, 2) + "/" + MID$(DATE$, 1, 2) + "/" + MID$(DATE$, 7, 4))
		CALL Center(3, "*** N O T A S   D E   C R E D I T O ***")
		CALL Box(5, 10, 7, 70): CALL Center(6, " Introduzca Clave de Vendedor:²   ²"): Box 21, 10, 23, 70
		CALL Center(25, "[F1] Ayuda [Esc] Menu Principal ")
		llevasitio% = 1: COLOR colors(7, colorpref), colors(4, colorpref): REDIM eleccion(0 TO 4) AS STRING * 1: seguir = true: eleccion$ = ""
		llave$ = "": lastTime# = TIMER
		DO
			llave$ = "": DO WHILE llave$ = "": llave$ = INKEY$: LOOP
			SELECT CASE llave$
				CASE CHR$(27):  EXIT SUB
				CASE CHR$(0) + CHR$(59) 'ayuda
				CASE ELSE: eleccion$ = eleccion$ + llave$: LOCATE 6, 54 + LEN(eleccion$): PRINT "²"
				IF LEN(eleccion$) >= 3 THEN EXIT DO
			END SELECT
		LOOP
		IF sonido = "si" THEN BEEP
		IF LEN(Trim$(eleccion$)) = 3 THEN
			COLOR colors(7, colorpref), colors(4, colorpref): elegsal% = 0
			CLOSE : OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #1 LEN = 100: FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
			nursaler = LOF(1) / 100: ClaveLarga$ = ""
			FOR k = 1 TO nursaler: GET #1, k
			IF eleccion$ = clco$ AND acti$ = " " AND inout$ = " " THEN
				ClaveLarga$ = clla$: Cedula$ = cedu$: RSET inout$ = "*"
				PUT #1, k: EXIT FOR
			END IF
			NEXT k: CLOSE
			IF ClaveLarga$ = "" THEN CALL Center(8, "No Hay Ningun Vendedor Con Esa Clave o Esta Fuera El Vendedor"): Espera ELSE EXIT DO
		END IF: eleccion$ = ""
	LOOP
	COLOR colors(7, colorpref), colors(4, colorpref): CLS
	LOCATE 1, 1: PRINT "Vendedor(a):"; ClaveLarga$: CALL Box(2, 1, 16, 80)
	salir = false: total = MKD$(0): renglon = 1
	fx = "F:\kevinprg\kevindat\" + MID$(STR$(CVL(Cedula$)), 2, LEN(STR$(CVL(Cedula$)))) + ".caj"
	OPEN "F:\kevinprg\kevindat\maxnc.dat" FOR RANDOM AS #7: FIELD #7, 4 AS credito$
	GET #7, 1: numnc$ = credito$: notac = numnc$: numnc$ = MKL$(CVL(numnc$) + 1)
	IF CVL(numnc$) > 999999 THEN numnc$ = MKL$(1)
	LSET credito$ = numnc$: PUT #7, 1: CLOSE : COLOR colors(7, colorpref) + 16, colors(4, colorpref)
	LOCATE 1, 44: PRINT "Nota  de Credito,Numero: ²"; : PRINT USING "**####,"; CVL(notac); : PRINT "²": Fun(17) = "op": COLOR colors(7, colorpref), colors(4, colorpref)
	'AdicProd: ------------------===============---------------------------------
	DO
		IF Fun(17) = "no" AND renglon > 26 THEN
			DO
			GOSUB Limpio: Fun(9) = "si": GOSUB Cierre: IF salir = true THEN EXIT DO
			LOOP: salir = false: EXIT DO
		END IF
		IF Fun(17) = "no" THEN EXIT DO
		CALL Center(25, "[1]Ayuda [6]Cambio [7]Limpiar [10]Cierre"): COLOR colors(7, colorpref), colors(4, colorpref): CALL Box(2, 1, 16, 80)
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
		llave$ = "": DO WHILE llave$ = "": llave$ = INKEY$: LOOP
		 
			SELECT CASE llave$
				CASE CHR$(0) + CHR$(59)
				CASE CHR$(0) + CHR$(64): GOSUB Cambiar
				CASE CHR$(0) + CHR$(65): GOSUB Limpio: GOSUB Anula
				CASE CHR$(0) + CHR$(68): GOSUB Limpio: Fun(10) = "si": GOSUB Cierre  ' cierra nota de credito
				CASE ELSE: keya = llave$: GOSUB BuscaOrden
			END SELECT
	LOOP
LOOP
'   GOTO Pantalla  '-------------------===============--------------------------

BuscaOrden:
CALL Center(17, SPACE$(80))
mjfox% = Escoge%(19, 1, 17, keya)
IF mjfox% < 1 THEN devolver = false: GOSUB Limpio: RETURN
GOSUB Mostrar: GOSUB EscPvp
RETURN

Cierre: '       CIERRE-----CIERRE------CIERRE-----
CALL Box2(17, 15, 24, 65): LOCATE 21, 39: PRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
IF CVD(total) = 0 THEN Center 18, "Debe Tener Articulos Registrados Para Poder Cerrar la Nota": Espera: GOSUB Limpio: RETURN
LOCATE 21, 20: PRINT "Bruto a Devolver Bs:"; : PRINT USING "#########,.##"; CVD(total)
'abre: e imprime ojo
'OPEN "COM1" FOR RANDOM AS #29: PRINT #29, "A": CLOSE #29
'OPEN "COM2" FOR RANDOM AS #29: PRINT #29, "A": CLOSE #29
'insertar la palabra contado
Fun(9) = "no": nombre$ = "           " + SPACE$(29): domicilio1$ = SPACE$(60)
rifo$ = SPACE$(20): nito$ = SPACE$(40): domicilio2$ = SPACE$(60): tlf$ = SPACE$(20): salir = false: sit% = 1: GOSUB HaceCaja
DO WHILE NOT salir
	SELECT CASE sit%
	CASE 1
		nombre$ = MeteDatoLetra(12, 28, 40, nombre$, "0")
		IF devolver = true THEN devolver = false: sit% = 6 ELSE sit% = 2
	CASE 2
		domicilio1$ = MeteDatoLetra(14, 10, 60, domicilio1$, "0")
		IF devolver = true THEN devolver = false: sit% = 1 ELSE sit% = 3
	CASE 3
		domicilio2$ = MeteDatoLetra(15, 10, 60, domicilio2$, "0")
		IF devolver = true THEN devolver = false: sit% = 2 ELSE sit% = 4
	CASE 4
		rifo$ = MeteDatoLetra(16, 18, 20, rifo$, "0")
		IF devolver = true THEN devolver = false: sit% = 3 ELSE sit% = 5
	CASE 5
		nito$ = MeteDatoLetra(18, 18, 20, nito$, "0")
		IF devolver = true THEN devolver = false: sit% = 4 ELSE sit% = 6
	CASE 6
		tlf$ = MeteDatoLetra(16, 50, 20, tlf$, "0")
		IF devolver = true THEN devolver = false: sit% = 5 ELSE sit% = 1
	END SELECT
	IF Fun(2) = "si" THEN
		Fun(2) = "no": GOSUB BuscaCliente: GOSUB HaceCaja
	END IF
	IF LEN(Trim$(nombre$)) > 0 THEN imprint$ = "*" ELSE imprint$ = " "
	IF Fun(9) = "si" THEN
		Fun(9) = "no": salir = true
		IF imprint$ = "*" THEN
			COLOR colors(7, colorpref), colors(4, colorpref): Box2 10, 8, 20, 72
			Center 15, "Desea Imprimir nota de cr‚dito? Pulse -S- para IMPRIMIR"
			llave$ = "": DO WHILE llave$ = "": llave$ = INKEY$: LOOP: llave$ = UCASE$(llave$)
			IF llave$ = "S" THEN salir = true: imprint$ = "*" ELSE salir = false: imprint$ = " ": GOSUB HaceCaja
		END IF
	END IF
LOOP: CLOSE
		OPEN fx FOR RANDOM SHARED AS #17 LEN = 8: FIELD #17, 8 AS caja$: GET #17, 1: RSET caja$ = MKD$(CVD(caja$) - CVD(total)): PUT #17, 1: CLOSE #17
		'ojo devolver al inventario , hacer una subfuncion "grande"
		res% = true
		'fx = ""
		'CALL Box(12, 5, 14, 75): salir = false: Center 25, "INTRODUZCA LA FECHA EN QUE DESEA HACER LA NOTA DE CREDITO"
		aux$ = MID$(DATE$, 4, 2) + "/" + MID$(DATE$, 1, 2) + "/" + MID$(DATE$, 7, 4)
		'DO WHILE salir = false
		'  aux$ = MeteDatoLetra$(13, 31, 10, aux$, "0")
		'  IF CheqDate(aux$, 0) = true THEN
		'    salir = true
		'  ELSE
		'    Center 25, "INTRODUZCA UNA FECHA CORRECTA, POR FAVOR"
		'    IF sonido = "si" THEN BEEP: BEEP
		'  END IF
		'LOOP
		'res% = Verify(13, "¨Est  completamente seguro que esta fecha es la correcta")
	 
		IF res% = true THEN
			fx = "F:\kevinprg\kevindat\" + MID$(aux$, 1, 2) + MID$(aux$, 4, 2) + MID$(aux$, 7, 4) + ".crd"
		ELSE
			aux$ = MID$(DATE$, 4, 2) + "/" + MID$(DATE$, 1, 2) + "/" + MID$(DATE$, 7, 4)
			fx = "F:\kevinprg\kevindat\" + MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4) + ".crd"
		END IF
		OPEN fx FOR RANDOM AS #17 LEN = 30: FIELD #17, 30 AS todo$
		nursaler = LOF(17) / 30: nursaler = nursaler + 1
		RSET todo$ = "J" + MKI$(renglon - 1) + imprint$ + total + MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MID$(TIME$, 7, 2) + Cedula$ + notac + SPACE$(4)
		PUT #17, nursaler: salir = false     ': LOCATE 1, 1: PRINT nursaler; "   "; renglon: Espera
		FOR k = 1 TO renglon - 1
		RSET todo$ = ivaf1(k) + code1(k) + cant1(k) + prec1(k) + cost1(k) + psic1(k) + SPACE$(4): IF psic1(k) = "*" THEN salir = true
		PUT #17, (nursaler + k)   ': LOCATE 1, 1: PRINT todo$: Espera
		NEXT k: CLOSE             ': LOCATE 1, 1: PRINT nursaler: Espera
		IF salir = true THEN
			OPEN "F:\kevinprg\kevindat\movpsico.dat" FOR RANDOM AS #1 LEN = 40: FIELD #1, 40 AS todo$: max% = LOF(1) / 40: nursaler = nursaler + 1
			FOR k = 1 TO renglon - 1
			IF psic1(k) = "*" THEN RSET todo$ = "DEVOLU" + notac + DATE$ + TIME$ + code1$(k) + cant1(k) + Cedula$: PUT #1, nursaler: nursaler = nursaler + 1
			NEXT k: CLOSE
		END IF
		CLOSE : OPEN "F:\kevinprg\kevindat\bitacora.dat" FOR RANDOM AS #17 LEN = 40: FIELD #17, 40 AS tod$: nursaler = LOF(17) / 40
		FOR k = 1 TO renglon - 1
		LSET tod$ = code1$(k) + MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4) + MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + cant1(k) + "04" + notac + Cedula$ + prec1(k)
		nursaler = nursaler + 1: PUT #17, nursaler: NEXT k: CLOSE
		OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #1 LEN = 100
		FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
		nursaler = LOF(1) / 100
		FOR k = 1 TO nursaler: GET #1, k
		IF Cedula$ = cedu$ THEN RSET inout$ = " ": RSET cong$ = " ": Cedula$ = clla$: PUT #1, k: EXIT FOR
		NEXT k: CLOSE : GOSUB Devuelve: neaux$ = MKD$(0)
		IF imprint$ = "*" THEN CALL Imprimir(ClaveLarga$, nombre$, rifo$, nito$, domicilio1$, domicilio2$, tlf$, aux$)
CLOSE : EXIT SUB
RETURN    ' FIN CIERRE-----FIN CIERRE-----FIN CIERRE------

Limpia:
COLOR colors(7, colorpref), colors(4, colorpref): FOR k = 5 TO 24: LOCATE k, 1: PRINT SPACE$(80); : NEXT k
RETURN

Limpio:
COLOR colors(7, colorpref), colors(4, colorpref): FOR k = 17 TO 24: LOCATE k, 1: PRINT SPACE$(80); : NEXT k
RETURN

EscPvp:              ' EscPvp---------EscPvp----------EscPvp---------
GOSUB Mostrar
IF preg$ = "*" THEN
		COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 17, 1, 24, 80
		LOCATE 18, 25: PRINT "* * *  A T E N C I O N  * * *": COLOR colors(7, colorpref), colors(4, colorpref)
		Center 20, "A Este Articulo se le vigila el Vencimiento , Usted DEBE": Center 21, "VERIFICAR la fecha de vencimiento": Espera
		GOSUB Mostrar
END IF
CALL Center(25, "Introduzca el Precio del Articulo y Presione [Enter] cuando est‚ listo.")
Box 21, 3, 23, 65: LOCATE 22, 5: PRINT "¨P.V.P. del Art¡culo?"
keya = EntraDatoNumero(22, 30, 6, "0     ", 1): prec1(renglon) = MKD$(VAL(keya))
IF CVD(prec1(renglon)) = 0 THEN
	mjfox% = 0: desc1(renglon) = "": code1(renglon) = "": cant1(renglon) = MKI$(0): prec1(renglon) = MKD$(0): cost1(renglon) = MKD$(0): GOSUB Limpio: RETURN
END IF
DO
	CALL Center(25, "Introduzca el Precio al que fue cobrado el Articulo y Presione [Enter].")
	Box 21, 3, 23, 65: LOCATE 22, 5: PRINT "¨ Precio Cobrado    ?"
	keya = EntraDatoNumero(22, 30, 6, "0     ", 1): cobrado1(renglon) = MKD$(VAL(keya))
	IF CVD(cobrado1(renglon)) > CVD(prec1(renglon)) THEN Center 25, "El precio cobrado no puede ser mayor al PVP": BEEP ELSE EXIT DO
LOOP
IF CVD(cobrado1(renglon)) = 0 THEN
	mjfox% = 0: desc1(renglon) = "": code1(renglon) = "": cant1(renglon) = MKI$(0): prec1(renglon) = MKD$(0): cost1(renglon) = MKD$(0): GOSUB Limpio: RETURN
END IF
CALL Center(25, "Introduzca el N£mero de Unidades y Presione [Enter] cuando est‚ listo.")
DO
	Box 21, 3, 23, 75: LOCATE 22, 5: PRINT "¨Cantidad a DEVOLVER ?"
	keya = EntraDatoNumero(22, 30, 6, "0     ", 0): llave$ = ""
	IF VAL(keya) = 0 OR VAL(keya) = 1 THEN EXIT DO
	IF VAL(keya) > 1 AND VAL(keya) < 32501 THEN
		BEEP: Box 21, 3, 23, 75: LOCATE 22, 5: PRINT "¨ESTA SEGURO QUE DESEA DEVOLVER "; keya; " UNIDADES?"
		Center 25, "PULSE LA TECLA -S- PARA CONTINUAR , -N- PARA RECTIFICAR CANTIDAD": llave$ = ""
		DO WHILE llave$ = ""
			llave$ = INKEY$: llave$ = UCASE$(llave$): IF llave$ = "S" OR llave$ = "N" THEN EXIT DO ELSE BEEP
		LOOP
	ELSE
		BEEP: Center 25, "Introduzca una cantidad comprendida entre 1 y 32500"
	END IF
	IF llave$ = "S" THEN EXIT DO
LOOP
cant1(renglon) = MKI$(VAL(keya))
IF CVI(cant1(renglon)) = 0 THEN
	mjfox% = 0: desc1(renglon) = "": code1(renglon) = "": cant1(renglon) = MKI$(0): prec1(renglon) = MKD$(0): cost1(renglon) = MKD$(0): GOSUB Limpio: RETURN
END IF: tour% = 1
IF renglon > 1 THEN
	FOR k = 1 TO renglon - 1
		IF codigo2$ = code1(k) THEN
			IF CVD(prec1(k)) = CVD(prec1(renglon)) AND CVD(cobrado1(k)) = CVD(cobrado1(renglon)) THEN
				cant1(k) = MKI$(CVI(cant1(k)) + CVI(cant1(renglon))): total = MKD$((CVD(prec1(k)) * CVI(cobrado1(renglon))) + (CVD(total)))
				tour% = 2: k = renglon: cant1(renglon) = MKI$(0): prec1(renglon) = MKD$(0): cobrado1(renglon) = MKD$(0): CLOSE
			END IF
		END IF
	NEXT k
END IF
SELECT CASE tour%
	CASE 1
		ivaf1(renglon) = iv$: regi2(renglon) = MID$(STR$(sit%), 2, 1): regi1(renglon) = MKI$(mjfox%)
		IF CVI(ubic2$) = 1 THEN psic1(renglon) = "*"
		code1(renglon) = codigo2$: desc1(renglon) = nomb2$
		cost1(renglon) = MKD$(.7 * CVD(prec1(renglon)))
		total = MKD$(CVD(total) + CVD(cobrado1(renglon)) * CVI(cant1(renglon))): CLOSE
	CASE 2
		renglon = renglon - 1
END SELECT
GOSUB Limpia: renglon = renglon + 1: IF renglon > 26 THEN Fun(17) = "no"
RETURN

Anula:
Box2 17, 10, 24, 70: LOCATE 20, 20: PRINT "¨ Desea LIMPIAR esta Nota ?"
keya$ = MeteDatoLetra$(20, 48, 1, "N", "0")
IF MID$(keya$, 1, 1) = "s" OR MID$(keya$, 1, 1) = "S" THEN
	FOR k = 1 TO 26
		regi1(k) = MKI$(0): regi2(k) = " ": desc1(k) = "": prec1(k) = MKD$(0): cobrado1(k) = MKD$(0)
		cant1(k) = MKI$(0): cost1(k) = MKD$(0): ivaf1(k) = " ": psic1(k) = " "
	NEXT k
	CLOSE : total = MKD$(0): renglon = 1
END IF: GOSUB Limpia
RETURN

Cambiar:
IF renglon = 1 THEN Center 20, SPACE$(20) + "NO hay articulos para cambiar." + SPACE$(20)
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
salir = false
Center 24, "Introduzca La Cantidad Correcta ."
DO WHILE NOT salir
IF sit% < 14 THEN
	canti$ = EntraDatoNumero$(sit% + 2, 24, 4, STR$(CVI(cant1(sit%))), 0)
ELSE
	canti$ = EntraDatoNumero$(sit% - 11, 64, 4, STR$(CVI(cant1(sit%))), 0)
END IF
IF ABS(VAL(canti$)) > 32500 THEN Center 20, "La Cantidad DEBE Ser Menor o Igual a 32.500 Unidades": Espera ELSE salir = true
LOOP
cant1(sit%) = MKI$(ABS(VAL(canti$))): total = MKD$(0)
IF CVI(cant1(sit%)) = 0 THEN
	renglon = renglon - 1
	FOR k = sit% TO renglon
	regi1(k) = regi1(k + 1): regi2(k) = regi2(k + 1): desc1(k) = desc1(k + 1): psic1(k) = psic1(k + 1)
	cant1(k) = cant1(k + 1): cost1(k) = cost1(k + 1): ivaf1(k) = ivaf1(k + 1): prec1(k) = prec1(k + 1)
	NEXT k
END IF
FOR k = 1 TO 26: total = MKD$(CVD(total) + CVD(cobrado1(k)) * CVI(cant1(k))): NEXT k: GOSUB Limpio
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
LOCATE 20, 32: PRINT USING "######,.##"; CVD(pv12$)
LOCATE 20, 51: PRINT USING "######,.##"; CVD(pv22$)
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
PRINT "] [Maximo2="; : PRINT USING "####"; CVI(maxi22$);
PRINT "]";
IF iv$ = "*" THEN PRINT " Producto Que Paga I.C.S.V.M. " ELSE PRINT " Producto Exento "
IF Fun(17) = "no" THEN Espera
RETURN


BuscaCliente:
CALL Center(25, " *** Espere , Por Favor ... ***")
LOCATE 12, 10: PRINT "Elija el Cliente: ²   ²";
mjfox% = Elije%(7, 3, 60, 12, 29, 0): IF mjfox% < 1 THEN RETURN
client% = VAL(Yname1(mjfox%))
OPEN "F:\kevinprg\kevindat\clientes.dat" FOR RANDOM SHARED AS #5 LEN = 38: FIELD #5, 3 AS codigo$, 35 AS nomb$
FOR k = 1 TO 199
		 GET #5, k
		 IF VAL(codigo$) = VAL(Yname1(mjfox%)) THEN
					nombre$ = nomb$ + "     ": EXIT FOR
		 END IF
NEXT k: CLOSE #5
OPEN "f:\kevinprg\kevindat\fichacli.ent" FOR RANDOM SHARED AS #5 LEN = 326
FIELD #5, 20 AS rif5$, 20 AS nit5$, 2 AS diacred5$, 4 AS monmax5$, 60 AS direc15$, 60 AS direc25$, 20 AS tele5$, 140 AS nada5$
GET #5, client%: domicilio1$ = direc15$: domicilio2$ = direc25$
rifo$ = rif5$: nito$ = nit5$: tlf$ = tele5$: CLOSE
RETURN

HaceCaja:
COLOR colors(7, colorpref), colors(4, colorpref): Box2 10, 8, 20, 72: Center 10, "Datos del Cliente": Center 25, "[F2]Elije Cliente Fijo [F9] Salir e Imprimir"
LOCATE 12, 10: PRINT "Nombre  ¢ Raz¢n : ²"; nombre$; "²": LOCATE 13, 10: PRINT "Domicilio Fiscal:"
LOCATE 14, 10: PRINT "²"; domicilio1$; "²": LOCATE 15, 10: PRINT "²"; domicilio2$; "²"
LOCATE 16, 10: PRINT "R.I.F.: ²"; rifo$; "²": LOCATE 16, 40: PRINT "Telefono: ²"; tlf$; "²"
LOCATE 18, 10: PRINT "N.I.T.: ²"; nito$; "²": LOCATE 18, 40: PRINT "Devolucion de  CONTADO"
RETURN

Devuelve:
CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
FOR k = 1 TO renglon
	GET #1, CVI(regi1(k)): devolver = false: LOCK #1, CVI(regi1(k))
	IF CVD(pv12$) = CVD(prec1(k)) THEN RSET exi12$ = MKI$(CVI(exi12$) + CVI(cant1(k))): devolver = true
	IF CVD(pv22$) = CVD(prec1(k)) AND devolver = false THEN RSET exi22$ = MKI$(CVI(exi22$) + CVI(cant1(k))): devolver = true
	IF CVD(pv32$) = CVD(prec1(k)) AND devolver = false THEN RSET exi32$ = MKI$(CVI(exi32$) + CVI(cant1(k))): devolver = true
	IF CVD(pv42$) = CVD(prec1(k)) AND devolver = false THEN RSET exi42$ = MKI$(CVI(exi42$) + CVI(cant1(k))): devolver = true
	IF CVI(exi12$) = 0 AND devolver = false THEN RSET exi12$ = MKI$(CVI(exi12$) + CVI(cant1(k))): devolver = true: RSET pv12$ = prec1(k): RSET cos12$ = MKD$(.7 * CVD(prec1(k)))
	IF CVI(exi22$) = 0 AND devolver = false THEN RSET exi22$ = MKI$(CVI(exi22$) + CVI(cant1(k))): devolver = true: RSET pv22$ = prec1(k): RSET cos22$ = MKD$(.7 * CVD(prec1(k)))
	IF CVI(exi32$) = 0 AND devolver = false THEN RSET exi32$ = MKI$(CVI(exi32$) + CVI(cant1(k))): devolver = true: RSET pv32$ = prec1(k): RSET cos32$ = MKD$(.7 * CVD(prec1(k)))
	PUT #1, CVI(regi1(k)): UNLOCK #1, CVI(regi1(k))
NEXT k
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

