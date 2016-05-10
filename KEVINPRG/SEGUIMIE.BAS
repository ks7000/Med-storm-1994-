'modulo de seguimiento de productos
DEFINT A-Z
DECLARE SUB HelpMate (opcion%, contexto$)
DECLARE SUB PopUp (ka%, la%, ma%, na%, pa%, ra%, ta%, sa%)
DECLARE SUB VentaContado ()
DECLARE SUB Espera ()
DECLARE SUB Box (Row1, Col1, Row2, Col2)
DECLARE SUB Box2 (Row1, Col1, Row2, Col2)
DECLARE SUB Center (Row, text$)
DECLARE SUB FancyCls (dots%, Background%)
DECLARE SUB LeeEstado ()
DECLARE SUB Espere (fila%, columna%)
DECLARE SUB Seguimiento ()
DECLARE SUB Panel (Row%, Col%, Rows%, Cols%, Border%, Attrib%)
DECLARE SUB SigueCompras ()
DECLARE SUB SigueVentas ()
DECLARE SUB SigueDevoluciones ()
DECLARE FUNCTION DateToJulian& (day%, month%, year%)
DECLARE FUNCTION DateValid% (day%, month%, year%)
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$, tr$)
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Escoge% (fila%, edo%, fila2%, Prod$)
DECLARE FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%)
DECLARE FUNCTION Trim$ (x$)
COMMON SHARED guia, colorpref
DIM SHARED nursaler, ybusqueda, llave$, kbe$: guia = 1
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED Sonido  AS STRING * 2, Impuesto AS STRING * 6, NombFarma AS STRING * 25, Direccion AS STRING * 30, Rif     AS STRING * 25, Nit      AS STRING * 25, telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2, NuevoIva AS STRING * 2, ShowPerc AS STRING * 2
DIM SHARED regi1(26) AS STRING * 2, regi2(26) AS STRING * 1, desc1(26) AS STRING * 35
DIM SHARED cant1(26) AS STRING * 2, cost1(26) AS STRING * 8, ivaf1(26) AS STRING * 1
DIM SHARED prec1(26) AS STRING * 8, code1(26) AS STRING * 6, psic1(26) AS STRING * 1
DIM SHARED PrintErr AS INTEGER, devolver, Fun(17) AS STRING * 2, sitio, salir
DIM SHARED renglon AS INTEGER, total AS STRING * 8, neto AS STRING * 8, numcheq AS STRING * 8, moncheq AS STRING * 8, numbank AS STRING * 2
DIM SHARED entrega AS STRING * 13, keya AS STRING * 6, costo AS STRING * 8, encabezado AS STRING * 2
DIM SHARED previo AS STRING * 8, fx AS STRING * 33, fx2 AS STRING * 33
DIM SHARED fct AS STRING * 4, conda AS STRING * 8, DescGer AS STRING * 8, newiva AS STRING * 8
DIM SHARED Yname1(15) AS STRING * 3, Yname2(15) AS STRING * 15, mjfox AS INTEGER, rus AS STRING
DIM SHARED dia AS INTEGER, mes AS INTEGER, ano AS INTEGER, auxi AS STRING, num2 AS STRING * 33
REM $DYNAMIC
DIM SHARED Nombra(5) AS STRING * 35, Pvp1a(5) AS STRING * 8, Pvp2a(5) AS STRING * 8, Pvp3a(5) AS STRING * 8, Pvp4a(5) AS STRING * 8, Numa(5) AS STRING * 2, Exis1a(5) AS STRING * 2, Exis2a(5) AS STRING * 2, Exis3a(5) AS STRING * 2, Exis4a(5) AS  _
STRING * 2
DIM SHARED Cant1a(5) AS STRING * 2, Cant2a(5) AS STRING * 2, Cant3a(5) AS STRING * 2, Cant4a(5) AS STRING * 2, Ubica(5) AS STRING * 2
CONST true = -1: CONST false = NOT true:  ybusqueda = 0
ON ERROR GOTO ErrorTrap
ON TIMER(60) GOSUB Reloj
CALL LeeEstado: CALL Seguimiento
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... "
CHAIN "F:\kevinprg\kevin01.bas"
END

ErrorTrap:
SELECT CASE ERR
	CASE 25, 27, 68
		Box2 5, 2, 7, 78: Center 5, "Mensaje": Center 6, "No Se Detect¢ Gavetero;NO DEBE Usar Esta Estacion Para Ventas de Contado": RESUME NEXT
	CASE 57
		Box2 5, 2, 8, 78: Center 5, "Mensaje": Center 6, "SE PERDIO CONEXION CON LA INTRANET": Center 7, " ­­­ TRATANDO DE RECONECTAR !!! "
		RESUME
	CASE 70
		Center 25, "Esperando Acceso a La Red , Espere Por Favor . . .": SLEEP 2: BEEP: RESUME
	CASE ELSE
		LOCATE 1, 1: PRINT ERR
END SELECT
RESUME NEXT
DATA 0,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  2,     0,     1,     15,     0,     7,      0
DATA 3,    15,   13,  1,     14,    3,     15,     0,     7,      0
DATA 7,    12,   15,  4,     14,    0,     15,     15,    1,      0

Reloj:
llave$ = CHR$(0) + CHR$(67)
kbe$ = CHR$(27)
RETURN

REM $STATIC
SUB Box (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿"; : FOR a = Row1 + 1 TO Row2 - 1: LOCATE a, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Box2 (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "²"; STRING$(BoxWidth - 2, "²"); "²"; : FOR a = Row1 + 1 TO Row2 - 1: LOCATE a, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Center (Row, text$)
IF Row = 25 THEN COLOR 0, 15: LOCATE 25, 1: PRINT SPACE$(80);
LOCATE Row, 41 - LEN(text$) / 2: PRINT text$;
END SUB

FUNCTION DateToJulian& (day%, month%, year%)
' reparado el error para fechas mayores que 29 de Febrero de 2000
mesr% = month%: anor% = year%
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
d& = a& + B& + C&
SELECT CASE anor%
	CASE 2000
		IF mesr% > 2 THEN
			d& = d& - 109573
		END IF
	CASE IS > 2000
			d& = d& - 109573
	CASE ELSE
END SELECT
DateToJulian = d&
END FUNCTION

FUNCTION DateValid% (day%, month%, year%)
		SELECT CASE month%                   'verify date within month
	
		CASE 4, 6, 9, 11                  'months that should
		SELECT CASE day%               'only  have
		CASE 1 TO 30                '30 days
		CASE ELSE                   'otherwise
		DateValid% = 0           'not valid
		EXIT FUNCTION
		END SELECT

		CASE 1, 3, 5, 7, 8, 10, 12        'months that
		SELECT CASE day%               'should have
		CASE 1 TO 31                '31 days
		CASE ELSE                   'otherwise
		DateValid% = 0           'not valid
		EXIT FUNCTION
		END SELECT
		
		CASE 2                            'February
		SELECT CASE year% MOD 4        'account for leap years
		CASE 0                      'a leap year
		SELECT CASE day%
			 CASE 1 TO 29          'may have 29 days
				CASE ELSE                        '
				DateValid% = 0    'not valid
				EXIT FUNCTION
		END SELECT
		CASE ELSE                   'not a leap year
		SELECT CASE day%
			 CASE 1 TO 28          'only 28 days acceptable
			 CASE ELSE
			 DateValid% = 0
			 EXIT FUNCTION
		END SELECT
		END SELECT
						
		CASE ELSE                         'anything else
		DateValid% = 0                 'is invalid date entry
		EXIT FUNCTION
	
		END SELECT
	
		DateValid% = 1                       'Date passes all tests

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
llevasitio% = 2: REDIM eleccion(largo% + 1) AS STRING * 1
FOR k = 1 TO largo%
	eleccion(k) = MID$(caden$, k, 1)
NEXT k
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
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna%: PRINT "²"; elccion$; "²": IF Sonido = "si" THEN BEEP
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
	IF Fun(17) = "op" THEN Prod$ = UCASE$(MeteDatoLetra(fila2%, 34, 6, keya, "1")) ELSE Prod$ = UCASE$(MeteDatoLetra(fila2%, 34, 6, keya, "2"))
	Prud = Prod$: Prod$ = Trim$(Prod$)
	IF Fun(9) = "si" OR Prod$ = "" THEN : Escoge% = -1: EXIT FUNCTION
	LOOP
END IF: selecc% = 0
CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
OPEN "f:\kevinprg\kevindat\ordubica.dat" FOR RANDOM SHARED AS #77 LEN = 15
FIELD #77, 15 AS describe$
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
TIMER ON
DO
	kbd$ = "": kbe$ = ""
	WHILE kbd$ = "" AND kbe$ = "": kbd$ = INKEY$: WEND
	IF kbe$ = "" THEN kbd$ = UCASE$(kbd$) ELSE kbd$ = kbe$
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
		IF resal% = l THEN
			IF CVD(Pvp3a(l)) = 0 THEN Pvp3a(l) = MKD$(1)
			perco$ = MKD$((CVD(Pvp4a(l)) / (CVD(Pvp3a(l))))): IF CVD(perco$) = 0 THEN perco$ = MKD$(1)
			LOCATE fila% - 2, 5: PRINT Prod$: LOCATE fila% - 2, 20: GET #77, CVI(Ubica(l)): PRINT describe$
			LOCATE fila% - 2, 40: PRINT "En Oferta: "; : PRINT USING "#####, "; CVI(Cant4a(l)); : PRINT " a Bs."; : PRINT USING "######,"; CVD(Pvp4a(l)); : PRINT USING " (##.##%)"; (100 - (100 * CVD(perco$)));
		END IF
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
TIMER OFF
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
		Nombra(selecc%) = nomb2$: Cant1a(selecc%) = exi12$: Cant2a(selecc%) = exi22$: Cant3a(selecc%) = exi32$: Cant4a(selecc%) = exi42$: Pvp1a(selecc%) = pv12$: Pvp2a(selecc%) = pv22$: Pvp3a(selecc%) = pv32$: Pvp4a(selecc%) = pv42$:  Numa(selecc%) =  _
num$: Ubica(selecc%) = ubic2$
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
num$: Ubica(selecc%) = ubic2$
	ELSE
		EXIT FOR
	END IF
NEXT k
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila% - 2, 1: PRINT SPACE$(70): COLOR colors(4, colorpref), colors(7, colorpref)
wer$ = STRING$(35, "Ä") + "ÂCAN1ÄÄÄÄÄÄPVP1ÂCAN2" + STRING$(6, "Ä") + "PVP2ÂCAN3" + STRING$(5, "Ä") + "PVP3" + "Ä"
LOCATE fila% - 1, 1: PRINT wer$;
wer$ = STRING$(35, "Ä") + "ÁCAN1ÄÄÄÄÄÄPVP1ÁCAN2" + STRING$(6, "Ä") + "PVP2ÁCAN3" + STRING$(5, "Ä") + "PVP3" + "Ä"
LOCATE fila% + 5, 1: PRINT wer$; : COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE fila%, 1: PRINT Nombra(1); "³"; : LOCATE fila%, 37: PRINT USING "#####, "; CVI(Cant1a(1)); : LOCATE fila%, 51: PRINT "³"; : PRINT USING "#####, "; CVI(Cant2a(1)); : LOCATE fila%, 66: PRINT "³"; : PRINT USING "#####, "; CVI(Cant3a(1))
LOCATE fila%, 44: PRINT USING "######,"; CVD(Pvp1a(1)): LOCATE fila%, 59: PRINT USING "######,"; CVD(Pvp2a(1)): LOCATE fila%, 74: PRINT USING "######,"; CVD(Pvp3a(1)): COLOR colors(4, colorpref), colors(7, colorpref)
IF CVD(Pvp3a(1)) = 0 THEN Pvp3a(1) = MKD$(1)
perco$ = MKD$((CVD(Pvp4a(1)) / (CVD(Pvp3a(1))))): IF CVD(perco$) = 0 THEN perco$ = MKD$(1)
LOCATE fila% - 2, 5: PRINT Prod$: LOCATE fila% - 2, 20: GET #77, CVI(Ubica(1)): PRINT describe$
LOCATE fila% - 2, 40: PRINT "En Oferta: "; : PRINT USING "#####, "; CVI(Cant4a(1)); : PRINT " a Bs."; : PRINT USING "######,"; CVD(Pvp4a(1)); : PRINT USING " (##.##%)"; (100 - (100 * CVD(perco$)));
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
Center 25, "Presione Cualquier Tecla Para Continuar..."
kbe$ = ""
DO WHILE kbe$ = ""
	kbe$ = INKEY$
LOOP
END SUB

SUB Espere (fila%, columna%)
DO
	axi% = FIX(RND * 100): IF axi% >= 65 AND axi% <= 90 THEN EXIT DO
LOOP
LOCATE fila%, columna%: PRINT "Presione la tecla:"; CHR$(axi%); " para Continuar"
DO
	llave$ = "": DO WHILE llave$ = "": llave$ = INKEY$: LOOP
	llave$ = UCASE$(llave$): IF llave$ = CHR$(axi%) THEN EXIT DO ELSE BEEP
LOOP
END SUB

SUB LeeEstado
CLOSE : OPEN "F:\kevinprg\kevindat\kaos.dat" FOR INPUT SHARED AS #2
INPUT #2, Sonido: INPUT #2, Impuesto: INPUT #2, NombFarma: INPUT #2, Direccion: INPUT #2, Rif: INPUT #2, Nit: INPUT #2, telefono: INPUT #2, contri: INPUT #2, contra: INPUT #2, Stock: INPUT #2, Blanq
INPUT #2, LargoH: INPUT #2, TipoL: INPUT #2, Mensaje1: INPUT #2, Mensaje2: INPUT #2, Mensaje3: INPUT #2, Redondeo: INPUT #2, desc: INPUT #2, DescMax: INPUT #2, Exactitud: INPUT #2, Prov1: INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo
INPUT #2, NuevoIva: INPUT #2, ShowPerc: CLOSE : NuevoIva = "si": ShowPerc = "si"
OPEN "f:\kevinprg\kevindat\encabeza.dat" FOR INPUT SHARED AS #2: INPUT #2, encabezado: CLOSE
END SUB

FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$, tr$)
IF tr$ = "0" THEN llevasitio% = 1
IF tr$ = "1" THEN llevasitio% = 2
IF tr$ = "2" THEN
	FOR k = 1 TO largo%
		IF MID$(caden$, k, 1) = " " THEN llevasitio% = k: tr$ = MID$(STR$(k), 2, 1): EXIT FOR
	NEXT k
END IF
COLOR 0, 15: LOCATE fila%, columna%: PRINT CHR$(178) + caden$: LOCATE fila%, columna% + largo% + 1: PRINT CHR$(178): REDIM eleccion(0 TO largo% + 1) AS STRING * 1
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
MeteDatoLetra$ = eleccion$: IF Sonido = "si" THEN BEEP
EXIT FUNCTION
Sumar:
FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR 0, 15: LOCATE fila%, columna%: PRINT "²"; eleccion$; "²"
RETURN
END FUNCTION

SUB Seguimiento
COLOR colors(7, colorpref), colors(4, colorpref): CLS
CALL Box2(1, 1, 5, 80): Center 3, " Seguimiento a UN art¡culo: seleccione el producto"
Fun(9) = "no": CALL Center(17, SPACE$(80)): IF Fun(17) <> "op" THEN keya = eleccion$
mjfox = Escoge%(19, 1, 17, keya)
COLOR colors(7, colorpref), colors(4, colorpref): CLS
IF mjfox <= 0 THEN EXIT SUB
CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
GET #1, mjfox
CALL Box2(2, 1, 6, 80): Center 4, " Seguimiento a UN art¡culo, producto seleccionado"
CALL Center(5, nomb2$ + codigo2$)
CALL PopUp(12, 10, 8, 60, 112, 1, 0, 0)
LOCATE 13, 11: PRINT "¨Desde que fecha desea buscar?:"
DO
	devolver = false: Center 25, "Pulse Escape para abortar"
	auxi = MeteDatoLetra$(13, 50, 10, "26/08/2002", "0")
	dia = VAL(MID$(auxi, 1, 2)): dia2% = dia: auxe$ = auxi
	mes = VAL(MID$(auxi, 4, 2)): mes2% = mes
	ano = VAL(MID$(auxi, 7, 4)): ano2% = ano
	IF devolver = true THEN EXIT SUB
	IF DateValid%(dia2%, mes2%, ano2%) = 1 THEN
		IF DateToJulian(dia2%, mes2%, ano2%) >= 2341516 THEN
			EXIT DO
		ELSE
			Center 15, "Debe ser mayor que 01/10/1998"
		END IF
	ELSE
		Center 15, "Introduzca una fecha correcta": Center 16, "Utilize el formato (dd/mm/aaaa)"
	END IF
LOOP
CALL ShutUp
CALL PopUp(12, 10, 10, 60, 112, 1, 0, 0)
LOCATE 13, 11: PRINT "¨Qu‚ desea buscar? Pulse un n£mero para elejir"
LOCATE 14, 11: PRINT "1.- Ventas"
LOCATE 15, 11: PRINT "2.- Compras"
LOCATE 16, 11: PRINT "3.- Devoluciones"
LOCATE 18, 11: PRINT "5.- ABORTAR proceso y salir al men£ principal"
kbe$ = ""
DO WHILE kbe$ = ""
	kbe$ = INKEY$
	IF VAL(kbe$) = 1 THEN CALL SigueVentas
	IF VAL(kbe$) = 2 THEN CALL SigueCompras
	IF VAL(kbe$) = 3 THEN CALL SigueDevoluciones
	IF VAL(kbe$) = 5 OR kbe$ = CHR$(27) THEN EXIT SUB
LOOP
CALL ShutUp

CLOSE #3: OPEN rus FOR RANDOM AS #3
FIELD #3, 70 AS todo$
sitio% = 2: va& = LOF(3) / 70
IF va& = 0 THEN Center 11, "No se encontraron ventas en este per¡odo de tiempo": Espera: EXIT SUB
CLS : Box2 2, 1, 24, 80
IF VAL(kbe$) = 1 THEN LOCATE 2, 3: PRINT "Fecha²²²²²²²²Vendedor²²²²²²²²²²²²Hora²²²²²²²²²²Cantidad²²²²²²²²Precio²²²²"
IF VAL(kbe$) = 2 THEN LOCATE 2, 3: PRINT "Fecha²²²²²Factura²²Proveedor²²²²²²²Cantidad,precio y vencimiento²²²²²²²²²"
CALL Center(1, "Producto:" + nomb2$ + " desde " + auxe$)
Center 25, "Pulse las teclas de direcci¢n, Escape para salir": puntero& = 1
DO
	GOSUB Dibuja
	kbd$ = ""
	DO WHILE kbd$ = ""
		kbd$ = INKEY$
	LOOP
	SELECT CASE kbd$
		CASE CHR$(0) + "H"
			puntero& = puntero& - 22: IF puntero& < 1 THEN puntero& = 1: IF Sonido = "si" THEN BEEP
		CASE CHR$(0) + "P"
			puntero& = puntero& + 22: IF puntero& > va& THEN puntero& = va&: IF Sonido = "si" THEN BEEP
		CASE CHR$(27): EXIT DO
		CASE ELSE: IF Sonido = "si" THEN BEEP
	END SELECT
	CALL Panel(3, 2, 21, 78, 0, 47)
LOOP
EXIT SUB

Dibuja:
sitio% = 3
FOR k% = puntero& TO puntero& + 20
	IF k% > va& THEN
		EXIT FOR
	ELSE
		GET #3, k%
		LOCATE sitio%, 2: PRINT todo$;
		sitio% = sitio% + 1
	END IF
NEXT k%
RETURN

END SUB

SUB SigueCompras
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... "
Center 25, "Leyendo datos desde el servidor... Espere sin pulsar ninguna tecla"
CLOSE #27
OPEN "F:\kevinprg\kevindat\master.fct" FOR RANDOM SHARED AS #27 LEN = 70
FIELD #27, 70 AS linea$: final% = (LOF(27) / 70): selecc% = 0
IF final% = 0 THEN Box 5, 1, 7, 80: Center 6, "NO Hay NINGUNA Factura Para Visualizar": Espera: EXIT SUB
RANDOMIZE TIMER
rus = "f:\kevinprg\kevindat\" + MID$(STR$(FIX(RND * 10000)), 2, 3) + ".ver"
OPEN rus FOR RANDOM AS #3: va& = 1
FIELD #3, 70 AS todo$

dia = VAL(MID$(auxi, 1, 2))
mes = VAL(MID$(auxi, 4, 2))
ano = VAL(MID$(auxi, 7, 4))
desde& = DateToJulian(dia, mes, ano)
FOR k = 1 TO final%
	GET #27, k
	IF MID$(linea$, 1, 1) = " " THEN
		dia2% = VAL(MID$(linea$, 6, 2))
		mes2% = VAL(MID$(linea$, 8, 2))
		ano2% = VAL(MID$(linea$, 10, 4))
		IF DateToJulian(dia2%, mes2%, ano2%) >= desde& THEN
			CLOSE #7: OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #7 LEN = 18
			FIELD #7, 3 AS codigo$, 15 AS descripcion$: max7% = LOF(7) / 18
			FOR l = 1 TO max7%
				GET #7, l
				IF CVI(MID$(linea$, 24, 2)) = VAL(codigo$) THEN
					provex$ = codigo$: provix$ = descripcion$: EXIT FOR
				END IF
			NEXT l
			'linea$ 26 credito o contado   59 actualizada si/no  62 cancelada si/no
			num2 = STR$(CVL(MID$(linea$, 2, 4))): num2 = Trim$(MID$(num2, 2, 10)): larg% = 0
			FOR l = 1 TO 33: IF MID$(num2, l, 1) <> " " THEN larg% = larg% + 1
			NEXT l
			num2 = "F:\kevinprg\kevindat\" + STRING$(8 - larg%, "0") + MID$(num2, 1, larg%) + "." + provex$
			CLOSE #2: OPEN num2 FOR RANDOM AS #2 LEN = 40
			FIELD #2, 6 AS odigo$, 2 AS antid$, 8 AS recio$, 8 AS osto$, 8 AS ferta$, 2 AS oferantid$, 1 AS va$, 2 AS sitio$, 3 AS venci$
			maxx2% = LOF(2) / 40
			FOR l = 1 TO maxx2%
				GET #2, l
				IF CVI(sitio$) = mjfox THEN
					IF venci$ <> "   " THEN
						  IF MID$(venci$, 1, 1) = "A" OR MID$(venci$, 1, 1) = "B" OR MID$(venci$, 1, 1) = "C" THEN
								 IF MID$(venci$, 1, 1) = "A" THEN auxilior$ = "10/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4)
								 IF MID$(venci$, 1, 1) = "B" THEN auxilior$ = "11/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4)
								 IF MID$(venci$, 1, 1) = "C" THEN auxilior$ = "12/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4)
						  ELSE
								 auxilior$ = "0" + MID$(venci$, 1, 1) + "/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4)
						  END IF
					ELSE
						  auxilior$ = "  /    "
					END IF
					LSET todo$ = MID$(linea$, 6, 2) + "/" + MID$(linea$, 8, 2) + "/" + MID$(linea$, 10, 4) + " " + MID$(num2, 22, 8) + " " + provix$ + " " + STR$(CVI(antid$) + CVI(oferantid$)) + " " + STR$(CVD(recio$)) + "(" + auxilior$ + ")"
					PUT #3, va&: va& = va& + 1
				END IF
			NEXT l
		END IF
	END IF
NEXT k
'FOR k = final% TO 1 STEP -1
'     GET #27, k: IF MID$(linea$, 1, 1) = " " THEN comienzo% = comienzo% + 1
'     IF comienzo% > 9 THEN comienzo% = k: EXIT FOR
'NEXT k
'FOR k = comienzo% TO final%
'IF k > 0 THEN
'     GET #27, k
'     IF MID$(linea$, 1, 1) = " " THEN
'          selecc% = selecc% + 1: numfact(selecc%) = MID$(linea$, 2, 4)
'          provee(selecc%) = MID$(linea$, 24, 2): edofact(selecc%) = MID$(linea$, 26, 1) + MID$(linea$, 59, 1) + MID$(linea$, 62, 1)
'          fechaacc(selecc%) = MID$(linea$, 6, 8): transpor(selecc%) = MID$(linea$, 14, 10): fechafact(selecc%) = MID$(linea$, 51, 8): ubifact(selecc%) = MKI$(k)
'          bruto(selecc%) = MID$(linea$, 27, 8): neto(selecc%) = MID$(linea$, 35, 8): ivafact(selecc%) = MID$(linea$, 43, 8)
'     END IF
'END IF
'NEXT k
'IF selecc% = 0 THEN Box 5, 1, 7, 80: Center 6, "NO Hay NINGUNA Factura Para Visualizar": Espera: EXIT SUB
'OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$
'FOR k = 1 TO 199: GET #2, k: Code(k) = codigo$: Contenido(k) = descripcion$: NEXT k: CLOSE #2
'FOR k = 1 TO selecc%
'     FOR l = 1 TO 199
'          IF CVI(MID$(provee(k), 1, 2)) = VAL(Code(l)) THEN
'               provee(k) = Code(l) + Contenido(l): l = 199
'          END IF
'     NEXT l
'NEXT k: resal% = 0
'DO
'         COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box 1, 1, 3, 80
'         Center 2, "Consulta de una Factura de Compra."
'         COLOR colors(7, colorpref), colors(4, colorpref): Box 4, 1, 16, 80: Center 4, "Facturas Registradas": LOCATE 5, 2: PRINT "Factura:         Proveedor:       FechaFact:  FechaAcce:  Transcrita: F.Pago: "
'         Center 24, "[F3]Ir a fecha [F4]Suma montos seg£n fecha [F5]Edita [F8]Borra facturas"
'         Center 25, "Use Las Teclas de Direccion y Enter ; Presione Escape Para Salir "
'         IF resal% = 0 THEN
'             COLOR colors(7, colorpref), colors(4, colorpref)
'             LOCATE 6, 2: PRINT USING " ##############,³"; CVL(numfact(1)); : PRINT MID$(provee(1), 4, 15); "³ "; MID$(fechafact(1), 3, 2); "/"; MID$(fechafact(1), 1, 2); "/"; MID$(fechafact(1), 5, 4); "³ "; MID$(fechaacc(1), 1, 2); "/"; MID$(fechaacc(1), 3, 2); "/"; MID$(fechaacc(1), 5, 4); "³ "; transpor(1);
'             IF MID$(edofact(1), 1, 1) = "*" THEN PRINT " ³Contado ";  ELSE PRINT " ³Credito ";
'             'IF MID$(edofact(1), 2, 1) = "*" THEN PRINT "   SI   ";  ELSE PRINT "   NO   ";
'            LOCATE 18, 4: PRINT USING "Bruto Bs. ########,.##"; CVD(bruto(1)); : PRINT USING "   Impuesto Bs. ########,.##"; CVD(ivafact(1)); : PRINT USING "   Neto Bs. ########,.##"; CVD(neto(1));
'             COLOR colors(4, colorpref), colors(7, colorpref)
'         ELSE
'             COLOR colors(4, colorpref), colors(7, colorpref)
'             LOCATE 6, 2: PRINT USING " ##############,³"; CVL(numfact(1)); : PRINT MID$(provee(1), 4, 15); "³ "; MID$(fechafact(1), 3, 2); "/"; MID$(fechafact(1), 1, 2); "/"; MID$(fechafact(1), 5, 4); "³ "; MID$(fechaacc(1), 1, 2); "/"; MID$(fechaacc(1), 3, 2); "/"; MID$(fechaacc(1), 5, 4); "³ "; transpor(1);
'             IF MID$(edofact(1), 1, 1) = "*" THEN PRINT " ³Contado ";  ELSE PRINT " ³Credito ";
'             'IF MID$(edofact(1), 2, 1) = "*" THEN PRINT "   SI   ";  ELSE PRINT "   NO   ";
'             LOCATE 18, 4: PRINT USING "Bruto Bs. ########,.##"; CVD(bruto(1)); : PRINT USING "   Impuesto Bs. ########,.##"; CVD(ivafact(1)); : PRINT USING "   Neto Bs. ########,.##"; CVD(neto(1));
'             COLOR colors(4, colorpref), colors(7, colorpref)
'         END IF
'         FOR k = 2 TO 10
'              IF resal% = k THEN COLOR colors(7, colorpref), colors(4, colorpref)
'              IF k <= selecc% THEN
'                   LOCATE 5 + k, 2: PRINT USING "###############,³"; CVL(numfact(k)); : PRINT MID$(provee(k), 4, 15); "³ "; MID$(fechafact(k), 3, 2); "/"; MID$(fechafact(k), 1, 2); "/"; MID$(fechafact(k), 5, 4); "³ "; MID$(fechaacc(k), 1, 2); "/"; MID$(fechaacc(k), 3, 2); "/"; MID$(fechaacc(k), 5, 4); "³ "; transpor(k);
'                   IF MID$(edofact(k), 1, 1) = "*" THEN PRINT " ³Contado ";  ELSE PRINT " ³Credito ";
'                   'IF MID$(edofact(k), 2, 1) = "*" THEN PRINT "   SI   ";  ELSE PRINT "   NO   ";
'              ELSE
'                   LOCATE 5 + k, 2: PRINT SPACE$(78)
'              END IF
'              IF resal% = k THEN COLOR colors(4, colorpref), colors(7, colorpref)
'         NEXT k
'         IF resal% = 0 THEN resal% = 1
'         ventana% = selecc%: up% = final% - 10
'         IF up% < 1 THEN up% = 1
'         DO
'              kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
'              SELECT CASE kbd$
'                   CASE CHR$(0) + "H": GOSUB Arri
'                   CASE CHR$(0) + "P": GOSUB Abaj
'                   CASE CHR$(0) + CHR$(73): GOSUB Agup
'                   CASE CHR$(0) + CHR$(81): GOSUB Agdn
'                   CASE CHR$(0) + CHR$(61): GOSUB CalculaMontos: EXIT SUB
'                   CASE CHR$(0) + CHR$(62): GOSUB CalculaMontos: EXIT SUB
'                   CASE CHR$(0) + CHR$(63): eleg = resal%: GOSUB ModificaFactura
'                   CASE CHR$(0) + CHR$(66)
'                        Box 10, 5, 12, 75: Center 11, "¨SEGURO DESEA BORRAR ESTA FACTURA? Pulse S o N "
'                        llave$ = "": DO WHILE llave$ = "": llave$ = INKEY$: LOOP: llave$ = UCASE$(llave$)
'                        IF llave$ = "S" THEN
'                             eleg% = resal%
'                             CLOSE : OPEN "F:\kevinprg\kevindat\master.fct" FOR RANDOM SHARED AS #27 LEN = 70: FIELD #27, 70 AS linea$
'                             GET #27, CVI(ubifact(eleg%))
'                             IF numfact(eleg%) = MID$(linea$, 2, 4) THEN
'                                  RSET linea$ = "*" + MID$(linea$, 2, 69): PUT #27, CVI(ubifact(eleg%))
'                             END IF: CLOSE : EXIT SUB
'                        END IF
'                   CASE CHR$(13): eleg = resal%: EXIT DO
'                   CASE CHR$(27): resal% = 0: up% = 1: EXIT SUB
'                   CASE ELSE:  BEEP
'              END SELECT
'            l = 0
'            FOR k = 1 TO 10: l = l + 1   ': LOCATE 1, 1: PRINT "k="; k, "up%="; up%, "l="; l, "resal%="; resal%, "selecc%="; selecc%
'               IF resal% = l THEN COLOR colors(7, colorpref), colors(4, colorpref)
'               IF k <= selecc% THEN
'                    LOCATE 5 + l, 2: PRINT USING "###############,³"; CVL(numfact(k)); : PRINT MID$(provee(k), 4, 15); "³ "; MID$(fechafact(k), 3, 2); "/"; MID$(fechafact(k), 1, 2); "/"; MID$(fechafact(k), 5, 4); "³ "; MID$(fechaacc(k), 1, 2); "/"; MID$(fechaacc(k), 3, 2); "/"; MID$(fechaacc(k), 5, 4); "³ "; transpor(k);
'                    IF MID$(edofact(k), 1, 1) = "*" THEN PRINT " ³Contado ";  ELSE PRINT " ³Credito ";
'                    'IF MID$(edofact(k), 2, 1) = "*" THEN PRINT "   SI   ";  ELSE PRINT "   NO   ";
'               ELSE
'                    LOCATE 5 + l, 2: PRINT SPACE$(78)
'               END IF
'               IF resal% = l THEN LOCATE 18, 4: PRINT USING "Bruto Bs. ########,.##"; CVD(bruto(k)); : PRINT USING "   Impuesto Bs. ########,.##"; CVD(ivafact(k)); : PRINT USING "   Neto Bs. ########,.##"; CVD(neto(k));
'               IF resal% = l THEN COLOR colors(4, colorpref), colors(7, colorpref)
'            NEXT k
'         LOOP
'    salir = false
'    'linea$ 26 credito o contado   59 actualizada si/no  62 cancelada si/no
'    num2 = STR$(CVL(numfact(eleg))): num2 = Trim$(MID$(num2, 2, 10)): larg% = 0
'    FOR k = 1 TO 33: IF MID$(num2, k, 1) <> " " THEN larg% = larg% + 1
'    NEXT k
'    num2 = "F:\kevinprg\kevindat\" + STRING$(8 - larg%, "0") + MID$(num2, 1, larg%) + "." + MID$(provee(eleg), 1, 3)
'    CLOSE #2: OPEN num2 FOR RANDOM AS #2 LEN = 40: FIELD #2, 6 AS odigo$, 2 AS antid$, 8 AS recio$, 8 AS osto$, 8 AS ferta$, 2 AS oferantid$, 1 AS va$, 2 AS sitio$, 3 AS venci$: lleped% = LOF(2) / 40
'    CLOSE #1: OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
'    FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
'    seguir = true: apu% = 1: Box 4, 1, 6, 80
'    LOCATE 4, 2: PRINT "Factura:         Proveedor:       FechaFact:  FechaAcce:  Transcrita: F.Pago: "
'    LOCATE 5, 2: PRINT USING "###############,³"; CVL(numfact(eleg)); : PRINT MID$(provee(eleg), 3, 15); "³ "; MID$(fechafact(eleg), 3, 2); "/"; MID$(fechafact(eleg), 1, 2); "/"; MID$(fechafact(eleg), 5, 4); "³ "; MID$(fechaacc(eleg), 1, 2); "/"; MID$(fechaacc(eleg), 3, 2); "/"; MID$(fechaacc(eleg), 5, 4); "³ "; transpor(eleg);
'    IF MID$(edofact(eleg), 1, 1) = "*" THEN PRINT " ³Contado ";  ELSE PRINT " ³Credito ";
'    'IF MID$(edofact(eleg), 2, 1) = "*" THEN PRINT "   SI   ";  ELSE PRINT "   NO   ";
'    LOCATE 6, 4: PRINT USING "Bruto Bs. ########,.##"; CVD(bruto(eleg)); : PRINT USING "   Impuesto Bs. ########,.##"; CVD(ivafact(eleg)); : PRINT USING "   Neto Bs. ########,.##"; CVD(neto(eleg));
'    LOCATE 7, 1: PRINT "ÄDescripci¢nÄÄÄÄÄÄÄÄÄÄÄÄÄÂ% GanÂCantÄÂOferÄÂÄPVP ÄÄÄÄÄÂÄPVPOferÄÄÂÄÄCostoÄÄÄÄÄÂÄ";
'    LOCATE 23, 1: PRINT "ÄDescripci¢nÄÄÄÄÄÄÄÄÄÄÄÄÄÁ%ÄGanÁCantÄÁOferÄÁÄPVP ÄÄÄÄÄÁÄPVPOferÄÄÁÄÄCostoÄÄÄÄÄÁÄ";
'    Center 25, "[Esc]Salir [F9]Imprimir [F10]Salir  -Use las Teclas de Direccion-"
'
'   DO WHILE seguir
'        COLOR colors(7, colorpref), colors(4, colorpref): l = 7
'         FOR k = apu% - 7 TO apu% + 7
'              IF k = apu% THEN COLOR colors(4, colorpref), colors(7, colorpref)
'              l = l + 1: LOCATE l, 1: PRINT SPACE$(80); : LOCATE l, 1: porc = MKD$(0)
'              IF k > 0 AND k <= lleped% THEN
'                   GET #2, k: GET #1, CVI(sitio$): PRINT MID$(nomb2$, 1, 25); "³";
'                   IF CVD(osto$) > 0 THEN
'                        IF va$ = "I" THEN
'                             IF CVD(ferta$) > 0 THEN
'                                  porc = MKD$(100 - (((CVD(osto$) / (CVI(antid$)))) / (CVD(ferta$) / (VAL(Impuesto) / 100 + 1))) * 100)
'                             ELSE
'                                  porc = MKD$(100 - (((CVD(osto$) / (CVI(antid$)))) / (CVD(recio$) / (VAL(Impuesto) / 100 + 1))) * 100)
'                             END IF
'                        ELSE
'                             IF CVD(ferta$) > 0 THEN
'                                  porc = MKD$(100 - (((CVD(osto$) / (CVI(antid$)))) / CVD(ferta$)) * 100)
'                             ELSE
'                                  porc = MKD$(100 - (((CVD(osto$) / (CVI(antid$)))) / CVD(recio$)) * 100)
'                             END IF
'                        END IF
'                        IF CVD(porc) < 29.95 THEN COLOR colors(4, colorpref) + 16, colors(7, colorpref)
'                        PRINT USING "##.##"; CVD(porc);
'                   ELSE
'                        COLOR colors(4, colorpref) + 16, colors(7, colorpref): PRINT USING "##.##"; 0;
'                   END IF
'                   IF k = apu% THEN COLOR colors(4, colorpref), colors(7, colorpref) ELSE COLOR colors(7, colorpref), colors(4, colorpref)
'                   PRINT USING "³#####³"; CVI(antid$); : PRINT USING "#####³"; CVI(oferantid$); : PRINT USING "######,.##³"; CVD(recio$); : PRINT USING "######,.##³"; CVD(ferta$); : PRINT USING "########,.##³"; CVD(osto$); : PRINT va$;
'              ELSE
'                   PRINT SPACE$(25); "³     ³     ³     ³          ³          ³            ³ ";
'              END IF
'              IF k = apu% THEN COLOR colors(7, colorpref), colors(4, colorpref)
'         NEXT k
'              GET #2, apu%: GET #1, CVI(sitio$): LOCATE 24, 1: PRINT nomb2$; "  Fecha de Vencimiento:"; : auxiliar$ = ""
'              IF venci$ <> "   " THEN
'                   IF MID$(venci$, 1, 1) = "A" OR MID$(venci$, 1, 1) = "B" OR MID$(venci$, 1, 1) = "C" THEN
'                        IF MID$(venci$, 1, 1) = "A" THEN auxiliar$ = "10/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4)
'                        IF MID$(venci$, 1, 1) = "B" THEN auxiliar$ = "11/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4)
'                        IF MID$(venci$, 1, 1) = "C" THEN auxiliar$ = "12/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4)
'                   ELSE
'                        auxiliar$ = "0" + MID$(venci$, 1, 1) + "/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4)
'                   END IF
'                   PRINT auxiliar$
'              ELSE
'                   PRINT "  /         ";
'              END IF
'              kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
'              SELECT CASE kbd$
'                   CASE CHR$(0) + "H": apu% = apu% - 1: IF apu% < 1 THEN apu% = 1
'                   CASE CHR$(0) + "P": apu% = apu% + 1: IF apu% > lleped% THEN apu% = lleped%
'                   CASE CHR$(0) + CHR$(81): apu% = apu% + 14: IF apu% > lleped% THEN apu% = lleped%
'                   CASE CHR$(0) + CHR$(73): apu% = apu% - 14: IF apu% < 1 THEN apu% = 1
'                   CASE CHR$(0) + CHR$(67): Box 10, 10, 12, 70: Center 11, "Imprimiendo , espere por favor ...": GOSUB ImprimeFact
'                   CASE CHR$(0) + CHR$(68): EXIT DO
'                   CASE CHR$(27): EXIT DO
'                   CASE ELSE:  BEEP
'              END SELECT
'    LOOP
'LOOP
'CLOSE
END SUB

SUB SigueDevoluciones
' busca en devoluciones
dia = VAL(MID$(auxi, 1, 2))
mes = VAL(MID$(auxi, 4, 2))
ano = VAL(MID$(auxi, 7, 4))
CLOSE #1: OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
GET #1, mjfox
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... "
Center 25, "Leyendo datos desde el servidor... Espere sin pulsar ninguna tecla"
OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #2 LEN = 100
FIELD #2, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS Cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
selecc% = LOF(2) / 100
DIM Cedula(selecc%) AS STRING * 4, nombres(selecc%) AS STRING * 20
IF selecc% = 0 THEN CALL Center(8, "** No Hay Ningun Vendedor Registrado **"): Espera: EXIT SUB
FOR k% = 1 TO selecc%
	GET #2, k%: Cedula(k%) = Cedu$: nombres(k%) = nomb$
NEXT k%: CLOSE #2
hoy& = DateToJulian(VAL(MID$(DATE$, 4, 2)), VAL(MID$(DATE$, 1, 2)), VAL(MID$(DATE$, 7, 4)))

RANDOMIZE TIMER
rus = "f:\kevinprg\kevindat\" + MID$(STR$(FIX(RND * 10000)), 2, 3) + ".ver"
OPEN rus FOR RANDOM AS #3: va& = 1
FIELD #3, 70 AS todo$
DO
	linea$ = ""
	SELECT CASE dia
		CASE 1: archivo$ = "01"
		CASE 2: archivo$ = "02"
		CASE 3: archivo$ = "03"
		CASE 4: archivo$ = "04"
		CASE 5: archivo$ = "05"
		CASE 6: archivo$ = "06"
		CASE 7: archivo$ = "07"
		CASE 8: archivo$ = "08"
		CASE 9: archivo$ = "09"
		CASE ELSE: archivo$ = MID$(STR$(dia), 2, 2)
	END SELECT
	SELECT CASE mes
		CASE 1: archivo$ = archivo$ + "01"
		CASE 2: archivo$ = archivo$ + "02"
		CASE 3: archivo$ = archivo$ + "03"
		CASE 4: archivo$ = archivo$ + "04"
		CASE 5: archivo$ = archivo$ + "05"
		CASE 6: archivo$ = archivo$ + "06"
		CASE 7: archivo$ = archivo$ + "07"
		CASE 8: archivo$ = archivo$ + "08"
		CASE 9: archivo$ = archivo$ + "09"
		CASE ELSE: archivo$ = archivo$ + MID$(STR$(mes), 2, 2)
	END SELECT
	archivo$ = "f:\kevinprg\kevindat\" + archivo$ + MID$(STR$(ano), 2, 4) + ".crd"
	CLOSE #2: OPEN archivo$ FOR RANDOM SHARED AS #2 LEN = 30
	FIELD #2, 30 AS all$: max% = LOF(2) / 30
	FOR k% = 1 TO max%
		GET #2, k%
		IF MID$(all$, 1, 1) = "J" THEN
			FOR l% = 1 TO selecc%
				IF CVL(MID$(all$, 19, 4)) = CVL(Cedula(l%)) THEN
					linea$ = MID$(archivo$, 22, 2) + "/" + MID$(archivo$, 24, 2) + "/" + MID$(archivo$, 26, 4) + " " + nombres(l%) + " " + MID$(all$, 13, 2) + ":" + MID$(all$, 15, 2) + ":" + MID$(all$, 17, 2) + " "
					EXIT FOR
				END IF
			NEXT l%
		ELSE
			IF MID$(all$, 2, 6) = codigo2$ THEN
				linea$ = linea$ + SPACE$(8) + STR$(CVI(MID$(all$, 8, 2))) + SPACE$(12) + STR$(CVD(MID$(all$, 10, 8)))
				LSET todo$ = SPACE$(70): LSET todo$ = linea$: PUT #3, va&: va& = va& + 1
			END IF
		END IF
	NEXT k%
	dia = dia + 1
	IF dia > 31 THEN dia = 1: mes = mes + 1
	IF mes > 12 THEN mes = 1: ano = ano + 1
	dia2% = dia: mes2% = mes: ano2% = ano
	cuando& = DateToJulian(dia2%, mes2%, ano2%)
	IF cuando& > hoy& THEN
		EXIT DO
	END IF
LOOP

END SUB

SUB SigueVentas
' busca en ventas
dia = VAL(MID$(auxi, 1, 2))
mes = VAL(MID$(auxi, 4, 2))
ano = VAL(MID$(auxi, 7, 4))
CLOSE #1: OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
GET #1, mjfox
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... "
Center 25, "Leyendo datos desde el servidor... Espere sin pulsar ninguna tecla"
OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #2 LEN = 100
FIELD #2, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS Cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
selecc% = LOF(2) / 100
DIM Cedula(selecc%) AS STRING * 4, nombres(selecc%) AS STRING * 20
IF selecc% = 0 THEN CALL Center(8, "** No Hay Ningun Vendedor Registrado **"): Espera: EXIT SUB
FOR k% = 1 TO selecc%
	GET #2, k%: Cedula(k%) = Cedu$: nombres(k%) = nomb$
NEXT k%: CLOSE #2
hoy& = DateToJulian(VAL(MID$(DATE$, 4, 2)), VAL(MID$(DATE$, 1, 2)), VAL(MID$(DATE$, 7, 4)))

RANDOMIZE TIMER
rus = "f:\kevinprg\kevindat\" + MID$(STR$(FIX(RND * 10000)), 2, 3) + ".ver"
OPEN rus FOR RANDOM AS #3: va& = 1
FIELD #3, 70 AS todo$
DO
	linea$ = ""
	SELECT CASE dia
		CASE 1: archivo$ = "01"
		CASE 2: archivo$ = "02"
		CASE 3: archivo$ = "03"
		CASE 4: archivo$ = "04"
		CASE 5: archivo$ = "05"
		CASE 6: archivo$ = "06"
		CASE 7: archivo$ = "07"
		CASE 8: archivo$ = "08"
		CASE 9: archivo$ = "09"
		CASE ELSE: archivo$ = MID$(STR$(dia), 2, 2)
	END SELECT
	SELECT CASE mes
		CASE 1: archivo$ = archivo$ + "01"
		CASE 2: archivo$ = archivo$ + "02"
		CASE 3: archivo$ = archivo$ + "03"
		CASE 4: archivo$ = archivo$ + "04"
		CASE 5: archivo$ = archivo$ + "05"
		CASE 6: archivo$ = archivo$ + "06"
		CASE 7: archivo$ = archivo$ + "07"
		CASE 8: archivo$ = archivo$ + "08"
		CASE 9: archivo$ = archivo$ + "09"
		CASE ELSE: archivo$ = archivo$ + MID$(STR$(mes), 2, 2)
	END SELECT
	archivo$ = "f:\kevinprg\kevinvnt\" + archivo$ + MID$(STR$(ano), 2, 4) + ".vnt"
	CLOSE #2: OPEN archivo$ FOR RANDOM SHARED AS #2 LEN = 30
	FIELD #2, 30 AS all$: max% = LOF(2) / 30
	FOR k% = 2 TO max%
		GET #2, k%
		IF MID$(all$, 1, 1) = "J" THEN
			FOR l% = 1 TO selecc%
				IF CVL(MID$(all$, 19, 4)) = CVL(Cedula(l%)) THEN
					linea$ = MID$(archivo$, 22, 2) + "/" + MID$(archivo$, 24, 2) + "/" + MID$(archivo$, 26, 4) + " " + nombres(l%) + " " + MID$(all$, 13, 2) + ":" + MID$(all$, 15, 2) + ":" + MID$(all$, 17, 2) + " "
					EXIT FOR
				END IF
			NEXT l%
		ELSE
			IF MID$(all$, 2, 6) = codigo2$ THEN
				linea$ = linea$ + SPACE$(8) + STR$(CVI(MID$(all$, 8, 2))) + SPACE$(12) + STR$(CVD(MID$(all$, 10, 8)))
				LSET todo$ = SPACE$(70): LSET todo$ = linea$: PUT #3, va&: va& = va& + 1
			END IF
		END IF
	NEXT k%
	dia = dia + 1
	IF dia > 31 THEN dia = 1: mes = mes + 1
	IF mes > 12 THEN mes = 1: ano = ano + 1
	dia2% = dia: mes2% = mes: ano2% = ano
	cuando& = DateToJulian(dia2%, mes2%, ano2%)
	IF cuando& > hoy& THEN
		EXIT DO
	END IF
LOOP
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

