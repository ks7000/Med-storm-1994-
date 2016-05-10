'operaciones con tarjetas telefonicas
DEFINT A-Z
DECLARE SUB Tarjetas ()
DECLARE SUB Espera (edi%)
DECLARE SUB Box (Row1, Col1, Row2, Col2)
DECLARE SUB Box2 (Row1, Col1, Row2, Col2)
DECLARE SUB Center (Row, text$)
DECLARE SUB Termometro (va%, total%, edo%, text$, fila%, columna%)
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$, tr$)
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Trim$ (X$)
DECLARE FUNCTION DateToJulian& (dat$, caso%)
DECLARE FUNCTION Escoge% (que%)
DECLARE FUNCTION DateValid (cad$)
DECLARE FUNCTION TrataFechas$ (dat$, fechaentra$, fechasale$)
DECLARE FUNCTION TrataHoras$ (dat$, horaentra$, horasale$)
DECLARE SUB Indexa ()
DECLARE SUB Compra ()
DECLARE SUB Nueva ()
DECLARE SUB Saldo ()
DECLARE SUB Capital (cual%)
DECLARE SUB Registro ()
DECLARE SUB Retiro ()
DECLARE SUB Modifica ()
DECLARE SUB Preferencias ()
DECLARE SUB Devolucion ()
DECLARE SUB Reincorporar ()
DECLARE SUB Resumen ()
DECLARE SUB Anular ()
DECLARE SUB Reporte ()
DECLARE SUB Toma ()
' procedimientos externos
	DECLARE SUB PopUp (Y%, X%, h%, W%, C%, B%, s%, Z%)
	DECLARE SUB ShutUp ()
	DECLARE FUNCTION Verify (Row%, Prompt$)
	DECLARE SUB BackFill (Row%, col%, Rows%, Cols%, Attribute%)

COMMON SHARED guia, colorpref: IF colorpref = 0 THEN colorpref = 1
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR X = 1 TO 10: READ colors(X, Ct): NEXT X: NEXT Ct
DIM SHARED Fun(12) AS STRING * 2, devolver AS SINGLE, escape, grabGav AS STRING * 1
CONST true = -1: CONST false = NOT true: CLOSE
'declaracion personalizada de variables
TYPE dbf 'para tarjetas.dbf
	acti AS STRING * 1
	nomb AS STRING * 15
	pvp  AS LONG
	cant AS SINGLE
	porc AS DOUBLE
END TYPE
TYPE idx 'para vivos.idx y todos.idx
	nomb AS STRING * 15
	sit  AS SINGLE
END TYPE
TYPE cta 'para cuenta.cta
	oper  AS STRING * 1
	fech  AS STRING * 8
	hora  AS STRING * 8
	nota  AS STRING * 10
	sit   AS SINGLE
	cant  AS SINGLE
	debe  AS DOUBLE
	haber AS DOUBLE
	Saldo AS DOUBLE
END TYPE
DIM SHARED tarje AS dbf, vivos AS idx, todos AS idx, cuenta AS cta
'apertura de archivos
ON ERROR GOTO ErrorTrap
CLS

OPEN "f:\kevinprg\tarjetas\tarjetas.dbf" FOR RANDOM AS #1 LEN = 32
OPEN "f:\kevinprg\tarjetas\vivos.idx" FOR RANDOM AS #2 LEN = 19
OPEN "f:\kevinprg\tarjetas\todos.idx" FOR RANDOM AS #3 LEN = 19
OPEN "f:\kevinprg\tarjetas\cuenta.dbf" FOR RANDOM AS #4 LEN = 59
IF LOF(4) / 59 = 0 THEN
	cuenta.oper = "0"
	cuenta.fech = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4)
	cuenta.hora = MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MID$(TIME$, 7, 2)
	cuenta.nota = "Apertura  "
	cuenta.sit = 0
	cuenta.cant = 0
	cuenta.debe = 0
	cuenta.haber = 0
	cuenta.Saldo = 0
	PUT #4, 1, cuenta
END IF
ON TIMER(5) GOSUB AbreGav
TIMER ON

CALL Tarjetas
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... "
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

SUB Anular
PopUp 4, 5, 16, 70, 31, 1, 1, 0
Center 5, "ANULACION de compra de tarjetas telefonicas"
Center 25, "ANULACION de compra de tarjetas telefonicas"
max% = LOF(4) / 59: salir = false: auxi% = max%
DIM sald AS DOUBLE
DO WHILE salir = false
	GET #4, auxi%, cuenta
	IF cuenta.oper = "1" THEN
			LOCATE 7, 10
			GET #1, cuenta.sit, tarje
			PRINT tarje.nomb; " ... ";
			fecha2$ = cuenta.fech: fecha2$ = TrataFechas(fecha2$, "ddmmaaaa", "dd/mm/aaaa")
			hora2$ = TrataHoras$(cuenta.hora, "hhmmss", "hh:mma")
			PRINT fecha2$; " ... "; hora2$; " ... "; cuenta.nota
			CALL Center(8, "Nota: " + cuenta.nota)
			res% = Verify(13, "¨ Desea ANULAR esta compra de tarjetas ")
			IF res% = true THEN
				IF tarje.cant >= cuenta.cant THEN
					salir = true
					tarje.cant = tarje.cant - cuenta.cant: PUT #1, cuenta.sit, tarje
					OPEN "f:\kevinprg\tarjetas\temp.dbf" FOR RANDOM AS #17 LEN = 59
					max% = LOF(4) / 59
					CALL Termometro(1, max%, 0, "Anulando la compra", 10, 17)
					FOR k = 1 TO max%
						CALL Termometro(k, max%, 1, "Anulando la compra", 10, 17)
						GET #4, k%, cuenta
						IF k% < auxi% THEN
							PUT #17, k%, cuenta
						END IF
						IF k% = auxi% THEN
							GET #4, k% - 1, cuenta
							sald = cuenta.Saldo
						END IF
						IF k% > auxi% THEN
							cuenta.Saldo = sald - cuenta.debe + cuenta.haber
							sald = cuenta.Saldo
							PUT #17, k% - 1, cuenta
						END IF
					NEXT k
					CLOSE #17: CLOSE #4
					KILL "F:\kevinprg\tarjetas\cuenta.dbf"
					NAME "F:\kevinprg\tarjetas\temp.dbf" AS "F:\kevinprg\tarjetas\cuenta.dbf"
					OPEN "f:\kevinprg\tarjetas\cuenta.dbf" FOR RANDOM AS #4 LEN = 59
				END IF
			END IF
		BEEP
		IF salir = false THEN res% = Verify(15, "¨ Desea SALIR AL MENU PRINCIPAL ")
		IF res% = true THEN salir = true
	END IF
	auxi% = auxi% - 1
	IF auxi% = 0 THEN salir = true
LOOP
CALL ShutUp
END SUB

SUB Box (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿"; : FOR A = Row1 + 1 TO Row2 - 1: LOCATE A, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT A: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Box2 (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "²"; STRING$(BoxWidth - 2, "²"); "²"; : FOR A = Row1 + 1 TO Row2 - 1: LOCATE A, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT A: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Capital (cual%)
'cual%=1 aumento, otro valor retira
max1% = LOF(1) / 32
max4% = LOF(4) / 59
GET #4, max4%, cuenta
FOR k% = 1 TO max1%
		GET #1, k%, tarje
		IF tarje.acti = " " THEN van# = van# + (tarje.pvp * tarje.cant)
NEXT k%
PopUp 4, 5, 16, 70, 31, 1, 1, 0
IF cual% = 1 THEN
	Center 25, "Aumento de Capital aportando efectivo"
	Center 5, "Aumento de Capital aportando efectivo"
ELSE
	Center 25, "Retiro de Capital (del efectivo disponible)"
	Center 5, "Retiro de Capital (del efectivo disponible)"
END IF
IF cual% <> 1 THEN
	LOCATE 6, 20: PRINT "Efectivo disponible:"; : PRINT USING "########,"; (cuenta.Saldo - van#);
	IF (cuenta.Saldo - van#) <= 0 THEN Center 13, "NO HAY EFECTIVO DISPONIBLE: 0 Bs.": Center 14, "(use la opcion 6 de Totalizar Saldo para m s informaci¢n)": Espera 0: ShutUp: EXIT SUB
END IF
IF cual% = 1 THEN
	LOCATE 11, 9: PRINT "Monto a aportar al capital:    ²"; SPACE$(8); "²"
ELSE
	LOCATE 11, 9: PRINT "Monto a retirar del capital:   ²"; SPACE$(8); "²"
END IF
Center 25, "Introduzca el monto o pulse Escape para abortar": auxi$ = "0": devolver = false
IF cual% = 1 THEN
	DO WHILE VAL(auxi$) = 0
		auxi$ = EntraDatoNumero(11, 40, 8, auxi$, 0)
		IF devolver = true THEN ShutUp: EXIT SUB
	LOOP
ELSE
	DO WHILE VAL(auxi$) = 0 OR VAL(auxi$) > (cuenta.Saldo - van#)
		auxi$ = EntraDatoNumero(11, 40, 8, auxi$, 0)
		IF devolver = true THEN ShutUp: EXIT SUB
	LOOP
END IF
monto# = VAL(auxi$)
Center 25, "Coloque cualquier nota que desee, Escape para abortar"
LOCATE 13, 7: PRINT "¨Desea anotar alguna observaci¢n?²"; SPACE$(10); "²"
auxi$ = MeteDatoLetra(13, 40, 10, SPACE$(10), "0")
IF devolver = true THEN ShutUp: EXIT SUB
IF cual% = 1 THEN
	h% = Verify(10, "¨Desea Guardar el aumento de capital")
ELSE
	h% = Verify(10, "¨Desea Guardar el retiro de capital")
END IF
IF h% = true THEN
		cuenta.cant = 0
		cuenta.nota = auxi$
		cuenta.fech = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4)
		cuenta.hora = MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MID$(TIME$, 7, 2)
		cuenta.sit = 0
		IF cual% = 1 THEN
			cuenta.oper = "4"
			cuenta.haber = monto#
			cuenta.debe = 0
			cuenta.Saldo = cuenta.Saldo - cuenta.debe + cuenta.haber
		ELSE
			cuenta.oper = "5"
			cuenta.haber = 0
			cuenta.debe = monto#
			cuenta.Saldo = cuenta.Saldo - cuenta.debe + cuenta.haber
		END IF
		PUT #4, (max4% + 1), cuenta
END IF
ShutUp
END SUB

SUB Center (Row, text$)
IF Row = 25 THEN COLOR 0, 15: LOCATE 25, 1: PRINT SPACE$(80);
LOCATE Row, 41 - LEN(text$) / 2: PRINT text$;
IF Row = 25 THEN COLOR colors(7, colorpref), colors(4, colorpref)
END SUB

SUB Compra
PopUp 4, 5, 16, 70, 31, 1, 1, 0
Center 25, "Compra de tarjetas telefonicas: escoja la tarjeta"
Center 5, "Compra de tarjetas telefonicas: escoja la tarjeta"
' completar el m¢dulo:si no hay suficiente efectivo no dejar comprar
'max1% = LOF(1) / 32
'max4% = LOF(4) / 59
'GET #4, max4%, cuenta
'FOR k% = 1 TO max1%
'    GET #1, k%, tarje
'    IF tarje.acti = " " THEN van# = van# + (tarje.pvp * tarje.cant)
'NEXT k%
'LOCATE 6, 20: PRINT "Efectivo disponible:"; : PRINT USING "########,"; (cuenta.Saldo - van#);
'IF (cuenta.Saldo - van#) <= 0 THEN Center 13, "NO HAY EFECTIVO DISPONIBLE: 0 Bs.": Center 14, "(use la opcion 6 de Totalizar Saldo para m s informaci¢n)": Espera 0: ShutUp: EXIT SUB
'
g% = Escoge(2): IF g% <= 0 THEN ShutUp: EXIT SUB
GET #1, g%, tarje
Center 7, ("Tarjeta seleccionada: " + tarje.nomb + " Bs." + STR$(tarje.pvp))
LOCATE 11, 9: PRINT "Cantidad de tarjetas a comprar:²"; SPACE$(7); "²"
Center 25, "Introduzca la cantidad de tarjetas a comprar o Escape para abortar": auxi$ = "0": devolver = false
DO WHILE VAL(auxi$) = 0
	auxi$ = EntraDatoNumero(11, 40, 7, auxi$, 0)
	IF devolver = true THEN ShutUp: EXIT SUB
LOOP
canti% = VAL(auxi$)
Center 25, "Coloque cualquier nota que desee, Escape para abortar"
LOCATE 13, 7: PRINT "¨Desea anotar alguna observaci¢n?²"; SPACE$(10); "²"
auxi$ = MeteDatoLetra(13, 40, 10, SPACE$(10), "0")
IF devolver = true THEN ShutUp: EXIT SUB
h% = Verify(16, "¨Desea Guardar la operaci¢n de compra")
IF h% = true THEN
	max% = LOF(4) / 59
	GET #4, max%, cuenta
	cuenta.cant = canti%
	tarje.cant = tarje.cant + cuenta.cant: PUT #1, g%, tarje
	cuenta.nota = auxi$
	cuenta.oper = "1"
	cuenta.fech = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4)
	cuenta.hora = MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MID$(TIME$, 7, 2)
	cuenta.sit = g%
	cuenta.debe = cuenta.cant * tarje.pvp * (100 - tarje.porc) / 100
	cuenta.haber = cuenta.cant * tarje.pvp
	cuenta.Saldo = cuenta.Saldo - cuenta.debe + cuenta.haber
	PUT #4, (max% + 1), cuenta
END IF
CALL ShutUp
END SUB

DEFSNG A-Z
FUNCTION DateToJulian& (dat$, caso%)
IF caso% = 1 THEN   ' formato ddmmaaaa
		Day% = VAL(MID$(dat$, 1, 2))
		Month% = VAL(MID$(dat$, 3, 2))
		Year% = VAL(MID$(dat$, 5, 4))
ELSE                ' formato dd/mm/aaaa
		Day% = VAL(MID$(dat$, 1, 2))
		Month% = VAL(MID$(dat$, 4, 2))
		Year% = VAL(MID$(dat$, 7, 4))
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

DEFINT A-Z
FUNCTION DateValid% (cad$)
Day% = VAL(MID$(cad$, 1, 2))
Month% = VAL(MID$(cad$, 4, 2))
Year% = VAL(MID$(cad$, 7, 4))
		SELECT CASE Month%                   'verify date within month
			
			 CASE 4, 6, 9, 11                  'months that should
					SELECT CASE Day%               'only  have
						 CASE 1 TO 30                '30 days
						 CASE ELSE                   'otherwise
								DateValid% = 0           'not valid
								EXIT FUNCTION
					END SELECT

			 CASE 1, 3, 5, 7, 8, 10, 12        'months that
					SELECT CASE Day%               'should have
						 CASE 1 TO 31                '31 days
						 CASE ELSE                   'otherwise
								DateValid% = 0           'not valid
								EXIT FUNCTION
					END SELECT
								 
			 CASE 2                            'February
					SELECT CASE Year% MOD 4        'account for leap years
						 CASE 0                      'a leap year
								SELECT CASE Day%
									 CASE 1 TO 29          'may have 29 days
										CASE ELSE                        '
											 DateValid% = 0    'not valid
											 EXIT FUNCTION
								END SELECT
						 CASE ELSE                   'not a leap year
								SELECT CASE Day%
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
DateValid% = true                       'Date passes all tests
END FUNCTION

SUB Devolucion
PopUp 4, 5, 15, 70, 31, 1, 1, 0
Center 25, "DEVOLUCION de tarjetas telefonicas: escoja la tarjeta"
Center 5, "DEVOLUCION de tarjetas telefonicas: escoja la tarjeta"
g% = Escoge(2): IF g% <= 0 THEN ShutUp: EXIT SUB
GET #1, g%, tarje
max% = LOF(4) / 59
GET #4, max%, cuenta
cuenta.cant = -1
tarje.cant = tarje.cant + 1
PUT #1, g%, tarje
cuenta.nota = SPACE$(10)
cuenta.oper = "7"
cuenta.fech = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4)
cuenta.hora = MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MID$(TIME$, 7, 2)
cuenta.sit = g%
cuenta.debe = 0
cuenta.haber = 0
PUT #4, (max% + 1), cuenta
gav$ = "com1": OPEN gav$ FOR RANDOM AS #27: PRINT #27, "A": PRINT #27, "A": PRINT #27, "A": CLOSE #27
CALL ShutUp
END SUB

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
			CASE CHR$(0) + CHR$(68): Fun(10) = "si": EXIT DO
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
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna%: PRINT "²"; elccion$; "²"
EntraDatoNumero$ = elccion$: EXIT FUNCTION
END FUNCTION

FUNCTION Escoge% (que%)
Center 25, " Escoja una tarjeta con enter o Escape para abortar"
max% = LOF(que%) / 19
IF max% = 0 THEN Escoge% = 0: PopUp 12, 27, 2, 30, 31, 1, 1, 0: Center 13, "No hay tarjetas creadas": Espera 0: ShutUp: EXIT FUNCTION
DIM nombre(max%) AS STRING * 15, canti(max%) AS SINGLE, pvp(max%) AS LONG, sitio(max%) AS SINGLE
FOR k% = 1 TO max%
	GET #que%, k%, vivos
	nombre(k%) = vivos.nomb
	sitio(k%) = vivos.sit
	GET #1, vivos.sit, tarje
	canti(k%) = tarje.cant: pvp(k) = tarje.pvp
NEXT k%
resal% = 1: up% = 1
PopUp 7, 26, 12, 27, 31, 1, 1, 0
GOSUB Dibuja
DO
	kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
	SELECT CASE kbd$
		CASE CHR$(0) + "H"          'Arriba
			resal% = resal% - 1
			IF resal% < 1 THEN resal% = 1: up% = up% - 1
			IF up% < 1 THEN up% = 1
		CASE CHR$(0) + "P"          'Abajo
			resal% = resal% + 1
			IF max% > 10 THEN
				IF resal% > 10 THEN resal% = 10: up% = up% + 1
				IF (up% + 9) > max% THEN up% = max% - 9
			ELSE
				IF resal% > max% THEN resal% = max%
			END IF
		CASE CHR$(13): EXIT DO
		CASE CHR$(27): Escoge% = -1: ShutUp: EXIT FUNCTION
		CASE ELSE:  BEEP
	END SELECT
	GOSUB Dibuja
LOOP
CALL ShutUp
Escoge% = sitio(up% + resal% - 1)
EXIT FUNCTION
Dibuja:
FOR k% = 1 TO 10
	IF k% <= max% THEN
		LOCATE 7 + k%, 28
		IF (k% + up% - 1) <= max% THEN
			PRINT nombre(k% + up% - 1); "  ";
			PRINT USING "######,"; pvp(k% + up% - 1);
			IF resal% = k THEN
				LOCATE 7 + resal%, 27: PRINT "û"
			ELSE
				LOCATE 7 + k, 27: PRINT " "
			END IF
		ELSE
			LOCATE 7 + k%, 27: PRINT SPACE$(25);
		END IF
	ELSE
		LOCATE 7 + k%, 27: PRINT SPACE$(25);
	END IF
NEXT k%

BackFill 8, 27, 10, 25, 289
BackFill 7 + resal%, 27, 1, 25, 31
RETURN

END FUNCTION

SUB Espera (edi%)
IF edi% = 1 THEN
	Center 25, "Pulse [Esc] para abortar y cualquier otra para continuar..."
ELSE
	Center 25, "Presione Cualquier Tecla Para Continuar..."
END IF
kbd$ = ""
WHILE kbd$ = ""
	kbd$ = INKEY$
WEND
IF kbd$ = CHR$(27) THEN escape = true
END SUB

SUB Indexa
PopUp 5, 5, 2, 20, 31, 1, 1, 0
Center 6, "Espere, por favor..."
CLOSE #2: CLOSE #3
KILL "f:\kevinprg\tarjetas\*.idx"
OPEN "f:\kevinprg\tarjetas\vivos.idx" FOR RANDOM AS #2 LEN = 19
OPEN "f:\kevinprg\tarjetas\todos.idx" FOR RANDOM AS #3 LEN = 19
DIM a1 AS STRING * 15, a2 AS STRING * 15
DIM a3 AS INTEGER, a4 AS INTEGER
max% = LOF(1) / 32
IF max% = 1 THEN
	GET #1, 1, tarje
	vivos.nomb = tarje.nomb: todos.nomb = tarje.nomb
	vivos.sit = 1: todos.sit = 1
	PUT #2, 1, vivos: PUT #2, 1, todos: ShutUp: EXIT SUB
ELSE
	l% = 1: m% = 1
	FOR k% = 1 TO max%
		GET #1, k%, tarje
		todos.nomb = tarje.nomb: todos.sit = k%: PUT #3, l%, todos: l% = l% + 1
		IF tarje.acti = " " THEN
			vivos.nomb = tarje.nomb: vivos.sit = k%
			PUT #2, m%, vivos: m% = m% + 1
		END IF
	NEXT k%
	max% = LOF(2) / 19
	FOR k% = 1 TO max% - 1
		FOR l% = k% + 1 TO max%
			GET #2, k%, vivos: a1 = vivos.nomb: a3 = vivos.sit
			GET #2, l%, vivos: a2 = vivos.nomb: a4 = vivos.sit
			IF a1 > a2 THEN
				SWAP a1, a2: SWAP a3, a4
				vivos.nomb = a1: vivos.sit = a3
				PUT #2, k%, vivos
				vivos.nomb = a2: vivos.sit = a4
				PUT #2, l%, vivos
			END IF
		NEXT l%
	NEXT k%
	max% = LOF(3) / 19
	FOR k% = 1 TO max% - 1
		FOR l% = k% + 1 TO max%
			GET #3, k%, todos: a1 = todos.nomb: a3 = todos.sit
			GET #3, l%, todos: a2 = todos.nomb: a4 = todos.sit
			IF a1 > a2 THEN
				SWAP a1, a2: SWAP a3, a4
				todos.nomb = a1: todos.sit = a3
				PUT #3, k%, todos
				todos.nomb = a2: todos.sit = a4
				PUT #3, l%, todos
			END IF
		NEXT l%
	NEXT k%
END IF
CALL ShutUp
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
MeteDatoLetra$ = eleccion$
EXIT FUNCTION
Sumar:
FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR 0, 15: LOCATE fila%, columna%: PRINT "²"; eleccion$; "²"
RETURN
END FUNCTION

SUB Modifica
PopUp 4, 5, 15, 70, 31, 1, 1, 0
Center 25, "Modificaci¢n de tarjetas telefonicas: escoja la tarjeta"
Center 5, "Modificaci¢n de tarjetas telefonicas: escoja la tarjeta"
g% = Escoge(2): IF g% <= 0 THEN ShutUp: EXIT SUB
GET #1, g%, tarje
Center 5, "           Modifique la tarjeta seleccionada.          "
Center 6, ("Tarjeta seleccionada: " + tarje.nomb + " Bs." + STR$(tarje.pvp))
LOCATE 8, 10: PRINT "Descripcion de la tarjeta    :²"; tarje.nomb; "²"
LOCATE 10, 10: PRINT "Precio de la tarjeta con iva :²"; tarje.pvp; "²"
LOCATE 12, 10: PRINT "Porcentaje de ganancia       :²"; tarje.porc; "²"
LOCATE 14, 10: PRINT "Cantidad de tarjetas         :²"; tarje.cant; "²";
Center 25, " [F9]SALVAR y salir [F10]Abortar cambios de tarjeta"
puntero% = 1: Fun(9) = "no": Fun(10) = "no": devolver = false
DO
	SELECT CASE puntero%
		CASE 1
			tarje.nomb = MeteDatoLetra(8, 40, 15, tarje.nomb, "0")
		CASE 2
			auxi$ = STR$(tarje.pvp)
			auxi$ = EntraDatoNumero(10, 40, 7, auxi$, 0)
			tarje.pvp = VAL(auxi$)
		CASE 3
			auxi$ = STR$(tarje.porc)
			auxi$ = EntraDatoNumero(12, 40, 3, auxi$, 1)
			tarje.porc = VAL(auxi$)
		CASE 4
			auxi$ = STR$(tarje.cant)
			auxi$ = EntraDatoNumero(14, 40, 4, auxi$, 1)
			tarje.cant = VAL(auxi$)
	END SELECT
	IF devolver = true THEN
		devolver = false: puntero% = puntero% - 1: IF puntero% < 1 THEN puntero% = 4
	ELSE
		puntero% = puntero% + 1: IF puntero% > 4 THEN puntero% = 1
	END IF
	IF Fun(9) = "si" THEN 'salvar en archivo
			PUT #1, g%, tarje
			va% = LOF(4) / 59: GET #4, va%, cuenta
			cuenta.oper = "6"
			cuenta.fech = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4)
			cuenta.hora = MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MID$(TIME$, 7, 2)
			cuenta.nota = "Modificac."
			cuenta.sit = g%: cuenta.cant = 0
			cuenta.debe = 0: va% = va% + 1: cuenta.haber = 0
			PUT #4, va%, cuenta
		EXIT DO
	END IF
	IF Fun(10) = "si" THEN 'aborta la creacion
		k% = Verify(13, "¨Seguro desea abortar la modificaci¢n")
		IF k% = true THEN EXIT DO
	END IF
LOOP
CALL ShutUp
CALL Indexa
END SUB

SUB Nueva
PopUp 4, 5, 15, 70, 31, 1, 1, 0
Center 5, " Escriba los datos de la tarjeta a crear"
LOCATE 7, 10: PRINT "Descripcion de la tarjeta    :²"; SPACE$(15); "²"
LOCATE 9, 10: PRINT "Precio de la tarjeta con iva :²"; SPACE$(7); "²"
LOCATE 11, 10: PRINT "Cantidad de tarjetas a crear :²"; SPACE$(7); "²"
LOCATE 13, 10: PRINT "Porcentaje de ganancia       :²"; SPACE$(7); "²"
Center 25, " [F9]SALVAR y salir [F10]Abortar creaci¢n de tarjeta"
max% = LOF(1) / 32
tarje.acti = " "
tarje.nomb = SPACE$(15)
tarje.pvp = 0
tarje.cant = 0
tarje.porc = 0: puntero% = 1: Fun(9) = "no": Fun(10) = "no": devolver = false
DO
	SELECT CASE puntero%
		CASE 1
			tarje.nomb = MeteDatoLetra(7, 40, 15, tarje.nomb, "0")
		CASE 2
			auxi$ = STR$(tarje.pvp)
			auxi$ = EntraDatoNumero(9, 40, 7, auxi$, 0)
			tarje.pvp = VAL(auxi$)
		CASE 3
			auxi$ = STR$(tarje.cant)
			auxi$ = EntraDatoNumero(11, 40, 7, auxi$, 0)
			tarje.cant = VAL(auxi$)
		CASE 4
			auxi$ = STR$(tarje.porc)
			auxi$ = EntraDatoNumero(13, 40, 7, auxi$, 1)
			tarje.porc = VAL(auxi$)
	END SELECT
	IF devolver = true THEN
		devolver = false: puntero% = puntero% - 1: IF puntero% < 1 THEN puntero% = 4
	ELSE
		puntero% = puntero% + 1: IF puntero% > 4 THEN puntero% = 1
	END IF
	IF Fun(9) = "si" THEN 'salvar en archivo
			max% = max% + 1: PUT #1, max%, tarje
			va% = LOF(4) / 59: GET #4, va%, cuenta
			cuenta.oper = "2"
			cuenta.fech = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4)
			cuenta.hora = MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MID$(TIME$, 7, 2)
			cuenta.nota = "Aporte    "
			cuenta.sit = max%
			cuenta.cant = tarje.cant
			cuenta.debe = 0: va% = va% + 1
			cuenta.haber = tarje.cant * tarje.pvp
			cuenta.Saldo = cuenta.Saldo + tarje.cant * tarje.pvp
			PUT #4, va%, cuenta
		EXIT DO
	END IF
	IF Fun(10) = "si" THEN 'aborta la creacion
		k% = Verify(13, "¨Seguro desea abortar la creaci¢n")
		IF k% = true THEN EXIT DO
	END IF
LOOP
CALL ShutUp
CALL Indexa
END SUB

SUB Preferencias
END SUB

SUB Registro
PopUp 3, 5, 20, 74, 31, 1, 1, 0
Center 5, "Registro de movimientos hechos seg£n fecha"
fecha1$ = MID$(DATE$, 4, 2) + "/" + MID$(DATE$, 1, 2) + "/" + MID$(DATE$, 7, 4)
fecha2$ = MID$(DATE$, 4, 2) + "/" + MID$(DATE$, 1, 2) + "/" + MID$(DATE$, 7, 4)
LOCATE 7, 19: PRINT "¨ DESDE cu l fecha ?": Fun(9) = "no"
DO
	Center 25, "Introduzca la fecha de inicio, [F9] para abortar"
	fecha1$ = MeteDatoLetra(7, 40, 10, fecha1$, "0")
	IF Fun(9) = "si" THEN ShutUp: EXIT SUB
	LOCATE 9, 19: PRINT "¨ HASTA cu l fecha ?"
	Center 25, "Introduzca la fecha de finalizaci¢n, [F9] para abortar"
	fecha2$ = MeteDatoLetra(9, 40, 10, fecha2$, "0")
	IF Fun(9) = "si" THEN ShutUp: EXIT SUB
	IF DateValid(fecha1$) = true THEN
		IF DateValid(fecha2$) = true THEN
			IF DateToJulian(fecha1$, 0) <= DateToJulian(fecha2$, 0) THEN
				EXIT DO
			ELSE
				PopUp 17, 15, 2, 50, 31, 1, 1, 0: Center 18, "La fecha de inicio es MAYOR que la fecha final": Espera 0: ShutUp
			END IF
		ELSE
			PopUp 17, 20, 2, 40, 31, 1, 1, 0: Center 18, "La fecha de finalizaci¢n NO ES VALIDA": Espera 0: ShutUp
		END IF
	ELSE
		PopUp 17, 20, 2, 40, 31, 1, 1, 0: Center 18, "La fecha de inicio NO ES VALIDA": Espera 0: ShutUp
	END IF
LOOP
ShutUp
PopUp 3, 3, 20, 74, 31, 1, 1, 0
max4% = (LOF(4) / 59): l% = 1: fechaux$ = fecha1$
Center 5, "Registro de movimientos hechos seg£n fecha"
Center 25, "Registro de movimientos hechos seg£n fecha"
LOCATE 7, 4
PRINT fecha1$; " Operaci¢n           Nota"; SPACE$(14); "   DEBE   HABER   SALDO"

encontrado = false: primero = 1: ultimo = max4%
WHILE NOT encontrado AND primero <= ultimo
	aur& = primero
	aur& = aur& + ultimo
	aur& = aur& / 2
	central = aur&
 
	GET #4, central, cuenta

	IF DateToJulian(cuenta.fech, 1) = DateToJulian(fecha1$, 0) THEN
			encontrado = true
	ELSE
		IF DateToJulian(cuenta.fech, 1) > DateToJulian(fecha1$, 0) THEN
			ultimo = central - 1
		ELSE
			primero = central + 1
		END IF
	END IF
WEND
IF encontrado = false THEN
	GET #4, 1, cuenta
	CALL ShutUp: PopUp 3, 3, 20, 74, 31, 1, 1, 0
	Center 13, "El d¡a de inicio(" + fecha1$ + ") no tiene operaciones. Pruebe otra fecha"
	aux$ = TrataFechas$(cuenta.fech, "ddmmaaaa", "dd/mm/aa")
	Center 14, "El primer d¡a de operaciones es:" + aux$
	CALL Espera(1): ShutUp: EXIT SUB
ELSE
	FOR k = central TO 1 STEP -1
		GET #4, k, cuenta
		IF DateToJulian(cuenta.fech, 1) <> DateToJulian(fecha1$, 0) THEN
			centrol = k + 1: EXIT FOR
		END IF
	NEXT k
END IF

FOR k% = centrol TO max4%
GET #4, k%, cuenta
IF DateToJulian(cuenta.fech, 1) >= DateToJulian(fecha1$, 0) AND DateToJulian(cuenta.fech, 1) <= DateToJulian(fecha2$, 0) THEN
	LOCATE 7 + l%, 5
	IF fechaux$ <> (MID$(cuenta.fech, 1, 2) + "/" + MID$(cuenta.fech, 3, 2) + "/" + MID$(cuenta.fech, 5, 4)) THEN
		LOCATE 7 + l%, 4
		PRINT MID$(cuenta.fech, 1, 2); "/"; MID$(cuenta.fech, 3, 2); "/"; MID$(cuenta.fech, 5, 4); " Operaci¢n           Nota"; SPACE$(14); "   DEBE   HABER   SALDO"
		l% = l% + 1:  LOCATE 7 + l%, 5
		fechaux$ = (MID$(cuenta.fech, 1, 2) + "/" + MID$(cuenta.fech, 3, 2) + "/" + MID$(cuenta.fech, 5, 4))
	END IF
	PRINT MID$(cuenta.hora, 1, 2); ":"; MID$(cuenta.hora, 3, 2); " ";
	SELECT CASE cuenta.oper
		CASE "0": PRINT "Apertura de registro"; STRING$(18, ".");
		CASE "1"
			PRINT "Compra de tarjetas..";
			GET #1, cuenta.sit, tarje
			PRINT tarje.nomb; "...";
		CASE "2": PRINT "Tarjeta nueva......."; : GET #1, cuenta.sit, tarje: PRINT tarje.nomb; "...";
		CASE "3": PRINT "Tarjeta borrada....."; : GET #1, cuenta.sit, tarje: PRINT tarje.nomb; "...";
		CASE "4": PRINT "Aumento de capital.."; cuenta.nota; STRING$(8, ".");
		CASE "5": PRINT "Retiro de capital..."; cuenta.nota; STRING$(8, ".");
		CASE "6"
			PRINT "Modificaci¢n tarj...";
			GET #1, cuenta.sit, tarje
			PRINT tarje.nomb; "...";
		CASE "7"
			PRINT "Devoluci¢n tarjeta..";
			GET #1, cuenta.sit, tarje: PRINT tarje.nomb; "...";
		CASE "8": PRINT "Tarjeta reincorpor.."; : GET #1, cuenta.sit, tarje: PRINT tarje.nomb; "...";
		CASE "9": PRINT "Venta de tarjetas..."; : GET #1, cuenta.sit, tarje: PRINT tarje.nomb; "...";
	END SELECT
	GOSUB DebeHaberSaldo
	l% = l% + 1
	IF l% > 14 THEN
		l% = 0: escape = false: Espera 1: IF escape = true THEN ShutUp: EXIT SUB
		ShutUp
		PopUp 3, 3, 20, 74, 31, 1, 1, 0
		Center 5, "Registro de movimientos hechos seg£n fecha"
	END IF
ELSE
	EXIT FOR
END IF
NEXT k%
Center 23, "Visualizacion de registros finalizada"
Espera 0
ShutUp
EXIT SUB

DebeHaberSaldo:
	LOCATE 7 + l%, 47
	PRINT USING "########, "; cuenta.debe;
	PRINT USING "########, "; cuenta.haber;
	PRINT USING "########,"; cuenta.Saldo;
RETURN

END SUB

SUB Reincorporar
PopUp 4, 5, 16, 70, 31, 1, 1, 0
Center 25, "REINCORPORAR tarjetas telefonicas: escoja la tarjeta"
Center 5, "REINCORPORAR  tarjetas telefonicas: escoja la tarjeta"
max1% = LOF(1) / 32
IF max1% = 0 THEN PopUp 12, 27, 2, 30, 31, 1, 1, 0: Center 13, "No hay tarjetas creadas": Espera 0: ShutUp: ShutUp: EXIT SUB
g% = Escoge(2): IF g% <= 0 THEN ShutUp: EXIT SUB
GET #1, g%, tarje
Center 7, ("Tarjeta seleccionada: " + tarje.nomb + " Bs." + STR$(tarje.pvp))
h% = Verify(10, "¨Est  seguro que desea REINCORPORAR esta tarjeta")
IF h% = true THEN
	tarje.acti = " "
	PUT #1, g%, tarje
	max4% = LOF(4) / 59
	GET #4, max4%, cuenta
	cuenta.cant = 0
	cuenta.nota = SPACE$(10)
	cuenta.oper = "8"
	cuenta.fech = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4)
	cuenta.hora = MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MID$(TIME$, 7, 2)
	cuenta.sit = g%
	cuenta.debe = 0
	cuenta.haber = 0
	cuenta.Saldo = cuenta.Saldo - cuenta.debe + cuenta.haber
	PUT #4, (max4% + 1), cuenta
END IF
CALL Indexa
CALL ShutUp
END SUB

SUB Reporte

sino% = Verify(13, "¨Desea un reporte impreso del saldo de las tarjetas")
IF sino% = true THEN
	max% = (LOF(1) / 32): l% = 0: van# = 0
	CLOSE #10: OPEN "lpt1" FOR OUTPUT AS #10
	WIDTH #10, 255
	PRINT #10, STRING$(80, "-")
	PRINT #10, STRING$(80, "-")
	hoy$ = TrataFechas(DATE$, "mm/dd/aaaa", "dd/mm/aaaa")
	PRINT #10, "Fecha: "; hoy$, "Hora:"; TIME$
		FOR k% = 1 TO max%
			GET #1, k%, tarje
			IF tarje.acti = " " THEN
				PRINT #10, tarje.nomb; ": hay";
				PRINT #10, USING "####,"; tarje.cant;
				PRINT #10, " de ";
				PRINT #10, USING "#####,"; tarje.pvp;
				PRINT #10, STRING$(26, ".");
				PRINT #10, USING "**########,"; (tarje.cant * tarje.pvp)
				van# = van# + (tarje.pvp * tarje.cant)
			END IF
		NEXT k%
	PRINT #10, CHR$(10);
	PRINT #10, "Sub-Total:"; STRING$(51, ".");
	PRINT #10, USING "**########,"; van#
	PRINT #10, CHR$(10);
	PRINT #10, "Efectivo :"; STRING$(51, ".");
	max2% = LOF(4) / 59: GET #4, max2%, cuenta
	PRINT #10, USING "**########,"; (cuenta.Saldo - van#)
	PRINT #10, CHR$(10);
	PRINT #10, "T O T A L:"; STRING$(51, ".");
	PRINT #10, USING "**########,"; (cuenta.Saldo)
	PRINT #10, STRING$(80, "-")
	PRINT #10, STRING$(80, "-")
	PRINT #10, CHR$(12);
END IF

END SUB

SUB Resumen
tod% = LOF(3) / 19: max4% = (LOF(4) / 59)
REDIM NomTar(tod%, 1) AS STRING * 15, MonCom(tod%) AS DOUBLE, MonGan(tod%) AS DOUBLE, MonVen(tod%) AS DOUBLE
FOR k = 0 TO tod%
	MonCom(k) = 0
	MonGan(k) = 0
	MonVen(k) = 0
	NomTar(k, 0) = SPACE$(15)
	NomTar(k, 1) = SPACE$(15)
NEXT k%
PopUp 3, 5, 20, 74, 31, 1, 1, 0
Center 5, "RESUMEN DE MOVIMIENTOS seg£n fecha"
fecha1$ = MID$(DATE$, 4, 2) + "/" + MID$(DATE$, 1, 2) + "/" + MID$(DATE$, 7, 4)
fecha2$ = MID$(DATE$, 4, 2) + "/" + MID$(DATE$, 1, 2) + "/" + MID$(DATE$, 7, 4)
LOCATE 7, 19: PRINT "¨ DESDE cu l fecha ?": Fun(9) = "no"
DO
	Center 25, "Introduzca la fecha de inicio, [F2]Ultimo retiro capital [F9] para abortar"
	fecha1$ = MeteDatoLetra(7, 40, 10, fecha1$, "0")
	IF Fun(9) = "si" THEN ShutUp: EXIT SUB
	IF Fun(2) = "si" THEN
		Fun(2) = "no"
		PopUp 9, 18, 5, 46, 31, 1, 1, 0
		COLOR colors(7, colorpref) + 16, colors(4, colorpref)
		Box 10, 19, 12, 62: Center 11, "Buscando £ltimo retiro de capital..."
		COLOR colors(7, colorpref), colors(4, colorpref): encontrado = false
		FOR k% = max4% TO 1 STEP -1
			GET #4, k%, cuenta
			IF cuenta.oper = "5" THEN
				fecha1$ = cuenta.fech: encontrado = true
				fecha1$ = TrataFechas(fecha1$, "ddmmaaaa", "dd/mm/aaaa")
				EXIT FOR
			END IF
		NEXT k%
		CALL ShutUp
		IF encontrado% = false THEN
			Center 13, "No se encontr¢ ning£n retiro de capital. Intente de nuevo"
			CALL Espera(0): ShutUp: EXIT SUB
		END IF
		LOCATE 7, 41: PRINT fecha1$
	END IF
	LOCATE 9, 19: PRINT "¨ HASTA cu l fecha ?"
	Center 25, "Introduzca la fecha de finalizaci¢n, [F9] para abortar"
	fecha2$ = MeteDatoLetra(9, 40, 10, fecha2$, "0")
	IF Fun(9) = "si" THEN ShutUp: EXIT SUB
	IF DateValid(fecha1$) = true THEN
		IF DateValid(fecha2$) = true THEN
			IF DateToJulian(fecha1$, 0) <= DateToJulian(fecha2$, 0) THEN
				EXIT DO
			ELSE
				PopUp 17, 15, 2, 50, 31, 1, 1, 0: Center 18, "La fecha de inicio es MAYOR que la fecha final": Espera 0: ShutUp
			END IF
		ELSE
			PopUp 17, 20, 2, 40, 31, 1, 1, 0: Center 18, "La fecha de finalizaci¢n NO ES VALIDA": Espera 0: ShutUp
		END IF
	ELSE
		PopUp 17, 20, 2, 40, 31, 1, 1, 0: Center 18, "La fecha de inicio NO ES VALIDA": Espera 0: ShutUp
	END IF
LOOP
ShutUp
PopUp 3, 3, 20, 74, 31, 1, 1, 0
max4% = (LOF(4) / 59): l% = 1: fechaux$ = fecha1$
Center 5, "RESUMEN DE MOVIMIENTOS seg£n fecha"
Center 25, "RESUMEN DE MOVIMIENTOS seg£n fecha"
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... "
COLOR colors(7, colorpref), colors(4, colorpref)
encontrado = false: primero = 1: ultimo = max4%
WHILE NOT encontrado AND primero <= ultimo
	central = (primero + ultimo) / 2: GET #4, central, cuenta
	IF DateToJulian(cuenta.fech, 1) = DateToJulian(fecha1$, 0) THEN
			encontrado = true
	ELSE
		IF DateToJulian(cuenta.fech, 1) > DateToJulian(fecha1$, 0) THEN
				ultimo = central - 1
		ELSE
				primero = central + 1
		END IF
	END IF
WEND
IF encontrado = false THEN
	CALL ShutUp: PopUp 3, 3, 20, 74, 31, 1, 1, 0
	Center 13, "El d¡a de inicio(" + fecha1$ + ") no tiene operaciones. Pruebe otra fecha"
	CALL Espera(0): ShutUp: EXIT SUB
ELSE
	FOR k = central TO 1 STEP -1
		GET #4, k, cuenta
		IF DateToJulian(cuenta.fech, 1) <> DateToJulian(fecha1$, 0) THEN
			centrol = k + 1: EXIT FOR
		END IF
	NEXT k
END IF

FOR k% = centrol TO max4%
	GET #4, k%, cuenta
	IF DateToJulian(cuenta.fech, 1) >= DateToJulian(fecha1$, 0) AND DateToJulian(cuenta.fech, 1) <= DateToJulian(fecha2$, 0) THEN
		
		SELECT CASE cuenta.oper
			CASE "0": PRINT "Apertura de registro"; STRING$(18, ".");
			CASE "1"
				'PRINT "Compra de tarjetas..";
				GET #1, cuenta.sit, tarje
				NomTar(cuenta.sit, 0) = tarje.nomb
				MonCom(cuenta.sit) = MonCom(cuenta.sit) + cuenta.debe
				MonGan(cuenta.sit) = MonGan(cuenta.sit) + cuenta.haber - cuenta.debe
			CASE "4" ': PRINT "Aumento de capital.."; cuenta.nota; STRING$(8, ".");
			CASE "5" ': PRINT "Retiro de capital..."; cuenta.nota; STRING$(8, ".");
			CASE "9"
				'PRINT "Venta de tarjetas...";
				GET #1, cuenta.sit, tarje
				NomTar(cuenta.sit, 1) = tarje.nomb
				MonVen(cuenta.sit) = MonVen(cuenta.sit) + tarje.pvp
		END SELECT
	ELSE
		EXIT FOR
	END IF
NEXT k%
ShutUp
PopUp 3, 3, 20, 74, 31, 1, 1, 0: l% = 0
FOR k = 1 TO tod%
	MonCom(0) = MonCom(0) + MonCom(k)
	MonGan(0) = MonGan(0) + MonGan(k)
	MonVen(0) = MonVen(0) + MonVen(k)
NEXT k%
Center 4, "COMPRAS HECHAS DESDE EL " + fecha1$ + " HASTA EL " + fecha2$
LOCATE 6, 10: PRINT "Tarjeta        ³   Monto comprado   ³   Monto ganancia   "
FOR k = 1 TO tod%
	IF MonVen(k) > 0 THEN
		LOCATE 7 + l%, 10: l% = l% + 1
		IF MonGan(k) > 0 THEN
			PRINT NomTar(k, 0); : PRINT USING " ###################,"; MonCom(k);
			PRINT USING " ###################,"; MonGan(k)
		END IF
	END IF
NEXT k
LOCATE 8 + l%, 10: PRINT "Total monto comprado:";
PRINT USING "#################,"; MonCom(0): l% = l% + 1
LOCATE 8 + l%, 10: PRINT "Total monto ganancia:";
PRINT USING "#################,"; MonGan(0): l% = l% + 1
LOCATE 8 + l%, 10: PRINT "GRAN TOTAL COMPRADO :";
PRINT USING "#################,"; MonGan(0) + MonCom(0): l% = l% + 1
Espera 0
ShutUp
PopUp 3, 3, 20, 74, 31, 1, 1, 0: l% = 0
Center 4, "VENTAS  HECHAS DESDE EL " + fecha1$ + " HASTA EL " + fecha2$
LOCATE 6, 20: PRINT "Tarjeta        ³   Monto vendido    "
FOR k = 1 TO tod%
	IF MonVen(k) > 0 THEN
		LOCATE 7 + l%, 20: l% = l% + 1
		PRINT NomTar(k, 1); : PRINT USING "####################,"; MonVen(k)
	END IF
NEXT k
LOCATE 8 + l%, 20: PRINT "Monto vendido :";
PRINT USING "####################,"; MonVen(0): l% = l% + 1
Espera 0
ShutUp
END SUB

SUB Retiro
PopUp 4, 5, 16, 70, 31, 1, 1, 0
Center 25, "BORRAR tarjetas telefonicas: escoja la tarjeta"
Center 5, "BORRAR  tarjetas telefonicas: escoja la tarjeta"
max1% = LOF(1) / 32
IF max1% = 0 THEN PopUp 12, 27, 2, 30, 31, 1, 1, 0: Center 13, "No hay tarjetas creadas": Espera 0: ShutUp: ShutUp: EXIT SUB
g% = Escoge(2): IF g% <= 0 THEN ShutUp: EXIT SUB
GET #1, g%, tarje
Center 7, ("Tarjeta seleccionada: " + tarje.nomb + " Bs." + STR$(tarje.pvp))
IF tarje.cant > 0 THEN
	PopUp 12, 23, 2, 36, 31, 1, 1, 0: Center 13, "Hay existencia, NO SE PUEDE BORRAR": Espera 0: ShutUp: ShutUp: EXIT SUB
END IF
h% = Verify(10, "¨Est  seguro que desea borrar esta tarjeta")
IF h% = true THEN
	tarje.acti = "*"
	PUT #1, g%, tarje
	max4% = LOF(4) / 59
	GET #4, max4%, cuenta
	cuenta.cant = 0
	cuenta.nota = SPACE$(10)
	cuenta.oper = "3"
	cuenta.fech = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4)
	cuenta.hora = MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MID$(TIME$, 7, 2)
	cuenta.sit = g%
	cuenta.debe = 0
	cuenta.haber = 0
	cuenta.Saldo = cuenta.Saldo - cuenta.debe + cuenta.haber
	PUT #4, (max4% + 1), cuenta
END IF
CALL Indexa
CALL ShutUp
END SUB

SUB Saldo
PopUp 4, 5, 20, 70, 31, 1, 1, 0
Center 5, "Saldo de tarjetas telefonicas:incluye efectivo y tarjetas"
Center 25, "Saldo de tarjetas telefonicas:incluye efectivo y tarjetas"
max% = (LOF(1) / 32): l% = 0: van# = 0
IF max% = 0 THEN
	LOCATE 7, 6: PRINT "No hay tarjetas registradas"; STRING$(34, "."); "      0"; : EXIT SUB
ELSE
	FOR k% = 1 TO max%
		GET #1, k%, tarje
		IF tarje.acti = " " THEN
			l% = l% + 1
			LOCATE l% + 6, 6: PRINT tarje.nomb; ": hay";
			PRINT USING "####,"; tarje.cant;
			PRINT " de ";
			PRINT USING "#####,"; tarje.pvp;
			PRINT STRING$(26, ".");
			LOCATE l% + 6, 63: PRINT USING "##########,"; (tarje.cant * tarje.pvp); : van# = van# + (tarje.pvp * tarje.cant)
			IF l% > 10 THEN l% = l% + 1: GOSUB Nuev
		END IF
	NEXT k%
END IF
l% = l% + 2: IF l% > 10 THEN l% = l% + 1: GOSUB Nuev: l% = l% + 1
LOCATE l% + 6, 6: PRINT "Sub-Total:"; STRING$(49, ".");
LOCATE l% + 6, 64: PRINT USING "**#######,"; van#; : l% = l% + 1
LOCATE l% + 6, 6: PRINT "Efectivo :"; STRING$(49, ".");
max2% = LOF(4) / 59: GET #4, max2%, cuenta
LOCATE l% + 6, 64: PRINT USING "**#######,"; (cuenta.Saldo - van#);
l% = l% + 1
LOCATE l% + 6, 6: PRINT "T O T A L:"; STRING$(49, ".");
LOCATE l% + 6, 64: PRINT USING "**#######,"; (cuenta.Saldo);
Espera 0
ShutUp
EXIT SUB

Nuev:
	LOCATE l% + 6, 6: PRINT "Van"; STRING$(56, ".");
	LOCATE l% + 6, 65: PRINT USING "**######,"; van#;
	l% = 1: Espera 0: ShutUp
	PopUp 4, 5, 20, 70, 31, 1, 1, 0
	Center 5, "Saldo de tarjetas telefonicas:incluye efectivo y tarjetas"
	LOCATE l% + 6, 6: PRINT "Vienen"; STRING$(54, ".");
	LOCATE l% + 6, 65: PRINT USING "**######,"; van#;
RETURN
END SUB

SUB Tarjetas
DO
	COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box2 1, 5, 23, 75
	Center 2, "Operaciones disponibles con tarjetas telefonicas"
	Center 4, "1.- Compra de tarjetas                          "
	Center 6, "2.- Adici¢n de un nuevo tipo de tarjeta         "
	Center 8, "3.- Retiro de un tipo de tarjeta                "
	Center 10, "4.- Aumento de capital de trabajo               "
	Center 12, "5.- Retiro de capital de trabajo                "
	Center 13, "6.- Totalizar saldo(incluye efectivo y tarjetas)"
	Center 14, "7.- Ver registro de movimientos por fecha       "
	Center 15, "8.- Modificar un tipo de tarjeta                "
	Center 16, "9.- Ordenar e indexar datos                     "
	Center 17, "A.- ANULACION de compra de un tipo de tarjeta   "
	Center 18, "D.- DEVOLUCION de una tarjeta                   "
	Center 19, "R.- REINCORPORACION de un tipo de tarjeta       "
	Center 20, "S.- Sumatoria de compras y ventas               "
	Center 21, "I.- Imprimir en papel el total de tarjetas      "
	Center 22, "V.- Venta de tarjetas SIN abrir gavetero        "
	Center 25, "Pulse del 0 al 9 o D o R para ejecutar, Escape para salir"
	kbd$ = INKEY$: DO WHILE kbd$ = "": kbd$ = INKEY$: LOOP: kbd$ = UCASE$(kbd$)
	SELECT CASE kbd$
		CASE "1": CALL Compra
		CASE "2": CALL Nueva
		CASE "3": CALL Retiro
		CASE "4": CALL Capital(1)
		CASE "5": CALL Capital(0)
		CASE "6": CALL Saldo
		CASE "7": CALL Registro
		CASE "8": CALL Modifica
		CASE "9": CALL Indexa
		CASE "S": CALL Resumen
		CASE "D": CALL Devolucion
		CASE "R": CALL Reincorporar
		CASE "A": CALL Anular
		CASE "I": CALL Reporte
		CASE "V": CALL Toma
		CASE CHR$(27): EXIT DO
	END SELECT
LOOP
END SUB

SUB Termometro (va%, total%, edo%, text$, fila%, columna%)
IF edo% = 0 THEN Box2 fila%, columna%, fila% + 6, columna% + 31
LOCATE fila% + 1, columna% + 1: PRINT "0%                        100%"
IF edo% = 0 THEN LOCATE fila% + 2, columna% + 1: PRINT STRING$(30, "±")
LOCATE fila% + 2, columna% + 1: PRINT STRING$(INT((va% / total%) * 30), "Û")
LOCATE fila% + 3, columna% + 1: PRINT "Van"; va%; " de "; total%;
LOCATE fila% + 4, columna% + 9: PRINT USING "###.## % Hecho "; ((va% / total%) * 100)
LOCATE fila% + 5, columna% + 1: PRINT text$
END SUB

SUB Toma
Center 25, " Escoja una tarjeta con enter o Escape para abortar"
max& = LOF(2) / 19
IF max& = 0 THEN
	CLOSE #1: CLOSE #2: CLOSE #4
	PopUp 12, 27, 2, 30, 31, 1, 1, 0: Center 13, "No hay tarjetas creadas": CALL Espera(2): CALL ShutUp: EXIT SUB
END IF
DIM nombre(max&) AS STRING * 15, siti(max&) AS SINGLE, pvp(max&) AS LONG
l% = 1
FOR k% = 1 TO max&
	GET #2, k%, vivos
	GET #1, vivos.sit, tarje
	IF tarje.cant > 0 THEN
		nombre(l%) = vivos.nomb: pvp(l%) = tarje.pvp
		siti(l%) = vivos.sit: l% = l% + 1
	END IF
NEXT k%
IF l% - 1 = 0 THEN
	PopUp 12, 25, 2, 32, 31, 1, 1, 0: Center 13, "No hay tarjetas de ning£n tipo": CALL Espera(2): CALL ShutUp: EXIT SUB
END IF
max& = l% - 1: resal% = 1: up% = 1
PopUp 7, 26, 12, 27, 31, 1, 1, 0
GOSUB Dibuja2
DO
	kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
	SELECT CASE kbd$
		CASE CHR$(0) + "H"          'Arriba
			resal% = resal% - 1
			IF resal% < 1 THEN resal% = 1: up% = up% - 1
			IF up% < 1 THEN up% = 1
		CASE CHR$(0) + "P"          'Abajo
			resal% = resal% + 1
			IF max& > 10 THEN
				IF resal% > 10 THEN resal% = 10: up% = up% + 1
				IF (up% + 9) > max& THEN up% = max& - 9
			ELSE
				IF resal% > max& THEN resal% = max&
			END IF
		CASE CHR$(13): EXIT DO
		CASE CHR$(27): CALL ShutUp: EXIT SUB
		CASE ELSE:  BEEP
	END SELECT
	GOSUB Dibuja2
LOOP
g% = siti(up% + resal% - 1)
	max& = LOF(4) / 59
	GET #4, max&, cuenta
	cuenta.cant = 1
	GET #1, g%, tarje
	tarje.cant = tarje.cant - 1
	PUT #1, g%, tarje
	cuenta.nota = SPACE$(10)
	cuenta.oper = "9"
	cuenta.fech = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4)
	cuenta.hora = MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MID$(TIME$, 7, 2)
	cuenta.sit = g%
	cuenta.debe = 0
	cuenta.haber = 0
	PUT #4, (max& + 1), cuenta
'gav$ = "com1": OPEN gav$ FOR RANDOM AS #29: PRINT #29, "A": PRINT #29, "A": PRINT #29, "A": CLOSE #29
CALL ShutUp
EXIT SUB

Dibuja2:
FOR k% = 1 TO 10
	IF k% <= max& THEN
		LOCATE 7 + k%, 28
		IF (k% + up% - 1) <= max& THEN
			PRINT nombre(k% + up% - 1); "  ";
			PRINT USING "######,"; pvp(k% + up% - 1);
			IF resal% = k THEN
				LOCATE 7 + resal%, 27: PRINT "û"
			ELSE
				LOCATE 7 + k, 27: PRINT " "
			END IF
		ELSE
			LOCATE 7 + k%, 27: PRINT SPACE$(25);
		END IF
	ELSE
		LOCATE 7 + k%, 27: PRINT SPACE$(25);
	END IF
NEXT k%

BackFill 8, 27, 10, 25, 289
BackFill 7 + resal%, 27, 1, 25, 31
RETURN
END SUB

FUNCTION TrataFechas$ (dat$, fechaentra$, fechasale$)
fechaentra$ = LCASE$(fechaentra$)
fechasale$ = LCASE$(fechasale$)
tomadia% = false: tomames% = false: tomaano% = false
IF INSTR(1, fechaentra$, "d") > 0 THEN
	IF INSTR(1, fechaentra$, "dd") > 0 AND NOT tomadia% THEN
		dia% = VAL(MID$(dat$, INSTR(1, fechaentra$, "dd"), 2))
		tomadia% = true
	END IF
	IF INSTR(1, fechaentra$, "d") > 0 AND NOT tomadia% THEN
		dia% = VAL(MID$(dat$, INSTR(1, fechaentra$, "d"), 1))
		tomadia% = true
	END IF
END IF
IF INSTR(1, fechaentra$, "m") > 0 THEN
	IF INSTR(1, fechaentra$, "mm") > 0 AND NOT tomames% THEN
		mes% = VAL(MID$(dat$, INSTR(1, fechaentra$, "mm"), 2))
		tomames% = true
	END IF
	IF INSTR(1, fechaentra$, "m") > 0 AND NOT tomames% THEN
		mes% = VAL(MID$(dat$, INSTR(1, fechaentra$, "m"), 1))
		tomames% = true
	END IF
END IF
IF INSTR(1, fechaentra$, "a") > 0 THEN
	IF INSTR(1, fechaentra$, "aaaa") > 0 AND NOT tomaano% THEN
		ano% = VAL(MID$(dat$, INSTR(1, fechaentra$, "aaaa"), 4))
		tomaano% = true
	END IF
	IF INSTR(1, fechaentra$, "aa") > 0 AND NOT tomaano% THEN
		ano% = VAL(MID$(dat$, INSTR(1, fechaentra$, "aa"), 2))
		tomaano% = true
	END IF
END IF
IF dia% < 10 THEN
	dia2$ = "0" + MID$(STR$(dia%), 2, 1)
ELSE
	dia2$ = MID$(STR$(dia%), 2, 2)
END IF
IF mes% < 10 THEN
	mes2$ = "0" + MID$(STR$(mes%), 2, 1)
ELSE
	mes2$ = MID$(STR$(mes%), 2, 2)
END IF
IF INSTR(fechasale$, "a") > 0 THEN
	IF INSTR(fechasale$, "aaaa") > 0 THEN
		IF ano% >= 80 AND ano% < 100 THEN
			ano2$ = "19" + MID$(STR$(ano%), 2, 2)
		ELSEIF ano% >= 0 AND ano% < 80 THEN
			IF ano% < 10 THEN
				ano2$ = "200" + MID$(STR$(ano%), 2, 1)
			ELSE
				ano2$ = "20" + MID$(STR$(ano%), 2, 1)
			END IF
		ELSE
			ano2$ = MID$(STR$(ano%), 2, 4)
		END IF
		MID$(fechasale$, INSTR(fechasale$, "aaaa"), 4) = ano2$
	ELSE
		IF ano% < 100 THEN
			ano2$ = MID$(STR$(ano%), 2, 2)
		ELSE
			ano2$ = MID$(STR$(ano%), 4, 2)
		END IF
		MID$(fechasale$, INSTR(fechasale$, "aa"), 2) = ano2$
	END IF
END IF
IF INSTR(fechasale$, "d") > 0 THEN
	MID$(fechasale$, INSTR(fechasale$, "dd"), 2) = dia2$
END IF
IF INSTR(fechasale$, "m") > 0 THEN
	MID$(fechasale$, INSTR(fechasale$, "mm"), 2) = mes2$
END IF
TrataFechas$ = fechasale$
END FUNCTION

FUNCTION TrataHoras$ (dat$, horaentra$, horasale$)
horaentra$ = LCASE$(horaentra$)
horasale$ = LCASE$(horasale$)
tomahora% = false: tomaminuto% = false: tomasegundo% = false
IF INSTR(1, horaentra$, "h") > 0 THEN
	IF INSTR(1, horaentra$, "hh") > 0 AND NOT tomahora% THEN
		hora% = VAL(MID$(dat$, INSTR(1, horaentra$, "hh"), 2))
		tomahora% = true
	END IF
	IF INSTR(1, horaentra$, "h") > 0 AND NOT tomahora% THEN
		hora% = VAL(MID$(dat$, INSTR(1, horaentra$, "h"), 1))
		tomahora% = true
	END IF
ELSE
	hora% = 0
END IF
IF INSTR(1, horaentra$, "m") > 0 THEN
	IF INSTR(1, horaentra$, "mm") > 0 AND NOT tomaminuto% THEN
		minuto% = VAL(MID$(dat$, INSTR(1, horaentra$, "mm"), 2))
		tomaminuto% = true
	END IF
	IF INSTR(1, horaentra$, "m") > 0 AND NOT tomaminuto% THEN
		minuto% = VAL(MID$(dat$, INSTR(1, horaentra$, "m"), 1))
		tomaminuto% = true
	END IF
ELSE
	minuto% = 0
END IF
IF INSTR(1, horaentra$, "s") > 0 THEN
	IF INSTR(1, horaentra$, "ss") > 0 AND NOT tomasegundo% THEN
		segundo% = VAL(MID$(dat$, INSTR(1, horaentra$, "ss"), 2))
		tomasegundo% = true
	END IF
	IF INSTR(1, horaentra$, "h") > 0 AND NOT tomasegundo% THEN
		segundo% = VAL(MID$(dat$, INSTR(1, horaentra$, "s"), 1))
		tomasegundo% = true
	END IF
ELSE
	segundo% = 0
END IF
pm = false
IF INSTR(horasale$, "a") > 0 THEN
	IF hora% > 12 THEN hora% = hora% - 12: pm = true
	IF hora% < 10 THEN
		hora2$ = "0" + MID$(STR$(hora%), 2, 1)
	ELSE
		hora2$ = MID$(STR$(hora%), 2, 2)
	END IF
ELSE
	IF hora% < 10 THEN
		hora2$ = "0" + MID$(STR$(hora%), 2, 1)
	ELSE
		hora2$ = MID$(STR$(hora%), 2, 2)
	END IF
END IF
IF minuto% < 10 THEN
	minuto2$ = "0" + MID$(STR$(minuto%), 2, 1)
ELSE
	minuto2$ = MID$(STR$(minuto%), 2, 2)
END IF
IF segundo% < 10 THEN
	segundo2$ = "0" + MID$(STR$(segundo%), 2, 1)
ELSE
	segundo2$ = MID$(STR$(segundo%), 2, 2)
END IF
IF INSTR(horasale$, "h") > 0 THEN
	MID$(horasale$, INSTR(horasale$, "hh"), 2) = hora2$
END IF
IF INSTR(horasale$, "m") > 0 THEN
	MID$(horasale$, INSTR(horasale$, "mm"), 2) = minuto2$
END IF
IF INSTR(horasale$, "s") > 0 THEN
	MID$(horasale$, INSTR(horasale$, "ss"), 2) = segundo2$
END IF
IF INSTR(horasale$, "a") > 0 THEN
	MID$(horasale$, INSTR(horasale$, "a"), 1) = " "
	IF pm = true THEN
		horasale$ = horasale$ + "p.m."
	ELSE
		horasale$ = horasale$ + "a.m."
	END IF
END IF
TrataHoras$ = horasale$
END FUNCTION

FUNCTION Trim$ (X$)
IF X$ = "" THEN
	Trim$ = ""
ELSE
lastChar = 0
FOR A = 1 TO LEN(X$): Y$ = MID$(X$, A, 1): IF Y$ <> CHR$(0) AND Y$ <> " " THEN lastChar = A
NEXT A: Trim$ = LEFT$(X$, lastChar)
END IF
END FUNCTION

