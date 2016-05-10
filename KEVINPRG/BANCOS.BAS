'operaciones con cuentas bancarias
DEFINT A-Z
DECLARE SUB Bancos ()
DECLARE SUB Espera ()
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
DECLARE FUNCTION Elije$ (fila%, columna%, fila2%, columna2%)
DECLARE FUNCTION NorteaSur$ (cad$)
DECLARE FUNCTION TrataFechas$ (dat$, fechaentra$, fechasale$)
DECLARE SUB Nueva ()
DECLARE SUB Preferencias ()
DECLARE SUB DepRet ()
DECLARE SUB Totalizar ()
DECLARE SUB Registro ()
DECLARE SUB Conciliar ()
DECLARE SUB Ordenar (opc%)
DECLARE SUB Imprimir ()
DECLARE SUB ModificaCuenta ()
DECLARE SUB Modificar ()
DECLARE SUB Borrar ()
DECLARE SUB BuscarPorNumero ()
' procedimientos externos
	DECLARE SUB PopUp (y%, X%, h%, W%, C%, B%, s%, Z%)
	DECLARE SUB ShutUp ()
	DECLARE FUNCTION Verify (Row%, Prompt$)
	DECLARE SUB BackFill (Row%, col%, Rows%, Cols%, Attribute%)
COMMON SHARED GUIA, colorpref
'colorpref = 3
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR ct = 1 TO 4: FOR X = 1 TO 10: READ colors(X, ct): NEXT X: NEXT ct
DIM SHARED Fun(12) AS STRING * 2, devolver AS SINGLE
CONST true = -1: CONST false = NOT true: CLOSE
'declaracion personalizada de variables
TYPE cuentas 'maneja la cantidad de cuentas
	acti   AS STRING * 1
	codi   AS STRING * 3
	fecha  AS STRING * 10 'ddmmaaaa de creaci¢n
	mesano AS STRING * 6  'mmaaaa de £ltima conciliaci¢n
	numcta AS STRING * 25
	saldo  AS DOUBLE
END TYPE
TYPE cuenta     'maneja la cuenta en s¡
	oper  AS STRING * 1   'tipo de operaci¢n
	' 0=apertura         1=dep¢sito               2=retiro
	' 3=D‚bito bancario  4=Nota cr‚dito           5=Nota d‚bito
	' 6=Emisi¢n edo cta  7=Manten. cta.           8=Emisi¢n Chequera
	' 9=Emisi¢n libreta  A=Emisi¢n tarjeta d‚bito
	' B=Pago tja. d‚bito C=Pago cheque            D=Intereses
	' E=Retiro Caj. aut. F=Cobro tja. d‚bito
	auxi  AS STRING * 1   'Conciliaci¢n:" " no, "*" si conciliado
	fech  AS STRING * 10  'fecha de operaci¢n dd/mm/aaaa
	fech2 AS STRING * 10  'fecha de anotaci¢n dd/mm/aaaa
	hora2 AS STRING * 5   'hora de anotaci¢n
	nota  AS STRING * 30  'descripci¢n
	num   AS DOUBLE       'n£mero de operaci¢n (planilla, cheque, etc.)
	debe  AS DOUBLE
	haber AS DOUBLE
	saldo AS DOUBLE
	dias  AS LONG
END TYPE
TYPE idx
	num AS STRING * 3
	nom AS STRING * 15
END TYPE
DIM SHARED ctaaux AS cuenta, cta AS cuenta, master AS cuentas, bank AS idx, ordbank AS STRING * 15
DIM SHARED Contenido(15) AS STRING * 15, Code(15) AS STRING * 3, resal%
DIM SHARED ao1 AS STRING * 15, ao2 AS STRING * 15, ao3 AS STRING * 3, ao4 AS STRING * 3, deskrip AS STRING * 15
'CLS : PRINT LEN(cta), LEN(master): END
' cta=93 , master=53
'apertura de archivos
OPEN "f:\kevinprg\bancos\bancos.dat" FOR RANDOM SHARED AS #1 LEN = 53
OPEN "f:\kevinprg\bancos\bank.dat" FOR RANDOM SHARED AS #5 LEN = 18
OPEN "f:\kevinprg\bancos\ordbank.dat" FOR RANDOM SHARED AS #6 LEN = 15
ON ERROR GOTO ErrorTrap
DO
	COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box2 1, 5, 24, 75
	Center 2, "Operaciones disponibles con cuentas bancarias     "
	Center 4, "1.- CREAR una nueva cuenta bancaria               "
	Center 5, "2.- Anotar OPERACIONES en una cuenta bancaria     "
	Center 6, "3.- Anotar OPERACIONES en una cuenta bancaria     "
	Center 7, "4.- CONCILIAR una cuenta bancaria (cta. corriente)"
	Center 8, "5.- BORRAR una cuenta bancaria (saldo= Bs. 0)     "
	Center 9, "6.- TOTALIZAR saldos (todas las cuentas)          "
	Center 10, "7.- VER registro de movimientos por fecha         "
	Center 11, "8.- ORDENAR e indexar datos DE UNA CUENTA         "
	Center 12, "9.- IMPRIMIR estados de cuenta por fecha          "
	Center 13, "A.- MODIFICAR datos de una cuenta bancaria        "
	Center 14, "B.- MODIFICAR UNA OPERACION de retiro o deposito  "
	Center 15, "C.- Buscar por n£mero de operaci¢n                "
	Center 25, "Pulse del 0 al 9 y A a B para ejecutar, Escape para salir "
	Kbd$ = INKEY$: DO WHILE Kbd$ = "": Kbd$ = INKEY$: LOOP: Kbd$ = UCASE$(Kbd$)
	SELECT CASE Kbd$
		CASE CHR$(27): EXIT DO
		CASE "1": CALL Nueva
		CASE "2": CALL DepRet
		CASE "3": CALL DepRet
		CASE "4"
		CASE "5": CALL Borrar
		CASE "6": CALL Totalizar
		CASE "7": CALL Registro
		CASE "8": CALL Ordenar(0)
		CASE "9": CALL Imprimir
		CASE "A": CALL ModificaCuenta
		CASE "B": CALL Modificar
		CASE "C": CALL BuscarPorNumero
	END SELECT
LOOP
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... "
CHAIN "F:\kevinprg\kevin01.bas" 'vuelve a medstorm
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

SUB Bancos
END SUB

SUB Borrar
COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box2 1, 5, 24, 75
Center 2, "BORRAR una cuenta bancaria"
kta% = Escoge%(1): IF kta% < 0 THEN EXIT SUB
GET #1, kta%, master
Center 5, master.numcta
IF master.saldo > 0 AND master.saldo < 1 THEN
	r% = Verify(13, "¨Est  seguro que desea eliminar esta cuenta")
	IF r% = true THEN
		master.acti = "*": PUT #1, kta%, master
		CALL Ordenar(0)
		EXIT SUB
	ELSE
		EXIT SUB
	END IF
ELSE
	Center 13, "Esta cuenta no se puede borrar, pues su saldo no es cero": Espera: EXIT SUB
END IF
END SUB

SUB Box (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿"; : FOR A = Row1 + 1 TO Row2 - 1: LOCATE A, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT A: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Box2 (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "²"; STRING$(BoxWidth - 2, "²"); "²"; : FOR A = Row1 + 1 TO Row2 - 1: LOCATE A, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT A: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB BuscarPorNumero
DIM nume AS DOUBLE
CLS : Box2 1, 5, 24, 75
Center 2, "Ver anotaciones seg£n n£mero de instrumento"
kta% = Escoge%(1): IF kta% < 0 THEN EXIT SUB
PopUp 3, 5, 20, 74, 31, 1, 1, 0
Center 5, "Registro de movimientos hechos seg£n b£squeda"
Fun(9) = "no"
DO
	Center 25, "Introduzca el instrumento, [F9] para abortar"
	numInst$ = MeteDatoLetra(7, 40, 10, "      0", "0")
	IF Fun(9) = "si" THEN ShutUp: EXIT SUB
	IF VAL(numInst$) > 0 THEN EXIT DO
LOOP
ShutUp: nume = VAL(numInst$)
PopUp 3, 3, 20, 74, 31, 1, 1, 0
auxi$ = "f:\kevinprg\bancos\" + MID$(STR$(kta%), 2, LEN(STR$(kta%)) - 1) + ".cta"
CLOSE #2: OPEN auxi$ FOR RANDOM AS #2 LEN = 93: max% = LOF(2) / 93
l% = 1: fechaux$ = fecha1$
Center 5, "Registro de anotaciones hechos seg£n fecha"
Center 25, "Registro de anotaciones hechos seg£n fecha"
LOCATE 7, 4
PRINT fecha1$; "  Operaci¢n               DEBE          HABER          SALDO"
FOR k% = 1 TO max%
GET #2, k%, cta
IF cta.num = nume THEN
	IF fechaux$ <> cta.fech THEN
		LOCATE 7 + l%, 4
		PRINT cta.fech; "  Operaci¢n               DEBE          HABER          SALDO"
		l% = l% + 1:  LOCATE 7 + l%, 14: fechaux$ = cta.fech
	END IF
	LOCATE 7 + l%, 4
	IF cta.num <= 99999999 THEN
		PRINT USING "#########, "; cta.num;
	ELSEIF cta.num <= 9999999999# AND cta.num > 99999999 THEN
		PRINT USING "########## "; cta.num;
	ELSE
		PRINT "desborde   ";
	END IF
	' 0=apertura         1=dep¢sito               2=retiro
	' 3=D‚bito bancario  4=Nota cr‚dito           5=Nota d‚bito
	' 6=Emisi¢n edo cta  7=Manten. cta.           8=Emisi¢n Chequera
	' 9=Emisi¢n libreta  A=Emisi¢n tarjeta d‚bito
	' B=Pago tja. d‚bito C=Pago cheque            D=Intereses
	' E=Retiro Caj. aut. F=Cobro tja. d‚bito
	SELECT CASE cta.oper ' cuenta
		CASE "0": PRINT "Apertura ......";
		CASE "1": PRINT "Dep¢sito ......";
		CASE "2": PRINT "Retiro ........";
		CASE "3": PRINT "D‚bito bancario";
		CASE "4": PRINT "Nota de cr‚dito";
		CASE "5": PRINT "Nota de d‚bito.";
		CASE "6": PRINT "Emis. edo. cta.";
		CASE "7": PRINT "Manten. de cta.";
		CASE "8": PRINT "Emis. chequera.";
		CASE "9": PRINT "Emis. libreta..";
		CASE "A": PRINT "Emis. tja. d‚b.";
		CASE "B": PRINT "Pago. tja. d‚b.";
		CASE "C": PRINT "Pago con cheque";
		CASE "D": PRINT "Intereses......";
		CASE "E": PRINT "Retiro caj. aut";
		CASE "F": PRINT "Cobro tja.d‚bit";
	END SELECT
	GOSUB DebeHaberSaldo
	l% = l% + 1
	IF l% > 10 THEN
		l% = 0: Espera
		ShutUp
		PopUp 3, 3, 20, 74, 31, 1, 1, 0
		Center 5, "Registro de anotaciones hechos seg£n fecha"
	END IF
END IF
NEXT k%
GET #2, max%, cta
LOCATE 20, 4: PRINT "Saldo disponible ACTUAL de cuenta:";
PRINT USING "###########,.##"; cta.saldo;
CLOSE #2
Center 21, "Visualizacion de registros finalizada"
Espera
ShutUp
EXIT SUB

DebeHaberSaldo:
	PRINT USING "###########,.##"; cta.debe;
	PRINT USING "###########,.##"; cta.haber;
	IF cta.saldo < 0 THEN COLOR colors(7, 4), colors(4, 4)
	PRINT USING "###########,.##"; cta.saldo;
	IF cta.saldo < 0 THEN COLOR colors(7, colorpref), colors(4, colorpref)
RETURN
END SUB

SUB Center (Row, text$)
IF Row = 25 THEN COLOR 0, 15: LOCATE 25, 1: PRINT SPACE$(80);
LOCATE Row, 41 - LEN(text$) / 2: PRINT text$;
IF Row = 25 THEN COLOR colors(7, colorpref), colors(4, colorpref)
END SUB

SUB Conciliar
END SUB

DEFSNG A-Z
FUNCTION DateToJulian& (dat$, caso%)
' reparado el error para fechas mayores que 29 de Febrero de 2000
SELECT CASE caso%
	 CASE 1
			' formato ddmmaaaa
			day% = VAL(MID$(dat$, 1, 2))
			month% = VAL(MID$(dat$, 3, 2))
			year% = VAL(MID$(dat$, 5, 4))
			mes% = VAL(MID$(dat$, 3, 2))
			ano% = VAL(MID$(dat$, 5, 4))
	 CASE 2
			' formato dd/mm/aaaa
			day% = VAL(MID$(dat$, 1, 2))
			month% = VAL(MID$(dat$, 4, 2))
			year% = VAL(MID$(dat$, 7, 4))
			mes% = VAL(MID$(dat$, 4, 2))
			ano% = VAL(MID$(dat$, 7, 4))
	 CASE 3
			' formato mm/dd/aaaa
			day% = VAL(MID$(dat$, 4, 2))
			month% = VAL(MID$(dat$, 1, 2))
			year% = VAL(MID$(dat$, 7, 4))
			mes% = VAL(MID$(dat$, 1, 2))
			ano% = VAL(MID$(dat$, 7, 4))
	 CASE 4
			' formato mmddaaaa
			day% = VAL(MID$(dat$, 3, 2))
			month% = VAL(MID$(dat$, 1, 2))
			year% = VAL(MID$(dat$, 5, 4))
			mes% = VAL(MID$(dat$, 1, 2))
			ano% = VAL(MID$(dat$, 5, 4))
END SELECT
			SELECT CASE month%
					CASE IS > 2
							 month% = month% - 3
					CASE IS <= 2
							 month% = month% + 9
							 year% = year% - 1
			END SELECT
			A& = 146097# * FIX(FIX(year% / 100) / 4)
			B& = FIX(1461# * (year% MOD 100) / 4)
			C& = FIX((153# * month% + 2) / 5) + day% + 1721119
			D& = A& + B& + C&
			SELECT CASE ano%
				 CASE 2000
						IF mes% > 2 THEN
							 D& = D& - 109573
						END IF
				 CASE IS > 2000
							 D& = D& - 109573
				 CASE ELSE
			END SELECT
			DateToJulian = D&
END FUNCTION

DEFINT A-Z
FUNCTION DateValid% (cad$)
day% = VAL(MID$(cad$, 1, 2)): month% = VAL(MID$(cad$, 4, 2)): year% = VAL(MID$(cad$, 7, 4))
IF year% > 2100 OR year% < 2000 THEN DateValid% = 0: EXIT FUNCTION
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
DateValid% = true                       'Date passes all tests
END FUNCTION

SUB DepRet
	' i% dice que operaci¢n se realiza
	' 0=apertura         1=dep¢sito               2=retiro
	' 3=D‚bito bancario  4=Nota cr‚dito           5=Nota d‚bito
	' 6=Emisi¢n edo cta  7=Manten. cta.           8=Emisi¢n Chequera
	' 9=Emisi¢n libreta  A=Emisi¢n tarjeta d‚bito
	' B=Pago tja. d‚bito C=Pago cheque            D=Intereses
	' E=Retiro Caj. aut. F=Cobro tja. d‚bito
DIM Criterio(15) AS STRING * 60
salir = false: I$ = ""
Criterio(1) = "DEPOSITO: por ventanilla o taquilla externa.               "
Criterio(2) = "RETIRO: por ventanilla o taquilla externa.                 "
Criterio(3) = "DEBITO BANCARIO: impuesto del gobierno nacional.           "
Criterio(4) = "NOTA DE CREDITO: reintegro d‚b. bancario, ajustes, etc.    "
Criterio(5) = "NOTA DE DEBITO: consulta de saldo, corte de cta., etc.     "
Criterio(6) = "EMISION DE ESTADO DE CUENTA CORRIENTE.                     "
Criterio(7) = "MANTENIMIENTO DE CUENTA:si el saldo es bajo(seg£n el banco)"
Criterio(8) = "EMISION DE CHEQUERA(S): recuerde anotar los N§ de cheques  "
Criterio(9) = "EMISION DE LIBRETA DE AHORROS.                             "
Criterio(10) = "EMISION DE TARJETA DE DEBITO.                              "
Criterio(11) = "PAGO CON TARJETA DE DEBITO:en un punto de venta (P.O.S.)   "
Criterio(12) = "PAGO CON CHEQUE: recuerde anotar el N§ de cheque.          "
Criterio(13) = "INTERESES: si es que el banco los paga, cualquier monto.   "
Criterio(14) = "RETIRO POR CAJERO AUTOMATICO:guarde muy bien su tarjeta.   "
Criterio(15) = "COBRO CON TARJETA DE DEBITO:si se tiene punto de venta.    "
PopUp 2, 9, 19, 62, 112, 1, 0, 0
Center 3, "*** OPERACIONES BANCARIAS DISPONIBLES ***"
Center 4, "Escoja que opci¢n desea realizar."
Center 5, STRING$(40, "-")
CALL Center(25, "[F1]Ayuda -Use las teclas de direcci¢n y Enter para escojer [F9]Abortar")
resal% = 1: salir = false
DO WHILE salir = false
		FOR k = 1 TO 15
			 IF resal% = k THEN COLOR colors(7, colorpref), colors(4, colorpref)
			 LOCATE 4 + k, 10: PRINT Criterio(k)
			 IF resal% = k THEN COLOR colors(4, colorpref), colors(7, colorpref)
		NEXT k
		key$ = "": DO WHILE key$ = "": key$ = INKEY$: LOOP
		IF key$ = CHR$(0) + "H" THEN resal% = resal% - 1: IF resal% < 1 THEN resal% = 15
		IF key$ = CHR$(0) + "P" THEN resal% = resal% + 1: IF resal% > 15 THEN resal% = 1
		IF key$ = CHR$(0) + CHR$(67) THEN ShutUp: EXIT SUB
		IF key$ = CHR$(27) THEN ShutUp: EXIT SUB
		IF key$ = CHR$(13) THEN ShutUp: salir = true
LOOP
SELECT CASE resal%
	CASE 1: I$ = "1": J% = 1 'suma  a la cuenta (HABER)
	CASE 2: I$ = "2": J% = 2 'resta a la cuenta (DEBE )
	CASE 3: I$ = "3": J% = 2
	CASE 4: I$ = "4": J% = 1
	CASE 5: I$ = "5": J% = 2
	CASE 6: I$ = "6": J% = 2
	CASE 7: I$ = "7": J% = 2
	CASE 8: I$ = "8": J% = 2
	CASE 9: I$ = "9": J% = 2
	CASE 10: I$ = "A": J% = 2
	CASE 11: I$ = "B": J% = 2
	CASE 12: I$ = "C": J% = 2
	CASE 13: I$ = "D": J% = 1
	CASE 14: I$ = "E": J% = 2
	CASE 15: I$ = "F": J% = 1
END SELECT

COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box2 1, 5, 24, 75
Center 2, Criterio(resal%)
kta% = Escoge%(1): IF kta% < 0 THEN EXIT SUB
GET #1, kta%, master: GET #6, VAL(master.codi), ordbank
auxi$ = "f:\kevinprg\bancos\" + MID$(STR$(kta%), 2, LEN(STR$(kta%)) - 1) + ".cta"
CLOSE #2: OPEN auxi$ FOR RANDOM AS #2 LEN = 93: max% = LOF(2) / 93
GET #2, 1, ctaaux
Center 3, "Cuenta Bancaria elegida: " + ordbank + master.numcta
Center 5, "Escriba los datos de la operaci¢n"
LOCATE 7, 10: PRINT "Fecha de la operaci¢n        :²  /  /    ²"
LOCATE 9, 10: PRINT "Observaci¢n o anotaci¢n      :²"; SPACE$(30); "²"
LOCATE 11, 10: PRINT "N£mero de planilla,cheque,etc:²              0²"
LOCATE 13, 10: PRINT "Monto en bol¡vares           :²           0.00²"
Center 25, " [F9]SALVAR y salir [F10]Abortar anotaci¢n"
cta.oper = I$
cta.auxi = " ": cta.fech = "  /  /    ": auxi1$ = "0": auxi2$ = "0"
cta.nota = SPACE$(30): cta.num = 0
cta.debe = 0: cta.haber = 0: cta.saldo = 0
puntero% = 1: Fun(9) = "no": Fun(10) = "no": devolver = false
DO
	SELECT CASE puntero%
		CASE 1
			DO
				Fun(10) = "no"
				cta.fech = MeteDatoLetra(7, 40, 10, cta.fech, "0")
				IF Fun(10) = "si" THEN EXIT DO
				IF DateValid%(cta.fech) = true THEN
					IF DateToJulian(cta.fech, 2) >= ctaaux.dias THEN
						cta.fech = TrataFechas$(cta.fech, "dd/mm/aaaa", "dd/mm/aaaa")
						LOCATE 7, 10: PRINT "Fecha de la operaci¢n        :²"; cta.fech; "²"
						EXIT DO
					ELSE
						Center 25, "La fecha debe ser mayor o igual a la de apertura:" + ctaaux.fech
					END IF
				END IF
			LOOP
		CASE 2
			Center 25, " [F9]SALVAR y salir [F10]Abortar anotaci¢n"
			cta.nota = MeteDatoLetra(9, 40, 30, cta.nota, "0")
		CASE 3
			auxi1$ = EntraDatoNumero(11, 40, 15, auxi1$, 1)
			LOCATE 11, 41: PRINT USING "##############,"; VAL(auxi1$)
		CASE 4
			DO
				Fun(10) = "no"
				auxi2$ = EntraDatoNumero(13, 40, 15, auxi2$, 1)
				LOCATE 13, 41: PRINT USING "###########,.##"; VAL(auxi2$)
				IF Fun(10) = "si" THEN EXIT DO
				IF ABS(VAL(auxi2$)) > 0 THEN
					Center 25, " [F9]SALVAR y salir [F10]Abortar anotaci¢n"
					EXIT DO
				ELSE
					Center 25, "Introduzca un monto mayor que cero"
				END IF
			LOOP
	END SELECT
	IF devolver = true THEN
		devolver = false: puntero% = puntero% - 1: IF puntero% < 1 THEN puntero% = 4
	ELSE
		puntero% = puntero% + 1: IF puntero% > 4 THEN puntero% = 1
	END IF
	IF Fun(9) = "si" THEN 'salvar en archivo
		cta.fech2 = NorteaSur(DATE$): cta.hora2 = MID$(TIME$, 1, 5)
		cta.dias = DateToJulian(cta.fech, 2)
		GET #2, max%, ctaaux: max% = max% + 1: cta.num = VAL(auxi1$)
		IF J% = 1 THEN
			cta.haber = ABS(VAL(auxi2$))
		ELSE
			cta.debe = ABS(VAL(auxi2$))
		END IF
		cta.saldo = ctaaux.saldo + cta.haber - cta.debe
		master.saldo = cta.saldo
		PUT #1, kta%, master: PUT #2, max%, cta: CLOSE #2
		IF cta.dias <> ctaaux.dias THEN Ordenar (kta%)
		EXIT DO
	END IF
	IF Fun(10) = "si" THEN 'aborta la anotaci¢n
		k% = Verify(13, "¨Seguro desea abortar la anotaci¢n")
		IF k% = true THEN CLOSE #2: EXIT DO
	END IF
LOOP
END SUB

FUNCTION Elije$ (fila%, columna%, fila2%, columna2%)
PopUp fila%, columna%, fila% + 13, columna% + 11, 31, 1, 1, 0
COLOR colors(7, colorpref), colors(4, colorpref)
CALL Center(25, "[F1] Ayuda [Enter]Tomar Opcion [A-Z]Ir a... [Esc]Menu Anterior"): COLOR colors(4, colorpref), colors(7, colorpref)
Center 25, "[F5]Edita/Adiciona Linea [F8]Borrar Linea [Enter]Tomar Opcion [Esc]Abortar"
LOCATE fila%, columna% + 2: PRINT "Bancos   ": Code(0) = "fin"
FOR k = 1 TO 15
	GET #5, k, bank
	Contenido(k) = bank.nom
	Code(k) = bank.num
NEXT k: selecc% = 199
COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE fila% + 1, columna% + 1
PRINT Contenido(1)
LOCATE fila2%, columna2%: PRINT Code(1); "²"; Contenido(1)
COLOR colors(4, colorpref), colors(7, colorpref)
FOR k = 2 TO 15: LOCATE fila% + k, columna% + 1: PRINT Contenido(k): NEXT k
resal% = 1: up% = 1: terminar = false
DO
		 Kbd$ = "": WHILE Kbd$ = "": Kbd$ = INKEY$: WEND: Kbd$ = UCASE$(Kbd$)
		 SELECT CASE Kbd$
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
						CASE CHR$(13): IF MID$(Contenido(resal%), 1, 1) <> "þ" THEN terminar = true ELSE BEEP
						CASE CHR$(27): terminar = true: resal% = 0: up% = 0: devolver = true
						CASE IS >= CHR$(65)
							 IF Kbd$ <= CHR$(90) THEN
										FOR k = 1 TO selecc%
												 GET #5, k, bank
												 IF Kbd$ = MID$(bank.nom, 1, 1) THEN up% = k: resal% = 1: EXIT FOR
												 IF k = selecc% THEN BEEP
										NEXT k
										IF up% + 14 > selecc% THEN up% = selecc% - 14: resal% = 1
										GOSUB Lee15
							 END IF
						CASE CHR$(0) + CHR$(63): deskrip = "Edita": GOSUB BorraEdita
						CASE CHR$(0) + CHR$(66): deskrip = "Borra": GOSUB BorraEdita
						CASE CHR$(0) + CHR$(67): terminar = true: Fun(9) = "si": resal% = 0: resal% = 0
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
					IF resal% = l THEN COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila2%, columna2%: PRINT Code(k); "²"; Contenido(k)
					LOCATE fila% + l, columna% + 1: PRINT Contenido(k): IF resal% = l THEN COLOR colors(4, colorpref), colors(7, colorpref)
		 NEXT k
		 IF terminar = true THEN
					EXIT DO
		 END IF
LOOP
Elije$ = Code(resal%): ShutUp
EXIT FUNCTION

Lee15:
FOR k = 0 TO 14
		 GET #5, up% + k, bank: Contenido(k + 1) = bank.nom: Code(k + 1) = bank.num
NEXT k
RETURN

BorraEdita:
IF MID$(deskrip, 1, 5) = "Edita" THEN
	Contenido(resal%) = MeteDatoLetra$(fila% + resal%, columna%, 15, Contenido(resal%), "0")
ELSE
	Contenido(resal%) = "þ" + SPACE$(14)
END IF
COLOR colors(4, colorpref), colors(7, colorpref)
ShutUp
PopUp fila%, columna%, fila% + 13, columna% + 11, 31, 1, 1, 0
LOCATE fila% + resal%, columna% + 1: PRINT "Espere..."
	FOR k = 1 TO selecc%
			 GET #5, k, bank
			 IF VAL(bank.num) = VAL(Code(resal%)) THEN
						'LOCK #5, k
						bank.num = Code(resal%)
						bank.nom = Contenido(resal%)
						PUT #5, k, bank
						'UNLOCK #5, k
						EXIT FOR
			 END IF
	NEXT k ': LOCK #5, 1 TO selecc%
	N = selecc%: Salto = N
	WHILE Salto > 1
			 Salto = Salto / 2
			 DO
						Sw = 1
						FOR J = 1 TO N - Salto
								 I = J + Salto
								 GET #5, J, bank: ao1 = bank.nom: ao3 = bank.num
								 GET #5, I, bank: ao2 = bank.nom: ao4 = bank.num
								 IF ao1 > ao2 THEN
											bank.nom = ao2: bank.num = ao4: PUT #5, J, bank
											bank.nom = ao1: bank.num = ao3: PUT #5, I, bank
											Sw = 0
								 END IF
						NEXT J
			 LOOP UNTIL Sw = 1
	WEND
	FOR k = 1 TO selecc%
			 GET #5, k, bank
			 deskrip = bank.nom
			 PUT #6, VAL(bank.num), deskrip
	NEXT k
GOSUB Lee15
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
'que%: 0=todos 1=vivos solamente
Center 25, " Escoja una cuenta con enter o Escape para abortar"
max% = LOF(1) / 51: l% = 1
IF max% = 0 THEN Escoge% = -1: PopUp 12, 27, 2, 30, 31, 1, 1, 0: Center 13, "No hay cuentas creadas": Espera: ShutUp: EXIT FUNCTION
DIM nombre(max%) AS STRING * 15, cuen(max%) AS STRING * 25, sitio(max%) AS SINGLE
FOR k% = 1 TO max%
	GET #1, k%, master
	IF que% = 0 THEN
		GET #6, VAL(master.codi), ordbank
		nombre(l%) = ordbank
		cuen(l%) = master.numcta
		sitio(l%) = k%: l% = l% + 1
	ELSEIF master.acti = " " THEN
		GET #6, VAL(master.codi), ordbank
		nombre(l%) = ordbank
		cuen(l%) = master.numcta
		sitio(l%) = k%: l% = l% + 1
	END IF
NEXT k%
resal% = 1: up% = 1: max% = l% - 1
PopUp 7, 11, 12, 45, 31, 1, 1, 0
GOSUB dibuja
DO
	Kbd$ = "": WHILE Kbd$ = "": Kbd$ = INKEY$: WEND
	SELECT CASE Kbd$
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
	GOSUB dibuja
LOOP
CALL ShutUp
Escoge% = sitio(up% + resal% - 1)
EXIT FUNCTION

dibuja:
FOR k% = 1 TO 10
	IF k% <= max% THEN
		LOCATE 7 + k%, 14
		IF (k% + up% - 1) <= max% THEN
			PRINT nombre(k% + up% - 1); " "; cuen(k% + up% - 1);
			IF resal% = k THEN
				LOCATE 7 + resal%, 12: PRINT "û"
			ELSE
				LOCATE 7 + k, 12: PRINT " "
			END IF
		ELSE
			LOCATE 7 + k%, 14: PRINT SPACE$(41);
		END IF
	ELSE
		LOCATE 7 + k%, 14: PRINT SPACE$(41);
	END IF
NEXT k%
BackFill 8, 12, 10, 43, 289
BackFill 7 + resal%, 12, 1, 43, 31
RETURN
END FUNCTION

SUB Espera
Center 25, "Presione Cualquier Tecla Para Continuar...": DO: LOOP WHILE INKEY$ = ""
END SUB

SUB Imprimir
CALL Ordenar(0)
CLS : Box2 1, 5, 24, 75
Center 2, "IMPRIMIR ESTADOS DE CUENTA SEGéN FECHA"
kta% = Escoge%(1): IF kta% < 0 THEN EXIT SUB
PopUp 3, 5, 20, 74, 31, 1, 1, 0
Center 5, "Imprimir movimientos seg£n fecha"
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
			IF DateToJulian(fecha1$, 2) <= DateToJulian(fecha2$, 2) THEN
				fecha1$ = TrataFechas$(fecha1$, "dd/mm/aaaa", "dd/mm/aaaa")
				fecha2$ = TrataFechas$(fecha2$, "dd/mm/aaaa", "dd/mm/aaaa")
				EXIT DO
			ELSE
				PopUp 17, 15, 2, 50, 31, 1, 1, 0: Center 18, "La fecha de inicio es MAYOR que la fecha final": Espera: ShutUp
			END IF
		ELSE
			PopUp 17, 20, 2, 40, 31, 1, 1, 0: Center 18, "La fecha de finalizaci¢n NO ES VALIDA": Espera: ShutUp
		END IF
	ELSE
		PopUp 17, 20, 2, 40, 31, 1, 1, 0: Center 18, "La fecha de inicio NO ES VALIDA": Espera: ShutUp
	END IF
LOOP
ShutUp
Box2 12, 5, 14, 75: Center 13, "Espere mientras se imprime..."

auxi$ = "f:\kevinprg\bancos\" + MID$(STR$(kta%), 2, LEN(STR$(kta%)) - 1) + ".cta"
OPEN auxi$ FOR RANDOM AS #2 LEN = 93: max% = LOF(2) / 93
l% = 1: fechaux$ = fecha1$
OPEN "lpt1:" FOR OUTPUT AS #10
WIDTH #10, 255
GET #1, kta%, master
PRINT #10, CHR$(15); : aux$ = DATE$
aux$ = TrataFechas(aux$, "mm-dd-aaaa", "dd/mm/aaaa")
PRINT #10, "Fecha: "; aux$; " Hora: "; TIME$; CHR$(10);
PRINT #10, "Cuenta solicitada:"; master.numcta; CHR$(10);
PRINT #10, "Estado de cuenta desde "; fecha1$; " hasta "; fecha2$; CHR$(10);
PRINT #10, STRING$(101, "Ä"); CHR$(10);
PRINT #10, fecha1$; " Operaci¢n      Descripci¢n                              DEBE          HABER          SALDO"; CHR$(10);
FOR k% = 1 TO max%
	GET #2, k%, cta
	IF cta.dias >= DateToJulian(fecha1$, 2) AND cta.dias <= DateToJulian(fecha2$, 2) THEN
		IF fechaux$ <> cta.fech THEN
			PRINT #10, CHR$(10);
			PRINT #10, cta.fech; " Operaci¢n      Descripci¢n                              DEBE          HABER          SALDO"; CHR$(10);
			l% = l% + 1: fechaux$ = cta.fech
		END IF
	 
		IF cta.num <= 99999999 THEN
			PRINT #10, USING "#########, "; cta.num;
		ELSEIF cta.num <= 9999999999# AND cta.num > 99999999 THEN
			PRINT #10, USING "########## "; cta.num;
		ELSE
			PRINT #10, "desborde   ";
		END IF
	 
		' 0=apertura        1=dep¢sito     2=retiro
		' 3=D‚bito bancario 4=Nota cr‚dito 5=Nota d‚bito
		' 6=Emisi¢n edo cta 7=Manten. cta. 8=Chequera/libreta
		' 9=Tarjeta d‚bito
		SELECT CASE cta.oper ' cuenta
			CASE "0": PRINT #10, "Apertura ......";
			CASE "1": PRINT #10, "Dep¢sito ......";
			CASE "2": PRINT #10, "Retiro ........";
		END SELECT
		PRINT #10, Trim$(cta.nota);
		PRINT #10, STRING$((30 - LEN(Trim$(cta.nota))), ".");
		GOSUB DebeHaberSaldo2
		l% = l% + 1
		IF l% > 60 THEN
			l% = 0:
			PRINT #10, STRING$(101, "Ä"); CHR$(10);
			PRINT #10, CHR$(12);
			PRINT #10, "Fecha: "; aux$; " Hora: "; TIME$; CHR$(10);
			PRINT #10, "Cuenta solicitada:"; master.numcta; CHR$(10);
			PRINT #10, "Estado de cuenta desde "; fecha1$; " hasta "; fecha2$; CHR$(10);
			PRINT #10, fecha1$; " Operaci¢n      Descripci¢n                              DEBE          HABER          SALDO"; CHR$(10);
			PRINT #10, STRING$(101, "Ä"); CHR$(10);
		END IF
	END IF
NEXT k%
PRINT #10, STRING$(101, "Ä"); CHR$(10);
PRINT #10, CHR$(12);
CLOSE #2: CLOSE #10
EXIT SUB

DebeHaberSaldo2:
	PRINT #10, USING "###########,.##"; cta.debe;
	PRINT #10, USING "###########,.##"; cta.haber;
	PRINT #10, USING "###########,.##"; cta.saldo;
	PRINT #10, CHR$(10);
RETURN
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

SUB ModificaCuenta
Box2 1, 5, 24, 75: Center 2, "Modificar UNA cuenta bancaria: escoja la cuenta"
kta% = Escoge%(1): IF kta% < 0 THEN EXIT SUB
GET #1, kta%, master
GET #6, VAL(master.codi), ordbank

COLOR colors(7, colorpref), colors(4, colorpref): Box2 1, 5, 24, 75
PopUp 4, 5, 15, 70, 31, 1, 1, 0
Center 5, "Modifique los datos de la cuenta( el saldo no )"
LOCATE 7, 10: PRINT "N£mero de cuenta             :²"; master.numcta; "²"
LOCATE 9, 10: PRINT "Saldo                        :²";
PRINT USING "###########,.##²"; master.saldo;
LOCATE 11, 10: PRINT "Fecha de este saldo          :²"; master.fecha; "²"
LOCATE 13, 10: PRINT "Esta cuenta est  en el banco :²"; ordbank; "²"
Center 25, " [F9]SALVAR y salir [F10]Abortar modificaci¢n de cuenta"
puntero% = 1: Fun(9) = "no": Fun(10) = "no": devolver = false
DO
	SELECT CASE puntero%
		CASE 1
			DO
				master.numcta = MeteDatoLetra(7, 40, 25, master.numcta, "0")
				IF master.numcta <> SPACE$(25) THEN EXIT DO
			LOOP
		CASE 2
			DO
				master.fecha = MeteDatoLetra(11, 40, 10, master.fecha, "0")
				IF DateValid%(master.fecha) = true THEN EXIT DO
				IF Fun(10) = "si" THEN EXIT DO
			LOOP
		CASE 3
			k$ = Elije$(4, 7, 13, 41)
			COLOR colors(7, colorpref), colors(4, colorpref)
			IF k$ <> "fin" THEN
				master.codi = k$
				GET #6, VAL(master.codi), ordbank
				LOCATE 13, 10: PRINT "Esta cuenta est  en el banco :²"; ordbank; "²        "
			ELSE
				LOCATE 13, 10: PRINT "Esta cuenta est  en el banco :²"; ordbank; "²        "
			END IF
	END SELECT
	IF devolver = true THEN
		devolver = false: puntero% = puntero% - 1: IF puntero% < 1 THEN puntero% = 3
	ELSE
		puntero% = puntero% + 1: IF puntero% > 4 THEN puntero% = 1
	END IF
	IF Fun(9) = "si" THEN 'salvar en archivo
		PUT #1, kta%, master: EXIT DO
	END IF
	IF Fun(10) = "si" THEN 'aborta la creacion
		k% = Verify(13, "¨Seguro desea abortar la modificaci¢n")
		IF k% = true THEN EXIT DO
	END IF
LOOP
CALL ShutUp

END SUB

SUB Modificar
CLS : Box2 1, 5, 24, 75
Center 2, "MODIFICACION de un registro, elija la cuenta"
kta% = Escoge%(1): IF kta% < 0 THEN EXIT SUB
GET #1, kta%, master: GET #6, VAL(master.codi), ordbank
auxi$ = "f:\kevinprg\bancos\" + MID$(STR$(kta%), 2, LEN(STR$(kta%)) - 1) + ".cta"
CLOSE #2: OPEN auxi$ FOR RANDOM AS #2 LEN = 93: max% = LOF(2) / 93
GET #2, 1, ctaaux
Center 3, "Cuenta Bancaria elegida: " + ordbank + master.numcta
Center 25, "Introduzca la fecha de inicio, [F9] para abortar"
fecha1$ = NorteaSur$(DATE$)
PopUp 6, 17, 3, 40, 31, 1, 1, 0
DO
	LOCATE 7, 19: PRINT "¨ Que fecha desea  ?": Fun(9) = "no"
	Center 25, "Introduzca la fecha de b£squeda, [F9] para abortar"
	fecha1$ = MeteDatoLetra(7, 40, 10, fecha1$, "0")
	IF Fun(9) = "si" THEN ShutUp: EXIT SUB
	IF DateValid(fecha1$) = true THEN EXIT DO
LOOP
CALL ShutUp: puntero% = 0
IF max% > 1 THEN
	FOR k% = 1 TO max%
		GET #2, k%, cta
		IF cta.dias >= DateToJulian(fecha1$, 2) THEN puntero% = k%: EXIT FOR
	NEXT k%
	IF puntero% = 0 THEN puntero% = max%
	IF puntero% = 1 THEN puntero% = 2
	PopUp 12, 3, 4, 74, 31, 1, 1, 0
	Center 25, "Use las teclas de direcci¢n; [Enter]Escoger [Esc]Salir al men£ principal"
	DO
		GET #2, puntero%, cta
		LOCATE 13, 4: PRINT cta.fech; "  Operaci¢n               DEBE          HABER          SALDO"
		SELECT CASE puntero%
			CASE max%
				Center 20, "Final  de archivo    "
			CASE 2
				Center 20, "Inicio de archivo    "
			CASE ELSE
				Center 20, "Registro" + SPACE$(5 - LEN(STR$(puntero%))) + STR$(puntero%) + " de" + SPACE$(5 - LEN(STR$(max%))) + STR$(max%)
		END SELECT
		LOCATE 14, 4
		IF cta.num <= 99999999 THEN
			PRINT USING "#########, "; cta.num;
		ELSEIF cta.num <= 9999999999# AND cta.num > 99999999 THEN
			PRINT USING "########## "; cta.num;
		ELSE
			PRINT "desborde   ";
		END IF
		SELECT CASE cta.oper ' cuenta
			CASE "0": PRINT "Apertura ......";
			CASE "1": PRINT "Dep¢sito ......";
			CASE "2": PRINT "Retiro ........";
		END SELECT
		PRINT USING "###########,.##"; cta.debe;
		PRINT USING "###########,.##"; cta.haber;
		IF cta.saldo < 0 THEN COLOR colors(7, 4), colors(4, 4)
		PRINT USING "###########,.##"; cta.saldo;
		IF cta.saldo < 0 THEN COLOR colors(7, colorpref), colors(4, colorpref)
		Kbd$ = "": WHILE Kbd$ = "": Kbd$ = INKEY$: WEND: Kbd$ = UCASE$(Kbd$)
		SELECT CASE Kbd$
					 CASE CHR$(0) + "K" 'izquierda
					 CASE CHR$(0) + "M" 'derecha
					 CASE CHR$(0) + "H" 'arriba
						puntero% = puntero% - 1
						IF puntero% < 2 THEN puntero% = 2
					 CASE CHR$(0) + "P" 'abajo
						puntero% = puntero% + 1
						IF puntero% > max% THEN puntero% = max%
					 CASE CHR$(13)      'enter
						EXIT DO
					 CASE CHR$(27)      'escape
						CALL ShutUp: EXIT SUB
					 CASE ELSE:  BEEP
			 END SELECT
	LOOP
	ShutUp
ELSE
	puntero% = 1
END IF
GET #2, 1, ctaaux: days& = ctaaux.dias: fec$ = ctaaux.fech
GET #2, puntero%, ctaaux
GET #2, puntero%, cta
LOCATE 5, 10: PRINT "Tipo de operaci¢n: "
SELECT CASE cta.oper ' cuenta
		CASE "0": PRINT "apertura.";
		CASE "1": PRINT "dep¢sito.";
		CASE "2": PRINT "retiro.";
		CASE "3": PRINT "d‚bito bancario.";
		CASE "4": PRINT "nota de cr‚dito.";
		CASE "5": PRINT "nota de d‚bito.";
		CASE "6": PRINT "emisi¢n de estado de cuenta.";
		CASE "7": PRINT "mantenimiento de cuenta.";
		CASE "8": PRINT "emisi¢n de chequera.";
		CASE "9": PRINT "emisi¢n de libreta.";
		CASE "A": PRINT "emisi¢n de tarjeta de d‚bito.";
		CASE "B": PRINT "pago con tarjeta de d‚bito.";
		CASE "C": PRINT "pago con cheque.";
		CASE "D": PRINT "intereses.";
		CASE "E": PRINT "retiro por cajero autom tico.";
		CASE "F": PRINT "cobro con tarjeta de d‚bito.";
END SELECT
LOCATE 7, 10: PRINT "Fecha de la operaci¢n        :²"; cta.fech; "²"
LOCATE 9, 10: PRINT "Observaci¢n o anotaci¢n      :²"; cta.nota; "²"
LOCATE 11, 10: PRINT "N£mero de planilla,cheque,etc:²";
PRINT USING "##############,²"; cta.num
auxi1$ = STR$(cta.num)
LOCATE 13, 10: PRINT "Monto en bol¡vares           :²";
IF cta.debe > 0 THEN
	PRINT USING "###########,.##²"; cta.debe: auxi2$ = STR$(cta.debe)
ELSE
	PRINT USING "###########,.##²"; cta.haber: auxi2$ = STR$(cta.haber)
END IF
Center 25, " [F9]SALVAR y salir [F10]Abortar anotaci¢n"
punter% = 1: Fun(9) = "no": Fun(10) = "no": devolver = false
DO
	SELECT CASE punter%
		CASE 1
			DO
				Fun(10) = "no"
				cta.fech = MeteDatoLetra(7, 40, 10, cta.fech, "0")
				IF Fun(10) = "si" THEN EXIT DO
				IF DateValid%(cta.fech) = true THEN
					IF DateToJulian(cta.fech, 2) >= days& THEN
						cta.fech = TrataFechas$(cta.fech, "dd/mm/aaaa", "dd/mm/aaaa")
						EXIT DO
					ELSE
						Center 25, "La fecha debe ser mayor o igual a la de apertura:" + fec$
					END IF
				END IF
			LOOP
		CASE 2
			Center 25, " [F9]SALVAR y salir [F10]Abortar anotaci¢n"
			cta.nota = MeteDatoLetra(9, 40, 30, cta.nota, "0")
		CASE 3
			auxi1$ = EntraDatoNumero(11, 40, 15, auxi1$, 1)
			LOCATE 11, 41: PRINT USING "##############,"; VAL(auxi1$)
		CASE 4
			DO
				Fun(10) = "no"
				auxi2$ = EntraDatoNumero(13, 40, 15, auxi2$, 1)
				LOCATE 13, 41: PRINT USING "###########,.##"; VAL(auxi2$)
				IF Fun(10) = "si" THEN EXIT DO
				IF ABS(VAL(auxi2$)) > 0 THEN
					Center 25, " [F9]SALVAR y salir [F10]Abortar anotaci¢n"
					EXIT DO
				ELSE
					Center 25, "Introduzca un monto mayor que cero"
				END IF
			LOOP
	END SELECT
	IF devolver = true THEN
		devolver = false: punter% = punter% - 1: IF punter% < 1 THEN punter% = 4
	ELSE
		punter% = punter% + 1: IF punter% > 4 THEN punter% = 1
	END IF
	IF Fun(9) = "si" THEN 'salvar en archivo
		IF puntero% > 1 THEN
			IF cta.debe > 0 THEN
				cta.debe = ABS(VAL(auxi2$))
			ELSE
				cta.haber = ABS(VAL(auxi2$))
			END IF
			cta.dias = DateToJulian(cta.fech, 2)
			PUT #2, puntero%, cta: CLOSE #2
			ordenador = false
			IF cta.dias <> ctaaux.dias THEN ordenador = true
			IF cta.debe <> ctaaux.debe THEN ordenador = true
			IF cta.haber <> ctaaux.haber THEN ordenador = true
			IF ordenador = true THEN Ordenar (kta%)
			EXIT DO
		ELSE
			PUT #2, puntero%, cta: CLOSE #2
		END IF
	END IF
	IF Fun(10) = "si" THEN 'aborta la anotaci¢n
		k% = Verify(13, "¨Seguro desea abortar la modificaci¢n")
		IF k% = true THEN CLOSE #2: EXIT DO
	END IF
LOOP
END SUB

FUNCTION NorteaSur$ (cad$)
NorteaSur$ = MID$(cad$, 4, 2) + "/" + MID$(cad$, 1, 2) + "/" + MID$(cad$, 7, 4)
END FUNCTION

SUB Nueva
CLS
Box2 1, 5, 24, 75: Center 2, "Crear una nueva cuenta bancaria: escoja el banco"
k$ = Elije$(4, 7, 3, 7): IF k$ = "fin" THEN EXIT SUB
COLOR colors(7, colorpref), colors(4, colorpref): Box2 1, 5, 24, 75
Center 3, "Creaci¢n de una nueva cuenta, banco elegido:" + Contenido(resal%)
PopUp 4, 5, 15, 70, 31, 1, 1, 0
Center 5, " Escriba los datos de la cuenta a crear"
LOCATE 7, 10: PRINT "N£mero de cuenta             :²"; SPACE$(15); "²"
LOCATE 9, 10: PRINT "Saldo                        :²"; SPACE$(15); "²"
LOCATE 11, 10: PRINT "Fecha de este saldo          :²  /  /    ²"
LOCATE 13, 10: PRINT "Observaci¢n o anotaci¢n      :²"; SPACE$(30); "²"
Center 25, " [F9]SALVAR y salir [F10]Abortar creaci¢n de cuenta"
master.acti = " ": master.codi = k$
master.numcta = SPACE$(25): cta.fech = "  /  /    "
master.saldo = 0: master.fecha = NorteaSur$(DATE$): cta.nota = SPACE$(30)
puntero% = 1: Fun(9) = "no": Fun(10) = "no": devolver = false
DO
	SELECT CASE puntero%
		CASE 1
			DO
				master.numcta = MeteDatoLetra(7, 40, 25, master.numcta, "0")
				IF master.numcta <> SPACE$(25) THEN EXIT DO
			LOOP
		CASE 2
			auxi$ = STR$(master.saldo)
			auxi$ = EntraDatoNumero(9, 40, 15, auxi$, 1)
			master.saldo = VAL(auxi$): LOCATE 9, 41: PRINT USING "###########,.##"; master.saldo
		CASE 3
			DO
				cta.fech = MeteDatoLetra(11, 40, 10, cta.fech, "0")
				IF DateValid%(cta.fech) = true THEN EXIT DO
				IF Fun(10) = "si" THEN EXIT DO
			LOOP
		CASE 4
			cta.nota = MeteDatoLetra(13, 40, 30, cta.nota, "0")
	END SELECT
	IF devolver = true THEN
		devolver = false: puntero% = puntero% - 1: IF puntero% < 1 THEN puntero% = 4
	ELSE
		puntero% = puntero% + 1: IF puntero% > 4 THEN puntero% = 1
	END IF
	'  OJO OJO OJO OJO OJO
	'  1.- LA FECHA DE LA OPERACIàN DEBE SER MAYOR QUE LA APERTURA
	'  2.- EL MONTO DEL DEPàSITO O RETIRO DEBE SER MAYOR QUE CERO
	'
	'
	IF Fun(9) = "si" THEN 'salvar en archivo
		max% = LOF(1) / 53: max% = max% + 1: PUT #1, max%, master
		auxi$ = "f:\kevinprg\bancos\" + MID$(STR$(max%), 2, LEN(STR$(max%)) - 1) + ".cta"
		OPEN auxi$ FOR RANDOM AS #2 LEN = 93
		cta.oper = "0": cta.auxi = " ": cta.fech2 = NorteaSur$(DATE$): cta.hora2 = MID$(TIME$, 1, 5)
		cta.num = 0: cta.debe = 0: cta.haber = 0: cta.saldo = master.saldo
		PUT #2, 1, cta: CLOSE #2: EXIT DO
	END IF
	IF Fun(10) = "si" THEN 'aborta la creacion
		k% = Verify(13, "¨Seguro desea abortar la creaci¢n")
		IF k% = true THEN EXIT DO
	END IF
LOOP
CALL ShutUp
END SUB

SUB Ordenar (opc%)
' i%=0 ordena todas las cuentas, otro valor, una cuenta espec¡fica
COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box2 1, 5, 24, 75
max% = LOF(1) / 51
IF max% = 0 THEN PopUp 12, 27, 2, 30, 31, 1, 1, 0: Center 13, "No hay cuentas creadas": Espera: ShutUp: EXIT SUB
Termometro 0, max%, 0, "Ordenando por fechas... ", 13, 19
IF opc% = 0 THEN 'ordena todas las cuentas
	FOR k% = 1 TO max%
		Termometro k, max%, 1, "Ordenando por fechas... ", 13, 19
		GET #1, k%, master
		IF master.acti = " " THEN
			auxi$ = "f:\kevinprg\bancos\" + MID$(STR$(k%), 2, LEN(STR$(k%)) - 1) + ".cta"
			CLOSE #2: OPEN auxi$ FOR RANDOM AS #2 LEN = 93: N = LOF(2) / 93: Salto = N
			'cta y ctaaux
				WHILE Salto > 1
						 Salto = Salto / 2
						 DO
									Sw = 1
									FOR J = 1 TO N - Salto
											 I = J + Salto
											 GET #2, J, ctaaux
											 GET #2, I, cta
											 IF ctaaux.dias > cta.dias THEN
														LOCATE 2, 2: PRINT J, I
														PUT #2, J, cta
														PUT #2, I, ctaaux
														Sw = 0
											 END IF
									NEXT J
						 LOOP UNTIL Sw = 1
				WEND
			 
				GET #2, 1, ctaaux
				FOR l% = 2 TO N
					GET #2, l%, cta
					cta.fech = TrataFechas$(cta.fech, "dd/mm/aaaa", "dd/mm/aaaa")
					cta.saldo = ctaaux.saldo + cta.haber - cta.debe
					PUT #2, l%, cta
					GET #2, l%, ctaaux
				NEXT l%
				master.saldo = cta.saldo
				PUT #1, k%, master
				CLOSE #2
		END IF
	NEXT k%
ELSE ' ordena una sola cuenta, opc% indica cual cuemta
		GET #1, opc%, master
		IF master.acti = " " THEN
			auxi$ = "f:\kevinprg\bancos\" + MID$(STR$(opc%), 2, LEN(STR$(opc%)) - 1) + ".cta"
			CLOSE #2: OPEN auxi$ FOR RANDOM AS #2 LEN = 93: N = LOF(2) / 93: Salto = N
			'cta y ctaaux
				WHILE Salto > 1
						 Salto = Salto / 2
						 DO
									Sw = 1
									FOR J = 1 TO N - Salto
											 I = J + Salto
											 GET #2, J, ctaaux
											 GET #2, I, cta
											 IF ctaaux.dias > cta.dias THEN
														LOCATE 2, 2: PRINT J, I
														PUT #2, J, cta
														PUT #2, I, ctaaux
														Sw = 0
											 END IF
									NEXT J
						 LOOP UNTIL Sw = 1
				WEND
			
				GET #2, 1, ctaaux
				FOR l% = 2 TO N
					GET #2, l%, cta
					cta.fech = TrataFechas$(cta.fech, "dd/mm/aaaa", "dd/mm/aaaa")
					cta.saldo = ctaaux.saldo + cta.haber - cta.debe
					PUT #2, l%, cta
					GET #2, l%, ctaaux
				NEXT l%
				master.saldo = cta.saldo
				PUT #1, opc%, master
				CLOSE #2
		END IF
END IF
FOR k = 1 TO 99
	GET #5, k, bank: PUT #6, VAL(bank.num), bank.nom
NEXT k
END SUB

SUB Preferencias
END SUB

SUB Registro
CLS : Box2 1, 5, 24, 75
Center 2, "Ver anotaciones seg£n fecha"
kta% = Escoge%(1): IF kta% < 0 THEN EXIT SUB
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
			IF DateToJulian(fecha1$, 2) <= DateToJulian(fecha2$, 2) THEN
				fecha1$ = TrataFechas$(fecha1$, "dd/mm/aaaa", "dd/mm/aaaa")
				fecha2$ = TrataFechas$(fecha2$, "dd/mm/aaaa", "dd/mm/aaaa")
				EXIT DO
			ELSE
				PopUp 17, 15, 2, 50, 31, 1, 1, 0: Center 18, "La fecha de inicio es MAYOR que la fecha final": Espera: ShutUp
			END IF
		ELSE
			PopUp 17, 20, 2, 40, 31, 1, 1, 0: Center 18, "La fecha de finalizaci¢n NO ES VALIDA": Espera: ShutUp
		END IF
	ELSE
		PopUp 17, 20, 2, 40, 31, 1, 1, 0: Center 18, "La fecha de inicio NO ES VALIDA": Espera: ShutUp
	END IF
LOOP
ShutUp
PopUp 3, 3, 20, 74, 31, 1, 1, 0
auxi$ = "f:\kevinprg\bancos\" + MID$(STR$(kta%), 2, LEN(STR$(kta%)) - 1) + ".cta"
CLOSE #2: OPEN auxi$ FOR RANDOM AS #2 LEN = 93: max% = LOF(2) / 93
l% = 1: fechaux$ = fecha1$
Center 5, "Registro de anotaciones hechos seg£n fecha"
Center 25, "Registro de anotaciones hechos seg£n fecha"
LOCATE 7, 4
PRINT fecha1$; "  Operaci¢n               DEBE          HABER          SALDO"
FOR k% = 1 TO max%
GET #2, k%, cta
IF cta.dias >= DateToJulian(fecha1$, 2) AND cta.dias <= DateToJulian(fecha2$, 2) THEN
	IF fechaux$ <> cta.fech THEN
		LOCATE 7 + l%, 4
		PRINT cta.fech; "  Operaci¢n               DEBE          HABER          SALDO"
		l% = l% + 1:  LOCATE 7 + l%, 14: fechaux$ = cta.fech
	END IF
	LOCATE 7 + l%, 4
	IF cta.num <= 99999999 THEN
		PRINT USING "#########, "; cta.num;
	ELSEIF cta.num <= 9999999999# AND cta.num > 99999999 THEN
		PRINT USING "########## "; cta.num;
	ELSE
		PRINT "desborde   ";
	END IF
	' 0=apertura         1=dep¢sito               2=retiro
	' 3=D‚bito bancario  4=Nota cr‚dito           5=Nota d‚bito
	' 6=Emisi¢n edo cta  7=Manten. cta.           8=Emisi¢n Chequera
	' 9=Emisi¢n libreta  A=Emisi¢n tarjeta d‚bito
	' B=Pago tja. d‚bito C=Pago cheque            D=Intereses
	' E=Retiro Caj. aut. F=Cobro tja. d‚bito
	SELECT CASE cta.oper ' cuenta
		CASE "0": PRINT "Apertura ......";
		CASE "1": PRINT "Dep¢sito ......";
		CASE "2": PRINT "Retiro ........";
		CASE "3": PRINT "D‚bito bancario";
		CASE "4": PRINT "Nota de cr‚dito";
		CASE "5": PRINT "Nota de d‚bito.";
		CASE "6": PRINT "Emis. edo. cta.";
		CASE "7": PRINT "Manten. de cta.";
		CASE "8": PRINT "Emis. chequera.";
		CASE "9": PRINT "Emis. libreta..";
		CASE "A": PRINT "Emis. tja. d‚b.";
		CASE "B": PRINT "Pago. tja. d‚b.";
		CASE "C": PRINT "Pago con cheque";
		CASE "D": PRINT "Intereses......";
		CASE "E": PRINT "Retiro caj. aut";
		CASE "F": PRINT "Cobro tja.d‚bit";
	END SELECT
	GOSUB DebeHaberSald
	l% = l% + 1
	IF l% > 10 THEN
		l% = 0: Espera
		ShutUp
		PopUp 3, 3, 20, 74, 31, 1, 1, 0
		Center 5, "Registro de anotaciones hechos seg£n fecha"
	END IF
END IF
NEXT k%
GET #2, max%, cta
LOCATE 20, 4: PRINT "Saldo disponible ACTUAL de cuenta:";
PRINT USING "###########,.##"; cta.saldo;
CLOSE #2
Center 21, "Visualizacion de registros finalizada"
Espera
ShutUp
EXIT SUB

DebeHaberSald:
	PRINT USING "###########,.##"; cta.debe;
	PRINT USING "###########,.##"; cta.haber;
	IF cta.saldo < 0 THEN COLOR colors(7, 4), colors(4, 4)
	PRINT USING "###########,.##"; cta.saldo;
	IF cta.saldo < 0 THEN COLOR colors(7, colorpref), colors(4, colorpref)
RETURN


END SUB

SUB Termometro (va%, total%, edo%, text$, fila%, columna%)
IF edo% = 0 THEN Box fila%, columna%, fila% + 6, columna% + 31
LOCATE fila% + 1, columna% + 1: PRINT "0%                        100%"
IF edo% = 0 THEN LOCATE fila% + 2, columna% + 1: PRINT STRING$(30, "±")
LOCATE fila% + 2, columna% + 1: PRINT STRING$(INT((va% / total%) * 30), "Û")
LOCATE fila% + 3, columna% + 1: PRINT "Van"; va%; " de "; total%;
LOCATE fila% + 4, columna% + 9: PRINT USING "##.## % Hecho "; ((va% / total%) * 100)
LOCATE fila% + 5, columna% + 1: PRINT text$
END SUB

SUB Totalizar
CLS
Box2 1, 5, 24, 75: Center 2, "Totalizaci¢n de TODAS las cuentas activas"
max% = LOF(1) / 51: l% = 1
IF max% = 0 THEN PopUp 12, 27, 2, 30, 31, 1, 1, 0: Center 13, "No hay cuentas creadas": Espera: ShutUp: EXIT SUB
FOR k% = 1 TO max%
	GET #1, k%, master
	IF master.acti = " " THEN
		IF master.acti = " " THEN
			GET #6, VAL(master.codi), ordbank
		END IF
		LOCATE 2 + l%, 8: PRINT ordbank; " ";
		PRINT master.numcta; "       Bs.";
		PRINT USING "###########,.##"; master.saldo
		l% = l% + 1
	END IF
NEXT k%
Espera
END SUB

FUNCTION TrataFechas$ (dat$, fechaentra$, fechasale$)
fechaentra$ = LCASE$(fechaentra$)
fechasale$ = LCASE$(fechasale$)
FOR k = 1 TO LEN(fechasale$)
	r$ = MID$(fechasale$, k, 1)
	SELECT CASE r$
		CASE "d", "m", "a"
		CASE ELSE
			MID$(fechasale$, k, 1) = "/"
	END SELECT
NEXT k
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

FUNCTION Trim$ (X$)
IF X$ = "" THEN
	Trim$ = ""
ELSE
lastChar = 0
FOR A = 1 TO LEN(X$): y$ = MID$(X$, A, 1): IF y$ <> CHR$(0) AND y$ <> " " THEN lastChar = A
NEXT A: Trim$ = LEFT$(X$, lastChar)
END IF
END FUNCTION

