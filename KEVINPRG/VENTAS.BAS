'modulo de ventas de contado
DEFINT A-Z
DECLARE SUB BackFill (Row%, col%, Rows%, Cols%, Attribute%)
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
DECLARE SUB Ayuda (nomb$)
DECLARE SUB Toma ()
DECLARE SUB Telefonos ()
DECLARE SUB ImprimePrevia ()
DECLARE SUB Anula ()
DECLARE SUB Cambiar ()
DECLARE SUB Impresionar ()
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$, tr$)
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Escoge% (fila%, edo%, fila2%, Prod$)
DECLARE FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%)
DECLARE FUNCTION Trim$ (x$)
DECLARE FUNCTION DateToJulian& (dat$, caso%)
COMMON SHARED guia, colorpref
colorpref = 1: guia = 1
DIM SHARED nursaler, ybusqueda, llave$, kbe$, sino: guia = 1
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, impuesto AS STRING * 6, NombFarma AS STRING * 25, Direccion AS STRING * 30, Rif     AS STRING * 25, Nit      AS STRING * 25, telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
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
DIM SHARED Yname1(15) AS STRING * 3, Yname2(15) AS STRING * 15
REM $DYNAMIC
DIM SHARED nombra(5) AS STRING * 35, Pvp1a(5) AS STRING * 8, Pvp2a(5) AS STRING * 8, Pvp3a(5) AS STRING * 8, Pvp4a(5) AS STRING * 8, Numa(5) AS STRING * 2, Exis1a(5) AS STRING * 2, Exis2a(5) AS STRING * 2, Exis3a(5) AS STRING * 2, Exis4a(5) AS  _
STRING * 2
DIM SHARED Cant1a(5) AS STRING * 2, Cant2a(5) AS STRING * 2, Cant3a(5) AS STRING * 2, Cant4a(5) AS STRING * 2, Ubica(5) AS STRING * 2
CONST true = -1: CONST false = NOT true:  ybusqueda = 0
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
DIM SHARED tarje AS dbf, vivos AS idx, cuenta AS cta
ON ERROR GOTO ErrorTrap
ON TIMER(60) GOSUB Reloj
CALL LeeEstado: CALL VentaContado
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
		LOCATE 1, 1: PRINT "Err:"; ERR
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

ErrLocal:
sino = true
CLOSE #117
CALL PopUp(7, 30, 9, 22, 112, 1, 0, 0)
CALL Espera
CALL ShutUp
ON ERROR GOTO ErrorTrap
RETURN

REM $STATIC
SUB Anula
Box2 17, 10, 24, 70: LOCATE 20, 20: PRINT "¨Desea LIMPIAR esta Factura?"
keya$ = UCASE$(MeteDatoLetra$(20, 48, 1, "N", "0"))
IF MID$(keya$, 1, 1) = "s" OR MID$(keya$, 1, 1) = "S" THEN
	CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200: renglon = renglon - 1
	FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS  _
cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS Saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
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
	CLOSE : total = MKD$(0): costo = MKD$(0): neto = MKD$(0): DescGer = MKD$(0): GOSUB GrabaPrv: renglon = 1
END IF
COLOR colors(7, colorpref), colors(4, colorpref): FOR k = 5 TO 24: LOCATE k, 1: PRINT SPACE$(80); : NEXT k
END SUB

SUB Ayuda (nomb$)
sino = false: puntero% = 1
ON ERROR GOTO ErrLocal
DIM ayu(15) AS STRING * 60
nomb$ = "f:\kevinprg\help\" + Trim$(nomb$) + ".hlp"
OPEN nomb$ FOR INPUT SHARED AS #117
IF sino = true THEN EXIT SUB
ON ERROR GOTO ErrorTrap
EXIT SUB

LeeQuince:

RETURN
END SUB

SUB Box (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿"; : FOR a = Row1 + 1 TO Row2 - 1: LOCATE a, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Box2 (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "²"; STRING$(BoxWidth - 2, "²"); "²"; : FOR a = Row1 + 1 TO Row2 - 1: LOCATE a, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Cambiar
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
	IF VAL(desc) > 0 THEN DescGer = MKD$(CVD(total) * VAL(desc) / 100)
	IF CVD(DescGer) > 0 THEN LOCATE 2, 3: PRINT "Sub-total Neto:"; : PRINT USING "#########,.## Bs"; (CVD(total) - CVD(DescGer)); : PRINT USING " (##.##%)"; (100 - (100 * ((CVD(total) - CVD(DescGer)) / CVD(total))));
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
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS Saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
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
					DescGer = MKD$(CVD(DescGer) - ((CVD(pv32$) - CVD(pv42$)) * VAL(canti$)))
				END IF
			END IF
		END IF
	END IF
	IF salir = true THEN
		PUT #1, CVI(regi1(sit%)): UNLOCK #1, CVI(regi1(sit%))
	ELSE
		PRINT "Problema con:"; desc1(sit%): Espera
	END IF: CLOSE
total = MKD$(CVD(total) - CVD(prec1(sit%)) * VAL(canti$)): costo = MKD$(CVD(costo) - CVD(cost1(sit%)) * VAL(canti$))
IF VAL(canti$) = CVI(cant1(sit%)) THEN
	renglon = renglon - 1
	FOR k = sit% TO renglon
	regi1(k) = regi1(k + 1): regi2(k) = regi2(k + 1): desc1(k) = desc1(k + 1): psic1(k) = psic1(k + 1)
	cant1(k) = cant1(k + 1): cost1(k) = cost1(k + 1): ivaf1(k) = ivaf1(k + 1): prec1(k) = prec1(k + 1): code1(k) = code1(k + 1)
	NEXT k
ELSE
cant1(sit%) = MKI$((CVI(cant1(sit%)) - (VAL(canti$))))
END IF
COLOR colors(7, colorpref), colors(4, colorpref): FOR k = 17 TO 24: LOCATE k, 1: PRINT SPACE$(80); : NEXT k
OPEN fx2 FOR RANDOM AS #17 LEN = 63: FIELD #17, 35 AS descri$, 2 AS cantid$, 8 AS precio$, 8 AS costid$, 1 AS isvcm$, 6 AS codito$, 2 AS regist1$, 1 AS regist2$
LSET descri$ = fct + DescGer + MKI$(renglon) + SPACE$(21): PUT #17, 1
FOR k = 2 TO 27
RSET descri$ = desc1(k - 1): RSET cantid$ = cant1(k - 1): RSET precio$ = prec1(k - 1): RSET costid$ = cost1(k - 1)
RSET isvcm$ = ivaf1(k - 1): RSET codito$ = code1(k - 1): RSET regist1$ = regi1(k - 1): RSET regist2$ = regi2(k - 1): PUT #17, k
NEXT k: CLOSE
IF Fun(12) = "si" THEN
	Fun(12) = "no"
	OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #1 LEN = 100
	FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS Cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
	nursaler = LOF(1) / 100
	FOR k = 1 TO nursaler: GET #1, k
	IF Cedula$ = Cedu$ THEN RSET inout$ = " ": RSET cong$ = "*": PUT #1, k: EXIT FOR
	NEXT k: CLOSE
	FOR k = 1 TO 26
	regi1(k) = MKI$(0): regi2(k) = " ": desc1(k) = "": cant1(k) = MKI$(0)
	cost1(k) = MKD$(0): ivaf1(k) = " ": prec1(k) = MKD$(0): code1(k) = "": psic1(k) = " "
	NEXT k: DescGer = MKD$(0): CLOSE : FOR k = 1 TO 17: Fun(k) = "no": NEXT k: salir = true
END IF
END SUB

SUB Center (Row, text$)
IF Row = 25 THEN COLOR 0, 15: LOCATE 25, 1: PRINT SPACE$(80);
LOCATE Row, 41 - LEN(text$) / 2: PRINT text$;
END SUB

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
		a& = 146097# * FIX(FIX(year% / 100) / 4)
		B& = FIX(1461# * (year% MOD 100) / 4)
		C& = FIX((153# * month% + 2) / 5) + day% + 1721119
		D& = a& + B& + C&
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
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna%: PRINT "²"; elccion$; "²": IF sonido = "si" THEN BEEP
EntraDatoNumero$ = elccion$: EXIT FUNCTION
END FUNCTION

FUNCTION Escoge% (fila%, edo%, fila2%, Prod$)
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila% - 1, 1: PRINT STRING$(80, "Ä"): LOCATE fila% + 5, 1
PRINT STRING$(80, "Ä"); : FOR k = fila% TO fila% + 4: LOCATE k, 1: PRINT SPACE$(80); : NEXT k
STATIC max2%, Prud AS STRING * 6
IF edo% = 1 THEN
	CALL Center(25, "[F5]Equiv [F9]Salir SIN Escoger [Enter]Toma Articulo [A-Z]Ir a [F11]Vademecum")
ELSE
	CALL Center(25, "[F5]Equiv [F9]Salir SIN Escoger [Enter]Toma Articulo [F11]Vademecum")
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
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS Saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
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
IF edo% <> 2 THEN TIMER ON
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
		CASE CHR$(0) + CHR$(63): GOSUB Parche3
		CASE CHR$(0) + CHR$(133)
			auxi$ = Trim$(MID$(nombra(resal%), 1, 8))
			FOR m% = 1 TO LEN(auxi$)
				IF MID$(auxi$, m%, 1) = " " THEN
					EXIT FOR
				ELSE
					aux$ = aux$ + MID$(auxi$, m%, 1)
				END IF
			NEXT m%: aux$ = UCASE$(aux$)
			CALL HelpMate(1, aux$): aux$ = ""
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
			LOCATE fila% + l - 1, 1: PRINT nombra(l); "³"; : LOCATE fila% + l - 1, 37: PRINT USING "#####, "; CVI(Cant1a(l)); : LOCATE fila% + l - 1, 51: PRINT "³"; : PRINT USING "#####, "; CVI(Cant2a(l));
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
		nombra(selecc%) = nomb2$: Cant1a(selecc%) = exi12$: Cant2a(selecc%) = exi22$: Cant3a(selecc%) = exi32$: Cant4a(selecc%) = exi42$: Pvp1a(selecc%) = pv12$: Pvp2a(selecc%) = pv22$: Pvp3a(selecc%) = pv32$: Pvp4a(selecc%) = pv42$:  Numa(selecc%) =  _
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
		nombra(selecc%) = nomb2$: Cant1a(selecc%) = exi12$: Cant2a(selecc%) = exi22$: Cant3a(selecc%) = exi32$: Cant4a(selecc%) = exi42$: Pvp1a(selecc%) = pv12$: Pvp2a(selecc%) = pv22$: Pvp3a(selecc%) = pv32$: Pvp4a(selecc%) = pv42$:  Numa(selecc%) =  _
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
LOCATE fila%, 1: PRINT nombra(1); "³"; : LOCATE fila%, 37: PRINT USING "#####, "; CVI(Cant1a(1)); : LOCATE fila%, 51: PRINT "³"; : PRINT USING "#####, "; CVI(Cant2a(1)); : LOCATE fila%, 66: PRINT "³"; : PRINT USING "#####, "; CVI(Cant3a(1))
LOCATE fila%, 44: PRINT USING "######,"; CVD(Pvp1a(1)): LOCATE fila%, 59: PRINT USING "######,"; CVD(Pvp2a(1)): LOCATE fila%, 74: PRINT USING "######,"; CVD(Pvp3a(1)): COLOR colors(4, colorpref), colors(7, colorpref)
IF CVD(Pvp3a(1)) = 0 THEN Pvp3a(1) = MKD$(1)
perco$ = MKD$((CVD(Pvp4a(1)) / (CVD(Pvp3a(1))))): IF CVD(perco$) = 0 THEN perco$ = MKD$(1)
LOCATE fila% - 2, 5: PRINT Prod$: LOCATE fila% - 2, 20: GET #77, CVI(Ubica(1)): PRINT describe$
LOCATE fila% - 2, 40: PRINT "En Oferta: "; : PRINT USING "#####, "; CVI(Cant4a(1)); : PRINT " a Bs."; : PRINT USING "######,"; CVD(Pvp4a(1)); : PRINT USING " (##.##%)"; (100 - (100 * CVD(perco$)));
FOR k = 2 TO 5
	IF k <= selecc% THEN
		LOCATE fila% + k - 1, 1: PRINT nombra(k); "³"; : LOCATE fila% + k - 1, 37: PRINT USING "#####, "; CVI(Cant1a(k)); : LOCATE fila% + k - 1, 51: PRINT "³"; : PRINT USING "#####, "; CVI(Cant2a(k));
		LOCATE fila% + k - 1, 66: PRINT "³"; : PRINT USING "#####, "; CVI(Cant3a(k)): LOCATE fila% + k - 1, 44: PRINT USING "######,"; CVD(Pvp1a(k)): LOCATE fila% + k - 1, 59: PRINT USING "######,"; CVD(Pvp2a(k))
		LOCATE fila% + k - 1, 74: PRINT USING "######,"; CVD(Pvp3a(k))
	ELSE
		LOCATE fila% + k - 1, 1: PRINT SPACE$(80)
	END IF
NEXT k: resal% = 1
RETURN
Parche3:
		ge% = CVI(Numa(resal%))
		GET #1, ge%: masdeuno = false
		IF CVI(compo12$) > 1 THEN Prod$ = compo12$
		IF CVI(compo22$) > 1 THEN Prod$ = compo22$: masdeuno = true
		IF CVI(compo32$) > 1 THEN Prod$ = compo32$: masdeuno = true
		IF CVI(compo42$) > 1 THEN Prod$ = compo42$: masdeuno = true
		IF CVI(compo52$) > 1 THEN Prod$ = compo52$: masdeuno = true
		IF CVI(compo62$) > 1 THEN Prod$ = compo62$: masdeuno = true
		IF masdeuno = true THEN
			PopUp 7, 30, 9, 22, 112, 1, 0, 0
			LOCATE 8, 31: PRINT "Escoja un componente";
			OPEN "f:\kevinprg\kevindat\ordcompo.dat" FOR RANDOM SHARED AS #78 LEN = 15
			FIELD #78, 15 AS describe$
			IF CVI(compo12$) > 1 THEN GET #78, CVI(compo12$): LOCATE 9, 31: PRINT "1.-"; describe$;
			IF CVI(compo22$) > 1 THEN GET #78, CVI(compo22$): LOCATE 10, 31: PRINT "2.-"; describe$;
			IF CVI(compo32$) > 1 THEN GET #78, CVI(compo32$): LOCATE 11, 31: PRINT "3.-"; describe$;
			IF CVI(compo42$) > 1 THEN GET #78, CVI(compo42$): LOCATE 12, 31: PRINT "4.-"; describe$;
			IF CVI(compo52$) > 1 THEN GET #78, CVI(compo52$): LOCATE 13, 31: PRINT "5.-"; describe$;
			IF CVI(compo62$) > 1 THEN GET #78, CVI(compo62$): LOCATE 14, 31: PRINT "6.-"; describe$;
			CLOSE #78: kd$ = "": Center 25, "Escoja un componente, pulse del 1 al 6"
			DO WHILE kd$ = "": kd$ = INKEY$: LOOP
			SELECT CASE kd$
				CASE "1": IF CVD(compo12$) > 1 THEN Prod$ = compo12$ ELSE RETURN
				CASE "2": IF CVD(compo22$) > 1 THEN Prod$ = compo22$ ELSE RETURN
				CASE "3": IF CVD(compo32$) > 1 THEN Prod$ = compo32$ ELSE RETURN
				CASE "4": IF CVD(compo42$) > 1 THEN Prod$ = compo42$ ELSE RETURN
				CASE "5": IF CVD(compo52$) > 1 THEN Prod$ = compo52$ ELSE RETURN
				CASE "6": IF CVD(compo62$) > 1 THEN Prod$ = compo62$ ELSE RETURN
				CASE ELSE: RETURN
			END SELECT
			CALL ShutUp
		END IF
		CLOSE #2: OPEN "F:\kevinprg\kevindat\compos.idx" FOR RANDOM SHARED AS #2 LEN = 4
		FIELD #2, 2 AS nomshort$, 2 AS num$: selecc% = 0: max2% = LOF(2) / 4
		IF max2% = 0 THEN LOCATE fila% + 5, 5: PRINT STRING$(5, "Ä"); "² NO Hay Datos ²"; STRING$(7, "Ä"); : CALL Center(25, "Presione Cualquier Tecla Para Continuar"): Espera: Escoge% = 0: EXIT FUNCTION
		GOSUB Parche1: GOSUB Parche2
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
	auxi% = FIX(RND * 100): IF auxi% >= 65 AND auxi% <= 90 THEN EXIT DO
LOOP
LOCATE fila%, columna%: PRINT "Presione la tecla:"; CHR$(auxi%); " para Continuar"
DO
	llave$ = "": DO WHILE llave$ = "": llave$ = INKEY$: LOOP
	llave$ = UCASE$(llave$): IF llave$ = CHR$(auxi%) THEN EXIT DO ELSE BEEP
LOOP
END SUB

SUB Impresionar
					PRINT #10, USING "      Factura:######,"; CVL(fct$); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
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
					PRINT #10, USING "Factura:######,"; CVL(fct$); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
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
					PRINT #10, "      Cliente:                               Vendedor:"; Cedu$; SPACE$(11);
					PRINT #10, "Cliente:                               Vendedor:"; Cedu$; CHR$(10);
					PRINT #10, "      "; nombre$; SPACE$(29);
					PRINT #10, nombre$; CHR$(10);
					rifo$ = Trim$(rifo$): nito$ = Trim$(nito$)
					PRINT #10, "      R.I.F.:"; rifo$; SPACE$(22 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(23 - LEN(nito$)); SPACE$(9);
					PRINT #10, "R.I.F.:"; rifo$; SPACE$(22 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(23 - LEN(nito$)); CHR$(10);
					domicilio1$ = Trim$(domicilio1$): domicilio2$ = Trim$(domicilio2$): tlf$ = Trim$(tlf$)
					PRINT #10, "      Domicilio Fiscal:"; SPACE$(52);
					PRINT #10, "Domicilio Fiscal:"; CHR$(10);
					PRINT #10, "      "; domicilio1$; SPACE$(60 - LEN(domicilio1$)); SPACE$(9);
					PRINT #10, domicilio1$; CHR$(10);
					PRINT #10, "      "; domicilio2$; SPACE$(60 - LEN(domicilio2$)); SPACE$(9);
					PRINT #10, domicilio2$; CHR$(10);
					PRINT #10, "      Telefonos:"; tlf$; SPACE$(20 - LEN(tlf$)); " Forma de Pago: Contado       "; SPACE$(9);
					PRINT #10, "Telefonos:"; tlf$; SPACE$(20 - LEN(tlf$)); " Forma de Pago: Contado"; CHR$(10);
					PRINT #10, "      "; STRING$(60, "-"); SPACE$(9);
					PRINT #10, STRING$(60, "-"); CHR$(10);
					renglon = renglon - 1: auxe$ = MKD$(0)
					PRINT #10, "      ³Descripcion                        ³Cantidad³Precio Venta³ "; SPACE$(9);
					PRINT #10, "³Descripcion                        ³Cantidad³Precio Venta³ "; CHR$(10);
					SELECT CASE renglon
						  CASE 1 TO 7
								 FOR k = 1 TO renglon
										PRINT #10, "      ³"; desc1(k); "³";
										PRINT #10, USING "#######,³"; CVI(cant1(k));
										IF ivaf1(k) = "*" THEN
											  IF NuevoIva = "si" THEN
													 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
													 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100)))))
											  ELSE
													 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
													 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)))))
											  END IF
										ELSE
											  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
											  auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(prec1(k))))
										END IF
									
										PRINT #10, SPACE$(9); "³"; desc1(k); "³";
										PRINT #10, USING "#######,³"; CVI(cant1(k));
										IF ivaf1(k) = "*" THEN
											  IF NuevoIva = "si" THEN
													 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
											  ELSE
													 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
											  END IF
										ELSE
											  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
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
														 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
														 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100)))))
												  ELSE
														 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
														 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)))))
												  END IF
											ELSE
												  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
												  auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(prec1(k))))
											END IF
									  
											PRINT #10, SPACE$(9); "³"; desc1(k); "³";
											PRINT #10, USING "#######,³"; CVI(cant1(k));
											IF ivaf1(k) = "*" THEN
												  IF NuevoIva = "si" THEN
														 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
												  ELSE
														 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
												  END IF
											ELSE
												  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
											END IF
											PRINT #10, CHR$(10);
										ELSE
											PRINT #10, SPACE$(6); SPACE$(ler%); " \ \ \ \"; SPACE$(60 - ler% - 8); SPACE$(9);
											PRINT #10, SPACE$(ler%); " \ \ \ \"; CHR$(10); : ler% = ler% + 1
										END IF
								 NEXT k
								 PRINT #10, SPACE$(6); SPACE$(34); "VAN --> Bs "; : PRINT #10, USING "##########,.##"; CVD(auxe$); : PRINT #10, SPACE$(9);
								 PRINT #10, SPACE$(34); "VAN --> "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); CHR$(10);
								 FOR mrw% = 1 TO 9: PRINT #10, CHR$(10); : NEXT mrw%
								 PRINT #10, USING "      Factura:######,"; CVL(fct$); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
								 PRINT #10, " Pagina:2 de 2"; SPACE$(9);
								 PRINT #10, USING "Factura:######,"; CVL(fct$); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
								 PRINT #10, " Pagina:2 de 2"; CHR$(10);
								 PRINT #10, "      Cliente:                               Vendedor:"; Cedu$; SPACE$(11);
								 PRINT #10, "Cliente:                               Vendedor:"; Cedu$; CHR$(10);
								 PRINT #10, "      "; nombre$; SPACE$(29);
								 PRINT #10, nombre$; CHR$(10);
								 rifo$ = Trim$(rifo$): nito$ = Trim$(nito$)
								 PRINT #10, "      R.I.F.:"; rifo$; SPACE$(22 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(23 - LEN(nito$)); SPACE$(9);
								 PRINT #10, "R.I.F.:"; rifo$; SPACE$(22 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(23 - LEN(nito$)); CHR$(10);
								 domicilio1$ = Trim$(domicilio1$): domicilio2$ = Trim$(domicilio2$)
								 PRINT #10, "      Domicilio Fiscal:"; SPACE$(52);
								 PRINT #10, "Domicilio Fiscal:"; CHR$(10);
								 PRINT #10, "      "; domicilio1$; SPACE$(60 - LEN(domicilio1$)); SPACE$(9);
								 PRINT #10, domicilio1$; CHR$(10);
								 PRINT #10, "      "; domicilio2$; SPACE$(60 - LEN(domicilio2$)); SPACE$(9);
								 PRINT #10, domicilio2$; CHR$(10);
								 PRINT #10, "      Telefonos:"; tlf$; SPACE$(20 - LEN(tlf$)); " Forma de Pago: Contado       "; SPACE$(9);
								 PRINT #10, "Telefonos:"; tlf$; SPACE$(20 - LEN(tlf$)); " Forma de Pago: Contado"; CHR$(10);
								 PRINT #10, "      "; STRING$(60, "-"); SPACE$(9);
								 PRINT #10, STRING$(60, "-"); CHR$(10);
								 PRINT #10, "      ³Descripcion                        ³Cantidad³Precio Venta³ "; SPACE$(9);
								 PRINT #10, "³Descripcion                        ³Cantidad³Precio Venta³ "; CHR$(10);
								 PRINT #10, SPACE$(6); SPACE$(35); "VIENEN->"; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$);
								 PRINT #10, SPACE$(9); SPACE$(35); "VIENEN->"; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$);
								 PRINT #10, CHR$(10);
								 FOR k = 16 TO 21
										IF k <= renglon THEN
											PRINT #10, "      ³"; desc1(k); "³";
											PRINT #10, USING "#######,³"; CVI(cant1(k));
											IF ivaf1(k) = "*" THEN
												  IF NuevoIva = "si" THEN
														 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
														 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100)))))
												  ELSE
														 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
														 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)))))
												  END IF
											ELSE
												  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
												  auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(prec1(k))))
											END IF
									 
											PRINT #10, SPACE$(9); "³"; desc1(k); "³";
											PRINT #10, USING "#######,³"; CVI(cant1(k));
											IF ivaf1(k) = "*" THEN
												  IF NuevoIva = "si" THEN
														 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
												  ELSE
														 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
												  END IF
											ELSE
												  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
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
														 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
														 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100)))))
												  ELSE
														 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
														 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)))))
												  END IF
											ELSE
												  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
												  auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(prec1(k))))
											END IF
									 
											PRINT #10, SPACE$(9); "³"; desc1(k); "³";
											PRINT #10, USING "#######,³"; CVI(cant1(k));
											IF ivaf1(k) = "*" THEN
												  IF NuevoIva = "si" THEN
														 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
												  ELSE
														 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
												  END IF
											ELSE
												  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
											END IF
											PRINT #10, CHR$(10);
										ELSE
											PRINT #10, SPACE$(6); SPACE$(ler%); " \ \ \ \"; SPACE$(60 - ler% - 8); SPACE$(9);
											PRINT #10, SPACE$(ler%); " \ \ \ \"; CHR$(10); : ler% = ler% + 1
										END IF
								 NEXT k
								 PRINT #10, SPACE$(6); SPACE$(34); "VAN --> Bs "; : PRINT #10, USING "##########,.##"; CVD(auxe$); : PRINT #10, SPACE$(9);
								 PRINT #10, SPACE$(34); "VAN --> "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); CHR$(10);
								 FOR mrw% = 1 TO 9: PRINT #10, CHR$(10); : NEXT mrw%
								 PRINT #10, USING "      Factura:######,"; CVL(fct$); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
								 PRINT #10, " Pagina:2 de 3"; SPACE$(9);
								 PRINT #10, USING "Factura:######,"; CVL(fct$); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
								 PRINT #10, " Pagina:2 de 3"; CHR$(10);
								 PRINT #10, "      Cliente:                               Vendedor:"; Cedu$; SPACE$(11);
								 PRINT #10, "Cliente:                               Vendedor:"; Cedu$; CHR$(10);
								 PRINT #10, "      "; nombre$; SPACE$(29);
								 PRINT #10, nombre$; CHR$(10);
								 rifo$ = Trim$(rifo$): nito$ = Trim$(nito$)
								 PRINT #10, "      R.I.F.:"; rifo$; SPACE$(22 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(23 - LEN(nito$)); SPACE$(9);
								 PRINT #10, "R.I.F.:"; rifo$; SPACE$(22 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(23 - LEN(nito$)); CHR$(10);
								 domicilio1$ = Trim$(domicilio1$): domicilio2$ = Trim$(domicilio2$)
								 PRINT #10, "      Domicilio Fiscal:"; SPACE$(52);
								 PRINT #10, "Domicilio Fiscal:"; CHR$(10);
								 PRINT #10, "      "; domicilio1$; SPACE$(60 - LEN(domicilio1$)); SPACE$(9);
								 PRINT #10, domicilio1$; CHR$(10);
								 PRINT #10, "      "; domicilio2$; SPACE$(60 - LEN(domicilio2$)); SPACE$(9);
								 PRINT #10, domicilio2$; CHR$(10);
								 PRINT #10, "      Telefonos:"; tlf$; SPACE$(20 - LEN(tlf$)); " Forma de Pago: Contado       "; SPACE$(9);
								 PRINT #10, "Telefonos:"; tlf$; SPACE$(20 - LEN(tlf$)); " Forma de Pago: Contado"; CHR$(10);
								 PRINT #10, "      "; STRING$(60, "-"); SPACE$(9);
								 PRINT #10, STRING$(60, "-"); CHR$(10);
								 PRINT #10, "      ³Descripcion                        ³Cantidad³Precio Venta³ "; SPACE$(9);
								 PRINT #10, "³Descripcion                        ³Cantidad³Precio Venta³ "; CHR$(10);
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
														 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
														 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100)))))
												  ELSE
														 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
														 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)))))
												  END IF
											ELSE
												  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
												  auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(prec1(k))))
											END IF
									
											PRINT #10, SPACE$(9); "³"; desc1(k); "³";
											PRINT #10, USING "#######,³"; CVI(cant1(k));
											IF ivaf1(k) = "*" THEN
												  IF NuevoIva = "si" THEN
														 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
												  ELSE
														 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
												  END IF
											ELSE
												  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
											END IF
											PRINT #10, CHR$(10);
										ELSE
											PRINT #10, SPACE$(6); SPACE$(ler%); " \ \ \ \"; SPACE$(60 - ler% - 8); SPACE$(9);
											PRINT #10, SPACE$(ler%); " \ \ \ \"; CHR$(10); : ler% = ler% + 1
										END IF
								 NEXT k
								 PRINT #10, SPACE$(6); SPACE$(34); "VAN --> Bs "; : PRINT #10, USING "##########,.##"; CVD(auxe$); : PRINT #10, SPACE$(9);
								 PRINT #10, SPACE$(34); "VAN --> "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); CHR$(10);
								 PRINT #10, CHR$(12);
								 PRINT #10, USING "      Factura:######,"; CVL(fct$); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
								 PRINT #10, " Pagina:3 de 3"; SPACE$(9);
								 PRINT #10, USING "Factura:######,"; CVL(fct$); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
								 PRINT #10, " Pagina:3 de 3"; CHR$(10);
								 PRINT #10, "      Cliente:                               Vendedor:"; Cedu$; SPACE$(11);
								 PRINT #10, "Cliente:                               Vendedor:"; Cedu$; CHR$(10);
								 PRINT #10, "      "; nombre$; SPACE$(29);
								 PRINT #10, nombre$; CHR$(10);
								 rifo$ = Trim$(rifo$): nito$ = Trim$(nito$)
								 PRINT #10, "      R.I.F.:"; rifo$; SPACE$(22 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(23 - LEN(nito$)); SPACE$(9);
								 PRINT #10, "R.I.F.:"; rifo$; SPACE$(22 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(23 - LEN(nito$)); CHR$(10);
								 domicilio1$ = Trim$(domicilio1$): domicilio2$ = Trim$(domicilio2$)
								 PRINT #10, "      Domicilio Fiscal:"; SPACE$(52);
								 PRINT #10, "Domicilio Fiscal:"; CHR$(10);
								 PRINT #10, "      "; domicilio1$; SPACE$(60 - LEN(domicilio1$)); SPACE$(9);
								 PRINT #10, domicilio1$; CHR$(10);
								 PRINT #10, "      "; domicilio2$; SPACE$(60 - LEN(domicilio2$)); SPACE$(9);
								 PRINT #10, domicilio2$; CHR$(10);
								 PRINT #10, "      Telefonos:"; tlf$; SPACE$(20 - LEN(tlf$)); " Forma de Pago: Contado       "; SPACE$(9);
								 PRINT #10, "Telefonos:"; tlf$; SPACE$(20 - LEN(tlf$)); " Forma de Pago: Contado"; CHR$(10);
								 PRINT #10, "      "; STRING$(60, "-"); SPACE$(9);
								 PRINT #10, STRING$(60, "-"); CHR$(10);
								 PRINT #10, "      ³Descripcion                        ³Cantidad³Precio Venta³ "; SPACE$(9);
								 PRINT #10, "³Descripcion                        ³Cantidad³Precio Venta³ "; CHR$(10);
								 PRINT #10, SPACE$(6); SPACE$(35); "VIENEN->"; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$);
								 PRINT #10, SPACE$(9); SPACE$(35); "VIENEN->"; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$);
								 PRINT CHR$(10);
					END SELECT
					PRINT #10, "      "; SPACE$(45); STRING$(15, "="); SPACE$(9);
					PRINT #10, SPACE$(45); STRING$(15, "="); CHR$(10);
					PRINT #10, "      "; SPACE$(30); "Sub - Total: "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); SPACE$(9);
					PRINT #10, SPACE$(38); "Sub - Total: "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); : PRINT #10, CHR$(10);
					IF NuevoIva = "si" THEN
						  PRINT #10, "      "; SPACE$(30); "- Descuento: "; : PRINT #10, USING "##########,.## Bs"; (CVD(auxe$) * (1 - (CVD(neto) / CVD(total)))); : PRINT #10, SPACE$(9);
					ELSE
						  PRINT #10, "      "; SPACE$(30); "- Descuento: "; : PRINT #10, USING "##########,.## Bs"; (CVD(total) - CVD(neto)); : PRINT #10, SPACE$(9);
					END IF
					IF NuevoIva = "si" THEN
						  PRINT #10, SPACE$(30); "- Descuento: "; : PRINT #10, USING "##########,.## Bs"; (CVD(auxe$) * (1 - (CVD(neto) / CVD(total)))); : PRINT #10, CHR$(10);
					ELSE
						  PRINT #10, SPACE$(30); "- Descuento: "; : PRINT #10, USING "##########,.## Bs"; (CVD(total) - CVD(neto)); : PRINT #10, CHR$(10);
					END IF
					IF ShowPerc = "si" THEN
						  IF (100 - (100 * (CVD(neto) / CVD(total)))) > 0 THEN
								 PRINT #10, SPACE$(6); SPACE$(40); "%descuento:"; : PRINT #10, USING "###.##  %"; (100 - (100 * (CVD(neto) / CVD(total)))); : PRINT #10, SPACE$(9);
						  END IF
					END IF
					IF ShowPerc = "si" THEN
						  IF (100 - (100 * (CVD(neto) / CVD(total)))) > 0 THEN
								 PRINT #10, SPACE$(40); "%descuento:"; : PRINT #10, USING "###.##  %"; (100 - (100 * (CVD(neto) / CVD(total)))); : PRINT #10, CHR$(10);
						  END IF
					ELSE
						PRINT #10, CHR$(10);
					END IF
					IF NuevoIva = "si" THEN
						  PRINT #10, SPACE$(6); SPACE$(24); "+Impuesto("; impuesto; "%):";
						  PRINT #10, USING "##########,.## Bs"; CVD(neto) - (CVD(auxe$) * (CVD(neto) / CVD(total)));
						  PRINT #10, SPACE$(9);
					END IF
					IF NuevoIva = "si" THEN
						  PRINT #10, SPACE$(24); "+Impuesto("; impuesto; "%):";
						  PRINT #10, USING "##########,.## Bs"; CVD(neto) - (CVD(auxe$) * (CVD(neto) / CVD(total)));
						  PRINT #10, CHR$(10);
					ELSE
						  PRINT #10, CHR$(10);
					END IF
					PRINT #10, SPACE$(6); SPACE$(30); "Neto Factura:"; : PRINT #10, USING "##########,.## Bs"; CVD(neto); : PRINT #10, SPACE$(9);
					PRINT #10, SPACE$(30); "Neto Factura:"; : PRINT #10, USING "##########,.## Bs"; CVD(neto); : PRINT #10, CHR$(10);
					PRINT #10, "      "; STRING$(60, "-"); SPACE$(9);
					PRINT #10, STRING$(60, "-"); CHR$(10);
					PRINT #10, "      "; Mensaje1; SPACE$(9);
					PRINT #10, Mensaje1; CHR$(10);
					PRINT #10, "      "; Mensaje2; SPACE$(9);
					PRINT #10, Mensaje2; CHR$(10);
					PRINT #10, "      "; Mensaje3; SPACE$(9);
					PRINT #10, Mensaje3; CHR$(10);
					PRINT #10, CHR$(12); : CLOSE #10

END SUB

SUB ImprimePrevia
			CLOSE #10: OPEN "LPT1:" FOR OUTPUT AS #10
			WIDTH #10, 255: auxe$ = MKD$(0)
			PRINT #10, CHR$(27); CHR$(64);
			PRINT #10, CHR$(27); CHR$(64);
			PRINT #10, CHR$(27); CHR$(15);
			PRINT #10, "Fecha:"; MID$(DATE$, 4, 2); "/"; MID$(DATE$, 1, 2); "/"; MID$(DATE$, 7, 4); CHR$(10);
			PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
			PRINT #10, "Presupuesto de medicinas varias,"
			PRINT #10, "no v lido como factura"
			PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
			PRINT #10, STRING$(60, "=")
			FOR k = 1 TO (renglon - 1)
						PRINT #10, "="; desc1(k); "=";
						PRINT #10, USING "#######,="; CVI(cant1(k));
						PRINT #10, USING "#########,.##="; CVD(prec1(k));
						auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)))))
						PRINT #10, CHR$(10);
			NEXT k
			PRINT #10, STRING$(60, "=")
			PRINT #10, SPACE$(30); "TOTAL--->>";
			PRINT #10, USING "################,.##"; CVD(auxe$)
			PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
			PRINT #10, STRING$(30, "=")
			PRINT #10, "NETO--->>>";
			PRINT #10, USING "################,.##"; (CVD(auxe$) - CVD(DescGer))
			PRINT #10, STRING$(30, "=")
			PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
			PRINT #10, CHR$(12);
			CLOSE #10

END SUB

SUB LeeEstado
CLOSE : OPEN "F:\kevinprg\kevindat\kaos.dat" FOR INPUT SHARED AS #2
INPUT #2, sonido: INPUT #2, impuesto: INPUT #2, NombFarma: INPUT #2, Direccion: INPUT #2, Rif: INPUT #2, Nit: INPUT #2, telefono: INPUT #2, contri: INPUT #2, contra: INPUT #2, Stock: INPUT #2, Blanq
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
MeteDatoLetra$ = eleccion$: IF sonido = "si" THEN BEEP
EXIT FUNCTION
Sumar:
FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR 0, 15: LOCATE fila%, columna%: PRINT "²"; eleccion$; "²"
RETURN
END FUNCTION

SUB Telefonos
DIM Criterio(7) AS STRING * 60: salir = false: DIM Prud1 AS STRING * 4, Prud2 AS STRING * 4, mes1 AS STRING * 7, mes2 AS STRING * 7
Criterio(1) = "FARMACIAS: tel‚fonos de la zona de Valencia y Naguanagua.  "
Criterio(2) = "EMERGENCIAS: polic¡a, EMI, bomberos, etc.                  "
Criterio(3) = "BANCOS: conformaci¢n de cheques y contactos en agencia.    "
Criterio(4) = "EMPRESAS VARIAS: Hidrocentro, Imgeve y por el estilo (etc.)"
Criterio(5) = "PERSONALES: personas naturales, amigos, etc.               "
Criterio(6) = "CANTV: UN1CA  , saldos, informaci¢n, taquillas de paso,etc."
Criterio(7) = "Salir al men£ de ventas.                                   "
PopUp 2, 9, 12, 62, 112, 1, 0, 0
Center 3, "*** CARTELERA INFORMATIVA ***"
Center 4, "Escoja que opci¢n desea realizar."
Center 5, STRING$(40, "-")
CALL Center(25, "[F1]Ayuda -Use las teclas de direcci¢n y Enter para escojer [F9]Abortar")
resal% = 1: salir = false
DO WHILE salir = false
	  FOR k = 1 TO 7
			 IF resal% = k THEN COLOR colors(7, colorpref), colors(4, colorpref)
			 LOCATE 5 + k, 10: PRINT Criterio(k)
			 IF resal% = k THEN COLOR colors(4, colorpref), colors(7, colorpref)
	  NEXT k
	  key$ = "": DO WHILE key$ = "": key$ = INKEY$: LOOP
	  IF key$ = CHR$(0) + "H" THEN resal% = resal% - 1: IF resal% < 1 THEN resal% = 7
	  IF key$ = CHR$(0) + "P" THEN resal% = resal% + 1: IF resal% > 7 THEN resal% = 1
	  IF key$ = CHR$(0) + CHR$(67) THEN EXIT SUB
	  IF key$ = CHR$(27) THEN EXIT SUB
	  IF key$ = CHR$(13) THEN salir = true
LOOP
SELECT CASE resal%
	CASE 1: HelpMate 1, "telefono"
	CASE 2: HelpMate 1, "emergenc"
	CASE 3: HelpMate 1, "bancos"
	CASE 4: HelpMate 1, "empresas"
	CASE 5: HelpMate 1, "personas"
	CASE 6: HelpMate 1, "cantv"
	CASE 7: EXIT SUB
END SELECT
END SUB

SUB Toma
Center 25, " Escoja una tarjeta con enter o Escape para abortar"
CLOSE #100: CLOSE #101: CLOSE #104
OPEN "f:\kevinprg\tarjetas\tarjetas.dbf" FOR RANDOM AS #100 LEN = 32
OPEN "f:\kevinprg\tarjetas\vivos.idx" FOR RANDOM AS #101 LEN = 19
OPEN "f:\kevinprg\tarjetas\cuenta.dbf" FOR RANDOM AS #104 LEN = 59
max% = LOF(101) / 19
IF max% = 0 THEN
	CLOSE #100: CLOSE #101: CLOSE #104
	PopUp 12, 27, 2, 30, 31, 1, 1, 0: Center 13, "No hay tarjetas creadas": Espera: CALL ShutUp: EXIT SUB
END IF
DIM nombre(max%) AS STRING * 15, siti(max%) AS SINGLE, pvp(max%) AS LONG
l% = 1
FOR k% = 1 TO max%
	GET #101, k%, vivos
	GET #100, vivos.sit, tarje
	IF tarje.cant > 0 THEN
		nombre(l%) = vivos.nomb: pvp(l%) = tarje.pvp
		siti(l%) = vivos.sit: l% = l% + 1
	END IF
NEXT k%
IF l% - 1 = 0 THEN
	CLOSE #100: CLOSE #101: CLOSE #104
	PopUp 12, 25, 2, 32, 31, 1, 1, 0: Center 13, "No hay tarjetas de ning£n tipo": Espera: CALL ShutUp: EXIT SUB
END IF
max% = l% - 1: resal% = 1: up% = 1
PopUp 7, 27, 12, 26, 31, 1, 1, 0
GOSUB dibuja
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
		CASE CHR$(27): CLOSE #100: CLOSE #101: CLOSE #104: CALL ShutUp: EXIT SUB
		CASE ELSE:  BEEP
	END SELECT
	GOSUB dibuja
LOOP
g% = siti(up% + resal% - 1)
	max% = LOF(104) / 59
	GET #104, max%, cuenta
	cuenta.cant = 1
	GET #100, g%, tarje
	tarje.cant = tarje.cant - 1
	PUT #100, g%, tarje
	cuenta.nota = SPACE$(10)
	cuenta.oper = "9"
	cuenta.fech = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4)
	cuenta.hora = MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MID$(TIME$, 7, 2)
	cuenta.sit = g%
	cuenta.debe = 0
	cuenta.haber = 0
	PUT #104, (max% + 1), cuenta
gav$ = "com1": OPEN gav$ FOR RANDOM AS #27: PRINT #27, "A": PRINT #27, "A": PRINT #27, "A": CLOSE #27
CLOSE #100: CLOSE #101: CLOSE #104
CALL ShutUp
EXIT SUB

dibuja:
FOR k% = 1 TO 10
	IF k% <= max% THEN
		LOCATE 7 + k%, 28
		IF (k% + up% - 1) <= max% THEN
			PRINT nombre(k% + up% - 1); "  ";
			PRINT USING "######,"; pvp(k% + up% - 1);
		ELSE
			LOCATE 7 + k%, 28: PRINT SPACE$(24);
		END IF
	ELSE
		LOCATE 7 + k%, 28: PRINT SPACE$(24);
	END IF
NEXT k%
BackFill 8, 28, 10, 24, 31
BackFill 7 + resal%, 28, 1, 24, 25
RETURN

END SUB

FUNCTION Trim$ (x$)
IF x$ = "" THEN
	Trim$ = ""
ELSE
	lastChar = 0
	FOR a = 1 TO LEN(x$)
		y$ = MID$(x$, a, 1)
		IF y$ <> CHR$(0) AND y$ <> " " THEN lastChar = a
	NEXT a: Trim$ = LEFT$(x$, lastChar)
END IF
END FUNCTION

SUB VentaContado
COLOR colors(7, colorpref), colors(4, colorpref): CLS
DO
	FOR k = 1 TO 17: Fun(k) = "no": NEXT k
	DO 'Pantalla:
		FOR k = 1 TO 12: Fun(k) = "no": NEXT k
		seguir = true: devolver = false: llave$ = "": keya = "      "
		COLOR colors(7, colorpref), colors(4, colorpref): CLS : CALL Box(1, 1, 4, 80): CALL Center(2, "Farmacia:" + NombFarma + " Fecha:" + MID$(DATE$, 4, 2) + "/" + MID$(DATE$, 1, 2) + "/" + MID$(DATE$, 7, 4)): CALL Center(3,  _
"*** V E N T A S   D E   C O N T A D O ***")
		CALL Box(5, 10, 7, 70): CALL Center(6, " Introduzca Clave de Vendedor:²   ²"): Box 21, 10, 23, 70: CALL Center(22, " Consultas SIN Abrir Factura ")
		Box2 8, 5, 20, 75: Center 8, " Caja de Tareas "
		CALL Center(24, "[F1]Ayuda [F2]Laboratorio [F3]PorCodigo [F4]Oferta [F5]Compos. [F7]PorNombre"): CALL Center(25, "[Esc]Menu Principal [F10]Tarjetas tlf. [F11]Vademecum  [F12]Telefonos")
		llevasitio% = 1: COLOR colors(7, colorpref), colors(4, colorpref): REDIM eleccion(0 TO 4) AS STRING * 1: seguir = true: eleccion$ = ""
		llave$ = "": lastTime# = TIMER
		DO
			llave$ = ""
			DO WHILE llave$ = ""
				llave$ = INKEY$: LOCATE 2, 68
				IF VAL(MID$(TIME$, 1, 2)) >= 12 THEN
					tim$ = STR$((VAL(MID$(TIME$, 1, 2)) - 12)) + MID$(TIME$, 3, 6) + "p.m.": PRINT tim$
				ELSE
					PRINT TIME$; "a.m."
				END IF
				NewTime# = TIMER: IF (NewTime# - lastTime#) > VAL(Blanq) THEN GOSUB Triangul
			LOOP
			SELECT CASE llave$
				CASE CHR$(27):  EXIT SUB
				CASE CHR$(0) + CHR$(59)
				CASE CHR$(0) + CHR$(60): GOSUB Limpia: GOSUB BuscaLab: eleccion$ = "": EXIT DO
				CASE CHR$(0) + CHR$(61): GOSUB Limpia: GOSUB BuscaCodigo: eleccion$ = "": EXIT DO
				CASE CHR$(0) + CHR$(62): GOSUB Limpia: GOSUB BuscaOfer: eleccion$ = "": EXIT DO
				CASE CHR$(0) + CHR$(63): GOSUB Limpia: GOSUB BuscaComp: eleccion$ = "": EXIT DO
				CASE CHR$(0) + CHR$(65): GOSUB Limpia: GOSUB BuscaOrden: eleccion$ = "": EXIT DO
				CASE CHR$(0) + CHR$(68): CALL Toma: CALL Center(25, "[Esc]Menu Principal [F10]Tarjetas tlf. [F11]Vademecum  [F12]Telefonos")
				CASE CHR$(0) + CHR$(133)
					  
						PopUp 12, 10, 2, 60, 112, 1, 0, 0
						LOCATE 13, 11: PRINT "Introduzca el nombre a buscar:"
						auxi$ = MeteDatoLetra$(13, 50, 8, SPACE$(8), "0")
						auxi$ = Trim$(auxi$)
						CALL ShutUp
						CALL HelpMate(1, auxi$)
				CASE CHR$(0) + CHR$(134): CALL Telefonos: CALL ShutUp
				CASE ELSE: eleccion$ = eleccion$ + llave$: LOCATE 6, 54 + LEN(eleccion$): PRINT "²"
				IF LEN(eleccion$) >= 3 THEN EXIT DO
			END SELECT
		LOOP
		IF sonido = "si" THEN BEEP
		IF LEN(Trim$(eleccion$)) = 3 THEN
			COLOR colors(7, colorpref), colors(4, colorpref): elegsal% = 0
			CLOSE : OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #1 LEN = 100: FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS Cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
			nursaler = LOF(1) / 100: ClaveLarga$ = "": si$ = " "
			FOR k = 1 TO nursaler: GET #1, k
			IF eleccion$ = clco$ AND acti$ = " " AND inout$ = " " THEN
				ClaveLarga$ = clla$: gav$ = gaveta$: Cedula$ = Cedu$: RSET inout$ = "*": IF cong$ = "*" THEN si$ = "*": RSET cong$ = " "
				PUT #1, k
				IF CVL(Cedula$) = 11846659 THEN Box2 8, 5, 23, 75: CALL Center(8, "LUIS RAMON , ORDENA LOS BILLETES POR FAVOR"): CALL Espere(10, 10)
				EXIT FOR
			END IF
			NEXT k: CLOSE
			IF ClaveLarga$ = "" THEN Box2 8, 5, 16, 75: CALL Center(8, "No Hay Ningun Vendedor Con Esa Clave o Esta Fuera El Vendedor"): GOSUB BuscaOrden ELSE EXIT DO
		END IF: eleccion$ = ""
	LOOP
	COLOR colors(7, colorpref), colors(4, colorpref): CLS
	LOCATE 1, 1: PRINT "Vendedor(a):"; ClaveLarga$: CALL Box(2, 1, 16, 80)
	salir = false: costo = MKD$(0): neto = MKD$(0): total = MKD$(0): OtroDescGer$ = MKD$(0): DescGer = MKD$(0): renglon = 1
	fx = "F:\kevinprg\kevindat\" + MID$(STR$(CVL(Cedula$)), 2, LEN(STR$(CVL(Cedula$)))) + ".caj"
	fx2 = "F:\kevinprg\kevindat\" + MID$(STR$(CVL(Cedula$)), 2, LEN(STR$(CVL(Cedula$)))) + ".prv"
	IF si$ = " " THEN
		OPEN "F:\kevinprg\kevindat\maxfact.dat" FOR RANDOM AS #7: FIELD #7, 4 AS factura$
		GET #7, 1: numfct$ = factura$: fct = numfct$: numfct$ = MKL$(CVL(numfct$) + 1)
		IF CVL(numfct$) > 999999 THEN numfct$ = MKL$(1)
		LSET factura$ = numfct$: PUT #7, 1: CLOSE #7
		OPEN fx2 FOR RANDOM SHARED AS #17 LEN = 63: FIELD #17, 35 AS descri$, 2 AS cantid$, 8 AS precio$, 8 AS costid$, 1 AS isvcm$, 6 AS codito$, 2 AS regist1$, 1 AS regist2$
		RSET descri$ = fct + DescGer + MKI$(renglon) + SPACE$(21): PUT #17, 1
		FOR k = 2 TO 27
		RSET descri$ = desc1(k - 1): RSET cantid$ = cant1(k - 1): RSET precio$ = prec1(k - 1): RSET costid$ = cost1(k - 1)
		RSET isvcm$ = ivaf1(k - 1): RSET codito$ = code1(k - 1): RSET regist1$ = regi1(k - 1): RSET regist2$ = regi2(k - 1): PUT #17, k
		NEXT k: CLOSE
	ELSE
		OPEN fx2 FOR RANDOM SHARED AS #17 LEN = 63: FIELD #17, 35 AS descri$, 2 AS cantid$, 8 AS precio$, 8 AS costid$, 1 AS isvcm$, 6 AS codito$, 2 AS regist1$, 1 AS regist2$
		GET #17, 1: fct = MID$(descri$, 1, 4): DescGer = MID$(descri$, 5, 8): renglon = CVI(MID$(descri$, 13, 2))
		FOR k = 2 TO 27: GET #17, k
		desc1(k - 1) = descri$: cant1(k - 1) = cantid$: prec1(k - 1) = precio$: cost1(k - 1) = costid$
		ivaf1(k - 1) = isvcm$: code1(k - 1) = codito$: regi1(k - 1) = regist1$: regi2(k - 1) = regist2$
		total = MKD$((CVD(total)) + (CVD(precio$) * CVI(cantid$))): costo = MKD$(CVD(costo) + (CVD(costid$) * CVI(cantid$)))
		NEXT k: CLOSE
	END IF
	LOCATE 1, 44: PRINT "Venta de Contado,n£mero :²"; : PRINT USING "**####,"; CVL(fct); : PRINT "²": Fun(17) = "op"
	'AdicProd: ------------------===============---------------------------------
	DO
		IF Fun(17) = "no" AND renglon > 26 THEN
			DO
			GOSUB Limpio: Fun(9) = "si": GOSUB Cierre: IF salir = true THEN EXIT DO
			LOOP: salir = false: EXIT DO
		END IF
		IF Fun(17) = "no" THEN EXIT DO
		CALL Center(25, "1]Ayuda 2]Lab 3]Cod 4]Prev 5]Com 6]Cambio 7]Lim 8]SIN 9]CON 10]COM 11]CHEQ"): COLOR colors(7, colorpref), colors(4, colorpref)
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
		IF VAL(desc) > 0 THEN DescGer = MKD$(CVD(total) * VAL(desc) / 100)
		IF CVD(DescGer) > 0 THEN LOCATE 2, 3: PRINT "Sub-total Neto:"; : PRINT USING "#########,.## Bs"; (CVD(total) - CVD(DescGer)); : PRINT USING " (##.##%)"; (100 - (100 * ((CVD(total) - CVD(DescGer)) / CVD(total))));
		COLOR colors(7, colorpref), colors(4, colorpref)
			llave$ = "": DO WHILE llave$ = "": llave$ = INKEY$: LOOP
			SELECT CASE llave$
				CASE CHR$(0) + CHR$(59)
				CASE CHR$(0) + CHR$(60): GOSUB Limpia: GOSUB BuscaLab
				CASE CHR$(0) + CHR$(61): GOSUB Limpio: GOSUB BuscaCodigo
				CASE CHR$(0) + CHR$(62): GOSUB ImprimePrevia
				CASE CHR$(0) + CHR$(63): GOSUB Limpia: GOSUB BuscaComp
				CASE CHR$(0) + CHR$(64): GOSUB Cambiar
				CASE CHR$(0) + CHR$(65): GOSUB Limpio: CALL Anula
				CASE CHR$(0) + CHR$(66): GOSUB Limpio: Fun(8) = "si": GOSUB Cierre   'sin descuento  F8
				CASE CHR$(0) + CHR$(67): GOSUB Limpio: Fun(9) = "si": GOSUB Cierre   'con descuento  F9
				CASE CHR$(0) + CHR$(68): GOSUB Limpio: Fun(10) = "si": GOSUB Cierre  'completo       F10
				CASE CHR$(0) + CHR$(133): GOSUB Limpio: Fun(9) = "si": Fun(11) = "si": GOSUB Cierre ' con cheque y descuento
				CASE CHR$(0) + CHR$(134): GOSUB Limpio: Center 20, "Congelando Factura": Fun(12) = "si": GOSUB GrabaPrv
				CASE ELSE: keya = llave$: GOSUB BuscaOrden
			END SELECT
	LOOP
LOOP
'   GOTO Pantalla  '-------------------===============--------------------------

BuscaLab:
IF Fun(17) = "op" THEN
	COLOR colors(7, colorpref) + 16, colors(4, colorpref): LOCATE 2, 45
	PRINT "** SUB - TOTAL ="; : PRINT USING "########,.##"; CVD(total); : PRINT " Bs. **"
	IF VAL(desc) > 0 THEN DescGer = MKD$(CVD(total) * VAL(desc) / 100)
	IF CVD(DescGer) > 0 THEN LOCATE 2, 3: PRINT "Sub-total Neto:"; : PRINT USING "#########,.## Bs"; (CVD(total) - CVD(DescGer)); : PRINT USING " (##.##%)"; (100 - (100 * ((CVD(total) - CVD(DescGer)) / CVD(total))));
	COLOR colors(7, colorpref), colors(4, colorpref)
END IF
LOCATE 14, 2: PRINT " *** Espere , Por Favor ... ***";
CALL Box(13, 1, 15, 48): LOCATE 14, 2: PRINT "Elija el Laboratorio:²   ²";
mjfox% = Elije%(2, 6, 50, 14, 24, 0): IF mjfox% < 1 THEN GOSUB Limpio: RETURN
gh$ = MKI$(VAL(Yname1(mjfox%))): GOSUB Limpia
CALL Box(13, 1, 15, 48): LOCATE 14, 2: PRINT "Laboratorio Elegido:²"; Yname1(mjfox%); "²"; Yname2(mjfox%)
mjfox% = Escoge%(19, 2, 17, gh$): IF mjfox% < 1 THEN GOSUB Limpio: RETURN
IF Fun(17) = "op" THEN Fun(3) = "si": GOSUB EscPvp ELSE GOSUB Mostrar
RETURN

BuscaOfer:
IF Fun(17) = "op" THEN
	COLOR colors(7, colorpref) + 16, colors(4, colorpref): LOCATE 2, 45
	PRINT "**  SUB - TOTAL ="; : PRINT USING "########,.##"; CVD(total); : PRINT " Bs. **"
	IF VAL(desc) > 0 THEN DescGer = MKD$(CVD(total) * VAL(desc) / 100)
	IF CVD(DescGer) > 0 THEN LOCATE 2, 3: PRINT "Sub-total Neto:"; : PRINT USING "#########,.## Bs"; (CVD(total) - CVD(DescGer)); : PRINT USING " (##.##%)"; (100 - (100 * ((CVD(total) - CVD(DescGer)) / CVD(total))));
	COLOR colors(7, colorpref), colors(4, colorpref)
END IF
CALL Center(25, "[ 1.-" + Prov1 + "] [ 2.-" + Prov2 + "] [ 3.-" + Prov3 + "]")
CALL Box(13, 13, 15, 72): CALL Center(14, "Introduzca la Oferta 1,2,3 o 4 para ir a Menu Anterior:"): nim$ = "0"
DO WHILE VAL(nim$) < 1 OR VAL(nim$) > 4: nim$ = MeteDatoLetra$(14, 69, 1, " ", "0"): LOOP
IF nim$ = "4" THEN GOSUB Limpio: RETURN
mjfox% = Escoge%(19, 4, 17, nim$): IF mjfox% < 1 THEN GOSUB Limpio: RETURN
IF Fun(17) = "op" THEN GOSUB EscPvp ELSE GOSUB Mostrar
RETURN

BuscaComp:
IF Fun(17) = "op" THEN
	COLOR colors(7, colorpref) + 16, colors(4, colorpref): LOCATE 2, 45
	PRINT "** SUB - TOTAL ="; : PRINT USING "########,.##"; CVD(total); : PRINT " Bs. **"
	IF VAL(desc) > 0 THEN DescGer = MKD$(CVD(total) * VAL(desc) / 100)
	IF CVD(DescGer) > 0 THEN LOCATE 2, 3: PRINT "Sub-total Neto:"; : PRINT USING "#########,.## Bs"; (CVD(total) - CVD(DescGer)); : PRINT USING " (##.##%)"; (100 - (100 * ((CVD(total) - CVD(DescGer)) / CVD(total))));
	COLOR colors(7, colorpref), colors(4, colorpref)
END IF
LOCATE 14, 2: PRINT " *** Espere , Por Favor ... ***";
CALL Box(13, 1, 15, 48): LOCATE 14, 2: PRINT "Elija el  Componente:²   ²";
mjfox% = Elije%(4, 6, 50, 14, 24, 0): IF mjfox% < 1 THEN GOSUB Limpio: RETURN
gh$ = MKI$(VAL(Yname1(mjfox%))): mjfox% = Escoge%(19, 5, 17, gh$): IF mjfox% < 1 THEN GOSUB Limpio: RETURN
IF Fun(17) = "op" THEN GOSUB EscPvp ELSE GOSUB Mostrar
RETURN

BuscaOrden:
Fun(9) = "no": CALL Center(17, SPACE$(80)): IF Fun(17) <> "op" THEN keya = eleccion$
mjfox% = Escoge%(19, 1, 17, keya)
IF mjfox% < 1 THEN devolver = false: GOSUB Limpio: RETURN
GOSUB Mostrar
IF Fun(17) = "op" THEN GOSUB EscPvp
RETURN

BuscaCodigo:
CALL Center(17, SPACE$(80))
mjfox% = Escoge%(19, 6, 17, "      ")
IF mjfox% < 1 THEN devolver = false: GOSUB Limpio: RETURN
GOSUB Mostrar
IF Fun(17) = "op" THEN GOSUB EscPvp
RETURN

Cierre: '       CIERRE-----CIERRE------CIERRE-----
CALL Box2(17, 15, 24, 65): LOCATE 21, 39: PRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
IF CVD(total) = 0 THEN Center 18, "Debe Tener Articulos Vendidos Para Poder Cerrar Factura": Espera: GOSUB Limpio: RETURN
LOCATE 18, 20: PRINT "Bruto a Pagar  Bs.:"; : PRINT USING "#########,.##"; CVD(total)
	IF Fun(8) = "si" THEN    'sin descuento
			OtroDescGer$ = DescGer: DescGer = MKD$(0)
			IF VAL(desc) > 0 THEN
				neto = MKD$(CVD(total) * ((100 - VAL(desc)) / 100)): DescGer = MKD$(CVD(total) - CVD(neto))
			ELSE
				neto = total
			END IF
	END IF
	IF Fun(9) = "si" THEN    'con descuento
			CALL Center(25, "Introduzca el Descuento en Bs. y si es por Porcentaje Agregue el Signo `%'")
			LOCATE 19, 20: PRINT "Descuento en Bs ¢ %²             ²": salir = false
			DO WHILE NOT salir
			LOCATE 19, 20: PRINT "Descuento en Bs ¢ %"
			entrega = EntraDatoNumero(19, 39, 13, STR$((FIX(CVD(DescGer) * 100)) / 100), 0)
			IF devolver = true THEN devolver = false: GOSUB Limpio: Fun(8) = "no": Fun(9) = "no": Fun(10) = "no": Fun(11) = "no": RETURN
			llevas$ = "": IF Fun(11) = "si" THEN Fun(9) = "no"
			FOR k = 1 TO 13
				IF MID$(entrega, k, 1) = "%" THEN
				entrega = MID$(entrega, 1, k - 1) + MID$(entrega, k + 1, 13 - k): llevas$ = "si"
				END IF
			NEXT k
			IF llevas$ = "si" THEN
				IF VAL(entrega) > VAL(DescMax) THEN Center 23, "El Descuento DEBE Ser MENOR del " + DescMax + " %": Espera: Center 23, SPACE$(40) ELSE salir = true
			ELSE
				entrega = STR$(VAL(entrega) - CVD(DescGer))
				IF VAL(entrega) > 0 THEN
				neto = MKD$(CVD(total) - CVD(DescGer))
				entri$ = MKD$(100 - ((CVD(neto) - VAL(entrega)) * 100) / (CVD(total) - CVD(DescGer))): LOCATE 19, 40: PRINT USING "#######,.## %"; CVD(entri$): LOCATE 19, 20: PRINT "Descuento Actual : "
				IF CVD(entri$) > VAL(DescMax) THEN
					Center 23, "El Descuento DEBE Ser MENOR del " + DescMax + " %": Espera: Center 23, SPACE$(40)
				ELSE
					neto = MKD$(CVD(neto) - VAL(entrega)): entre$ = MKD$(100 - (CVD(neto) * 100) / (CVD(total))): salir = true
				END IF
				END IF
				IF VAL(entrega) <= 0 THEN
				entrega = STR$(VAL(entrega) + CVD(DescGer))
				neto = MKD$(CVD(total) - VAL(entrega)): salir = true
				entre$ = MKD$(100 - (CVD(neto) * 100) / (CVD(total))): LOCATE 19, 40: PRINT USING "#######,.## %"; CVD(entre$): LOCATE 19, 20: PRINT "Descuento Actual : "
				END IF
			END IF
			LOOP: salir = false
			IF llevas$ = "si" THEN
				DO WHILE NOT salir
				CALL Center(25, "Introduzca el monto deseado o [Enter] si est  Conforme")
				llevas$ = ""
				neto = MKD$(CVD(total) * (1 - (VAL(entrega) / 100)))
				LOCATE 20, 20: PRINT "Desea Redondear  ?"
				neto = MKD$(VAL(EntraDatoNumero(20, 39, 13, Trim$(STR$(FIX(CVD(neto)))), 0)))
				IF devolver = true THEN devolver = false: GOSUB Limpio: Fun(8) = "no": Fun(9) = "no": Fun(10) = "no": : RETURN
				IF VAL(Redondeo) >= 0 THEN
					IF CVD(neto) > 100 THEN
						neto = MKD$(VAL(Redondeo) * (FIX(CVD(neto) / VAL(Redondeo))))
					END IF
				END IF
				entre$ = MKD$(100 - (CVD(neto) * 100) / CVD(total)): LOCATE 19, 40: PRINT USING "#######,.## %"; CVD(entre$): LOCATE 19, 20: PRINT "Descuento Actual : "
				IF CVD(entre$) > VAL(DescMax) THEN
					Center 23, "El Descuento DEBE Ser MENOR del " + DescMax + " %": Espera: Center 23, SPACE$(40)
				ELSE
					IF CVD(neto) > CVD(total) THEN
					Center 23, "El Neto NO Debe Ser MAYOR Al Total .": Espera: Center 23, SPACE$(40)
					ELSE
					salir = true
					END IF
				END IF
				LOCATE 20, 16: PRINT SPACE$(49)
				LOOP
			END IF
	END IF
	IF Fun(10) = "si" THEN   'completo
			IF VAL(desc) > 0 THEN
				neto = MKD$(CVD(total) * ((100 - VAL(desc)) / 100)): DescGer = MKD$(CVD(total) - CVD(neto))
			ELSE
				neto = MKD$(CVD(total) - CVD(DescGer))
			END IF
	END IF
IF VAL(Redondeo) > 0 THEN
	IF CVD(neto) > 100 THEN
		neto = MKD$(VAL(Redondeo) * (FIX(CVD(neto) / VAL(Redondeo))))
	END IF
END IF
'anexa:
		LOCATE 19, 20: PRINT "Descuento (en Bs.):"; : PRINT USING "#########,.##  "; (CVD(total) - CVD(neto))
		'LOCATE 1, 1: PRINT USING "#######,.##"; CVD(neto); CVD(total)
		entre$ = MKD$(100 - (CVD(neto) * 100) / CVD(total)): IF CVD(DescGer) > 0 THEN LOCATE 20, 55: PRINT "(Gerencia)"
		LOCATE 20, 20: PRINT "Descuento ( en % ):"; : PRINT USING "#########,.## %"; CVD(entre$)
		LOCATE 22, 20: PRINT "Neto  a Pagar Bs. :"; : PRINT USING "#########,.##  "; CVD(neto)
'paga:
		IF Fun(8) = "si" OR Fun(9) = "si" THEN
			LOCATE 23, 20: PRINT "El cliente Entrega? ": salir = false
			CALL Center(25, "Introduzca el Monto que Entrega el Cliente")
			DO WHILE NOT salir
			entrega = EntraDatoNumero(23, 40, 13, " _           ", 0)
			IF devolver = true THEN
				devolver = false: Fun(8) = "no": Fun(9) = "no": Fun(10) = "no": Fun(11) = "no": salir = false
				IF CVD(OtroDescGer$) > 0 THEN DescGer = OtroDescGer$: OtroDescGer$ = MKD$(0)
				GOSUB Limpio: RETURN
			END IF
			IF VAL(entrega) >= CVD(neto) THEN salir = true
			LOOP
			entre$ = MKD$(VAL(entrega) - CVD(neto))
			LOCATE 24, 20: PRINT "Su vuelto:-->>>-->"; : PRINT USING "#########,.##"; CVD(entre$); : PRINT " <<< Bs.  ";
		ELSE
			IF Fun(11) = "si" THEN
			salir = false: Center 25, "Introduzca Banco,Numero y Monto de Cheque y/o Efectivo"
			DO WHILE NOT salir
			CALL Box2(17, 15, 24, 65): LOCATE 21, 20: PRINT "Numero Cheque:": LOCATE 22, 20: PRINT "Monto Cheque:": LOCATE 23, 20: PRINT "Efectivo:"
			LOCATE 19, 20: PRINT "Neto  a Pagar Bs. :"; : PRINT USING "#########,.##  "; CVD(neto)
			GOSUB BuscaBank
			IF devolver = true THEN
				devolver = false: Fun(8) = "no": Fun(9) = "no": Fun(10) = "no": Fun(11) = "no": salir = false
				IF CVD(OtroDescGer$) > 0 THEN DescGer = OtroDescGer$: OtroDescGer$ = MKD$(0)
				GOSUB Limpio: RETURN
			END IF
			numcheq = MKD$(VAL(EntraDatoNumero(21, 40, 13, " _           ", 0)))
			moncheq = MKD$(VAL(EntraDatoNumero(22, 40, 13, " _           ", 0)))
			entrega = EntraDatoNumero(23, 40, 13, " _           ", 0)
			IF devolver = true THEN
				devolver = false: Fun(8) = "no": Fun(9) = "no": Fun(10) = "no": Fun(11) = "no": salir = false
				IF CVD(OtroDescGer$) > 0 THEN DescGer = OtroDescGer$: OtroDescGer$ = MKD$(0)
				GOSUB Limpio: RETURN
			END IF
			IF (VAL(entrega) + CVD(moncheq)) >= CVD(neto) AND CVD(numcheq) > 0 THEN salir = true
			LOOP
			entre$ = MKD$(VAL(entrega) + CVD(moncheq) - CVD(neto))
			LOCATE 24, 20: PRINT "Su vuelto:-->>>-->"; : PRINT USING "#########,.##"; CVD(entre$); : PRINT " <<< Bs.  ";
			OPEN "f:\kevinprg\kevindat\pordepot.dat" FOR RANDOM SHARED AS #17 LEN = 27: FIELD #17, 1 AS vive$, 8 AS fech$, 8 AS mon$, 8 AS numc$, 2 AS ban$: max3% = 1 + LOF(17) / 27
			RSET fech$ = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4): RSET vive$ = " ": RSET mon$ = moncheq: RSET numc$ = numcheq: RSET ban$ = numbank
			PUT #17, max3%: CLOSE #17
			ELSE
				LOCATE 23, 20: PRINT "Cliente Entreg¢ Bs"; : PRINT USING "#########,.##"; CVD(neto);
				LOCATE 24, 20: PRINT "Su vuelto:-->>>-->         0.00 <<< Bs.  ";
			END IF
		END IF
'revisa:
IF CVD(neto) < CVD(costo) THEN
COLOR colors(7, colorpref) + 16, colors(4, colorpref): CALL Box2(17, 15, 24, 65): Center 17, "Error Administrativo"
COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 19, 20: PRINT "Neto de Factura   :"; : PRINT USING "#########,.## Bs."; CVD(neto)
LOCATE 21, 20: PRINT "Costo de Factura  :"; : PRINT USING "#########,.## Bs."; CVD(costo)
Center 23, "El Proceso de Cierre Se REPETIRA": Espera: devolver = false: Fun(8) = "no": Fun(9) = "no": Fun(10) = "no": GOSUB Limpio: RETURN
END IF
'abre: e imprime ojo
gav$ = "com" + gav$: OPEN gav$ FOR RANDOM AS #27: PRINT #27, "A": PRINT #27, "A": PRINT #27, "A": CLOSE #27: Fun(9) = "no": nombre$ = SPACE$(40): domicilio1$ = SPACE$(60): rifo$ = SPACE$(20)
nito$ = SPACE$(20): domicilio2$ = SPACE$(60): tlf$ = SPACE$(20): salir = false: sit% = 1: GOSUB HaceCaja
TIMER ON
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
END SELECT: IF LEN(Trim$(nombre$)) > 0 THEN imprint$ = "*" ELSE imprint$ = " "
IF Fun(2) = "si" THEN Fun(2) = "no": GOSUB BuscaCliente: GOSUB HaceCaja
IF Fun(9) = "si" THEN
	Fun(9) = "no": salir = true
	IF imprint$ = "*" THEN
		COLOR colors(7, colorpref), colors(4, colorpref): Box2 10, 8, 20, 72
		Center 15, "Desea Imprimir Factura? Pulse -S- para IMPRIMIR"
		llave$ = "": DO WHILE llave$ = "": llave$ = INKEY$: LOOP: llave$ = UCASE$(llave$)
		IF llave$ = "S" THEN salir = true: Impresion = true ELSE salir = false: GOSUB HaceCaja
	END IF
END IF
LOOP: CLOSE : TIMER OFF
		OPEN fx FOR RANDOM SHARED AS #17 LEN = 8: FIELD #17, 8 AS caja$: GET #17, 1: RSET caja$ = MKD$(CVD(caja$) + CVD(neto)): PUT #17, 1: CLOSE #17
		fx = "": fx = "F:\kevinprg\kevinvnt\" + MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4) + ".vnt"
		OPEN fx FOR RANDOM AS #17 LEN = 30: FIELD #17, 30 AS todo$: RSET todo$ = SPACE$(30): PUT #17, 1: CLOSE #17
		OPEN fx FOR RANDOM AS #17 LEN = 30: FIELD #17, 30 AS todo$: nursaler = LOF(17) / 30: nursaler = nursaler + 1
		RSET todo$ = "J" + MKI$(renglon - 1) + imprint$ + neto + MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MID$(TIME$, 7, 2) + Cedula$ + fct + MID$(DescGer, 1, 4)
		PUT #17, nursaler: salir = false: newiva = MKL$(0)  ': LOCATE 1, 1: PRINT nursaler; "   "; renglon: Espera
		FOR k = 1 TO renglon - 1
			RSET todo$ = ivaf1(k) + code1(k) + cant1(k) + prec1(k) + cost1(k) + psic1(k) + MID$(DescGer, 5, 4)
			IF psic1(k) = "*" THEN salir = true
			PUT #17, (nursaler + k)   ': LOCATE 1, 1: PRINT todo$: Espera
		NEXT k: CLOSE  ': LOCATE 1, 1: PRINT nursaler: Espera
		IF salir = true THEN
			OPEN "F:\kevinprg\kevindat\movpsico.dat" FOR RANDOM AS #1 LEN = 40: FIELD #1, 40 AS todo$: max% = LOF(1) / 40: max% = max% + 1
			FOR k = 1 TO renglon - 1
			IF psic1(k) = "*" THEN RSET todo$ = "VENTA " + fct + DATE$ + TIME$ + code1$(k) + cant1(k) + Cedula$: PUT #1, max%: max% = max% + 1
			NEXT k: CLOSE
		END IF
		CLOSE : OPEN "F:\kevinprg\kevindat\bitacora.dat" FOR RANDOM AS #17 LEN = 40: FIELD #17, 40 AS tod$: max% = LOF(17) / 40
		FOR k = 1 TO renglon - 1
		LSET tod$ = code1$(k) + MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4) + MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + cant1(k) + "01" + fct + Cedula$ + prec1(k)
		max% = max% + 1: PUT #17, max%: NEXT k: CLOSE
		OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #1 LEN = 100
		FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS Cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
		nursaler = LOF(1) / 100
		FOR k = 1 TO nursaler: GET #1, k
		IF Cedula$ = Cedu$ THEN RSET inout$ = " ": RSET cong$ = " ": Cedula$ = clla$: PUT #1, k: EXIT FOR
		NEXT k: CLOSE : devolver = false: auxe$ = MKD$(0)
		IF imprint$ = "*" AND Impresion = true THEN
			OPEN "LPT1:" FOR OUTPUT AS #10
			IF devolver = false THEN
			WIDTH #10, 255
			PRINT #10, CHR$(27); CHR$(64); : PRINT #10, CHR$(27); CHR$(64);
			'PRINT #10, CHR$(27); CHR$(67); CHR$(45); 'largo de hoja ojo
			CLOSE #7: OPEN "F:\kevinprg\kevindat\maxfact2.dat" FOR RANDOM AS #7: FIELD #7, 4 AS factura$
			GET #7, 1: numfct$ = factura$: fct = numfct$: numfct$ = MKL$(CVL(numfct$) + 1)
			IF CVL(numfct$) > 999999 THEN numfct$ = MKL$(1)
			LSET factura$ = numfct$: PUT #7, 1: CLOSE #7
		  
					PRINT #10, USING "      Factura:######,"; CVL(fct$); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
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
					PRINT #10, USING "Factura:######,"; CVL(fct$); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
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
					PRINT #10, "      Cliente:                               Vendedor:"; Cedu$; SPACE$(11);
					PRINT #10, "Cliente:                               Vendedor:"; Cedu$; CHR$(10);
					PRINT #10, "      "; nombre$; SPACE$(29);
					PRINT #10, nombre$; CHR$(10);
					rifo$ = Trim$(rifo$): nito$ = Trim$(nito$)
					PRINT #10, "      R.I.F.:"; rifo$; SPACE$(22 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(23 - LEN(nito$)); SPACE$(9);
					PRINT #10, "R.I.F.:"; rifo$; SPACE$(22 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(23 - LEN(nito$)); CHR$(10);
					domicilio1$ = Trim$(domicilio1$): domicilio2$ = Trim$(domicilio2$): tlf$ = Trim$(tlf$)
					PRINT #10, "      Domicilio Fiscal:"; SPACE$(52);
					PRINT #10, "Domicilio Fiscal:"; CHR$(10);
					PRINT #10, "      "; domicilio1$; SPACE$(60 - LEN(domicilio1$)); SPACE$(9);
					PRINT #10, domicilio1$; CHR$(10);
					PRINT #10, "      "; domicilio2$; SPACE$(60 - LEN(domicilio2$)); SPACE$(9);
					PRINT #10, domicilio2$; CHR$(10);
					PRINT #10, "      Telefonos:"; tlf$; SPACE$(20 - LEN(tlf$)); " Forma de Pago: Contado       "; SPACE$(9);
					PRINT #10, "Telefonos:"; tlf$; SPACE$(20 - LEN(tlf$)); " Forma de Pago: Contado"; CHR$(10);
					PRINT #10, "      "; STRING$(60, "-"); SPACE$(9);
					PRINT #10, STRING$(60, "-"); CHR$(10);
					renglon = renglon - 1: auxe$ = MKD$(0)
					PRINT #10, "      ³Descripcion                        ³Cantidad³Precio Venta³ "; SPACE$(9);
					PRINT #10, "³Descripcion                        ³Cantidad³Precio Venta³ "; CHR$(10);
					SELECT CASE renglon
						  CASE 1 TO 7
								 FOR k = 1 TO renglon
										PRINT #10, "      ³"; desc1(k); "³";
										PRINT #10, USING "#######,³"; CVI(cant1(k));
										IF ivaf1(k) = "*" THEN
											  IF NuevoIva = "si" THEN
													 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
													 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100)))))
											  ELSE
													 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
													 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)))))
											  END IF
										ELSE
											  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
											  auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(prec1(k))))
										END IF
								  
										PRINT #10, SPACE$(9); "³"; desc1(k); "³";
										PRINT #10, USING "#######,³"; CVI(cant1(k));
										IF ivaf1(k) = "*" THEN
											  IF NuevoIva = "si" THEN
													 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
											  ELSE
													 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
											  END IF
										ELSE
											  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
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
														 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
														 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100)))))
												  ELSE
														 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
														 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)))))
												  END IF
											ELSE
												  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
												  auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(prec1(k))))
											END IF
									 
											PRINT #10, SPACE$(9); "³"; desc1(k); "³";
											PRINT #10, USING "#######,³"; CVI(cant1(k));
											IF ivaf1(k) = "*" THEN
												  IF NuevoIva = "si" THEN
														 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
												  ELSE
														 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
												  END IF
											ELSE
												  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
											END IF
											PRINT #10, CHR$(10);
										ELSE
											PRINT #10, SPACE$(6); SPACE$(ler%); " \ \ \ \"; SPACE$(60 - ler% - 8); SPACE$(9);
											PRINT #10, SPACE$(ler%); " \ \ \ \"; CHR$(10); : ler% = ler% + 1
										END IF
								 NEXT k
								 PRINT #10, SPACE$(6); SPACE$(34); "VAN --> Bs "; : PRINT #10, USING "##########,.##"; CVD(auxe$); : PRINT #10, SPACE$(9);
								 PRINT #10, SPACE$(34); "VAN --> "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); CHR$(10);
								 FOR mrw% = 1 TO 9: PRINT #10, CHR$(10); : NEXT mrw%
								 PRINT #10, USING "      Factura:######,"; CVL(fct$); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
								 PRINT #10, " Pagina:2 de 2"; SPACE$(9);
								 PRINT #10, USING "Factura:######,"; CVL(fct$); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
								 PRINT #10, " Pagina:2 de 2"; CHR$(10);
								 PRINT #10, "      Cliente:                               Vendedor:"; Cedu$; SPACE$(11);
								 PRINT #10, "Cliente:                               Vendedor:"; Cedu$; CHR$(10);
								 PRINT #10, "      "; nombre$; SPACE$(29);
								 PRINT #10, nombre$; CHR$(10);
								 rifo$ = Trim$(rifo$): nito$ = Trim$(nito$)
								 PRINT #10, "      R.I.F.:"; rifo$; SPACE$(22 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(23 - LEN(nito$)); SPACE$(9);
								 PRINT #10, "R.I.F.:"; rifo$; SPACE$(22 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(23 - LEN(nito$)); CHR$(10);
								 domicilio1$ = Trim$(domicilio1$): domicilio2$ = Trim$(domicilio2$)
								 PRINT #10, "      Domicilio Fiscal:"; SPACE$(52);
								 PRINT #10, "Domicilio Fiscal:"; CHR$(10);
								 PRINT #10, "      "; domicilio1$; SPACE$(60 - LEN(domicilio1$)); SPACE$(9);
								 PRINT #10, domicilio1$; CHR$(10);
								 PRINT #10, "      "; domicilio2$; SPACE$(60 - LEN(domicilio2$)); SPACE$(9);
								 PRINT #10, domicilio2$; CHR$(10);
								 PRINT #10, "      Telefonos:"; tlf$; SPACE$(20 - LEN(tlf$)); " Forma de Pago: Contado       "; SPACE$(9);
								 PRINT #10, "Telefonos:"; tlf$; SPACE$(20 - LEN(tlf$)); " Forma de Pago: Contado"; CHR$(10);
								 PRINT #10, "      "; STRING$(60, "-"); SPACE$(9);
								 PRINT #10, STRING$(60, "-"); CHR$(10);
								 PRINT #10, "      ³Descripcion                        ³Cantidad³Precio Venta³ "; SPACE$(9);
								 PRINT #10, "³Descripcion                        ³Cantidad³Precio Venta³ "; CHR$(10);
								 PRINT #10, SPACE$(6); SPACE$(35); "VIENEN->"; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$);
								 PRINT #10, SPACE$(9); SPACE$(35); "VIENEN->"; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$);
								 PRINT #10, CHR$(10);
								 FOR k = 16 TO 21
										IF k <= renglon THEN
											PRINT #10, "      ³"; desc1(k); "³";
											PRINT #10, USING "#######,³"; CVI(cant1(k));
											IF ivaf1(k) = "*" THEN
												  IF NuevoIva = "si" THEN
														 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
														 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100)))))
												  ELSE
														 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
														 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)))))
												  END IF
											ELSE
												  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
												  auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(prec1(k))))
											END IF
									
											PRINT #10, SPACE$(9); "³"; desc1(k); "³";
											PRINT #10, USING "#######,³"; CVI(cant1(k));
											IF ivaf1(k) = "*" THEN
												  IF NuevoIva = "si" THEN
														 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
												  ELSE
														 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
												  END IF
											ELSE
												  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
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
														 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
														 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100)))))
												  ELSE
														 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
														 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)))))
												  END IF
											ELSE
												  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
												  auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(prec1(k))))
											END IF
									
											PRINT #10, SPACE$(9); "³"; desc1(k); "³";
											PRINT #10, USING "#######,³"; CVI(cant1(k));
											IF ivaf1(k) = "*" THEN
												  IF NuevoIva = "si" THEN
														 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
												  ELSE
														 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
												  END IF
											ELSE
												  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
											END IF
											PRINT #10, CHR$(10);
										ELSE
											PRINT #10, SPACE$(6); SPACE$(ler%); " \ \ \ \"; SPACE$(60 - ler% - 8); SPACE$(9);
											PRINT #10, SPACE$(ler%); " \ \ \ \"; CHR$(10); : ler% = ler% + 1
										END IF
								 NEXT k
								 PRINT #10, SPACE$(6); SPACE$(34); "VAN --> Bs "; : PRINT #10, USING "##########,.##"; CVD(auxe$); : PRINT #10, SPACE$(9);
								 PRINT #10, SPACE$(34); "VAN --> "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); CHR$(10);
								 FOR mrw% = 1 TO 9: PRINT #10, CHR$(10); : NEXT mrw%
								 PRINT #10, USING "      Factura:######,"; CVL(fct$); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
								 PRINT #10, " Pagina:2 de 3"; SPACE$(9);
								 PRINT #10, USING "Factura:######,"; CVL(fct$); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
								 PRINT #10, " Pagina:2 de 3"; CHR$(10);
								 PRINT #10, "      Cliente:                               Vendedor:"; Cedu$; SPACE$(11);
								 PRINT #10, "Cliente:                               Vendedor:"; Cedu$; CHR$(10);
								 PRINT #10, "      "; nombre$; SPACE$(29);
								 PRINT #10, nombre$; CHR$(10);
								 rifo$ = Trim$(rifo$): nito$ = Trim$(nito$)
								 PRINT #10, "      R.I.F.:"; rifo$; SPACE$(22 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(23 - LEN(nito$)); SPACE$(9);
								 PRINT #10, "R.I.F.:"; rifo$; SPACE$(22 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(23 - LEN(nito$)); CHR$(10);
								 domicilio1$ = Trim$(domicilio1$): domicilio2$ = Trim$(domicilio2$)
								 PRINT #10, "      Domicilio Fiscal:"; SPACE$(52);
								 PRINT #10, "Domicilio Fiscal:"; CHR$(10);
								 PRINT #10, "      "; domicilio1$; SPACE$(60 - LEN(domicilio1$)); SPACE$(9);
								 PRINT #10, domicilio1$; CHR$(10);
								 PRINT #10, "      "; domicilio2$; SPACE$(60 - LEN(domicilio2$)); SPACE$(9);
								 PRINT #10, domicilio2$; CHR$(10);
								 PRINT #10, "      Telefonos:"; tlf$; SPACE$(20 - LEN(tlf$)); " Forma de Pago: Contado       "; SPACE$(9);
								 PRINT #10, "Telefonos:"; tlf$; SPACE$(20 - LEN(tlf$)); " Forma de Pago: Contado"; CHR$(10);
								 PRINT #10, "      "; STRING$(60, "-"); SPACE$(9);
								 PRINT #10, STRING$(60, "-"); CHR$(10);
								 PRINT #10, "      ³Descripcion                        ³Cantidad³Precio Venta³ "; SPACE$(9);
								 PRINT #10, "³Descripcion                        ³Cantidad³Precio Venta³ "; CHR$(10);
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
														 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
														 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100)))))
												  ELSE
														 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
														 auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)))))
												  END IF
											ELSE
												  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
												  auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(prec1(k))))
											END IF
								  
											PRINT #10, SPACE$(9); "³"; desc1(k); "³";
											PRINT #10, USING "#######,³"; CVI(cant1(k));
											IF ivaf1(k) = "*" THEN
												  IF NuevoIva = "si" THEN
														 PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
												  ELSE
														 PRINT #10, USING "########,.##³ "; CVD(prec1(k));
												  END IF
											ELSE
												  PRINT #10, USING "########,.##³ "; CVD(prec1(k));
											END IF
											PRINT #10, CHR$(10);
										ELSE
											PRINT #10, SPACE$(6); SPACE$(ler%); " \ \ \ \"; SPACE$(60 - ler% - 8); SPACE$(9);
											PRINT #10, SPACE$(ler%); " \ \ \ \"; CHR$(10); : ler% = ler% + 1
										END IF
								 NEXT k
								 PRINT #10, SPACE$(6); SPACE$(34); "VAN --> Bs "; : PRINT #10, USING "##########,.##"; CVD(auxe$); : PRINT #10, SPACE$(9);
								 PRINT #10, SPACE$(34); "VAN --> "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); CHR$(10);
								 PRINT #10, CHR$(12);
								 PRINT #10, USING "      Factura:######,"; CVL(fct$); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
								 PRINT #10, " Pagina:3 de 3"; SPACE$(9);
								 PRINT #10, USING "Factura:######,"; CVL(fct$); : PRINT #10, " Fecha:"; dia$; " Hora:"; hora$;
								 PRINT #10, " Pagina:3 de 3"; CHR$(10);
								 PRINT #10, "      Cliente:                               Vendedor:"; Cedu$; SPACE$(11);
								 PRINT #10, "Cliente:                               Vendedor:"; Cedu$; CHR$(10);
								 PRINT #10, "      "; nombre$; SPACE$(29);
								 PRINT #10, nombre$; CHR$(10);
								 rifo$ = Trim$(rifo$): nito$ = Trim$(nito$)
								 PRINT #10, "      R.I.F.:"; rifo$; SPACE$(22 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(23 - LEN(nito$)); SPACE$(9);
								 PRINT #10, "R.I.F.:"; rifo$; SPACE$(22 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(23 - LEN(nito$)); CHR$(10);
								 domicilio1$ = Trim$(domicilio1$): domicilio2$ = Trim$(domicilio2$)
								 PRINT #10, "      Domicilio Fiscal:"; SPACE$(52);
								 PRINT #10, "Domicilio Fiscal:"; CHR$(10);
								 PRINT #10, "      "; domicilio1$; SPACE$(60 - LEN(domicilio1$)); SPACE$(9);
								 PRINT #10, domicilio1$; CHR$(10);
								 PRINT #10, "      "; domicilio2$; SPACE$(60 - LEN(domicilio2$)); SPACE$(9);
								 PRINT #10, domicilio2$; CHR$(10);
								 PRINT #10, "      Telefonos:"; tlf$; SPACE$(20 - LEN(tlf$)); " Forma de Pago: Contado       "; SPACE$(9);
								 PRINT #10, "Telefonos:"; tlf$; SPACE$(20 - LEN(tlf$)); " Forma de Pago: Contado"; CHR$(10);
								 PRINT #10, "      "; STRING$(60, "-"); SPACE$(9);
								 PRINT #10, STRING$(60, "-"); CHR$(10);
								 PRINT #10, "      ³Descripcion                        ³Cantidad³Precio Venta³ "; SPACE$(9);
								 PRINT #10, "³Descripcion                        ³Cantidad³Precio Venta³ "; CHR$(10);
								 PRINT #10, SPACE$(6); SPACE$(35); "VIENEN->"; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$);
								 PRINT #10, SPACE$(9); SPACE$(35); "VIENEN->"; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$);
								 PRINT CHR$(10);
					END SELECT
					PRINT #10, "      "; SPACE$(45); STRING$(15, "="); SPACE$(9);
					PRINT #10, SPACE$(45); STRING$(15, "="); CHR$(10);
					PRINT #10, "      "; SPACE$(30); "Sub - Total: "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); SPACE$(9);
					PRINT #10, SPACE$(38); "Sub - Total: "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); : PRINT #10, CHR$(10);
					IF NuevoIva = "si" THEN
						  PRINT #10, "      "; SPACE$(30); "- Descuento: "; : PRINT #10, USING "##########,.## Bs"; (CVD(auxe$) * (1 - (CVD(neto) / CVD(total)))); : PRINT #10, SPACE$(9);
					ELSE
						  PRINT #10, "      "; SPACE$(30); "- Descuento: "; : PRINT #10, USING "##########,.## Bs"; (CVD(total) - CVD(neto)); : PRINT #10, SPACE$(9);
					END IF
					IF NuevoIva = "si" THEN
						  PRINT #10, SPACE$(30); "- Descuento: "; : PRINT #10, USING "##########,.## Bs"; (CVD(auxe$) * (1 - (CVD(neto) / CVD(total)))); : PRINT #10, CHR$(10);
					ELSE
						  PRINT #10, SPACE$(30); "- Descuento: "; : PRINT #10, USING "##########,.## Bs"; (CVD(total) - CVD(neto)); : PRINT #10, CHR$(10);
					END IF
					IF ShowPerc = "si" THEN
						  IF (100 - (100 * (CVD(neto) / CVD(total)))) > 0 THEN
								 PRINT #10, SPACE$(6); SPACE$(40); "%descuento:"; : PRINT #10, USING "###.##  %"; (100 - (100 * (CVD(neto) / CVD(total)))); : PRINT #10, SPACE$(9);
						  END IF
					END IF
					IF ShowPerc = "si" THEN
						  IF (100 - (100 * (CVD(neto) / CVD(total)))) > 0 THEN
								 PRINT #10, SPACE$(40); "%descuento:"; : PRINT #10, USING "###.##  %"; (100 - (100 * (CVD(neto) / CVD(total)))); : PRINT #10, CHR$(10);
						  END IF
					ELSE
						PRINT #10, CHR$(10);
					END IF
					IF NuevoIva = "si" THEN
						  PRINT #10, SPACE$(6); SPACE$(24); "+Impuesto("; impuesto; "%):";
						  PRINT #10, USING "##########,.## Bs"; CVD(neto) - (CVD(auxe$) * (CVD(neto) / CVD(total)));
						  PRINT #10, SPACE$(9);
					END IF
					IF NuevoIva = "si" THEN
						  PRINT #10, SPACE$(24); "+Impuesto("; impuesto; "%):";
						  PRINT #10, USING "##########,.## Bs"; CVD(neto) - (CVD(auxe$) * (CVD(neto) / CVD(total)));
						  PRINT #10, CHR$(10);
					ELSE
						  PRINT #10, CHR$(10);
					END IF
					PRINT #10, SPACE$(6); SPACE$(30); "Neto Factura:"; : PRINT #10, USING "##########,.## Bs"; CVD(neto); : PRINT #10, SPACE$(9);
					PRINT #10, SPACE$(30); "Neto Factura:"; : PRINT #10, USING "##########,.## Bs"; CVD(neto); : PRINT #10, CHR$(10);
					PRINT #10, "      "; STRING$(60, "-"); SPACE$(9);
					PRINT #10, STRING$(60, "-"); CHR$(10);
					PRINT #10, "      "; Mensaje1; SPACE$(9);
					PRINT #10, Mensaje1; CHR$(10);
					PRINT #10, "      "; Mensaje2; SPACE$(9);
					PRINT #10, Mensaje2; CHR$(10);
					PRINT #10, "      "; Mensaje3; SPACE$(9);
					PRINT #10, Mensaje3; CHR$(10);
					PRINT #10, CHR$(12); : CLOSE #10

			renglon = renglon + 1: CLOSE
			END IF
		END IF
		FOR k = 1 TO 26
		regi1(k) = MKI$(0): regi2(k) = " ": desc1(k) = "": cant1(k) = MKI$(0)
		cost1(k) = MKD$(0): ivaf1(k) = " ": prec1(k) = MKD$(0): code1(k) = ""
		NEXT k: DescGer = MKD$(0): CLOSE : FOR k = 1 TO 17: Fun(k) = "no": NEXT k: salir = true
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
		LOCATE 2, 24: PRINT "Â": LOCATE 2, 40: PRINT "ÂÂ": LOCATE 2, 29: PRINT "Â": LOCATE 2, 64: PRINT "Â": LOCATE 2, 69: PRINT "Â": LOCATE 16, 24: PRINT "Á": LOCATE 16, 29: PRINT "Á": LOCATE 16, 40: PRINT "ÁÁ": LOCATE 16, 64: PRINT "Á": LOCATE 16,  _
69: PRINT "Á"
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
		IF VAL(desc) > 0 THEN DescGer = MKD$(CVD(total) * VAL(desc) / 100)
		IF CVD(DescGer) > 0 THEN LOCATE 2, 3: PRINT "Sub-total Neto:"; : PRINT USING "#########,.## Bs"; (CVD(total) - CVD(DescGer)); : PRINT USING " (##.##%)"; (100 - (100 * ((CVD(total) - CVD(DescGer)) / CVD(total))));
		COLOR colors(7, colorpref), colors(4, colorpref)
END IF: sit% = 0
IF preg$ = "*" THEN
	venzo& = 1000000: lado% = 0: auxvenzo& = 0: veni$ = "No hay fecha registrada , por favor repare el articulo.": vena$ = veni$
	IF ven12$ <> "  " AND CVI(exi12$) <> 0 THEN
		auxvenzo& = 12 * CVI(ven12$): IF ven11$ = "A" THEN auxvenzo& = auxvenzo& + 10: vena$ = "Octubre de" + STR$(CVI(ven12$))
		IF ven11$ = "B" THEN auxvenzo& = auxvenzo& + 11: vena$ = "Noviembre de" + STR$(CVI(ven12$))
		IF ven11$ = "C" THEN auxvenzo& = auxvenzo& + 12: vena$ = "Diciembre de" + STR$(CVI(ven12$)) ELSE auxvenzo& = auxvenzo& + VAL(ven11$): rit% = 1: GOSUB Dames
		IF auxvenzo& < venzo& THEN venzo& = auxvenzo&: lado$ = "uno (1)": sit% = 1: condi% = CVI(exi12$): veni$ = vena$
	END IF: auxvenzo& = 0
	IF ven22$ <> "  " AND CVI(exi22$) <> 0 THEN
		auxvenzo& = 12 * CVI(ven22$): IF ven21$ = "A" THEN auxvenzo& = auxvenzo& + 10: vena$ = "Octubre de" + STR$(CVI(ven22$))
		IF ven21$ = "B" THEN auxvenzo& = auxvenzo& + 11: vena$ = "Noviembre de" + STR$(CVI(ven22$))
		IF ven21$ = "C" THEN auxvenzo& = auxvenzo& + 12: vena$ = "Diciembre de" + STR$(CVI(ven22$)) ELSE auxvenzo& = auxvenzo& + VAL(ven21$): rit% = 2: GOSUB Dames
		IF auxvenzo& < venzo& THEN venzo& = auxvenzo&: lado$ = "dos (2)": sit% = 2: condi% = CVI(exi22$): veni$ = vena$
	END IF: auxvenzo& = 0
	IF ven32$ <> "  " AND CVI(exi32$) <> 0 THEN
		auxvenzo& = 12 * CVI(ven32$): IF ven31$ = "A" THEN auxvenzo& = auxvenzo& + 10: vena$ = "Octubre de" + STR$(CVI(ven32$))
		IF ven31$ = "B" THEN auxvenzo& = auxvenzo& + 11: vena$ = "Noviembre de" + STR$(CVI(ven32$))
		IF ven31$ = "C" THEN auxvenzo& = auxvenzo& + 12: vena$ = "Diciembre de" + STR$(CVI(ven32$)) ELSE auxvenzo& = auxvenzo& + VAL(ven31$): rit% = 3: GOSUB Dames
		IF auxvenzo& < venzo& THEN venzo& = auxvenzo&: lado$ = "tres (3)": sit% = 3: condi% = CVI(exi32$): veni$ = vena$
	END IF: auxvenzo& = 0
	IF ven42$ <> "  " AND CVI(exi42$) <> 0 THEN
		auxvenzo& = 12 * CVI(ven42$): IF ven41$ = "A" THEN auxvenzo& = auxvenzo& + 10: vena$ = "Octubre de" + STR$(CVI(ven42$))
		IF ven41$ = "B" THEN auxvenzo& = auxvenzo& + 11: vena$ = "Noviembre de" + STR$(CVI(ven42$))
		IF ven41$ = "C" THEN auxvenzo& = auxvenzo& + 12: vena$ = "Diciembre de" + STR$(CVI(ven42$)) ELSE auxvenzo& = auxvenzo& + VAL(ven41$): rit% = 4: GOSUB Dames
		IF auxvenzo& < venzo& THEN venzo& = auxvenzo&: lado$ = "cuatro (4)": sit% = 4: condi% = CVI(exi42$): veni$ = vena$
	END IF: auxvenzo& = 0
	IF venzo& < 1000000 THEN
		COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 21, 1, 24, 80
		LOCATE 21, 25: PRINT "* * *  A T E N C I O N  * * *"; : IF MID$(veni$, 1, 6) = "No hay" THEN RSET iv42$ = "*"
		COLOR colors(7, colorpref), colors(4, colorpref): Center 22, "El PVP Numero " + lado$ + " se vence en:"
		CALL Center(23, veni$): Espere 24, 20: GOSUB Mostrar: LOCK #1, mjfox%
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
				GOSUB Limpio: UNLOCK #1, mjfox%
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
old% = 0: IF CVI(exi12$) > 0 THEN old% = old% + 1
IF CVI(exi22$) > 0 THEN old% = old% + 1
IF CVI(exi32$) + CVI(exi42$) > 0 THEN old% = old% + 1
LOCATE 20, 24: PRINT "³"; : PRINT USING "#####,"; CVI(exi12$); : LOCATE 20, 42: PRINT "³"; : PRINT USING "#####,"; CVI(exi22$);
LOCATE 20, 61: PRINT "³"; : PRINT USING "#####,"; CVI(exi32$): LOCATE 20, 32: PRINT USING "######,.##"; CVD(pv12$)
LOCATE 20, 51: PRINT USING "######,.##"; CVD(pv22$): LOCATE 20, 70: PRINT USING "######,.##"; CVD(pv32$)
CALL Center(25, "Introduzca el N£mero de Unidades y Presione [Enter] cuando est‚ listo.")
Box 21, 3, 23, 50: LOCATE 22, 5: PRINT " Cantidad a vender?   ": salir = false
IF negativo = "si" THEN
	keya = EntraDatoNumero(22, 25, 6, " 1    ", 1)
ELSE
	DO WHILE NOT salir
	keya = EntraDatoNumero(22, 25, 6, " 1    ", 1): IF VAL(keya) <= condi% THEN salir = true
	Center 25, " La Cantidad NO debe ser MAYOR que la Existencia "
	LOOP
END IF: cant1(renglon) = MKI$(VAL(keya))
IF old% > 1 AND CVI(cant1(renglon)) > 0 THEN
		COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 21, 1, 24, 80
		LOCATE 21, 25: PRINT "* * *  A T E N C I O N  * * *": COLOR colors(7, colorpref), colors(4, colorpref)
		Center 22, "Usted esta Vendiendo del PVP numero:" + STR$(sit%): Center 23, "ES SU OBLIGACION VERIFICAR DE QUE PVP ESTA ENTREGANDO": Espere 24, 20
END IF
IF CVI(cant1(renglon)) = 0 THEN
	UNLOCK #1, mjfox%
	mjfox% = 0: desc1(renglon) = "": code1(renglon) = "": cant1(renglon) = MKI$(0): prec1(renglon) = MKD$(0): cost1(renglon) = MKD$(0): GOSUB Limpio: RETURN
END IF
tour% = 1
IF renglon > 1 THEN
FOR k = 1 TO renglon - 1
IF codigo2$ = code1(k) THEN
	IF sit% = 1 THEN
		IF CVD(prec1(k)) = CVD(pv12$) THEN
				IF CVD(cost1(k)) = CVD(cos12$) THEN
					cant1(k) = MKI$(CVI(cant1(k)) + CVI(cant1(renglon))): total = MKD$((CVD(prec1(k)) * CVI(cant1(renglon))) + (CVD(total)))
					costo = MKD$((CVD(cost1(k)) * CVI(cant1(renglon))) + (CVD(costo)))
					tour% = 2: k = renglon: RSET exi12$ = MKI$(CVI(exi12$) - CVI(cant1(renglon))): cant1(renglon) = MKI$(0)
					PUT #1, mjfox: CLOSE
				END IF
		END IF
	END IF
	IF sit% = 2 THEN
		IF CVD(prec1(k)) = CVD(pv22$) THEN
				IF CVD(cost1(k)) = CVD(cos22$) THEN
					cant1(k) = MKI$(CVI(cant1(k)) + CVI(cant1(renglon))): total = MKD$((CVD(prec1(k)) * CVI(cant1(renglon))) + (CVD(total)))
					costo = MKD$((CVD(cost1(k)) * CVI(cant1(renglon))) + (CVD(costo)))
					tour% = 2: k = renglon: RSET exi22$ = MKI$(CVI(exi22$) - CVI(cant1(renglon))): cant1(renglon) = MKI$(0)
					PUT #1, mjfox: CLOSE
				END IF
		END IF
	END IF
	IF sit% = 3 THEN
		IF CVD(prec1(k)) = CVD(pv32$) THEN
				IF CVD(cost1(k)) = CVD(cos32$) THEN
					cant1(k) = MKI$(CVI(cant1(k)) + CVI(cant1(renglon))): total = MKD$((CVD(prec1(k)) * CVI(cant1(renglon))) + (CVD(total)))
					costo = MKD$((CVD(cost1(k)) * CVI(cant1(renglon))) + (CVD(costo)))
					tour% = 2: k = renglon: RSET exi32$ = MKI$(CVI(exi32$) - CVI(cant1(renglon))): cant1(renglon) = MKI$(0)
					PUT #1, mjfox: CLOSE
				END IF
		END IF
	END IF
	IF sit% = 4 THEN
		IF CVD(prec1(k)) = CVD(pv32$) THEN
				IF CVD(cost1(k)) = CVD(cos42$) THEN
					cant1(k) = MKI$(CVI(cant1(k)) + CVI(cant1(renglon))): total = MKD$((CVD(prec1(k)) * CVI(cant1(renglon))) + (CVD(total)))
					costo = MKD$((CVD(cost1(k)) * CVI(cant1(renglon))) + (CVD(costo))): DescGer = MKD$(CVD(DescGer) + ((CVD(pv32$) - CVD(pv42$)) * CVI(cant1(renglon))))
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
			ivaf1(renglon) = iv$: cost1(renglon) = cos42$: DescGer = MKD$(CVD(DescGer) + ((CVD(pv32$) - CVD(pv42$)) * CVI(cant1(renglon))))
		END IF
		regi2(renglon) = MID$(STR$(sit%), 2, 1)
		regi1(renglon) = MKI$(mjfox%)
		LOCATE 1, 1
		IF CVI(ubic2$) = 1 THEN psic1(renglon) = "*" ELSE psic1(renglon) = " "
REM        'LOCATE 1, 1: PRINT USING "(####)"; VAL(regi2(renglon)); renglon; mjfox%; CVI(regi1(renglon))
		code1(renglon) = codigo2$: desc1(renglon) = nomb2$: costo = MKD$((CVD(cost1(renglon)) * CVI(cant1(renglon))) + (CVD(costo)))
		total = MKD$(CVD(total) + CVD(prec1(renglon)) * CVI(cant1(renglon)))
		PUT #1, mjfox: CLOSE
	CASE 2
		renglon = renglon - 1
END SELECT
GOSUB GrabaPrv: GOSUB Limpia: renglon = renglon + 1: IF renglon > 26 THEN Fun(17) = "no"
RETURN

Triangul:
	COLOR 15, 0: r$ = "Pulse cualquier tecla": CLS
	y% = FIX((RND * 24) + 1): x% = FIX((RND * 59) + 1)
	LOCATE y%, x%: PRINT r$; : lastTime# = TIMER
	WHILE INKEY$ = ""
				NewTime# = TIMER
				IF (NewTime# - lastTime#) > VAL(Blanq) THEN
					CLS : y% = FIX((RND * 24) + 1): x% = FIX((RND * 59) + 1)
					LOCATE y%, x%: PRINT r$; : lastTime# = TIMER
				END IF
	WEND
	COLOR colors(7, colorpref), colors(4, colorpref): CLS : CALL Box(1, 1, 4, 80): CALL Center(2, "Farmacia:" + NombFarma + " Fecha:" + MID$(DATE$, 4, 2) + "/" + MID$(DATE$, 1, 2) + "/" + MID$(DATE$, 7, 4)): CALL Center(3,  _
"*** V E N T A S   D E   C O N T A D O ***")
	CALL Box(5, 10, 7, 70): CALL Center(6, " Introduzca Clave de Vendedor:²   ²"): Box 21, 10, 23, 70: CALL Center(22, " Consultas SIN Abrir Factura ")
	Box2 8, 5, 20, 75: Center 8, " Caja de Tareas "
	CALL Center(24, "[F1]Ayuda [F2]Laboratorio [F3]PorCodigo [F4]Oferta [F5]Compos. [F7]PorNombre"): CALL Center(25, "[Esc]Menu Principal  [F10]Tarjetas tlf.[F11]Vademecum  [F12]Telefonos")
lastTime# = TIMER
RETURN

Mostrar:
COLOR colors(4, colorpref), colors(7, colorpref): GOSUB Limpio
CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS Saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
GET #1, mjfox%
OPEN "F:\kevinprg\kevinvnt\estadist.dat" FOR RANDOM SHARED AS #3 LEN = 20
FIELD #3, 4 AS unid$, 8 AS primera$, 8 AS ultima$
GET #3, mjfox%
LOCATE 17, 1: PRINT "Codigo Nombre[";
PRINT USING "######,"; CVL(unid$);
: PRINT "]["; primera$; "]["; ultima$; "]";
PRINT "Ofertas Observaciones"; : CLOSE #3
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
LOCATE 22, 24: PRINT "Á"; : LOCATE 22, 42: PRINT "Á"; : LOCATE 22, 61: PRINT "Á";
LOCATE 23, 1: PRINT "Composici¢n:     [M¡n1="; : PRINT USING "####"; CVI(mini12$);
PRINT "] [M¡n2="; : PRINT USING "####"; CVI(mini22$);
PRINT "] [M x1="; : PRINT USING "####"; CVI(maxi12$);
PRINT "] [M x2="; : PRINT USING "####"; CVI(maxi22$); : PRINT "]";
IF iv$ = "*" THEN PRINT " Paga I.V.A.    " ELSE PRINT " Producto Exento"
OPEN "f:\kevinprg\kevindat\ordcompo.dat" FOR RANDOM SHARED AS #78 LEN = 15
FIELD #78, 15 AS describe$: LOCATE 24, 1
IF CVI(compo12$) > 1 THEN GET #78, CVI(compo12$): PRINT describe$; "²";
IF CVI(compo22$) > 1 THEN GET #78, CVI(compo22$): PRINT describe$; "²";
IF CVI(compo32$) > 1 THEN GET #78, CVI(compo32$): PRINT describe$; "²";
IF CVI(compo42$) > 1 THEN GET #78, CVI(compo42$): PRINT describe$; "²";
IF CVI(compo52$) > 1 THEN GET #78, CVI(compo52$): PRINT describe$; "²";
IF CVI(compo62$) > 1 THEN GET #78, CVI(compo62$): PRINT describe$; "²";
CLOSE #78
IF Fun(17) = "no" THEN TIMER ON: Espera: TIMER OFF
RETURN

GrabaPrv:
OPEN fx2 FOR RANDOM AS #17 LEN = 63: FIELD #17, 35 AS descri$, 2 AS cantid$, 8 AS precio$, 8 AS costid$, 1 AS isvcm$, 6 AS codito$, 2 AS regist1$, 1 AS regist2$
LSET descri$ = fct + DescGer + MKI$(renglon) + SPACE$(21): PUT #17, 1
FOR k = 2 TO 27
RSET descri$ = desc1(k - 1): RSET cantid$ = cant1(k - 1): RSET precio$ = prec1(k - 1): RSET costid$ = cost1(k - 1)
RSET isvcm$ = ivaf1(k - 1): RSET codito$ = code1(k - 1): RSET regist1$ = regi1(k - 1): RSET regist2$ = regi2(k - 1): PUT #17, k
NEXT k: CLOSE
IF Fun(12) = "si" THEN
	Fun(12) = "no"
	OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #1 LEN = 100
	FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS Cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
	nursaler = LOF(1) / 100
	FOR k = 1 TO nursaler: GET #1, k
	IF Cedula$ = Cedu$ THEN RSET inout$ = " ": RSET cong$ = "*": PUT #1, k: EXIT FOR
	NEXT k: CLOSE
	FOR k = 1 TO 26
	regi1(k) = MKI$(0): regi2(k) = " ": desc1(k) = "": cant1(k) = MKI$(0)
	cost1(k) = MKD$(0): ivaf1(k) = " ": prec1(k) = MKD$(0): code1(k) = "": psic1(k) = " "
	NEXT k: DescGer = MKD$(0): CLOSE : FOR k = 1 TO 17: Fun(k) = "no": NEXT k: salir = true
END IF
RETURN
BuscaBank:
CALL Center(25, " *** Espere , Por Favor ... ***")
IF ybusqueda <> 6 THEN
	CLOSE : OPEN "F:\kevinprg\kevindat\bank.dat" FOR RANDOM SHARED AS #5 LEN = 18: FIELD #5, 3 AS codigo$, 15 AS nomb$
	FOR k = 1 TO 199: GET #5, k: Yname1(k) = codigo$: Yname2(k) = nomb$: NEXT k: CLOSE #5: ybusqueda = 6
END IF
LOCATE 20, 20: PRINT "Elija el Banco :  ²"; Yname1(1); "²"; Yname2(1)
mjfox% = Elije%(6, 3, 60, 20, 39, 0): IF mjfox% < 1 THEN GOSUB Limpio: RETURN
numbank = MKI$(VAL(Yname1(mjfox%))): LOCATE 20, 20: PRINT "Banco  Elegido:     ²"; Yname1(mjfox%); "²"; Yname2(mjfox%)
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
LOCATE 18, 10: PRINT "N.I.T.: ²"; nito$; "²": LOCATE 18, 40: PRINT "Forma de Pago: CONTADO"
RETURN

Dames:
SELECT CASE rit%
	CASE 1
		lode$ = ven11$: lodi$ = STR$(CVI(ven12$))
	CASE 2
		lode$ = ven21$: lodi$ = STR$(CVI(ven22$))
	CASE 3
		lode$ = ven31$: lodi$ = STR$(CVI(ven32$))
	CASE 4
		lode$ = ven41$: lodi$ = STR$(CVI(ven42$))
END SELECT
IF lode$ = "1" THEN vena$ = "Enero de" + lodi$
IF lode$ = "2" THEN vena$ = "Febrero de" + lodi$
IF lode$ = "3" THEN vena$ = "Marzo de" + lodi$
IF lode$ = "4" THEN vena$ = "Abril de" + lodi$
IF lode$ = "5" THEN vena$ = "Mayo de" + lodi$
IF lode$ = "6" THEN vena$ = "Junio de" + lodi$
IF lode$ = "7" THEN vena$ = "Julio de" + lodi$
IF lode$ = "8" THEN vena$ = "Agosto de" + lodi$
IF lode$ = "9" THEN vena$ = "Septiembre de" + lodi$
RETURN
END SUB

