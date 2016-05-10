' cobra por renglones a un cliente de credito
DEFINT A-Z
DECLARE SUB CobraRenglones ()
DECLARE SUB Espera ()
DECLARE SUB Box (Row1, Col1, Row2, Col2)
DECLARE SUB Box2 (Row1, Col1, Row2, Col2)
DECLARE SUB Center (row, text$)
DECLARE SUB LeeEstado ()
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$, tr$)
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Elije% (fila%, columna%, fila2%, columna2%, edo%)
DECLARE FUNCTION Trim$ (x$)
COMMON SHARED guia, colorpref AS INTEGER
DIM SHARED nursaler, tomados: guia = 1
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, impuesto AS STRING * 6, NombFarma AS STRING * 25, Direccion AS STRING * 30, Rif     AS STRING * 25, Nit      AS STRING * 25, telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2, NuevoIva AS STRING * 2, ShowPerc AS STRING * 2
DIM SHARED regi1(26) AS STRING * 2, regi2(26) AS STRING * 1, desc1(26) AS STRING * 35
DIM SHARED cant1(26) AS STRING * 2, cost1(26) AS STRING * 8, ivaf1(26) AS STRING * 1
DIM SHARED prec1(26) AS STRING * 8, code1(26) AS STRING * 6, psic1(26) AS STRING * 1
DIM SHARED PrintErr AS INTEGER, devolver, Fun(17) AS STRING * 2, sitio, salir
DIM SHARED renglon AS INTEGER, total AS STRING * 8, neto AS STRING * 8, numcheq AS STRING * 8, moncheq AS STRING * 8, numbank AS STRING * 2
DIM SHARED entrega AS STRING * 13, keya AS STRING * 6, costa AS STRING * 8, credit AS STRING * 8, encabezado AS STRING * 2
DIM SHARED previo AS STRING * 8, fx AS STRING * 33, fx2 AS STRING * 33
DIM SHARED fct AS STRING * 4, conda AS STRING * 8, descger AS STRING * 8
DIM SHARED Yname1(199) AS STRING * 3, Yname2(199) AS STRING * 35
DIM SHARED elejidos(26) AS STRING * 2
CONST true = -1: CONST false = NOT true:  ybusqueda = 0
ON ERROR GOTO ErrorTrap
CALL LeeEstado: CALL CobraRenglones: CLOSE
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
		LOCATE 1, 1: PRINT "Error:"; ERR
END SELECT
RESUME NEXT
DATA 0,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  2,     0,     1,     15,     0,     7,      0
DATA 3,    15,   13,  1,     14,    3,     15,     0,     7,      0
DATA 7,    12,   15,  4,     14,    0,     15,     15,    1,      0

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

SUB CobraRenglones
	COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box2 1, 1, 3, 80: Center 2, "Cancelacion POR RENGLONES de Cuenta de Credito"
	Center 25, "[F1]Ayuda [Enter]Escoje Cliente [Esc]Salir Use las Teclas de direccion"
	cliente% = Elije%(5, 5, 4, 18, 7): IF cliente% < 1 THEN EXIT SUB
	IF Yname2(cliente%) = "þ" + SPACE$(34) THEN EXIT SUB
	CALL Center(4, "Cliente Elejido:²" + Yname1(cliente%) + "²" + Yname2(cliente%))
	COLOR colors(7, colorpref), colors(4, colorpref): CALL Box2(5, 1, 21, 80)
	fx = "F:\kevinprg\kevindat\cli" + Yname1(cliente%) + ".crd"
	OPEN fx FOR RANDOM AS #4 LEN = 38
	FIELD #4, 1 AS acti$, 6 AS codi$, 2 AS canti$, 8 AS pvp$, 8 AS costo$, 1 AS psico$, 1 AS iva$, 1 AS auxi$, 8 AS fecha$, 2 AS lugar$
	max% = LOF(4) / 38: total = MKD$(0): credit = MKD$(0)
	CLOSE #5: OPEN "f:\kevinprg\kevindat\fichacli.ent" FOR RANDOM SHARED AS #5 LEN = 326
	FIELD #5, 20 AS rif5$, 20 AS nit5$, 2 AS diacred5$, 4 AS monmax5$, 60 AS direc15$, 60 AS direc25$, 20 AS tele5$, 1 AS edo15$, 2 AS dia15$, 1 AS edo25$, 2 AS dia25$, 1 AS edo35$, 2 AS dia35$, 1 AS edo45$, 2 AS dia45$, 30 AS autor15$, 30 AS  _
autor25$, 30 AS autor35$, 30 AS autor45$, 8 AS desde$
	GET #5, VAL(Yname1(cliente%))
	IF max% > 0 THEN
		FOR k = 1 TO max%: GET #4, k: RSET auxi$ = " "
		IF acti$ = " " AND codi$ <> "NOTADE" THEN
			total = MKD$(CVD(total) + CVI(canti$) * CVD(pvp$))
		END IF
		PUT #4, k: NEXT k
	END IF
	IF CVD(total) = 0 THEN Box2 10, 10, 12, 70: Center 11, " Este cliente NO Adeuda ": Espera: EXIT SUB
	costa = MKD$(0): total = MKD$(0): neto = MKD$(0): lleva% = 1: GET #4, 1
	OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
	FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS  _
cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$ _
, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
	DO WHILE acti$ = "*"
		lleva% = lleva% + 1: IF lleva% > max% THEN lleva% = max%
		GET #4, lleva%
	LOOP
	DO
		GET #4, lleva%: Box 12, 1, 14, 80: LOCATE 12, 7: PRINT "Renglones Seleccionados:"; renglon; " (maximo 26 renglones)"
		IF codi$ <> "NOTADE" THEN
			GET #1, CVI(lugar$)
			LOCATE 13, 2: PRINT codi$; " "; nomb2$;
			PRINT USING "######, "; CVI(canti$);
			PRINT USING "###########,.##"; CVD(pvp$);
			IF auxi$ = "*" THEN PRINT " Seleccionado"
		ELSE
			LOCATE 13, 2: PRINT codi$; " Nota de credito "; : PRINT USING "########,                   "; CVD(costo$);
			PRINT USING "###########,.##"; CVD(pvp$);
			IF auxi$ = "*" THEN PRINT " Seleccionado"
		END IF
			descger = MKD$(CVD(total) * ((CVI(dia45$)) / 100)): neto = MKD$(CVD(total) - CVD(descger))
			IF VAL(Redondeo) > 0 THEN
				IF CVD(neto) > 100 THEN
					neto = MKD$(VAL(Redondeo) * (FIX(CVD(neto) / VAL(Redondeo))))
					descger = MKD$(CVD(total) - CVD(neto))
				END IF
			END IF
			IF CVD(neto) < CVD(costa) THEN neto = costa: descger = MKD$(CVD(total) - CVD(neto))
			Box2 5, 10, 11, 70: Center 6, "Montos Parciales Seleccionados:"
			LOCATE 7, 12: PRINT " Total Adeudado    :"; : PRINT USING "###########,.##"; CVD(total);
			LOCATE 8, 12: PRINT " Total Descuento   :"; : PRINT USING "###########,.##"; CVD(descger);
			LOCATE 9, 12: PRINT " Total Abonado     :"; : PRINT USING "###########,.##"; CVD(credit);
			LOCATE 10, 12: PRINT " Total Neto a Pagar:"; : PRINT USING "###########,.##"; (CVD(total) - CVD(descger) + CVD(credit));
			Center 24, "Utilize las Teclas de Direccion Para subir o bajar y con [Enter] Selecciona"
			Center 25, "[F1] Ayuda [F9]Paga efectivo [F10]Paga cheque [Esc]Salir SIN cobrar": COLOR colors(7, colorpref), colors(4, colorpref)
			llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
			IF llave$ = CHR$(0) + CHR$(67) THEN
				IF (CVD(total) - CVD(descger) + CVD(credit)) > 0 THEN Fun(9) = "si": Fun(10) = "no": GOSUB cierre: EXIT SUB
			END IF
			IF llave$ = CHR$(0) + CHR$(68) THEN
				IF (CVD(total) - CVD(descger) + CVD(credit)) > 0 THEN Fun(9) = "si": Fun(10) = "si": GOSUB cierre: EXIT SUB
			END IF
			IF llave$ = CHR$(27) THEN EXIT SUB
			IF llave$ = CHR$(13) THEN
				IF codi$ <> "NOTADE" THEN
					IF auxi$ = " " THEN
						RSET auxi$ = "*": renglon = renglon + 1
						total = MKD$(CVD(total) + CVI(canti$) * CVD(pvp$))
						costa = MKD$(CVD(costa) + CVI(canti$) * CVD(costo$))
						PUT #4, lleva%: IF renglon >= 26 THEN Fun(9) = "si": Fun(10) = "no": GOSUB cierre
					ELSE
						RSET auxi$ = " ": renglon = renglon - 1
						total = MKD$(CVD(total) - CVI(canti$) * CVD(pvp$))
						costa = MKD$(CVD(costa) - CVI(canti$) * CVD(costo$)): PUT #4, lleva%:
					END IF
				ELSE
					IF auxi$ = " " THEN
						RSET auxi$ = "*"
						credit = MKD$(CVD(credit) + CVD(pvp$))
						PUT #4, lleva%
					ELSE
						RSET auxi$ = " "
						credit = MKD$(CVD(credit) - CVD(pvp$))
						PUT #4, lleva%
					END IF
				END IF
			END IF
			IF llave$ = CHR$(0) + "P" THEN
				lleva% = lleva% + 1: IF lleva% > max% THEN GOSUB BuscaArriba ELSE GOSUB BuscaAbajo
			END IF
			IF llave$ = CHR$(0) + "H" THEN
				lleva% = lleva% - 1: IF lleva% < 1 THEN GOSUB BuscaAbajo ELSE GOSUB BuscaArriba
			END IF
	LOOP
EXIT SUB

BuscaAbajo:
DO
	GET #4, lleva%: IF acti$ = " " THEN EXIT DO
	lleva% = lleva% + 1: IF lleva% > max% THEN lleva% = max%: GOSUB BuscaArriba
LOOP
RETURN

BuscaArriba:
DO
	GET #4, lleva%: IF acti$ = " " THEN EXIT DO
	lleva% = lleva% - 1: IF lleva% < 1 THEN lleva% = 1: GOSUB BuscaAbajo
LOOP
RETURN

cierre: '       CIERRE-----CIERRE------CIERRE-----
COLOR colors(7, colorpref), colors(4, colorpref): Box2 12, 10, 14, 70
LOCATE 13, 17: PRINT "Introduzca Clave de Vendedor:": eleccion$ = MeteDatoLetra$(13, 50, 3, "   ", "")
OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #2 LEN = 100: FIELD #2, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS Cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
nursaler = LOF(2) / 100: ClaveLarga$ = ""
FOR k = 1 TO nursaler: GET #2, k
			IF eleccion$ = clco$ AND acti$ = " " AND inout$ = " " THEN
			ClaveLarga$ = clla$: gav$ = "com" + gaveta$: Cedula$ = Cedu$: RSET inout$ = "*"
			PUT #2, k: EXIT FOR
			END IF
NEXT k: CLOSE #2: conforme% = false
IF Fun(9) = "si" THEN prevcierr$ = "9"
IF Fun(10) = "si" THEN prevcierr$ = "10"
IF ClaveLarga$ = "" THEN CALL Center(14, "No Hay Ningun Vendedor Con Esa Clave o Esta Fuera El Vendedor"): Espera: EXIT SUB

DO WHILE conforme% = false
COLOR colors(7, colorpref), colors(4, colorpref): Box2 12, 10, 14, 70: CALL Box2(16, 15, 24, 65): LOCATE 21, 39: PRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
LOCATE 17, 20: PRINT "Bruto a Pagar  Bs.:"; : PRINT USING "#########,.##"; CVD(total)
LOCATE 20, 20: PRINT "Total Abonado  Bs.:"; : PRINT USING "#########,.##"; CVD(credit)
	IF Fun(9) = "si" THEN    'con descuento
			CALL Center(25, "Introduzca el Descuento en Bs. y si es por Porcentaje Agregue el Signo `%'")
			LOCATE 18, 20: PRINT "Descuento en Bs ¢ %²             ²": salir = false
			DO WHILE NOT salir
			LOCATE 18, 20: PRINT "Descuento en Bs ¢ %"
			entrega = EntraDatoNumero(18, 39, 13, STR$((FIX(CVD(descger) * 100)) / 100), 0)
			llevas$ = "": IF Fun(11) = "si" THEN Fun(9) = "no"
			FOR k = 1 TO 13
				IF MID$(entrega, k, 1) = "%" THEN
				entrega = MID$(entrega, 1, k - 1) + MID$(entrega, k + 1, 13 - k): llevas$ = "si"
				END IF
			NEXT k
			IF llevas$ = "si" THEN
				salir = true
			ELSE
				entrega = STR$(VAL(entrega) - CVD(descger))
				IF VAL(entrega) > 0 THEN
				neto = MKD$(CVD(total) - CVD(descger))
				entri$ = MKD$(100 - ((CVD(neto) - VAL(entrega)) * 100) / (CVD(total) - CVD(descger))): LOCATE 18, 40: PRINT USING "#######,.## %"; CVD(entri$): LOCATE 18, 20: PRINT "Descuento Actual : "
				neto = MKD$(CVD(neto) - VAL(entrega)): entre$ = MKD$(100 - (CVD(neto) * 100) / (CVD(total))): salir = true
				END IF
				IF VAL(entrega) <= 0 THEN
				entrega = STR$(VAL(entrega) + CVD(descger))
				neto = MKD$(CVD(total) - VAL(entrega)): salir = true
				entre$ = MKD$(100 - (CVD(neto) * 100) / (CVD(total))): LOCATE 18, 40: PRINT USING "#######,.## %"; CVD(entre$): LOCATE 18, 20: PRINT "Descuento Actual : "
				END IF
			END IF
			LOOP: salir = false
			IF llevas$ = "si" THEN
				DO WHILE NOT salir
					CALL Center(25, "Introduzca el monto deseado o [Enter] si est  Conforme")
					llevas$ = ""
					neto = MKD$(CVD(total) * (1 - (VAL(entrega) / 100)))
					LOCATE 19, 20: PRINT "Desea Redondear  ?"
					neto = MKD$(VAL(EntraDatoNumero(19, 39, 13, Trim$(STR$(FIX(CVD(neto)))), 0)))
					IF VAL(Redondeo) > 0 THEN
						IF CVD(neto) > 100 THEN
							neto = MKD$(VAL(Redondeo) * (FIX(CVD(neto) / VAL(Redondeo))))
						END IF
					END IF: salir = true
					entre$ = MKD$(100 - (CVD(neto) * 100) / CVD(total)): LOCATE 18, 40: PRINT USING "#######,.## %"; CVD(entre$): LOCATE 18, 20: PRINT "Descuento Actual : "
				LOOP
			END IF
	END IF
IF VAL(Redondeo) > 0 THEN
	IF CVD(neto) > 100 THEN
		neto = MKD$(VAL(Redondeo) * (FIX(CVD(neto) / VAL(Redondeo))))
	END IF
END IF
'anexa:
		LOCATE 18, 20: PRINT "Descuento (en Bs.):"; : PRINT USING "#########,.##  "; (CVD(total) - CVD(neto))
		'LOCATE 1, 1: PRINT USING "#######,.##"; CVD(neto); CVD(total)
		entre$ = MKD$(100 - (CVD(neto) * 100) / CVD(total)): IF CVD(descger) > 0 THEN LOCATE 19, 55: PRINT "(Gerencia)"
		LOCATE 19, 20: PRINT "Descuento ( en % ):"; : PRINT USING "#########,.## %"; CVD(entre$)
		LOCATE 22, 20: PRINT "Neto  a Pagar Bs. :"; : PRINT USING "#########,.##  "; (CVD(neto) + CVD(credit))
'paga:
		IF Fun(8) = "si" OR Fun(9) = "si" THEN
			LOCATE 23, 20: PRINT "¨El cliente Entrega?": salir = false: devolver = false
			CALL Center(25, "Tipee el monto [Esc]Realizar de nuevo el cierre [F10]Abortar operaci¢n")
			DO WHILE NOT salir
				entrega = EntraDatoNumero(23, 40, 13, " _           ", 0)
				IF VAL(entrega) >= (CVD(neto) + CVD(credit)) THEN salir = true: conforme% = true
				IF devolver = true THEN salir = true
				IF Fun(10) = "si" THEN
					OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #2 LEN = 100
					FIELD #2, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS Cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
					nursaler = LOF(2) / 100
					FOR k = 1 TO nursaler: GET #2, k
						IF Cedula$ = Cedu$ THEN RSET inout$ = " ": Cedula$ = clla$: PUT #2, k: EXIT FOR
					NEXT k: CLOSE : EXIT SUB
				END IF
			LOOP
			entre$ = MKD$(VAL(entrega) - CVD(neto) + CVD(credit))
			LOCATE 24, 20: PRINT "Su vuelto:-->>>-->"; : PRINT USING "#########,.##"; CVD(entre$); : PRINT " <<< Bs.  ";
		ELSE
			IF Fun(11) = "si" THEN
			salir = false: Center 25, "Introduzca Banco,Numero y Monto de Cheque y/o Efectivo"
			DO WHILE NOT salir
				CALL Box2(17, 15, 24, 65): LOCATE 21, 20: PRINT "Numero Cheque:": LOCATE 22, 20: PRINT "Monto Cheque:": LOCATE 23, 20: PRINT "Efectivo:"
				LOCATE 19, 20: PRINT "Neto  a Pagar Bs. :"; : PRINT USING "#########,.##  "; (CVD(neto) + CVD(credit))
				GOSUB BuscaBank
				numcheq = MKD$(VAL(EntraDatoNumero(21, 40, 13, " _           ", 0)))
				moncheq = MKD$(VAL(EntraDatoNumero(22, 40, 13, " _           ", 0)))
				entrega = EntraDatoNumero(23, 40, 13, " _           ", 0)
				IF (VAL(entrega) + CVD(moncheq)) >= (CVD(neto) + CVD(credit)) AND CVD(numcheq) > 0 THEN salir = true: conforme% = true
				IF devolver = true THEN salir = true
			LOOP
			entre$ = MKD$(VAL(entrega) + CVD(moncheq) - CVD(neto) + CVD(credit))
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
IF CVD(neto) < CVD(costa) THEN
	COLOR colors(7, colorpref) + 16, colors(4, colorpref): CALL Box2(17, 15, 24, 65): Center 17, "Error Administrativo"
	COLOR colors(7, colorpref), colors(4, colorpref): conforme% = false
	LOCATE 19, 20: PRINT "Neto de Factura   :"; : PRINT USING "#########,.## Bs."; CVD(neto)
	LOCATE 21, 20: PRINT "Costo de Factura  :"; : PRINT USING "#########,.## Bs."; CVD(costa)
	Center 23, "El Proceso de CIERRE se Repetira": Espera: Fun(8) = "no": IF prevcierr$ = "10" THEN Fun(10) = "si"
	salir = false: CALL Box2(17, 15, 24, 65): IF prevcierr$ = "9" THEN Fun(9) = "si"
END IF
LOOP
'abre: e imprime ojo
OPEN gav$ FOR RANDOM AS #27: PRINT #27, "A": CLOSE #27: Fun(9) = "no": GET #5, VAL(Yname1(cliente%))
nombre$ = Yname2(cliente%): domicilio1$ = direc15$: rifo$ = rif5$
nito$ = nit5$: domicilio2$ = direc25$: tlf$ = tele5$: salir = false: sit% = 1: GOSUB Hacecaja
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
IF Fun(9) = "si" THEN Fun(9) = "no": salir = true
LOOP
CLOSE #1: OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
CLOSE #4: OPEN fx FOR RANDOM AS #4 LEN = 38
FIELD #4, 1 AS acti$, 6 AS codi$, 2 AS canti$, 8 AS pvp$, 8 AS costo$, 1 AS psico$, 1 AS iva$, 1 AS auxi$, 8 AS fecha$, 2 AS lugar$
max% = LOF(4) / 38: imprint$ = "*": salir = false: devolver = false: lleva3% = 1: siono% = 0
	renglon = 0: neaux$ = MKD$(0):
	DO WHILE renglon < 26
		'PRINT lleva3%; renglon; max%
		IF lleva3% > max% THEN
			siono% = 777: EXIT DO
		ELSE
			GET #4, lleva3%
			IF auxi$ = "*" THEN
				IF codi$ <> "NOTADE" THEN
					GET #1, CVI(lugar$): renglon = renglon + 1
					desc1(renglon) = nomb2$: cant1(renglon) = canti$
					prec1(renglon) = pvp$: cost1(renglon) = costo$
					'PRINT lleva3%; renglon; max%
					ivaf1(renglon) = iva$: psic1(renglon) = psico$
					code1(renglon) = codi$: auxi% = auxi% + 1
					'PRINT lleva3%; renglon; max%
					neaux$ = MKD$(CVD(neaux$) + ((CVI(cant1(renglon)) * (CVD(prec1(renglon))))))
				END IF
				LSET acti$ = "*": PUT #4, lleva3%
			END IF
		END IF
		'PRINT acti$; nomb2$; CVD(pvp$)
		lleva3% = lleva3% + 1
	LOOP
		renglon = renglon + 1: IF renglon > 26 THEN renglon = 27
		descger = MKD$(CVD(neaux$) - CVD(neto))
		OPEN "F:\kevinprg\kevindat\maxfact2.dat" FOR RANDOM AS #7: FIELD #7, 4 AS FACTURA$
		GET #7, 1: numfct$ = FACTURA$: fct = numfct$: numfct$ = MKL$(CVL(numfct$) + 1)
		IF CVL(numfct$) > 999999 THEN numfct$ = MKL$(1)
		LSET FACTURA$ = numfct$: PUT #7, 1: CLOSE #7
		fx = "F:\kevinprg\kevindat\" + MID$(STR$(CVL(Cedula$)), 2, LEN(STR$(CVL(Cedula$)))) + ".caj"
		OPEN fx FOR RANDOM SHARED AS #17 LEN = 8: FIELD #17, 8 AS caja$: GET #17, 1: RSET caja$ = MKD$(CVD(caja$) + CVD(neto)): PUT #17, 1: CLOSE #17
		fx = "": fx = "F:\kevinprg\kevinvnt\" + MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4) + ".vnt"
		OPEN fx FOR RANDOM AS #17 LEN = 30: FIELD #17, 30 AS todo$: RSET todo$ = SPACE$(30): PUT #17, 1: CLOSE #17
		OPEN fx FOR RANDOM AS #17 LEN = 30: FIELD #17, 30 AS todo$: nursaler = LOF(17) / 30: nursaler = nursaler + 1
		RSET todo$ = "J" + MKI$(renglon - 1) + imprint$ + neto + MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MID$(TIME$, 7, 2) + Cedula$ + fct + MID$(descger, 1, 4)
		PUT #17, nursaler: salir = false: ': LOCATE 1, 1: PRINT nursaler; "   "; renglon: Espera
		FOR k = 1 TO renglon - 1
			RSET todo$ = ivaf1(k) + code1(k) + cant1(k) + prec1(k) + cost1(k) + psic1(k) + MID$(descger, 5, 4): IF psic1(k) = "*" THEN salir = true
			PUT #17, (nursaler + k)   ': LOCATE 1, 1: PRINT todo$: Espera
		NEXT k: CLOSE #17 ': LOCATE 1, 1: PRINT nursaler: Espera
		IF salir = true THEN
			OPEN "F:\kevinprg\kevindat\movpsico.dat" FOR RANDOM AS #17 LEN = 40: FIELD #17, 40 AS todo$: max2% = LOF(17) / 40: max2% = max2% + 1
			FOR k = 1 TO renglon - 1
			IF psic1(k) = "*" THEN RSET todo$ = "VENTA " + fct + DATE$ + TIME$ + code1$(k) + cant1(k) + Cedula$: PUT #17, max2%: max2% = max2% + 1
			NEXT k: CLOSE #17
		END IF
		OPEN "F:\kevinprg\kevindat\bitacora.dat" FOR RANDOM AS #17 LEN = 40: FIELD #17, 40 AS tod$: max2% = LOF(17) / 40
		FOR k = 1 TO renglon
		LSET tod$ = code1$(k) + MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4) + MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + cant1(k) + "01" + fct + Cedula$ + prec1(k)
		max2% = max2% + 1: PUT #17, max2%: NEXT k: CLOSE #17
		devolver = false: auxe$ = MKD$(0)
		IF imprint$ = "*" THEN
			OPEN "LPT1:" FOR RANDOM AS #10
			IF devolver = false THEN
			WIDTH #10, 255
			PRINT #10, CHR$(27); CHR$(64); : PRINT #10, CHR$(27); CHR$(64);
			PRINT #10, CHR$(15);
				 
			dia$ = MID$(DATE$, 4, 2) + "/" + MID$(DATE$, 1, 2) + "/" + MID$(DATE$, 7, 4): hora$ = TIME$
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
							 
			PRINT #10, "      Cliente:                               Vendedor:"; ClaveLarga$; SPACE$(11);
			PRINT #10, "Cliente:                               Vendedor:"; ClaveLarga$; CHR$(10);

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
							 
			'-- 1
			PRINT #10, "      "; SPACE$(45); STRING$(15, "="); SPACE$(9);
			PRINT #10, SPACE$(45); STRING$(15, "="); CHR$(10);
							 
			'-- 2
			PRINT #10, "      "; SPACE$(30); "Sub - Total: "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); SPACE$(9);
			PRINT #10, SPACE$(38); "Sub - Total: "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); : PRINT #10, CHR$(10);
							 
			'-- 3
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
							 
			'-- 4
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
							 
			'-- 5
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
							 
			'-- 6
			PRINT #10, SPACE$(6); SPACE$(30); "Neto Factura:"; : PRINT #10, USING "##########,.## Bs"; CVD(neto); : PRINT #10, SPACE$(9);
			PRINT #10, SPACE$(30); "Neto Factura:"; : PRINT #10, USING "##########,.## Bs"; CVD(neto); : PRINT #10, CHR$(10);
							 
			'-- 7
			PRINT #10, "      "; STRING$(60, "-"); SPACE$(9);
			PRINT #10, STRING$(60, "-"); CHR$(10);
							 
			'-- 8,9 y 10
			PRINT #10, "      "; Mensaje1; SPACE$(9);
			PRINT #10, Mensaje1; CHR$(10);
			PRINT #10, "      "; Mensaje2; SPACE$(9);
			PRINT #10, Mensaje2; CHR$(10);
			PRINT #10, "      "; Mensaje3; SPACE$(9);
			PRINT #10, Mensaje3; CHR$(10);
							 
							 
			PRINT #10, CHR$(12); : CLOSE #10
		  
			'PRINT #10, STRING$(60, "-"); CHR$(10);
			'IF encabezado = "si" THEN
			'   PRINT #10, "Farmacia:"; NombFarma; CHR$(10);
			'   PRINT #10, "RIF"; Rif; " NIT:"; Nit; CHR$(10);
			'   PRINT #10, "Telefono(s):"; telefono; CHR$(10);
			'   'OPEN "f:\kevinprg\kevindat\encabeza.dat" FOR INPUT SHARED AS #2: INPUT #2, encabezado: CLOSE
			'END IF
			'PRINT #10, USING "Factura:######,"; CVL(fct); : PRINT #10, " Fecha:"; MID$(DATE$, 4, 2); "/"; MID$(DATE$, 1, 2); "/"; MID$(DATE$, 7, 4); " Hora:"; TIME$;
			'PRINT #10, " Pagina:1 de "; : IF (renglon - 1) <= 11 THEN PRINT #10, "1"; CHR$(10);  ELSE PRINT #10, "2"; CHR$(10);
			'PRINT #10, "Cliente:                               Vendedor:"; ClaveLarga$; CHR$(10);
			'PRINT #10, nombre$; CHR$(10);
			'PRINT #10, "R.I.F.:"; rifo$; SPACE$(20 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(20 - LEN(rifo$)); CHR$(10);
			'PRINT #10, "Domicilio Fiscal:"; CHR$(10);
			'PRINT #10, domicilio1$; CHR$(10); : PRINT #10, domicilio2$; CHR$(10);
			'PRINT #10, "Telefonos:"; tlf$; SPACE$(20 - LEN(rifo$)); " Forma de Pago: Contado"; CHR$(10);
			'PRINT #10, STRING$(60, "-"); CHR$(10); : renglon = renglon - 1
			'PRINT #10, "³Descripcion                        ³Cantidad³Precio Venta³"; CHR$(10);
			'SELECT CASE renglon
			'   CASE 1 TO 8
			'      FOR k = 1 TO renglon
			'         PRINT #10, "³"; desc1(k); "³";
			'         PRINT #10, USING "#######,³"; CVI(cant1(k));
			'         IF ivaf1(k) = "*" THEN
			'            IF NuevoIva = "si" THEN
			'               PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
			'               auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100)))))
			'            ELSE
			'               PRINT #10, USING "########,.##³"; CVD(prec1(k));
			'               auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)))))
			'            END IF
			'         ELSE
			'            PRINT #10, USING "########,.##³"; CVD(prec1(k));
			'            auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(prec1(k))))
			'         END IF: PRINT #10, CHR$(10);
			'      NEXT k
			'   CASE 9 TO 21
			'      FOR k = 1 TO renglon
			'         PRINT #10, "³"; desc1(k); "³";
			'         PRINT #10, USING "#######,³"; CVI(cant1(k));
			'         IF ivaf1(k) = "*" THEN
			'            IF NuevoIva = "si" THEN
			'               PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
			'               auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100)))))
			'            ELSE
			'               PRINT #10, USING "########,.##³"; CVD(prec1(k));
			'               auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)))))
			'            END IF
			'         ELSE
			'            PRINT #10, USING "########,.##³"; CVD(prec1(k));
			'            auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(prec1(k))))
			'         END IF: PRINT #10, CHR$(10);
			'      NEXT k
			'      FOR k = 1 TO (21 - renglon): PRINT #10, SPACE$(k + 5); "\"; SPACE$(20); "\"; CHR$(10); : NEXT k
			'      PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
			'      PRINT #10, "VAN --> Bs "; : PRINT #10, USING "##########,.##"; CVD(auxe$); : PRINT CHR$(10);
			'      PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
			'      PRINT #10, CHR$(12); : PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
			'      PRINT #10, STRING$(60, "-"); CHR$(10);
			'      PRINT #10, USING "Factura:######,"; CVL(fct); : PRINT #10, " Fecha:"; MID$(DATE$, 4, 2); "/"; MID$(DATE$, 1, 2); "/"; MID$(DATE$, 7, 4); " Hora:"; TIME$;
			'      PRINT #10, " Pagina:2 de 2"; CHR$(10); : PRINT #10, "Cliente:                               Vendedor:"; Cedula$; CHR$(10);
			'      PRINT #10, nombre$; CHR$(10);
			'      PRINT #10, "R.I.F.:"; rifo$; SPACE$(20 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(20 - LEN(rifo$)); CHR$(10);
			'      PRINT #10, "Domicilio Fiscal:"; CHR$(10);
			'      PRINT #10, domicilio1$; CHR$(10); : PRINT #10, domicilio2$; CHR$(10);
			'      PRINT #10, "Telefonos:"; tlf$; SPACE$(20 - LEN(tlf$)); " Forma de Pago: Contado"; CHR$(10);
			'      PRINT #10, STRING$(60, "-"); CHR$(10);
			'      PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
			'      PRINT #10, "VIENEN->Bs "; : PRINT #10, USING "##########,.##"; CVD(auxe$); : PRINT CHR$(10);
			'      PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
			'   CASE 22 TO 26
			'      FOR k = 1 TO 21
			'         PRINT #10, "³"; desc1(k); "³";
			'         PRINT #10, USING "#######,³"; CVI(cant1(k));
			'         IF ivaf1(k) = "*" THEN
			'            IF NuevoIva = "si" THEN
			'               PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
			'               auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100)))))
			'            ELSE
			'               PRINT #10, USING "########,.##³"; CVD(prec1(k));
			'               auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)))))
			'            END IF
			'         ELSE
			'            PRINT #10, USING "########,.##³"; CVD(prec1(k));
			'            auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(prec1(k))))
			'         END IF: PRINT #10, CHR$(10);
			'      NEXT k
			'      PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
			'      PRINT #10, "VAN --> Bs "; : PRINT #10, USING "##########,.##"; CVD(auxe$); : PRINT CHR$(10);
			'      PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
			'      PRINT #10, CHR$(12); : PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
			'      PRINT #10, STRING$(60, "-"); CHR$(10);
			'      PRINT #10, USING "Factura:######,"; CVL(fct); : PRINT #10, " Fecha:"; MID$(DATE$, 4, 2); "/"; MID$(DATE$, 1, 2); "/"; MID$(DATE$, 7, 4); " Hora:"; TIME$;
			'      PRINT #10, " Pagina:2 de 2"; CHR$(10); : PRINT #10, "Cliente:                               Vendedor:"; Cedula$; CHR$(10);
			'      PRINT #10, nombre$; CHR$(10);
			'      PRINT #10, "R.I.F.:"; rifo$; SPACE$(20 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(20 - LEN(nito$)); CHR$(10);
			'      PRINT #10, "Domicilio Fiscal:"; CHR$(10);
			'      PRINT #10, domicilio1$; CHR$(10);
			'      PRINT #10, domicilio2$; CHR$(10);
			'      PRINT #10, "Telefonos:"; tlf$; SPACE$(20 - LEN(tlf$)); " Forma de Pago: Contado"; CHR$(10);
			'      PRINT #10, STRING$(60, "-"); CHR$(10);
			'      PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
			'      PRINT #10, "VIENEN->Bs "; : PRINT #10, USING "##########,.##"; CVD(auxe$); : PRINT CHR$(10);
			'      PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
			'      FOR k = 22 TO renglon
			'         PRINT #10, "³"; desc1(k); "³"; : PRINT #10, USING "#######,³"; CVI(cant1(k));
			'         IF ivaf1(k) = "*" THEN
			'            IF NuevoIva = "si" THEN
			'               PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
			'               auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100)))))
			'            ELSE
			'               PRINT #10, USING "########,.##³"; CVD(prec1(k));
			'               auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)))))
			'            END IF
			'         ELSE
			'            PRINT #10, USING "########,.##³"; CVD(prec1(k));
			'            auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(prec1(k))))
			'         END IF: PRINT #10, CHR$(10);
			'      NEXT k
			'END SELECT
			'PRINT #10, SPACE$(38); STRING$(15, "="); CHR$(10);
			'PRINT #10, SPACE$(24); "Sub - Total: "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); : PRINT #10, CHR$(10);
			'IF NuevoIva = "si" THEN
			'   PRINT #10, SPACE$(24); "- Descuento: "; : PRINT #10, USING "##########,.## Bs"; (CVD(auxe$) * (1 - (CVD(neto) / CVD(total)))); : PRINT #10, CHR$(10);
			'ELSE
			'   PRINT #10, SPACE$(24); "- Descuento: "; : PRINT #10, USING "##########,.## Bs"; (CVD(total) - CVD(neto)); : PRINT #10, CHR$(10);
			'END IF
			'IF ShowPerc = "si" THEN
			'   IF (100 - (100 * (CVD(neto) / CVD(total)))) > 0 THEN
			'      PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
			'      PRINT #10, "    % descuento:"; : PRINT #10, USING "###.##  %"; (100 - (100 * (CVD(neto) / CVD(total)))); : PRINT #10, CHR$(10);
			'      PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
			'   END IF
			'END IF
			'IF NuevoIva = "si" THEN
			'   PRINT #10, SPACE$(18); "+I. V. A.("; impuesto; "%):"; : PRINT #10, USING "##########,.## Bs"; CVD(neto) - (CVD(auxe$) * (CVD(neto) / CVD(total))); : PRINT #10, CHR$(10);
			'END IF
			'PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
			'PRINT #10, "Neto Factura"; : PRINT #10, USING "##########,.## Bs"; CVD(neto); : PRINT #10, CHR$(10);
			'PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
			'PRINT #10, STRING$(60, "-"); CHR$(10);
			'PRINT #10, SPACE$((60 - LEN(Trim$(Mensaje1))) / 2); Mensaje1; CHR$(10);
			'PRINT #10, SPACE$((60 - LEN(Trim$(Mensaje2))) / 2); Mensaje2; CHR$(10);
			'PRINT #10, SPACE$((60 - LEN(Trim$(Mensaje3))) / 2); Mensaje3; CHR$(10);
			'PRINT #10, STRING$(60, "-"); CHR$(10);
			'PRINT #10, CHR$(12); : CLOSE #10
			END IF
		END IF
OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #17 LEN = 100
FIELD #17, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS Cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
nursaler = LOF(17) / 100
FOR k = 1 TO nursaler: GET #17, k
IF Cedula$ = Cedu$ THEN RSET inout$ = " ": Cedula$ = clla$: PUT #17, k: EXIT FOR
NEXT k: CLOSE #17
EXIT SUB
RETURN    ' FIN CIERRE-----FIN CIERRE-----FIN CIERRE------

BuscaBank:
CALL Center(25, " *** Espere , Por Favor ... ***")
IF ybusqueda <> 6 THEN
	CLOSE #5: OPEN "F:\kevinprg\kevindat\bank.dat" FOR RANDOM SHARED AS #5 LEN = 18: FIELD #5, 2 AS codigo$, 15 AS nomb$
	FOR k = 1 TO 199: GET #5, k: Yname1(k) = codigo$: Yname2(k) = nomb$: NEXT k: CLOSE #5: ybusqueda = 6
END IF
LOCATE 20, 20: PRINT "Elija el Banco :  ²"; Yname1(1); "²"; Yname2(1)
mjfox% = Elije%(3, 60, 20, 39, 6): IF mjfox% < 1 THEN : RETURN
numbank = MKI$(VAL(Yname1(mjfox%))): LOCATE 20, 20: PRINT "Banco  Elegido:     ²"; Yname1(mjfox%); "²"; Yname2(mjfox%)

Hacecaja:
COLOR colors(7, colorpref), colors(4, colorpref): Box2 10, 8, 20, 72: Center 10, "Datos del Cliente": Center 25, "[F2]Elije Cliente Fijo [F9] Salir e Imprimir"
LOCATE 12, 10: PRINT "Nombre  ¢ Raz¢n : ²"; nombre$; "²": LOCATE 13, 10: PRINT "Domicilio Fiscal:"
LOCATE 14, 10: PRINT "²"; domicilio1$; "²": LOCATE 15, 10: PRINT "²"; domicilio2$; "²"
LOCATE 16, 10: PRINT "R.I.F.: ²"; rifo$; "²": LOCATE 16, 40: PRINT "Telefono: ²"; tlf$; "²"
LOCATE 18, 10: PRINT "N.I.T.: ²"; nito$; "²": LOCATE 18, 40: PRINT "Forma de Pago: CONTADO"
RETURN

END SUB

FUNCTION Elije% (fila%, columna%, fila2%, columna2%, edo%)
COLOR colors(7, colorpref), colors(4, colorpref): CALL Box(fila%, columna%, fila% + 16, columna% + 36): CALL Center(25, "[F1] Ayuda [Enter]Tomar Opcion [A-Z]Ir a... [Esc]Menu Anterior"): COLOR colors(4, colorpref), colors(7, colorpref): CLOSE #5
SELECT CASE edo%
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
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna%: PRINT "²"; elccion$; "²": IF sonido = "si" THEN BEEP
EntraDatoNumero$ = elccion$: EXIT FUNCTION
END FUNCTION

SUB Espera
Center 25, "Presione Cualquier Tecla Para Continuar...": DO: LOOP WHILE INKEY$ = ""
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

