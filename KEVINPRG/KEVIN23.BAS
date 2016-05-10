' calcula totales del mes
DEFINT A-Z
DECLARE SUB LeeEstado ()
DECLARE SUB Visualiza ()
DECLARE SUB Center (row, text$)
DECLARE SUB Box2 (Row1, Col1, Row2, Col2)
DECLARE SUB Espera ()
DECLARE SUB Termometro (va%, total%, edo%, text$, fila%, columna%)
DECLARE FUNCTION CheqDate (caden$, op%)
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$, tr$)
DECLARE FUNCTION Trim$ (x$)
COMMON SHARED guia, colorpref
DIM SHARED fx AS STRING * 33, turno, costo AS STRING * 8, bruto AS STRING * 8, neto AS STRING * 8, DescGer AS STRING * 8
DIM SHARED totalbruto AS STRING * 8, totalneto AS STRING * 8, totalsin AS STRING * 8, totalcon AS STRING * 8, totalcosto AS STRING * 8
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, impuesto AS STRING * 6, nombfarma AS STRING * 25, Direccion AS STRING * 30, rif     AS STRING * 25, nit      AS STRING * 25, telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2
DIM SHARED PrintErr AS INTEGER, devolver, Fun(17) AS STRING * 2, sitio, salir, este, proximo
CONST true = -1: CONST false = NOT true: turno = false
CALL LeeEstado: CALL Visualiza: CLOSE
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box2 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... ": COLOR colors(7, colorpref), colors(4, colorpref)
CHAIN "F:\kevinprg\kevin01.bas"
END

ErrorTrap:
SELECT CASE ERR
     CASE 53
          Box2 15, 10, 20, 70: Center 15, "Mensaje"
          Center 17, "NO Se Encontro el Archivo de Ventas:"
          Center 18, "Cerciorese de Que Ese Dia Se Hayan Registrado Ventas"
          Center 19, "Solicite de Nuevo el Archivo.": Espera: devolver = true
          RESUME NEXT
     CASE 71
          Box2 10, 10, 20, 70: Center 10, "Mensaje"
          Center 11, "Diskette NO Esta Listo.Por Favor Introduzcalo"
          Center 12, "Presion Escape Para Abortar."
          Center 17, "Presione Cualquier otra Tecla Para REINTENTAR."
          kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
          IF kbd$ = CHR$(27) THEN devolver = true: RESUME NEXT ELSE RESUME
     CASE 57
          RESUME NEXT
     CASE 25, 27, 57, 68
          Box2 10, 10, 20, 70: Center 10, "Mensaje"
          Center 11, "La Impresora NO Responde. Por Favor Verifique Que:"
          Center 12, "1.- Tenga Papel y que No Este Atascado.           "
          Center 13, "2.- Tenga Energia Electrica y/o Este Conectada.   "
          Center 14, "3.- El Cable de Datos Este Conectado y en Linea.  "
          Center 16, "Si Todo Esto NO Funciona Presione Escape y Proceda"
          Center 17, "a Llenar Manualmente la Factura ; Presione   Cual-"
          Center 18, "quier otra Tecla Para REINTENTAR.                 "
          kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
          IF kbd$ = CHR$(27) THEN devolver = true: RESUME NEXT ELSE RESUME
     CASE 70
          Center 25, "Esperando Acceso a La Red , Espere Por Favor . . .": SLEEP 1: RESUME
     CASE ELSE
END SELECT
RESUME NEXT
ErrorTrop:
SELECT CASE ERR
     CASE 25, 27, 57, 68
          Box2 5, 2, 7, 78: Center 5, "Mensaje": Center 6, "No Se Detect¢ Gavetero;NO DEBE Usar Esta Estacion Para Ventas de Contado": RESUME NEXT
     CASE ELSE
END SELECT
RESUME NEXT
ErrorTrep:
SELECT CASE ERR
     CASE 68
           Box2 10, 10, 20, 70: Center 10, "Mensaje": Center 15, " Dispositivo NO Disponible : Ahora Se Saldra al Menu Principal . . .": Espera: devolver = true: RESUME NEXT
END SELECT
RESUME NEXT
DATA 0,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  2,     0,     1,     15,     0,     7,      0
DATA 3,    15,   13,  1,     14,    3,     15,     0,     7,      0
DATA 7,    12,   15,  4,     14,    0,     15,     15,    1,      0
DATA 02,03,04,05,06,07,08,09,10
DATA 11,12,13,14,15,16,17,18,19,20
DATA 21,22,23,24,25,26,27,28,29,30,31,32

SUB Box2 (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "²"; STRING$(BoxWidth - 2, "²"); "²"; : FOR a = Row1 + 1 TO Row2 - 1: LOCATE a, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Center (row, text$)
IF row = 25 THEN COLOR 0, 15: LOCATE 25, 1: PRINT SPACE$(80);
LOCATE row, 41 - LEN(text$) / 2: PRINT text$;
END SUB

FUNCTION CheqDate (caden$, op%)
CheqDate = true: IF caden$ = "        " THEN EXIT FUNCTION
dd% = VAL(MID$(caden$, 1, 2)): mm% = VAL(MID$(caden$, 4, 2)): aa% = VAL(MID$(caden$, 7, 4))
dd2% = VAL(MID$(DATE$, 4, 2)): mm2% = VAL(MID$(DATE$, 1, 2)): aa2% = VAL(MID$(DATE$, 7, 4))
IF dd% < 1 THEN CheqDate = false: EXIT FUNCTION
IF mm% < 1 OR mm% > 12 THEN CheqDate = false: EXIT FUNCTION
IF aa% < 1996 OR aa% > 2020 THEN CheqDate = false: EXIT FUNCTION
IF mm% = 2 AND dd% > 28 THEN CheqDate = false: EXIT FUNCTION
IF mm% = 1 OR mm% = 3 OR mm% = 5 OR mm% = 7 OR mm% = 8 OR mm% = 10 OR mm% = 12 THEN IF dd% > 31 THEN CheqDate = false: EXIT FUNCTION
IF mm% = 4 OR mm% = 6 OR mm% = 9 OR mm% = 11 THEN IF dd% > 30 THEN CheqDate = false: EXIT FUNCTION
IF op% = 2 THEN
     IF aa% < aa2% THEN CheqDate = false: EXIT FUNCTION
     IF aa% = aa2% THEN IF mm% < mm2% THEN CheqDate = false: EXIT FUNCTION
     IF aa% = aa2% THEN IF mm% = mm2% THEN IF dd% < dd2% THEN CheqDate = false: EXIT FUNCTION
END IF
END FUNCTION

SUB Espera
Center 25, "Presione Cualquier Tecla Para Continuar...": DO: LOOP WHILE INKEY$ = ""
END SUB

SUB LeeEstado
CLOSE : OPEN "F:\kevinprg\kevindat\kaos.dat" FOR INPUT AS #2
INPUT #2, sonido: INPUT #2, impuesto: INPUT #2, nombfarma: INPUT #2, Direccion: INPUT #2, rif: INPUT #2, nit: INPUT #2, telefono: INPUT #2, contri: INPUT #2, contra: INPUT #2, Stock: INPUT #2, Blanq
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
eleccion$ = "": eleccion(1) = UCASE$(eleccion(1)): FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR colors(5, colorpref), colors(4, colorpref): LOCATE fila%, columna% + 1: PRINT eleccion$
MeteDatoLetra$ = eleccion$: IF sonido = "si" THEN BEEP
EXIT FUNCTION
Sumar:
FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR 0, 15: LOCATE fila%, columna%: PRINT "²"; eleccion$; "²"
RETURN
END FUNCTION

SUB Termometro (va%, total%, edo%, text$, fila%, columna%)
IF edo% = 0 THEN Box2 fila%, columna%, fila% + 6, columna% + 31
LOCATE fila% + 1, columna% + 1: PRINT "0%                        100%"
IF edo% = 0 THEN LOCATE fila% + 2, columna% + 1: PRINT STRING$(30, "±")
LOCATE fila% + 2, columna% + 1: PRINT STRING$(INT((va% / total%) * 30), "Û")
LOCATE fila% + 3, columna% + 1: PRINT "Van"; va%; " de "; total%;
LOCATE fila% + 4, columna% + 9: PRINT USING "##.## % Hecho "; ((va% / total%) * 100)
LOCATE fila% + 5, columna% + 1: PRINT text$
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

SUB Visualiza
DIM promedio AS DOUBLE
CLOSE : OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #1 LEN = 100
FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
selecc% = LOF(1) / 100: selecc% = selecc% + 1: DIM nombre(selecc%) AS STRING * 20, apellido(selecc%) AS STRING * 20, Cedula(selecc%) AS STRING * 4, clave(selecc%) AS STRING * 10, subven(selecc%) AS STRING * 8
FOR k = 1 TO selecc% - 1: GET #1, k: nombre(k) = nomb$: apellido(k) = apel$: Cedula(k) = cedu$: clave(k) = clla$: NEXT k: CLOSE
nombre(selecc%) = "Todos Los Vendedores": apellido(selecc%) = "": Cedula(selecc%) = MKL$(0)
IF selecc% = 0 THEN CALL Center(8, "** No Hay Ningun Vendedor Registrado **"): Espera: EXIT SUB
totalbruto = MKD$(0): totalneto = MKD$(0): totalsin = MKD$(0)
totalcon = MKD$(0): totalcosto = MKD$(0): dias% = 0
Box2 1, 5, 3, 75: Center 2, "Totalizacion de Ventas Hechas En Un Mes": Center 25, "Emplee el formato mm/aaaa [F9]Escape": hoy$ = MID$(DATE$, 1, 2) + "/" + MID$(DATE$, 7, 4)
COLOR colors(7, colorpref), colors(4, colorpref): escoge% = selecc%: Box2 4, 10, 22, 70: Center 5, "Introduzca Mes y A¤o": salir = false
DO WHILE NOT salir
     dia$ = MeteDatoLetra$(6, 35, 7, hoy$, "0")
     IF VAL(MID$(dia$, 1, 2)) > 0 AND VAL(MID$(dia$, 1, 2)) < 13 THEN
          IF VAL(MID$(dia$, 4, 4)) > 1997 AND VAL(MID$(dia$, 4, 4)) < 2017 THEN salir = true
     END IF
LOOP
IF Fun(9) = "si" THEN EXIT SUB
die$ = MID$(dia$, 1, 2) + MID$(dia$, 4, 4)
'DO
'     Center 9, "Desea Calcular el dia Primero de ESTE Mes ?"
'     Center 10, "Pulse -S- ¢ -N- para salir "
'     llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND: llave$ = UCASE$(llave$)
'     IF llave$ = "S" THEN este = true: EXIT DO
'     IF llave$ = "N" THEN este = false: EXIT DO
'     BEEP
'LOOP
'DO
'     Center 11, "Desea Calcular el dia Primero del PROXIMO Mes ?"
'     Center 12, "Pulse -S- ¢ -N- para salir "
'     llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND: llave$ = UCASE$(llave$)
'     IF llave$ = "S" THEN proximo = true: EXIT DO
'     IF llave$ = "N" THEN proximo = false: EXIT DO
'     BEEP
'LOOP
este = true: proximo = false
IF este = true THEN cambdia$ = "01": GOSUB LeeDia
CALL Termometro(0, 31, 0, "Calculando cada dia...", 10, 10)
Center 25, "Buscando , Espere Por Favor . . ."
DO WHILE VAL(cambdia$) < 32
     READ cambdia$: GOSUB LeeDia
LOOP
IF proximo = true THEN
     aux% = VAL(MID$(die$, 1, 2)): cambdia$ = "01"
     aux% = aux% + 1: IF aux% > 13 THEN aux% = 1
     SELECT CASE aux%
          CASE 1: die$ = "01" + MID$(die$, 3, 4): GOSUB LeeDia
          CASE 2: die$ = "02" + MID$(die$, 3, 4): GOSUB LeeDia
          CASE 3: die$ = "03" + MID$(die$, 3, 4): GOSUB LeeDia
          CASE 4: die$ = "04" + MID$(die$, 3, 4): GOSUB LeeDia
          CASE 5: die$ = "05" + MID$(die$, 3, 4): GOSUB LeeDia
          CASE 6: die$ = "06" + MID$(die$, 3, 4): GOSUB LeeDia
          CASE 7: die$ = "07" + MID$(die$, 3, 4): GOSUB LeeDia
          CASE 8: die$ = "08" + MID$(die$, 3, 4): GOSUB LeeDia
          CASE 9: die$ = "09" + MID$(die$, 3, 4): GOSUB LeeDia
          CASE 10: die$ = "10" + MID$(die$, 3, 4): GOSUB LeeDia
          CASE 11: die$ = "11" + MID$(die$, 3, 4): GOSUB LeeDia
          CASE 12: die$ = "12" + MID$(die$, 3, 4): GOSUB LeeDia
     END SELECT
END IF
     Box2 1, 5, 3, 75: Center 2, SPACE$(65): Center 2, ("Vendedor y Dia Seleccionado:" + Trim$(nombre(escoge%)) + " " + dia$)
     COLOR colors(7, colorpref), colors(4, colorpref): subven(selecc%) = MKD$(0)
     Box2 4, 1, 24, 80: Center 4, "Vendedores Registrados"
     LOCATE 5, 2: PRINT "Nombres:             Apellidos:         Total:": puntero% = 6
     FOR k = 1 TO (selecc% - 1)
          IF CVD(subven(k)) > 0 THEN
               LOCATE puntero%, 2: PRINT nombre(k); " "; apellido(k);
               PRINT USING "################,.##"; CVD(subven(k))
               puntero% = puntero% + 2: subven(selecc%) = MKD$(CVD(subven(selecc%)) + CVD(subven(k)))
          END IF
     NEXT k: LOCATE 23, 2: PRINT "Total NETO de todos los Vendedores  :  "; : PRINT USING "################,.##"; CVD(subven(selecc%))
     CALL Espera: COLOR colors(7, colorpref), colors(4, colorpref)
     Box2 4, 1, 24, 80: Center 4, "Totales Generales del Mes:"
     LOCATE 6, 2: PRINT "   Total Ventas SIN I.C.S.V.M.  :";
     PRINT USING "################,.##"; CVD(totalsin);
     LOCATE 8, 2: PRINT "   Total Ventas CON I.C.S.V.M.  :";
     PRINT USING "################,.##"; CVD(totalcon);
     LOCATE 9, 35: PRINT STRING$(20, "=");
     LOCATE 10, 2: PRINT "   Total Ventas BRUTAS          :";
     PRINT USING "################,.##"; CVD(totalbruto);
     LOCATE 12, 2: PRINT "   Total Descuento de Ventas    :";
     PRINT USING "+###############,.##"; -1 * (CVD(totalbruto) - CVD(totalneto));
     LOCATE 13, 35: PRINT STRING$(20, "=");
     LOCATE 14, 2: PRINT "   Total NETO de Ventas del mes :";
     PRINT USING "################,.##"; CVD(totalneto);
     LOCATE 16, 2: PRINT "   Total COSTO de Ventas del mes:";
     PRINT USING "+###############,.##"; -1 * CVD(totalcosto);
     LOCATE 17, 35: PRINT STRING$(20, "=");
     LOCATE 18, 2: PRINT "   Total GANANCIA BRUTA del mes :";
     PRINT USING "################,.##"; (CVD(totalneto) - CVD(totalcosto));
     LOCATE 20, 2: PRINT "   (promedio diario de venta)   :";
     PRINT USING "################,.##"; (CVD(totalneto) / dias%);
     CALL Espera: Center 25, "¨ Desea un reporte escrito en papel ? Pulse -S- o -N-"
     COLOR colors(7, colorpref), colors(4, colorpref): Box2 8, 4, 11, 76: Center 9, "Desea Imprimir Reporte ?"
     Center 10, "Pulse -S- para imprimir , cualquier otra tecla para salir "
     llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND: llave$ = UCASE$(llave$)
     IF llave$ = "S" THEN
          CLOSE : OPEN "lpt1" FOR RANDOM AS #10: WIDTH #10, 255
          PRINT #10, "   Farmacia: "; nombfarma; CHR$(10); : PRINT #10, CHR$(10);
          PRINT #10, "   R.I.F. :"; rif
          PRINT #10, CHR$(10);
          PRINT #10, "         Fecha: "; dia$; CHR$(10); : PRINT #10, CHR$(10);
          PRINT #10, "         Hora: "; TIME$
          PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
          PRINT #10, "   Reporte Total de Ventas del mes:"; dia$; CHR$(10);
          PRINT #10, STRING$(42, "="); CHR$(10);
          PRINT #10, "Vendedor:               Total:"
          FOR k = 1 TO (selecc% - 1)
          IF CVD(subven(k)) > 0 THEN
               PRINT #10, nombre(k); "  ";
               PRINT #10, USING "Bs #############,.##"; CVD(subven(k));
               PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
          END IF
          NEXT k
          PRINT #10, "Total NETO de todos los Vendedores  :  "; CHR$(10);
          PRINT #10, USING "Bs ###############,.##"; CVD(subven(selecc%));
          PRINT #10, CHR$(10);
          PRINT #10, STRING$(42, "="); CHR$(10);
          PRINT #10, CHR$(12);
         
          PRINT #10, STRING$(43, "="); CHR$(10); CHR$(10);
          PRINT #10, "   Farmacia: "; nombfarma; CHR$(10); : PRINT #10, CHR$(10);
          PRINT #10, "   R.I.F. :"; rif; : PRINT #10, CHR$(10);
          PRINT #10, "   Totales Generales del Mes:    "; CHR$(10); CHR$(10);
          PRINT #10, "         Fecha: "; dia$; CHR$(10); : PRINT #10, CHR$(10);
          PRINT #10, "Total Ventas SIN I.V.A.      :";
          PRINT #10, USING "#########,.##"; CVD(totalsin)
          PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
          PRINT #10, "Total Ventas CON I.V.A.      :";
          PRINT #10, USING "#########,.##"; CVD(totalcon)
          PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
          PRINT #10, "Total Ventas BRUTAS          :";
          PRINT #10, USING "#########,.##"; CVD(totalbruto)
          PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
          PRINT #10, "Total Descuento de Ventas    :";
          PRINT #10, USING "+########,.##"; -(CVD(totalbruto) - CVD(totalneto))
          PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
          PRINT #10, "Total NETO de Ventas del mes :";
          PRINT #10, USING "+########,.##"; CVD(totalneto)
          PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
          PRINT #10, "Total COSTO de Ventas del mes:";
          PRINT #10, USING "+########,.##"; -CVD(totalcosto)
          PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
          PRINT #10, "Total UTILIDAD BRUTA del mes :";
          PRINT #10, USING "#########,.##"; (CVD(totalneto) - CVD(totalcosto))
          PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
          PRINT #10, USING "Dias  #############,"; dias%;
          PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
          PRINT #10, STRING$(43, "="); CHR$(10);
          PRINT #10, CHR$(12);
          CLOSE
     END IF
EXIT SUB
LeeDia:
     CLOSE : fx = "f:\kevinprg\kevinvnt\" + cambdia$ + die$ + ".vnt": devolver = false
     OPEN fx FOR RANDOM AS #1 LEN = 30: FIELD #1, 30 AS all$: maxi% = LOF(1) / 30
     ini% = 1: puntero% = 2: salir = false: CALL Termometro(VAL(cambdia$), 31, 1, "Calculando cada dia...", 10, 10)
     IF maxi% > 1 THEN
          dias% = dias% + 1
          DO WHILE NOT salir
               GET #1, puntero%
               IF MID$(all$, 1, 1) <> "J" THEN Box2 10, 10, 20, 70: Center 15, "Archivo Corrupto , Llame a Kevin Software 7000": Center 17, "Tan Pronto Como Le Sea Posible": Espera: EXIT SUB
               bruto = MKD$(0): neto = (MID$(all$, 5, 8)): costo = MKD$(0)
               FOR k = 1 TO selecc%
                    IF CVL(MID$(all$, 19, 4)) = CVL(Cedula(k)) THEN vanotar% = k
               NEXT k
               otropuntero% = puntero% + CVI(MID$(all$, 2, 2))
               FOR k = puntero% + 1 TO otropuntero%
                    GET #1, k
                    costo = MKD$(CVD(costo) + (CVD(MID$(all$, 18, 8)) * CVI(MID$(all$, 8, 2))))
                    bruto = MKD$(CVD(bruto) + (CVD(MID$(all$, 10, 8)) * CVI(MID$(all$, 8, 2))))
                    IF MID$(all$, 1, 1) = " " THEN
                         totalsin = MKD$(CVD(totalsin) + (CVD(MID$(all$, 10, 8)) * CVI(MID$(all$, 8, 2))))
                    ELSE
                         totalcon = MKD$(CVD(totalcon) + (CVD(MID$(all$, 10, 8)) * CVI(MID$(all$, 8, 2))))
                    END IF
               NEXT k
               subven(vanotar%) = MKD$(CVD(subven(vanotar%)) + CVD(neto))
               totalbruto = MKD$(CVD(totalbruto) + CVD(bruto))
               totalneto = MKD$(CVD(totalneto) + CVD(neto))
               totalcosto = MKD$(CVD(totalcosto) + CVD(costo))
               SWAP puntero%, otropuntero%
               puntero% = puntero% + 1
               IF puntero% >= maxi% THEN EXIT DO
          LOOP
     END IF
     fx = "": fx = "F:\kevinprg\kevindat\" + cambdia$ + die$ + ".crd"
     OPEN fx FOR RANDOM AS #17 LEN = 30: FIELD #17, 30 AS todo$
     nursaler = LOF(17) / 30: k = 1
     IF nursaler > 0 THEN
               DO
                    GET #17, k: mas% = CVI(MID$(todo$, 2, 2)): neto = (MID$(todo$, 5, 8))
                    IF MID$(todo$, 1, 1) = "J" THEN
                         FOR l = 1 TO selecc%
                              IF CVL(MID$(todo$, 19, 4)) = CVL(Cedula(l)) THEN vanotar% = l
                         NEXT l
                    END IF
                    costo = MKD$(0): bruto = MKD$(0)
                    FOR l = (k + 1) TO (k + CVI(MID$(todo$, 2, 2)))
                         GET #17, l
                         costo = MKD$(CVD(costo) + (CVD(MID$(todo$, 18, 8)) * CVI(MID$(todo$, 8, 2))))
                         bruto = MKD$(CVD(bruto) + (CVD(MID$(todo$, 10, 8)) * CVI(MID$(todo$, 8, 2))))
                         IF MID$(todo$, 1, 1) = " " THEN
                              totalsin = MKD$(CVD(totalsin) - (CVD(MID$(todo$, 10, 8)) * CVI(MID$(todo$, 8, 2))))
                         ELSE
                              totalcon = MKD$(CVD(totalcon) - (CVD(MID$(todo$, 10, 8)) * CVI(MID$(todo$, 8, 2))))
                         END IF
                    NEXT l
                    IF neto = SPACE$(8) THEN neto = bruto
                    subven(vanotar%) = MKD$(CVD(subven(vanotar%)) - CVD(bruto))
                    totalbruto = MKD$(CVD(totalbruto) - CVD(bruto))
                    totalneto = MKD$(CVD(totalneto) - CVD(neto))
                    totalcosto = MKD$(CVD(totalcosto) - CVD(costo))
                    k = (1 + k + mas%)
                    IF k > nursaler THEN EXIT DO
               LOOP
     END IF
     CLOSE
RETURN
END SUB

