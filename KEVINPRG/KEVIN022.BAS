'corta caja -z- o subtotal -x- de caja
DEFINT A-Z
DECLARE SUB LeeEstado ()
DECLARE SUB Visualiza ()
DECLARE SUB Center (row, text$)
DECLARE SUB Box2 (Row1, Col1, Row2, Col2)
DECLARE SUB Espera ()
DECLARE FUNCTION CheqDate (caden$, op%)
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$, tr$)
DECLARE FUNCTION Trim$ (x$)
COMMON SHARED guia, colorpref
DIM SHARED fx AS STRING * 33, turno, costo AS STRING * 8, bruto AS STRING * 8
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, impuesto AS STRING * 6, NombFarma AS STRING * 25, Direccion AS STRING * 30, rif     AS STRING * 25, nit      AS STRING * 25, telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2
DIM SHARED PrintErr AS INTEGER, devolver, Fun(17) AS STRING * 2, sitio, salir, grabGav AS STRING * 1
ON TIMER(5) GOSUB AbreGav
TIMER ON
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
          Center 16, "Si Todo Esto NO Funciona Presione Escape  Para    "
          Center 17, "Salir sin Imprimir Documento    ; Presione   Cual-"
          Center 18, "quier otra Tecla Para REINTENTAR.                 "
          kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
          IF kbd$ = CHR$(27) THEN devolver = true: RESUME NEXT ELSE RESUME
     CASE 70
          Box2 10, 10, 20, 70: Center 10, "Mensaje": Center 15, "Esperando Acceso a La Red , Espere Por Favor . . .": SLEEP 1: RESUME
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
INPUT #2, sonido: INPUT #2, impuesto: INPUT #2, NombFarma: INPUT #2, Direccion: INPUT #2, rif: INPUT #2, nit: INPUT #2, telefono: INPUT #2, contri: INPUT #2, contra: INPUT #2, Stock: INPUT #2, Blanq
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
ON ERROR GOTO ErrorTrap
CLOSE : OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #1 LEN = 100
FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
max% = LOF(1) / 100: selecc% = 0: COLOR colors(7, colorpref), colors(4, colorpref): CLS :
IF max% = 0 THEN CALL Center(8, "** No Hay Ningun Vendedor Registrado **"): Espera: EXIT SUB
FOR k = 1 TO max%
     GET #1, k: IF acti$ = " " THEN selecc% = selecc% + 1
NEXT k: selecc% = selecc% + 1: DIM nombre(selecc%) AS STRING * 20, apellido(selecc%) AS STRING * 20, cedula(selecc%) AS STRING * 4, seli(selecc%) AS STRING * 1, monto(selecc%) AS STRING * 8: van% = 1
FOR k = 1 TO max%
     GET #1, k
     IF acti$ = " " THEN GET #1, k: nombre(van%) = nomb$: apellido(van%) = apel$: cedula(van%) = cedu$: monto(van%) = MKD$(0): van% = van% + 1
NEXT k: CLOSE : fila% = 6
nombre(selecc%) = "Todos Los Vendedores": apellido(selecc%) = "": cedula(selecc%) = MKL$(0)
     Fun(12) = "si": Center 25, "[F1]Ayuda  [Enter](Des)Marcar  [Esc]Continuar  [F9]Abortar": GOSUB Mostrador: Center 25, "Informe el monto entregado (DEBE SER MAYOR A CERO)"
     Box2 17, 10, 23, 70: LOCATE 18, 11: PRINT "Introduzca Monto Total Entregado:": salir = false
     DO WHILE NOT salir
     dia$ = MeteDatoLetra$(18, 46, 15, " 0             ", "1")
     IF VAL(dia$) > 0 THEN salir = true
     LOOP: total$ = MKD$(0): LOCATE 18, 46: PRINT USING "²###########,.##²"; VAL(dia$)
     FOR k = 1 TO (selecc% - 1)
          IF seli(k) = "*" THEN
               fx = "F:\kevinprg\kevindat\" + MID$(STR$(CVL(cedula(k))), 2, LEN(STR$(CVL(cedula(k))))) + ".caj"
               CLOSE : OPEN fx FOR RANDOM AS #17 LEN = 8: FIELD #17, 8 AS caja$: GET #17, 1
               total$ = MKD$(CVD(total$) + CVD(caja$)): monto(k) = caja$
               IF guia = 2 THEN RSET caja$ = MKD$(0): PUT #17, 1
          END IF
     NEXT k: COLOR colors(4, colorpref), colors(7, colorpref)
     LOCATE 20, 11: PRINT "Suma  Total  de  los  Vendedores:  "; : PRINT USING "²###########,.##²"; CVD(total$)
     LOCATE 21, 46: PRINT STRING$(17, "=")
     IF VAL(dia$) > CVD(total$) THEN LOCATE 22, 11: PRINT "Hay  Un   EXCEDENTE  de  Dinero :  ";
     IF VAL(dia$) = CVD(total$) THEN LOCATE 22, 11: PRINT "Caja  EXACTA  ,  Buen  Trabajo! :  ";
     IF VAL(dia$) < CVD(total$) THEN LOCATE 22, 11: PRINT "Hay  Un   FALTANTE   de  Dinero :  ";
     PRINT USING "²###########,.##²"; (VAL(dia$) - CVD(total$))
     ' guarda en un archivo los cortes X y Z
     CLOSE #10: OPEN "f:\kevinprg\cortes.txt" FOR APPEND AS #10
     PRINT #10, STRING$(60, "-"); CHR$(10);
     PRINT #10, "Farmacia:"; NombFarma; CHR$(10);
     IF guia = 1 THEN PRINT #10, "<Reporte de Caja Opcion -X->";  ELSE PRINT #10, "Reporte de Caja Opcion -Z-:";
     PRINT #10, "Fecha:"; MID$(DATE$, 4, 2); "/"; MID$(DATE$, 1, 2); "/"; MID$(DATE$, 7, 4); " Hora:"; TIME$; CHR$(10);
     PRINT #10, SPACE$(14); "Montos Detallados Por Vendedor:"; CHR$(10);
     PRINT #10, STRING$(60, "-"); CHR$(10);
     FOR l = 1 TO (selecc% - 1)
          IF seli(l) = "*" THEN
               PRINT #10, "³"; nombre(l); "³"; apellido(l); "³";
               PRINT #10, USING "**###########,.##³"; CVD(monto(l));
               PRINT #10, CHR$(10);
          END IF
     NEXT l
     PRINT #10, SPACE$(43); STRING$(17, "="); CHR$(10);
     PRINT #10, SPACE$(28); "Total de Ventas: "; : PRINT #10, USING "**########,.## Bs"; CVD(total$); : PRINT #10, CHR$(10);
     PRINT #10, SPACE$(28); "Monto Entregado:"; : PRINT #10, USING "**########,.## Bs"; (VAL(dia$)); : PRINT #10, CHR$(10);
     IF VAL(dia$) > CVD(total$) THEN PRINT #10, "EXCEDENTE:";
     IF VAL(dia$) = CVD(total$) THEN PRINT #10, "EXACTA   :";
     IF VAL(dia$) < CVD(total$) THEN PRINT #10, "FALTANTE :";
     PRINT #10, USING "**########,.## Bs"; ABS(VAL(dia$) - CVD(total$)); : PRINT #10, CHR$(10);
     PRINT #10, STRING$(60, "-"); CHR$(10);
     CLOSE #10
     Fun(12) = "no": Center 25, "[F1]Ayuda  [Esc]Continuar  [F9]Abortar": GOSUB Mostrador
     CALL Box2(4, 5, 16, 75): Center 5, "Cuantas Copias Desea Imprimir de Este Reporte?": copia$ = MeteDatoLetra$(6, 40, 15, " 0             ", "1")
     FOR k = 1 TO VAL(copia$)
          OPEN "LPT1:" FOR RANDOM AS #10
          IF devolver = false THEN
          WIDTH #10, 255
          PRINT #10, CHR$(27); CHR$(67); CHR$(128); CHR$(11);
          PRINT #10, STRING$(60, "-"); CHR$(10);
          PRINT #10, "Farmacia:"; NombFarma; CHR$(10);
          IF guia = 1 THEN PRINT #10, "<Reporte de Caja Opcion -X->";  ELSE PRINT #10, "Reporte de Caja Opcion -Z-:";
          PRINT #10, "Fecha:"; MID$(DATE$, 4, 2); "/"; MID$(DATE$, 1, 2); "/"; MID$(DATE$, 7, 4); " Hora:"; TIME$; CHR$(10);
          PRINT #10, SPACE$(14); "Montos Detallados Por Vendedor:"; CHR$(10);
          PRINT #10, STRING$(60, "-"); CHR$(10);
          FOR l = 1 TO (selecc% - 1)
               IF seli(l) = "*" THEN
                    PRINT #10, "³"; nombre(l); "³"; apellido(l); "³";
                    PRINT #10, USING "**###########,.##³"; CVD(monto(l));
                    PRINT #10, CHR$(10);
               END IF
          NEXT l
          PRINT #10, SPACE$(43); STRING$(17, "="); CHR$(10);
          PRINT #10, SPACE$(28); "Total de Ventas: "; : PRINT #10, USING "**########,.## Bs"; CVD(total$); : PRINT #10, CHR$(10);
          PRINT #10, SPACE$(28); "Monto Entregado:"; : PRINT #10, USING "**########,.## Bs"; (VAL(dia$)); : PRINT #10, CHR$(10);
          PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
          IF VAL(dia$) > CVD(total$) THEN PRINT #10, "EXCEDENTE:";
          IF VAL(dia$) = CVD(total$) THEN PRINT #10, "EXACTA   :";
          IF VAL(dia$) < CVD(total$) THEN PRINT #10, "FALTANTE :";
          PRINT #10, USING "**########,.## Bs"; ABS(VAL(dia$) - CVD(total$)); : PRINT #10, CHR$(10);
          PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
          PRINT #10, STRING$(60, "-"); CHR$(10);
          END IF
     NEXT k: CLOSE
EXIT SUB
Arriba:
     resal% = resal% - 1
     IF selecc% < 6 THEN
          IF resal% < 1 THEN resal% = 1: IF sonido = "si" THEN BEEP
     ELSE
          IF resal% < 1 THEN resal% = 1: up% = up% - 1
     END IF
     IF up% < 1 THEN up% = 1: IF sonido = "si" THEN BEEP
RETURN
Abajo: resal% = resal% + 1
     IF selecc% < 10 THEN
          IF resal% > ventana% THEN resal% = ventana%: IF sonido = "si" THEN BEEP
     ELSE
          IF resal% > 10 THEN resal% = 10: up% = up% + 1
     END IF
     IF selecc% > 10 THEN
          IF up% + 10 > selecc% THEN up% = selecc% - 9: IF sonido = "si" THEN BEEP
     END IF
RETURN
Apgup: up% = up% - 9: IF up% < 1 THEN up% = 1: resal% = 1: IF sonido = "si" THEN BEEP
RETURN
Apgdn: up% = up% + 9: IF up% + 9 > selecc% THEN up% = selecc% - 9: resal% = 10: IF sonido = "si" THEN BEEP
RETURN
Mostrador:
     COLOR colors(7, colorpref), colors(4, colorpref): Box2 4, 5, 16, 75: Center 4, "Vendedores Registrados": LOCATE 5, 6: PRINT "Nombres:           Apellidos:             Estado:     Monto:"
     Box2 1, 5, 3, 75: IF guia = 1 THEN Center 2, "Sub-Total de Caja:Opcion -X- de Uno o Mas Vendedores" ELSE Center 2, "Total de Caja:Opcion -Z- de Uno o Mas Vendedores"
     COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, 6: PRINT nombre(1); " "; apellido(1); " ";
     LOCATE fila%, 48: IF seli(1) = "*" THEN PRINT "(Selecc.)";  ELSE PRINT "         ";
     PRINT USING "##############,.##"; CVD(monto(1)): COLOR colors(4, colorpref), colors(7, colorpref)
     FOR k = 2 TO 10
          IF k <= selecc% THEN
               LOCATE fila% + k - 1, 6: PRINT nombre(k); " "; apellido(k); " ";
               LOCATE fila% + k - 1, 48: IF seli(k) = "*" THEN PRINT "(Selecc.)";  ELSE PRINT "         ";
               PRINT USING "##############,.##"; CVD(monto(k))
          ELSE
               LOCATE fila% + k - 1, 6: PRINT SPACE$(69)
          END IF
     NEXT k
     resal% = 1: up% = 1: IF selecc% > 10 THEN ventana% = 10 ELSE ventana% = selecc%
     DO
          kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
          SELECT CASE kbd$
               CASE CHR$(0) + "H": GOSUB Arriba
               CASE CHR$(0) + "P": GOSUB Abajo
               CASE CHR$(0) + CHR$(73): IF selecc% > 10 THEN GOSUB Apgup ELSE BEEP
               CASE CHR$(0) + CHR$(81): IF selecc% > 10 THEN GOSUB Apgdn ELSE BEEP
               CASE CHR$(0) + CHR$(67): CLOSE : EXIT SUB
               CASE CHR$(13)
                    IF Fun(12) = "si" THEN
                         IF (up% + resal% - 1) = selecc% THEN
                              IF seli(up% + resal% - 1) = "*" THEN
                                   FOR m = 1 TO selecc%: seli(m) = " ": NEXT m
                              ELSE
                                   FOR m = 1 TO selecc%: seli(m) = "*": NEXT m
                              END IF
                         ELSE
                              IF seli(up% + resal% - 1) = "*" THEN seli(up% + resal% - 1) = " " ELSE seli(up% + resal% - 1) = "*"
                         END IF
                    ELSE
                         BEEP
                    END IF
               CASE CHR$(27): resal% = 0: up% = 1: EXIT DO
               CASE ELSE:  BEEP
          END SELECT
     IF up% = 0 THEN
          FOR k = 0 TO 4: LOCATE fila% + k, 6: PRINT SPACE$(80); : NEXT k
          EXIT DO
     END IF
     l = 0
     FOR k = up% TO up% + 9: l = l + 1
     IF resal% = l THEN COLOR colors(7, colorpref), colors(4, colorpref)
          IF k <= selecc% THEN
               LOCATE fila% + l - 1, 6: PRINT nombre(k); " "; apellido(k); " ";
               LOCATE fila% + l - 1, 48: IF seli(k) = "*" THEN PRINT "(Selecc.)";  ELSE PRINT "         ";
               PRINT USING "##############,.##"; CVD(monto(k))
          ELSE
               LOCATE fila% + l - 1, 6: PRINT SPACE$(63)
          END IF
     IF resal% = l THEN COLOR colors(4, colorpref), colors(7, colorpref)
     NEXT k
     LOOP
RETURN
END SUB

