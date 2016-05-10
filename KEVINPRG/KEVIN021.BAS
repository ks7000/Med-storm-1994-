'visualiza ventas por pantalla
DEFINT A-Z
DECLARE SUB LeeEstado ()
DECLARE SUB Visualiza ()
DECLARE SUB Center (row, text$)
DECLARE SUB Box2 (Row1, Col1, Row2, Col2)
DECLARE SUB Espera ()
DECLARE FUNCTION CheqDate (caden$, op%)
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$, tr$)
DECLARE FUNCTION Trim$ (x$)
DECLARE FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%)
COMMON SHARED guia, colorpref: colorpref = 3
DIM SHARED fx AS STRING * 33, turno, costo AS STRING * 8, bruto AS STRING * 8, neto AS STRING * 8, DescGer AS STRING * 8, total AS STRING * 8
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, impuesto AS STRING * 6, NombFarma AS STRING * 25, Direccion AS STRING * 30, Rif     AS STRING * 25, Nit      AS STRING * 25, telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2, Encabezado AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2, NuevoIva AS STRING * 2, ShowPerc AS STRING * 2
DIM SHARED PrintErr AS INTEGER, devolver, Fun(17) AS STRING * 2, sitio, salir, renglon
DIM SHARED desc1(26) AS STRING * 35, cant1(26) AS STRING * 2, prec1(26) AS STRING * 8, ivaf1(26) AS STRING * 1
DIM SHARED Yname1(15) AS STRING * 3, Yname2(15) AS STRING * 15, grabGav AS STRING * 1, hayDsk AS INTEGER
CONST true = -1: CONST false = NOT true: turno = false
     'ON ERROR GOTO ErrorTrup
     'devolver = true
     'FILES "A:\": COLOR colors(7, colorpref), colors(4, colorpref): CLS
     'hayDsk = devolver
     'ON ERROR GOTO 0
ON TIMER(5) GOSUB AbreGav
TIMER ON
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
ErrorTrup:
  devolver = false
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
IF row = 25 THEN COLOR colors(7, colorpref), colors(4, colorpref): LOCATE 25, 1: PRINT SPACE$(80);
LOCATE row, 41 - LEN(text$) / 2: PRINT text$;
END SUB

FUNCTION CheqDate (caden$, op%)
CheqDate = true: IF caden$ = "        " THEN EXIT FUNCTION
dd% = VAL(MID$(caden$, 1, 2)): mm% = VAL(MID$(caden$, 4, 2)): aa% = VAL(MID$(caden$, 7, 4))
dd2% = VAL(MID$(DATE$, 4, 2)): mm2% = VAL(MID$(DATE$, 1, 2)): aa2% = VAL(MID$(DATE$, 7, 4))
IF dd% < 1 THEN CheqDate = false: EXIT FUNCTION
IF mm% < 1 OR mm% > 12 THEN CheqDate = false: EXIT FUNCTION
IF aa% < 1996 OR aa% > 2020 THEN CheqDate = false: EXIT FUNCTION
IF mm% = 2 AND dd% > 29 THEN CheqDate = false: EXIT FUNCTION
IF mm% = 1 OR mm% = 3 OR mm% = 5 OR mm% = 7 OR mm% = 8 OR mm% = 10 OR mm% = 12 THEN IF dd% > 31 THEN CheqDate = false: EXIT FUNCTION
IF mm% = 4 OR mm% = 6 OR mm% = 9 OR mm% = 11 THEN IF dd% > 30 THEN CheqDate = false: EXIT FUNCTION
IF op% = 2 THEN
     IF aa% < aa2% THEN CheqDate = false: EXIT FUNCTION
     IF aa% = aa2% THEN IF mm% < mm2% THEN CheqDate = false: EXIT FUNCTION
     IF aa% = aa2% THEN IF mm% = mm2% THEN IF dd% < dd2% THEN CheqDate = false: EXIT FUNCTION
END IF
END FUNCTION

FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%)
COLOR colors(7, colorpref), colors(4, colorpref): CALL Box2(fila%, columna%, fila% + 16, columna% + 16): CALL Center(25, "[F1] Ayuda [Enter]Tomar Opcion [A-Z]Ir a... [Esc]Menu Anterior"): COLOR colors(4, colorpref), colors(7, colorpref): CLOSE #5
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

SUB Espera
Center 25, "Presione Cualquier Tecla Para Continuar...": DO: LOOP WHILE INKEY$ = ""
END SUB

SUB LeeEstado
CLOSE : OPEN "F:\kevinprg\kevindat\kaos.dat" FOR INPUT AS #2
INPUT #2, sonido: INPUT #2, impuesto: INPUT #2, NombFarma: INPUT #2, Direccion: INPUT #2, Rif: INPUT #2, Nit: INPUT #2, telefono: INPUT #2, contri: INPUT #2, contra: INPUT #2, Stock: INPUT #2, Blanq
INPUT #2, LargoH: INPUT #2, TipoL: INPUT #2, Mensaje1: INPUT #2, Mensaje2: INPUT #2, Mensaje3: INPUT #2, Redondeo: INPUT #2, Desc: INPUT #2, DescMax: INPUT #2, Exactitud: INPUT #2, Prov1: INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo
REM INPUT #2, NuevoIva: INPUT #2, ShowPerc
CLOSE : NuevoIva = "si": ShowPerc = "si"
CLOSE #2: OPEN "f:\kevinprg\kevindat\encabeza.dat" FOR INPUT SHARED AS #2: INPUT #2, Encabezado: CLOSE
END SUB

FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$, tr$)
IF tr$ = "1" THEN llevasitio% = 2 ELSE llevasitio% = 1
LOCATE fila%, columna%: PRINT CHR$(178) + caden$: LOCATE fila%, columna% + largo% + 1: PRINT CHR$(178): REDIM eleccion(0 TO largo% + 1) AS STRING * 1
FOR k = 1 TO largo%: eleccion(k) = MID$(caden$, k, 1): NEXT k: LOCATE fila%, (columna% + llevasitio%): COLOR colors(7, colorpref) + 15, colors(4, colorpref): IF eleccion(1 + VAL(tr$)) = " " THEN PRINT "_";  ELSE PRINT eleccion(1 + VAL(tr$))
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
          IF k = llevasitio% THEN COLOR colors(7, colorpref), colors(4, colorpref)
          LOCATE fila%, (columna% + k)
          IF k = llevasitio% THEN
                    IF eleccion(k) <> " " THEN PRINT eleccion(k);  ELSE PRINT "_";
          ELSE
                    PRINT eleccion(k);
          END IF
          IF k = llevasitio% THEN COLOR colors(4, colorpref), colors(7, colorpref)
          NEXT k
LOOP
eleccion$ = "": eleccion(1) = UCASE$(eleccion(1)): FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR colors(5, colorpref), colors(4, colorpref): LOCATE fila%, columna% + 1: PRINT eleccion$
MeteDatoLetra$ = eleccion$: IF sonido = "si" THEN BEEP
EXIT FUNCTION
Sumar:
FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna%: PRINT "²"; eleccion$; "²"
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
CLOSE : OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #1 LEN = 100
FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS Cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
selecc% = LOF(1) / 100: selecc% = selecc% + 1: DIM nombre(selecc%) AS STRING * 20, apellido(selecc%) AS STRING * 20, Cedula(selecc%) AS STRING * 4, clave(selecc%) AS STRING * 10
FOR k = 1 TO selecc% - 1: GET #1, k: nombre(k) = nomb$: apellido(k) = apel$: Cedula(k) = Cedu$: clave(k) = clla$: NEXT k: CLOSE : fila% = 6
nombre(selecc%) = "Todos Los Vendedores": apellido(selecc%) = "": Cedula(selecc%) = MKL$(0)
IF selecc% = 0 THEN CALL Center(8, "** No Hay Ningun Vendedor Registrado **"): Espera: EXIT SUB
DO
     COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box2 4, 15, 16, 68: Center 4, "Vendedores Registrados": LOCATE 5, 16: PRINT "Nombres:           Apellidos:                "
     Box2 1, 5, 3, 75: Center 2, "Visualizacion de Ventas Hechas En Un Dia": Center 25, "Use Las Teclas de Direccion y Enter ; Presion Escape Para Salir"
     COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, 16: PRINT nombre(1); " "; apellido(1); " "; : LOCATE fila%, 58 ': PRINT USING "#########,"; CVL(Cedula(1))
     COLOR colors(4, colorpref), colors(7, colorpref)
     FOR k = 2 TO 10
          IF k <= selecc% THEN
               LOCATE fila% + k - 1, 16: PRINT nombre(k); " "; apellido(k); " ";
               'LOCATE fila% + k - 1, 58: PRINT USING "#########,"; CVL(Cedula(k))
          ELSE
               LOCATE fila% + k - 1, 16: PRINT SPACE$(52)
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
               CASE CHR$(13): EXIT DO
               CASE CHR$(27): resal% = 0: up% = 1: EXIT DO
               CASE CHR$(0) + CHR$(67): resal% = 0: up% = 1: Fun(3) = "si": EXIT DO
               CASE ELSE:  BEEP
          END SELECT
     IF up% = 0 THEN
          FOR k = 0 TO 4: LOCATE fila% + k, 16: PRINT SPACE$(80); : NEXT k
          EXIT DO
     END IF
     l = 0
     FOR k = up% TO up% + 9: l = l + 1
     IF resal% = l THEN COLOR colors(7, colorpref), colors(4, colorpref)
          IF k <= selecc% THEN
               LOCATE fila% + l - 1, 16: PRINT nombre(k); " "; apellido(k); " "; ' : LOCATE fila% + k - 1, 58: PRINT USING "#########,"; CVL(Cedula(k))
          ELSE
               LOCATE fila% + l - 1, 16: PRINT SPACE$(52)
          END IF
     IF resal% = l THEN COLOR colors(4, colorpref), colors(7, colorpref)
     NEXT k
     LOOP: escoge% = (up% + resal% - 1): IF escoge% = 0 THEN EXIT SUB
     Box2 4, 10, 12, 70: Center 5, "Introduzca Fecha": salir = false: hoy$ = MID$(DATE$, 4, 2) + "/" + MID$(DATE$, 1, 2) + "/" + MID$(DATE$, 7, 4)
     DO WHILE NOT salir
     dia$ = MeteDatoLetra$(6, 35, 10, hoy$, "0")
     IF CheqDate(dia$, 1) = true THEN salir = true
     LOOP
     IF hayDsk THEN
       salir = false: Center 7, "Desea Buscar en Diskette (a) o DiscoDuro (f) ?"
       DO WHILE NOT salir
        via$ = MeteDatoLetra$(8, 35, 1, " ", "0"): via$ = LCASE$(via$)
        IF via$ = "a" OR via$ = "f" THEN salir = true
       LOOP
     ELSE
      via$ = "f"
     END IF
     Box2 10, 10, 20, 70: Center 15, "Seleccionando Ventas , Espere Por Favor . . ."
     die$ = MID$(dia$, 1, 2) + MID$(dia$, 4, 2) + MID$(dia$, 7, 4): Center 25, "Buscando , Espere Por Favor . . ."
     IF via$ = "a" THEN
      fx = via$ + ":\" + die$ + ".vnt": devolver = false
     ELSE
      fx = via$ + ":\kevinprg\kevinvnt\" + die$ + ".vnt": devolver = false
     END IF
     ON ERROR GOTO ErrorTrep
     FILES fx: COLOR colors(7, colorpref), colors(4, colorpref): CLS
     IF devolver = true THEN CLOSE : EXIT SUB
     ON ERROR GOTO 0
     Box2 1, 5, 3, 75: Center 2, SPACE$(65): Center 2, ("Vendedor y Dia Seleccionado:" + Trim$(nombre(escoge%)) + " " + dia$)
     ahora$ = "F:\kevinprg\kevindat\" + Trim$(MID$(STR$(TIMER * 100), 2, 10)) + ".ver"
     OPEN fx FOR RANDOM AS #1 LEN = 30: FIELD #1, 30 AS todo$: maxi% = LOF(1) / 30
     IF maxi% < 3 THEN Box2 10, 10, 20, 70: Center 15, "EL DIA SELECCIONADO NO TIENE VENTAS REGISTRADAS": Espera: CLOSE : EXIT SUB
     OPEN ahora$ FOR RANDOM AS #2 LEN = 30: FIELD #2, 30 AS all$: ini% = 1: GET #1, 1
     IF MID$(todo$, 1, 5) = "TURNO" THEN turno = true
     FOR k = 2 TO maxi%: GET #1, k
     IF escoge% = selecc% THEN
          RSET all$ = todo$: PUT #2, k - 1
     ELSE
          IF MID$(todo$, 1, 1) = "J" THEN
               IF CVL(MID$(todo$, 19, 4)) = CVL(Cedula(escoge%)) THEN
                    RSET all$ = todo$: m = CVI(MID$(todo$, 2, 2)): PUT #2, ini%: ini% = ini% + 1
                    FOR l = (k + 1) TO (k + m)
                    GET #1, l: RSET all$ = todo$: PUT #2, ini%: ini% = ini% + 1
                    NEXT l
               END IF
          END IF
     END IF
     NEXT k
     CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200: salir = false
     FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS  _
cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$ _
, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
     ON ERROR GOTO ErrorTrap
     IF salir = true THEN EXIT SUB
     OPEN ahora$ FOR RANDOM AS #2 LEN = 30: FIELD #2, 30 AS all$: puntero% = 1
     salir = false: maxi% = LOF(2) / 30: LOCATE 4, 1: PRINT STRING$(80, "²"): Center 23, "Utilize Las Teclas de Direccion Para Visualizar Las Facturas, Salga con Escape"
     IF maxi% = 0 THEN Box2 10, 10, 20, 70: Center 15, "NO Se Encontraron Ventas Hechas Por Los": Center 17, "Vendedores Seleccionados.": Espera: EXIT SUB
     OPEN "F:\kevinprg\kevindat\codigo.idx" FOR RANDOM SHARED AS #3 LEN = 8
     FIELD #3, 6 AS nomshort$, 2 AS num$: max2% = LOF(3) / 8
     'DIM Xa(max2%) AS STRING * 6, Xb(max2%) AS STRING * 2
     'FOR k = 1 TO max2%: GET #2, k: Xa(k) = nomshort$: Xb(k) = num$: NEXT k
     DO WHILE NOT salir
          GET #2, puntero%: IF MID$(all$, 1, 1) <> "J" THEN Box2 10, 10, 20, 70: Center 15, "Archivo Corrupto , Llame a Kevin Software 7000": Center 17, "Tan Pronto Como Le Sea Posible": Espera: EXIT SUB
          LOCATE 5, 1: PRINT "No  Factura:"; : PRINT USING "**######"; CVL(MID$(all$, 23, 4))
          LOCATE 5, 3: PRINT MID$(all$, 4, 1)
          LOCATE 5, 24: PRINT "²Total Bruto:": bruto = MKD$(0)
          LOCATE 5, 50: PRINT "Desc. Vend.:"
          LOCATE 6, 1: PRINT "Hora:"; MID$(all$, 13, 2); ":"; MID$(all$, 15, 2); ":"; MID$(all$, 17, 2)
          LOCATE 6, 24: PRINT "²Total  Neto:"; : PRINT USING "########,.##²"; CVD(MID$(all$, 5, 8)): neto = (MID$(all$, 5, 8))
          LOCATE 6, 50: PRINT "Desc. Gere.:"
          LOCATE 7, 1: PRINT "Vendedor:";
          LOCATE 7, 24: PRINT "²            ": costo = MKD$(0)
          LOCATE 7, 50: PRINT "            "
          FOR k = 1 TO selecc%: IF CVL(MID$(all$, 19, 4)) = CVL(Cedula(k)) THEN LOCATE 7, 10: PRINT MID$(nombre(k), 1, 14); : Cedu$ = clave(k)
          NEXT k: DescGer = MKD$(0): MID$(DescGer, 1, 4) = MID$(all$, 27, 4)
          otropuntero% = puntero% + CVI(MID$(all$, 2, 2)): Box2 8, 1, 22, 80: sitio% = 9
          FOR k = puntero% + 1 TO otropuntero%: GET #2, k
          costo = MKD$(CVD(costo) + (CVD(MID$(all$, 18, 8)) * CVI(MID$(all$, 8, 2)))): bruto = MKD$(CVD(bruto) + (CVD(MID$(all$, 10, 8)) * CVI(MID$(all$, 8, 2))))
          encontrado = false: primero = 1: ultimo = max2%: Prod$ = MID$(all$, 2, 6)
          WHILE NOT encontrado AND primero <= ultimo
          central = (primero + ultimo) / 2: GET #3, central
          IF Prod$ = nomshort$ THEN
               encontrado = true
          ELSE
          IF nomshort$ > Prod$ THEN
               ultimo = central - 1
          ELSE
               primero = central + 1
          END IF
          END IF
          WEND: GET #3, central: GET #1, CVI(num$)
          ax$ = nomb2$
          IF encontrado = false THEN ax$ = "  Producto borrado****"
          IF sitio% < 22 THEN
               LOCATE sitio%, 2: PRINT MID$(ax$, 1, 22); "³"; : PRINT USING "####³"; CVI(MID$(all$, 8, 2)); : PRINT USING "######,.##³³"; CVD(MID$(all$, 10, 8))
          ELSE
               LOCATE sitio% - 13, 42:  PRINT MID$(ax$, 1, 22); "³"; : PRINT USING "####³"; CVI(MID$(all$, 8, 2)); : PRINT USING "######,.##"; CVD(MID$(all$, 10, 8))
          END IF: sitio% = sitio% + 1
          NEXT k: MID$(DescGer, 5, 4) = MID$(all$, 27, 4): fuera = false: IF puntero% = 1 THEN Center 25, "Principio de Archivo de Ese Dia"
          LOCATE 7, 37: PRINT "            ²";
          'LOCATE 7, 37: PRINT USING "########,.##²"; CVD(costo)
          LOCATE 5, 37: PRINT USING "########,.##²"; CVD(bruto)
          LOCATE 5, 62: PRINT USING "########,.##"; (CVD(bruto) - CVD(DescGer) - CVD(neto)); : PRINT USING " ##.##%"; ((CVD(bruto) - CVD(DescGer) - CVD(neto)) / CVD(bruto)) * 100
          LOCATE 6, 62: PRINT USING "########,.##"; CVD(DescGer); : PRINT USING " ##.##%"; (CVD(DescGer) / CVD(bruto)) * 100
          'LOCATE 7, 62: PRINT USING "########,.##"; (CVD(neto) - CVD(costo)); : PRINT USING " ##.##%"; ((CVD(neto) - CVD(costo)) / CVD(neto)) * 100
          kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND: Center 25, SPACE$(50)
          SELECT CASE kbd$
               CASE CHR$(0) + "H"  'sube factura
                    DO WHILE NOT fuera
                    puntero% = puntero% - 1: IF puntero% < 1 THEN puntero% = 1
                    GET #2, puntero%: IF MID$(all$, 1, 1) = "J" THEN fuera = true
                    LOOP
               CASE CHR$(0) + "P"  'baja factura
                    SWAP puntero%, otropuntero%: puntero% = puntero% + 1: IF puntero% >= maxi% THEN puntero% = puntero% - 1: SWAP puntero%, otropuntero%
               CASE CHR$(27): salir = true
               CASE CHR$(0) + CHR$(67): GOSUB Imprimir
               CASE ELSE:  BEEP
          END SELECT
     LOOP: CLOSE
LOOP
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

Imprimir:
ON ERROR GOTO ErrorTrap
GET #2, puntero%: imprint$ = MID$(all$, 4, 1)
     IF imprint$ = "*" THEN
          DO
          Box2 10, 8, 20, 72: Center 12, "  *** A T E N C I O N ***  "
          Center 14, "Esta factura ya fue impresa en su debida oportunidad."
          Center 15, "¨ESTA TOTALMENTE SEGURO QUE DESEA IMPRIMIRLA DE NUEVO?"
          Center 16, "Pulse S ¢ N , no tiene otra opci¢n": Center 25, "Pulse S ¢ N , no tiene otra opci¢n"
          llave$ = "": DO WHILE llave$ = "": llave$ = INKEY$: LOOP: llave$ = UCASE$(llave$)
          IF llave$ = "S" THEN imprint$ = " ": EXIT DO
          IF llave$ = "N" THEN imprint$ = "*": EXIT SUB
          LOOP
     END IF
     IF imprint$ = " " THEN
               GOSUB HaceCaja
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
                    IF Fun(9) = "si" THEN
                         COLOR colors(7, colorpref), colors(4, colorpref): Box2 10, 8, 20, 72
                         Center 15, "Desea Imprimir Factura? Pulse -S- para IMPRIMIR"
                         llave$ = "": DO WHILE llave$ = "": llave$ = INKEY$: LOOP: llave$ = UCASE$(llave$)
                         IF llave$ = "S" THEN salir = true ELSE EXIT SUB
                    END IF
                    IF Fun(2) = "si" THEN GOSUB Buscacliente: Fun(2) = "no"
               LOOP
               GET #2, puntero%: IF MID$(all$, 1, 1) <> "J" THEN Box2 10, 10, 20, 70: Center 15, "Archivo Corrupto , Llame a Kevin Software 7000": Center 17, "Tan Pronto Como Le Sea Posible": Espera: EXIT SUB
               fct$ = (MID$(all$, 23, 4)): salir = false: neto = (MID$(all$, 5, 8))
               hora$ = MID$(all$, 13, 2) + ":" + MID$(all$, 15, 2) + ":" + MID$(all$, 17, 2)
               FOR k = 1 TO selecc%
                    IF CVL(MID$(all$, 19, 4)) = CVL(Cedula(k)) THEN Cedu$ = clave(k)
               NEXT k
               otropuntero% = puntero% + CVI(MID$(all$, 2, 2)): renglon = 1: total$ = MKD$(0)
               FOR k = puntero% + 1 TO otropuntero%
                    GET #2, k
                    encontrado = false: primero = 1: ultimo = max2%: Prod$ = MID$(all$, 2, 6)
                    WHILE NOT encontrado AND primero <= ultimo
                         central = (primero + ultimo) / 2: GET #3, central
                         IF Prod$ = nomshort$ THEN
                              encontrado = true
                         ELSE
                              IF nomshort$ > Prod$ THEN
                                   ultimo = central - 1
                              ELSE
                                   primero = central + 1
                              END IF
                         END IF
                    WEND: GET #3, central: GET #1, CVI(num$)
                    desc1(renglon) = nomb2$: ivaf1(renglon) = MID$(all$, 1, 1)
                    cant1(renglon) = MID$(all$, 8, 2): prec1(renglon) = MID$(all$, 10, 8)
                    total = MKD$(CVD(total) + (CVI(cant1(renglon)) * CVD(prec1(renglon)))): renglon = renglon + 1
               NEXT k
               OPEN "lpt1:" FOR OUTPUT AS #10
               WIDTH #10, 255
               PRINT #10, CHR$(27); CHR$(64); : PRINT #10, CHR$(27); CHR$(64);
               PRINT #10, CHR$(15);
              
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
                         
                         PRINT #10, SPACE$(6); SPACE$(35); "VAN --> "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); : PRINT #10, SPACE$(9);
                         PRINT #10, SPACE$(35); "VAN --> "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); CHR$(10);
                        
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
                        
                         PRINT #10, SPACE$(6); SPACE$(35); "VAN --> "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); : PRINT #10, SPACE$(9);
                         PRINT #10, SPACE$(35); "VAN --> "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); CHR$(10);
                       
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
                       
                         PRINT #10, SPACE$(6); SPACE$(35); "VAN --> "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); : PRINT #10, SPACE$(9);
                         PRINT #10, SPACE$(35); "VAN --> "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); CHR$(10);
                      
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
               FOR mrw% = 1 TO 12: PRINT #10, CHR$(10); : NEXT mrw%
               'PRINT #10, CHR$(12);
               CLOSE #10

               IF die$ = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4) THEN
                    CLOSE #17: OPEN fx FOR RANDOM AS #17 LEN = 30: FIELD #17, 30 AS marcador$: max% = LOF(17) / 30
                    FOR m = 1 TO max%
                         GET #17, m
                         IF MID$(marcador$, 1, 1) = "J" AND fct$ = MID$(marcador$, 23, 4) THEN
                              RSET marcador$ = MID$(marcador$, 1, 3) + "*" + MID$(marcador$, 5, 26): PUT #17, m: EXIT FOR
                         END IF
                    NEXT m: CLOSE #17
                    GET #2, puntero%: RSET all$ = MID$(all$, 1, 3) + "*" + MID$(all$, 5, 26): PUT #2, puntero%
               END IF
     END IF
RETURN

HaceCaja:
     Box2 10, 8, 20, 72: Center 10, "Datos del Cliente": Center 25, "[F9] Salir e Imprimir"
     LOCATE 12, 10: PRINT "Nombre  ¢ Raz¢n : ²"; SPACE$(40); "²": salir = false: sit% = 1
     LOCATE 13, 10: PRINT "Domicilio Fiscal:": LOCATE 15, 10: PRINT "²"; SPACE$(60); "²"
     LOCATE 14, 10: PRINT "²"; SPACE$(60); "²": Fun(9) = "no": nombre$ = " * CONTADO *" + SPACE$(20): domicilio1$ = "": rifo$ = "": nito$ = "": domicilio2$ = ""
     LOCATE 16, 10: PRINT "R.I.F.: ²"; SPACE$(20); "²": LOCATE 16, 40: PRINT "Telefono: ²"; SPACE$(20); "²": tlf$ = ""
     LOCATE 18, 10: PRINT "N.I.T.: ²"; SPACE$(20); "²": LOCATE 18, 40: PRINT "Forma de Pago: CONTADO"
RETURN

Buscacliente:
CALL Center(25, " *** Espere , Por Favor ... ***")
LOCATE 12, 10: PRINT "Elija el Cliente: ²   ²";
mjfox% = Elije%(7, 3, 60, 12, 29, 0): IF mjfox% < 1 THEN RETURN
client% = VAL(Yname1(mjfox%)): CLOSE #5
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
rifo$ = rif5$: nito$ = nit5$: tlf$ = tele5$: CLOSE #5
RETURN
END SUB

