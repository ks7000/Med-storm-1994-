DEFINT A-Z
DECLARE SUB Fecha ()
DECLARE SUB Espera ()
DECLARE SUB SalvaEstado ()
DECLARE SUB Center (row, text$)
DECLARE SUB Box (Row1%, Col1%, Row2%, Col2%)
DECLARE SUB Seniat ()
DECLARE SUB Farmacia ()
DECLARE SUB StockJob ()
DECLARE SUB Blanqueador ()
DECLARE SUB LargoFactura ()
DECLARE SUB TipoLetra ()
DECLARE SUB MarcaVence ()
DECLARE SUB MensajesFact ()
DECLARE SUB ArreglaTurno ()
DECLARE SUB LeeEstado ()
DECLARE SUB MaximoDesc ()
DECLARE SUB TodosDesc ()
DECLARE SUB Exacto ()
DECLARE SUB Redondear ()
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$, tr$)
DECLARE FUNCTION CheqNum$ (cadn$, ente%, larg%)
DECLARE FUNCTION Trim$ (x$)
COMMON SHARED guia, colorpref AS INTEGER
DIM SHARED turno AS STRING * 1, devolver
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, impuesto AS STRING * 6, Nombfarma AS STRING * 25, Direccion AS STRING * 30, Rif     AS STRING * 25, Nit      AS STRING * 25, Telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 3
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2, NuevoIva AS STRING * 2, ShowPerc AS STRING * 2
DIM SHARED Fun(17) AS STRING * 2, salir, Turna
CONST true = -1: CONST false = NOT true
LeeEstado
COLOR colors(7, colorpref), colors(4, colorpref)
    SELECT CASE guia
        CASE 6: Fecha
        CASE 7: Seniat
        CASE 8: Farmacia
        CASE 10: MarcaVence
        CASE 12: LargoFactura
        CASE 13: Blanqueador
        CASE 14: MensajesFact
        CASE 15: Redondear
        CASE 16: TodosDesc
        CASE 17: MaximoDesc
        CASE 18: Exacto
        CASE ELSE
    END SELECT
CHAIN "f:\kevinprg\kevin01.bas"
END
DATA 0,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  2,     0,     1,     15,     0,     7,      0
DATA 3,    15,   13,  1,     14,    3,     15,     0,     7,      0
DATA 7,    12,   15,  4,     14,    0,     15,     15,    1,      0

SUB ArreglaTurno
dia2 = VAL(MID$(DATE$, 4, 2))
mes2 = VAL(MID$(DATE$, 1, 2))
ano2 = VAL(MID$(DATE$, 7, 4))
dia1 = 18
mes1 = 9
ano1 = 1996
SELECT CASE Turna
     CASE 2: dia1 = 19
     CASE 3: dia1 = 20
     CASE 4: dia1 = 21
END SELECT: seguir$ = "true"
DO WHILE seguir$ = "true": dia1 = dia1 + 4
     IF dia1 >= 29 AND mes1 = 2 THEN dia1 = dia1 - 28: mes1 = 3
     IF dia1 >= 31 AND mes1 = 4 THEN dia1 = dia1 - 30: mes1 = mes1 + 1
     IF dia1 >= 31 AND mes1 = 6 THEN dia1 = dia1 - 30: mes1 = mes1 + 1
     IF dia1 >= 31 AND mes1 = 9 THEN dia1 = dia1 - 30: mes1 = mes1 + 1
     IF dia1 >= 31 AND mes1 = 11 THEN dia1 = dia1 - 30: mes1 = mes1 + 1
     IF dia1 >= 32 AND mes1 = 1 THEN dia1 = dia1 - 31: mes1 = mes1 + 1
     IF dia1 >= 32 AND mes1 = 3 THEN dia1 = dia1 - 31: mes1 = mes1 + 1
     IF dia1 >= 32 AND mes1 = 5 THEN dia1 = dia1 - 31: mes1 = mes1 + 1
     IF dia1 >= 32 AND mes1 = 7 THEN dia1 = dia1 - 31: mes1 = mes1 + 1
     IF dia1 >= 32 AND mes1 = 8 THEN dia1 = dia1 - 31: mes1 = mes1 + 1
     IF dia1 >= 32 AND mes1 = 10 THEN dia1 = dia1 - 31: mes1 = mes1 + 1
     IF dia1 >= 32 AND mes1 = 12 THEN dia1 = dia1 - 31: mes1 = 1: ano1 = ano1 + 1
     IF dia1 >= dia2 AND mes1 = mes2 AND ano1 = ano2 THEN seguir$ = "false"
     IF mes1 > mes2 AND ano1 = ano2 THEN seguir$ = "false"
     IF ano1 > ano2 THEN seguir$ = "false"
LOOP: SalvaEstado
END SUB

SUB Blanqueador
Box 9, 19, 15, 61
Center 11, "Introduzca el Nuevo Valor:"
Center 13, "Valor Actual:" + Blanq
Blanq = "0"
Center 14, " M ximo: 300 seg. M¡mimo: 30 seg."
DO WHILE VAL(Blanq) < 30 OR VAL(Blanq) > 300
Blanq = MeteDatoLetra$(12, 37, 3, Blanq, "0")
LOOP
Center 12, " Salvando... ": SalvaEstado
END SUB

SUB Box (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿"; : FOR a = Row1 + 1 TO Row2 - 1: LOCATE a, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Center (row, text$)
IF row = 25 THEN COLOR colors(5, colorpref), colors(4, colorpref): LOCATE 25, 1: PRINT SPACE$(80);
LOCATE row, 41 - LEN(text$) / 2: PRINT text$;
END SUB

FUNCTION CheqNum$ (cadn$, ente%, larg%)
cadn$ = LTRIM$(cadn$): cadn$ = RTRIM$(cadn$): cadn$ = STR$(VAL(cadn$)): punto% = 0: punt% = 0: longi% = LEN(cadn$): FOR k = 1 TO longi%: IF MID$(cadn$, k, 1) = "." THEN punto% = punto% + 1: punt% = k
NEXT k: IF punto% > 1 THEN cadn$ = "0.00": SOUND 400, 1: GOTO jfk
IF ente% = 1 THEN
     fr& = INT(VAL(cadn$)): IF MID$(STR$(fr&), 1, 1) = "-" THEN cadn$ = MID$(STR$(fr&), 1, larg% - 1) ELSE cadn$ = MID$(STR$(fr&), 2, larg%)
     GOTO jfk
ELSE
     IF punto% = 0 THEN cadn$ = cadn$ + ".00": GOTO jfk
     IF punt% = LEN(cadn$) THEN cadn$ = cadn$ + "00": GOTO jfk
     IF punt% = LEN(cadn$) - 1 THEN cadn$ = cadn$ + "0": GOTO jfk
     cadn$ = MID$(cadn$, 1, punt%) + MID$(cadn$, punt% + 1, 2)
END IF
jfk: IF LEN(cadn$) < larg% THEN cadn$ = STRING$((larg% - LEN(cadn$)), " ") + cadn$
CheqNum$ = cadn$: EXIT FUNCTION
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
          CASE CHR$(0) + CHR$(72): devolver = true: EXIT DO
          CASE CHR$(0) + CHR$(80): EXIT DO
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
          CASE " ", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "%", ".", "+", "-"
            eleccion(llevasitio%) = llave$: llevasitio% = llevasitio% + 1: IF llevasitio% > largo% THEN EXIT DO
          CASE CHR$(13): EXIT DO
          CASE CHR$(27): devolver = true: salir = true: EXIT DO
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
elccion$ = eleccion$: elccion$ = CheqNum(elccion$, entero%, largo%): IF ojo$ = "si" THEN ojo$ = "": elccion$ = MID$(elccion$, 2, largo% - 1) + "%"
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna%: PRINT "²"; elccion$; "²": IF sonido = "si" THEN BEEP
EntraDatoNumero$ = elccion$: EXIT FUNCTION
END FUNCTION

SUB Espera
DO: LOOP WHILE INKEY$ = ""
END SUB

SUB Exacto
Box 9, 19, 15, 61: Center 11, "Introduzca el Nuevo Valor:"
Center 13, "Valor Actual:" + Exactitud: Exactitud = "94": Center 14, " M ximo: 99 %     M¡mimo: 95 %   "
DO WHILE VAL(Exactitud) < 95
Exactitud = MeteDatoLetra$(12, 37, 2, "95", "0")
LOOP
IF VAL(Exactitud) < 95 THEN Exactitud = "95"
Center 12, " Salvando... ": SalvaEstado
END SUB

SUB Farmacia
Box 5, 5, 22, 75: Center 25, "Presione [Esc] para Salir SIN Salvar o [F9] para Salvar y salir"
COLOR colors(4, colorpref), colors(7, colorpref)
Center 6, "Introduccion de Datos Basicos de la Farmacia."
COLOR colors(4, colorpref), colors(7, colorpref)
LOCATE 8, 19: PRINT "Nombre de la Farmacia:"
LOCATE 10, 16: PRINT "Direccion de la Farmacia:"
LOCATE 12, 10: PRINT "Registro de Informacion Fiscal:"
LOCATE 14, 11: PRINT "Nacional Integrado Tributario:"
LOCATE 16, 31: PRINT "Telefonos:"
LOCATE 18, 20: PRINT "Turno Farmaceutico #:"
COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 8, 41: PRINT "²"; Nombfarma; "²"
LOCATE 10, 41: PRINT "²"; Direccion; "²"
LOCATE 12, 41: PRINT "²"; Rif; "²"
LOCATE 14, 41: PRINT "²"; Nit; "²"
LOCATE 16, 41: PRINT "²"; Telefono; "²"
LOCATE 18, 41: PRINT "²"; STR$(Turna); "²": salir = false: sit% = 1
DO WHILE NOT salir
    SELECT CASE sit%
        CASE 1
            Nombfarma = MeteDatoLetra(8, 41, 25, Nombfarma, "0")
            IF devolver = true THEN devolver = false: sit% = 1 ELSE sit% = 2
            IF Fun(9) = "si" THEN salir = true
            IF Fun(1) = "si" THEN Fun(1) = "no": SHELL "type b:\ayuda\0001.hlp"
        CASE 2
            Direccion = MeteDatoLetra(10, 41, 30, Direccion, "0")
            IF devolver = true THEN devolver = false: sit% = 1 ELSE sit% = 3
            IF Fun(9) = "si" THEN salir = true
            IF Fun(1) = "si" THEN Fun(1) = "no": SHELL "type b:\ayuda\0001.hlp"
        CASE 3:
            Rif = MeteDatoLetra(12, 41, 25, Rif, "0")
            IF devolver = true THEN devolver = false: sit% = 2 ELSE sit% = 4
            IF Fun(9) = "si" THEN salir = true
            IF Fun(1) = "si" THEN Fun(1) = "no": SHELL "type b:\ayuda\0001.hlp"
        CASE 4
            Nit = MeteDatoLetra(14, 41, 25, Nit, "0")
            IF devolver = true THEN devolver = false: sit% = 3 ELSE sit% = 5
            IF Fun(9) = "si" THEN salir = true
            IF Fun(1) = "si" THEN Fun(1) = "no": SHELL "type b:\ayuda\0001.hlp"
        CASE 5
            Telefono = MeteDatoLetra(16, 41, 25, Telefono, "0")
            IF devolver = true THEN devolver = false: sit% = 4 ELSE sit% = 6
            IF Fun(9) = "si" THEN salir = true
            IF Fun(1) = "si" THEN Fun(1) = "no": SHELL "type b:\ayuda\0001.hlp"
        CASE 6
            Turna = 0
            DO WHILE Turna > 4 OR Turna < 1
            Turna = VAL(MeteDatoLetra$(18, 41, 2, "0", "0"))
            LOOP
            turno = MID$(STR$(Turna), 2, 1)
            IF devolver = true THEN devolver = false: sit% = 5 ELSE sit% = 1
            IF Fun(9) = "si" THEN salir = true
        CASE ELSE
    END SELECT
LOOP
IF Fun(9) = "si" THEN Fun(9) = "no": Center 20, "Salvando la Informacion...": ArreglaTurno
END SUB

SUB Fecha
Box 9, 19, 14, 61
Center 11, "Introduzca la Nueva Fecha (dd/mm/aaaa):"
hoy$ = MID$(DATE$, 4, 2) + "/" + MID$(DATE$, 1, 2) + "/" + MID$(DATE$, 7, 4)
Center 13, "-La Fecha Actual es: " + hoy$ + " -"
hoy$ = MeteDatoLetra(12, 37, 10, hoy$, "0")
DATE$ = MID$(hoy$, 4, 2) + "/" + MID$(hoy$, 1, 2) + "/" + MID$(hoy$, 7, 4)
Box 9, 19, 14, 61
Center 11, "Introduzca la Nueva Hora (hh:mm:ss):"
Center 13, "-La Hora Actual es:" + TIME$
TIME$ = MeteDatoLetra(12, 37, 8, TIME$, "0")
END SUB

SUB LargoFactura
Box 9, 19, 15, 61
Center 11, "Introduzca el Nuevo Valor:"
Center 13, "Valor Actual:" + LargoH
LargoH = "0"
Center 14, " M ximo:50  M¡nimo:1 "
DO WHILE VAL(LargoH) < 1 OR VAL(LargoH) > 50
LargoH = MeteDatoLetra$(12, 37, 2, LargoH, "0")
LOOP
Center 12, " Salvando... "
SalvaEstado
END SUB

SUB LeeEstado
CLOSE : OPEN "f:\kevinprg\kevindat\kaos.dat" FOR INPUT AS #2
INPUT #2, sonido: INPUT #2, impuesto: INPUT #2, Nombfarma: INPUT #2, Direccion
INPUT #2, Rif: INPUT #2, Nit: INPUT #2, Telefono: INPUT #2, contri
INPUT #2, contra: INPUT #2, Stock: INPUT #2, Blanq: INPUT #2, LargoH
INPUT #2, TipoL: INPUT #2, Mensaje1: INPUT #2, Mensaje2: INPUT #2, Mensaje3
INPUT #2, Redondeo: INPUT #2, Desc: INPUT #2, DescMax: INPUT #2, Exactitud
INPUT #2, Prov1: INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo
INPUT #2, NuevoIva: INPUT #2, ShowPerc
CLOSE
END SUB

SUB MarcaVence
END SUB

SUB MaximoDesc
Box 9, 19, 15, 61: Center 11, "Introduzca el Nuevo Valor:"
Center 13, "Valor Actual:" + DescMax: DescMax = "99": Center 14, " M ximo: 50 %     M¡mimo: 0 %    "
DO WHILE VAL(DescMax) > 50
DescMax = MeteDatoLetra$(12, 37, 2, "0 ", "0")
LOOP
Center 12, " Salvando... ": SalvaEstado
END SUB

SUB MensajesFact
Box 5, 5, 18, 75: Center 25, "Presione [Esc] para Salir SIN Salvar o [F9] para Salvar y salir"
COLOR colors(4, colorpref), colors(7, colorpref)
Center 6, "Introducci¢n de Mensajes que Llevar n las Facturas de Venta."
Center 8, "Mensaje n£mero  Uno:": Center 10, "Mensaje n£mero  Dos:": Center 12, "Mensaje n£mero Tres:"
COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 9, 9: PRINT "²"; Mensaje1; "²": LOCATE 11, 9: PRINT "²"; Mensaje2; "²": LOCATE 13, 9: PRINT "²"; Mensaje3; "²"
IF NuevoIva = "si" THEN Center 15, "Se especificara el impuesto en las facturas." ELSE Center 15, "NO especificar  el impuesto en las facturas."
IF ShowPerc = "si" THEN Center 17, "Se especificara el porcentaje de descuento en las facturas." ELSE Center 17, "NO especificar  el porcentaje de descuento en las facturas."
salir = false: sit% = 1
DO WHILE NOT salir
    SELECT CASE sit%
        CASE 1
            Center 25, "Presione [Esc] para Salir SIN Salvar o [F9] para Salvar y salir"
            Mensaje1 = MeteDatoLetra(9, 9, 60, Mensaje1, "0")
            aux$ = Trim$(Mensaje1): libre% = INT((60 - LEN(aux$)) / 2)
            Mensaje1 = "": Mensaje1 = SPACE$(libre%) + aux$
            LOCATE 9, 9: PRINT "²"; Mensaje1; "²"
            IF devolver = true THEN devolver = false: sit% = 5 ELSE sit% = 2
            IF Fun(9) = "si" THEN salir = true
        CASE 2
            Center 25, "Presione [Esc] para Salir SIN Salvar o [F9] para Salvar y salir"
            Mensaje2 = MeteDatoLetra(11, 9, 60, Mensaje2, "0")
            aux$ = Trim$(Mensaje2): libre% = INT((60 - LEN(aux$)) / 2)
            Mensaje2 = "": Mensaje2 = SPACE$(libre%) + aux$
            LOCATE 11, 9: PRINT "²"; Mensaje2; "²"
            IF devolver = true THEN devolver = false: sit% = 1 ELSE sit% = 3
            IF Fun(9) = "si" THEN salir = true
        CASE 3
            Center 25, "Presione [Esc] para Salir SIN Salvar o [F9] para Salvar y salir"
            Mensaje3 = MeteDatoLetra(13, 9, 60, Mensaje3, "0")
            aux$ = Trim$(Mensaje3): libre% = INT((60 - LEN(aux$)) / 2)
            Mensaje3 = "": Mensaje3 = SPACE$(libre%) + aux$
            LOCATE 13, 9: PRINT "²"; Mensaje3; "²"
            IF devolver = true THEN devolver = false: sit% = 2 ELSE sit% = 4
            IF Fun(9) = "si" THEN salir = true
        CASE 4
            Center 15, "¨Desea especificar el impuesto? Pulse S ¢ N.": Center 25, "Solo pulse S ¢ N , NO TIENE OTRA OPCION"
            kbd$ = "": DO WHILE kbd$ = "": kbd$ = INKEY$: LOOP: kbd$ = UCASE$(kbd$)
            IF kbd$ = "S" OR kbd$ = "N" THEN
               IF kbd$ = "S" THEN NuevoIva = "si" ELSE NuevoIva = "no"
               sit% = 5
            ELSE
               sit% = 4: IF sonido = "si" THEN BEEP
            END IF
            IF NuevoIva = "si" THEN Center 15, "Se especificara el impuesto en las facturas." ELSE Center 15, "NO especificar  el impuesto en las facturas."
        CASE 5
            Center 17, "¨Desea especifar el porcentaje de descuento ? Pulse S ¢ N  ": Center 25, "Solo pulse S ¢ N , NO TIENE OTRA OPCION"
            kbd$ = "": DO WHILE kbd$ = "": kbd$ = INKEY$: LOOP: kbd$ = UCASE$(kbd$)
            IF kbd$ = "S" OR kbd$ = "N" THEN
               IF kbd$ = "S" THEN ShowPerc = "si" ELSE ShowPerc = "no"
               sit% = 1
            ELSE
               sit% = 5: IF sonido = "si" THEN BEEP
            END IF
            IF ShowPerc = "si" THEN Center 17, "Se especificara el porcentaje de descuento en las facturas." ELSE Center 17, "NO especificar  el porcentaje de descuento en las facturas."
        CASE ELSE
    END SELECT
LOOP
IF Fun(9) = "si" THEN Fun(9) = "no": Center 20, "Salvando la Informacion...": ArreglaTurno
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
            CASE CHR$(27): salida = true: salir = true: EXIT DO
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

SUB Redondear
Box 9, 19, 15, 61: Center 11, "Introduzca el Nuevo Valor:"
Center 13, "Valor Actual:" + Redondeo: Redondeo = "501": Center 14, " M ximo:999  M¡mimo: 0 (no redondea) "
DO WHILE VAL(Redondeo) > 500
  Redondeo = MeteDatoLetra$(12, 37, 3, "   ", "0")
LOOP
Center 12, " Salvando... ": SalvaEstado
END SUB

SUB SalvaEstado
'OPEN "A:\turno.dat" FOR OUTPUT SHARED AS #7
'PRINT #7, USING "#"; colorpref
'PRINT #7, turno
'PRINT #7, USING "##"; dia1
'PRINT #7, USING "##"; mes1
'PRINT #7, USING "####"; ano1
'CLOSE #7
OPEN "f:\kevinprg\kevindat\kaos.dat" FOR OUTPUT SHARED AS #2
PRINT #2, sonido: PRINT #2, impuesto
PRINT #2, Nombfarma: PRINT #2, Direccion
PRINT #2, Rif: PRINT #2, Nit
PRINT #2, Telefono: PRINT #2, contri
PRINT #2, contra: PRINT #2, Stock
PRINT #2, Blanq: PRINT #2, LargoH
PRINT #2, TipoL: PRINT #2, Mensaje1
PRINT #2, Mensaje2: PRINT #2, Mensaje3
PRINT #2, Redondeo: PRINT #2, Desc
PRINT #2, DescMax: PRINT #2, Exactitud
PRINT #2, Prov1: PRINT #2, Prov2
PRINT #2, Prov3: PRINT #2, negativo
PRINT #2, NuevoIva: PRINT #2, ShowPerc
CLOSE
END SUB

SUB Seniat
Box 9, 19, 14, 61
Center 11, "Introduzca el Nuevo Impuesto:"
Center 13, "Valor Actual:" + impuesto + " %"
impuesto = " 0.00"
DO WHILE VAL(impuesto) > 100 OR VAL(impuesto) <= 0
impuesto = EntraDatoNumero$(12, 37, 6, impuesto, 0)
LOOP
Center 12, " Salvando... "
SalvaEstado
END SUB

SUB StockJob
Box 9, 19, 14, 61
Center 11, "Introduzca el Nuevo Valor:"
Center 13, "Valor Actual:" + Stock
Stock = "0"
DO WHILE VAL(Stock) < 1 OR VAL(Stock) > 2
Stock = MeteDatoLetra$(12, 37, 1, Stock, "0")
LOOP
Center 12, " Salvando... "
SalvaEstado
END SUB

SUB TipoLetra
Box 9, 19, 15, 61: Center 11, "Introduzca el Tama¤o de Letra:": salir = false
Center 13, "Valor Actual:" + TipoL: TipoL = "N": Center 14, "Normal = [N ¢ n] Condensado = [C ¢ c]"
DO WHILE NOT salir
    TipoL = MeteDatoLetra$(12, 37, 1, TipoL, "0")
    IF TipoL = "c" OR TipoL = "C" OR TipoL = "n" OR TipoL = "N" THEN salir = true
LOOP
Center 12, " Salvando... "
SalvaEstado
END SUB

SUB TodosDesc
Box 9, 19, 15, 61: Center 11, "Introduzca el Nuevo Valor:"
Center 13, "Valor Actual:" + Desc: Desc = "90": Center 14, " M ximo: 50 %     M¡mimo: 0 %    "
DO WHILE VAL(Desc) > 50
Desc = MeteDatoLetra$(12, 37, 2, "0 ", "0")
LOOP
Center 12, " Salvando... ": SalvaEstado
END SUB

FUNCTION Trim$ (x$)
IF x$ = "" THEN
     Trim$ = ""
ELSE
lastChar = 0
FOR a = 1 TO LEN(x$): Y$ = MID$(x$, a, 1): IF Y$ <> CHR$(0) AND Y$ <> " " THEN lastChar = a
NEXT a: Trim$ = LEFT$(x$, lastChar)
END IF
END FUNCTION

