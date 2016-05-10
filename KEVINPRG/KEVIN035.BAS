'     borrar pedidos
DEFINT A-Z
DECLARE SUB Espera ()
DECLARE SUB Center (row, text$)
DECLARE SUB Box (Row1%, Col1%, Row2%, Col%)
DECLARE SUB Termometro (va%, total%, edo%, text$, fila%, columna%)
DECLARE SUB BorraPedidos ()
DECLARE SUB ConfigProv ()
DECLARE SUB LeeEstado ()
DECLARE SUB SalvaEstado ()
DECLARE SUB BorraPedidoSelecto (num%)
DECLARE FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%)
DECLARE FUNCTION Trim$ (x$)
DECLARE FUNCTION Verify (ubi%, text$)
COMMON SHARED guia, colorpref AS INTEGER
CONST true = -1: CONST false = NOT true
DIM SHARED turno AS STRING * 1, devolver
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, Impuesto AS STRING * 6, NombFarma AS STRING * 25, Direccion AS STRING * 30, Rif     AS STRING * 25, Nit      AS STRING * 25, Telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2, NuevoIva AS STRING * 2, ShowPerc AS STRING * 2
DIM SHARED Code(199) AS STRING * 3, Contenido(199) AS STRING * 15, nim AS STRING * 1, grabGav AS STRING * 1
ON TIMER(5) GOSUB AbreGav
TIMER ON
IF guia = 1 THEN CALL BorraPedidoSelecto(1)
IF guia = 2 THEN CALL BorraPedidoSelecto(2)
IF guia = 3 THEN CALL BorraPedidoSelecto(3)
IF guia = 4 THEN CALL BorraPedidos
IF guia = 5 THEN CALL ConfigProv
ON ERROR GOTO ErrorTrap
CLOSE : OPEN "f:\kevinprg\kevindat\compras.tkt" FOR RANDOM AS #1 LEN = 2: FIELD #1, 2 AS ticket$: GET #1, 1
RSET ticket$ = "si": PUT #1, 1: CLOSE
CHAIN "F:\kevinprg\kevin01.bas"
ErrorTrap:
    SELECT CASE ERR
        CASE 9
            Box 7, 20, 9, 60: Center 8, "No se pueden Adicionar Articulos Hasta Reiniciar el Sistema"
            SOUND 800, 2: Center 25, "No se pueden crear Mas Articulos Hasta Reiniciar el Sistema": Espera
            RESUME NEXT
        CASE 24, 25
            SOUND 600, 2: Center 25, "La impresora no responde ... (revise si esta encendida,on-line o falta papel)": Espera
            RESUME
        CASE 53 'no se encontro archivo ...
            RESUME NEXT
        CASE 70
            Center 25, " Esperando Acceso a Registro ...": SLEEP 1: Center 25, SPACE$(35): RESUME
        CASE ELSE
    END SELECT
    RESUME NEXT
END
DATA 0,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  3,     0,     1,     15,     0,     7,      0
DATA 3,    15,   13,  1,     14,    3,     15,     0,     7,      0
DATA 7,    12,   15,  4,     14,    0,     15,     15,    1,      0


'ATENCION AbreGav utiliza #29 y #30 en vez de #27 y #28
AbreGav:
  CLOSE #29
  OPEN "f:\kevinprg\kevindat\gaveta.sem" FOR RANDOM SHARED AS #29 LEN = 1
  GET #29, 1, grabGav
  SELECT CASE grabGav
    CASE "1"
      CLOSE #30: OPEN "com1" FOR OUTPUT AS #30: PRINT #30, "A": PRINT #30, "A": CLOSE #30
      grabGav = "0": PUT #29, 1, grabGav
    CASE "2"
      CLOSE #30: OPEN "com2" FOR OUTPUT AS #30: PRINT #30, "A": PRINT #30, "A": CLOSE #30
      grabGav = "0": PUT #29, 1, grabGav
    CASE ELSE
      'LOCATE 1, 1: PRINT grabGav
  END SELECT
  CLOSE #29
RETURN

SUB BorraPedidos
     aur% = Verify(13, "¨Desea borrar LOS ESPECIALMENTE MARCADOS")
     CLOSE : OPEN "F:\kevinprg\kevindat\master.pdd" FOR RANDOM AS #7 LEN = 67
     FIELD #7, 1 AS viva$, 4 AS numped$, 2 AS numprov$, 5 AS hora$, 10 AS fecha$, 4 AS cedven$, 40 AS obse$, 1 AS tipo$
     GET #7, 1: num$ = numped$: CLOSE : KILL "F:\kevinprg\kevindat\*.pdd"
     CLOSE : OPEN "F:\kevinprg\kevindat\master.pdd" FOR RANDOM AS #7 LEN = 67
     FIELD #7, 1 AS viva$, 4 AS numped$, 2 AS numprov$, 5 AS hora$, 10 AS fecha$, 4 AS cedven$, 40 AS obse$, 1 AS tipo$
     RSET numped$ = num$: PUT #7, 1: CLOSE
     OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
     FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS  _
cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$ _
, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
     max% = LOF(1) / 200: Termometro 1, max%, 0, "Borrando Pedidos...", 10, 10
     FOR k = 1 TO max%
          CALL Termometro(k, max%, 1, "Borrando Pedidos...", 10, 10)
          LOCK #1, k : GET #1, k: RSET ped2$ = MKI$(0)
          IF aur% = true THEN RSET upc$ = SPACE$(8)
          PUT #1, k: UNLOCK #1, k
     NEXT k: CLOSE
END SUB

SUB BorraPedidoSelecto (num%)
     CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
     FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS  _
cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$ _
, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
     OPEN "F:\kevinprg\kevindat\master.pdd" FOR RANDOM AS #7 LEN = 67
     FIELD #7, 1 AS viva$, 4 AS numped$, 2 AS numprov$, 5 AS hora$, 10 AS fecha$, 4 AS cedven$, 40 AS obse$, 1 AS tipo$
     maxped% = LOF(7) / 67: Termometro 1, maxped%, 0, "Borrando Pedidos...", 10, 10
     FOR k = 1 TO maxped%
          GET #7, k: CALL Termometro(k, maxped%, 1, "Borrando Pedidos...", 10, 10)
          IF VAL(tipo$) = num% AND viva$ = " " THEN
               RSET viva$ = "*": PUT #7, k
               num3$ = STR$(CVL(numped$)): num3$ = Trim$(MID$(num3$, 2, 8)): num3$ = "F:\kevinprg\kevindat\" + STRING$(8 - LEN(num3$), "0") + num3$ + ".pdd"
               OPEN num3$ FOR RANDOM AS #3 LEN = 19
               FIELD #3, 6 AS codi7$, 2 AS cant7$, 8 AS pvp37$, 1 AS paf7$, 2 AS sit7$
               maxpe% = LOF(3) / 19
               FOR l = 1 TO maxpe%
                    GET #3, l: LOCK #1, CVI(sit7$) : GET #1, CVI(sit7$)
                    IF codi7$ <> codigo2$ THEN Center 25, " OJO CODIGO NO SON IGUALES ": Espera
                    RSET ped2$ = MKI$(CVI(ped2$) - CVI(cant7$))
                    IF CVI(ped2$) < 0 THEN RSET ped2$ = MKI$(0)
                    PUT #1, CVI(sit7$): UNLOCK #1, CVI(sit7$)
               NEXT l
               CLOSE #3: KILL num3$
          END IF
     NEXT k
END SUB

SUB Box (Row1%, Col1%, Row2%, Col2%) STATIC
BoxWidth = Col2% - Col1% + 1: LOCATE Row1%, Col1%: PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿"; : FOR a = Row1% + 1 TO Row2% - 1: LOCATE a, Col1%: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2%, Col1%: PRINT "À"; STRING$(BoxWidth - 2,  _
"Ä"); "Ù";
END SUB

SUB Center (row, text$)
IF row = 25 THEN COLOR colors(5, colorpref), colors(4, colorpref): LOCATE 25, 1: PRINT SPACE$(80);
LOCATE row, 41 - LEN(text$) / 2: PRINT text$;
END SUB

SUB ConfigProv
CLS : Box 1, 1, 4, 80: Center 2, "Proceso de MARCAR Proveedores Con Ofertas"
CALL LeeEstado
CALL Center(24, "[ 1.-" + Prov1 + "] [ 2.-" + Prov2 + "] [ 3.-" + Prov3 + "]"): nim = "0"
CALL Box(6, 12, 8, 72): CALL Center(7, "Introduzca que Oferta 1,2,3 o 4 para ir a Menu Anterior:")
DO WHILE VAL(nim) < 1 OR VAL(nim) > 4
     llave$ = "": DO WHILE llave$ = "": llave$ = INKEY$: LOOP: nim = llave$: BEEP
LOOP
IF nim = "4" THEN CLOSE : EXIT SUB
FOR k = 5 TO 23: LOCATE k, 1: PRINT SPACE$(80); : NEXT k
Center 3, "Proveedor a ser Marcado:" + nim
COLOR colors(5, colorpref), colors(4, colorpref): LOCATE 5, 1: PRINT " Escoja el Proveedor:²000²Ninguno     "
control% = Elije%(5, 6, 22, 5, 23, 0)
IF control% > 199 OR control% < 1 THEN control% = 0
IF VAL(Code(control%)) = 1 OR control% = 0 THEN control% = 0: Center 24, "Proceso de Marcaje Abortado:Opcion Invalida.": Espera: EXIT SUB
IF nim = "1" THEN Prov1 = Contenido(control%)
IF nim = "2" THEN Prov2 = Contenido(control%)
IF nim = "3" THEN Prov3 = Contenido(control%)
CALL SalvaEstado
END SUB

FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%)
COLOR colors(4, colorpref), colors(7, colorpref): Box fila%, columna%, fila% + 16, columna% + 16: selecc% = 199
Center 25, "[F1]Ayuda [Enter]Tomar Opcion": COLOR colors(4, colorpref), colors(7, colorpref)
SELECT CASE estesi%
     CASE 1
          LOCATE fila%, columna + 2: PRINT "Ubicacion"
          CLOSE #2: OPEN "F:\kevinprg\kevindat\ubica.dat" FOR RANDOM AS #2 LEN = 18
          FIELD #2, 3 AS codigo$, 15 AS descripcion$
     CASE 2
          LOCATE fila%, columna% + 1: PRINT "Laboratorio"
          CLOSE #2: OPEN "F:\kevinprg\kevindat\lab.dat" FOR RANDOM AS #2 LEN = 18
          FIELD #2, 3 AS codigo$, 15 AS descripcion$
     CASE 4
          LOCATE fila%, columna% + 1: PRINT "Componente "; ingrediente%
          CLOSE #2: OPEN "F:\kevinprg\kevindat\componen.dat" FOR RANDOM AS #2 LEN = 18
          FIELD #2, 3 AS codigo$, 15 AS descripcion$
     CASE 5
          LOCATE fila%, columna% + 1: PRINT " Proveedor "
          CLOSE #2: OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #2 LEN = 18
          FIELD #2, 3 AS codigo$, 15 AS descripcion$
END SELECT
FOR k = 1 TO 199: GET #2, k: Code(k) = codigo$: Contenido(k) = descripcion$: NEXT k: CLOSE #2
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila% + 1, columna% + 1: PRINT Contenido(1): COLOR colors(4, colorpref), colors(7, colorpref)
FOR k = 2 TO 15: LOCATE fila% + k, columna% + 1: PRINT Contenido(k): NEXT k
resal% = 1: up% = 1: terminar = false
DO WHILE NOT terminar
kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
        SELECT CASE kbd$
            CASE CHR$(0) + "K": terminar = true: resal% = 0: up% = 0: devolver = true
            CASE CHR$(0) + "M": terminar = true: resal% = 0: up% = 0: devolver = false
            CASE CHR$(0) + "H"
               resal% = resal% - 1: IF resal% < 1 THEN resal% = 1: up% = up% - 1
               IF up% < 1 THEN up% = 1: terminar = true: devolver = true: resal% = 0: up% = 0
            CASE CHR$(0) + "P"
               resal% = resal% + 1: IF resal% > 15 THEN resal% = 15: up% = up% + 1
               IF up% + 14 > selecc% THEN terminar = true: resal% = 0: up% = 0: devolver = false
            CASE CHR$(0) + CHR$(73)
               up% = up% - 14: IF up% < 1 THEN up% = 1: resal% = 1
            CASE CHR$(0) + CHR$(81)
               up% = up% + 14: IF up% + 14 > selecc% THEN up% = selecc% - 14: resal% = 15
            CASE CHR$(13): terminar = true: devolver = false: EXIT DO
            CASE CHR$(27): terminar = true: resal% = 0: up% = 0: devolver = true
            CASE ELSE:  BEEP
        END SELECT
          IF up% = 0 THEN
               FOR k = 0 TO 14: LOCATE 8 + fila%, columna% + 1: PRINT SPACE$(15): NEXT k
               Elije% = up% + resal% - 1
               EXIT FUNCTION
          END IF
          l = 0
          FOR k = up% TO up% + 14: l = l + 1
          IF resal% = l THEN COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila2%, columna2% - 1: PRINT USING "²###²"; VAL(Code(k)); : PRINT Contenido(k)
          LOCATE fila% + l, columna% + 1: PRINT Contenido(k): IF resal% = l THEN COLOR colors(4, colorpref), colors(7, colorpref)
          NEXT k
LOOP
Elije% = up% + resal% - 1: EXIT FUNCTION
END FUNCTION

SUB Espera
Center 25, " Presione Cualquier Tecla Para Continuar ... "
DO: LOOP WHILE INKEY$ = ""
END SUB

SUB LeeEstado
CLOSE : OPEN "F:\kevinprg\kevindat\kaos.dat" FOR INPUT AS #2
INPUT #2, sonido: INPUT #2, Impuesto: INPUT #2, NombFarma: INPUT #2, Direccion
INPUT #2, Rif: INPUT #2, Nit: INPUT #2, Telefono: INPUT #2, contri
INPUT #2, contra: INPUT #2, Stock: INPUT #2, Blanq: INPUT #2, LargoH
INPUT #2, TipoL: INPUT #2, Mensaje1: INPUT #2, Mensaje2: INPUT #2, Mensaje3
INPUT #2, Redondeo: INPUT #2, Desc: INPUT #2, DescMax: INPUT #2, Exactitud
INPUT #2, Prov1: INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo
'INPUT #2, NuevoIva: INPUT #2, ShowPerc
CLOSE
END SUB

SUB SalvaEstado
CLOSE : OPEN "F:\kevinprg\kevindat\kaos.dat" FOR OUTPUT AS #2
PRINT #2, sonido: PRINT #2, Impuesto: PRINT #2, NombFarma: PRINT #2, Direccion
PRINT #2, Rif: PRINT #2, Nit: PRINT #2, Telefono: PRINT #2, contri
PRINT #2, contra: PRINT #2, Stock: PRINT #2, Blanq: PRINT #2, LargoH
PRINT #2, TipoL: PRINT #2, Mensaje1: PRINT #2, Mensaje2: PRINT #2, Mensaje3
PRINT #2, Redondeo: PRINT #2, Desc: PRINT #2, DescMax: PRINT #2, Exactitud
PRINT #2, Prov1: PRINT #2, Prov2: PRINT #2, Prov3: PRINT #2, negativo
PRINT #2, NuevoIva: PRINT #2, ShowPerc
CLOSE
END SUB

SUB Termometro (va%, total%, edo%, text$, fila%, columna%)
IF edo% = 0 THEN Box fila%, columna%, fila% + 6, columna% + 31
LOCATE fila% + 1, columna% + 1: PRINT "0%                        100%"
IF edo% = 0 THEN LOCATE fila% + 2, columna% + 1: PRINT STRING$(30, "±")
LOCATE fila% + 2, columna% + 1: PRINT STRING$(INT((va% / total%) * 30), "Û")
LOCATE fila% + 3, columna% + 1: PRINT "Van"; va%; " de "; total%;
LOCATE fila% + 4, columna% + 9: PRINT USING "###.## % Hecho "; ((va% / total%) * 100)
LOCATE fila% + 5, columna% + 1: PRINT text$
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

