DEFINT A-Z
DECLARE SUB MaxyMin ()
DECLARE SUB Inventario ()
DECLARE SUB Espera ()
DECLARE SUB LeeEstado ()
DECLARE SUB Center (row%, text$)
DECLARE SUB Box (row1%, col1%, row2%, col2%)
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$, tr$)
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%, est%)
DECLARE FUNCTION Trim$ (x$)
DECLARE FUNCTION DateToJulian& (dat$, caso%)
COMMON SHARED guia, colorpref AS INTEGER
DIM SHARED turno AS STRING * 1, devolver
DIM SHARED arriba AS INTEGER, abajo AS INTEGER, derecha AS INTEGER, izquierda AS INTEGER, RePag AS INTEGER, AvPag AS INTEGER
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, impuesto AS STRING * 6, Nombfarma AS STRING * 25, Direccion AS STRING * 30, Rif     AS STRING * 25, Nit      AS STRING * 25, Telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2
DIM SHARED Fun(17) AS STRING * 2, salir, Turna, terminar, sitio, busqueda
DIM SHARED Contenido(199) AS STRING * 15, Code(199) AS STRING * 3, grabGav AS STRING * 1
CONST true = -1: CONST false = NOT true: busqueda = 0
TYPE descuentos
  desc1 AS DOUBLE
  Desc2 AS DOUBLE
  Desc3 AS DOUBLE
END TYPE
REM $DYNAMIC
DIM SHARED Xa(1) AS STRING * 6, Xb(1) AS STRING * 2
'ON TIMER(5) GOSUB AbreGav
TIMER ON
LeeEstado
guia = 2: colorpref = 3
IF guia = 1 THEN MaxyMin
IF guia = 2 THEN Inventario
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... ": COLOR colors(7, colorpref), colors(4, colorpref)
CLOSE : CHAIN "F:\kevinprg\kevin01.bas"
ErrorTrap:
    SELECT CASE ERR
        CASE 9
            Box 7, 20, 9, 60: Center 8, "No se pueden Adicionar Articulos Hasta Reiniciar el Sistema"
            SOUND 800, 2: Center 25, "No se pueden crear Mas Articulos Hasta Reiniciar el Sistema": Espera: Fun(11) = "si"
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
DATA 1,    9,    12,  2,     0,     1,     15,     0,     7,      0
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

REM $STATIC
SUB Box (row1%, col1%, row2%, col2%) STATIC
BoxWidth% = col2% - col1% + 1: LOCATE row1%, col1%: PRINT "Ú"; STRING$(BoxWidth% - 2, "Ä"); "¿"; : FOR a = row1% + 1 TO row2% - 1: LOCATE a, col1%: PRINT "³"; SPACE$(BoxWidth% - 2); "³"; : NEXT a: LOCATE row2%, col1%: PRINT "À"; STRING$(BoxWidth% -  _
2, "Ä"); "Ù";
END SUB

SUB Center (row%, text$)
IF row% = 25 THEN COLOR colors(5, colorpref), colors(4, colorpref): LOCATE 25, 1: PRINT SPACE$(80);
LOCATE row%, 41 - LEN(text$) / 2: PRINT text$;
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
      d& = a& + B& + C&
      SELECT CASE ano%
         CASE 2000
            IF mes% > 2 THEN
               d& = d& - 109573
            END IF
         CASE IS > 2000
               d& = d& - 109573
         CASE ELSE
      END SELECT
      DateToJulian = d&


END FUNCTION

FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%, est%)
COLOR colors(4, colorpref), colors(7, colorpref): Box fila%, columna%, fila% + 16, columna% + 16: selecc% = 199
'atencion opcion de hacer inventario por componente,laboratorio,etc.
IF est% = 0 THEN Center 25, "[F1]Ayuda  [F5]Editar o Adicionar Linea  [F8]Borrar Linea  [Enter]Tomar Opcion" ELSE Center 25, "[F1]Ayuda  [Enter]Tomar Opcion"
COLOR colors(4, colorpref), colors(7, colorpref)
SELECT CASE estesi%
     CASE 1
          LOCATE fila%, columna + 2: PRINT "Ubicacion": CLOSE #2
          CLOSE #2: OPEN "F:\kevinprg\kevindat\ubica.dat" FOR RANDOM AS #2 LEN = 18
          FIELD #2, 3 AS codigo$, 15 AS Descripcion$
     CASE 2
          LOCATE fila%, columna% + 1: PRINT "Laboratorio"
          CLOSE #2: OPEN "F:\kevinprg\kevindat\lab.dat" FOR RANDOM AS #2 LEN = 18
          FIELD #2, 3 AS codigo$, 15 AS Descripcion$
     CASE 4
          LOCATE fila%, columna% + 1: PRINT "Componente "; ingrediente%
          CLOSE #2: OPEN "F:\kevinprg\kevindat\componen.dat" FOR RANDOM AS #2 LEN = 18
          FIELD #2, 3 AS codigo$, 15 AS Descripcion$
     CASE 5
          LOCATE fila%, columna% + 1: PRINT " Proveedor "
END SELECT
FOR k = 1 TO 199: GET #2, k: Code(k) = codigo$: Contenido(k) = Descripcion$: NEXT k: CLOSE #2
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
            CASE CHR$(13): terminar = true: devolver = false
            CASE CHR$(27): terminar = true: resal% = 0: up% = 0: devolver = true
            CASE CHR$(0) + CHR$(63): terminar = true: Fun(5) = "si": sitio = resal%
            CASE CHR$(0) + CHR$(66): terminar = true: Fun(8) = "si": sitio = resal%
            CASE ELSE:  BEEP
        END SELECT
          IF up% = 0 THEN
               FOR k = 0 TO 14: LOCATE 8 + fila%, columna% + 1: PRINT SPACE$(15): NEXT k
               Elije% = up% + resal% - 1 ' LOCATE 1, 1: PRINT "@@"; (up% + resal% - 1); "@@"
               EXIT FUNCTION
          END IF
          l = 0
          FOR k = up% TO up% + 14: l = l + 1
          IF resal% = l THEN COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila2%, columna2% - 1: PRINT USING "²###²"; VAL(Code(k)); : PRINT Contenido(k)
          LOCATE fila% + l, columna% + 1: PRINT Contenido(k): IF resal% = l THEN COLOR colors(4, colorpref), colors(7, colorpref)
          NEXT k
LOOP
Elije% = up% + resal% - 1  ': LOCATE 1, 1: PRINT "@@"; VAL(Code(up% + resal% - 1)); "@@"
END FUNCTION

FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
arriba = false: abajo = false: derecha = false: izquierda = false: RePag = false: AvPag = false
LOCATE fila%, columna%: PRINT CHR$(178) + caden$ + SPACE$(largo% - LEN(caden$)): LOCATE fila%, columna% + largo% + 1: PRINT CHR$(178)
REDIM eleccion(0 TO largo% + 1) AS STRING * 1: Fun(12) = "no": devolver = false: llevasitio% = 0
FOR k = 1 TO largo%
   eleccion(k) = MID$(caden$, k, 1)
   IF llevasitio% = 0 THEN
      IF eleccion(k) > " " THEN llevasitio = k
   END IF
NEXT k: IF llevasitio% = 0 THEN llevasitio% = 2
LOCATE fila%, (columna% + llevasitio%)
COLOR colors(4, colorpref) + 16, colors(7, colorpref)
IF eleccion(llevasitio%) = " " THEN PRINT "_";  ELSE PRINT eleccion(llevasitio%)
DO
      llave$ = "": COLOR colors(4, colorpref), colors(7, colorpref)
      WHILE llave$ = ""
         llave$ = INKEY$: LOCATE 2, 67
         IF VAL(MID$(TIME$, 1, 2)) > 12 THEN
            tim$ = STR$((VAL(MID$(TIME$, 1, 2)) - 12)) + MID$(TIME$, 3, 6) + "p.m.": PRINT tim$
         ELSE
            PRINT TIME$; "a.m."
         END IF
      WEND
          SELECT CASE llave$
               CASE CHR$(0) + "H": arriba = true: Fun(12) = "si": devolver = true: EXIT DO
               CASE CHR$(0) + "P": abajo = true: Fun(12) = "si": EXIT DO
               CASE CHR$(0) + "K"
                  llevasitio% = llevasitio% - 1
                  IF llevasitio% = 0 THEN izquierda = true: GOSUB Sumar2: devolver = true: EXIT DO
               CASE CHR$(0) + "M"
                  llevasitio% = llevasitio% + 1
                  IF llevasitio% > largo% THEN derecha = true: GOSUB Sumar2: EXIT DO
               CASE CHR$(8)
                  llevasitio% = llevasitio% - 1
                  IF llevasitio% = 0 THEN GOSUB Sumar2: izquierda = true: EXIT DO
                  FOR k = llevasitio% TO largo% - 1: eleccion(k) = eleccion(k + 1)
                  NEXT k: eleccion(largo%) = " "
               CASE CHR$(0) + CHR$(83)
                  FOR k = llevasitio% TO largo%: eleccion(k) = eleccion(k + 1): NEXT k
                  eleccion(largo%) = " "
               CASE CHR$(13): derecha = true: EXIT DO
               CASE CHR$(0) + CHR$(73): RePag = true: EXIT DO
               CASE CHR$(0) + CHR$(81): AvPag = true: EXIT DO
               CASE CHR$(27): izquierda = true: devolver = true: EXIT DO
               'CASE CHR$(0) + CHR$(59): Fun(1) = "si": EXIT DO
               'CASE CHR$(0) + CHR$(60): Fun(2) = "si": EXIT DO
               'CASE CHR$(0) + CHR$(61): Fun(3) = "si": EXIT DO
               'CASE CHR$(0) + CHR$(62): Fun(4) = "si": EXIT DO
               'CASE CHR$(0) + CHR$(63): Fun(5) = "si": EXIT DO
               'CASE CHR$(0) + CHR$(64): Fun(6) = "si": EXIT DO
               'CASE CHR$(0) + CHR$(65): Fun(7) = "si": EXIT DO
               'CASE CHR$(0) + CHR$(66): Fun(8) = "si": EXIT DO
               CASE CHR$(0) + CHR$(67): Fun(9) = "si": EXIT DO
               'CASE CHR$(0) + CHR$(68): Fun(10) = "si": EXIT DO
               CASE CHR$(0) + CHR$(71): llevasitio% = 1
               CASE CHR$(0) + CHR$(79): llevasitio% = largo%
               CASE " ": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: derecha = true: EXIT DO
               CASE "1": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: derecha = true: EXIT DO
               CASE "2": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: derecha = true: EXIT DO
               CASE "3": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: derecha = true: EXIT DO
               CASE "4": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: derecha = true: EXIT DO
               CASE "5": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: derecha = true: EXIT DO
               CASE "6": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: derecha = true: EXIT DO
               CASE "7": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: derecha = true: EXIT DO
               CASE "8": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: derecha = true: EXIT DO
               CASE "9": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: derecha = true: EXIT DO
               CASE "0": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: derecha = true: EXIT DO
               CASE "+": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: derecha = true: EXIT DO
               CASE "-": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: derecha = true: EXIT DO
               CASE ".": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: derecha = true: EXIT DO
               CASE ELSE: IF sonido = "si" THEN BEEP
          END SELECT
          FOR k = 1 TO largo%
          IF k = llevasitio% THEN COLOR colors(4, colorpref) + 16, colors(7, colorpref)
          LOCATE fila%, (columna% + k)
          IF k = llevasitio% THEN
                    IF eleccion(k) <> " " THEN PRINT eleccion(k);  ELSE PRINT "_";
          ELSE
                    PRINT eleccion(k);
          END IF
          IF k = llevasitio% THEN COLOR colors(7, colorpref), colors(4, colorpref)
          NEXT k
LOOP
eleccion$ = "": eleccion(1) = UCASE$(eleccion(1)): FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna% + 1: PRINT eleccion$
IF entero% = 1 THEN eleccion$ = STR$(FIX(VAL(eleccion$)))
EntraDatoNumero$ = eleccion$: IF sonido = "si" THEN BEEP
EXIT FUNCTION
EntraNumero:
eleccion(llevasitio%) = llave$: llevasitio% = llevasitio% + 1
RETURN
Sumar2:
FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna%: PRINT "²"; eleccion$; "²"
RETURN

END FUNCTION

SUB Espera
Center 25, "Presione Cualquier Tecla Para Continuar...": DO: LOOP WHILE INKEY$ = ""
END SUB

SUB Inventario
DIM dcto AS descuentos
COLOR colors(5, colorpref), colors(4, colorpref): CLS : Box 1, 1, 3, 80: Center 25, "[F1]Ayuda  [F9]Salir ": salir = false: sit% = 1
LOCATE 4, 34: PRINT "Desde:"; : Center 25, "[F1]Ayuda  [F9]Salir "
DO WHILE NOT salir
     SELECT CASE sit%
     CASE 1
          COLOR colors(5, colorpref), colors(4, colorpref): LOCATE 4, 9: PRINT "²04²Sin Ubicacion": Center 2, "Proceso de Introduccion de Inventario"
          'atencion con esta opcion sumar las 4 existencias y anotarlo en saldo$
          control% = Elije%(1, 5, 10, 4, 10, 0, 1): IF control% < 0 THEN sit% = 1 ELSE sit% = 2
     CASE 2
          CALL Center(25, "[F1]Ayuda  [F9]Salir ")
          Prud1$ = UCASE$(MeteDatoLetra$(4, 40, 4, Prud1$, "0"))
          : Prud1$ = Trim$(Prud1$)
          IF Fun(9) = "si" THEN EXIT SUB
          IF devolver = true THEN devolver = false: sit% = 1
          IF Prud1$ = "" THEN sit% = 2 ELSE salir = true
     END SELECT
LOOP
LOCATE 5, 1: PRINT "ÄDescripcionÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂExis2ÂÄÄÄÄÄPvp2ÄÂDesc2ÂExis3ÂÄÄÄÄÄPvp3ÄÂDesc3¿";
LOCATE 17, 1: PRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÙ";
Center 25, "Use Las Teclas de Direccion,Escape y Enter ; Salga Con [F9]"
ON ERROR GOTO ErrorTrap
CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
OPEN "F:\kevinprg\kevindat\ubica.idx" FOR RANDOM SHARED AS #2 LEN = 10: FIELD #2, 8 AS codigo$, 2 AS num$
OPEN "f:\kevinprg\kevindat\desc.dat" FOR RANDOM SHARED AS #48 LEN = LEN(dcto)
max2% = LOF(2) / 10: apu% = 0: salir = false: apa% = 1
COLOR 7, 0: FOR k = 12 TO 16: LOCATE k, 1: PRINT SPACE$(80); : NEXT k
COLOR 0, 15: FOR k = 18 TO 24: LOCATE k, 1: PRINT SPACE$(80); : NEXT k
Center 20, "Editando: descripcion y observaciones"
FOR k = 1 TO max2%
  GET #2, k
  IF MID$(codigo$, 1, LEN(Prud1$) + 2) = (MKI$(VAL(Code(control%))) + Prud1$) THEN apu% = k: k = max2%
NEXT k: IF apu% = 0 THEN CLOSE : Center 17, "² No se Encontro ²": Espera: EXIT SUB
''Impresion:
DO WHILE NOT salir
     COLOR colors(4, colorpref), colors(5, colorpref): l = 5
     FOR k = apu% - 5 TO apu% + 5
          l = l + 1: IF k = apu% THEN COLOR colors(7, colorpref), colors(4, colorpref)
          LOCATE l, 1: PRINT SPACE$(80): LOCATE l, 1
          IF k > 0 AND k <= max2% THEN
               GET #2, k: GET #1, CVI(num$): GET #48, CVI(num$), dcto
               PRINT MID$(nomb2$, 1, 33)
               LOCATE l, 34
               PRINT "³"; : PRINT USING "#####"; CVI(exi22$);
               PRINT "³"; : PRINT USING "######,.##"; CVD(pv22$);
               PRINT "³"; : PRINT USING "##.##"; dcto.Desc2;
               PRINT "³"; : PRINT USING "#####"; CVI(exi32$);
               PRINT "³"; : PRINT USING "######,.##"; CVD(pv32$);
               PRINT "³"; : PRINT USING "##.##"; dcto.Desc3;
          ELSE
              PRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÙ"
          END IF
          IF k = apu% THEN COLOR colors(5, colorpref), colors(4, colorpref)
     NEXT k
     GET #2, apu%: LOCK #1, CVI(num$)
     GET #1, CVI(num$): GET #48, CVI(num$), dcto: Fun(12) = "no": devolver = false: terminar = false: LOCATE 21, 10: PRINT nomb2$ + "     " + obser2$
     DO WHILE NOT terminar
          SELECT CASE apa%
               CASE 1
                    eleccion$ = EntraDatoNumero$(11, 34, 5, STR$(CVI(exi22$)), 1)
                    RSET exi12$ = MKI$(VAL(eleccion$)): LOCATE 11, 34: PRINT USING "³#####³"; CVI(exi22$);
                    apa% = 2
                    IF Fun(12) = "si" AND devolver = true THEN
                         GOSUB Salvar: apa% = 1: apu% = apu% - 1: terminar = true
                         IF apu% <= 0 THEN apu% = 1
                    END IF
                    IF devolver = true AND Fun(12) = "no" THEN
                         GOSUB Salvar: apa% = 6: apu% = apu% - 1: terminar = true
                         IF apu% <= 0 THEN apu% = 1
                    END IF
                    IF Fun(12) = "si" AND devolver = false THEN
                         GOSUB Salvar: apa% = 1: apu% = apu% + 1: terminar = true
                         IF apu% > max2% THEN EXIT SUB
                    END IF
                    IF devolver = true THEN UNLOCK #1, CVI(num$)
                    IF Fun(9) = "si" THEN GOSUB Salvar
               CASE 2
                    eleccion$ = EntraDatoNumero$(11, 40, 10, STR$((FIX(CVD(pv22$) * 100)) / 100), 1)
                    RSET pv22$ = MKD$(VAL(eleccion$)): LOCATE 11, 40: PRINT USING "³######,.##³"; CVD(pv22$);
                    apa% = 3
                    IF Fun(12) = "si" AND devolver = true THEN
                         GOSUB Salvar: apa% = 2: apu% = apu% - 1: terminar = true
                         IF apu% <= 0 THEN apu% = 1
                    END IF
                    IF devolver = true AND Fun(12) = "no" THEN apa% = 1
                    IF Fun(12) = "si" AND devolver = false THEN
                         GOSUB Salvar: apa% = 2: apu% = apu% + 1: terminar = true
                         IF apu% > max2% THEN EXIT SUB
                    END IF
                    IF devolver = true THEN UNLOCK #1, CVI(num$)
                    IF Fun(9) = "si" THEN GOSUB Salvar
               CASE 3
                    eleccion$ = EntraDatoNumero$(11, 51, 5, STR$(dcto.Desc2), 1)
                    dcto.Desc2 = VAL(eleccion$): LOCATE 11, 51: PRINT USING "³##.00³"; dcto.Desc2;
                    apa% = 4
                    IF Fun(12) = "si" AND devolver = true THEN
                         GOSUB Salvar: apa% = 3: apu% = apu% - 1: terminar = true
                         IF apu% <= 0 THEN apu% = 1
                    END IF
                    IF devolver = true AND Fun(12) = "no" THEN apa% = 2
                    IF Fun(12) = "si" AND devolver = false THEN
                         GOSUB Salvar: apa% = 3: apu% = apu% + 1: terminar = true
                         IF apu% > max2% THEN EXIT SUB
                    END IF
                    IF devolver = true THEN UNLOCK #1, CVI(num$)
                    IF Fun(9) = "si" THEN GOSUB Salvar
               CASE 4
                    eleccion$ = EntraDatoNumero$(11, 57, 5, STR$(CVI(exi32$)), 1)
                    RSET exi32$ = MKI$(VAL(eleccion$)): LOCATE 11, 57: PRINT USING "³#####³"; CVI(exi32$);
                    apa% = 5
                    IF Fun(12) = "si" AND devolver = true THEN
                         GOSUB Salvar: apa% = 4: apu% = apu% - 1: terminar = true
                         IF apu% <= 0 THEN apu% = 1
                    END IF
                    IF devolver = true AND Fun(12) = "no" THEN apa% = 3
                    IF Fun(12) = "si" AND devolver = false THEN
                        GOSUB Salvar: apa% = 4: apu% = apu% + 1: terminar = true
                        IF apu% > max2% THEN EXIT SUB
                    END IF
                    IF devolver = true THEN UNLOCK #1, CVI(num$)
                    IF Fun(9) = "si" THEN GOSUB Salvar
               CASE 5
                    eleccion$ = EntraDatoNumero$(11, 63, 10, STR$((FIX(CVD(pv32$) * 100)) / 100), 1)
                    RSET pv32$ = MKD$(VAL(eleccion$)): LOCATE 11, 63: PRINT USING "³######,.##³"; CVD(pv32$);
                    apa% = 6
                    IF Fun(12) = "si" AND devolver = true THEN
                         GOSUB Salvar: apa% = 5: apu% = apu% - 1: terminar = true
                         IF apu% <= 0 THEN apu% = 1
                    END IF
                    IF devolver = true AND Fun(12) = "no" THEN apa% = 4
                    IF Fun(12) = "si" AND devolver = false THEN
                         GOSUB Salvar: apa% = 5: apu% = apu% + 1: terminar = true
                         IF apu% > max2% THEN EXIT SUB
                    END IF
                    IF devolver = true THEN UNLOCK #1, CVI(num$)
                    IF Fun(9) = "si" THEN GOSUB Salvar
          CASE 6
               eleccion$ = EntraDatoNumero$(11, 74, 5, STR$(dcto.Desc3), 1)
               dcto.Desc3 = VAL(eleccion$): LOCATE 11, 74: PRINT USING "³##.00³"; dcto.Desc3;
               apa% = 1: terminar = true
               IF Fun(12) = "si" AND devolver = true THEN
                    GOSUB Salvar: apa% = 6: terminar = true
                    apu% = apu% - 1: IF apu% <= 0 THEN apu% = 1
               END IF
               IF devolver = true AND Fun(12) = "no" THEN apa% = 5
               IF Fun(12) = "si" AND devolver = false THEN
                    GOSUB Salvar: apa% = 6: apu% = apu% + 1: terminar = true
                    IF apu% > max2% THEN EXIT SUB
               END IF
               IF Fun(9) = "si" THEN GOSUB Salvar
               IF Fun(12) = "no" AND devolver = false THEN GOSUB Salvar: apa% = 1: apu% = apu% + 1
               IF devolver = true THEN UNLOCK #1, CVI(num$)
               IF apu% > max2% THEN EXIT SUB
          END SELECT
     LOOP
LOOP

Salvar:
'UNLOCK #1, apu%
IF (CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$)) > 32500 THEN RSET saldo$ = MKI$(32500) ELSE RSET saldo$ = MKI$((CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$)))
GET #2, apu%: PUT #1, CVI(num$): UNLOCK #1, CVI(num$) : PUT #48, CVI(num$), dcto
IF Fun(9) = "si" THEN CLOSE : Fun(9) = "no": EXIT SUB ELSE RETURN
RETURN
END SUB

SUB LeeEstado
CLOSE : OPEN "F:\kevinprg\kevindat\kaos.dat" FOR INPUT AS #2
INPUT #2, sonido: INPUT #2, impuesto: INPUT #2, Nombfarma: INPUT #2, Direccion
INPUT #2, Rif: INPUT #2, Nit: INPUT #2, Telefono: INPUT #2, contri: INPUT #2, contra
INPUT #2, Stock: INPUT #2, Blanq: INPUT #2, LargoH: INPUT #2, TipoL: INPUT #2, Mensaje1
INPUT #2, Mensaje2: INPUT #2, Mensaje3: INPUT #2, Redondeo: INPUT #2, desc
INPUT #2, DescMax: INPUT #2, Exactitud: INPUT #2, Prov1: INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo
CLOSE
END SUB

SUB MaxyMin
DIM promedio AS STRING * 8, dias AS INTEGER: dias = 7
COLOR colors(5, colorpref), colors(4, colorpref): CLS : Box 1, 1, 3, 80: Center 25, "[F1]Ayuda  [F9]Salir ": salir = false: sit% = 1
LOCATE 4, 34: PRINT "Desde:"; : Center 25, "[F1]Ayuda  [F9]Salir ": STATIC apu%, apa%
COLOR colors(5, colorpref), colors(4, colorpref): Center 2, " Proceso de Asignacion de Stocks Minimos y Maximos "
DO WHILE NOT salir
          Prud1$ = MeteDatoLetra$(4, 40, 4, Prud1$, "0")
          Prud1$ = Trim$(Prud1$): Prud1$ = UCASE$(Prud1$)
          IF Fun(9) = "si" THEN EXIT SUB
          IF LEN(Prud1$) > 0 THEN salir = true
LOOP
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... ": COLOR colors(7, colorpref), colors(4, colorpref)
re$ = "Descripci¢n" + STRING$(27, "Ä") + "M¡nimo1ÄM ximo1ÄM¡nimo2ÄM ximo2ÄÄÄÄExist."
Center 17, STRING$(80, "Ä"): LOCATE 5, 1: PRINT re$
CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
OPEN "F:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
FIELD #2, 35 AS nomshort$, 2 AS num$: max2% = LOF(2) / 37
OPEN "F:\kevinprg\kevinvnt\estadist.dat" FOR RANDOM SHARED AS #3 LEN = 20
FIELD #3, 4 AS unid$, 8 AS primera$, 8 AS ultima$
ERASE Xa, Xb
DIM Xa(max2%) AS STRING * 6, Xb(max2%) AS STRING * 2
apu% = 0: salir = false
FOR k = 1 TO max2%
   GET #2, k: Xa(k) = nomshort$: Xb(k) = num$
NEXT k
apa% = 1: COLOR 7, 0: FOR k = 12 TO 16: LOCATE k, 1: PRINT SPACE$(80): NEXT k: CLOSE #2
COLOR 0, 15: FOR k = 18 TO 24: LOCATE k, 1: PRINT SPACE$(80); : NEXT k
FOR k = 1 TO max2%   ': LOCATE 1, 1: PRINT MID$(Xa(k), 1, LEN(Prud1$)), Prud1$: SLEEP 1
IF MID$(Xa(k), 1, LEN(Prud1$)) = Prud1$ THEN apu% = k: k = max2%
NEXT k
IF apu% = 0 THEN CLOSE : Center 17, "² No se Encontro Articulos Con Ese Nombre ²": Espera: EXIT SUB
'Impresion:
DO WHILE NOT salir
     COLOR 7, 0: l = 5
     FOR k = apu% - 5 TO apu% + 5
          l = l + 1
          IF k = apu% THEN COLOR 0, 7
          LOCATE l, 1: PRINT SPACE$(80): LOCATE l, 1
          IF k > 0 AND k <= max2% THEN
             GET #1, CVI(Xb(k))
             PRINT nomb2$
             LOCATE l, 40: PRINT USING "#####,"; CVI(mini12$)
             LOCATE l, 48: PRINT USING "#####,"; CVI(maxi12$)
             LOCATE l, 56: PRINT USING "#####,"; CVI(mini22$)
             LOCATE l, 64: PRINT USING "#####,"; CVI(maxi22$)
             LOCATE l, 73: PRINT USING "######,"; (CVI(exi12$) + CVI(exi22$) + CVI(exi32$))
          ELSE
            PRINT SPACE$(80)
          END IF
     IF k = apu% THEN COLOR 7, 0
     NEXT k
     LOCK #1, CVI(Xb(apu%)) : GET #1, CVI(Xb(apu%)): terminar = false
     GOSUB CalculaPromedio
     LOCATE 20, 5: PRINT nomb2$; "    "; obser2$
     LOCATE 21, 5: PRINT SPACE$(71)
     IF CVI(exi12$) > 0 THEN
         LOCATE 21, 5: PRINT USING "#####,"; CVI(exi12$);
         PRINT " a ";
         PRINT USING "########,.##"; CVD(pv12$);
     END IF
     IF CVI(exi22$) > 0 THEN
         LOCATE 21, 30: PRINT USING "#####,"; CVI(exi22$);
         PRINT " a ";
         PRINT USING "########,.##"; CVD(pv22$);
     END IF
     IF CVI(exi32$) > 0 THEN
         LOCATE 21, 55: PRINT USING "#####,"; CVI(exi32$);
         PRINT " a ";
         PRINT USING "########,.##"; CVD(pv32$);
     END IF
     LOCATE 22, 5: PRINT "Existencia total: ";
     PRINT USING "#####,"; (CVI(exi12$) + CVI(exi22$) + CVI(exi32$));
     PRINT ", promedio semanal:";
     PRINT USING "####,.##"; CVD(promedio);
     DO WHILE NOT terminar
          SELECT CASE apa%
               CASE 1
                    eleccion$ = STR$(CVI(mini12$))
                    eleccion$ = SPACE$(6 - LEN(eleccion$)) + MID$(eleccion$, 2, LEN(eleccion$) - 1) + " "
                    eleccion$ = EntraDatoNumero$(11, 40, 6, eleccion$, 1): IF VAL(eleccion$) > 32500 THEN eleccion$ = "32500"
                    RSET mini12$ = MKI$(VAL(eleccion$))
                    LOCATE 11, 40: PRINT " "; : PRINT USING "#####,"; CVI(mini12$); : PRINT " ";
                    GOSUB Distribuidor
               CASE 2
                    eleccion$ = STR$(CVI(maxi12$))
                    eleccion$ = SPACE$(6 - LEN(eleccion$)) + MID$(eleccion$, 2, LEN(eleccion$) - 1) + " "
                    eleccion$ = EntraDatoNumero$(11, 48, 6, eleccion$, 1): IF VAL(eleccion$) > 32500 THEN eleccion$ = "32500"
                    RSET maxi12$ = MKI$(VAL(eleccion$))
                    LOCATE 11, 48: PRINT " "; : PRINT USING "#####,"; CVI(maxi12$); : PRINT " ";
                    GOSUB Distribuidor
                CASE 3
                    eleccion$ = STR$(CVI(mini22$))
                    eleccion$ = SPACE$(6 - LEN(eleccion$)) + MID$(eleccion$, 2, LEN(eleccion$) - 1) + " "
                    eleccion$ = EntraDatoNumero$(11, 56, 6, eleccion$, 1): IF VAL(eleccion$) > 32500 THEN eleccion$ = "32500"
                    RSET mini22$ = MKI$(VAL(eleccion$))
                    LOCATE 11, 56: PRINT " "; : PRINT USING "#####,"; CVI(mini22$); : PRINT " ";
                    GOSUB Distribuidor
                CASE 4
                    eleccion$ = STR$(CVI(maxi22$))
                    eleccion$ = SPACE$(6 - LEN(eleccion$)) + MID$(eleccion$, 2, LEN(eleccion$) - 1) + " "
                    eleccion$ = EntraDatoNumero$(11, 64, 6, eleccion$, 1): IF VAL(eleccion$) > 32500 THEN eleccion$ = "32500"
                    RSET maxi22$ = MKI$(VAL(eleccion$))
                    LOCATE 11, 64: PRINT " "; : PRINT USING "#####,"; CVI(mini22$); : PRINT " ";
                    GOSUB Distribuidor
           END SELECT
     LOOP
LOOP

Salv:
   IF CVI(maxi12$) = 0 OR CVI(maxi12$) < CVI(mini12$) THEN RSET maxi12$ = mini12$
   IF CVI(maxi22$) = 0 OR CVI(maxi22$) < CVI(mini22$) THEN RSET maxi22$ = mini22$
   PUT #1, CVI(Xb(apu%)): UNLOCK #1, CVI(Xb(apu%)) : Center 25, "[F1]Ayuda  [F9]Salir y Salvar"
   IF Fun(9) = "si" THEN CLOSE : Fun(9) = "no": EXIT SUB ELSE RETURN
RETURN

Distribuidor:
   terminar = true: GOSUB Salv
   SELECT CASE apa%
      CASE 1
         IF arriba = true THEN
            apa% = 1: apu% = apu% - 1
            IF apu% <= 0 THEN apu% = 1
         END IF
         IF abajo = true THEN
            apa% = 1: apu% = apu% + 1
            IF apu% > max2% THEN apu% = max2%: IF sonido = "si" THEN BEEP
         END IF
         IF derecha = true THEN
            apa% = 2
         END IF
         IF izquierda = true THEN
            apa% = 4: apu% = apu% - 1
            IF apu% <= 0 THEN apu% = 1
         END IF
         IF RePag = true THEN
            apa% = 1: apu% = apu% - 10
            IF apu% <= 0 THEN apu% = 1
         END IF
         IF AvPag = true THEN
            apa% = 1: apu% = apu% + 10
            IF apu% > max2% THEN apu% = max2%: IF sonido = "si" THEN BEEP
         END IF
      CASE 2
         IF arriba = true THEN
            apa% = 2: apu% = apu% - 1
            IF apu% <= 0 THEN apu% = 1
         END IF
         IF abajo = true THEN
            apa% = 2: apu% = apu% + 1
            IF apu% > max2% THEN EXIT SUB
         END IF
         IF derecha = true THEN
            apa% = 3
         END IF
         IF izquierda = true THEN
            apa% = 1
         END IF
         IF RePag = true THEN
            apa% = 2: apu% = apu% - 10
            IF apu% <= 0 THEN apu% = 1
         END IF
         IF AvPag = true THEN
            apa% = 2: apu% = apu% + 10
            IF apu% > max2% THEN apu% = max2%: IF sonido = "si" THEN BEEP
         END IF
      CASE 3
         IF arriba = true THEN
            apa% = 3: apu% = apu% - 1
            IF apu% <= 0 THEN apu% = 1
         END IF
         IF abajo = true THEN
            apa% = 3: apu% = apu% + 1
            IF apu% > max2% THEN EXIT SUB
         END IF
         IF derecha = true THEN
            apa% = 4
         END IF
         IF izquierda = true THEN
            apa% = 2
         END IF
         IF RePag = true THEN
            apa% = 3: apu% = apu% - 10
            IF apu% <= 0 THEN apu% = 1
         END IF
         IF AvPag = true THEN
            apa% = 3: apu% = apu% + 10
            IF apu% > max2% THEN apu% = max2%: IF sonido = "si" THEN BEEP
         END IF
      CASE 4
         IF arriba = true THEN
            apa% = 4: apu% = apu% - 1
            IF apu% <= 0 THEN apu% = 1
         END IF
         IF abajo = true THEN
            apa% = 4: apu% = apu% + 1
            IF apu% > max2% THEN EXIT SUB
         END IF
         IF derecha = true THEN
            apa% = 1: apu% = apu% + 1
            IF apu% > max2% THEN EXIT SUB
         END IF
         IF izquierda = true THEN
            apa% = 3
         END IF
         IF RePag = true THEN
            apa% = 4: apu% = apu% - 10
            IF apu% <= 0 THEN apu% = 1
         END IF
         IF AvPag = true THEN
            apa% = 4: apu% = apu% + 10
            IF apu% > max2% THEN apu% = max2%: IF sonido = "si" THEN BEEP
         END IF
   END SELECT
RETURN

CalculaPromedio:
GET #3, CVI(Xb(apu%)): promedio = MKD$(0)
IF ultima$ <> "        " AND primera$ <> "        " THEN
  IF ultima$ = primera$ THEN
     promedio = MKD$(0)
  ELSE
     IF (DateToJulian(ultima$, 1) - DateToJulian(primera$, 1)) > 90 THEN
        IF (DateToJulian(DATE$, 3) - DateToJulian(ultima$, 1)) < 90 THEN
           promedio = MKD$(dias * CVL(unid$) / (DateToJulian(ultima$, 1) - DateToJulian(primera$, 1)))
        ELSE
           promedio = MKD$(0)
        END IF
     ELSE
        promedio = MKD$(0)
     END IF
  END IF
ELSE
  ' verifica que de verdad sea "nuevo" el producto en la farmacia
  IF CVI(Xb(apu%)) >= (LOF(1) / 200 - 50) THEN
      promedio = MKD$(0)
  ELSE
      promedio = MKD$(0)
  END IF
END IF
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
                  llevasitio% = llevasitio% - 1: IF llevasitio% = 0 THEN GOSUB Sumar: devolver = true: EXIT DO
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
               CASE CHR$(27): devolver = true: EXIT DO
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

