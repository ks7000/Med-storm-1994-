'   permite sacar fallas de medicinas
DEFINT A-Z
DECLARE SUB Fallas (sta%)
DECLARE SUB Espera ()
DECLARE SUB LeeEstado ()
DECLARE SUB Center (row, text$)
DECLARE SUB Box (Row1%, Col1%, Row2%, Col%)
DECLARE SUB BorraBox (Row1%, Col1%, Row2%, Col%)
DECLARE SUB Termometro (va%, total%, edo%, text$, fila%, columna%)
DECLARE SUB Suma1Dia (dat$, caso%)
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$)
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Escoge% (fila%, edo%, fila2%, Prod$)
DECLARE FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%, est%)
DECLARE FUNCTION CheqDate (caden$, op%)
DECLARE FUNCTION CheqNum$ (cadn$, ente%, larg%)
DECLARE FUNCTION Trim$ (x$)
DECLARE FUNCTION DateToJulian& (dat$, caso%)
DECLARE FUNCTION Tratafechas$ (dat$, fechaentra$, fechasale$)
DECLARE FUNCTION Verify (ubi%, text$)
COMMON SHARED guia, colorpref AS INTEGER
'colorpref = 1: guia = 1
DIM SHARED devolver AS INTEGER, repag AS INTEGER
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, impuesto AS STRING * 6, Nombfarma AS STRING * 25, Direccion AS STRING * 30, Rif     AS STRING * 25, Nit      AS STRING * 25, Telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2
DIM SHARED Fun(17) AS STRING * 2, salir, Turna, terminar, sitio, busqueda, atras
DIM SHARED Contenido(199) AS STRING * 15, Code(199) AS STRING * 3, acum AS STRING * 8, van AS STRING * 8
DIM SHARED au1 AS STRING * 19, au2 AS STRING * 19, au3 AS STRING * 38, au4 AS STRING * 38
REM $DYNAMIC
DIM SHARED Nombra(5) AS STRING * 35, Pvp1a(5) AS STRING * 8, Pvp2a(5) AS STRING * 8, Pvp3a(5) AS STRING * 8, Pvp4a(5) AS STRING * 8, Numa(5) AS STRING * 2, Observa(5) AS STRING * 20, Exis1a(5) AS STRING * 2, Exis2a(5) AS STRING * 2, Exis3a(5) AS  _
STRING * 2, Exis4a(5) AS STRING * 2
DIM SHARED Cant1a(5) AS STRING * 2, Cant2a(5) AS STRING * 2, Cant3a(5) AS STRING * 2, Cant4a(5) AS STRING * 2
DIM SHARED Xa(10) AS STRING * 6, Xb(10) AS STRING * 2, acumulado AS STRING * 8, grabGav AS STRING * 1
CONST true = -1: CONST false = NOT true: busqueda = 0
ON TIMER(5) GOSUB AbreGav
TIMER ON
CALL LeeEstado: CALL Fallas(guia): CLOSE
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... "
CHAIN "F:\kevinprg\kevin01.bas"
ErrorTrap:
    SELECT CASE ERR
        CASE 9
            Box 7, 20, 9, 60: Center 8, "No se pueden Adicionar Articulos Hasta Reiniciar el Sistema"
            SOUND 800, 2: Center 25, "No se pueden crear Mas Articulos Hasta Reiniciar el Sistema": Espera: Fun(11) = "si"
            RESUME NEXT
        CASE 24, 25
            SOUND 600, 2
            Center 24, "La impresora no responde ... (revise si esta encendida,on-line o falta papel)"
            Center 25, "Presione [Esc] para anular o cualquier otra tecla para continuar"
            DO WHILE INKEY$ = "": LOOP
            LOCATE 24, 1: PRINT SPACE$(80): LOCATE 25, 1: PRINT SPACE$(80)
            IF INKEY$ = CHR$(27) THEN RESUME NEXT ELSE RESUME
        CASE 53 'no se encontro archivo ...
            RESUME NEXT
        CASE 70
            Center 25, " Esperando Acceso a Registro ...": SLEEP 1: Center 25, SPACE$(35): RESUME
        CASE ELSE
         LOCATE 1, 1: PRINT ERR
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
SUB BorraBox (Row1%, Col1%, Row2%, Col2%) STATIC
BoxWidth = Col2% - Col1% + 1: FOR A = Row1% TO Row2%: LOCATE A, Col1%: PRINT SPACE$(BoxWidth); : NEXT A
END SUB

SUB Box (Row1%, Col1%, Row2%, Col2%) STATIC
BoxWidth = Col2% - Col1% + 1: LOCATE Row1%, Col1%: PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿"; : FOR A = Row1% + 1 TO Row2% - 1: LOCATE A, Col1%: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT A: LOCATE Row2%, Col1%: PRINT "À"; STRING$(BoxWidth - 2,  _
"Ä"); "Ù";
END SUB

SUB Center (row, text$)
IF row = 25 THEN COLOR colors(5, colorpref), colors(4, colorpref): LOCATE 25, 1: PRINT SPACE$(80);
LOCATE row, 41 - LEN(text$) / 2: PRINT text$;
END SUB

FUNCTION CheqDate (caden$, op%)
CheqDate = true: IF caden$ = "          " THEN EXIT FUNCTION
dd% = VAL(MID$(caden$, 1, 2))
mm% = VAL(MID$(caden$, 4, 2))
aa% = VAL(MID$(caden$, 7, 4))
dd2% = VAL(MID$(DATE$, 4, 2))
mm2% = VAL(MID$(DATE$, 1, 2))
aa2% = VAL(MID$(DATE$, 7, 4))
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

FUNCTION CheqNum$ (cadn$, ente%, larg%)
END FUNCTION

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

FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%, est%)
COLOR colors(4, colorpref), colors(7, colorpref): Box fila%, columna%, fila% + 16, columna% + 16: selecc% = 199
IF est% = 0 THEN Center 25, "[F1]Ayuda  [F5]Editar o Adicionar Linea  [F8]Borrar Linea  [Enter]Tomar Opcion" ELSE Center 25, "[F1]Ayuda  [Enter]Tomar Opcion"
COLOR colors(4, colorpref), colors(7, colorpref)
SELECT CASE estesi%
     CASE 1
          LOCATE fila%, columna + 2: PRINT "Ubicacion": CLOSE #2
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
            CASE CHR$(13): terminar = true: devolver = false
            CASE CHR$(27): terminar = true: resal% = 0: up% = 0: devolver = true
            CASE CHR$(0) + CHR$(63): terminar = true: Fun(5) = "si": sitio = resal%
            CASE CHR$(0) + CHR$(66): terminar = true: Fun(8) = "si": sitio = resal%
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
Elije% = up% + resal% - 1
END FUNCTION

FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
LOCATE fila%, columna%: PRINT CHR$(178) + caden$ + SPACE$(largo% - LEN(caden$)): LOCATE fila%, columna% + largo% + 1: PRINT CHR$(178)
REDIM eleccion(0 TO largo% + 1) AS STRING * 1: llevasitio% = 2
FOR k = 1 TO largo%: eleccion(k) = MID$(caden$, k, 1): NEXT k: LOCATE fila%, (columna% + llevasitio%): COLOR colors(4, colorpref) + 16, colors(7, colorpref): IF eleccion(2) = " " THEN PRINT "_";  ELSE PRINT eleccion(2)
DO
      llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
          SELECT CASE llave$
               CASE CHR$(0) + "H": devolver = true: EXIT DO
               CASE CHR$(0) + "P": devolver = false: EXIT DO
               CASE CHR$(0) + CHR$(73): repag = true: EXIT DO
               CASE CHR$(0) + CHR$(81): repag = false: EXIT DO
               CASE CHR$(0) + "K"
                  llevasitio% = llevasitio% - 1: IF llevasitio% = 0 THEN GOSUB Sumar2: devolver = true: EXIT DO
               CASE CHR$(0) + "M"
                  llevasitio% = llevasitio% + 1: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE CHR$(8)
                  llevasitio% = llevasitio% - 1: IF llevasitio% = 0 THEN GOSUB Sumar2: EXIT DO
                  FOR k = llevasitio% TO largo% - 1: eleccion(k) = eleccion(k + 1)
                  NEXT k: eleccion(largo%) = " "
               CASE CHR$(0) + CHR$(83)
                  FOR k = llevasitio% TO largo%: eleccion(k) = eleccion(k + 1): NEXT k
                  eleccion(largo%) = " "
               CASE CHR$(13): EXIT DO
               CASE CHR$(27): atras = true: EXIT DO
               CASE CHR$(0) + CHR$(59): Fun(1) = "si": EXIT DO
               CASE CHR$(0) + CHR$(60): Fun(2) = "si": EXIT DO
               CASE CHR$(0) + CHR$(61): Fun(3) = "si": EXIT DO
               CASE CHR$(0) + CHR$(62): Fun(4) = "si": EXIT DO
               CASE CHR$(0) + CHR$(63): Fun(5) = "si": EXIT DO
               CASE CHR$(0) + CHR$(64): Fun(6) = "si": EXIT DO
               CASE CHR$(0) + CHR$(65): Fun(7) = "si": EXIT DO
               CASE CHR$(0) + CHR$(66): Fun(8) = "si": EXIT DO
               CASE CHR$(0) + CHR$(67): Fun(9) = "si": EXIT DO
               CASE CHR$(0) + CHR$(68): Fun(10) = "si": EXIT DO
               CASE CHR$(0) + CHR$(133): Fun(11) = "si": EXIT DO
               CASE CHR$(0) + CHR$(134): Fun(12) = "si": BEEP
               CASE CHR$(0) + CHR$(71): llevasitio% = 1
               CASE CHR$(0) + CHR$(79): llevasitio% = largo%
               CASE " ": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "1": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "2": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "3": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "4": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "5": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "6": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "7": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "8": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "9": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "0": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "+": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE "-": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE ".": GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
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

FUNCTION Escoge% (fila%, edo%, fila2%, Prod$)
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila% - 1, 1: PRINT STRING$(80, "Ä"): LOCATE fila% + 5, 1
PRINT STRING$(80, "Ä"); : FOR k = fila% TO fila% + 4: LOCATE k, 1: PRINT SPACE$(80); : NEXT k
STATIC max2%, Prud AS STRING * 6
IF edo% = 1 THEN
CALL Center(25, "[F1] Ayuda [F9] Salir SIN Escoger [Enter] Toma Articulo [A-Z] Ir a")
ELSE
CALL Center(25, "[F1] Ayuda [F9] Salir SIN Escoger [Enter] Toma Articulo")
END IF
IF edo% = 1 OR edo% = 6 THEN
     Prod$ = "      "
     DO WHILE Prod$ = "      "
     LOCATE fila2%, 5: PRINT "Introduzca Orden de Busqueda:"; SPACE$(15)
     IF Fun(17) = "op" THEN Prod$ = UCASE$(MeteDatoLetra(fila2%, 34, 6, "      ")) ELSE Prod$ = UCASE$(MeteDatoLetra(fila2%, 34, 6, "      "))
     Prud = Prod$: Prod$ = Trim$(Prod$)
     IF Fun(9) = "si" OR Prod$ = "" THEN : Escoge% = -1: EXIT FUNCTION
     LOOP
END IF: selecc% = 0
CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
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
               Prod$ = UCASE$(MeteDatoLetra(fila2%, 34, 6, "      "))
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
DO
     kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND: kbd$ = UCASE$(kbd$)
     SELECT CASE kbd$
          CASE CHR$(0) + "H": GOSUB Arriba
          CASE CHR$(0) + "P": GOSUB Abajo
          CASE CHR$(0) + CHR$(73): GOSUB Apgup
          CASE CHR$(0) + CHR$(81): GOSUB Apgdn
          CASE CHR$(13): EXIT DO
          CASE CHR$(27): resal% = 0: EXIT DO
          CASE CHR$(0) + CHR$(67): resal% = 0: Fun(3) = "si": EXIT DO
          'CASE IS >= CHR$(65)
          '     IF kbd$ <= CHR$(90) AND edo% = 1 THEN
          '          Prod$ = kbd$: OPEN "F:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
          '          FIELD #2, 35 AS nomshort$, 2 AS num$: max2% = LOF(2) / 37: GOSUB Parche1: GOSUB Parche2
          '     END IF
          CASE ELSE:  BEEP
     END SELECT
     IF up% = 0 THEN
          FOR k = 0 TO 4: LOCATE fila% + k, 1: PRINT SPACE$(80); : NEXT k
          EXIT DO
     END IF
     FOR l = 1 TO 5
          IF resal% = l THEN LOCATE fila% - 2, 50: PRINT "En Oferta:"; : PRINT USING "#####, "; CVI(Cant4a(l)); : PRINT " a Bs."; : PRINT USING "######,"; CVD(Pvp4a(l))
          IF resal% = l THEN COLOR colors(7, colorpref), colors(4, colorpref)
          IF l <= selecc% THEN
               LOCATE fila% + l - 1, 1: PRINT Nombra(l); "³"; : LOCATE fila% + l - 1, 37: PRINT USING "#####, "; CVI(Cant1a(l)); : LOCATE fila% + l - 1, 51: PRINT "³"; : PRINT USING "#####, "; CVI(Cant2a(l));
               LOCATE fila% + l - 1, 66: PRINT "³"; : PRINT USING "#####, "; CVI(Cant3a(l)): LOCATE fila% + l - 1, 44: PRINT USING "######,"; CVD(Pvp1a(l)): LOCATE fila% + l - 1, 59: PRINT USING "######,"; CVD(Pvp2a(l))
               LOCATE fila% + l - 1, 74: PRINT USING "######,"; CVD(Pvp3a(l))
          ELSE
               LOCATE fila% + l - 1, 1: PRINT SPACE$(80)
          END IF
          IF resal% = l THEN COLOR colors(4, colorpref), colors(7, colorpref)
     NEXT l
LOOP
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
          Nombra(selecc%) = nomb2$: Cant1a(selecc%) = exi12$: Cant2a(selecc%) = exi22$: Cant3a(selecc%) = exi32$: Cant4a(selecc%) = exi42$: Pvp1a(selecc%) = pv12$: Pvp2a(selecc%) = pv22$: Pvp3a(selecc%) = pv32$: Pvp4a(selecc%) = pv42$:  Numa(selecc% _
) = num$
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
          Nombra(selecc%) = nomb2$: Cant1a(selecc%) = exi12$: Cant2a(selecc%) = exi22$: Cant3a(selecc%) = exi32$: Cant4a(selecc%) = exi42$: Pvp1a(selecc%) = pv12$: Pvp2a(selecc%) = pv22$: Pvp3a(selecc%) = pv32$: Pvp4a(selecc%) = pv42$:  Numa(selecc% _
) = num$
     ELSE
          EXIT FOR
     END IF
NEXT k
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila% - 2, 1: PRINT SPACE$(70): COLOR colors(4, colorpref), colors(7, colorpref)
wer$ = STRING$(36, "Ä") + "CAN1ÄÄÄÄÄÄPVP1ÂCAN2" + STRING$(6, "Ä") + "PVP2ÂCAN3" + STRING$(5, "Ä") + "PVP3" + "Ä"
LOCATE fila% - 1, 1: PRINT wer$;
wer$ = STRING$(80, "Ä"): LOCATE fila% + 5, 1: PRINT wer$; : COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE fila%, 1: PRINT Nombra(1); "³"; : LOCATE fila%, 37: PRINT USING "#####, "; CVI(Cant1a(1)); : LOCATE fila%, 51: PRINT "³"; : PRINT USING "#####, "; CVI(Cant2a(1)); : LOCATE fila%, 66: PRINT "³"; : PRINT USING "#####, "; CVI(Cant3a(1))
LOCATE fila%, 44: PRINT USING "######,"; CVD(Pvp1a(1)): LOCATE fila%, 59: PRINT USING "######,"; CVD(Pvp2a(1)): LOCATE fila%, 74: PRINT USING "######,"; CVD(Pvp3a(1)): COLOR colors(4, colorpref), colors(7, colorpref)
LOCATE fila% - 2, 50: PRINT "En Oferta:"; : PRINT USING "#####, "; CVI(Cant4a(1)); : PRINT " a Bs."; : PRINT USING "######,"; CVD(Pvp4a(1))
FOR k = 2 TO 5
     IF k <= selecc% THEN
          LOCATE fila% + k - 1, 1: PRINT Nombra(k); "³"; : LOCATE fila% + k - 1, 37: PRINT USING "#####, "; CVI(Cant1a(k)); : LOCATE fila% + k - 1, 51: PRINT "³"; : PRINT USING "#####, "; CVI(Cant2a(k));
          LOCATE fila% + k - 1, 66: PRINT "³"; : PRINT USING "#####, "; CVI(Cant3a(k)): LOCATE fila% + k - 1, 44: PRINT USING "######,"; CVD(Pvp1a(k)): LOCATE fila% + k - 1, 59: PRINT USING "######,"; CVD(Pvp2a(k))
          LOCATE fila% + k - 1, 74: PRINT USING "######,"; CVD(Pvp3a(k))
     ELSE
          LOCATE fila% + k - 1, 1: PRINT SPACE$(80)
     END IF
NEXT k: resal% = 1
RETURN
END FUNCTION

SUB Espera
Center 25, " Presione Cualquier Tecla Para Continuar ... "
DO: LOOP WHILE INKEY$ = ""
END SUB

SUB Fallas (sta%)
'ojo sta%=1 -> medicinas sta%=2 -> miscelaneas sta%=1 -> quimicos
DIM promedio AS STRING * 8, ordpromedio AS STRING * 2, opcion AS INTEGER, tope AS STRING * 8
DIM aur1 AS STRING * 19, aur2 AS STRING * 19, aur3 AS STRING * 35, aur4 AS STRING * 35, dias AS SINGLE
dias = 7
COLOR colors(7, colorpref), colors(4, colorpref): STATIC apu%, selec%, control%
FOR k = 1 TO 12: Fun(k) = "no": NEXT k: Nuevos% = 0: CLS : Box 1, 1, 3, 80
IF sta% = 1 THEN
     Center 2, " Pedido de Fallas a Droguerias (Medicinas) [Stock de trabajo:" + Stock + "]"
END IF
IF sta% = 2 THEN
     Center 2, " Pedido de Fallas de Miscelaneas [Stock de trabajo:" + Stock + "]"
END IF
IF sta% = 3 THEN
     Center 2, " Pedido de Fallas de Qu¡micos para Envasar [Stock de trabajo:" + Stock + "]"
END IF
Center 25, "[F1]Ayuda  [F10]Salir SIN Salvar": LOCATE 4, 44: PRINT "Desde:²    ²": LOCATE 4, 64: PRINT "Hasta:²    ²": salir = false: sit% = 1
DO WHILE NOT salir
     COLOR colors(5, colorpref), colors(4, colorpref)
     LOCATE 4, 1: PRINT " Escoja el Proveedor:²   ²             "
     control% = Elije%(5, 5, 22, 4, 23, 0, 1)
     COLOR colors(5, colorpref), colors(4, colorpref)
     FOR k = 5 TO 21: Center k, SPACE$(80): NEXT k
     Center 25, "[F9]ABORTAR [F10]Fallas por d¡as  [F11]Solo ceros [F12]Por promedio"
     Prud1$ = MeteDatoLetra(4, 50, 4, Prud1$)
     Prud1$ = Trim$(UCASE$(Prud1$)): IF Fun(9) = "si" THEN Fun(9) = "no": EXIT SUB
     IF Fun(11) = "si" THEN EXIT DO
     IF Fun(12) = "si" THEN EXIT DO
     Prud2$ = MeteDatoLetra(4, 70, 4, Prud2$)
     Prud2$ = Trim$(UCASE$(Prud2$)): IF Fun(9) = "si" THEN Fun(9) = "no": EXIT SUB
     IF Fun(11) = "si" THEN EXIT DO
     IF Fun(12) = "si" THEN EXIT DO
     Box 10, 10, 12, 70: Center 11, "¨Desea Procesar En Este Momento el Pedido [S/N]?"
     res$ = MeteDatoLetra(11, 65, 1, "N"): res$ = UCASE$(res$)
     FOR k = 5 TO 21: Center k, SPACE$(80): NEXT k
     IF res$ = "S" THEN salir = true
     IF control% < 0 THEN salir = false
     IF Prud1$ = "" THEN salir = false
     IF Prud2$ = "" THEN salir = false
LOOP
aur% = Verify(13, "¨Desea salvar este pedido por montos parciales")
IF aur% THEN
   DO
      Center 25, "[Enter]Aceptar [Esc]Abortar pedido": devolver = false
      COLOR colors(5, colorpref), colors(4, colorpref)
      Box 12, 5, 14, 75: LOCATE 13, 6: PRINT "Introduzca el monto para salvar pedido parcial:"
      auxil$ = MeteDatoLetra(13, 60, 8, " 250000 ")
      IF devolver THEN EXIT SUB
      aur% = Verify(8, "¨Est  de acuerdo con este monto")
      IF aur% THEN EXIT DO
   LOOP
   tope = MKD$(VAL(auxil$))
ELSE
   tope = MKD$(0)
END IF
opcion = 0                          'fallas normales
IF Fun(11) = "si" THEN opcion = 1   'solo ceros
IF Fun(12) = "si" THEN opcion = 2   'por promedios
IF Fun(10) = "si" THEN opcion = 3   'por d¡as de venta
IF opcion = 3 THEN
DO
   Box 9, 5, 13, 70: LOCATE 10, 12: PRINT "Introduzca desde que fecha :"
   Center 24, "Introduzca DESDE que fecha desea pedir fallas: use el formato dd/mm/aaaa   "
   DO
     Center 25, "[Enter]Aceptar [F9]Abortar"
     desde$ = MeteDatoLetra(10, 40, 10, "  /  /    ")
     IF Fun(9) = "si" THEN Fun(9) = "no": EXIT SUB
     IF CheqDate(desde$, 1) = true THEN EXIT DO ELSE Center 25, "INTRODUZCA UNA FECHA VALIDA POR FAVOR": SLEEP 2
   LOOP
   Center 24, "Introduzca HASTA que fecha desea pedir fallas: use el formato dd/mm/aaaa   "
   LOCATE 11, 12: PRINT "Introduzca hasta que fecha :"
   DO
     Center 25, "[Enter]Aceptar [F9]Abortar"
     hasta$ = MeteDatoLetra(11, 40, 10, "  /  /    ")
     IF Fun(9) = "si" THEN Fun(9) = "no": EXIT SUB
     IF CheqDate(hasta$, 1) = true THEN EXIT DO ELSE Center 25, "INTRODUZCA UNA FECHA VALIDA POR FAVOR": SLEEP 2
   LOOP
   desde1& = DateToJulian(desde$, 2): hasta1& = DateToJulian(hasta$, 2)
   IF desde1& > hasta1& THEN
      Box 9, 5, 13, 70: Center 10, "Desde " + desde$ + " hasta " + hasta$
      Center 11, "La fecha final debe ser mayor que la inicial": SLEEP 2
   ELSE
      desde$ = Tratafechas(desde$, "dd/mm/aaaa", "ddmmaaaa")
      EXIT DO
   END IF
LOOP
IF hasta1& - desde1& > 30 THEN
   aure% = Verify(13, "Son m s de 30 d¡as de fallas ¨Desea continuar")
   IF aure% = false THEN EXIT SUB
END IF
END IF
ON ERROR GOTO ErrorTrap
CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
OPEN "F:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
FIELD #2, 35 AS nomshort$, 2 AS num$: selec% = 0: max% = LOF(2) / 37
KILL "F:\kevinprg\kevindat\auxilio.fll"
OPEN "F:\kevinprg\kevindat\auxilio.fll" FOR RANDOM AS #3 LEN = 57
FIELD #3, 1 AS vivo$, 2 AS canti$, 2 AS sitio$, 8 AS precio$, 6 AS codigo3$, 38 AS orden$
OPEN "F:\kevinprg\kevinvnt\estadist.dat" FOR RANDOM SHARED AS #6 LEN = 20
FIELD #6, 4 AS unid$, 8 AS primera$, 8 AS ultima$

aur% = Verify(13, "¨ DESEA PEDIR LOS PRODUCTOS ESPECIALMENTE MARCADOS")
IF aur% THEN
  Termometro central, max%, 0, "PRODUCTOS MARCADOS, espere ...", 10, 15
  FOR k = 1 TO max%
     GET #1, k: Termometro k, max%, 1, "PRODUCTOS MARCADOS, espere ...", 10, 15
     ord$ = "þþþ": RSET num$ = MKI$(k)
     IF upc$ = "*       " THEN
        IF opcion = 2 THEN
           GOSUB CalculaPromedio
           ordpromedio = MKI$(9900)
           GOSUB GrabaEnArch3
        ELSE
           selec% = selec% + 1: GOSUB CalculaPromedio: ord$ = "!!!"
           RSET orden$ = ord$ + MID$(nomb2$, 1, 27) + promedio
           RSET sitio$ = num$: PUT #3, selec%
        END IF
     END IF
   NEXT k
END IF

SELECT CASE opcion
   CASE 0, 2
          encontrado = false: primero = 1: ultimo = max%
          WHILE NOT encontrado AND primero <= ultimo
               central = (primero + ultimo) / 2: GET #2, central
               IF Prud1$ = MID$(nomshort$, 1, LEN(Prud1$)) THEN
                    encontrado = true
               ELSE
                    IF MID$(nomshort$, 1, LEN(Prud1$)) > Prud1$ THEN
                         ultimo = central - 1
                    ELSE
                         primero = central + 1
                    END IF
               END IF
          WEND: salir = false
          DO WHILE salir = false
               central = central - 1: IF central < 1 THEN salir = true: central = 1
               GET #2, central: IF Prud1$ <> MID$(nomshort$, 1, LEN(Prud1$)) THEN salir = true: central = central + 1
          LOOP
   CASE 1
      central = 1
   CASE 3
      central = max%
END SELECT
Termometro central, max%, 0, "Procesando Fallas , Espere ...", 10, 15
Center 25, "Espere, por favor..."
FOR k = central TO max%
IF opcion <> 3 THEN
   GET #2, k: Termometro k, max%, 1, "Procesando Fallas , Espere ...", 10, 15
   IF MID$(nomshort$, 35, 1) <> "þ" THEN
    SELECT CASE opcion
        CASE 0
        'fallas normales
               IF Prud1$ <= MID$(nomshort$, 1, LEN(Prud1$)) THEN
                  IF Prud2$ >= MID$(nomshort$, 1, LEN(Prud2$)) THEN
                       GET #1, CVI(num$): ord$ = "þþþ"
                       ElegOtr% = CVI(num$)
                       IF aur% = true THEN GOSUB Repetidos
                       IF devolver = 0 THEN
                       SELECT CASE sta%
                       CASE 1
                            IF medi2$ = "*" AND re2$ = " " THEN  'AND CVI(ubic2$) <> 2
                                 IF Stock = "1" THEN
                                      IF CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$) + CVI(ped2$) < CVI(mini12$) THEN
                                      IF CVI(exi12$) = 0 OR CVI(exi22$) = 0 OR CVI(exi32$) = 0 THEN
                                           MID$(ord$, 1, 1) = "*" 'IF iv$ = "*" THEN MID$(ord$, 1, 1) = "*"
                                           IF CVI(ubic2$) = 1 THEN MID$(ord$, 2, 1) = "*"
                                           IF of12$ = "@" OR of22$ = "#" OR of32$ = "$" THEN MID$(ord$, 3, 1) = "*"
                                           selec% = selec% + 1: GOSUB CalculaPromedio
                                           RSET orden$ = ord$ + MID$(nomb2$, 1, 27) + promedio: RSET sitio$ = num$: PUT #3, selec%
                                      END IF
                                      END IF
                                 ELSE
                                      IF CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$) + CVI(ped2$) < CVI(mini22$) THEN
                                      IF CVI(exi12$) = 0 OR CVI(exi22$) = 0 OR CVI(exi32$) = 0 THEN
                                           MID$(ord$, 1, 1) = "*" 'IF iv$ = "*" THEN MID$(ord$, 1, 1) = "*"
                                           IF CVI(ubic2$) = 1 THEN MID$(ord$, 2, 1) = "*"
                                           IF of12$ = "@" OR of22$ = "#" OR of32$ = "$" THEN MID$(ord$, 3, 1) = "*"
                                           selec% = selec% + 1: GOSUB CalculaPromedio
                                           RSET orden$ = ord$ + MID$(nomb2$, 1, 27) + promedio: RSET sitio$ = num$: PUT #3, selec%
                                      END IF
                                      END IF
                                 END IF
                            END IF
                       CASE 2
                            IF misc2$ = "*" AND re2$ = " " THEN 'AND CVI(ubic2$) <> 2
                                 IF Stock = "1" THEN
                                      IF CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$) + CVI(ped2$) < CVI(mini12$) THEN
                                           selec% = selec% + 1: GOSUB CalculaPromedio
                                           RSET orden$ = ord$ + MID$(nomb2$, 1, 27) + promedio: RSET sitio$ = num$: PUT #3, selec%
                                      END IF
                                 ELSE
                                      IF CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$) + CVI(ped2$) < CVI(mini22$) THEN
                                           selec% = selec% + 1: GOSUB CalculaPromedio
                                           RSET orden$ = ord$ + MID$(nomb2$, 1, 27) + promedio: RSET sitio$ = num$: PUT #3, selec%
                                      END IF
                                 END IF
                            END IF
                       CASE 3
                            IF quim2$ = "*" AND re2$ = " " AND CVI(ubic2$) <> 3 THEN
                            IF Stock = "1" THEN
                                      IF CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$) + CVI(ped2$) < CVI(mini12$) THEN
                                           selec% = selec% + 1: GOSUB CalculaPromedio
                                           RSET orden$ = ord$ + MID$(nomb2$, 1, 27) + promedio: RSET sitio$ = num$: PUT #3, selec%
                                      END IF
                                 ELSE
                                      IF CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$) + CVI(ped2$) < CVI(mini22$) THEN
                                           selec% = selec% + 1: GOSUB CalculaPromedio
                                           RSET orden$ = ord$ + MID$(nomb2$, 1, 27) + promedio: RSET sitio$ = num$: PUT #3, selec%
                                      END IF
                                 END IF
                            END IF
                       END SELECT
                       END IF
                  ELSE
                       k = max%
                  END IF
               END IF
        CASE 1
        'solo ceros
             GET #1, CVI(num$): ord$ = "þþþ"
                  IF medi2$ = "*" THEN
                   IF re2$ <> "*" AND CVI(mini12$) > 0 THEN
                     'IF CVI(ubic2$) <> 2 THEN
                      IF CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$) + CVI(ped2$) <= 0 THEN
                         ElegOtr% = CVI(num$)
                         IF aur% = true THEN GOSUB Repetidos
                         IF devolver = 0 THEN
                            MID$(ord$, 1, 1) = " "
                            'IF iv$ = "*" THEN MID$(ord$, 1, 1) = "*"
                            IF CVI(ubic2$) = 1 THEN MID$(ord$, 2, 1) = "*"
                            IF of12$ = "@" OR of22$ = "#" OR of32$ = "$" THEN MID$(ord$, 3, 1) = "*"
                            selec% = selec% + 1: GOSUB CalculaPromedio
                            RSET orden$ = ord$ + MID$(nomb2$, 1, 27) + promedio: RSET sitio$ = num$: PUT #3, selec%
                         END IF
                      END IF
                     'END IF
                    END IF
                  END IF
        CASE 2
        'por promedio
               IF Prud1$ <= MID$(nomshort$, 1, LEN(Prud1$)) THEN
                  IF Prud2$ >= MID$(nomshort$, 1, LEN(Prud2$)) THEN
                       GET #1, CVI(num$): ord$ = "þþþ"
                       IF medi2$ = "*" AND re2$ = " " THEN  'AND CVI(ubic2$) <> 2
                            IF Stock = "1" THEN
                              IF CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$) + CVI(ped2$) < CVI(mini12$) THEN
                                 IF CVI(exi12$) = 0 OR CVI(exi22$) = 0 OR CVI(exi32$) = 0 THEN
                                   ElegOtr% = CVI(num$)
                                   IF aur% = true THEN GOSUB Repetidos
                                   IF devolver = 0 THEN
                                     IF MID$(nomshort$, 1, 1) = "/" OR INSTR(1, obser2$, "[") > 0 THEN
                                        IF INSTR(1, obser2$, "[B") > 0 OR INSTR(1, obser2$, "[V") THEN
                                           'psicotr¢picos
                                            GOSUB CalculaPromedio
                                            ordpromedio = MKI$(6000)
                                            GOSUB GrabaEnArch3
                                        ELSE
                                           'dispensadores
                                            GOSUB CalculaPromedio
                                            ordpromedio = MKI$(8000)
                                            GOSUB GrabaEnArch3
                                        END IF
                                     ELSE
                                        GOSUB CalculaPromedio: GOSUB GrabaEnArch3
                                     END IF
                                   END IF
                                 END IF
                              END IF
                            ELSE
                              IF CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$) + CVI(ped2$) < CVI(mini22$) THEN
                                IF CVI(exi12$) = 0 OR CVI(exi22$) = 0 OR CVI(exi32$) = 0 THEN
                                  ElegOtr% = CVI(num$)
                                  IF aur% = true THEN GOSUB Repetidos
                                  IF devolver = 0 THEN
                                     IF MID$(nomshort$, 1, 1) = "/" OR INSTR(1, obser2$, "[") > 0 THEN
                                        IF INSTR(1, obser2$, "[B") > 0 OR INSTR(1, obser2$, "[V") THEN
                                           'psicotr¢picos
                                            GOSUB CalculaPromedio
                                            ordpromedio = MKI$(6000)
                                            GOSUB GrabaEnArch3
                                        ELSE
                                           'dispensadores
                                            GOSUB CalculaPromedio
                                            ordpromedio = MKI$(8000)
                                            GOSUB GrabaEnArch3
                                        END IF
                                     ELSE
                                        GOSUB CalculaPromedio: GOSUB GrabaEnArch3
                                     END IF
                                  END IF
                                END IF
                              END IF
                            END IF
                       END IF
                  ELSE
                       k = max%
                  END IF
               END IF
    END SELECT
   END IF
ELSE
   ' por d¡as de ventas
   DO
      GOSUB SacaFallasPorDia
      CALL Suma1Dia(desde$, 1)
      Box 10, 15, 16, 65: Center 13, "Desde: " + Tratafechas(desde$, "ddmmaaaa", "dd/mm/aaaa") + " hasta: " + hasta$
      desde2& = DateToJulian(desde$, 1)
      hasta2& = DateToJulian(hasta$, 2)
      Center 15, STR$(desde2&) + STR$(hasta2&)
      IF desde2& > hasta2& THEN
         EXIT DO
      END IF
   LOOP
   EXIT FOR
END IF
NEXT k
IF selec% = 0 THEN Center 7, "² No se Encontr¢ Fallas... ²": Espera: CLOSE : EXIT SUB
GOSUB Ordenar: van = MKD$(0): acumulado = MKD$(0)
CLOSE #3: OPEN "F:\kevinprg\kevindat\auxilio.fll" FOR RANDOM AS #3 LEN = 57
FIELD #3, 1 AS vivo$, 2 AS canti$, 2 AS sitio$, 8 AS precio$, 6 AS codigo3$, 38 AS orden$
LOCATE 6, 1: PRINT "ÄÄDescripcion" + STRING$(22, "Ä") + "ÂOf.ÂM¡nimÂM ximÂExisten.ÂPedidoÂPVP 3ÄÄÂPro."
LOCATE 16, 1: PRINT "ÄÄDescripcion" + STRING$(22, "Ä") + "ÁOf.ÁM¡nimÁM ximÁExisten.ÁPedidoÁPVP 3ÄÄÁPro."
Center 23, "1.-[ @=" + Prov1 + " ]  2.-[ #=" + Prov2 + " ]  3.-[ $=" + Prov3 + " ]"
apu% = 1: seguir = true: GET #3, apu%: ord$ = MID$(orden$, 1, 3): LOCATE 4, 1: PRINT " Proveedor escogido :"

'**************************************************************************
DO WHILE seguir
'Impresionable:
Center 24, "[1]CortarPed [F2]Lab. [F3]ReFallas [F4]Auto   [F5]Compos [F6]Nuevo [F7]Consult"
Center 25, "[F8]Calcular [F9]Salvar [F10]SinSalvar [F11]Salvar e imprimir [F12]Marcar art."
FOR k = 1 TO 12: Fun(k) = "no": NEXT k
COLOR colors(5, colorpref), colors(4, colorpref): l = 6
'LOCATE 1, 1: PRINT apu%, selec%
FOR k = apu% - 4 TO apu% + 4
     l = l + 1
     LOCATE l, 1: PRINT SPACE$(80)
     IF k = apu% THEN COLOR colors(4, colorpref), colors(7, colorpref)
     IF k > 0 AND k <= selec% THEN
          GET #3, k: GET #1, CVI(sitio$)
          LOCATE l, 1: PRINT nomb2$; "³"; of12$; of22$; of32$; "³";
          cantaux& = CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$) + CVI(ped2$)
          LOCATE l, 53: PRINT USING "#######,³"; cantaux&; : PRINT USING "#####,"; CVI(canti$)
          IF Stock = "1" THEN
               LOCATE l, 40: PRINT USING "³#####³"; CVI(mini12$); : PRINT USING "#####³"; CVI(maxi12$); : LOCATE l, 68: PRINT USING "³#######³"; CVD(pv32$);
               IF opcion = 2 THEN
                  PRINT USING "####"; CVD(MID$(orden$, 1, 8)) 'CVI(prom2$)
               ELSE
                  PRINT USING "####"; CVD(MID$(orden$, 31, 8))'CVI(prom2$)
               END IF
             ELSE
               LOCATE l, 40: PRINT USING "³#####³"; CVI(mini22$); : PRINT USING "#####³"; CVI(maxi22$); : LOCATE l, 68: PRINT USING "³#######³"; CVD(pv32$);
               IF opcion = 2 THEN
                  PRINT USING "####"; CVD(MID$(orden$, 1, 8)) 'CVI(prom2$)
               ELSE
                  PRINT USING "####"; CVD(MID$(orden$, 31, 8))'CVI(prom2$)
               END IF
          END IF
     ELSE
          PRINT SPACE$(35); "³   ³     ³     ³        ³      ³       ³    ";
     END IF
     IF k = apu% THEN COLOR colors(7, colorpref), colors(4, colorpref)
NEXT k
GET #3, apu%: GET #1, CVI(sitio$): LOCK #1, CVI(sitio$)
RSET precio$ = pv32$: GOSUB Mostrar: atras = false: devolver = 777: repag = 777
'------
pedir$ = EntraDatoNumero$(11, 61, 6, STR$(CVI(canti$)), 1)
'------
van = MKD$(CVD(van) + (.7 * CVD(pv32$) * (VAL(pedir$) - CVI(canti$)))): LOCATE 5, 1: PRINT "Este pedido,aprox. (PVP3 -30%):"; : PRINT USING "###########,.##"; CVD(van); : PRINT " acumulados :"; : PRINT USING "###########,.##"; (CVD(acumulado) + CVD( _
van))
RSET ped2$ = MKI$(CVI(ped2$) - CVI(canti$))
RSET ped2$ = MKI$(CVI(ped2$) + VAL(pedir$))
RSET codigo3$ = codigo2$: RSET canti$ = MKI$(VAL(pedir$))
IF atras = true THEN
          IF Stock = "1" THEN
               nume$ = EntraDatoNumero$(11, 40, 5, STR$(CVI(mini12$)), 1)
               RSET mini12$ = MKI$(VAL(nume$))
          ELSE
               nume$ = EntraDatoNumero$(11, 40, 5, STR$(CVI(mini22$)), 1)
               RSET mini22$ = MKI$(VAL(nume$))
          END IF
END IF
IF Fun(12) = "si" THEN Fun(12) = "no": RSET iv42$ = "*"
PUT #1, CVI(sitio$): PUT #3, apu%: Fun(12) = "no": UNLOCK #1, CVI(sitio$)
'salvar pedidos parciales
IF CVD(tope) > 0 AND Fun(10) = "no" THEN
   IF CVD(van) > CVD(tope) THEN
      GOSUB SalvaPedidoParcial
   END IF
END IF
IF devolver = true THEN
     apu% = apu% - 1: IF apu% < 1 THEN apu% = 1
ELSEIF devolver = false THEN
     apu% = apu% + 1: IF apu% > selec% THEN apu% = selec%: Center 25, "Fin de la Seleccion . . . "
END IF
IF repag = true THEN
     apu% = apu% - 15: IF apu% < 1 THEN apu% = 1
ELSEIF repag = false THEN
     apu% = apu% + 15: IF apu% > selec% THEN apu% = selec%: Center 25, "Fin de la Seleccion . . . "
END IF
'IF opcion <> 2 THEN
'   IF ord$ <> MID$(orden$, 1, 3) THEN Fun(9) = "si"
'END IF
Center 25, " Procesando Nuevas Opciones ... Espere Por Favor."
ord$ = MID$(orden$, 1, 3)
IF Fun(1) = "si" THEN Fun(1) = "no": GOSUB SalvaPedidoParcial
'IF Fun(2) = "si" THEN Fun(2) = "no": GOSUB BusqF2 'por laboratorio
'IF Fun(3) = "si" THEN Fun(3) = "no": GOSUB BusqF3 'por falla de fallas
IF Fun(4) = "si" THEN Fun(4) = "no": GOSUB BusqF4 'listado autom tico de fallas
'IF Fun(5) = "si" THEN Fun(5) = "no": GOSUB BusqF5 'por composicion
'IF Fun(6) = "si" THEN Fun(6) = "no": GOSUB BusqF6 'producto nuevo
IF Fun(7) = "si" THEN Fun(7) = "no": GOSUB BusqF7 'insertar
IF Fun(9) = "si" THEN Fun(9) = "no": GOSUB BusqF9 'salvar
IF Fun(8) = "si" THEN Fun(8) = "no": GOSUB BusqF8' calcular acumulado de compras
IF Fun(10) = "si" THEN Fun(10) = "no": GOSUB BusqF10 'salir sin salvar
IF Fun(11) = "si" THEN Fun(11) = "no": GOSUB BusqF11 'salir e imprimir
LOOP
'**************************************************************************
CLOSE : EXIT SUB

BusqF7:
Center 25, "[Esc]-[F6,F7,F9]Volver a Fallas SIN Seleccionar  [Enter]Seleccionar"
CLOSE : ElegOtr% = Escoge%(18, 1, 16, "")
CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
OPEN "F:\kevinprg\kevindat\auxilio.fll" FOR RANDOM AS #3 LEN = 57
FIELD #3, 1 AS vivo$, 2 AS canti$, 2 AS sitio$, 8 AS precio$, 6 AS codigo3$, 38 AS orden$
COLOR colors(7, colorpref), colors(4, colorpref): FOR k = 16 TO 23: Center k, SPACE$(80): NEXT k
LOCATE 17, 1: PRINT "ÄÄDescripcion" + STRING$(22, "Ä") + "ÁOf.ÁM¡nimÁM ximÁExisten.ÁPedidoÁPVP 3ÄÄÁPro."
IF ElegOtr% < 1 THEN RETURN
GOSUB Repetidos
IF devolver > 0 THEN
     apu% = devolver: devolver = false
ELSE
     selec% = selec% + 1: GET #1, ElegOtr%
     FOR k = selec% TO apu% STEP -1: GET #3, k - 1: PUT #3, k: NEXT k
     IF iv$ = "*" THEN MID$(ord$, 1, 1) = "*"
     IF CVI(ubic2$) = 1 THEN MID$(ord$, 2, 1) = "*"
     IF of12$ = "@" OR of22$ = "#" OR of32$ = "$" THEN MID$(ord$, 3, 1) = "*"
     RSET sitio$ = MKI$(ElegOtr%): RSET orden$ = ord$ + nomb2$: PUT #3, apu%
END IF
RETURN

BusqF9:
COLOR colors(5, colorpref), colors(4, colorpref): Ofr$ = "r": Box 13, 20, 15, 70
LOCATE 14, 21: PRINT "¨Desea Salvar este Pedido o Regresar? [S/R]": Ofr$ = MeteDatoLetra$(14, 65, 1, Ofr$)
IF Ofr$ = "s" OR Ofr$ = "S" THEN
     FOR k = 1 TO selec%: GET #3, k
          IF CVI(canti$) > 0 THEN valid% = valid% + 1
     NEXT k
     IF valid% = 0 THEN
          Center 24, " No hay Nada Para Salvar este Pedido. Pedido Abortado . . . "
          CALL Espera: CLOSE : EXIT SUB
     END IF
     COLOR colors(7, colorpref), colors(4, colorpref)
     eleccion$ = "": ClaveLarga$ = "": CLOSE #1
     OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #4 LEN = 100
     FIELD #4, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$: nursaler = LOF(4) / 100
     Box 5, 5, 9, 75: Center 7, "Pulse Ahora las Tres Letras de Su Codigo de Vendedor (ClaveCorta)"
     DO WHILE ClaveLarga$ = ""
          eleccion$ = MeteDatoLetra$(8, 40, 3, "   ")
          FOR k = 1 TO nursaler: GET #4, k
          IF eleccion$ = clco$ AND acti$ = " " THEN ClaveLarga$ = clla$: Cedula$ = cedu$: EXIT FOR
          NEXT k
          IF ClaveLarga$ = "" THEN Center 23, "Introduzca un Codigo de Vendedor Valido": Center 24, "No Hay Ningun Vendedor Con Esa Clave o Esta Fuera El Vendedor": Espera
     LOOP
     GOSUB OrdenarAlfabeticamente
     OPEN "F:\kevinprg\kevindat\master.pdd" FOR RANDOM AS #7 LEN = 67
     FIELD #7, 1 AS viva$, 4 AS numped$, 2 AS numprov$, 5 AS hora$, 10 AS fecha$, 4 AS cedven$, 40 AS obse$, 1 AS tipo$
     Center 6, "Introduzca las Observaciones a Este Pedido": Center 7, "                                                                 "
     eleccion$ = MeteDatoLetra$(8, 20, 40, SPACE$(40))
     Center 25, "Salvando Pedido , Por Favor Espere SIN Pulsar Ninguna Tecla ...": valid% = 0: CLOSE #1
     GET #7, 1: num$ = numped$: IF CVL(numped$) = 99999999 THEN RSET numped$ = MKD$(0)
     RSET numped$ = MKL$((CVL(numped$) + 1)): PUT #7, 1: final% = (LOF(7) / 67) + 1: RSET viva$ = " "
     RSET numped$ = num$: RSET numprov$ = MKI$(VAL(Code(control%))): RSET hora$ = MID$(TIME$, 1, 5): RSET fecha$ = DATE$: RSET cedven$ = Cedula$: RSET obse$ = eleccion$
     IF sta% = 1 THEN RSET tipo$ = "1"
     IF sta% = 2 THEN RSET tipo$ = "2"
     IF sta% = 3 THEN RSET tipo$ = "3"
     PUT #7, final%: CLOSE
     OPEN "F:\kevinprg\kevindat\auxilio.fll" FOR RANDOM AS #3 LEN = 57
     FIELD #3, 1 AS vivo$, 2 AS canti$, 2 AS sitio$, 8 AS precio$, 6 AS codigo3$, 38 AS orden$
     num2$ = STR$(CVL(num$)): num2$ = Trim$(MID$(num2$, 2, 8)): num2$ = "F:\kevinprg\kevindat\" + STRING$(8 - LEN(num2$), "0") + num2$ + ".pdd"
     OPEN num2$ FOR RANDOM AS #7 LEN = 19: FIELD #7, 6 AS codi7$, 2 AS cant7$, 8 AS pvp37$, 1 AS paf7$, 2 AS sit7$: lleva7% = 0
     FOR k = 1 TO selec%: GET #3, k     ': LOCATE 1, 1: PRINT codigo3$: Espera
          IF CVI(canti$) > 0 THEN
               RSET sit7$ = sitio$: RSET paf7$ = " ": RSET cant7$ = canti$: RSET pvp37$ = precio$: RSET codi7$ = codigo3$: lleva7% = lleva7% + 1: PUT #7, lleva7%
          END IF
     NEXT k: EXIT SUB
END IF
COLOR colors(7, colorpref), colors(4, colorpref)
RETURN

BusqF10:
Ofr$ = "N": Box 13, 20, 15, 70: LOCATE 14, 22: PRINT "¨Desea ABORTAR este Pedido? [S/N]"
DO
    Ofr$ = MeteDatoLetra$(14, 65, 1, Ofr$): Ofr$ = UCASE$(Ofr$)
    IF Ofr$ = "N" THEN EXIT DO
    IF Ofr$ = "S" THEN
'OPEN "F:\kevinprg\kevindat\auxilio.fll" FOR RANDOM AS #3 LEN = 57
'FIELD #3, 1 AS vivo$, 2 AS canti$, 2 AS sitio$, 8 AS precio$, 6 AS codigo3$, 38 AS orden$
      max3% = LOF(3) / 57: Termometro 0, max3%, 0, "Borrando cantidades,Espere ...", 10, 15
      FOR klm% = 1 TO max3%
        GET #3, klm%: Termometro klm%, max3%, 1, "Borrando cantidades,Espere ...", 10, 15
        IF CVI(canti$) > 0 THEN
          GET #1, CVI(sitio$)
          RSET ped2$ = MKI$(CVI(ped2$) - CVI(canti$)): PUT #1, CVI(sitio$)
        END IF
      NEXT klm%
      EXIT SUB
    END IF
LOOP
RETURN

BusqF11:
COLOR colors(5, colorpref), colors(4, colorpref): Ofr$ = "r": Box 13, 10, 15, 70
LOCATE 14, 11: PRINT "¨Desea Imprimir y Salvar este Pedido o Regresar?[S/R]": Ofr$ = MeteDatoLetra$(14, 65, 1, Ofr$)
IF Ofr$ = "s" OR Ofr$ = "S" THEN
     FOR k = 1 TO selec%: GET #3, k
          IF CVI(canti$) > 0 THEN valid% = valid% + 1
     NEXT k
     IF valid% = 0 THEN
          Center 24, " No hay Nada Para Salvar este Pedido. Pedido Abortado . . . "
          CALL Espera: CLOSE : EXIT SUB
     END IF
     COLOR colors(7, colorpref), colors(4, colorpref)
     eleccion$ = "": ClaveLarga$ = "": CLOSE #1: OPEN "f:\kevinprg\kevindat\impfall.ver" FOR OUTPUT AS #11
     auxiliar$ = STRING$(80, "-"): PRINT #11, auxiliar$: l% = 0
     auxiliar$ = "Solicitud de Pedido a Proveedores. Fecha:" + MID$(DATE$, 4, 2) + "/" + MID$(DATE$, 1, 2) + "/" + MID$(DATE$, 7, 4) + "  Hora:" + TIME$: PRINT #11, auxiliar$
     OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #4 LEN = 100
     FIELD #4, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$: nursaler = LOF(4) / 100
     Box 5, 5, 9, 75: Center 7, "Pulse Ahora las Tres Letras de Su Codigo de Vendedor (ClaveCorta)"
     DO WHILE ClaveLarga$ = ""
          eleccion$ = MeteDatoLetra$(8, 40, 3, "   ")
          FOR k = 1 TO nursaler: GET #4, k
          IF eleccion$ = clco$ AND acti$ = " " THEN ClaveLarga$ = clla$: Cedula$ = cedu$: auxiliar$ = "Solicitado por:" + Trim$(nomb$) + Trim$(apel$): PRINT #11, auxiliar$: EXIT FOR
          NEXT k
          IF ClaveLarga$ = "" THEN Center 23, "Introduzca un Codigo de Vendedor Valido": Center 24, "No Hay Ningun Vendedor Con Esa Clave o Esta Fuera El Vendedor": Espera
     LOOP
     GOSUB Ordenar
     OPEN "F:\kevinprg\kevindat\master.pdd" FOR RANDOM AS #7 LEN = 67
     FIELD #7, 1 AS viva$, 4 AS numped$, 2 AS numprov$, 5 AS hora$, 10 AS fecha$, 4 AS cedven$, 40 AS obse$, 1 AS tipo$
     Center 6, "Introduzca las Observaciones a Este Pedido": Center 7, "                                                                 "
     eleccion$ = MeteDatoLetra$(8, 20, 40, SPACE$(40)): auxiliar$ = "Observaciones:" + eleccion$: PRINT #11, auxiliar$
     Center 25, "Salvando Pedido , Por Favor Espere SIN Pulsar Ninguna Tecla ...": valid% = 0: CLOSE #1
     GET #7, 1: num$ = numped$: IF CVL(numped$) = 99999999 THEN RSET numped$ = MKD$(0)
     RSET numped$ = MKL$((CVL(numped$) + 1)): PUT #7, 1: final% = (LOF(7) / 66) + 1: RSET viva$ = " "
     RSET numped$ = num$: RSET numprov$ = MKI$(VAL(Code(control%)))
     auxiliar$ = "Proveedor:" + Contenido(control%): PRINT #11, auxiliar$: auxiliar$ = STRING$(80, "-"): PRINT #11, auxiliar$
     auxiliar$ = "Nombre                                     Cantidad ": PRINT #11, auxiliar$: auxiliar$ = STRING$(80, "-"): PRINT #11, auxiliar$
     RSET hora$ = MID$(TIME$, 1, 5): RSET fecha$ = DATE$: RSET cedven$ = Cedula$: RSET obse$ = eleccion$
     IF sta% = 1 THEN RSET tipo$ = "1"
     IF sta% = 2 THEN RSET tipo$ = "2"
     IF sta% = 3 THEN RSET tipo$ = "3"
     PUT #7, final%: CLOSE
     OPEN "F:\kevinprg\kevindat\auxilio.fll" FOR RANDOM AS #3 LEN = 57: OPEN "f:\kevinprg\kevindat\impfall.ver" FOR OUTPUT AS #11
     FIELD #3, 1 AS vivo$, 2 AS canti$, 2 AS sitio$, 8 AS precio$, 6 AS codigo3$, 38 AS orden$
     num2$ = STR$(CVL(num$)): num2$ = Trim$(MID$(num2$, 2, 8)): num2$ = "F:\kevinprg\kevindat\" + STRING$(8 - LEN(num2$), "0") + num2$ + ".pdd"
     OPEN num2$ FOR RANDOM AS #7 LEN = 19: FIELD #7, 6 AS codi7$, 2 AS cant7$, 8 AS pvp37$, 1 AS paf7$, 2 AS sit7$: lleva7% = 0
     FOR k = 1 TO selec%: GET #3, k     ': LOCATE 1, 1: PRINT codigo3$: Espera
          IF CVI(canti$) > 0 THEN
               RSET sit7$ = sitio$: RSET paf7$ = " ": RSET cant7$ = canti$: RSET pvp37$ = precio$: RSET codi7$ = codigo3$: lleva7% = lleva7% + 1: PUT #7, lleva7%
               auxiliar$ = MID$(orden$, 4, 35) + " " + STR$(CVI(cant7$)): PRINT #11, auxiliar$: l% = l% + 1: IF l% > 50 THEN l% = 0: PRINT #11, STRING$(80, "-"): PRINT #11, CHR$(12): PRINT #11, "Nombre                                     Cantidad " _
: PRINT #11, STRING$(80, "-")
          END IF
     NEXT k: PRINT #11, STRING$(80, "-"): PRINT #11, CHR$(12): CLOSE
     ON ERROR GOTO ErrorTrap
     OPEN "lpt1" FOR RANDOM AS #10: WIDTH #10, 255
     OPEN "f:\kevinprg\kevindat\impfall.ver" FOR INPUT AS #11
     PRINT #10, CHR$(10);
     DO WHILE NOT EOF(11)
          INPUT #11, auxiliar$: PRINT #10, auxiliar$: PRINT #10, CHR$(10);
     LOOP
     CLOSE : KILL "f:\kevinprg\kevindat\impfall.ver": EXIT SUB
END IF
COLOR colors(7, colorpref), colors(4, colorpref)
RETURN

Repetidos:
devolver = 0
FOR m = 1 TO selec%: GET #3, m
   IF CVI(sitio$) = ElegOtr% THEN devolver = m: EXIT FOR
NEXT m
FOR m = 1 TO 12: Fun(m) = "no": NEXT m
RETURN

Mostrar:
COLOR colors(4, colorpref), colors(7, colorpref)
LOCATE 17, 1: PRINT "Codigo Nombre                             Ofertas Observaciones" + SPACE$(17)
LOCATE 18, 1: PRINT codigo2$; " "; : PRINT nomb2$;
PRINT " "; of12$; " "; of22$; " "; of32$; obser2$; SPACE$(12)
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
LOCATE 22, 23: PRINT "ÄÁ"; : LOCATE 22, 41: PRINT "ÄÁ"; : LOCATE 22, 60: PRINT "ÄÁ";
IF iv$ = "*" THEN PRINT " Paga I.C.S.V.M." ELSE PRINT " Producto Exento"
RETURN

Ordenar:
SELECT CASE opcion
   CASE 0, 1, 3
      CLOSE #3: OPEN "F:\kevinprg\kevindat\auxilio.fll" FOR RANDOM AS #3 LEN = 57
      FIELD #3, 19 AS semi$, 38 AS orden$: N = LOF(3) / 57: Salto = N: Box 10, 5, 16, 75: Center 13, "Ordenando , Espere ..."
      WHILE Salto > 1
           Salto = Salto / 2
           DO
                Sw = 1
                FOR J = 1 TO N - Salto
                     I = J + Salto
                     GET #3, J: au1 = semi$: au3 = orden$
                     GET #3, I: au2 = semi$: au4 = orden$
                     IF MID$(au3, 1, 30) > MID$(au4, 1, 30) THEN
                          RSET semi$ = au2: RSET orden$ = au4: PUT #3, J
                          RSET semi$ = au1: RSET orden$ = au3: PUT #3, I: Sw = 0
                     END IF
                NEXT J
           LOOP UNTIL Sw = 1
      WEND
   CASE 2
      CLOSE #3: OPEN "F:\kevinprg\kevindat\auxilio.fll" FOR RANDOM AS #3 LEN = 57
      FIELD #3, 19 AS semi$, 38 AS orden$: N = LOF(3) / 57: Salto = N: Box 10, 5, 16, 75: Center 13, "Ordenando , Espere ..."
      WHILE Salto > 1
           Salto = Salto / 2
           DO
                Sw = 1
                FOR J = 1 TO N - Salto
                     I = J + Salto
                     GET #3, J: au1 = semi$: au3 = orden$
                     GET #3, I: au2 = semi$: au4 = orden$
                     IF (CVD(MID$(au3, 1, 8)) + CVI(MID$(au3, 9, 2))) < (CVD(MID$(au4, 1, 8)) + CVI(MID$(au4, 9, 2))) THEN
                          RSET semi$ = au2: RSET orden$ = au4: PUT #3, J
                          RSET semi$ = au1: RSET orden$ = au3: PUT #3, I
                          Sw = 0
                     END IF
                NEXT J
           LOOP UNTIL Sw = 1
      WEND
END SELECT
RETURN

BusqF8:
Box 9, 5, 13, 70: LOCATE 10, 12: PRINT "Introduzca desde que fecha :"
Center 24, "Introduzca DESDE que fecha desea el calculo: use el formato dd/mm/aaaa     "
DO
     Center 25, "[Enter]Aceptar [F9]Abortar"
     desde$ = MeteDatoLetra(10, 40, 10, "  /  /    ")
     IF Fun(9) = "si" THEN Fun(9) = "no": RETURN
     IF CheqDate(desde$, 1) = true THEN EXIT DO ELSE Center 25, "INTRODUZCA UNA FECHA VALIDA POR FAVOR": SLEEP 2
LOOP
Center 24, "Introduzca HASTA que fecha desea el calculo: use el formato dd/mm/aaaa     "
LOCATE 11, 12: PRINT "Introduzca hasta que fecha :"
DO
     Center 25, "[Enter]Aceptar [F9]Abortar"
     hasta$ = MeteDatoLetra(11, 40, 10, "  /  /    ")
     IF Fun(9) = "si" THEN Fun(9) = "no": RETURN
     IF CheqDate(hasta$, 1) = true THEN EXIT DO ELSE Center 25, "INTRODUZCA UNA FECHA VALIDA POR FAVOR": SLEEP 2
LOOP
desde1& = DateToJulian(desde$, 2): hasta1& = DateToJulian(hasta$, 2)
IF desde1& > hasta1& THEN Box 9, 5, 13, 70: Center 10, "Desde " + desde$ + " hasta " + hasta$: Center 11, "La fecha final debe ser mayor que la inicial": SLEEP 2
Box 9, 5, 13, 70: Center 11, "Calculando , espere por favor ... "
CLOSE #27: OPEN "F:\kevinprg\kevindat\master.fct" FOR RANDOM AS #27 LEN = 70
FIELD #27, 1 AS acti$, 4 AS numfact$, 8 AS fechaacc$, 10 AS vendedor$, 2 AS prove$, 1 AS contacredi$, 8 AS brut$, 8 AS net$, 8 AS ivafact$, 8 AS fechafact$, 1 AS actual$, 4 AS numped$, 1 AS cancelada$, 6 AS vacio$
final27% = LOF(27) / 70: acumulado = MKD$(0)
FOR k = 1 TO final27%
     GET #27, k: IF k = (final27% - 10) THEN Espera
     IF acti$ = " " THEN
          IF CVI(prove$) = VAL(Code(control%)) THEN
               IF DateToJulian(fechafact$, 4) >= desde1& THEN
                    IF DateToJulian(fechafact$, 4) <= hasta1& THEN
                         acumulado = MKD$(CVD(acumulado) + CVD(net$))
                    END IF
               END IF
          END IF
     END IF
NEXT k: CLOSE #27
LOCATE 5, 1: PRINT "Este pedido,aprox. (PVP3 -30%):"; : PRINT USING "###########,.##"; CVD(van); : PRINT " acumulados :"; : PRINT USING "###########,.##"; (CVD(acumulado) + CVD(van))
RETURN

SalvaPedidoParcial:
COLOR colors(5, colorpref), colors(4, colorpref)
Ofr$ = "r": Box 13, 13, 15, 70
LOCATE 14, 14: PRINT "¨Desea Salvar este pedido PARCIAL o Regresar?[S/R]"
Ofr$ = MeteDatoLetra$(14, 65, 1, Ofr$)
IF Ofr$ = "s" OR Ofr$ = "S" THEN
   Box 13, 20, 15, 70: Center 13, "Espere por favor..."
   FOR k = 1 TO apu%: GET #3, k
        IF CVI(canti$) > 0 THEN valid% = valid% + 1
   NEXT k
   IF valid% = 0 THEN
        Center 24, " No hay nada para salvar. Pedido parcial abortado..."
        CALL Espera: RETURN
   END IF
   COLOR colors(7, colorpref), colors(4, colorpref)
   eleccion$ = "": ClaveLarga$ = ""
   OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #4 LEN = 100
   FIELD #4, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$: nursaler = LOF(4) / 100
   Box 5, 5, 9, 75: Center 7, "Pulse Ahora las Tres Letras de Su Codigo de Vendedor (ClaveCorta)"
   DO WHILE ClaveLarga$ = ""
        eleccion$ = MeteDatoLetra$(8, 40, 3, "   ")
        FOR k = 1 TO nursaler: GET #4, k
        IF eleccion$ = clco$ AND acti$ = " " THEN
          ClaveLarga$ = clla$: Cedula$ = cedu$: EXIT FOR
        END IF
        NEXT k
        IF ClaveLarga$ = "" THEN Center 23, "Introduzca un Codigo de Vendedor Valido": Center 24, "No Hay Ningun Vendedor Con Esa Clave o Esta Fuera El Vendedor": Espera
   LOOP
   OPEN "F:\kevinprg\kevindat\master.pdd" FOR RANDOM AS #7 LEN = 67
   FIELD #7, 1 AS viva$, 4 AS numped$, 2 AS numprov$, 5 AS hora$, 10 AS fecha$, 4 AS cedven$, 40 AS obse$, 1 AS tipo$
   Center 6, "Introduzca las Observaciones a Este Pedido": Center 7, "                                                                 "
   eleccion$ = MeteDatoLetra$(8, 20, 40, SPACE$(40))
   Center 25, "Salvando Pedido , Por Favor Espere SIN Pulsar Ninguna Tecla ...": valid% = 0
   GET #7, 1: num$ = numped$: IF CVL(numped$) = 99999999 THEN RSET numped$ = MKD$(0)
   RSET numped$ = MKL$((CVL(numped$) + 1)): PUT #7, 1: final% = (LOF(7) / 66) + 1: RSET viva$ = " "
   RSET numped$ = num$: RSET numprov$ = MKI$(VAL(Code(control%))): RSET hora$ = MID$(TIME$, 1, 5): RSET fecha$ = DATE$: RSET cedven$ = Cedula$: RSET obse$ = eleccion$
   IF sta% = 1 THEN RSET tipo$ = "1"
   IF sta% = 2 THEN RSET tipo$ = "2"
   IF sta% = 3 THEN RSET tipo$ = "3"
   PUT #7, final%: CLOSE
   OPEN "F:\kevinprg\kevindat\auxilio.fll" FOR RANDOM AS #3 LEN = 57
   FIELD #3, 1 AS vivo$, 2 AS canti$, 2 AS sitio$, 8 AS precio$, 6 AS codigo3$, 38 AS orden$
   num2$ = STR$(CVL(num$)): num2$ = Trim$(MID$(num2$, 2, 8)): num2$ = "F:\kevinprg\kevindat\" + STRING$(8 - LEN(num2$), "0") + num2$ + ".pdd"
   OPEN num2$ FOR RANDOM AS #7 LEN = 19: auxe% = 0
   FIELD #7, 6 AS codi7$, 2 AS cant7$, 8 AS pvp37$, 1 AS paf7$, 2 AS sit7$: lleva7% = 0
   FOR k = 1 TO selec%
      GET #3, k    ': LOCATE 1, 1: PRINT codigo3$: Espera
      IF CVI(canti$) > 0 THEN
          RSET vivo$ = "*": PUT #3, k
          RSET sit7$ = sitio$: RSET paf7$ = " "
          RSET cant7$ = canti$: RSET pvp37$ = precio$
          RSET codi7$ = codigo3$: lleva7% = lleva7% + 1: PUT #7, lleva7%
      END IF
      IF k >= apu% AND auxe% = 0 THEN
         IF CVI(canti$) = 0 THEN auxe% = CVI(sitio$)
      END IF
   NEXT k
   'ordenar archivo num2$
      CLOSE #1: OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
      FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS  _
cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
      CLOSE #7: OPEN num2$ FOR RANDOM AS #7 LEN = 19
      FIELD #7, 19 AS all$
      N = LOF(7) / 19: Salto = N
      Box 10, 5, 16, 75: Center 13, "Ordenando , Espere ..."
      WHILE Salto > 1
           Salto = Salto / 2
           DO
                Sw = 1
                FOR J = 1 TO N - Salto
                     I = J + Salto
                     GET #7, J: aur1 = all$: GET #1, CVI(MID$(aur1, 18, 2)): aur3 = nomb2$
                     GET #7, I: aur2 = all$: GET #1, CVI(MID$(aur2, 18, 2)): aur4 = nomb2$
                     IF aur3 > aur4 THEN
                          RSET all$ = aur2: PUT #7, J
                          RSET all$ = aur1: PUT #7, I: Sw = 0
                     END IF
                NEXT J
           LOOP UNTIL Sw = 1
      WEND
   'fin ordenar archivo num2$
   CLOSE #7: CLOSE #3
   OPEN "F:\kevinprg\kevindat\auxilio.fll" FOR RANDOM AS #3 LEN = 57
   FIELD #3, 57 AS todo$
   OPEN "F:\kevinprg\kevindat\auxilia.fll" FOR RANDOM AS #7 LEN = 57
   FIELD #7, 57 AS all$: l% = 0
   FOR k = 1 TO selec%
      GET #3, k
      IF MID$(todo$, 1, 1) = "*" THEN
              
      ELSE
         l% = l% + 1: RSET all$ = todo$: PUT #7, l%
      END IF
   NEXT k
   CLOSE #7: CLOSE #3
   KILL "F:\kevinprg\kevindat\auxilio.fll"
   NAME "F:\kevinprg\kevindat\auxilia.fll" AS "F:\kevinprg\kevindat\auxilio.fll"
   OPEN "F:\kevinprg\kevindat\auxilio.fll" FOR RANDOM AS #3 LEN = 57
   FIELD #3, 1 AS vivo$, 2 AS canti$, 2 AS sitio$, 8 AS precio$, 6 AS codigo3$, 38 AS orden$
   selec% = LOF(3) / 57
   FOR k = 1 TO selec%
      GET #3, k: IF auxe% = CVI(sitio$) THEN apu% = k
   NEXT k%
   CLOSE #1: OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
   FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS  _
cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
   acumulado = MKD$(CVD(acumulado) + CVD(van)): van = MKD$(0)
END IF
COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 6, 1: PRINT "ÄÄDescripcion" + STRING$(22, "Ä") + "ÂOf.ÂM¡nimÂM ximÂExisten.ÂPedidoÂPVP 3ÄÄÂPro."
RETURN

CalculaPromedio:
GET #6, CVI(num$)
IF ultima$ <> "        " AND primera$ <> "        " THEN
  IF ultima$ = primera$ THEN
     promedio = MKD$(0): ordpromedio = MKI$(1000)
  ELSE
     IF (DateToJulian(ultima$, 1) - DateToJulian(primera$, 1)) > 90 THEN
        IF (DateToJulian(DATE$, 3) - DateToJulian(ultima$, 1)) < 90 THEN
           promedio = MKD$(dias * CVL(unid$) / (DateToJulian(ultima$, 1) - DateToJulian(primera$, 1)))
           ordpromedio = MKI$(4000)
        ELSE
           promedio = MKD$(0): ordpromedio = MKI$(2000)
        END IF
     ELSE
        promedio = MKD$(0): ordpromedio = MKI$(2000)
     END IF
  END IF
ELSE
  ' verifica que de verdad sea "nuevo" el producto en la farmacia
  IF CVI(num$) >= (LOF(1) / 200 - 50) THEN
      promedio = MKD$(0): ordpromedio = MKI$(5000)
  ELSE
      promedio = MKD$(0): ordpromedio = MKI$(0)
  END IF
END IF
RETURN

GrabaEnArch3:
selec% = selec% + 1: RSET sitio$ = num$
RSET orden$ = promedio + ordpromedio + nomb2$: PUT #3, selec%
RETURN

SacaFallasPorDia:
     CLOSE #20: CLOSE #21
     fx$ = "f:\kevinprg\kevinvnt\" + desde$ + ".vnt": devolver = false
     OPEN fx$ FOR RANDOM SHARED AS #20 LEN = 30: FIELD #20, 30 AS all$: maxi20% = LOF(20) / 30
     OPEN "F:\kevinprg\kevindat\codigo.idx" FOR RANDOM SHARED AS #21 LEN = 8
     FIELD #21, 6 AS nomshort$, 2 AS num$: max21% = LOF(21) / 8
     ini% = 1: puntero% = 2: salir = false
     IF maxi20% > 1 THEN
          DO WHILE NOT salir
               GET #20, puntero%
               IF MID$(all$, 1, 1) <> "J" THEN Box 10, 10, 20, 70: Center 15, "Archivo Corrupto , Llame a Kevin Software 7000": Center 17, "Tan Pronto Como Le Sea Posible": Espera: EXIT SUB
               otropuntero% = puntero% + CVI(MID$(all$, 2, 2))
               FOR l = puntero% + 1 TO otropuntero%
                    GET #20, l
                    encontrado = false: primero = 1: ultimo = max21%
                    Prod$ = MID$(all$, 2, 6)
                    WHILE NOT encontrado AND primero <= ultimo
                       central = (primero + ultimo) / 2: GET #21, central
                       IF Prod$ = nomshort$ THEN
                            encontrado = true
                       ELSE
                          IF nomshort$ > Prod$ THEN
                               ultimo = central - 1
                          ELSE
                               primero = central + 1
                          END IF
                       END IF
                    WEND
                    IF encontrado = true THEN
                       GET #21, central: GET #1, CVI(num$)
                       IF Prud1$ <= MID$(nomb2$, 1, LEN(Prud1$)) THEN
                          IF Prud2$ >= MID$(nomb2$, 1, LEN(Prud2$)) THEN
                               ord$ = "þþþ"
                               IF medi2$ = "*" AND re2$ = " " THEN  'AND CVI(ubic2$) <> 2
                                    IF Stock = "1" THEN
                                         IF CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$) + CVI(ped2$) < CVI(mini12$) THEN
                                            IF CVI(exi12$) = 0 OR CVI(exi22$) = 0 OR CVI(exi32$) = 0 THEN
                                                 ElegOtr% = CVI(num$)
                                                 GOSUB Repetidos
                                                 IF devolver = 0 THEN
                                                    IF iv$ = "*" THEN MID$(ord$, 1, 1) = "*"
                                                    IF CVI(ubic2$) = 1 THEN MID$(ord$, 2, 1) = "*"
                                                    IF of12$ = "@" OR of22$ = "#" OR of32$ = "$" THEN MID$(ord$, 3, 1) = "*"
                                                    selec% = selec% + 1: GOSUB CalculaPromedio
                                                    RSET orden$ = ord$ + MID$(nomb2$, 1, 27) + promedio: RSET sitio$ = num$: PUT #3, selec%
                                                 END IF
                                            END IF
                                         END IF
                                    ELSE
                                         IF CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$) + CVI(ped2$) < CVI(mini22$) THEN
                                            IF CVI(exi12$) = 0 OR CVI(exi22$) = 0 OR CVI(exi32$) = 0 THEN
                                                 ElegOtr% = CVI(num$)
                                                 GOSUB Repetidos
                                                 IF devolver = 0 THEN
                                                    IF iv$ = "*" THEN MID$(ord$, 1, 1) = "*"
                                                    IF CVI(ubic2$) = 1 THEN MID$(ord$, 2, 1) = "*"
                                                    IF of12$ = "@" OR of22$ = "#" OR of32$ = "$" THEN MID$(ord$, 3, 1) = "*"
                                                    selec% = selec% + 1: GOSUB CalculaPromedio
                                                    RSET orden$ = ord$ + MID$(nomb2$, 1, 27) + promedio: RSET sitio$ = num$: PUT #3, selec%
                                                 END IF
                                            END IF
                                         END IF
                                    END IF
                               END IF
                          END IF
                       END IF
                    ELSE
                        mjfox = 0
                    END IF
               NEXT l
               SWAP puntero%, otropuntero%
               puntero% = puntero% + 1
               maxi20% = LOF(20) / 30
               IF puntero% >= maxi20% THEN EXIT DO
          LOOP
     END IF
     CLOSE #20: CLOSE #21
RETURN

BusqF4:
resp% = Verify(13, "¨Desea sacar fallas autom ticas")
IF resp% = true THEN
   COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... "
     FOR k = 1 TO selec%
         GET #3, k: GET #1, CVI(sitio$): LOCK #1, CVI(sitio$)
         RSET precio$ = pv32$
         '------
         'pedir$ = EntraDatoNumero$(11, 61, 6, STR$(CVI(canti$)), 1)
         '------
         IF Stock = "1" THEN
            pedir$ = STR$(CVI(mini12$) - (CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$) + CVI(ped2$)))
         ELSE
            pedir$ = STR$(CVI(mini22$) - (CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$) + CVI(ped2$)))
         END IF
         van = MKD$(CVD(van) + (.7 * CVD(pv32$) * (VAL(pedir$) - CVI(canti$)))): LOCATE 5, 1: PRINT "Este pedido,aprox. (PVP3 -30%):"; : PRINT USING "###########,.##"; CVD(van); : PRINT " acumulados :"; : PRINT USING "###########,.##"; (CVD( _
acumulado) + CVD(van))
         RSET ped2$ = MKI$(CVI(ped2$) - CVI(canti$))
         RSET ped2$ = MKI$(CVI(ped2$) + VAL(pedir$))
         RSET codigo3$ = codigo2$: RSET canti$ = MKI$(VAL(pedir$))
         PUT #1, CVI(sitio$): PUT #3, k: UNLOCK #1, CVI(sitio$)
     NEXT k
     GOSUB BusqF9
END IF
RETURN

OrdenarAlfabeticamente:
   CLOSE #3: OPEN "F:\kevinprg\kevindat\auxilio.fll" FOR RANDOM AS #3 LEN = 57
   FIELD #3, 19 AS semi$, 38 AS orden$: N = LOF(3) / 57: Salto = N: Box 10, 5, 16, 75: Center 13, "Ordenando , Espere ..."
   WHILE Salto > 1
        Salto = Salto / 2
        DO
             Sw = 1
             FOR J = 1 TO N - Salto
                  I = J + Salto
                  GET #3, J: au1 = semi$: au3 = orden$
                  GET #3, I: au2 = semi$: au4 = orden$
                  'LOCATE 1, 1: PRINT "-"; MID$(au3, 4, 30); "-"; MID$(au4, 4, 30); "-";
                  IF MID$(au3, 4, 30) > MID$(au4, 4, 30) THEN
                       RSET semi$ = au2: RSET orden$ = au4: PUT #3, J
                       RSET semi$ = au1: RSET orden$ = au3: PUT #3, I: Sw = 0
                  END IF
             NEXT J
        LOOP UNTIL Sw = 1
   WEND
RETURN
END SUB

SUB LeeEstado
OPEN "F:\kevinprg\kevindat\kaos.dat" FOR INPUT AS #2
INPUT #2, sonido: INPUT #2, impuesto: INPUT #2, Nombfarma: INPUT #2, Direccion
INPUT #2, Rif: INPUT #2, Nit: INPUT #2, Telefono: INPUT #2, contri
INPUT #2, contra: INPUT #2, Stock: INPUT #2, Blanq: INPUT #2, LargoH
INPUT #2, TipoL: INPUT #2, Mensaje1: INPUT #2, Mensaje2: INPUT #2, Mensaje3
INPUT #2, Redondeo: INPUT #2, Desc: INPUT #2, DescMax: INPUT #2, Exactitud
INPUT #2, Prov1: INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo: CLOSE
END SUB

FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$)
LOCATE fila%, columna%: PRINT CHR$(178) + caden$: LOCATE fila%, columna% + largo% + 1: PRINT CHR$(178): REDIM eleccion(0 TO largo% + 1) AS STRING * 1: tr$ = "0": llevasitio% = 1
FOR k = 1 TO largo%: eleccion(k) = MID$(caden$, k, 1): NEXT k: LOCATE fila%, (columna% + llevasitio%): COLOR colors(7, colorpref) + 16, colors(4, colorpref): IF eleccion(1 + VAL(tr$)) = " " THEN PRINT "_";  ELSE PRINT eleccion(1 + VAL(tr$))
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
               CASE CHR$(0) + CHR$(59): Fun(1) = "si": EXIT DO
               CASE CHR$(0) + CHR$(60): Fun(2) = "si": EXIT DO
               CASE CHR$(0) + CHR$(61): Fun(3) = "si": EXIT DO
               CASE CHR$(0) + CHR$(62): Fun(4) = "si": EXIT DO
               CASE CHR$(0) + CHR$(63): Fun(5) = "si": EXIT DO
               CASE CHR$(0) + CHR$(64): Fun(6) = "si": EXIT DO
               CASE CHR$(0) + CHR$(65): Fun(7) = "si": EXIT DO
               CASE CHR$(0) + CHR$(66): Fun(8) = "si": EXIT DO
               CASE CHR$(0) + CHR$(67): Fun(9) = "si": EXIT DO
               CASE CHR$(0) + CHR$(68): Fun(10) = "si": EXIT DO
               CASE CHR$(0) + CHR$(133): Fun(11) = "si": EXIT DO
               CASE CHR$(0) + CHR$(134): Fun(12) = "si": EXIT DO
               CASE CHR$(0) + CHR$(71): llevasitio% = 1
               CASE CHR$(0) + CHR$(79): llevasitio% = largo%
               CASE ELSE
                  eleccion(llevasitio%) = llave$
                  llevasitio% = llevasitio% + 1
                  IF llevasitio% > largo% THEN GOSUB Sumar: EXIT DO
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
eleccion$ = "": FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR colors(5, colorpref), colors(4, colorpref): LOCATE fila%, columna% + 1: PRINT eleccion$
MeteDatoLetra$ = eleccion$: IF sonido = "si" THEN BEEP
EXIT FUNCTION
Sumar:
FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna%: PRINT "²"; eleccion$; "²"
RETURN
END FUNCTION

SUB Suma1Dia (dat$, caso%)
SELECT CASE caso%
   CASE 1
      ' formato ddmmaaaa
      day% = VAL(MID$(dat$, 1, 2))
      month% = VAL(MID$(dat$, 3, 2))
      year% = VAL(MID$(dat$, 5, 4))
   CASE 2
      ' formato dd/mm/aaaa
      day% = VAL(MID$(dat$, 1, 2))
      month% = VAL(MID$(dat$, 4, 2))
      year% = VAL(MID$(dat$, 7, 4))
   CASE 3
      ' formato mm/dd/aaaa
      day% = VAL(MID$(dat$, 4, 2))
      month% = VAL(MID$(dat$, 1, 2))
      year% = VAL(MID$(dat$, 7, 4))
END SELECT
day% = day% + 1
SELECT CASE month%
   CASE 4, 6, 9, 11
      IF day% > 30 THEN day% = 1: month% = month% + 1
   CASE 2
     IF year% = 2000 THEN
          IF day% > 29 THEN day% = 1: month% = 3
     ELSE
          IF day% > 28 THEN day% = 1: month% = 3
     END IF
   CASE 12
     IF day% > 31 THEN day% = 1: month% = 1: year% = year% + 1
   CASE ELSE
     IF day% > 31 THEN day% = 1: month% = month% + 1
END SELECT
IF day% < 10 THEN
     dia$ = "0" + MID$(STR$(day%), 2, 1)
ELSE
     dia$ = MID$(STR$(day%), 2, 2)
END IF
IF month% < 10 THEN
     mes$ = "0" + MID$(STR$(month%), 2, 1)
ELSE
     mes$ = MID$(STR$(month%), 2, 2)
END IF
ano$ = MID$(STR$(year%), 2, 4)
SELECT CASE caso%
     CASE 1
       ' formato ddmmaaaa
       dat$ = dia$ + mes$ + ano$
     CASE 2
       ' formato dd/mm/aaaa
       dat$ = dia$ + "/" + mes$ + "/" + ano$
     CASE 3
       ' formato mm/dd/aaaa
       dat$ = mes$ + "/" + dia$ + "/" + ano$
END SELECT

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

FUNCTION Tratafechas$ (dat$, fechaentra$, fechasale$)
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
Tratafechas$ = fechasale$

END FUNCTION

FUNCTION Trim$ (x$)
IF x$ = "" THEN
     Trim$ = ""
ELSE
lastChar = 0
FOR A = 1 TO LEN(x$): Y$ = MID$(x$, A, 1): IF Y$ <> CHR$(0) AND Y$ <> " " THEN lastChar = A
NEXT A: Trim$ = LEFT$(x$, lastChar)
END IF
END FUNCTION

