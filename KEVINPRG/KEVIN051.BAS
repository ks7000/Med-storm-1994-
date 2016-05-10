DEFINT A-Z
DECLARE SUB Espera ()
DECLARE SUB ModProd (cadena$, opcion%, nombr$)
DECLARE SUB NuevoArt ()
DECLARE SUB LeeEstado ()
DECLARE SUB SalvaEstado ()
DECLARE SUB Center (row, text$)
DECLARE SUB Box (Row1%, Col1%, Row2%, Col%)
DECLARE SUB BorraBox (Row1%, Col1%, Row2%, Col%)
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$)
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Escoge% (fila%, edo%, fila2%, Prod$)
DECLARE FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%, est%)
DECLARE FUNCTION Trim$ (x$)
DECLARE FUNCTION Inexpertos$ (cadena AS STRING, opcion AS INTEGER)
DECLARE FUNCTION Substituye$ (cad1 AS STRING, cad2 AS STRING, cad3 AS STRING)
DECLARE FUNCTION Verify% (ubi%, text$)
COMMON SHARED guia, colorpref AS INTEGER
DIM SHARED turno AS STRING * 1, devolver
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, impuesto AS STRING * 6, Nombfarma AS STRING * 25, Direccion AS STRING * 30, rif     AS STRING * 25, nit      AS STRING * 25, telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2
DIM SHARED Fun(17) AS STRING * 2, salir, Turna, terminar, sitio
DIM SHARED Contenido(199) AS STRING * 15, Code(199) AS STRING * 3
DIM SHARED Nombra(5) AS STRING * 35, Pvp1a(5) AS STRING * 8, Pvp2a(5) AS STRING * 8, Pvp3a(5) AS STRING * 8, Pvp4a(5) AS STRING * 8, Numa(5) AS STRING * 2, Observa(5) AS STRING * 20, Exis1a(5) AS STRING * 2, Exis2a(5) AS STRING * 2, Exis3a(5) AS  _
STRING * 2, Exis4a(5) AS STRING * 2
DIM SHARED Cant1a(5) AS STRING * 2, Cant2a(5) AS STRING * 2, Cant3a(5) AS STRING * 2, Cant4a(5) AS STRING * 2, grabGav AS STRING * 1
CONST true = -1: CONST false = NOT true
ON TIMER(5) GOSUB AbreGav
TIMER ON
LeeEstado
DO WHILE guia < 3
     CALL NuevoArt
LOOP
CHAIN "f:\kevinprg\kevin01.bas"
ErrorTrap:
    SELECT CASE ERR
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

FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%, est%)
COLOR colors(7, colorpref), colors(4, colorpref): CALL Box(fila%, columna%, fila% + 16, columna% + 16): CALL Center(25, "[F1] Ayuda [Enter]Tomar Opcion [A-Z]Ir a... [Esc]Menu Anterior"): COLOR colors(4, colorpref), colors(7, colorpref): CLOSE #5
Center 25, "[F5]Edita/Adiciona Linea [F8]Borrar Linea [F9]Salir/Salvar [Enter]Tomar Opcion"
SELECT CASE estesi%
     CASE 1
          LOCATE fila%, columna% + 2: PRINT "Ubicacion"
          OPEN "F:\kevinprg\kevindat\ubica.dat" FOR RANDOM SHARED AS #5 LEN = 18: FIELD #5, 3 AS codigo$, 15 AS nomb$
          FOR k = 1 TO 15: GET #5, k: Contenido(k) = nomb$: Code(k) = codigo$: NEXT k: selecc% = 199
     CASE 2
          LOCATE fila%, columna% + 1: PRINT "Laboratorio"
          OPEN "F:\kevinprg\kevindat\lab.dat" FOR RANDOM SHARED AS #5 LEN = 18: FIELD #5, 3 AS codigo$, 15 AS nomb$
          FOR k = 1 TO 15: GET #5, k: Contenido(k) = nomb$: Code(k) = codigo$: NEXT k: selecc% = 999
     CASE 4
          LOCATE fila%, columna% + 1: PRINT "Componente"; ingrediente%
          OPEN "F:\kevinprg\kevindat\componen.dat" FOR RANDOM SHARED AS #5 LEN = 18: FIELD #5, 3 AS codigo$, 15 AS nomb$
          FOR k = 1 TO 15: GET #5, k: Contenido(k) = nomb$: Code(k) = codigo$: NEXT k: selecc% = 999
     CASE 5
          LOCATE fila%, columna% + 1: PRINT " Proveedor "
          OPEN "F:\kevinprg\kevindat\lab.dat" FOR RANDOM SHARED AS #5 LEN = 18: FIELD #5, 3 AS codigo$, 15 AS nomb$
          FOR k = 1 TO 15: GET #5, k: Contenido(k) = nomb$: Code(k) = codigo$: NEXT k: selecc% = 199
     CASE 6
          LOCATE fila%, columna% + 1: PRINT " Banco     "
          OPEN "F:\kevinprg\kevindat\bank.dat" FOR RANDOM SHARED AS #5 LEN = 18: FIELD #5, 3 AS codigo$, 15 AS nomb$
          FOR k = 1 TO 15: GET #5, k: Contenido(k) = nomb$: Code(k) = codigo$: NEXT k: selecc% = 199
     CASE 7
          LOCATE fila%, columna% + 1: PRINT " Cliente   "
          OPEN "F:\kevinprg\kevindat\clientes.dat" FOR RANDOM SHARED AS #5 LEN = 38: FIELD #5, 3 AS codigo$, 35 AS nomb$
          FOR k = 1 TO 15: GET #5, k: Contenido(k) = nomb$: Code(k) = codigo$: NEXT k: selecc% = 999
END SELECT
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila% + 1, columna% + 1: PRINT Contenido(1): COLOR colors(4, colorpref), colors(7, colorpref)
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
            CASE CHR$(13): terminar = true
            CASE CHR$(27): terminar = true: resal% = 0: up% = 0: devolver = true
            CASE IS >= CHR$(65)
               IF Kbd$ <= CHR$(90) THEN
                    FOR k = 1 TO selecc%
                         GET #5, k
                         IF Kbd$ = MID$(nomb$, 1, 1) THEN up% = k: resal% = 1: EXIT FOR
                         IF k = selecc% THEN BEEP
                    NEXT k
                    IF up% + 14 > selecc% THEN up% = selecc% - 14: resal% = 1
                    GOSUB Lee15
               END IF
            CASE CHR$(0) + CHR$(63): terminar = true: Fun(5) = "si": sitio = resal%
            CASE CHR$(0) + CHR$(66): terminar = true: Fun(8) = "si": sitio = resal%
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
Elije% = resal%
EXIT FUNCTION

Lee15:
FOR k = 0 TO 14
     GET #5, up% + k: Contenido(k + 1) = nomb$: Code(k + 1) = codigo$
NEXT k
RETURN

END FUNCTION

FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
LOCATE fila%, columna%: PRINT CHR$(178) + caden$ + SPACE$(largo% - LEN(caden$)): LOCATE fila%, columna% + largo% + 1: PRINT CHR$(178)
REDIM eleccion(0 TO largo% + 1) AS STRING * 1: llevasitio% = 2
FOR k = 1 TO largo%: eleccion(k) = MID$(caden$, k, 1): NEXT k: LOCATE fila%, (columna% + llevasitio%): COLOR colors(4, colorpref) + 16, colors(7, colorpref): IF eleccion(2) = " " THEN PRINT "_";  ELSE PRINT eleccion(2)
DO
      llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
          SELECT CASE llave$
               CASE CHR$(0) + "H": devolver = true: EXIT DO
               CASE CHR$(0) + "P": EXIT DO
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
PRINT STRING$(80, "Ä");
FOR k = fila% TO fila% + 4: LOCATE k, 1: PRINT SPACE$(80); : NEXT k: STATIC max2%
CALL Center(25, "[F1] Ayuda [F9]y[Esc] Menu Principal [Enter] Toma Articulo [A-Z] Ir a")
Prod$ = "      "
DO WHILE Prod$ = "      "
     LOCATE fila2%, 5: PRINT "Introduzca Orden de Busqueda:"; SPACE$(15)
     Prod$ = UCASE$(MeteDatoLetra(fila2%, 34, 6, "      ")): Prod$ = Trim$(Prod$)
     IF Fun(9) = "si" OR Prod$ = "" THEN : Escoge% = -1: EXIT FUNCTION
LOOP
selecc% = 0
CLOSE : OPEN "f:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
GOSUB Parche1: GOSUB Parche2
DO
     Kbd$ = "": WHILE Kbd$ = "": Kbd$ = INKEY$: WEND: Kbd$ = UCASE$(Kbd$)
     SELECT CASE Kbd$
          CASE CHR$(0) + "H": GOSUB Arriba
          CASE CHR$(0) + "P": GOSUB Abajo
          CASE CHR$(0) + CHR$(73): GOSUB Apgup
          CASE CHR$(0) + CHR$(81): GOSUB Apgdn
          CASE CHR$(13): EXIT DO
          CASE CHR$(27): resal% = 0: EXIT DO
          CASE CHR$(0) + CHR$(67): resal% = 0: Fun(3) = "si": EXIT DO
          CASE CHR$(0) + CHR$(62): Fun(4) = "si": EXIT DO
          CASE CHR$(0) + CHR$(64): Fun(6) = "si": EXIT DO
          CASE CHR$(0) + CHR$(65): Fun(7) = "si": EXIT DO
          CASE CHR$(0) + CHR$(67): Fun(9) = "si": EXIT DO
          CASE CHR$(0) + CHR$(68): Fun(10) = "si": EXIT DO
          CASE IS >= CHR$(65)
               IF Kbd$ <= CHR$(90) AND edo% = 1 THEN
                    Prod$ = Kbd$: GOSUB Parche1: GOSUB Parche2
               END IF
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
          selecc% = selecc% + 1: Nombra(selecc%) = nomb2$: Cant1a(selecc%) = exi12$: Cant2a(selecc%) = exi22$: Cant3a(selecc%) = exi32$: Cant4a(selecc%) = exi42$: Pvp1a(selecc%) = pv12$: Pvp2a(selecc%) = pv22$: Pvp3a(selecc%) = pv32$: Pvp4a(selecc%) _
 = pv42$: Numa(selecc%) = num$
          END IF
     ELSE
          EXIT FOR
     END IF
NEXT k
RETURN
Parche1:
          CLOSE #2: OPEN "f:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
          FIELD #2, 35 AS nomshort$, 2 AS num$: max2% = LOF(2) / 37: busqueda = 1
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

FUNCTION Inexpertos$ (cadena AS STRING, opcion AS INTEGER) STATIC
DIM auxa AS STRING, prev AS STRING, nivel AS INTEGER, escape AS INTEGER
DIM cad AS STRING
IF opcion = -1 THEN prev = "": Inexpertos$ = cadena: EXIT FUNCTION
auxa = UCASE$(cadena): escape = false
IF auxa = prev THEN
   nivel = nivel + 1
ELSE
   nivel = 1: prev = auxa
END IF
IF opcion > 0 THEN nivel = opcion
DO WHILE escape = false
   SELECT CASE nivel
      CASE 1 TO 100
         GOSUB Etapa1
      CASE 101 TO 200
         GOSUB Etapa2
      CASE 201 TO 300
         GOSUB Etapa3
      CASE ELSE
         auxa = "": escape = true
   END SELECT
LOOP
Inexpertos$ = auxa
EXIT FUNCTION

Etapa1:
   SELECT CASE nivel
      CASE 1
         auxa = "-" + auxa: escape = true
      CASE 2
         auxa = Substituye$(auxa, "MALOX", "MAALOX"): escape = true
         'auxa = "/" + auxa: escape = true
      CASE 3
         auxa = "*" + auxa: escape = true
      CASE 4
         auxa = "!" + auxa: escape = true
      CASE 5
         auxa = "/-" + auxa: escape = true
      CASE 6
         auxa = ">" + auxa: escape = true
      CASE 7
         auxa = "<" + auxa: escape = true
      CASE 8
         auxa = Substituye$(auxa, "-", ""): escape = true
      CASE 9
         auxa = Substituye$(auxa, "'", ""): escape = true
      CASE 10
         auxa = Substituye$(auxa, "+", ""): escape = true
      CASE ELSE
         nivel = 101
   END SELECT
RETURN

Etapa2:
   SELECT CASE nivel
      CASE 101
         auxa = Substituye$(auxa, "¤", "¥"): escape = true
      CASE 102
         auxa = Substituye$(auxa, "¥", "¤"): escape = true
      CASE 103
         auxa = Substituye$(auxa, "B", "V"): escape = true
      CASE 104
         auxa = Substituye$(auxa, "V", "B"): escape = true
      CASE 105
         auxa = Substituye$(auxa, "S", "Z"): escape = true
      CASE 106
         auxa = Substituye$(auxa, "S", "C"): escape = true
      CASE 107
         auxa = Substituye$(auxa, "S", "X"): escape = true
      CASE 108
         auxa = Substituye$(auxa, "Y", "I"): escape = true
      CASE 109
         auxa = Substituye$(auxa, "I", "Y"): escape = true
      CASE 110
         auxa = Substituye$(auxa, "Z", "S"): escape = true
      CASE 111
         auxa = Substituye$(auxa, "C", "S"): escape = true
      CASE 112
         auxa = Substituye$(auxa, "C", "X"): escape = true
      CASE 113
         auxa = Substituye$(auxa, "C", "K"): escape = true
      CASE 114
         auxa = Substituye$(auxa, "K", "C"): escape = true
      CASE 115
         auxa = Substituye$(auxa, "G", "J"): escape = true
      CASE 116
         auxa = Substituye$(auxa, "J", "G"): escape = true
      CASE 117
         auxa = Substituye$(auxa, "W", "V"): escape = true
      CASE 118
         auxa = Substituye$(auxa, "V", "W"): escape = true
      CASE 119
         auxa = Substituye$(auxa, "W", "U"): escape = true
      CASE 120
         auxa = Substituye$(auxa, "U", "W"): escape = true
      CASE 121
         auxa = Substituye$(auxa, "N", "M"): escape = true
      CASE 122
         auxa = Substituye$(auxa, "M", "N"): escape = true
   
      CASE 123
         auxa = Substituye$(auxa, "TA", "THA"): escape = true
      CASE 124
         auxa = Substituye$(auxa, "TE", "THE"): escape = true
      CASE 125
         auxa = Substituye$(auxa, "TI", "THI"): escape = true
      CASE 126
         auxa = Substituye$(auxa, "TO", "THO"): escape = true
      CASE 127
         auxa = Substituye$(auxa, "TU", "THU"): escape = true
      CASE 128
         auxa = Substituye$(auxa, "PA", "PHA"): escape = true
      CASE 129
         auxa = Substituye$(auxa, "PE", "PHE"): escape = true
      CASE 130
         auxa = Substituye$(auxa, "PI", "PHI"): escape = true
      CASE 131
         auxa = Substituye$(auxa, "PO", "PHO"): escape = true
      CASE 132
         auxa = Substituye$(auxa, "PU", "PHU"): escape = true
      CASE 133
         auxa = Substituye$(auxa, "MI", "MY"): escape = true
      CASE 134
         auxa = Substituye$(auxa, "ZAR", "ZAAR"): escape = true
      CASE 135
         auxa = Substituye$(auxa, "ERL", "EHRL"): escape = true
      CASE 136
         auxa = Substituye$(auxa, "ROPI", "-ROHPI"): escape = true
      CASE 137
         auxa = Substituye$(auxa, "ROPI", "ROHPI"): escape = true
      CASE 138
         auxa = Substituye$(auxa, "MALOX", "MAALOX"): escape = true
      CASE 139
         auxa = Substituye$(auxa, "DIDER", "DIHY"): escape = true
      CASE 140
         auxa = Substituye$(auxa, "DYDER", "DIHY"): escape = true
      CASE 141
         auxa = Substituye$(auxa, "EUTI", "EUTHY"): escape = true
      CASE 142
         auxa = Substituye$(auxa, "EUTY", "EUTHY"): escape = true
      CASE 143
         auxa = Substituye$(auxa, "MILE", "MILLE"): escape = true

      CASE 144
         auxa = Substituye$(auxa, "Y", "LL"): escape = true
      CASE 145
         auxa = Substituye$(auxa, "LL", "Y"): escape = true
      CASE 146
         auxa = Substituye$(auxa, "Y", "GH"): escape = true
      CASE 147
         auxa = Substituye$(auxa, "LL", "GH"): escape = true
      CASE 148
         auxa = Substituye$(auxa, "CC", "X"): escape = true
      CASE 149
         auxa = Substituye$(auxa, "X", "CC"): escape = true
      CASE 150
         auxa = Substituye$(auxa, "K", "KK"): escape = true
      CASE 151
         auxa = Substituye$(auxa, "C", "CC"): escape = true
      CASE 152
         auxa = Substituye$(auxa, "S", "SS"): escape = true
      CASE 153
         auxa = Substituye$(auxa, "E", "EE"): escape = true
      CASE 154
         auxa = Substituye$(auxa, "AA", "A"): escape = true
  
      CASE 155
         auxa = Substituye$(auxa, "CA", "KA"): escape = true
      CASE 156
         auxa = Substituye$(auxa, "CI", "ZY"): escape = true
      CASE 157
         auxa = Substituye$(auxa, "CO", "KO"): escape = true
      CASE 158
         auxa = Substituye$(auxa, "CU", "KU"): escape = true
      CASE 159
         auxa = Substituye$(auxa, "CU", "QU"): escape = true
      CASE 160
         auxa = Substituye$(auxa, "KA", "CA"): escape = true
      CASE 161
         auxa = Substituye$(auxa, "KE", "QUE"): escape = true
      CASE 162
         auxa = Substituye$(auxa, "KI", "QUI"): escape = true
      CASE 163
         auxa = Substituye$(auxa, "KO", "CO"): escape = true
      CASE 164
         auxa = Substituye$(auxa, "KU", "QU"): escape = true
      CASE 165
         auxa = Substituye$(auxa, "CN", "MN"): escape = true
      CASE 166
         auxa = Substituye$(auxa, "KN", "MN"): escape = true
      CASE 167
         auxa = Substituye$(auxa, "CS", "XS"): escape = true
      CASE 168
         auxa = Substituye$(auxa, "SI", "ZY"): escape = true
      CASE 169
         auxa = Substituye$(auxa, "ZI", "ZY"): escape = true
      CASE 170
         auxa = Substituye$(auxa, "WA", "KWA"): escape = true
      CASE 171
         auxa = Substituye$(auxa, "QUE", "KE"): escape = true
      CASE 172
         auxa = Substituye$(auxa, "QUI", "KI"): escape = true
      CASE 173
         auxa = Substituye$(auxa, "M", "MM"): escape = true
      CASE 174
         auxa = Substituye$(auxa, "N", "NS"): escape = true
   
      CASE ELSE
         nivel = 201
   END SELECT
RETURN

Etapa3:
   SELECT CASE nivel
      CASE 201
         cad = MID$(auxa, 1, 1): escape = true
         SELECT CASE cad
            CASE "A", "E", "I", "O", "U", "Y"
               auxa = Substituye$(auxa, cad, "H" + cad): escape = true
            CASE ELSE
         END SELECT
      CASE 202
         auxa = Substituye$(auxa, " ", "A"): escape = true
      CASE 203
         auxa = Substituye$(auxa, "‚", "E"): escape = true
      CASE 204
         auxa = Substituye$(auxa, "¡", "I"): escape = true
      CASE 205
         auxa = Substituye$(auxa, "¢", "O"): escape = true
      CASE 206
         auxa = Substituye$(auxa, "£", "U"): escape = true
      CASE 207
         auxa = Substituye$(auxa, "L", "LL"): escape = true
      CASE 208
         auxa = Substituye$(auxa, "L", "Y"): escape = true
      CASE 209
         auxa = Substituye$(auxa, "CS", "X"): escape = true

      CASE ELSE
         nivel = 301
   END SELECT
RETURN
END FUNCTION

SUB LeeEstado
CLOSE : OPEN "f:\kevinprg\kevindat\kaos.dat" FOR INPUT SHARED AS #2
INPUT #2, sonido: INPUT #2, impuesto: INPUT #2, Nombfarma: INPUT #2, Direccion: INPUT #2, rif: INPUT #2, nit: INPUT #2, telefono: INPUT #2, contri: INPUT #2, contra: INPUT #2, Stock: INPUT #2, Blanq
INPUT #2, LargoH: INPUT #2, TipoL: INPUT #2, Mensaje1: INPUT #2, Mensaje2: INPUT #2, Mensaje3: INPUT #2, Redondeo: INPUT #2, Desc: INPUT #2, DescMax: INPUT #2, Exactitud: INPUT #2, Prov1: INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo: CLOSE
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
                  FOR k = largo% TO llevasitio% + 1 STEP -1: eleccion(k) = eleccion(k - 1): NEXT k
                  eleccion(llevasitio%) = llave$: llevasitio% = llevasitio% + 1
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
FOR k = 1 TO largo%
   IF ASC(MID$(eleccion$, k, 1)) < 32 THEN
      MID$(eleccion$, k, 1) = " "
   END IF
NEXT k
MeteDatoLetra$ = eleccion$
IF sonido = "si" THEN BEEP
EXIT FUNCTION
Sumar:
FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna%: PRINT "²"; eleccion$; "²"
RETURN
END FUNCTION

SUB ModProd (cadena$, opcion%, nombr$)
COLOR colors(7, colorpref), colors(4, colorpref)
CLS : FOR k = 1 TO 12: Fun(k) = "no": NEXT k
Box 1, 1, 3, 80: Center 1, "- Adici¢n de un art¡culo nuevo -"
Box 11, 5, 20, 75: COLOR colors(4, colorpref), colors(7, colorpref): Center 12, "Introduzca su Clave de Vendedor.": COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 14, 16: PRINT "Clave Corta del Vendedor:": Busca$ = MeteDatoLetra(14, 41, 3, "   ")
CLOSE : OPEN "f:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #1 LEN = 100
FIELD #1, 1 AS acti2$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
nursaler = LOF(1) / 100: eleg% = 0: IF nursaler = 0 THEN Box 11, 5, 12, 75: Center 11, "No hay Vendedores Registrados": Espera: CLOSE : EXIT SUB
FOR k = 1 TO nursaler: GET #1, k
IF Busca$ = clco$ AND acti2$ = " " THEN eleg% = k: cedula$ = cedu$
NEXT k: CLOSE
IF eleg% = 0 THEN Center 14, "No Hay Ningun Vendedor Con Esa Clave.": Espera: EXIT SUB
CLOSE : OPEN "f:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$

ElegMod% = (LOF(1) / 200) + 1
RSET codigo2$ = cadena$: LSET nomb2$ = nombr$
RSET mini12$ = MKI$(1): RSET mini22$ = MKI$(1): RSET maxi12$ = MKI$(0): RSET maxi22$ = MKI$(0): RSET ubic2$ = MKI$(4): LSET obser2$ = SPACE$(35): RSET acti2$ = " ": RSET exi12$ = MKI$(0): RSET exi22$ = MKI$(0)
RSET exi32$ = MKI$(0): RSET exi42$ = MKI$(0): RSET pv12$ = MKD$(0): RSET pv22$ = MKD$(0): RSET pv32$ = MKD$(10): RSET pv42$ = MKD$(0): RSET cos12$ = MKD$(0): RSET cos22$ = MKD$(0): RSET cos32$ = MKD$(0): RSET cos42$ = MKD$(0): RSET iv$ = " "
RSET of12$ = " ": RSET of22$ = " ": RSET of32$ = " ": RSET labo2$ = MKI$(1): RSET re2$ = " ": RSET ped2$ = MKI$(0): RSET medi2$ = "*": RSET misc2$ = " ": RSET quim2$ = " ": RSET compo12$ = MKI$(1): RSET compo22$ = MKI$(1): RSET compo32$ = MKI$(1)
RSET compo42$ = MKI$(1): RSET compo52$ = MKI$(1): RSET compo62$ = MKI$(1): RSET preg$ = " ": RSET saldo$ = MKI$(0): RSET upc$ = MKD$(0): RSET ven11$ = " ": RSET ven12$ = MKI$(0): RSET ven21$ = " ": RSET ven22$ = MKI$(0): RSET ven31$ = " ": RSET  _
ven32$ = MKI$(0): RSET ven41$ = " ": RSET ven42$ = MKI$(0)
PUT #1, ElegMod%: LOCK #1, ElegMod%

COLOR colors(7, colorpref), colors(4, colorpref): Center 1, "ÄÄÄ Adicion de Un Nuevo Articulo , Codigo:²" + cadena$ + "²"
FOR k = 4 TO 12: LOCATE k, 1: PRINT SPACE$(80); : NEXT k
COLOR colors(4, colorpref), colors(7, colorpref): Box 2, 2, 24, 79
Center 25, "[F1]Ayuda [F6]Ir PVP [F7]Ir Oferta [F9]Salir y Salvar [F10]Salir SIN Salvar"
COLOR colors(4, colorpref), colors(7, colorpref)
LOCATE 3, 3: PRINT "Nombre:"; SPACE$(37); "Ubicacion:"
LOCATE 4, 3: PRINT "Observaciones:"; SPACE$(23); "Laboratorio:"
LOCATE 5, 2: PRINT "ÃMinimo1ÄÂMaximo1ÄÂMinimo2ÄÂMaximo2Ä¿";
LOCATE 6, 2: PRINT "³"; STRING$(8, " "); "³"; STRING$(8, " "); "³"; STRING$(8, " "); "³"; STRING$(8, " "); "³";
LOCATE 7, 2: PRINT "Ã"; STRING$(8, "Ä"); "Á"; STRING$(8, "Ä"); "Á"; STRING$(8, "Ä"); "Á"; STRING$(8, "Ä"); "Ù";
LOCATE 8, 1: PRINT "ÕÏ"; STRING$(18, "Í"); "Ñ"; STRING$(19, "Í"); "Ñ"; STRING$(19, "Í"); "ÑOFERTAÍÍÍÍÍÍÍÍÍÍÍµ";
LOCATE 9, 1: PRINT "³"; "PVP.1"; SPACE$(14); "³"; "PVP.2"; SPACE$(14); "³"; "PVP.3"; SPACE$(14); "³PVP"; SPACE$(14); "³";
LOCATE 10, 1: PRINT "Æ"; STRING$(19, "Í"); "Ø"; STRING$(19, "Í"); "Ø"; STRING$(19, "Í"); "Ø"; STRING$(17, "Í"); "µ";
LOCATE 11, 1: PRINT "³Exist1."; STRING$(12, " "); "³Exist2."; STRING$(12, " "); "³Exist3."; STRING$(12, " "); ; "³Exist"; STRING$(12, " "); "³";
LOCATE 12, 1: PRINT "Æ"; STRING$(19, "Í"); "Ø"; STRING$(19, "Í"); "Ø"; STRING$(19, "Í"); "Ø"; STRING$(17, "Í"); "µ";
LOCATE 13, 1: PRINT "³Csto1"; SPACE$(14); "³Csto2"; SPACE$(14); "³Csto3"; SPACE$(14); "³Cto"; SPACE$(14); "³";
LOCATE 14, 1: PRINT "Æ"; STRING$(19, "Í"); "Ø"; STRING$(19, "Í"); "Ø"; STRING$(19, "Í"); "Ø"; STRING$(17, "Í"); "µ";
LOCATE 15, 1: PRINT "³Vence 1"; STRING$(12, " "); "³Vence 2"; STRING$(12, " "); "³Vence 3"; STRING$(12, " "); "³Vence"; STRING$(12, " "); "³";
LOCATE 16, 1: PRINT "ÔÑ"; STRING$(18, "Í"); "Ï"; STRING$(19, "Í"); "Ï"; STRING$(19, "Í"); "Ï"; STRING$(17, "Í"); "µ";
LOCATE 17, 2: PRINT "Ã"; "Oferta1"; STRING$(12, "Ä"); "Â"; "Oferta2"; STRING$(12, "Ä"); "Â"; "Oferta3"; STRING$(12, "Ä"); "¿";
LOCATE 18, 22: PRINT "³"; SPACE$(19); "³"; SPACE$(19); "³";
LOCATE 19, 2: PRINT "Ã"; STRING$(19, "Ä"); "Á"; STRING$(19, "Ä"); "Á"; STRING$(19, "Ä"); "Ù";
LOCATE 20, 3: PRINT "Falla de Falla "; SPACE$(5); "Medicina:    Miscelanea:    Quimico:"
LOCATE 21, 3: PRINT "Componentes:        Cantidad de Solicitud de Pedido:";
COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 3, 10: PRINT "²"; nomb2$; "²": LOCATE 4, 18: PRINT "²"; obser2$; "²"
LOCATE 6, 3: PRINT USING "²#####,²"; CVI(mini12$);
LOCATE 6, 12: PRINT USING "²#####,²"; CVI(maxi12$);
LOCATE 6, 21: PRINT USING "²#####,²"; CVI(mini22$);
LOCATE 6, 30: PRINT USING "²#####,²"; CVI(maxi22$);
LOCATE 6, 40: PRINT "Producto Exento"
LOCATE 9, 7: PRINT USING "²########,.##²"; CVD(pv12$); : LOCATE 9, 27: PRINT USING "²########,.##²"; CVD(pv22$);
LOCATE 9, 47: PRINT USING "²########,.##²"; CVD(pv32$); : LOCATE 11, 9: PRINT USING "²#########,²"; CVI(exi12$);
LOCATE 11, 29: PRINT USING "²#########,²"; CVI(exi22$); : LOCATE 11, 49: PRINT USING "²#########,²"; CVI(exi32$);
LOCATE 13, 7: PRINT USING "²########,.##²"; CVD(cos12$); : LOCATE 13, 27: PRINT USING "²########,.##²"; CVD(cos22$);
LOCATE 13, 47: PRINT USING "²########,.##²"; CVD(cos32$); : GOSUB Buscaubicacion: GOSUB ImpriOfer: GOSUB Buscalaboratorio
gr$ = "²  /    ²"
LOCATE 15, 10: PRINT gr$: LOCATE 15, 30: PRINT gr$: LOCATE 15, 50: PRINT gr$: LOCATE 15, 68: PRINT gr$
LOCATE 20, 18: PRINT "²"; re2$; "²": LOCATE 20, 32: PRINT "²"; medi2$; "²"
LOCATE 20, 47: PRINT "²"; misc2$; "²": LOCATE 20, 59: PRINT "²"; quim2$; "²"
LOCATE 21, 55: PRINT "²"; : PRINT USING "##"; CVI(ped2$); : PRINT "²"
LOCATE 22, 3: PRINT "²"; : PRINT USING "###"; CVI(compo12$); : PRINT "²"
GOSUB BuscaComponente1: GOSUB BuscaComponente2: GOSUB BuscaComponente3
GOSUB BuscaComponente4: GOSUB BuscaComponente5: GOSUB BuscaComponente6
LOCATE 18, 3: PRINT "²"; of12$; "²"; : IF of12$ = "@" THEN PRINT Prov1 ELSE PRINT SPACE$(15)
LOCATE 18, 23: PRINT "²"; of22$; "²"; : IF of22$ = "#" THEN PRINT Prov2 ELSE PRINT SPACE$(15)
LOCATE 18, 43: PRINT "²"; of32$; "²"; : IF of32$ = "$" THEN PRINT Prov3 ELSE PRINT SPACE$(15)

salir = false: sit% = 2
DO WHILE NOT salir
    SELECT CASE sit%
     CASE 2
          Center 25, "[F1]Ayuda [F6]Ir A PVP  [F7]Ir A Oferta  [F9]Salir y Salvar"
          COLOR colors(4, colorpref), colors(7, colorpref): BorraBox 5, 63, 21, 79: GOSUB ImpriOfer
          auxilio$ = MeteDatoLetra(3, 10, 35, nomb2$)
          RSET nomb2$ = auxilio$
          IF devolver = true THEN devolver = false: sit% = 2 ELSE sit% = 4: GOSUB Guiar
     CASE 4
          control% = Elije%(1, 5, 63, 3, 59, 0, 0)
          IF control% < 1 THEN
                sit% = 6
                IF devolver = true THEN
                    devolver = false: sit% = 2
                END IF
          ELSE
               IF devolver = true THEN
                    devolver = false: sit% = 2
               ELSE
                    sit% = 6
               END IF
          END IF
          IF Fun(8) = "si" THEN
               Fun(8) = "no"
               IF Code(control%) = "001" OR Code(control%) = "002" OR Code(control%) = "03" OR Code(control%) = "04" OR Code(control%) = "05" THEN
                    GOSUB NoBorrar: sit% = 4: control% = -1
               ELSE
                    LOCATE sitio + 5, 64: PRINT "þ            ": Contenido(control%) = "þ": control% = 1: GOSUB SortYname: sit% = 4
                    control% = 1: GOSUB SortYname: sit% = 4: control% = -1
               END IF
          END IF
          IF Fun(5) = "si" THEN
               Fun(5) = "no"
                    IF Code(control%) = "001" OR Code(control%) = "002" OR Code(control%) = "03" OR Code(control%) = "04" OR Code(control%) = "05" THEN
                         GOSUB NoEdit: sit% = 4: control% = -1
                    ELSE
                         Contenido(control%) = MeteDatoLetra(sitio + 5, 63, 15, (Contenido(control%)))
                         control% = 1: GOSUB SortYname: sit% = 4: control% = -1
                    END IF
          END IF
          IF control% > 0 THEN RSET ubic2$ = MKI$(VAL(Code(control%)))
          GOSUB Buscaubicacion: IF CVI(ubic2$) = 1 THEN Center 24, "Recuerde colocarle numero de tabletas/ampollas": BEEP
     CASE 6
          Center 25, "[F1]Ayuda  [F6]Ir A PVP  [F7]Ir A Oferta  [F9] Salir y Salvar "
          COLOR colors(4, colorpref), colors(7, colorpref): BorraBox 5, 63, 21, 79: GOSUB ImpriOfer
          auxilio$ = MeteDatoLetra(4, 18, 20, obser2$)
          RSET obser2$ = auxilio$
          IF devolver = true THEN devolver = false: sit% = 4 ELSE sit% = 8
          GOSUB Guiar
     CASE 8
          control% = Elije%(2, 5, 63, 4, 53, 0, 0)
          IF control% < 1 THEN
                sit% = 10
                IF devolver = true THEN
                    devolver = false: sit% = 6
                END IF
          ELSE
               IF devolver = true THEN
                    devolver = false: sit% = 6
               ELSE
                    sit% = 10
               END IF
          END IF
          IF Fun(8) = "si" THEN
               Fun(8) = "no"
               IF Code(control%) = "001" THEN
                    GOSUB NoBorrar: sit% = 8: control% = -1
               ELSE
                    LOCATE sitio + 5, 64: PRINT "þ            ": Contenido(control%) = "þ": control% = 2: GOSUB SortYname: sit% = 8
                    control% = 2: GOSUB SortYname: sit% = 8: control% = -1
               END IF
          END IF
          IF Fun(5) = "si" THEN
               Fun(5) = "no"
               IF Code(control%) = "001" THEN
                    GOSUB NoEdit: sit% = 8: control% = -1
               ELSE
                    Contenido(control%) = MeteDatoLetra(sitio + 5, 63, 15, (Contenido(control%)))
                    control% = 2: GOSUB SortYname: sit% = 8: control% = -1
               END IF
          END IF
          IF control% > 0 THEN RSET labo2$ = MKI$(VAL(Code(control%)))
          GOSUB Buscalaboratorio
     CASE 10
          Center 25, "[F1]Ayuda  [F6]Ir A PVP  [F7]Ir A Oferta  [F9]Salir y Salvar"
          COLOR colors(4, colorpref), colors(7, colorpref): BorraBox 5, 63, 21, 79: GOSUB ImpriOfer: auxilio$ = "32501"
          DO WHILE ABS(VAL(auxilio$)) > 32500: auxilio$ = EntraDatoNumero$(6, 3, 6, STR$(CVI(mini12$)), 1): LOOP
          RSET mini12$ = MKI$(VAL(auxilio$)): LOCATE 6, 3: PRINT USING "²#####,²"; CVI(mini12$);
          IF devolver = true THEN devolver = false: sit% = 8 ELSE sit% = 12
          GOSUB Guiar
     CASE 12
          auxilio$ = "32501"
          DO WHILE ABS(VAL(auxilio$)) > 32500: auxilio$ = EntraDatoNumero$(6, 12, 6, STR$(CVI(maxi12$)), 1): LOOP
          RSET maxi12$ = MKI$(VAL(auxilio$)): LOCATE 6, 12: PRINT USING "²#####,²"; CVI(maxi12$);
          IF devolver = true THEN devolver = false: sit% = 10 ELSE sit% = 14
          GOSUB Guiar
     CASE 14
          auxilio$ = "32501"
          DO WHILE ABS(VAL(auxilio$)) > 32500: auxilio$ = EntraDatoNumero$(6, 21, 6, STR$(CVI(mini22$)), 1): LOOP
          RSET mini22$ = MKI$(VAL(auxilio$)): LOCATE 6, 21: PRINT USING "²#####,²"; CVI(mini22$);
          IF devolver = true THEN devolver = false: sit% = 12 ELSE sit% = 16
          GOSUB Guiar
     CASE 16
          auxilio$ = "32501"
          DO WHILE ABS(VAL(auxilio$)) > 32500: auxilio$ = EntraDatoNumero$(6, 30, 6, STR$(CVI(maxi22$)), 1): LOOP
          RSET maxi22$ = MKI$(VAL(auxilio$)): LOCATE 6, 30: PRINT USING "²#####,²"; CVI(maxi22$);
          IF devolver = true THEN devolver = false: sit% = 14 ELSE sit% = 17
          GOSUB Guiar
     CASE 17
          IF opcion% = 2 THEN
               devolver = false: LOCATE 6, 40: PRINT "Paga I.C.S.V.M.(s/n)?            ": IF iv$ = " " THEN auxilio$ = "N" ELSE auxilio$ = "S"
               auxilio$ = MeteDatoLetra(6, 62, 1, auxilio$): auxilio$ = UCASE$(auxilio$)
               COLOR colors(7, colorpref), colors(4, colorpref)
               IF auxilio$ = "S" OR auxilio$ = "N" THEN
                    IF devolver = true THEN devolver = false: sit% = 16 ELSE sit% = 18
                    IF auxilio$ = "S" THEN RSET iv$ = "*" ELSE RSET iv$ = " "
                    GOSUB Guiar
               ELSE
                    sit% = 17
               END IF
               LOCATE 6, 40: IF iv$ = "*" THEN PRINT "Producto Que Paga I.C.S.V.M." ELSE PRINT "Producto Exento:Es Un Medicamento"
          ELSE
               IF devolver = true THEN devolver = false: sit% = 16 ELSE sit% = 18
               GOSUB Guiar
          END IF
     CASE 18
          auxilio$ = "10000000"
          DO WHILE ABS(VAL(auxilio$)) > 9999999 OR VAL(auxilio$) < CVD(cos12$): auxilio$ = EntraDatoNumero$(9, 7, 12, STR$((FIX(CVD(pv12$) * 100)) / 100), 0): LOOP
          RSET pv12$ = MKD$(VAL(auxilio$)): LOCATE 9, 6: PRINT USING "########,.##"; CVD(pv12$)
          IF devolver = true THEN sit% = 17 ELSE sit% = 20
          GOSUB Guiar
     CASE 20
          auxilio$ = "10000000"
          DO WHILE ABS(VAL(auxilio$)) > 9999999 OR VAL(auxilio$) < CVD(cos22$): auxilio$ = EntraDatoNumero$(9, 27, 12, STR$((FIX(CVD(pv22$) * 100)) / 100), 0): LOOP
          RSET pv22$ = MKD$(VAL(auxilio$)): LOCATE 9, 26: PRINT USING "########,.##"; CVD(pv22$)
          IF devolver = true THEN devolver = false: sit% = 18 ELSE sit% = 22
          GOSUB Guiar
     CASE 22
          auxilio$ = "10000000"
          DO WHILE ABS(VAL(auxilio$)) > 9999999 OR VAL(auxilio$) < CVD(cos32$): auxilio$ = EntraDatoNumero$(9, 47, 12, STR$((FIX(CVD(pv32$) * 100)) / 100), 0): LOOP
          RSET pv32$ = MKD$(VAL(auxilio$)): LOCATE 9, 46: PRINT USING "########,.##"; CVD(pv32$)
          IF devolver = true THEN devolver = false: sit% = 20 ELSE sit% = 24
          GOSUB Guiar
     CASE 24
          auxilio$ = "10000000"
          DO WHILE ABS(VAL(auxilio$)) > 9999999 OR VAL(auxilio$) >= CVD(pv32$): auxilio$ = EntraDatoNumero$(9, 65, 12, STR$((FIX(CVD(pv42$) * 100)) / 100), 0): LOOP
          RSET pv42$ = MKD$(VAL(auxilio$)): LOCATE 9, 65: PRINT USING "########,.##"; CVD(pv42$)
          IF devolver = true THEN devolver = false: sit% = 22 ELSE sit% = 25
          GOSUB Guiar
     CASE 25
          auxilio$ = "32501"
          DO WHILE ABS(VAL(auxilio$)) > 32500: auxilio$ = EntraDatoNumero$(11, 9, 10, STR$(CVI(exi12$)), 1): LOOP
          IF CVD(pv12$) = 0 THEN auxilio$ = "0"
          IF VAL(auxilio$) <> 0 THEN auxilio$ = "0": Center 25, "UN ARTICULO NUEVO NO DEBE SER CREADO CON EXISTENCIA PREVIA": BEEP
          RSET exi12$ = MKI$(VAL(auxilio$)): LOCATE 11, 10: PRINT USING "#########,"; CVI(exi12$);
          IF devolver = true THEN devolver = false: sit% = 24 ELSE sit% = 26
          GOSUB Guiar
     CASE 26
          auxilio$ = "32501"
          DO WHILE ABS(VAL(auxilio$)) > 32500: auxilio$ = EntraDatoNumero$(11, 29, 10, STR$(CVI(exi22$)), 1): LOOP
          IF CVD(pv22$) = 0 THEN auxilio$ = "0"
          IF VAL(auxilio$) <> 0 THEN auxilio$ = "0": Center 25, "UN ARTICULO NUEVO NO DEBE SER CREADO CON EXISTENCIA PREVIA": BEEP
          RSET exi22$ = MKI$(VAL(auxilio$)): LOCATE 11, 30: PRINT USING "#########,"; CVI(exi22$);
          IF devolver = true THEN devolver = false: sit% = 25 ELSE sit% = 27
          GOSUB Guiar
     CASE 27
          auxilio$ = "32501"
          IF CVD(pv32$) = 0 THEN auxilio$ = "0"
          DO WHILE ABS(VAL(auxilio$)) > 32500: auxilio$ = EntraDatoNumero$(11, 49, 10, STR$(CVI(exi32$)), 1): LOOP
          IF VAL(auxilio$) <> 0 THEN auxilio$ = "0": Center 25, "UN ARTICULO NUEVO NO DEBE SER CREADO CON EXISTENCIA PREVIA": BEEP
          RSET exi32$ = MKI$(VAL(auxilio$)): LOCATE 11, 50: PRINT USING "#########,"; CVI(exi32$);
          IF devolver = true THEN devolver = false: sit% = 26 ELSE sit% = 28
          GOSUB Guiar
     CASE 28
          auxilio$ = "32501"
          DO WHILE ABS(VAL(auxilio$)) > 32500: auxilio$ = EntraDatoNumero$(11, 67, 10, STR$(CVI(exi42$)), 1): LOOP
          IF CVD(pv42$) = 0 THEN auxilio$ = "0"
          IF VAL(auxilio$) <> 0 THEN auxilio$ = "0": Center 25, "UN ARTICULO NUEVO NO DEBE SER CREADO CON EXISTENCIA PREVIA": BEEP
          RSET exi42$ = MKI$(VAL(auxilio$)): LOCATE 11, 68: PRINT USING "#########,"; CVI(exi42$);
          IF devolver = true THEN devolver = false: sit% = 27 ELSE sit% = 31
          GOSUB Guiar
     CASE 31
          auxilio$ = "10000000"
          DO WHILE ABS(VAL(auxilio$)) > 9999999 OR VAL(auxilio$) > CVD(pv12$): auxilio$ = EntraDatoNumero$(13, 7, 12, STR$((FIX(CVD(cos12$) * 100)) / 100), 0): LOOP
          RSET cos12$ = MKD$(VAL(auxilio$)): LOCATE 13, 8: PRINT USING "########,.##"; CVD(cos12$)
          IF devolver = true THEN devolver = false: sit% = 28 ELSE sit% = 32
          GOSUB Guiar
     CASE 32
          auxilio$ = "10000000"
          DO WHILE ABS(VAL(auxilio$)) > 9999999 OR VAL(auxilio$) > CVD(pv22$): auxilio$ = EntraDatoNumero$(13, 27, 12, STR$((FIX(CVD(cos22$) * 100)) / 100), 0): LOOP
          RSET cos22$ = MKD$(VAL(auxilio$)): LOCATE 13, 28: PRINT USING "########,.##"; CVD(cos22$)
          IF devolver = true THEN devolver = false: sit% = 31 ELSE sit% = 33
          GOSUB Guiar
     CASE 33
          auxilio$ = "10000000"
          DO WHILE ABS(VAL(auxilio$)) > 9999999 OR VAL(auxilio$) > CVD(pv32$): auxilio$ = EntraDatoNumero$(13, 47, 12, STR$((FIX(CVD(cos32$) * 100)) / 100), 0): LOOP
          RSET cos32$ = MKD$(VAL(auxilio$)): LOCATE 13, 48: PRINT USING "########,.##"; CVD(cos32$)
          IF devolver = true THEN devolver = false: sit% = 32 ELSE sit% = 34
          GOSUB Guiar
     CASE 34
          auxilio$ = "10000000": Center 25, "[F1]Ayuda [F6]Ir PVP [F7]Ir Oferta [F9]Salir y Salvar [F10]Salir SIN Salvar"
          DO WHILE ABS(VAL(auxilio$)) > 9999999 OR VAL(auxilio$) > CVD(pv42$): auxilio$ = EntraDatoNumero$(13, 65, 12, STR$((FIX(CVD(cos42$) * 100)) / 100), 0): LOOP
          RSET cos42$ = MKD$(VAL(auxilio$)): LOCATE 13, 68: PRINT USING "########,.##"; CVD(cos42$)
          IF devolver = true THEN devolver = false: sit% = 33 ELSE sit% = 36
          GOSUB Guiar
     CASE 36
          IF ven11$ = "A" OR ven11$ = "B" OR ven11$ = "C" THEN
               IF ven11$ = "A" THEN auxilio$ = "10/" + MID$(STR$(CVI(ven12$)), 2, 4)
               IF ven11$ = "B" THEN auxilio$ = "11/" + MID$(STR$(CVI(ven12$)), 2, 4)
               IF ven11$ = "C" THEN auxilio$ = "12/" + MID$(STR$(CVI(ven12$)), 2, 4)
          ELSE
               IF ven11$ <> " " THEN auxilio$ = "0" + ven11$ + "/" + MID$(STR$(CVI(ven12$)), 2, 4) ELSE auxilio$ = "  /    "
          END IF
          Center 25, "Atenci¢n: SOLO EMPLEE EL FORMATO ²mm/aaaa²"
          DO
          auxilio$ = MeteDatoLetra(15, 10, 7, auxilio$): IF auxilio$ = "  /    " OR auxilio$ = "       " THEN RSET ven11$ = " ": RSET ven12$ = "  ": EXIT DO
          IF VAL(MID$(auxilio$, 1, 2)) > 0 AND VAL(MID$(auxilio$, 1, 2)) < 13 THEN
               IF VAL(MID$(auxilio$, 4, 4)) > 1990 AND VAL(MID$(auxilio$, 4, 4)) < 2050 THEN
                    auxi% = VAL(MID$(auxilio$, 1, 2))
                         SELECT CASE auxi%
                              CASE 1: RSET ven11$ = "1"
                              CASE 2: RSET ven11$ = "2"
                              CASE 3: RSET ven11$ = "3"
                              CASE 4: RSET ven11$ = "4"
                              CASE 5: RSET ven11$ = "5"
                              CASE 6: RSET ven11$ = "6"
                              CASE 7: RSET ven11$ = "7"
                              CASE 8: RSET ven11$ = "8"
                              CASE 9: RSET ven11$ = "9"
                              CASE 10: RSET ven11$ = "A"
                              CASE 11: RSET ven11$ = "B"
                              CASE 12: RSET ven11$ = "C"
                         END SELECT: RSET ven12$ = MKI$(VAL(MID$(auxilio$, 4, 4))): EXIT DO
                    END IF
               END IF
          LOOP
          IF devolver = true THEN devolver = false: sit% = 34 ELSE sit% = 37
          GOSUB Guiar
     CASE 37
          IF ven21$ = "A" OR ven21$ = "B" OR ven21$ = "C" THEN
               IF ven21$ = "A" THEN auxilio$ = "10/" + MID$(STR$(CVI(ven22$)), 2, 4)
               IF ven21$ = "B" THEN auxilio$ = "11/" + MID$(STR$(CVI(ven22$)), 2, 4)
               IF ven21$ = "C" THEN auxilio$ = "12/" + MID$(STR$(CVI(ven22$)), 2, 4)
          ELSE
               IF ven21$ <> " " THEN auxilio$ = "0" + ven21$ + "/" + MID$(STR$(CVI(ven22$)), 2, 4) ELSE auxilio$ = "  /    "
          END IF
          Center 25, "Atenci¢n: SOLO EMPLEE EL FORMATO ²mm/aaaa²"
          DO
          auxilio$ = MeteDatoLetra(15, 30, 7, auxilio$): IF auxilio$ = "  /    " OR auxilio$ = "       " THEN RSET ven21$ = " ": RSET ven22$ = "  ": EXIT DO
          IF VAL(MID$(auxilio$, 1, 2)) > 0 AND VAL(MID$(auxilio$, 1, 2)) < 13 THEN
               IF VAL(MID$(auxilio$, 4, 4)) > 1990 AND VAL(MID$(auxilio$, 4, 4)) < 2050 THEN
                    auxi% = VAL(MID$(auxilio$, 1, 2))
                         SELECT CASE auxi%
                              CASE 1: RSET ven21$ = "1"
                              CASE 2: RSET ven21$ = "2"
                              CASE 3: RSET ven21$ = "3"
                              CASE 4: RSET ven21$ = "4"
                              CASE 5: RSET ven21$ = "5"
                              CASE 6: RSET ven21$ = "6"
                              CASE 7: RSET ven21$ = "7"
                              CASE 8: RSET ven21$ = "8"
                              CASE 9: RSET ven21$ = "9"
                              CASE 10: RSET ven21$ = "A"
                              CASE 11: RSET ven21$ = "B"
                              CASE 12: RSET ven21$ = "C"
                         END SELECT: : RSET ven22$ = MKI$(VAL(MID$(auxilio$, 4, 4))): EXIT DO
                    END IF
               END IF
          LOOP
          IF devolver = true THEN devolver = false: sit% = 36 ELSE sit% = 38
          GOSUB Guiar
     CASE 38
          IF ven31$ = "A" OR ven31$ = "B" OR ven31$ = "C" THEN
               IF ven31$ = "A" THEN auxilio$ = "10/" + MID$(STR$(CVI(ven32$)), 2, 4)
               IF ven31$ = "B" THEN auxilio$ = "11/" + MID$(STR$(CVI(ven32$)), 2, 4)
               IF ven31$ = "C" THEN auxilio$ = "12/" + MID$(STR$(CVI(ven32$)), 2, 4)
          ELSE
               IF ven31$ <> " " THEN auxilio$ = "0" + ven31$ + "/" + MID$(STR$(CVI(ven32$)), 2, 4) ELSE auxilio$ = "  /    "
          END IF
          Center 25, "Atenci¢n: SOLO EMPLEE EL FORMATO ²mm/aaaa²"
          DO
          auxilio$ = MeteDatoLetra(15, 50, 7, auxilio$): IF auxilio$ = "  /    " OR auxilio$ = "       " THEN RSET ven31$ = " ": RSET ven32$ = "  ": EXIT DO
          IF VAL(MID$(auxilio$, 1, 2)) > 0 AND VAL(MID$(auxilio$, 1, 2)) < 13 THEN
               IF VAL(MID$(auxilio$, 4, 4)) > 1990 AND VAL(MID$(auxilio$, 4, 4)) < 2050 THEN
                    auxi% = VAL(MID$(auxilio$, 1, 2))
                         SELECT CASE auxi%
                              CASE 1: RSET ven31$ = "1"
                              CASE 2: RSET ven31$ = "2"
                              CASE 3: RSET ven31$ = "3"
                              CASE 4: RSET ven31$ = "4"
                              CASE 5: RSET ven31$ = "5"
                              CASE 6: RSET ven31$ = "6"
                              CASE 7: RSET ven31$ = "7"
                              CASE 8: RSET ven31$ = "8"
                              CASE 9: RSET ven31$ = "9"
                              CASE 10: RSET ven31$ = "A"
                              CASE 11: RSET ven31$ = "B"
                              CASE 12: RSET ven31$ = "C"
                         END SELECT: RSET ven32$ = MKI$(VAL(MID$(auxilio$, 4, 4))): EXIT DO
                    END IF
               END IF
          LOOP
          IF devolver = true THEN devolver = false: sit% = 37 ELSE sit% = 39
          GOSUB Guiar
     CASE 39
          IF ven41$ = "A" OR ven41$ = "B" OR ven41$ = "C" THEN
               IF ven41$ = "A" THEN auxilio$ = "10/" + MID$(STR$(CVI(ven42$)), 2, 4)
               IF ven41$ = "B" THEN auxilio$ = "11/" + MID$(STR$(CVI(ven42$)), 2, 4)
               IF ven41$ = "C" THEN auxilio$ = "12/" + MID$(STR$(CVI(ven42$)), 2, 4)
          ELSE
               IF ven41$ <> " " THEN auxilio$ = "0" + ven41$ + "/" + MID$(STR$(CVI(ven12$)), 2, 4) ELSE auxilio$ = "  /    "
          END IF
          Center 25, "Atenci¢n: SOLO EMPLEE EL FORMATO ²mm/aaaa²"
          DO
          auxilio$ = MeteDatoLetra(15, 68, 7, auxilio$): IF auxilio$ = "  /    " OR auxilio$ = "       " THEN RSET ven41$ = " ": RSET ven42$ = "  ": EXIT DO
          IF VAL(MID$(auxilio$, 1, 2)) > 0 AND VAL(MID$(auxilio$, 1, 2)) < 13 THEN
               IF VAL(MID$(auxilio$, 4, 4)) > 1990 AND VAL(MID$(auxilio$, 4, 4)) < 2050 THEN
                    auxi% = VAL(MID$(auxilio$, 1, 2))
                         SELECT CASE auxi%
                              CASE 1: RSET ven41$ = "1"
                              CASE 2: RSET ven41$ = "2"
                              CASE 3: RSET ven41$ = "3"
                              CASE 4: RSET ven41$ = "4"
                              CASE 5: RSET ven41$ = "5"
                              CASE 6: RSET ven41$ = "6"
                              CASE 7: RSET ven41$ = "7"
                              CASE 8: RSET ven41$ = "8"
                              CASE 9: RSET ven41$ = "9"
                              CASE 10: RSET ven41$ = "A"
                              CASE 11: RSET ven41$ = "B"
                              CASE 12: RSET ven41$ = "C"
                         END SELECT: RSET ven42$ = MKI$(VAL(MID$(auxilio$, 4, 4))): EXIT DO
                    END IF
               END IF
          LOOP
          IF devolver = true THEN devolver = false: sit% = 38 ELSE sit% = 40
          gr$ = auxilio$: GOSUB Guiar
     CASE 40
          Center 25, "[F1]Ayuda [F6]Ir PVP [F7]Ir Oferta [F9]Salir y Salvar [F10]Salir SIN Salvar"
          auxilio$ = MeteDatoLetra(18, 3, 1, of12$): RSET of12$ = auxilio$
          COLOR colors(7, colorpref), colors(4, colorpref): IF of12$ = "@" THEN LOCATE 18, 6: PRINT Prov1 ELSE LOCATE 18, 6: PRINT SPACE$(15)
          IF of12$ = " " OR of12$ = "@" THEN
               IF devolver = true THEN devolver = false: sit% = 39 ELSE sit% = 42
               GOSUB Guiar
          ELSE
               sit% = 40
          END IF
     CASE 42
          auxilio$ = MeteDatoLetra(18, 23, 1, of22$): RSET of22$ = auxilio$
          COLOR colors(7, colorpref), colors(4, colorpref): IF of22$ = "#" THEN LOCATE 18, 26: PRINT Prov2 ELSE LOCATE 18, 26: PRINT SPACE$(15)
          IF of22$ = " " OR of22$ = "#" THEN
               IF devolver = true THEN devolver = false: sit% = 40 ELSE sit% = 44
               GOSUB Guiar
          ELSE
               sit% = 42
          END IF
     CASE 44
          auxilio$ = MeteDatoLetra(18, 43, 1, of32$): RSET of32$ = auxilio$
          COLOR colors(7, colorpref), colors(4, colorpref): IF of32$ = "$" THEN LOCATE 18, 46: PRINT Prov3 ELSE LOCATE 18, 46: PRINT SPACE$(15)
          IF of32$ = " " OR of32$ = "$" THEN
               IF devolver = true THEN devolver = false: sit% = 42 ELSE sit% = 46
               GOSUB Guiar
          ELSE
               sit% = 44
          END IF
     CASE 46
          auxilio$ = MeteDatoLetra(20, 18, 1, re$): RSET re$ = auxilio$
          IF auxilio$ = " " OR auxilio$ = "*" THEN
               IF devolver = true THEN devolver = false: sit% = 44 ELSE sit% = 48
               GOSUB Guiar
          ELSE
               sit% = 46
          END IF
     CASE 48
          auxilio$ = MeteDatoLetra(20, 32, 1, medi2$): RSET medi2$ = auxilio$
          IF medi2$ = " " OR medi2$ = "*" THEN
               IF devolver = true THEN devolver = false: sit% = 46 ELSE sit% = 50
               GOSUB Guiar
          ELSE
               sit% = 48
          END IF
     CASE 50
          auxilio$ = MeteDatoLetra(20, 47, 1, misc2$): RSET misc2$ = auxilio$
          IF misc2$ = " " OR misc2$ = "*" THEN
               IF devolver = true THEN devolver = false: sit% = 48 ELSE sit% = 52
               GOSUB Guiar
          ELSE
               sit% = 50
          END IF
     CASE 52
          Center 25, "[F1]Ayuda  [F6]Ir A PVP  [F7]Ir A Oferta  [F9]Salir y Salvar": COLOR colors(4, colorpref), colors(7, colorpref): BorraBox 5, 63, 21, 79: GOSUB ImpriOfer
          auxilio$ = MeteDatoLetra(20, 59, 1, quim2$): RSET quim2$ = auxilio$
          IF quim2$ = " " OR quim2$ = "*" THEN
               IF devolver = true THEN devolver = false: sit% = 50 ELSE sit% = 54
               GOSUB Guiar
          ELSE
               sit% = 52
          END IF
    CASE 54
          control% = Elije%(4, 5, 63, 22, 4, 1, 0)
          IF control% < 1 THEN
                sit% = 56: IF devolver = true THEN devolver = false: sit% = 52
          ELSE
               IF devolver = true THEN
                    devolver = false: sit% = 52
               ELSE
                    sit% = 56
               END IF
          END IF
          IF Fun(8) = "si" THEN
               IF Code(control%) = "001" THEN
                    GOSUB NoBorrar: sit% = 54
               ELSE
                    LOCATE sitio + 5, 64: PRINT "þ            ": donde% = control%
                    Contenido(control%) = "þ": control% = 4: GOSUB SortYname: sit% = 54
               END IF
               Fun(8) = "no": control% = -1
          END IF
          IF Fun(5) = "si" THEN
               IF Code(control%) = "001" THEN
                    GOSUB NoEdit: sit% = 54
               ELSE
                    Contenido(control%) = MeteDatoLetra(sitio + 5, 63, 15, (Contenido(control%)))
                    donde% = control%: control% = 4: GOSUB SortYname: sit% = 54
               END IF
               Fun(5) = "no": control% = -1
          END IF
          IF control% > 0 THEN RSET compo12$ = MKI$(VAL(Code(control%)))
          GOSUB BuscaComponente1
    CASE 56
          control% = Elije%(4, 5, 63, 22, 29, 2, 0)
          IF control% < 1 THEN
                sit% = 58
                IF devolver = true THEN
                    devolver = false: sit% = 54
                END IF
          ELSE
               IF devolver = true THEN
                    devolver = false: sit% = 54
               ELSE
                    sit% = 58
               END IF
          END IF
          IF Fun(8) = "si" THEN
               IF Code(control%) = "001" THEN
                    GOSUB NoBorrar: sit% = 56
               ELSE
                    LOCATE sitio + 5, 64: PRINT "þ            ": donde% = control%
                    Contenido(control%) = "þ": control% = 4: GOSUB SortYname: sit% = 56
               END IF
               Fun(8) = "no": control% = -1
          END IF
          IF Fun(5) = "si" THEN
               IF Code(control%) = "001" THEN
                    GOSUB NoEdit: sit% = 56
               ELSE
                    Contenido(control%) = MeteDatoLetra(sitio + 5, 63, 15, (Contenido(control%)))
                    donde% = control%: control% = 4: GOSUB SortYname: sit% = 56
               END IF
               Fun(5) = "no": control% = -1
          END IF
          IF control% > 0 THEN RSET compo22$ = MKI$(VAL(Code(control%)))
          GOSUB BuscaComponente2
    CASE 58
          control% = Elije%(4, 5, 63, 22, 53, 3, 0)
          IF control% < 1 THEN
                sit% = 60
                IF devolver = true THEN
                    devolver = false: sit% = 56
                END IF
          ELSE
               IF devolver = true THEN
                    devolver = false: sit% = 56
               ELSE
                    sit% = 60
               END IF
          END IF
          IF Fun(8) = "si" THEN
               IF Code(control%) = "001" THEN
                    GOSUB NoBorrar: sit% = 58
               ELSE
                    LOCATE sitio + 5, 64: PRINT "þ            ": donde% = control%
                    Contenido(control%) = "þ": control% = 4: GOSUB SortYname: sit% = 58
               END IF
               Fun(8) = "no": control% = -1
          END IF
          IF Fun(5) = "si" THEN
               IF Code(control%) = "001" THEN
                    GOSUB NoEdit: sit% = 58
               ELSE
                    Contenido(control%) = MeteDatoLetra(sitio + 5, 63, 15, (Contenido(control%)))
                    donde% = control%: control% = 4: GOSUB SortYname: sit% = 58
               END IF
               Fun(5) = "no": control% = -1
          END IF
          IF control% > 0 THEN RSET compo32$ = MKI$(VAL(Code(control%)))
          GOSUB BuscaComponente3
     CASE 60
          control% = Elije%(4, 5, 63, 23, 4, 4, 0)
          IF control% < 1 THEN
                sit% = 62
                IF devolver = true THEN
                    devolver = false: sit% = 58
                END IF
          ELSE
               IF devolver = true THEN
                    devolver = false: sit% = 58
               ELSE
                    sit% = 62
               END IF
          END IF
          IF Fun(8) = "si" THEN
               IF Code(control%) = "001" THEN
                    GOSUB NoBorrar: sit% = 60
               ELSE
                    LOCATE sitio + 5, 64: PRINT "þ            ": donde% = control%
                    Contenido(control%) = "þ": control% = 4: GOSUB SortYname: sit% = 60
               END IF
               Fun(8) = "no": control% = -1
          END IF
          IF Fun(5) = "si" THEN
               IF Code(control%) = "001" THEN
                    GOSUB NoEdit: sit% = 60
               ELSE
                    Contenido(control%) = MeteDatoLetra(sitio + 5, 63, 15, (Contenido(control%)))
                    donde% = control%: control% = 4: GOSUB SortYname: sit% = 60
               END IF
               Fun(5) = "no": control% = -1
          END IF
          IF control% > 0 THEN RSET compo42$ = MKI$(VAL(Code(control%)))
          GOSUB BuscaComponente4
    CASE 62
          control% = Elije%(4, 5, 63, 23, 29, 5, 0)
          IF control% < 1 THEN
                sit% = 64
                IF devolver = true THEN
                    devolver = false: sit% = 60
                END IF
          ELSE
               IF devolver = true THEN
                    devolver = false: sit% = 60
               ELSE
                    sit% = 64
               END IF
          END IF
          IF Fun(8) = "si" THEN
               IF Code(control%) = "001" THEN
                    GOSUB NoBorrar: sit% = 62
               ELSE
                    LOCATE sitio + 5, 64: PRINT "þ            ": donde% = control%
                    Contenido(control%) = "þ": control% = 4: GOSUB SortYname: sit% = 62
               END IF
               Fun(8) = "no": control% = -1
          END IF
          IF Fun(5) = "si" THEN
               IF Code(control%) = "001" THEN
                    GOSUB NoEdit: sit% = 64
               ELSE
                    Contenido(control%) = MeteDatoLetra(sitio + 5, 63, 15, (Contenido(control%)))
                    donde% = control%: control% = 4: GOSUB SortYname: sit% = 62
               END IF
               Fun(5) = "no": control% = -1
          END IF
          IF control% > 0 THEN RSET compo52$ = MKI$(VAL(Code(control%)))
          GOSUB BuscaComponente5
    CASE 64
          control% = Elije%(4, 5, 63, 23, 53, 6, 0)
          IF control% < 1 THEN
                sit% = 64
                IF devolver = true THEN
                    devolver = false: sit% = 62
                END IF
          ELSE
               IF devolver = true THEN
                    devolver = false: sit% = 62
               ELSE
                    sit% = 64
               END IF
          END IF
          IF Fun(8) = "si" THEN
               IF Code(control%) = "001" THEN
                    GOSUB NoBorrar: sit% = 64
               ELSE
                    LOCATE sitio + 5, 64: PRINT "þ            ": donde% = control%
                    Contenido(control%) = "þ": control% = 4: GOSUB SortYname: sit% = 64
               END IF
               Fun(8) = "no": control% = -1
          END IF
          IF Fun(5) = "si" THEN
               IF Code(control%) = "001" THEN
                    GOSUB NoEdit: sit% = 64
               ELSE
                    Contenido(control%) = MeteDatoLetra(sitio + 5, 63, 15, (Contenido(control%)))
                    donde% = control%: control% = 4: GOSUB SortYname: sit% = 64
               END IF
               Fun(5) = "no": control% = -1
          END IF
          IF control% > 0 THEN RSET compo62$ = MKI$(VAL(Code(control%)))
          GOSUB BuscaComponente6
     CASE 66
     IF Trim$(nomb2$) <> "" THEN
      salvar = true
      IF CVI(ubic2$) = 4 THEN
         salvar = Verify(13, "No ha escogido ubicaci¢n ¨desea continuar")
      END IF
      IF CVI(labo2$) = 1 THEN
         salvar = Verify(13, "No ha escogido laboratorio ¨desea continuar")
      END IF
      IF (CVI(compo12$) + CVI(compo22$) + CVI(compo32$) + CVI(compo42$) + CVI(compo32$) + CVI(compo62$)) = 6 THEN
         salvar = Verify(13, "No ha elegido componentes ¨desea continuar")
      END IF
      IF CVI(mini12$) = 0 AND CVI(mini22$) = 0 THEN
         salvar = Verify(13, "No ha establecido cantidades m¡nimas ¨desea continuar")
      END IF
      IF medi2$ = " " AND misc2$ = " " AND quim2$ = " " THEN
         salvar = Verify(13, "No ha clasificado el producto(medicina,misc.,quim.)¨continuar")
      END IF
      IF salvar = true THEN
          Center 25, "Salvando... Por Favor Espere SIN Pulsar Ninguna Tecla": final& = CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$)
          PUT #1, ElegMod%: salir = true
          OPEN "f:\kevinprg\kevindat\bitacora.dat" FOR RANDOM AS #17 LEN = 40: FIELD #17, 40 AS tod$: max% = LOF(17) / 40
          LSET tod$ = codigo2$ + MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4) + MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MKI$(final%) + "10" + cedula$
          REM max% = max% + 1: PUT #17, max%: CLOSE #17
         
          CLOSE #2: OPEN "f:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
          FIELD #2, 35 AS nomshort$, 2 AS num$
          max% = (LOF(2) / 37) + 1: central = max%
          LSET nomshort$ = nomb2$: LSET num$ = MKI$(ElegMod%)
          PUT #2, max%: GET #2, (central - 1): aux$ = nomb2$
          GOSUB AgregaIdx
         
          CLOSE #2: OPEN "f:\kevinprg\kevindat\codigo.idx" FOR RANDOM SHARED AS #2 LEN = 8
          FIELD #2, 6 AS nomshort$, 2 AS num$: max% = (LOF(2) / 8) + 1: central = max%
          LSET nomshort$ = codigo2$: LSET num$ = MKI$(ElegMod%)
          PUT #2, max%: GET #2, (central - 1): aux$ = codigo2$
          GOSUB AgregaIdx
         
          CLOSE #2: OPEN "f:\kevinprg\kevindat\lab.idx" FOR RANDOM SHARED AS #2 LEN = 10
          FIELD #2, 8 AS nomshort$, 2 AS num$: max% = (LOF(2) / 10) + 1: central = max%
          LSET nomshort$ = labo2$ + MID$(nomb2$, 1, 6): LSET num$ = MKI$(ElegMod%)
          PUT #2, max%: GET #2, (central - 1): aux$ = labo2$ + MID$(nomb2$, 1, 6)
          GOSUB AgregaIdx
         
          IF (CVI(compo12$) + CVI(compo22$) + CVI(compo32$) + CVI(compo42$) + CVI(compo32$) + CVI(compo62$)) > 6 THEN
            CLOSE #2: OPEN "f:\kevinprg\kevindat\compos.idx" FOR RANDOM SHARED AS #2 LEN = 4
            FIELD #2, 2 AS nomshort$, 2 AS num$
            IF CVI(compo12$) > 1 THEN
               max% = (LOF(2) / 4) + 1: central = max%
               LSET nomshort$ = compo12$: LSET num$ = MKI$(ElegMod%)
               PUT #2, max%: GET #2, (central - 1): aux$ = compo12$
               GOSUB AgregaIdx
            END IF
            IF CVI(compo22$) > 1 THEN
               max% = (LOF(2) / 4) + 1: central = max%
               LSET nomshort$ = compo22$: LSET num$ = MKI$(ElegMod%)
               PUT #2, max%: GET #2, (central - 1): aux$ = compo22$
               GOSUB AgregaIdx
            END IF
            IF CVI(compo32$) > 1 THEN
               max% = (LOF(2) / 4) + 1: central = max%
               LSET nomshort$ = compo32$: LSET num$ = MKI$(ElegMod%)
               PUT #2, max%: GET #2, (central - 1): aux$ = compo32$
               GOSUB AgregaIdx
            END IF
            IF CVI(compo42$) > 1 THEN
               max% = (LOF(2) / 4) + 1: central = max%
               LSET nomshort$ = compo42$: LSET num$ = MKI$(ElegMod%)
               PUT #2, max%: GET #2, (central - 1): aux$ = compo42$
               GOSUB AgregaIdx
            END IF
            IF CVI(compo52$) > 1 THEN
               max% = (LOF(2) / 4) + 1: central = max%
               LSET nomshort$ = compo52$: LSET num$ = MKI$(ElegMod%)
               PUT #2, max%: GET #2, (central - 1): aux$ = compo52$
               GOSUB AgregaIdx
            END IF
            IF CVI(compo62$) > 1 THEN
               max% = (LOF(2) / 4) + 1: central = max%
               LSET nomshort$ = compo62$: LSET num$ = MKI$(ElegMod%)
               PUT #2, max%: GET #2, (central - 1): aux$ = compo62$
               GOSUB AgregaIdx
            END IF
          END IF
         
          CLOSE #2: OPEN "f:\kevinprg\kevindat\ubica.idx" FOR RANDOM SHARED AS #2 LEN = 10
          FIELD #2, 8 AS nomshort$, 2 AS num$: max% = (LOF(2) / 10) + 1: central = max%
          LSET nomshort$ = ubic2$ + MID$(nomb2$, 1, 6): LSET num$ = MKI$(ElegMod%)
          PUT #2, max%: GET #2, (central - 1): aux$ = ubic2$ + MID$(nomb2$, 1, 6)
          GOSUB AgregaIdx
          CLOSE #2
         
          IF CVI(ubic2$) = 1 THEN
            CLOSE #3: OPEN "f:\kevinprg\kevindat\saldos.dat" FOR RANDOM SHARED AS #3 LEN = 45
            FIELD #3, 6 AS codipsi$, 35 AS nombpsi$, 2 AS saldopsi$, 2 AS sitiopsi$: maxpsi% = (LOF(3) / 45) + 1
            LSET codipsi$ = codigo2$: LSET nombpsi$ = nomb2$: LSET saldopsi$ = MKI$(0): LSET sitiopsi$ = MKI$(ElegMod%): PUT #3, maxpsi%: CLOSE #3
          END IF
         
          UNLOCK #1, ElegMod%
      ELSE
         sit% = 2
      END IF
     ELSE
      res% = Verify(13, "No se puede guardar sin nombre.¨"): sit% = 2
     END IF
     CASE ELSE: salir = true
    END SELECT
LOOP
CLOSE : EXIT SUB
'*****************************************************************************

Buscaubicacion:
CLOSE #2: OPEN "F:\kevinprg\kevindat\ordubica.dat" FOR RANDOM SHARED AS #2 LEN = 15: FIELD #2, 15 AS descripcion$
GET #2, CVI(ubic2$): COLOR colors(7, colorpref), colors(4, colorpref): LOCATE 3, 58: PRINT USING "²###²"; CVI(ubic2$); : PRINT descripcion$
RETURN

Buscalaboratorio:
CLOSE #2: OPEN "F:\kevinprg\kevindat\ordlab.dat" FOR RANDOM SHARED AS #2 LEN = 15: FIELD #2, 15 AS descripcion$
GET #2, CVI(labo2$): COLOR colors(7, colorpref), colors(4, colorpref): LOCATE 4, 52: PRINT USING "²###²"; CVI(labo2$); : PRINT descripcion$
RETURN

SortYname:
Center 25, " Sorteando Los Campos De Menor A Mayor. Espere Por Favor . . . "
IF control% = 1 THEN
     CLOSE #2: OPEN "F:\kevinprg\kevindat\ubica.dat" FOR RANDOM SHARED AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$: selecc% = 199
     CLOSE #3: OPEN "F:\kevinprg\kevindat\ordubica.dat" FOR RANDOM SHARED AS #3 LEN = 15: FIELD #3, 15 AS deskrip$
END IF
IF control% = 2 THEN
     CLOSE #2: OPEN "F:\kevinprg\kevindat\lab.dat" FOR RANDOM SHARED AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$: selecc% = 999
     CLOSE #3: OPEN "F:\kevinprg\kevindat\ordlab.dat" FOR RANDOM SHARED AS #3 LEN = 15: FIELD #3, 15 AS deskrip$
END IF
IF control% = 4 THEN
     CLOSE #2: OPEN "F:\kevinprg\kevindat\componen.dat" FOR RANDOM SHARED AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$: selecc% = 999
     CLOSE #3: OPEN "F:\kevinprg\kevindat\ordcompo.dat" FOR RANDOM SHARED AS #3 LEN = 15: FIELD #3, 15 AS deskrip$
END IF
FOR k = 1 TO selecc%
     GET #2, k
     IF VAL(codigo$) = VAL(Code(donde%)) THEN
          LOCK #2, k :
          RSET codigo$ = Code(donde%): RSET descripcion$ = Contenido(donde%)
          PUT #2, k
          UNLOCK #2, k
          EXIT FOR
     END IF
NEXT k: LOCK #2, 1 TO selecc%
N = selecc%: Salto = N
WHILE Salto > 1
     Salto = Salto / 2
     DO
          Sw = 1
          FOR J = 1 TO N - Salto
               I = J + Salto
               GET #2, J: ao1$ = descripcion$: ao3$ = codigo$
               GET #2, I: ao2$ = descripcion$: ao4$ = codigo$
               IF ao1$ > ao2$ THEN
                    RSET descripcion$ = ao2$: RSET codigo$ = ao4$: PUT #2, J
                    RSET descripcion$ = ao1$: RSET codigo$ = ao3$: PUT #2, I
                    Sw = 0
               END IF
          NEXT J
     LOOP UNTIL Sw = 1
WEND
FOR k = 1 TO selecc%
     GET #2, k
     RSET deskrip$ = descripcion$: PUT #3, VAL(codigo$)
NEXT k
CLOSE #2: CLOSE #3
GOSUB BuscaComponente1: GOSUB BuscaComponente2: GOSUB BuscaComponente3: GOSUB BuscaComponente4: GOSUB BuscaComponente5: GOSUB BuscaComponente6: GOSUB Buscalaboratorio: GOSUB Buscaubicacion
RETURN

BuscaComponente1:
GOSUB LeeDatComp: GET #2, CVI(compo12$): COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 22, 3: PRINT USING "²###²"; CVI(compo12$); : PRINT descripcion$: CLOSE #2
RETURN

BuscaComponente2:
GOSUB LeeDatComp: GET #2, CVI(compo22$): COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 22, 28: PRINT USING "²###²"; CVI(compo22$); : PRINT descripcion$: CLOSE #2
RETURN

BuscaComponente3:
GOSUB LeeDatComp: GET #2, CVI(compo32$): COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 22, 52: PRINT USING "²###²"; CVI(compo32$); : PRINT descripcion$: CLOSE #2
RETURN

BuscaComponente4:
GOSUB LeeDatComp: GET #2, CVI(compo42$): COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 23, 3: PRINT USING "²###²"; CVI(compo42$); : PRINT descripcion$: CLOSE #2
RETURN

BuscaComponente5:
GOSUB LeeDatComp: GET #2, CVI(compo52$): COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 23, 28: PRINT USING "²###²"; CVI(compo52$); : PRINT descripcion$: CLOSE #2
RETURN

BuscaComponente6:
GOSUB LeeDatComp: GET #2, CVI(compo62$): COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 23, 52: PRINT USING "²###²"; CVI(compo62$); : PRINT descripcion$: CLOSE #2
RETURN

NoEdit:
Center 25, "Este Campo NO ES EDITABLE... Pulse Cualquier Tecla Para  Continuar": DO WHILE INKEY$ = "": LOOP
RETURN

NoBorrar:
Center 25, "Este Campo NO SE PUEDE BORRAR... Presione Cualquier Tecla Para Continuar": DO WHILE INKEY$ = "": LOOP
RETURN

Guiar:
     IF Fun(4) = "si" THEN Fun(4) = "no": sit% = 2
     IF Fun(6) = "si" THEN Fun(6) = "no": sit% = 18
     IF Fun(7) = "si" THEN Fun(7) = "no": sit% = 40
     IF Fun(9) = "si" THEN Fun(9) = "no": sit% = 66
     IF Fun(10) = "si" THEN sit% = 66: devolver = true
RETURN

ImpriOfer:
COLOR colors(4, colorpref), colors(7, colorpref)
LOCATE 5, 79: PRINT "³": LOCATE 6, 79: PRINT "³": LOCATE 7, 79: PRINT "³"
LOCATE 8, 63: PRINT "FERTAÍÍÍÍÍÍÍÍÍÍÍµ"; : LOCATE 9, 63: PRINT "VP"; SPACE$(14); "³";
LOCATE 10, 63: PRINT STRING$(16, "Í"); "µ"; : LOCATE 11, 63: PRINT "xist"; SPACE$(12); "³";
LOCATE 12, 63: PRINT STRING$(16, "Í"); "µ"; : LOCATE 13, 63: PRINT "to"; SPACE$(14); "³";
LOCATE 14, 63: PRINT STRING$(16, "Í"); "µ"; : LOCATE 15, 63: PRINT "ence"
LOCATE 16, 63: PRINT STRING$(16, "Í"); "µ";
LOCATE 15, 79: PRINT "³": LOCATE 17, 79: PRINT "³": LOCATE 18, 79: PRINT "³"
LOCATE 19, 79: PRINT "³": LOCATE 20, 79: PRINT "³": LOCATE 21, 79: PRINT "³"
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE 9, 65: PRINT USING "²########,.##²"; CVD(pv42$); : LOCATE 15, 68: PRINT gr$
LOCATE 11, 67: PRINT USING "²######,.##²"; CVI(exi42$); : LOCATE 13, 65: PRINT USING "²########,.##²"; CVD(cos42$);
RETURN

OrdenaOferta:
     RSET nomshort$ = MID$(nomb2$, 1, 6): RSET num$ = MKI$(ElegMod%): max% = max% + 1: PUT #2, max%: central = max% - 1: GET #2, central
     DO WHILE MID$(nomb2$, 1, 6) < nomshort$
          LOCK #2, central
          GET #2, (central - 1): PUT #2, central
          central = central - 1: LOCATE 1, 1: PRINT central
          IF central < 1 THEN
               central = 1: UNLOCK #2, central
               EXIT DO
          END IF
          UNLOCK #2, central
     LOOP
     LOCATE 1, 1: PRINT central; "k="; k: GET #2, central: IF MID$(nomb2$, 1, 6) > nomshort$ THEN central = central + 1
     LOCK #2, central : UNLOCK #2, central
     LSET nomshort$ = MID$(nomb2$, 1, 6): LSET num$ = MKI$(ElegMod%): PUT #2, central
RETURN

LeeDatComp:
CLOSE #2: OPEN "F:\kevinprg\kevindat\ordcompo.dat" FOR RANDOM SHARED AS #2 LEN = 15: FIELD #2, 15 AS descripcion$
RETURN

AgregaIdx:
   IF aux$ < nomshort$ THEN
        DO WHILE aux$ < nomshort$
           IF central = 1 THEN
              LSET nomshort$ = aux$: LSET num$ = MKI$(ElegMod%)
              PUT #2, central: CLOSE : EXIT DO
           END IF
           LOCK #2, central
           GET #2, (central - 1): PUT #2, central: UNLOCK #2, central
           central = central - 1
           IF central <= 1 THEN central = 1: EXIT DO
        LOOP
        LOCK #2, central
        GET #2, central
        IF aux$ > nomshort$ THEN
             UNLOCK #2, central
             central = central + 1
             LOCK #2, central
        END IF
        LSET nomshort$ = aux$: LSET num$ = MKI$(ElegMod%): PUT #2, central
        UNLOCK #2, central
   END IF
RETURN
END SUB

SUB NuevoArt
COLOR colors(5, colorpref), colors(4, colorpref): CLS : Box 1, 1, 3, 80
Center 1, "Adicion de Un Articulo": salir = false
DIM nuevocod AS STRING * 6
DO WHILE NOT salir
     Center 25, "[F1]Ayuda  [F9]Salir  [Esc]Salir"
     LOCATE 2, 2: PRINT "Introduzca E.F., P.N., A., P.B. o P.C. a usar :"
     aux$ = MeteDatoLetra(2, 52, 6, "      ")
     IF aux$ = "NOTADE" THEN nuevocod = ""
     IF Fun(9) = "si" THEN Fun(9) = "no": guia = 7: EXIT SUB
     IF devolver = true THEN devolver = false: guia = 7: EXIT SUB
     IF LEN(aux$) > 0 THEN salir = true: nuevocod = aux$
LOOP
salir = false
DO WHILE NOT salir
     Center 25, "[F1]Ayuda  [F9 y Esc]Salir"
     LOCATE 2, 2: PRINT "Introduzca SOLO el nombre del art¡culo nuevo   :"
     aux$ = MeteDatoLetra(2, 52, 15, SPACE$(15))
     IF Fun(9) = "si" THEN Fun(9) = "no": guia = 7: EXIT SUB
     IF devolver = true THEN devolver = false: guia = 7: EXIT SUB
     IF LEN(aux$) > 0 THEN
         salir = true: aux2$ = "": aux$ = Trim(aux$)
         FOR k = 1 TO LEN(aux$)
            IF MID$(aux$, k, 1) = " " THEN
               aux2$ = UCASE$(aux2$): EXIT FOR
            ELSE
               aux2$ = aux2$ + MID$(aux$, k, 1)
            END IF
         NEXT k
     END IF
LOOP

CLOSE : OPEN "f:\kevinprg\kevindat\codigold.idx" FOR RANDOM SHARED AS #1 LEN = 8
FIELD #1, 6 AS codigo$, 2 AS mini1$: max1% = LOF(1) / 8: seguir = true
FOR k = 1 TO max1%: GET #1, k
   IF codigo$ = nuevocod THEN seguir = false: EXIT FOR
NEXT k: CLOSE
OPEN "f:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
OPEN "f:\kevinprg\kevindat\codigo.idx" FOR RANDOM SHARED AS #2 LEN = 8
FIELD #2, 6 AS nomshort$, 2 AS num$: max2% = LOF(2) / 8
encontrado = false: primero = 1: ultimo = max2%
IF max2% = 0 THEN encontrado = true
WHILE NOT encontrado AND primero <= ultimo
     central = (primero + ultimo) / 2: GET #2, central
     IF nuevocod = nomshort$ THEN
          encontrado = true
     ELSE
          IF nomshort$ > nuevocod THEN
              ultimo = central - 1
          ELSE
              primero = central + 1
          END IF
     END IF
WEND: salir = false
IF encontrado = true THEN seguir = false
IF seguir = false THEN
     Center 5, " Este Codigo Ya Esta En Uso, Utilize Otro Diferente": Espera
ELSE
      CLOSE : OPEN "f:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
      FIELD #2, 35 AS nomshort$, 2 AS num$: max2% = LOF(2) / 37: busqueda = 1
      l = 7: avz = false: aux2$ = Inexpertos(aux$, -1)
      DO WHILE avz = false
         encontrado = false: primero = 1: ultimo = max2%
         WHILE NOT encontrado AND primero <= ultimo
              central = (primero + ultimo) / 2: GET #2, central
              IF aux2$ = MID$(nomshort$, 1, LEN(aux2$)) THEN
                   encontrado = true
              ELSE
                   IF MID$(nomshort$, 1, LEN(aux2$)) > aux2$ THEN
                        ultimo = central - 1
                   ELSE
                        primero = central + 1
                   END IF
              END IF
         WEND: salir = false
         DO WHILE salir = false
              central = central - 1: IF central < 1 THEN salir = true: central = 1
              GET #2, central
              IF aux2$ <> MID$(nomshort$, 1, LEN(aux2$)) THEN
               salir = true: central = central + 1
              END IF
         LOOP
         IF encontrado = true THEN
            FOR k = central TO max2%
               GET #2, k
               IF MID$(nomshort$, 1, LEN(aux2$)) = aux2$ THEN
                  IF l < 25 THEN l = l + 1: Center l, nomshort$ ELSE EXIT FOR
               ELSE
                  EXIT FOR
               END IF
            NEXT k
         END IF
         aux2$ = Inexpertos(aux$, 0)
         IF aux2$ = "" THEN avz = true
      LOOP
      'preg usuar
      res% = true
      IF l > 7 THEN
         res% = Verify(4, "Hay otros art¡culos con el mismo nombre ¨Desea continuar")
      END IF
      IF res% = true THEN CALL ModProd(nuevocod, 2, UCASE$(aux$)) ELSE EXIT SUB
END IF
END SUB

SUB SalvaEstado
OPEN "f:\kevinprg\kevindat\turno.dat" FOR OUTPUT AS #7
PRINT #7, USING "#"; colorpref
PRINT #7, turno
PRINT #7, USING "##"; dia1
PRINT #7, USING "##"; mes1
PRINT #7, USING "####"; ano1
CLOSE #7
OPEN "f:\kevinprg\kevindat\kaos.dat" FOR OUTPUT AS #2
PRINT #2, sonido
PRINT #2, impuesto
PRINT #2, Nombfarma
PRINT #2, Direccion
PRINT #2, rif
PRINT #2, nit
PRINT #2, telefono
PRINT #2, contri
PRINT #2, contra
PRINT #2, Stock
PRINT #2, Blanq
PRINT #2, LargoH
PRINT #2, TipoL
PRINT #2, Mensaje1
PRINT #2, Mensaje2
PRINT #2, Mensaje3
PRINT #2, Redondeo
PRINT #2, Desc
PRINT #2, DescMax
PRINT #2, Exactitud
PRINT #2, Prov1
PRINT #2, Prov2
PRINT #2, Prov3
PRINT #2, negativo
CLOSE
END SUB

FUNCTION Substituye$ (cad1 AS STRING, cad2 AS STRING, cad3 AS STRING)
lug% = INSTR(1, cad1, cad2)
IF lug% > 0 THEN
   IF LEN(cad1) = LEN(cad2) THEN
      Substituye$ = cad3
   ELSE
      Substituye$ = MID$(cad1, 1, (lug% - 1)) + cad3 + MID$(cad1, (lug% + LEN(cad2)), (LEN(cad1) - lug% - LEN(cad2) + 1))
   END IF
ELSE
   Substituye$ = cad1
END IF
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

