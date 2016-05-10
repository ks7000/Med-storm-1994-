DEFINT A-Z
DECLARE SUB HelpMate (opcion%, contexto$)
DECLARE SUB PopUp (ka%, la%, ma%, na%, pa%, ra%, ta%, sa%)
DECLARE SUB ShutUp ()
DECLARE SUB Espera ()
DECLARE SUB ModProd ()
DECLARE SUB LeeEstado ()
DECLARE SUB SalvaEstado ()
DECLARE SUB Center (row, text$)
DECLARE SUB Box (Row1%, Col1%, Row2%, Col%)
DECLARE SUB BorraBox (Row1%, Col1%, Row2%, Col%)
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$)
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Escoge% (fila%, edo%, fila2%, Prod$)
DECLARE FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%)
DECLARE FUNCTION Trim$ (x$)
COMMON SHARED guia, colorpref
DIM SHARED turno AS STRING * 1, devolver
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED SONIDO  AS STRING * 2, impuesto AS STRING * 6, Nombfarma AS STRING * 25, Direccion AS STRING * 30, rif     AS STRING * 25, nit      AS STRING * 25, telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2
DIM SHARED Fun(17) AS STRING * 2, Salir, Turna, terminar, sitio
DIM SHARED Contenido(15) AS STRING * 15, Code(15) AS STRING * 3
DIM SHARED Nombra(5) AS STRING * 35, Pvp1a(5) AS STRING * 8, Pvp2a(5) AS STRING * 8, Pvp3a(5) AS STRING * 8, Pvp4a(5) AS STRING * 8, Numa(5) AS STRING * 2, Observa(5) AS STRING * 20, Exis1a(5) AS STRING * 2, Exis2a(5) AS STRING * 2, Exis3a(5) AS  _
STRING * 2, Exis4a(5) AS STRING * 2
DIM SHARED Cant1a(5) AS STRING * 2, Cant2a(5) AS STRING * 2, Cant3a(5) AS STRING * 2, Cant4a(5) AS STRING * 2
CONST true = -1: CONST false = NOT true: colorpref = 1: guia = 1
ON ERROR GOTO ErrorTrap
CALL LeeEstado
DO
     CALL ModProd
     IF devolver = true OR Fun(9) = "si" THEN EXIT DO
LOOP
CLS : SYSTEM
END

ErrorTrap:
    SELECT CASE ERR
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

SUB BorraBox (Row1%, Col1%, Row2%, Col2%) STATIC
BoxWidth = Col2% - Col1% + 1: FOR a = Row1% TO Row2%: LOCATE a, Col1%: PRINT SPACE$(BoxWidth); : NEXT a
END SUB

SUB Box (Row1%, Col1%, Row2%, Col2%) STATIC
BoxWidth = Col2% - Col1% + 1: LOCATE Row1%, Col1%: PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿"; : FOR a = Row1% + 1 TO Row2% - 1: LOCATE a, Col1%: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2%, Col1%: PRINT "À"; STRING$(BoxWidth - 2,  _
"Ä"); "Ù";
END SUB

SUB Center (row, text$)
IF row = 25 THEN COLOR colors(5, colorpref), colors(4, colorpref): LOCATE 25, 1: PRINT SPACE$(80);
LOCATE row, 41 - LEN(text$) / 2: PRINT text$;
END SUB

FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%)
COLOR colors(7, colorpref), colors(4, colorpref): CALL Box(fila%, columna%, fila% + 16, columna% + 16): CALL Center(25, "[F1] Ayuda [Enter]Tomar Opcion [A-Z]Ir a... [Esc]Menu Anterior"): COLOR colors(4, colorpref), colors(7, colorpref): CLOSE #5
Center 25, "[F5]Edita/Adiciona Linea [F8]Borrar Linea [F9]Salir/Salvar [Enter]Tomar Opcion"
SELECT CASE estesi%
     CASE 1
          LOCATE fila%, columna% + 2: PRINT "Ubicacion"
          OPEN "f:\kevinprg\kevindat\ubica.dat" FOR RANDOM SHARED AS #5 LEN = 18: FIELD #5, 3 AS codigo$, 15 AS nomb$
          FOR k = 1 TO 15: GET #5, k: Contenido(k) = nomb$: Code(k) = codigo$: NEXT k: selecc% = 199
     CASE 2
          LOCATE fila%, columna% + 1: PRINT "Laboratorio"
          OPEN "f:\kevinprg\kevindat\lab.dat" FOR RANDOM SHARED AS #5 LEN = 18: FIELD #5, 3 AS codigo$, 15 AS nomb$
          FOR k = 1 TO 15: GET #5, k: Contenido(k) = nomb$: Code(k) = codigo$: NEXT k: selecc% = 999
     CASE 4
          LOCATE fila%, columna% + 1: PRINT "Componente"; ingrediente%
          OPEN "f:\kevinprg\kevindat\componen.dat" FOR RANDOM SHARED AS #5 LEN = 18: FIELD #5, 3 AS codigo$, 15 AS nomb$
          FOR k = 1 TO 15: GET #5, k: Contenido(k) = nomb$: Code(k) = codigo$: NEXT k: selecc% = 999
     CASE 5
          LOCATE fila%, columna% + 1: PRINT " Proveedor "
          OPEN "f:\kevinprg\kevindat\lab.dat" FOR RANDOM SHARED AS #5 LEN = 18: FIELD #5, 3 AS codigo$, 15 AS nomb$
          FOR k = 1 TO 15: GET #5, k: Contenido(k) = nomb$: Code(k) = codigo$: NEXT k: selecc% = 199
     CASE 6
          LOCATE fila%, columna% + 1: PRINT " Banco     "
          OPEN "f:\kevinprg\kevindat\bank.dat" FOR RANDOM SHARED AS #5 LEN = 18: FIELD #5, 3 AS codigo$, 15 AS nomb$
          FOR k = 1 TO 15: GET #5, k: Contenido(k) = nomb$: Code(k) = codigo$: NEXT k: selecc% = 199
     CASE 7
          LOCATE fila%, columna% + 1: PRINT " Cliente   "
          OPEN "f:\kevinprg\kevindat\clientes.dat" FOR RANDOM SHARED AS #5 LEN = 38: FIELD #5, 3 AS codigo$, 35 AS nomb$
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
            CASE CHR$(0) + CHR$(61): terminar = true: Fun(3) = "si": resal% = 0
            CASE CHR$(0) + CHR$(63): terminar = true: Fun(5) = "si": sitio = resal%
            CASE CHR$(0) + CHR$(66): terminar = true: Fun(8) = "si": sitio = resal%
            CASE CHR$(0) + CHR$(67): terminar = true: Fun(9) = "si": resal% = 0
            CASE CHR$(0) + CHR$(68): terminar = true: Fun(10) = "si": resal% = 0
            CASE CHR$(0) + CHR$(133): terminar = true: Fun(11) = "si": resal% = 0
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
               CASE CHR$(0) + CHR$(71): llevasitio% = 1
               CASE CHR$(0) + CHR$(79): llevasitio% = largo%
               CASE " ", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "+", "-", "."
                    GOSUB EntraNumero: IF llevasitio% > largo% THEN GOSUB Sumar2: EXIT DO
               CASE ELSE: IF SONIDO = "si" THEN BEEP
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
EntraDatoNumero$ = eleccion$: IF SONIDO = "si" THEN BEEP
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
     'TIMER ON: Center 25, "Utilize la teclas de direccion y enter para escoger": COLOR colors(4, colorpref), colors(7, colorpref)
     Kbd$ = "": WHILE Kbd$ = "": Kbd$ = INKEY$: LOCATE 2, 65: PRINT TIME$: WEND: Kbd$ = UCASE$(Kbd$)
     SELECT CASE Kbd$
          CASE CHR$(0) + "H": GOSUB Arriba
          CASE CHR$(0) + "P": GOSUB Abajo
          CASE CHR$(0) + CHR$(73): GOSUB Apgup
          CASE CHR$(0) + CHR$(81): GOSUB Apgdn
          CASE CHR$(13): EXIT DO                                         ' TIMER OFF:
          CASE CHR$(27): resal% = 0: EXIT DO                             ' TIMER OFF:
          CASE CHR$(0) + CHR$(67): resal% = 0: Fun(3) = "si": EXIT DO    ' TIMER OFF:
          CASE ELSE: IF SONIDO = "SI" THEN BEEP
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
          IF SONIDO = "si" THEN BEEP
          GET #2, up%: IF Prod$ <> MID$(nomshort$, 1, LEN(Prod$)) THEN up% = up% + 1
          GOSUB LeeCinco
     END IF
RETURN
Abajo:
     resal% = resal% + 1
     IF resal% > selecc% THEN
          resal% = selecc%: up% = up% + 1
          IF up% > max2% THEN up% = max2%
          IF SONIDO = "si" THEN BEEP
          GET #2, (up% + 4): IF Prod$ <> MID$(nomshort$, 1, LEN(Prod$)) THEN up% = up% - 1
          GOSUB LeeCinco
     END IF
RETURN
Apgup:
up% = up% - 4: IF up% < 1 THEN up% = 1: IF SONIDO = "si" THEN BEEP
DO
GET #2, up%: IF Prod$ = MID$(nomshort$, 1, LEN(Prod$)) THEN EXIT DO ELSE up% = up% + 1
LOOP
resal% = 1: GOSUB LeeCinco
RETURN
Apgdn:
up% = up% + 4: IF up% > max2% THEN up% = max2%: IF SONIDO = "si" THEN BEEP
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
          WEND: Salir = false
          DO WHILE Salir = false
               central = central - 1: IF central < 1 THEN Salir = true: central = 1
               GET #2, central
               IF Prod$ <> MID$(nomshort$, 1, LEN(Prod$)) THEN Salir = true: central = central + 1
          LOOP
RETURN
Parche2:
IF encontrado = false THEN LOCATE fila% + 5, 5: PRINT STRING$(5, "Ä"); "²No se encontr¢²"; STRING$(7, "Ä"); : Espera: Escoge% = 0: EXIT FUNCTION
Salir = false: selecc% = 0: up% = central
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

SUB LeeEstado
CLOSE : OPEN "f:\kevinprg\kevindat\kaos.dat" FOR INPUT SHARED AS #2
INPUT #2, SONIDO: INPUT #2, impuesto: INPUT #2, Nombfarma: INPUT #2, Direccion: INPUT #2, rif: INPUT #2, nit: INPUT #2, telefono: INPUT #2, contri: INPUT #2, contra: INPUT #2, Stock: INPUT #2, Blanq
INPUT #2, LargoH: INPUT #2, TipoL: INPUT #2, Mensaje1: INPUT #2, Mensaje2: INPUT #2, Mensaje3: INPUT #2, Redondeo: INPUT #2, Desc: INPUT #2, DescMax: INPUT #2, Exactitud: INPUT #2, Prov1: INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo: CLOSE
END SUB

FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$)
LOCATE fila%, columna%: PRINT CHR$(178) + caden$: LOCATE fila%, columna% + largo% + 1: PRINT CHR$(178): REDIM eleccion(0 TO largo% + 1) AS STRING * 1: tr$ = "0": llevasitio% = 1
FOR k = 1 TO largo%: eleccion(k) = MID$(caden$, k, 1): NEXT k: LOCATE fila%, (columna% + llevasitio%): COLOR colors(7, colorpref) + 16, colors(4, colorpref): IF eleccion(1 + VAL(tr$)) = " " THEN PRINT "_";  ELSE PRINT eleccion(1 + VAL(tr$))
DO
      COLOR colors(7, colorpref), colors(4, colorpref)
      llave$ = "": WHILE llave$ = "": llave$ = INKEY$: LOCATE 2, 65: PRINT TIME$: WEND
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
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna% + 1: PRINT eleccion$
MeteDatoLetra$ = eleccion$: IF SONIDO = "si" THEN BEEP
EXIT FUNCTION
Sumar:
FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna%: PRINT "²"; eleccion$; "²"
RETURN
END FUNCTION

SUB ModProd
DIM ao1 AS STRING * 15, ao2 AS STRING * 15, ao3 AS STRING * 3, ao4 AS STRING * 3
COLOR colors(7, colorpref), colors(4, colorpref): CLS : FOR k = 1 TO 12: Fun(k) = "no": NEXT k
Box 1, 1, 3, 80: IF guia = 1 THEN Center 1, "Modificacion de Un Articulo" ELSE Center 1, "Reparacion  de  Un Articulo"
ElegMod% = 0: devolver = false

IF guia = 1 THEN
     ElegMod% = Escoge%(5, 1, 2, "")
     IF Fun(1) = "si" THEN
            PopUp 12, 10, 2, 60, 112, 1, 0, 0
            LOCATE 13, 11: PRINT "Introduzca el nombre a buscar:"
            auxi$ = MeteDatoLetra$(13, 50, 8, SPACE$(8))
            auxi$ = Trim$(auxi$)
            CALL ShutUp
            CALL HelpMate(1, auxi$): Fun(1) = "no"
     END IF
     IF Fun(10) = "si" THEN
          Fun(10) = "no"
          OPEN "f:\kevinprg\kevindat\arregla.dat" FOR RANDOM AS #10 LEN = 2
          FIELD #10, 2 AS campo$
          GET #10, 1
          ElegMod% = CVI(campo$)
          LOCATE 1, 1
          PRINT CVI(campo$)
          LSET campo$ = MKI$(CVI(campo$) + 1)
          PUT #10, 1
          CLOSE
          guia = 2
          OPEN "f:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #10 LEN = 37
          FIELD #10, 35 AS nonshort$, 2 AS sit$
          max% = LOF(10) / 37
          LOCATE 2, 1
          PRINT ElegMod%
          IF ElegMod% > max% THEN ElegMod% = 1
          GET #10, ElegMod%
          ElegMod% = CVI(sit$)
          LOCATE 3, 1: PRINT CVI(sit$)
          CLOSE
     END IF
ELSE
          OPEN "f:\kevinprg\kevindat\arregla.dat" FOR RANDOM AS #10 LEN = 2
          FIELD #10, 2 AS campo$
          GET #10, 1
          ElegMod% = CVI(campo$)
          LOCATE 1, 1
          PRINT CVI(campo$)
          LSET campo$ = MKI$(CVI(campo$) + 1)
          PUT #10, 1
          CLOSE
          guia = 2
          OPEN "f:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #10 LEN = 37
          FIELD #10, 35 AS nonmshort$, 2 AS sit$
          max% = LOF(10) / 37
          LOCATE 2, 1
          PRINT ElegMod%
          IF ElegMod% > max% THEN ElegMod% = 1
          GET #10, ElegMod%
          ElegMod% = CVI(sit$)
          LOCATE 3, 1: PRINT CVI(sit$)
          CLOSE
END IF
IF devolver = true THEN EXIT SUB
IF Fun(9) = "si" THEN EXIT SUB
IF ElegMod% <= 0 THEN EXIT SUB
Box 11, 5, 20, 75: COLOR colors(4, colorpref), colors(7, colorpref): Center 12, "Introduzca su Clave de Vendedor.": COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 14, 16: PRINT "Clave Corta del Vendedor:"; ElegMod%: Busca$ = MeteDatoLetra(14, 41, 3, "   ")
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
GET #1, ElegMod%: LOCK #1, ElegMod%
Indexar1$ = nomb2$ + MKI$(ElegMod%): Indexar2$ = ubic2$ + MID$(nomb2$, 1, 6) + MKI$(ElegMod%)
'LOCATE 1, 1: PRINT SPACE$(70): LOCATE 1, 1: PRINT labo2$; "="; CVI(labo2$): Espera: Indexar3$ = labo2$ + MID$(nomb2$, 1, 6) + MKI$(ElegMod%): Indexar4$ = of12$ + of22$ + of32$ + MKI$(ElegMod%): Indexar5% = CVI(compo12$) + CVI(compo22$) + CVI(compo32$) + CVI(compo42$) + CVI(compo52$) + CVI(compo62$)
COLOR colors(7, colorpref), colors(4, colorpref)
inicio& = CVI(exi12$): inicio& = inicio& + CVI(exi22$): inicio& = inicio& + CVI(exi32$): inicio& = inicio& + CVI(exi42$)
FOR k = 4 TO 12: LOCATE k, 1: PRINT SPACE$(80); : NEXT k
COLOR colors(4, colorpref), colors(7, colorpref): Box 2, 2, 24, 79
Center 25, "[F3]Compos. [F4]Ir a Nombre [F6]Ir PVP [F7]Ir Oferta [F9]Salir y Salvar"
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
LOCATE 21, 3: PRINT "Componentes:                   Cantidad solicitada :";
COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 3, 10: PRINT "²"; nomb2$; "²": LOCATE 4, 18: PRINT "²"; obser2$; "²": LOCATE 1, 60: PRINT "Codigo:"; codigo2$;
LOCATE 6, 3: PRINT USING "²#####,²"; CVI(mini12$);
LOCATE 6, 12: PRINT USING "²#####,²"; CVI(maxi12$);
LOCATE 6, 21: PRINT USING "²#####,²"; CVI(mini22$);
LOCATE 6, 30: PRINT USING "²#####,²"; CVI(maxi22$);
LOCATE 6, 40: IF iv$ = "*" THEN PRINT "Paga I.C.S.V.M." ELSE PRINT "Producto Exento"
LOCATE 9, 7: PRINT USING "²########,.##²"; CVD(pv12$); : LOCATE 9, 27: PRINT USING "²########,.##²"; CVD(pv22$);
LOCATE 9, 47: PRINT USING "²########,.##²"; CVD(pv32$); : LOCATE 11, 9: PRINT USING "²#########,²"; CVI(exi12$);
LOCATE 11, 29: PRINT USING "²#########,²"; CVI(exi22$); : LOCATE 11, 49: PRINT USING "²#########,²"; CVI(exi32$);
LOCATE 13, 7: PRINT USING "²########,.##²"; CVD(cos12$); : LOCATE 13, 27: PRINT USING "²########,.##²"; CVD(cos22$);
LOCATE 13, 47: PRINT USING "²########,.##²"; CVD(cos32$); : GOSUB Buscaubicacion: GOSUB ImpriOfer: GOSUB Buscalaboratorio
IF ven11$ = "A" OR ven11$ = "B" OR ven11$ = "C" THEN
     IF ven11$ = "A" THEN gr$ = "10/" + MID$(STR$(CVI(ven12$)), 2, 4)
     IF ven11$ = "B" THEN gr$ = "11/" + MID$(STR$(CVI(ven12$)), 2, 4)
     IF ven11$ = "C" THEN gr$ = "12/" + MID$(STR$(CVI(ven12$)), 2, 4)
ELSE
     IF ven11$ <> " " THEN gr$ = "0" + ven11$ + "/" + MID$(STR$(CVI(ven12$)), 2, 4) ELSE gr$ = "  /    "
END IF: LOCATE 15, 10: gr$ = "²" + gr$ + "²": PRINT gr$: gr$ = ""
IF ven21$ = "A" OR ven21$ = "B" OR ven21$ = "C" THEN
     IF ven21$ = "A" THEN gr$ = "10/" + MID$(STR$(CVI(ven22$)), 2, 4)
     IF ven21$ = "B" THEN gr$ = "11/" + MID$(STR$(CVI(ven22$)), 2, 4)
     IF ven21$ = "C" THEN gr$ = "12/" + MID$(STR$(CVI(ven22$)), 2, 4)
ELSE
     IF ven21$ <> " " THEN gr$ = "0" + ven21$ + "/" + MID$(STR$(CVI(ven22$)), 2, 4) ELSE gr$ = "  /    "
END IF: LOCATE 15, 30: gr$ = "²" + gr$ + "²": PRINT gr$: gr$ = ""
IF ven31$ = "A" OR ven31$ = "B" OR ven31$ = "C" THEN
     IF ven31$ = "A" THEN gr$ = "10/" + MID$(STR$(CVI(ven32$)), 2, 4)
     IF ven31$ = "B" THEN gr$ = "11/" + MID$(STR$(CVI(ven32$)), 2, 4)
     IF ven31$ = "C" THEN gr$ = "12/" + MID$(STR$(CVI(ven32$)), 2, 4)
ELSE
     IF ven31$ <> " " THEN gr$ = "0" + ven31$ + "/" + MID$(STR$(CVI(ven32$)), 2, 4) ELSE gr$ = "  /    "
END IF: LOCATE 15, 50: gr$ = "²" + gr$ + "²": PRINT gr$: gr$ = ""
IF ven41$ = "A" OR ven41$ = "B" OR ven41$ = "C" THEN
     IF ven41$ = "A" THEN gr$ = "10/" + MID$(STR$(CVI(ven42$)), 2, 4)
     IF ven41$ = "B" THEN gr$ = "11/" + MID$(STR$(CVI(ven42$)), 2, 4)
     IF ven41$ = "C" THEN gr$ = "12/" + MID$(STR$(CVI(ven42$)), 2, 4)
ELSE
     IF ven41$ <> " " THEN gr$ = "0" + ven41$ + "/" + MID$(STR$(CVI(ven42$)), 2, 4) ELSE gr$ = "  /    "
END IF: LOCATE 15, 68: gr$ = "²" + gr$ + "²": PRINT gr$
LOCATE 20, 18: PRINT "²"; re2$; "²": LOCATE 20, 32: PRINT "²"; medi2$; "²"
LOCATE 20, 47: PRINT "²"; misc2$; "²": LOCATE 20, 59: PRINT "²"; quim2$; "²"
LOCATE 21, 55: PRINT "²"; : PRINT USING "##"; CVI(ped2$); : PRINT "²"
LOCATE 22, 3: PRINT "²"; : PRINT USING "##"; CVI(compo12$); : PRINT "²"
GOSUB BuscaComponente1: GOSUB BuscaComponente2: GOSUB BuscaComponente3
GOSUB BuscaComponente4: GOSUB BuscaComponente5: GOSUB BuscaComponente6
LOCATE 18, 3: PRINT "²"; of12$; "²"; : IF of12$ = "@" THEN PRINT Prov1 ELSE PRINT SPACE$(15)
LOCATE 18, 23: PRINT "²"; of22$; "²"; : IF of22$ = "#" THEN PRINT Prov2 ELSE PRINT SPACE$(15)
LOCATE 18, 43: PRINT "²"; of32$; "²"; : IF of32$ = "$" THEN PRINT Prov3 ELSE PRINT SPACE$(15)

Salir = false: sit% = 2
DO WHILE NOT Salir
    SELECT CASE sit%
     CASE 2
          Center 25, "[F3]Compos. [F4]Ir a Nombre [F6]Ir A PVP  [F7]Ir A Oferta  [F9]Salir y Salvar"
          COLOR colors(4, colorpref), colors(7, colorpref): BorraBox 5, 63, 21, 79: GOSUB ImpriOfer
          auxilio$ = MeteDatoLetra(3, 10, 35, nomb2$)
          RSET nomb2$ = auxilio$
          IF devolver = true THEN devolver = false: sit% = 2 ELSE sit% = 4: GOSUB Guiar
     CASE 4
          control% = Elije%(1, 5, 63, 3, 59, 0)
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
          GOSUB Guiar
          IF Fun(8) = "si" THEN
               Fun(8) = "no"
               IF VAL(Code(control%)) >= 1 AND VAL(Code(control%)) <= 5 THEN
                    GOSUB NoBorrar: sit% = 4: control% = -1
               ELSE
                    LOCATE sitio + 5, 64: PRINT "þ            ": Contenido(control%) = "þ            "
                    donde% = control%: control% = 1: GOSUB SortYname: sit% = 4: control% = -1
               END IF
          END IF
          IF Fun(5) = "si" THEN
               Fun(5) = "no"
                    IF VAL(Code(control%)) >= 1 AND VAL(Code(control%)) <= 5 THEN
                         GOSUB NoEdit: sit% = 4: control% = -1
                    ELSE
                         Contenido(control%) = MeteDatoLetra(sitio + 5, 63, 15, (Contenido(control%)))
                         donde% = control%: control% = 1: GOSUB SortYname: sit% = 4: control% = -1
                    END IF
          END IF
          IF control% > 0 THEN RSET ubic2$ = MKI$(VAL(Code(control%)))
          GOSUB Buscaubicacion
     CASE 6
          Center 25, "[F3]Compos. [F4]Ir a Nombre [F6]Ir A PVP  [F7]Ir A Oferta  [F9] Salir y Salvar "
          COLOR colors(4, colorpref), colors(7, colorpref): BorraBox 5, 63, 21, 79: GOSUB ImpriOfer
          auxilio$ = MeteDatoLetra(4, 18, 20, obser2$)
          RSET obser2$ = auxilio$
          IF devolver = true THEN devolver = false: sit% = 4 ELSE sit% = 8
          GOSUB Guiar
     CASE 8
          control% = Elije%(2, 5, 63, 4, 53, 0)
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
          GOSUB Guiar
          IF Fun(8) = "si" THEN
               Fun(8) = "no"
               IF VAL(Code(control%)) = 1 THEN
                    GOSUB NoBorrar: sit% = 8: control% = -1
               ELSE
                    LOCATE sitio + 5, 64: PRINT "þ            ": Contenido(control%) = "þ            "
                    donde% = control%: control% = 2: GOSUB SortYname: sit% = 8: control% = -1
               END IF
          END IF
          IF Fun(5) = "si" THEN
               Fun(5) = "no"
               IF VAL(Code(control%)) = 1 THEN
                    GOSUB NoEdit: sit% = 8: control% = -1
               ELSE
                    Contenido(control%) = MeteDatoLetra(sitio + 5, 63, 15, (Contenido(control%)))
                    donde% = control%: control% = 2: GOSUB SortYname: sit% = 8: control% = -1
               END IF
          END IF
          IF control% > 0 THEN RSET labo2$ = MKI$(VAL(Code(control%)))
          GOSUB Buscalaboratorio
     CASE 10
          Center 25, "[F3]Compos. [F4]Ir a Nombre [F6]Ir A PVP [F7]Ir A Oferta [F9]Salir y Salvar"
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
          LOCATE 6, 40: PRINT "Paga I.C.S.V.M.(s/n)?            ": IF iv$ = " " THEN auxilio$ = "N" ELSE auxilio$ = "S"
          auxilio$ = MeteDatoLetra(6, 62, 1, auxilio$): auxilio$ = UCASE$(auxilio$)
          COLOR colors(7, colorpref), colors(4, colorpref)
          IF auxilio$ = "S" OR auxilio$ = "N" THEN
               IF devolver = true THEN devolver = false: sit% = 16 ELSE sit% = 18
               IF auxilio$ = "S" THEN RSET iv$ = "*" ELSE RSET iv$ = " "
               GOSUB Guiar
          ELSE
               sit% = 17
          END IF
          LOCATE 6, 40: IF iv$ = "*" THEN PRINT "Paga I.C.S.V.M.                       " ELSE PRINT "Exento                           "
     CASE 18
          auxilio$ = "10000000"
          DO WHILE ABS(VAL(auxilio$)) > 9999999 OR VAL(auxilio$) < CVD(cos12$): auxilio$ = EntraDatoNumero$(9, 7, 12, STR$((FIX(CVD(pv12$) * 100)) / 100), 0): LOOP
          RSET pv12$ = MKD$(VAL(auxilio$)): LOCATE 9, 9: PRINT USING "########,.##"; CVD(pv12$)
          IF devolver = true THEN sit% = 17 ELSE sit% = 20
          GOSUB Guiar
     CASE 20
          auxilio$ = "10000000"
          DO WHILE ABS(VAL(auxilio$)) > 9999999 OR VAL(auxilio$) < CVD(cos22$): auxilio$ = EntraDatoNumero$(9, 27, 12, STR$((FIX(CVD(pv22$) * 100)) / 100), 0): LOOP
          RSET pv22$ = MKD$(VAL(auxilio$)): LOCATE 9, 29: PRINT USING "########,.##"; CVD(pv22$)
          IF devolver = true THEN devolver = false: sit% = 18 ELSE sit% = 22
          GOSUB Guiar
     CASE 22
          auxilio$ = "10000000"
          DO WHILE ABS(VAL(auxilio$)) > 9999999 OR VAL(auxilio$) < CVD(cos32$): auxilio$ = EntraDatoNumero$(9, 47, 12, STR$((FIX(CVD(pv32$) * 100)) / 100), 0): LOOP
          RSET pv32$ = MKD$(VAL(auxilio$)): LOCATE 9, 49: PRINT USING "########,.##"; CVD(pv32$)
          IF devolver = true THEN devolver = false: sit% = 20 ELSE sit% = 24
          GOSUB Guiar
     CASE 24
          auxilio$ = "10000000"
          DO WHILE ABS(VAL(auxilio$)) > 9999999 OR VAL(auxilio$) >= CVD(pv32$): auxilio$ = EntraDatoNumero$(9, 65, 12, STR$((FIX(CVD(pv42$) * 100)) / 100), 0): LOOP
          RSET pv42$ = MKD$(VAL(auxilio$)): LOCATE 9, 66: PRINT USING "########,.##"; CVD(pv42$)
          IF devolver = true THEN devolver = false: sit% = 22 ELSE sit% = 25
          GOSUB Guiar
     CASE 25
          auxilio$ = "32501"
          DO WHILE ABS(VAL(auxilio$)) > 32500: auxilio$ = EntraDatoNumero$(11, 9, 10, STR$(CVI(exi12$)), 1): LOOP
          IF CVD(pv12$) = 0 THEN auxilio$ = "0"
          RSET exi12$ = MKI$(VAL(auxilio$)): LOCATE 11, 10: PRINT USING "#########,"; CVI(exi12$);
          IF devolver = true THEN devolver = false: sit% = 24 ELSE sit% = 26
          GOSUB Guiar
     CASE 26
          auxilio$ = "32501"
          DO WHILE ABS(VAL(auxilio$)) > 32500: auxilio$ = EntraDatoNumero$(11, 29, 10, STR$(CVI(exi22$)), 1): LOOP
          IF CVD(pv22$) = 0 THEN auxilio$ = "0"
          RSET exi22$ = MKI$(VAL(auxilio$)): LOCATE 11, 30: PRINT USING "#########,"; CVI(exi22$);
          IF devolver = true THEN devolver = false: sit% = 25 ELSE sit% = 27
          GOSUB Guiar
     CASE 27
          auxilio$ = "32501"
          IF CVD(pv32$) = 0 THEN auxilio$ = "0"
          DO WHILE ABS(VAL(auxilio$)) > 32500: auxilio$ = EntraDatoNumero$(11, 49, 10, STR$(CVI(exi32$)), 1): LOOP
          RSET exi32$ = MKI$(VAL(auxilio$)): LOCATE 11, 50: PRINT USING "#########,"; CVI(exi32$);
          IF devolver = true THEN devolver = false: sit% = 26 ELSE sit% = 28
          GOSUB Guiar
     CASE 28
          auxilio$ = "32501"
          DO WHILE ABS(VAL(auxilio$)) > 32500: auxilio$ = EntraDatoNumero$(11, 67, 10, STR$(CVI(exi42$)), 1): LOOP
          IF CVD(pv42$) = 0 THEN auxilio$ = "0"
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
          auxilio$ = "10000000": Center 25, "[F3]Compos. [F4]Ir a Nombre [F6]Ir PVP [F7]Ir Oferta [F9]Salir y Salvar": COLOR colors(7, colorpref), colors(4, colorpref)
          DO WHILE ABS(VAL(auxilio$)) > 9999999 OR VAL(auxilio$) > CVD(pv42$): auxilio$ = EntraDatoNumero$(13, 65, 12, STR$((FIX(CVD(cos42$) * 100)) / 100), 0): LOOP
          RSET cos42$ = MKD$(VAL(auxilio$)): LOCATE 13, 66: PRINT USING "########,.##"; CVD(cos42$)
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
          Center 25, "Atenci¢n: SOLO EMPLEE EL FORMATO ²mm/aaaa²": COLOR colors(7, colorpref), colors(4, colorpref)
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
          Center 25, "Atenci¢n: SOLO EMPLEE EL FORMATO ²mm/aaaa²": COLOR colors(7, colorpref), colors(4, colorpref)
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
          Center 25, "Atenci¢n: SOLO EMPLEE EL FORMATO ²mm/aaaa²": COLOR colors(7, colorpref), colors(4, colorpref)
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
               IF ven41$ <> " " THEN auxilio$ = "0" + ven41$ + "/" + MID$(STR$(CVI(ven42$)), 2, 4) ELSE auxilio$ = "  /    "
          END IF
          Center 25, "Atenci¢n: SOLO EMPLEE EL FORMATO ²mm/aaaa²": COLOR colors(7, colorpref), colors(4, colorpref)
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
          Center 25, "[F3]Compos. [F4]Ir a Nombre [F6]Ir PVP [F7]Ir Oferta [F9]Salir y Salvar": COLOR colors(7, colorpref), colors(4, colorpref)
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
          auxilio$ = MeteDatoLetra(20, 18, 1, re2$)
          IF auxilio$ = " " OR auxilio$ = "*" THEN
               IF devolver = true THEN devolver = false: sit% = 44 ELSE sit% = 48
               RSET re2$ = auxilio$
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
          Center 25, "[F3]Compos. [F4]Ir a Nombre [F6]Ir A PVP [F7]Ir A Oferta [F9]Salir y Salvar": COLOR colors(4, colorpref), colors(7, colorpref): BorraBox 5, 63, 21, 79: GOSUB ImpriOfer
          auxilio$ = MeteDatoLetra(20, 59, 1, quim2$): RSET quim2$ = auxilio$
          IF quim2$ = " " OR quim2$ = "*" THEN
               IF devolver = true THEN devolver = false: sit% = 50 ELSE sit% = 54
               GOSUB Guiar
          ELSE
               sit% = 52
          END IF
    CASE 54
          control% = Elije%(4, 5, 63, 22, 4, 1)
          IF control% < 1 THEN
                sit% = 56: IF devolver = true THEN devolver = false: sit% = 52
          ELSE
               IF devolver = true THEN
                    devolver = false: sit% = 52
               ELSE
                    sit% = 56
               END IF
          END IF
          GOSUB Guiar
          IF Fun(8) = "si" THEN
               IF VAL(Code(control%)) = 1 THEN
                    GOSUB NoBorrar: sit% = 54
               ELSE
                    LOCATE sitio + 5, 64: PRINT "þ            ": Contenido(control%) = "þ            "
                    donde% = control%: control% = 4: GOSUB SortYname: sit% = 54: control = -1
               END IF
               Fun(8) = "no": control% = -1
          END IF
          IF Fun(5) = "si" THEN
               IF VAL(Code(control%)) = 1 THEN
                    GOSUB NoEdit: sit% = 54
               ELSE
                    LOCATE 1, 1: PRINT control%
                    Contenido(control%) = MeteDatoLetra(sitio + 5, 63, 15, (Contenido(control%)))
                    LOCATE 1, 1: PRINT control%, Code(control%)
                    donde% = control%: control% = 4: GOSUB SortYname: sit% = 54: control% = -1
               END IF
               Fun(5) = "no": control% = -1
          END IF
          IF control% > 0 THEN RSET compo12$ = MKI$(VAL(Code(control%)))
          GOSUB BuscaComponente1
    CASE 56
          control% = Elije%(4, 5, 63, 22, 29, 2)
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
          GOSUB Guiar
          IF Fun(8) = "si" THEN
               IF VAL(Code(control%)) = 1 THEN
                    GOSUB NoBorrar: sit% = 56
               ELSE
                    LOCATE sitio + 5, 64: PRINT "þ            "; : Contenido(control%) = "þ            "
                    donde% = control%: control% = 4: GOSUB SortYname: sit% = 56
               END IF
               Fun(8) = "no": control% = -1
          END IF
          IF Fun(5) = "si" THEN
               IF VAL(Code(control%)) = 1 THEN
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
          control% = Elije%(4, 5, 63, 22, 53, 3)
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
          GOSUB Guiar
          IF Fun(8) = "si" THEN
               IF VAL(Code(control%)) = 1 THEN
                    GOSUB NoBorrar: sit% = 58
               ELSE
                    LOCATE sitio + 5, 64: PRINT "þ            ": Contenido(control%) = "þ            "
                    donde% = control%: control% = 4: GOSUB SortYname: sit% = 58
               END IF
               Fun(8) = "no": control% = -1
          END IF
          IF Fun(5) = "si" THEN
               IF VAL(Code(control%)) = 1 THEN
                    GOSUB NoEdit: sit% = 58
               ELSE
                    Contenido(control%) = MeteDatoLetra(sitio + 5, 63, 15, (Contenido(control%)))
                    control% = 4: GOSUB SortYname: sit% = 58
               END IF
               Fun(5) = "no": control% = -1
          END IF
          IF control% > 0 THEN RSET compo32$ = MKI$(VAL(Code(control%)))
          GOSUB BuscaComponente3
     CASE 60
          control% = Elije%(4, 5, 63, 23, 4, 4)
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
          GOSUB Guiar
          IF Fun(8) = "si" THEN
               IF VAL(Code(control%)) = 1 THEN
                    GOSUB NoBorrar: sit% = 60
               ELSE
                    LOCATE sitio + 5, 64: PRINT "þ            ": Contenido(control%) = "þ            "
                    donde% = control%: control% = 4: GOSUB SortYname: sit% = 60
               END IF
               Fun(8) = "no": control% = -1
          END IF
          IF Fun(5) = "si" THEN
               IF VAL(Code(control%)) = 1 THEN
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
          control% = Elije%(4, 5, 63, 23, 29, 5)
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
          GOSUB Guiar
          IF Fun(8) = "si" THEN
               IF VAL(Code(control%)) = 1 THEN
                    GOSUB NoBorrar: sit% = 62
               ELSE
                    LOCATE sitio + 5, 64: PRINT "þ            ": Contenido(control%) = "þ            "
                    donde% = control%: control% = 4: GOSUB SortYname: sit% = 62
               END IF
               Fun(8) = "no": control% = -1
          END IF
          IF Fun(5) = "si" THEN
               IF VAL(Code(control%)) = 1 THEN
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
          control% = Elije%(4, 5, 63, 23, 53, 6)
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
          GOSUB Guiar
          IF Fun(8) = "si" THEN
               IF VAL(Code(control%)) = 1 THEN
                    GOSUB NoBorrar: sit% = 64
               ELSE
                    LOCATE sitio + 5, 64: PRINT "þ            ": Contenido(control%) = "þ            "
                    donde% = control%: control% = 4: GOSUB SortYname: sit% = 64
               END IF
               Fun(8) = "no": control% = -1
          END IF
          IF Fun(5) = "si" THEN
               IF VAL(Code(control%)) = 1 THEN
                    GOSUB NoEdit: sit% = 64
               ELSE
                    Contenido(control%) = MeteDatoLetra(sitio + 5, 63, 15, (Contenido(control%)))
                    control% = 4: GOSUB SortYname: sit% = 64
               END IF
               Fun(5) = "no": control% = -1
          END IF
          IF control% > 0 THEN RSET compo62$ = MKI$(VAL(Code(control%)))
          GOSUB BuscaComponente6
     CASE 66
          Center 25, "Salvando... Por Favor Espere SIN Pulsar Ninguna Tecla"
          final& = CVI(exi12$): final& = final& + CVI(exi22$): final& = final& + CVI(exi32$): final& = final& + CVI(exi42$)
'         LOCATE 1, 1: PRINT SPACE$(79): LOCATE 1, 1: PRINT final&; inicio&; final& - inicio&: Espera
          IF ABS(final& - inicio&) > 32500 THEN Box 5, 5, 20, 75: Center 10, "Usted ha cometido un grave error en cantidades:": Center 12, "NO SE SALVARAN LOS CAMBIOS": Espera: UNLOCK #1, ElegMod% : EXIT SUB
          'IF guia = 2 THEN RSET iv42$ = " "
          PUT #1, ElegMod%: Salir = true: saldo% = (final& - inicio&)
          OPEN "f:\kevinprg\kevindat\bitacora.dat" FOR RANDOM AS #17 LEN = 40: FIELD #17, 40 AS tod$: max% = LOF(17) / 40
          LSET tod$ = codigo2$ + MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4) + MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MKI$(saldo%) + "03" + cedula$
          max% = max% + 1: PUT #17, max%: CLOSE #17
          IF Indexar1$ <> (nomb2$ + MKI$(ElegMod%)) THEN
               CLOSE #2: OPEN "f:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
               FIELD #2, 35 AS nomshort$, 2 AS num$: max% = LOF(2) / 37
               encontrado = false: primero = 1: ultimo = max%
               WHILE NOT encontrado AND primero <= ultimo
                    central = (primero + ultimo) / 2: GET #2, central
                    IF Indexar1$ = (nomshort$ + num$) THEN
                         encontrado = true
                    ELSE
                         IF (nomshort$ + num$) > Indexar1$ THEN
                              ultimo = central - 1
                         ELSE
                              primero = central + 1
                         END IF
                    END IF
               WEND: inic% = central: GET #2, central
               IF nomb2$ > nomshort$ THEN
                    IF central = max% THEN LSET nomshort$ = nomb2$: LSET num$ = MKI$(ElegMod%): PUT #2, central: CLOSE : EXIT SUB
                    DO WHILE nomb2$ > nomshort$
                         LOCK #2, central
                         GET #2, (central + 1): PUT #2, central: UNLOCK #2, central
                         central = central + 1  ': LOCATE 1, 1: PRINT central
                         IF central >= max% THEN central = max%: EXIT DO
                    LOOP
                    LOCK #2, central
                    GET #2, central
                    IF nomb2$ < nomshort$ THEN
                         UNLOCK #2, central
                         central = central - 1
                         LOCK #2, central
                    END IF
                    LSET nomshort$ = nomb2$: LSET num$ = MKI$(ElegMod%): PUT #2, central
                    UNLOCK #2, central
               ELSE
                    DO WHILE nomb2$ < nomshort$
                         IF central = 1 THEN LSET nomshort$ = nomb2$: LSET num$ = MKI$(ElegMod%): PUT #2, central: CLOSE : EXIT SUB
                         LOCK #2, central
                         GET #2, (central - 1): PUT #2, central: UNLOCK #2, central
                         central = central - 1 ': LOCATE 1, 1: PRINT central
                         IF central <= 1 THEN central = 1: EXIT DO
                    LOOP
                    LOCK #2, central
                    GET #2, central
                    IF nomb2$ > nomshort$ THEN
                         UNLOCK #2, central
                         central = central + 1
                         LOCK #2, central
                    END IF
                    LSET nomshort$ = nomb2$: LSET num$ = MKI$(ElegMod%): PUT #2, central
                    UNLOCK #2, central
               END IF
          END IF
'               IF MID$(Indexar4$, 1, 1) = "@" THEN
'                    CLOSE #2: OPEN "f:\kevinprg\kevindat\oferta1.idx" FOR RANDOM SHARED AS #2 LEN = 8
'                    FIELD #2, 6 AS nomshort$, 2 AS num$: max% = LOF(2) / 8: IF max% > 0 THEN GOSUB OrdenOfe ELSE LSET nomshort$ = MID$(nomb2$, 1, 6): LSET num$ = MKI$(ElegMod%): PUT #2, 1
'               END IF
'               IF MID$(Indexar4$, 2, 1) = "#" THEN
'                    CLOSE #2: OPEN "f:\kevinprg\kevindat\oferta2.idx" FOR RANDOM SHARED AS #2 LEN = 8
'                    FIELD #2, 6 AS nomshort$, 2 AS num$: max% = LOF(2) / 8: IF max% > 0 THEN GOSUB OrdenOfe ELSE LSET nomshort$ = MID$(nomb2$, 1, 6): LSET num$ = MKI$(ElegMod%): PUT #2, 1
'               END IF
'               IF MID$(Indexar4$, 3, 1) = "$" THEN
'                    CLOSE #2: OPEN "f:\kevinprg\kevindat\oferta3.idx" FOR RANDOM SHARED AS #2 LEN = 8
'                    FIELD #2, 6 AS nomshort$, 2 AS num$: max% = LOF(2) / 8: IF max% > 0 THEN GOSUB OrdenOfe ELSE LSET nomshort$ = MID$(nomb2$, 1, 6): LSET num$ = MKI$(ElegMod%): PUT #2, 1
'               END IF
'          END IF
'          IF Indexar2$ <> (ubic2$ + MID$(nomb2$, 1, 6) + MKI$(ElegMod%)) THEN
'               CLOSE #2: OPEN "f:\kevinprg\kevindat\ubica.idx" FOR RANDOM SHARED AS #2 LEN = 10
'               FIELD #2, 8 AS nomshort$, 2 AS num$: max% = LOF(2) / 10
'               encontrado = false: primero = 1: ultimo = max%
'               WHILE NOT encontrado AND primero <= ultimo
'                    central = (primero + ultimo) / 2: GET #2, central
'                    IF Indexar2$ = (nomshort$ + num$) THEN
'                         encontrado = true
'                    ELSE
'                         IF (nomshort$ + num$) > Indexar2$ THEN
'                              ultimo = central - 1
'                         ELSE
'                              primero = central + 1
'                         END IF
'                    END IF
'               WEND: GET #2, central
'               IF (ubic2$ + MID$(nomb2$, 1, 6)) > nomshort$ THEN
'                    DO WHILE (ubic2$ + MID$(nomb2$, 1, 6)) > nomshort$
'                         LOCK #2, central
'                         GET #2, (central + 1): PUT #2, central
'                         central = central + 1: LOCATE 1, 1: PRINT central
'                         IF central > max% THEN
'                              central = max%: UNLOCK #2, central
'                              EXIT DO
'                         END IF
'                         UNLOCK #2, central
'                         LOOP
'                         GET #2, central: IF (ubic2$ + MID$(nomb2$, 1, 6)) < nomshort$ THEN central = central - 1
'                         LOCK #2, central : UNLOCK #2, central
'                         LSET nomshort$ = (ubic2$ + MID$(nomb2$, 1, 6)): LSET num$ = MKI$(ElegMod%): PUT #2, central
'               ELSE
'                         DO WHILE (ubic2$ + MID$(nomb2$, 1, 6)) < nomshort$
'                         LOCK #2, central
'                         GET #2, (central - 1): PUT #2, central
'                         central = central - 1: LOCATE 1, 1: PRINT central
'                         IF central < 1 THEN
'                              central = 1: UNLOCK #2, central
'                              EXIT DO
'                         END IF
'                         UNLOCK #2, central
'                         LOOP
'                         GET #2, central: IF (ubic2$ + MID$(nomb2$, 1, 6)) > nomshort$ THEN central = central + 1
'                         LOCK #2, central : UNLOCK #2, central
'                         LSET nomshort$ = (ubic2$ + MID$(nomb2$, 1, 6)): LSET num$ = MKI$(ElegMod%): PUT #2, central
'               END IF
'          END IF
'          IF Indexar3$ <> (labo2$ + MID$(nomb2$, 1, 6) + MKI$(ElegMod%)) THEN
'               CLOSE #2: OPEN "f:\kevinprg\kevindat\lab.idx" FOR RANDOM SHARED AS #2 LEN = 10
'               FIELD #2, 8 AS nomshort$, 2 AS num$: max% = LOF(2) / 10
'                    encontrado = false: primero = 1: ultimo = max%
'                    WHILE NOT encontrado AND primero <= ultimo
'                         central = (primero + ultimo) / 2: GET #2, central
'                         IF Indexar3$ = (nomshort$ + num$) THEN
'                              encontrado = true
'                         ELSE
'                              IF (nomshort$ + num$) > Indexar3$ THEN
'                                   ultimo = central - 1
'                              ELSE
'                                   primero = central + 1
'                              END IF
'                         END IF
'                    WEND: GET #2, central
'
'               IF (labo2$ + MID$(nomb2$, 1, 6)) > nomshort$ THEN
'                    DO WHILE (labo2$ + MID$(nomb2$, 1, 6)) > nomshort$
'                         LOCK #2, central
'                         GET #2, (central + 1): PUT #2, central
'                         central = central + 1: LOCATE 1, 1: PRINT central
'                         IF central > max% THEN
'                              central = max%: UNLOCK #2, central
'                              EXIT DO
'                         END IF
'                         UNLOCK #2, central
'                    LOOP
'                         GET #2, central: IF (labo2$ + MID$(nomb2$, 1, 6)) < nomshort$ THEN central = central - 1
'                         LOCK #2, central : UNLOCK #2, central
'                         LSET nomshort$ = (labo2$ + MID$(nomb2$, 1, 6)): LSET num$ = MKI$(ElegMod%): PUT #2, central
'               ELSE
'                         DO WHILE (labo2$ + MID$(nomb2$, 1, 6)) < nomshort$
'                         LOCK #2, central
'                         GET #2, (central - 1): PUT #2, central
'                         central = central - 1: LOCATE 1, 1: PRINT central
'                         IF central < 1 THEN
'                              central = 1: UNLOCK #2, central
'                              EXIT DO
'                         END IF
'                         UNLOCK #2, central
'                         LOOP
'                         GET #2, central: IF (labo2$ + MID$(nomb2$, 1, 6)) > nomshort$ THEN central = central + 1
'                         LOCK #2, central : UNLOCK #2, central
'                         LSET nomshort$ = (labo2$ + MID$(nomb2$, 1, 6)): LSET num$ = MKI$(ElegMod%): PUT #2, central
'               END IF
'          END IF
'          IF Indexar4$ <> (of12$ + of22$ + of32$ + MKI$(ElegMod%)) THEN
'               IF MID$(Indexar4$, 1, 1) <> of12$ THEN
'                    CLOSE #2: OPEN "f:\kevinprg\kevindat\oferta1.idx" FOR RANDOM SHARED AS #2 LEN = 8
'                    FIELD #2, 6 AS nomshort$, 2 AS num$: max% = LOF(2) / 8: k = 1: GOSUB OrdenaOferta
'               END IF
'               IF MID$(Indexar4$, 2, 1) <> of22$ THEN
'                    CLOSE #2: OPEN "f:\kevinprg\kevindat\oferta2.idx" FOR RANDOM SHARED AS #2 LEN = 8
'                    FIELD #2, 6 AS nomshort$, 2 AS num$: max% = LOF(2) / 8: k = 2: GOSUB OrdenaOferta
'               END IF
'               IF MID$(Indexar4$, 3, 1) <> of32$ THEN
'                    CLOSE #2: OPEN "f:\kevinprg\kevindat\oferta3.idx" FOR RANDOM SHARED AS #2 LEN = 8
'                    FIELD #2, 6 AS nomshort$, 2 AS num$: max% = LOF(2) / 8: k = 3: GOSUB OrdenaOferta
'               END IF
'          END IF
'          IF Indexar5% <> (CVI(compo12$) + CVI(compo22$) + CVI(compo32$) + CVI(compo42$) + CVI(compo52$) + CVI(compo62$)) THEN
'          'abrir compos.idx len=8 :6 codigo componente mki$*6 2 ubicacion en new.dbf mki$
'          END IF
          UNLOCK #1, ElegMod%
     CASE ELSE: Salir = true
    END SELECT
LOOP
CLOSE : EXIT SUB
'****************************************************************************
Buscaubicacion:
CLOSE #2: OPEN "f:\kevinprg\kevindat\ordubica.dat" FOR RANDOM SHARED AS #2 LEN = 15: FIELD #2, 15 AS descripcion$
GET #2, CVI(ubic2$): COLOR colors(7, colorpref), colors(4, colorpref): LOCATE 3, 58: PRINT USING "²###²"; CVI(ubic2$); : PRINT descripcion$
RETURN

Buscalaboratorio:
CLOSE #2: OPEN "f:\kevinprg\kevindat\ordlab.dat" FOR RANDOM SHARED AS #2 LEN = 15: FIELD #2, 15 AS descripcion$
GET #2, CVI(labo2$): COLOR colors(7, colorpref), colors(4, colorpref): LOCATE 4, 52: PRINT USING "²###²"; CVI(labo2$); : PRINT descripcion$
RETURN

SortYname:
Center 25, " Sorteando Los Campos De Menor A Mayor. Espere Por Favor . . . "
IF control% = 1 THEN
     CLOSE #2: OPEN "f:\kevinprg\kevindat\ubica.dat" FOR RANDOM SHARED AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$: selecc% = 199
     CLOSE #3: OPEN "f:\kevinprg\kevindat\ordubica.dat" FOR RANDOM SHARED AS #3 LEN = 15: FIELD #3, 15 AS deskrip$
END IF
IF control% = 2 THEN
     CLOSE #2: OPEN "f:\kevinprg\kevindat\lab.dat" FOR RANDOM SHARED AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$: selecc% = 999
     CLOSE #3: OPEN "f:\kevinprg\kevindat\ordlab.dat" FOR RANDOM SHARED AS #3 LEN = 15: FIELD #3, 15 AS deskrip$
END IF
IF control% = 4 THEN
     CLOSE #2: OPEN "f:\kevinprg\kevindat\componen.dat" FOR RANDOM SHARED AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$: selecc% = 999
     CLOSE #3: OPEN "f:\kevinprg\kevindat\ordcompo.dat" FOR RANDOM SHARED AS #3 LEN = 15: FIELD #3, 15 AS deskrip$
END IF
FOR k% = 1 TO selecc%
     GET #2, k%
     LOCATE 1, 1: PRINT VAL(codigo$) = VAL(Code(donde%))
     IF VAL(codigo$) = VAL(Code(donde%)) THEN
          LOCK #2, k%
          RSET codigo$ = Code(donde%): RSET descripcion$ = Contenido(donde%)
          PUT #2, k%
          UNLOCK #2, k%
          EXIT FOR
     END IF
NEXT k: LOCK #2, 1 TO selecc%
N% = selecc%: Salto% = N%
WHILE Salto% > 1
     Salto% = Salto% / 2
     DO
          Sw% = 1
          FOR J% = 1 TO N% - Salto%
               I% = J% + Salto%
               GET #2, J%: ao1 = descripcion$: ao3 = codigo$
               GET #2, I%: ao2 = descripcion$: ao4 = codigo$
               IF ao1 > ao2 THEN
                    RSET descripcion$ = ao2: RSET codigo$ = ao4: PUT #2, J%
                    RSET descripcion$ = ao1: RSET codigo$ = ao3: PUT #2, I%
                    Sw = 0
               END IF
          NEXT J%
     LOOP UNTIL Sw% = 1
WEND
FOR k% = 1 TO selecc%
     GET #2, k%
     RSET deskrip$ = descripcion$: PUT #3, VAL(codigo$)
NEXT k%
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
     IF Fun(1) = "si" THEN
            PopUp 12, 10, 2, 60, 112, 1, 0, 0
            LOCATE 13, 11: PRINT "Introduzca el nombre a buscar:"
            auxi$ = MeteDatoLetra$(13, 50, 8, SPACE$(8))
            auxi$ = Trim$(auxi$)
            CALL ShutUp
            CALL HelpMate(1, auxi$): Fun(1) = "no"
     END IF
     IF Fun(3) = "si" THEN Fun(3) = "no": sit% = 54
     IF Fun(4) = "si" THEN Fun(4) = "no": sit% = 2
     IF Fun(6) = "si" THEN Fun(6) = "no": sit% = 18
     IF Fun(7) = "si" THEN Fun(7) = "no": sit% = 40
     IF Fun(9) = "si" THEN Fun(9) = "no": sit% = 66: guia = 1
     IF Fun(10) = "si" THEN sit% = 66: guia = 2
     IF Fun(11) = "si" THEN
          sit% = 66: guia = 2
          CLOSE #10: OPEN "f:\kevinprg\kevindat\arregla.dat" FOR RANDOM AS #10 LEN = 2
          FIELD #10, 2 AS campo$
          GET #10, 1: newreg% = CVI(campo$)
          Box 5, 5, 8, 75: Center 6, "Estamos ubicado en el registro numero:": Center 7, "¨ A que numero de registro queremos ir ahora ?"
          newreg% = VAL(EntraDatoNumero(6, 60, 10, STR$(newreg%), 1))
          LSET campo$ = MKI$(newreg%): PUT #10, 1: CLOSE #10
     END IF
RETURN

LeeDatComp:
CLOSE #2: OPEN "f:\kevinprg\kevindat\ordcompo.dat" FOR RANDOM SHARED AS #2 LEN = 15: FIELD #2, 15 AS descripcion$
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
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE 9, 67: PRINT USING "²######,.##²"; CVD(pv42$); : LOCATE 15, 68: PRINT gr$
LOCATE 11, 67: PRINT USING "²#########,²"; CVI(exi42$); : LOCATE 13, 67: PRINT USING "²######,.##²"; CVD(cos42$);
RETURN
OrdenaOferta:
IF MID$(Indexar4$, k, 1) = " " THEN
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
ELSE
     encontrado = false: primero = 1: ultimo = max%
     WHILE NOT encontrado AND primero <= ultimo
          central = (primero + ultimo) / 2: GET #2, central
          IF (MID$(nomb2$, 1, 6) + MKI$(ElegMod%)) = (nomshort$ + num$) THEN
               encontrado = true
          ELSE
               IF (nomshort$ + num$) > (MID$(nomb2$, 1, 6) + MKI$(ElegMod%)) THEN
                    ultimo = central - 1
               ELSE
                    primero = central + 1
               END IF
          END IF
     WEND: GET #2, central
     DO WHILE "þ     " > nomshort$
          LOCATE 1, 1: PRINT nomshort$; "-"; central: Espera: LOCK #2, central
          GET #2, (central + 1): PUT #2, central
          central = central + 1: LOCATE 1, 1: PRINT central
          IF central = max% THEN
               GET #2, central: PUT #2, (central - 1): UNLOCK #2, central
               EXIT DO
          END IF
          UNLOCK #2, central
     LOOP
     LOCK #2, central : UNLOCK #2, central
     LOCATE 1, 1: PRINT central; nomshort$
     LSET nomshort$ = "þ     ": LSET num$ = MKI$(ElegMod%): PUT #2, central
END IF
RETURN
OrdenOfe:
     encontrado = false: primero = 1: ultimo = max%
     WHILE NOT encontrado AND primero <= ultimo
          central = (primero + ultimo) / 2: GET #2, central
          IF (MID$(Indexar1$, 1, 6) + MKI$(ElegMod%)) = (nomshort$ + num$) THEN
               encontrado = true
          ELSE
               IF (nomshort$ + num$) > (MID$(Indexar1$, 1, 6) + MKI$(ElegMod%)) THEN
                    ultimo = central - 1
               ELSE
                    primero = central + 1
               END IF
          END IF
     WEND: GET #2, central
               IF MID$(nomb2$, 1, 6) > nomshort$ THEN
                    DO WHILE MID$(nomb2$, 1, 6) > nomshort$
                         LOCK #2, central
                         GET #2, (central + 1): PUT #2, central
                         central = central + 1: LOCATE 1, 1: PRINT central
                         IF central > max% THEN
                              central = max%: UNLOCK #2, central
                              EXIT DO
                         END IF
                         UNLOCK #2, central
                         LOOP
                         GET #2, central: IF MID$(nomb2$, 1, 6) < nomshort$ THEN central = central - 1
                         LOCK #2, central : UNLOCK #2, central
                         LSET nomshort$ = MID$(nomb2$, 1, 6): LSET num$ = MKI$(ElegMod%): PUT #2, central
                    ELSE
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
                         GET #2, central: IF MID$(nomb2$, 1, 6) > nomshort$ THEN central = central + 1
                         LOCK #2, central : UNLOCK #2, central
                         LSET nomshort$ = MID$(nomb2$, 1, 6): LSET num$ = MKI$(ElegMod%): PUT #2, central
               END IF
RETURN
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
PRINT #2, SONIDO
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

FUNCTION Trim$ (x$)
IF x$ = "" THEN
     Trim$ = ""
ELSE
lastChar = 0
FOR a = 1 TO LEN(x$): y$ = MID$(x$, a, 1): IF y$ <> CHR$(0) AND y$ <> " " THEN lastChar = a
NEXT a: Trim$ = LEFT$(x$, lastChar)
END IF
END FUNCTION

