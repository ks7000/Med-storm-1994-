DEFINT A-Z
DECLARE SUB Espera ()
DECLARE SUB LeeEstado ()
DECLARE SUB SalvaEstado ()
DECLARE SUB Center (row, text$)
DECLARE SUB Box (Row1%, Col1%, Row2%, Col%)
DECLARE SUB BorraBox (Row1%, Col1%, Row2%, Col%)
DECLARE SUB Imprime (opcion%)
DECLARE SUB ImprimeVenc ()
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$)
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Escoge% (fila%, edo%, fila2%, Prod$)
DECLARE FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%, est%)
DECLARE FUNCTION Trim$ (x$)
DECLARE FUNCTION CalculaMes% (mes$)
COMMON SHARED guia, colorpref: colorpref = 1: guia = 2
DIM SHARED turno AS STRING * 1, devolver
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED SONIDO  AS STRING * 2, impuesto AS STRING * 6, Nombfarma AS STRING * 25, Direccion AS STRING * 30, rif     AS STRING * 25, nit      AS STRING * 25, telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2
DIM SHARED Fun(17) AS STRING * 2, salir, Turna, terminar, sitio
DIM SHARED Contenido(199) AS STRING * 15, Code(199) AS STRING * 3, monto AS STRING * 8
DIM SHARED Nombra(5) AS STRING * 35, Pvp1a(5) AS STRING * 8, Pvp2a(5) AS STRING * 8, Pvp3a(5) AS STRING * 8, Pvp4a(5) AS STRING * 8, Numa(5) AS STRING * 2, Observa(5) AS STRING * 20, Exis1a(5) AS STRING * 2, Exis2a(5) AS STRING * 2, Exis3a(5) AS  _
STRING * 2, Exis4a(5) AS STRING * 2
DIM SHARED Cant1a(5) AS STRING * 2, Cant2a(5) AS STRING * 2, Cant3a(5) AS STRING * 2, Cant4a(5) AS STRING * 2
CONST true = -1: CONST false = NOT true
ON ERROR GOTO ErrorTrap
CALL LeeEstado
IF guia = 1 OR guia = 2 THEN CALL Imprime(guia)
IF guia = 7 THEN ImprimeVenc
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... "
'CHAIN "F:\kevinprg\kevin01.bas"
END
ErrorTrap:
    SELECT CASE ERR
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

SUB BorraBox (Row1%, Col1%, Row2%, Col2%) STATIC
BoxWidth = Col2% - Col1% + 1: FOR a = Row1% TO Row2%: LOCATE a, Col1%: PRINT SPACE$(BoxWidth); : NEXT a
END SUB

SUB Box (Row1%, Col1%, Row2%, Col2%) STATIC
BoxWidth = Col2% - Col1% + 1: LOCATE Row1%, Col1%: PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿"; : FOR a = Row1% + 1 TO Row2% - 1: LOCATE a, Col1%: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2%, Col1%: PRINT "À"; STRING$(BoxWidth - 2,  _
"Ä"); "Ù";
END SUB

FUNCTION CalculaMes% (mes$)
CalculaMes% = 0
IF MID$(mes$, 1, 1) = "A" THEN CalculaMes% = 10 + 12 * (CVI(MID$(mes$, 2, 2)) - 1990): EXIT FUNCTION
IF MID$(mes$, 1, 1) = "B" THEN CalculaMes% = 11 + 12 * (CVI(MID$(mes$, 2, 2)) - 1990): EXIT FUNCTION
IF MID$(mes$, 1, 1) = "C" THEN CalculaMes% = 12 + 12 * (CVI(MID$(mes$, 2, 2)) - 1990): EXIT FUNCTION
IF VAL(MID$(mes$, 1, 1)) > 0 AND VAL(MID$(mes$, 1, 1)) < 10 THEN
     CalculaMes% = VAL(MID$(mes$, 1, 1)) + 12 * (CVI(MID$(mes$, 2, 2)) - 1990): EXIT FUNCTION
END IF
EXIT FUNCTION
END FUNCTION

SUB Center (row, text$)
IF row = 25 THEN COLOR colors(5, colorpref), colors(4, colorpref): LOCATE 25, 1: PRINT SPACE$(80);
LOCATE row, 41 - LEN(text$) / 2: PRINT text$;
END SUB

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
END SELECT
FOR k = 1 TO 199: GET #2, k: Code(k) = codigo$: Contenido(k) = descripcion$: NEXT k: CLOSE #2
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila% + 1, columna% + 1: PRINT Contenido(1): COLOR colors(4, colorpref), colors(7, colorpref)
FOR k = 2 TO 15: LOCATE fila% + k, columna% + 1: PRINT Contenido(k): NEXT k
resal% = 1: up% = 1: terminar = false
DO WHILE NOT terminar
Kbd$ = "": WHILE Kbd$ = "": Kbd$ = INKEY$: WEND
        SELECT CASE Kbd$
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
     Kbd$ = "": WHILE Kbd$ = "": Kbd$ = INKEY$: WEND: Kbd$ = UCASE$(Kbd$)
     SELECT CASE Kbd$
          CASE CHR$(0) + "H": GOSUB Arriba
          CASE CHR$(0) + "P": GOSUB Abajo
          CASE CHR$(0) + CHR$(73): GOSUB Apgup
          CASE CHR$(0) + CHR$(81): GOSUB Apgdn
          CASE CHR$(13): EXIT DO
          CASE CHR$(27): resal% = 0: EXIT DO
          CASE CHR$(0) + CHR$(67): resal% = 0: Fun(3) = "si": EXIT DO
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

SUB Imprime (opcion%)
'lista01(opcion%)
FOR k = 1 TO 12: Fun(k) = "no": NEXT k
CLS : Box 1, 1, 3, 80
DIM Prud1 AS STRING * 4: DIM Prud2 AS STRING * 4: DIM costo AS STRING * 8
LOCATE 4, 44: PRINT "Desde:²    ²": LOCATE 4, 64: PRINT "Hasta:²    ²"
IF opcion% = 1 THEN
     Center 2, " Proceso de Impresion de Listado Para HACER Inventario "
     Center 25, "[F1]Ayuda  [F9]Salir SIN Imprimir"
ELSE
     Center 2, " Proceso de Impresion de Listado De Inventario"
END IF
sit% = 1: salir = false
DO WHILE salir = false
     SELECT CASE sit%
     CASE 1
          COLOR 0, 15: LOCATE 4, 1: PRINT " Escoja la Ubicacion:²04²Sin Ubicacion"
          control% = Elije%(1, 5, 22, 4, 23, 0, 1)
          IF control% < 0 THEN sit% = 1 ELSE sit% = 2
     CASE 2
          COLOR 0, 15: FOR k = 5 TO 21: Center k, SPACE$(80): NEXT k
          Center 25, "[F1]Ayuda  [F9]Salir SIN Imprimir"
          Prud1 = MeteDatoLetra(4, 50, 4, Prud1)
          Prud1 = Trim$(Prud1)
          IF Fun(9) = "si" THEN Fun(9) = "no": EXIT SUB
          IF devolver = true THEN devolver = false: sit% = 1
          IF Prud1 = "" THEN sit% = 1 ELSE sit% = 3
     CASE 3
          Prud2 = MeteDatoLetra(4, 70, 4, Prud2)
          Prud2 = Trim$(Prud2)
          IF Fun(9) = "si" THEN Fun(9) = "no": EXIT SUB
          IF Prud2 = "" THEN sit% = 2 ELSE salir = true
          IF devolver = true THEN devolver = false: sit% = 2
     END SELECT
LOOP
Center 25, " Imprimiendo Datos. Espere Por favor..."
CLOSE : OPEN "f:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
OPEN "f:\kevinprg\kevindat\ubica.idx" FOR RANDOM AS #2 LEN = 10
FIELD #2, 8 AS codigo$, 2 AS sitio$: max2% = LOF(2) / 10
apu% = 1: apa% = 1: COLOR 7, 0: monto = MKD$(0): costo = MKD$(0): h$ = STRING$(80, 176): S = 1
ha$ = "Listado para HACER inventario.Ubicaci¢n:"
hb$ = "Listado de inventario.        Ubicaci¢n:": ro% = 0
ON ERROR GOTO ErrorTrap
OPEN "LPT1:" FOR RANDOM AS #10
WIDTH #10, 255
'GOSUB Encabezado
FOR k = 1 TO 7: PRINT #10, CHR$(10); : NEXT k
PRINT #10, "."; CHR$(10);
FOR k = 1 TO max2%
     GET #2, k: GET #1, CVI(sitio$)
     LOCATE 2, 1: PRINT nomb2$; Prud1; Prud2; Code(control%)
     IF Prud1 <= MID$(nomb2$, 1, LEN(Prud1)) AND Prud2 >= MID$(nomb2$, 1, LEN(Prud2)) AND CVI(ubic2$) = VAL(Code(control%)) THEN
          IF opcion% = 1 THEN
               PRINT #10, "®___³________¯ ®___³________¯ ®___³________¯"; CHR$(10);
          ELSE
               IF (CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$)) > 0 THEN
                    ro% = ro% + 1
                    IF INSTR(UCASE$(nomb2$), "AMP") > 0 THEN tipo$ = " AMP." ELSE tipo$ = " TAB."
                    PRINT #10, SPACE$(15);
                    PRINT #10, nomb2$;
                    total% = CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$)
                    total% = total% * VAL(MID$(obser2$, 3, 3))
                    PRINT #10, USING "#######,"; total%; : PRINT #10, tipo$;
                    PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
               END IF
'               IF CVI(exi12$) > 0 THEN
'                    PRINT #10, USING "#####,"; CVI(exi12$);
'                    PRINT #10, USING "#####,.##"; CVD(pv12$);
'               ELSE
'                    PRINT #10, SPACE$(15);
'               END IF
'               IF CVI(exi22$) > 0 THEN
'                    PRINT #10, USING "#####,"; CVI(exi22$);
'                    PRINT #10, USING "#####,.##"; CVD(pv22$);
'               ELSE
'                    PRINT #10, SPACE$(15);
'               END IF
'               IF CVI(exi32$) + CVI(exi42$) > 0 THEN
'                    PRINT #10, USING "#####,"; CVI(exi32$) + CVI(exi42$);
'                    PRINT #10, USING "#####,.##"; CVD(pv32$);
'               ELSE
'                    PRINT #10, SPACE$(15);
'               END IF
'               monto = MKD$(CVD(monto) + (CVI(exi12$) * CVD(pv12$)) + (CVI(exi22$) * CVD(pv22$)) + ((CVI(exi32$) + CVI(exi42$)) * CVD(pv32$)))
'               costo = MKD$(CVD(costo) + (CVI(exi12$) * CVD(cos12$)) + (CVI(exi22$) * CVD(cos22$)) + (CVI(exi32$) * CVD(cos32$)) + (CVI(exi42$) * CVD(cos42$)))
          END IF
          IF ro% > 28 THEN
               ro% = 0
               'IF opcion% = 2 THEN
               '     PRINT #10, "Sub-total costo:"; : PRINT #10, USING "############,.##"; CVD(costo);
               '     PRINT #10, "  Sub-total PVP:"; : PRINT #10, USING "############,.##"; CVD(monto); : PRINT #10, CHR$(10)
               'END IF
               'PRINT #10, h$; CHR$(10);
               PRINT #10, CHR$(12); ' : GOSUB Encabezado
          END IF
     END IF
NEXT k
'IF opcion% = 2 THEN
'     PRINT #10, "Total costo:"; : PRINT #10, USING "#############,.##"; CVD(costo);
'     PRINT #10, "  Total PVP:"; : PRINT #10, USING "#############,.##"; CVD(monto); : PRINT #10, CHR$(10);
'END IF
'PRINT #10, h$; CHR$(10);
PRINT #10, CHR$(12); : PRINT #10, CHR$(27); "<";
CLOSE
EXIT SUB

Encabezado:
     PRINT #10, CHR$(27); "C"; CHR$(0); CHR$(11)
     PRINT #10, h$; CHR$(10);
     IF opcion% = 1 THEN
          PRINT #10, CHR$(14); ha$; CHR$(10);
     ELSE
          PRINT #10, CHR$(14); hb$; CHR$(10);
     END IF
     PRINT #10, CHR$(14); "²"; Code(control%); "²"; Contenido(control%); " desde "; Prud1; " hasta "; Prud2; CHR$(10);
     PRINT #10, CHR$(20);
RETURN
END SUB

SUB ImprimeVenc
CLS : Box 1, 1, 3, 80: Center 2, "Reporte de productos con pronto vencimiento": Center 4, "Escoja bajo que orden se imprimir ."
CALL Center(25, "[F1]Ayuda -Use las teclas de direcci¢n y Enter para escojer [F9]Abortar"): Box 5, 9, 11, 71
DIM Criterio(5) AS STRING * 60: salir = false: DIM Prud1 AS STRING * 4, Prud2 AS STRING * 4, mes1 AS STRING * 7, mes2 AS STRING * 7
Criterio(1) = "Orden alfabetico solamente(respecto al nombre del articulo)"
Criterio(2) = "Por ubicaci¢n    (y en orden alfabetico nombre y ubicaci¢n)"
Criterio(3) = "Por fecha de vencimiento(m s pr¢ximo y en orden alfabetico)"
Criterio(4) = "Por laboratorio        (ambos nombres por orden alfabetico)"
Criterio(5) = "Por c¢digo de articulo         (ambos por orden alfabetico)": resal% = 1
DO WHILE salir = false
     FOR k = 1 TO 5
          IF resal% = k THEN COLOR colors(7, colorpref), colors(4, colorpref)
          LOCATE 5 + k, 10: PRINT Criterio(k)
          IF resal% = k THEN COLOR colors(4, colorpref), colors(7, colorpref)
     NEXT k
     key$ = "": DO WHILE key$ = "": key$ = INKEY$: LOOP
     IF key$ = CHR$(0) + "H" THEN resal% = resal% - 1: IF resal% < 1 THEN resal% = 5
     IF key$ = CHR$(0) + "P" THEN resal% = resal% + 1: IF resal% > 5 THEN resal% = 1
     IF key$ = CHR$(0) + CHR$(67) THEN EXIT SUB
     IF key$ = CHR$(13) THEN salir = true
LOOP
BorraBox 5, 9, 11, 71: Box 5, 9, 7, 71: LOCATE 6, 10: PRINT Criterio(resal%): Prud1 = "    ": Prud2 = "    "
LOCATE 8, 24: PRINT "Desde:²    ²": LOCATE 8, 44: PRINT "Hasta:²    ²": sit% = 1: salir = false: Center 4, "Introduzca desde y hasta que nombre de articulo quiere el listado."
DO WHILE salir = false
     SELECT CASE sit%
     CASE 1
          CALL Center(25, "[F1]Ayuda [F8]Todos [F9]Abortar -Teclee DESDE que nombre")
          Prud1 = MeteDatoLetra(8, 30, 4, Prud1): Prud1 = Trim$(Prud1): Prud1 = UCASE$(Prud1)
          IF Fun(8) = "si" THEN salir = true: LOCATE 8, 24: PRINT "Desde:²AAAA²": LOCATE 8, 44: PRINT "Hasta:²ZZZZ²"
          IF Fun(9) = "si" THEN Fun(9) = "no": EXIT SUB
          IF devolver = true THEN devolver = false: sit% = 1 ELSE sit% = 2
     CASE 2
          CALL Center(25, "[F1]Ayuda [Esc]Campo anterior [F8]Todos [F9]Abortar -Teclee HASTA que nombre")
          Prud2 = MeteDatoLetra(8, 50, 4, Prud2): Prud2 = Trim$(Prud2): Prud2 = UCASE$(Prud2)
          IF Fun(8) = "si" THEN salir = true: LOCATE 8, 24: PRINT "Desde:²AAAA²": LOCATE 8, 44: PRINT "Hasta:²ZZZZ²"
          IF Fun(9) = "si" THEN Fun(9) = "no": EXIT SUB
          IF devolver = true THEN devolver = false: sit% = 1 ELSE salir = true
     END SELECT
     IF Prud1 = "" OR Prud2 = "" THEN sit% = 1: salir = false
LOOP: salir = false
BorraBox 5, 9, 11, 71: Box 5, 9, 9, 71
LOCATE 6, 10: PRINT Criterio(resal%)
Center 7, "Desde " + Prud1 + " hasta " + Prud2
LOCATE 8, 24: PRINT "Desde:²    ²"
LOCATE 8, 44: PRINT "Hasta:²    ²": sit% = 1: salir = false
Center 4, "Introduzca desde y hasta que mes de vencimiento quiere el listado."
DO WHILE salir = false
     SELECT CASE sit%
     CASE 1
          CALL Center(25, "[F1]Ayuda [F9]Abortar -Teclee DESDE que mes")
          mes1 = MeteDatoLetra(8, 30, 7, mes1)
          IF Fun(9) = "si" THEN Fun(9) = "no": EXIT SUB
          IF devolver = true THEN devolver = false: sit% = 1 ELSE sit% = 2
     CASE 2
          CALL Center(25, "[F1]Ayuda [Esc]Campo anterior [F9]Abortar -Teclee HASTA que mes")
          mes2 = MeteDatoLetra(8, 50, 7, mes2)
          IF Fun(9) = "si" THEN Fun(9) = "no": EXIT SUB
          IF devolver = true THEN devolver = false: sit% = 1 ELSE salir = true
     END SELECT
     IF mes1 = "" OR mes2 = "" THEN sit% = 1: salir = false
LOOP

Center 25, " Imprimiendo Datos. Espere Por favor..."
mes11% = VAL(MID$(mes1, 1, 2)) + ((VAL(MID$(mes1, 4, 4)) - 1990) * 12)
mes22% = VAL(MID$(mes2, 1, 2)) + ((VAL(MID$(mes2, 4, 4)) - 1990) * 12)
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
SELECT CASE resal%
     CASE 1
          OPEN "f:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
          FIELD #2, 35 AS codigo$, 2 AS sitio$: max% = LOF(2) / 37
     CASE 2
          OPEN "f:\kevinprg\kevindat\ubica.idx" FOR RANDOM SHARED AS #2 LEN = 10
          FIELD #2, 8 AS codigo$, 2 AS sitio$: max% = LOF(2) / 10
     CASE ELSE
          EXIT SUB
END SELECT
          OPEN "lpt1" FOR RANDOM AS #10: divisor$ = "": largo% = 0
          WIDTH #10, 255
          FOR k = 1 TO max%
               GET #2, k: GET #1, CVI(sitio$)
               IF resal% = 2 THEN
                    IF divisor$ <> MID$(codigo$, 1, 2) THEN divisor$ = MID$(codigo$, 1, 2): PRINT #10, CHR$(10); CHR$(10);
               END IF
               imprimir = false: LOCATE 1, 1: PRINT k; 'nomb2$
               venci1% = CalculaMes%(ven11$ + ven12$): venci2% = CalculaMes%(ven21$ + ven22$): venci3% = CalculaMes%(ven31$ + ven32$): venci4% = CalculaMes%(ven41$ + ven42$)
               'LOCATE 1, 40: PRINT k ;venci1%; venci2%; venci3%; venci4%
               IF venci1% >= mes11% AND venci1% <= mes22% THEN IF CVI(exi12$) <> 0 THEN imprimir = true
               IF venci2% >= mes11% AND venci2% <= mes22% THEN IF CVI(exi22$) <> 0 THEN imprimir = true
               IF venci3% >= mes11% AND venci3% <= mes22% THEN IF CVI(exi32$) <> 0 THEN imprimir = true
               IF venci4% >= mes11% AND venci4% <= mes22% THEN IF CVI(exi42$) <> 0 THEN imprimir = true
               'IF (CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$)) = 0 THEN imprimir = false
               IF MID$(nomb2$, 1, LEN(Prud1)) < Prud1 OR MID$(nomb2$, 1, LEN(Prud2)) > Prud2 THEN
                    IF MID$(codigo$, 35, 1) = "þ" THEN imprimir = false
               END IF
               IF imprimir = true THEN
                    'LOCATE 20, 20: PRINT k; nomb2$; CVI(ven12$); CVI(ven22$); CVI(ven32$); CVI(ven42$)
                    IF largo% = 0 THEN
                         PRINT #10, "Listado de productos con pronto vencimiento: desde la fecha "; mes1; CHR$(10);
                         PRINT #10, " hasta la fecha "; mes2; " y desde la letra "; UCASE$(Prud1); " hasta la letra "; UCASE$(Prud2); CHR$(10);
                         PRINT #10, STRING$(80, "*"); CHR$(10);
                    END IF
                    PRINT #10, nomb2$; : largo% = largo% + 1
                    IF venci1% >= mes11% AND venci1% <= mes22% THEN
                         IF ven11$ = "A" THEN PRINT #10, "10";
                         IF ven11$ = "B" THEN PRINT #10, "11";
                         IF ven11$ = "C" THEN PRINT #10, "12";
                         IF ven11$ <> "A" AND ven11$ <> "B" AND ven11$ <> "C" THEN PRINT #10, "0"; ven11$;
                         PRINT #10, "/"; MID$(STR$(CVI(ven12$)), 2, 4); SPACE$(5);
                    END IF
                    IF venci2% >= mes11% AND venci2% <= mes22% THEN
                         IF ven21$ = "A" THEN PRINT #10, "10";
                         IF ven21$ = "B" THEN PRINT #10, "11";
                         IF ven21$ = "C" THEN PRINT #10, "12";
                         IF ven21$ <> "A" AND ven21$ <> "B" AND ven21$ <> "C" THEN PRINT #10, "0"; ven21$;
                         PRINT #10, "/"; MID$(STR$(CVI(ven22$)), 2, 4); SPACE$(5);
                    END IF
                    IF venci3% >= mes11% AND venci3% <= mes22% THEN
                         IF ven31$ = "A" THEN PRINT #10, "10";
                         IF ven31$ = "B" THEN PRINT #10, "11";
                         IF ven31$ = "C" THEN PRINT #10, "12";
                         IF ven31$ <> "A" AND ven31$ <> "B" AND ven31$ <> "C" THEN PRINT #10, "0"; ven31$;
                         PRINT #10, "/"; MID$(STR$(CVI(ven32$)), 2, 4); SPACE$(5);
                    END IF
                    IF venci4% >= mes11% AND venci4% <= mes22% THEN
                         IF ven41$ = "A" THEN PRINT #10, "10";
                         IF ven41$ = "B" THEN PRINT #10, "11";
                         IF ven41$ = "C" THEN PRINT #10, "12";
                         IF ven41$ <> "A" AND ven41$ <> "B" AND ven41$ <> "C" THEN PRINT #10, "0"; ven41$;
                         PRINT #10, "/"; MID$(STR$(CVI(ven42$)), 2, 4); SPACE$(5);
                    END IF
                    PRINT #10, CHR$(10); : imprimir = true
                    IF largo% > 40 THEN largo% = 0: PRINT #10, STRING$(80, "*"); CHR$(10); : PRINT #10, CHR$(12);
               END IF
          NEXT k: IF imprimir = true THEN PRINT #10, STRING$(80, "*"); CHR$(10); : PRINT #10, CHR$(12);
CLOSE
EXIT SUB
END SUB

SUB LeeEstado
CLOSE : OPEN "F:\kevinprg\kevindat\kaos.dat" FOR INPUT SHARED AS #2
INPUT #2, SONIDO: INPUT #2, impuesto: INPUT #2, Nombfarma: INPUT #2, Direccion: INPUT #2, rif: INPUT #2, nit: INPUT #2, telefono: INPUT #2, contri: INPUT #2, contra: INPUT #2, Stock: INPUT #2, Blanq
INPUT #2, LargoH: INPUT #2, TipoL: INPUT #2, Mensaje1: INPUT #2, Mensaje2: INPUT #2, Mensaje3: INPUT #2, Redondeo: INPUT #2, Desc: INPUT #2, DescMax: INPUT #2, Exactitud: INPUT #2, Prov1: INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo: CLOSE
END SUB

FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$)
LOCATE fila%, columna%: PRINT CHR$(178) + caden$: LOCATE fila%, columna% + largo% + 1: PRINT CHR$(178)
REDIM eleccion(0 TO largo% + 1) AS STRING * 1: llevasitio% = 1
FOR k = 1 TO largo%: eleccion(k) = MID$(caden$, k, 1): NEXT k
LOCATE fila%, (columna% + llevasitio%): COLOR colors(7, colorpref) + 16, colors(4, colorpref)
IF eleccion(1) = " " THEN PRINT "_";  ELSE PRINT eleccion(1)
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
MeteDatoLetra$ = eleccion$: IF SONIDO = "si" THEN BEEP
EXIT FUNCTION
Sumar:
FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna%: PRINT "²"; eleccion$; "²"
RETURN
END FUNCTION

SUB SalvaEstado
OPEN "F:\kevinprg\kevindat\turno.dat" FOR OUTPUT AS #7
PRINT #7, USING "#"; colorpref
PRINT #7, turno
PRINT #7, USING "##"; dia1
PRINT #7, USING "##"; mes1
PRINT #7, USING "####"; ano1
CLOSE #7
OPEN "F:\kevinprg\kevindat\kaos.dat" FOR OUTPUT AS #2
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
FOR a = 1 TO LEN(x$): Y$ = MID$(x$, a, 1): IF Y$ <> CHR$(0) AND Y$ <> " " THEN lastChar = a
NEXT a: Trim$ = LEFT$(x$, lastChar)
END IF
END FUNCTION

