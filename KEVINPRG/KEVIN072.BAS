DEFINT A-Z
DECLARE SUB Espera ()
DECLARE SUB Center (row, text$)
DECLARE SUB Box (Row1%, Col1%, Row2%, Col%)
DECLARE SUB BorraBox (Row1%, Col1%, Row2%, Col%)
DECLARE SUB MarcaVencimiento ()
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$)
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Escoge% (fila%, edo%, fila2%, Prod$)
DECLARE FUNCTION CheqDate (caden$, op%)
DECLARE FUNCTION Trim$ (x$)
COMMON SHARED guia, colorpref AS INTEGER
DIM SHARED Fun(17) AS STRING * 2
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED salir, terminar, sitio, busqueda, devolver, place
DIM SHARED Articulo1(10) AS STRING * 41, Articulo2(10) AS STRING * 2
DIM SHARED Nombra(5) AS STRING * 35, Pvp1a(5) AS STRING * 8, Pvp2a(5) AS STRING * 8, Pvp3a(5) AS STRING * 8, Pvp4a(5) AS STRING * 8, Numa(5) AS STRING * 2, Observa(5) AS STRING * 20, Exis1a(5) AS STRING * 2, Exis2a(5) AS STRING * 2, Exis3a(5) AS  _
STRING * 2, Exis4a(5) AS STRING * 2
DIM SHARED Cant1a(5) AS STRING * 2, Cant2a(5) AS STRING * 2, Cant3a(5) AS STRING * 2, Cant4a(5) AS STRING * 2, Prega(5) AS STRING * 1
CONST true = -1: CONST false = NOT true: busqueda = 0
ON ERROR GOTO ErrorTrap
MarcaVencimiento
CLOSE
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... ": COLOR colors(7, colorpref), colors(4, colorpref)
CHAIN "F:\kevinprg\kevin01.bas"
END
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
            Center 25, " Esperando Acceso a La Red   ...": SLEEP 1: Center 25, SPACE$(35): RESUME
        CASE ELSE
REM            CLS : Box 10, 10, 20, 70: Center 13, "Error Inesperado Numero:" + STR$(ERR) + " (Anote Este Numero)": Center 14, "DESCONECTANDOSE DE LA RED ...": LOCATE 21, 1: SYSTEM
    END SELECT
    RESUME NEXT
END
DATA 0,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  4,     0,     1,     15,     0,     7,      0
DATA 3,    15,   13,  1,     14,    3,     15,     0,     7,      0
DATA 7,    12,   15,  4,     14,    0,     15,     15,    1,      0
'Atributo     Valor pre-       Color pre-        Valor pre-    Color pre-
'de color     determinado(a)   sentado           determinado   sentado
'0            0                Negro             0          Desactivado
'1            1                Azul                            Subrayado(d)
'2            2                Verde             1          Activado(d)
'3            3                Azul-verdoso      1          Activado(d)
'4            4                Rojo              1          Activado(d)
'5            5                Magenta           1          Activado(d)
'6            6                Marr¢n            1          Activado(d)
'7            7                Blanco            1          Activado(d)
'8            8                Gris              0          Desactivado
'9            9                Azul claro                      Alta intensidad
'                                                              Subrayado
'10           10               Verde claro       2          Alta intensidad
'11           11               Azul-verdoso claro2          Alta intensidad
'12           12               Rojo claro        2          Alta intensidad
'13           13               Magenta claro     2          Alta intensidad
'14           14               Amarillo          2          Alta intensidad
'15           15               Blanco de alta    0          Desactivado
'                              intensidad
'

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

FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
IF LEN(caden$) > largo% THEN caden$ = MID$(caden$, 1, largo%)
LOCATE fila%, columna%: PRINT CHR$(178) + caden$ + SPACE$(largo% - LEN(caden$)): LOCATE fila%, columna% + largo% + 1: PRINT CHR$(178)
REDIM eleccion(0 TO largo% + 1) AS STRING * 1: llevasitio% = 2
FOR k = 1 TO largo%: eleccion(k) = MID$(caden$, k, 1): NEXT k: LOCATE fila%, (columna% + llevasitio%): COLOR colors(4, colorpref) + 16, colors(7, colorpref): IF eleccion(2) = " " THEN PRINT "_";  ELSE PRINT eleccion(2)
DO
      llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
          SELECT CASE llave$
               CASE CHR$(0) + "H": devolver = true: Fun(12) = "si": EXIT DO
               CASE CHR$(0) + "P": Fun(12) = "si": EXIT DO
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
               'CASE CHR$(0) + CHR$(60): Fun(2) = "si": EXIT DO
               'CASE CHR$(0) + CHR$(61): Fun(3) = "si": EXIT DO
               'CASE CHR$(0) + CHR$(62): Fun(4) = "si": EXIT DO
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
               CASE ELSE
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
EntraDatoNumero$ = eleccion$
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
CALL Center(25, "[F1] Ayuda [F9] Salir SIN Escoger [Enter] Toma Articulo [A-Z] Ir a")
     Prod$ = "      "
     DO WHILE Prod$ = "      "
     LOCATE fila2%, 5: PRINT "Introduzca Orden de Busqueda:"; SPACE$(15)
     Prod$ = UCASE$(MeteDatoLetra(fila2%, 34, 6, ""))
     Prud = Prod$: Prod$ = Trim$(Prod$)
     IF Fun(9) = "si" OR Prod$ = "" THEN : Escoge% = -1: EXIT FUNCTION
     LOOP
selecc% = 0
CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
OPEN "F:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
FIELD #2, 35 AS nomshort$, 2 AS num$: max2% = LOF(2) / 37
GOSUB Parche1
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
          CASE ELSE:  BEEP
     END SELECT
     IF up% = 0 THEN
          FOR k = 0 TO 4: LOCATE fila% + k, 1: PRINT SPACE$(80); : NEXT k
          EXIT DO
     END IF
     FOR l = 1 TO 5
          IF resal% = l THEN LOCATE fila% - 2, 50: PRINT "En Oferta:"; : PRINT USING "#####, "; CVI(Cant4a(l)); : PRINT " a Bs."; : PRINT USING "######,"; CVD(Pvp4a(l))
          IF resal% = l THEN
               IF Prega(l) = "*" THEN LOCATE fila% - 2, 5: PRINT "SI se le vigila vencimiento" ELSE LOCATE fila% - 2, 5: PRINT "NO se le vigila vencimiento"
          END IF
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
          GET #2, up%: IF Prod$ <> MID$(nomshort$, 1, LEN(Prod$)) THEN up% = up% + 1
          GOSUB LeeCinco
     END IF
RETURN
Abajo:
     resal% = resal% + 1
     IF resal% > selecc% THEN
          resal% = selecc%: up% = up% + 1
          IF up% > max2% THEN up% = max2%
          GET #2, (up% + 4): IF Prod$ <> MID$(nomshort$, 1, LEN(Prod$)) THEN up% = up% - 1
          GOSUB LeeCinco
     END IF
RETURN
Apgup:
up% = up% - 4: IF up% < 1 THEN up% = 1
DO
GET #2, up%: IF Prod$ = MID$(nomshort$, 1, LEN(Prod$)) THEN EXIT DO ELSE up% = up% + 1
LOOP
resal% = 1: GOSUB LeeCinco
RETURN
Apgdn:
up% = up% + 4: IF up% > max2% THEN up% = max2%
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
          GET #1, CVI(num$): selecc% = selecc% + 1
          Nombra(selecc%) = nomb2$: Cant1a(selecc%) = exi12$: Cant2a(selecc%) = exi22$: Cant3a(selecc%) = exi32$: Cant4a(selecc%) = exi42$: Pvp1a(selecc%) = pv12$: Pvp2a(selecc%) = pv22$: Pvp3a(selecc%) = pv32$: Pvp4a(selecc%) = pv42$:  Numa(selecc% _
) = num$: Prega(selecc%) = preg$
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
) = num$: Prega(selecc%) = preg$
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
IF Prega(1) = "*" THEN LOCATE fila% - 2, 5: PRINT "SI se le vigila vencimiento" ELSE LOCATE fila% - 2, 5: PRINT "NO se le vigila vencimiento"
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

SUB MarcaVencimiento
COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box 1, 1, 3, 80: place = 0: LOCATE 24, 1: PRINT STRING$(80, "Ä"); : LOCATE 13, 1: PRINT STRING$(80, "Ä");
Center 2, "(Des)Marque el (los) Articulo(s) para Vigilarle(s) el Vencimiento": Center 25, "[F1] Ayuda [F9] Salir de Este Modulo"
FOR k = 1 TO 10: Center 13 + k, MID$(Articulo1(k), 1, 6) + "..." + MID$(Articulo1(k), 7, 35) + "..." + Articulo2(k): NEXT k
DO
     m = -1
     DO WHILE m <= 0
          m = Escoge(6, 1, 7, ""): LOCATE 4, 1: PRINT SPACE$(79)
          IF Fun(9) = "si" THEN CLOSE : EXIT SUB
     LOOP
     CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
     FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS  _
cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$ _
, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
     place = place + 1
     IF place > 10 THEN
          place = 10: FOR k = 1 TO 9: Articulo1(k) = Articulo1(k + 1): Articulo2(k) = Articulo2(k + 1): NEXT k
     END IF: COLOR colors(7, colorpref), colors(4, colorpref)
     GET #1, m    ': LOCK #1, m  ': LOCATE 1, 1: PRINT SPACE$(80); : LOCATE 1, 1: PRINT m; codigo2$; nomb2$: Espera
     Articulo1(place) = codigo2$ + nomb2$: LOCATE 4, 1: PRINT SPACE$(80); : LOCATE 13, 16: PRINT "Codigo": LOCATE 13, 25: PRINT "Descripcion": LOCATE 13, 60: PRINT "Vigilar Vencimiento?"
     IF preg$ = "*" THEN RSET preg$ = " ": Articulo2(place) = "NO" ELSE RSET preg$ = "*": Articulo2(place) = "SI"
     PUT #1, m: FOR k = 1 TO 10: Center (13 + k), (MID$(Articulo1(k), 1, 6) + "..." + MID$(Articulo1(k), 7, 35) + "..." + Articulo2(k)): NEXT k        ': UNLOCK #1, m
LOOP
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
MeteDatoLetra$ = eleccion$
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
FOR a = 1 TO LEN(x$): Y$ = MID$(x$, a, 1): IF Y$ <> CHR$(0) AND Y$ <> " " THEN lastChar = a
NEXT a: Trim$ = LEFT$(x$, lastChar)
END IF
END FUNCTION

