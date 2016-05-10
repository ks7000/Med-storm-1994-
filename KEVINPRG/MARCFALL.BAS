'marcafalla(sta%)
COLOR colors(5, colorpref), colors(4, colorpref): CLS
Box 1, 1, 3, 80: Center 25, "[F1]Ayuda  [F9]Salir"
FOR k = 1 TO 12: Fun(k) = "no": NEXT k: salir = false: devolver = false
DIM Prud1 AS STRING * 4, Drog1 AS STRING * 1
COLOR colors(5, colorpref), colors(4, colorpref): LOCATE 4, 50
PRINT "Desde:²"; Prud1; "²": Center 25, "[F1]Ayuda  [F9]Salir ": sit% = 1
IF sta% = 2 THEN Center 23, "1.-[ @=" + Prov1 + " ] 2.-[ #=" + Prov2 + " ] 3.-[ $=" + Prov3 + " ]"
IF sta% = 1 THEN Center 2, " Proceso de Marcar Productos para que sean Fallas de Fallas " ELSE Center 2, " Proceso de Marcaje de Articulos en Oferta de Proveedores "

DO WHILE NOT salir
     SELECT CASE sit%
          CASE 1
               IF sta% = 2 THEN
                    COLOR colors(5, colorpref), colors(4, colorpref)
                    LOCATE 4, 2: PRINT "Introduzca n£mero de Proveedor [1,2 o 3]:"
                    Drog1 = MeteDatoLetra(4, 45, 1, "0")
                    IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DEF
                    IF VAL(Drog1) < 1 OR 3 < VAL(Drog1) THEN sit% = 1
                    IF Drog1 = "1" THEN Center 24, "  Proveedor Elejido: 1.-" + Prov1 + "  "
                    IF Drog1 = "2" THEN Center 24, "  Proveedor Elejido: 2.-" + Prov2 + "  "
                    IF Drog1 = "3" THEN Center 24, "  Proveedor Elejido: 3.-" + Prov3 + "  "
               ELSE
                    sit% = 2
               END IF
          CASE 2
               Prud1 = MeteDatoLetra(4, 56, 4, Prud1)
               Prud1 = Trim$(Prud1)
               IF Fun(9) = "si" THEN EXIT DEF
               IF devolver = true THEN devolver = false: sit% = 1 ELSE salir = true
               IF Prud1 = "" THEN sit% = 2
     END SELECT
LOOP
Center 5, "ÄÄDescripcion" + STRING$(22, "Ä") + "ÂObservacionesÄÄÄÄÄÄÄÂÄÄÄÂÄCondicionÄÄÄÄÄÄÄÄÄ"
Center 23, "ÄÄDescripcion" + STRING$(22, "Ä") + "ÁObservacionesÄÄÄÄÄÄÄÁÄÄÄÁÄCondicionÄÄÄÄÄÄÄÄÄ"
Center 25, "[F1]Ayuda  [F5÷F8]Marcar\Desmarcar  [F9]Salir y Salvar  [F10]Reiniciar": salir = false
Center 24, "Use las Teclas de Direcci¢n , PageUp y PageDown para Situar la Linea Cursora"
CLOSE : OPEN "f:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
OPEN "f:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 8
FIELD #2, 6 AS nomshort$, 2 AS num$
max2% = LOF(1) / 8: apu% = 0
FOR k = 1 TO max2%
GET #2, k: IF MID$(nomshort$, 1, LEN(Prud1)) = Prud1 THEN apu% = k
NEXT k
IF apu% = 0 THEN Center 24, "No Se Encontraron Articulos Con Ese Nombre . . .": Espera: CLOSE : EXIT DEF
DO WHILE NOT salir
     COLOR 7, 0: l = 5
     FOR k = apu% - 8 TO apu% + 8
     l = l + 1
     IF k = apu% THEN COLOR 0, 7
     LOCATE l, 1
     PRINT SPACE$(80)
     LOCATE l, 1
     IF k > 0 AND k <= max2% THEN
          GET #2, k
          IF MID$(nomshort$, 1, LEN(Prud1)) >= Prud1 THEN
               GET #1, CVI(num$)
               IF sta% = 1 THEN
                    PRINT nomb2$; "³"; obser2$; "³ "; re2$; " ³";
                    IF re2$ = "*" THEN PRINT " Falla de Falla "
               ELSE
                    IF Drog1 = "1" THEN PRINT nomb2$; "³"; obser2$; "³ "; of12$; " ³"; : IF of12$ = "@" THEN PRINT " "; Prov1
                    IF Drog1 = "2" THEN PRINT nomb2$; "³"; obser2$; "³ "; of22$; " ³"; : IF of22$ = "#" THEN PRINT " "; Prov2
                    IF Drog1 = "3" THEN PRINT nomb2$; "³"; obser2$; "³ "; of32$; " ³"; : IF of32$ = "$" THEN PRINT " "; Prov3
               END IF
          END IF
     ELSE
          PRINT SPACE$(35); "³                    ³   ³"
     END IF
     IF k = apu% THEN COLOR 7, 0
     NEXT k
     GET #1, apu%   ': LOCK #1, apu%
     seguir = true
     DO WHILE seguir: llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
          SELECT CASE llave$
               CASE CHR$(0) + "H": seguir = false: apu% = apu% - 1: IF apu% < 1 THEN apu% = 1
               CASE CHR$(0) + "P": seguir = false: apu% = apu% + 1: IF apu% > max2% THEN apu% = max2%
               CASE CHR$(0) + CHR$(73):  apu% = apu% - 8: IF apu% < 1 THEN apu% = 1
               CASE CHR$(0) + CHR$(81):  apu% = apu% + 8: IF apu% > max2% THEN apu% = max2%
               CASE CHR$(0) + CHR$(63): GOSUB Salva
               CASE CHR$(0) + CHR$(64): GOSUB Salva
               CASE CHR$(0) + CHR$(65): GOSUB Salva
               CASE CHR$(0) + CHR$(66): GOSUB Salva
               CASE CHR$(0) + CHR$(67): seguir = false: Fun(9) = "si": CLOSE : EXIT DEF
               CASE CHR$(0) + CHR$(68): seguir = false: Fun(10) = "si": salir = true
               CASE ELSE
          END SELECT
     LOOP
LOOP
EXIT DEF

Salva:
     Fun(5) = "no": Fun(6) = "no": Fun(7) = "no": Fun(8) = "no": seguir = false
     IF sta% = 1 THEN IF re2$ = "*" THEN RSET re2$ = " " ELSE RSET re$ = "*"
     IF sta% = 2 THEN
        IF of12$ = "@" AND Drog1 = "1" THEN RSET of12$ = " " ELSE IF Drog1 = "1" THEN RSET of12$ = "@"
        IF of22$ = "#" AND Drog1 = "2" THEN RSET of22$ = " " ELSE IF Drog1 = "2" THEN RSET of22$ = "#"
        IF of32$ = "$" AND Drog1 = "3" THEN RSET of32$ = " " ELSE IF Drog1 = "3" THEN RSET of32$ = "$"
     END IF
     'UNLOCK #1, apu%
     PUT #1, apu%
RETURN

