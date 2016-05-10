DEFINT A-Z
DECLARE SUB Espera ()
DECLARE SUB Detal (state%)
DECLARE SUB LeeEstado ()
DECLARE SUB Center (row%, text$)
DECLARE SUB Box (row1%, col1%, row2%, col2%)
DECLARE SUB Borra ()
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$, tr$)
DECLARE FUNCTION Escoge% (fila%, edo%, fila2%, Prod$, caso%)
DECLARE FUNCTION Trim$ (x$)
COMMON SHARED guia, colorpref AS INTEGER
DIM SHARED turno AS STRING * 1, devolver
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, Impuesto AS STRING * 6, Nombfarma AS STRING * 25, Direccion AS STRING * 30, Rif     AS STRING * 25, Nit      AS STRING * 25, Telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2
DIM SHARED Fun(17) AS STRING * 2, salir, Turna, terminar, sitio, busqueda
REM $DYNAMIC
DIM SHARED Nombra(5) AS STRING * 35, Pvp1a(5) AS STRING * 8, Pvp2a(5) AS STRING * 8, Pvp3a(5) AS STRING * 8, Pvp4a(5) AS STRING * 8, Numa(5) AS STRING * 2, Observa(5) AS STRING * 20, Exis1a(5) AS STRING * 2, Exis2a(5) AS STRING * 2, Exis3a(5) AS  _
STRING * 2, Exis4a(5) AS STRING * 2
DIM SHARED Cant1a(5) AS STRING * 2, Cant2a(5) AS STRING * 2, Cant3a(5) AS STRING * 2, Cant4a(5) AS STRING * 2, grabGav AS STRING * 1
CONST true = -1: CONST false = NOT true: busqueda = 0
ON TIMER(5) GOSUB AbreGav
TIMER ON
LeeEstado
IF guia < 3 THEN Detal (guia)
IF guia = 3 THEN CALL Borra
CLOSE : COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... ": COLOR colors(7, colorpref), colors(4, colorpref)
CHAIN "F:\kevinprg\kevin01.bas"
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
'    scrn  dots  bar  back   title  shdow  choice  curs   cursbk  shdow
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
SUB Borra
'RegisResp
CLS : Box 1, 1, 3, 80: Fun(2) = "no": Fun(3) = "no": Center 1, "Borrar o Recuperar Un Articulo": Center 25, "[F1]Ayuda  [F2]Borrar  [F3]Recuperar  [F9]Salir": Center 2, " Presione [F1]Ayuda  [F2]Borrar  [F3]Recuperar  [F9]Salir"
seguir = true
DO WHILE seguir = true: llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
        SELECT CASE llave$
            CASE CHR$(0) + CHR$(60): Fun(2) = "si": seguir = false
            CASE CHR$(0) + CHR$(61): Fun(3) = "si": seguir = false
            CASE CHR$(0) + CHR$(67): seguir = false
            CASE ELSE: BEEP
        END SELECT
LOOP: LOCATE 2, 2: PRINT SPACE$(78)
IF Fun(2) = "si" THEN LOCATE 2, 2: PRINT "¨Desea buscar por Nombre ¢ por C¢digo? Pulse N ¢ C ": Center 25, "Pulse N para buscar por nombre y C por C¢digo  Pulse [Esc] para Abortar"
IF Fun(3) = "si" THEN LOCATE 2, 2: PRINT "Escoja uno de estos productos para recuperarlo"
IF Fun(2) = "si" THEN
     kdb$ = "": salir = false
     DO
          kdb$ = INKEY$: kdb$ = UCASE$(kdb$)
          SELECT CASE kdb$
               CASE "N"
                    Box 1, 1, 3, 80: Center 1, "Borrar UN Articulo de la base de datos"
                    ElegMod% = 0: ElegMod% = Escoge%(9, 1, 2, "      ", 0)
                    IF Fun(9) = "si" THEN Fun(9) = "no": Fun(2) = "no": Fun(3) = "no": EXIT SUB
                    IF ElegMod% = 0 THEN Fun(2) = "no": Fun(3) = "no": EXIT SUB ELSE salir = true
               CASE "C"
               CASE CHR$(27): EXIT SUB
               CASE ELSE: BEEP
          END SELECT
     IF salir = true THEN EXIT DO
     LOOP
     CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
     FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS  _
cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$ _
, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
     GET #1, ElegMod%
     IF CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$) = 0 THEN
          RSET acti2$ = "*": PUT #1, ElegMod%: CLOSE
          Center 18, "Este producto ha sido inactivado , pero aun permanece en disco duro.": Center 20, "Si desea recuperarlo use otra vez esta opcion.": Espera
          ' ojo arreglar los indexados
     ELSE
          Center 18, "Este Producto Tiene Existencia Diferente a Cero:": Center 19, "Por lo Tanto NO SE PUEDE BORRAR . . .": Espera: CLOSE : EXIT SUB
     END IF
END IF
IF Fun(3) = "si" THEN
     ElegMod% = 0: ElegMod% = Escoge%(9, 7, 2, "", 0)
     IF Fun(9) = "si" THEN Fun(9) = "no": Fun(2) = "no": Fun(3) = "no": EXIT SUB
     IF ElegMod% <= 0 OR ElegMod% >= max2% THEN Fun(2) = "no": Fun(3) = "no": EXIT SUB
     CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
     FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS  _
cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$ _
, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
     GET #1, ElegMod%: RSET acti2$ = " ": PUT #1, ElegMod%: CLOSE
     ' ojo arreglar los indexados ojo
END IF
END SUB

SUB Box (row1%, col1%, row2%, col2%) STATIC
BoxWidth% = col2% - col1% + 1: LOCATE row1%, col1%: PRINT "Ú"; STRING$(BoxWidth% - 2, "Ä"); "¿"; : FOR a = row1% + 1 TO row2% - 1: LOCATE a, col1%: PRINT "³"; SPACE$(BoxWidth% - 2); "³"; : NEXT a: LOCATE row2%, col1%: PRINT "À"; STRING$(BoxWidth% -  _
2, "Ä"); "Ù";
END SUB

SUB Center (row%, text$)
IF row% = 25 THEN COLOR colors(5, colorpref), colors(4, colorpref): LOCATE 25, 1: PRINT SPACE$(80);
LOCATE row%, 41 - LEN(text$) / 2: PRINT text$;
END SUB

SUB Detal (state%)
COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box 1, 1, 3, 80
ON ERROR GOTO ErrorTrap
IF state% = 1 THEN
     Center 2, "Proceso de Detallar: Mueve Existencia de Dispensadores a Gavetas"
ELSE
     Center 2, " Proceso :  Envasar Productos Quimicos , Mueve Existencia a Envasados "
END IF: Center 14, "Escoja el articulo detallado que va a surtir"
Eleg% = Escoge%(6, 1, 4, "      ", 0)
IF Eleg% <= 0 THEN Center 4, "NO Se Encontro Articulos...": Espera: EXIT SUB

CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
GET #1, Eleg%: LOCK #1, Eleg%
'IF state% = 1 THEN
'     IF CVI(ubic2$) <> 2 THEN Box 10, 10, 12, 70: Center 11, "Este producto NO esta ubicado en Detallados": Espera: EXIT SUB
'ELSE
'     IF CVI(ubic2$) <> 3 THEN Box 10, 10, 12, 70: Center 11, "Este producto NO esta ubicado en Envasados.": Espera: EXIT SUB
'END IF
IF MID$(obser2$, 1, 1) <> "[" THEN Box 10, 10, 12, 70: Center 11, "Debe colocarle observaciones a este producto:abre corchete": Espera: EXIT SUB
IF MID$(obser2$, 5, 1) <> "]" THEN Box 10, 10, 12, 70: Center 11, "Debe colocarle observaciones a este producto:cierra corchete": Espera: EXIT SUB
LOCATE 12, 65: PRINT USING "####,"; CVI(exi42$); : PRINT USING "######,.##"; CVD(pv42$);
LOCATE 13, 1: PRINT nomb2$; : mbre$ = MID$(nomb2$, 1, 6): canti1% = VAL(MID$(obser2$, 2, 3))
PRINT USING "####,"; CVI(exi12$); : PRINT USING "######,.##"; CVD(pv12$);
PRINT USING "####,"; CVI(exi22$); : PRINT USING "######,.##"; CVD(pv22$);
PRINT USING "####,"; CVI(exi32$); : PRINT USING "######,.##"; CVD(pv32$);
FOR k = 6 TO 9: LOCATE k, 1: PRINT SPACE$(80): NEXT k: CLOSE
Center 14, "Escoja de que Articulo surtira el detallado "
IF state% = 2 THEN mbre$ = "*" + MID$(mbre$, 1, 5)
Eleg2% = Escoge(6, 10, 2, mbre$, 1)
IF Eleg2% = 0 THEN Eleg2% = Escoge(6, 10, 2, MID$(mbre$, 1, 6), 0)
IF Eleg2% = 0 THEN
     Center 24, "No Se Hallaron Otros Articulos Con El MISMO Nombre de Donde Detallar/Envasar"
     EXIT SUB
END IF
IF Eleg% = Eleg2% THEN Box 10, 10, 12, 70: Center 11, "Debe tomar diferentes Articulos": Espera: EXIT SUB

CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
LOCK #1, Eleg% : GET #1, Eleg2%: LOCK #1, Eleg2%
IF MID$(obser2$, 1, 1) <> "[" THEN Box 10, 10, 12, 70: Center 11, "Debe colocarle observaciones a este producto:abre corchete": Espera: EXIT SUB
IF MID$(obser2$, 5, 1) <> "]" THEN Box 10, 10, 12, 70: Center 11, "Debe colocarle observaciones a este producto:cierra corchete": Espera: EXIT SUB
canti2% = VAL(MID$(obser2$, 2, 3)) 'cantidad de pastillas o gramos o mililitros
FOR k = 6 TO 10: LOCATE k, 1: PRINT SPACE$(80): NEXT k
IF state% = 1 THEN
     IF (canti2% / canti1%) - FIX((canti2% / canti1%)) > 0 THEN Box 10, 10, 12, 70: Center 11, "No son multiplos las  cantidades en observaciones.": Espera: EXIT SUB
     canti% = canti2% / canti1%
ELSE
     IF (canti1% / canti2%) - FIX((canti1% / canti2%)) > 0 THEN Box 10, 10, 12, 70: Center 11, "No son multiplos las  cantidades en observaciones.": Espera: EXIT SUB
     canti% = canti1% / canti2%
END IF
LOCATE 15, 1: PRINT SPACE$(80): Center 25, "[F1]Ayuda  [Enter]Escoger  [F9]Salir SIN Salvar"
LOCATE 14, 1
IF state% = 1 THEN
     PRINT " sera surtido del siguiente dispensador : Escoja el PVP del mismo "
ELSE
     PRINT " sera envasado del siguiente quimico : Escoja el PVP del mismo"
END IF
GOSUB EscojePvp

SELECT CASE sit%
     CASE 1: IF state% = 1 THEN prize# = CVD(pv12$) / canti%: costo$ = MKD$(CVD(cos12$) / canti%) ELSE prize# = CVD(pv12$) * canti%: costo$ = cos12$
     CASE 2: IF state% = 1 THEN prize# = CVD(pv22$) / canti%: costo$ = MKD$(CVD(cos22$) / canti%) ELSE prize# = CVD(pv22$) * canti%: costo$ = cos22$
     CASE 3: IF state% = 1 THEN prize# = CVD(pv32$) / canti%: costo$ = MKD$(CVD(cos32$) / canti%) ELSE prize# = CVD(pv32$) * canti%: costo$ = cos32$
     CASE 4: IF state% = 1 THEN prize# = CVD(pv32$) / canti%: costo$ = MKD$(CVD(cos42$) / canti%) ELSE prize# = CVD(pv32$) * canti%: costo$ = cos42$
END SELECT
FOR k = 17 TO 23: LOCATE k, 1: PRINT SPACE$(80): NEXT k   'EntraDatoNumero$(9, 9, 10, STR$((FIX(CVD(pv12$) * 100)) / 100), 0)
LOCATE 18, 1: PRINT " El precio por unidad es de Bs. "; : PRINT USING "######,.##"; prize#; : PRINT SPACE$(25)
Center 25, " Introduzca el  precio ajustado [F9] Abortar "
LOCATE 19, 1: PRINT " Desea ajustar este precio? ":
LOCATE 14, 1: PRINT " sera surtido del siguiente dispensador :                         "
LOCATE 15, 1: PRINT nomb2$; : PRINT USING "####,"; CVI(exi12$);
PRINT USING "######,.##"; CVD(pv12$); : PRINT USING "####,"; CVI(exi22$);
PRINT USING "######,.##"; CVD(pv22$); : PRINT USING "####,"; CVI(exi32$);
PRINT USING "######,.##"; CVD(pv32$); : precio$ = MKD$(0): salir = false
DO WHILE salir = false
   precio$ = MKD$(VAL(MeteDatoLetra$(19, 30, 10, STR$((FIX(prize# * 100)) / 100), "0")))
   IF CVD(precio$) < CVD(costo$) THEN
      Center 25, "El precio no puede ser menor al costo"
      IF sonido = "si" THEN BEEP: BEEP
   ELSE
      salir = true
   END IF
LOOP
IF Fun(9) = "si" THEN CLOSE : EXIT SUB
IF state% = 1 THEN
     Center 25, " Introduzca Cantidad de Dispensadores a ser detallados [F9]Salir SIN Salvar"
ELSE
     Center 25, " Introduzca Cantidad de Papeletas a ser Envasadas [F9]Salir SIN Salvar"
END IF
LOCATE 20, 1: prize# = CVD(precio$)
IF state% = 1 THEN
     PRINT "¨Cuantos Dispensadores desea detallar? "
ELSE
     PRINT "¨Cuantos papeletas o frascos desea envasar? "
END IF: canti2$ = MKI$(0): salir = false
DO WHILE NOT salir
canti2$ = MKI$(VAL(MeteDatoLetra$(20, 55, 4, " 1  ", "1")))
IF Fun(9) = "si" THEN Fun(9) = "no": CLOSE : EXIT SUB
IF negativo = "no" THEN
     SELECT CASE sit%
          CASE 1
          IF state% = 1 THEN
               IF CVI(canti2$) <= CVI(exi12$) AND CVI(canti2$) > 0 THEN salir = true ELSE Center 25, "NO Puede Trabajar en Negativo o Tipee Una Cantidad Mayor a Cero"
          ELSE
               IF (CVI(canti2$) * canti%) <= CVI(exi12$) AND CVI(canti2$) > 0 THEN salir = true ELSE Center 25, "NO Puede Trabajar en Negativo o Tipee Una Cantidad Mayor a Cero"
          END IF
          CASE 2
          IF state% = 1 THEN
               IF CVI(canti2$) <= CVI(exi22$) AND CVI(canti2$) > 0 THEN salir = true ELSE Center 25, "NO Puede Trabajar en Negativo o Tipee Una Cantidad Mayor a Cero"
          ELSE
               IF (CVI(canti2$) * canti%) <= CVI(exi22$) AND CVI(canti2$) > 0 THEN salir = true ELSE Center 25, "NO Puede Trabajar en Negativo o Tipee Una Cantidad Mayor a Cero"
          END IF
          CASE 3
          IF state% = 1 THEN
               IF CVI(canti2$) <= CVI(exi32$) AND CVI(canti2$) > 0 THEN salir = true ELSE Center 25, "NO Puede Trabajar en Negativo o Tipee Una Cantidad Mayor a Cero"
          ELSE
               IF (CVI(canti2$) * canti%) <= CVI(exi32$) AND CVI(canti2$) > 0 THEN salir = true ELSE Center 25, "NO Puede Trabajar en Negativo o Tipee Una Cantidad Mayor a Cero"
          END IF
          CASE 4
          IF state% = 1 THEN
               IF CVI(canti2$) <= CVI(exi42$) AND CVI(canti2$) > 0 THEN salir = true ELSE Center 25, "NO Puede Trabajar en Negativo o Tipee Una Cantidad Mayor a Cero"
          ELSE
               IF (CVI(canti2$) * canti%) <= CVI(exi42$) AND CVI(canti2$) > 0 THEN salir = true ELSE Center 25, "NO Puede Trabajar en Negativo o Tipee Una Cantidad Mayor a Cero"
          END IF
     END SELECT
END IF
LOOP
IF state% = 1 THEN
     IF sit% = 1 THEN RSET exi12$ = MKI$(CVI(exi12$) - CVI(canti2$))
     IF sit% = 2 THEN RSET exi22$ = MKI$(CVI(exi22$) - CVI(canti2$))
     IF sit% = 3 THEN RSET exi32$ = MKI$(CVI(exi32$) - CVI(canti2$))
     IF sit% = 4 THEN RSET exi42$ = MKI$(CVI(exi42$) - CVI(canti2$))
ELSE
     IF sit% = 1 THEN RSET exi12$ = MKI$(CVI(exi12$) - (CVI(canti2$) * canti%))
     IF sit% = 2 THEN RSET exi22$ = MKI$(CVI(exi22$) - (CVI(canti2$) * canti%))
     IF sit% = 3 THEN RSET exi32$ = MKI$(CVI(exi32$) - (CVI(canti2$) * canti%))
     IF sit% = 4 THEN RSET exi42$ = MKI$(CVI(exi42$) - (CVI(canti2$) * canti%))
END IF
OPEN "F:\kevinprg\kevindat\bitacora.dat" FOR RANDOM AS #17 LEN = 40: FIELD #17, 40 AS tod$: max% = (LOF(17) / 40) + 1
LSET tod$ = codigo2$ + MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4) + MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MKI$(-1 * CVI(canti2$)) + "20 disp a det": PUT #17, max%: CLOSE #17
'ojo bitacora para envasados
PUT #1, Eleg2%: UNLOCK #1, Eleg2%
GET #1, Eleg%   'prize#,costo$,canti2$
     IF CVI(exi12$) = 0 THEN
          RSET pv12$ = MKD$(0): RSET cos12$ = MKD$(0)
          RSET ven11$ = " ": RSET ven12$ = "  "
     END IF
     IF CVI(exi22$) = 0 THEN
          RSET pv22$ = MKD$(0): RSET cos22$ = MKD$(0)
          RSET ven21$ = " ": RSET ven22$ = "  "
     END IF
     IF CVI(exi32$) = 0 THEN RSET ven31$ = " ": RSET ven32$ = "  "
     IF prize# <> CVD(pv42$) THEN
      IF CVI(exi42$) = 0 THEN
          RSET pv42$ = MKD$(0): RSET cos42$ = MKD$(0)
          RSET ven41$ = " ": RSET ven42$ = "  "
      END IF
     END IF
          IF CVD(pv32$) = prize# THEN GOSUB Carga3
          IF CVD(pv42$) = prize# AND devolver = false THEN GOSUB Carga4
          IF CVD(pv22$) = prize# AND devolver = false THEN GOSUB Carga2
          IF CVD(pv12$) = prize# AND devolver = false THEN GOSUB Carga1
          IF CVI(exi12$) = 0 AND devolver = false THEN
               IF state% = 1 THEN RSET cos12$ = costo$: RSET exi12$ = MKI$(CVI(canti2$) * canti%) ELSE RSET cos12$ = MKD$(CVD(costo$) * canti%): RSET exi12$ = canti2$
               devolver = true: RSET pv12$ = MKD$(prize#)
          END IF
          IF CVI(exi22$) = 0 AND devolver = false THEN
               IF state% = 1 THEN RSET cos22$ = costo$: RSET exi22$ = MKI$(CVI(canti2$) * canti%) ELSE RSET cos22$ = MKD$(CVD(costo$) * canti%): RSET exi22$ = canti2$
               devolver = true: RSET pv22$ = MKD$(prize#)
          END IF
          IF CVI(exi32$) = 0 AND devolver = false THEN
               IF state% = 1 THEN RSET cos32$ = costo$: RSET exi32$ = MKI$(CVI(canti2$) * canti%) ELSE RSET cos32$ = MKD$(CVD(costo$) * canti%): RSET exi32$ = canti2$
               devolver = true: RSET pv32$ = MKD$(prize#)
          END IF
     sitio = true
     IF CVI(exi42$) = 0 THEN
          RSET pv42$ = MKD$(0)
          RSET cos42$ = MKD$(0)
          RSET ven41$ = " "
          RSET ven42$ = "  "
          IF CVD(pv12$) > CVD(pv22$) AND CVD(pv12$) < CVD(pv32$) THEN GOSUB Cambia1y2: sitio = false
          IF sitio = true THEN IF CVD(pv12$) < CVD(pv22$) AND CVD(pv22$) < CVD(pv32$) THEN sitio = false
          IF sitio = true THEN IF CVD(pv12$) < CVD(pv22$) AND CVD(pv12$) > CVD(pv32$) THEN GOSUB Cambia1y2: GOSUB Cambia1y3: sitio = false
          IF sitio = true THEN IF CVD(pv12$) > CVD(pv22$) AND CVD(pv22$) < CVD(pv32$) THEN GOSUB Cambia1y2: GOSUB Cambia2y3: sitio = false
          IF sitio = true THEN IF CVD(pv12$) < CVD(pv22$) AND CVD(pv22$) > CVD(pv32$) THEN GOSUB Cambia2y3: sitio = false
          IF sitio = true THEN IF CVD(pv12$) > CVD(pv22$) AND CVD(pv22$) > CVD(pv32$) THEN GOSUB Cambia1y3: sitio = false
          IF sitio = true THEN IF CVD(pv12$) = CVD(pv22$) AND CVD(pv22$) < CVD(pv32$) THEN sitio = false
          IF sitio = true THEN IF CVD(pv12$) = CVD(pv32$) AND CVD(pv22$) > CVD(pv32$) THEN GOSUB Cambia2y3: sitio = false
          IF sitio = true THEN IF CVD(pv12$) > CVD(pv22$) AND CVD(pv22$) = CVD(pv32$) THEN GOSUB Cambia1y3: sitio = false
          IF sitio = true THEN IF CVD(pv12$) = CVD(pv22$) AND CVD(pv22$) > CVD(pv32$) THEN GOSUB Cambia1y3: sitio = false
          IF sitio = true THEN IF CVD(pv12$) = CVD(pv32$) AND CVD(pv22$) < CVD(pv32$) THEN GOSUB Cambia1y2: sitio = false
          IF sitio = true THEN IF CVD(pv22$) = CVD(pv32$) AND CVD(pv12$) < CVD(pv32$) THEN sitio = false
     ELSE
          IF CVD(pv12$) > CVD(pv22$) AND CVD(pv12$) < CVD(pv32$) THEN GOSUB Cambia1y2: sitio = false
          IF sitio = true THEN IF CVD(pv12$) < CVD(pv22$) AND CVD(pv22$) < CVD(pv32$) THEN sitio = false
          IF sitio = true THEN IF CVD(pv12$) = CVD(pv22$) AND CVD(pv22$) < CVD(pv32$) THEN sitio = false
          IF sitio = true THEN IF CVD(pv12$) = CVD(pv32$) AND CVD(pv22$) < CVD(pv32$) THEN GOSUB Cambia1y2: sitio = false
          IF sitio = true THEN IF CVD(pv22$) = CVD(pv32$) AND CVD(pv12$) < CVD(pv32$) THEN sitio = false
     END IF
OPEN "F:\kevinprg\kevindat\bitacora.dat" FOR RANDOM AS #17 LEN = 40: FIELD #17, 40 AS tod$: max% = (LOF(17) / 40) + 1
LSET tod$ = codigo2$ + MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4) + MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MKI$(canti% * CVI(canti2$)) + "21 vino de disp": PUT #17, max%: CLOSE #17
PUT #1, Eleg%: UNLOCK #1, Eleg% : CLOSE
IF state% = 2 THEN
     CALL Box(4, 1, 14, 80): Center 5, "¨Desea Ajustar tambien la existencia de frascos/bolsas utilizados?": Center 6, "Pulse S ¢ N"
     DO
          llave$ = "": DO WHILE llave$ = "": llave$ = INKEY$: LOOP: llave$ = UCASE$(llave$)
          IF llave$ = "S" OR llave$ = "N" THEN EXIT DO
     LOOP
     IF llave$ = "S" THEN
          Eleg2% = 0
          DO WHILE Eleg2% <= 0
               CALL Center(25, "DEBE Escojer UN Articulo"): Eleg2% = Escoge%(6, 1, 4, "      ", 0)
          LOOP
          CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
          FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8  _
AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
          GET #1, Eleg2%: LOCK #1, Eleg2% : Center 25, "Escoja el PVP con las teclas de direccion": GOSUB EscojePvp
          IF sit% = 1 THEN RSET exi12$ = MKI$(CVI(exi12$) - CVI(canti2$)): costo$ = cos12$: IF CVI(exi12$) < 0 THEN RSET exi12$ = MKI$(0)
          IF sit% = 2 THEN RSET exi22$ = MKI$(CVI(exi22$) - CVI(canti2$)): costo$ = cos22$: IF CVI(exi22$) < 0 THEN RSET exi22$ = MKI$(0)
          IF sit% = 3 THEN RSET exi32$ = MKI$(CVI(exi32$) - CVI(canti2$)): costo$ = cos32$: IF CVI(exi32$) < 0 THEN RSET exi32$ = MKI$(0)
          IF sit% = 4 THEN RSET exi42$ = MKI$(CVI(exi42$) - CVI(canti2$)): costo$ = cos42$: IF CVI(exi42$) < 0 THEN RSET exi42$ = MKI$(0)
          PUT #1, Eleg2%: UNLOCK #1, Eleg2%
          GET #1, Eleg%: LOCK #1, Eleg%
          IF prize# = CVD(pv12$) THEN
               RSET cos12$ = MKD$(CVD(cos12$) + CVD(costo$))
               IF CVD(pv12$) * .7 <= CVD(cos12$) THEN prize# = CVD(pv12$): GOSUB Ajustar: RSET pv12$ = MKD$(prize#)
          END IF
          IF prize# = CVD(pv22$) THEN
               RSET cos22$ = MKD$(CVD(cos22$) + CVD(costo$))
               IF CVD(pv22$) * .7 <= CVD(cos22$) THEN prize# = CVD(pv22$): GOSUB Ajustar: RSET pv22$ = MKD$(prize#)
          END IF
          IF prize# = CVD(pv32$) THEN
               RSET cos32$ = MKD$(CVD(cos32$) + CVD(costo$))
               IF CVD(pv32$) * .7 <= CVD(cos32$) THEN prize# = CVD(pv32$): GOSUB Ajustar: RSET pv32$ = MKD$(prize#)
          END IF
          IF prize# = CVD(pv42$) THEN
               RSET cos42$ = MKD$(CVD(cos42$) + CVD(costo$))
               IF CVD(pv42$) * .7 <= CVD(cos42$) THEN prize# = CVD(pv42$): GOSUB Ajustar: RSET pv42$ = MKD$(prize#)
          END IF
          PUT #1, Eleg%: UNLOCK #1, Eleg%
     END IF
END IF
Box 21, 5, 23, 75: Center 22, " El Proceso se realiz¢ sin Problemas "
CALL Espera
EXIT SUB

Cambia1y2:
Auxi$ = pv12$: RSET pv12$ = pv22$: RSET pv22$ = Auxi$
Auxi$ = cos12$: RSET cos12$ = cos22$: RSET cos22$ = Auxi$
Auxi$ = exi12$: RSET exi12$ = exi22$: RSET exi22$ = Auxi$
Auxi$ = ven11$: RSET ven21$ = ven11$: RSET ven11$ = Auxi$
Auxi$ = ven12$: RSET ven22$ = ven12$: RSET ven12$ = Auxi$
RETURN
Cambia1y3:
Auxi$ = pv12$: RSET pv12$ = pv32$: RSET pv32$ = Auxi$
Auxi$ = cos12$: RSET cos12$ = cos32$: RSET cos32$ = Auxi$
Auxi$ = exi12$: RSET exi12$ = exi32$: RSET exi32$ = Auxi$
Auxi$ = ven11$: RSET ven31$ = ven11$: RSET ven11$ = Auxi$
Auxi$ = ven12$: RSET ven32$ = ven12$: RSET ven32$ = Auxi$
RETURN
Cambia2y3:
Auxi$ = pv22$: RSET pv22$ = pv32$: RSET pv32$ = Auxi$
Auxi$ = cos22$: RSET cos22$ = cos32$: RSET cos32$ = Auxi$
Auxi$ = exi22$: RSET exi22$ = exi32$: RSET exi32$ = Auxi$
Auxi$ = ven21$: RSET ven21$ = ven31$: RSET ven31$ = Auxi$
Auxi$ = ven22$: RSET ven22$ = ven32$: RSET ven32$ = Auxi$
RETURN
Carga4:
RSET cos42$ = MKD$((CVI(exi42$) * CVD(cos42$) + CVD(costo$) * CVI(canti2$) * canti%) / (CVI(exi42$) + CVI(canti2$) * canti%))
IF state% = 1 THEN RSET exi42$ = MKI$(CVI(exi42$) + CVI(canti2$) * canti%) ELSE RSET exi42$ = MKI$(CVI(exi42$) + CVI(canti2$))
devolver = true
RETURN
Carga3:
IF state% = 1 THEN
     RSET exi32$ = MKI$(CVI(exi32$) + CVI(canti2$) * canti%)
     RSET cos32$ = MKD$((CVI(exi32$) * CVD(cos32$) + (CVD(costo$) * CVI(canti2$) * canti%)) / (CVI(exi32$) + (CVI(canti2$) * canti%)))
ELSE
     RSET exi32$ = MKI$(CVI(exi32$) + CVI(canti2$))
     RSET cos32$ = MKD$((CVI(exi32$) * CVD(cos32$) + (CVD(costo$) * CVI(canti2$) * canti%)) / (CVI(exi32$) + CVI(canti2$)))
END IF: devolver = true
RETURN
Carga2:
IF state% = 1 THEN
     RSET exi22$ = MKI$(CVI(exi22$) + CVI(canti2$) * canti%)
     RSET cos22$ = MKD$((CVI(exi22$) * CVD(cos22$) + (CVD(costo$) * CVI(canti2$) * canti%)) / (CVI(exi22$) + (CVI(canti2$) * canti%)))
ELSE
     RSET exi22$ = MKI$(CVI(exi22$) + CVI(canti2$))
     RSET cos22$ = MKD$((CVI(exi22$) * CVD(cos22$) + (CVD(costo$) * CVI(canti2$) * canti%)) / (CVI(exi22$) + CVI(canti2$)))
END IF: devolver = true
RETURN
Carga1:
IF state% = 1 THEN
     RSET exi12$ = MKI$(CVI(exi12$) + CVI(canti2$) * canti%)
     RSET cos12$ = MKD$((CVI(exi12$) * CVD(cos12$) + (CVD(costo$) * CVI(canti2$) * canti%)) / (CVI(exi12$) + (CVI(canti2$) * canti%)))
ELSE
     RSET exi12$ = MKI$(CVI(exi12$) + CVI(canti2$))
     RSET cos12$ = MKD$((CVI(exi12$) * CVD(cos12$) + (CVD(costo$) * CVI(canti2$) * canti%)) / (CVI(exi12$) + CVI(canti2$)))
END IF: devolver = true
RETURN
EscojePvp:
COLOR colors(7, colorpref), colors(4, colorpref)
FOR k = 14 TO 17: LOCATE k, 1: PRINT SPACE$(80): NEXT k: sit% = 3
LOCATE 17, 1: PRINT "Codigo Nombre                             Ofertas Observaciones"
LOCATE 18, 1: PRINT codigo2$; " ";
COLOR colors(7, colorpref) + 16, colors(4, colorpref): PRINT nomb2$; : COLOR colors(4, colorpref), colors(7, colorpref)
PRINT " "; of12$; " "; of22$; " "; of32$; obser2$
Box 19, 1, 22, 80: wer$ = "En Oferta:" + STRING$(24, "Ä") + "CAN1ÄÄÄÄÄÄPVP1ÂCAN2" + STRING$(6, "Ä") + "PVP2ÂCAN3" + STRING$(5, "Ä") + "PVP3"
LOCATE 19, 2: PRINT wer$; : LOCATE 21, 2: PRINT "Costos:"
LOCATE 20, 36: PRINT USING "####"; CVI(exi12$); : LOCATE 20, 50: PRINT "³"; : PRINT USING "####"; CVI(exi22$); : LOCATE 20, 65: PRINT "³"; : PRINT USING "####"; CVI(exi32$)
LOCATE 20, 40: PRINT USING "######,.##"; CVD(pv12$): LOCATE 20, 55: PRINT USING "######,.##"; CVD(pv22$): LOCATE 20, 70: PRINT USING "######,.##"; CVD(pv32$)
LOCATE 20, 2: PRINT USING "####"; CVI(exi42$); : PRINT " a Bs."; : PRINT USING "######,.##"; CVD(pv42$)
LOCATE 21, 40: PRINT USING "######,.##"; CVD(cos12$); : PRINT "³"; : LOCATE 21, 55: PRINT USING "######,.##"; CVD(cos22$); : PRINT "³"; : LOCATE 21, 70: PRINT USING "######,.##"; CVD(cos32$); : PRINT "³";
LOCATE 21, 12: PRINT USING "######,.##"; CVD(cos42$): LOCATE 22, 44: PRINT "IVA:"; iv12$; "ÄÁ"; : LOCATE 22, 59: PRINT "IVA:"; iv22$; "ÄÁ"; : LOCATE 22, 74: PRINT "IVA:"; iv32$
LOCATE 23, 1: PRINT "[Minimo1="; : PRINT USING "####"; CVI(mini12$); : PRINT "] [Minimo2="; : PRINT USING "####"; CVI(mini22$); : PRINT "] [Maximo1="; : PRINT USING "####"; CVI(maxi12$); : PRINT "] [Maximo2="; : PRINT USING "####"; CVI(maxi22$); : _
              PRINT "]"
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE 20, 70: PRINT USING "######,.##"; CVD(pv32$): condi% = CVI(exi32$): conda$ = pv32$
DO
     COLOR colors(4, colorpref), colors(7, colorpref)
     LOCATE 22, 2: PRINT STRING$(33, "Ä"): Center 25, " Utilize Teclas de Direccion y Escoja PVP con [Enter];salga con [Esc] "
     llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
        SELECT CASE llave$
            CASE CHR$(0) + "K": old% = sit%: sit% = sit% - 1: IF sit% < 1 THEN sit% = 4
            CASE CHR$(0) + "M": old% = sit%: sit% = sit% + 1: IF sit% > 4 THEN sit% = 1
            CASE CHR$(27): CLOSE : EXIT SUB
            CASE CHR$(13)
            IF CVD(conda$) > 0 THEN EXIT DO ELSE LOCATE 22, 3: PRINT "El PVP Debe Ser MAYOR Que 0": Espera
            CASE ELSE: IF sonido = "si" THEN BEEP
        END SELECT
     COLOR colors(4, colorpref), colors(7, colorpref)
     IF old% = 1 THEN LOCATE 20, 40: PRINT USING "######,.##"; CVD(pv12$)
     IF old% = 2 THEN LOCATE 20, 55: PRINT USING "######,.##"; CVD(pv22$)
     IF old% = 3 THEN LOCATE 20, 70: PRINT USING "######,.##"; CVD(pv32$)
     IF old% = 4 THEN LOCATE 20, 12: PRINT USING "######,.##"; CVD(pv42$)
     COLOR colors(7, colorpref), colors(4, colorpref)
     IF sit% = 1 THEN LOCATE 20, 40: PRINT USING "######,.##"; CVD(pv12$): condi% = CVI(exi12$): conda$ = pv12$
     IF sit% = 2 THEN LOCATE 20, 55: PRINT USING "######,.##"; CVD(pv22$): condi% = CVI(exi22$): conda$ = pv22$
     IF sit% = 3 THEN LOCATE 20, 70: PRINT USING "######,.##"; CVD(pv32$): condi% = CVI(exi32$): conda$ = pv32$
     IF sit% = 4 THEN LOCATE 20, 12: PRINT USING "######,.##"; CVD(pv42$): condi% = CVI(exi42$): conda$ = pv42$
LOOP
RETURN
Ajustar:
     CALL Box(17, 1, 23, 80)
     LOCATE 18, 1: PRINT " El precio por unidad es de Bs. "; : PRINT USING "######,.##"; prize#; : PRINT SPACE$(25)
     Center 25, " ATENCION: LA GANANCIA ES MENOR DEL 30% , POR FAVOR AJUSTE EL PVP"
     LOCATE 19, 1: PRINT "¨Desea ajustar este precio? ": precio$ = "0"
     DO WHILE VAL(precio$) <= prize#
          precio$ = MeteDatoLetra$(19, 30, 10, STR$((FIX(prize# * 100)) / 100), "0")
     LOOP: prize# = VAL(precio$)
RETURN
END SUB

FUNCTION Escoge% (fila%, edo%, fila2%, Prod$, caso%)
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila% - 1, 1: PRINT STRING$(80, "Ä"): LOCATE fila% + 5, 1
PRINT STRING$(80, "Ä"); : FOR k = fila% TO fila% + 4: LOCATE k, 1: PRINT SPACE$(80); : NEXT k
STATIC max2%, Prud AS STRING * 6
IF edo% = 1 THEN
CALL Center(25, "[F1] Ayuda [F9] Salir SIN Escoger [Enter] Toma Articulo [A-Z] Ir a")
ELSE
CALL Center(25, "[F1] Ayuda [F9] Salir SIN Escoger [Enter] Toma Articulo")
END IF
IF edo% = 1 OR edo% = 7 THEN
     Prod$ = "      "
     DO WHILE Prod$ = "      "
          LOCATE fila2%, 5: PRINT "Introduzca Orden de Busqueda:"; SPACE$(15)
          Prod$ = UCASE$(MeteDatoLetra(fila2%, 34, 6, "      ", "0"))
          Prud = Prod$: Prod$ = Trim$(Prod$)
          IF Fun(9) = "si" OR Prod$ = "" THEN : Escoge% = -1: EXIT FUNCTION
     LOOP
END IF: selecc% = 0
IF edo% = 10 THEN edo% = 1
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
               Prod$ = UCASE$(MeteDatoLetra(fila2%, 34, 6, "      ", "0"))
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
     CASE 7  'busqueda de borrados
          OPEN "f:\kevinprg\kevindat\borrados.idx" FOR RANDOM AS #2 LEN = 37
          FIELD #2, 35 AS nomshort$, 2 AS num$: max2% = LOF(2) / 37
          IF max2% = 0 THEN LOCATE fila% + 5, 5: PRINT STRING$(5, "Ä"); "² NO Hay Datos ²"; STRING$(7, "Ä"); : Espera: Escoge% = 0: EXIT FUNCTION
          GOSUB Parche1
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
          CASE CHR$(0) + CHR$(67): resal% = 0: Fun(9) = "si": EXIT DO
          CASE IS >= CHR$(42)
               IF kbd$ <= CHR$(90) AND edo% = 1 THEN
                    IF caso% = 1 THEN
                       IF UCASE$(kbd$) = UCASE$(MID$(Prod$, 2, 1)) THEN
                           Prod$ = MID$(Prod$, 2, LEN(Prod$) - 1)
                       ELSE
                           IF kbd$ = "/" AND caso% = 1 THEN
                              Prod$ = "/" + Prod$: caso% = 0
                           ELSE
                              Prod$ = kbd$
                           END IF
                       END IF
                    ELSE
                       Prod$ = kbd$
                    END IF
                    CLOSE #2: OPEN "F:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
                    FIELD #2, 35 AS nomshort$, 2 AS num$: max2% = LOF(2) / 37: GOSUB Parche1: GOSUB Parche2
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
Center 25, "Presione Cualquier Tecla Para Continuar...": DO: LOOP WHILE INKEY$ = ""
END SUB

SUB LeeEstado
CLOSE : OPEN "F:\kevinprg\kevindat\kaos.dat" FOR INPUT AS #2
INPUT #2, sonido: INPUT #2, Impuesto: INPUT #2, Nombfarma: INPUT #2, Direccion
INPUT #2, Rif: INPUT #2, Nit: INPUT #2, Telefono: INPUT #2, contri: INPUT #2, contra
INPUT #2, Stock: INPUT #2, Blanq: INPUT #2, LargoH: INPUT #2, TipoL: INPUT #2, Mensaje1
INPUT #2, Mensaje2: INPUT #2, Mensaje3: INPUT #2, Redondeo: INPUT #2, Desc
INPUT #2, DescMax: INPUT #2, Exactitud: INPUT #2, Prov1: INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo
CLOSE
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

