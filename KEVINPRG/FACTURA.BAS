'imprimir facturas al final del dia
DEFINT A-Z
DECLARE SUB LeeEstado ()
DECLARE SUB Visualiza ()
DECLARE SUB Center (row, text$)
DECLARE SUB Box2 (Row1, Col1, Row2, Col2)
DECLARE SUB Espera ()
DECLARE FUNCTION CheqDate (caden$, op%)
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$, tr$)
DECLARE FUNCTION Trim$ (x$)
COMMON SHARED guia, colorpref
colorpref = 3
DIM SHARED fx AS STRING * 33, turno, bruto AS STRING * 8, neto AS STRING * 8
DIM SHARED totalbruto AS STRING * 8, totalneto AS STRING * 8, fct AS STRING * 4, total AS STRING * 8
DIM SHARED imptotbruto AS STRING * 8, imptotneto AS STRING * 8, descuento AS STRING * 8
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, impuesto AS STRING * 6, Nombfarma AS STRING * 25, Direccion AS STRING * 30, rif     AS STRING * 25, nit      AS STRING * 25, telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2, NuevoIva AS STRING * 2, ShowPerc AS STRING * 2
DIM SHARED PrintErr AS INTEGER, devolver, Fun(17) AS STRING * 2, sitio, salir
DIM SHARED desc1(26) AS STRING * 35, cant1(26) AS STRING * 2, prec1(26) AS STRING * 8, ivaf1(26) AS STRING * 1
CONST true = -1: CONST false = NOT true: turno = false
CALL LeeEstado: CALL Visualiza: CLOSE
END

ErrorTrap:

SELECT CASE ERR
     CASE 53
          Box2 15, 10, 20, 70: Center 15, "Mensaje"
          Center 17, "NO Se Encontro el Archivo de Ventas:"
          Center 18, "Cerciorese de Que Ese Dia Se Hayan Registrado Ventas"
          Center 19, "Solicite de Nuevo el Archivo.": Espera: devolver = true
          RESUME NEXT
     CASE 71
          Box2 10, 10, 20, 70: Center 10, "Mensaje"
          Center 11, "Diskette NO Esta Listo.Por Favor Introduzcalo"
          Center 12, "Presion Escape Para Abortar."
          Center 17, "Presione Cualquier otra Tecla Para REINTENTAR."
          kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
          IF kbd$ = CHR$(27) THEN devolver = true: RESUME NEXT ELSE RESUME
     CASE 57
          RESUME NEXT
     CASE 25, 27, 57, 68
          Box2 10, 10, 20, 70: Center 10, "Mensaje"
          Center 11, "La Impresora NO Responde. Por Favor Verifique Que:"
          Center 12, "1.- Tenga Papel y que No Este Atascado.           "
          Center 13, "2.- Tenga Energia Electrica y/o Este Conectada.   "
          Center 14, "3.- El Cable de Datos Este Conectado y en Linea.  "
          Center 16, "Si Todo Esto NO Funciona Presione Escape y Proceda"
          Center 17, "a Llenar Manualmente la Factura ; Presione   Cual-"
          Center 18, "quier otra Tecla Para REINTENTAR.                 "
          kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
          IF kbd$ = CHR$(27) THEN devolver = true: RESUME NEXT ELSE RESUME
     CASE 70
          Center 25, "Esperando Acceso a La Red , Espere Por Favor . . .": SLEEP 1: RESUME
     CASE ELSE
          LOCATE 1, 1: PRINT ERR
END SELECT
RESUME NEXT
ErrorTrop:
SELECT CASE ERR
     CASE 25, 27, 57, 68
          Box2 5, 2, 7, 78: Center 5, "Mensaje": Center 6, "No Se Detect¢ Gavetero;NO DEBE Usar Esta Estacion Para Ventas de Contado": RESUME NEXT
     CASE ELSE
END SELECT
RESUME NEXT
ErrorTrep:
SELECT CASE ERR
     CASE 68
           Box2 10, 10, 20, 70: Center 10, "Mensaje": Center 15, " Dispositivo NO Disponible : Ahora Se Saldra al Menu Principal . . .": Espera: devolver = true: RESUME NEXT
END SELECT
RESUME NEXT
DATA 0,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  2,     0,     1,     15,     0,     7,      0
DATA 3,    15,   13,  1,     14,    3,     15,     0,     7,      0
DATA 7,    12,   15,  4,     14,    0,     15,     15,    1,      0

DATA "01/10/2001"
DATA "02/10/2001"
DATA "03/10/2001"
DATA "04/10/2001"
DATA "05/10/2001"
DATA "06/10/2001"
DATA "07/10/2001"
DATA "08/10/2001"
DATA "09/10/2001"
DATA "10/10/2001"
DATA "11/10/2001"
DATA "12/10/2001"
DATA "13/10/2001"
DATA "14/10/2001"
DATA "15/10/2001"
DATA "16/10/2001"
DATA "17/10/2001"
DATA "18/10/2001"
DATA "19/10/2001"
DATA "20/10/2001"
DATA "21/10/2001"
DATA "22/10/2001"
DATA "23/10/2001"
DATA "24/10/2001"
DATA "25/10/2001"
DATA "26/10/2001"
DATA "27/10/2001"
DATA "28/10/2001"
DATA "29/10/2001"
DATA "30/10/2001"
DATA "31/10/2001"
DATA "fin"

SUB Box2 (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "²"; STRING$(BoxWidth - 2, "²"); "²"; : FOR a = Row1 + 1 TO Row2 - 1: LOCATE a, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù";
END SUB

SUB Center (row, text$)
IF row = 25 THEN COLOR 0, 15: LOCATE 25, 1: PRINT SPACE$(80);
LOCATE row, 41 - LEN(text$) / 2: PRINT text$;
END SUB

FUNCTION CheqDate (caden$, op%)
CheqDate = true: IF caden$ = "        " THEN EXIT FUNCTION
dd% = VAL(MID$(caden$, 1, 2)): mm% = VAL(MID$(caden$, 4, 2)): aa% = VAL(MID$(caden$, 7, 4))
dd2% = VAL(MID$(DATE$, 4, 2)): mm2% = VAL(MID$(DATE$, 1, 2)): aa2% = VAL(MID$(DATE$, 7, 4))
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

SUB Espera
Center 25, "Presione Cualquier Tecla Para Continuar...": DO: LOOP WHILE INKEY$ = ""
END SUB

SUB LeeEstado
CLOSE : OPEN "F:\kevinprg\kevindat\kaos.dat" FOR INPUT AS #2
INPUT #2, sonido: INPUT #2, impuesto: INPUT #2, Nombfarma: INPUT #2, Direccion: INPUT #2, rif: INPUT #2, nit: INPUT #2, telefono: INPUT #2, contri: INPUT #2, contra: INPUT #2, Stock: INPUT #2, Blanq
INPUT #2, LargoH: INPUT #2, TipoL: INPUT #2, Mensaje1: INPUT #2, Mensaje2: INPUT #2, Mensaje3: INPUT #2, Redondeo: INPUT #2, Desc: INPUT #2, DescMax: INPUT #2, Exactitud: INPUT #2, Prov1: INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo
'INPUT #2, NuevoIva: INPUT #2, ShowPerc
CLOSE : NuevoIva = "si"
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

SUB Visualiza
OPEN "F:\kevinprg\kevindat\codigo.idx" FOR RANDOM SHARED AS #2 LEN = 8
FIELD #2, 6 AS nomshort$, 2 AS num$: max2% = LOF(2) / 8
DIM Xa(max2%) AS STRING * 6, Xb(max2%) AS STRING * 2
FOR k = 1 TO max2%: GET #2, k: Xa(k) = nomshort$: Xb(k) = num$: NEXT k: CLOSE
     'COLOR colors(7, colorpref), colors(4, colorpref): CLS
DO
     Box2 1, 5, 3, 75: Center 2, "Imprimir Facturas de Un Dia": Center 25, "Espere , por favor . . ."
     salir = false ': dia$ = MID$(DATE$, 4, 2) + "/" + MID$(DATE$, 1, 2) + "/" + MID$(DATE$, 7, 4)
     deacuerdo = false
     DO WHILE NOT salir
          COLOR colors(7, colorpref), colors(4, colorpref): Box2 4, 10, 12, 70: Center 5, "Introduzca Fecha"
          Center 25, "Emplee el formato dd/mm/aaaa [F9]Abortar": Fun(9) = "no"
          READ dia$: PRINT dia$
          IF dia$ = "fin" THEN EXIT SUB
          'dia$ = MeteDatoLetra$(6, 35, 10, dia$, "0")
          IF Fun(9) = "si" THEN EXIT SUB
          die$ = MID$(DATE$, 4, 2) + "/" + MID$(DATE$, 1, 2) + "/" + MID$(DATE$, 7, 4)
          'IF dia$ <> die$ THEN
          '     Box2 10, 10, 12, 70: Center 11, "La fecha no concide con el dia de hoy ¨Desea imprimir (S/N)?"
          '     kbd$ = ""
          '     DO
          '          kbd$ = INKEY$
          '          kbd$ = UCASE$(kbd$)
          '          IF kbd$ = "S" OR kbd$ = "N" THEN EXIT DO
          '     LOOP
          '     IF kbd$ = "S" THEN deacuerdo = true ELSE deacuerdo = false
          'ELSE
               deacuerdo = true
          'END IF
          IF CheqDate(dia$, 1) = true AND deacuerdo = true THEN salir = true ELSE deacuerdo = false
     LOOP
     Box2 10, 10, 20, 70: Center 15, "Seleccionando Ventas , Espere Por Favor . . ."
     die$ = MID$(dia$, 1, 2) + MID$(dia$, 4, 2) + MID$(dia$, 7, 4): Center 25, "Buscando , Espere Por Favor . . ."
     fx = "f:\kevinprg\kevinvnt\" + die$ + ".vnt": devolver = false
     ON ERROR GOTO ErrorTrep
     FILES fx: COLOR colors(7, colorpref), colors(4, colorpref): CLS
     IF devolver = true THEN CLOSE : EXIT SUB
     ON ERROR GOTO 0
     Box2 10, 10, 20, 70: Center 15, "Seleccionando Ventas , Espere Por Favor . . ." + dia$
     ahora$ = "F:\kevinprg\kevindat\" + Trim$(MID$(STR$(TIMER * 100), 2, 10)) + ".ver"
     CLOSE #1: OPEN fx FOR RANDOM AS #1 LEN = 30: FIELD #1, 30 AS todo$: maxi% = LOF(1) / 30
     CLOSE #2: OPEN ahora$ FOR RANDOM AS #2 LEN = 30: FIELD #2, 30 AS all$: ini% = 1: GET #1, 1
     IF MID$(todo$, 1, 5) = "TURNO" THEN turno = true
     FOR k = 2 TO maxi%
          GET #1, k: RSET all$ = todo$: PUT #2, k - 1
     NEXT k
     CLOSE
    
     OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200: salir = false
     FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
     ON ERROR GOTO ErrorTrap
     IF salir = true THEN EXIT SUB
     OPEN ahora$ FOR RANDOM AS #2 LEN = 30: FIELD #2, 30 AS all$: puntero% = 1
     salir = false: maxi% = LOF(2) / 30
    
IF maxi% > 0 THEN
     KILL "f:\kevinprg\kevindat\seniat.ves"
     OPEN "f:\kevinprg\kevindat\seniat.ves" FOR RANDOM SHARED AS #3 LEN = 52
     FIELD #3, 6 AS cod$, 35 AS nom$, 2 AS can$, 8 AS pvp$, 1 AS ivo$: sit3% = 0
     totalbruto = MKD$(0): totalneto = MKD$(0)
     imptotbruto = MKD$(0): imptotneto = MKD$(0)
     DO
          GET #2, puntero%: IF MID$(all$, 1, 1) <> "J" THEN Box2 10, 10, 20, 70: Center 15, "Archivo Corrupto , Llame a Kevin Software 7000": Center 17, "Tan Pronto Como Le Sea Posible": EXIT SUB
          otropuntero% = puntero% + CVI(MID$(all$, 2, 2))
          agr$ = "": agr$ = MID$(all$, 4, 1)
          IF agr$ = " " THEN
          bruto = MKD$(0)
          neto = (MID$(all$, 5, 8)): LOCATE 10, 1: PRINT agr$
          imptotneto = MKD$(CVD(imptotneto) + CVD(neto))
          totalneto = MKD$(CVD(totalneto) + CVD(neto))
          FOR k = (puntero% + 1) TO otropuntero%
               GET #2, k
               bruto = MKD$(CVD(bruto) + (CVD(MID$(all$, 10, 8)) * CVI(MID$(all$, 8, 2))))
               Prod$ = MID$(all$, 2, 6)
               IF sit3% > 0 THEN
                    salir = false
                    FOR l = 1 TO sit3%
                         GET #3, l
                         IF Prod$ = cod$ AND CVD(MID$(all$, 10, 8)) = CVD(pvp$) THEN
                              'LOCATE 7, 1: PRINT USING " ########,"; CVI(can$);
                              RSET can$ = MKI$(CVI(can$) + CVI(MID$(all$, 8, 2)))
                              'LOCATE 1, 1: PRINT nom$; " ";
                              'LOCATE 2, 1: PRINT USING " ########,"; CVI(can$);
                              'LOCATE 3, 1: PRINT USING " #################,.##"; CVD(pvp$)
                              'Espera
                              salir = true: PUT #3, l: l = sit3%
                         END IF
                    NEXT l
                    IF salir = false THEN
                         RSET can$ = MID$(all$, 8, 2): RSET ivo$ = MID$(all$, 1, 1)
                         RSET pvp$ = MID$(all$, 10, 8)
                         encontrado = false: primero = 1: ultimo = max2%
                         WHILE NOT encontrado AND primero <= ultimo
                              central = (primero + ultimo) / 2
                              IF Prod$ = Xa(central) THEN
                                   encontrado = true
                              ELSE
                                   IF Xa(central) > Prod$ THEN
                                        ultimo = central - 1
                                   ELSE
                                        primero = central + 1
                                   END IF
                              END IF
                         WEND: GET #1, CVI(Xb(central))
                         RSET nom$ = nomb2$: sit3% = sit3% + 1
                         RSET cod$ = Prod$: PUT #3, sit3%: 'LOCATE 20, 1: PRINT USING " ########,"; CVI(can$);
                              'LOCATE 1, 1: PRINT nom$; " ";
                              'LOCATE 2, 1: PRINT USING " ########,"; CVI(can$);
                              'LOCATE 3, 1: PRINT USING " #################,.##"; CVD(pvp$)
                              'Espera
                    END IF
               ELSE
                    encontrado = false: primero = 1: ultimo = max2%
                    WHILE NOT encontrado AND primero <= ultimo
                         central = (primero + ultimo) / 2
                         IF Prod$ = Xa(central) THEN
                              encontrado = true
                         ELSE
                              IF Xa(central) > Prod$ THEN
                                   ultimo = central - 1
                              ELSE
                                   primero = central + 1
                              END IF
                         END IF
                    WEND: GET #1, CVI(Xb(central))
                    RSET can$ = MID$(all$, 8, 2): RSET ivo$ = MID$(all$, 1, 1)
                    RSET pvp$ = MID$(all$, 10, 8)
                    RSET nom$ = nomb2$: sit3% = sit3% + 1
                    RSET cod$ = Prod$: PUT #3, sit3%
                              'LOCATE 1, 1: PRINT nom$; " ";
                              'LOCATE 2, 1: PRINT USING " ########,"; CVI(can$);
                              'LOCATE 3, 1: PRINT USING " #################,.##"; CVD(pvp$)
                              'Espera
               END IF
          NEXT k
          totalbruto = MKD$(CVD(totalbruto) + CVD(bruto))
          imptotbruto = MKD$(CVD(imptotbruto) + CVD(bruto))
          END IF
          'baja factura
          SWAP puntero%, otropuntero%: puntero% = puntero% + 1
          IF puntero% >= maxi% THEN EXIT DO
     LOOP
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box2 10, 10, 20, 70
Center 11, "Leyendo Datos. Espere Por Favor ... ": COLOR colors(7, colorpref), colors(4, colorpref)
'PRINT "Total Bruto"; : PRINT USING "####################,.##"; CVD(totalbruto)
'PRINT "Total Neto "; : PRINT USING "####################,.##"; CVD(totalneto)
descuento = MKD$(1 - (CVD(imptotneto) / CVD(imptotbruto)))
'Espera
'END
'rutina de imprimir facturas
OPEN "LPT1:" FOR OUTPUT AS #10
WIDTH #10, 255
sit3% = LOF(3) / 52: salir = false: lleva3% = 1: auxi% = 0
DO
     auxi% = lleva3% + 25: renglon = 0
     IF auxi% >= sit3% THEN auxi% = sit3%: salir = true
     FOR m = lleva3% TO auxi%
               GET #3, m: renglon = renglon + 1
               desc1(renglon) = nom$: cant1(renglon) = can$
               prec1(renglon) = pvp$: ivaf1(renglon) = ivo$
               'LOCATE 1, 1: PRINT nom$; " ";
               'LOCATE 2, 1: PRINT USING " ########,"; CVI(can$);
               'LOCATE 3, 1: PRINT USING " #################,.##"; CVD(pvp$)
     NEXT m
          SWAP auxi%, lleva3%: lleva3% = lleva3% + 1
          neaux$ = MKD$(0): IF renglon > 26 THEN renglon = 26
          'LOCATE 1, 1: PRINT "renglon:"; renglon: Espera
          auxe$ = MKD$(0): auxa$ = MKD$(0)
          PRINT #10, CHR$(27); CHR$(64); : PRINT #10, CHR$(27); CHR$(64);
          PRINT #10, CHR$(27); CHR$(67); CHR$(45);
          PRINT #10, CHR$(10);
          PRINT #10, CHR$(15);
          PRINT #10, STRING$(45, "-"); CHR$(10);
          'PRINT #10, USING "Factura:######,"; CVL(fct);
          PRINT #10, "      Fecha:"; dia$;
          PRINT #10, "      Pagina:1 de "; : IF (renglon - 1) <= 11 THEN PRINT #10, "1"; CHR$(10);  ELSE PRINT #10, "2"; CHR$(10);
          PRINT #10, "Cliente:            Vendedor:Gerencia"; CHR$(10);
          PRINT #10, CHR$(10);
          PRINT #10, "R.I.F.:"; SPACE$(20); " N.I.T.:"; SPACE$(20); CHR$(10);
          PRINT #10, "Domicilio Fiscal:"; CHR$(10);
          PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
          PRINT #10, "Telefonos:"; SPACE$(5); " Forma de Pago: Contado"; CHR$(10);
          PRINT #10, STRING$(45, "-"); CHR$(10);
          PRINT #10, "³Descripcion             ³Cant³Precio Venta³"; CHR$(10);
               SELECT CASE renglon
                    CASE 1 TO 26
                         FOR k = 1 TO renglon
                              PRINT #10, "³"; MID$(desc1(k), 1, 24); "³";
                              PRINT #10, USING "###,³"; CVI(cant1(k));
                              auxa$ = MKD$(CVD(auxa$) + (CVI(cant1(k)) * CVD(prec1(k))))
                              IF ivaf1(k) = "*" THEN
                                   IF NuevoIva = "si" THEN
                                        PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
                                        auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100)))))
                                   ELSE
                                        PRINT #10, USING "########,.##³"; CVD(prec1(k));
                                        auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)))))
                                   END IF
                              ELSE
                                   PRINT #10, USING "########,.##³"; CVD(prec1(k));
                                   auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(prec1(k))))
                              END IF: PRINT #10, CHR$(10);
                         NEXT k
                    CASE 9 TO 21
                         FOR k = 1 TO renglon
                              PRINT #10, "³"; MID$(desc1(k), 1, 15); "³";
                              PRINT #10, USING "#######,³"; CVI(cant1(k));
                              auxa$ = MKD$(CVD(auxa$) + (CVI(cant1(k)) * CVD(prec1(k))))
                              IF ivaf1(k) = "*" THEN
                                   IF NuevoIva = "si" THEN
                                        PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
                                        auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100)))))
                                   ELSE
                                        PRINT #10, USING "########,.##³"; CVD(prec1(k));
                                        auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)))))
                                   END IF
                              ELSE
                                   PRINT #10, USING "########,.##³"; CVD(prec1(k));
                                   auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(prec1(k))))
                              END IF: PRINT #10, CHR$(10);
                         NEXT k
                         FOR k = 1 TO (21 - renglon): PRINT #10, SPACE$(k + 5); "\"; SPACE$(20); "\"; CHR$(10); : NEXT k
                         'PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
                         PRINT #10, "VAN --> Bs "; : PRINT #10, USING "##########,.##"; CVD(auxe$); : PRINT CHR$(10);
                         'PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
                         'PRINT #10, CHR$(12); : PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
                         PRINT #10, STRING$(45, "-"); CHR$(10);
                         'PRINT #10, USING "Factura:######,"; CVL(fct);
                         PRINT #10, " Fecha:"; dia$; " Hora:"; TIME$;
                         PRINT #10, " Pagina:2 de 2"; CHR$(10); : PRINT #10, "Cliente:          Vendedor:Gerencia"; Cedula$; CHR$(10);
                         PRINT #10, nombre$; CHR$(10);
                         PRINT #10, "R.I.F.:"; rifo$; SPACE$(20 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(20 - LEN(rifo$)); CHR$(10);
                         PRINT #10, "Domicilio Fiscal:"; CHR$(10);
                         PRINT #10, domicilio1$; CHR$(10); : PRINT #10, domicilio2$; CHR$(10);
                         PRINT #10, "Telefonos:"; SPACE$(5); " Forma de Pago: Contado"; CHR$(10);
                         PRINT #10, STRING$(45, "-"); CHR$(10);
                         'PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
                         PRINT #10, "VIENEN->Bs "; : PRINT #10, USING "##########,.##"; CVD(auxe$); : PRINT #10, CHR$(10);
                         'PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
                    CASE 22 TO 26
                         FOR k = 1 TO 21
                              PRINT #10, "³"; MID$(desc1(k), 1, 15); "³";
                              PRINT #10, USING "#######,³"; CVI(cant1(k));
                              auxa$ = MKD$(CVD(auxa$) + (CVI(cant1(k)) * CVD(prec1(k))))
                              IF ivaf1(k) = "*" THEN
                                   IF NuevoIva = "si" THEN
                                        PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
                                        auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100)))))
                                   ELSE
                                        PRINT #10, USING "########,.##³"; CVD(prec1(k));
                                        auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)))))
                                   END IF
                              ELSE
                                   PRINT #10, USING "########,.##³"; CVD(prec1(k));
                                   auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(prec1(k))))
                              END IF: PRINT #10, CHR$(10);
                         NEXT k
                         'PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
                         PRINT #10, "VAN --> Bs "; : PRINT #10, USING "##########,.##"; CVD(auxe$); : PRINT CHR$(10);
                         'PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
                         'PRINT #10, CHR$(12); : PRINT #10, CHR$(10); : PRINT #10, CHR$(10);
                         PRINT #10, STRING$(45, "-"); CHR$(10);
                         'PRINT #10, USING "Factura:######,"; CVL(fct);
                         PRINT #10, " Fecha:"; dia$; " Hora:"; TIME$;
                         PRINT #10, " Pagina:2 de 2"; CHR$(10); : PRINT #10, "Cliente:       Vendedor:Gerencia"; ; CHR$(10);
                         PRINT #10, nombre$; CHR$(10);
                         PRINT #10, "R.I.F.:"; rifo$; SPACE$(20 - LEN(rifo$)); " N.I.T.:"; nito$; SPACE$(20 - LEN(nito$)); CHR$(10);
                         PRINT #10, "Domicilio Fiscal:"; CHR$(10);
                         PRINT #10, domicilio1$; CHR$(10);
                         PRINT #10, domicilio2$; CHR$(10);
                         PRINT #10, "Telefonos:"; SPACE$(5); " Forma de Pago: Contado"; CHR$(10);
                         PRINT #10, STRING$(45, "-"); CHR$(10);
                         'PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
                         PRINT #10, "VIENEN->Bs "; : PRINT #10, USING "##########,.##"; CVD(auxe$); : PRINT #10, CHR$(10);
                         'PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
                         FOR k = 22 TO renglon
                              PRINT #10, "³"; MID$(desc1(k), 1, 15); "³"; : PRINT #10, USING "#######,³"; CVI(cant1(k));
                              auxa$ = MKD$(CVD(auxa$) + (CVI(cant1(k)) * CVD(prec1(k))))
                              IF ivaf1(k) = "*" THEN
                                   IF NuevoIva = "si" THEN
                                        PRINT #10, USING "########,.##³"; (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100))); : PRINT #10, "I";
                                        auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)) / (1 + (VAL(impuesto) / 100)))))
                                   ELSE
                                        PRINT #10, USING "########,.##³"; CVD(prec1(k));
                                        auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * (CVD(prec1(k)))))
                                   END IF
                              ELSE
                                   PRINT #10, USING "########,.##³"; CVD(prec1(k));
                                   auxe$ = MKD$(CVD(auxe$) + (CVI(cant1(k)) * CVD(prec1(k))))
                              END IF: PRINT #10, CHR$(10);
                         NEXT k
               END SELECT
               PRINT #10, SPACE$(14); STRING$(15, "="); CHR$(10);
               PRINT #10, "Sub - Total: "; : PRINT #10, USING "##########,.## Bs"; CVD(auxe$); : PRINT #10, CHR$(10);
               IF NuevoIva = "si" THEN
                    PRINT #10, "- Descuento: "; : PRINT #10, USING "##########,.## Bs"; (CVD(auxe$) * CVD(descuento)); : PRINT #10, CHR$(10);
               ELSE
                    PRINT #10, "- Descuento: "; : PRINT #10, USING "##########,.## Bs"; (CVD(auxa$) * CVD(descuento)); : PRINT #10, CHR$(10);
               END IF
               IF ShowPerc = "si" THEN
                    IF CVD(descuento) > 0 THEN
                         'PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
                         PRINT #10, "    % descuento:"; : PRINT #10, USING "###.##  %"; (100 * CVD(descuento)); : PRINT #10, CHR$(10);
                         'PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
                    END IF
               END IF
               IF NuevoIva = "si" THEN                                                                           '  CVD(neto) - (CVD(auxe$) * (CVD(neto) / CVD(total)))               
                    PRINT #10, SPACE$(3); "+I. V. A.("; impuesto; "%):"; : PRINT #10, USING "##########,.## Bs"; ((CVD(auxa$) * (1 - CVD(descuento))) - (CVD(auxe$) * (1 - CVD(descuento)))); : PRINT #10, CHR$(10);
               END IF
               'PRINT #10, CHR$(27); "W"; CHR$(1); CHR$(10);
               PRINT #10, "Neto Factura"; : PRINT #10, USING "##########,.## Bs"; CVD(auxa$) * (1 - CVD(descuento)); : PRINT #10, CHR$(10);
               'PRINT #10, CHR$(27); "W"; CHR$(0); CHR$(10);
               PRINT #10, STRING$(45, "-"); CHR$(10);
               'PRINT #10, CHR$(12);
          FOR k = 1 TO 26
          desc1(k) = "": cant1(k) = MKI$(0): prec1(k) = MKD$(0): ivaf1(k) = " "
          NEXT k
     'Espera
     IF salir = true THEN EXIT DO
LOOP
END IF
LOOP
END SUB

