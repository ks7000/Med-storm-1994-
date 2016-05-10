'    modulo de reclamos
DEFINT A-Z
DECLARE SUB Espera ()
DECLARE SUB LeeEstado ()
DECLARE SUB Center (row, text$)
DECLARE SUB Box (Row1%, Col1%, Row2%, Col%)
DECLARE SUB Borrabox (Row1%, Col1%, Row2%, Col%)
DECLARE SUB VerFactCompras ()
DECLARE SUB Termometro (va%, total%, edo%, text$, fila%, columna%)
DECLARE SUB Reclamos (edo%)
DECLARE SUB Imprime ()
DECLARE SUB Salida ()
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$)
DECLARE FUNCTION EntraDatoNumero$ (fila%, columna%, largo%, caden$, entero%)
DECLARE FUNCTION Escoge% (fila%, edo%, fila2%, Prod$)
DECLARE FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%, est%)
DECLARE FUNCTION CheqDate (caden$, op%)
DECLARE FUNCTION Trim$ (x$)
DECLARE FUNCTION Verify (ubi%, text$)
COMMON SHARED guia, colorpref
DIM SHARED turno AS STRING * 1, devolver
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED Sonido  AS STRING * 2, Impuesto AS STRING * 6, NombFarma AS STRING * 25, Direccion AS STRING * 30, Rif     AS STRING * 25, Nit      AS STRING * 25, Telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2, num2 AS STRING * 33
DIM SHARED Fun(17) AS STRING * 2, Salir, Turna, terminar, sitio, eleg
DIM SHARED Contenido(199) AS STRING * 30, Code(199) AS STRING * 3, Accion(99) AS STRING * 1, porc AS STRING * 8
DIM SHARED numfact(10) AS STRING * 4, provee(10) AS STRING * 17, fechafact(10) AS STRING * 8, fechaacc(10) AS STRING * 8, bruto(10) AS STRING * 8, neto(10) AS STRING * 8, ivafact(10) AS STRING * 8, transpor(10) AS STRING * 10, edofact(10) AS STRING  _
* 3, ubifact(10) AS STRING * 2, imprimir(10) AS STRING * 1
REM $DYNAMIC
DIM SHARED Nombra(50) AS STRING * 35, Pvp1a(50) AS STRING * 8, Pvp2a(50) AS STRING * 8, Pvp3a(50) AS STRING * 8, Pvp4a(50) AS STRING * 8, Numa(50) AS STRING * 2, observa(50) AS STRING * 20, Exis1a(50) AS STRING * 2, Exis2a(50) AS STRING * 2, Exis3a( _
50) AS STRING * 2, Exis4a(50) AS STRING * 2
DIM SHARED Cant1a(50) AS STRING * 2, Cant2a(50) AS STRING * 2, Cant3a(50) AS STRING * 2, Cant4a(50) AS STRING * 2
DIM SHARED Xa(10) AS STRING * 6, Xb(10) AS STRING * 2, grabGav AS STRING * 1: busqueda = 0
CONST true = -1: CONST false = NOT true
ON ERROR GOTO ErrorTrap
ON TIMER(5) GOSUB AbreGav
TIMER ON
CALL LeeEstado
IF guia = 1 THEN
   resp% = true
   WHILE resp% = true
      CALL VerFactCompras
      resp% = Verify(13, "¨Desea hacer otro reclamo")
   WEND
END IF
IF guia = 2 THEN CALL Imprime
CALL Salida
END
ErrorTrap:
    SELECT CASE ERR
        CASE 24, 25
            SOUND 600, 2: Center 25, "La impresora no responde ... (revise si esta encendida,on-line o falta papel)": Espera
            RESUME
        CASE 53 'no se encontro archivo ...
            RESUME NEXT
        CASE 70
            Center 25, " Esperando Acceso a La Red   ...": SLEEP 1: Center 25, SPACE$(35): RESUME
        CASE ELSE
            LOCATE 1, 1: PRINT "Error:"; ERR
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
SUB Borrabox (Row1%, Col1%, Row2%, Col2%) STATIC
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

FUNCTION Elije% (estesi%, fila%, columna%, fila2%, columna2%, ingrediente%, est%)
COLOR colors(4, colorpref), colors(7, colorpref): IF estesi% = 6 THEN selecc% = 99 ELSE selecc% = 199
IF estesi% = 5 THEN Box fila%, columna%, fila% + 16, columna% + 16 ELSE Box fila%, columna%, fila% + 16, columna% + 31
IF est% = 0 THEN Center 25, "[F1]Ayuda  [F5]Editar o Adicionar Linea  [Enter]Tomar Opcion" ELSE Center 25, "[F1]Ayuda  [Enter]Tomar Opcion"
COLOR colors(4, colorpref), colors(7, colorpref)
SELECT CASE estesi%
     CASE 5
          LOCATE fila%, columna% + 1: PRINT " Proveedor "
          CLOSE #2: OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #2 LEN = 18
          FIELD #2, 3 AS codigo$, 15 AS descripcion$
          FOR k = 1 TO 199: GET #2, k: Code(k) = codigo$: Contenido(k) = descripcion$: NEXT k: CLOSE #2
     CASE 6
          LOCATE fila%, columna% + 1: PRINT "Causas de Reclamo"
          CLOSE #2: OPEN "F:\kevinprg\kevindat\causas.dat" FOR RANDOM AS #2 LEN = 33
          FIELD #2, 2 AS codigo$, 30 AS descripcion$, 1 AS edo$
          FOR k = 1 TO 99: GET #2, k: Code(k) = codigo$: Contenido(k) = descripcion$: Accion(k) = edo$: NEXT k: CLOSE #2
END SELECT
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE (fila% + 1), (columna% + 1): IF estesi% = 5 THEN PRINT MID$(Contenido(1), 1, 15) ELSE PRINT Contenido(1)
COLOR colors(4, colorpref), colors(7, colorpref)
FOR k = 2 TO 15: LOCATE (fila% + k), (columna% + 1)
IF estesi% = 5 THEN PRINT MID$(Contenido(k), 1, 15) ELSE PRINT Contenido(k)
NEXT k: resal% = 1: up% = 1: terminar = false
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
            CASE CHR$(13)
                    IF MID$(Contenido(resal% + up% - 1), 1, 1) = "þ" THEN resal% = 0: up% = 0
                    terminar = true: devolver = false
            CASE CHR$(27): terminar = true: resal% = 0: up% = 0: devolver = true
            CASE CHR$(0) + CHR$(63)
               IF est% = 0 THEN
                    IF estesi% = 5 THEN
                         Contenido(up% + resal% - 1) = MeteDatoLetra(resal% + 3, columna%, 15, MID$(Contenido(up% + resal% - 1), 1, 15)): Center 25, "Organizando , Espere ...": GOSUB SortYname2
                         COLOR colors(4, colorpref), colors(7, colorpref): Box fila%, columna%, fila% + 16, columna% + 16
                    ELSE
                         IF VAL(Code(up% + resal% - 1)) = 1 THEN
                              Center 25, "Esta Causa NO es editable": Espera
                         ELSE
                              Contenido(up% + resal% - 1) = MeteDatoLetra(resal% + 4, columna%, 30, (Contenido(up% + resal% - 1)))
                              Center 25, "Seleccione la accion:0 no altera , 1 suma , 2 resta": LOCATE (resal% + 4), (columna% + 1): PRINT "¨ Accion a tomar ?            "
                              DO
                                   auxu$ = MeteDatoLetra(resal% + 4, (columna% + 25), 1, " ")
                                   IF auxu$ = "0" THEN EXIT DO
                                   IF auxu$ = "1" THEN EXIT DO
                                   IF auxu$ = "2" THEN EXIT DO
                              LOOP: Accion(up% + resal% - 1) = auxu$
                              Center 25, "Organizando , Espere ...": GOSUB SortYname1
                         END IF
                         IF est% = 0 THEN Center 25, "[F1]Ayuda  [F5]Editar o Adicionar Linea  [Enter]Tomar Opcion" ELSE Center 25, "[F1]Ayuda  [Enter]Tomar Opcion"
                         COLOR colors(4, colorpref), colors(7, colorpref): Box fila%, columna%, fila% + 16, columna% + 31
                    END IF
               ELSE
                    BEEP
               END IF
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
               IF resal% = l THEN
                    COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila2%, columna2% - 1: PRINT USING "²###²"; VAL(Code(k));
                    IF estesi% = 5 THEN PRINT MID$(Contenido(k), 1, 15) ELSE PRINT Contenido(k)
               END IF
               LOCATE fila% + l, columna% + 1: IF estesi% = 5 THEN PRINT MID$(Contenido(k), 1, 15) ELSE PRINT Contenido(k)
               IF resal% = l THEN COLOR colors(4, colorpref), colors(7, colorpref)
          NEXT k
LOOP
Elije% = up% + resal% - 1: EXIT FUNCTION

SortYname1:
Center 25, " Sorteando Los Campos De Menor A Mayor. Espere Por Favor . . . "
FOR a% = 1 TO 98: FOR b% = a% + 1 TO 99
IF Contenido(a%) > Contenido(b%) THEN SWAP Contenido(a%), Contenido(b%): SWAP Code(a%), Code(b%): SWAP Accion(a%), Accion(b%)
NEXT b%: NEXT a%:
OPEN "F:\kevinprg\kevindat\causas.dat" FOR RANDOM AS #2 LEN = 33: FIELD #2, 2 AS codigo$, 30 AS descripcion$, 1 AS edo$
FOR k = 1 TO 99: RSET codigo$ = Code(k): RSET descripcion$ = Contenido(k): RSET edo$ = Accion(k): PUT #2, k: NEXT k: CLOSE #2
RETURN

SortYname2:
Center 25, " Sorteando Los Campos De Menor A Mayor. Espere Por Favor . . . "
FOR a% = 1 TO 198: FOR b% = a% + 1 TO 199
IF Contenido(a%) > Contenido(b%) THEN SWAP Contenido(a%), Contenido(b%): SWAP Code(a%), Code(b%)
NEXT b%: NEXT a%:
OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$
FOR k = 1 TO 99: RSET codigo$ = Code(k): RSET descripcion$ = Contenido(k): PUT #2, k: NEXT k: CLOSE #2
RETURN

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
               CASE ELSE: IF Sonido = "si" THEN BEEP
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
EntraDatoNumero$ = eleccion$: IF Sonido = "si" THEN BEEP
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
CALL Center(25, "[F1] Ayuda [F9] Salir SIN Escoger [Enter] Toma Articulo")
LOCATE fila2%, 5: PRINT "Introduzca Orden de Busqueda:"; SPACE$(15)
Prod$ = UCASE$(MeteDatoLetra(fila2%, 34, 6, "      "))
Prud = Prod$: Prod$ = Trim$(Prod$)
IF Fun(9) = "si" OR Prod$ = "" THEN Fun(9) = "no": Escoge% = -1: EXIT FUNCTION
selecc% = 0: CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS _
 compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
OPEN "F:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
FIELD #2, 35 AS nomshort$, 2 AS num$: max2% = LOF(2) / 37
GOSUB Parche1: GOSUB Parche2
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
          'IF resal% = l THEN LOCATE fila% - 2, 50: PRINT "En Oferta:"; : PRINT USING "#####, "; CVI(Cant4a(l)); : PRINT " a Bs."; : PRINT USING "######,"; CVD(Pvp4a(l))
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
          IF Sonido = "si" THEN BEEP
          GET #2, up%: IF Prod$ <> MID$(nomshort$, 1, LEN(Prod$)) THEN up% = up% + 1
          GOSUB LeeCinco
     END IF
RETURN
Abajo:
     resal% = resal% + 1
     IF resal% > selecc% THEN
          resal% = selecc%: up% = up% + 1
          IF up% > max2% THEN up% = max2%
          IF Sonido = "si" THEN BEEP
          GET #2, (up% + 4): IF Prod$ <> MID$(nomshort$, 1, LEN(Prod$)) THEN up% = up% - 1
          GOSUB LeeCinco
     END IF
RETURN
Apgup:
up% = up% - 4: IF up% < 1 THEN up% = 1: IF Sonido = "si" THEN BEEP
DO
GET #2, up%: IF Prod$ = MID$(nomshort$, 1, LEN(Prod$)) THEN EXIT DO ELSE up% = up% + 1
LOOP
resal% = 1: GOSUB LeeCinco
RETURN
Apgdn:
up% = up% + 4: IF up% > max2% THEN up% = max2%: IF Sonido = "si" THEN BEEP
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
'LOCATE fila% - 2, 50: PRINT "En Oferta:"; : PRINT USING "#####, "; CVI(Cant4a(1)); : PRINT " a Bs."; : PRINT USING "######,"; CVD(Pvp4a(1))
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

SUB Imprime
COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box 1, 1, 3, 80
Center 2, "Imprimir Solicitudes de Reclamo.": unico% = 0
CLOSE : OPEN "F:\kevinprg\kevindat\reclamos.dat" FOR RANDOM AS #5 LEN = 150: selecc% = 0    ': selecc% = final%
FIELD #5, 1 AS acti$, 4 AS numefact$, 4 AS numped$, 8 AS fecfact$, 4 AS horafact$, 10 AS codefarm$, 10 AS codedrog$, 2 AS numcausa$, 2 AS canti$, 8 AS precneto$, 8 AS icsvm$, 30 AS observa1$, 30 AS observa2$, 2 AS numprov$, 6 AS codenew$, 2 AS  _
numreg$, 8 AS pvpfact$, 1 AS auxprint$, 10 AS cedu2$: final% = (LOF(5) / 150)
IF final% = 0 THEN Box 5, 1, 7, 80: Center 6, "NO Hay NINGUNA Solicitud de Reclamo": Espera: EXIT SUB
FOR k = 1 TO final%: GET #5, k: RSET auxprint$ = " ": PUT #5, k: NEXT k
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
FOR k = (final% - 9) TO final%
IF k > 0 THEN
     GET #5, k
     IF acti$ = " " THEN
          selecc% = selecc% + 1
          numfact(selecc%) = numefact$: edofact$(selecc%) = numreg$ + " "
          provee(selecc%) = numprov$: transpor(selecc%) = cedu2$
          fechafact(selecc%) = fecfact$: ubifact(selecc%) = MKI$(k)
          bruto(selecc%) = pvpfact$: neto(selecc%) = precneto$: ivafact(selecc%) = icsvm$
     END IF
END IF
NEXT k
IF selecc% = 0 THEN Box 5, 1, 7, 80: Center 6, "NO Hay NINGUNA Solicitud de Reclamo": Espera: EXIT SUB
OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$
FOR k = 1 TO 199: GET #2, k: Code(k) = codigo$: Contenido(k) = descripcion$: NEXT k: CLOSE #2
FOR k = 1 TO selecc%: FOR l = 1 TO 199
IF CVI(MID$(provee(k), 1, 2)) = VAL(Code(l)) THEN provee(k) = Code(l) + Contenido(l): l = 199
NEXT l: NEXT k
     COLOR colors(7, colorpref), colors(4, colorpref): Box 4, 1, 16, 80: Center 4, "Solicitudes Registradas": LOCATE 5, 2: PRINT "Factura:    Proveedor:      FechaFact:  Articulo:                   ¨Imprimir?"
     Center 25, "[Enter]Escoge sol. [Esc]Salir [F8]Seleccionar [F9]Salir [F10]Imprimir": COLOR colors(7, colorpref), colors(4, colorpref)
     LOCATE 6, 2: PRINT USING "##########,³"; CVL(numfact(1)); : PRINT MID$(provee(1), 4, 15); "³"; MID$(fechafact(1), 1, 2); "/"; MID$(fechafact(1), 3, 2); "/"; MID$(fechafact(1), 5, 4); "³";
     GET #1, CVI(MID$(edofact(1), 1, 2)): PRINT nomb2$; "    ";
     LOCATE 18, 10: PRINT USING "PVP  Bs. ########,.##"; CVD(bruto(1)); : PRINT USING "  Impuesto Bs. ########,.##"; CVD(ivafact(1));
     LOCATE 19, 10: PRINT USING "Neto Bs. ########,.##"; CVD(neto(1)); : PRINT "  Vendedor:"; transpor(1); "  N§ Reclamo:"; CVI(ubifact(1));
     COLOR colors(4, colorpref), colors(7, colorpref)
     FOR k = 2 TO 10
          IF k <= selecc% THEN
               LOCATE 5 + k, 2: PRINT USING "##########,³"; CVL(numfact(k)); : PRINT MID$(provee(k), 4, 15); "³"; MID$(fechafact(k), 1, 2); "/"; MID$(fechafact(k), 3, 2); "/"; MID$(fechafact(k), 5, 4); "³";
               GET #1, CVI(MID$(edofact(k), 1, 2)): PRINT nomb2$; : IF imprimir(k) = "*" THEN PRINT "³si " ELSE PRINT "    "
          ELSE
               LOCATE 5 + k, 2: PRINT SPACE$(78)
          END IF
     NEXT k
     resal% = 1: ventana% = selecc%: up% = final% - 10: IF up% < 1 THEN up% = 1
     DO
          kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
         
          SELECT CASE kbd$
               CASE CHR$(0) + "H": GOSUB Arri2
               CASE CHR$(0) + "P": GOSUB Abaj2
               CASE CHR$(0) + CHR$(73): GOSUB Agup2
               CASE CHR$(0) + CHR$(81): GOSUB Agdn2
               CASE CHR$(0) + CHR$(66)
               IF unico% = 0 THEN
                       unico% = CVI(MID$(provee(resal%), 1, 2)): drogueria$ = MID$(provee(resal%), 4, 15)
                       IF imprimir(resal%) <> "*" THEN
                         imprimir(resal%) = "*": GET #5, CVI(ubifact(resal%)): RSET auxprint$ = "*": PUT #5, CVI(ubifact(resal%))
                       ELSE
                         imprimir(resal%) = " ": GET #5, CVI(ubifact(resal%)): RSET auxprint$ = " ": PUT #5, CVI(ubifact(resal%))
                       END IF
               ELSE
                       IF imprimir(resal%) <> "*" THEN
                         IF unico% = CVI(provee(resal%)) THEN imprimir(resal%) = "*": GET #5, CVI(ubifact(resal%)): RSET auxprint$ = "*": PUT #5, CVI(ubifact(resal%))
                       ELSE
                         imprimir(resal%) = " ": GET #5, CVI(ubifact(resal%)): RSET auxprint$ = " ": PUT #5, CVI(ubifact(resal%))
                       END IF
               END IF: cambiaunico = true
               FOR k = 1 TO final%: GET #5, k
                    IF auxprint$ = "*" THEN cambiaunico = false: EXIT FOR
               NEXT k
               IF cambiaunico = true THEN unico% = 0
               CASE CHR$(0) + CHR$(67): EXIT SUB
               CASE CHR$(0) + CHR$(68): GOSUB Imprim: EXIT SUB
               CASE CHR$(13): eleg = resal%: EXIT DO
               CASE CHR$(27): resal% = 0: up% = 1: EXIT SUB
               CASE ELSE:  BEEP
          END SELECT
     l = 0: IF resal% > 10 AND selecc% > 10 THEN selecc% = 10: resal% = 10
     FOR k = 1 TO 10: l = l + 1
     IF resal% = l THEN
      COLOR colors(7, colorpref), colors(4, colorpref)
     END IF
          IF k <= selecc% THEN
               LOCATE 5 + l, 2: PRINT USING "##########,³"; CVL(numfact(k)); : PRINT MID$(provee(k), 4, 15); "³"; MID$(fechafact(k), 1, 2); "/"; MID$(fechafact(k), 3, 2); "/"; MID$(fechafact(k), 5, 4); "³";
               GET #1, CVI(MID$(edofact(k), 1, 2)): PRINT nomb2$; : IF imprimir(k) = "*" THEN PRINT "³si " ELSE PRINT "    "
          ELSE
               LOCATE 5 + l, 2: PRINT SPACE$(78)
          END IF
     IF resal% = l THEN LOCATE 18, 10: PRINT USING "PVP  Bs. ########,.##"; CVD(bruto(k)); : PRINT USING "  Impuesto Bs. ########,.##"; CVD(ivafact(k)); : LOCATE 19, 10: PRINT USING "Neto Bs. ########,.##"; CVD(neto(k)); : PRINT "  Vendedor:";  _
transpor(k); "  N§ Reclamo:"; CVI(ubifact(k));
     IF resal% = l THEN COLOR colors(4, colorpref), colors(7, colorpref)
     NEXT k
     LOOP
CLOSE : EXIT SUB
    
Imprim:
CLOSE #10: OPEN "F:\kevinprg\kevindat\causas.dat" FOR RANDOM AS #10 LEN = 33
FIELD #10, 2 AS codigo$, 30 AS descripcion$, 1 AS edo$
FOR k = 1 TO 99: GET #10, k: Code(k) = codigo$: Contenido(k) = descripcion$
NEXT k: CLOSE #10
Box 11, 5, 20, 75: COLOR colors(4, colorpref), colors(7, colorpref): Center 12, "Introduzca la Cantidad de Copias.": COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 14, 16: PRINT "Copias a imprimir (min. 1,max. 7) : ": busca$ = MeteDatoLetra(14, 51, 2, "2 ")
IF VAL(busca$) < 0 THEN busca$ = "0"
IF VAL(busca$) > 7 THEN busca$ = "7"
Center 25, "Pulse un numero del uno(1) al tres(3)": formato$ = "1"
COLOR colors(4, colorpref), colors(7, colorpref): Box 11, 5, 20, 75
Center 16, "Formatos disponibles hasta ahora:"
Center 17, "1.- Formato por defecto de Kevin Software 7000"
Center 18, "2.- Formato de Drogueria Capital Valencia     "
Center 19, "3.- Formato de Farmaceutica Blaubach Valencia "
COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 14, 16: PRINT "En que formato desea imprimir 1,2 o 3": formato$ = MeteDatoLetra(14, 52, 2, "1 ")
IF VAL(formato$) < 0 THEN formato$ = "1"
IF VAL(formato$) > 3 THEN formato$ = "1"
OPEN "LPT1" FOR OUTPUT AS #10: WIDTH #10, 255
PRINT #10, CHR$(27); CHR$(64); : PRINT #10, CHR$(27); CHR$(64);
SELECT CASE VAL(formato$)
     CASE 1               ' formato Kevin Software 7000
          PRINT #10, CHR$(27); "C"; CHR$(0); CHR$(9); ' largo de 9 pulgadas
          PRINT #10, CHR$(18); ' sin condensado
          FOR k = 1 TO VAL(busca$)
               'PRINT #10, CHR$(27); "G"; ' double-strike
               PRINT #10, STRING$(80, "-"); : PRINT #10, CHR$(10);
               PRINT #10, STRING$(80, "*"); : PRINT #10, CHR$(10);
               PRINT #10, "*Farmacia: "; NombFarma; " Codigo: "; codefarm$; " SOLICITUD DE RECLAMO   *"; : PRINT #10, CHR$(10);
               PRINT #10, STRING$(80, "*"); : PRINT #10, CHR$(10); : van$ = MKD$(0)
               PRINT #10, CHR$(15); ' condensado
               PRINT #10, SPACE$(5); "ÚÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"; : PRINT #10, "ÄÂÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄ¿"; : PRINT #10, CHR$(10);
               PRINT #10, SPACE$(5); "³RECLAMO³ FACTURA  ³  FECHA   ³  CODIGO  ³       DESCRIPCION PRODUCTO       "; : PRINT #10, " ³CAUSA³CANT.³PRECIO NETO³I.C.S.V.M.³ SUB-TOTAL ³"; : PRINT #10, CHR$(10);
               PRINT #10, SPACE$(5); "ÃÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"; : PRINT #10, "ÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"; : PRINT #10, CHR$(10);
               FOR l = 1 TO final%: GET #5, l
                    IF auxprint$ = "*" THEN
                         PRINT #10, "     ³"; : PRINT #10, USING "**####,³"; l;
                         PRINT #10, USING "#########,³"; CVL(numefact$);
                         PRINT #10, MID$(fecfact$, 3, 2); "/"; MID$(fecfact$, 1, 2); "/"; MID$(fecfact$, 5, 4); "³";
                         PRINT #10, codedrog$; "³"; : GET #1, CVI(numreg$): PRINT #10, nomb2$; "³"; : PRINT #10, USING "#####³"; CVI(numcausa$);
                         PRINT #10, USING "#####³"; CVI(canti$); : PRINT #10, USING "#######,.##³"; CVD(precneto$);
                         PRINT #10, USING "######,.##³"; CVD(icsvm$); : PRINT #10, USING "#######,.##³"; (CVI(canti$) * (CVD(precneto$) + CVD(icsvm$))); : PRINT #10, CHR$(10);
                         van$ = MKD$(CVD(van$) + (CVI(canti$) * (CVD(precneto$) + CVD(icsvm$))))
                    END IF
               NEXT l
               PRINT #10, SPACE$(5); "ÀÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"; : PRINT #10, "ÄÁÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"; : PRINT #10, CHR$(10);
               PRINT #10, SPACE$(5); "                                                                            "; : PRINT #10, "                 TOTAL RECLAMADO:   ³"; : PRINT #10, USING "#######,.##³"; CVD(van$); : PRINT #10, CHR$(10);
               PRINT #10, SPACE$(5); "                                                                            "; : PRINT #10, "                                    ÀÄÄÄÄÄÄÄÄÄÄÄÙ"; : PRINT #10, CHR$(10);
               PRINT #10, CHR$(18); ' sin condensado
               PRINT #10, "Causa(s) de Reclamo :"; : PRINT #10, CHR$(10);
               FOR l = 1 TO final%: GET #5, l
                    IF auxprint$ = "*" THEN
                         PRINT #10, "<"; : PRINT #10, USING "##"; CVI(numcausa$);
                         FOR m = 1 TO 99
                              IF VAL(Code(m)) = CVI(numcausa$) THEN PRINT #10, ">-"; Contenido(m); CHR$(10); : EXIT FOR
                         NEXT m
                         observa$ = observa1$ + " " + observa2$: observa$ = Trim$(observa$)
                         IF LEN(observa$) > 0 THEN PRINT #10, "Observaciones:"; observa$; CHR$(10);
                         PRINT #10, CHR$(10);
                    END IF
               NEXT l
               PRINT #10, "Los montos positivos son a favor de la Farmacia y los negativos a favor del Proveedor."; : PRINT #10, CHR$(10);
               PRINT #10, CHR$(14); ' doble-espacio
               PRINT #10, STRING$(40, "*"); CHR$(10);
               PRINT #10, CHR$(14); ' doble-espacio
               PRINT #10, "* PROVEEDOR :"; drogueria$; SPACE$(11); "*"; CHR$(10);
               PRINT #10, CHR$(14); ' doble-espacio
               PRINT #10, STRING$(40, "*"); CHR$(10);
               PRINT #10, STRING$(80, "-"); : PRINT #10, CHR$(10);
               PRINT #10, CHR$(27); "H"; ' without double-strike
               PRINT #10, CHR$(12);
          NEXT k
     CASE 2                 '  formato Capital
          par% = false
          PRINT #10, CHR$(27); "C"; CHR$(0); CHR$(9); ' largo de 9 pulgadas
          PRINT #10, CHR$(18); ' sin condensado
          FOR k = 1 TO VAL(busca$)
               PRINT #10, CHR$(27); "G"; ' double-strike
               PRINT #10, CHR$(10);
               PRINT #10, CHR$(10);
               PRINT #10, SPACE$(10); NombFarma; SPACE$(28);
               PRINT #10, codefarm$;  'mID$(DATE$, 4, 2); "/"; MID$(DATE$, 1, 2); "/"; MID$(DATE$, 9, 2); : PRINT #10, CHR$(10);
               PRINT #10, CHR$(10);
               PRINT #10, CHR$(10);
               PRINT #10, CHR$(10);
               PRINT #10, CHR$(10);
               van$ = MKD$(0)
               FOR l = 1 TO final%: GET #5, l
                    IF auxprint$ = "*" THEN
                         PRINT #10, USING "########, "; CVL(numefact$); : PRINT #10, codedrog$; SPACE$(11 - LEN(codedrog$));
                         GET #1, CVI(numreg$): PRINT #10, MID$(nomb2$, 1, 12); "   *"; SPACE$(8);
                        
                         'pregunta causa del reclamo
                         COLOR colors(4, colorpref), colors(7, colorpref): Box 10, 5, 20, 75
                         Center 25, "Especifique el numero de causa de reclamo (del 1 al 11)": COLOR colors(7, colorpref), colors(4, colorpref)
                         Center 11, "Reclamo:" + nomb2$
                         Center 13, "Causa de reclamo:             1.-Mercancia no facturada.          "
                         Center 14, " 2.-Mercancia no solicitada.  3.-Faltante.                        "
                         Center 15, " 4.-Diferencia en precio.     5.-Producto diferente al solicitado."
                         Center 16, " 6.-Pedido repetido.          7.-Producto o empaque manchado.     "
                         Center 17, " 8.-Producto incompleto.      9.-Por Vencer.                      "
                         Center 18, "10.-Producto inservible.     11.-Empaque equivocado.              "
                         COLOR colors(7, colorpref), colors(4, colorpref): numcaso$ = "0"
                         DO
                              LOCATE 12, 16: PRINT "Especifique el numero de causa(1-11):": numcaso$ = MeteDatoLetra(12, 52, 2, "  ")
                              IF VAL(numcaso$) > 0 AND VAL(numcaso$) < 12 THEN EXIT DO
                         LOOP
                         PRINT #10, USING "###### "; VAL(numcaso$);
                         PRINT #10, USING "#### "; CVI(canti$);
                         PRINT #10, USING "#####,.## "; CVD(precneto$);
                         PRINT #10, USING "#######,.##"; (CVI(canti$) * (CVD(precneto$)));
                         PRINT #10, CHR$(10);
                         IF par% = true THEN PRINT #10, CHR$(10); : par% = false ELSE par% = true
                         van$ = MKD$(CVD(van$) + (CVI(canti$) * (CVD(precneto$))))
                    END IF
               NEXT l
               PRINT #10, SPACE$(56); "TOTAL:   ";
               PRINT #10, USING "########,.##"; CVD(van$);
               PRINT #10, CHR$(27); "H"; ' without double-strike
               PRINT #10, CHR$(12);
          NEXT k
     CASE 3      ' formato Blaubach pre-impreso
          PRINT #10, CHR$(27); "C"; CHR$(0); CHR$(9); ' largo de 9 pulgadas
          PRINT #10, CHR$(18); ' sin condensado
          PRINT #10, CHR$(27); "G"; ' double-strike
          PRINT #10, CHR$(27); "G"; ' double-strike
          'como es preimpreso , solo se imprime una vez , con papel carbon
               PRINT #10, CHR$(10);
               PRINT #10, CHR$(10);
               PRINT #10, SPACE$(25); NombFarma; SPACE$(20); codefarm$;
               PRINT #10, CHR$(10);
               PRINT #10, CHR$(10);
               PRINT #10, CHR$(10);
               PRINT #10, CHR$(10);
               PRINT #10, CHR$(10);
               PRINT #10, SPACE$(35); "Facturas:"; : van% = 0
               FOR l = 1 TO final%: GET #5, l
                    IF auxprint$ = "*" THEN
                         van% = van% + 1
                         IF van% = 4 THEN PRINT #10, CHR$(10); : PRINT #10, SPACE$(35); "Facturas:";
                         PRINT #10, USING "#########, "; CVL(numefact$);
                    END IF
               NEXT l
               IF van% < 4 THEN PRINT #10, CHR$(10);
               PRINT #10, CHR$(10);
               PRINT #10, CHR$(10);
               PRINT #10, CHR$(10);
               PRINT #10, CHR$(10);
               PRINT #10, CHR$(10); : van$ = MKD$(0): llena% = 0
               FOR l = 1 TO final%: GET #5, l
                    IF auxprint$ = "*" THEN
                         GET #1, CVI(numreg$): llena% = llena% + 1
                         PRINT #10, " "; MID$(nomb2$, 1, 32);
                         PRINT #10, codedrog$; SPACE$(7 - LEN(codedrog$));
                         PRINT #10, USING "###"; CVI(canti$);
                                                  
                         'pregunta causa del reclamo Blaubach
                         COLOR colors(4, colorpref), colors(7, colorpref): Box 10, 5, 22, 75
                         Center 25, "Especifique el numero de causa de reclamo(del 01 al 09):": COLOR colors(7, colorpref), colors(4, colorpref)
                         Center 11, "Reclamo:" + nomb2$
                         Center 13, "Causa de reclamo:            01.-Prod. Facturado y no Enviado.    "
                         Center 14, "02.-Prod. Env. y Fact. c/ Present. Erronea                        "
                         Center 15, "03.-Prod. Enviado y Fact. Otro.                                   "
                         Center 16, "04.-Prod. Enviado y Fact. no solicitado.                          "
                         Center 17, "05.-Prod. Enviado y no Facturado.                                 "
                         Center 18, "06.-Prod. Enviado en mal Estado.                                  "
                         Center 19, "07.-Prod. Env. Proximo a Vencer C/F Reciente.                     "
                         Center 20, "08.-Prod. Proximo a Vencer S/F.                                   "
                         Center 21, "09.-Error en Precio.                                              "
                         COLOR colors(7, colorpref), colors(4, colorpref): numcaso$ = "0"
                         DO
                              LOCATE 12, 16: PRINT "Especifique el numero de causa(01-09):": numcaso$ = MeteDatoLetra(12, 53, 2, "  ")
                              IF VAL(numcaso$) >= 1 AND VAL(numcaso$) <= 9 THEN EXIT DO
                         LOOP
                         PRINT #10, "      ";
                         REM PRINT #10, USING " #### "; VAL(numcaso$);
                        
                         PRINT #10, SPACE$(8);
                         PRINT #10, USING "######,.##"; CVD(precneto$);
                         van$ = MKD$(CVD(van$) + (CVI(canti$) * CVD(precneto$)))
                         PRINT #10, CHR$(10);
                         PRINT #10, CHR$(10);
                    END IF
               NEXT l
               FOR l = 1 TO (6 - llena%)
                  PRINT #10, CHR$(10);
                  PRINT #10, CHR$(10);
               NEXT l
               PRINT #10, SPACE$(60);
               PRINT #10, USING "#############,.##"; CVD(van$);
               PRINT #10, CHR$(27); "H"; ' without double-strike
               PRINT #10, CHR$(12);
END SELECT
RETURN

Arri2:
     lee% = 0: resal% = resal% - 1: IF resal% < 1 THEN resal% = 1: up% = up% - 1: lee% = 1
     IF up% < 1 THEN up% = 1: IF Sonido = "si" THEN BEEP
     IF lee% = 1 THEN GOSUB LeeDiez2: GOSUB BuscActivo2
RETURN
Abaj2:
     lee% = 0: resal% = resal% + 1
     IF resal% >= selecc% THEN
         IF selecc% > 10 THEN
            selecc% = 10: resal% = last
         ELSE
            resal% = selecc%
         END IF
         up% = up% + 1: lee% = 2
     END IF
     IF up% > final% THEN
      up% = final%: IF Sonido = "si" THEN BEEP
     END IF
     IF lee% = 2 THEN
      GOSUB LeeDiez2: GOSUB BuscActivo2
     END IF
RETURN
Agup2:
     up% = up% - 9: lee% = 1
     IF up% < 1 THEN up% = 1: resal% = 1: IF Sonido = "si" THEN BEEP
     GOSUB LeeDiez2: GOSUB BuscActivo2
RETURN
Agdn2:
     up% = up% + 9: lee% = 1
     IF up% >= final% THEN
         up% = final%: resal% = 1: IF Sonido = "si" THEN BEEP
     END IF
     GOSUB LeeDiez2: GOSUB BuscActivo2
RETURN
LeeDiez2:
selecc% = 0
FOR k = up% TO final%
     IF k > 0 AND k <= final% THEN
          GET #5, k
          IF acti$ = " " THEN
               selecc% = selecc% + 1
               IF selecc% <= 10 THEN
                    numfact(selecc%) = numefact$: edofact$(selecc%) = numreg$ + " "
                    provee(selecc%) = numprov$: transpor(selecc%) = cedu2$
                    fechafact(selecc%) = fecfact$: ubifact(selecc%) = MKI$(k): IF auxprint$ = "*" THEN imprimir(selecc%) = "*" ELSE imprimir(selecc%) = " "
                    bruto(selecc%) = pvpfact$: neto(selecc%) = precneto$: ivafact(selecc%) = icsvm$
               ELSE
                    EXIT FOR
               END IF
          END IF
     END IF
NEXT k
FOR k = 1 TO selecc%: FOR l = 1 TO 199
   IF k < 11 THEN
      IF CVI(MID$(provee(k), 1, 2)) = VAL(Code(l)) THEN
         provee(k) = Code(l) + Contenido(l)
         l = 199
      END IF
   END IF
NEXT l: NEXT k
IF lee% = 2 THEN
   resal% = selecc%
END IF
RETURN
BuscActivo2:
encontrado = false: GET #5, up%: IF acti$ = " " THEN encontrado = true
DO WHILE encontrado = false
     IF lee% = 1 THEN
          up% = up% - 1
          IF up% < 1 THEN
               FOR k = 1 TO final%
                    GET #5, k: IF acti$ = " " THEN up% = k: EXIT DO
               NEXT k
          END IF
     END IF
     IF lee% = 2 THEN
          up% = up% + 1
          IF up% > final% THEN
               FOR k = final% TO 1 STEP -1
                    GET #5, k: IF acti$ = " " THEN up% = k: EXIT DO
               NEXT k
          END IF
     END IF
     GET #5, up%: IF acti$ = " " THEN encontrado = true
LOOP
RETURN
END SUB

SUB LeeEstado
OPEN "F:\kevinprg\kevindat\kaos.dat" FOR INPUT AS #2
INPUT #2, Sonido: INPUT #2, Impuesto: INPUT #2, NombFarma: INPUT #2, Direccion
INPUT #2, Rif: INPUT #2, Nit: INPUT #2, Telefono: INPUT #2, contri: INPUT #2, contra
INPUT #2, Stock: INPUT #2, Blanq: INPUT #2, LargoH: INPUT #2, TipoL: INPUT #2, Mensaje1
INPUT #2, Mensaje2: INPUT #2, Mensaje3: INPUT #2, Redondeo: INPUT #2, Desc
INPUT #2, DescMax: INPUT #2, Exactitud: INPUT #2, Prov1
INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo
CLOSE
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
MeteDatoLetra$ = eleccion$: IF Sonido = "si" THEN BEEP
EXIT FUNCTION
Sumar:
FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE fila%, columna%: PRINT "²"; eleccion$; "²"
RETURN
END FUNCTION

SUB Reclamos (edo%)
IF edo% = 0 THEN
     COLOR colors(7, colorpref), colors(4, colorpref): CLS : CALL Box(1, 1, 24, 80): Center 2, "Creacion de una Solicitud de Reclamo SIN Factura Previa": mjfox% = 0
     DO WHILE mjfox% = 0
          mjfox% = Escoge%(5, 1, 3, "      ")
     LOOP: IF mjfox% <= 0 THEN EXIT SUB
     CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
     FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS  _
cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$ _
, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
     OPEN "F:\kevinprg\kevindat\reclamos.dat" FOR RANDOM AS #5 LEN = 150
     FIELD #5, 1 AS acti$, 4 AS numefact$, 4 AS numped$, 8 AS fecfact$, 4 AS horafact$, 10 AS codefarm$, 10 AS codedrog$, 2 AS numcausa$, 2 AS canti$, 8 AS precneto$, 8 AS icsvm$, 30 AS observa1$, 30 AS observa2$, 2 AS numprov$, 6 AS codenew$, 2 AS  _
numreg$, 8 AS pvpfact$, 1 AS auxprint$, 10 AS cedu2$
     registr% = (LOF(5) / 150) + 1: GET #1, mjfox%
     RSET acti$ = " ": RSET numefact$ = MKL$(0): RSET numped$ = MKL$(0)
     RSET fecfact$ = SPACE$(8): RSET horafact$ = "0000"
     RSET codefarm$ = SPACE$(6)
     RSET codedrog$ = SPACE$(10): RSET numcausa$ = MKI$(0): RSET canti$ = MKI$(0)
     RSET precneto$ = MKD$(0): RSET icsvm$ = MKD$(0)
     RSET observa1$ = SPACE$(30): RSET observa2$ = SPACE$(30)
     RSET numprov$ = MKI$(0): RSET codenew$ = codigo2$
     RSET numreg$ = MKI$(mjfox%): RSET pvpfact$ = MKD$(0)
ELSE
     CLOSE : OPEN "F:\kevinprg\kevindat\reclamos.dat" FOR RANDOM AS #5 LEN = 150
     FIELD #5, 1 AS acti$, 4 AS numefact$, 4 AS numped$, 8 AS fecfact$, 4 AS horafact$, 10 AS codefarm$, 10 AS codedrog$, 2 AS numcausa$, 2 AS canti$, 8 AS precneto$, 8 AS icsvm$, 30 AS observa1$, 30 AS observa2$, 2 AS numprov$, 6 AS codenew$, 2 AS  _
numreg$, 8 AS pvpfact$, 1 AS auxprint$, 10 AS cedu2$
     registr% = (LOF(5) / 150) + 1: CLS
     OPEN num2 FOR RANDOM AS #2 LEN = 40: FIELD #2, 6 AS odigo$, 2 AS antid$, 8 AS recio$, 8 AS osto$, 8 AS ferta$, 2 AS oferantid$, 1 AS va$, 2 AS sitio$, 3 AS venci$
     OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
     FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS  _
cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$ _
, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
     GET #2, edo%: GET #1, CVI(sitio$)
     LOCATE 1, 1: PRINT odigo$, codigo2$
     RSET acti$ = " ": RSET numefact$ = numfact(eleg): RSET numped$ = MKL$(0)
     RSET fecfact$ = MID$(fechafact(eleg), 3, 2) + MID$(fechafact(eleg), 1, 2) + MID$(fechafact(eleg), 5, 4): RSET horafact$ = "0000"
     RSET codedrog$ = SPACE$(10): RSET numcausa$ = MKI$(0): RSET canti$ = MKI$(0): RSET precneto$ = MKD$((CVD(osto$) / (CVI(antid$) + CVI(oferantid$))) * CVD(neto(eleg)) / CVD(bruto(eleg))): difcosto$ = precneto$: difrealpvp$ = "0"
     IF va$ <> " " THEN RSET icsvm$ = MKD$(CVD(precneto$) * (VAL(Impuesto) / 100) * CVD(neto(eleg)) / CVD(bruto(eleg))) ELSE RSET icsvm$ = MKD$(0)
     RSET observa1$ = SPACE$(30): RSET observa2$ = SPACE$(30): difoldiva$ = icsvm$: difoldpvfac$ = recio$
     RSET numprov$ = MKI$(VAL(MID$(provee(eleg), 1, 3))): RSET codenew$ = odigo$: difoldprecnet$ = precneto$
     RSET numreg$ = sitio$
     RSET pvpfact$ = recio$: auxpvpfact$ = MKD$(0)
     OPEN "f:\kevinprg\kevindat\fichapro.vee" FOR RANDOM SHARED AS #15 LEN = 400
     FIELD #15, 20 AS rif5$, 20 AS nit5$, 2 AS diacred5$, 4 AS monmax5$, 60 AS nombre5$, 60 AS direc15$, 20 AS tele5$, 10 AS codicli5$, 30 AS autor15$, 30 AS autor25$, 30 AS autor35$, 30 AS autor45$, 84 AS vacio$
     GET #15, VAL(MID$(provee(eleg), 1, 3)): RSET codefarm$ = codicli5$: CLOSE #15
END IF
Salir = false: GOSUB Presenta: sit% = 1
     DO WHILE Salir = false
        Center 25, "[F1]Ayuda [F9]Salir y Salvar [F10]Abortar Reclamo":
        SELECT CASE sit%
        CASE 1
          auxi$ = EntraDatoNumero(3, 29, 10, STR$(CVL(numefact$)), 0): LSET numefact$ = MKL$(VAL(auxi$))
          IF devolver = true THEN devolver = false: sit% = 17 ELSE sit% = 2
        CASE 2
          auxi$ = EntraDatoNumero(4, 29, 10, STR$(CVL(numped$)), 0): LSET numped$ = MKL$(VAL(auxi$))
          IF devolver = true THEN devolver = false: sit% = 1 ELSE sit% = 3
        CASE 3
          DO
               auxi$ = MeteDatoLetra(5, 29, 10, (MID$(fecfact$, 1, 2) + "/" + MID$(fecfact$, 3, 2) + "/" + MID$(fecfact$, 5, 4)))
               IF CheqDate(auxi$, 1) = true THEN EXIT DO ELSE Center 25, "Introduzca un fecha correcta": BEEP: Center 25, "[F1]Ayuda [F9]Salir y Salvar [F10]Abortar Reclamo"
          LOOP
          RSET fecfact$ = MID$(auxi$, 1, 2) + MID$(auxi$, 4, 2) + MID$(auxi$, 7, 4)
          IF devolver = true THEN devolver = false: sit% = 2 ELSE sit% = 4
        CASE 4
          DO
               auxi$ = MeteDatoLetra(6, 29, 5, (MID$(horafact$, 1, 2) + ":" + MID$(horafact$, 3, 2)))
               IF VAL(MID$(auxi$, 1, 2)) >= 0 AND VAL(MID$(auxi$, 1, 2)) <= 23 THEN
                    IF VAL(MID$(auxi$, 4, 2)) >= 0 AND VAL(MID$(auxi$, 4, 2)) < 60 THEN EXIT DO ELSE BEEP: Center 25, "Introduzca unos minutos correctos: de 00 a 59"
               ELSE
                    BEEP: Center 25, "Introduzca una hora correcta: de 00 a 23"
               END IF
          LOOP: RSET horafact$ = MID$(auxi$, 1, 2) + MID$(auxi$, 4, 2)
          IF devolver = true THEN devolver = false: sit% = 3 ELSE sit% = 5
        CASE 5
          auxi$ = MeteDatoLetra(7, 29, 10, codefarm$): LSET codefarm$ = auxi$
          IF devolver = true THEN devolver = false: sit% = 4 ELSE sit% = 6
        CASE 6
          auxi$ = MeteDatoLetra(8, 29, 10, codedrog$): LSET codedrog$ = auxi$
          IF devolver = true THEN devolver = false: sit% = 5 ELSE sit% = 7
        CASE 7
          IF CVI(numcausa$) <> 1 THEN
          mjfox% = Elije%(6, 4, 47, 3, 44, 0, 0): COLOR colors(7, colorpref), colors(4, colorpref)
          IF mjfox% > 0 THEN RSET numcausa$ = MKI$(VAL(Code(mjfox%))): action% = VAL(Accion(mjfox%))
          Borrabox 3, 43, 21, 78: LOCATE 10, 2
          IF CVI(numcausa$) > 0 THEN
               FOR l = 1 TO 99
                    IF CVI(numcausa$) = VAL(Code(l)) THEN PRINT "²"; Code(l); "²"; Contenido(l); "²": EXIT FOR
                    IF CVI(numcausa$) = 1 THEN
                         CALL Box(3, 43, 21, 78): action% = 4
                         LOCATE 4, 44: PRINT "*Diferencia de Precios*"
                         LOCATE 5, 44: PRINT "Teclee el PVP Verdadero:": auxi$ = "0"
                         DO WHILE VAL(difrealpvp$) <= 0
                              difrealpvp$ = EntraDatoNumero(6, 44, 11, difrealpvp$, 0)
                         LOOP
                         DO WHILE CVD(pvpfact$) <= 0
                              LOCATE 8, 44: PRINT "Teclee el PVP de factura:1"; : auxi$ = EntraDatoNumero(9, 44, 11, STR$((FIX(CVD(pvpfact$) * 100)) / 100), 0)
                              IF VAL(auxi$) > 0 THEN RSET pvpfact$ = MKD$(VAL(auxi$))
                         LOOP
                         DO WHILE CVD(precneto$) <= 0
                              LOCATE 11, 44: PRINT "Teclee el PVP de factura:2"
                              auxi$ = EntraDatoNumero(12, 44, 11, STR$((FIX(CVD(precneto$) * 100)) / 100), 0)
                              IF VAL(auxi$) > 0 THEN RSET precneto$ = MKD$(VAL(auxi$))
                         LOOP
                         IF CVD(auxpvpfact$) = 0 THEN
                              auxe$ = MKD$((VAL(difrealpvp$) * CVD(precneto$) / CVD(pvpfact$))): RSET precneto$ = MKD$(CVD(precneto$) - CVD(auxe$))
                         ELSE
                              auxe$ = MKD$((VAL(difrealpvp$) * CVD(precneto$) / CVD(auxpvpfact$))): RSET precneto$ = MKD$(CVD(precneto$) - CVD(auxe$))
                         END IF
                         IF CVD(icsvm$) > 0 THEN RSET icsvm$ = MKD$(CVD(precneto$) * (VAL(Impuesto) / 100))
                         GOSUB Presenta: EXIT FOR
                    END IF
               NEXT l
          ELSE
               PRINT " No seleccionada    "
          END IF
          END IF
          IF devolver = true THEN devolver = false: sit% = 6 ELSE sit% = 8
        CASE 8
          DO
               auxi$ = EntraDatoNumero(11, 29, 6, STR$(CVI(canti$)), 0): IF CVI(auxi$) > 0 AND CVI(auxi$) < 32501 THEN EXIT DO
          LOOP
          LSET canti$ = MKI$(VAL(auxi$))
          IF devolver = true THEN sit% = 7 ELSE sit% = 12
        CASE 12
          DO
               auxi$ = EntraDatoNumero(12, 29, 11, STR$((FIX(CVD(precneto$) * 100)) / 100), 0): IF VAL(auxi$) > 0 THEN EXIT DO
          LOOP
          LSET precneto$ = MKD$(VAL(auxi$))
          IF devolver = true THEN devolver = false: sit% = 8 ELSE sit% = 13
        CASE 13
          DO
               auxi$ = EntraDatoNumero(13, 29, 11, STR$((FIX(CVD(icsvm$) * 100)) / 100), 0): IF CVD(icsvm$) >= 0 THEN EXIT DO
          LOOP
          LSET icsvm$ = MKD$(VAL(auxi$))
          IF devolver = true THEN devolver = false: sit% = 12 ELSE sit% = 14
        CASE 14
          auxi$ = MeteDatoLetra(15, 2, 30, observa1$): LSET observa1$ = auxi$
          IF devolver = true THEN devolver = false: sit% = 13 ELSE sit% = 15
        CASE 15
          auxi$ = MeteDatoLetra(17, 2, 30, observa2$): LSET observa2$ = auxi$
          IF devolver = true THEN devolver = false: sit% = 14 ELSE sit% = 16
        CASE 16
          mjfox% = 0: mjfox% = Elije%(5, 4, 60, 3, 59, 0, 0): COLOR colors(7, colorpref), colors(4, colorpref)
          IF mjfox% > 0 THEN RSET numprov$ = MKI$(VAL(Code(mjfox%)))
          Borrabox 3, 43, 21, 78: LOCATE 19, 2
          IF CVI(numprov$) > 0 THEN
               FOR l = 1 TO 99
                    IF CVI(numprov$) = VAL(Code(l)) THEN PRINT "²"; Code(l); "²"; Contenido(l); "²": EXIT FOR
               NEXT l
          ELSE
               PRINT " No seleccionado    "
          END IF
          IF devolver = true THEN devolver = false: sit% = 15 ELSE sit% = 17
        CASE 17
          DO
               auxi$ = EntraDatoNumero(20, 29, 11, STR$((FIX(CVD(pvpfact$) * 100)) / 100), 0): IF VAL(auxi$) > 0 THEN EXIT DO
          LOOP
          LSET pvpfact$ = MKD$(VAL(auxi$)): difoldpvfac$ = pvpfact$
          IF devolver = true THEN devolver = false: sit% = 16 ELSE sit% = 1
        END SELECT
        IF Fun(9) = "si" THEN       'salva  reclamo
             'verifica si el reclamo esta bien hecho
            Fun(9) = "no": salvar = true
            IF CVL(numefact$) <= 0 THEN Box 12, 10, 14, 70: Center 13, "Debe introducir un numero de factura mayor que 0.": Espera: GOSUB Presenta: salvar = false: sit% = 1
            IF LEN(codefarm$) = 0 THEN Box 12, 10, 14, 70: Center 13, "Debe introducir el codigo de cliente de esta Farmacia.": Espera: GOSUB Presenta: salvar = false: sit% = 5
            IF CVI(numcausa$) <= 0 THEN Box 12, 10, 14, 70: Center 13, "Debe especificar una causa de reclamo.": Espera: GOSUB Presenta: salvar = false: sit% = 7
            IF CVI(canti$) <= 0 THEN Box 12, 10, 14, 70: Center 13, "Debe especificar una cantidad de unidades mayor que cero.": Espera: GOSUB Presenta: salvar = false: sit% = 8
            IF CVD(precneto$) = 0 THEN Box 12, 10, 14, 70: Center 13, "Debe introducir un monto neto a reclamar mayor que 0.": Espera: GOSUB Presenta: salvar = false: sit% = 12
            IF CVI(numprov$) <= 0 THEN Box 12, 10, 14, 70: Center 13, "Debe especificar un proveedor a quien reclamar.": Espera: GOSUB Presenta: salvar = false: sit% = 16
            IF CVD(pvpfact$) <= 0 THEN Box 12, 10, 14, 70: Center 13, "Debe introducir un precio de venta mayor que 0.": Espera: GOSUB Presenta: salvar = false: sit% = 17
            IF salvar = true THEN
                SELECT CASE action%
                  CASE 0: res% = Verify(13, "Este reclamo NO ALTERARA las existencias: ¨desea continuar")
                  CASE 1: res% = Verify(13, "Este reclamo SUMARA a las existencias: ¨desea continuar")
                  CASE 2: res% = Verify(13, "Este reclamo RESTARA de las existencias: ¨desea continuar")
                  CASE 4: res% = Verify(13, "Este reclamo es por DIFERENCIA EN PRECIOS ¨desea continuar")
                END SELECT
                IF res% = true THEN
                   CLOSE #37: OPEN "f:\kevinprg\kevindat\saler.dat" FOR RANDOM AS #37 LEN = 100
                   FIELD #37, 1 AS acti2$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
                   nursaler = LOF(37) / 100: eleg% = 0: IF nursaler = 0 THEN Box 11, 5, 12, 75: Center 11, "No hay Vendedores Registrados": Espera: CLOSE : EXIT SUB
                   DO WHILE eleg% = 0
                       Box 11, 5, 20, 75: COLOR colors(4, colorpref), colors(7, colorpref): Center 12, "Introduzca su Clave de Vendedor.": COLOR colors(7, colorpref), colors(4, colorpref)
                       LOCATE 14, 16: PRINT "Clave Corta del Vendedor:": busca$ = MeteDatoLetra(14, 41, 3, "   ")
                       FOR k = 1 TO nursaler: GET #37, k
                       IF busca$ = clco$ AND acti2$ = " " THEN eleg% = k: cedula$ = clla$
                       NEXT k: IF eleg% = 0 THEN Center 14, "No Hay Ningun Vendedor Con Esa Clave.": Espera
                   LOOP
                   RSET cedu2$ = cedula$: GET #1, CVI(numreg$)
                   ajustado = false: LOCK #1, CVI(numreg$)
                   IF CVI(numcausa$) = 1 THEN
                        IF difoldpvfac$ = pv12$ AND CVI(exi12$) >= CVI(canti$) THEN RSET exi12$ = MKI$(CVI(exi12$) - CVI(canti$)): ajustado = true
                        IF difoldpvfac$ = pv22$ AND CVI(exi22$) >= CVI(canti$) AND ajustado = false THEN RSET exi22$ = MKI$(CVI(exi22$) - CVI(canti$)): ajustado = true
                        IF difoldpvfac$ = pv32$ AND CVI(exi32$) >= CVI(canti$) AND ajustado = false THEN RSET exi32$ = MKI$(CVI(exi32$) - CVI(canti$)): ajustado = true
                        IF ajustado = true THEN
                           sumado = false: difrealpvp$ = MKD$(VAL(difrealpvp$))
                           IF CVD(difoldiva$) > 0 THEN auxo% = 1 ELSE auxo% = 0
                           IF difrealpvp$ = pv12$ THEN RSET exi12$ = MKI$(CVI(exi12$) + CVI(canti$)): sumado = true
                           IF difrealpvp$ = pv22$ AND sumado = false THEN RSET exi22$ = MKI$(CVI(exi22$) + CVI(canti$)): sumado = true
                           IF difrealpvp$ = pv32$ AND sumado = false THEN RSET exi32$ = MKI$(CVI(exi32$) + CVI(canti$)): sumado = true
                           IF sumado = false THEN
                             IF CVI(exi12$) = 0 THEN RSET exi12$ = canti$: sumado = true: RSET cos12$ = MKD$(CVD(auxe$) + (auxo% * CVD(auxe$) * (VAL(Impuesto) / 100))): RSET pv12$ = difrealpvp$
                             IF CVI(exi22$) = 0 AND sumado = false THEN RSET exi22$ = MKI$(CVI(exi22$) + CVI(canti$)): sumado = true: RSET cos22$ = MKD$(CVD(auxe$) + (auxo% * CVD(auxe$) * (VAL(Impuesto) / 100))): RSET pv22$ = difrealpvp$
                             IF CVI(exi32$) = 0 AND sumado = false THEN RSET exi32$ = MKI$(CVI(exi32$) + CVI(canti$)): sumado = true: RSET cos32$ = MKD$(CVD(auxe$) + (auxo% * CVD(auxe$) * (VAL(Impuesto) / 100))): RSET pv32$ = difrealpvp$
                           END IF
                           IF sumado = false THEN ajustado = false
                        END IF
                   ELSE
                    'determina si resta, suma o no toma acci¢n
                    SELECT CASE action%
                      CASE 0 ' no hace nada
                        ajustado = true
                      CASE 1 'suma
                        IF pvpfact$ = pv12$ THEN RSET exi12$ = MKI$(CVI(exi12$) + CVI(canti$)): ajustado = true
                        IF pvpfact$ = pv22$ AND ajustado = false THEN RSET exi22$ = MKI$(CVI(exi22$) + CVI(canti$)): ajustado = true
                        IF pvpfact$ = pv32$ AND ajustado = false THEN RSET exi32$ = MKI$(CVI(exi32$) + CVI(canti$)): ajustado = true
                        IF ajustado = false THEN
                             IF CVI(exi12$) = 0 THEN exi12$ = canti$: ajustado = true: RSET cos12$ = MKD$(CVD(precneto$) + CVD(icsvm$)): RSET pv12$ = pvpfact$
                             IF CVI(exi22$) = 0 AND ajustado = false THEN RSET exi22$ = canti$: ajustado = true: RSET cos22$ = MKD$(CVD(precneto$) + CVD(icsvm$)): RSET pv22$ = pvpfact$
                             IF CVI(exi32$) = 0 AND ajustado = false THEN RSET exi32$ = canti$: ajustado = true: RSET cos32$ = MKD$(CVD(precneto$) + CVD(icsvm$)): RSET pv32$ = pvpfact$
                             IF CVI(exi42$) = 0 AND ajustado = false THEN RSET exi42$ = canti$: ajustado = true: RSET cos42$ = MKD$(CVD(precneto$) + CVD(icsvm$)): RSET pv42$ = pvpfact$
                        END IF
                      CASE 2 'resta
                        IF pvpfact$ = pv12$ AND CVI(exi12$) >= CVI(canti$) THEN RSET exi12$ = MKI$(CVI(exi12$) - CVI(canti$)): ajustado = true
                        IF pvpfact$ = pv22$ AND ajustado = false AND CVI(exi22$) >= CVI(canti$) THEN RSET exi22$ = MKI$(CVI(exi22$) - CVI(canti$)): ajustado = true
                        IF pvpfact$ = pv32$ AND ajustado = false AND CVI(exi32$) >= CVI(canti$) THEN RSET exi32$ = MKI$(CVI(exi32$) - CVI(canti$)): ajustado = true
                     END SELECT
                   END IF
                   IF ajustado = true THEN
                       PUT #1, CVI(numreg$): PUT #5, registr%
                       Salir = true: UNLOCK #1, CVI(numreg$)
                       'ojo colocar aqui bitacora
                   ELSE
                       Box 2, 5, 23, 75
                       Center 3, "************************************"
                       Center 4, "*********  A T E N C I O N *********"
                       Center 5, "************************************"
                      
                       Center 7, "El reclamo no pudo ser salvado debido a una o alguna(s) de las"
                       Center 8, "siguinetes razones:                                           "
                      
                       Center 10, "* El precio reclamado no es igual a ninguno de los precios que"
                       Center 11, "  tiene registrado la computadora.                            "
                      
                       Center 13, "* Los precios coinciden, pero la cantidad reclamada es mayor a"
                       Center 14, "  la que tiene registrada la computadora.                     "
                      
                       Center 16, "* Si el reclamo es por DIFERENCIA DE PRECIOS, hay la posibili-"
                       Center 17, "  dad de que el precio 1,2 ¢ 3 tengan existencia con un precio"
                       Center 18, "  diferente al que realmente lleg¢. Esto sucede rara vez  pero"
                       Center 19, "  se debe tomar en cuenta como una posibilidad remota.        "
                      
                       Center 21, "EN TODO CASO USTED DEBE  REVISAR BIEN ESTE RECLAMO  PARA PODER"
                       Center 22, "SER GUARDADO CORRECTAMENTE. (versi¢n 1.007.001 corregida)     "
                       CALL Espera: GOSUB Presenta: sit% = 17: UNLOCK #1, CVI(numreg$)
                   END IF
                END IF
            END IF
        END IF
        IF Fun(10) = "si" THEN Salir = true: Fun(10) = "no"  'aborta reclamo
     LOOP
EXIT SUB
Presenta:
COLOR colors(7, colorpref), colors(4, colorpref): CLS : CALL Box(1, 1, 24, 80)
CALL Center(1, "Creando Solicitud de Reclamo #" + STR$(registr%))   'ojo registr% es el numero de registro en el archivo reclamos.dat
LOCATE 2, 2: PRINT "Articulo en reclamacion:"; nomb2$
LOCATE 3, 2: PRINT "N£mero de Factura         :²"; : PRINT USING "#########,²"; CVL(numefact$)
LOCATE 4, 2: PRINT "N£mero de Pedido          :²"; : PRINT USING "#########,²"; CVL(numped$)
LOCATE 5, 2: PRINT "Fecha Factura (dd/mm/aaaa):²"; MID$(fecfact$, 1, 2); "/"; MID$(fecfact$, 3, 2); "/"; MID$(fecfact$, 5, 4); "²"
LOCATE 6, 2: PRINT "Hora  Factura (hh:mm)     :²"; MID$(horafact$, 1, 2); ":"; MID$(horafact$, 3, 2); "²"
LOCATE 7, 2: PRINT "Codigo Farmacia(Cliente)  :²"; codefarm$; "²"
LOCATE 8, 2: PRINT "Codigo Articulo(Droguer¡a):²"; codedrog$; "²"
LOCATE 9, 2: PRINT "Causa de Reclamo N£mero   :"; : LOCATE 10, 2
CLOSE #2: OPEN "F:\kevinprg\kevindat\causas.dat" FOR RANDOM AS #2 LEN = 33: FIELD #2, 2 AS codigo$, 30 AS descripcion$, 1 AS edo$
FOR k = 1 TO 99: GET #2, k: Code(k) = codigo$: Contenido(k) = descripcion$
NEXT k: CLOSE #2
IF CVI(numcausa$) > 0 THEN
     FOR l = 1 TO 99
          IF CVI(numcausa$) = VAL(Code(l)) THEN PRINT "²"; Code(l); "²"; Contenido(l); "²": EXIT FOR
     NEXT l
ELSE
     PRINT " No seleccionada    "
END IF
LOCATE 11, 2: PRINT "Cantidad      (unidades)  :²"; : PRINT USING "#####,²"; CVI(canti$)
LOCATE 12, 2: PRINT "Precio Neto,Costo  Unidad :²"; : PRINT USING "#######,.##²"; CVD(precneto$)
LOCATE 13, 2: PRINT "I.C.S.V.M. (impuesto IVA) :²"; : PRINT USING "#######,.##²"; CVD(icsvm$)
LOCATE 14, 2: PRINT "Observaciones  linea #1   :";
LOCATE 15, 2: PRINT "²"; observa1$; "²"
LOCATE 16, 2: PRINT "Observaciones  linea #2   :";
LOCATE 17, 2: PRINT "²"; observa2$; "²"
LOCATE 18, 2: PRINT "Droguer¡a ¢ Proveedor     :"; : LOCATE 19, 2
CLOSE #2: OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$
FOR k = 1 TO 199: GET #2, k: Code(k) = codigo$: Contenido(k) = descripcion$
NEXT k: CLOSE #2
IF CVI(numprov$) > 0 THEN
     FOR l = 1 TO 199
          IF CVI(numprov$) = VAL(Code(l)) THEN PRINT "²"; Code(l); "²"; Contenido(l); "²": EXIT FOR
     NEXT l
ELSE
     PRINT " No seleccionado    "
END IF
LOCATE 20, 2: PRINT "P.V.P. precio por Unidad  :²"; : PRINT USING "#######,.##²"; CVD(pvpfact$)
LOCATE 21, 2: PRINT "Codigo Art¡culo(farmacia) :²"; codenew$; "²"
IF VAL(difrealpvp$) > 0 THEN LOCATE 22, 2: PRINT "El PVP IMPRESO en el articulo en reclamo es de Bs."; : PRINT USING "#########,.##"; VAL(difrealpvp$)
RETURN
END SUB

SUB Salida
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... ": COLOR colors(7, colorpref), colors(4, colorpref)
CLOSE : OPEN "f:\kevinprg\kevindat\compras.tkt" FOR RANDOM AS #1 LEN = 2: FIELD #1, 2 AS ticket$: RSET ticket$ = "si": PUT #1, 1: CLOSE
CHAIN "F:\kevinprg\kevin01.bas"
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

FUNCTION Trim$ (x$)
IF x$ = "" THEN
     Trim$ = ""
ELSE
lastChar = 0
FOR a = 1 TO LEN(x$): y$ = MID$(x$, a, 1): IF y$ <> CHR$(0) AND y$ <> " " THEN lastChar = a
NEXT a: Trim$ = LEFT$(x$, lastChar)
END IF
END FUNCTION

SUB VerFactCompras
COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box 1, 1, 3, 80
Center 2, "Creacion de una NUEVA Solicitud de Reclamo.": comienzo% = 0
CLOSE : OPEN "F:\kevinprg\kevindat\master.fct" FOR RANDOM SHARED AS #27 LEN = 70: FIELD #27, 70 AS linea$: final% = (LOF(27) / 70): selecc% = 0    ': selecc% = final%
IF final% = 0 THEN Box 5, 1, 7, 80: Center 6, "NO Hay NINGUNA Factura Para Visualizar": Espera: EXIT SUB
FOR k = final% TO 1 STEP -1
     GET #27, k: IF MID$(linea$, 1, 1) = " " THEN comienzo% = comienzo% + 1
     IF comienzo% > 9 THEN comienzo% = k: EXIT FOR
NEXT k
FOR k = comienzo% TO final%
IF k > 0 THEN
     GET #27, k
     IF MID$(linea$, 1, 1) = " " THEN
          selecc% = selecc% + 1: numfact(selecc%) = MID$(linea$, 2, 4): provee(selecc%) = MID$(linea$, 24, 2): edofact(selecc%) = MID$(linea$, 26, 1) + MID$(linea$, 59, 1) + MID$(linea$, 62, 1)
          fechaacc(selecc%) = MID$(linea$, 6, 8): transpor(selecc%) = MID$(linea$, 14, 10): fechafact(selecc%) = MID$(linea$, 51, 8): ubifact(selecc%) = MKI$(k)
          bruto(selecc%) = MID$(linea$, 27, 8): neto(selecc%) = MID$(linea$, 35, 8): ivafact(selecc%) = MID$(linea$, 43, 8)
     END IF
END IF
NEXT k
IF selecc% = 0 THEN Box 5, 1, 7, 80: Center 6, "NO Hay NINGUNA Factura Para Visualizar": Espera: sitio = 0: Reclamos (sitio): EXIT SUB
OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #2 LEN = 18: FIELD #2, 3 AS codigo$, 15 AS descripcion$
FOR k = 1 TO 199: GET #2, k: Code(k) = codigo$: Contenido(k) = descripcion$: NEXT k: CLOSE #2
FOR k = 1 TO selecc%: FOR l = 1 TO 199
IF CVI(MID$(provee(k), 1, 2)) = VAL(Code(l)) THEN provee(k) = Code(l) + Contenido(l): l = 199
NEXT l: NEXT k
     COLOR colors(7, colorpref), colors(4, colorpref): Box 4, 1, 16, 80: Center 4, "Facturas Registradas": LOCATE 5, 2: PRINT "Factura:         Proveedor:       FechaFact:  FechaAcce:  Transcrita: F.Pago:"
     Center 25, "[Enter]Escoge fact.[Esc]Salir [F9]Ir a Reclamo sin factura": COLOR colors(7, colorpref), colors(4, colorpref)
     LOCATE 6, 2: PRINT USING "###############,³"; CVL(numfact(1)); : PRINT MID$(provee(1), 4, 15); "³ "; MID$(fechafact(1), 3, 2); "/"; MID$(fechafact(1), 1, 2); "/"; MID$(fechafact(1), 5, 4); "³ "; MID$(fechaacc(1), 1, 2); "/"; MID$(fechaacc(1), 3 _
, 2); "/"; MID$(fechaacc(1), 5, 4); "³ "; transpor(1);
     IF MID$(edofact(1), 1, 1) = "*" THEN PRINT " ³Contado ";  ELSE PRINT " ³Credito ";
     'IF MID$(edofact(1), 2, 1) = "*" THEN PRINT "   SI   ";  ELSE PRINT "   NO   ";
     LOCATE 18, 4: PRINT USING "Bruto Bs. ########,.##"; CVD(bruto(1)); : PRINT USING "   Impuesto Bs. ########,.##"; CVD(ivafact(1)); : PRINT USING "   Neto Bs. ########,.##"; CVD(neto(1));
     COLOR colors(4, colorpref), colors(7, colorpref)
     FOR k = 2 TO 10
          IF k <= selecc% THEN
               LOCATE 5 + k, 2: PRINT USING "###############,³"; CVL(numfact(k)); : PRINT MID$(provee(k), 4, 15); "³ "; MID$(fechafact(k), 3, 2); "/"; MID$(fechafact(k), 1, 2); "/"; MID$(fechafact(k), 5, 4); "³ "; MID$(fechaacc(k), 1, 2); "/"; MID$( _
fechaacc(k), 3, 2); "/"; MID$(fechaacc(k), 5, 4); "³ "; transpor(k);
               IF MID$(edofact(k), 1, 1) = "*" THEN PRINT " ³Contado ";  ELSE PRINT " ³Credito ";
               'IF MID$(edofact(k), 2, 1) = "*" THEN PRINT "   SI   ";  ELSE PRINT "   NO   ";
          ELSE
               LOCATE 5 + k, 2: PRINT SPACE$(78)
          END IF
     NEXT k
     resal% = 1: ventana% = selecc%: up% = final% - 10: IF up% < 1 THEN up% = 1
     DO
          kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
          SELECT CASE kbd$
               CASE CHR$(0) + "H": GOSUB Arri
               CASE CHR$(0) + "P": GOSUB Abaj
               CASE CHR$(0) + CHR$(73): GOSUB Agup
               CASE CHR$(0) + CHR$(81): GOSUB Agdn
               CASE CHR$(0) + CHR$(67): sitio = 0: Reclamos (sitio): EXIT SUB
               CASE CHR$(13): eleg = resal%: EXIT DO
               CASE CHR$(27): resal% = 0: up% = 1: CALL Salida
               CASE ELSE:  BEEP
          END SELECT
     l = 0
     FOR k = 1 TO 10: l = l + 1   ': LOCATE 1, 1: PRINT "k="; k, "up%="; up%, "l="; l, "resal%="; resal%, "selecc%="; selecc%
     IF resal% = l THEN COLOR colors(7, colorpref), colors(4, colorpref)
          IF k <= selecc% THEN
               LOCATE 5 + l, 2: PRINT USING "###############,³"; CVL(numfact(k)); : PRINT MID$(provee(k), 4, 15); "³ "; MID$(fechafact(k), 3, 2); "/"; MID$(fechafact(k), 1, 2); "/"; MID$(fechafact(k), 5, 4); "³ "; MID$(fechaacc(k), 1, 2); "/"; MID$( _
fechaacc(k), 3, 2); "/"; MID$(fechaacc(k), 5, 4); "³ "; transpor(k);
               IF MID$(edofact(k), 1, 1) = "*" THEN PRINT " ³Contado ";  ELSE PRINT " ³Credito ";
               'IF MID$(edofact(k), 2, 1) = "*" THEN PRINT "   SI   ";  ELSE PRINT "   NO   ";
          ELSE
               LOCATE 5 + l, 2: PRINT SPACE$(78)
          END IF
     IF resal% = l THEN LOCATE 18, 4: PRINT USING "Bruto Bs. ########,.##"; CVD(bruto(k)); : PRINT USING "   Impuesto Bs. ########,.##"; CVD(ivafact(k)); : PRINT USING "   Neto Bs. ########,.##"; CVD(neto(k));
     IF resal% = l THEN COLOR colors(4, colorpref), colors(7, colorpref)
     NEXT k
     LOOP
Salir = false
'linea$ 26 credito o contado   59 actualizada si/no  62 cancelada si/no
num2 = STR$(CVL(numfact(eleg))): num2 = Trim$(MID$(num2, 2, 10))
FOR k = 1 TO 33: IF MID$(num2, k, 1) <> " " THEN larg% = larg% + 1
NEXT k: num2 = "F:\kevinprg\kevindat\" + STRING$(8 - larg%, "0") + MID$(num2, 1, larg%) + "." + MID$(provee(eleg), 1, 3)
OPEN num2 FOR RANDOM AS #2 LEN = 40: FIELD #2, 6 AS odigo$, 2 AS antid$, 8 AS recio$, 8 AS osto$, 8 AS ferta$, 2 AS oferantid$, 1 AS va$, 2 AS sitio$, 3 AS venci$: lleped% = LOF(2) / 40
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
seguir = true: apu% = 1: Box 4, 1, 6, 80
LOCATE 4, 2: PRINT "Factura:         Proveedor:       FechaFact:  FechaAcce:  Transcrita: F.Pago:"
LOCATE 5, 2: PRINT USING "#######,³"; CVL(numfact(eleg)); : PRINT MID$(provee(eleg), 4, 15); "³ "; MID$(fechafact(eleg), 3, 2); "/"; MID$(fechafact(eleg), 1, 2); "/"; MID$(fechafact(eleg), 5, 4); "³ "; MID$(fechaacc(eleg), 1, 2); "/"; MID$(fechaacc( _
eleg), 3, 2); "/"; MID$(fechaacc(eleg), 5, 4); "³ "; transpor(eleg);
IF MID$(edofact(eleg), 1, 1) = "*" THEN PRINT " ³Contado ";  ELSE PRINT " ³Credito ";
'IF MID$(edofact(eleg), 2, 1) = "*" THEN PRINT "   SI   ";  ELSE PRINT "   NO   ";
LOCATE 6, 4: PRINT USING "Bruto Bs. ########,.##"; CVD(bruto(eleg)); : PRINT USING "   Impuesto Bs. ########,.##"; CVD(ivafact(eleg)); : PRINT USING "   Neto Bs. ########,.##"; CVD(neto(eleg));
LOCATE 7, 1: PRINT "ÄDescripci¢nÄÄÄÄÄÄÄÄÄÄÄÄÄÂ% GanÂCantÄÂOferÄÂÄPVP ÄÄÄÄÄÂdesc.p£bl.ÂÄÄCostoÄÄÄÄÄÂÄ";
LOCATE 23, 1: PRINT "ÄDescripci¢nÄÄÄÄÄÄÄÄÄÄÄÄÄÁ%ÄGanÁCantÄÁOferÄÁÄPVP ÄÄÄÄÄÁdesc.p£bl.ÁÄÄCostoÄÄÄÄÄÁÄ";
Center 25, "[Enter]Seleccionar  [F9]SIN Seleccionar [F10]Salir  [Teclas de Direccion]"

DO WHILE seguir
     COLOR colors(7, colorpref), colors(4, colorpref): l = 7
     FOR k = apu% - 7 TO apu% + 7
          IF k = apu% THEN COLOR colors(4, colorpref), colors(7, colorpref)
          l = l + 1: LOCATE l, 1: PRINT SPACE$(80); : LOCATE l, 1: porc = MKD$(0)
          IF k > 0 AND k <= lleped% THEN
               GET #2, k: GET #1, CVI(sitio$): PRINT MID$(nomb2$, 1, 25); "³";
               IF CVD(osto$) > 0 THEN
                    IF va$ = "I" THEN
                              porc = MKD$(100 - (((CVD(osto$) / (CVI(antid$)))) / (CVD(recio$) / (VAL(Impuesto) / 100 + 1))) * 100)
                    ELSE
                              porc = MKD$(100 - (((CVD(osto$) / (CVI(antid$)))) / CVD(recio$)) * 100)
                    END IF
                    IF CVD(porc) < 29.95 THEN COLOR colors(4, colorpref) + 16, colors(7, colorpref)
                    PRINT USING "##.##"; CVD(porc);
               ELSE
                    COLOR colors(4, colorpref) + 16, colors(7, colorpref): PRINT USING "##.##"; 0;
               END IF
               IF k = apu% THEN COLOR colors(4, colorpref), colors(7, colorpref) ELSE COLOR colors(7, colorpref), colors(4, colorpref)
               PRINT USING "³#####³"; CVI(antid$); : PRINT USING "#####³"; CVI(oferantid$); : PRINT USING "######,.##³"; CVD(recio$); : PRINT USING "######,.##³"; CVD(ferta$); : PRINT USING "########,.##³"; CVD(osto$); : PRINT va$;
          ELSE
               PRINT SPACE$(25); "³     ³     ³     ³          ³          ³            ³ ";
          END IF
          IF k = apu% THEN COLOR colors(7, colorpref), colors(4, colorpref)
     NEXT k
          GET #2, apu%: GET #1, CVI(sitio$): LOCATE 24, 1: PRINT nomb2$; "  Fecha de Vencimiento:"; : auxiliar$ = ""
          IF venci$ <> "   " THEN
               IF MID$(venci$, 1, 1) = "A" OR MID$(venci$, 1, 1) = "B" OR MID$(venci$, 1, 1) = "C" THEN
                    IF MID$(venci$, 1, 1) = "A" THEN auxiliar$ = "10/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4)
                    IF MID$(venci$, 1, 1) = "B" THEN auxiliar$ = "11/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4)
                    IF MID$(venci$, 1, 1) = "C" THEN auxiliar$ = "12/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4)
               ELSE
                    auxiliar$ = "0" + MID$(venci$, 1, 1) + "/" + MID$(STR$(CVI(MID$(venci$, 2, 2))), 2, 4)
               END IF
               PRINT auxiliar$
          ELSE
               PRINT "  /    "
          END IF
          kbd$ = "": WHILE kbd$ = "": kbd$ = INKEY$: WEND
          SELECT CASE kbd$
               CASE CHR$(0) + "H": apu% = apu% - 1: IF apu% < 1 THEN apu% = 1
               CASE CHR$(0) + "P": apu% = apu% + 1: IF apu% > lleped% THEN apu% = lleped%
               CASE CHR$(0) + CHR$(81): apu% = apu% + 14: IF apu% > lleped% THEN apu% = lleped%
               CASE CHR$(0) + CHR$(73): apu% = apu% - 14: IF apu% < 1 THEN apu% = 1
               CASE CHR$(13): sitio = apu%: CLOSE : Reclamos (sitio): EXIT SUB
               CASE CHR$(0) + CHR$(67): sitio = 0: CLOSE : Reclamos (sitio): EXIT SUB
               CASE CHR$(0) + CHR$(68): EXIT SUB
               CASE CHR$(27): EXIT SUB
               CASE ELSE:  BEEP
          END SELECT
LOOP
CLOSE : EXIT SUB

Arri:
     lee% = 0: resal% = resal% - 1: IF resal% < 1 THEN resal% = 1: up% = up% - 1: lee% = 1
     IF up% < 1 THEN up% = 1: IF Sonido = "si" THEN BEEP
     IF lee% = 1 THEN GOSUB LeeDiez: GOSUB BuscActivo
RETURN
Abaj:
     lee% = 0: resal% = resal% + 1: IF resal% > selecc% THEN up% = up% + 1: lee% = 2
     IF up% > final% THEN up% = final%: IF Sonido = "si" THEN BEEP
     IF lee% = 2 THEN GOSUB LeeDiez: GOSUB BuscActivo
RETURN
Agup:
     up% = up% - 9: lee% = 1
     IF up% < 1 THEN up% = 1: resal% = 1: IF Sonido = "si" THEN BEEP
     GOSUB LeeDiez: GOSUB BuscActivo
RETURN
Agdn:
     up% = up% + 9: lee% = 1
     IF up% > final% THEN up% = final%: resal% = 1: IF Sonido = "si" THEN BEEP
     GOSUB LeeDiez: GOSUB BuscActivo
RETURN
ImprimeFact:
RETURN
LeeDiez:
selecc% = 0
FOR k = up% TO final%
IF k > 0 AND k <= final% THEN
     GET #27, k
     IF MID$(linea$, 1, 1) = " " THEN
          selecc% = selecc% + 1: numfact(selecc%) = MID$(linea$, 2, 4): provee(selecc%) = MID$(linea$, 24, 2): edofact(selecc%) = MID$(linea$, 26, 1) + MID$(linea$, 59, 1) + MID$(linea$, 62, 1)
          fechaacc(selecc%) = MID$(linea$, 6, 8): transpor(selecc%) = MID$(linea$, 14, 10): fechafact(selecc%) = MID$(linea$, 51, 8): ubifact(selecc%) = MKI$(k)
          bruto(selecc%) = MID$(linea$, 27, 8): neto(selecc%) = MID$(linea$, 35, 8): ivafact(selecc%) = MID$(linea$, 43, 8): IF selecc% = 10 THEN EXIT FOR
     END IF
END IF
NEXT k
FOR k = 1 TO selecc%: FOR l = 1 TO 199
IF CVI(MID$(provee(k), 1, 2)) = VAL(Code(l)) THEN provee(k) = Code(l) + Contenido(l): l = 199
NEXT l: NEXT k: IF lee% = 2 THEN resal% = selecc%
RETURN
BuscActivo:
encontrado = false: GET #27, up%: IF MID$(linea$, 1, 1) = " " THEN encontrado = true
DO WHILE encontrado = false
     IF lee% = 1 THEN
          up% = up% - 1
          IF up% < 1 THEN
               FOR k = 1 TO final%
                    GET #27, k: IF MID$(linea$, 1, 1) = " " THEN up% = k: EXIT DO
               NEXT k
          END IF
     END IF
     IF lee% = 2 THEN
          up% = up% + 1
          IF up% > final% THEN
               FOR k = final% TO 1 STEP -1
                    GET #27, k: IF MID$(linea$, 1, 1) = " " THEN up% = k: EXIT DO
               NEXT k
          END IF
     END IF
     GET #27, up%: IF MID$(linea$, 1, 1) = " " THEN encontrado = true
LOOP
RETURN
END SUB

