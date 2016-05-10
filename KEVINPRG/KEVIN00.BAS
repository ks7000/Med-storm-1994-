DEFINT A-Z
DECLARE SUB Intro ()
DECLARE SUB Initialize ()
DECLARE SUB LeeEstado ()
DECLARE SUB Box (Row1, Col1, Row2, Col2)
DECLARE SUB ArreglaTurno ()
DECLARE SUB SalvaEstado ()
DECLARE SUB Center (row, text$)
DECLARE SUB Espera ()
COMMON SHARED guia AS INTEGER, colorpref AS INTEGER
DIM SHARED turna, dia1, mes1, ano1, dia2, mes2, ano2
DIM SHARED turno  AS STRING * 1, fx AS STRING * 33
DIM SHARED colors(0 TO 20, 1 TO 4)
DIM SHARED devolver
CONST true = -1: CONST false = NOT true: devolver = false
ON ERROR GOTO ErrorTrap
CALL LeeEstado
RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct: COLOR colors(7, colorpref), colors(4, colorpref)
CALL Initialize: CALL Center(24, "Leyendo Varios Registros Necesarios ...")
CLOSE : CHAIN "c:\kevinprg\kevin01.bas"
END

ErrorTrap:
SELECT CASE ERR
     CASE 70
          Center 25, "Esperando Acceso a La Red , Espere Por Favor . . .": SLEEP 1: BEEP: RESUME
     CASE 53
          Box 10, 10, 20, 70: Center 15, "Este Diskette no contiene Licencia de Usuario": Espera: SYSTEM
     CASE 71
          Box 10, 10, 20, 70: Center 15, "Introduzca el diskette de Licencia en el drive A": Espera
          Center 15, " Abriendo acceso al Diskette , Espere ...       ": RESUME
     CASE 53
          Box 10, 10, 20, 70: Center 15, "No se encuentra acceso al Diskette:Por Favor llame al 0144062869": Espera: SYSTEM
     CASE ELSE
          LOCATE 1, 1: PRINT ERR
END SELECT
RESUME NEXT
'    scrn  dots  bar  back   title  shdow  choice  curs   cursbk  shdow
DATA 0,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  3,     0,     1,     15,     0,     7,      0
DATA 3,    15,   13,  1,     14,    3,     15,     0,     7,      0
DATA 7,    12,   15,  4,     14,    0,     15,     15,    1,      0

SUB ArreglaTurno
dia2 = VAL(MID$(DATE$, 4, 2)): mes2 = VAL(MID$(DATE$, 1, 2)): ano2 = VAL(MID$(DATE$, 7, 4))
dia1 = 3: mes1 = 1: ano1 = 1998
'PRINT USING " [####] "; dia1; mes1; ano1; dia2; mes2; ano2: Espera
SELECT CASE turna
     CASE 2: dia1 = 4
     CASE 3: dia1 = 5
     CASE 4: dia1 = 6
END SELECT: seguir$ = "true"
DO WHILE seguir$ = "true": dia1 = dia1 + 4
     IF dia1 >= 29 AND mes1 = 2 THEN dia1 = dia1 - 28: mes1 = 3
     IF dia1 >= 31 AND mes1 = 4 THEN dia1 = dia1 - 30: mes1 = mes1 + 1
     IF dia1 >= 31 AND mes1 = 6 THEN dia1 = dia1 - 30: mes1 = mes1 + 1
     IF dia1 >= 31 AND mes1 = 9 THEN dia1 = dia1 - 30: mes1 = mes1 + 1
     IF dia1 >= 31 AND mes1 = 11 THEN dia1 = dia1 - 30: mes1 = mes1 + 1
     IF dia1 >= 32 AND mes1 = 1 THEN dia1 = dia1 - 31: mes1 = mes1 + 1
     IF dia1 >= 32 AND mes1 = 3 THEN dia1 = dia1 - 31: mes1 = mes1 + 1
     IF dia1 >= 32 AND mes1 = 5 THEN dia1 = dia1 - 31: mes1 = mes1 + 1
     IF dia1 >= 32 AND mes1 = 7 THEN dia1 = dia1 - 31: mes1 = mes1 + 1
     IF dia1 >= 32 AND mes1 = 8 THEN dia1 = dia1 - 31: mes1 = mes1 + 1
     IF dia1 >= 32 AND mes1 = 10 THEN dia1 = dia1 - 31: mes1 = mes1 + 1
     IF dia1 >= 32 AND mes1 = 12 THEN dia1 = dia1 - 31: mes1 = 1: ano1 = ano1 + 1
     IF dia1 >= dia2 AND mes1 = mes2 AND ano1 = ano2 THEN seguir$ = "false"
     IF mes1 > mes2 AND ano1 = ano2 THEN seguir$ = "false"
     IF ano1 > ano2 THEN seguir$ = "false"
LOOP: SalvaEstado
END SUB

SUB Box (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿"; : FOR a = Row1 + 1 TO Row2 - 1: LOCATE a, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Center (row, text$)
IF row = 25 THEN COLOR colors(5, colorpref), colors(4, colorpref): LOCATE 25, 1: PRINT SPACE$(80);
LOCATE row, 41 - LEN(text$) / 2: PRINT text$;
END SUB

SUB Espera
Center 25, "Pulse Cualquier Tecla Para Continuar": SLEEP 5
END SUB

SUB Initialize
CALL LeeEstado: dia2 = VAL(MID$(DATE$, 4, 2)): mes2 = VAL(MID$(DATE$, 1, 2)): ano2 = VAL(MID$(DATE$, 7, 4))
CALL Intro
fx = "": fx = "F:\kevinprg\kevinvnt\" + MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4) + ".vnt"
OPEN fx FOR RANDOM SHARED AS #17 LEN = 30: FIELD #17, 30 AS todo$: RSET todo$ = SPACE$(30): PUT #17, 1
CLOSE : EXIT SUB
'no funciona de aqui en adelante
IF dia1 = dia2 AND mes1 = mes2 AND ano1 = ano2 THEN
     LSET todo$ = "TURNO hoy hubo turno": PUT #17, 1: CLOSE
     dia2 = dia2 + 1
     IF dia2 = 29 AND mes2 = 2 THEN dia2 = 1: mes2 = 3
     IF dia2 = 30 AND mes2 = 2 THEN dia2 = 1: mes2 = 3
     IF dia2 = 31 AND mes2 = 4 THEN dia2 = 1: mes2 = 5
     IF dia2 = 31 AND mes2 = 6 THEN dia2 = 1: mes2 = 7
     IF dia2 = 31 AND mes2 = 9 THEN dia2 = 1: mes2 = 10
     IF dia2 = 31 AND mes2 = 11 THEN dia2 = 1: mes2 = 12
     IF dia2 = 32 AND mes2 = 1 THEN dia2 = 1: mes2 = 2
     IF dia2 = 32 AND mes2 = 3 THEN dia2 = 1: mes2 = 4
     IF dia2 = 32 AND mes2 = 5 THEN dia2 = 1: mes2 = 6
     IF dia2 = 32 AND mes2 = 7 THEN dia2 = 1: mes2 = 8
     IF dia2 = 32 AND mes2 = 8 THEN dia2 = 1: mes2 = 9
     IF dia2 = 32 AND mes2 = 10 THEN dia2 = 1: mes2 = 11
     IF dia2 = 32 AND mes2 = 12 THEN dia2 = 1: mes2 = 1: ano2 = ano2 + 1
     fa$ = "": fa$ = "F:\kevinprg\kevindat\": gh$ = STR$(dia2)
     IF dia2 > 9 THEN gh$ = MID$(gh$, 2, 2) ELSE gh$ = "0" + MID$(gh$, 2, 1)
     fa$ = fa$ + gh$: gh$ = STR$(mes2): IF mes2 > 9 THEN gh$ = MID$(gh$, 2, 2) ELSE gh$ = "0" + MID$(gh$, 2, 1)
     fa$ = fa$ + gh$ + MID$(STR$(ano2), 2, 4) + ".vnt"
     OPEN fa$ FOR RANDOM AS #17 LEN = 30: FIELD #17, 30 AS todo$: LSET todo$ = "No hubo turno": PUT #17, 1: CLOSE
ELSE
     LSET todo$ = "No hubo turno": PUT #17, 1: CLOSE
END IF
dia2 = VAL(MID$(DATE$, 4, 2)): mes2 = VAL(MID$(DATE$, 1, 2)): ano2 = VAL(MID$(DATE$, 7, 4))
DO WHILE NOT devolver
IF dia1 = dia2 AND mes1 = mes2 AND ano1 = ano2 THEN
     totak$ = STR$(dia1) + "/" + STR$(mes1) + "/" + STR$(ano1): COLOR colors(1, colorpref) + 16, colors(8, colorpref): Box 18, 10, 21, 70
     COLOR colors(7, colorpref), colors(4, colorpref): Center 19, "Hoy es un d¡a de Turno Farmace£tico": Center 20, totak$: devolver = true: SLEEP 5
END IF
IF dia2 = (dia1 - 1) AND mes2 = mes1 THEN totak$ = STR$(dia1) + "/" + STR$(mes1) + "/" + STR$(ano1): COLOR colors(1, colorpref) + 16, colors(8, colorpref): Box 18, 10, 21, 70: COLOR colors(7, colorpref), colors(4, colorpref): Center 19,  _
"Pr¢ximo D¡a de Turno Farmace£tico:": Center 20, totak$: devolver = true: CALL Espera
IF dia2 = 28 OR dia2 = 30 OR dia2 = 31 AND dia1 = 1 THEN totak$ = STR$(dia1) + "/" + STR$(mes1) + "/" + STR$(ano1): COLOR colors(1, colorpref) + 16, colors(8, colorpref): Box 18, 10, 21, 70: COLOR colors(7, colorpref), colors(4, colorpref): Center  _
19, "Pr¢ximo D¡a de Turno Farmace£tico:": Center 20, totak$: devolver = true: CALL Espera
IF dia2 > dia1 AND mes2 = mes1 THEN turna = VAL(turno): CALL ArreglaTurno
IF mes2 > mes1 THEN turna = VAL(turno): CALL ArreglaTurno
IF ano2 > ano1 THEN turna = VAL(turno): CALL ArreglaTurno
IF dia2 < dia1 AND mes2 = mes1 THEN devolver = true
IF mes2 < mes1 THEN devolver = true
IF ano2 < ano2 THEN devolver = true
LOOP
END SUB

SUB Intro
COLOR colors(7, colorpref), colors(4, colorpref): CLS
Center 1, "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»": Center 2, "º°°²²°°²²°°²²²²²²°°²²°°°°²²°°²²°°²²°°°²²°°º"
Center 3, "º°°²²°²²°°°²²°°°°°°²²°°°°²²°°²²°°²²²°°²²°°º": Center 4, "º°°²²²²°°°°²²²²°°°°°²²°°²²°°°²²°°²²°²°²²°°º"
Center 5, "º°°²²°²²°°°²²°°°°°°°°²²²²°°°°²²°°²²°°²²²°°º": Center 6, "º°°²²°°²²°°²²²²²²°°°°°²²°°°°°²²°°²²°°°²²°°º"
Center 7, "              ÚÄ½°°°°°°°°°°°°°Software7000°°°°°°°°°°°°°°°°ÓÄ¿             ": Center 8, "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°°°°°°°°°°°°°°  Presenta: °°°°°°°°°°°°°°°°°°ÀÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center 9, "³°°²²°°°°²²°°²²²²²²°°²²²²²°°°°²²²²²²°°²²²²²²°°°²²²²°°°²²²²²²°°°²²°°°°²²°°³": Center 10, "³°°²²²°°²²²°°²²°°°°°°²²°°²²°°°²²°°°°°°°°²²°°°°²²°°²²°°²²°°°²²°°²²²°°²²²°°³"
Center 11, "³°°²²°²²°²²°°²²²²°°°°²²°°°²²°°²²²²²²°°°°²²°°°°²²°°²²°°²²²²²²°°°²²°²²°²²°°³": Center 12, "³°°²²°°°°²²°°²²°°°°°°²²°°²²°°°°°°°²²°°°°²²°°°°²²°°²²°°²²°²²°°°°²²°°°°²²°°³"
Center 13, "³°°²²°°°°²²°°²²²²²²°°²²²²²°°°°²²²²²²°°°°²²°°°°°²²²²°°°²²°°²²°°°²²°°°°²²°°³": Center 14, "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Version 1.0 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
COLOR 7: Center 15, "Un Empleado Virtual Para Farmacia Realmente Eficiente ": Center 16, " Escrito Por : Jimmy Javier Olano Sayago "
END SUB

SUB LeeEstado
OPEN "c:\kevinprg\turno.dat" FOR INPUT AS #7
INPUT #7, colorpref
INPUT #7, turno
INPUT #7, dia1
INPUT #7, mes1
INPUT #7, ano1
CLOSE
END SUB

SUB SalvaEstado
OPEN "a:\turno.dat" FOR OUTPUT AS #7
PRINT #7, USING "#"; colorpref
PRINT #7, turno
PRINT #7, USING "##"; dia1
PRINT #7, USING "##"; mes1
PRINT #7, USING "####"; ano1
CLOSE #7
END SUB

