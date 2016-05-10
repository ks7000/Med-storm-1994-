DEFINT A-Z
DECLARE SUB Intro ()
DECLARE SUB LeeEstado ()
DECLARE SUB Box (Row1, Col1, Row2, Col2)
DECLARE SUB Center (row, text$)
DECLARE SUB Espera ()
COMMON SHARED guia AS INTEGER, colorpref AS INTEGER
DIM SHARED turno  AS STRING * 1, fx AS STRING * 33
DIM SHARED colors(0 TO 20, 1 TO 4)
DIM SHARED devolver
CONST true = -1: CONST false = NOT true: devolver = false: colorpref = 1
ON ERROR GOTO ErrorTrap
CALL LeeEstado
CLS : RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
COLOR colors(7, colorpref), colors(4, colorpref)
CALL Intro
CALL Center(24, "Leyendo Varios Registros Necesarios ...")
CLOSE : CHAIN "F:\kevinprg\kevin01.bas"
END

ErrorTrap:
SELECT CASE ERR
     CASE 70
          Center 25, "Esperando Acceso a La Red , Espere Por Favor . . .": SLEEP 1: BEEP: RESUME
     CASE 53
          Box 10, 10, 20, 70: Center 15, "Este Diskette no contiene Licencia de Usuario": Espera: SYSTEM
     CASE 71, 76
          Box 10, 10, 20, 70: Center 15, "Introduzca el diskette de Licencia en el drive A": Espera
          Center 15, " Abriendo acceso al Diskette , Espere ...       ": RESUME
     CASE ELSE
          LOCATE 1, 1: PRINT ERR
END SELECT
RESUME
'    scrn  dots  bar  back   title  shdow  choice  curs   cursbk  shdow
DATA 0,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  3,     0,     1,     15,     0,     7,      0
DATA 3,    15,   13,  1,     14,    3,     15,     0,     7,      0
DATA 7,    12,   15,  4,     14,    0,     15,     15,    1,      0

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
'OPEN "c:\turno.dat" FOR INPUT AS #7
'INPUT #7, colorpref
'INPUT #7, turno
'INPUT #7, dia1
'INPUT #7, mes1
'INPUT #7, ano1
'CLOSE
colorpref = 3
END SUB

