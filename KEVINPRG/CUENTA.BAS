'determina la cadena mas larga de un archivo .idx
CLS : CLOSE : OPEN "f:\kevinprg\kevindat\nombre.idx" FOR RANDOM AS #1 LEN = 37
FIELD #1, 35 AS codigo$, 2 AS sitio$: max2% = LOF(1) / 37: maximo% = 0: van% = 0
DIM vial AS STRING * 3, vial2 AS STRING * 3: GET #1, 1: vial = MID$(codigo$, 1, 3): vial2 = vial
FOR k = 1 TO max2%
     GET #1, k
     IF MID$(codigo$, 1, 3) = vial THEN
          van% = van% + 1
     ELSE
          LOCATE 10, 10: PRINT "Sumando:"; vial; van%: SLEEP
          vial = MID$(codigo$, 1, 3): IF van% > maximo% THEN maximo% = van%: vial2 = vial
          van% = 0
     END IF
NEXT k
PRINT "Cadena:"; vial2; "   Valor Maximo"; maximo%
END
    
     'CASE 1 'por nombre
     '     IF busqueda <> 1 THEN
     '          OPEN "f:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
     '          FIELD #2, 35 AS nomshort$, 2 AS num$: max2% = LOF(2) / 37: busqueda = 1
     '          ERASE Xa, Xb: REDIM Xa(max2%) AS STRING * 6, Xb(max2%) AS STRING * 2
     '          FOR k = 1 TO max2%: GET #2, k: Xa(k) = MID$(nomshort$, 1, 6): Xb(k) = num$: NEXT k
     '     END IF

