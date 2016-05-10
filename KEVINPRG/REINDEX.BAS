CLOSE : OPEN "f:\kevinprg\kevindat\nombre.idx" FOR RANDOM AS #1 LEN = 37
FIELD #1, 35 AS codigo$, 2 AS sitio$: max2% = LOF(1) / 37
sitio% = 0: largo% = 0: DIM vial AS STRING * 1: vial = CHR$(1)
OPEN "f:\kevinprg\kevindat\reindex.dat" FOR RANDOM SHARED AS #2 LEN = 2
FIELD #2, 2 AS nume$: RSET nume$ = MKI$(0): FOR k = 1 TO 254: PUT #2, k: NEXT k
FOR k = 1 TO max2%
     GET #1, k
     IF MID$(codigo$, 1, 1) > vial THEN
          vial = MID$(codigo$, 1, 1): RSET nume$ = MKI$(k): PUT #2, ASC(vial): PRINT vial
     END IF
NEXT k
END
    
     'CASE 1 'por nombre
     '     IF busqueda <> 1 THEN
     '          OPEN "f:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
     '          FIELD #2, 35 AS nomshort$, 2 AS num$: max2% = LOF(2) / 37: busqueda = 1
     '          ERASE Xa, Xb: REDIM Xa(max2%) AS STRING * 6, Xb(max2%) AS STRING * 2
     '          FOR k = 1 TO max2%: GET #2, k: Xa(k) = MID$(nomshort$, 1, 6): Xb(k) = num$: NEXT k
     '     END IF

