DIM cta AS STRING * 59
OPEN "f:\KEVINPRG\TARJETAS\CUENTA.dbf" FOR RANDOM AS #1 LEN = 59
max% = LOF(1) / 59: k% = 0: l% = 2
OPEN "f:\KEVINPRG\TARJETAS\CUENTA2.dbf" FOR RANDOM AS #2 LEN = 59
GET #1, 1, cta1: PUT #2, 1, cta
FOR k% = 16692 TO max%
  GET #1, k%, cta: PUT #2, l%, cta: l% = l% + 1
NEXT k%
CLOSE

