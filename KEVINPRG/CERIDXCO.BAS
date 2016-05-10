'visualiza un archivo .idx
DEFINT A-Z
'indexado de codigo
OPEN "f:\kevinprg\kevindat\codigo.idx" FOR RANDOM SHARED AS #2 LEN = 8
FIELD #2, 6 AS codigo$, 2 AS sitio$: lleva% = LOF(2) / 8
FOR k = 1 TO lleva%
GET #2, k: PRINT "³"; codigo$; "³"
NEXT k: CLOSE : END

