DEFINT A-Z
TYPE idxs
   come AS STRING * 2
   nomb AS STRING * 6
   ubic AS STRING * 2
END TYPE
DIM idx AS idxs
OPEN "F:\kevinprg\kevindat\ubica.idx" FOR RANDOM SHARED AS #1 LEN = 10
max% = LOF(1) / 10: CLS
CLS : previo% = 0
GET #1, 1, idx: previo% = CVI(idx.come)
FOR k = 1 TO max%
   GET #1, k, idx
   PRINT k, CVI(idx.come), idx.nomb
   IF previo% <> CVI(idx.come) THEN SLEEP: previo% = CVI(idx.come)
   'l = l + 1: IF l > 22 THEN SLEEP 1: CLS : l = 0
NEXT k

