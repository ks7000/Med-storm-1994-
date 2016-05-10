TYPE idxs
   nomb AS STRING * 35
   ubic AS STRING * 2
END TYPE
DIM idx AS idxs
OPEN "F:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #1 LEN = 37
max% = LOF(1) / 37
CLS
FOR k = 6000 TO max%
   GET #1, k, idx
   IF INSTR(1, idx.nomb, "VUSCO") > 0 THEN
      PRINT k, idx.nomb, CVI(idx.ubic)
   END IF
NEXT k

