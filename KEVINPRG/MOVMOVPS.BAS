'#3 mueve a movpsico.dat el renglon de compras
OPEN "f:\kevinprg\kevindat\movpsico.dat" FOR RANDOM SHARED AS #3 LEN = 40
FIELD #3, 40 AS todo$: CLS
maxpsi% = LOF(3) / 40
FOR k = 1 TO maxpsi%
     GET #3, k: LOCATE 1, 1: PRINT todo$
     IF MID$(todo$, 1, 10) = "    COMPRA" THEN LSET todo$ = MID$(todo$, 5, 36): PUT #3, k: LOCATE 3, 1: PRINT todo$: SLEEP
NEXT k

