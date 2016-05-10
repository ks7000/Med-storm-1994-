'hace un archivo .dat a partir de univeral.dat
OPEN "f:\kevinprg\kevindat\univeral.dat" FOR RANDOM SHARED AS #1 LEN = 18
FIELD #1, 2 AS num$, 16 AS descrip$
OPEN "F:\kevinprg\kevindat\causas.dat" FOR RANDOM AS #2 LEN = 33
FIELD #2, 2 AS codigo$, 30 AS descripcion$, 1 AS edo$
FOR k = 1 TO 99: GET #1, k: RSET codigo$ = num$: RSET descripcion$ = descrip$ + SPACE$(14): PUT #2, k: NEXT k

