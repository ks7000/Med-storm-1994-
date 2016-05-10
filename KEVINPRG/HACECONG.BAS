'borra facturas congeladas
CLOSE : OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM AS #1 LEN = 100
FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
nursaler = LOF(1) / 100
FOR k = 1 TO nursaler: GET #1, k: RSET cong$ = " ": PUT #1, k: NEXT k
CLOSE : END

