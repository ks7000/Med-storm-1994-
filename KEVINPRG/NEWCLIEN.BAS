CLOSE
OPEN "F:\kevinprg\kevindat\clientes.dat" FOR RANDOM AS #5 LEN = 37
FIELD #5, 2 AS codigo1$, 35 AS nomb1$
OPEN "F:\kevinprg\kevindat\clientes.jim" FOR RANDOM AS #6 LEN = 38
FIELD #6, 3 AS codigo2$, 35 AS nomb2$
FOR k = 1 TO 99
     GET #5, k
     LSET codigo2$ = "0" + codigo1$: LSET nomb2$ = nomb1$
     PUT #6, k
NEXT k
FOR k = 100 TO 199
     LSET codigo2$ = MID$(STR$(k), 2, 3): LSET nomb2$ = "þ" + SPACE$(34)
     PUT #6, k
NEXT k
CLOSE
KILL "F:\kevinprg\kevindat\clientes.dat"
NAME "F:\kevinprg\kevindat\clientes.jim" AS "F:\kevinprg\kevindat\clientes.dat"
CLOSE
SYSTEM

