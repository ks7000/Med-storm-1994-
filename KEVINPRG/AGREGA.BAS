DEFINT A-Z
OPEN "F:\kevinprg\kevindat\componen.dat" FOR RANDOM AS #2 LEN = 18
FIELD #2, 3 AS codigo$, 15 AS descripcion$
FOR k = 200 TO 999
     RSET codigo$ = MID$(STR$(k), 2, 3): RSET descripcion$ = "þ" + SPACE$(14)
     PUT #2, k
NEXT k
CLOSE
OPEN "F:\kevinprg\kevindat\lab.dat" FOR RANDOM AS #2 LEN = 18
FIELD #2, 3 AS codigo$, 15 AS descripcion$
FOR k = 200 TO 999
     RSET codigo$ = MID$(STR$(k), 2, 3): RSET descripcion$ = "þ" + SPACE$(14)
     PUT #2, k
NEXT k
CLOSE
OPEN "F:\kevinprg\kevindat\clientes.dat" FOR RANDOM AS #2 LEN = 38
FIELD #2, 3 AS codigo$, 35 AS descripcion$
FOR k = 200 TO 999
     RSET codigo$ = MID$(STR$(k), 2, 3): RSET descripcion$ = "þ" + SPACE$(34)
     PUT #2, k
NEXT k

OPEN "f:\kevinprg\kevindat\fichacli.ent" FOR RANDOM SHARED AS #5 LEN = 326
FIELD #5, 20 AS rif5$, 20 AS nit5$, 2 AS diacred5$, 4 AS monmax5$, 60 AS direc15$, 60 AS direc25$, 20 AS tele5$, 1 AS edo15$, 2 AS dia15$, 1 AS edo25$, 2 AS dia25$, 1 AS edo35$, 2 AS dia35$, 1 AS edo45$, 2 AS dia45$, 30 AS autor15$, 30 AS autor25$, 30 AS autor35$, 30 AS autor45$, 8 AS desde$
FOR k = 200 TO 999
     LSET rif5$ = SPACE$(20)
     LSET nit5$ = SPACE$(20)
     LSET diacred5$ = MKI$(0)
     LSET monmax5$ = MKL$(0)
     LSET direc15$ = SPACE$(60)
     LSET direc25$ = SPACE$(60)
     LSET tele5$ = SPACE$(20)
     LSET edo15$ = " "
     LSET dia15$ = MKI$(0)
     LSET edo25$ = " "
     LSET dia25$ = MKI$(0)
     LSET edo35$ = " "
     LSET dia35$ = MKI$(0)
     LSET edo45$ = " "
     LSET dia45$ = MKI$(0)
     LSET autor15$ = SPACE$(30)
     LSET autor25$ = SPACE$(30)
     LSET autor35$ = SPACE$(30)
     LSET autor45$ = SPACE$(30)
     LSET desde5$ = SPACE$(8)
     PUT #5, k
NEXT k
CLOSE
END

