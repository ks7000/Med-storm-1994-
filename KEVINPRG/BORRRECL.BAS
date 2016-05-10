OPEN "F:\kevinprg\kevindat\reclamos.dat" FOR RANDOM AS #5 LEN = 150
FIELD #5, 1 AS acti$, 4 AS numefact$, 4 AS numped$, 8 AS fecfact$, 4 AS horafact$, 10 AS codefarm$, 10 AS codedrog$, 2 AS numcausa$, 2 AS canti$, 8 AS precneto$, 8 AS icsvm$, 30 AS observa1$, 30 AS observa2$, 2 AS numprov$, 6 AS codenew$, 2 AS  _
numreg$, 8 AS pvpfact$, 1 AS auxprint$, 10 AS cedu2$
GET #5, 474: RSET acti$ = "*": PUT #5, 474: CLOSE

