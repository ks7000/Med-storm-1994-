'convierte codigos de 7 letras a 6 letras no repetidos
OPEN "c:\kevinprg\kevindat\newcod.dbf" FOR RANDOM AS #1 LEN = 8
FIELD #1, 1 AS activo$, 7 AS codigo$
max1% = LOF(1) / 8
OPEN "c:\kevinprg\kevindat\new.dbf" FOR RANDOM AS #2 LEN = 200
FIELD #2, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
FOR k = 1 TO max1%: LOCATE 1, 1: PRINT k
GET #1, k: GET #2, k
LSET codigo2$ = MID$(codigo$, 1, 6)
PUT #2, k
NEXT k
CLOSE

