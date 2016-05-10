'hace registro saldos.psi
CLOSE : CLS : KILL "f:\kevinprg\kevindat\saldos.dat"

OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv$, 2 AS prom2$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$

OPEN "F:\kevinprg\kevindat\nombre.idx" FOR RANDOM SHARED AS #2 LEN = 37
FIELD #2, 35 AS nomshort$, 2 AS num$: max2% = LOF(2) / 37

OPEN "f:\kevinprg\kevindat\saldos.dat" FOR RANDOM SHARED AS #3 LEN = 45
FIELD #3, 6 AS codipsi$, 35 AS nombpsi$, 2 AS saldopsi$, 2 AS sitiopsi$: maxpsi% = 1

FOR k = 1 TO max2%
     GET #2, k: GET #1, CVI(num$)
     IF CVI(ubic2$) = 1 THEN
        RSET codipsi$ = codigo2$
        RSET nombpsi$ = nomb2$
        RSET sitiopsi$ = num$
        RSET saldopsi$ = MKI$(0)
        PUT #3, maxpsi%: maxpsi% = maxpsi% + 1
        LOCATE 1, 1: PRINT k: PRINT nomb2$; CVI(ubic2$): SLEEP 1
     END IF
NEXT k

CLOSE

