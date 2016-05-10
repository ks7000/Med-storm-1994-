DEFINT A-Z
'indexado de composicion
KILL "f:\kevinprg\kevindat\compos.idx"
OPEN "f:\kevinprg\kevindat\new.dbf" FOR RANDOM AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 18 AS reserva$
max1% = LOF(1) / 200
OPEN "f:\kevinprg\kevindat\compos.idx" FOR RANDOM AS #2 LEN = 14
FIELD #2, 12 AS codigo$, 2 AS sitio$: lleva = 0
FOR k = 1 TO max1%: GET #1, k
IF acti2$ = " " THEN
     IF (CVI(compo12$) + CVI(compo22$) + CVI(compo32$) + CVI(compo42$) + CVI(compo52$) + CVI(compo62$)) > 6 THEN
          lleva = lleva + 1: RSET sitio$ = MKI$(k)
          RSET codigo$ = (compo12$ + compo22$ + compo32$ + compo42$ + compo52$ + compo62$)
          PUT #2, lleva: LOCATE 1, 1: PRINT k
     END IF
END IF
NEXT k: CLOSE

