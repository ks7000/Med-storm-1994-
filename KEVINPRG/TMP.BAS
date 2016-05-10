OPEN "f:\kevinprg\kevindat\2230443.caj" FOR RANDOM SHARED AS #17 LEN = 8
FIELD #17, 8 AS caja$
GET #17, 1
RSET caja$ = MKD$(CVD(caja$) + 99000)
PUT #17, 1
CLOSE #17
         
OPEN "f:\kevinprg\kevindat\13091999.crd" FOR RANDOM AS #17 LEN = 30
FIELD #17, 4 AS todo1$, 8 AS total$, 18 AS todo2$
GET #17, 1: RSET total$ = MKD$(11000): PUT #17, 1: CLOSE
END

