'restaura registros de otra base de datos
CLOSE
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 200 AS all$
OPEN "c:\tmp\new.dbf" FOR RANDOM SHARED AS #2 LEN = 200
FIELD #2, 200 AS todo$
CLS
FOR k = 974 TO 995
     GET #2, k: RSET all$ = todo$: PUT #1, k
NEXT k
     GET #2, 1012: RSET all$ = todo$: PUT #1, 1012
CLOSE

