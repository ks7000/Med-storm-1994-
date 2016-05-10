CLS
CLOSE
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 200 AS todo1$
OPEN "F:\kevinprg\kevindat\new.jim" FOR RANDOM SHARED AS #2 LEN = 200
FIELD #2, 200 AS todo2$
FOR k% = 1 TO 5803
     GET #1, k%: RSET todo2$ = todo1$: PUT #2, k%: LOCATE 1, 1: PRINT k%
NEXT k%
CLOSE
KILL "F:\kevinprg\kevindat\new.dbf"
NAME "F:\kevinprg\kevindat\new.jim" AS "F:\kevinprg\kevindat\new.dbf"
CLOSE
SYSTEM

