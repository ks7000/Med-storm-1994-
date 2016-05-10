'restaura registro a partir de otra base de datos
DEFINT A-Z
CLOSE
OPEN "c:\tmp\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 200 AS linea1$
OPEN "f:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #2 LEN = 200
FIELD #2, 200 AS linea2$
GET #1, 2215: RSET linea2$ = linea1$: PUT #2, 2215
CLOSE

