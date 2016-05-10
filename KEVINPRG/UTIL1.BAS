DECLARE SUB BackFill (Row%, col%, Rows%, Cols%, Attribute%)
CLS
FOR k% = 1 TO 25
   LOCATE k%, 1: PRINT STRING$(70, "a")
NEXT k%: l% = 1
SLEEP
FOR k% = 1 TO 5000
   BackFill l%, 1, 1, 24, k%
   LOCATE l%, 25: PRINT k%; : l% = l% + 1
   IF l% = 26 THEN l% = 1: SLEEP 1
NEXT k%

