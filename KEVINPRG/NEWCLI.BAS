OPEN "c:\kevinprg\kevindat\clientes.dat" FOR RANDOM AS #1 LEN = 37
FIELD #1, 2 AS num$, 35 AS nombre$
RSET num$ = "01": RSET nombre$ = "þ" + SPACE$(34): PUT #1, 1
RSET num$ = "02": RSET nombre$ = "þ" + SPACE$(34): PUT #1, 2
RSET num$ = "03": RSET nombre$ = "þ" + SPACE$(34): PUT #1, 3
RSET num$ = "04": RSET nombre$ = "þ" + SPACE$(34): PUT #1, 4
RSET num$ = "05": RSET nombre$ = "þ" + SPACE$(34): PUT #1, 5
RSET num$ = "06": RSET nombre$ = "þ" + SPACE$(34): PUT #1, 6
RSET num$ = "07": RSET nombre$ = "þ" + SPACE$(34): PUT #1, 7
RSET num$ = "08": RSET nombre$ = "þ" + SPACE$(34): PUT #1, 8
RSET num$ = "09": RSET nombre$ = "þ" + SPACE$(34): PUT #1, 9
FOR k = 10 TO 99
RSET num$ = MID$(STR$(k), 2, 2): RSET nombre$ = "þ" + SPACE$(34): PUT #1, k
NEXT k
END

