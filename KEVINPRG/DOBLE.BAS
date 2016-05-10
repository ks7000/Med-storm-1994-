KILL "a:\integer.txt"
OPEN "a:\integer.txt" FOR RANDOM AS #1 LEN = 4
DIM total AS LONG
FOR k = 65279 TO 65300
   total = k
   PUT #1, (k - 65278), total
NEXT k

