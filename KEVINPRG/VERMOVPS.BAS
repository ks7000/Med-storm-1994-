OPEN "F:\kevinprg\kevindat\movpsico.dat" FOR RANDOM AS #4 LEN = 40
FIELD #4, 40 AS todo$: maxmov% = LOF(4) / 40: CLS
     FOR l = 1 TO maxmov%
          GET #4, l
          IF MID$(todo$, 1, 6) = "VENTA " THEN
          END IF
          IF MID$(todo$, 1, 6) = "CREDIT" THEN
          END IF
          IF MID$(todo$, 1, 6) = "COMPRA" THEN
               buscanumfac$ = MID$(todo$, 28, 8): buscaprovfac$ = MID$(todo$, 38, 3)
               PRINT buscanumfac$; "  "; buscaprovfac$
          END IF
          IF MID$(todo$, 1, 6) = "DEVCRE" THEN
          END IF
          IF MID$(todo$, 1, 6) = "DEVOLU" THEN
          END IF
     NEXT l

