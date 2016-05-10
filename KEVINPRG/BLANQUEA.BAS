'programa para dibujar triangulos
DEFDBL A-Z
CLS : cont = 0
COLOR 7, 0
SCREEN 2
VIEW
WINDOW (-1, -0)-(4, 1)
RANDOMIZE TIMER
xi = 0
yi = 0
DO
ri = RND
IF ri <= .33 THEN
     ai = .5: bi = 0: ci = 0: di = .5: ei = 0: fi = 0
     ELSEIF ri > .33 AND ri <= .66 THEN
          ai = .5: bi = 0: ci = 0: di = .5: ei = 1: fi = 0
     ELSE
          ai = .5: bi = 0: ci = 0: di = .5: ei = .5: fi = .5
END IF
newx = (ai * xi) + (bi * yi) + ei
newy = (ci * xi) + (di * yi) + fi
xi = newx
yi = newy
cont = cont + 1
LOCATE 1, 1: PRINT USING "#####.##"; xi; yi; cont
PSET (xi, yi), 2
LOOP

END

