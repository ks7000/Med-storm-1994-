'funcion para determinar si hay valores mal escritos
'cadn$ = LTRIM$(cadn$): cadn$ = RTRIM$(cadn$): cadn$ = STR$(VAL(cadn$)): punto% = 0: punt% = 0: longi% = LEN(cadn$): FOR k = 1 TO longi%: IF MID$(cadn$, k, 1) = "." THEN punto% = punto% + 1: punt% = k
'NEXT k: IF punto% > 1 THEN cadn$ = "0.00": SOUND 400, 1: GOTO jfk
'IF ente% = 1 THEN
     'fr& = INT(VAL(cadn$)): IF MID$(STR$(fr&), 1, 1) = "-" THEN cadn$ = MID$(STR$(fr&), 1, larg% - 1) ELSE cadn$ = MID$(STR$(fr&), 2, larg%)
     'GOTO jfk
'ELSE
     'IF punto% = 0 THEN cadn$ = cadn$ + ".00": GOTO jfk
     'IF punt% = LEN(cadn$) THEN cadn$ = cadn$ + "00": GOTO jfk
     'IF punt% = LEN(cadn$) - 1 THEN cadn$ = cadn$ + "0": GOTO jfk
     'cadn$ = MID$(cadn$, 1, punt%) + MID$(cadn$, punt% + 1, 2)
'END IF
'jfk: IF LEN(cadn$) < larg% THEN cadn$ = STRING$((larg% - LEN(cadn$)), " ") + cadn$
'CheqNum$ = cadn$: EXIT DEF

