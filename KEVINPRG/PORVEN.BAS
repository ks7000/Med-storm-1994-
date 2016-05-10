'CLS : COLOR 0, 15: Box 1, 1, 3, 80
'Center 2, "Revision de Productos con Pronto Vencimiento"
'Center 5, " Elija la Salida de los Datos y Presione Enter"
'Center 25, "[F1]Ayuda  [F9]Salir SIN Procesar Vencimientos"
'COLOR 15, 0: Box 6, 20, 9, 60
'Center 8, "Por Impresora"
'COLOR 0, 15: Center 7, "Por Pantalla "
'elf% = 1: seguir = true
'FOR k = 1 TO 12: Fun(k) = "no": NEXT k
'paso140:
'DO WHILE seguir = true
'llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
        'SELECT CASE llave$
            'CASE CHR$(0) + "P": IF elf% = 2 THEN COLOR 15, 0: Center 8, "Por Impresora": COLOR 0, 15: Center 7, "Por Pantalla ": elf% = 1 ELSE elf% = 2: COLOR 0, 15: Center 8, "Por Impresora": COLOR 15, 0: Center 7, "Por Pantalla "
            'CASE CHR$(0) + "H": IF elf% = 2 THEN COLOR 15, 0: Center 8, "Por Impresora": COLOR 0, 15: Center 7, "Por Pantalla ": elf% = 1 ELSE elf% = 2: COLOR 0, 15: Center 8, "Por Impresora": COLOR 15, 0: Center 7, "Por Pantalla "
            'CASE CHR$(13): seguir = false
            'CASE CHR$(27): seguir = false: Fun(9) = "si"
            'CASE CHR$(0) + CHR$(67): seguir = false: Fun(9) = "si"
            'CASE ELSE: BEEP
        'END SELECT
'LOOP
'IF Fun(9) = "si" THEN Fun(9) = "no": EXIT SUB
'paso141:
'Center 25, " Introduzca Fecha. [Esc]Salir SIN Procesar Vencimientos"
'fech$ = "  /  /    ": COLOR 0, 15
'LOCATE 11, 10: PRINT " Hasta Que Fecha (dd/mm/aaaa) Desea que se Reporten:"
'fich$ = MeteDatoLetra$(11, 62, 10, fech$)
'fech$ = MID$(fich$, 1, 2) + MID$(fich$, 4, 2) + MID$(fich$, 7, 4)
'IF devolver = true THEN EXIT SUB
'IF CheqDate(fech$, 1) = false THEN BEEP: Center 25, "Fecha NO Valida , Presione Cualquier Tecla Para Continuar...": Espera: GOTO paso141
'fch1% = VAL(MID$(fech$, 1, 2))
'fch2% = VAL(MID$(fech$, 3, 2))
'fch3% = VAL(MID$(fech$, 5, 4))
'Center 25, "Buscando... Por Favor Espere."
'FOR k = 1 TO MaxArt: sel(k) = 0: NEXT k: selecc% = 0
'FOR k = 1 TO MaxArt
'IF Xname(k).Activo = "*" AND Xname(k).Ubicacion <> "05" THEN
     'IF VAL(MID$(Xname(k).Vence1, 5, 4)) < fch3% AND VAL(MID$(Xname(k).Vence1, 5, 4)) > 0 THEN selecc% = selecc% + 1: sel(selecc%) = k: GOTO Er2
     'IF VAL(MID$(Xname(k).Vence2, 5, 4)) < fch3% AND VAL(MID$(Xname(k).Vence2, 5, 4)) > 0 THEN selecc% = selecc% + 1: sel(selecc%) = k: GOTO Er2
     'IF VAL(MID$(Xname(k).Vence3, 5, 4)) < fch3% AND VAL(MID$(Xname(k).Vence3, 5, 4)) > 0 THEN selecc% = selecc% + 1: sel(selecc%) = k: GOTO Er2
     'IF VAL(MID$(Xname(k).Vence1, 5, 4)) = fch3% AND VAL(MID$(Xname(k).Vence1, 5, 4)) > 0 AND VAL(MID$(Xname(k).Vence1, 3, 2)) < fch2% THEN selecc% = selecc% + 1: sel(selecc%) = k: GOTO Er2
     'IF VAL(MID$(Xname(k).Vence2, 5, 4)) = fch3% AND VAL(MID$(Xname(k).Vence2, 5, 4)) > 0 AND VAL(MID$(Xname(k).Vence2, 3, 2)) < fch2% THEN selecc% = selecc% + 1: sel(selecc%) = k: GOTO Er2
     'IF VAL(MID$(Xname(k).Vence3, 5, 4)) = fch3% AND VAL(MID$(Xname(k).Vence3, 5, 4)) > 0 AND VAL(MID$(Xname(k).Vence3, 3, 2)) < fch2% THEN selecc% = selecc% + 1: sel(selecc%) = k: GOTO Er2
     'IF VAL(MID$(Xname(k).Vence1, 5, 4)) = fch3% AND VAL(MID$(Xname(k).Vence1, 5, 4)) > 0 AND VAL(MID$(Xname(k).Vence1, 3, 2)) = fch2% AND VAL(MID$(Xname(k).Vence1, 1, 2)) <= fch1% THEN selecc% = selecc% + 1: sel(selecc%) = k: GOTO Er2
     'IF VAL(MID$(Xname(k).Vence2, 5, 4)) = fch3% AND VAL(MID$(Xname(k).Vence2, 5, 4)) > 0 AND VAL(MID$(Xname(k).Vence2, 3, 2)) = fch2% AND VAL(MID$(Xname(k).Vence2, 1, 2)) <= fch1% THEN selecc% = selecc% + 1: sel(selecc%) = k: GOTO Er2
     'IF VAL(MID$(Xname(k).Vence3, 5, 4)) = fch3% AND VAL(MID$(Xname(k).Vence3, 5, 4)) > 0 AND VAL(MID$(Xname(k).Vence3, 3, 2)) = fch2% AND VAL(MID$(Xname(k).Vence3, 1, 2)) <= fch1% THEN selecc% = selecc% + 1: sel(selecc%) = k: GOTO Er2
'END IF
'Er2:
'NEXT k
'IF selecc% = 0 THEN Center 13, "² No se Encontro ²": Espera: EXIT SUB
'IF elf% = 1 THEN
'FOR k = 4 TO 24: LOCATE k, 1: PRINT SPACE$(80): NEXT k
'punt% = 0
'LOCATE 5, 1
'PRINT STRING$(80, "Ä")
'LOCATE 24, 1: PRINT STRING$(80, "Ä")
'LOCATE 4, 10: PRINT " Productos Por Vencer Hasta:"; fich$
'Er3:
'F'OR k = 6 TO 23
'punt% = punt% + 1
'LOCATE k, 1
'IF punt% <= selecc% THEN PRINT Xname(sel(punt%)).Nombre; "³"; Xname(sel(punt%)).exis1; "³"; Xname(sel(punt%)).Vence1; "³"; Xname(sel(punt%)).Exis2; "³"; Xname(sel(punt%)).Vence2; "³"; Xname(sel(punt%)).Exis3; "³"; Xname(sel(punt%)).Vence3 ELSE PRINT SPACE$(80)
'NEXT k
'Espera
'IF punt% >= selecc% THEN EXIT SUB
'GOTO Er3
'ELSE
'Center 25, " Imprimiendo Datos. Espere Por favor..."
'monto! = 0
'FOR k = 1 TO 80
'h$ = h$ + CHR$(176)
'NEXT k
'S = 1
'OPEN "LPT1:" FOR RANDOM AS #10
'WIDTH #10, 255
'PRINT #10, CHR$(27); "C"; CHR$(0); CHR$(11)
'ha$ = "Descripcion                        Cant1  Vence1 Cant2  Vence2 Cant3  Vence3 "
'PRINT #10, h$; CHR$(10);
'PRINT #10, CHR$(14); "Listado Por Vencer Hasta:"; fich$; CHR$(10);
'PRINT #10, CHR$(20);
'PRINT #10, ha$; CHR$(10);
'ro% = 0
'FOR S = 1 TO selecc%
'ro% = ro% + 1
'PRINT #10, Xname(sel(S)).Nombre; " "; Xname(sel(S)).exis1; " "; Xname(sel(S)).Vence1; " "; Xname(sel(S)).Exis2; " "; Xname(sel(S)).Vence2; " "; Xname(sel(S)).Exis3; " "; Xname(sel(S)).Vence3; CHR$(10);
'monto! = monto! + VAL(Xname(sel(S)).exis1) * VAL(Xname(sel(S)).Pvp1) + VAL(Xname(sel(S)).Exis2) * VAL(Xname(sel(S)).Pvp2) + VAL(Xname(sel(S)).Exis3) * VAL(Xname(sel(S)).Pvp3)
'IF ro% > 50 THEN
'ro% = 0
'PRINT #10, "Sub-total:"; monto!; " Bs."; CHR$(10)
'PRINT #10, h$; CHR$(10);
'PRINT #10, CHR$(12);
'PRINT #10, h$; CHR$(10);
'PRINT #10, CHR$(14); "Listado Por Vencer Hasta:"; fich$; CHR$(10);
'PRINT #10, CHR$(20);
'PRINT #10, ha$; CHR$(10);
'END IF
'NEXT S
'PRINT #10, "Total:";
'PRINT #10, USING "##############.##"; monto!;
'PRINT #10, " Bs."; CHR$(10)
'PRINT #10, h$; CHR$(10);
'PRINT #10, CHR$(12);
'PRINT #10, CHR$(27); "<";
'CLOSE #10
'EXIT SUB
'END IF

