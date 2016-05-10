'detal(state%)
CLS : Box 1, 1, 3, 80
IF state% = 1 THEN
     Center 2, "Proceso Detallar: Mueve Existencia de Dispensadores a Gavetas"
ELSE
     Center 2, " Proceso :  Envasar Productos Quimicos , Mueve Existencia a Envasados "
END IF
IF state% = 1 THEN
     Eleg% = Escoge%(6, 8, 2, "")
ELSE
     Eleg% = Escoge%(6, 9, 2, "")
END IF
IF Eleg% <= 0 THEN EXIT DEF
CLOSE : OPEN "f:\kevinprg\kevindat\new.dbf" FOR RANDOM AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
GET #1, Eleg%  ': LOCK #1, Ele%
LOCATE 12, 1: PRINT nomb2$;
PRINT USING "#####,"; CVI(exi12$);
PRINT USING "######,.##"; CVD(pv12$);
PRINT USING "#####,"; CVI(exi22$);
PRINT USING "######,.##"; CVD(pv22$);
PRINT USING "#####,"; CVI(exi32$);
PRINT USING "######,.##"; CVD(pv32$);
FOR k = 6 TO 10: LOCATE k, 1: PRINT SPACE$(80): NEXT k
IF state% = 1 THEN
     Eleg2% = Escoge(6, 3, 2, MID$(nomb2$, 1, 6))
ELSE
     Eleg2% = Escoge(6, 5, 2, MID$(nomb2$, 1, 6))
END IF
IF Eleg2% = 0 THEN
     Center 24, "No Se Hallaron Otros Articulos Con El MISMO Nombre de Donde Detallar/Envasar"
     EXIT DEF
END IF
FOR k = 6 TO 10: LOCATE k, 1: PRINT SPACE$(80): NEXT k
LOCATE 15, 1: PRINT SPACE$(80)
Center 25, "[F1]Ayuda  [Enter]Escoger  [F9]Salir SIN Salvar"
LOCATE 13, 1
IF state% = 1 THEN
     PRINT " sera surtido del siguiente dispensador : Escoja el PVP del mismo "
ELSE
     PRINT " sera envasado del siguiente quimico : Escoja el PVP del mismo"
END IF

paso111a:
'COLOR 15, 0
'FOR k = 14 TO 17
'LOCATE k, 1
'PRINT SPACE$(80)
'NEXT k
'LOCATE 15, 1
'PRINT Xname(Eleg2%).Nombre; "³";
'LOCATE 15, 41
'PRINT Xname(Eleg2%).Pvp1
'LOCATE 15, 56
'PRINT Xname(Eleg2%).Pvp2
'LOCATE 15, 70
'PRINT Xname(Eleg2%).Pvp3; "³"
'sitio% = 1
'LOCATE 16, 47
'PRINT Xname(Eleg2%).exis1;
'LOCATE 16, 62
'PRINT Xname(Eleg2%).Exis2;
'LOCATE 16, 76
'PRINT Xname(Eleg2%).Exis3; "³"
'LOCATE 16, 36
'PRINT "³"
'LOCATE 15, 51
'PRINT "³"
'LOCATE 15, 66
'PRINT "³"
'LOCATE 16, 51
'PRINT "³"
'LOCATE 16, 66
'PRINT "³"
'LOCATE 14, 36
'PRINT "ÚPVP1"; STRING$(10, "Ä"); "ÂPVP2"; STRING$(10, "Ä"); "ÂPVP3"; STRING$(9, "Ä"); "¿";
'LOCATE 17, 36
'PRINT "ÀExis. 1"; STRING$(7, "Ä"); "ÁExis. 2"; STRING$(7, "Ä"); "ÁExis. 3"; STRING$(6, "Ä"); "Ù";
'
paso111:
'COLOR 0, 15
'LOCATE 15, 41
'PRINT Xname(Eleg2%).Pvp1
'GOSUB obti
'COLOR 15, 0
'LOCATE 15, 41
'PRINT Xname(Eleg2%).Pvp1
'IF sitio% = 3 THEN GOTO paso113

paso112:
'COLOR 0, 15
'LOCATE 15, 56
'PRINT Xname(Eleg2%).Pvp2
'GOSUB obti
'COLOR 15, 0
'LOCATE 15, 56
'PRINT Xname(Eleg2%).Pvp2
'IF sitio% = 1 THEN GOTO paso111

paso113:
'COLOR 0, 15
'LOCATE 15, 70
'PRINT Xname(Eleg2%).Pvp3
'GOSUB obti
'COLOR 15, 0
'LOCATE 15, 70
'PRINT Xname(Eleg2%).Pvp3
'IF sitio% = 1 THEN GOTO paso111
'IF sitio% = 2 THEN GOTO paso112

paso114:
'LOCATE 18, 1
'IF state% = 1 THEN
     'PRINT " Cuantas Unidades tiene este dispensador? "
'ELSE
     'PRINT " Cuantos gramos o mililitros tiene este envase? "
'END IF
'canti% = VAL(EntraDatoNumero(18, 55, 4, "   0", 1))
'IF canti% <= 0 THEN GOTO paso114
'Center 25, " Introduzca el  precio ajustado para ser Actualizado en la Base de Datos "
'SELECT CASE sitio%
     'CASE 1: IF state% = 1 THEN prize! = VAL(Xname(Eleg2%).Pvp1) / canti%: precio2$ = Xname(Eleg2%).Pvp1 ELSE prize! = VAL(Xname(Eleg2%).Pvp1) * canti%: precio2$ = Xname(Eleg2%).Pvp1
     'CASE 2: IF state% = 1 THEN prize! = VAL(Xname(Eleg2%).Pvp2) / canti%: precio2$ = Xname(Eleg2%).Pvp2 ELSE prize! = VAL(Xname(Eleg2%).Pvp2) * canti%: precio2$ = Xname(Eleg2%).Pvp2
     'CASE 3: IF state% = 1 THEN prize! = VAL(Xname(Eleg2%).Pvp3) / canti%: precio2$ = Xname(Eleg2%).Pvp3 ELSE prize! = VAL(Xname(Eleg2%).Pvp3) * canti%: precio2$ = Xname(Eleg2%).Pvp3
'END SELECT
'IF prize! = 0 THEN
'Center 25, "Este PVP es de 0.00 , Por favor Escoja Otro PVP": GOTO paso111a
'END IF
'precio$ = STR$(prize!)
'precio$ = CheqNum$(precio$, 0, 10)
'LOCATE 18, 1
'PRINT " El precio por unidad es de Bs. "; precio$; SPACE$(25)
'precio$ = CheqNum$(precio$, 0, 10)
'LOCATE 19, 1
'PRINT " Desea ajustar este precio? "
'precio$ = EntraDatoNumero(19, 30, 10, precio$, 0)
'precio$ = CheqNum$(precio$, 0, 10)
'Center 25, " Introduzca Cantidad de Dispensadores a ser detallados [F9]Salir SIN Salvar"

paso115:
'LOCATE 20, 1
'IF state% = 1 THEN
     'PRINT " Cuantos Dispensadores desea detallar? "
'ELSE
     'PRINT " Cuantos papeletas o frascos desea envasar? "
'END IF
'Canti2$ = EntraDatoNumero(20, 55, 4, "   1", 1)
'IF Fun(9) = "si" THEN
     'Fun(9) = "no": EXIT SUB
'END IF
'IF VAL(Canti2$) = 0 THEN
     'GOTO paso115
'END IF
'IF state% = 2 THEN
     'Canti3$ = Canti2$: Canti2$ = STR$(canti% * VAL(Canti2$))
'END IF
'SELECT CASE sitio%
     'CASE 1: IF state% = 1 THEN IF VAL(Canti2$) > VAL(Xname(Eleg2%).exis1) THEN Center 24, "La Cantidad sobrepasa la Existencia,Tendra Inventario en NEGATIVO"
     'CASE 2: IF state% = 1 THEN IF VAL(Canti2$) > VAL(Xname(Eleg2%).Exis2) THEN Center 24, "La Cantidad sobrepasa la Existencia,Tendra Inventario en NEGATIVO"
     'CASE 3: IF state% = 1 THEN IF VAL(Canti2$) > VAL(Xname(Eleg2%).Exis3) THEN Center 24, "La Cantidad sobrepasa la Existencia,Tendra Inventario en NEGATIVO"
'END SELECT
'IF state% = 1 THEN
     'Canti3$ = STR$(VAL(Canti2$) * canti%): Canti2$ = "-" + Canti2$
'ELSE
     'Canti2$ = "-" + Canti2$
'END IF
'' atencion: la idea aqui para actualizar cantidades es la misma de borrar
' ofetas , pero de un solo renglon que es el que recibe envasado o detallado
EXIT DEF

obti:
'seguir = true: llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
'SELECT CASE llave$
            'CASE CHR$(0) + "K": sitio% = sitio% - 1: IF sitio% = 0 THEN sitio% = 3
            'CASE CHR$(0) + "M": sitio% = sitio% + 1: IF sitio% = 4 THEN sitio% = 1
            'CASE CHR$(0) + CHR$(67): EXIT SUB
            'CASE CHR$(13): seguir = false
            'CASE ELSE: BEEP: GOTO obti
'END SELECT
'IF seguir = true THEN RETURN ELSE GOTO paso114
RETURN

