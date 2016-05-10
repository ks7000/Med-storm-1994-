'borraoferta
COLOR colors(5, colorpref), colors(4, colorpref): CLS : Box 1, 1, 3, 80: Center 25, "[F1]Ayuda  [F9]Salir": FOR k = 1 TO 12: Fun(k) = "no": NEXT k: Center 2, " Borrar Articulos en Oferta de una Drogueria "
DIM auxil AS STRING * 2, Prud1 AS STRING * 4, Prud2 AS STRING * 4, Drog1 AS STRING * 1
COLOR colors(5, colorpref), colors(4, colorpref)
LOCATE 4, 50: PRINT "Desde:²"; Prud1; "²"
LOCATE 4, 64: PRINT "Hasta:²"; Prud2; "²"
Center 25, "[F1]Ayuda  [F9]Salir": salir = false: sit% = 1
DO WHILE NOT salir
SELECT CASE sit%
CASE 1
     Center 23, "1.-[ @=" + Prov1 + " ] 2.-[ #=" + Prov2 + " ] 3.-[ $=" + Prov3 + " ]": COLOR colors(5, colorpref), colors(4, colorpref): LOCATE 4, 2: PRINT "Introduzca numero de Proveedor [1,2 o 3]:"
     Drog1 = MeteDatoLetra(4, 45, 1, "0")
     IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DEF
     IF VAL(Drog1) > 0 OR VAL(Drog1) < 4 THEN sit% = 2
     IF Drog1 = "1" THEN Center 24, "Proveedor Elejido: 1.-" + Prov1
     IF Drog1 = "2" THEN Center 24, "Proveedor Elejido: 2.-" + Prov2
     IF Drog1 = "3" THEN Center 24, "Proveedor Elejido: 3.-" + Prov3
     Fun(9) = "no"
CASE 2
     Prud1 = MeteDatoLetra(4, 56, 4, Prud1): Prud1 = Trim$(Prud1)
     IF Fun(9) = "si" THEN EXIT DEF
     IF devolver = true THEN devolver = false: sit% = 1
     IF LEN(Prud1) > 0 THEN sit% = 3
CASE 3
     Prud2 = MeteDatoLetra(4, 70, 4, Prud2): Prud2 = Trim$(Prud2): IF Fun(9) = "si" THEN EXIT DEF
     IF LEN(Prud2) > 0 THEN salir = true
     IF devolver = true THEN devolver = false: sit% = 2
END SELECT
LOOP
Box 10, 10, 20, 70: Center 15, " Borrando Ofertas , Incluyendo Precios de Oferta ... Espere ... "
ON ERROR GOTO ErrorTrap
CLOSE : OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
OPEN "F:\kevinprg\kevindat\auxilio.fct" FOR RANDOM AS #2 LEN = 40
FIELD #2, 6 AS cod$, 2 AS can$, 8 AS pre$, 8 AS cos$, 8 AS ofe$, 2 AS ofecan$, 1 AS IVA$, 3 AS vence$: lleva% = 1
OPEN "F:\kevinprg\kevindat\oferta.idx" FOR RANDOM SHARED AS #3 LEN = 5
FIELD #3, 3 AS nomshort$, 2 AS num$: max1% = LOF(3) / 5
FOR k = 1 TO max1%: GET #3, k
IF MID$(nomshort$, VAL(Drog1), 1) <> " " THEN
     GET #1, CVI(num$) ': LOCK #1, CVI(num$)
     IF MID$(nomb2$, 1, 6) >= Prud1 AND MID$(nomb2$, 1, 6) <= Prud2 THEN
          RSET cod$ = codigo2$: RSET can$ = exi42$: RSET pre$ = pv32$: RSET cos$ = cos42$: RSET IVA$ = iv42$: RSET ofecan$ = MKI$(0)
          IF preg$ = "*" THEN RSET vence$ = ven41$ + ven42$
          IF Drog1 = "1" THEN RSET of12$ = " "
          IF Drog1 = "2" THEN RSET of22$ = " "
          IF Drog1 = "3" THEN RSET of32$ = " "
          RSET exi42$ = MKI$(0): RSET pv42$ = MKD$(0): RSET cos42$ = MKD$(0): RSET iv42$ = " ": RSET ven41$ = " ": RSET ven42$ = MKI$(0)
          'UNLOCK #1, CVI(num$)
          PUT #1, CVI(num$): PUT #2, lleva%: lleva% = lleva% + 1
     ELSE
          'UNLOCK #1, CVI(num$)
     END IF
END IF
NEXT k: CLOSE
Actualiza ("F:\kevinprg\kevindat\auxilio.fct")
KILL "F:\kevinprg\kevindat\auxilio.fct"
ON ERROR GOTO 0
'atencion , indexar ofertas en este caso y mandar a ErrorTrap si esta siendo usado el idx por otro usuario

