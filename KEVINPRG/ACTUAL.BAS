'sub rutina de actualizacion.
Center 25, "Actualizando Existencias , Por Favor Espere . . ."
ON ERROR GOTO ErrorTrap
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
OPEN "F:\kevinprg\kevindat\codigo.idx" FOR RANDOM SHARED AS #2 LEN = 8
FIELD #2, 6 AS codigo$, 2 AS mini1$: max1% = LOF(2) / 8: CLOSE #2
DIM codigos(max1%) AS STRING * 6: DIM registros(max1%) AS STRING * 2
FOR k = 1 TO max1%: GET #2, k: codigos(k) = codigo$: registros(k) = mini1$: NEXT k
factura$ = "F:\kevinprg\kevindat" + factura$
OPEN factura$ FOR RANDOM AS #2 LEN = 40
FIELD #2, 6 AS cod$, 2 AS can$, 8 AS pre$, 8 AS cos$, 8 AS ofe$, 2 AS ofecan$, 1 AS iva$, 3 AS vence$
max2% = LOF(2) / 40
FOR k = 1 TO max2%: GET #2, k
salir = false: primero = 1: ultimo = max1%: central = 0: terminar = false: sitio = false
WHILE NOT salir AND primero <= ultimo 'busca en indexado de codigo
     central = (primero + ultimo) / 2
     IF cod$ = codigos(central) THEN
          salir = true
     ELSE
          IF codigos(central) > cod$ THEN
               ultimo = central - 1
          ELSE
               primero = central + 1
          END IF
     END IF
WEND
GET #1, CVI(registros(central%)) ': LOCK #1, CVI(registros(central%))
IF CVD(ofe$) = 0 THEN
     IF pv32$ = pre$ AND iva$ = iv32$ THEN
          IF CVD(cos32$) > (CVD(cos$) / CVI(can$)) THEN
               acer$ = MKD$(((CVD(cos$) / CVI(can$)) / CVD(cos32$)) * 100)
          ELSE
               acer$ = MKD$((CVD(cos32$) / (CVD(cos$) / CVI(can$))) * 100)
          END IF
          IF CVD(acer$) > VAL(Exactitud) THEN
               IF preg$ = "*" THEN
                    IF vence$ = (ven31$ + ven32$) THEN
                         RSET exi32$ = MKI$(CVD(exi32$) + CVI(can$) + CVI(ofecan$)): terminar = true
                    END IF
               ELSE
                    RSET exi32$ = MKI$(CVD(exi32$) + CVI(can$) + CVI(ofecan$)): terminar = true
               END IF
          END IF
     END IF
     IF terminar = false THEN
     IF pv22$ = pre$ AND iva$ = iv22$ THEN
          IF CVD(cos22$) > (CVD(cos$) / CVI(can$)) THEN
               acer$ = MKD$(((CVD(cos$) / CVI(can$)) / CVD(cos22$)) * 100)
          ELSE
               acer$ = MKD$((CVD(cos22$) / (CVD(cos$) / CVI(can$))) * 100)
          END IF
          IF CVD(acer$) > VAL(Exactitud) THEN
               IF preg$ = "*" THEN
                    IF vence$ = (ven21$ + ven22$) THEN
                         RSET exi22$ = MKI$(CVD(exi22$) + CVI(can$) + CVI(ofecan$)): terminar = true
                    END IF
               ELSE
                    RSET exi22$ = MKI$(CVD(exi22$) + CVI(can$) + CVI(ofecan$)): terminar = true
               END IF
          END IF
     END IF
     END IF
     IF terminar = false THEN
     IF pv12$ = pre$ AND iva$ = iv12$ THEN
          IF CVD(cos12$) > (CVD(cos$) / CVI(can$)) THEN
               acer$ = MKD$(((CVD(cos$) / CVI(can$)) / CVD(cos12$)) * 100)
          ELSE
               acer$ = MKD$((CVD(cos12$) / (CVD(cos$) / CVI(can$))) * 100)
          END IF
          IF CVD(acer$) > VAL(Exactitud) THEN
               IF preg$ = "*" THEN
                    IF vence$ = (ven11$ + ven12$) THEN
                         RSET exi12$ = MKI$(CVD(exi12$) + CVI(can$) + CVI(ofecan$)): terminar = true
                    END IF
               ELSE
                    RSET exi12$ = MKI$(CVD(exi12$) + CVI(can$) + CVI(ofecan$)): terminar = true
               END IF
          END IF
     END IF
     END IF
     IF terminar = false THEN sitio = true
     IF terminar = false THEN
     IF CVD(pv32$) = 0 AND CVI(exi42$) = 0 THEN
               RSET iv32$ = iva$: RSET pv32$ = pre$: RSET cos32$ = cos$
               RSET exi32$ = MKI$(CVI(can$) + CVI(ofecan$)): terminar = true
               IF preg$ = "*" THEN RSET ven31$ = MID$(vence$, 1, 1): RSET ven32$ = MID$(vence$, 2, 2)
     END IF
     END IF
     IF terminar = false THEN
     IF CVD(pv22$) = 0 THEN
               RSET iv22$ = iva$: RSET pv22$ = pre$: RSET cos22$ = cos$
               RSET exi22$ = MKI$(CVI(can$) + CVI(ofecan$)): terminar = true
               IF preg$ = "*" THEN RSET ven21$ = MID$(vence$, 1, 1): RSET ven22$ = MID$(vence$, 2, 2)
     END IF
     END IF
     IF terminar = false THEN
     IF CVD(pv12$) = 0 THEN
               RSET iv12$ = iva$: RSET pv12$ = pre$: RSET cos12$ = cos$
               RSET exi12$ = MKI$(CVI(can$) + CVI(ofecan$)): terminar = true
               IF preg$ = "*" THEN RSET ven11$ = MID$(vence$, 1, 1): RSET ven12$ = MID$(vence$, 2, 2)
     END IF
     END IF
     IF terminar = false THEN
     IF CVI(exi32$) = 0 AND CVI(exi42$) = 0 THEN
               RSET iv32$ = iva$: RSET pv32$ = pre$: RSET cos32$ = cos$
               RSET exi32$ = MKI$(CVI(can$) + CVI(ofecan$)): terminar = true
               IF preg$ = "*" THEN RSET ven31$ = MID$(vence$, 1, 1): RSET ven32$ = MID$(vence$, 2, 2)
     END IF
     END IF
     IF terminar = false THEN
     IF CVI(exi22$) = 0 THEN
               RSET iv22$ = iva$: RSET pv22$ = pre$: RSET cos22$ = cos$
               RSET exi22$ = MKI$(CVI(can$) + CVI(ofecan$)): terminar = true
               IF preg$ = "*" THEN RSET ven21$ = MID$(vence$, 1, 1): RSET ven22$ = MID$(vence$, 2, 2)
     END IF
     END IF
     IF terminar = false THEN
     IF CVI(exi12$) = 0 THEN
               RSET iv12$ = iva$: RSET pv12$ = pre$: RSET cos12$ = cos$
               RSET exi12$ = MKI$(CVI(can$) + CVI(ofecan$)): terminar = true
               IF preg$ = "*" THEN RSET ven11$ = MID$(vence$, 1, 1): RSET ven12$ = MID$(vence$, 2, 2)
     END IF
     END IF
ELSE
     RSET exi42$ = MKI$(CVD(exi42$) + CVI(can$) + CVI(ofecan$))
     IF preg$ = "*" THEN RSET ven41$ = MID$(vence$, 1, 1): RSET ven42$ = MID$(vence$, 2, 2)
END IF
IF sitio = true THEN
     IF CVD(exi42$) = 0 THEN
          RSET iv42$ = " ": RSET pv42$ = MKD$(0): RSET cos42$ = MKD$(0): RSET ven41$ = "0": RSET ven42$ = MKL$(0)
          IF CVD(pv12$) > CVD(pv22$) AND CVD(pv12$) < CVD(pv32$) THEN GOSUB Cambia1y2: sitio = false
          IF sitio = true THEN IF CVD(pv12$) < CVD(pv22$) AND CVD(pv22$) < CVD(pv32$) THEN sitio = false
          IF sitio = true THEN IF CVD(pv12$) < CVD(pv22$) AND CVD(pv12$) > CVD(pv32$) THEN GOSUB Cambia1y2: GOSUB Cambia1y3: sitio = false
          IF sitio = true THEN IF CVD(pv12$) > CVD(pv22$) AND CVD(pv22$) < CVD(pv32$) THEN GOSUB Cambia1y2: GOSUB Cambia2y3: sitio = false
          IF sitio = true THEN IF CVD(pv12$) < CVD(pv22$) AND CVD(pv22$) > CVD(pv32$) THEN GOSUB Cambia2y3: sitio = false
          IF sitio = true THEN IF CVD(pv12$) > CVD(pv22$) AND CVD(pv22$) > CVD(pv32$) THEN GOSUB Cambia1y3: sitio = false
          IF sitio = true THEN IF CVD(pv12$) = CVD(pv22$) AND CVD(pv22$) < CVD(pv32$) THEN sitio = false
          IF sitio = true THEN IF CVD(pv12$) = CVD(pv32$) AND CVD(pv22$) > CVD(pv32$) THEN GOSUB Cambia2y3: sitio = false
          IF sitio = true THEN IF CVD(pv12$) > CVD(pv22$) AND CVD(pv22$) = CVD(pv32$) THEN GOSUB Cambia1y3: sitio = false
          IF sitio = true THEN IF CVD(pv12$) = CVD(pv22$) AND CVD(pv22$) > CVD(pv32$) THEN GOSUB Cambia1y3: sitio = false
          IF sitio = true THEN IF CVD(pv12$) = CVD(pv32$) AND CVD(pv22$) < CVD(pv32$) THEN GOSUB Cambia1y2: sitio = false
          IF sitio = true THEN IF CVD(pv22$) = CVD(pv32$) AND CVD(pv12$) < CVD(pv32$) THEN sitio = false
     ELSE
          IF CVD(pv12$) > CVD(pv22$) AND CVD(pv12$) < CVD(pv32$) THEN GOSUB Cambia1y2: sitio = false
          IF sitio = true THEN IF CVD(pv12$) < CVD(pv22$) AND CVD(pv22$) < CVD(pv32$) THEN sitio = false
          IF sitio = true THEN IF CVD(pv12$) = CVD(pv22$) AND CVD(pv22$) < CVD(pv32$) THEN sitio = false
          IF sitio = true THEN IF CVD(pv12$) = CVD(pv32$) AND CVD(pv22$) < CVD(pv32$) THEN GOSUB Cambia1y2: sitio = false
          IF sitio = true THEN IF CVD(pv22$) = CVD(pv32$) AND CVD(pv12$) < CVD(pv32$) THEN sitio = false
     END IF
END IF
'UNLOCK #1, CVI(registros(central%))
PUT #1, CVI(registros(central%))
NEXT k: CLOSE
ON ERROR GOTO 0
EXIT DEF

Cambia1y2:
Auxi$ = pv12$: RSET pv12$ = pv22$: RSET pv22$ = Auxi$
Auxi$ = cos12$: RSET cos12$ = cos22$: RSET cos22$ = Auxi$
Auxi$ = iv12$: RSET iv12$ = iv22$: RSET iva22$ = Auxi$
Auxi$ = exi12$: RSET exi12$ = exi22$: RSET exi22$ = Auxi$
IF preg$ = "*" THEN
Auxi$ = ven11$: RSET ven21$ = ven11$: RSET ven11$ = Auxi$
Auxi$ = ven12$: RSET ven22$ = ven12$: RSET ven12$ = Auxi$
END IF
RETURN
Cambia1y3:
Auxi$ = pv12$: RSET pv12$ = pv32$: RSET pv32$ = Auxi$
Auxi$ = cos12$: RSET cos12$ = cos32$: RSET cos32$ = Auxi$
Auxi$ = iv12$: RSET iv12$ = iv32$: RSET iva32$ = Auxi$
Auxi$ = exi12$: RSET exi12$ = exi32$: RSET exi32$ = Auxi$
IF preg$ = "*" THEN
Auxi$ = ven11$: RSET ven31$ = ven11$: RSET ven11$ = Auxi$
Auxi$ = ven12$: RSET ven32$ = ven12$: RSET ven32$ = Auxi$
END IF
RETURN
Cambia2y3:
Auxi$ = pv22$: RSET pv22$ = pv32$: RSET pv32$ = Auxi$
Auxi$ = cos22$: RSET cos22$ = cos32$: RSET cos32$ = Auxi$
Auxi$ = iv22$: RSET iv22$ = iv32$: RSET iva32$ = Auxi$
Auxi$ = exi22$: RSET exi22$ = exi32$: RSET exi32$ = Auxi$
IF preg$ = "*" THEN
Auxi$ = ven11$: RSET ven21$ = ven11$: RSET ven11$ = Auxi$
Auxi$ = ven12$: RSET ven22$ = ven12$: RSET ven12$ = Auxi$
END IF
RETURN

