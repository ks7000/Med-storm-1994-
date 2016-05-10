DECLARE FUNCTION DateToJulian& (dat$, caso%)
DEFINT A-Z
DIM promedio AS STRING * 8
OPEN "F:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$, 8 AS cos12$,  _
8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS compo32$, 2 AS  _
compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
max1% = LOF(1) / 200
CLS

OPEN "F:\kevinprg\kevinvnt\estadist.dat" FOR RANDOM AS #3 LEN = 20
FIELD #3, 4 AS unid$, 8 AS primera$, 8 AS ultima$
max% = LOF(3) / 20: l = 0

FOR k = 1 TO max%
     GET #3, k
     IF CVL(unid$) > 0 THEN
          GET #1, k: l = l + 1
          PRINT nomb2$; "³";
          PRINT USING "#####³"; CVL(unid$);
          PRINT primera$; "³"; ultima$;
           IF ultima$ <> "        " AND primera$ <> "        " THEN
             IF ultima$ = primera$ THEN
                promedio = MKD$(0)
             ELSE
                promedio = MKD$(CVL(unid$) / (DateToJulian(ultima$, 1) - DateToJulian(primera$, 1)))
             END IF
           ELSE
              promedio = MKD$(0)
           END IF
          PRINT USING "####,.######"; CVD(promedio) * 7
          IF l > 20 THEN
               l = 0: SLEEP
          END IF
     END IF
NEXT k

FUNCTION DateToJulian& (dat$, caso%)
' reparado el error para fechas mayores que 29 de Febrero de 2000
SELECT CASE caso%
   CASE 1
      ' formato ddmmaaaa
      day% = VAL(MID$(dat$, 1, 2))
      month% = VAL(MID$(dat$, 3, 2))
      year% = VAL(MID$(dat$, 5, 4))
      mes% = VAL(MID$(dat$, 3, 2))
      ano% = VAL(MID$(dat$, 5, 4))
   CASE 2
      ' formato dd/mm/aaaa
      day% = VAL(MID$(dat$, 1, 2))
      month% = VAL(MID$(dat$, 4, 2))
      year% = VAL(MID$(dat$, 7, 4))
      mes% = VAL(MID$(dat$, 4, 2))
      ano% = VAL(MID$(dat$, 7, 4))
   CASE 3
      ' formato mm/dd/aaaa
      day% = VAL(MID$(dat$, 4, 2))
      month% = VAL(MID$(dat$, 1, 2))
      year% = VAL(MID$(dat$, 7, 4))
      mes% = VAL(MID$(dat$, 1, 2))
      ano% = VAL(MID$(dat$, 7, 4))
   CASE 4
      ' formato mmddaaaa
      day% = VAL(MID$(dat$, 3, 2))
      month% = VAL(MID$(dat$, 1, 2))
      year% = VAL(MID$(dat$, 5, 4))
      mes% = VAL(MID$(dat$, 1, 2))
      ano% = VAL(MID$(dat$, 5, 4))
END SELECT
      SELECT CASE month%
          CASE IS > 2
               month% = month% - 3
          CASE IS <= 2
               month% = month% + 9
               year% = year% - 1
      END SELECT
      A& = 146097# * FIX(FIX(year% / 100) / 4)
      B& = FIX(1461# * (year% MOD 100) / 4)
      C& = FIX((153# * month% + 2) / 5) + day% + 1721119
      D& = A& + B& + C&
      SELECT CASE ano%
         CASE 2000
            IF mes% > 2 THEN
               D& = D& - 109573
            END IF
         CASE IS > 2000
               D& = D& - 109573
         CASE ELSE
      END SELECT
      DateToJulian = D&

END FUNCTION

