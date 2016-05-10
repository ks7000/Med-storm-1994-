  'menu principal
DEFINT A-Z
DECLARE SUB ScrollUp ()
DECLARE SUB ScrollDown ()
DECLARE SUB Center (row%, text$)
DECLARE SUB FancyCls (dots%, Background%)
DECLARE SUB MenuMadre ()
DECLARE SUB Box (Row1%, Col1%, Row2%, Col2%)
DECLARE SUB Box2 (Row1, Col1, Row2, Col2)
DECLARE SUB PrintHelpLine (help$)
DECLARE SUB NotaSalir ()
DECLARE SUB LeeEstado ()
DECLARE SUB EntraSena ()
DECLARE SUB SalvaEstado ()
DECLARE SUB Bloqueo ()
DECLARE SUB Espera ()
DECLARE SUB Graba ()
DECLARE FUNCTION Menu% (CurrChoiceX%, MaxChoice%, choice$(), ItemRow%(), ItemCol%(), help$(), BarMode%)
DECLARE FUNCTION Verif$ (fila%, columna%)
DECLARE FUNCTION Trim$ (x$)
DECLARE FUNCTION Verify (ubi%, text$)
DECLARE FUNCTION DateToJulian& (dat$, caso%)
COMMON SHARED guia AS INTEGER, colorpref AS INTEGER
DIM SHARED nursaler, turno, dia1, mes1, ano1, salir, estacion AS STRING * 2
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, impuesto AS STRING * 6, Nombfarma AS STRING * 25, Direccion AS STRING * 30, Rif     AS STRING * 25, Nit      AS STRING * 25, Telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 3
DIM SHARED Desc AS STRING * 2, DescMax AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, Exactitud AS STRING * 2, NuevoIva AS STRING * 2, ShowPerc AS STRING * 2
DIM SHARED ScrollUpAsm(1 TO 7), Fun(17) AS STRING * 2, eleccion AS STRING
DIM SHARED ScrollDownAsm(1 TO 7), grabGav AS STRING * 1
CONST true = -1: CONST false = NOT true
'CLOSE : OPEN "c:\kevinprg\kevindat\sistema.tkt" FOR RANDOM AS #1 LEN = 1: FIELD #1, 1 AS ticket$
'RSET ticket$ = "*": PUT #1, 1: CLOSE
ON ERROR GOTO ErrorTrap
ON TIMER(5) GOSUB AbreGav
TIMER ON
RESTORE: WIDTH , 25: VIEW PRINT: FOR ColorSet = 1 TO 4: FOR x = 1 TO 10: READ colors(x, ColorSet): NEXT x: NEXT ColorSet: LeeEstado: dia2 = VAL(MID$(DATE$, 4, 2)): mes2 = VAL(MID$(DATE$, 1, 2)): ano2 = VAL(MID$(DATE$, 7, 4))
p = VARPTR(ScrollUpAsm(1)): DEF SEG = VARSEG(ScrollUpAsm(1)): FOR I = 0 TO 13: READ J: POKE (p + I), J: NEXT I: p = VARPTR(ScrollDownAsm(1)): DEF SEG = VARSEG(ScrollDownAsm(1)): FOR I = 0 TO 13: READ J: POKE (p + I), J: NEXT I: DEF SEG
CALL MenuMadre
END

ErrorTrap:
SELECT CASE ERR
     CASE 25, 27, 57, 68
          Box2 5, 2, 7, 78: Center 5, "Mensaje": Center 6, "No Se Detect¢ Gavetero;NO DEBE Usar Esta Estacion Para Ventas de Contado": RESUME NEXT
     CASE 70
          Center 25, "Esperando Acceso a La Red , Espere Por Favor . . .": SLEEP 1: BEEP: RESUME
     CASE ELSE
          PRINT ERR
END SELECT
RESUME NEXT

'    scrn  dots  bar  back   title  shdow  choice  curs   cursbk  shdow
DATA 0,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  2,     0,     1,     15,     0,     7,      0
DATA 3,    15,   13,  1,     14,    3,     15,     0,     7,      0
DATA 7,    12,   15,  4,     14,    0,     15,     15,    1,      0
DATA &HB8,&H01,&H06,&HB9,&H01,&H04,&HBA,&H4E,&H16,&HB7,&H00,&HCD,&H10,&HCB
DATA &HB8,&H01,&H07,&HB9,&H01,&H04,&HBA,&H4E,&H16,&HB7,&H00,&HCD,&H10,&HCB

AbreGav:
'  CLOSE #27:
'  OPEN "c:\kevinprg\kevindat\gaveta.sem" FOR RANDOM SHARED AS #27 LEN = 1
'  GET #27, 1, grabGav
'  SELECT CASE grabGav
'    CASE "1"
'      CLOSE #28: OPEN "com1" FOR OUTPUT AS #28: PRINT #28, "A": PRINT #28, "A": CLOSE #28
'      grabGav = "0": PUT #27, 1, grabGav
'    CASE "2"
'      CLOSE #28: OPEN "com2" FOR OUTPUT AS #28: PRINT #28, "A": PRINT #28, "A": CLOSE #28
'      grabGav = "0": PUT #27, 1, grabGav
'    CASE ELSE
'      'LOCATE 1, 1: PRINT grabGav
'  END SELECT
'  CLOSE #27
RETURN

SUB Bloqueo
CLOSE
IF guia = 1 THEN
     OPEN "c:\kevinprg\kevindat\credito.tkt" FOR RANDOM AS #1 LEN = 2: FIELD #1, 2 AS ticket$: GET #1, 1
ELSE
     OPEN "c:\kevinprg\kevindat\compras.tkt" FOR RANDOM AS #1 LEN = 2: FIELD #1, 2 AS ticket$: GET #1, 1
END IF
IF ticket$ = "si" THEN
  RSET ticket$ = "no": PUT #1, 1: guia = 777
ELSE
  Box 10, 10, 14, 70: Center 11, "Modulo OCUPADO por otro Usuario"
  Center 13, "¨ DESEA FORZAR LA ENTRADA AL MODULO DESEADO ? Pulse S"
  Kbd$ = "": DO WHILE Kbd$ = "": Kbd$ = INKEY$: LOOP: Kbd$ = UCASE$(Kbd$)
  IF Kbd$ = "S" THEN
      RSET ticket$ = "no": PUT #1, 1: guia = 777
  END IF
END IF
CLOSE
'OPEN "c:\kevinprg\kevindat\compras.tkt" FOR RANDOM AS #1 LEN = 2: FIELD #1, 2 AS ticket$: RSET ticket$ = "si": PUT #1, 1: CLOSE
END SUB

SUB Box (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿"; : FOR A = Row1 + 1 TO Row2 - 1: LOCATE A, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT A: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Box2 (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "²"; STRING$(BoxWidth - 2, "²"); "²"; : FOR A = Row1 + 1 TO Row2 - 1: LOCATE A, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT A: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Center (row, text$)
IF row = 25 THEN COLOR colors(5, colorpref), colors(4, colorpref): LOCATE 25, 1: PRINT SPACE$(80);
LOCATE row, 41 - LEN(text$) / 2: PRINT text$;
END SUB

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

SUB EntraSena
IF contri = "si" THEN IF Verif$(23, 25) = "false" THEN LOCATE 23, 25: PRINT SPC(6); CHR$(178); "Clave Incorrecta"; CHR$(178); SPC(6); : FOR k = 1 TO 10000: NEXT k: Fun(12) = "si": EXIT SUB
END SUB

SUB Espera
Center 25, "Presione Cualquier Tecla Para Continuar...": DO: LOOP WHILE INKEY$ = ""
END SUB

SUB FancyCls (dots, Background)
VIEW PRINT 2 TO 24: COLOR dots, Background: CLS 2: FOR A = 95 TO 1820 STEP 45: row = A / 80 + 1: col = A MOD 80 + 1: LOCATE row, col: PRINT CHR$(250); : NEXT A: VIEW PRINT
END SUB

SUB Graba
CLOSE #10: OPEN "c:\kevinprg\kevindat\shell.sys" FOR APPEND LOCK READ WRITE AS #10
IF VAL(MID$(TIME$, 1, 2)) > 12 THEN
   tim$ = STR$((VAL(MID$(TIME$, 1, 2)) - 12)) + MID$(TIME$, 3, 6) + " p.m."
ELSE
   tim$ = TIME$ + " a.m."
END IF
aux$ = "Fecha: " + MID$(DATE$, 4, 2) + "/" + MID$(DATE$, 1, 2) + "/" + MID$(DATE$, 7, 4) + " hora:" + tim$ + " clave intentada:" + eleccion + CHR$(13)
PRINT #10, aux$: CLOSE #10
END SUB

SUB LeeEstado
DIM fecha AS STRING
CLOSE
OPEN "c:\kevinprg\kevindat\kaos.dat" FOR INPUT AS #2
INPUT #2, sonido: INPUT #2, impuesto
INPUT #2, Nombfarma: INPUT #2, Direccion
INPUT #2, Rif: INPUT #2, Nit
INPUT #2, Telefono: INPUT #2, contri
INPUT #2, contra: INPUT #2, Stock
INPUT #2, Blanq: INPUT #2, LargoH
INPUT #2, TipoL: INPUT #2, Mensaje1
INPUT #2, Mensaje2: INPUT #2, Mensaje3
INPUT #2, Redondeo: INPUT #2, Desc
INPUT #2, DescMax: INPUT #2, Exactitud
INPUT #2, Prov1: INPUT #2, Prov2
INPUT #2, Prov3: INPUT #2, negativo
INPUT #2, NuevoIva: INPUT #2, ShowPerc
CLOSE #2
OPEN "c:\kevinprg\fecha.dat" FOR INPUT AS #2
INPUT #2, fecha: CLOSE #2
IF DateToJulian(DATE$, 3) < DateToJulian(fecha, 3) THEN
  guia = 6: NotaSalir: CHAIN "c:\kevinprg\kevin07.bas"
ELSE
  OPEN "c:\kevinprg\fecha.dat" FOR OUTPUT AS #2
  PRINT #2, DATE$: CLOSE #2
END IF
END SUB

FUNCTION Menu (CurrChoiceX, MaxChoice, choice$(), ItemRow(), ItemCol(), help$(), BarMode)
    currChoice = CurrChoiceX
    IF BarMode THEN
        COLOR colors(7, colorpref), colors(4, colorpref): LOCATE 1, 1: PRINT SPACE$(80);
    ELSE
        FancyCls colors(2, colorpref), colors(1, colorpref): COLOR colors(7, colorpref), colors(4, colorpref): Box ItemRow(1) - 1, ItemCol(1) - 1, ItemRow(MaxChoice) + 1, ItemCol(1) + LEN(choice$(1)) + 1: COLOR colors(10, colorpref), colors(6,  _
colorpref): FOR A = 1 TO MaxChoice + 1: LOCATE ItemRow(1) + A - 1, ItemCol(1) + LEN(choice$(1)) + 2: PRINT CHR$(178); CHR$(178); : NEXT A: LOCATE ItemRow(MaxChoice) + 2, ItemCol(MaxChoice) + 2: PRINT STRING$(LEN(choice$(MaxChoice)) + 2, 178);
    END IF
    COLOR colors(7, colorpref), colors(4, colorpref): FOR A = 1 TO MaxChoice: LOCATE ItemRow(A), ItemCol(A): PRINT choice$(A); : NEXT A: finished = false
    WHILE NOT finished: GOSUB MenuShowCursor: GOSUB MenuGetKey: GOSUB MenuHideCursor
        SELECT CASE Kbd$
            CASE CHR$(0) + "H": GOSUB MenuUp
            CASE CHR$(0) + "P": GOSUB MenuDown
            CASE CHR$(0) + "K": GOSUB MenuLeft
            CASE CHR$(0) + "M": GOSUB MenuRight
            CASE CHR$(13): GOSUB MenuEnter
            CASE CHR$(27): GOSUB MenuEscape
            CASE ELSE:  BEEP
        END SELECT
    WEND
    Menu = currChoice: EXIT FUNCTION
MenuEnter: finished = true: RETURN
MenuEscape: currChoice = 0: finished = true: RETURN
MenuUp: IF BarMode THEN BEEP ELSE currChoice = currChoice - 1: IF currChoice = 0 THEN currChoice = MaxChoice
    RETURN
MenuLeft:
    IF BarMode THEN
        currChoice = currChoice - 1: IF currChoice = 0 THEN currChoice = 7
    ELSE
        currChoice = -2: finished = true
    END IF: RETURN
MenuRight:
    IF BarMode THEN
        currChoice = currChoice + 1: IF currChoice = 8 THEN currChoice = 1
    ELSE
        currChoice = -3: finished = true
    END IF: RETURN
MenuDown: IF BarMode THEN finished = true ELSE currChoice = currChoice + 1: IF currChoice > MaxChoice THEN currChoice = 1
    RETURN
MenuShowCursor: COLOR colors(8, colorpref), colors(9, colorpref): LOCATE ItemRow(currChoice), ItemCol(currChoice): PRINT choice$(currChoice); : PrintHelpLine help$(currChoice): RETURN
MenuGetKey: lastTime# = TIMER: Kbd$ = "": WHILE Kbd$ = "": Kbd$ = INKEY$: LOCATE 1, 73: PRINT TIME$: NewTime# = TIMER: IF (NewTime# - lastTime#) > VAL(Blanq) THEN GOSUB Triangul
    WEND: RETURN
MenuHideCursor: COLOR colors(7, colorpref), colors(4, colorpref): LOCATE ItemRow(currChoice), ItemCol(currChoice): PRINT choice$(currChoice); : RETURN
Triangul:
CLOSE : OPEN "c:\pantalla.dat" FOR INPUT AS #1: INPUT #1, ord%: lastTime# = TIMER
IF ord% = 1 THEN
  COLOR 15, 0: r$ = "Pulse cualquier tecla": CLS
  y% = FIX((RND * 24) + 1): x% = FIX((RND * 59) + 1)
  LOCATE y%, x%: PRINT r$; : lastTime# = TIMER
  WHILE INKEY$ = ""
        NewTime# = TIMER
        IF (NewTime# - lastTime#) > VAL(Blanq) THEN
          CLS : y% = FIX((RND * 24) + 1): x% = FIX((RND * 59) + 1)
          LOCATE y%, x%: PRINT r$; : lastTime# = TIMER
        END IF
  WEND: : FancyCls colors(2, colorpref), colors(1, colorpref): Kbd$ = CHR$(27): RETURN
END IF
IF ord% = 2 THEN SCREEN 12 ELSE RETURN
CLS : STATIC xi AS SINGLE, yi AS SINGLE, ai AS SINGLE, bi AS SINGLE, ci AS SINGLE, di AS SINGLE: STATIC ei AS SINGLE, fi AS SINGLE, newx AS SINGLE, newy AS SINGLE, ri AS SINGLE, rt AS SINGLE, ru AS SINGLE: DIM Cop AS STRING * 1: Cop = ""
Fini1:
CLS : VIEW (0, 0)-(639, 479): WINDOW (-1, -0)-(4, 1): RANDOMIZE TIMER: xi = 0: yi = 0: lastTime# = TIMER
WHILE INKEY$ = "": ri = RND: Cop = INKEY$
NewTime# = TIMER: IF (NewTime# - lastTime#) > VAL(Blanq) THEN GOTO Fini2
IF ri <= .33 THEN
     ai = .5: bi = 0: ci = 0: di = .5: ei = 0: fi = 0
     ELSEIF ri > .33 AND ri <= .66 THEN
          ai = .5: bi = 0: ci = 0: di = .5: ei = 1: fi = 0
     ELSE
          ai = .5: bi = 0: ci = 0: di = .5: ei = .5: fi = .5
END IF
newx = (ai * xi) + (bi * yi) + ei: newy = (ci * xi) + (di * yi) + fi: xi = newx: yi = newy
IF ord% = 2 THEN PSET (xi, yi), (ri * 10) ELSE PSET (xi, yi), 10
WEND
IF Cop <> "" THEN GOTO Fini3
Fini2:
CLS : WINDOW (-2, 0)-(6, 10): RANDOMIZE TIMER: xi = 0: yi = 0: lastTime# = TIMER
WHILE INKEY$ = ""
ri = RND: NewTime# = TIMER: IF (NewTime# - lastTime#) > VAL(Blanq) THEN GOTO Fini1
IF (ri <= .02) THEN
     ai = 0: bi = 0: ci = 0: di = .16: ei = 0: fi = 0
     ELSEIF ri > .01 AND ri <= .86 THEN
          ai = .85: bi = .04: ci = -.04: di = .85: ei = 0: fi = 1.6
     ELSEIF ri > .86 AND ri <= .94 THEN
          ai = .2: bi = -.26: ci = .23: di = .22: ei = 0: fi = 1.6
     ELSE
          ai = -.15: bi = .28: ci = .26: di = .24: ei = 0: fi = .44
END IF
newx = (ai * xi) + (bi * yi) + ei: newy = (ci * xi) + (di * yi) + fi: xi = newx: yi = newy: PSET (xi, yi), (ri * 10)
WEND
Fini3:
SCREEN 0: FancyCls colors(2, colorpref), colors(1, colorpref): Kbd$ = CHR$(27)
RETURN
END FUNCTION

SUB MenuMadre
    DIM choice$(20), menuRow(20), menuCol(20), help$(20): LOCATE , , 0: choice = 1: finished = false
    WHILE NOT finished: GOSUB MenuPrincipal: subchoice = -1
        WHILE subchoice < 0
            SELECT CASE choice
                CASE 1: GOSUB MenuSalir
                CASE 2: GOSUB MenuVentas
                CASE 3: GOSUB MenuCompras
                CASE 4: GOSUB MenuBancos
                CASE 5: GOSUB MenuInventarios
                CASE 6: GOSUB MenuPersonal
                CASE 7: GOSUB MenuConfiguracion
            END SELECT: FancyCls colors(2, colorpref), colors(1, colorpref)
            SELECT CASE subchoice
                CASE -2
                    choice = choice - 1: IF choice = 0 THEN choice = 7
                CASE -3
                    choice = choice + 1: IF choice = 8 THEN choice = 1
                END SELECT
        WEND
    WEND: EXIT SUB
MenuPrincipal:
     FancyCls colors(2, colorpref), colors(1, colorpref)
     COLOR colors(7, colorpref), colors(4, colorpref)
     Box 9, 16, 16, 65: f$ = Trim$(Nombfarma$)
     Center 9, "FARMACIA  " + f$: Center 10, "Use Las Teclas De Direcci¢n Para El Men£"
     hoy$ = MID$(DATE$, 4, 2) + "/" + MID$(DATE$, 1, 2) + "/" + MID$(DATE$, 7, 4)
     Center 11, "Presione Entrar Para Elegir Elemento": Center 12, hoy$
     Center 13, "Jimmy Olano Sayago e-mail ks7000@telcel.net.ve": Center 14, "Tlf. 835.60.07 y 0414.417.30.05": Center 15, "Visite nuestra pagina web:": Center 16, "www.orbita.starmedia.com/~ks7000"
    choice$(1) = " Salir "
    choice$(2) = " Ventas "
    choice$(3) = " Compras "
    choice$(4) = " Bancos "
    choice$(5) = " Inventarios "
    choice$(6) = " Personal "
    choice$(7) = " Configuraci¢n "
    menuRow(1) = 1: menuCol(1) = 2
    menuRow(2) = 1: menuCol(2) = 9
    menuRow(3) = 1: menuCol(3) = 17
    menuRow(4) = 1: menuCol(4) = 26
    menuRow(5) = 1: menuCol(5) = 35
    menuRow(6) = 1: menuCol(6) = 48
    menuRow(7) = 1: menuCol(7) = 58
    help$(1) = "Salir del Administrador"
    help$(2) = "Ventas Contado/Credito/Reportes/Clientes"
    help$(3) = "Compras Contado/Credito/Proveedores"
    help$(4) = "Bancos Dep¢sitos/Retiros/Conciliaci¢n"
    help$(5) = "Inventario Modifica/Crea/Borra/Ordena/Varios"
    help$(6) = "Claves/Datos/Horarios/Nomina"
    help$(7) = "Configura el Programa:Colores/Hora y Fecha/Sonidos/I.V.M./Datos de la Farmacia"
    DO
        NewChoice = Menu((choice), 7, choice$(), menuRow(), menuCol(), help$(), true)
    LOOP WHILE NewChoice = 0
    choice = NewChoice
    RETURN
MenuSalir:
    choice$(1) = " Salir    "
    choice$(2) = " Respaldo "
    menuRow(1) = 3: menuCol(1) = 2
    menuRow(2) = 4: menuCol(2) = 2
    help$(1) = "Salir del Administrador"
    help$(2) = "RESPALDAR y Salir del Administrador"
    subchoice = Menu(1, 2, choice$(), menuRow(), menuCol(), help$(), false)
    SELECT CASE subchoice
        CASE 1, 2
          'OPEN "c:\estacion.dat" FOR INPUT AS #1: INPUT #1, estacion: CLOSE
          'IF VAL(estacion) = 1 THEN
               'OPEN "c:\kevinprg\kevindat\semaforo.dat" FOR RANDOM AS #1 LEN = 1
               'FIELD #1, 1 AS boleto$: max% = LOF(1): FOR k = 2 TO max%: GET #1, k: IF boleto$ = "*" THEN salir = false
               'NEXT k
               'IF salir = false THEN
                    'Box2 10, 10, 20, 70: Center 15, "Se Han Detectado Usuarios Conectados: Desea FORZAR el Cierre"
                    'Center 17, "Pulse 'S' Para Salir , Cualquier otra Tecla Para Continuar": kbd$ = INKEY$: kbd$ = UCASE$(kbd$)
                    'IF kbd$ = "S" THEN salir = true
               'END IF
               'IF salir = true THEN FOR k = 1 TO max%: RSET boleto$ = " ": PUT #1, k: NEXT k
          'ELSE
               'OPEN "c:\kevinprg\kevindat\semaforo.dat" FOR RANDOM AS #1 LEN = 1
               'FIELD #1, 1 AS boleto$: RSET boleto$ = " ": PUT #1, VAL(estacion)
          'END IF
          salir = true: CLOSE : OPEN "c:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #2 LEN = 100: FIELD #2, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS  _
vac$: nursaler = LOF(2) / 100
          pregu% = false
          FOR k = 1 TO nursaler: GET #2, k
               IF cong$ = "*" THEN pregu% = true
          NEXT k
          IF pregu% = true THEN
            Box2 10, 10, 20, 70: Center 15, "Se han detectado facturas congeladas:"
            res% = Verify(16, "¨Desea salir, dejando facturas congeladas")
            IF res% = false THEN salir = false
            'Center 17, "el Proceso de Cierre NO PROCEDERA": salir = false: Espera
          END IF
          IF salir = true THEN
          DO
               Box2 10, 5, 20, 75: Center 15, "SI ESTA TOTALMENTE SEGURO QUE DESEA SALIR,PULSE SOLO LA TECLA -S-"
               Center 17, "Pulse 'S' Para Salir , Pulse 'N' para REGRESAR": Kbd$ = ""
               DO WHILE Kbd$ = "": Kbd$ = INKEY$: Kbd$ = UCASE$(Kbd$): LOOP
               IF Kbd$ = "S" THEN salir = true: EXIT DO
               IF Kbd$ = "N" THEN salir = false: EXIT DO
               FOR l = 50 TO 6000 STEP 100: SOUND l, 1: NEXT l
          LOOP
          END IF
          IF salir = true THEN
               'FOR k = 1 TO max%: RSET ticket$ = " ": PUT #1, k: NEXT k
               IF sonido = "si" THEN MUSICA$ = "MFT110o2P8P8L8GGGL2E-P10P8L8FFFL3D": PLAY MUSICA$
               CLS : Box2 1, 1, 7, 80: Center 2, "Gracias Por Usar Medstorm Version 1.00 Multiusuario"
               Center 4, "+++ El UNICO Programa Hecho Por Profesionales de Farmacia Para Farmacia +++"
               CLOSE : OPEN "c:\kevinprg\kevindat\credito.tkt" FOR RANDOM AS #1 LEN = 2
               FIELD #1, 2 AS ticket$: RSET ticket$ = "si": PUT #1, 1: CLOSE
               OPEN "c:\kevinprg\kevindat\respaldo.dat" FOR RANDOM AS #1 LEN = 2
               FIELD #1, 2 AS ticket$: RSET ticket$ = "si": PUT #1, 1: CLOSE
               IF subchoice = 1 THEN KILL "c:\kevinprg\kevindat\respaldo.dat"
               LOCATE 8, 1: SYSTEM
          END IF
        CASE ELSE
    END SELECT
    RETURN
MenuVentas:     'aqui va KEVIN02.BAS
    choice$(1) = " Ventas al Detal de Contado     "
    choice$(2) = " Devolucion Productos de Contado"
    choice$(3) = " Ventas al Detal a  Credito     "
    choice$(4) = " Devolucion Productos a Credito "
    choice$(5) = " Estado de Cuenta de Cliente    "
    choice$(6) = " Cobro Por Renglones a Cliente  "
    choice$(7) = " Abono a Cuenta de Cliente      "
    choice$(8) = " Cobro de Cuenta de Cliente     "
    choice$(9) = " Modifica/Crea Datos de Cliente "
    choice$(10) = " Ventas Detal Con MAS de 26 Art."
    choice$(11) = " Ventas al Mayor de Contado     "
    choice$(12) = " Presupuestos Escritos          "
    choice$(13) = " Totalizar Ventas del Dia       "
    choice$(14) = " Totalizar Ventas de UN Mes     "
    choice$(15) = " Imprimir Facturas Final del dia"
    choice$(16) = " Sub-Total de Vendedores    -X- "
    choice$(17) = " Total de Vendedores        -Z- "
    choice$(18) = " Visualizacion Ventas de Un Dia "
    choice$(19) = " Visualizacion NOTAS DE CREDITO "
    menuRow(1) = 3: menuCol(1) = 9
    menuRow(2) = 4: menuCol(2) = 9
    menuRow(3) = 5: menuCol(3) = 9
    menuRow(4) = 6: menuCol(4) = 9
    menuRow(5) = 7: menuCol(5) = 9
    menuRow(6) = 8: menuCol(6) = 9
    menuRow(7) = 9: menuCol(7) = 9
    menuRow(8) = 10: menuCol(8) = 9
    menuRow(9) = 11: menuCol(9) = 9
    menuRow(10) = 12: menuCol(10) = 9
    menuRow(11) = 13: menuCol(11) = 9
    menuRow(12) = 14: menuCol(12) = 9
    menuRow(13) = 15: menuCol(13) = 9
    menuRow(14) = 16: menuCol(14) = 9
    menuRow(15) = 17: menuCol(15) = 9
    menuRow(16) = 18: menuCol(16) = 9
    menuRow(17) = 19: menuCol(17) = 9
    menuRow(18) = 20: menuCol(18) = 9
    menuRow(19) = 21: menuCol(19) = 9
    help$(1) = "Ventas al Publico en General : M ximo 26 Articulos,SIN Derecho Credito Fiscal"
    help$(2) = "Devuelve Productos a Farmacia y SE DEVUELVE EL DINERO:Debe Saber la Contrase¤a"
    help$(3) = "Ventas a Clientes Fijos Con Historial de Credito"
    help$(4) = "Devolucion a Farmacia y Se Borra de la Cuenta del Cliente"
    help$(5) = "Visualiza Por Pantalla y/o Impresora La Deuda de UN Cliente"
    help$(6) = "Permite Escoger Varios Articulos Entregados a Credito y Cancelarlos"
    help$(7) = "Se le Hace una Nota de Credito En La Cuenta a UN Cliente"
    help$(8) = "Paga Totalmente la Cta. de un Cliente ( Saldo = 0.00 )"
    help$(9) = "Crea y/o Modifica los datos de UN Cliente"
    help$(10) = "Casos Excepcionales : Si Una Venta Lleva M s de 26 Articulos"
    help$(11) = "CON Derecho a Credito Fiscal:Si Hay Productos Gravables,Especifica el IVA"
    help$(12) = "Guarda En Disco Duro El Presupuesto y Lo Envia a Impresora"
    help$(13) = "Vsualiza por pantalla (e impresora) el total de las ventas de un dia"
    help$(14) = "Totaliza por pantalla (e impresora) el total de las ventas de un mes"
    help$(15) = "Imprime Todas las Facturas que no se imprimieron en el Dia"
    help$(16) = "Sub-Total de Caja de uno o mas Vendedores (opcion -X- de Cajas Registradoras)"
    help$(17) = "BORRA Caja de uno o mas Vendedores(OPCION -Z- de Cajas Registradoras)"
    help$(18) = "Visualizacion de Ventas de Uno o Mas Vendedores"
    help$(19) = "Visualizacion de NOTA DE CREDITO hechas."
    subchoice = Menu(1, 19, choice$(), menuRow(), menuCol(), help$(), false)
    SELECT CASE subchoice
        CASE 1: NotaSalir: CHAIN "c:\kevinprg\kevin02.bas"
        CASE 2
               IF Verif$(12, 20) = "true" THEN CALL NotaSalir: CHAIN "c:\kevinprg\kevin023.bas" ELSE CALL Graba
        CASE 3
          'IF Verif$(12, 20) = "true" THEN
            guia = 1: CALL Bloqueo
            IF guia = 777 THEN CALL NotaSalir: CHAIN "c:\kevinprg\kevin024.bas"
          'END IF
        CASE 4
          IF Verif$(12, 20) = "true" THEN
            guia = 1: CALL Bloqueo
            IF guia = 777 THEN CALL NotaSalir: CHAIN "c:\kevinprg\kevin028.bas"
          END IF
        CASE 5
          IF Verif$(12, 20) = "true" THEN
            guia = 1: CALL Bloqueo
            IF guia = 777 THEN CALL NotaSalir: CHAIN "c:\kevinprg\kevin026.bas"
          END IF
        CASE 6
          IF Verif$(12, 20) = "true" THEN
            guia = 1: CALL Bloqueo
            IF guia = 777 THEN CALL NotaSalir: CHAIN "c:\kevinprg\kevin29.bas"
          END IF
        CASE 7
          IF Verif$(12, 20) = "true" THEN
            guia = 1: CALL Bloqueo
            IF guia = 777 THEN CALL NotaSalir: CHAIN "c:\kevinprg\kevin027.bas"
          END IF
        CASE 8
          IF Verif$(12, 20) = "true" THEN
            guia = 1: CALL Bloqueo
            IF guia = 777 THEN CALL NotaSalir: CHAIN "c:\kevinprg\kevin029.bas"
          END IF
        CASE 9
          IF Verif$(12, 20) = "true" THEN
            guia = 1: CALL Bloqueo
            IF guia = 777 THEN CALL NotaSalir: CHAIN "c:\kevinprg\kevin025.bas"
          END IF
        CASE 13
          IF Verif$(12, 20) = "true" THEN
            guia = 1: NotaSalir: CHAIN "c:\kevinprg\kevin21.bas"
          END IF
        CASE 14
          IF Verif$(12, 20) = "true" THEN NotaSalir: CHAIN "c:\kevinprg\kevin23.bas"
        CASE 15: CALL NotaSalir: CHAIN "c:\kevinprg\kevin22.bas"
        CASE 16: IF Verif$(12, 20) = "true" THEN guia = 1: NotaSalir: CHAIN "c:\kevinprg\kevin022.bas" ELSE CALL Graba
        CASE 17: IF Verif$(12, 20) = "true" THEN guia = 2: NotaSalir: CHAIN "c:\kevinprg\kevin022.bas" ELSE CALL Graba
        CASE 18: CHAIN "c:\kevinprg\kevin021.bas"
        CASE 19: IF Verif$(12, 20) = "true" THEN NotaSalir: CHAIN "c:\kevinprg\vercrd.bas"
        CASE ELSE
    END SELECT
    RETURN
MenuCompras:    'aqui va KEVIN03.BAS
    choice$(1) = " Compras de Contado    "
    choice$(2) = " Compras a  Credito    "
    choice$(3) = " Proveedores c/oferta  "
    choice$(4) = " Hacer Nuevo Reclamo   "
    choice$(5) = " Modificar Reclamo     "
    choice$(6) = " Eliminar Reclamo      "
    choice$(7) = " Imprimir Reclamo      "
    choice$(8) = " Reclamo a N.C. o N.D. "
    choice$(9) = " Conciliacion          "
    choice$(10) = " Visualizar  Facturas  "
    choice$(11) = " Ajuste al Inventario  "
    choice$(12) = " Borrar Pedidos        "
    choice$(13) = " Borra Pedido Medicinas"
    choice$(14) = " Borra Pedido Miscelan."
    choice$(15) = " Borra Pedido Quimicos "
    choice$(16) = " Modifica Proveedores  "
    choice$(17) = " Imprime Solicitud Ped."
    choice$(18) = " Tarjetas Telefonicas  "
    menuRow(1) = 3: menuCol(1) = 17
    menuRow(2) = 4: menuCol(2) = 17
    menuRow(3) = 5: menuCol(3) = 17
    menuRow(4) = 6: menuCol(4) = 17
    menuRow(5) = 7: menuCol(5) = 17
    menuRow(6) = 8: menuCol(6) = 17
    menuRow(7) = 9: menuCol(7) = 17
    menuRow(8) = 10: menuCol(8) = 17
    menuRow(9) = 11: menuCol(9) = 17
    menuRow(10) = 12: menuCol(10) = 17
    menuRow(11) = 13: menuCol(11) = 17
    menuRow(12) = 14: menuCol(12) = 17
    menuRow(13) = 15: menuCol(13) = 17
    menuRow(14) = 16: menuCol(14) = 17
    menuRow(15) = 17: menuCol(15) = 17
    menuRow(16) = 18: menuCol(16) = 17
    menuRow(17) = 19: menuCol(17) = 17
    menuRow(18) = 20: menuCol(18) = 17
    help$(1) = "Compras a Cualquier Empresa/Proveedor/Persona de Contado"
    help$(2) = "Compras a Credito Proveedores Fijos/Droguerias"
    help$(3) = "Configura Los Proveedores Que Ofrecen Ofertas"
    help$(4) = "Hacer Una Nueva Solicitud de Reclamo"
    help$(5) = "Modificar Una Solicitud de Reclamo"
    help$(6) = "Eliminar Una Solicitud de Reclamo"
    help$(7) = "Imprimir Una Solicitud de Reclamo (-X- cantidad de copias)"
    help$(8) = "Convierte Una Solicitud de Reclamo a N.C. o N.D.,Segun el Caso"
    help$(9) = "Conciliacion del Estado de Cuenta de un Proveedor"
    help$(10) = "Consulta y/o Imprime Una Factura Transcrita"
    help$(11) = "Modifica el Inventario por Factura Mal Transcrita,Perdida,Robo,etc."
    help$(12) = "Borra TODAS las Solicitudes de Pedido de Droguerias,Miscelaneas y Quimicos"
    help$(13) = "Borra SOLAMENTE las Solicitudes de Pedido de MEDICINAS"
    help$(14) = "Borra SOLAMENTE las Solicitudes de Pedido de MISCELANEAS"
    help$(15) = "Borra SOLAMENTE las Solicitudes de Pedido de QUIMICOS"
    help$(16) = "Modifica los datos de UN Proveedor :RIF,NIT,codigo de Farmacia,etc."
    help$(17) = "Imprime una solicitud de Pedido hecha previamente"
    help$(18) = "Permite administrar las tarjetas telefonicas"
    subchoice = Menu(1, 18, choice$(), menuRow(), menuCol(), help$(), false)
    SELECT CASE subchoice
        CASE 1
        IF Verif$(12, 20) = "true" THEN
         guia = 2: CALL Bloqueo: IF guia = 777 THEN NotaSalir: guia = 1: CHAIN "c:\kevinprg\kevin032.bas"
        END IF
        CASE 2
        IF Verif$(12, 20) = "true" THEN
         guia = 2: CALL Bloqueo: IF guia = 777 THEN NotaSalir: guia = 2: CHAIN "c:\kevinprg\kevin032.bas"
        END IF
        CASE 3
        IF Verif$(12, 20) = "true" THEN
         guia = 2: CALL Bloqueo: IF guia = 777 THEN NotaSalir: guia = 5: CHAIN "c:\kevinprg\kevin035bas"
        END IF
        CASE 4
        IF Verif$(12, 20) = "true" THEN
         guia = 2: CALL Bloqueo: IF guia = 777 THEN NotaSalir: guia = 1: CHAIN "c:\kevinprg\kevin037bas"
        END IF
        CASE 7
        IF Verif$(12, 20) = "true" THEN
         guia = 2: CALL Bloqueo: IF guia = 777 THEN NotaSalir: guia = 2: CHAIN "c:\kevinprg\kevin037bas"
        END IF
        CASE 10
        IF Verif$(12, 20) = "true" THEN
         guia = 2: CALL Bloqueo: IF guia = 777 THEN NotaSalir: CHAIN "c:\kevinprg\kevin033.bas"
        END IF
        CASE 11: IF Verif$(12, 20) = "true" THEN CHAIN "c:\kevinprg\kevin031.bas" ELSE CALL Graba
        CASE 12
        IF Verif$(12, 20) = "true" THEN
         guia = 2: CALL Bloqueo: IF guia = 777 THEN NotaSalir: guia = 4: CHAIN "c:\kevinprg\kevin035bas"
        END IF
        CASE 13
        IF Verif$(12, 20) = "true" THEN
         guia = 2: CALL Bloqueo: IF guia = 777 THEN NotaSalir: guia = 1: CHAIN "c:\kevinprg\kevin035bas"
        END IF
        CASE 14
        IF Verif$(12, 20) = "true" THEN
         guia = 2: CALL Bloqueo: IF guia = 777 THEN NotaSalir: guia = 2: CHAIN "c:\kevinprg\kevin035bas"
        END IF
        CASE 15
        IF Verif$(12, 20) = "true" THEN
         guia = 2: CALL Bloqueo: IF guia = 777 THEN NotaSalir: guia = 3: CHAIN "c:\kevinprg\kevin035bas"
        END IF
        CASE 16
        IF Verif$(12, 20) = "true" THEN
         guia = 2: CALL Bloqueo: IF guia = 777 THEN CALL NotaSalir: CHAIN "c:\kevinprg\kevin036.bas"
        END IF
        CASE 17
        IF Verif$(12, 20) = "true" THEN
         CALL NotaSalir: CHAIN "c:\kevinprg\kevin039.bas"
        END IF
        CASE 18
        'IF Verif$(12, 20) = "true" THEN
         CALL NotaSalir: CHAIN "c:\kevinprg\tarjetas.bas"
        'END IF
        CASE ELSE
    END SELECT
    RETURN
MenuBancos:     'aqui va KEVIN04.BAS
    choice$(1) = " Bancos       "
    choice$(2) = " Total Inven. "
    menuRow(1) = 3: menuCol(1) = 26
    menuRow(2) = 4: menuCol(2) = 26
    help$(1) = "Llevar contabilidad bancaria"
    help$(2) = "Totaliza Inventario (PVP x Cantidad) : NO debe Haber Existencia en NEGATIVO"
    subchoice = Menu(1, 2, choice$(), menuRow(), menuCol(), help$(), false)
    SELECT CASE subchoice
        CASE 1: IF Verif$(12, 20) = "true" THEN NotaSalir: CHAIN "c:\kevinprg\bancos.bas"
        CASE 2
        IF Verif$(12, 20) = "true" THEN
               CLOSE : OPEN "c:\kevinprg\kevindat\new.dbf" FOR RANDOM SHARED AS #1 LEN = 200
               FIELD #1, 6 AS codigo2$, 2 AS mini12$, 2 AS mini22$, 2 AS maxi12$, 2 AS maxi22$, 2 AS ubic2$, 35 AS nomb2$, 20 AS obser2$, 1 AS acti2$, 2 AS exi12$, 2 AS exi22$, 2 AS exi32$, 2 AS exi42$, 8 AS pv12$, 8 AS pv22$, 8 AS pv32$, 8 AS pv42$ _
, 8 AS cos12$, 8 AS cos22$, 8 AS cos32$, 8 AS cos42$, 1 AS iv12$, 1 AS iv22$, 1 AS iv32$, 1 AS iv42$, 1 AS of12$, 1 AS of22$, 1 AS of32$, 2 AS labo2$, 1 AS re2$, 2 AS ped2$, 1 AS medi2$, 1 AS misc2$, 1 AS quim2$, 2 AS compo12$, 2 AS compo22$, 2 AS  _
compo32$, 2 AS compo42$, 2 AS compo52$, 2 AS compo62$, 1 AS preg$, 2 AS saldo$, 8 AS upc$, 1 AS ven11$, 2 AS ven12$, 1 AS ven21$, 2 AS ven22$, 1 AS ven31$, 2 AS ven32$, 1 AS ven41$, 2 AS ven42$
               CALL NotaSalir: max% = LOF(1) / 200: totpvp$ = MKD$(0): totcos$ = MKD$(0): totuni$ = MKL$(0): nota$ = "El c lculo se realizo SIN problemas"
               FOR k = 1 TO max%
                    LOCK #1, k : GET #1, k
                    IF CVI(exi12$) < 0 OR CVI(exi22$) < 0 OR CVI(exi32$) < 0 THEN
                         nota$ = "HAY EXISTENCIAS EN NEGATIVO : ESTE TOTAL NO ES FIABLE"
                         RSET iv42$ = "*": PUT #1, k
                    END IF
                    totpvp$ = MKD$(CVD(totpvp$) + (CVI(exi12$) * CVD(pv12$)) + (CVI(exi22$) * CVD(pv22$)) + (CVI(exi32$) * CVD(pv32$)) + (CVI(exi42$) * CVD(pv32$)))
                    totcos$ = MKD$(CVD(totcos$) + (CVI(exi12$) * CVD(cos12$)) + (CVI(exi22$) * CVD(cos22$)) + (CVI(exi32$) * CVD(cos32$)) + (CVI(exi42$) * CVD(cos42$)))
                    totuni$ = MKL$(CVL(totuni$) + CVI(exi12$) + CVI(exi22$) + CVI(exi32$) + CVI(exi42$))
                    UNLOCK #1, k
               NEXT k
               Box 5, 10, 20, 70: Center 5, "Totalizacion de Inventario": CLOSE
               Center 7, "Total CANTIDAD*PVP =": Center 10, "Total CANTIDAD*COSTO =": Center 13, "Total GANANCIA NETA DE INVENTARIO =": Center 16, "Total de unidades:"
               LOCATE 8, 30: PRINT USING "**##############,.##"; CVD(totpvp$): LOCATE 11, 30: PRINT USING "**##############,.##"; CVD(totcos$): Center 19, nota$
               LOCATE 14, 30: PRINT USING "**##############,.##"; (CVD(totpvp$) - CVD(totcos$)): Center 25, "Presione Cualquier Tecla Para Continuar"
               LOCATE 17, 30: PRINT USING "**#################,"; CVL(totuni$); : SLEEP
               impr% = Verify(13, "¨Desea un reporte impreso")
               IF impr% = true THEN
                  OPEN "lpt1" FOR OUTPUT AS #10
                  PRINT #10, "Farmacia: "; Nombfarma; " Fecha: "; MID$(DATE$, 4, 2); "/"; MID$(DATE$, 1, 2); "/"; MID$(DATE$, 7, 4)
                  PRINT #10, "Total CANTIDAD*PVP ="
                  PRINT #10, USING "**##############,.##"; CVD(totpvp$)
                  PRINT #10, CHR$(10);
                  PRINT #10, "Total CANTIDAD*COSTO ="
                  PRINT #10, USING "**##############,.##"; CVD(totcos$)
                  PRINT #10, CHR$(10);
                  PRINT #10, "Total GANANCIA BRUTA DE INVENTARIO ="
                  PRINT #10, USING "**##############,.##"; (CVD(totpvp$) - CVD(totcos$))
                  PRINT #10, CHR$(10);
                  PRINT #10, "Total de unidades:"
                  PRINT #10, USING "**#################,"; CVL(totuni$);
                  CLOSE #10
               END IF
        END IF
    END SELECT
    RETURN
MenuInventarios:     'aqui va KEVIN05.BAS
    choice$(1) = " Modificar un Articulo       "
    choice$(2) = " Reparar Articulos Marcados  "
    choice$(3) = " Borrar\Recuperar un Articulo"
    choice$(4) = " Adicionar un Articulo       "
    choice$(5) = " Ordenar Archivo Maestro     "
    choice$(6) = " Stock Minimo y Maximo       "
    choice$(7) = " Listado p/ Hacer Inventario "
    choice$(8) = " Listado de Inventario       "
    choice$(9) = " Relacion de Psicotropicos   "
    choice$(10) = " Seguimiento a Un Articulo   "
    choice$(11) = " Detallar Productos          "
    choice$(12) = " Envasar Productos           "
    choice$(13) = " Introducir Inventario       "
    choice$(14) = " Listado Por Vencer          "
    choice$(15) = " Marcar Fallas de Fallas     "
    choice$(16) = " Marcar Ofertas              "
    choice$(17) = " Borrar Ofertas              "
    choice$(18) = " Pedir Fallas Drogueria      "
    choice$(19) = " Pedir Fallas Miscelaneas    "
    choice$(20) = " Pedir Fallas Quimicos       "
    menuRow(1) = 3: menuCol(1) = 35
    menuRow(2) = 4: menuCol(2) = 35
    menuRow(3) = 5: menuCol(3) = 35
    menuRow(4) = 6: menuCol(4) = 35
    menuRow(5) = 7: menuCol(5) = 35
    menuRow(6) = 8: menuCol(6) = 35
    menuRow(7) = 9: menuCol(7) = 35
    menuRow(8) = 10: menuCol(8) = 35
    menuRow(9) = 11: menuCol(9) = 35
    menuRow(10) = 12: menuCol(10) = 35
    menuRow(11) = 13: menuCol(11) = 35
    menuRow(12) = 14: menuCol(12) = 35
    menuRow(13) = 15: menuCol(13) = 35
    menuRow(14) = 16: menuCol(14) = 35
    menuRow(15) = 17: menuCol(15) = 35
    menuRow(16) = 18: menuCol(16) = 35
    menuRow(17) = 19: menuCol(17) = 35
    menuRow(18) = 20: menuCol(18) = 35
    menuRow(19) = 21: menuCol(19) = 35
    menuRow(20) = 22: menuCol(20) = 35
    help$(1) = "Modifica los Productos del Inventario"
    help$(2) = "Modifica los Productos del Inventario PREVIAMENTE marcados"
    help$(3) = "Borra Productos del Inventario"
    help$(4) = "Adiciona Un Nuevo Producto a La Base de Datos Maestra"
    help$(5) = "Ordena Productos En Orden Alfabetico y Precios De Menor a Mayor"
    help$(6) = "Modifica el Stock Minimo/Maximo de los Articulos por Ubicacion"
    help$(7) = "Imprime Listado para Hacer Inventario(sin PVP's ni existencia)"
    help$(8) = "Imprime Listado con precios y cantidades"
    help$(9) = "Relaciona e Imprime Resultado de Movimiento de Psicotropicos"
    help$(10) = "Visualiza Compras/Ventas/Otros de un Articulo"
    help$(11) = "Mueve Existencia de Dispensadores a Detallados"
    help$(12) = "Mueve Existencia de Quimicos a Envasados"
    help$(13) = "Introduccion de Datos del Inventario"
    help$(14) = "Lista por Pantalla o Impresora Articulos Por Vencer"
    help$(15) = "Marca Articulos Para que Sean Fallas de Fallas"
    help$(16) = "Marca Articulos Para que Sean Ofertas de Una Drogueria"
    help$(17) = "Borra Articulos Que se Han Marcado Como Ofertas"
    help$(18) = "Pedir Fallas de Medicinas a Droguerias"
    help$(19) = "Pedir Fallas de Productos Miscelaneos"
    help$(20) = "Pedir Fallas de Productos Quimicos"
    subchoice = Menu(1, 20, choice$(), menuRow(), menuCol(), help$(), false)
    SELECT CASE subchoice
        CASE 1: IF Verif$(12, 20) = "true" THEN guia = 1: NotaSalir: CHAIN "c:\kevinprg\kevin05.bas" ELSE CALL Graba
        CASE 2: IF Verif$(12, 20) = "true" THEN guia = 2: NotaSalir: CHAIN "c:\kevinprg\kevin05.bas" ELSE CALL Graba
        CASE 3: IF Verif$(12, 20) = "true" THEN guia = 3: NotaSalir: CHAIN "c:\kevinprg\kevin052.bas" ELSE CALL Graba
        CASE 4: IF Verif$(12, 20) = "true" THEN NotaSalir: guia = 1: CHAIN "c:\kevinprg\kevin051.bas" ELSE CALL Graba
        CASE 5: IF Verif$(12, 20) = "true" THEN guia = 1: NotaSalir: CHAIN "c:\kevinprg\kevin053.bas"
        CASE 6: IF Verif$(12, 20) = "true" THEN guia = 1: NotaSalir: CHAIN "c:\kevinprg\kevin054.bas"
        CASE 7: IF Verif$(12, 20) = "true" THEN NotaSalir: guia = 1: CHAIN "c:\kevinprg\kevin055.bas"
        CASE 8: IF Verif$(12, 20) = "true" THEN NotaSalir: guia = 2: CHAIN "c:\kevinprg\kevin055.bas"
        CASE 9: IF Verif$(12, 20) = "true" THEN NotaSalir: CHAIN "c:\kevinprg\kevin057.bas" ELSE CALL Graba
        CASE 10: IF Verif$(12, 20) = "true" THEN NotaSalir: CHAIN "c:\kevinprg\seguimie.bas"
        CASE 11: IF Verif$(12, 20) = "true" THEN guia = 1: NotaSalir: CHAIN "c:\kevinprg\kevin052.bas"
        CASE 12: IF Verif$(12, 20) = "true" THEN guia = 2: NotaSalir: CHAIN "c:\kevinprg\kevin052.bas"
        CASE 13: IF Verif$(12, 20) = "true" THEN guia = 2: NotaSalir: CHAIN "c:\kevinprg\kevin054.bas" ELSE CALL Graba
        CASE 14: IF Verif$(12, 20) = "true" THEN guia = 7: NotaSalir: CHAIN "c:\kevinprg\kevin055.bas"
        CASE 16: IF Verif$(12, 20) = "true" THEN guia = 1: NotaSalir: CHAIN "c:\kevinprg\kevin52.bas"
        CASE 17: IF Verif$(12, 20) = "true" THEN guia = 2: NotaSalir: CHAIN "c:\kevinprg\kevin52.bas"
        CASE 18: IF Verif$(12, 20) = "true" THEN guia = 1: NotaSalir: CHAIN "c:\kevinprg\kevin059.bas"
        CASE 19: IF Verif$(12, 20) = "true" THEN guia = 2: NotaSalir: CHAIN "c:\kevinprg\kevin059.bas"
        CASE 20: IF Verif$(12, 20) = "true" THEN guia = 3: NotaSalir: CHAIN "c:\kevinprg\kevin059.bas"
    END SELECT
    RETURN
MenuPersonal:      'aqui va KEVIN06.BAS
    choice$(1) = " Cambiar Claves    "
    choice$(2) = " Adiciona Vendedor "
    choice$(3) = " Modifica Vendedor "
    choice$(4) = " Lista Vendedores  "
    choice$(5) = " Imprime Horarios  "
    choice$(6) = " Horarios          "
    choice$(7) = " Borrar Vendedores "
    choice$(8) = " Contrase¤a        "
    menuRow(1) = 3: menuCol(1) = 48
    menuRow(2) = 4: menuCol(2) = 48
    menuRow(3) = 5: menuCol(3) = 48
    menuRow(4) = 6: menuCol(4) = 48
    menuRow(5) = 7: menuCol(5) = 48
    menuRow(6) = 8: menuCol(6) = 48
    menuRow(7) = 9: menuCol(7) = 48
    menuRow(8) = 10: menuCol(8) = 48
    help$(1) = "Modifica las Claves de los Vendedores"
    help$(2) = "Adiciona un Nuevo Vendedor"
    help$(3) = "Modifica los Datos de un Vendedor"
    help$(4) = "Lista los Datos de Todo el Personal"
    help$(5) = "Imprime un Listado de Entradas/Salidas del Personal"
    help$(6) = "Control de Entradas/Salida del Personal"
    help$(7) = "Calculo del Salario de los Vendedores"
    help$(8) = "Activa/Desactiva Contrase¤a de Acceso a las Utilidades" + contri
    subchoice = Menu(1, 8, choice$(), menuRow(), menuCol(), help$(), false)
    SELECT CASE subchoice
        CASE 1, 2, 3, 4, 6, 7, 8
        IF Verif$(12, 20) = "true" THEN
            guia = subchoice: NotaSalir: CHAIN "c:\kevinprg\kevin06.bas"
        END IF
    END SELECT
    RETURN
MenuConfiguracion:     'aqui va KEVIN07.BAS
    choice$(1) = " Monocolor       "
    choice$(2) = " Verde           "
    choice$(3) = " Azul            "
    choice$(4) = " Rojo            "
    choice$(5) = " Sonido          "
    choice$(6) = " Fecha/Hora      "
    choice$(7) = " Impuesto        "
    choice$(8) = " Datos Farmacia  "
    choice$(9) = " Stock 1 o 2     "
    choice$(10) = " Marcar Vencim.  "
    choice$(11) = " Tama¤o de Letra "
    choice$(12) = " Largo de Hoja  "
    choice$(13) = " Tiempo Limpiar "
    choice$(14) = " Mensajes Fact. "
    choice$(15) = " Redondeo Fact. "
    choice$(16) = " Desc. en Fact. "
    choice$(17) = " Max. Descuento "
    choice$(18) = " Exactitud Inv. "
    choice$(19) = " Negativo Exis. "
    menuRow(1) = 3: menuCol(1) = 58
    menuRow(2) = 4: menuCol(2) = 58
    menuRow(3) = 5: menuCol(3) = 58
    menuRow(4) = 6: menuCol(4) = 58
    menuRow(5) = 7: menuCol(5) = 58
    menuRow(6) = 8: menuCol(6) = 58
    menuRow(7) = 9: menuCol(7) = 58
    menuRow(8) = 10: menuCol(8) = 58
    menuRow(9) = 11: menuCol(9) = 58
    menuRow(10) = 12: menuCol(10) = 58
    menuRow(11) = 13: menuCol(11) = 58
    menuRow(12) = 14: menuCol(12) = 58
    menuRow(13) = 15: menuCol(13) = 58
    menuRow(14) = 16: menuCol(14) = 58
    menuRow(15) = 17: menuCol(15) = 58
    menuRow(16) = 18: menuCol(16) = 58
    menuRow(17) = 19: menuCol(17) = 58
    menuRow(18) = 20: menuCol(18) = 58
    menuRow(19) = 21: menuCol(19) = 58
    help$(1) = "Color para monitores monocromaticos y LCD"
    help$(2) = "Color verde"
    help$(3) = "Color azul"
    help$(4) = "Color rojo"
    help$(5) = "Encender/Apagar Sonidos de Verificacion , Estado:" + sonido
    help$(6) = "Cambiar Fecha y/o Hora al Computador"
    help$(7) = "Cambiar Tasa de Impuesto de Ventas al Mayor:" + impuesto + " %": help$(8) = "Cambiar Datos de Farmacia:Direccion,RIF,NIT,Telefonos,Grupo"
    help$(8) = "Cambiar Los Datos de La Farmacia"
    help$(9) = "Cambiar Stocks de Existencias de Trabajo: " + Stock + " valor actual"
    help$(10) = "Marca Articulos a los cuales se les Vigilara el Vencimiento"
    help$(11) = "Por Impresora , Condensado o Normal. Opcion Actual: " + TipoL
    help$(12) = "Numero de Lineas por Hoja. Valor Actual:" + LargoH
    help$(13) = "Tiempo en Segundos Para el Blanqueador de Pantalla. Valor Actual:" + Blanq
    help$(14) = "Mensajes que se Imprimiran en las Facturas de Venta"
    help$(15) = "Redondear el Neto de Facturas a Multiplos de " + Redondeo
    help$(16) = "Descuento Que se Dara en TODAS las Facturas de Venta:" + Desc + "%"
    help$(17) = "Maximo Descuento Que se Dara en Facturas de Venta:" + DescMax + "%"
    help$(18) = "Porcentaje de Exactitud en Costo de Inventario:" + Exactitud + "%"
    help$(19) = "Trabaja Con Existencias en Negativo en el Inventario , Estado:" + negativo
    subchoice = Menu(1, 19, choice$(), menuRow(), menuCol(), help$(), false)
    SELECT CASE subchoice
        CASE 1: IF Verif$(12, 20) = "true" THEN colorpref = 1: SalvaEstado
        CASE 2: IF Verif$(12, 20) = "true" THEN colorpref = 2: SalvaEstado
        CASE 3: IF Verif$(12, 20) = "true" THEN colorpref = 3: SalvaEstado
        CASE 4: IF Verif$(12, 20) = "true" THEN colorpref = 4: SalvaEstado
        CASE 6, 7, 8, 12, 13, 14, 15, 16, 17, 18: IF Verif$(12, 20) = "true" THEN guia = subchoice: NotaSalir: CHAIN "c:\kevinprg\kevin07.bas" ELSE CALL Graba
        CASE 5
         IF Verif$(12, 20) = "true" THEN
            IF sonido = "si" THEN sonido = "no" ELSE sonido = "si"
            CALL SalvaEstado
         END IF
        CASE 9
         IF Verif$(12, 20) = "true" THEN
            IF Stock = "1" THEN Stock = "2" ELSE Stock = "1"
            CALL SalvaEstado
         END IF
        CASE 10: IF Verif$(12, 20) = "true" THEN NotaSalir: CHAIN "c:\kevinprg\kevin072.bas"
        CASE 11
         IF Verif$(12, 20) = "true" THEN
            IF TipoL = "C" THEN TipoL = "N" ELSE TipoL = "C"
            CALL SalvaEstado
         END IF
        CASE 19
         IF Verif$(12, 20) = "true" THEN
            IF negativo = "si" THEN negativo = "no" ELSE negativo = "si"
            CALL SalvaEstado
         END IF
    END SELECT: RETURN
END SUB

SUB NotaSalir
COLOR colors(7, colorpref) + 16, colors(4, colorpref): Box 10, 19, 12, 62: Center 11, "Leyendo Datos. Espere Por Favor ... ": COLOR colors(7, colorpref), colors(4, colorpref)
END SUB

SUB PrintHelpLine (help$)
COLOR colors(5, colorpref), colors(4, colorpref): Center 25, help$
END SUB

SUB SalvaEstado
OPEN "c:\turno.dat" FOR OUTPUT SHARED AS #7
PRINT #7, USING "#"; colorpref
PRINT #7, turno
PRINT #7, USING "##"; dia1
PRINT #7, USING "##"; mes1
PRINT #7, USING "####"; ano1
CLOSE #7
OPEN "c:\kevinprg\kevindat\kaos.dat" FOR OUTPUT SHARED AS #2
PRINT #2, sonido: PRINT #2, impuesto
PRINT #2, Nombfarma: PRINT #2, Direccion
PRINT #2, Rif: PRINT #2, Nit
PRINT #2, Telefono: PRINT #2, contri
PRINT #2, contra: PRINT #2, Stock
PRINT #2, Blanq: PRINT #2, LargoH
PRINT #2, TipoL: PRINT #2, Mensaje1
PRINT #2, Mensaje2: PRINT #2, Mensaje3
PRINT #2, Redondeo: PRINT #2, Desc
PRINT #2, DescMax: PRINT #2, Exactitud
PRINT #2, Prov1: PRINT #2, Prov2
PRINT #2, Prov3: PRINT #2, negativo
PRINT #2, NuevoIva: PRINT #2, ShowPerc
CLOSE
END SUB

SUB ScrollDown
'DEF SEG = VARSEG(ScrollDownAsm(1)): CALL Absolute(VARPTR(ScrollDownAsm(1))): DEF SEG
END SUB

SUB ScrollUp
'DEF SEG = VARSEG(ScrollUpAsm(1)): CALL Absolute(VARPTR(ScrollUpAsm(1))): DEF SEG
END SUB

FUNCTION Trim$ (x$)
IF x$ = "" THEN
     Trim$ = ""
ELSE
lastChar = 0
FOR A = 1 TO LEN(x$): y$ = MID$(x$, A, 1): IF y$ <> CHR$(0) AND y$ <> " " THEN lastChar = A
NEXT A: Trim$ = LEFT$(x$, lastChar)
END IF
END FUNCTION

FUNCTION Verif$ (fila%, columna%)
COLOR colors(7, colorpref), colors(4, colorpref): Box 11, 19, 13, 60
COLOR colors(4, colorpref), colors(7, colorpref)
llevalargo = 0: eleccion = "": LOCATE fila%, columna%: PRINT CHR$(168); "Clave?"; CHR$(178); SPACE$(LEN(Trim$(contra))); CHR$(178)
DO WHILE llevalargo < LEN(Trim$(contra))
   llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
   IF llave$ = CHR$(27) THEN Verif$ = "false": EXIT FUNCTION
   IF llave$ = CHR$(8) THEN
      llevalargo = llevalargo - 1: IF llevalargo < 1 THEN llevalargo = 0
      LOCATE fila%, columna% + llevalargo + 8: PRINT " ";
      IF LEN(eleccion) > 1 THEN
         eleccion = MID$(eleccion, 1, LEN(eleccion) - 1)
      ELSE
         eleccion = ""
      END IF
   ELSE
      llevalargo = llevalargo + 1
      LOCATE fila%, columna% + llevalargo + 7: PRINT CHR$(176);
      eleccion = eleccion + llave$
   END IF
LOOP
IF sonido = "si" THEN BEEP
aux7$ = Trim$(contra): aux7$ = UCASE$(aux7$)
IF UCASE$(eleccion) = aux7$ THEN Verif$ = "true" ELSE Verif$ = "false"
EXIT FUNCTION
END FUNCTION

