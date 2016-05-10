DEFINT A-Z
DECLARE SUB AdicionaSaler ()
DECLARE SUB Box (Row1, Col1, Row2, Col2)
DECLARE SUB Box2 (Row1, Col1, Row2, Col2)
DECLARE SUB Center (row, text$)
DECLARE SUB Borrar ()
DECLARE SUB Claves ()
DECLARE SUB ModificaSaler ()
DECLARE SUB ListaSaler ()
DECLARE SUB Universal ()
DECLARE SUB ImprHorario ()
DECLARE SUB Horario ()
DECLARE SUB SalvaEstado ()
DECLARE SUB LeeEstado ()
DECLARE SUB Espera ()
DECLARE FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$)
DECLARE FUNCTION Verif$ (fila%, columna)
DECLARE FUNCTION Trim$ (x$)
COMMON SHARED guia, colorpref AS INTEGER
DIM SHARED nursaler
DIM SHARED colors(0 TO 20, 1 TO 4) AS INTEGER: RESTORE: FOR Ct = 1 TO 4: FOR x = 1 TO 10: READ colors(x, Ct): NEXT x: NEXT Ct
DIM SHARED sonido  AS STRING * 2, impuesto AS STRING * 6, Nombfarma AS STRING * 25, Direccion AS STRING * 30, Rif     AS STRING * 25, Nit      AS STRING * 25, Telefono AS STRING * 25, contri AS STRING * 2, contra AS STRING * 15
DIM SHARED Stock   AS STRING * 1, Blanq    AS STRING * 3, LargoH    AS STRING * 2, TipoL      AS STRING * 1, Mensaje1 AS STRING * 60, Mensaje2 AS STRING * 60, Mensaje3 AS STRING * 60, Redondeo AS STRING * 2
DIM SHARED DescMax AS STRING * 2, Desc AS STRING * 2, Prov1 AS STRING * 15, Prov2 AS STRING * 15, Prov3 AS STRING * 15, negativo AS STRING * 2, NuevoIva AS STRING * 2, ShowPerc AS STRING * 2
DIM SHARED PrintErr AS INTEGER, devolver, Fun(12) AS STRING * 2, salida
DIM SHARED Nombres AS STRING * 20, Apellidos AS STRING * 20
CONST true = -1: CONST false = NOT true
CALL LeeEstado
SELECT CASE guia
     CASE 1: Claves
     CASE 2: AdicionaSaler
     CASE 3: ModificaSaler
     CASE 4: ListaSaler
     CASE 5: ImprHorario
     CASE 6: Horario
     CASE 7: Borrar
     CASE 8: Universal
     CASE ELSE
END SELECT
CHAIN "F:\kevinprg\kevin01.bas"
ErrorTrap:
    SELECT CASE ERR
        CASE 9
            Box 7, 20, 9, 60: Center 8, "No se pueden Adicionar Articulos Hasta Reiniciar el Sistema"
            SOUND 800, 2: Center 25, "No se pueden crear Mas Articulos Hasta Reiniciar el Sistema":  Fun(11) = "si"
            RESUME NEXT
        CASE 24, 25
            SOUND 600, 2
            Center 24, "La impresora no responde ... (revise si esta encendida,on-line o falta papel)"
            Center 25, "Presione [Esc] para anular o cualquier otra tecla para continuar"
            DO WHILE INKEY$ = "": LOOP
            LOCATE 24, 1: PRINT SPACE$(80): LOCATE 25, 1: PRINT SPACE$(80)
            IF INKEY$ = CHR$(27) THEN RESUME NEXT ELSE RESUME
        CASE ELSE
    END SELECT
    RESUME NEXT
END
DATA 0,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  2,     0,     1,     15,     0,     7,      0
DATA 3,    15,   13,  1,     14,    3,     15,     0,     7,      0
DATA 7,    12,   15,  4,     14,    0,     15,     15,    1,      0

SUB AdicionaSaler
COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box 5, 5, 22, 75: COLOR colors(4, colorpref), colors(7, colorpref): Center 6, "Introduccion de Datos de un Nuevo Vendedor."
COLOR colors(7, colorpref), colors(4, colorpref): LOCATE 8, 22: PRINT "Clave Corta a Usar:": LOCATE 10, 23: PRINT "Nombres Completos:"
LOCATE 11, 21: PRINT "Apellidos Completos:": LOCATE 12, 24: PRINT "Numero de Cedula:"
LOCATE 13, 11: PRINT "Cargo a Ocupar en la Farmacia:": LOCATE 14, 17: PRINT "Clave Larga (Iniciales):": LOCATE 15, 10: PRINT "Gaveta a Abrir 2 o 1 Solamente:"
LOCATE 16, 10: PRINT "Numero de M.S.A.S.            :": LOCATE 17, 10: PRINT "Numero de COL.FAR.            :": LOCATE 18, 10: PRINT "Numero de IN.PRE.FAR.         :"
Center 25, " [F1] Ayuda [F9]Abortar Proceso ": COLOR colors(7, colorpref), colors(4, colorpref): LOCATE 8, 6: ClaveCorta$ = "   "
DO WHILE ClaveCorta$ = "   "
LOCATE 8, 6: PRINT SPACE$(15); "Clave Corta a Usar:"; SPC(33); : ClaveCorta$ = MeteDatoLetra(8, 41, 3, "   ")
IF Fun(9) = "si" THEN EXIT SUB
LOOP
IF Fun(10) = "si" THEN Fun(10) = "no": EXIT SUB
CLOSE : OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM AS #1 LEN = 100
FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 4 AS msas$, 4 AS colfar$, 4 AS inprefar$, 7 AS vac$
nursaler = LOF(1) / 100
FOR k = 1 TO nursaler: GET #1, k
IF ClaveCorta$ = clco$ THEN Center 8, "Esta Clave ya Esta en USO. Introduzca una nueva Clave.": SLEEP 3: CLOSE : EXIT SUB
NEXT k: CLOSE : pepsi% = 8: Msas2$ = "0": Colfar2$ = "0": Inprefar2$ = "0"
DO
SELECT CASE pepsi%
     CASE 8
          Nombres = MeteDatoLetra(10, 41, 20, Nombres)
          IF devolver = true THEN devolver = false: pepsi% = 8 ELSE pepsi% = 9
          IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DO
     CASE 9
          Apellidos = MeteDatoLetra(11, 41, 20, Apellidos)
          IF devolver = true THEN devolver = false: pepsi% = 8 ELSE pepsi% = 10
          IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DO
     CASE 10
          Cedula$ = MeteDatoLetra(12, 41, 9, Cedula$):
          IF devolver = true THEN devolver = false: pepsi% = 9 ELSE pepsi% = 11
          IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DO
     CASE 11
          Cargo$ = MeteDatoLetra(13, 41, 20, Cargo$)
          IF devolver = true THEN devolver = false: pepsi% = 10 ELSE pepsi% = 12
          IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DO
     CASE 12
          ClaveLarga$ = MeteDatoLetra(14, 41, 10, ClaveLarga$)
          IF devolver = true THEN devolver = false: pepsi% = 11 ELSE pepsi% = 13
          IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DO
     CASE 13
          Gav$ = MeteDatoLetra(15, 41, 1, Gav$)
          IF devolver = true THEN devolver = false: pepsi% = 12 ELSE pepsi% = 14
          IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DO
     CASE 14
          Msas2$ = MeteDatoLetra(16, 41, 8, Msas2$)
          IF devolver = true THEN devolver = false: pepsi% = 13 ELSE pepsi% = 15
          IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DO
     CASE 15
          Colfar2$ = MeteDatoLetra(17, 41, 8, Colfar2$)
          IF devolver = true THEN devolver = false: pepsi% = 14 ELSE pepsi% = 16
          IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DO
     CASE 16
          Inprefar2$ = MeteDatoLetra(18, 41, 8, Inprefar2$)
          IF devolver = true THEN devolver = false: pepsi% = 15 ELSE pepsi% = 18
          IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DO
     CASE 18
          Center 20, "         Salvando la Informacion...        "
          CLOSE : OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM AS #1 LEN = 100
          FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 4 AS msas$, 4 AS colfar$, 4 AS inprefar$, 7 AS vac$
          nursaler = LOF(1) / 100: nursaler = nursaler + 1
          LSET acti$ = " ": LSET nomb$ = Nombres: LSET apel$ = Apellidos: LSET cedu$ = MKL$(VAL(Cedula$)): LSET carg$ = Cargo$: LSET clco$ = ClaveCorta$: LSET clla$ = ClaveLarga$: LSET inout$ = " ": LSET gaveta$ = Gav$
          RSET msas$ = MKL$(VAL(Msas2$)): RSET inprefar$ = MKL$(VAL(Inprefar2$)): RSET colfar$ = MKL$(VAL(Colfar2$))
          PUT #1, nursaler: CLOSE
          kontrol$ = "F:\kevinprg\kevindat\" + MID$(STR$(VAL(Cedula$)), 2, LEN(STR$(VAL(Cedula$)))) + ".dat"
          g$ = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4) + MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MID$(TIME$, 7, 2) + "I"
          OPEN kontrol$ FOR RANDOM AS #1 LEN = 15: FIELD #1, 15 AS regis$
          Reg% = (LOF(1) / 15) + 1: LSET regis$ = g$: PUT #1, Reg%: CLOSE : EXIT SUB
          kontrol$ = "F:\kevinprg\kevindat\" + MID$(STR$(VAL(Cedula$)), 2, LEN(STR$(VAL(Cedula$)))) + ".caj"
          OPEN kontrol$ FOR RANDOM AS #1 LEN = 8: FIELD #1, 8 AS regis$: RSET regis$ = MKD$(0): PUT #1, 1: CLOSE
     END SELECT
LOOP
CLOSE
END SUB

SUB Borrar
COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box2 5, 5, 20, 75: Center 6, "² Borrado de UN Vendedor ²": Center 25, "[F1] Ayuda [Escape] Salir SIN Salvar": ClaveCorta$ = "   "
DO WHILE ClaveCorta$ = "   "
LOCATE 8, 20: PRINT "Clave Corta en Uso:"; : ClaveCorta$ = MeteDatoLetra(8, 41, 3, "   ")
IF salida = true THEN salida = false: EXIT SUB
LOOP
IF Fun(10) = "si" THEN Fun(10) = "no": EXIT SUB
CLOSE : OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM AS #1 LEN = 100
FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
nursaler = LOF(1) / 100: eleg = 0: IF nursaler = 0 THEN Box2 10, 5, 12, 75: Center 11, "No hay Vendedores Registrados": Espera: CLOSE : EXIT SUB
FOR k = 1 TO nursaler: GET #1, k: IF ClaveCorta$ = clco$ AND acti$ = " " THEN eleg% = k: EXIT FOR
NEXT k: IF eleg% = 0 THEN Center 8, "No Hay Ningun Vendedor Con Esa Clave.Pruebe Otra Vez.": SLEEP 3: CLOSE : EXIT SUB
GET #1, eleg%: Nombres = nomb$: Apellidos = apel$
Center 9, "Borrando el Vendedor:": Center 10, Trim$(Nombres) + " " + Trim(Apellidos)
Box 19, 10, 21, 50: LOCATE 20, 13: PRINT "Es Este el Vendedor a Borrar?"
dec$ = MeteDatoLetra(20, 45, 1, "")
IF sonido = "si" THEN BEEP
IF dec$ = "s" OR dec$ = "S" THEN
LSET acti$ = "*": PUT #1, eleg%
END IF: CLOSE
END SUB

SUB Box (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "Ú"; STRING$(BoxWidth - 2, "Ä"); "¿"; : FOR a = Row1 + 1 TO Row2 - 1: LOCATE a, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Box2 (Row1, Col1, Row2, Col2) STATIC
BoxWidth = Col2 - Col1 + 1: LOCATE Row1, Col1: PRINT "²"; STRING$(BoxWidth - 2, "²"); "²"; : FOR a = Row1 + 1 TO Row2 - 1: LOCATE a, Col1: PRINT "³"; SPACE$(BoxWidth - 2); "³"; : NEXT a: LOCATE Row2, Col1: PRINT "À"; STRING$(BoxWidth - 2, "Ä"); "Ù"; _

END SUB

SUB Center (row, text$)
IF row = 25 THEN COLOR 0, 15: LOCATE 25, 1: PRINT SPACE$(80);
LOCATE row, 41 - LEN(text$) / 2: PRINT text$;
END SUB

SUB Claves
COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box2 5, 5, 20, 75: Center 6, "² Cambio de Clave de Un Vendedor ²": Center 25, "[F1] Ayuda [Escape] Salir SIN Salvar": ClaveCorta$ = "   "
DO WHILE ClaveCorta$ = "   "
LOCATE 8, 20: PRINT "Clave Corta en Uso:"; : ClaveCorta$ = MeteDatoLetra(8, 41, 3, "   ")
IF salida = true THEN salida = false: EXIT SUB
LOOP
IF Fun(10) = "si" THEN Fun(10) = "no": EXIT SUB
CLOSE : OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM AS #1 LEN = 100
FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
nursaler = LOF(1) / 100: eleg = 0: IF nursaler = 0 THEN Box2 10, 5, 12, 75: Center 11, "No hay Vendedores Registrados": Espera: CLOSE : EXIT SUB
FOR k = 1 TO nursaler: GET #1, k
IF ClaveCorta$ = clco$ AND acti$ = " " THEN eleg% = k: EXIT FOR
NEXT k
IF eleg% = 0 THEN Center 8, "No Hay Ningun Vendedor Con Esa Clave.Pruebe Otra Vez.": SLEEP 3: CLOSE : EXIT SUB
GET #1, eleg%: activo$ = acti$: Nombres = nomb$: Apellidos = apel$: Cedula$ = cedu$: Cargo$ = carg$
ClaveCorta$ = clco$: ClaveLarga$ = clla$: ClaveCorta$ = "   "
Center 9, "Cambiando la Clave Corta al Vendedor ": Center 10, Trim$(Nombres) + " " + Trim(Apellidos)
DO WHILE ClaveCorta$ = "   "
LOCATE 12, 15: PRINT "Nueva Clave Corta a Usar:"; : ClaveCorta$ = MeteDatoLetra(12, 41, 3, "   ")
IF salida = true THEN salida = false: CLOSE : EXIT SUB
LOOP
FOR k = 1 TO nursaler: GET #1, k
IF ClaveCorta$ = clco$ THEN Center 15, "Esta Clave ya Esta en USO. Introduzca una nueva Clave.": Espera: CLOSE : EXIT SUB
NEXT k
LSET acti$ = activo$: LSET nomb$ = Nombres: LSET apel$ = Apellidos: LSET cedu$ = Cedula$: LSET carg$ = Cargo$: LSET clco$ = ClaveCorta$: LSET clla$ = ClaveLarga$
PUT #1, eleg%: CLOSE
END SUB

SUB Espera
Center 25, "Presione Cualquier Tecla Para Continuar...": DO: LOOP WHILE INKEY$ = ""
END SUB

SUB Horario
COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box2 5, 5, 22, 75: COLOR 0, 15: Center 6, "Control de Entrada y Salida del Personal."
COLOR 15, 0: LOCATE 12, 17: PRINT "Clave Corta del Vendedor:": Busca$ = MeteDatoLetra(12, 45, 3, "   ")
IF Busca$ = "   " THEN EXIT SUB
CLOSE : OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM AS #1 LEN = 100
FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
nursaler = LOF(1) / 100: IF nursaler = 0 THEN Box2 10, 5, 12, 75: Center 11, "No hay Vendedores Registrados": Espera: CLOSE : EXIT SUB
FOR k = 1 TO nursaler: GET #1, k
IF Busca$ = clco$ THEN eleg% = k: EXIT FOR
NEXT k
IF eleg% = 0 THEN Center 12, "No Hay Ningun Vendedor Con Esa Clave.Pruebe Otra Vez.": SLEEP 3: EXIT SUB
GET #1, eleg%: Busca$ = MID$(STR$(CVL(cedu$)), 2, LEN(STR$(CVL(cedu$))))
IF acti$ = "*" THEN COLOR colors(7, colorpref) + 16, colors(4, colorpref): Center 17, "*** ESTE VENDEDOR ESTA BORRADO ***": COLOR colors(7, colorpref), colors(4, colorpref): Espera: CLOSE : EXIT SUB
IF inout$ = " " THEN
     LSET inout$ = "*": entrasale$ = "O": Box 15, 20, 17, 60: Center 16, "Clave Desactivada":
ELSE
     LSET inout$ = " ": entrasale$ = "I": Box 15, 20, 17, 60: Center 16, "Clave Activada"
END IF
PUT #1, eleg%: CLOSE
F$ = "F:\kevinprg\kevindat\" + Busca$ + ".dat": g$ = MID$(DATE$, 4, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 7, 4) + MID$(TIME$, 1, 2) + MID$(TIME$, 4, 2) + MID$(TIME$, 7, 2) + entrasale$
OPEN F$ FOR RANDOM AS #1 LEN = 15: FIELD #1, 15 AS regis$
Reg% = (LOF(1) / 15) + 1: LSET regis$ = g$: PUT #1, Reg%: CLOSE
END SUB

SUB ImprHorario
COLOR colors(7, colorpref), colors(4, colorpref): Box 5, 5, 22, 75: COLOR colors(4, colorpref), colors(7, colorpref)
Center 6, "Imprimir Datos de Entrada y Salida de Un Vendedor.": COLOR colors(4, colorpref), colors(7, colorpref)
LOCATE 12, 17: PRINT "Clave Corta del Vendedor:"
Busca$ = MeteDatoLetra(12, 41, 3, "   "): IF Busca$ = "   " THEN EXIT SUB
CLOSE : OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM AS #1 LEN = 100
FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 19 AS vac$
nursaler = LOF(1) / 100: IF nursaler = 0 THEN Box2 10, 5, 12, 75: Center 11, "No hay Vendedores Registrados": Espera: CLOSE : EXIT SUB
FOR k = 1 TO nursaler: GET #1, k
IF Busca$ = clco$ THEN eleg% = k: EXIT FOR
NEXT k: CLOSE
IF eleg% = 0 THEN Center 12, "No Hay Ningun Vendedor Con Esa Clave.Pruebe Otra Vez.": SLEEP 3: EXIT SUB
GET #1, eleg%: Nombres = nomb$: Apellidos = apel$: Busca$ = MID$(STR$(VAL(Cedula$)), 2, LEN(STR$(VAL(Cedula$)))): CLOSE
Center 18, "Leyendo Archivos e Imprimiendo Datos. Espere Por favor..."
g$ = "F:\kevinprg\kevindat\" + Busca$ + ".dat": FOR k = 1 TO 80: h$ = h$ + CHR$(176): NEXT k
ON ERROR GOTO ErrorTrap
OPEN g$ FOR RANDOM AS #4 LEN = 15: FIELD #4, 15 AS r$: top% = 1: s = 1
OPEN "LPT1:" FOR RANDOM AS #10: WIDTH #10, 255
PRINT #10, CHR$(27); "C"; CHR$(0); CHR$(11)
PRINT #10, h$; CHR$(10); : PRINT #10, CHR$(14); "Horarios de Entrada y Salida del Vendedor"; CHR$(10);
PRINT #10, CHR$(14); Trim$(Nombres); " "; Trim$(Apellidos); CHR$(10); : PRINT #10, CHR$(20);
DO UNTIL (EOF(4))
inf$ = "": GET #4, top%: inf$ = "    {" + MID$(r$, 1, 2) + "/" + MID$(r$, 3, 2) + "/" + MID$(r$, 5, 4) + "-" + MID$(r$, 9, 2) + ":" + MID$(r$, 11, 2) + ":" + MID$(r$, 13, 2) + "}     "
GET #4, (top% + 1): inf$ = "{" + MID$(r$, 1, 2) + "/" + MID$(r$, 3, 2) + "/" + MID$(r$, 5, 4) + "-" + MID$(r$, 9, 2) + ":" + MID$(r$, 11, 2) + ":" + MID$(r$, 13, 2) + "}     "
GET #4, (top% + 2): inf$ = "{" + MID$(r$, 1, 2) + "/" + MID$(r$, 3, 2) + "/" + MID$(r$, 5, 4) + "-" + MID$(r$, 9, 2) + ":" + MID$(r$, 11, 2) + ":" + MID$(r$, 13, 2) + "}"
PRINT #10, inf$; CHR$(10); : s = s + 1: top% = top% + 3
IF s > 50 THEN PRINT #10, h$; CHR$(10); : PRINT #10, CHR$(12); : PRINT #10, CHR$(14); "Horarios de Entrada y Salida del Vendedor"; CHR$(10); : PRINT #10, CHR$(14); Trim$(Nombres); " "; Trim$(Apellidos); CHR$(10); : PRINT #10, CHR$(20); : s = 1
LOOP
PRINT #10, h$; CHR$(10); : PRINT #10, CHR$(12); : PRINT #10, CHR$(27); "<"; : CLOSE #4: CLOSE #10
ON ERROR GOTO 0
CLOSE
END SUB

SUB LeeEstado
CLOSE : OPEN "f:\kevinprg\kevindat\kaos.dat" FOR INPUT AS #2
INPUT #2, sonido: INPUT #2, impuesto: INPUT #2, Nombfarma: INPUT #2, Direccion
INPUT #2, Rif: INPUT #2, Nit: INPUT #2, Telefono: INPUT #2, contri
INPUT #2, contra: INPUT #2, Stock: INPUT #2, Blanq: INPUT #2, LargoH
INPUT #2, TipoL: INPUT #2, Mensaje1: INPUT #2, Mensaje2: INPUT #2, Mensaje3
INPUT #2, Redondeo: INPUT #2, Desc: INPUT #2, DescMax: INPUT #2, Exactitud
INPUT #2, Prov1: INPUT #2, Prov2: INPUT #2, Prov3: INPUT #2, negativo
INPUT #2, NuevoIva: INPUT #2, ShowPerc
CLOSE
END SUB

SUB ListaSaler
CLOSE
OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM SHARED AS #1 LEN = 100
FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 4 AS msas$, 4 AS colfar$, 4 AS inprefar$, 7 AS vac$
nursaler = LOF(1) / 100
IF nursaler = 0 THEN Box2 10, 5, 12, 75: Center 11, "No hay Vendedores Registrados": Espera: CLOSE : EXIT SUB
COLOR colors(7, colorpref), colors(4, colorpref)
CLS
Box2 5, 5, 22, 75
Center 6, "²Presentacion de Datos de Cada Vendedor.²"
LOCATE 8, 29: PRINT "Clave Corta:"
LOCATE 9, 28: PRINT "Vendedor    de "; nursaler
LOCATE 10, 23: PRINT "Nombres Completos:";
LOCATE 11, 21: PRINT "Apellidos Completos:";
LOCATE 12, 24: PRINT "Numero de Cedula:";
LOCATE 13, 35: PRINT "Cargo:";
LOCATE 14, 17: PRINT "Clave Larga (Iniciales):";
LOCATE 15, 21: PRINT "Credencial M.S.D.S.:";
LOCATE 16, 18: PRINT "Credencial IN.PRE.FAR.:";
LOCATE 17, 21: PRINT "Credencial COL.FAR::";
lo% = 1: Box 21, 10, 23, 50
DO
   GET #1, lo%: LOCATE 8, 41: PRINT clco$
   LOCATE 10, 41: PRINT nomb$: LOCATE 9, 36: PRINT lo%
   LOCATE 11, 41: PRINT apel$: LOCATE 12, 41: PRINT USING "#########,"; CVL(cedu$): LOCATE 14, 41: PRINT clla$: LOCATE 13, 41: PRINT carg$
   LOCATE 15, 41: PRINT USING "########,"; CVL(msas$): LOCATE 16, 41: PRINT USING "########,"; CVL(inprefar$): LOCATE 17, 41: PRINT USING "########,"; CVL(colfar$)
   IF inout$ = " " THEN Center 18, " Clave Activada : Autorizado a Trabajar " ELSE Center 18, " Clave Desactivada : NO Puede a Trabajar "
   IF cong$ = "*" THEN Center 19, " Tiene Una Factura Congelada. " ELSE Center 19, " NO Tiene Factura Congelada.  "
   IF acti$ = "*" THEN COLOR colors(7, colorpref) + 16, colors(4, colorpref): Center 20, "*** ESTE VENDEDOR ESTA BORRADO ***": COLOR colors(7, colorpref), colors(4, colorpref) ELSE Center 20, SPACE$(35)
   LOCATE 22, 13: PRINT "[P]revio , [O]tro o [S]alir?"
   dec$ = MeteDatoLetra(22, 45, 1, "")
   IF sonido = "si" THEN BEEP
   IF dec$ = "S" OR dec$ = "s" THEN CLOSE : EXIT DO
   IF dec$ = "p" OR dec$ = "P" THEN lo% = lo% - 1: IF lo% = 0 THEN lo% = 1
   IF dec$ = "o" OR dec$ = "O" THEN lo% = lo% + 1: IF nursaler < lo% THEN lo% = nursaler%
LOOP: CLOSE
END SUB

FUNCTION MeteDatoLetra$ (fila%, columna%, largo%, caden$)
llevasitio% = 1: LOCATE fila%, columna%: PRINT CHR$(178) + caden$: LOCATE fila%, columna% + largo% + 1: PRINT CHR$(178): REDIM eleccion(0 TO largo% + 1) AS STRING * 1
FOR k = 1 TO largo%: eleccion(k) = MID$(caden$, k, 1): NEXT k: LOCATE fila%, (columna% + llevasitio%): COLOR 31, 0: IF eleccion(1) = " " THEN PRINT "_";  ELSE PRINT eleccion(1)
DO
     llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
        SELECT CASE llave$
            CASE CHR$(0) + "H": devolver = true: EXIT DO
            CASE CHR$(0) + "P": EXIT DO
            CASE CHR$(0) + "K"
               llevasitio% = llevasitio% - 1: IF llevasitio% = 0 THEN GOSUB Sumar: EXIT DO
            CASE CHR$(0) + "M"
               llevasitio% = llevasitio% + 1: IF llevasitio% > largo% THEN GOSUB Sumar: EXIT DO
            CASE CHR$(8)
               llevasitio% = llevasitio% - 1: IF llevasitio% = 0 THEN GOSUB Sumar: EXIT DO
               FOR k = llevasitio% TO largo% - 1: eleccion(k) = eleccion(k + 1)
               NEXT k: eleccion(largo%) = " "
            CASE CHR$(0) + CHR$(83)
               FOR k = llevasitio% TO largo%: eleccion(k) = eleccion(k + 1): NEXT k
               eleccion(largo%) = " "
            CASE CHR$(13): EXIT DO
            CASE CHR$(27): salida = true: EXIT DO
            CASE CHR$(0) + CHR$(67): Fun(9) = "si": EXIT DO
            'CASE CHR$(0) + CHR$(64): Fun(6) = "si": EXIT DO
            'CASE CHR$(0) + CHR$(65): Fun(7) = "si": EXIT DO
            CASE CHR$(0) + CHR$(71): llevasitio% = 1
            CASE CHR$(0) + CHR$(79): llevasitio% = largo%
            CASE ELSE
               eleccion(llevasitio%) = llave$
               llevasitio% = llevasitio% + 1
               IF llevasitio% > largo% THEN GOSUB Sumar: EXIT DO
        END SELECT
        FOR k = 1 TO largo%
        IF k = llevasitio% THEN COLOR 31, 0
        LOCATE fila%, (columna% + k)
        IF k = llevasitio% THEN
                IF eleccion(k) <> " " THEN PRINT eleccion(k);  ELSE PRINT "_";
        ELSE
                PRINT eleccion(k);
        END IF
        IF k = llevasitio% THEN COLOR colors(5, colorpref), colors(4, colorpref)
        NEXT k
LOOP
eleccion$ = "": FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR colors(5, colorpref), colors(4, colorpref): LOCATE fila%, columna% + 1: PRINT eleccion$
MeteDatoLetra$ = eleccion$: IF sonido = "si" THEN BEEP
EXIT FUNCTION
Sumar:
FOR k = 1 TO largo%: eleccion$ = eleccion$ + eleccion(k): NEXT k
COLOR 0, 15: LOCATE fila%, columna%: PRINT "²"; eleccion$; "²"
RETURN
END FUNCTION

SUB ModificaSaler
COLOR colors(7, colorpref), colors(4, colorpref): CLS : Box 5, 5, 22, 75: COLOR colors(4, colorpref), colors(7, colorpref): Center 6, "Modificacion de Datos de un Vendedor.": COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 8, 16: PRINT "Clave Corta del Vendedor:"
Busca$ = MeteDatoLetra(8, 41, 3, "   ")
CLOSE : OPEN "F:\kevinprg\kevindat\saler.dat" FOR RANDOM AS #1 LEN = 100
FIELD #1, 1 AS acti$, 20 AS nomb$, 20 AS apel$, 4 AS cedu$, 20 AS carg$, 3 AS clco$, 10 AS clla$, 1 AS inout$, 1 AS cong$, 1 AS gaveta$, 4 AS msas$, 4 AS colfar$, 4 AS inprefar$, 7 AS vac$
nursaler = LOF(1) / 100: eleg% = 0: IF nursaler = 0 THEN Box2 10, 5, 12, 75: Center 11, "No hay Vendedores Registrados": Espera: CLOSE : EXIT SUB
FOR k = 1 TO nursaler: GET #1, k
IF Busca$ = clco$ THEN eleg% = k
NEXT k
IF eleg% = 0 THEN Center 8, "No Hay Ningun Vendedor Con Esa Clave.Pruebe Otra Vez.": SLEEP 3: CLOSE : EXIT SUB
GET #1, eleg%: Nombres = nomb$: Apellidos = apel$: Cedula$ = STR$(CVL(cedu$)): Cargo$ = carg$
ClaveCorta$ = clco$: ClaveLarga$ = clla$: Gav$ = gaveta$: Msas2$ = STR$(CVL(msas$)): Colfar2$ = STR$(CVL(colfar$)): Inprefar2$ = STR$(CVL(inprefar$))
Center 25, " [F1] Ayuda [F9] Salir y Salvar ": COLOR colors(7, colorpref), colors(4, colorpref): pepsi% = 1
LOCATE 10, 23: PRINT "Nombres Completos:"; : COLOR colors(4, colorpref), colors(7, colorpref): PRINT "²"; Nombres; "²": COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 11, 21: PRINT "Apellidos Completos:"; : COLOR colors(7, colorpref), colors(4, colorpref): PRINT "²"; Apellidos; "²": COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 12, 24: PRINT "Numero de Cedula:"; : COLOR colors(7, colorpref), colors(4, colorpref): PRINT "²"; Cedula$; "²": COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 13, 11: PRINT "Cargo a Ocupar en la Farmacia:"; : COLOR colors(7, colorpref), colors(4, colorpref): PRINT "²"; Cargo$; "²": COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 14, 17: PRINT "Clave Larga (Iniciales):"; : COLOR colors(7, colorpref), colors(4, colorpref): PRINT "²"; ClaveLarga$; "²": COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 15, 11: PRINT "Gaveta a Abrir 2 o 1 Solamente"; : COLOR colors(4, colorpref), colors(7, colorpref): PRINT "²"; Gav$; "²": COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 16, 11: PRINT "Numero del M.S.A.S.          :"; : COLOR colors(4, colorpref), colors(7, colorpref): PRINT "²"; Msas2$; "²": COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 17, 11: PRINT "Numero del COL.FAR.          :"; : COLOR colors(4, colorpref), colors(7, colorpref): PRINT "²"; Colfar2$; "²": COLOR colors(7, colorpref), colors(4, colorpref)
LOCATE 18, 11: PRINT "Numero del IN.PRE.FAR        :"; : COLOR colors(4, colorpref), colors(7, colorpref): PRINT "²"; Inprefar2$; "²": COLOR colors(7, colorpref), colors(4, colorpref)
IF inout$ = " " THEN Center 19, " Clave Activada : Autorizado a Trabajar " ELSE Center 19, " Clave Activada : NO Puede a Trabajar "
DO
     SELECT CASE pepsi%
          CASE 1
               Nombres = MeteDatoLetra(10, 41, 20, Nombres)
               IF devolver = true THEN devolver = false: pepsi% = 1 ELSE pepsi% = 2
               IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DO
          CASE 2
               Apellidos = MeteDatoLetra(11, 41, 20, Apellidos)
               IF devolver = true THEN devolver = false: pepsi% = 1 ELSE pepsi% = 3
               IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DO
          CASE 3
               Cedula$ = MeteDatoLetra(12, 41, 9, Cedula$)
               IF devolver = true THEN devolver = false: pepsi% = 2 ELSE pepsi% = 4
               IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DO
          CASE 4
               Cargo$ = MeteDatoLetra(13, 41, 20, Cargo$)
               IF devolver = true THEN devolver = false: pepsi% = 3 ELSE pepsi% = 5
               IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DO
          CASE 5
               ClaveLarga$ = MeteDatoLetra(14, 41, 10, ClaveLarga$)
               IF devolver = true THEN devolver = false: pepsi% = 4 ELSE pepsi% = 6
               IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DO
          CASE 6
               Gav$ = MeteDatoLetra(15, 41, 1, Gav$)
               IF devolver = true THEN devolver = false: pepsi% = 5 ELSE pepsi% = 7
               IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DO
          CASE 7
               Msas2$ = MeteDatoLetra(16, 41, 8, Msas2$)
               IF devolver = true THEN devolver = false: pepsi% = 6 ELSE pepsi% = 8
               IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DO
          CASE 8
               Colfar2$ = MeteDatoLetra(17, 41, 8, Colfar2$)
               IF devolver = true THEN devolver = false: pepsi% = 7 ELSE pepsi% = 9
               IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DO
          CASE 9
               Inprefar2$ = MeteDatoLetra(18, 41, 8, Inprefar2$)
               IF devolver = true THEN devolver = false: pepsi% = 8 ELSE pepsi% = 1
               IF Fun(9) = "si" THEN Fun(9) = "no": EXIT DO
     END SELECT
LOOP
Center 20, "         Salvando la Informacion...        "
LSET nomb$ = Nombres: LSET apel$ = Apellidos: LSET cedu$ = MKL$(VAL(Cedula$))
LSET carg$ = Cargo$: LSET clco$ = ClaveCorta$: LSET clla$ = ClaveLarga$: RSET gaveta$ = Gav$
RSET msas$ = MKL$(VAL(Msas2$)): RSET inprefar$ = MKL$(VAL(Inprefar2$)): RSET colfar$ = MKL$(VAL(Colfar2$))
PUT #1, eleg%: CLOSE
END SUB

SUB SalvaEstado
OPEN "C:\turno.dat" FOR OUTPUT SHARED AS #7
PRINT #7, USING "#"; colorpref
PRINT #7, turno
PRINT #7, USING "##"; dia1
PRINT #7, USING "##"; mes1
PRINT #7, USING "####"; ano1
CLOSE #7
OPEN "f:\kevinprg\kevindat\kaos.dat" FOR OUTPUT SHARED AS #2
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

FUNCTION Trim$ (x$)
IF x$ = "" THEN
     Trim$ = ""
ELSE
lastChar = 0
FOR a = 1 TO LEN(x$): Y$ = MID$(x$, a, 1): IF Y$ <> CHR$(0) AND Y$ <> " " THEN lastChar = a
NEXT a: Trim$ = LEFT$(x$, lastChar)
END IF
END FUNCTION

SUB Universal
Box2 9, 10, 17, 70: Center 10, "- Introduzca NUEVA Contrase¤a para esta Utilidad -": Center 14, "- Se Procedera a Activar la Clave -": Center 15, "- No Olvide por NINGUN MOTIVO la Contrase¤a Secreta -"
contra1$ = MeteDatoLetra$(12, 30, 15, ""): contra = Trim$(contra1$): contri = "si": Center 12, SPACE$(13) + CHR$(178) + "Salvando" + CHR$(178) + SPACE$(13)
SalvaEstado
END SUB

FUNCTION Verif$ (fila%, columna%)
COLOR colors(7, colorpref), colors(4, colorpref): IF contri = "no" THEN Verif$ = "true": EXIT FUNCTION
Box2 fila% - 1, columna% - 2, fila% + 1, columna% + 10 + LEN(Trim$(contra)): llevalargo = 0: eleccion$ = "": LOCATE fila%, columna%: PRINT CHR$(168); "Clave? "; CHR$(178); SPACE$(LEN(Trim$(contra))); CHR$(178)
DO WHILE llevalargo < LEN(Trim$(contra))
llave$ = "": WHILE llave$ = "": llave$ = INKEY$: WEND
llevalargo = llevalargo + 1: LOCATE fila%, columna% + llevalargo + 8: PRINT CHR$(176);
eleccion$ = eleccion$ + llave$
LOOP
IF sonido = "si" THEN BEEP
IF eleccion$ = Trim$(contra) THEN Verif$ = "true" ELSE Verif$ = "false": Box2 fila% - 1, columna% - 2, fila% + 1, columna% + 25: LOCATE fila%, columna%: PRINT "   Clave Incorrecta    ": SLEEP 3
EXIT FUNCTION
END FUNCTION

