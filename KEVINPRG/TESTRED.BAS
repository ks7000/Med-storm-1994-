    'Este ejemplo s¢lo funcionar  en una red.
    ON ERROR GOTO ErrorTrap
    CLS : CLOSE
    OPEN "F:\KEVINPRG\KEVINDAT\TEST.DAT" FOR RANDOM SHARED AS #1 LEN = 2: FIELD #1, 2 AS todo$
DO
    PRINT "Que Desea Hacer [1.-Bloquear] [2.-Desbloquear] [3.-Mostrar] [4.-Ver de `x'] ?"
    v$ = INPUT$(1)
    SELECT CASE v$
    CASE "1"
          INPUT "Que Registro Desea Bloquear?", l%
          IF l% = -1 THEN EXIT DO ELSE LOCK #1, l% TO l% + 5
    CASE "3"
          INPUT "Que Registro Desea Obtener?", l%
          IF l% = -1 THEN EXIT DO ELSE GET #1, l%
          PRINT CVI(todo$)
    CASE "2"
          INPUT "Que Registro Desea Desbloquear?", l%
          IF l% = -1 THEN EXIT DO ELSE UNLOCK #1, l% TO l% + 5
    CASE "4"
          ON ERROR GOTO 0
          ON ERROR GOTO ErrorTrop
          INPUT "Que Registro Quiere Comenzar a Ver?", l%
          FOR k% = l% TO l% + 10: GET #1, k%: PRINT CVI(todo$): NEXT k%
          ON ERROR GOTO 0
          ON ERROR GOTO ErrorTrap
    CASE ELSE: BEEP
    END SELECT
LOOP
CLOSE
END

ErrorTrap:
PRINT "Esperando acceso a la Red . . .": SLEEP 1: RESUME

ErrorTrop:
PRINT "Registro Bloqueado:"; k%
RESUME NEXT

