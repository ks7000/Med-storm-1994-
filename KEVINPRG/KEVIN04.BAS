DEFINT A-Z
DECLARE SUB VentaContado ()
DECLARE SUB Espera ()
SELECT CASE guia
     CASE 1: VentaContado
END SELECT
END

SUB Espera
DO: LOOP WHILE INKEY$ = ""
END SUB

SUB VentaContado
CLS : Espera
END SUB

