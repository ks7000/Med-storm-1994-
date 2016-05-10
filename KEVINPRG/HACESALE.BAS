'actualiza archivo de personal-pre-Medstorm a Medstorm version 1.00
CLS
DIM Apellidos AS STRING * 20, Nombres AS STRING * 20, Cedula AS STRING * 10, Cargo AS STRING * 20, ClaveCorta AS STRING * 3, ClaveLarga AS STRING * 10, Direccion1 AS STRING * 20, Direccion2 AS STRING * 20, Tele AS STRING * 20, Sueldo AS STRING * 10
OPEN "a:\kevindat\saler.dat" FOR INPUT AS #1: INPUT #1, nursaler
OPEN "a:\kevindat\maxsaler.dat" FOR OUTPUT AS #2: PRINT #2, USING "##"; nursaler: CLOSE #2
OPEN "a:\kevindat\newsaler.dat" FOR RANDOM AS #2 LEN = 153: lo% = 1
FIELD #2, 20 AS Nom$, 20 AS Ape$, 10 AS Ced$, 20 AS Car$, 3 AS Cor$, 10 AS Lar$, 20 AS Dir1$, 20 AS Dir2$, 20 AS Tel$, 10 AS Sue$
DO UNTIL EOF(1)
          INPUT #1, Apellidos: PRINT Apellidos
          INPUT #1, Nombres
          INPUT #1, Cedula
          INPUT #1, Cargo
          INPUT #1, ClaveCorta
          INPUT #1, ClaveLarga
          INPUT #1, Direccion1
          INPUT #1, Direccion2
          INPUT #1, Tele
          INPUT #1, Sueldo
          RSET Nom$ = Nombres: RSET Ape$ = Apellidos: RSET Ced$ = Cedula: RSET Car$ = Cargo
          RSET Cor$ = ClaveCorta: RSET Lar$ = ClaveLarga: RSET Dir1$ = Direccion1: RSET Dir2$ = Direccion2: RSET Tel$ = Tele: RSET Sue$ = Sueldo
          PUT #2, lo%: lo% = lo% + 1: PRINT lo%
LOOP
lo% = 1
DO UNTIL EOF(2)
GET #2, lo%: lo% = lo% + 1
PRINT "========================================"
PRINT Nom$; Ape$
PRINT Ced$; Car$
PRINT Cor$; Lar$
PRINT Dir1$; Dir2$
PRINT Tel$; Sue$
PRINT "========================================"
DO WHILE INKEY$ = "": LOOP
LOOP
END

