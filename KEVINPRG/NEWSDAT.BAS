DEFINT A-Z
DECLARE FUNCTION Trim$ (x$)
'amplia codigos a 199 : por ubicacion

CLOSE
OPEN "F:\kevinprg\kevindat\ubica.dat" FOR RANDOM AS #5 LEN = 18
FIELD #5, 2 AS codigo1$, 15 AS nomb1$, 1 AS vacio$
OPEN "F:\kevinprg\kevindat\ubica.jim" FOR RANDOM AS #6 LEN = 18
FIELD #6, 3 AS codigo2$, 15 AS nomb2$
FOR k = 1 TO 99
     GET #5, k
     LSET codigo2$ = "0" + codigo1$: LSET nomb2$ = nomb1$
     PUT #6, k
NEXT k
FOR k = 100 TO 199
     LSET codigo2$ = MID$(STR$(k), 2, 3): LSET nomb2$ = "þ" + SPACE$(14)
     PUT #6, k
NEXT k: CLOSE
KILL "F:\kevinprg\kevindat\ubica.dat"
NAME "F:\kevinprg\kevindat\ubica.jim" AS "F:\kevinprg\kevindat\ubica.dat"


'amplia codigos a 199 : por proveedor

CLOSE
OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #5 LEN = 18
FIELD #5, 2 AS codigo1$, 15 AS nomb1$, 1 AS vacio$
OPEN "F:\kevinprg\kevindat\proveedo.jim" FOR RANDOM AS #6 LEN = 18
FIELD #6, 3 AS codigo2$, 15 AS nomb2$
FOR k = 1 TO 99
     GET #5, k
     LSET codigo2$ = "0" + codigo1$: LSET nomb2$ = nomb1$
     PUT #6, k
NEXT k
FOR k = 100 TO 199
     LSET codigo2$ = MID$(STR$(k), 2, 3): LSET nomb2$ = "þ" + SPACE$(14)
     PUT #6, k
NEXT k: CLOSE
KILL "F:\kevinprg\kevindat\proveedo.dat"
NAME "F:\kevinprg\kevindat\proveedo.jim" AS "F:\kevinprg\kevindat\proveedo.dat"


'acomoda para proveedores que no se han usado "þ                    "

CLOSE
OPEN "F:\kevinprg\kevindat\proveedo.dat" FOR RANDOM AS #5 LEN = 18
FIELD #5, 3 AS codigo$, 15 AS nomb$
OPEN "f:\kevinprg\kevindat\fichapro.vee" FOR RANDOM SHARED AS #6 LEN = 400
FIELD #6, 20 AS rif5$, 20 AS nit5$, 2 AS diacred5$, 4 AS monmax5$, 60 AS nombre5$, 60 AS direc15$, 20 AS tele5$, 10 AS codicli5$, 30 AS autor15$, 30 AS autor25$, 30 AS autor35$, 30 AS autor45$, 84 AS vacio$
FOR k = 1 TO 199
     GET #5, k
     IF nomb$ = "þ" + SPACE$(14) THEN
          RSET rif5$ = SPACE$(20): RSET nit5$ = SPACE$(20)
          RSET nombre5$ = SPACE$(60): RSET direc15$ = SPACE$(60)
          RSET tele5$ = SPACE$(20): RSET codicli5$ = SPACE$(10)
          RSET autor15$ = SPACE$(30): RSET autor25$ = SPACE$(30)
          RSET autor35$ = SPACE$(30): RSET autor45$ = SPACE$(30)
          RSET vacio$ = SPACE$(84): RSET diacred5$ = MKI$(0)
          RSET monmax5$ = MKL$(0): PUT #6, VAL(codigo$)
     END IF
NEXT k: CLOSE


'amplia codigos a 199 : por componentes

CLOSE
OPEN "F:\kevinprg\kevindat\componen.dat" FOR RANDOM AS #5 LEN = 18
FIELD #5, 2 AS codigo1$, 15 AS nomb1$, 1 AS vacio$
OPEN "F:\kevinprg\kevindat\componen.jim" FOR RANDOM AS #6 LEN = 18
FIELD #6, 3 AS codigo2$, 15 AS nomb2$
FOR k = 1 TO 99
     GET #5, k
     LSET codigo2$ = "0" + codigo1$: LSET nomb2$ = nomb1$
     PUT #6, k
NEXT k
FOR k = 100 TO 199
     LSET codigo2$ = MID$(STR$(k), 2, 3): LSET nomb2$ = "þ" + SPACE$(14)
     PUT #6, k
NEXT k: CLOSE
KILL "F:\kevinprg\kevindat\componen.dat"
NAME "F:\kevinprg\kevindat\componen.jim" AS "F:\kevinprg\kevindat\componen.dat"


'amplia codigos a 199 : por laboratorios

CLOSE
OPEN "F:\kevinprg\kevindat\lab.dat" FOR RANDOM AS #5 LEN = 18
FIELD #5, 2 AS codigo1$, 15 AS nomb1$, 1 AS vacio$
OPEN "F:\kevinprg\kevindat\lab.jim" FOR RANDOM AS #6 LEN = 18
FIELD #6, 3 AS codigo2$, 15 AS nomb2$
FOR k = 1 TO 99
     GET #5, k
     LSET codigo2$ = "0" + codigo1$: LSET nomb2$ = nomb1$
     PUT #6, k
NEXT k
FOR k = 100 TO 199
     LSET codigo2$ = MID$(STR$(k), 2, 3): LSET nomb2$ = "þ" + SPACE$(14)
     PUT #6, k
NEXT k: CLOSE
KILL "F:\kevinprg\kevindat\lab.dat"
NAME "F:\kevinprg\kevindat\lab.jim" AS "F:\kevinprg\kevindat\lab.dat"
         

'amplia codigos a 199 : por cliente con credito

CLOSE
OPEN "F:\kevinprg\kevindat\clientes.dat" FOR RANDOM AS #5 LEN = 37
FIELD #5, 2 AS codigo1$, 35 AS nomb1$
OPEN "F:\kevinprg\kevindat\clientes.jim" FOR RANDOM AS #6 LEN = 38
FIELD #6, 3 AS codigo2$, 35 AS nomb2$
FOR k = 1 TO 99
     GET #5, k
     LSET codigo2$ = "0" + codigo1$: LSET nomb2$ = nomb1$
     PUT #6, k
NEXT k
FOR k = 100 TO 199
     LSET codigo2$ = MID$(STR$(k), 2, 3): LSET nomb2$ = "þ" + SPACE$(34)
     PUT #6, k
NEXT k
CLOSE
        

'acomoda para clientes que no se han usado "þ                    "

CLOSE
OPEN "F:\kevinprg\kevindat\clientes.dat" FOR RANDOM AS #5 LEN = 38
FIELD #5, 3 AS codigo$, 35 AS nomb$
OPEN "f:\kevinprg\kevindat\fichacli.ent" FOR RANDOM SHARED AS #6 LEN = 326
FIELD #6, 20 AS rif5$, 20 AS nit5$, 2 AS diacred5$, 4 AS monmax5$, 60 AS direc15$, 60 AS direc25$, 20 AS tele5$, 1 AS edo15$, 2 AS dia15$, 1 AS edo25$, 2 AS dia25$, 1 AS edo35$, 2 AS dia35$, 1 AS edo45$, 2 AS dia45$, 30 AS autor15$, 30 AS autor25$, 30 AS autor35$, 30 AS autor45$, 8 AS desde$
FOR k = 1 TO 199
     GET #5, k
     IF nomb$ = "þ" + SPACE$(34) THEN
          RSET rif5$ = SPACE$(20): RSET nit5$ = SPACE$(20)
          RSET direc15$ = SPACE$(60): RSET direc25$ = SPACE$(60)
          RSET tele5$ = SPACE$(20)
          RSET edo15$ = " ": RSET dia15$ = MKI$(0)
          RSET edo25$ = " ": RSET dia25$ = MKI$(0)
          RSET edo35$ = " ": RSET dia35$ = MKI$(0)
          RSET edo45$ = " ": RSET dia45$ = MKI$(0)
          RSET autor15$ = SPACE$(30): RSET autor25$ = SPACE$(30)
          RSET autor35$ = SPACE$(30): RSET autor45$ = SPACE$(30)
          RSET desde$ = SPACE$(8): RSET diacred5$ = MKI$(0)
          RSET monmax5$ = MKL$(0): PUT #6, VAL(codigo$)
     END IF
NEXT k: CLOSE


'convierte el archivo master.pdd

OPEN "F:\kevinprg\kevindat\master.pdd" FOR RANDOM AS #7 LEN = 66
FIELD #7, 66 AS todo$: max% = LOF(7) / 66
OPEN "F:\kevinprg\kevindat\master.ver" FOR RANDOM AS #8 LEN = 67
FIELD #8, 66 AS all$, 1 AS tipo$: RSET tipo$ = "1"
FOR k = 1 TO max%: GET #7, k: RSET all$ = todo$: PUT #8, k: NEXT k
CLOSE
KILL "F:\kevinprg\kevindat\master.pdd"
NAME "F:\kevinprg\kevindat\master.ver" AS "F:\kevinprg\kevindat\master.pdd"


'convierte las solicitudes de pedido *.pdd con el numero de registro de new.dbf

OPEN "F:\kevinprg\kevindat\codigo.idx" FOR RANDOM SHARED AS #2 LEN = 8
FIELD #2, 6 AS nomshort$, 2 AS num$: max2% = LOF(2) / 8
OPEN "F:\kevinprg\kevindat\master.pdd" FOR RANDOM AS #4 LEN = 67
FIELD #4, 1 AS viva$, 4 AS numped$, 2 AS numprov$, 5 AS hora$, 10 AS fecha$, 4 AS cedven$, 40 AS obse$, 1 AS tipo$
max4% = LOF(4) / 67

FOR k = 1 TO max4%
     GET #4, k
     IF viva$ = " " THEN
          num7$ = STR$(CVL(numped$)): num7$ = Trim$(MID$(num7$, 2, 8)): num7$ = "F:\kevinprg\kevindat\" + STRING$(8 - LEN(num7$), "0") + num7$ + ".pdd"
          num8$ = MID$(num7$, 1, 29) + ".ver"
          OPEN num7$ FOR RANDOM AS #7 LEN = 17
          FIELD #7, 6 AS codi7$, 2 AS cant7$, 8 AS pvp37$, 1 AS paf7$
          max7% = LOF(7) / 17
          OPEN num8$ FOR RANDOM AS #8 LEN = 19
          FIELD #8, 6 AS codi8$, 2 AS cant8$, 8 AS pvp38$, 1 AS paf8$, 2 AS sit$
          FOR l = 1 TO max7%: LOCATE 1, 1: PRINT l
               GET #7, l: encontrado = 0: primero = 1: ultimo = max2%: Prod$ = codi7$
               WHILE encontrado = 0 AND primero <= ultimo
                    central = (primero + ultimo) / 2: GET #2, central: LOCATE 2, 1: PRINT primero; ultimo
                    IF Prod$ = nomshort$ THEN
                         encontrado = 1
                    ELSE
                         IF nomshort$ > Prod$ THEN
                              ultimo = central - 1
                         ELSE
                              primero = central + 1
                         END IF
                    END IF
               WEND
               IF encontrado = 0 THEN PRINT "ojo problemas con los codigos de pedido": SLEEP
               GET #2, central
               RSET sit$ = num$: RSET codi8$ = codi7$: RSET cant8$ = cant7$
               RSET pvp38$ = pvp37$: RSET paf8$ = paf7$: PUT #8, l
          NEXT l
          CLOSE #7: CLOSE #8
          KILL num7$: NAME num8$ AS num7$
          END IF
NEXT k

CLOSE
SYSTEM
END

DEFSNG A-Z
FUNCTION Trim$ (x$)
IF x$ = "" THEN
     Trim$ = ""
ELSE
lastChar = 0
FOR a = 1 TO LEN(x$): y$ = MID$(x$, a, 1): IF y$ <> CHR$(0) AND y$ <> " " THEN lastChar = a
NEXT a: Trim$ = LEFT$(x$, lastChar)
END IF
END FUNCTION

