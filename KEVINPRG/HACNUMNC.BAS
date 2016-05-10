'inicializa numero de notas de creditos a clientes   
     OPEN "F:\kevinprg\kevindat\maxnc.dat" FOR RANDOM AS #7
     FIELD #7, 4 AS credito$
     LSET credito$ = MKL$(1)
     PUT #7, 1
     CLOSE

