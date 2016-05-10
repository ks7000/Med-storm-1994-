'inicializa archivos de pedidos de compras
CLOSE : OPEN "c:\kevinprg\kevindat\master.pdd" FOR RANDOM AS #1 LEN = 66
FIELD #1, 4 AS pedido$, 62 AS resto$: RSET pedido$ = MKL$(1): PUT #1, 1: CLOSE : END

