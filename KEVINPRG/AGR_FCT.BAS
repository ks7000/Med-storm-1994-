CLOSE : OPEN "F:\kevinprg\kevindat\master.fct" FOR RANDOM AS #27 LEN = 70
FIELD #27, 1 AS acti$, 4 AS numfact$, 8 AS fechaacc$, 10 AS vendedor$, 2 AS prove$, 1 AS contacredi$, 8 AS brut$, 8 AS net$, 8 AS ivafact$, 8 AS fechafact$, 1 AS actual$, 4 AS numped$, 1 AS cancelada$, 6 AS vacio$
'2 AS prove$, 1 AS contacredi$, 8 AS brut$, 8 AS net$, 8 AS ivafact$,
'8 AS fechafact$, 1 AS actual$, 4 AS numped$, 1 AS cancelada$, 6 AS vacio$
max% = LOF(27) / 70 + 1
RSET acti$ = " "
LSET numfact$ = MKL$(20903737)
LSET fechaacc$ = "22/04/2003"
LSET vendedor$ = MKL$(7137258)
LSET prove$ = MKI$(16)
LSET contacred$ = "*"
LSET brut$ = MKD$(620179.2)
LSET net$ = MKD$(551959.49#)
LSET ivafact$ = MKD$(3806.64)
LSET fechafact$ = "21042003"
LSET actual$ = " "
LSET numped$ = MKL$(0)
LSET cancelada$ = " "
LSET vacio$ = "      "
PUT #27, max%
CLOSE

