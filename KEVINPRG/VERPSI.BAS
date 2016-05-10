TYPE saldos
   codi AS STRING * 6
   nomb AS STRING * 35
   saldo AS STRING * 2
   sitio AS STRING * 2
END TYPE
DIM dat AS saldos, rat AS saldos
OPEN "f:\kevinprg\kevindat\saldos.dat" FOR RANDOM SHARED AS #3 LEN = 45
'OPEN "f:\kevinprg\kevindat\saldos.rat" FOR RANDOM SHARED AS #4 LEN = 45
maxpsi% = LOF(3) / 45: CLS : l% = 0: m% = 1
FOR k = 1 TO maxpsi%
   GET #3, k, dat
   PRINT dat.codi; "_"; dat.nomb; "_"; CVI(dat.saldo),
   PRINT "_"; CVI(dat.sitio)
   l% = l% + 1: IF l% > 22 THEN l% = 0: SLEEP: CLS
   ' copia de saldos.dat a saldos.rat, a excepcion del registro 29
   'IF k <> 29 THEN
   '   rat = dat
   '   PUT #4, m%, rat: m% = m% + 1
   'END IF
NEXT k
CLOSE

