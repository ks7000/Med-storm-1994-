echo off
cd\kevinprg
qb /run kevin000.bas /l toolbox

if exist f:\kevinprg\kevindat\respaldo.dat goto :respaldo
goto :fin

:respaldo
del f:\kevinprg\kevindat\*.ver
del f:\kevinprg\kevindat\kevindat.lzh
del c:\kevindat.lzh
del C:\TMP\*.*
c:\dos\xcopy f:\kevinprg\kevinvnt\*.* C:\TMP /d:01-12-2004 /v
c:\dos\xcopy f:\kevinprg\kevindat\*.* C:\TMP /d:01-12-2004 /v
c:\dos\xcopy f:\kevinprg\tarjetas\*.* C:\TMP /d:01-12-2004 /v
f:\kevinprg\lha a f:\kevinprg\kevindat\kevindat.lzh C:\TMP\*.*
copy f:\kevinprg\kevindat\kevindat.lzh c:\ /v
rem copy f:\kevinprg\kevindat\kevindat.lzh a:\ /v

:fin
c:
rem cd\red
rem logout
