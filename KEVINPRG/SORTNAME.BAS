'Box 11, 15, 16, 65: Center 13, "Ordenando Los Registros Alfabeticamente...": Center 14, " Accesando el registro       de  " + STR$(MaxArt)
'FOR a% = 1 TO MaxArt - 1: FOR b% = a% + 1 TO MaxArt: LOCATE 14, 45: PRINT a%
'IF Xname(a%).Nombre < Xname(b%).Nombre THEN GOTO Nomayor
'Temp$ = Xname(a%).Codi: Xname(a%).Codi = Xname(b%).Codi: Xname(b%).Codi = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Min1: Xname(a%).Min1 = Xname(b%).Min1: Xname(b%).Min1 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Min2: Xname(a%).Min2 = Xname(b%).Min2: Xname(b%).Min2 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).max1: Xname(a%).max1 = Xname(b%).max1: Xname(b%).max1 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).max2: Xname(a%).max2 = Xname(b%).max2: Xname(b%).max2 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Ubicacion: Xname(a%).Ubicacion = Xname(b%).Ubicacion: Xname(b%).Ubicacion = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Nombre: Xname(a%).Nombre = Xname(b%).Nombre: Xname(b%).Nombre = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Obsv: Xname(a%).Obsv = Xname(b%).Obsv: Xname(b%).Obsv = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Activo: Xname(a%).Activo = Xname(b%).Activo: Xname(b%).Activo = Temp$: Temp$ = ""
'Temp$ = Xname(a%).exis1: Xname(a%).exis1 = Xname(b%).exis1: Xname(b%).exis1 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Exis2: Xname(a%).Exis2 = Xname(b%).Exis2: Xname(b%).Exis2 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Exis3: Xname(a%).Exis3 = Xname(b%).Exis3: Xname(b%).Exis3 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Pvp1: Xname(a%).Pvp1 = Xname(b%).Pvp1: Xname(b%).Pvp1 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Pvp2: Xname(a%).Pvp2 = Xname(b%).Pvp2: Xname(b%).Pvp2 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Pvp3: Xname(a%).Pvp3 = Xname(b%).Pvp3: Xname(b%).Pvp3 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Costo1: Xname(a%).Costo1 = Xname(b%).Costo1: Xname(b%).Costo1 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Costo2: Xname(a%).Costo2 = Xname(b%).Costo2: Xname(b%).Costo2 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Costo3: Xname(a%).Costo3 = Xname(b%).Costo3: Xname(b%).Costo3 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Vence1: Xname(a%).Vence1 = Xname(b%).Vence1: Xname(b%).Vence1 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Vence2: Xname(a%).Vence2 = Xname(b%).Vence2: Xname(b%).Vence2 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Vence3: Xname(a%).Vence3 = Xname(b%).Vence3: Xname(b%).Vence3 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Iva1: Xname(a%).Iva1 = Xname(b%).Iva1: Xname(b%).Iva1 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Iva2: Xname(a%).Iva2 = Xname(b%).Iva2: Xname(b%).Iva2 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Iva3: Xname(a%).Iva3 = Xname(b%).Iva3: Xname(b%).Iva3 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Oferta1: Xname(a%).Oferta1 = Xname(b%).Oferta1: Xname(b%).Oferta1 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Oferta2: Xname(a%).Oferta2 = Xname(b%).Oferta2: Xname(b%).Oferta2 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Oferta3: Xname(a%).Oferta3 = Xname(b%).Oferta3: Xname(b%).Oferta3 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Lab: Xname(a%).Lab = Xname(b%).Lab: Xname(b%).Lab = Temp$: Temp$ = ""
'Temp$ = Xname(a%).ReFalla: Xname(a%).ReFalla = Xname(b%).ReFalla: Xname(b%).ReFalla = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Pedido: Xname(a%).Pedido = Xname(b%).Pedido: Xname(b%).Pedido = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Med: Xname(a%).Med = Xname(b%).Med: Xname(b%).Med = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Mis: Xname(a%).Mis = Xname(b%).Mis: Xname(b%).Mis = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Qui: Xname(a%).Qui = Xname(b%).Qui: Xname(b%).Qui = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Comp1: Xname(a%).Comp1 = Xname(b%).Comp1: Xname(b%).Comp1 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Comp2: Xname(a%).Comp2 = Xname(b%).Comp2: Xname(b%).Comp2 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Comp3: Xname(a%).Comp3 = Xname(b%).Comp3: Xname(b%).Comp3 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Comp4: Xname(a%).Comp4 = Xname(b%).Comp4: Xname(b%).Comp4 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Comp5: Xname(a%).Comp5 = Xname(b%).Comp5: Xname(b%).Comp5 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).Comp6: Xname(a%).Comp6 = Xname(b%).Comp6: Xname(b%).Comp6 = Temp$: Temp$ = ""
'Temp$ = Xname(a%).PreVe: Xname(a%).PreVe = Xname(b%).
'PreVe: Xname(b%).PreVe = Temp$: Temp$ = ""
'Nomayor:
'NEXT b%: NEXT a%

