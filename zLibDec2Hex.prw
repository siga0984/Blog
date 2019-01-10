
/* ======================================================

	Funcoes de Conversao Decimal / Hexa e vice-versa

====================================================== */

USER Function HexTest()
Local cHex := 'AA'
Local nVal := HEX2DEC(cHex)
Local cHex2 := DEC2HEX(nVal)
conout(cHex)
conout(nVal)
conout(cHex2)
return

// ----------------------------------------
// Converte um valort decimal de 0 a 255 para Hexadecimal 

STATIC __aHEX := {'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'}

STATIC Function DEC2HEX(nByte)
Local nL := ( nByte % 16 ) 
Local nH := ( nByte-nL) / 16 
Return __aHEX[nH+1]+__aHEX[nL+1]

// ----------------------------------------
// Converte um valort hexadecimal de 00 a FF para decimal 

STATIC Function HEX2DEC(cHex)
Local nH := asc(Upper(substr(cHex,1,1)))
Local nL := asc(Upper(substr(cHex,2,1)))
If nH <= 57 ;    nH -= 48 ; Else ;    nH -= 55 ; Endif
If nL <= 57 ;    nL -= 48 ; Else ;    nL -= 55 ; Endif
Return (nH*16)+nL


