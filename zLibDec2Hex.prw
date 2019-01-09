
// ----------------------------------------
// Converte um valort decimal de 0 a 255 para Hexadecimal 

STATIC __aHEX := {'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'}

STATIC Function DEC2HEX(nByte)
Local nL := ( nByte % 16 ) 
Local nH := ( nByte-nL) / 16 
Return __aHEX[nH+1]+__aHEX[nL+1]

