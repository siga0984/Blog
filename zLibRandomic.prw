
/* ======================================================

	Função Randomic() -- Não tem o limite de 32K da Randomize()

====================================================== */

STATIC Function Randomic(nMin,nMax)
Local nDiff := nMax-nMin
Local nDec := 0
If nDiff < 32766
	Return Randomize(nMin,nMax)
Endif
While nDiff > 32765
	nDiff /= 10 
	nDec++
Enddo
nTmp := randomize(0,int(nDiff))
While nDec > 0 
	nTmp *= 10 
	nTmp += randomize(1,10)	
	nDec--
Enddo
Return nMin+nTmp

