#include 'Protheus.ch'

// ----------------------------------------
// Converte data no formato AAAAMMDD para Data do AdvPL 
STATIC Function STOD(cValue)
Local cOldSet := Set(_SET_DATEFORMAT, 'yyyy:mm:dd')
Local dRet := CTOD(Substr(cValue,1,4)+":"+Substr(cValue,5,2)+":"+Substr(cValue,7,2))
Set(_SET_DATEFORMAT, cOldSet)
Return dRet

