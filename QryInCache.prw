#include "protheus.ch"

/* -------------------------------------------------------------------
Função     QryInCache()   
Autor      Julio Wittwer
Data       01/2019
Descrição  Funcao de exemplo de uso do MemCached  para armazenar uma Query 
------------------------------------------------------------------- */

User Function QryInCache()   
Local oMemCache , cError
Local aQryData := {}
Local nTimer
Local nH 

SET DATE BRITISH 
SET CENTURY ON 

// Obtém o objeto de cache 
ZMEMCACHEDPOOL():GetCache( @oMemCache , @cError )

IF oMemCache == NIL 
	conout("ZMEMCACHEDPOOL:GetCache() FAILED")	
	conout(oClient:GetErrorStr())
	return .F.
Endif

// Recupera o array do cache 
If !oMemCache:Get("TEST_QRYINCACHE",@aQryData)
	conout("oMemCache:Get() ERROR")
	conout(oClient:GetErrorStr())
	return .F.
Endif

If aQryData = NIL 

	// O array ainda não está em cache 
	// Pega a Query do banco e coloca no array 

	nH := tclink("MSSQL/DBLIGHT","localhost",7890)
	IF nH < 0 
		MsgStop("TCLINK ERROR - "+cValToChar(nH))
		Return
	Endif

	cQuery := "SELECT CPF , NOME, VALOR from DOADORES WHERE VALOR > 2000 order by 3 DESC"
	USE (TcGenQry(,,cQuery)) ALIAS QRY EXCLUSIVE NEW VIA "TOPCONN"
	TCSetField("QRY","VALOR","N",12,2)

	While !eof()
		AADD(aQryData , { QRY->CPF , QRY->NOME , QRY->VALOR } ) 
		QRY->(DbSkip())
	Enddo
	
	USE
	
	// Agora coloca os dados no cache 
	oMemCache:Set("TEST_QRYINCACHE",aQryData)

	TCUnlink(nH)
	
	conout("*** Array lido da Query ")

Else

	conout("*** Array lido do CACHE ")

Endif 

// Dados recuperados 
conout(padc(" DATA RETRIEVED ",79,"-"))	
aEval(aQryData , { |x|  conout( x[1] + " " + x[2] + " " + cvaltochar(x[3]) ) } )

// Solta o objeto de cache 
ZMEMCACHEDPOOL():ReleaseCache( @oMemCache )

return

