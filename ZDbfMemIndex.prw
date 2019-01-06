#include 'protheus.ch'

/* ==================================================

Classe      ZDBFMEMINDEX
Autor       Julio Wittwer
Data        05/01/2019
Descrição   A partir de um objeto ZDBFTABLE, permite 
            a criação de um índice em memória 

================================================== */

CLASS ZDBFMEMINDEX

   DATA oDBF
   DATA cIndexExpr
   DATA bIndexBlock
   DATA aIndexData
   DATA nCurrentRow

   METHOD NEW(oDBF)
   METHOD CREATEINDEX(cIndexExpr)
   METHOD CLOSE()

   METHOD GetFirstRec()
   METHOD GetPrevRec()
   METHOD GetNextRec()
   METHOD GetLastRec()
   
   METHOD GetIndexExpr()
   METHOD GetIndexValue()
   METHOD IndexSeek()
   
   METHOD _BuildIndexBlock(cIndexExpr)
   METHOD _Sync()

ENDCLASS

// ----------------------------------------
// Construtor do indice em memoria
// Recebe o objeto da tabela 

METHOD NEW(oDBF) CLASS ZDBFMEMINDEX
::oDBF := oDBF
::cIndexExpr := ''
::bIndexBlock := NIL
::aIndexData := {}
::nCurrentRow := 0
Return self

// ----------------------------------------
// *** METODO DE USO INTERNO ***

METHOD _Sync() CLASS ZDBFMEMINDEX
conout("[TODO] ZDBFMEMINDEX:_Sync()")
Return

// ----------------------------------------
// *** METODO DE USO INTERNO ***
// A partir da expressão de indexação fornecida, 
// cria um codeblock para gerar uma linha de dados

METHOD _BuildIndexBlock(cIndexExpr) CLASS ZDBFMEMINDEX
Local aCampos := {}
Local cTemp
Local nI, nPos

// Cria lista de campos
aEval( ::oDBF:aStruct , {|x| aadd(aCampos , x[1]) } )

// Ordena pelos maiores campos primeiro
aSort( aCampos ,,, {|x,y| alltrim(len(x)) > alltrim(len(y)) } )

// Copia a expressao de índice
cTemp := cIndexExpr

// Troca os campos por o:Fieldget(nCpo)
// Exemplo : CAMPO1 + CAMPO2 será trocado para o:FieldGet(1) + o:FieldGet(2)

For nI := 1 to len(aCampos)
	cCampo := alltrim(aCampos[nI])
	nPos   := ::oDBF:Fieldpos(cCampo)
	cTemp  := StrTran( cTemp , cCampo,"o:FieldGet(" +cValToChar(nPos)+ ")")
Next

// Monta a string com o codeblock para indice
cTemp := "{|o| "+cTemp+"}"

// Monta efetivamente o codeblock de indice
::bIndexBlock := &(cTemp)

Return

// ----------------------------------------
// Cria um indice na memoria usando a expressao 
// enviada como parametro

METHOD CREATEINDEX( cIndexExpr ) CLASS ZDBFMEMINDEX

// Guarda a expressão original do indice
::cIndexExpr := cIndexExpr

// Monta o CodeBlock para a montagem da linha de dados
// com a chave de indice
::_BuildIndexBlock( cIndexExpr )

// Agora varre a tabela montando o o set de dados para criar o índice
::aIndexData := {}

// Coloca a tabela em ordem de regisrtros para a criação do indice
::oDBF:DbSetORder(0)
::oDBF:DBClearFilter()
::oDBF:DbGoTop()

conout("["+time()+"] Lendo dados para criar o indice" )

While !::oDBF:Eof()
	aadd( ::aIndexData , { Eval( ::bIndexBlock , ::oDBF ) , ::oDBF:Recno() } )
	::oDBF:DbSkip()
Enddo

conout("["+time()+"] Ordenando em memória" )

// Sorteia pela chave de indice, usando o RECNO como criterio de desempate
// Duas chaves iguais, prevalesce a ordem fisica ( o menor recno vem primeiro )
aSort( ::aIndexData ,,, { |x,y| ( x[1] < y[1] ) .OR. ( x[1] == y[1] .AND. x[2] < y[2] ) } )

// Acrescenta o indice criado na tabela 
// E reposiciona ela no primeiro registro da ordem 
::oDBF:_AddIndex(self)
::oDBF:DbSetORder(1)
::oDBF:DbGoTop()

conout("["+time()+"] Finalizado" )

Return .T.

// ----------------------------------------
// Retorna o primeiro RECNO da ordem atual 
// Caso nao tenha dados, retorna zero

METHOD GetFirstRec() CLASS ZDBFMEMINDEX
If Len(::aIndexData) > 0
	::nCurrentRow := 1
	Return ::aIndexData[::nCurrentRow][2]
Endif
Return 0

// ----------------------------------------
// Retorna o RECNO anterior da ordem atual 
// Caso já esieta no primeiro registro ou 
// nao tenha dados, retorna zero

METHOD GetPrevRec() CLASS ZDBFMEMINDEX
If Len(::aIndexData) > 0 .and. ::nCurrentRow > 1
	::nCurrentRow--
	Return ::aIndexData[::nCurrentRow][2]
Endif
Return 0

// ----------------------------------------
// Retorna o próximo RECNO da ordem atual 
// Caso nao tenha dados ou tenha chego em EOF 
// retorna zero 

METHOD GetNextRec() CLASS ZDBFMEMINDEX
If ::nCurrentRow < Len(::aIndexData)
	::nCurrentRow++
	Return ::aIndexData[::nCurrentRow][2]
Endif
Return 0 

// ----------------------------------------
// Retorna o numero do ultimo RECNO da ordem atual 
// Caso nao tenha dados retorna zero 

METHOD GetLastRec() CLASS ZDBFMEMINDEX
If Len(::aIndexData) > 0
	::nCurrentRow := Len(::aIndexData)
	Return ::aIndexData[::nCurrentRow][2]
Endif
Return 0

// ----------------------------------------
// Retorna a expressao de indice original 
METHOD GetIndexExpr() CLASS ZDBFMEMINDEX
return ::cIndexExpr


// ----------------------------------------
// REtorna o valor da chave de indice do registro atual 
// Em AdvPL, seria o equivalente a &(Indexkey())

METHOD GetIndexValue() CLASS ZDBFMEMINDEX
Return Eval( ::bIndexBlock , ::oDBF )

// ----------------------------------------
// Realiza uma busca exata pela chave de indice informada 
// Leva em consideração chaves repetidas buscando 
// pelo menor RECNO neste caso 

METHOD IndexSeek(cSeekKey) CLASS ZDBFMEMINDEX
Local nTop := 1 
Local nBottom := Len(::aIndexData)
Local nMid 
Local lFound := .F. 

If nBottom > 0

	If cSeekKey < ::aIndexData[1][1]
		// Chave de busca é menor que a primeira chave do indice
		Return 0
	Endif

	If cSeekKey > aTail(::aIndexData)[1]
		// Chave de busca é maior que a última chave
		Return 0
	Endif

	While nBottom >= nTop

		// Procura o meio dos dados ordenados
		nMid := Int( ( nTop + nBottom ) / 2 )
		
		If cSeekKey < ::aIndexData[nMid][1]
			// Chave menor, desconsidera daqui pra baixo 
			nBottom := nMid-1
		ElseIf cSeekKey > ::aIndexData[nMid][1]
			// Chave maior, desconsidera daqui pra cima
			nTop := nMid+1
		Else
			// Nao é maior nem menor .. achou ! 
			lFound := .T. 
			EXIT
		Endif
	
	Enddo

	If lFound

		// Ao encontrar uma chave, busca pelo menor RECNO 
		// entre chaves repetidas

		While cSeekKey == ::aIndexData[nMid][1]
			nMid--
			If nMid == 0 
				EXIT
			Endif
		Enddo

		Return ::aIndexData[nMid+1][2]

	Endif
	
Endif

Return 0

// ----------------------------------------
// Fecha o indice aberto 
// limpa flags e dados da memoria

METHOD CLOSE() CLASS ZDBFMEMINDEX
::oDBF := NIL
::cIndexExpr := ''
::bIndexBlock := NIL
::nCurrentRow := 0

aSize( ::aIndexData,0 )

Return

