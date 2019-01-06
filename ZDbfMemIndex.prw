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
   DATA lSetResync

   METHOD NEW(oDBF)
   METHOD CREATEINDEX(cIndexExpr)
   METHOD CLOSE()

   METHOD GetFirstRec()
   METHOD GetPrevRec()
   METHOD GetNextRec()
   METHOD GetLastRec()
   
   METHOD GetIndexExpr()
   METHOD GetIndexValue()
   METHOD GetIndexRecno()
   METHOD IndexSeek()
   
   METHOD _BuildIndexBlock(cIndexExpr)
   METHOD _CheckSync()
   METHOD SetResync()

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
::lSetResync := .F. 
Return self

// ----------------------------------------
// Chamado pela ZDBFTABLE para indicar que o registro ou chave atuais 
// precisam ser resincronizados devido a mudança de indice ativo
// ou reposicionamento de registro direto por DBGoto()
METHOD SetResync() CLASS ZDBFMEMINDEX
::lSetResync := .T. 
Return

// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Verifica se existe sincronismo pendente antes de fazer uma movimentacao 
// Caso tenha, efetua o sincronismo da posicao do indice com a posicao do RECNO 

METHOD _CheckSync() CLASS ZDBFMEMINDEX
Local cKey, nKey 

If ::lSetResync

	// Desliga flag de resync
	::lSetResync := .F. 

	// Pega o valor da chave baseado no RECNO posicionado no DBF
	// e o numero do recno posicionado no DBF 
	cKey := ::GetIndexValue()
	nKey := ::oDBF:Recno()

	IF  ::aIndexData[::nCurrentRow][2] != nKey .OR. ;
		::aIndexData[::nCurrentRow][1] != cKey
	   
		// Se a chave da posicao do indice ou o RECNO 
		// da posicao do indice estao diferentes, 
		// precisa resincronizar a posical do indice

		If !::IndexSeek(cKey,nKey)		

		    // Uma sincronizacao de indice NUNCA pode falhar
			// Eu estou buscando no indice a chave atualmente 
			// posicionada no arquivo. Se eu nao achei, 
			// o indice nao esta sincronizado com o arquivo 
			
			UserException("*** INDEX RESYNC FAILED ***")
		Endif
		
	Endif

Endif

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
	::_CheckSync()
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
	::_CheckSync()
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
// Que a tabela esta posicionada 
// Em AdvPL, seria o equivalente a &(Indexkey())

METHOD GetIndexValue() CLASS ZDBFMEMINDEX
Return Eval( ::bIndexBlock , ::oDBF )

// ----------------------------------------
// REtorna o numero do RECNO da posição de indice atual 

METHOD GetIndexRecno() CLASS ZDBFMEMINDEX
Return ::aIndexData[::nCurrentRow][2]


// ----------------------------------------
// Realiza uma busca exata pela chave de indice informada 
// Leva em consideração chaves repetidas buscando 
// pelo menor RECNO neste caso. Caso tenha recebido um RECNO
// está sendo chamado para fazer sincronismo da chave

METHOD IndexSeek(cSeekKey,nRecno) CLASS ZDBFMEMINDEX
Local nTop := 1 
Local nBottom := Len(::aIndexData)
Local nMiddle 
Local lFound := .F. 

IF nRecno = NIL 
	nRecno := 0 
Endif

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
		nMiddle := Int( ( nTop + nBottom ) / 2 )

		If ::aIndexData[nMiddle][1] = cSeekKey
		
			// Operador de igualdade ao comparar a chave do indice 
			// com a chave informada para Busca. O Advpl opr default 
			// considera que ambas sao iguais mesmo que a chave de busca
			// seja menor, desde que os caracteres iniciais até o tamanho da 
			// chave de busca sejam iguais. 
		
			If nRecno > 0  
				// Foi informado RECNO 
				// Sincronismo do indice com o registro 
				// posicionado na tabela. Busca o RECNO 
				If nRecno < ::aIndexData[nMiddle][2]
					// RECNO menor, desconsidera daqui pra baixo 
					nBottom := nMiddle-1
				ElseIf nRecno > ::aIndexData[nMiddle][2]
					// RECNO maior, desconsidera daqui pra cima
					nTop := nMiddle+1
				Else    
					// ACHOU a chave completa 
					lFound := .T. 
					EXIT
				Endif
			Else
				// Nao é maior nem menor .. achou ! 
				lFound := .T. 
				EXIT
			Endif

		ElseIf cSeekKey < ::aIndexData[nMiddle][1]
			// Chave menor, desconsidera daqui pra baixo 
			nBottom := nMiddle-1
		ElseIf cSeekKey > ::aIndexData[nMiddle][1]
			// Chave maior, desconsidera daqui pra cima
			nTop := nMiddle+1
		Endif
	
	Enddo

	If lFound

		If nRecno > 0 

			// Foi informado RECNO 
			// Busca para sincronizar o indice baseado no movimento 
			// direto do registro na tabela ( dbgoto por exemplo ) 
			// A chave procurada é essa mesmo. 
			::nCurrentRow := nMiddle
		
		Else
		
			// Nao foi informado RECNO 
			// Ao encontrar uma chave, busca pelo menor RECNO 
			// entre chaves repetidas, do ponto atual para cima
			// enquanto a chave de busca for a mesma. 
			// Compara sempre a chave do indice com a chave de busca
			// com igualdade simples 
	
			While ::aIndexData[nMiddle][1] = cSeekKey
				nMiddle--
				If nMiddle == 0 
					EXIT
				Endif
			Enddo

			// A posicao encontrada é a próxima, onde a 
			// chave ainda era igual 
			::nCurrentRow := nMiddle+1

		Endif

		Return ::aIndexData[::nCurrentRow][2]

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
::lSetResync := .F. 

aSize( ::aIndexData,0 )

Return

