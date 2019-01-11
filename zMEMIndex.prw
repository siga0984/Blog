#include 'protheus.ch'

/* ==================================================

Classe      ZMEMINDEX
Autor       Julio Wittwer
Data        05/01/2019
Descrição   A partir de um objeto ZISAMFILE, permite 
            a criação de um índice em memória 

================================================== */

CLASS ZMEMINDEX FROM LONGNAMECLASS

   DATA oDBF			// Objeto ZISAMFILE relacionado ao índice 
   DATA cIndexExpr      // Expressão AdvPL original do índice
   DATA bIndexBlock     // CodeBlock para montar uma linha de dados do índice
   DATA aIndexData      // Array com os dados do índice ordenado pela chave 
   DATA aRecnoData      // Array com os dados do índice ordenado pelo RECNO 
   DATA nCurrentRow     // Numero da linha atual do índice 
   DATA lSetResync      // Flag de resincronismo pendente da posição do índice
   DATA lVerbose        // Modo Verbose (echo em console ligado)

   METHOD NEW(oDBF)     // Cria o objeto do índice
   METHOD CREATEINDEX(cIndexExpr) // Cria o índice baseado na chave fornecida 
   METHOD CLOSE()       // Fecha o índice e limpa os dados da memória 

   METHOD GetFirstRec() // Retorna o RECNO do primeiro registro do índice
   METHOD GetPrevRec()  // Retorna o RECNO do Registro anterior do índice
   METHOD GetNextRec()  // Retorna o RECNO do próximo registro do índice
   METHOD GetLastRec()  // Retorna o RECNO do último registro do índice 
   
   METHOD GetIndexExpr()  // Rertorna a expressão de indexação 
   METHOD GetIndexValue() // Retorna o valor da chave de indice do registro atual 
   METHOD GetIndexRecno() // REtorna o numero do RECNO da posição do índice atual 
   METHOD IndexSeek()     // Realiza uma busca ordenada por um valor informado 
   METHOD RecordSeek()    // REaliza uma busca no indice pelo RECNO 
   METHOD UpdateKey()     // Atualiza uma chave de indice ( em implementação ) 
   
   METHOD CheckSync()    // Verifica a necessidade de sincronizar o indice 
   METHOD SetResync()     // Seta flag de resincronismo pendente
   METHOD SetVerbose()    // Seta modo verbose com echo em console ( em implementação 
   
ENDCLASS

// ----------------------------------------
// Construtor do indice em memoria
// Recebe o objeto da tabela 

METHOD NEW(oDBF) CLASS ZMEMINDEX
::oDBF := oDBF
::cIndexExpr := ''
::bIndexBlock := NIL
::aIndexData := {}
::aRecnoData := {}
::nCurrentRow := 0
::lSetResync := .F. 
::lVerbose   := .F. 
Return self

// ----------------------------------------
// Chamado pela ZISAMFILE para indicar que o registro ou chave atuais 
// precisam ser resincronizados devido a mudança de indice ativo
// ou reposicionamento de registro direto por DBGoto()
METHOD SetResync() CLASS ZMEMINDEX
::lSetResync := .T. 
Return      

// ----------------------------------------
// Permite ligar ou desligar o modo verbose da classe de indice
METHOD SetVerbose( lSet ) CLASS ZMEMINDEX
::lVerbose := lSet
Return


// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Verifica se existe sincronismo pendente antes de fazer uma movimentacao 
// Caso tenha, efetua o sincronismo da posicao do indice com a posicao do RECNO 

METHOD CheckSync() CLASS ZMEMINDEX
Local nRecno

If ::lSetResync

	If ::oDBF:Eof()
		// Nao posso sincronizar em EOF()
		Return
	Endif

	// Desliga flag de resync
	::lSetResync := .F. 

	// Pega o numero do RECNO atual do DBF 
	nRecno := ::oDBF:Recno()

	IF  ::aIndexData[::nCurrentRow][2] != nRecno
	   
		// Se o RECNO da posicao de indice nao está sincronizado,
		// Busca pela posicao correta do indice de addos ordenados
		// correspondente ao RECNO atual

		::nCurrentRow := ::RecordSeek(nRecno)
        
		If ::nCurrentRow <= 0 
			UserException("*** INDEX RESYNC FAILED - RECNO "+cValToChar(nRecno)+" ***")
		Endif
		
	Endif

Endif

Return

// ----------------------------------------
// Cria um indice na memoria usando a expressao 
// enviada como parametro

METHOD CREATEINDEX( cIndexExpr ) CLASS ZMEMINDEX
Local cIndexBlk

// Guarda a expressão original do indice
::cIndexExpr := cIndexExpr

// Monta o CodeBlock para a montagem da linha de dados
// com a chave de indice
cIndexBlk := ::oDbf:_BuildFieldBlock( cIndexExpr )

// Faz a macro da Expressao 
::bIndexBlock := &(cIndexBlk)

// Agora varre a tabela montando o o set de dados para criar o índice
::aIndexData := {}
::aRecnoData := {}

// Coloca a tabela em ordem de regisrtros para a criação do indice
::oDBF:SetOrder(0)
::oDBF:ClearFilter()
::oDBF:GoTop()

While !::oDBF:Eof()
	// Array de dados 
	// [1] Chave do indice
	// [2] RECNO
	// [3] Numero do elemento do array aIndexData que contém este RECNO
	aadd( ::aIndexData , { Eval( ::bIndexBlock , ::oDBF ) , ::oDBF:Recno() , NIL } )
	::oDBF:Skip()
Enddo

// Sorteia pela chave de indice, usando o RECNO como criterio de desempate
// Duas chaves iguais, prevalesce a ordem fisica ( o menor recno vem primeiro )
aSort( ::aIndexData ,,, { |x,y| ( x[1] < y[1] ) .OR. ( x[1] == y[1] .AND. x[2] < y[2] ) } )

// Guardando a posicao do array ordenado pelos dados na terceira coluna do array 
aEval( ::aIndexData , {| x,y| x[3] := y })

// Agora, eu preciso tambem de um indice ordenado por RECNO 
// Porem fazendo referencia a todos os elementos do array, mudandi apenas a ordenação 

// Para fazer esta magica, cria um novo array, referenciando 
// todos os elementos do array principal , então ordena
// este array pelo RECNO
::aRecnoData := Array(len(::aIndexData))
aEval(::aIndexData , {|x,y| ::aRecnoData[y] := x })
aSort( ::aRecnoData ,,, { |x,y| x[2] < y[2] } )

Return .T.

// ----------------------------------------
// Retorna o primeiro RECNO da ordem atual 
// Caso nao tenha dados, retorna zero

METHOD GetFirstRec() CLASS ZMEMINDEX
If Len(::aIndexData) > 0
	::nCurrentRow := 1
	Return ::aIndexData[::nCurrentRow][2]
Endif
Return 0

// ----------------------------------------
// Retorna o RECNO anterior da ordem atual 
// Caso já esieta no primeiro registro ou 
// nao tenha dados, retorna zero

METHOD GetPrevRec() CLASS ZMEMINDEX
If Len(::aIndexData) > 0 .and. ::nCurrentRow > 1
	::CheckSync()
	::nCurrentRow--
	Return ::aIndexData[::nCurrentRow][2]
Endif
Return 0

// ----------------------------------------
// Retorna o próximo RECNO da ordem atual 
// Caso nao tenha dados ou tenha chego em EOF 
// retorna zero 

METHOD GetNextRec() CLASS ZMEMINDEX
If ::nCurrentRow < Len(::aIndexData)
	::CheckSync()
	::nCurrentRow++
	Return ::aIndexData[::nCurrentRow][2]
Endif
Return 0 

// ----------------------------------------
// Retorna o numero do ultimo RECNO da ordem atual 
// Caso nao tenha dados retorna zero 

METHOD GetLastRec() CLASS ZMEMINDEX
If Len(::aIndexData) > 0
	::nCurrentRow := Len(::aIndexData)
	Return ::aIndexData[::nCurrentRow][2]
Endif
Return 0

// ----------------------------------------
// Retorna a expressao de indice original 
METHOD GetIndexExpr() CLASS ZMEMINDEX
return ::cIndexExpr

// ----------------------------------------
// REtorna o valor da chave de indice do registro atual 
// Que a tabela esta posicionada 
// Em AdvPL, seria o equivalente a &(Indexkey())

METHOD GetIndexValue() CLASS ZMEMINDEX
Return Eval( ::bIndexBlock , ::oDBF )

// ----------------------------------------
// REtorna o numero do RECNO da posição de indice atual 

METHOD GetIndexRecno() CLASS ZMEMINDEX
Return ::aIndexData[::nCurrentRow][2]

// ----------------------------------------
// Um registro do dbf foi alterado. 
// Preciso ver se houve alteração nos valores dos campos chave de indice 
// Caso tenha havido, preciso remover a antiga e inserir a nova 
// na ordem certa. 

METHOD UpdateKey() CLASS ZMEMINDEX
Local cKeyDBF, nRecDBF 
Local cKeyIndex , nRecIndex

// Valores da chave atual do DBF 
cKeyDBF := ::GetIndexValue()
nRecDBF := ::oDBF:Recno()

// Valores da chave atual do indice 
cKeyIndex := ::aIndexData[::nCurrentRow][1]
nRecIndex := ::aIndexData[::nCurrentRow][2]

IF nRecDBF == nRecIndex
	IF cKeyDBF == cKeyIndex
		// Nenhum campo chave alterado 
		// Nada para fazer 
		Return
	Endif
Endif

// [TODO] Atualização de campo chave em 
// inclusao ou alteração de registro com 
// o indice aberto 

conout("")
conout("*** PENDING ZMEMINDEX::UpdateKey() ***")
conout("... DBF KeyValue   ["+cValToChar(cKeyDBF)+"]")
conout("... DBF Recno      ["+cValToChar(nRecDBF)+"]")
conout("... Index KeyValue ["+cValToChar(cKeyIndex)+"]")
conout("... Index Recno    ["+cValToChar(nRecIndex)+"]")
conout("")

UserException("*** UPDATEKEY() ON ZMEMINDEX NOT AVAILABLE YET ***")

Return

// ----------------------------------------
// Realiza uma busca exata pela chave de indice informada 
// Leva em consideração chaves repetidas buscando 
// sempre a com menor RECNO 

METHOD IndexSeek(cSeekKey) CLASS ZMEMINDEX
Local nTop := 1 
Local nBottom := Len(::aIndexData)
Local nMiddle 
Local lFound := .F. 

If nBottom > 0

	If cSeekKey < ::aIndexData[nTop][1]
		// Chave de busca é menor que a primeira chave do indice
		Return 0
	Endif

	If cSeekKey > ::aIndexData[nBottom][1]
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
			lFound := .T. 
			EXIT
		ElseIf cSeekKey < ::aIndexData[nMiddle][1]
			// Chave menor, desconsidera daqui pra baixo 
			nBottom := nMiddle-1
		ElseIf cSeekKey > ::aIndexData[nMiddle][1]
			// Chave maior, desconsidera daqui pra cima
			nTop := nMiddle+1
		Endif
	
	Enddo

	If lFound
		
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

		// Retorna o RECNO correspondente a esta chave 
		Return ::aIndexData[::nCurrentRow][2]
		
	Endif
		
Endif

Return 0


// ----------------------------------------
// Realiza uma busca ordenada pelo RECNO no indice 
// retorna a posicao do array de dados ordenado 
// ( aIndexData ) que aponta para este RECNO

METHOD RecordSeek(nRecno) CLASS ZMEMINDEX
Local lFound := .F. 
Local nTop := 1 
Local nBottom := Len(::aRecnoData)
Local nMiddle 

If nBottom > 0

	If nRecno < ::aRecnoData[nTop][2]
		// Chave de busca é menor que a primeira chave do indice
		Return 0
	Endif

	If nRecno > ::aRecnoData[nBottom][2]
		// Chave de busca é maior que a última chave
		Return 0
	Endif

	While nBottom >= nTop

		// Procura o meio dos dados ordenados
		nMiddle := Int( ( nTop + nBottom ) / 2 )

		If ::aIndexData[nMiddle][2] == nRecno
			// Achou 
			lFound := .T. 
			EXIT
		ElseIf nRecno < ::aRecnoData[nMiddle][2]
			// RECNO menor, desconsidera daqui pra baixo 
			nBottom := nMiddle-1
		ElseIf nRecno > ::aRecnoData[nMiddle][2]
			// RECNO maior, desconsidera daqui pra cima
			nTop := nMiddle+1
		Endif
	
	Enddo

	If lFound
		// Retorna a posição do array de dados 
		// ordenados (aIndexData) que contem este RECNO 
		Return ::aRecnoData[nMiddle][3]
	Endif
	
Endif

Return 0


// ----------------------------------------
// Fecha o indice aberto 
// limpa flags e dados da memoria

METHOD CLOSE() CLASS ZMEMINDEX

::oDBF := NIL
::cIndexExpr := ''
::bIndexBlock := NIL
::nCurrentRow := 0
::lSetResync := .F. 

// Zera os arrays ordenados pela CHAVE de Indice e pelo RECNO 
aSize( ::aIndexData,0 )
aSize( ::aRecnoData,0 )

Return

