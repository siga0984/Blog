#include 'Protheus.ch'

STATIC _aMemFiles := {}

/* ===========================================================================

Classe		ZMEMFILE
Autor		Júlio Wittwer
Data		01/2019

Descrição   Classe de persistencia de arquivos em memória 

Versao		1.0 
			-- Arquivos em memória não são compartilhados entre Threads
			
Observação  A classe usa a implementação de indices em memoria 

=========================================================================== */

CLASS ZMEMFILE FROM LONGNAMECLASS

  DATA cMemFile			    // Nome / Identificador do arquivo de dados na memória 
  DATA lOpened              // Indica se o arquivo está aberto 

  DATA aFileData			// Array com os registros do arquivo 
  DATA aStruct		   		// Array com a estrutura do arquivo -- ( DBF LIKE )

  DATA dLastUpd				// Data registrada dentro do arquivo como ultimo UPDATE 
  DATA nLastRec				// Ultimo registro do arquivo - Total de registros
  DATA nRecLength			// Tamanho de cada registro 
  DATA lHasMemo				// Tabela possui campo MEMO ?
  DATA nFldCount			// Quantidade de campos do arquivo 
  DATA aGetRecord			// Array com todas as colunas do registro atual 
  DATA aPutRecord           // Array com campos para update 
  DATA lExclusive           // Arquivo aberto em modo exclusivo ?
  DATA lCanWrite            // Arquivo aberto para gravacao 
  DATA lUpdPend             // Flag indicando update pendente 
  DATA lSetDeleted          // Filtro de registros deletados ativo 
  DATA nRecno				// Número do registro (RECNO) atualmnete posicionado 
  DATA bFilter              // Codeblock de filtro 
  DATA lVerbose             // Classe DBF em modo "verbose" com echo no console

  DATA nIndexOrd            // Ordem de indice atual 
  DATA aIndexes             // Array com objetos de indice 
  DATA oCurrentIndex        // Objeto do indice atual 

  DATA lBOF					// Flag de inicio de arquivo 
  DATA lEOF					// Flag de final de arquivo 
  
    	
  DATA nLastError			// Ultimo erro ocorrido 
  DATA cLastError			// Descrição do último erro 

  // ========================= Metodos de uso público da classe

  METHOD NEW(cFile)			// Construtor 
  METHOD OPEN()				// Abertura da tabela 
  METHOD CLOSE()			// Fecha a tabela 
  METHOD EXISTS()           // Verifica se a tabela existe 
  METHOD CREATE()           // Cria a tabela no disco 

  METHOD Lastrec()			// Retorna o total de registros / numero do ultimo registro da tabela 
  METHOD RecCount()			// Retorna o total de registros / numero do ultimo registro da tabela 
  METHOD GetStruct()		// Retorna CLONE da estrutura de dados da tabela 
  METHOD GoTo(nRec)		    // Posiciona em um registro informado. 
  METHOD GoTop()			// Posiciona no RECNO 1 da tabela 
  METHOD GoBottom()   	    // Posiciona em LASTREC da tabela 
  METHOD Skip( nQtd )       // Navega para frente ou para tráz uma quantidade de registros 
  METHOD FieldGet( nPos )   // Recupera o conteudo da coluna informada do registro atual 
  METHOD FieldPut( nPos )   // Faz update em uma coluna do registro atual 
  METHOD FieldName( nPos )	// Recupera o nome da coluna informada 
  METHOD FieldPos( cField ) // Retorna a posicao de um campo na estrutura da tabela ( ID da Coluna )
  METHOD FileName()         // Retorna nome do arquivo aberto 
  METHOD FCount()           // Retorna o numero de campo / colunas da tabela
  METHOD BOF()				// Retorna .T. caso tenha se tentado navegar antes do primeiro registro 
  METHOD EOF()				// Retorna .T, caso o final de arquivo tenha sido atingido 
  METHOD Recno()			// Retorna o numero do registro (RECNO) posicionado 
  METHOD Deleted()			// REtorna .T. caso o registro atual esteja DELETADO ( Marcado para deleção ) 
  METHOD SetFilter()        // Permite setar um filtro para os dados 
  METHOD ClearFilter()      // Limpa o filtro 
  METHOD SetDeleted()       // Liga ou desliga filtro de registros deletados
  
  METHOD Insert()           // Insere um registro em branco no final da tabela
  METHOD Update()           // Atualiza o registro atual na tabela 

  METHOD SetOrder()         // Seta um indice / ordem ativa 
  METHOD IndexOrd()         // Retorna a ordem ativa
  METHOD IndexKey()         // Retorna a expressao de indice ativa 
  METHOD IndexValue()       // Retorna o valor da chave de indice do registro atual 
  METHOD Seek(cKeyExpr)     // Realiza uma busca usando o indice ativo 
  METHOD CreateIndex()      // Cria um Indice ( em memoria ) para a tabela 

  METHOD Header() 			// Retorna tamanho em Bytes do Header da Tabela
  METHOD FileSize()         // Retorna o tamanho ocupado pelo arquivo em bytes 
  METHOD RecSize()			// Retorna o tamanho de um registro da tabela 
  METHOD LUpdate()			// Retorna a data interna do arquivo que registra o ultimo update 
 
  METHOD GetError() 		// Retorna o Codigo e Descricao por referencia do ultimo erro 
  METHOD GetErrorCode()     // Retorna apenas oCodigo do ultimo erro ocorrido
  METHOD GetErrorStr()		// Retorna apenas a descrição do último erro ocorrido
  METHOD SetVerbose()       // Liga ou desliga o modo "verbose" da classe

  // ========================= Metodos de uso interno da classe

  METHOD _ResetError()		// Limpa a ultima ocorrencia de erro 
  METHOD _SetError()        // Seta uma nova ocorrencia de erro 
  METHOD _InitVars() 		// Inicializa propriedades do Objeto, no construtor e no CLOSE
  METHOD _ReadStruct()		// Lê a estrutura do arquivo de dados 
  METHOD _ReadRecord()		// Le um registro do arquivo de dados
  METHOD _ClearRecord()		// Limpa o registro da memoria (EOF por exemplo) 
  METHOD _CheckFilter()     // Verifica se o registro atual está contemplado no filtro 
  METHOD _SkipNext()		// Le o proximo registro da tabela 
  METHOD _SkipPrev()        // Le o registro anterior da tabela 

ENDCLASS

// ----------------------------------------------------------
// Construtor do objeto DBF 
// Apenas recebe o nome do arquivo e inicializa as propriedades

METHOD NEW(cFile) CLASS ZMEMFILE 

::_InitVars() 
::cMemFile   := lower(cFile)

Return self


// ----------------------------------------------------------
// Abertura da tabela -- READ ONLE 
// Caso retorne .F. , consulte o ultimo erro usando GetErrorStr() / GetErrorCode()
// Por hora apenas a abertura possui tratamento de erro 

METHOD OPEN(lExclusive,lCanWrite) CLASS ZMEMFILE 
Local nFMode := 0

::_ResetError()

If ::lOpened
	::_SetError(-1,"File Already Open")
	Return .F.
Endif

IF !::Exists()
	::_SetError(-6,"Unable to OPEN - MEM File ["+::cMemFile+"] DOES NOT EXIST")
	Return .F.
Endif

If lExclusive = NIL ; 	lExclusive := .F. ; Endif
If lCanWrite = NIL ; 	lCanWrite := .F.  ; Endif

// Por enquanto faz escrita apenas em modo exclusivo
If lCanWrite .AND. !lExclusive
	::_SetError(-6,"Unable to OPEN for WRITE in SHARED MODE -- Use Exclusive mode or OPEN FOR READ")
	Return .F.
Endif

// Atualiza propriedades de controle da classe
::lExclusive   := lExclusive
::lCanWrite    := lCanWrite

If !::_ReadStruct()
	// Em caso de falha na leitura da estrutura 
	Return .F.
Endif

// Cria o array de campos do registro atual 
// Aloca uma coluna a mais para o Flag de deletado 
::aGetRecord := Array(::nFldCount+1)
::aPutRecord := Array(::nFldCount+1)

// Seta que o arquivo está aberto 
::lOpened := .T. 

// Vai para o topo do arquivo 
// e Lê o primeiro registro físico 
::GoTop()

Return .T. 


// ----------------------------------------------------------
// Fecha a tabela aberta 
// Limpa as variaveis de controle. 
// A tabela pode ser aberta novamente pela mesma instancia 

METHOD CLOSE() CLASS ZMEMFILE 
Local nI
Local nPos

// Localiza a estrutura da tabela 
// e some com ela 
nPos := ascan(_aMemFiles,{|x| x[1] == ::cMemFile })
If nPos > 0 
	_aMemFiles[nPos] := NIL
	aDel(_aMemFiles,nPos)
	aSize(_aMemFiles,len(_aMemFiles)-1)
Endif

// Ao fechar, evapora com os dados da tabela 
aSize( ::aFileData , 0 ) 

// Fecha também, todos os indices 
For nI := 1 to len(::aIndexes)
	::oCurrentIndex := ::aIndexes[nI]
	::oCurrentIndex:Close()
	FreeObj(::oCurrentIndex)
Next

// Limpa as propriedades
::_InitVars()


Return 


// ----------------------------------------------------------\
// Verifica se a tabela existe na memoria
METHOD EXISTS() CLASS ZMEMFILE 
Local nPos
nPos := ascan(_aMemFiles,{|x| x[1] == ::cMemFile })
Return nPos > 0 


// ----------------------------------------------------------\
// Cria a tabela no disco 
// O nome já foi recebido no construtor 
// Recebe a estrutura e a partir dela cria a tabela 

METHOD CREATE( aStru ) CLASS ZMEMFILE
Local nFields := 0
Local nI

If ::EXISTS()
	::_SetError(-7,"CREATE ERROR - File Already Exists")
Endif

If ::lOpened
	::_SetError(-8,"CREATE ERROR - File Already Opened")
Endif

// Valida a estrutura informada
nFields := len(aStru)
For nI := 1 to nFields
	If !aStru[nI][2]$"CNDLM"
		UserException("CREATE ERROR - INVALID FIELD TYPE "+aStru[nI][2]+ " ("+aStru[nI][1]+")" )
	Endif
	// Apenas Ajusta nome do campo 
	aStru[nI][1] := Upper(padr(aStru[nI][1],10))
Next

// Guarda a estrutura do arquivo em array estático 
// Visivel apenas na Thread / Processo atual 
aadd( _aMemFiles , { ::cMemFile , aClone(aStru) }  )

Return .T. 

// ----------------------------------------------------------
// Permite setar um filtro para a navegação de dados 
// Todos os campos devem estar em letras maiusculas 

/*

X3_ARQUIVO => [AA1]
X3_ORDEM   => [01]
X3_CAMPO   => [AA1_FILIAL]
X3_TIPO    => [C]
X3_TAMANHO => [2]
X3_DECIMAL => [0]
X3_TITULO  => [Filial      ]
X3_TITSPA  => [Sucursal    ]
X3_TITENG  => [Branch      ]

*/


METHOD SetFilter( cFilter ) CLASS ZMEMFILE
Local aCampos := {}
Local cTemp
Local nI, nPos

If !::lOpened
	::_SetError(-8,"SETFILTER ERROR - File Not Opened")
	Return .F.
Endif

// Cria lista de campos 
aEval( ::aStruct , {|x| aadd(aCampos , x[1]) } )

// Ordena pelos maiores campos primeiro 
aSort( aCampos ,,, {|x,y| alltrim(len(x)) > alltrim(len(y)) } )

// Copia a expressao filtro
cTemp := cFilter

// Troca os campos por o:Fieldget(nCpo)
// Exemplo : CAMPO > 123 será trocado para o:FieldGet(1) > 123

For nI := 1 to len(aCampos)
	cCampo := alltrim(aCampos[nI])
	nPos := ::Fieldpos(cCampo)
	cTemp := Strtran( cTemp , cCampo,"o:FieldGet(" +cValToChar(nPos)+ ")")
Next

// Monta a string com o codeblock para filtro 
cTemp := "{|o| "+cTemp+"}"

// Monta efetivamente o codeblock 
::bFilter := &(cTemp)

Return .T. 

// ----------------------------------------------------------
// Limpa a expressao de filtro atual 

METHOD ClearFilter() CLASS ZMEMFILE 
::bFilter := NIL
Return

// ----------------------------------------------------------
// Permite ligar filtro de navegação de registros deletados
// Defaul = desligado

METHOD SetDeleted( lSet ) CLASS ZMEMFILE 
Local lOldSet := ::lSetDeleted
If pCount() > 0 
	::lSetDeleted := lSet
Endif
Return lOldSet


// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Inicializa / Limpa as propriedades padrao do Objeto 

METHOD _InitVars() CLASS ZMEMFILE 

::aFileData   := {}
::lOpened     := .F. 
::cLastError  := ''
::nLastError  := 0 
::nLastRec    := 0
::lBof        := .F. 
::lEof        := .F. 
::lHasMemo    := .F. 
::lExclusive  := .F. 
::lCanWrite   := .T. 
::aStruct     := {}
::dLastUpd    := ctod("")
::nFldCount   := 0
::aGetRecord  := {}
::aPutRecord  := {}
::lUpdPend    := .F. 

::lVerbose    := .F. 
::lSetDeleted := .F. 
::nRecno      := 0
::bFilter     := NIL
::nIndexOrd   := 0
::aIndexes    := {}
::oCurrentIndex := {}

Return

// ----------------------------------------------------------
// Retorna o código do ultimo erro e a descrição por referencia

METHOD GetError( cRefError ) CLASS ZMEMFILE 
cRefError := ::cLastError
Return ::nLastError

// ----------------------------------------------------------
// Retorna apenas o código do ultimo erro 

METHOD GetErrorCode() CLASS ZMEMFILE 
Return ::nLastError

// ----------------------------------------------------------
// Retorna apenas a descrição do ultimo erro 

METHOD GetErrorStr() CLASS ZMEMFILE 
Return ::cLastError

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Limpa o registro do ultimo erro 

METHOD _ResetError() CLASS ZMEMFILE 
::cLastError := ''
::nLastError := 0 
Return

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Seta uma nova ocorrencia de erro

METHOD _SetError(nCode,cErrorMsg) CLASS ZMEMFILE 
::cLastError := cErrorMsg
::nLastError := nCode
Return

// ----------------------------------------------------------
// Retorna a data do ultimo update feito no arquivo 

METHOD LUPDATE() CLASS ZMEMFILE 
Return ::dLastUpd

// ----------------------------------------------------------
// Retorna o numero do ultimo registro da tabela 

METHOD Lastrec() CLASS ZMEMFILE 
Return ::nLastRec

// ----------------------------------------------------------
// Colocado apenas por compatibilidade 
// 

METHOD Reccount() CLASS ZMEMFILE 
Return ::nLastRec

// ----------------------------------------------------------
// Retorna um clone do Array da estrutura da tabela 

METHOD GetStruct() CLASS ZMEMFILE 
Return aClone( ::aStruct )

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Lê a estrutura de campos da tabela 

METHOD _ReadStruct() CLASS ZMEMFILE 
Local nPos

nPos := ascan(_aMemFiles,{|x| x[1] == ::cMemFile })

If nPos > 0 
	::aStruct := aClone(_aMemFiles[nPos][2])
	::nFldCount := len(::aStruct)
	Return .T. 
Endif

Return .F. 

// ----------------------------------------------------------
// Posiciona diretamente em um regsitro 

METHOD GoTo(nRec)  CLASS ZMEMFILE

// Verifica se o registro é válido 
// Se não for, vai para EOF

If nRec > ::nLastRec .or. nRec < 1
	::lEOF := .T.
	::_ClearRecord()
	Return
Endif

// ----------------------------------------
// Atualiza o numero do registro atual 
::nRecno := nRec

If ::nIndexOrd > 0 
	// Eu tenho indice ativo, sincroniza a posicao do indice 
	// com a posicao do registro atual 
	::oCurrentIndex:SetResync()
Endif

// Traz o registro atual para a memória
::_ReadRecord()

Return

// ----------------------------------------------------------
// Movimenta a tabela para o primeiro registro 
// Release 20190105 : Contempla uso de indice

METHOD GoTop() CLASS ZMEMFILE 

IF ::nLastRec == 0 
	// Nao há registros 
	::lBOF := .T. 
	::lEOF := .T. 
	::nRecno   := 0
	::_ClearRecord()
	Return
Endif

If ::nIndexOrd > 0 
	// Se tem indice ativo, pergunta pro indice
	// quanl é o primeiro registro da ordem 
	::nRecno := ::oCurrentIndex:GetFirstRec()
Else
	// Ordem fisica 
	// Atualiza para o primeiro registtro 
	::nRecno     := 1
Endif

// Traz o registro atual para a memória
::_ReadRecord()

If ( !::_CheckFilter() )
	// Nao passou na verificacao do filtro
	// busca o proximo registro que atenda
	::_SkipNext()
Endif

Return

// ----------------------------------------------------------
// Movimenta a tabela para o último registro

METHOD GoBottom() CLASS ZMEMFILE 

IF ::nLastRec == 0 
	// Nao há registros 
	::lBOF := .T. 
	::lEOF := .T. 
	::nRecno   := 0
	::_ClearRecord()
	Return
Endif

If ::nIndexOrd > 0 
	// Se tem indice ativo, pergunta pro indice
	// quanl é o primeiro registro da ordem 
	::nRecno := ::oCurrentIndex:GetLastRec()
Else
	// Ordem fisica 
	// Atualiza o RECNO para o ultimo registro 
	::nRecno     := ::nLastRec
Endif

// Traz o registro atual para a memória
::_ReadRecord()

If ( !::_CheckFilter() )
	// Nao passou na verificacao do filtro
	// busca nos registros anteriores o primeiro que atende
	::_SkipPrev()
Endif

Return

// ----------------------------------------------------------
// Permite setar o modo "verbose" da classe
METHOD SetVerbose( lSet ) CLASS ZMEMFILE 
::lVerbose := lSet
Return

// ----------------------------------------------------------
// Recupera o conteúdo de um campo da tabela 
// a partir da posiçao do campo na estrutura

METHOD FieldGet(nPos) CLASS ZMEMFILE 

If nPos > 0 .and. nPos <= ::nFldCount 

	Return ::aGetRecord[nPos]
	
Endif

Return NIL


// ----------------------------------------------------------
// Atualiza um valor na coluna informada do registro atual 
// Por hora nao critica nada, apenas coloca o valor no array 

METHOD FieldPut(nPos,xValue) CLASS ZMEMFILE 

If ( !::lCanWrite )
	UserException("Invalid FieldPut() -- File NOT OPEN for WRITING")
Endif

If ( ::lEOF )
	UserException("Invalid FieldPut() -- File is in EOF")
Endif

If nPos > 0 .and. nPos <= ::nFldCount 
	::aPutRecord[nPos] := xValue
	::lUpdPend := .T. 
Endif

Return NIL

// ----------------------------------------------------------
// Recupera o nome de um campo da tabela 
// a partir da posicao do campo na estrutura

METHOD FieldName(nPos) CLASS ZMEMFILE 
If nPos > 0 .and. nPos <= ::nFldCount 
	Return ::aStruct[nPos][1]
Endif
Return NIL

// ----------------------------------------------------------
// Recupera o numero do campo na estrutura da tabela 
// a partir do nome do campo 

METHOD FieldPos( cField ) CLASS ZMEMFILE 
Return ASCAN( ::aStruct , {|x| x[1] = cField })

// ----------------------------------------------------------
// Recupera o nome do arquivo no disco 
METHOD FileName() CLASS ZMEMFILE 
Return ::cMemFile

// ----------------------------------------------------------
// Retorna o numero de campo / colunas da tabela
METHOD FCount()  CLASS ZMEMFILE 
Return ::nFldCount

// ----------------------------------------
// Retorna .T. caso a ultima movimentação de registro 
// tentou ir antes do primeiro registro 
METHOD BOF() CLASS ZMEMFILE 
Return ::lBOF

// ----------------------------------------\
// Retorna .T. caso a tabela esteja em EOF
METHOD EOF() CLASS ZMEMFILE 
Return ::lEOF

// ----------------------------------------
// Retorna .T. caso o registro atual esteja deletado 
METHOD DELETED() CLASS ZMEMFILE 
If !::lEOF
	Return ::aGetRecord[::nFldCount+1]
Endif
Return .F. 

// ----------------------------------------
// Retorna o tamanho do HEader
// Baseado na estrutura DBF, o tamanho seria 32 * o tamanho da estrutura 
// Mais 32 bytes do Header, mais 2 do final da estrutura 

METHOD HEADER() CLASS ZMEMFILE 
Return ( len(::aStruct) * 32 ) + 32 + 2 

// ----------------------------------------
// Retorna o tamanho aproximado do arquivo na memoria 
// -- Desconsidera campos MEMO 

METHOD FileSize() CLASS ZMEMFILE 
Local nFileSize := 0 
nFileSize := ( ::nLastRec * ::nRecLength )  
Return nFileSize

// ----------------------------------------
// Retorna o tamanho de um registro da tabela no arquivo 
// Cada campo MEMO ocupa 10 bytes 

METHOD RECSIZE() CLASS ZMEMFILE 
Return ::nRecLength

// ----------------------------------------
// Retorna o numero do registro atualmente posicionado

METHOD RECNO() CLASS ZMEMFILE 
If ::lEOF
	Return ::nLastRec+1
Endif
Return ::nRecno 

// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Lê o registro posicionado no offset de dados atual 

METHOD _ReadRecord() CLASS ZMEMFILE 

// Copia os dados do array para o registro atual 
aCopy( ::aFileData[::nRecno], ::aGetRecord )

// Reseta flags de BOF e EOF 
::lBOF := .F. 
::lEOF := .F. 

Return .T. 


// ----------------------------------------
// Insere um registro em branco no final da tabela
// Apos a inserção, voce pode fazer fieldput 
// e confirmar tudo com UPDATE 
METHOD Insert() CLASS ZMEMFILE

// Limpa o conteudo do registro em memoria 
::_ClearRecord()

// Insere o registro em branco 
aadd( ::aFileData , aClone(::aGetRecord) )

// Nao estou em BOF ou EOF, 
// Estou em modo de inserção de registro
::lBOF := .F. 
::lEOF := .F. 

// Atualiza contador de registros 
::nLastRec := len( ::aFileData )

// Recno atual = registro novo 
::nRecno := ::nLastRec

Return .T. 

// ----------------------------------------
// Grava as alterações do registro atual na tabela 

METHOD Update() CLASS ZMEMFILE
Local nI

If !::lUpdPend
	// Nao tem update pendente, nao faz nada
	Return
Endif

For nI := 1 to ::nFldCount
	// Atualiza apenas os campos que receberam conteudo 
	// Atualiza tambel o registro atual na memoria
	// E limpa o elemento do array de update pendente 
	If ::aPutRecord[nI] != NIL 
		::aFileData[::nRecno][nI] := ::aPutRecord[nI]
		::aGetRecord[nI] := ::aPutRecord[nI]
		::aPutRecord[nI] := NIL 
	Endif
Next

// Agora que o registro está atualizado, atualiza os indices 
aEval(::aIndexes , {|oIndex| oIndex:UpdateKey() })

// Desliga flag de update pendente 
::lUpdPend := .F. 

Return .T. 

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Limpa os campos do registro atual 
// ( Inicializa todos com os valores DEFAULT ) 

METHOD _ClearRecord()  CLASS ZMEMFILE
Local nI

For nI := 1 to ::nFldCount
	cTipo := ::aStruct[nI][2]
	nTam  := ::aStruct[nI][3]
	If cTipo == 'C'
		::aGetRecord[nI] := space(nTam)
	ElseIf cTipo == 'N'
		::aGetRecord[nI] := 0
	ElseIf cTipo == 'D'
		::aGetRecord[nI] := ctod('')
	ElseIf cTipo == 'L'
		::aGetRecord[nI] := .F.
	ElseIf cTipo == 'M'
		::aGetRecord[nI] := 0
	Endif
Next

// Flag de deletado 
::aGetRecord[::nFldCount+1] := .F. 

Return


// ----------------------------------------------------------
// Avança ou retrocede o ponteiro de registro 
// No caso de DBSkip(0), apenas faz refresh do registro atual   
// Default = 1 ( Próximo Registro ) 

METHOD Skip( nQtd ) CLASS ZMEMFILE 
Local lForward := .T. 

If nQtd  == NIL
	nQtd := 1
ElseIF nQtd < 0 
	lForward := .F. 
Endif

// Quantidade de registros para mover o ponteiro
// Se for negativa, remove o sinal 
nQtd := abs(nQtd)

While nQtd > 0 
	If lForward
		IF ::_SkipNext()
			nQtd--
		Else
			// Bateu EOF()
			::_ClearRecord()
			Return
		Endif
	Else
		IF ::_SkipPrev()
			nQtd--
		Else
			// Bateu BOF()
			Return
		Endif
	Endif
Enddo

// Traz o registro atual para a memória
::_ReadRecord()

Return

// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Verifica se o registro atual está contemplado pelo filtro 
// Release 20190106 -- Contempla filtro de registros deletados

METHOD _CheckFilter() CLASS ZMEMFILE

If ::lSetDeleted 
	If ::Deleted()
		// Filtro de deletados está ligado 
		// e este registro está deletado .. ignora
		Return .F. 
	Endif
Endif

If ::bFilter != NIL 
	// Existe uma expressao de filtro 
	// Roda a expressão para saber se este registro 
	// deve estar  "Visivel" 
	Return Eval(::bFilter , self )	
Endif

Return .T. 

// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Le e posiciona no proximo registro, considerando filtro 

METHOD _SkipNext() CLASS ZMEMFILE
Local nNextRecno

While (!::lEOF)

	If ::nIndexOrd > 0 
		// Se tem indice ativo, pergunta pro indice
		// qual é o próximo registro
		nNextRecno := ::oCurrentIndex:GetNextRec()
	Else
		// Estou na ordem fisica
		// Parte do registro atual , soma 1 
		nNextRecno := ::Recno() + 1 
	Endif
	
	// Retornou ZERO ou 
	// Passou do final de arquivo, esquece
	If nNextRecno == 0 .OR. nNextRecno > ::nLastRec
		::lEOF := .T.
		::_ClearRecord()
		Return .F. 
	Endif

	// ----------------------------------------
	// Atualiza o numero do registro atual 
	::nRecno := nNextRecno

	// Traz o registro atual para a memória
	::_ReadRecord()

	// Passou na checagem de filtro ? Tudo certo 
	// Senao , continua lendo ate achar um registro valido 
	If ::_CheckFilter()
		Return .T. 
	Endif

Enddo

Return .F. 

// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Le e posiciona no registro anmterior, considerando filtro 

METHOD _SkipPrev() CLASS ZMEMFILE
Local nPrevRecno

While (!::lBOF)

	If ::nIndexOrd > 0 
		// Se tem indice ativo, pergunta pro indice
		// qual é o registro anterior
		nPrevRecno := ::oCurrentIndex:GetPrevRec()
	Else
		// Estou na ordem fisica
		// Parte do registro atual , subtrai 1
		nPrevRecno := ::Recno() - 1 
    Endif
    
	// Tentou ler antes do primeiro registro 
	// Bateu em BOF()
	If nPrevRecno < 1 
		::lBOF := .T.
		Return .F. 
	Endif

	// ----------------------------------------
	// Atualiza o numero do registro atual 
	::nRecno := nPrevRecno

	// Traz o registro atual para a memória
	::_ReadRecord()

	// Passou na checagem de filtro ? Tudo certo 
	// Senao , continua lendo ate achar um registro valido 
	If ::_CheckFilter()
		Return .T. 
	Endif

Enddo

// Chegou no topo. 
// Se tem filtro, e o registro nao entra no filtro, localiza 
// o primeir registro válido 
If ( !::_CheckFilter() )
	::GoTop()
	::lBOF := .T. 
Endif

Return .F. 

// ----------------------------------------
// Permite trocar a ordedm atual usando 
// um indice aberto 

METHOD SetOrder(nOrd) CLASS ZMEMFILE
If nOrd < 0 .OR.  nOrd > len( ::aIndexes )
	UserException("DbSetOrder - Invalid Order "+cValToChar(nOrd))
Endif
::nIndexOrd := nOrd
If ::nIndexOrd > 0 
	::oCurrentIndex := ::aIndexes[::nIndexOrd]
	::oCurrentIndex:SetResync()
Else
	::oCurrentIndex := NIL
Endif
Return

// ----------------------------------------
// Retorna o numero da ordem do indce ativo 

METHOD IndexOrd() CLASS ZMEMFILE
Return ::nIndexOrd

// ----------------------------------------
// Retorna a expressao da chave de indice atual 
// Caso nao haja indice ativo, retorna ""

METHOD IndexKey() CLASS ZMEMFILE
IF ::nIndexOrd > 0 
	Return ::oCurrentIndex:GetIndexExpr()
Endif
Return ""

// ----------------------------------------
// Retorna o numero da ordem do indce ativo 
METHOD IndexValue() CLASS ZMEMFILE
IF ::nIndexOrd > 0 
	Return ::oCurrentIndex:GetIndexValue()
Endif
Return NIL


// ----------------------------------------
// Retorna o numero da ordem do indce ativo 
METHOD Seek(cKeyExpr) CLASS ZMEMFILE
Local nRecFound := 0

IF ::nIndexOrd <= 0
	UserException("DBSeek Failed - No active Index")
Endif

nRecFound := ::oCurrentIndex:IndexSeek(cKeyExpr)

If nRecFound > 0
	// NAo precisa resincronizar o indice
	// Eu já fiz a busca pelo indice
	::nRecno := nRecFound
	::_ReadRecord()
	Return .T.
Endif

// Nao achou nada, vai para EOF 
::lEOF := .T.
::_ClearRecord()

Return .F.
	
  
// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Cria uma instancia de um indice em memoria 
// Acrescenta na lista de indices 
// Torna o indice ativo e posiciona no primeiro 
// registro da nova ordem 

METHOD CreateIndex(cIndexExpr) CLASS ZMEMFILE
Local oMemIndex
Local nLastIndex

// Cria o objeto do indice passando a instancia
// do arquivo DBF atual 
oMemIndex := ZDBFMEMINDEX():New(self)

// Cria o indice com a expressao informada
oMemIndex:CreateIndex(cIndexExpr) 

// Acrescenta o indice criado na tabela 
aadd(::aIndexes,oMemIndex)

// E torna este indice atual 
nLastIndex := len(::aIndexes)
::SetOrder( nLastIndex ) 

// Posiciona no primeiro registro da nova ordem 
::GoTop()

Return

