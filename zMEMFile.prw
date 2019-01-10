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

CLASS ZMEMFILE FROM ZISAMFILE

  DATA cMemFile			    // Nome / Identificador do arquivo de dados na memória 
  DATA lOpened              // Indica se o arquivo está aberto 

  DATA aFileData			// Array com os registros do arquivo 

  DATA dLastUpd				// Data registrada dentro do arquivo como ultimo UPDATE 
  DATA nRecLength			// Tamanho de cada registro 

  DATA aGetRecord			// Array com todas as colunas do registro atual 
  DATA aPutRecord           // Array com campos para update 
  DATA lExclusive           // Arquivo aberto em modo exclusivo ?
  DATA lCanWrite            // Arquivo aberto para gravacao 
  DATA lUpdPend             // Flag indicando update pendente 
  DATA lSetDeleted          // Filtro de registros deletados ativo 
  DATA nRecno				// Número do registro (RECNO) atualmnete posicionado 

  // ========================= Metodos de uso público da classe

  METHOD NEW(cFile)			// Construtor 
  METHOD OPEN()				// Abertura da tabela 
  METHOD CLOSE()			// Fecha a tabela 
  METHOD EXISTS()           // Verifica se a tabela existe 
  METHOD CREATE()           // Cria a tabela no disco 

  METHOD FieldGet( nPos )   // Recupera o conteudo da coluna informada do registro atual 
  METHOD FieldPut( nPos )   // Faz update em uma coluna do registro atual 
  METHOD FileName()         // Retorna nome do arquivo aberto 
  METHOD Recno()			// Retorna o numero do registro (RECNO) posicionado 
  METHOD Deleted()			// REtorna .T. caso o registro atual esteja DELETADO ( Marcado para deleção ) 
  METHOD SetDeleted()       // Liga ou desliga filtro de registros deletados
  
  METHOD Insert()           // Insere um registro em branco no final da tabela
  METHOD Update()           // Atualiza o registro atual na tabela 

  METHOD Header() 			// Retorna tamanho em Bytes do Header da Tabela
  METHOD FileSize()         // Retorna o tamanho ocupado pelo arquivo em bytes 
  METHOD RecSize()			// Retorna o tamanho de um registro da tabela 
  METHOD LUpdate()			// Retorna a data interna do arquivo que registra o ultimo update 
 
  // ========================= Metodos de uso interno da classe

  METHOD _InitVars() 		// Inicializa propriedades do Objeto, no construtor e no CLOSE
  METHOD _ReadStruct()		// Lê a estrutura do arquivo de dados 
  METHOD _ReadRecord()		// Le um registro do arquivo de dados
  METHOD _ClearRecord()		// Limpa o registro da memoria (EOF por exemplo) 

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
::ClearIndex()

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

// Inicialização das propriedades da classe pai
_Super:_InitVars()

// Inicializa demais propriedades da ZMEMFILE
::aFileData   := {}
::lOpened     := .F. 
::lExclusive  := .F. 
::lCanWrite   := .T. 
::dLastUpd    := ctod("")
::aGetRecord  := {}
::aPutRecord  := {}
::lUpdPend    := .F. 
::lSetDeleted := .F. 
::nRecno      := 0

Return

// ----------------------------------------------------------
// Retorna a data do ultimo update feito no arquivo 

METHOD LUPDATE() CLASS ZMEMFILE 
Return ::dLastUpd

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
// Recupera o nome do arquivo no disco 
METHOD FileName() CLASS ZMEMFILE 
Return ::cMemFile

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

// Inicializa com o valor default os campos da estrutura 
_Super:_ClearRecord()

// Limpa Flag de deletado 
::aGetRecord[::nFldCount+1] := .F. 

Return

