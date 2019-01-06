#include  'protheus.ch'

/* ===========================================================================

Classe		ZDBFTABLE
Autor		Júlio Wittwer
Data		04/01/2019

Descrição   Classe de acesso a arquivos DBF - SOMENTE LEITURA 
            Suporte DBF DBASE III / Clipper / ADS 
			Suporte a leitura de campos MEMO dos formatos DBT e FPT

Observações Embora todas as propriedades sejam públicas, o uso da classe 
            deve ser feito totalmente pelos MÉTODOS implementados
            Os métodos de uso interno e/ou restritos da classe são 
            iniciados com "_"

			O objeto gerado exige apenas o nome do arquivo no construtor. 
			O arquivo é aberto em modo de leitura compartilhado. 
			Pode ser usado inclusive para ler arquivos no SmartClient 
			( Porém o desempenho pode ser prejudicado devido ao tráfego 
			de rede entre APPServer e SmartClient a cada leitura de registro


Release 20190105
- Implementação de filtro -- DbSetFilter e DBCleanFilter()

Release 20190106 
- Implementação de indice em memória 
- Implementação de filtro de registros deletados



Referências do Formato de Arquivos DBF / DBT / FPT 

https://www.dbase.com/Knowledgebase/INT/db7_file_fmt.htm
http://dbfviewer.com/dbf-file-structure/
https://www.loc.gov/preservation/digital/formats/fdd/fdd000325.shtml
https://en.wikipedia.org/wiki/.dbf
http://www.dbfree.org/webdocs/1-documentation/b-dbf_header_specifications.htm
http://www.independent-software.com/dbase-dbf-dbt-file-format.html
http://web.tiscali.it/SilvioPitti/
http://www.idea2ic.com/File_Formats/DBF%20FILE%20STRUCTURE.pdf
http://www.oocities.org/geoff_wass/dBASE/GaryWhite/dBASE/FAQ/qformt.htm
http://www.dbfree.org/webdocs/1-documentation/a-about_indexes.htm

=========================================================================== */

CLASS ZDBFTABLE FROM LONGNAMECLASS

  DATA cDataFile			// Nome do arquivo de dados
  DATA cMemoFile			// Nome do arquivo memo (DBT/FPT) 

  DATA cDBFType				// Identificador hexadecimal do tipo do DBF 
  DATA dLastUpd				// Data registrada dentro do arquivo como ultimo UPDATE 
  DATA nLastRec				// Ultimo registro do arquivo - Total de registros
  DATA nRecLength			// Tamanho de cada registro 
  DATA nDataPos 			// Offset de inicio dos dados 
  DATA lHasMemo				// Tabela possui campo MEMO ?
  DATA cMemoType			// Identificador (extensao) do tipo do campo MEMO
  DATA nFileSize 			// Tamanho total do arquivo em bytes 
  DATA nFldCount			// Quantidade de campos do arquivo 
  DATA aRecord				// Array com todas as colunas do registro atual 
  DATA lDeleted				// Indicador de registro corrente deletado (marcado para deleção ) 
  DATA lSetDeleted          // Filtro de registros deletados ativo 
  DATA nRecno				// Número do registro (RECNO) atualmnete posicionado 
  DATA bFilter              // Codeblock de filtro 

  DATA nIndexOrd            // Ordem de indice atual 
  DATA aIndexes             // Array com objetos de indice 
  DATA oCurrentIndex        // Objeto do indice atual 

  DATA lBOF					// Flag de inicio de arquivo 
  DATA lEOF					// Flag de final de arquivo 
  
  DATA nHData				// Handler do arquivo de dados
  DATA nHMemo				// Handler do arquivo de MEMO
  DATA aStruct		   		// Array com a estrutura do DBF 
    	
  DATA nLastError			// Ultimo erro ocorrido 
  DATA cLastError			// Descrição do último erro 

  // ========================= Metodos de uso público da classe

  METHOD NEW(cFile)			// Construtor 
  METHOD OPEN()				// Abertura da tabela 
  METHOD CLOSE()			// Fecha a tabela 

  METHOD GetDBType()		// REtorna identificador hexadecimal do tipo da tabela 
  METHOD GetDBTypeStr() 	// Retorna string identificando o tipo da tabela 

  METHOD Lastrec()			// Retorna o total de registros / numero do ultimo registro da tabela 
  METHOD RecCount()			// Retorna o total de registros / numero do ultimo registro da tabela 
  METHOD DbStruct()			// Retorna CLONE da estrutura de dados da tabela 
  METHOD DBGoTo(nRec)		// Posiciona em um registro informado. 
  METHOD DBGoTop()			// Posiciona no RECNO 1 da tabela 
  METHOD DBGoBottom()   	// Posiciona em LASTREC da tabela 
  METHOD DbSkip( nQtd )     // Navega para frente ou para tráz uma quantidade de registros 
  METHOD FieldGet( nPos )   // Recupera o conteudo da coluna informada do registro atual 
  METHOD FieldName( nPos )	// Recupera o nome da coluna informada 
  METHOD FieldPos( cField ) // Retorna a posicao de um campo na estrutura da tabela ( ID da Coluna )
  METHOD BOF()				// Retorna .T. caso tenha se tentado navegar antes do primeiro registro 
  METHOD EOF()				// Retorna .T, caso o final de arquivo tenha sido atingido 
  METHOD Recno()			// Retorna o numero do registro (RECNO) posicionado 
  METHOD Deleted()			// REtorna .T. caso o registro atual esteja DELETADO ( Marcado para deleção ) 
  METHOD DBSetFilter()      // Permite setar um filtro para os dados 
  METHOD DBClearFilter()    // Limpa o filtro 
  METHOD SetDeleted()       // Liga ou desliga filtro de registros deletados

  METHOD DbSetOrder()       
  METHOD IndexOrd()
  METHOD IndexKey()
  METHOD IndexValue()
  METHOD DBSeek(cKeyExpr)
  METHOD _AddIndex(oIndex)

  METHOD Header() 			// Retorna tamanho em Bytes do Header da Tabela
  METHOD RecSize()			// Retorna o tamanho de um registro da tabela 
  METHOD LUpdate()			// Retorna a data interna do arquivo que registra o ultimo update 
 
  METHOD GetError() 		// Retorna o Codigo e Descricao por referencia do ultimo erro 
  METHOD GetErrorCode()     // Retorna apenas oCodigo do ultimo erro ocorrido
  METHOD GetErrorStr()		// Retorna apenas a descrição do último erro ocorrido

  // ========================= Metodos de uso interno da classe

  METHOD _ResetError()		// Limpa a ultima ocorrencia de erro 
  METHOD _SetError()        // Seta uma nova ocorrencia de erro 
  METHOD _ResetVars() 		// Inicializa propriedades do Objeto, no construtor e no CLOSE
  METHOD _ReadHeader()		// Lê o Header do arquivo  de dados
  METHOD _ReadStruct()		// Lê a estrutura do arquivo de dados 
  METHOD _ReadRecord()		// Le um registro do arquivo de dados
  METHOD _ClearRecord()		// Limpa o registro da memoria (EOF por exemplo) 
  METHOD _ReadMemo()        // Recupera um conteudo de campo memo por OFFSET
  METHOD _CheckFilter()     // Verifica se o registro atual está contemplado no filtro 
  METHOD _SkipNext()		// Le o proximo registro da tabela 
  METHOD _SkipPrev()        // Le o registro anterior da tabela 

ENDCLASS

// ----------------------------------------------------------
// Construtor do objeto DBF 
// Apenas recebe o nome do arquivo e inicializa as propriedades

METHOD NEW(cFile) CLASS ZDBFTABLE 

::_ResetVars() 
::cDataFile   := lower(cFile)

Return self


// ----------------------------------------------------------
// Abertura da tabela -- READ ONLE 
// Caso retorne .F. , consulte o ultimo erro usando GetErrorStr() / GetErrorCode()
// Por hora apenas a abertura possui tratamento de erro 

METHOD OPEN() CLASS ZDBFTABLE 

::_ResetError()

If ::nHData <> -1
	::_SetError(-1,"File Already Open")
	Return .F.
Endif

// Abre o arquivo de dados
::nHData := Fopen(::cDataFile)

If ::nHData == -1
	::_SetError(-2,"Open Error - File ["+::cDataFile+"] - FERROR "+cValToChar(Ferror()))
	Return .F.
Endif

// Pega o tamanho do arquivo 
::nFileSize := fSeek(::nHData,0,2)

// Lê o Header do arquivo 
If !::_ReadHEader()
	FClose(::nHData)
	::nHData := -1
	Return .F. 
Endif

If ::lHasMemo

	// Se o header informa que a tabela possui campo MEMO 
	// Determina o nome do arquivo MEMO 

	::cMemoFile := substr(::cDataFile,1,at(".dbf",::cDataFile)-1)
	::cMemoFile += ::cMemoType
	
	If !file(::cMemoFile)
		::_SetError(-3,"Memo file ["+::cMemoFile+"] NOT FOUND.")
		::Close()
		Return .F. 
	Endif

	// Abre o arquivo MEMO 
	::nHMemo := FOpen(::cMemoFile)
    
	If ::nHMemo == -1
		::_SetError(-4,"Open Error - File ["+::cMemoFile+"] - FERROR "+cValToChar(Ferror()))
		::Close()
		Return .F. 
	Endif
	
Endif

If !::_ReadStruct()

	// Em caso de falha na leitura da estrutura 

	FClose(::nHData)
	::nHData := -1
    
	IF ::nHMemo <> -1
		FClose(::nHMemo)
		::nHMemo := -1
	Endif

	Return .F.
	
Endif

// Cria o array de campos do registro atual 
::aRecord := Array(::nFldCount)

// Vai para o topo do arquivo 
// e Lê o primeiro registro físico 
::DBGoTop()

Return .T. 


// ----------------------------------------------------------
// Fecha a tabela aberta 
// Limpa as variaveis de controle. 
// A tabela pode ser aberta novamente pela mesma instancia 

METHOD CLOSE() CLASS ZDBFTABLE 
Local nI

If ::nHData <> -1
	fClose(::nHData)
Endif

If ::nHMemo <> -1
	fClose(::nHMemo)
Endif

For nI := 1 to len(::aIndexes)
	::oCurrentIndex := ::aIndexes[nI]
	::oCurrentIndex:Close()
	FreeObj(::oCurrentIndex)
Next

::_ResetVars()

Return 


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


METHOD DBSetFilter( cFilter ) CLASS ZDBFTABLE
Local aCampos := {}
Local cTemp
Local nI, nPos

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

Return

// ----------------------------------------------------------
// Limpa a expressao de filtro atual 

METHOD DBClearFilter() CLASS ZDBFTABLE 
::bFilter := NIL
Return

// ----------------------------------------------------------
// Permite ligar filtro de navegação de registros deletados
// Defaul = desligado

METHOD SetDeleted( lSet ) CLASS ZDBFTABLE 
Local lOldSet := ::lSetDeleted
If pCount() > 0 
	::lSetDeleted := lSet
Endif
Return lOldSet




// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Inicializa / Limpa as propriedades padrao do Objeto 

METHOD _ResetVars() CLASS ZDBFTABLE 

::nHData      := -1
::nHMemo      := -1
::cLastError  := ''
::nLastError  := 0 
::nLastRec    := 0
::nDataPos    := 0 
::lBof        := .F. 
::lEof        := .F. 
::lHasMemo    := .F. 
::aStruct     := {}
::nFileSize   := 0
::dLastUpd    := ctod("")
::nFldCount   := 0
::aRecord     := {}
::lDeleted    := .F. 
::lSetDeleted := .F. 
::nRecno      := 0
::cMemoType   := ''
::bFilter     := NIL
::nIndexOrd   := 0
::aIndexes    := {}
::oCurrentIndex := {}

Return


// ----------------------------------------------------------
// Retorna o código do ultimo erro e a descrição por referencia

METHOD GetError( cRefError ) CLASS ZDBFTABLE 
cRefError := ::cLastError
Return ::nLastError

// ----------------------------------------------------------
// Retorna apenas o código do ultimo erro 

METHOD GetErrorCode() CLASS ZDBFTABLE 
Return ::nLastError

// ----------------------------------------------------------
// Retorna apenas a descrição do ultimo erro 

METHOD GetErrorStr() CLASS ZDBFTABLE 
Return ::cLastError

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Limpa o registro do ultimo erro 

METHOD _ResetError() CLASS ZDBFTABLE 
::cLastError := ''
::nLastError := 0 
Return

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Seta uma nova ocorrencia de erro

METHOD _SetError(nCode,cErrorMsg) CLASS ZDBFTABLE 
::cLastError := cErrorMsg
::nLastError := nCode
Return

// ----------------------------------------------------------
// Retorna o identificador hexadecimal do tipo do DBF

METHOD GetDBType() CLASS ZDBFTABLE 
Return ::cDBFType

// ----------------------------------------------------------
// Retorna a descrição do tipo de arquivo DBF 

METHOD GetDBTypeStr() CLASS ZDBFTABLE 
If empty(::cDBFType)
	Return ""
Endif
Return GetDBTypeStr(::cDBFType)

// ----------------------------------------------------------
// Retorna a data do ultimo update feito no arquivo 

METHOD LUPDATE() CLASS ZDBFTABLE 
Return ::dLastUpd

// ----------------------------------------------------------
// Retorna o numero do ultimo registro da tabela 

METHOD Lastrec() CLASS ZDBFTABLE 
Return ::nLastRec

// ----------------------------------------------------------
// Colocado apenas por compatibilidade 
// 

METHOD Reccount() CLASS ZDBFTABLE 
Return ::nLastRec


// ----------------------------------------------------------
// Retorna um clone do Array da estrutura da tabela 

METHOD DbStruct() CLASS ZDBFTABLE 
Return aClone( ::aStruct )

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Realiza a leitura do Header do arquivo DBF 

METHOD _ReadHeader() CLASS ZDBFTABLE 
Local cBuffer := space(32)
Local nYear, nMonth, nDay
Local cTemp := ''
Local nTemp := 0

// Reposicionao o arquivo no Offset 0
// Le os primeiros 32 bytes do Header
FSeek(::nHData,0)
FRead(::nHData,@cBuffer,32)

// ----------------------------------------
// Database File Type

cTemp := GetOffset(cBuffer,0,1)       
nTemp := ASC(cTemp)

::cDBFType := Byte2Hex(nTemp)
                                 
If ::cDBFType == '0x83'   
	// FoxBASE+/dBASE III PLUS, with memo
	::lHasMemo := .T. 
	::cMemoType := ".dbt"
ElseIf ::cDBFType == '0xF5'
	// FoxPro 2.x (or earlier) with memo
	::lHasMemo := .T. 
	::cMemoType := ".fpt"
Endif

If !DBSuported(::cDBFType)
	::_SetError(-5,"Format ("+::cDBFType+") not supported")
	Return .F. 
Endif

// ----------------------------------------
// Last Update ( YMD => 3 Bytes, binary )

cTemp := GetOffset(cBuffer,1,3) 

nYear  := ASC( substr(cTemp,1,1))
nMonth := ASC( substr(cTemp,2,1))
nDay   := ASC( substr(cTemp,3,1))

If nYear < 50 
	nYear += 2000
Else
	nYear += 1900
Endif

::dLastUpd := ctod(strzero(nDay,2)+"/"+strzero(nMonth,2)+"/"+strzero(nYear,4))

// ----------------------------------------
// 4 bytes (32 bits), Record Count (  LastRec ) 

cTemp := GetOffset(cBuffer,4,4) 
::nLastRec := Bin2L(cTemp)

// ----------------------------------------
// First Data Record Position  ( Offset ) 

cTemp := GetOffset(cBuffer,8,2) 
::nDataPos := Bin2I(cTemp)

// ----------------------------------------
// Length of one data record, including delete flag

cTemp := GetOffset(cBuffer,10,2) 
::nRecLength := Bin2I(cTemp)

// Limpeza de variáveis 
cTemp := NIL
cBuffer := NIL

Return .T. 


/*
FIELD DESCRIPTOR ARRAY TABLE
BYTES DESCRIPTION
0-10 Field Name ASCII padded with 0x00
11 Field Type Identifier (see table)
12-15 Displacement of field in record
16 Field length in bytes
17 Field decimal places
18-19 Reserved
20 dBaseIV work area ID
21-30 Reserved
31 Field is part of production index - 0x01 else 0x00
*/

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Lê a estrutura de campos da tabela 

METHOD _ReadStruct() CLASS ZDBFTABLE 
Local cFldBuff := space(32)
Local cFldName, cFldType  , nFldLen , nFldDec 

// Reposicionao o arquivo no Offset 32
FSeek(::nHData,32)

While .T.

	FRead(::nHData,@cFldBuff,32)
	
	If substr(cFldBuff,1,1) == chr(13) 
		// 0x0D => Indica final da estrutura
		EXIT
	Endif
	
	cFldName := GetOffset(cFldBuff,0,11)
	cFldName := left( cFldName,AT(chr(0),cFldName )-1 )
	cFldName := padr(cFldName,10)
	
	cFldType := GetOffset(cFldBuff,11,1)

	nFldLen  := ASC(GetOffset(cFldBuff,16,1))
	nFldDec  := ASC(GetOffset(cFldBuff,17,1))
	
	aadd(::aStruct , { cFldName , cFldType , nFldLen , nFldDec } )

Enddo

::nFldCount := len(::aStruct)

Return .T. 


// ----------------------------------------------------------
// Posiciona diretamente em um regsitro 

METHOD DBGoTo(nRec)  CLASS ZDBFTABLE

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

METHOD DBGoTop() CLASS ZDBFTABLE 

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

METHOD DBGoBottom() CLASS ZDBFTABLE 

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
// Recupera o conteúdo de um campo da tabela 
// a partir da posiçao do campo na estrutura

METHOD FieldGet(nPos) CLASS ZDBFTABLE 

If nPos > 0 .and. nPos <= ::nFldCount 

	IF ::aStruct[nPos][2] == 'M'
		// Ccampo MEMO, faz a leitura baseado 
		// no Bloco gravado na tabela 
		Return ::_ReadMemo( ::aRecord[nPos] )
	Else
		Return ::aRecord[nPos]
	Endif
	
Endif

Return NIL

// ----------------------------------------------------------
// Recupera o nome de um campo da tabela 
// a partir da posicao do campo na estrutura

METHOD FieldName(nPos) CLASS ZDBFTABLE 
If nPos > 0 .and. nPos <= ::nFldCount 
	Return ::aStruct[nPos][1]
Endif
Return NIL

// ----------------------------------------------------------
// Recupera o numero do campo na estrutura da tabela 
// a partir do nome do campo 

METHOD FieldPos( cField ) CLASS ZDBFTABLE 
Return ASCAN( ::aStruct , {|x| x[1] = cField })

// ----------------------------------------
// Retorna .T. caso a ultima movimentação de registro 
// tentou ir antes do primeiro registro 
METHOD BOF() CLASS ZDBFTABLE 
Return ::lBOF

// ----------------------------------------\
// Retorna .T. caso a tabela esteja em EOF
METHOD EOF() CLASS ZDBFTABLE 
Return ::lEOF

// ----------------------------------------
// Retorna .T. caso o registro atual esteja deletado 
METHOD DELETED() CLASS ZDBFTABLE 
Return ::lDeleted

// ----------------------------------------
// Retorna o tamanho do HEader
// -- O tamanho do Header é justamente a posicao do offser de dados 
// da tabela, após o final do Header. 

METHOD HEADER() CLASS ZDBFTABLE 
Return ::nDataPos

// ----------------------------------------
// Retorna o tamanho de um registro da tabela no arquivo 
// Cada campo MEMO ocupa 10 bytes 

METHOD RECSIZE() CLASS ZDBFTABLE 
Return ::nRecLength

// ----------------------------------------
// Retorna o numero do registro atualmente posicionado

METHOD RECNO() CLASS ZDBFTABLE 
If ::lEOF
	Return ::nLastRec+1
Endif
Return ::nRecno 

// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Lê o registro posicionado no offset de dados atual 

METHOD _ReadRecord() CLASS ZDBFTABLE 
Local cTipo , nTam , cValue
Local nBuffPos := 2 , nI
Local cRecord := '' , nOffset

// ----------------------------------------
// Calcula o offset do registro atual baseado no RECNO

nOffset := ::nDataPos 
nOffset += (::nRecno * ::nRecLength)
nOffset -= ::nRecLength

// Posiciona o arquivo de dados no offset do registro 
FSeek(::nHData , nOffset )

// Lê o registro do offset atual 
FRead(::nHData , @cRecord , ::nRecLength )

// Primeiro byte = Flag de deletato
// Pode ser " " (espaço)    registro ativo 
//          "*" (asterisco) registro deletado 
   
::lDeleted := ( left(cRecord,1) = '*' )

// Agora lê os demais campos e coloca no ::aRecord

For nI := 1 to ::nFldCount

	cTipo := ::aStruct[nI][2]
	nTam  := ::aStruct[nI][3]
	cValue := substr(cRecord,nBuffPos,nTam)

	If cTipo == 'C'
		::aRecord[nI] := cValue
		nBuffPos += nTam
	ElseIf cTipo == 'N'
		::aRecord[nI] := val(cValue)
		nBuffPos += nTam
	ElseIf cTipo == 'D'
		::aRecord[nI] := STOD(cValue)
		nBuffPos += nTam
	ElseIf cTipo == 'L'
		::aRecord[nI] := ( cValue=='T' )
		nBuffPos += nTam
	ElseIf cTipo == 'M'
		// Recupera o Offset do campo no DBT/FPT
		::aRecord[nI] := val(cValue)
		nBuffPos += nTam
	Endif
  
Next

// Reseta flags de BOF e EOF 
::lBOF := .F. 
::lEOF := .F. 

Return .T. 


// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Limpa os campos do registro atual 
// ( Inicializa todos com os valores DEFAULT ) 

METHOD _ClearRecord()  CLASS ZDBFTABLE
Local nI
::lDeleted := .F. 
For nI := 1 to ::nFldCount
	cTipo := ::aStruct[nI][2]
	nTam  := ::aStruct[nI][3]
	If cTipo == 'C'
		::aRecord[nI] := space(nTam)
	ElseIf cTipo == 'N'
		::aRecord[nI] := 0
	ElseIf cTipo == 'D'
		::aRecord[nI] := ctod('')
	ElseIf cTipo == 'L'
		::aRecord[nI] := .F.
	ElseIf cTipo == 'M'
		::aRecord[nI] := ""
	Endif
Next
Return


// ----------------------------------------------------------
// Avança ou retrocede o ponteiro de registro 
// No caso de DBSkip(0), apenas faz refresh do registro atual   
// Default = 1 ( Próximo Registro ) 

METHOD DbSkip( nQtd ) CLASS ZDBFTABLE 
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
// Lë um campo MEMO de um arquivo DBT 
// baseado no numero do bloco rececido como parametro 

METHOD _ReadMemo(nBlock) CLASS ZDBFTABLE
Local nFilePos
Local cMemo := '' 
Local cBlock := ''
Local nEndPos
Local nByte1,nByte2,nByte3,nByte4

If nBlock > 0

	IF ::cMemoType == ".dbt"

		// Leitura de MEMO em Arquivo DBT
		
		cBlock := space(512)
		nFilePos := nBlock * 512 
		fSeek(::nHMemo , nFilePos)
		While .T. 
			fRead(::nHMemo,@cBlock,512)
			nEndPos := at(chr(26),cBlock)
			If nEndPos > 0 
				cBlock := left(cBlock,nEndPos-1)
				cMemo += cBlock
				EXIT
			Else
				cMemo += cBlock
			Endif
		Enddo	
		
		// -- Quebra de linha "soft" = 8D 0A
		// -- Remove a quebra 
		cMemo := strtran(cMemo , chr(141)+chr(10) , '' ) 

	ElseIF ::cMemoType == ".fpt"

		// Leitura de MEMO em Arquivo FPT  
		// Offset 1-4 Tipo do registro ( 0x00 0x00 0x00 0x01 )
		// Offset 5-8 Tamanho do registro 

		cBlock := space(8)
		nFilePos := nBlock * 16384
		fSeek(::nHMemo , nFilePos)
		fRead(::nHMemo,@cBlock,8)

		// Calcula o tamnho do registro 

		nByte1 := asc(substr(cBlock,5,4))
		nByte2 := asc(substr(cBlock,6,4))
		nByte3 := asc(substr(cBlock,7,4))
		nByte4 := asc(substr(cBlock,8,4))
		
		nMemoSize := nByte4
		If nByte3 > 0 
			nMemoSize += ( nByte3 * 256 )
		Endif
		If nByte2 > 0 
			nMemoSize += ( nByte3 * 65536 )
		Endif
		If nByte1 > 0 
			nMemoSize += ( nByte3 * 16777216 )
		Endif

		// Lê o registro direto para a memória
		fRead(::nHMemo,@cMemo,nMemoSize)

	Endif
	
Endif

Return cMemo

// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Verifica se o registro atual está contemplado pelo filtro 
// Release 20190106 -- Contempla filtro de registros deletados

METHOD _CheckFilter() CLASS ZDBFTABLE

If ::lSetDeleted .AND. ::lDeleted
	// Filtro de deletados está ligado 
	// e este registro está deletado .. ignora
	Return .F. 
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

METHOD _SkipNext() CLASS ZDBFTABLE
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

METHOD _SkipPrev() CLASS ZDBFTABLE
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
	::DBGoTop()
	::lBOF := .T. 
Endif

Return .F. 

// ----------------------------------------
// Permite trocar a ordedm atual usando 
// um indice aberto 

METHOD DbSetOrder(nOrd) CLASS ZDBFTABLE
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

METHOD IndexOrd() CLASS ZDBFTABLE
Return ::nIndexOrd

// ----------------------------------------
// Retorna a expressao da chave de indice atual 
// Caso nao haja indice ativo, retorna ""

METHOD IndexKey() CLASS ZDBFTABLE
IF ::nIndexOrd > 0 
	Return ::oCurrentIndex:GetIndexExpr()
Endif
Return ""

// ----------------------------------------
// Retorna o numero da ordem do indce ativo 
METHOD IndexValue() CLASS ZDBFTABLE
IF ::nIndexOrd > 0 
	Return ::oCurrentIndex:GetIndexValue()
Endif
Return NIL


// ----------------------------------------
// Retorna o numero da ordem do indce ativo 
METHOD DBSeek(cKeyExpr) CLASS ZDBFTABLE
Local nRecFound := 0
IF ::nIndexOrd <= 0
	UserException("DBSeek Failed - No active Index")
Endif
nRecFound := ::oCurrentIndex:IndexSeek(cKeyExpr)
If nRecFound > 0
	::DbGoto(nRecFound)
	Return .T.
Endif
::lEOF := .T.
::_ClearRecord()
Return .F.
	
  
// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Acrescenta um objeto de indice na lista de indices abertos

METHOD _AddIndex(oIndex)  CLASS ZDBFTABLE
aadd(::aIndexes,oIndex)
::nIndexOrd := len(::aIndexes)
Return

// =================================================
// Funcoes Auxiliares internas da classe 
// =================================================

// Array com os tipos de DBF reconhecidos 
// O 3o elemento quando .T. indoca se o formato é suportado 

STATIC _aDbTypes := { { '0x02','FoxBASE'                                              , .F. } , ;
                      { '0x03','FoxBASE+/Dbase III plus, no memo'                     , .T. } , ;  // ####  
                      { '0x04','dBASE IV or IV w/o memo file'                         , .F. } , ;
                      { '0x05','dBASE V w/o memo file'                                , .F. } , ;
                      { '0x30','Visual FoxPro'                                        , .F. } , ;
                      { '0x31','Visual FoxPro, autoincrement enabled'                 , .F. } , ;
                      { '0x32','Visual FoxPro, Varchar, Varbinary, or Blob-enabled'   , .F. } , ;
                      { '0x43','dBASE IV SQL table files, no memo'                    , .F. } , ;
                      { '0x63','dBASE IV SQL system files, no memo'                   , .F. } , ;
                      { '0x7B','dBASE IV with memo'                                   , .F. } , ;
                      { '0x83','FoxBASE+/dBASE III PLUS, with memo'                   , .T. } , ;  // ####
                      { '0x8B','dBASE IV with memo'                                   , .F. } , ;
                      { '0x8E','dBASE IV w. SQL table'                                , .F. } , ;
                      { '0xCB','dBASE IV SQL table files, with memo'                  , .F. } , ;
                      { '0xF5','FoxPro 2.x (or earlier) with memo'                    , .T. } , ;  // ####  
                      { '0xE5','HiPer-Six format with SMT memo file'                  , .F. } , ;
                      { '0xFB','FoxBASE'                                              , .F. } } 


// ----------------------------------------
// A partir do bnuffer informado, retorna o conteúdo 
// a partir do Offset e tamanho informados 
STATIC Function GetOffset(cBuffer,nOffset,nSize)
Return substr(cBuffer,nOffset+1,nSize)


// ----------------------------------------
// Converte um valor decimal para Hexadecimal maiusculo, 
// prefixando o retorno com "0x"
STATIC Function Byte2Hex(nByte)
Return '0x'+padl( upper(DEC2HEX(nByte)) , 2 , '0')

               
// ----------------------------------------
// Recebe identificaedor Hexadecimal do tipo do DBF
// Retorna a string a partir do tipo 

STATIC Function GetDBTypeStr(cTypeHex)
Local cRet := '(Unknow DBF file / NOT a DBF File)'
Local nPos := ascan(_aDbTypes,{|x| x[1] == cTypeHex })
If nPos > 0 
	cRet := _aDbTypes[nPos][2]
Endif
Return cREt
                
// ----------------------------------------
// Recebe identificaedor Hexadecimal do tipo do DBF
// Retorna .T. caso o suporte esteja implementado 

STATIC Function DBSuported(cTypeHex)
Local lSup := .F.
Local nPos := ascan(_aDbTypes,{|x| x[1] == cTypeHex })
If nPos > 0 
	lSup := _aDbTypes[nPos][3]
Endif
Return lSup


// ----------------------------------------
// Converte um valort decimal de 0 a 255 para Hexadecimal 

STATIC __aHEx := {'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'}

STATIC Function DEC2HEX(nByte)
Local nL := ( nByte % 16 ) 
Local nH := ( nByte-nL) / 16 
Return __aHex[nH+1]+__aHex[nL+1]

// ----------------------------------------
// Converte data no formato AAAAMMDD para Data do AdvPL 
STATIC Function STOD(cValue)
Local cOldSet := Set(_SET_DATEFORMAT, 'yyyy:mm:dd')
Local dRet := CTOD(Substr(cValue,1,4)+":"+Substr(cValue,5,2)+":"+Substr(cValue,7,2))
Set(_SET_DATEFORMAT, cOldSet)
Return dRet

