#include 'protheus.ch'
#include 'fileio.ch'
#include "zLibDec2Hex.ch"
#include "zLibDateTime.ch"

/* ===========================================================================

Classe		ZDBFFILE
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

Debitos Tecnicos

1) Melhorar a inserção. Atualmente é inserido um registro em branco 
direto no final da tabela, e então ele é alterado. Verificar forma 
de postergar a inserção, para escrever os novos dados de uma vez 

Referências do Formato de Arquivos DBF / DBT / FPT 

http://web.tiscali.it/SilvioPitti/
https://www.dbase.com/Knowledgebase/INT/db7_file_fmt.htm
http://dbfviewer.com/dbf-file-structure/
https://www.loc.gov/preservation/digital/formats/fdd/fdd000325.shtml
https://en.wikipedia.org/wiki/.dbf
http://www.dbfree.org/webdocs/1-documentation/b-dbf_header_specifications.htm
http://www.independent-software.com/dbase-dbf-dbt-file-format.html
http://www.idea2ic.com/File_Formats/DBF%20FILE%20STRUCTURE.pdf
http://www.oocities.org/geoff_wass/dBASE/GaryWhite/dBASE/FAQ/qformt.htm
http://www.dbfree.org/webdocs/1-documentation/a-about_indexes.htm

=========================================================================== */

CLASS ZDBFFILE FROM ZISAMFILE

  DATA cDataFile			// Nome do arquivo de dados
  DATA cMemoFile			// Nome do arquivo memo (DBT/FPT) 
  DATA lOpened              // Indica se o arquivo está aberto 

  DATA cDBFType				// Identificador hexadecimal do tipo do DBF 
  DATA dLastUpd				// Data registrada dentro do arquivo como ultimo UPDATE 
  DATA nRecLength			// Tamanho de cada registro 
  DATA nDataPos 			// Offset de inicio dos dados 
  DATA lHasMemo				// Tabela possui campo MEMO ?
  DATA nMemoType            // Tipo de campo MEMO da RDD ( 1 = DBT, 2 = FPT ) 
  DATA cMemoExt             // Identificador (extensao) do tipo do campo MEMO
  DATA aGetRecord			// Array com todas as colunas do registro atual 
  DATA aPutRecord           // Array com campos para update 
  DATA lExclusive           // Arquivo aberto em modo exclusivo ?
  DATA lCanWrite            // Arquivo aberto para gravacao 
  DATA lUpdPend             // Flag indicando update pendente 
  DATA lDeleted				// Indicador de registro corrente deletado (marcado para deleção ) 
  DATA lSetDeleted          // Filtro de registros deletados ativo 
  DATA nRecno				// Número do registro (RECNO) atualmnete posicionado 

  DATA nHData				// Handler do arquivo de dados
  DATA oMemoFile			// Objeto para lidar com campo Memo 
    	
  // ========================= Metodos de uso público da classe

  METHOD NEW(cFile)			// Construtor 
  METHOD OPEN()				// Abertura da tabela 
  METHOD CLOSE()			// Fecha a tabela 
  METHOD EXISTS()           // Verifica se a tabela existe 
  METHOD CREATE()           // Cria a tabela no disco 

  METHOD GetDBType()		// REtorna identificador hexadecimal do tipo da tabela 
  METHOD GetDBTypeStr() 	// Retorna string identificando o tipo da tabela 
  METHOD GetMemoType()      // Tipo do MEMO usado, 1 = DBT , 2 = FPT

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
  METHOD _ReadHeader()		// Lê o Header do arquivo  de dados
  METHOD _ReadStruct()		// Lê a estrutura do arquivo de dados 
  METHOD _SetLUpdate()      // Atualiza data do Last Update no Header do Arquivo 
  METHOD _ReadRecord()		// Le um registro do arquivo de dados
  METHOD _ClearRecord()		// Limpa o registro da memoria (EOF por exemplo) 
  METHOD _ReadMemo()        // Recupera um conteudo de campo memo por OFFSET

ENDCLASS

// ----------------------------------------------------------
// Construtor do objeto DBF 
// Apenas recebe o nome do arquivo e inicializa as propriedades

METHOD NEW(cFile) CLASS ZDBFFILE 

::_InitVars() 
::cDataFile   := lower(cFile)

Return self


// ----------------------------------------------------------
// Abertura da tabela -- READ ONLE 
// Caso retorne .F. , consulte o ultimo erro usando GetErrorStr() / GetErrorCode()
// Por hora apenas a abertura possui tratamento de erro 

METHOD OPEN(lExclusive,lCanWrite) CLASS ZDBFFILE 
Local nFMode := 0

::_ResetError()

If ::lOpened
	::_SetError(-1,"File Already Open")
	Return .F.
Endif

IF !::Exists()
	::_SetError(-6,"Unable to OPEN - File ["+::cDataFile+"] DOES NOT EXIST")
	Return .F.
Endif

If lExclusive = NIL ; 	lExclusive := .F. ; Endif
If lCanWrite = NIL ; 	lCanWrite := .F.  ; Endif

If lExclusive
	nFMode += FO_EXCLUSIVE
Else
	nFMode += FO_SHARED
Endif

If lCanWrite
	nFMode += FO_READWRITE
Else
	nFMode += FO_READ
Endif

// Por enquanto faz escrita apenas em modo exclusivo
If lCanWrite .AND. !lExclusive
	::_SetError(-6,"Unable to OPEN for WRITE in SHARED MODE -- Use Exclusive mode or OPEN FOR READ")
	Return .F.
Endif

// Atualiza propriedades de controle da classe
::lExclusive   := lExclusive
::lCanWrite    := lCanWrite

// Abre o arquivo de dados
::nHData := Fopen(::cDataFile,nFMode)

If ::nHData == -1
	::_SetError(-2,"Open Error - File ["+::cDataFile+"] Mode ["+cValToChar(nFMode)+"] - FERROR "+cValToChar(Ferror()))
	Return .F.
Endif

// Lê o Header do arquivo 
If !::_ReadHEader()
	FClose(::nHData)
	::nHData := -1
	Return .F. 
Endif

If ::lHasMemo

	// Se o header informa que a tabela possui campo MEMO 
	// Determina o nome do arquivo MEMO 

	::cMemoFile := substr(::cDataFile,1,rat(".",::cDataFile)-1)
	::cMemoFile += ::cMemoExt
	
	If !file(::cMemoFile)
		::_SetError(-3,"Memo file ["+::cMemoFile+"] NOT FOUND.")
		::Close()
		Return .F. 
	Endif

	If ::nMemoType == 1
		::oMemoFile  := ZDBTFILE():New(self,::cMemoFile)
	ElseIF ::nMemoType == 2
		::oMemoFile  := ZFPTFILE():New(self,::cMemoFile)
	Endif

	If !::oMemoFile:Open(::lExclusive,::lCanWrite)
		::_SetError(-4,"Open Error - File ["+::cMemoFile+"] - FERROR "+cValToChar(Ferror()))
		::Close()
		Return .F. 
	Endif
	
Endif

If !::_ReadStruct()

	// Em caso de falha na leitura da estrutura 

	FClose(::nHData)
	::nHData := -1
    
	IF ::oMemoFile != NIL 
		::oMemoFile:Close()
		FreeObj(::oMemoFile)
	Endif

	Return .F.
	
Endif

// Cria o array de campos do registro atual 
::aGetRecord := Array(::nFldCount)
::aPutRecord := Array(::nFldCount)

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

METHOD CLOSE() CLASS ZDBFFILE 
Local nI

// Fecha o arquivo aberto 
If ::nHData <> -1
	fClose(::nHData)
Endif

// Se tem memo, fecha 
IF ::oMemoFile != NIL 
	::oMemoFile:Close()
	FreeObj(::oMemoFile)
Endif

// Fecha todos os indices abertos 
::ClearIndex()

// Limpa as propriedades
::_InitVars()

Return 


// ----------------------------------------------------------\
// Verifica se a tabela existe no disco 
METHOD EXISTS() CLASS ZDBFFILE 
IF File(::cDataFile)
	Return .T. 
Endif
Return .F. 

// ----------------------------------------------------------\
// Cria a tabela no disco 
// O nome já foi recebido no construtor 
// Recebe a estrutura e a partir dela cria a tabela 

METHOD CREATE( aStru ) CLASS ZDBFFILE 
Local lHasMemo := .F.
Local nFields := 0
Local nRecSize := 1 
Local nI, nH
Local cNewHeader := ''
Local lOk, cMemoFile, oMemoFile
Local cFldName

If ::EXISTS()
	::_SetError(-7,"CREATE ERROR - File Already Exists")
Endif

If ::lOpened
	::_SetError(-8,"CREATE ERROR - File Already Opened")
Endif

nFields := len(aStru)

For nI := 1 to nFields
	If aStru[nI][2] == 'M'
		lHasMemo := .T. 
	Endif
	If !aStru[nI][2]$"CNDLM"
		UserException("CREATE ERROR - INVALID FIELD TYPE "+aStru[nI][2]+ " ("+aStru[nI][1]+")" )
	Endif
	// Ajusta nome do campo 
	aStru[nI][1] := Upper(padr(aStru[nI][1],10))
	nRecSize += aStru[nI][3]
Next

// Inicio do Header
// 1o Byte - Formato do aRquivo 
// Campo memo será criado como FPT 
If lHasMemo
	::nMemoType := 2
	cNewHeader += Chr(245) // FoxPro 2.x (or earlier) with memo ( FPT ) 
Else
	cNewHeader += chr(003) // FoxBASE+/Dbase III plus, no memo
Endif

// 3 Byte(2) = Last Update Date = TODAY
cNewHeader +=  chr( Year(date())-2000 ) + ;
	           chr( Month(date()) ) + ;
	           Chr( Day(date()) ) 

// 4 byte(S) - Last Record
cNewHeader +=  L2BIN(0) 

// 2 byte(s) -- Begin Data Offset
cNewHeader +=  I2Bin( ( (nFields+1) * 32) + 2 ) 

// 2 byte(s) -- Record Size 
cNewHeader +=  I2Bin(nRecSize) 

// Filler ( 32 Bytes  )
cNewHeader +=  replicate( chr(0) , 4 )
cNewHeader +=  replicate( chr(0) , 16 )

// Acrescenta no Header a estrutura
For nI := 1 to nFields

	cFldName := alltrim(aStru[nI][1])
	while len(cFldName) < 10
		cFldName += chr(0)
	Enddo

	cNewHeader +=  cFldName + chr(0) // Nome
	cNewHeader +=  aStru[nI][2]  // Tipo 
	cNewHeader +=  replicate( chr(0) , 4 ) // Filler - Reserved
	cNewHeader +=  chr(aStru[nI][3]) // Size
	cNewHeader +=  chr(aStru[nI][4]) // Decimal
	cNewHeader +=  replicate( chr(0) , 14 ) // Filler - Reserved

Next

// Final do Header apos estrutura 

cNewHeader +=  chr(13)  // 0x0D = Fim da estrutura 
cNewHeader +=  chr(0)   // 0c00 = Filler
cNewHeader +=  chr(26)  // 0x1A = End Of File

// Cria a tabela no disco 
nH := fCreate(::cDataFile)

If nH == -1
	::_SetError(-9,"CREATE ERROR - Data File ["+::cDataFile+"] - FERROR ("+cValToChar(Ferror())+")")
	Return .F. 
Endif

fWrite(nH,cNewHeader)
fCLose(nH)

If lHasMemo
	cMemoFile := substr(::cDataFile,1,rat(".",::cDataFile)-1)
	cMemoFile += '.fpt'
	oMemoFile := ZFPTFILE():New(self,cMemoFile)
	lOk := oMemoFile:Create()
	FreeObj(oMemoFile)
	If !lOk
		::_SetError(-9,"CREATE ERROR - Data File ["+::cDataFile+"] - FERROR ("+cValToChar(Ferror())+")")
		Return .F. 
	Endif
Endif

Return .T. 


// ----------------------------------------------------------
// Permite ligar filtro de navegação de registros deletados
// Defaul = desligado

METHOD SetDeleted( lSet ) CLASS ZDBFFILE 
Local lOldSet := ::lSetDeleted
If pCount() > 0 
	::lSetDeleted := lSet
Endif
Return lOldSet


// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Inicializa / Limpa as propriedades padrao do Objeto 

METHOD _InitVars() CLASS ZDBFFILE 

// Inicialização das propriedades da classe pai
_Super:_InitVars()

::nHData      := -1
::lOpened     := .F. 
::nDataPos    := 0 
::lHasMemo    := .F. 
::lExclusive  := .F. 
::lCanWrite   := .T. 
::dLastUpd    := ctod("")
::aGetRecord  := {}
::aPutRecord  := {}
::lUpdPend    := .F. 
::lDeleted    := .F. 
::lSetDeleted := .F. 
::nRecno      := 0
::cMemoExt    := ''
::nMemoType   := 0 


Return

// ----------------------------------------------------------
// Retorna o identificador hexadecimal do tipo do DBF

METHOD GetDBType() CLASS ZDBFFILE 
Return ::cDBFType

// ----------------------------------------------------------
// Tipo do MEMO usado, 1 = DBT , 2 = FPT

METHOD GetMemoType()  CLASS ZDBFFILE 
Return ::nMemoType

// ----------------------------------------------------------
// Retorna a descrição do tipo de arquivo DBF 

METHOD GetDBTypeStr() CLASS ZDBFFILE 
If empty(::cDBFType)
	Return ""
Endif
Return GetDBTypeStr(::cDBFType)

// ----------------------------------------------------------
// Retorna a data do ultimo update feito no arquivo 

METHOD LUPDATE() CLASS ZDBFFILE 
Return ::dLastUpd

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Realiza a leitura do Header do arquivo DBF 

METHOD _ReadHeader() CLASS ZDBFFILE 
Local cBuffer := space(32)
Local nYear, nMonth, nDay
Local cTemp := ''
Local nTemp := 0

If ::nHData == -1 
	UserException("_ReadHeader() ERROR - DBF File Not Opened")
Endif

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
	::cMemoExt := ".dbt"
	::nMemoType := 1
ElseIf ::cDBFType == '0xF5'
	// FoxPro 2.x (or earlier) with memo
	::lHasMemo := .T. 
	::cMemoExt := ".fpt"
	::nMemoType := 2
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

METHOD _ReadStruct() CLASS ZDBFFILE 
Local cFldBuff := space(32)
Local cFldName, cFldType  , nFldLen , nFldDec 

If ::nHData == -1 
	UserException("_ReadStruct() ERROR - DBF File Not Opened")
Endif

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
// Recupera o conteúdo de um campo da tabela 
// a partir da posiçao do campo na estrutura

METHOD FieldGet(nPos) CLASS ZDBFFILE 

If nPos > 0 .and. nPos <= ::nFldCount 

	IF ::aStruct[nPos][2] == 'M'
		// Campo MEMO, faz a leitura baseado 
		// no Bloco gravado na tabela 
		Return ::_ReadMemo( ::aGetRecord[nPos] )
	Else
		Return ::aGetRecord[nPos]
	Endif
	
Endif

Return NIL


// ----------------------------------------------------------
// Atualiza um valor na coluna informada do registro atual 
// Por hora nao critica nada, apenas coloca o valor no array 

METHOD FieldPut(nPos,xValue) CLASS ZDBFFILE 

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
METHOD FileName() CLASS ZDBFFILE 
Return ::cDataFile

// ----------------------------------------
// Retorna .T. caso o registro atual esteja deletado 
METHOD DELETED() CLASS ZDBFFILE 
Return ::lDeleted

// ----------------------------------------
// Retorna o tamanho do HEader
// -- O tamanho do Header é justamente a posicao do offser de dados 
// da tabela, após o final do Header. 

METHOD HEADER() CLASS ZDBFFILE 
Return ::nDataPos

// ----------------------------------------
// Retorna o tamanho ocupado pelo arquivo em bytes 
METHOD FileSize() CLASS ZDBFFILE 
Local nFileSize := 0
If ::lOpened
	nFileSize := fSeek(::nHData,0,2)
Endif
Return nFileSize

// ----------------------------------------
// Retorna o tamanho de um registro da tabela no arquivo 
// Cada campo MEMO ocupa 10 bytes 

METHOD RECSIZE() CLASS ZDBFFILE 
Return ::nRecLength

// ----------------------------------------
// Retorna o numero do registro atualmente posicionado

METHOD RECNO() CLASS ZDBFFILE 
If ::lEOF
	Return ::nLastRec+1
Endif
Return ::nRecno 

// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Lê o registro posicionado no offset de dados atual 

METHOD _ReadRecord() CLASS ZDBFFILE 
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

// Agora lê os demais campos e coloca no ::aGetRecord

For nI := 1 to ::nFldCount

	cTipo := ::aStruct[nI][2]
	nTam  := ::aStruct[nI][3]
	cValue := substr(cRecord,nBuffPos,nTam)

	If cTipo == 'C'
		::aGetRecord[nI] := cValue
		nBuffPos += nTam
	ElseIf cTipo == 'N'
		::aGetRecord[nI] := val(cValue)
		nBuffPos += nTam
	ElseIf cTipo == 'D'
		::aGetRecord[nI] := STOD(cValue)
		nBuffPos += nTam
	ElseIf cTipo == 'L'
		::aGetRecord[nI] := ( cValue=='T' )
		nBuffPos += nTam
	ElseIf cTipo == 'M'
		// Recupera o Offset do campo no DBT/FPT
		// aGetRecord sempre vai conter o OFFSET
		::aGetRecord[nI] := val(cValue)
		nBuffPos += nTam
	Endif
  
Next

// Reseta flags de BOF e EOF 
::lBOF := .F. 
::lEOF := .F. 

Return .T. 


// ----------------------------------------
// Insere um registro em branco no final da tabela
// Apos a inserção, voce pode fazer fieldput 
// e confirmar tudo com UPDATE 
METHOD Insert() CLASS ZDBFFILE

If ::lUpdPend
	// Antes de mais nada, se tem um update pendente
	// Faz primeiro o update 
	::Update()
Endif

// Limpa o conteudo do registro 
::_ClearRecord()

// Nao estou em BOF ou EOF, 
// Estou em modo de inserção de registro
::lBOF := .F. 
::lEOF := .F. 
            
// Incrementa uma unidade no contador de registros
::nLastRec++

// Recno atual = registro novo 
::nRecno := ::nLastRec

// Cria uma pendencia de update 
// O update vai fazer a inserção no final do arquivo 
::lUpdPend := .T. 

// Faz o update inserir o registro em branco 
IF ::Update()
               
	// Escreve o novo final de arquivo 
	FSeek(::nHData,0,2)
	fWrite(::nHData , chr(26) ) // !a = End Of File 

	// Atualiza o numero do ultimo registro no Header
	FSeek(::nHData,4)
	fWrite(::nHData , L2Bin(::nLastRec) )
	
	Return .T. 

Endif

Return .F. 

// ----------------------------------------
// Grava as alterações do registro atual na tabela 

METHOD Update() CLASS ZDBFFILE
Local cTipo , nTam , xValue
Local nI
Local cSaveRec := '' , nOffset
Local nMemoBlock, nNewBlock

If !::lUpdPend
	// Nao tem update pendente, nao faz nada
	Return
Endif

// ----------------------------------------
// Calcula o offset do registro atual baseado no RECNO

nOffset := ::nDataPos 
nOffset += (::nRecno * ::nRecLength)
nOffset -= ::nRecLength

// Primeiro byte do registro
// Flag de deletado 
cSaveRec := IIF(::lDeleted ,'*',' ') 

// Agora concatena os demais campos 
// Se nao houve alteração, monta o buffer com o valor lido

For nI := 1 to ::nFldCount

	cTipo := ::aStruct[nI][2]
	nTam  := ::aStruct[nI][3]
	nDec  := ::aStruct[nI][4]

	If cTipo == 'C'

		If ::aPutRecord[nI] != NIL 
			xValue := PADR( ::aPutRecord[nI] ,nTam)
			cSaveRec += xValue
			::aPutRecord[nI] := NIL
			::aGetRecord[nI] := xValue
		Else
			cSaveRec += ::aGetRecord[nI]
		Endif	

	ElseIf cTipo == 'N'

		If ::aPutRecord[nI] != NIL 
			xValue := ::aPutRecord[nI]
			cSaveRec += STR( xValue , nTam, nDec)
			::aPutRecord[nI] := NIL
			::aGetRecord[nI] := xValue
		Else
			cSaveRec += STR( ::aGetRecord[nI], nTam, nDec)
		Endif

	ElseIf cTipo == 'D'

		If ::aPutRecord[nI] != NIL 
			xValue := ::aPutRecord[nI]
			cSaveRec += DTOS( xValue )
			::aPutRecord[nI] := NIL
			::aGetRecord[nI] := xValue
		Else
			cSaveRec += DTOS( ::aGetRecord[nI] )
		Endif

	ElseIf cTipo == 'L'

		If ::aPutRecord[nI] != NIL 
			xValue := ::aPutRecord[nI]
			cSaveRec += IIF( xValue , 'T' , 'F')
			::aPutRecord[nI] := NIL
			::aGetRecord[nI] := xValue
		Else
			cSaveRec += IIF( ::aGetRecord[nI] , 'T' , 'F')
		Endif


	ElseIf cTipo == 'M'

		// Update de campo memo
		// Se realmente foi feito uma troca de valor, vamos ver o que fazer 
		// O bloco usado ( caso tivesse um ) está no ::aGetRecord[nI]

		If ::aPutRecord[nI] != NIL 

			// Pega o valor a atualizar no campo memo 
			xValue := ::aPutRecord[nI]
			
			// Verifica o numero do bloco usado 
			// 0 = sem bloco , sem conteudo 
			nMemoBlock := ::aGetRecord[nI]
			
			// Faz update deste memo. Se nao usava bloco, pode passar
			// a usar. Se já usava, se o memo nao for maior do que o já existente
			// ou nao atingir o limite do block, pode usar o mesmo espaço
			nNewBlock := ::oMemoFile:WRITEMEMO( nMemoBlock , xValue ) 
			
			If nNewBlock <> nMemoBlock
				// Trocou de bloco 
				cSaveRec += str( nNewBlock , 10 )
				// Atualiza a variavel de memoria 
				::aGetRecord[nI] := nNewBlock
			Else
				// Manteve o bloco 
				cSaveRec += str( nMemoBlock , 10 )
			Endif
		
		Else

			// Memo nao foi atualizado. 
			// Mantem valor atual 
			cSaveRec += STR( ::aGetRecord[nI] , 10 )

		Endif
		
	Endif

Next

IF len(cSaveRec) > ::nRecLength
	// Jamais, nunca. 
	// Se meu buffer em memoria passou o tamanho do registro 
	// do arquivo, algo deu muito errado ... 
	UserException("ZDBFFILE::Update() ERROR - FIELD BUFFER OVERFLOW")
Endif

// Posiciona o arquivo de dados no offset do registro 
FSeek(::nHData , nOffset )

// Agora grava o buffer do registro inteiro 
fWrite(::nHData , cSaveRec , ::nRecLength )

// Desliga flag de update pendente 
::lUpdPend := .F. 

// Atualiza o header do DBF com a data do ultimo update 
// caso necessario \

If Date() > ::dLastUpd 
	// Atualiza a data em memoria 
	::dLastUpd  := Date()
	// Regrava a nova data no header 
	::_SetLUpdate()
Endif

// Agora que o registro está atualizado, atualiza os indices 
aEval(::aIndexes , {|oIndex| oIndex:UpdateKey() })

Return .T. 

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Atualiza o header da tabela com a data atualizada
// do ultimo update realizado na tabela 
// Metodo chamado apenas quando a data do header 
// da tabela estiver desatualizada 

METHOD _SetLUpdate() CLASS ZDBFFILE
Local cBuffer

// Vai para o offset 1 -- 3 bytes com a data do ultimo update 
FSeek(::nHData,1)

// Monta a nova data em 3 bytes 
cBuffer := chr( Year(::dLastUpd)-2000 ) + ;
           chr( Month(::dLastUpd) ) + ;
           Chr( Day(::dLastUpd) ) 

// Grava a nova data no header 
fWrite(::nHData , cBuffer)

Return

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Limpa os campos do registro atual 
// ( Inicializa todos com os valores DEFAULT ) 

METHOD _ClearRecord()  CLASS ZDBFFILE

// Inicializa com o valor default os campos da estrutura 
_Super:_ClearRecord()

// Limpa flag de registro deletado 
::lDeleted := .F. 

Return

// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Lë um campo MEMO de um arquivo DBT 
// baseado no numero do bloco rececido como parametro 

METHOD _ReadMemo(nBlock) CLASS ZDBFFILE
Local cMemo := '' 

If nBlock > 0

	// Le o conteúdo do campo MEMO 
	cMemo := ::oMemoFile:ReadMemo(nBlock)

Endif

Return cMemo


// =================================================
// Funcoes Auxiliares internas da classe 
// =================================================

// Array com os tipos de DBF reconhecidos 
// O 3o elemento quando .T. indoca se o formato é suportado 

STATIC _aDbTypes := { { '0x02','FoxBASE'                                              , .F. } , ;
                      { '0x03','FoxBASE+/Dbase III plus, no memo'                     , .T. } , ;  // ####  (No Memo)
                      { '0x04','dBASE IV or IV w/o memo file'                         , .F. } , ;
                      { '0x05','dBASE V w/o memo file'                                , .F. } , ;
                      { '0x30','Visual FoxPro'                                        , .F. } , ;
                      { '0x31','Visual FoxPro, autoincrement enabled'                 , .F. } , ;
                      { '0x32','Visual FoxPro, Varchar, Varbinary, or Blob-enabled'   , .F. } , ;
                      { '0x43','dBASE IV SQL table files, no memo'                    , .F. } , ;
                      { '0x63','dBASE IV SQL system files, no memo'                   , .F. } , ;
                      { '0x7B','dBASE IV with memo'                                   , .F. } , ;
                      { '0x83','FoxBASE+/dBASE III PLUS, with memo'                   , .T. } , ;  // ####  DBT
                      { '0x8B','dBASE IV with memo'                                   , .F. } , ;
                      { '0x8E','dBASE IV w. SQL table'                                , .F. } , ;
                      { '0xCB','dBASE IV SQL table files, with memo'                  , .F. } , ;
                      { '0xF5','FoxPro 2.x (or earlier) with memo'                    , .T. } , ;  // ####  FPT
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
Local cRet := '(Unknow file -- NOT a DBF File)'
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

