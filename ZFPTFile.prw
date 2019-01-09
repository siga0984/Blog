#include 'protheus.ch'
#include 'fileio.ch'

/* ==========================================================

Classe 		ZFPTFILE
Autor		Julio Wittwer
Data		01/2019
Descrição 	Classe de manutenção de arquivo DBF MEMO
			Formato FTP ( FoxPro2 ) 

Implementação de Leitura e Gravação 
			
========================================================== */

CLASS ZFPTFILE

   DATA oDBF
   DATA cFileName
   DATA nFileSize
   DATA nHMemo
   DATA nNextBlock
   DATA nBlockSize

   METHOD NEW()
   METHOD CREATE()
   METHOD OPEN()
   METHOD CLOSE()
   METHOD READMEMO()
   METHOD WRITEMEMO()

ENDCLASS
              
// ----------------------------------------------------------

METHOD NEW(_oDBF,_cFileName) CLASS ZFPTFILE

::oDBF       := _oDBF
::cFileName  := _cFileName
::nHMemo     := -1
::nBlockSize := 16384
::nNextBlock := 1 
::nFileSize  := 0 

Return self


// ----------------------------------------------------------

METHOD CREATE() CLASS ZFPTFILE
Local nHFile, cHeader
Local cNextBlock, cBlockSize
Local cFiller1, cFiller2

// Cria o arquivo MEMO 
nHFile := FCreate(::cFileName)

If nHFile == -1
	Return .F.
Endif

// Cria o Header ( 512 bytes ) 
// Block Size = 16 K

/*
 0 | Number of next        |  ^
 1 | available block       |  |
 2 | for appending data    | Header
 3 | (binary)           *1 |  |
   |-----------------------|  |
 4 | ( Reserved )          |  |    
 5 |                       |  |
   |-----------------------|  |
 6 | Size of blocks N   *1 |  |
 7 |                    *2 |  |
*/

// ----- Build 512 Bytes FPT Empty File Header -----
cNextBlock := NtoBin4(1)                     // Proximo bloco livre para escrita 
cFiller1   := chr(0) + chr(0)               
cBlockSize := NToBin2( ::nBlockSize )                // Tamanho do Bloco 
cFiller2   := replicate( chr(0) , 504 )       

cHeader := cNextBlock + cFiller1 + cBlockSize +cFiller2

fWrite(nHFile,cHEader,512)
FClose(nHFile)

Return .T. 

// ----------------------------------------------------------

METHOD OPEN(lExclusive,lCanWrite) CLASS ZFPTFILE
Local cBuffer := ''
Local nFMode := 0 

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

// Abre o arquivo MEMO 
::nHMemo := FOpen(::cFileName,nFMode)

IF ::nHMemo == -1
	Return .F. 
Endif

// P{ega o tamanho do arquivo 
::nFileSize := fSeek(::nHMemo,0,2)

// Le  o Header do arquivo ( 512 bytes ) 
fSeek(::nHMemo,0)
fRead(::nHMemo,@cBuffer,512)

// Pega o numero do proximo bloco para append 
::nNextBlock  := Bin4toN( substr(cBuffer,1,4) )

// Le o Block Size do arquivo 
::nBlockSize := Bin2ToN( substr(cBuffer,7,2) )

conout("")
conout("FPT Next Append Block ......: "+cValToChar(::nNextBlock))
conout("FPT Block Size ...... ......: "+cValToChar(::nBlockSize))
conout("")


Return .T. 

// ----------------------------------------------------------

METHOD CLOSE() CLASS ZFPTFILE

IF ::nHMemo != -1
	fClose(::nHMemo)
	::nHMemo := -1
Endif

Return


// ----------------------------------------------------------

METHOD READMEMO(nBlock) CLASS ZFPTFILE
Local cMemo   := ''
Local cBlock  := space(8)
Local nFilePos := nBlock * ::nBlockSize
Local nRecType

// Leitura de MEMO em Arquivo FPT
// Offset 1-4 Record type 
// Offset 5-8 Tamanho do registro

fSeek(::nHMemo , nFilePos)
fRead(::nHMemo,@cBlock,8)

// Pega o tipo do Registro ( Offset 0 size 4 ) 
nRecType := Bin4toN( substr(cBlock,1,4) )

/*	
Record Type 
00h	Picture
01h	Memo
02h	Object
*/

If nRecType <> 1 
	UserException("Unsupported MEMO Record Type "+cValToChar(nRecType))
Endif

// Obrtém o tamanho do registro ( Offset 4 Size 4 ) 
nMemoSize := Bin4toN( substr(cBlock,5,4) )

// Lê o registro direto para a memória
fRead(::nHMemo,@cMemo,nMemoSize)

Return cMemo


// ------------------------------------------------------------
// Atualiza ou insere um valor em um campo memo 
// Se nBlock = 0 , Conteudo novo 
// Se nBlock > 0 , Conteudo j[a existente 

METHOD WRITEMEMO( nBlock , cMemo ) CLASS ZFPTFILE
Local nTamFile
Local nFiller
Local cBuffer := ''
Local nFilePos
Local nMemoSize
Local nChuckSize
Local nUsedBlocks
Local nMaxMemoUpd

If nBlock > 0 

	// Estou atualizamdo um memo já gravado. 
	// verifica se cabe no blocco atual 
	// Se nao couber , usa um novo . 
	// Primeiro lê o tamanho do campo atual
	// e quantos blocos ele usa 
	
	cBuffer  := space(8)
	nFilePos := nBlock * ::nBlockSize
	fSeek(::nHMemo , nFilePos)
	fRead(::nHMemo,@cBuffer,8)
	nMemoSize := Bin4toN( substr(cBuffer,5,4) )
	nChuckSize :=  nMemoSize + 8

	// Calcula quantos blocos foram utilizados 
	nUsedBlocks := int( nChuckSize / ::nBlockSize )
	IF nChuckSize > ( nUsedBlocks * ::nBlockSize)
		nUsedBlocks++
	Endif

	// Calcula o maior campo memo que poderia reapproveitar 
	// o(s) bloco(s) usado(s), descontando os 8 bytes de controle
	nMaxMemoUpd := (nUsedBlocks * ::nBlockSize) - 8 

	If len(cMemo) > nMaxMemoUpd

		// Passou, nao dá pra reaproveitar. 
		// Zera o nBlock, para alocar um novo bloco 
		nBlock := 0

	Else

		// Cabe no mesmo bloco ... remonta o novo buffer
		// e atualiza o campo 

		nMemoSize  := len(cMemo)
		nChuckSize :=  nMemoSize + 8
		
		cBuffer := NtoBin4( 01 ) // Tipo de registro = Memo 
		cBuffer += NtoBin4( nMemoSize )
		cBuffer += cMemo

		// Posiciona no inicio do bloco já usado 
		fSeek(::nHMemo , nFilePos)
		fWrite(::nHMemo,cBuffer,nChuckSize)
	
	
	Endif	
	
Endif

If nBlock == 0 
	
	// Estou inserindo um conteudo em um campo memo ainda nao utilizado. 
	// Ou estou usando um novo bloco , pois o campo memo 
	// nao cabe no bloco anteriormente utilizado 
	// Utiliza o proximo bloco para inserção do Header
	
	nTamFile := ::nNextBlock * ::nBlockSize

	If ::nFileSize < nTamFile
		// Se o ultimo bloco do arquivo ainda nao foi preenchido com um "Filler" 
		// até o inicio do proximo bloco , preenche agora
		nFiller := nTamFile - ::nFileSize
		fSeek(::nHMemo,0,2)
		fWrite(::nHMemo , replicate( chr(0) , nFiller ) , nFiller ) 
	Endif
	
	// Monta o buffer para gravar 
	
	nMemoSize := len(cMemo)
	
	cBuffer := NtoBin4( 01 ) // Tipo de registro = Memo 
	cBuffer += NtoBin4( nMemoSize )
	cBuffer += cMemo
	
	// Tamanho do campo memo no bloco 
	// soma 8 bytes ( tipo , tamanho ) 
	nChuckSize :=  nMemoSize + 8
	
	// Posiciona no proximo bloco livre e grava
	nFilePos := ::nNextBlock * ::nBlockSize 
	fSeek(::nHMemo,nFilePos)
	fWrite(::nHMemo,cBuffer,nChuckSize)
    
	// Guarda o bloco usado para retorno 
	nBlock := ::nNextBlock
	
	// Calcula quantos blocos foram utilizados 
	nUsedBlocks := int( nChuckSize / ::nBlockSize )
	IF nChuckSize > ( nUsedBlocks * ::nBlockSize)
		nUsedBlocks++
	Endif

	// Agora define o proximo bloco livre 
	// Soma no ultimo valor a quantidade de blocos usados 
	::nNextBlock += nUsedBlocks
	
	// Agora atualiza no Header
	fSeek(::nHMemo,0)
	fWrite( ::nHMemo , nToBin4(::nNextBlock) , 4 )

Endif

// Apos a gravacao, atualiza o tamanho do arquivo 
::nFileSize := fSeek(::nHMemo,0,2)

// Retorna o numero do bloco usado para a operação 
Return nBlock

// ------------------------------------------------------------
// Converte buffer de 4 bytes ( 32 Bits ) no seu valor numerico  

STATIC Function Bin4toN(cBin4)
Local nByte1,nByte2,nByte3,nByte4

nByte1 := asc(substr(cBin4,1,1))
nByte2 := asc(substr(cBin4,2,1))
nByte3 := asc(substr(cBin4,3,1))
nByte4 := asc(substr(cBin4,4,1))

If nByte3 > 0
	nByte4 += ( nByte3 * 256 )
Endif
If nByte2 > 0
	nByte4 += ( nByte2 * 65536 )
Endif
If nByte1 > 0
	nByte4 += ( nByte1 * 16777216 )
Endif

Return nByte4


// ------------------------------------------------------------
// Converte valor numérico em buffer de 4 bytes ( 32 Bits ) 
// ( HIgh Byte First ) 

STATIC Function NtoBin4(nNum)
Local cBin4 := '' , nTmp
While nNum > 0
	nTmp := nNum % 256 
	cBin4 := chr(nTmp) + cBin4
	nNum := ( ( nNum - nTmp ) / 256 )
Enddo
While len(cBin4) < 4
	cBin4 := CHR(0) + cBin4
Enddo
Return cBin4



// ------------------------------------------------------------
// Converte buffer de 2 bytes ( 16 Bits ) no seu valor numerico  
// ( HIgh Byte First ) 

STATIC Function Bin2toN(cBin4)
Local nByte1,nByte2

nByte1 := asc(substr(cBin4,1,1))
nByte2 := asc(substr(cBin4,2,1))

If nByte1 > 0
	nByte2 += ( nByte1 * 256 )
Endif

Return nByte2

// ------------------------------------------------------------
// Converte valor numérico em buffer de 2 bytes ( 16 Bits ) 
// ( High Byte First ) 

STATIC Function NtoBin2(nNum)
Local cBin2 := '' , nTmp
Local nL := ( nNum % 256 ) 
Local nH := ( nNum-nL ) / 256 
Return chr(nH) + chr(nL)

