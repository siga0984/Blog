#include 'protheus.ch'
#include 'fileio.ch'

/* ==========================================================

Classe 		ZDBTFILE
Autor		Julio Wittwer
Data		01/2019
Descrição 	Classe de manutenção de arquivo DBF MEMO
			Formato DBT 
			
Observação  Somente leitura implementada 

========================================================== */

CLASS ZDBTFILE

   DATA oDBF
   DATA cFileName
   DATA nHMemo

   METHOD NEW()
   METHOD OPEN()
   METHOD CLOSE()
   METHOD READMEMO()
   METHOD WRITEMEMO()

ENDCLASS
              
// ----------------------------------------------------------

METHOD NEW(_oDBF,_cFileName) CLASS ZDBTFILE

::oDBF      := _oDBF
::cFileName := _cFileName
::nHMemo    := -1

Return self


// ----------------------------------------------------------

METHOD OPEN() CLASS ZDBTFILE

// Abre o arquivo MEMO 
::nHMemo := FOpen(::cFileName)

IF ::nHMemo == -1
	Return .T. 
Endif

Return .T. 

// ----------------------------------------------------------

METHOD CLOSE() CLASS ZDBTFILE

IF ::nHMemo != -1
	fClose(::nHMemo)
	::nHMemo := -1
Endif

Return


// ----------------------------------------------------------

METHOD READMEMO(nBlock) CLASS ZDBTFILE
Local cMemo   := ''
Local cBlock  := space(512)
Local nFilePos := nBlock * 512
Local nEndPos

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

Return cMemo


METHOD WRITEMEMO( nBlock , cMemo ) CLASS ZDBTFILE
UserException("*** WRITEMEMO NOT AVAILABLE FOR DBT MEMO FILE ***")
Return




