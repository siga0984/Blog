#include "protheus.ch"

/* ---------------------------------------------------
Classe    ApDBImage
Autor     Júlio Wittwer
Data      27/02/2015         
Versão    1.150308
Descrição Classe para encapsular leitura e gravação de 
          imagens em tabela do SGDB através do DBACCESS

Observação

Como apenas o banco MSSQL aceita conteúdo binário ( ASCII 0 a 255 )
para campos MEMO, e os bancos ORACLE e DB2 ( quando usado BLOB ), 
para servir para todos os bancos, a imagem é gravada no banco 
usando Encode64 -- para converter conteúdo binário em Texto 
codificado em Base64, a maior imagem nao pode ter mais de 745000 bytes

Referências

http://tdn.totvs.com/display/tec/Acesso+ao+banco+de+dados+via+DBAccess
http://tdn.totvs.com/pages/viewpage.action?pageId=6063692
http://tdn.totvs.com/display/tec/Encode64
http://tdn.totvs.com/display/tec/Decode64

--------------------------------------------------- */

#define MAX_IMAGE_SIZE 	745000

CLASS APDBIMAGE

	// Propriedades
  DATA bOpened 
  DATA cError

	// Métodos 
  METHOD New()    
  METHOD Open()
  METHOD Close() 
  METHOD ReadStr( cImgId , /* @ */ cImgType , /* @ */ cImgBuffer )   
  METHOD Insert( cImgId , cImgType , /* @ */ cImgBuffer )   
  METHOD Update( cImgId , cImgType , /* @ */ cImgBuffer )   
  METHOD Delete( cImgId )   
  METHOD Status()   

	// Metodos de acesso de imagens no disco
  METHOD LoadFrom( cFile, cImgBuffer )
  METHOD SaveTo( cFile, cImgBuffer )
   
ENDCLASS

/* ---------------------------------------------------------
Construtor da classe de Imagens no SGDB
Apenas inicializa propriedades
-------------------------------------------------------- */
METHOD New() CLASS APDBIMAGE
::bOpened := .F.
::cError := ''
Return self


/* ---------------------------------------------------------
Abre a tabela de imagens no SGDB
Conecta no DBAccess caso nao haja conexão
--------------------------------------------------------- */

METHOD Open( ) CLASS APDBIMAGE
Local nDBHnd := -1
Local aStru := {}
Local cOldAlias := Alias()

::cError := ''  

IF ::bOpened 
	// Ja estava aberto, retorna direto
	Return .T.
Endif

If !TcIsConnected()  
	// Se não tem conexão com o DBAccess, cria uma agora
	// Utiliza as configurações default do appserver.ini
	nDBHnd := tcLink()
	If nDBHnd < 0
		::cError := "TcLink() error "+cValToChar(nDbHnd)
		Return .F.
	Endif
Endif

If !TCCanOpen("ZDBIMAGE")
	
	// Cria array com a estrutura da tabela
	aAdd(aStru,{"ZDB_IMGID"  ,"C",40,0})
	aAdd(aStru,{"ZDB_TYPE"   ,"C",3,0}) // BMP JPG PNG 
	aAdd(aStru,{"ZDB_HASH"   ,"C",32,0}) 
	aAdd(aStru,{"ZDB_SIZE"   ,"N",8,0})
	aAdd(aStru,{"ZDB_MEMO"   ,"M",10,0})

	// Cria a tabela direto no SGDB
	DBCreate("ZDBIMAGE",aStru,"TOPCONN")
	
	// Abre em modo exclusivo para criar o índice de ID
	USE  ("ZDBIMAGE") ALIAS ZDBIMAGE EXCLUSIVE NEW VIA "TOPCONN"
	
	If NetErr()
		::cError := "Failed to open [ZDBIMAGE] on EXCLUSIVE Mode"
		Return
	Endif
	
	// Cria o índice por ID da imagem 
	INDEX ON ZDB_IMGID TO ("ZDBIMAGE1")
	
	// Fecha a tabela
	USE
	
Endif
         
// Abre em modo compartilhado
USE  ("ZDBIMAGE") ALIAS ZDBIMAGE SHARED NEW VIA "TOPCONN"
If NetErr()
	::cError := "Failed to open [ZDBIMAGE] on SHARED Mode"
	Return .F.
Endif

DbSetIndex("ZDBIMAGE1")
DbSetOrder(1)

::bOpened := .T.

If !Empty(cOldAlias) .and. Select(cOldAlias) > 0
	DbSelectArea(cOldAlias)
Endif

Return ::bOpened

/* ---------------------------------------------------------
Le uma imagem do banco para a memoria
recebe o nome da imgem, retorna por referencia o tipo
da imagem e seu conteudo 
-------------------------------------------------------- */
METHOD ReadStr( cImgId , /* @ */cImgType, /* @ */ cImgBuffer ) CLASS APDBIMAGE

::cError := ''  

If !::bOpened
	::cError := "APDBIMAGE:ReadStr() Error: Instance not opened."
	Return .F.
Endif

If empty(cImgId)
	::cError := "APDBIMAGE:ReadStr() Error: ImageId not specified."
	Return .F. 
Endif

cImgId := Lower(cImgId)

If !ZDBIMAGE->(DbSeek(cImgId))
	::cError := "APDBIMAGE:ReadStr() ImageId ["+cImgId+"] not found."
	Return .F.
Endif

// Caso a imagem com o ID informado seja encontrada
// Carrega o buffer da imagem para a variável de memória
cImgBuffer := Decode64(ZDBIMAGE->ZDB_MEMO)
cImgType   := ZDBIMAGE->ZDB_TYPE

Return .T.


/* ---------------------------------------------------------
Insere uma imagem na tabela de imagens do SGDB
Recebe o ID da imagem, o tipo e o buffer 
-------------------------------------------------------- */
METHOD Insert( cImgId , cImgType, cImgBuffer ) CLASS APDBIMAGE
Local bOk  := .F.

::cError := ''  

If !::bOpened
	::cError := "APDBIMAGE:Insert() Error: Instance not opened."
	Return .F. 
Endif

If empty(cImgId)
	::cError := "APDBIMAGE:Insert() Error: ImageId not specified."
	Return .F. 
Endif

If empty(cImgType)
	::cError := "APDBIMAGE:Insert() Error: ImageType not specified."
	Return .F. 
Endif

cImgId := Lower(cImgId)
cImgType := Lower(cImgType)

If !ZDBIMAGE->(DbSeek(cImgId))
	// Se a imagem não existe, insere
	ZDBIMAGE->(DBAppend(.T.))
	ZDBIMAGE->ZDB_IMGID := cImgId
	ZDBIMAGE->ZDB_TYPE  := cImgType
	ZDBIMAGE->ZDB_SIZE  := len(cImgBuffer)
	ZDBIMAGE->ZDB_HASH  := Md5(cImgBuffer,2) // Hash String Hexadecimal
	ZDBIMAGE->ZDB_MEMO  := Encode64(cImgBuffer)
	ZDBIMAGE->(DBRUnlock())
	bOk := .T.
else
	::cError := 'Image Id ['+cImgId+'] already exists.'
Endif

Return bOk

/* ---------------------------------------------------------
Atualiza uma imagem ja existente no banco de imagens
Recebe ID, tipo e buffer
-------------------------------------------------------- */
METHOD Update( cImgId , cImgType, cImgBuffer ) CLASS APDBIMAGE

::cError := ''  

If !::bOpened
	::cError := "APDBIMAGE:Update() Error: Instance not opened."
	Return .F. 
Endif

If empty(cImgId)
	::cError := "APDBIMAGE:Update() Error: ImageId not specified."
	Return .F. 
Endif

If empty(cImgType)
	::cError := "APDBIMAGE:Update() Error: ImageType not specified."
	Return .F. 
Endif

cImgId := Lower(cImgId)
cImgType := Lower(cImgType)
   
If !ZDBIMAGE->(DbSeek(cImgId))
	::cError := 'Image Id ['+cImgId+'] not found.'
	Return .F.
Endif

// Se a imagem  existe, atualiza
IF !ZDBIMAGE->(DbrLock(recno()))
	::cError := 'Image Id ['+cImgId+'] update lock failed.'
	Return .F.
Endif

ZDBIMAGE->ZDB_TYPE  := cImgType
ZDBIMAGE->ZDB_SIZE  := len(cImgBuffer)
ZDBIMAGE->ZDB_HASH  := MD5(cImgBuffer,2) // Hash String Hexadecimal
ZDBIMAGE->ZDB_MEMO  := Encode64(cImgBuffer)

ZDBIMAGE->(DBRUnlock())

Return .T.


/* ---------------------------------------------------------
Deleta fisicamente uma imagem da Tabela de Imagens
-------------------------------------------------------- */

METHOD Delete( cImgId , lHard ) CLASS APDBIMAGE
Local nRecNo

::cError := ''  

If !::bOpened
	::cError := "APDBIMAGE:Delete() Error: Instance not opened."
	Return .F. 
Endif

If empty(cImgId)
	::cError := "APDBIMAGE:Delete() Error: ImageId not specified."
	Return .F. 
Endif

If !ZDBIMAGE->(DbSeek(cImgId))
	::cError := 'Image Id ['+cImgId+'] not found.'
	Return .F.
Endif

// Se a imagem  existe, marca o registro para deleção
nRecNo := ZDBIMAGE->(recno())

// Mesmo que a deleção seja fisica, eu garanto 
// o lock do registro na camada do dbaccess
If !ZDBIMAGE->(DbrLock(nRecNo))
	::cError := 'Image Id ['+cImgId+'] delete lock failed.'
	Return .F.
Endif

// Deleta fisicamente no SGBD
nErr := TcSqlExec("DELETE FROM ZDBIMAGE WHERE R_E_C_N_O_ = " + cValToChar(nRecNo) )
If nErr < 0
	::cError := 'Image Id ['+cImgId+'] delete error: '+TcSqlError()
Endif

// Solto o lock do registro no DBAccess
ZDBIMAGE->(DBRUnlock())

Return .T.

/* ---------------------------------------------------------
Fecha a tabela de imagens
-------------------------------------------------------- */

METHOD Close() CLASS APDBIMAGE

If Select('ZDBIMAGE') > 0
	ZDBIMAGE->(DbCloseArea())
Endif

::cError := ''  
::bOpened := .F.

Return .T. 

/* ---------------------------------------------------------
Metodo      Status()
Classe      APDBIMAGE
Descrição   Monta array por referencia contendo as informações da base 
            de imagens: Quantidade de registros total, tamanho estimado 
            total das imagens, quantidade de registros marcados para 
            deleção e tamanho estimado de imagens marcadas para deleçao 
-------------------------------------------------------- */

METHOD Status( /* @ */ aStat ) CLASS APDBIMAGE
Local cOldAlias := Alias()
Local cQuery                  
Local nCountAll := 0
Local nSizeAll := 0

::cError := ''  
aStat := {}

If !::bOpened
	::cError := "APDBIMAGE:Status() Error: Instance not opened."
	Return .F. 
Endif

// Conta quantas imagens tem na tabela, por tipo 
cQuery := "SELECT ZDB_TYPE, count(*) AS TOTAL"+;
          " FROM ZDBIMAGE GROUP BY ZDB_TYPE ORDER BY ZDB_TYPE"
          
USE (TcGenQry(,,cQuery)) ALIAS QRY EXCLUSIVE NEW VIA "TOPCONN"
While !eof()
	aadd(aStat , {"TOTAL_COUNT_"+QRY->ZDB_TYPE,QRY->TOTAL})
	nCountAll += QRY->TOTAL
	DbSkip()
Enddo
USE

// Acrescenta total de imagens
aadd(aStat , {"TOTAL_COUNT_ALL",nCountAll})
                                                                                  
// Levanta o total de bytes usados por tipo de imagem
cQuery := "SELECT ZDB_TYPE, SUM(ZDB_SIZE) AS TOTAL"+;
          " FROM ZDBIMAGE GROUP BY ZDB_TYPE ORDER BY ZDB_TYPE"
          
USE (TcGenQry(,,cQuery)) ALIAS QRY EXCLUSIVE NEW VIA "TOPCONN"
While !eof()
	aadd(aStat , {"TOTAL_SIZE_"+QRY->ZDB_TYPE,QRY->TOTAL})
	nSizeAll += QRY->TOTAL
	DbSkip()
Enddo
USE

// Acrescenta total de bytes usados 
aadd(aStat , {"TOTAL_SIZE_ALL",nSizeAll})

If !Empty(cOldAlias)
	DbSelectArea(cOldAlias)
Endif

Return .T. 


/* ---------------------------------------------------------
Ler um arquivo de imagem do disco para a memoria
Nao requer que a instancia esteja inicializada / Aberta
--------------------------------------------------------- */

METHOD LoadFrom( cFile, /* @ */ cImgBuffer ) CLASS APDBIMAGE
Local nH, nSize, nRead
::cError := ''  

If !file(cFile)
	::cError := "APDBIMAGE:LoadFrom() Error: File ["+cFile+"]not found."
	Return .F. 
Endif

nH := Fopen(cFile,0)

If nH == -1 
	::cError := "APDBIMAGE:LoadFrom() File Open Error ( FERROR "+cValToChar( Ferror() )+")" 
	Return .F. 
Endif

nSize := fSeek(nH,0,2)
fSeek(nH,0)

If nSize <= 0  
	::cError := "APDBIMAGE:LoadFrom() File Size Error : Empty File" 
	fClose(nH)
	Return .F. 
Endif

If nSize > MAX_IMAGE_SIZE
	::cError := "APDBIMAGE:LoadFrom() File TOO BIG ("+ cValToChar(nSize) +" bytes)" 
	fClose(nH)
	Return .F. 
Endif

// Aloca buffer para ler o arquivo do disco 
// e le o arquivo para a memoria
cImgBuffer := space(nSize)
nRead := fRead(nH,@cImgBuffer,nSize)           

// e fecha o arquivo no disco 
fClose(nH)

If nRead < nSize
	cImgBuffer := ''
	::cError := "APDBIMAGE:LoadFrom() Read Error ( FERROR "+cValToChar( Ferror() )+")" 
	Return .F. 
Endif

Return .T. 


/* ---------------------------------------------------------
Gravar um arquivo de imagem no disco a partir de uma imagem na memoria
Nao requer que a instancia esteja inicializada / Aberta
--------------------------------------------------------- */

METHOD SaveTo( cFile, cImgBuffer ) CLASS APDBIMAGE
Local nH, nSize , nSaved 
::cError := ''  

If file(cFile)
	::cError := "APDBIMAGE:SaveTo() Error: File ["+cFile+"] alreay exists."
	Return .F. 
Endif

// Cria o arquivo no disco 
nH := fCreate(cFile)

If nH == -1 
	::cError := "APDBIMAGE:SaveTo() File Create Error ( FERROR "+cValToChar( Ferror() )+")" 
	Return .F. 
Endif
     
// Calcula tamanho do buffer de memoria
// e grava ele no arquivo 
nSize := len(cImgBuffer)
nSaved := fWrite(nH,cImgBuffer)

// Fecha o arquivo 
fClose(nH)

If nSaved < nSize
	::cError := "APDBIMAGE:SaveTo() Write Error ( FERROR "+cValToChar( Ferror() )+")" 
	Return .F. 
Endif

Return .T. 
