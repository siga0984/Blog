#include "protheus.ch"

/* ====================================================================
Função     U_TSTDBIMG
Autor      Júlio Wittwer
Versão     1.150303
Data       27/02/2015
Descrição  Função de teste da classe ApDBImage, para lidar com imagens
armazenadas no Banco de Dados

Referências

http://tdn.totvs.com/display/tec/Como+alinhar+controles+em+uma+janela
http://tdn.totvs.com/display/tec/cGetFile

Alinhamento de componentes de interface

CONTROL_ALIGN_ALLCLIENT  // Alinha preenchendo todo o conteúdo da Janela ou Painel onde estiver
CONTROL_ALIGN_TOP        // Alinha ao Topo
CONTROL_ALIGN_BOTTOM     // Alinha ao Rodapé
CONTROL_ALIGN_LEFT       // Alinha à Esquerda
CONTROL_ALIGN_RIGHT      // Alinha à Direita
CONTROL_ALIGN_NONE       // Não utiliza alinhamento

==================================================================== */

// Imagem atual mostrada na interface
STATIC _cImageName := ""
STATIC _nImageFrom := 0
STATIC _cImageType := ""
STATIC _oBmp
STATIC _oScroll

User Function TSTDBIMG()
Local oDBImg

/*
If MsgYesNo("apaga banco de imagens")
	tcLink("MSSQL/STRESS","localhost",7890)
	tcdelfile("ZDBIMAGE")
Endif
*/

// Informa para a interface o tema DEFAULT
PTSetTheme("OCEAN")

// Cria uma instância do Manager de Imagens
oDBImg := APDBIMAGE():New()

// Inicializa a instância para uso
// Você já deve estar conectado no DBACcess

If !oDBImg:Open()
	MsgStop(oDBImg:cError,"Falha ao iniciar")
	Return
Endif

DEFINE DIALOG oDlg TITLE "Exemplo de Imagem no SGDB" FROM 0,0 TO 600,800 PIXEL

// Cria um painel para os botões de ações
// e Alinha o painel à esquerda
@ 0,0 MSPANEL oPanelMenu RAISED SIZE 90,1 OF oDlg
oPanelMenu:align := CONTROL_ALIGN_LEFT

@ 0,0 SCROLLBOX _oScroll HORIZONTAL VERTICAL SIZE 10, 10 OF oDlg
_oScroll:align := CONTROL_ALIGN_ALLCLIENT

// Cria o botão para carregar imagem do disco

@ 10,05 BUTTON oButton1 PROMPT "Ler do &Disco" ;
	ACTION (LoadFromDSK()) ;
	SIZE 080, 15 of oPanelMenu  PIXEL

@ 30,05 BUTTON oButton1 PROMPT "Ler do &RPO" ;
	ACTION (LoadFromRPO()) ;
	SIZE 080, 15 of oPanelMenu  PIXEL

@ 50,05 BUTTON oButton1 PROMPT "Ler do DB&IMAGE" ;
	ACTION (LoadFromDBI(oDBImg)) ;
	SIZE 080, 15 of oPanelMenu  PIXEL

@ 70,05 BUTTON oButton2 PROMPT "&Salvar em Disco" ;
	ACTION (SaveToDisk(oDBImg)) ;
	SIZE 080, 15 of oPanelMenu  PIXEL

@ 90,05 BUTTON oButton1 PROMPT "I&nserir no DBIMAGE" ;
	ACTION (InsertDBImg(oDBImg)) ;
	SIZE 080, 15 of oPanelMenu  PIXEL

@ 110,05 BUTTON oButton1 PROMPT "&Alterar o DBIMAGE" ;
	ACTION (UpdDBImg(oDBImg)) ;
	SIZE 080, 15 of oPanelMenu  PIXEL

@ 130,05 BUTTON oButton1 PROMPT "Apaga&r do DBIMAGE" ;
	ACTION (DelDBImg(oDBImg)) ;
	SIZE 080, 15 of oPanelMenu  PIXEL

@ 150,05 BUTTON oButton1 PROMPT "Es&tatísticas DBIMAGE" ;
	ACTION (DBImgStat(oDBImg)) ;
	SIZE 080, 15 of oPanelMenu  PIXEL

ACTIVATE DIALOG oDlg CENTER

// Fecha o DBImage
oDBImg:Close()

Return


/* --------------------------------------------------------
Le uma imagem ( bmp / jpg / png ) do disco e coloca na tela
A Leitura é feita direto pelo objeto de interface tBitmap())
-------------------------------------------------------- */
STATIC Function LoadFromDSK()
Local cImgFile := ''
Local cImgType := ''

cImgFile := cGetFile(NIL,NIL,0,"\",.F., NIL ,.T.)

If !empty(cImgFile)
	
	If !ImgFileType( cImgFile , @cImgType )
		msgStop("Arquivo ["+cImgFile+"] não suportado.")
		Return
	Endif
	
	// Cria um objeto bitmap dentro do scroll
	// Já pré-aloca uma área grande para caber qqer imagem

	ReNewBmp(cImgFile,.T.)
	
	// Imagem carregada do disco
	_cImageName := cImgFile
	_nImageFrom := 1
	_cImageType := cImgType

Endif

Return

/* --------------------------------------------------------
Le uma imagem gravada como resorce do RPO
Deve ser informado o nome do resource a ser carregado
O proprio componente de interface faz a carga
-------------------------------------------------------- */
STATIC Function LoadFromRPO()
Local cImgRes
Local cImgBuffer
Local cImgType

cImgRes := AskUser("Abrir imagem do RPO","Resource")

If !empty( cImgRes )

	If !LoadRes(cImgRes,@cImgType,@cImgBuffer)
		MsgStop("Resource ["+cImgRes+"] não encontrado.","LoadFromRPO")
		Return .F.
	Endif

	// Carrega a imagem como resource
	// direto pelo componente tBitMap
	
	ReNewBmp(cImgRes+'.'+cImgType , .F. )

	// Imagem carregada direto do RPO
	_cImageName := cImgRes
	_nImageFrom := 2
	_cImageType := cImgType
		
Endif

Return

/* --------------------------------------------------------
Carrega uma imagem do banco de dados
deve ser informado o ID da imagem
-------------------------------------------------------- */
STATIC Function LoadFromDBI(oDBImg)
Local cImgId
Local cImgBuffer := ''
Local cImgType := ''
Local cTmpFile := '\tmpimage.'

cImgId := AskUser("Abrir imagem do DBIMAGE","Id")

If !empty(cImgId)
	
	// Primeiro carrega do DBIMAGE
	If !oDBImg:ReadStr(cImgId,@cImgType,@cImgBuffer)
		MsgStop(oDBImg:cError,"LoadFromDBI")
		Return
	Endif
	
	// Imagem carregada do Banco na memoria
	// Agora salva em um arquivo temporario
	
	// A extensao do temporario deve ser o tipo da imagem
	cTmpFile += lower(cImgType)
	
	if file(cTmpFile)
		Ferase(cTmpFile)
	Endif

	// Salva a imagem no temporario do disco 	
	If !oDBImg:SaveTo( cTmpFile , cImgBuffer )
		MsgStop(oDBImg:cError,"LoadFromDBI")
		Return
	Endif
	
	ReNewBmp( cTmpFile , .T. )

	_cImageName := cImgId
	_nImageFrom := 3
	_cImageType := cImgType
	
Endif

Return


/* --------------------------------------------------------
Tela de interface para perguntar uma string ao usuario
Usada para pedir ID de imagem ou nome de resource
-------------------------------------------------------- */
STATIC Function AskUser(cTitle, cMessage , cRet )
Local oDlg
Local lOk  := .F.

If cRet == NIL
	cRet := space(30)
else
	cRet := padr( Upper(alltrim(cRet)) , 30 )
Endif

DEFINE DIALOG oDlg TITLE (cTitle) FROM 0,0 TO 60,340 PIXEL

@ 08,05 SAY oSay Prompt (cMessage) SIZE 40,13 OF oDlg PIXEL

@ 05,50 GET oGet VAR cRet PICTURE "@!" SIZE 80,13  OF oDlg PIXEL

DEFINE SBUTTON osBtn01  FROM 05 , 140  TYPE 01 ACTION ( lOk := .T. , oDlg:End() ) OF oDlg  ENABLE
DEFINE SBUTTON osBtn02  FROM 15 , 140  TYPE 02 ACTION ( lOk := .F. , oDlg:End() ) OF oDlg  ENABLE

ACTIVATE DIALOG oDlg CENTER

Return IIF( lOk , Alltrim(cRet) , '' )

/* --------------------------------------------------------
Grava a imagem atualmente em foco no disco
-------------------------------------------------------- */
STATIC Function SaveToDisk(oDBImg)
Local cImgSave
Local cImgType := ''
Local cImgBuffer := ''

If empty( _cImageName )
	MsgInfo("Nao há imagem carregada para salvar em disco.")
	Return
Endif

cImgSave := cGetFile(NIL,NIL,1,"\",.T., NIL ,.T.)

If empty(cImgSave)
	Return
Endif

If !LoadCurrentImage(oDBImg,@cImgBuffer,@cImgType)
	Return
Endif

If !"."$cImgSave
	cImgSave += ("." + cImgType )
Endif

If !file(cImgSave) .or. MsgYesNo("Arquivo ["+cImgSave+"] já existe. Sobrescrever ? ")

	// Se vai gravar e o arquivo já existe, apaga
	If file(cImgSave)
		ferase(cImgSave)
	Endif
	
	// Apos a imagem estar na memoria, salva em disco
	If !oDBImg:SaveTo(cImgSave,cImgBuffer)
		MsgStop(oDBImg:cError,"SaveToDisk")
		Return
	Endif
	
	MsgInfo("Arquivo ["+cImgSave+"] salvo.")
	
Endif

Return

/* --------------------------------------------------------
Insere a imagem em foco no Database
-------------------------------------------------------- */
STATIC Function InsertDBImg(oDBImg)
Local cImgId
Local cImgBuffer := ''
Local cImgType

If empty( _cImageName )
	MsgInfo("Nao há imagem carregada para inserir.")
	Return
Endif

cImgId := AskUser("Inserir no ImageID","ID da Imagem")

If empty(cImgId)
	Return 
Endif	

// Carrega imagem corrente na memoria
If !LoadCurrentImage(oDBImg,@cImgBuffer,@cImgType)
	Return
Endif

// Salva imagem no DBImage, inserindo
If !oDBImg:Insert( cImgId , cImgType , cImgBuffer )
	MsgStop(oDBImg:cError,"InsertDBImg")
	Return
Endif

MsgInfo("ImageId ["+cImgId+"] inserida no DBImage.")

Return

/* --------------------------------------------------------
Atualiza / altera a imagem em foco no Database
Deve ser informado o ID da imagem do banco a ser alterado
-------------------------------------------------------- */
STATIC Function UpdDBImg(oDBImg)
Local cImgId
Local cImgBuffer := ''
Local cImgType := ''

If empty( _cImageName )
	MsgInfo("Nao há imagem carregada para atualizar.")
	Return
Endif

cImgId := AskUser("Atualizar o ImageID","ID da Imagem")

If empty(cImgId)
	Return
Endif

// Carrega imagem corrente na memoria
If !LoadCurrentImage(oDBImg,@cImgBuffer,@cImgType)
	Return
Endif

// Atualiza imagem no DbImage	
If !oDBImg:Update( cImgId , cImgType, cImgBuffer )
	MsgStop(oDBImg:cError,"UpdateDBImg")
	Return
Endif
	
MsgInfo("ImageId ["+cImgId+"] atualizada no DBImage.")
	
Return

/* --------------------------------------------------------
Deleta uma imagem do banco de imagens
Deve ser informado o ID da imagem do Banco a ser apagada
-------------------------------------------------------- */
STATIC Function DelDBImg(oDBImg)
Local cImgId
Local cImgBuffer := ''

cImgId := AskUser("Apagar do ImageID","ID da Imagem")

If empty(cImgId)
	Return
Endif

// Apaga a imagem do DBimage	
If !oDBImg:Delete( cImgId , cImgBuffer )
	MsgStop(oDBImg:cError,"DelDBImg")
	Return
Endif
	
MsgInfo("ImageId ["+cImgId+"] apagada no DBImage.")

Return

/* --------------------------------------------------------
Mostra mensagem de estatisticas do banco de imagens
-------------------------------------------------------- */
STATIC Function DBImgStat(oDBImg)
Local aStatus := {}
Local nI
Local cHtml := '<html><pre><hr>'

If !oDBImg:Status( @aStatus )
	MsgStop(oDBImg:cError,"DBImgStat")
	Return
Endif

For nI := 1 to len(aStatus)
	cHtml += padr(aStatus[nI][1],20,'.')+cValToChar(aStatus[nI][2]) + CRLF
Next

cHtml += '</pre></html>'

Return MSgInfo(cHtml,'DBI Status')

/* --------------------------------------------------------
Recebe cImgType por referencia (@)
Retorna .T. e preenche o cImgType caso o arquivo
tenha uma extensao suportada
-------------------------------------------------------- */

STATIC Function ImgFileType( cImgFile , /* @ */ cImgType )
Local cExt := ''
cImgType := ''
SplitPath(cImgFile,,,,@cExt)
If Upper('/'+cExt+'/')$'/.BMP/.PNG/.JPG/'
	cImgType := substr(cExt,2)
Endif
Return !Empty(cImgType)


/* --------------------------------------------------------
-------------------------------------------------------- */
STATIC Function LoadCurrentImage(oDBImg,  /* @ */ cImgBuffer , /* @ */ cImgType )
Local nPos

cImgBuffer := ''
cImgType   := ''

If _nImageFrom == 1 
	
	// Le Arquivo do disco para a memoria
	// Usa metodo da DBImage para acesso a disco 
	If !oDBImg:LoadFrom( _cImageName , @cImgBuffer )
		MsgStop(oDBImg:cError,"LoadImage")
		Return .F. 
	Endif        
	      
	// O tipo da imagem é a extensão do arquivo
	nPos := rat(".",_cImageName)
	If nPos > 0 
		cImgType := substr(_cImageName,nPos+1)
	Endif	

ElseIf _nImageFrom == 2
	
	If !LoadRes(_cImageName,@cImgType,@cImgBuffer)
		MsgStop("Resource ["+_cImageName+"] não encontrado.","LoadImage")
		Return .F.
	Endif

ElseIf _nImageFrom == 3
	
	// Carrega DBImage para a memoria
	If !oDBImg:ReadStr(_cImageName,@cImgType,@cImgBuffer)
		MsgStop(oDBImg:cError,"LoadImage")
		Return .F. 
	Endif

Else

	MsgStop("Nao há imagem na interface.","LoadImage")	
	Return .F.
	
Endif

Return .T.
 
/* --------------------------------------------------------
Le resource do RPO para a memoria E identifica o tipo.
Deve receber apenas o nome do resource, sem extensao
-------------------------------------------------------- */
 
STATIC Function LoadRes(cResName , /*@*/ cImgType, /*@*/ cImgBuffer)
cImgBuffer := GETAPORES(cResName+'.BMP')
If !empty(cImgBuffer)
	cImgType := 'BMP'
Else
	cImgBuffer := GETAPORES(cResName+'.PNG')
	If !empty(cImgBuffer)
		cImgType := 'PNG'
	Else
		cImgBuffer := GETAPORES(cResName+'.JPG')
		If !empty(cImgBuffer)
			cImgType := 'JPG'
		Endif
	Endif
Endif
Return !empty(cImgType)


/* --------------------------------------------------------
Refefine o objeto de interface para mostrar 
a imagem na tela ( Resource ou Arquivo ) 
-------------------------------------------------------- */

STATIC Function ReNewBMP( cName , lFile )

If _oBmp != NIL
	// Se já tinha uma imagem na interface, "fecha"
	FreeObj(_oBmp)
Endif

If lFile
	// Carga a partir de arquivo
	@ 0,0 BITMAP _oBmp  FILE (cName) SCROLL OF _oScroll PIXEL
Else
	// Carga a partir de resource no RPO 
	@ 0,0 BITMAP _oBmp  RESOURCE (cName) SCROLL OF _oScroll PIXEL
Endif

// Habilita a imagem para "se encaixar" na interface 
_oBmp:lAutoSize := .T.

Return




