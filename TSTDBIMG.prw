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

STATIC _aImageName := {"","",""} // [1] FilePath [2] Resource [3] cImageID 
STATIC _aImageType := {"","",""} // [1] FilePath [2] Resource [3] cImageID 

User Function TSTDBIMG()
Local oDBImg

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

@ 0,0 SCROLLBOX oScroll HORIZONTAL VERTICAL SIZE 10, 10 OF oDlg 
oScroll:align := CONTROL_ALIGN_ALLCLIENT

// Cria um objeto bitmap dentro do scroll
// Já pré-aloca uma área grande para caber qqer imagem
@ 0,0 BITMAP oBmp  SIZE 1024,768 OF oScroll PIXEL
oBmp:align := CONTROL_ALIGN_ALLCLIENT
oBmp:lAutoSize := .T.

// Cria o botão para carregar imagem do disco 

@ 10,05 BUTTON oButton1 PROMPT "Ler do &Disco" ;
  ACTION (LoadFromDSK(oBmp)) ;
  SIZE 080, 15 of oPanelMenu  PIXEL

@ 30,05 BUTTON oButton1 PROMPT "Ler do &RPO" ;
  ACTION (LoadFromRPO(oBmp)) ;
  SIZE 080, 15 of oPanelMenu  PIXEL

@ 50,05 BUTTON oButton1 PROMPT "Ler do DB&IMAGE" ;
  ACTION (LoadFromDBI(oBmp,oDBImg)) ;
  SIZE 080, 15 of oPanelMenu  PIXEL

@ 70,05 BUTTON oButton2 PROMPT "&Salvar em Disco" ;
  ACTION (SaveToDisk(oBmp,oDBImg)) ;
  SIZE 080, 15 of oPanelMenu  PIXEL

@ 90,05 BUTTON oButton1 PROMPT "I&nserir no DBIMAGE" ;
  ACTION (InsertDBImg(oBmp,oDBImg)) ;
  SIZE 080, 15 of oPanelMenu  PIXEL

@ 110,05 BUTTON oButton1 PROMPT "&Alterar o DBIMAGE" ;
  ACTION (UpdDBImg(oBmp,oDBImg)) ;
  SIZE 080, 15 of oPanelMenu  PIXEL

@ 130,05 BUTTON oButton1 PROMPT "Apaga&r do DBIMAGE" ;
  ACTION (DelDBImg(oBmp,oDBImg)) ;
  SIZE 080, 15 of oPanelMenu  PIXEL

@ 150,05 BUTTON oButton1 PROMPT "S&tatus do DBIMAGE" ;
  ACTION (DBImgStat(oBmp,oDBImg)) ;
  SIZE 080, 15 of oPanelMenu  PIXEL

ACTIVATE DIALOG oDlg CENTER

// Fecha o DBImage 
oDBImg:Close()

Return


/* --------------------------------------------------------
Le uma imagem ( bmp / jpg / png ) do disco e coloca na tela
A Leitura é feita direto pelo objeto de interface tBitmap())
-------------------------------------------------------- */
STATIC Function LoadFromDSK(oBmp)
Local cImgFile := ''
Local cImgType := ''

cImgFile := cGetFile(NIL,NIL,0,"\",.F., NIL ,.T.)

If !empty(cImgFile)            

	If !ImgFileType( cImgFile , @cImgType )
		msgStop("Arquivo ["+cImgFile+"] não suportado.")
	Endif
	            
	If oBmp:Load( '' , cImgFile )
		// Imagem carregada do disco
		_aImageName := { cImgFile ,'',''}
		_aImageType := { cImgType ,'',''}
	Else
		MsgStop("Falha ao abrir ["+cImgFile+"] Type ["+cImgType+"]")
	Endif
Endif

Return

/* --------------------------------------------------------
Le uma imagem gravada como resorce do RPO 
Deve ser informado o nome do resource a ser carregado 
O proprio componente de interface faz a carga
-------------------------------------------------------- */
STATIC Function LoadFromRPO(oBmp)
Local cImgRes

cImgRes := AskUser("Imagem do RPO","Resource")

If !empty( cImgRes )

	If oBmp:Load( cImgRes , '' )

		// Imagem carregada direto do RPO
		_aImageName := { '' , cImgRes , ''}
		_aImageType := { '','',''}

	Else
		MsgStop("Falha ao abrir resource ["+cImgRes+"]")
	Endif
Endif

Return

/* --------------------------------------------------------
Carrega uma imagem do banco de dados 
deve ser informado o ID da imagem 
-------------------------------------------------------- */
STATIC Function LoadFromDBI(oBmp,oDBImg)
Local cImgId               
Local cImgBuffer := ''
Local cImgType := ''
Local cTmpFile := '\tmpimage.'

cImgId := AskUser("Imagem do DBIMAGE","Id")

If !empty(cImgId)
	    
	// Primeiro carrega do DBIMAGE
	If !oDBImg:ReadStr(cImgId,@cImgType,@cImgBuffer)
			MsgStop(oDBImg:cError,"LoadFromDBI")
			Return
	Endif
	
	// Imagem carregada do Banco na memoria
	// Agora salva em um arquivo temporario 

	// A extensao do temporario deve ser o tipo da imagem 
	cTmpFile += '.'+lower(cImgType)
	
	if file(cTmpFile)
	  Ferase(cTmpFile) 
	Endif

	If !oDBImg:SaveTo( cTmpFile , cImgBuffer )
		MsgStop(oDBImg:cError,"LoadFromDBI")
		Return
	Endif

	// E entao, faz o componente de interface ler do  disco 
	If !oBmp:Load( '' , cTmpFile )
		MsgStop("Falha ao abrir imagem temporaria ["+cTmpFile+"]","LoadFromDBI")
		Return .F.
	Endif

	_aImageName := { '','', cImgId }
	_aImageType := { '','', cImgType }

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
STATIC Function SaveToDisk(oBmp,oDBImg)
Local cImgSave
Local cImgType := ''
Local cImgBuffer := ''
                             
If empty( _aImageName[1] + _aImageName[2] + _aImageName[3] )
		MsgInfo("Nao há imagem carregada para salvar em disco.")
		Return
Endif

cImgSave := cGetFile(NIL,NIL,1,"\",.T., NIL ,.T.)

If empty(cImgSave)
	Return
Endif

If !file(cImgSave) .or. MsgYesNo("Arquivo já existe. Sobrescrever ? ")
	
	If !empty(_aImageName[1])
		
		// Arquivo do disco para o disco
		// copia direto
		__copyfile(_aImageName[1],cImgSave)
		
		MsgInfo("Arquivo ["+cImgSave+"] salvo.")

		Return
				
	ElseIf !empty(_aImageName[2])
		
		// resource do RPO para o disco
		cImgBuffer := GETAPORES(_aImageName[2]+'.BMP')
		If !empty(cImgBuffer)
			cImgType := 'BMP'
		Else
			cImgBuffer := GETAPORES(_aImageName[2]+'.PNG')
			If !empty(cImgBuffer)
				cImgType := 'PNG'
			Else
				cImgBuffer := GETAPORES(_aImageName[2]+'.JPG')
				If !empty(cImgBuffer)
					cImgType := 'JPG'
				Endif
			Endif
		Endif

		If empty(cImgType)				
			MsgStop("Resource ["+_aImageName[2]+"] não encontrado.","SaveToDisk")
			Return
		Endif
		
	ElseIf !empty(_aImageName[3])

		// Carrega do DBIMAGE na memoria
		If !oDBImg:ReadStr(_aImageName[3],@cImgType,@cImgBuffer)
			MsgStop(oDBImg:cError,"SaveToDisk")
			Return
		Endif
		
Endif
	  
	// Apos a imagem estar na memoria, salva em disco
	If !oDBImg:SaveTo(cImgSave,cImgBuffer)
		MsgStop(oBmp:cError,"SaveToDisk")
		Return
	Endif
	
	MsgInfo("Arquivo ["+cImgSave+"] salvo.")
	
Endif

Return

/* --------------------------------------------------------
Insere a imagem em foco no Database
-------------------------------------------------------- */
STATIC Function InsertDBImg(oBmp,oDBImg)
Local cImgId
Local cDbType
Local cImgBuffer := ''

If empty( _aImageName[1] + _aImageName[2] + _aImageName[3] )
		MsgInfo("Nao há imagem carregada para inserir.")
		Return
Endif

cImgId := AskUser("ID da Imagem","Inserir no ImageID")

If !empty(cImgId)
	
	If !empty(_aImageName[1])
		
		// lE Arquivo do disco para a memoria
		If !oDBImg:LoadFrom( _aImageName[1] , @cImgBuffer )
			MsgStop(oDBImg:cError,"InsertDBImg")
			Return
		Endif

	ElseIf !empty(_aImageName[2])
		
		// resource do RPO para a memoria
		cImgBuffer := GETAPORES(_aImageName[2])

		// Caso o resource seja informado sem extensao
		// o programa tenta novamente colocando algumas extensoes padrao
		If empty(cImgBuffer)
			cImgBuffer := GETAPORES(_aImageName[2]+'.BMP')
		Endif
		If empty(cImgBuffer)
			cImgBuffer := GETAPORES(_aImageName[2]+'.PNG')
		Endif
		If empty(cImgBuffer)
			cImgBuffer := GETAPORES(_aImageName[2]+'.BMP')
		Endif
				
	ElseIf !empty(_aImageName[3])

		// Carrega DBImage para a memoria

		If !oDBImg:ReadStr(_aImageName[3],@cImgType,@cImgBuffer)
			MsgStop(oDBImg:cError,"SaveToDisk")
			Return
		Endif
		
	Endif

	If !oDBImg:Insert( cImgId , cImgType , cImgBuffer )
		MsgStop(oDBImg:cError,"InsertDBImg")
		Return
	Endif

	MsgInfo("ImageId ["+cImgId+"] inserida no DBImage.")

Endif

Return
        
/* --------------------------------------------------------
Atualiza / altera a imagem em foco no Database
Deve ser informado o ID da imagem do banco a ser alterado 
-------------------------------------------------------- */
STATIC Function UpdDBImg(oBmp,oDBImg)
Local cImgId
Local cImgBuffer := ''
Local cImgType 

If empty( _aImageName[1] + _aImageName[2] + _aImageName[3] )
		MsgInfo("Nao há imagem carregada para atualizar.")
		Return
Endif

cImgId := AskUser("ID da Imagem","Atualizar o ImageID")

If !empty(cImgId)
	
	If !empty(_aImageName[1])
		
		// Le Arquivo do disco para a memoria
		If !oDBImg:LoadFrom( _aImageName[1] , @cImgBuffer )
			MsgStop(oDBImg:cError,"UpdDBImg")
			Return
		Endif

	ElseIf !empty(_aImageName[2])
		
		// resource do RPO para a memoria
		cImgBuffer := GETAPORES(_aImageName[2])

		If empty(cImgBuffer)
			cImgBuffer := GETAPORES(_aImageName[2]+'.BMP')
		Endif
		If empty(cImgBuffer)
			cImgBuffer := GETAPORES(_aImageName[2]+'.PNG')
		Endif
		If empty(cImgBuffer)
			cImgBuffer := GETAPORES(_aImageName[2]+'.BMP')
		Endif
				
	ElseIf !empty(_aImageName[3])

		// Carrega DBImage para a memoria
		If !oDBImg:ReadStr(_aImageName[3],@cImgType,@cImgBuffer)
			MsgStop(oDBImg:cError,"UpdDBImg")
			Return
		Endif
		
	Endif

	If !oDBImg:Update( cImgId , cImgBuffer )
		MsgStop(oDBImg:cError,"UpdateDBImg")
		Return
	Endif

	MsgInfo("ImageId ["+cImgId+"] atualizada no DBImage.")

Endif

Return

/* --------------------------------------------------------
Deleta uma imagem do banco de imagens
Deve ser informado o ID da imagem do Banco a ser apagada
-------------------------------------------------------- */
STATIC Function DelDBImg(oBmp,oDBImg)
Local cImgId
Local cImgBuffer := ''

cImgId := AskUser("ID da Imagem","Apagar do ImageID")

If !empty(cImgId)
	
	If !oDBImg:Delete( cImgId , cImgBuffer )
		MsgStop(oDBImg:cError,"DelDBImg")
		Return
	Endif

	MsgInfo("ImageId ["+cImgId+"] apagada no DBImage.")

Endif

Return
                         
/* --------------------------------------------------------
Mostra mensagem de estatisticas do banco de imagens
-------------------------------------------------------- */
STATIC Function DBImgStat(oBmp,oDBImg)
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
-------------------------------------------------------- */
STATIC Function DBImgList(oBmp,oDBImg)
MsgInfo("TODO")
Return



/*
Recebe cImgType por referencia (@) 
Retorna .T. e preenche o cImgType caso o arquivo 
tenha uma extensao suportada
*/

STATIC Function ImgFileType( cImgFile , cImgType )
Local cExt := ''
cImgType := ''
SplitPath(cImgFile,,,,@cExt)
If Upper('/'+cExt+'/')$'/.BMP/.PNG/.JPG/'
	cImgType := substr(cExt,2)
Endif
Return !Empty(cImgType)



