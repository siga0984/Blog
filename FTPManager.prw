#include 'protheus.ch'
#include "vkey.ch"

/* ---------------------------------------------------------------
Funcao 	U_FTPManager
Autor	Julio Wittwer 
Data	10/11/2018
Versao 	1.0 
--------------------------------------------------------------- */

#define CALCSIZEGET( X )  (( X * 4 ) + 4)

STATIC _KeyRunning := .F.

USER Function FTPManager()
Local oDlg, oFont
Local cTitle := "FTP Client Manager"
Local oSplitter
Local oPLeft , oPRight
Local nOpLeft := 1
Local oLbxLeft,oLbxRight
Local aLList := {} , aLFiles := {}
Local cLPath := "\"
Local nOpRight := 1
Local aRList := {}, aRFiles := {}
Local cRPath := "FTP Client (Não Conectado)"
Local oMenuBar
Local oTMenu1, oTMenu2,  oTMenu3, oTMenu4
Local oFtp
Local lConnected := .F.
Local aGetsL := {} , aGetsR := {}
Local cFname := space(50)
Local cFAttr := space(50)
Local cFDate := space(18)
Local nFSize := 0
Local cRFname := space(50)
Local cRFAttr := space(50)
Local cRFDate := space(18)
Local nRFSize := 0
Local aFTPInfo := {}
Local oLSay,oRSay

aadd(aFTPInfo,space(50))   // 1 FTP Addr
aadd(aFTPInfo,21)          // 2 FTP Port
aadd(aFTPInfo,5)           // 3 FTP TimeOut (sec)
aadd(aFTPInfo,.T.)         // 4 Firewall Mode ( passive )
aadd(aFTPInfo,.F.)         // 5 USe IP Conn
aadd(aFTPInfo,.T.)         // 6 Anonymous Login
aadd(aFTPInfo,"anonymous") // 7 User
aadd(aFTPInfo,"")          // 8 Password

// Define Formato de data DD/MM/AAAA
SET DATE BRITISH
SET CENTURY ON

// Usa uma fonte Fixed Size, tamanho 10
oFont := TFont():New('Courier new',,-10,.T.)
SETDEFFONT(oFont)

// Cria o objeto Client de FTP
oFtp := tFtpClient():New()

// Cria a janela principal da Agenda como uma DIALOG
DEFINE WINDOW oDlg FROM 0,0 TO 600,800 PIXEL ;
TITLE (cTitle) NOSYSMENU

// Permiote ESC fechar a janela
oDlg:lEscClose := .T.

// Primeiro cria a barra superior de Menus da Aplicação na Janela Principal
oMenuBar := tMenuBar():New(oDlg)
oMenuBar:SetColor(CLR_BLACK,CLR_WHITE)

// Agora cria a tela inicial
oSplitter := tSplitter():New( 00,00,oDlg,400,280 )
oSplitter:ALIGN := CONTROL_ALIGN_ALLCLIENT

@ 0,0 MSPANEL oPLeft LOWERED SIZE 130, 36 OF oSplitter
@ 0,0 MSPANEL oPRight LOWERED SIZE 130, 36 OF oSplitter

oPLeft:ALIGN := CONTROL_ALIGN_ALLCLIENT
oPRight:ALIGN := CONTROL_ALIGN_ALLCLIENT

@ 0,0 MSPANEL oPLeftUp     SIZE 130, 10 OF oPLeft
@ 0,0 MSPANEL oPLeftMid    SIZE 130, 15 OF oPLeft
@ 0,0 MSPANEL oPLeftBottom SIZE 130, 65 OF oPLeft

oPLeftUp:ALIGN      := CONTROL_ALIGN_TOP
oPLeftBottom:ALIGN  := CONTROL_ALIGN_BOTTOM
oPLeftMid:ALIGN     := CONTROL_ALIGN_ALLCLIENT

@ 1,1 SAY oLSay PROMPT cLPath SIZE 120,15 COLOR CLR_BLACK,CLR_WHITE of oPLeftUp PIXEL

@ 0,0 MSPANEL oPRightUp     SIZE 130, 10 OF oPRight
@ 0,0 MSPANEL oPRightMid    SIZE 130, 15 OF oPRight
@ 0,0 MSPANEL oPRightBottom SIZE 130, 65 OF oPRight

oPRightUp:ALIGN      := CONTROL_ALIGN_TOP
oPRightBottom:ALIGN  := CONTROL_ALIGN_BOTTOM
oPRightMid:ALIGN     := CONTROL_ALIGN_ALLCLIENT

@ 1,1 SAY oRSay PROMPT cRPAth SIZE 120,15 COLOR CLR_BLACK,CLR_WHITE of oPRightUp PIXEL

// ListBox lado esquerdo
// Arquivos do Drive local do Remote
// Pode ser tambem arquivos do servidor

aLList := GetLFiles(cLPath,@aLFiles)

@0,0 LISTBOX oLbxLeft VAR nOpLeft;
	ITEMS aLList ;
	ON CHANGE ( doLChange(aGetsL,oLbxLeft,@aLList,@aLFiles) ) ;
	ON DBLCLICK ( EnterLeft(oLbxLeft,@aLList,@aLFiles,@cLPath) ) ;
	OF oPLeftMid

oLbxLeft:ALIGN := CONTROL_ALIGN_ALLCLIENT

aRList := {}

@ 15,15 LISTBOX oLbxRight VAR nOpRight;
	SIZE 300, 300 ;
	OF oPRightMid ;
	ITEMS aRList ;
	ON CHANGE ( doRChange(aGetsR,oLbxRight,@aRList,@aRFiles) ) ;
	ON DBLCLICK ( EnterRight(oFtp,aFtpInfo,oLbxRight,oRSay,@aRList,@aRFiles,@cRPath) ) ;
	PIXEL

oLbxRight:ALIGN := CONTROL_ALIGN_ALLCLIENT

oLSay:ALIGN := CONTROL_ALIGN_TOP
oRSay:ALIGN := CONTROL_ALIGN_TOP

// Insere os gets com os dados do arquifo atual
// do lado esquerdo. Os dados sao atualizados conforme
// é feita a navegação na lista

@ 05+3,02 SAY oSay PROMPT "Arquivo" SIZE 40,10 COLOR CLR_BLACK,CLR_WHITE OF oPLeftBottom PIXEL
@ 05,50 GET oGetFName VAR cFname SIZE CALCSIZEGET(40),10 OF oPLeftBottom PIXEL
oGetFName:SETENABLE(.F.)
aadd(aGetsL,oGetFName)

@ 20+3,02 SAY oSay PROMPT "Tamanho" SIZE 40,10 COLOR CLR_BLACK,CLR_WHITE OF oPLeftBottom PIXEL
@ 20,50 GET oGetFSize VAR nFSize PICTURE "999999999999999999" SIZE CALCSIZEGET(18),10 OF oPLeftBottom PIXEL
oGetFSize:SETENABLE(.F.)
aadd(aGetsL,oGetFSize)

@ 35+3,02 SAY oSay PROMPT "Data" SIZE 40,10 COLOR CLR_BLACK,CLR_WHITE OF oPLeftBottom PIXEL
@ 35,50 GET oGetFDate VAR cFDate SIZE CALCSIZEGET(18),10 OF oPLeftBottom PIXEL
oGetFDate:SETENABLE(.F.)
aadd(aGetsL,oGetFDate)

@ 50+3,02 SAY oSay PROMPT "Atributos" SIZE 40,10 COLOR CLR_BLACK,CLR_WHITE OF oPLeftBottom PIXEL
@ 50,50 GET oGetFAttr VAR cFAttr SIZE CALCSIZEGET(5),10 OF oPLeftBottom PIXEL
oGetFAttr:SETENABLE(.F.)
aadd(aGetsL,oGetFAttr)

// Insere dados e detalhes dos arquivos do FTP
// Os dados sao atualizados conforme
// é feita a navegação na lista

@ 05+3,02 SAY oSay PROMPT "Arquivo" SIZE 40,10 COLOR CLR_BLACK,CLR_WHITE OF oPRightBottom PIXEL
@ 05,50 GET oRFName VAR cRFname SIZE CALCSIZEGET(40),10 OF oPRightBottom PIXEL
oRFName:SETENABLE(.F.)
aadd(aGetsR,oRFName)

@ 20+3,02 SAY oSay PROMPT "Tamanho" SIZE 40,10 COLOR CLR_BLACK,CLR_WHITE OF oPRightBottom PIXEL
@ 20,50 GET oRFSize VAR nRFSize PICTURE "999999999999999999" SIZE CALCSIZEGET(18),10 OF oPRightBottom PIXEL
oRFSize:SETENABLE(.F.)
aadd(aGetsR,oRFSize)

@ 35+3,02 SAY oSay PROMPT "Data" SIZE 40,10 COLOR CLR_BLACK,CLR_WHITE OF oPRightBottom PIXEL
@ 35,50 GET oRFDate VAR cRFDate SIZE CALCSIZEGET(18),10 OF oPRightBottom PIXEL
oRFDate:SETENABLE(.F.)
aadd(aGetsR,oRFDate)

@ 50+3,02 SAY oSay PROMPT "Atributos" SIZE 40,10 COLOR CLR_BLACK,CLR_WHITE OF oPRightBottom PIXEL
@ 50,50 GET oRFAttr VAR cRFAttr SIZE CALCSIZEGET(5),10 OF oPRightBottom PIXEL
oRFAttr:SETENABLE(.F.)
aadd(aGetsR,oRFAttr)

// Insere as opções de Menu

oTMenu1 := TMenu():New(0,0,0,0,.T.,,oDlg,CLR_BLACK,CLR_WHITE)
oMenuBar:AddItem('&Local'  , oTMenu1, .T.)

oTMenu1:Add( TMenuItem():New(oDlg,'&Path',,,,{|| LocalPath(oLbxLeft,@aLList,@aLFiles,oLSay,@cLPath) },,,,,,,,,.T.))
oTMenu1:Add( TMenuItem():New(oDlg,'Sai&r',,,,{||oDlg:End()},,,,,,,,,.T.))

oTMenu2 := TMenu():New(0,0,0,0,.T.,,oDlg,CLR_BLACK,CLR_WHITE)
oMenuBar:AddItem('&FTP'  , oTMenu2, .T.)

oTMenu2:Add( TMenuItem():New(oDlg,'&Conectar'   ,,,,{|| FTPConn(oDlg,oPRight,oFtp,@aFTPInfo,oLbxRight,@aRList,@aRFiles,oRSay,@cRPath) },,,,,,,,,.T.))
oTMenu2:Add( TMenuItem():New(oDlg,'&Desconectar',,,,{|| FTPDesConn(oDlg,oPRight,oFtp,@aFTPInfo,oLbxRight,@aRList,@aRFiles,oRSay,@cRPath) },,,,,,,,,.T.))

oTMenu4 := TMenu():New(0,0,0,0,.T.,,oDlg,CLR_WHITE,CLR_BLACK)
oMenuBar:AddItem('&Ajuda', oTMenu4, .T.)

oTMenu4:Add(TMenuItem():New(oDlg,'&Sobre',,,,{|| About()},,,,,,,,,.T.))

// Ajusta o tamanho do Menu com o Tamanho da Janela
// PAra nao "suprimir" opções com ">>"
oMenuBar:NWIDTH := oDlg:nWidth

// Posiciona no primeiro arquivo
oLbxLeft:Gotop()

// Copiar Arquivo
SETKEY( VK_F5     , {|| IIF(_KeyRunning , NIL , ( _KeyRunning := .T. , CallKey(VK_F5    ,oLbxLeft,oLbxRight,aLFiles,aLList,aRFiles,aRList,cLPath,cRPath,oFtp) , _KeyRunning := .F. ))})


ACTIVATE WINDOW oDlg MAXIMIZED ;
	ON INIT DoInit(oDlg) ;
	VALID CanQuit()

// Fecha a conexao com o FTP caso esteja aberta
oFtp:Close()

Return


STATIC Function DoInit(oDlg)
Return

STATIC Function CanQuit()
Return MsgYesNo("Finaliza a aplicação ?")

/* ----------------------------------------------------------------------
Funçao usada para encapsular a busca dos arquivos locais
Retorna array para a lista de tela - visualização
E alimenta por referência uma lista paralela com todos
os detalhes dos arquivos
---------------------------------------------------------------------- */
STATIC Function GetLFiles(cPath,aLFiles)
Local aRet := {}, aTmp := {}
Local nI , nT , cFName
aSize(aLFiles,0)
aTmp := Directory(cPath+"*.*","AD",NIL,.F.,1)
nT := len(aTmp)
For nI := 1 to nT
	cFName := alltrim(aTmp[nI][1])
	IF cFName == '.'
		// Diretorio atual, pula
		LOOP
	ElseIf cFName == '..' .and. cPath == '\'
		// Diretorio anterior, só se eu não estiver no RootPAth "\"
		LOOP
	Else
		aadd(aRet , cFName)
		aadd(aLFiles , aclone(aTmp[nI]) )
	Endif
Next
return aRet

/* ----------------------------------------------------------------------
Função disparada na troca de posição da lista de arquivos
do lado esquerdo -- arquivos locais
Atualiza as informações do arquivo selecionado no painel inferior
---------------------------------------------------------------------- */

STATIC Function doLChange(aGetsL,oLbxLeft,aLList,aLFiles)
Local cFname , cFDate, nFSize , cFAttr
Local nOp := oLbxLeft:GetPos()
If nOp > 0 .and. nOp <= Len(aLList)
	cFname := aLFiles[nOp][1]
	nFSize := aLFiles[nOp][2]
	cFDate := dtoc(aLFiles[nOp][3])+' ' +aLFiles[nOp][4]
	cFattr := aLFiles[nOp][5]
	Eval(aGetsL[1]:bSetGet,cFname)
	Eval(aGetsL[2]:bSetGet,nFSize)
	Eval(aGetsL[3]:bSetGet,cFDate)
	Eval(aGetsL[4]:bSetGet,cFattr)
Else
	Eval(aGetsL[1]:bSetGet,"")
	Eval(aGetsL[2]:bSetGet,0)
	Eval(aGetsL[3]:bSetGet,"")
	Eval(aGetsL[4]:bSetGet,"")
Endif
aGetsL[1]:Refresh()
aGetsL[2]:Refresh()
aGetsL[3]:Refresh()
aGetsL[4]:Refresh()
return


/* ----------------------------------------------------------------------
Função disparada na troca de posição da lista de arquivos FTP
do lado direito.
Atualiza as informações do arquivo selecionado no painel inferior
---------------------------------------------------------------------- */
STATIC Function doRChange(aGetsR,oLbxRight,aRList,aRFiles)
Local cFname , cFDate, nFSize , cFAttr
Local nOp := oLbxRight:GetPos()
If nOp > 0 .and. nOp <= Len(arList)
	cFname := aRFiles[nOp][1]
	nFSize := aRFiles[nOp][2]
	cFDate := dtoc(aRFiles[nOp][3])+' ' +aRFiles[nOp][4]
	cFattr := aRFiles[nOp][5]
	Eval(aGetsR[1]:bSetGet,cFname)
	Eval(aGetsR[2]:bSetGet,nFSize)
	Eval(aGetsR[3]:bSetGet,cFDate)
	Eval(aGetsR[4]:bSetGet,cFattr)
Else
	Eval(aGetsR[1]:bSetGet,"")
	Eval(aGetsR[2]:bSetGet,0)
	Eval(aGetsR[3]:bSetGet,"")
	Eval(aGetsR[4]:bSetGet,"")
Endif
aGetsR[1]:Refresh()
aGetsR[2]:Refresh()
aGetsR[3]:Refresh()
aGetsR[4]:Refresh()
return

STATIC Function About()
MsgInfo("<html><center>FTP Client Manager<hr>Júlio Wittwer<br><b>Tudo em AdvPL</b>")
return

/* ----------------------------------------------------------------------
Permite trocar a pasta atual navegando diretamente
na estrutura de pastas do Servidor a partir do RootPath
---------------------------------------------------------------------- */

STATIC Function LocalPath(oLbxLeft,aLList,aLFiles,oLSay,cLPath)
Local cRet

cRet := cGetFile( 'Todos os Arquivos|*.*' , ;
'Path', 1, cLPath, .F., GETF_RETDIRECTORY ,.T., .T. )

If !empty(cRet)
	// Troca o path e atualiza a lista de arquivos na tela
	cLPath := cRet
	aLList := GetLFiles(cLPath,aLFiles)
	oLbxLeft:SetArray(aLList)
	oLbxLeft:GoTop()
	oLSay:SetText(cLPath)
Endif

Return

/* ----------------------------------------------------------------------
Funcao disparada em caso de [ENTER] ou Duplo Click em um arquivo
na lista de arquivos locais -- lado esquerdo. Permite a navegação
entre os diretorios.
---------------------------------------------------------------------- */

STATIC Function EnterLeft(oLbxLeft,aLList,aLFiles,cLPath)
Local cFile
Local aTmp, nI
Local nOp := oLbxLeft:GetPos()

If nOp > 0
	cFile := alltrim(aLList[nOp])
	If cFile == '..'
		// Tenta ir para o nivel anterior
		aTmp := StrTokarr(cLPath,'\')
		cLPath := ''
		For nI := 1 to len(aTmp)-1
			cLPath += ( aTmp[nI] + '\')
		Next
		if empty(cLPath)
			cLPath := '\'
		Endif
		aLList := GetLFiles(cLPath,aLFiles)
		oLbxLeft:SetArray(aLList)
		oLbxLeft:GoTop()
	Else
		// SE for um diretorio, entra nele
		aTmp := aLFiles[nOp]
		if 'D' $ aTmp[5]
			// Se for um diretorio , entra
			cLPath += ( cFile+'\' )
			aLList := GetLFiles(cLPath,aLFiles)
			oLbxLeft:SetArray(aLList)
			oLbxLeft:GoTop()
		Endif
	Endif
Endif
Return


/* ----------------------------------------------------------------------
Funcao disparada em caso de [ENTER] ou Duplo Click em um arquivo
na lista de arquivos de FTP - Lado direito -- Permite navegar
entre os diretorios.
---------------------------------------------------------------------- */
STATIC Function EnterRight(oFTP,aFtpInfo,oLbxRight,oRSay,aRList,aRFiles,cRPath)
Local cFile
Local aTmp, nI
Local nOp := oLbxRight:GetPos()
Local cCurrDir

If nOp > 0
	
	cFile := alltrim(aRList[nOp])
	
	If cFile == '..'
		
		// Volta ao nivel anterior
		nStat := oFTP:CDUP()
		
		If nStat != 0
			
			MsgStop("Falha ao mudar de Diretorio - Erro "+cValToChar(nStat),oFtp:CERRORSTRING)
			
		Else
			
			cCurrDir := ''
			nStat := oFtp:GETCURDIR(@cCurrDir)
			
			cRPath := "ftp://"+aFtpInfo[1]+cCurrDir
			oRSay:SetText(cRPath)
			oRSay:Refresh()
			
			// Pega os arquivos do diretorio atual
			MsgRun("Obtendo lista de arquivos",cRPath,{|| aRFiles := oFtp:Directory("*",.T.) })
			aSize(aRList,0)
			
			// Acrescenta um atalho para voltar para o nivel anterior
			// SE eu nao estiver no niver RAIZ ...
			IF !(cCurrDir == '/')
				aadd(aRFiles,{"..",0,ctod(""),"",""})
				aSort(aRFiles,,,{|x1,x2| lower(x1[1]) < lower(x2[1]) })
			Endif
			
			aEval(aRFiles,{|x| aadd( aRList , alltrim(x[1]) )})
			
			oLbxRight:SetArray(aRList)
			oLbxRight:GoTop()
			
			
		Endif
		
	Else
		
		// SE for um diretorio, entra nele
		aTmp := aRFiles[nOp]
		if 'D' $ aTmp[5]
			
			// Se for um diretorio , entra
			// Troca o diretorio atual
			nStat := oFTP:CHDIR(cFile)
			
			If nStat != 0
				
				MsgStop("Falha ao mudar de Diretorio - Erro "+cValToChar(nStat),oFtp:CERRORSTRING)
				
			Else
				
				cRPath += ( cFile+'/' )
				oRSay:SetText(cRPath)
				
				// Pega os arquivos do diretorio atual
				MsgRun("Obtendo lista de arquivos",cRPath,{|| aRFiles := oFtp:Directory("*",.T.) })
				aSize(aRList,0)
				
				// Acrescenta um atalho para voltar para o nivel anterior
				aadd(aRFiles,{"..",0,ctod(""),"",""})
				aSort(aRFiles,,,{|x1,x2| lower(x1[1]) < lower(x2[1]) })
				
				aEval(aRFiles,{|x| aadd( aRList , alltrim(x[1]) )})
				
				oLbxRight:SetArray(aRList)
				oLbxRight:GoTop()
				
			Endif
			
		Endif
		
	Endif
	
Endif
return


/* ----------------------------------------------------------------------
Diálogo de Conexão com FTP
Armazema parametros de conexao, e em caso de sucesso,
já alimenta a lista de arquivos do lado direito
---------------------------------------------------------------------- */

STATIC Function FTPConn(oDlg,oPRight,oFtp,aFTPInfo,oLbxRight,aRList,aRFiles,oRSay,cRPath)
Local cTitle := 'Conexão com FTP'
Local oSay1,oSay2
Local lGo := .F.
Local cFTPAddr := padr(aFTPInfo[1],40)
Local nFtpPort := aFTPInfo[2]
Local nTimeOut := aFTPInfo[3]
Local bPasv    := aFTPInfo[4]
Local bUseIP   := aFTPInfo[5]
Local bAnonymous := aFTPInfo[6]
Local cUser    := padr(aFTPInfo[7],40)
Local cPass    := padr(aFTPInfo[8],40)
Local nStat

DEFINE DIALOG oDlgConn TITLE (cTitle) ;
FROM 0,0 TO 220,450 PIXEL;
OF oDlg ;
COLOR CLR_WHITE, CLR_BROWN

@ 05+3,05 SAY oSay1 PROMPT "FTP"  RIGHT SIZE 40,12 OF oDlgConn PIXEL
@ 05,50 GET oGetFTP VAR cFtpAddr   SIZE CALCSIZEGET(40) ,12 OF oDlgConn PIXEL

@ 20+3,05 SAY oSay2 PROMPT "Porta"  RIGHT SIZE 40,12 OF oDlgConn PIXEL
@ 20,50 GET oGetPorta VAR nFtpPort PICTURE "99999" SIZE CALCSIZEGET(5) ,12 OF oDlgConn PIXEL

@ 20+3,100 SAY oSay3 PROMPT "TimeOut" RIGHT SIZE 40,12 OF oDlgConn PIXEL
@ 20,145 GET oGetTimeOut VAR nTimeOut PICTURE "999" SIZE CALCSIZEGET(3) ,12 OF oDlgConn PIXEL

@ 35,50 CHECKBOX oCkh1 VAR bPasv  PROMPT "Passive Mode"    SIZE 80,12 OF oDlgConn PIXEL

@ 45,50 CHECKBOX oCkh2 VAR bUseIP PROMPT "Use IP Conn"     SIZE 80,12 OF oDlgConn PIXEL

@ 55,50 CHECKBOX oCkh3 VAR bAnonymous PROMPT "Anonymous Login" SIZE 80,12 OF oDlgConn PIXEL

@ 65+3,05 SAY oSay1 PROMPT "Usuário"  RIGHT SIZE 40,12 OF oDlgConn PIXEL
@ 65,50 GET oGetUsr VAR cUser   SIZE CALCSIZEGET(40) ,12 WHEN !bAnonymous OF oDlgConn PIXEL

@ 80+3,05 SAY oSay2 PROMPT "Senha"    RIGHT SIZE 40,12 OF oDlgConn PIXEL
@ 80,50 GET oGetPsw VAR cPass SIZE CALCSIZEGET(40) ,12 WHEN !bAnonymous OF oDlgConn PIXEL
oGetPsw:LPASSWORD := .T.

@ 95, CALCSIZEGET(40) - 10  BUTTON oBtnOk PROMPT "Ok" SIZE 60,15 ;
ACTION (lGo := .T. , oDlgConn:End()) OF oDlgConn PIXEL

ACTIVATE DIALOG oDlgConn CENTER

If lGo
	
	// Fecha qqer conexao existente anteriormente
	oFTP:Close()
	
	// Ajusta os parametros
	cFTPAddr := alltrim(cFTPAddr)
	cUser := alltrim(cUser)
	cPass := alltrim(cPass)
	
	// Guarda os parametros utilizados
	aFTPInfo[1] := cFTPAddr
	aFTPInfo[2] := nFtpPort
	aFTPInfo[3] := nTimeOut
	aFTPInfo[4] := bPasv
	aFTPInfo[5] := bUseIP
	aFTPInfo[6] := bAnonymous
	aFTPInfo[7] := cUser
	aFTPInfo[8] := cPass
	
	// Seta parametros na classe
	oFtp:BFIREWALLMODE := bPasv
	oFtp:NCONNECTTIMEOUT := nTimeOut
	oFtp:BUSESIPCONNECTION := bUseIP
	
	// Conecta no FTP
	If !bAnonymous
		MsgRun("FTP Connect",cFtpAddr,{|| nStat := oFtp:FtpConnect(cFtpAddr,nFTPPort,cUser,cPass) })
	Else
		MsgRun("FTP Connect",cFtpAddr,{|| nStat := oFtp:FtpConnect(cFtpAddr,nFTPPort,"anonymous","anonymous") })
	Endif
	
	If nStat == 0
		
		cCurrDir := ''
		nStat := oFtp:GETCURDIR(@cCurrDir)
		
		If nStat <> 0
			
			cRPath := "ftp://"+cFtpAddr+"/"
			oRSay:SetText(cRPath)
			oRSay:Refresh()
			
			MsgStop("Falha ao recuperar executar GetCurDir() - Erro "+cValtoChar(nStat),oFtp:CERRORSTRING)
			
		Else
			
			// Atualiza pasta atual do FTP
			cRPath := "ftp://"+cFtpAddr+cCurrDir
			oRSay:SetText(cRPath)
			oRSay:Refresh()
			
		Endif
		
		// Limpa lado direito
		aSize(aRFiles,0)
		aSize(aRList,0)
		oLbxRight:SetArray(aRList)
		
		// Conectou com sucesso, recupera pasta atual e lista de arquivos
		MsgRun("Obtendo lista de arquivos",cRPath,{|| aRFiles := oFtp:Directory("*",.T.) })
		aSize(aRList,0)
		
		aEval(aRFiles,{|x| aadd( aRList , alltrim(x[1]) )})
		oLbxRight:SetArray(aRList)
		
	Else
		
		aSize(aRFiles,0)
		aSize(aRList,0)
		oLbxRight:SetArray(aRList)
		
		MsgStop("Falha de Conexão -- Erro "+cValToChar(nStat),oFtp:CERRORSTRING)
		cRPath := "FTP Client (Não Conectado)"
		oRSay:SetText(cRPath)
	Endif
	
	oLbxRight:GoTop()
	oPRight:Refresh()
	
Endif

Return

/* ----------------------------------------------------------------------
Desconexao com o FTP
Limpa as listas de arquivos e painel do lado direito
---------------------------------------------------------------------- */
STATIC Function FTPDesConn(oDlg,oPRight,oFtp,aFTPInfo,oLbxRight,aRList,aRFiles,oRSay,cRPath)

// Fecha a conexao
oFTP:Close()

// Limpa listas de arquivos
aSize(aRFiles,0)
aSize(aRList,0)
oLbxRight:SetArray(aRList)

// Atualiza Informação do FTP
cRPath := "FTP Client (Não Conectado)"
oRSay:SetText(cRPath)

oLbxRight:GoTop()
oDlg:Refresh()

Return


/* ----------------------------------------------------------------------
Teclas de Atalho de funcionalidades do FTP
F5 = Copiar Arquivo ( Download ou Upload ) 
---------------------------------------------------------------------- */

STATIC Function CallKey(nKey,oLbxLeft,oLbxRight,aLFiles,aLList,aRFiles,aRList,cLPath,cRPath,oFtp)
Local hHndFocus
Local nPos
Local cFile
Local cSource
Local cTarget
Local cCurrDir
Local lExist
Local lRun

// Pega Handle do componente de interface que estava com o foco
// quando a tecla de atalho foi pressionada
hHndFocus := GETFOCUS()

If hHndFocus == oLbxLeft:HWND
	
	// Caso o foco esteja na lista de arquivos locais
	// E exista um arquivo posicionado ... 
	
	nPos := oLbxLeft:GetPos()
	If nPos > 0
		
		cFile := alltrim(aLFiles[nPos][1])
		cAttr := aLFiles[nPos][5]
		
		If cFile == '.' .or. cFile == '..'
			MsgStop("Operação com pasta não implementada. Selecione um arquivo.","Atenção")
			return
		ElseIf 'D'$cAttr
			MsgStop("Operação com pasta não implementada. Selecione um arquivo.","Atenção")
			return
		Endif
		
		If nKey == VK_F5
			// Copia de arquivo Local para o FTP
			cSource := cLPath+cFile
			cTarget := cRPath+cFile
			If MsgYEsNo("Seseja copiar o arquivo local ["+cSource+"] para o FTP ["+cTarget+"] ?")
	`			MsgRun("FTP Upload",cFile,{|| nStat := oFTP:SENDFILE(cSource,cFile) })
				If nStat <> 0
					MsgStop("Falha no UPLOAD de Arquivo - Erro "+cValToChar(nStat),oFtp:CERRORSTRING)
				Else
					MsgInfo("Upload realizado com sucesso.")
					cCurrDir := ''
					oFtp:GETCURDIR(@cCurrDir)
					// Pega os arquivos do diretorio atual
					MsgRun("Obtendo lista de arquivos",cRPath,{|| aRFiles := oFtp:Directory("*",.T.) })
					aSize(aRList,0)
					// Acrescenta um atalho para voltar para o nivel anterior
					// SE eu nao estiver no niver RAIZ ...
					IF !(cCurrDir == '/')
						aadd(aRFiles,{"..",0,ctod(""),"",""})
						aSort(aRFiles,,,{|x1,x2| lower(x1[1]) < lower(x2[1]) })
					Endif
					aEval(aRFiles,{|x| aadd( aRList , alltrim(x[1]) )})
					oLbxRight:SetArray(aRList)
				Endif
			Endif
		Else
			MsgInfo("Operação com Arquivo Local ainda não implementada.")
		Endif
	Endif
	
ElseIf hHndFocus == oLbxRight:HWND
	
	// Copia arquivo do FTP para pasta Local
	// e exista algum arquivo posicionado
	nPos := oLbxRight:GetPos()
	IF nPos > 0
		
		cFile := alltrim(aRFiles[nPos][1])
		cAttr := aRFiles[nPos][5]
		
		If cFile == '.' .or. cFile == '..'
			MsgStop("Operação com pasta não implementada. Selecione um arquivo.","Atenção")
			return
		ElseIf 'D'$cAttr
			MsgStop("Operação com pasta não implementada. Selecione um arquivo.","Atenção")
			return
		Endif
		// Ajusta o nome vindo do FTP 
		AdjustFTP(@cFile)
		If nKey == VK_F5
			// Copia de arquivo do FTP para a pasta local 
			cSource := cRPath+cFile
			cTarget := cLPath+cFile
			lExist := File(cLPath+cFile)
			lRun := .F. 
			IF lExist
				If MsgYesNo("O Arquivo local já existe. Deseja continuar o Download ? ")
					lRun := .T.
					MsgRun("FTP Resume Download",cFile,{|| nStat := oFTP:RESUMERECEIVEFILE(cFile,cTarget) })
				ElseIf MsgYesNo("Apaga o arquivo local e reinicia o Download ?")
					lRun := .T.
					Ferase(cLPath+cFile)
					MsgRun("FTP Download",cFile,{|| nStat := oFTP:RECEIVEFILE(cFile,cTarget) })
				Endif			
			Else
				If MsgYEsNo("Deseja baixar o arquivo do FTP ["+cSource+"] para a pasta local ["+cTarget+"] ?")
					lRun := .T.
					MsgRun("FTP Download",cFile,{|| nStat := oFTP:RECEIVEFILE(cFile,cTarget) })
				Endif
			Endif
	`		If lRun			
				If nStat <> 0
					MsgStop("Falha no DOWNLOAD de Arquivo - Erro "+cValToChar(nStat),oFtp:CERRORSTRING)
				Else
					MsgInfo("Download realizado com sucesso.")
					// Atualiza lista de arquivos 
					aLList := GetLFiles(cLPath,aLFiles)
					oLbxLeft:SetArray(aLList)
				Endif
			Endif
		Else
			MsgInfo("Operação com Arquivo do FTP ainda não implementada.")
		Endif
	Endif
Endif

Return


/* ----------------------------------------------------------------------------------
Alguns FTPs podem criar link para o arquivo, e retornar o link nas informações
Esta informação deve ser ignorada, o que importa é o nome do arquivo 
Normalmente o link vem no formato arquivo -> link 
---------------------------------------------------------------------------------- */
STATIC Function AdjustFTP(cFile)
Local nPos
nPos := at("->",cFile)
IF nPos > 0 
	cFile := alltrim(substr(cFile,1,nPos-1))
Endif
return
