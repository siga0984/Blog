#include "protheus.ch"

#define CALCSIZESAY( X )  (( X * 4 ) + 4)

// --------------------------------------------------------------
// Programa principal AdvPLSuite
//
// Autor        Júlio Wittwer
// Data         28/10/2018
//
// Serve de Menu para os demais programas de exemplo do Blog 
// Primeiro programa acrescentado : U_AGENDA
// --------------------------------------------------------------

User Function AdvPLSuite()
Local oMainWnd
Local cTitle := "AdvPL Suite 1.00"
Local oFont

// Define Formato de data DD/MM/AAAA
SET DATE BRITISH
SET CENTURY ON

// Usa uma fonte Fixed Size
oFont := TFont():New('Courier new',,-14,.T.)

// Seta que esta é a fonte DEFAULT para Interface
SETDEFFONT(oFont)

DEFINE WINDOW oMainWnd FROM 0,0 to 768,1024 PIXEL ; 
	TITLE (cTitle) COLOR CLR_WHITE, CLR_BLUE  

// Monta o menu superior da aplicaçáo 
BuildMenu(oMainWnd)

// Executa rotina de entrada 
oMainWnd:bStart := {|| WndStart(oMainWnd) }
              
// Ativa a janela principal maximizada 
ACTIVATE WINDOW oMainWnd MAXIMIZED 

Return


// --------------------------------------------------------------
// Montagem do Menu do AdvPLSuite 
// --------------------------------------------------------------
STATIC Function BuildMenu(oMainWnd)
Local oMenuBar
Local oTMenu1, oTMenu2

conout('BuildMenu - IN')

// Cria a barra superior de Menus da Aplicação na Janela Principal
oMenuBar := tMenuBar():New(oMainWnd)
oMenuBar:SetColor(CLR_BLACK,CLR_WHITE)

// Cria o menu de Programas e acrescenta os itens
oTMenu1 := TMenu():New(0,0,0,0,.T.,,oMainWnd,CLR_BLACK,CLR_WHITE)
oMenuBar:AddItem('Programas'  , oTMenu1, .T.)

oTMenu1:Add(TMenuItem():New(oMainWnd,'&Agenda',,,,{||U_AGENDA()},,,,,,,,,.T.))
oTMenu1:Add(TMenuItem():New(oMainWnd,'Sai&r',,,,{||oMainWnd:End()},,,,,,,,,.T.))

// Cria o Menu de Ajuda e acrescenta Itens
oTMenu2 := TMenu():New(0,0,0,0,.T.,,oMainWnd,CLR_WHITE,CLR_BLACK)
oMenuBar:AddItem('Ajuda', oTMenu2, .T.)

oTMenu2:Add(TMenuItem():New(oMainWnd,'&TDN - AdvPL',,,,{||OpenURL("http://tdn.totvs.com/display/tec/AdvPL")},,,,,,,,,.T.))
oTMenu2:Add(TMenuItem():New(oMainWnd,'&Sobre',,,,{||HlpAbout()},,,,,,,,,.T.))

Return

// --------------------------------------------------------------
// Tela de inicialização -- Mensagem de Entrada
// --------------------------------------------------------------
STATIC Function WndStart(oMainWnd)
Local oDlg
Local cTitle
Local cMsg

cTitle := "Bem vindo ao AdvPL Suite 1.00"

DEFINE DIALOG oDlg TITLE (cTitle) ;
	FROM 0,0 TO 100,400 ;
	COLOR CLR_WHITE, CLR_RED PIXEL

@ 05,05 SAY "APPServer Build .... "+GetBuild() SIZE CALCSIZESAY(50),12 OF oDlg PIXEL 
@ 18,05 SAY "Smartclient Build .. "+GetBuild(.T.) SIZE CALCSIZESAY(50),12 OF oDlg PIXEL 

cMsg := "SERVER "

If IsSrvUnix()
  cMsg += 'LINUX '
Else
  cMsg += 'WINDOWS '
Endif

if ISSRV64()
  cMsg += '64 BITS'
Else
  cMsg += '32 BITS'
Endif

@ 31,05 SAY cMsg SIZE CALCSIZESAY(50),12 OF oDlg PIXEL 

ACTIVATE DIALOG oDlg CENTER

Return

// --------------------------------------------------------------
// Mensagem informativa sobre o autor
// --------------------------------------------------------------

STATIC Function HlpAbout()
MsgInfo("<html><center>AdvPL Suite V1.00<hr>Júlio Wittwer<br><b>Tudo em AdvPL</b>")
return

// --------------------------------------------------------------
// Encapsulamento de abertura de URL
// Abre a URL informada como parametro no navegador padrão
// da máquina onde está sendo executado o SmartClient
// --------------------------------------------------------------
STATIC Function OpenURL(cUrl)
shellExecute("Open", cUrl, "", "", 1 )
return

                         
