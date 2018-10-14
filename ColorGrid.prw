#include 'protheus.ch'

/* --------------------------------------------------------------------------
Funcao      U_ColorGrid()
Autor	    Julio Wittwer
Data	    12/10/2018
Descrição   Cria uma Dialog no AdvPL, com painéis internos, para mostrar 
            as 256 combinações de cores usando as 16 cores do DOS 
-------------------------------------------------------------------------- */

User Function ColorGrid()
Local oFont , oDlg
Local cTitle := "Grid de Cores"
Local aColors := {}
Local aNames := {}
Local nL , nC 

// Cria um array com as cores "padrao", disponiveis 
// desde a época que os monitors de vídeo tinham 
// apenas 16 cores. 

aadd(aColors,CLR_BLACK    ) ; aadd(aNames,"Black")
aadd(aColors,CLR_BLUE     ) ; aadd(aNames,"Blue")
aadd(aColors,CLR_GREEN    ) ; aadd(aNames,"Green")
aadd(aColors,CLR_CYAN     ) ; aadd(aNames,"Cyan")
aadd(aColors,CLR_RED      ) ; aadd(aNames,"Red")
aadd(aColors,CLR_MAGENTA  ) ; aadd(aNames,"Magenta")
aadd(aColors,CLR_BROWN    ) ; aadd(aNames,"Brown")
aadd(aColors,CLR_HGRAY    ) ; aadd(aNames,"Hgray")

aadd(aColors,CLR_GRAY     ) ; aadd(aNames,"Gray")
aadd(aColors,CLR_HBLUE    ) ; aadd(aNames,"HBlue")
aadd(aColors,CLR_HGREEN   ) ; aadd(aNames,"HGreen")
aadd(aColors,CLR_HCYAN    ) ; aadd(aNames,"HCyan")
aadd(aColors,CLR_HRED     ) ; aadd(aNames,"HRed")
aadd(aColors,CLR_HMAGENTA ) ; aadd(aNames,"HMagenta")
aadd(aColors,CLR_YELLOW   ) ; aadd(aNames,"Yellow")
aadd(aColors,CLR_WHITE    ) ; aadd(aNames,"White")

// Usa uma fonte Fixed Size
oFont := TFont():New('Courier new',,-14,.T.)

// Cria a janela principal da Agenda como uma DIALOG
DEFINE DIALOG oDlg TITLE (cTitle) ;
	FROM 0,0 TO 460,1020 ;
	FONT oFont ;
	COLOR CLR_BLACK, CLR_LIGHTGRAY PIXEL

// Cria um painel de topo, para colocar o nome das cores verticais
@ 0,0 MSPANEL oPanelTop OF oDlg SIZE 1100,20 COLOR CLR_BLACK,CLR_WHITE
oPanelTop:ALIGN := CONTROL_ALIGN_TOP

// Cria o painel lateral para colocar o nome das cores horizontais
@ 0,0 MSPANEL oPanel OF oDlg SIZE 30,600 COLOR CLR_BLACK,CLR_WHITE
oPanel:ALIGN := CONTROL_ALIGN_LEFT

For nL := 1 to len(aColors)

	// Mostra as cores no painel lateral 
	@ (nL * 12) , 2 SAY oSay PROMPT "" SIZE 30,10 COLOR CLR_BLACK OF oPanel PIXEL
	oSay:SetText( aNames[nL] )
	
Next

For nC := 1 to len(aColors)
	
	// Mostra a cor no painel superior
	@ 2 + iif(nC%2 == 0 , +2, -2 ),nC*30 SAY oSay PROMPT "" SIZE 30,10 COLOR CLR_BLACK OF oPanelTop PIXEL
	oSay:SetText( aNames[nC] )

	// Cria um painel vertical para cada cor, usando uma cor de fundo 
	@ 0,0 MSPANEL oPanel OF oDlg SIZE 30,600 COLOR 0,aColors[nC]
	oPanel:ALIGN := CONTROL_ALIGN_LEFT
	
	For nL := 1 to len(aColors)

		// Coloca as mensagens, uma de cada cor, no painel criado
		@ nL*12,2 SAY oSay PROMPT "" SIZE 30,10 COLOR aColors[nL] OF oPanel PIXEL
		oSay:SetText( " XXX " )
		
	Next
	
Next

ACTIVATE DIALOG oDlg CENTER

Return



