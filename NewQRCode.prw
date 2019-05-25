#include 'protheus.ch'
#include 'zlib.ch'

#define MODULE_PIXEL_SIZE     12   //  Tamanho em PIXELs do Modulo

/* ------------------------------------------------
Primeira interface para a criação de um QR Code 
------------------------------------------------ */

USER Function NewQRCode()
Local oDlg
Local cQRData := space(240)
Local cErrC , aErrorC := {}
Local nMode := 2  // Alfanumerico
Local aMasks := {}                
Local cMask 
Local nI
Local oQR

// Cria uma caixa de diálogo com área util de 800x600  PIXELs
DEFINE DIALOG oDlg TITLE "QR Code em AdvPL" FROM 0,0 TO 600,800 PIXEL

// Monta um GET para entrada de dados em variável string
// @ 18,15 SAY oSay1 PROMPT "Get String" SIZE 30,12 OF oDlg  PIXEL
@ 18,15 SAY "QR Data" PIXEL
@ 15,50 GET oGet1 VAR cQRData MULTILINE SIZE 260,36 OF oDlg  PIXEL

// Lista de níveis de correções de erro disponiveis 
aadd(aErrorC,"L.Low")
aadd(aErrorC,"M.Medium")
aadd(aErrorC,"Q.Quartile")
aadd(aErrorC,"H.High")

// Monta um objeto Combo, para escolha de string entre elementos de um array
@ 58,15 SAY "Error C" PIXEL
@ 55,50 COMBOBOX oComboBox1 VAR cErrC ITEMS aErrorC SIZE 80,12 OF oDlg  PIXEL

// Lista de Tipos de Mascaramento de dados 
// TODO - Escolha automatica de mascaramento 
// aadd(aMasks,"A.Auto")
For nI := 0 to 7
	aadd(aMasks,cValToChar(nI)+".Mask "+cValToChar(nI))
Next

// Monta um objeto Combo, para escolher o mascaramento de dados
// ou deixar no automatico ( TODO ) 
@ 73,15 SAY "Mask" PIXEL
@ 70,50 COMBOBOX oCombo2 VAR cMask ITEMS aMasks SIZE 80,12 OF oDlg  PIXEL

// Area reservada a Imagem 
@ 55,135 BITMAP oBmpQRCode OF oDlg PIXEL 
  
// Reserva a imagem em branco com 
// o Tamanho da versao 1 com margem de 4 modulos ) 
oBmpQRCode:nWidth := 29*MODULE_PIXEL_SIZE  
oBmpQRCode:nHeight := 29*MODULE_PIXEL_SIZE
oBmpQRCode:lStretch := .T.

// Botao para a geracão do QR Code
@ 85,50 BUTTON oButton1 PROMPT "&Gerar QR Code" ;
  ACTION (NewQRCode(@oQR,cQRData,cErrC,nMode,cMask,oBMPQRCode)) ;
  SIZE 080, 013 of oDlg  PIXEL

@ 100,50 BUTTON oButton2 PROMPT "&Exportar Imagem" ;
  ACTION (ExportImg(oQR)) WHEN oQR != NIL ;
  SIZE 080, 013 of oDlg  PIXEL

ACTIVATE DIALOG oDlg CENTER 

Return

// ------------------------------------------------
// Geração do QR Code na interface

STATIC Function NewQRCode(oQR,cQRData,cErrC,nMode,cMask,oBMPQRCode)

IF oQR != NIL
	FreeObj(oQR)
Endif

// Cria QRCode vazio 
oQR := ZQRCODE():New()

// Trata os dados informados 
cQRData := alltrim(cQRData)
cErrC := left(cErrC,1)
cMask := left(cMask,1)
                             
If empty(cQRData)
	MsgStop("Dados para o QR Code não informados.")
Endif

// Inicializa o QRCode com Error Correction "L", sem versao definida
// Informa o Error Correction
oQR:SetEC(cErrC)

// Seta o dado a ser colocado no QRCode
// e seta o modo de codificacao ( 1 = Numerico, 2 = Alfanumerico, 3 = Bytes , 4 = Kanji )
oQR:SetData(cQRData,nMode)

// Monta a matriz de dados 
// Se a versao nao foi especificada, determina de acordo 
// com o tamanho dos dados a serem plotados
// oQR:oLogger:SETECHO(.T.)

oQR:BuildData()

// oQR:oLogger:SETECHO(.F.)

// Monta a matriz final do QRCode 
oQR:DeployBits()

/*
conout("After Deploy Bits ")
oQR:PrintBits()
*/

// Seleciona a mascara de dados 
// Caso seja automatico, nao informa
// para a engine de emissao decidir
// O método SelectMask já aplica a máscara
If cMask <> 'A'
	oQR:SelectMask(val(cMask))
Else
	oQR:SelectMask()
Endif

/*
conout("After Mask Data ")
oQR:PrintBits()
*/

// Exporta para um BITMAP monocromatico
// Margem minima = 4 modulos ( de cada lado ) 
oBMP := ZBITMAP():NEW(oQR:nSize+8,oQR:nSize+8,1)

// Plota os módulos setados com 1 
// com a cor preta no Bitmap

For nL := 1 to oQR:nSize
	For nC := 1 to oQR:nSize
		If oQR:aGrid[nL][nC] == 1 .or. oQR:aGrid[nL][nC] == 3
			oBmp:SetPixel(nL+3,nC+3,0)
		Endif
	NExt
Next	

// Gera a imagem em disco 
// CRia um arquivo temporario com nome unico 
// para driblar o cache de imagem da interface
cFile := '\qrtmp_'+cvaltochar(seconds())+'.bmp'
oBmp:SaveToBmp(cFile)

// Redimensiona a imagem na tela
// com o tamanho da versao escolhida +margens
oBmpQRCode:nWidth := (oQR:nSize+8)*MODULE_PIXEL_SIZE 
oBmpQRCode:nHeight := (oQR:nSize+8)*MODULE_PIXEL_SIZE
oBmpQRCode:lStretch := .T.

// Carrega a imagem gerada na interface
oBMPQRCode:Load(,cFile)

// apaga a imagem temporária do disco 
// e limpa a classe bitmap da memoria 
ferase(cFile)
FreeObj(oBmp)

Return


// ------------------------------------------
// Exporta o QR Code para imagem BITMAP 

STATIC Function ExportImg(oQR)
Local nImgSize
Local oBmp
Local nL
Local nC
Local nL1,nL2,nC1,nC2
Local cFile

nImgSize := ( oQR:nSize+8 ) * MODULE_PIXEL_SIZE

oBMP := ZBITMAP():NEW( nImgSize,nImgSize, 1 )
oBMP:nPenSize := 1

// Plota os módulos setados com 1 
// com quadrados vazados no Bitmap
// e pinta o interior dos quadrados

For nL := 1 to oQR:nSize
	For nC := 1 to oQR:nSize
		If oQR:aGrid[nL][nC] == 1 .or. oQR:aGrid[nL][nC] == 3
		    nL1 := (nL+2)*MODULE_PIXEL_SIZE
		    nC1 := (nC+2)*MODULE_PIXEL_SIZE
		    nL2 := nL1+MODULE_PIXEL_SIZE
		    nC2 := nC1+MODULE_PIXEL_SIZE
			oBmp:Rectangle(nL1,nC1,nL2,nC2,0)
			oBmp:Paint(nL1+1,nC1+1)
		Endif
	Next
Next	

// Salva o arquivo em disc 
cFile := '\QRCode.bmp'
oBmp:SaveToBmp(cFile)
FreeObj(oBmp)

MsgInfo("Imagem do QR Code exportada para o arquivo ["+cFile+"]")

Return

