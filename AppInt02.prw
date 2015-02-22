#include 'protheus.ch'

/* ======================================================================
Fonte    APPINT02.PRW
Autor    Júlio Wittwer
Data    10/01/2015
Descrição  Fonte de exemplo de uso de multiplos componentes de interface
      dentro de uma caixa de diálogo.

Post relacionado

https://siga0984.wordpress.com/2015/01/24/interface-visual-do-advpl-parte-02/

====================================================================== */

USER Function APPINT02()

Local oDlg
Local oGroup1
Local aCores := {}
Local oSay1, oSay2 , oSay3 , oSay4
Local oGet1, oGet2, oGet3 , oGet4
Local oSayF1,oSayF2,oSayP,oSayT,oSayB,oSayM

Local cVarStr := space(40)
Local cVarMemo := ''
Local nVarNum := 0
Local dVarDate := date()
Local lCheckBox := .F.
Local cVarCombo
Local cVarList
Local nVarRadio := 1

Local oFolders
Local oPanel1
Local oScroll
Local oChkBox1
Local oComboBox1
Local oRadio1
Local oBmp
Local oMeter
Local nMeterPos := 1
Local oBtnBar,oBtnBar1
Local oButton1
Local oBmpBtn1

// Habilita interface com data mostrada com 4 digitos no ano
// e Habilita data em formato britânico ( Dia/Mes/Ano )
SET CENTURY ON
SET DATE BRITISH

// Alimenta array de itens usados no ComboBox e ListBox
aadd(aCores,"Azul")
aadd(aCores,"Branco")
aadd(aCores,"Vermelho")

// Cria uma caixa de diálogo com área util de 800x600  PIXELs
DEFINE DIALOG oDlg TITLE "Exemplo de Interface" FROM 0,0 TO 600,800 PIXEL

// Nao permite que a tecla ESC feche a caixa de dialogo
oDlg:lEscClose := .F.

// desenha uma borda com um título para demarcar os objetos 
// usados para a área de entrada de dados
@ 05,05 GROUP oGroup1 TO 130,330 LABEL "Entrada de Dados" OF oDlg  PIXEL

// Monta um GET para entrada de dados em variável string
@ 18,15 SAY oSay1 PROMPT "Get String" SIZE 30,12 OF oDlg  PIXEL
@ 15,50 GET oGet1 VAR cVarStr PICTURE "@!" SIZE 80,12 OF oDlg  PIXEL

// Monta um GET para entrada de dados numéricos
@ 33,15 SAY oSay2 PROMPT "Get Numeric" SIZE 30,12 OF oDlg  PIXEL
@ 30,50 GET oGet2 VAR nVarNum PICTURE "999999999" SIZE 80, 12  OF oDlg  PIXEL

// Monta um GET para entrada de uma data
@ 48,15 SAY oSay3 PROMPT "Get Date" SIZE 30,12 OF oDlg  PIXEL
@ 45,50 GET oGet3 VAR dVarDate SIZE 80,12 OF oDlg  PIXEL

// Monta um CheckBox para entrada de um valor lógico / booleano
@ 60,15 CHECKBOX oChkBox1 VAR lCheckBox PROMPT "CheckBox" SIZE 80,12 OF oDlg  PIXEL

// Monta um objeto Combo, para escolha de string entre elementos de um array
@ 75,15 COMBOBOX oComboBox1 VAR cVarCombo ITEMS aCores SIZE 80,12 OF oDlg  PIXEL

// Monta um objeto Radio, para escolha numérica a partir de um array
@ 90,15 RADIO oRadio1 VAR nVarRadio ITEMS "Um","Dois","Três" SIZE 40,36 OF oDlg  PIXEL

// Monta um listbox, para escolha de uma string entre elementos de um array
@ 15,200 LISTBOX oLst VAR cVarList ITEMS aCores SIZE 80,36 OF oDlg  PIXEL

// Monta um GET MULTILINE para a entrada livre de valores texto
// Com suporte a quebra de linha e tabulação
@ 60,200 SAY oSay4 PROMPT "Get Multiline" SIZE 30,12 OF oDlg  PIXEL
@ 70,200 GET oGet4 VAR cVarMemo MULTILINE SIZE 120, 48 of oDlg  PIXEL

// Exemplo de folders.
// Um folder é um container que pode receber outros componentes
// Cada objeto de dentro de um folder é da classe tFolderPage

@ 140,15 FOLDER oFolders PROMPTS "Folder1","Folder2" SIZE 130, 40 OF oDlg  PIXEL

// Dois objetos tSay(), colocados dentro de cada folder
@ 03,03 SAY oSayF1 PROMPT "Say Folder 1" SIZE 50,12 OF oFolders:aDialogs[1]  PIXEL
@ 03,03 SAY oSayF2 PROMPT "Say Folder 2" SIZE 50,12 OF oFolders:aDialogs[2]  PIXEL

// Um panel é um container que pode receber outros componentes
@ 190,15 MSPANEL oPanel1 PROMPT "Painel" RAISED SIZE 130, 36 OF oDlg

// Objeto tSay() clocado dentro do painel
@ 10,05 SAY oSayP PROMPT "Say Panel" SIZE 30,12 OF oPanel1  PIXEL

// Scrollbox , ou caixa de rolagem, tambem serve de container para outros objetos
// O tamanho é especificado em altura,comprimento, ao contrario dos demais componentes, 
// onde o tamanho é especificado em comprimento,altura
// O Scroll Box pode ser VERTICAL , HORIZONTAL ou ambos. A borda é opcional, e pode ser omitida
@ 140,200 SCROLLBOX oScroll VERTICAL SIZE 40, 130 OF oDlg BORDER

// Objetos tSay() colocados dentro do scroll
@ 005,03 SAY oSayT PROMPT "Topo do Scroll" SIZE 100,12 OF oScroll  PIXEL
@ 150,03 SAY oSayB PROMPT "Final do Scroll" SIZE 100,12 OF oScroll  PIXEL

// Exemplo de meter ( gauge ) de processos
// cria uma barra no comprimento e altura especificados
@ 190,200 METER oMeter VAR nMeterPos TOTAL 1000 SIZE 130,8 OF oDlg PIXEL
@ 200,200 SAY oSayM PROMPT "" SIZE 80,10 OF oDlg PIXEL

// Um objeto BUTTONBAR server de container para botões com imagens
// e é alinhado automaticamente dentro da dialog
// pode ser alinhado BOTTOM, TOP , LEFT ou RIGHT
DEFINE BUTTONBAR oBtnBar BOTTOM of oDlg
                   
DEFINE BUTTON oBtnBar1 BUTTONBAR oBtnBar RESOURCE "ATALHO" ;
   ACTION (MsgInfo(GetClassName(oBtnBar1)))

// Botão com atalho de teclado
// Basta prefixar a letra a ser usada como atalho com o caractere "&"
// Para acessar o botão pelo teclado, em qualquer ponto da interface,
// basta usar <ALT>+<B>. Caso o foco esteja em um componente que não seja
// de edição de dados, basta pressionar apenas a letra "B".

@ 230,05 BUTTON oButton1 PROMPT "o&Btn1" ;
  ACTION (MsgInfo(GetClassName(oButton1))) ;
  SIZE 040, 013 of oDlg  PIXEL

// Imagem com evento de click
@ 230,100 BITMAP oBmp RESOURCE "ATALHO" NOBORDER SIZE 15,15 OF oDlg ;
  ON CLICK (MsgInfo(GetClassName(oBmp))) OF oDlg  PIXEL

// Este componente nao multiplica as coordenadas ...
// usa coordenadas REAIS - classe tBtnBmp2 
@ 460,240 BTNBMP oBmpBtn1 RESOURCE "ATALHO" SIZE 30,30 OF oDlg  ACTION (MsgInfo(GetClassName(oBmpBtn1)))

// Botores antigos usados em RDmakes, mantidos por compatibilidade
// Cada botão é uma instância da classe SBUTTON(). 
// Sao 23 tipos de botoes, com imagens pré-definidas

DEFINE SBUTTON osBtn01  FROM 250 , 05  TYPE 01 ACTION (MsgInfo("SButton 01")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn02  FROM 250 , 45  TYPE 02 ACTION (MsgInfo("SButton 02")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn03  FROM 250 , 85  TYPE 03 ACTION (MsgInfo("SButton 03")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn04  FROM 250 , 125 TYPE 04 ACTION (MsgInfo("SButton 04")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn05  FROM 250 , 165 TYPE 05 ACTION (MsgInfo("SButton 05")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn06  FROM 250 , 205 TYPE 06 ACTION (MsgInfo("SButton 06")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn07  FROM 250 , 245 TYPE 07 ACTION (MsgInfo("SButton 07")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn08  FROM 250 , 285 TYPE 08 ACTION (MsgInfo("SButton 08")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn09  FROM 250 , 325 TYPE 09 ACTION (MsgInfo("SButton 09")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn10  FROM 250 , 365 TYPE 10 ACTION (MsgInfo("SButton 10")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn11  FROM 260 , 05  TYPE 11 ACTION (MsgInfo("SButton 11")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn12  FROM 260 , 45  TYPE 12 ACTION (MsgInfo("SButton 12")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn13  FROM 260 , 85  TYPE 13 ACTION (MsgInfo("SButton 13")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn14  FROM 260 , 125 TYPE 14 ACTION (MsgInfo("SButton 14")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn15  FROM 260 , 165 TYPE 15 ACTION (MsgInfo("SButton 15")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn16  FROM 260 , 205 TYPE 16 ACTION (MsgInfo("SButton 16")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn17  FROM 260 , 245 TYPE 17 ACTION (MsgInfo("SButton 17")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn18  FROM 260 , 285 TYPE 18 ACTION (MsgInfo("SButton 18")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn19  FROM 260 , 325 TYPE 19 ACTION (MsgInfo("SButton 19")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn20  FROM 260 , 365 TYPE 20 ACTION (MsgInfo("SButton 20")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn21  FROM 270 , 05  TYPE 21 ACTION (MsgInfo("SButton 21")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn22  FROM 270 , 45  TYPE 22 ACTION (MsgInfo("SButton 22")) OF oDlg  ENABLE
DEFINE SBUTTON osBtn23  FROM 270 , 85  TYPE 23 ACTION (MsgInfo("SButton 23")) OF oDlg  ENABLE

// Objeto tTimer(), configurado para disparar um CodeBlock
// a cada 1 segundo ( 1000 milissegundos, tempo minimo )
DEFINE TIMER oTimer INTERVAL 1000 ACTION ( UpdMeter(@nMeterPos,oMeter,oTimer,oSayM) ) OF oDlg

// Na ativação do dialogo, iniciamos o timer
// e colocamos uma funcao para permitir ou nao o fechamento/saida do dialogo
ACTIVATE DIALOG oDlg CENTER ;
  ON INIT (oTimer:Activate()) ;
  VALID (AllowClose(Self,oTimer))

Return

/*-------------------------------------------------------------------
Função UpdMeter()
Função chamada a cada 1 segundo pelo timer. Ela incrementa o contador do meter,
seta a nova posição do contador e atualiza o texto de um label logo abaixo
do meter com o valor atual do contador. Quando o contador passar de 1000,
o timer é desligado pela função
------------------------------------------------------------------- */

STATIC Function UpdMeter( nMeterPos , oMeter , oTimer , oSayM )

nMeterPos++
oMeter:Set(nMeterPos)
oSayM:SetText("Meter Position: "+cValToChar(nMeterPos)+"/1000")

If nMeterPos == 1000
  oTimer:Deactivate()
Endif

Return


/* -------------------------------------------------------------------
Função AllowClose()
Verifica se alguem esta tentando fechar o dialogo.
Caso ela retorne .T., o diálogo é fechado, caso contrário
o diálogo permanece aberto. Caso o fechamento seja autorizado,
a função desliga o timer, para evitar de khaver um disparo de timer
enquanto o diálogo está sendo destruído.
------------------------------------------------------------------- */

STATIC Function AllowClose(oDlg,oTimer)
If MsgYesNo("Deseja encerrar a aplicação ?")
  oTimer:deactivate()
  Return .T.
Endif
Return .F.


