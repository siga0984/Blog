#include 'protheus.ch'

// Macro para calcular o tamanho (WIDTH) cada GET da tela
// baseado no tamanho do campo caractere de entrada
#define CALCSIZEGET( X )  (( X * 4 ) + 4)

/* =============================================================================

Funcao 	U_AGENDA()
Autor	Julio Wittwer
Data 	30/09/2018

Função principal da Agenda, CRUD básico montada em cima de uma DIALOG
Deve ser chamada diretamente pelo SmartClient, não depende do Framework do ERP
Criada utilizando apenas as funções básicas da linguagem AdvPL

============================================================================= */

User Function Agenda()
Local oFont
Local oDlg
Local cTitle := "CRUD - Agenda"

// Define Formato de data DD/MM/AAAA
SET DATE BRITISH
SET CENTURY ON

// Usa uma fonte Fixed Size
oFont := TFont():New('Courier new',,-14,.T.)

// Cria a janela principal da Agenda como uma DIALOG
DEFINE DIALOG oDlg TITLE (cTitle) ;
	FROM 0,0 TO 350,800 ;
	FONT oFont ;
	COLOR CLR_BLACK, CLR_LIGHTGRAY PIXEL

// Ativa a janela principal
// O contexto da Agenda e os componentes são colocados na tela
// pela função DoInit()

ACTIVATE DIALOG oDlg CENTER ;
	ON INIT MsgRun("Aguarde...","Iniciando AGENDA", {|| DoInit(oDlg) }) ;
	VALID CanQuit()

// Fecha contexto da Agenda
CloseAgenda()

return

/* ------------------------------------------
Inicialização da Aplicação
Cria pain[eis e um menu de opções com botões
------------------------------------------ */

STATIC Function doInit(oDlg)
Local oPanel
Local oPanelCrud
Local oBtn1,oBtn2,oBtn3,oBtn4,oBtn5,oBtn6
Local oSay1,oSay2,oSay3,oSay4,oSay5,oSay6,oSay7,oSay8
Local oGet1,oGet2,oGet3,oGet4,oGet5,oGet6,oGet7,oGet8
Local aGets := {}
Local aBtns := {}
Local nMode := 0

Local cID      := Space(6)
Local cNome    := Space(50)
Local cEnder   := Space(50)
Local cCompl   := Space(20)
Local cBairro  := Space(30)
Local cCidade  := Space(40)
Local cUF      := Space(2)
Local cCEP     := Space(8)

CursorArrow() ; CursorWait()

// Abre contexto de dados da agenda
If !OpenAgenda()
	oDlg:End()
	Return .F.
Endif

@ 0,0 MSPANEL oPanelMenu OF oDlg SIZE 70,600 COLOR CLR_WHITE,CLR_GRAY
oPanelMenu:ALIGN := CONTROL_ALIGN_LEFT

@ 0,0 MSPANEL oPanelCrud OF oDlg SIZE 700,600 COLOR CLR_WHITE,CLR_LIGHTGRAY
oPanelCrud:ALIGN := CONTROL_ALIGN_ALLCLIENT

// Cria os botões no Painel Lateral ( Menu )

@ 05,05  BUTTON oBtn1 PROMPT "Incluir" SIZE 60,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,1,@nMode) OF oPanelMenu PIXEL
aadd(aBtns,oBtn1) // Botcao de Inclusao

@ 20,05  BUTTON oBtn2 PROMPT "Alterar" SIZE 60,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,2,@nMode) OF oPanelMenu PIXEL
aadd(aBtns,oBtn2) // Botao de alteração

@ 35,05  BUTTON oBtn3 PROMPT "Excluir" SIZE 60,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,3,@nMode) OF oPanelMenu PIXEL
aadd(aBtns,oBtn3) // Botão de exclusão

@ 50,05  BUTTON oBtn4 PROMPT "Consultar" SIZE 60,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,4,@nMode) OF oPanelMenu PIXEL
aadd(aBtns,oBtn4) // Botão de Consulta - Navegação

@ 65,05  BUTTON oBtn5 PROMPT "Sair" SIZE 60,15 ;
	ACTION oDlg:End() OF oPanelMenu PIXEL

// -----------------------------------------------------------------------------
// Desenha os componentes a partir do Painel de Manutenção
// Say sempre 3 linhas abaixo do GET
// -----------------------------------------------------------------------------

@   5+3,05 SAY oSay1 PROMPT "ID"          RIGHT SIZE 50,12 OF oPanelCrud PIXEL
@  20+3,05 SAY oSay2 PROMPT "Nome"        RIGHT SIZE 50,12 OF oPanelCrud PIXEL
@  35+3,05 SAY oSay3 PROMPT "Endereço"    RIGHT SIZE 50,12 OF oPanelCrud PIXEL
@  50+3,05 SAY oSay4 PROMPT "Complemento" RIGHT SIZE 50,12 OF oPanelCrud PIXEL
@  65+3,05 SAY oSay5 PROMPT "Bairo"       RIGHT SIZE 50,12 OF oPanelCrud PIXEL
@  80+3,05 SAY oSay6 PROMPT "Cidade"      RIGHT SIZE 50,12 OF oPanelCrud PIXEL
@  95+3,05 SAY oSay7 PROMPT "UF"          RIGHT SIZE 50,12 OF oPanelCrud PIXEL
@ 110+3,05 SAY oSay8 PROMPT "CEP"         RIGHT SIZE 50,12 OF oPanelCrud PIXEL

@   5,60 GET oGet1 VAR cID          PICTURE "@!"   SIZE CALCSIZEGET(6) ,12 OF oPanelCrud PIXEL
@  20,60 GET oGet2 VAR cNome        PICTURE "@!"   SIZE CALCSIZEGET(50),12 OF oPanelCrud PIXEL
@  35,60 GET oGet3 VAR cEnder       PICTURE "@!"   SIZE CALCSIZEGET(50),12 OF oPanelCrud PIXEL
@  50,60 GET oGet4 VAR cCompl       PICTURE "@!"   SIZE CALCSIZEGET(20),12 OF oPanelCrud PIXEL
@  65,60 GET oGet5 VAR cBairro      PICTURE "@!"   SIZE CALCSIZEGET(30),12 OF oPanelCrud PIXEL
@  80,60 GET oGet6 VAR cCidade      PICTURE "@!"   SIZE CALCSIZEGET(40),12 OF oPanelCrud PIXEL
@  95,60 GET oGet7 VAR cUF          PICTURE "!!"   SIZE CALCSIZEGET(2) ,12 OF oPanelCrud PIXEL
@ 110,60 GET oGet8 VAR cCEP         PICTURE "@R 99999-999" SIZE CALCSIZEGET(9),12 OF oPanelCrud PIXEL

// Acrescenta no array de GETS o nome do campo
// o objeto TGET correspondente
// e o valor inicial ( em branco ) do campo

aadd( aGets , {"ID"     , oGet1 , space(6)  } )
aadd( aGets , {"NOME"   , oGet2 , space(50) } )
aadd( aGets , {"ENDER"  , oGet3 , space(50) } )
aadd( aGets , {"COMPL"  , oGet4 , space(20) } )
aadd( aGets , {"BAIRR"  , oGet5 , space(30) } )
aadd( aGets , {"CIDADE" , oGet6 , space(40) } )
aadd( aGets , {"UF"     , oGet7 , space(2)  } )
aadd( aGets , {"CEP"    , oGet8 , space(8)  } )

// Cria os Botões de Ação sobre os dados
@ 130,60  BUTTON oBtnConf PROMPT "Confirmar" SIZE 50,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,5,@nMode)  OF oPanelCrud PIXEL

aadd(aBtns,oBtnConf) // [5] Botão de Confirmaçáo

@ 130,125  BUTTON oBtnCanc PROMPT "Voltar" SIZE 50,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,6,@nMode)  OF oPanelCrud PIXEL

aadd(aBtns,oBtnCanc) // [6] Botão de Cancelamento

// Cria os Botões de Navegação Livre
@ 150,60  BUTTON oBtnFirst PROMPT "Primeiro" SIZE 50,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,7,@nMode)  OF oPanelCrud PIXEL
aadd(aBtns,oBtnFirst) // [7] Primeiro

@ 150,125  BUTTON oBtnPrev PROMPT "Anterior" SIZE 50,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,8,@nMode)  OF oPanelCrud PIXEL
aadd(aBtns,oBtnPrev) // [8] Anterior

@ 150,190  BUTTON oBtnNext PROMPT "Próximo" SIZE 50,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,9,@nMode)  OF oPanelCrud PIXEL
aadd(aBtns,oBtnNext) // [9] Proximo

@ 150,255  BUTTON oBtnLast PROMPT "Último" SIZE 50,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,10,@n     Mode)  OF oPanelCrud PIXEL
aadd(aBtns,oBtnLast) // [10] Último

// Seta a interface para o estado inicial
// Habilita apenas inserção e consulta 
nMode := 0
AdjustMode(oDlg,aBtns,aGets,nMode)

Return .T.

/* ------------------------------------------
Validação da Main Window - Quer realmente sair ?
------------------------------------------ */

STATIC Function CanQuit()
Return MsgYesNo("Deseja fechar a Agenda ?")


// --------------------------------------------------------------
// Abertura do contexto de DADOS da Agenda
// Cria uma tabela chamda "AGENDA" no Banco de dados atual
// configurado no Environment em uso pelo DBAccess
// Cria a tabela caso nao exista, cria os índices caso nao existam
// Abre e mantém a tabela aberta em modo compartilhado
// --------------------------------------------------------------

STATIC Function OpenAgenda()
Local nH
Local cFile := "AGENDA"
Local aStru := {}

// Conecta com o DBAccess configurado no ambiente
nH := TCLink()

If nH < 0
	MsgStop("DBAccess - Erro de conexao "+cValToChar(nH))
	QUIT
Endif

If !tccanopen(cFile)
	
	// Se o arquivo nao existe no banco, cria
	
	aadd(aStru,{"ID"    ,"C",06,0})
	aadd(aStru,{"NOME"  ,"C",50,0})
	aadd(aStru,{"ENDER" ,"C",50,0})
	aadd(aStru,{"COMPL" ,"C",20,0})
	aadd(aStru,{"BAIRR" ,"C",30,0})
	aadd(aStru,{"CIDADE","C",40,0})
	aadd(aStru,{"UF"    ,"C",02,0})
	aadd(aStru,{"CEP"   ,"C",08,0})
	
	DBCreate(cFile,aStru,"TOPCONN")
	
Endif

If !tccanopen(cFile,cFile+'1')
	// Se o Indice por ID nao existe, cria
	USE (cFile) ALIAS (cFile) EXCLUSIVE NEW VIA "TOPCONN"
	INDEX ON ID TO (cFile+'1')
	USE
EndIf

If !tccanopen(cFile,cFile+'2')
	// Se o indice por nome nao existe, cria
	USE (cFile) ALIAS (cFile) EXCLUSIVE NEW VIA "TOPCONN"
	INDEX ON NOME TO (cFile+'2')
	USE
EndIf

// Abra o arquivo de agenda em modo compartilhado

USE (cFile) ALIAS AGENDA SHARED NEW VIA "TOPCONN"

If NetErr()
	MsgStop("Falha ao Abrir a Agenda em modo compartilhado.")
	QUIT
	return .F. 
Endif

// Liga o filtro para ignorar registros deletados 
SET DELETED ON 

// Abre os indices, seleciona ordem por ID
// E Posiciona no primeiro registro 
DbSetIndex(cFile+'1')
DbSetIndex(cFile+'2')
DbSetOrder(1)
DbGoTop()

Return .T.

// ----------------------------------------------------------------------
// Funcao de encerramento do contexto de dados da Agenda
// Fecha todos os alias abertos, encerra a conexão com o DBAccess

STATIC Function CloseAgenda()

DBCloseAll()   // Fecha todas as tabelas
Tcunlink()     // Desconecta do DBAccess

Return

// Limpa o conteudo dos GETS,
// e permite habilitar ou desabilitar a edição
STATIC Function ClearGets(aGets,lEnable)
Local nI , nT := len(aGets)
For nI := 1 to nT
	
	// Utiliza o codeblock de Set/Get do Objeto TGET para atribuir
	// os valores iniciais para cada get ( todos em branco )
	EVAL( aGets[nI][2]:bSetGet , aGets[nI][3] )
	
	// Aproveita e habilita ou desabilita a edição
	// de acordo com o parametro recebido
	If lEnable
		aGets[nI][2]:Enable()
	Else
		aGets[nI][2]:Disable()
	Endif
	
Next
Return


// Nao mexe no conteudo dos GETS,
// Permite apenas habilitar ou desabilitar a edição
STATIC Function EnableGets(aGets,lEnable)
Local nI , nT := len(aGets)
For nI := 1 to nT
	If lEnable
		aGets[nI][2]:Enable()
	Else
		aGets[nI][2]:Disable()
	Endif
Next
Return


/* ------------------------------------------
Manutenção da Agenda
Centraliza todas as operações
Monta o formulário baseado na ação escolhida
Cria uma janela de diálogo para fazer as operações 
A função atual faz toda a manutenção e navegação da Agenda
------------------------------------------ */

STATIC Function ManAgenda(oDlg,aBtns,aGets,nAction,nMode)
Local nI , nT
Local cNewId
Local lVolta

If nAction == 1
	
	// Inclusao
	
	// Limpa todos os valores de tela,
	// habilitando os campos para edição
	ClearGets(aGets , .T. )
	
	// Se eu vou incluir, pega o Ultimo ID e soma 1
	DBSelectArea("AGENDA")
	DbsetOrder(1)
	DBGobottom()
	
	// Coloca o novo ID no primeiro GET
	cNewId := StrZero( val(AGENDA->ID) + 1 , 6 )
	EVAL( aGets[1][2]:bSetGet , cNewId )
	aGets[1][2]:Disable()
	
	// Joga o foco para o nome
	aGets[2][2]:SetFocus()
	
	// Seta que o modo atual é Inclusao
	nMode := 1
	AdjustMode(oDlg,aBtns,aGets,nMode)
	
ElseIf nAction == 2
	
	// Alteração
	
	// Se a alteração está habilitada, eu estou posicionado
	// em um registro para alterar . Apenas habilita os GETS, sem mexer no conteuido 
	EnableGets(aGets,.T.)
	
	// Nao permite alterar o ID
	aGets[1][2]:Disable()
	
	// Joga o foco para o nome
	aGets[2][2]:SetFocus()
	
	// Seta que o modo atual é Alteração
	nMode := 2
	AdjustMode(oDlg,aBtns,aGets,nMode)
	
ElseIf nAction == 3
	
	// Exclusao
	// Se a exclusão está habilitada, eu estou posicionado em um registro
	
	// Seta que o modo atual é Exclusão
	nMode := 3
	AdjustMode(oDlg,aBtns,aGets,nMode)
	
ElseIf nAction == 4
	
	// Consulta
	
	DBSelectArea("AGENDA")
	
	// Primeiro verifica se tem alguma coisa para ser mostrada
	If BOF() .and. EOF()
		MsgStop("Não há registros na Agenda.","Consultar")
		Return .F.
	Endif
	
	// Consulta em ordem alfabética, inicia no priemiro registro
	DbsetOrder(2)
	DBGotop()
	
	// Coloca os dados do registro na tela
	ReadRecord(aGets)
	
	// Seta que o modo atual é Consulta
	nMode := 4
	AdjustMode(oDlg,aBtns,aGets,nMode)
	
ElseIf nAction == 5  // Confirma
	
	IF nMode == 1
		
		// Confirmando uma inclusao
		
		DBSelectArea("AGENDA")
		DBAppend()   // Inicia uma inserção
		
		// Coloca o valor dos GETs nos respectivos campos do Alias
		PutRecord(aGets)
		
		// Solta o lock pego automaticamente na inclusao
		// fazendo o flush do registro
		DBRUnlock()
		
		// Volta ao modo inicial
		nMode := 0
		AdjustMode(oDlg,aBtns,aGets,nMode)
		
	ElseIF nMode == 2
		
		// Confirmando uma alteração
		
		DBSelectArea("AGENDA")
		
		If DbrLock(recno())
			
			// Caso tenhha obtido o LOCK para alteração do registro
			// Coloca o valor dos GETs nos respectivos campos do Alias
			PutRecord(aGets)
			
			// Solta o lock obtido para alteração
			DBRUnlock()
			
			// Retorna ao modo de consulta
			nMode := 4
			AdjustMode(oDlg,aBtns,aGets,nMode)
			
		Else
			
			// Nao conseguiu bloqueio do registro
			// Mostra a mensagem e permanece no modo de alteração
			MsgStop("Registro não pode ser alterado, está sendo usado por outro usuário")
			
		Endif
		
	ElseIF nMode == 3
		
		// Confirmando uma exclusão
		
		If DbrLock(recno())
			
			// Apaga o registro ( marca para deleção )
			DBDelete()
			
			// E tenta posicionar no registro anterior
			DbSkip(-1)
			
			// Se nao tem mais registros visiveis, volta ao estado inicial
			If BOF() .and. EOF()
				                              
				MsgStop("Não há mais registros para visualização")
				
				// Volta ao modo inicial
				nMode := 0
				AdjustMode(oDlg,aBtns,aGets,nMode)
				
			Else
				
				// Retorna ao modo de consulta
				nMode := 4
				AdjustMode(oDlg,aBtns,aGets,nMode)
				
			Endif
			
			
		Else
			
			// Nao conseguiu bloqueio do registro
			// Mostra a mensagem e volta para o modo de consulta
			MsgStop("Registro não pode ser apagado, está sendo usado por outro usuário")
	
			nMode := 4
			AdjustMode(oDlg,aBtns,aGets,nMode)
			
		Endif
		
		
	Else
		
		// Confirmação somente é habilitada para inclusão, alteração e exclusão
		UserException("Unexpected Mode "+cValToChaR(nMode))
		
	Endif
	
	// Atualiza os componentes da tela
	oDlg:Refresh()
	
ElseIf nAction == 6  // Voltar / Abandonar operação atual
	
	lVolta := .T.
	
	IF nMode == 1
		// Pergunta se deseja cancelar a inclusão
		// Avisa que qualquer dado digitado será perdido
		lVolta := MsgYesNo("Deseja cancelar a inclusão ? Os dados digitados serão perdidos.")
	ElseIF nMode == 2
		// Pergunta se deseja cancelar a alteração
		// Avisa que qualquer dado digitado será perdido
		lVolta := MsgYesNo("Deseja cancelar a alteração ? Os dados digitados serão perdidos.")
	Endif
	
	If lVolta
		
		IF nMode == 2 .or. nMode == 3
			
			// Se eu estava fazendo uma alteração ou exclusão,
			// eu devo voltar para o modo de consulta
			nMode := 4
			AdjustMode(oDlg,aBtns,aGets,nMode)
			
		Else
			
			// Qualquer outro cancelamento, volta ao estado inicial
			nMode := 0
			AdjustMode(oDlg,aBtns,aGets,nMode)
			
		Endif
		
	Endif
	
ElseIf nAction == 7  // Consulta - Primeiro Registro
	
	DbSelectArea("AGENDA")
	Dbgotop()
	
	// Coloca na tela o conteudo do registro atual
	ReadRecord(aGets)
	
ElseIf nAction == 8  // Consulta - Registro anterior
	
	DbSelectArea("AGENDA")
	DbSkip(-1)
	
	IF BOF()
		// Bateu no início do arquivo
		MsgInfo("Não há registro anterior. Você está no primeiro registro da Agenda")
	ELSE
		// Coloca na tela o conteudo do registro atual
		ReadRecord(aGets)
	Endif
	
ElseIf nAction == 9  // Consulta - Próximo Registro
	
	DbSelectArea("AGENDA")
	DbSkip()
	
	IF Eof()

		// Bateu no final do arquivo
		MsgInfo("Nao há próximo registro. Você está no últmo registro da Agenda")

		// Reposiciona no ultimo registro 
		DBgobottom()
		
	Endif
	
	// Coloca na tela o conteudo do registro atual
	ReadRecord(aGets)
	
ElseIf nAction == 10  // Consulta - Últmio  Registro
	
	DbSelectArea("AGENDA")
	DBGoBottom()
	
	// Coloca na tela o conteudo do registro atual
	ReadRecord(aGets)
	
Else
	
	UserException("Unexpected Action "+cValToChaR(nAction))
	
Endif

// Atualiza os componentes da tela
oDlg:Refresh()

Return

// -------------------------------------------------
// Lê o conteudo do registro atual e alimenta
// os objetos GET na tela
// -------------------------------------------------

STATIC Function ReadRecord(aGets)
Local nI , nT := len(aGets)
Local nPos , cValue
For nI := 1 to nT
	nPos := Fieldpos( aGets[nI][1] )
	cValue := FieldGet(nPos)
	EVAL( aGets[nI][2]:bSetGet, cValue)
Next
Return


// -------------------------------------------------
// Pega os valores dos campos dos GETs da tela
// e atualiza os campos da AGENDA no registro atual
// Observação : O registro atualmente posicionado é atualizado,
// e para isso ele deve ser previamente bloqueado ( DBRLOCK  )
// -------------------------------------------------

STATIC Function PutRecord(aGets)
Local nI , nT := len(aGets)
Local nPos , cValue
For nI := 1 to len(aGets)
	cValue := EVAL( aGets[nI][2]:bSetGet )
	nPos := Fieldpos(aGets[nI][1])
	Fieldput(nPos, cValue)
Next
Return


// -------------------------------------------------
// Habilita ou desabilita os notões de navegação
// -------------------------------------------------

STATIC Function SetNavBtn(aBtns,lEnable)

IF lEnable
	aBtns[7]:Show()  // Primeiro
	aBtns[8]:Show()  // Anterior
	aBtns[9]:Show()  // Proximo
	aBtns[10]:Show() // Ultimo
Else
	aBtns[7]:Hide()  // Primeiro
	aBtns[8]:Hide()  // Anterior
	aBtns[9]:Hide()  // Proximo
	aBtns[10]:Hide() // Ultimo
Endif

Return

// -------------------------------------------------
// Desabilita os botoes de todas as operações
// -------------------------------------------------

STATIC Function DisableOPs(aBtns)
aBtns[1]:Disable() // Inclusao
aBtns[2]:Disable() // Alteracao
aBtns[3]:Disable() // Exclusao
aBtns[4]:Disable() // Consulta / Navegação
return

// -------------------------------------------------
// Ajusta os controles atuais baseado no modo atual
// -------------------------------------------------
STATIC Function AdjustMode(oDlg,aBtns,aGets,nMode)

If nMode == 0
	
	// Modo inicial 
	// Habilita apenas inclisao e consulta 
	oDlg:CTITLE("CRUD - Agenda")
	
	// Modo incial habilita apenas inclusao e consulta
	// Alteração e Exclusao somente serao habilitados 
	// quando a interface estiver mostrando um registro 
	
	aBtns[1]:Enable()   // Inclusao
	aBtns[2]:Disable()  // Alteracao
	aBtns[3]:Disable()  // Exclusao
	aBtns[4]:Enable()   // Consulta / Navegação
	
	// Esconde Confirmar e Voltar
	aBtns[5]:Hide()  // Confirma
	aBtns[6]:Hide()  // Volta
	
	// Esconde botoes de navegação
	SetNavBtn(aBtns,.F.)
	
	// Limpa todos os valores de tela , desabilitando os GETS
	ClearGets(aGets , .F. )

ElseIf nMode == 1
	
	oDlg:CTITLE("CRUD - Agenda (Inclusão)")
	
	// Ajusta botoes baseado no modo atual ( Inclusao )
	// Desliga todas as operaçòes
	DisableOPs(aBtns)
	
	// Mostra Confirmar e Voltar
	aBtns[5]:Show() // Confirmar
	aBtns[6]:Show() // Voltar
	
	// Esconde botoes de navegação
	SetNavBtn(aBtns,.F.)
	
ElseIF  nMode == 2
	
	oDlg:CTITLE("CRUD - Agenda (Alteração)")
	
	// Ajusta botoes baseado no modo atual ( Alteração )
	
	// Desliga todas as operaçòes
	DisableOPs(aBtns)
	                        
	// Mostra Confirmar e Voltar
	aBtns[5]:Show() // Confirmar
	aBtns[6]:Show() // Voltar
	
	// Esconde botoes de navegação
	SetNavBtn(aBtns,.F.)
	
ElseIF  nMode == 3
	
	oDlg:CTITLE("CRUD - Agenda (Exclusão)")
	
	// Ajusta botoes baseado no modo atual ( Exclusão )
	
	// Desliga todas as operaçòes
	DisableOPs(aBtns)
	
	// Mostra Confirmar e Voltar
	aBtns[5]:Show() // Confirmar
	aBtns[6]:Show() // Voltar
	
	// Esconde botoes de navegação
	SetNavBtn(aBtns,.F.)
	
ElseIF  nMode == 4
	
	oDlg:CTITLE("CRUD - Agenda (Consulta)")
	
	// Ajusta botoes baseado no modo atual ( Consulta )
	
	aBtns[1]:Enable() // Inclusao
	aBtns[2]:Enable() // Alteracao
	aBtns[3]:Enable() // Exclusao
	aBtns[4]:Disable() // Consulta / Navegação
	
	// Esconde Confirmar e Voltar
	aBtns[5]:Hide() // Confirmar
	aBtns[6]:Hide() // Voltar
	
	// Mostra botoes de navegação apenas na consulta 
	SetNavBtn(aBtns,.T.)
	
Endif

Return

