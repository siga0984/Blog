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

Release 1.1 em 05/10/2018

Correção - Após cancelar uma alteração, atualiza os campos da tela 
           com o conteúdo original do registro
Melhoria - Cria Novo ID apenas na hora de gravar
Melhoria - Inserir validação no campo UF, para permitir ou um valor em branco, ou 
           um valor válido
Melhoria - Inserir campos FONE1, FONE2, e EMAIL
Melhoria - Implementar busca indexada por ID e NOME

Release 1.3

Melhoria - Busca de CEP integrada
Melhoria - Mostrar Mapa
Melhoria - Enviar e-Mail 

Release 1.4 - Foto 3x4

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
	FROM 0,0 TO 420,830 ;
	FONT oFont ;
	COLOR CLR_BLACK, CLR_LIGHTGRAY PIXEL

// Ativa a janela principal
// O contexto da Agenda e os componentes são colocados na tela
// pela função DoInit()

ACTIVATE DIALOG oDlg CENTER ;
	ON INIT MsgRun("Aguarde...","Iniciando AGENDA", {|| DoInit(oDlg) }) ;
	VALID CanQuit()

// Fecha contexto de dados
CloseDB()

return

/* ------------------------------------------
Inicialização da Aplicação
Cria pain[eis e um menu de opções com botões
------------------------------------------ */

STATIC Function doInit(oDlg)
Local oPanelMenu, oPanelCrud, oPanelNav
Local oBtn1,oBtn2,oBtn3,oBtn4,oBtn5,oBtn6
Local oSay1,oSay2,oSay3,oSay4,oSay5,oSay6,oSay7,oSay8,oSay9,oSayA,oSayB
Local oGet1,oGet2,oGet3,oGet4,oGet5,oGet6,oGet7,oGet8,oGet9,oGetA,oGetB
Local oBtnFirst, oBtnPrev, oBtnNext, oBtnLast, oBtnPesq, oBtnOrd
Local oBtnMap, oBtnMail, oBtnImg
Local oSayOrd , oBtnCEP, oBmpFoto
Local aGets := {}
Local aBtns := {}
Local nMode := 0
Local cImgDefault := "\temp\tmp_photo_3x4.img"
Local nH

Local cID      := Space(6)
Local cNome    := Space(50)
Local cEnder   := Space(50)
Local cCompl   := Space(20)
Local cBairro  := Space(30)
Local cCidade  := Space(40)
Local cUF      := Space(2)
Local cCEP     := Space(8)
Local cFone1   := Space(20)
Local cFone2   := Space(20)
Local cEmail   := Space(40)

CursorArrow() ; CursorWait()

// Abra conexao com banco de dados 
OpenDB()

// Abre contexto de dados da agenda
OpenAgenda()

// Cria a imagem padrao da agenda 3x4
if !file(cImgDefault)
	nH := fCreate(cImgDefault)
	fWrite(nH,RAW3x4())
	fclose(nH)
Endif

@ 0,0 MSPANEL oPanelMenu OF oDlg SIZE 70,600 COLOR CLR_WHITE,CLR_GRAY
oPanelMenu:ALIGN := CONTROL_ALIGN_LEFT

@ 0,0 MSPANEL oPanelNav OF oDlg SIZE 70,600 COLOR CLR_WHITE,CLR_GRAY
oPanelNav:ALIGN := CONTROL_ALIGN_RIGHT

@ 0,0 MSPANEL oPanelCenter OF oDlg SIZE 700,600 COLOR CLR_WHITE,CLR_LIGHTGRAY
oPanelCenter:ALIGN := CONTROL_ALIGN_ALLCLIENT

@ 0,0 MSPANEL oPanelOrd OF oPanelCenter SIZE 100,20 COLOR CLR_WHITE,CLR_BLUE
oPanelOrd:ALIGN := CONTROL_ALIGN_TOP

@ 0,0 MSPANEL oPanelCrud OF oPanelCenter SIZE 700,600 COLOR CLR_WHITE,CLR_LIGHTGRAY
oPanelCrud:ALIGN := CONTROL_ALIGN_ALLCLIENT
                
// Mostra Ordenação atual do arquivo de agenda 

@   5,5 SAY oSayOrd PROMPT " " SIZE 100,12 COLOR CLR_WHITE,CLR_BLUE OF oPanelOrd PIXEL
oSayOrd:SetText("Ordem .... "+ AGENDA->(IndexKey()))

// Cria os botões no Painel Lateral ( Menu )

@ 05,05  BUTTON oBtn1 PROMPT "Incluir" SIZE 60,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,1,@nMode,oSayOrd,oBmpFoto) OF oPanelMenu PIXEL
aadd(aBtns,oBtn1) // Botcao de Inclusao

@ 20,05  BUTTON oBtn2 PROMPT "Alterar" SIZE 60,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,2,@nMode,oSayOrd,oBmpFoto) OF oPanelMenu PIXEL
aadd(aBtns,oBtn2) // Botao de alteração

@ 35,05  BUTTON oBtn3 PROMPT "Excluir" SIZE 60,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,3,@nMode,oSayOrd,oBmpFoto) OF oPanelMenu PIXEL
aadd(aBtns,oBtn3) // Botão de exclusão

@ 50,05  BUTTON oBtn4 PROMPT "Consultar" SIZE 60,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,4,@nMode,oSayOrd,oBmpFoto) OF oPanelMenu PIXEL
aadd(aBtns,oBtn4) // Botão de Consulta - Navegação

@ 65,05  BUTTON oBtn5 PROMPT "Sair" SIZE 60,15 ;
	ACTION oDlg:End() OF oPanelMenu PIXEL

@ 90,05 BITMAP oBmpFoto OF oPanelMenu PIXEL 
  
oBmpFoto:nWidth := 120
oBmpFoto:nHeight := 160
oBmpFoto:lStretch := .T.

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

// Novos campos inseridos em 07/10
@ 125+3,05 SAY oSay9 PROMPT "Fone 1"      RIGHT SIZE 50,12 OF oPanelCrud PIXEL
@ 140+3,05 SAY oSayA PROMPT "Fone 2"      RIGHT SIZE 50,12 OF oPanelCrud PIXEL
@ 155+3,05 SAY oSayB PROMPT "e-Mail"      RIGHT SIZE 50,12 OF oPanelCrud PIXEL

@   5,60 GET oGet1 VAR cID          PICTURE "@!"   SIZE CALCSIZEGET(6) ,12 OF oPanelCrud PIXEL
@  20,60 GET oGet2 VAR cNome        PICTURE "@!"   SIZE CALCSIZEGET(50),12 OF oPanelCrud PIXEL
@  35,60 GET oGet3 VAR cEnder       PICTURE "@!"   SIZE CALCSIZEGET(50),12 OF oPanelCrud PIXEL
@  50,60 GET oGet4 VAR cCompl       PICTURE "@!"   SIZE CALCSIZEGET(20),12 OF oPanelCrud PIXEL
@  65,60 GET oGet5 VAR cBairro      PICTURE "@!"   SIZE CALCSIZEGET(30),12 OF oPanelCrud PIXEL
@  80,60 GET oGet6 VAR cCidade      PICTURE "@!"   SIZE CALCSIZEGET(40),12 OF oPanelCrud PIXEL
@  95,60 GET oGet7 VAR cUF          PICTURE "!!"   SIZE CALCSIZEGET(2) ,12 VALID VldUf(cUF) OF oPanelCrud PIXEL
@ 110,60 GET oGet8 VAR cCEP         PICTURE "@R 99999-999" SIZE CALCSIZEGET(9),12 OF oPanelCrud PIXEL

// Habilita a busca de CEP com um botao do lado do GET
// O Botcao somente está disponivel caso o oGet8 ( campo CEP )
// estiver habilitado para edição 

@ 110,110  BUTTON oBtnCEP PROMPT "Buscar CEP" SIZE 60,14 ;
    WHEN (oGet8:LACTIVE) ; 
	ACTION BuscaCEP(oDlg,aBtns,aGets)  OF oPanelCrud PIXEL

// Novos campos inseridos em 07/10
@ 125,60 GET oGet9 VAR cFone1       PICTURE "@!" SIZE CALCSIZEGET(20),12 OF oPanelCrud PIXEL
@ 140,60 GET oGetA VAR cFone2       PICTURE "@!" SIZE CALCSIZEGET(20),12 OF oPanelCrud PIXEL
@ 155,60 GET oGetB VAR cEMAIL       PICTURE "@!" SIZE CALCSIZEGET(40),12 OF oPanelCrud PIXEL

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

// Novos campos inseridos em 07/10
aadd( aGets , {"FONE1"  , oGet9 , space(20)  } )
aadd( aGets , {"FONE2"  , oGetA , space(20)  } )
aadd( aGets , {"EMAIL"  , oGetB , space(40)  } )

// Cria os Botões de Ação sobre os dados
@ 175,60  BUTTON oBtnConf PROMPT "Confirmar" SIZE 60,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,5,@nMode,oSayOrd,oBmpFoto)  OF oPanelCrud PIXEL

aadd(aBtns,oBtnConf) // [5] Botão de Confirmaçáo

@ 175,125  BUTTON oBtnCanc PROMPT "Voltar" SIZE 60,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,6,@nMode,oSayOrd,oBmpFoto)  OF oPanelCrud PIXEL

aadd(aBtns,oBtnCanc) // [6] Botão de Cancelamento

// Cria os Botões de Navegação Livre
@ 05,05  BUTTON oBtnFirst PROMPT "Primeiro" SIZE 60,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,7,@nMode,oSayOrd,oBmpFoto)  OF oPanelNav PIXEL
aadd(aBtns,oBtnFirst) // [7] Primeiro

@ 020,05  BUTTON oBtnPrev PROMPT "Anterior" SIZE 60,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,8,@nMode,oSayOrd,oBmpFoto)  OF oPanelNav PIXEL
aadd(aBtns,oBtnPrev) // [8] Anterior

@ 35,05  BUTTON oBtnNext PROMPT "Próximo" SIZE 60,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,9,@nMode,oSayOrd,oBmpFoto)  OF oPanelNav PIXEL
aadd(aBtns,oBtnNext) // [9] Proximo

@ 50,05  BUTTON oBtnLast PROMPT "Último" SIZE 60,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,10,@nMode,oSayOrd,oBmpFoto)  OF oPanelNav PIXEL
aadd(aBtns,oBtnLast) // [10] Último

@ 65,05  BUTTON oBtnPesq PROMPT "Pesquisa" SIZE 60,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,11,@nMode,oSayOrd,oBmpFoto)  OF oPanelNav PIXEL
aadd(aBtns,oBtnPesq) // [11] Pesquisa

@ 80,05  BUTTON oBtnOrd PROMPT "Ordem" SIZE 60,15 ;
	ACTION ManAgenda(oDlg,aBtns,aGets,12,@nMode,oSayOrd,oBmpFoto)  OF oPanelNav PIXEL
aadd(aBtns,oBtnOrd) // [12] Ordem

@ 95,05  BUTTON oBtnMap PROMPT "Mapa" SIZE 60,15 ;
	ACTION ShowMap(oDlg,aBtns,aGets)  OF oPanelNav PIXEL
aadd(aBtns,oBtnMap) // [13] Mapa

@ 110,05  BUTTON oBtnMail PROMPT "G-Mail" SIZE 60,15 ;
    WHEN !empty(cEMAIL) ; 
	ACTION SendGMail(cEMAIL)  OF oPanelNav PIXEL
aadd(aBtns,oBtnMail) // [14] e-Mail

@ 125,05  BUTTON oBtnImg PROMPT "Foto 3x4" SIZE 60,15 ; 
    WHEN ( nMode == 4  ) ; // Editar foro disponivel apenas durante a consulta
	ACTION ChgImage(oDlg,aBtns,aGets,oBmpFoto)  OF oPanelNav PIXEL
aadd(aBtns,oBtnImg) // [15] Foto 3x4

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
Local cFile := "AGENDA"
Local aStru := {}
Local aDbStru := {}
Local nRet

// Coloca um MUTEX na operação de criar/abrir o arquivo de Agenda
// Assim apenas um processo por vez realiza a manutenção nas
// estruturas ou indices, caso necessario 

While !GlbNmLock("AGENDA_DB")
	If !MsgYesNo("Existe outro processo abrindo a Agenda. Deseja tentar novamente ?")
		MSgStop("Abertura da Agenda em uso -- tente novamente mais tarde.")
		QUIT
	Endif
Enddo

// Cria o array com os campos do arquivo 
aadd(aStru,{"ID"    ,"C",06,0})
aadd(aStru,{"NOME"  ,"C",50,0})
aadd(aStru,{"ENDER" ,"C",50,0})
aadd(aStru,{"COMPL" ,"C",20,0})
aadd(aStru,{"BAIRR" ,"C",30,0})
aadd(aStru,{"CIDADE","C",40,0})
aadd(aStru,{"UF"    ,"C",02,0})
aadd(aStru,{"CEP"   ,"C",08,0})

// Novos campos inseridos em 07/10
aadd(aStru,{"FONE1" ,"C",20,0})
aadd(aStru,{"FONE2" ,"C",20,0})
aadd(aStru,{"EMAIL" ,"C",40,0})

// Novos campos inseridos em 21/10
aadd(aStru,{"IMAGE" ,"M",10,0})

If !TCCanOpen(cFile)
	
	// Se o arquivo nao existe no banco, cria
	DBCreate(cFile,aStru,"TOPCONN")
	
Else

	// O Arquivo já existe, vamos comparar as estruturas
	USE (cFile) ALIAS (cFile) SHARED NEW VIA "TOPCONN"
	IF NetErr()
		MsgSTop("Falha ao abrir a Agenda em modo compartilhado. Tente novamente mais tarde.")
		QUIT
	Endif
	aDbStru := DBStruct()
	USE
	
	If len(aDbStru) != len(aStru)
		// Estao faltando campos no banco ? 
		// Vamos alterar a estrutura da tabela
		// Informamos a estrutura atual, e a estrutura esperada
		If !TCAlter(cFile,aDbStru,aStru)
			MsgSTop(tcsqlerror(),"Falha ao alterar a estrutura da AGENDA")
			QUIT
		Endif
		MsgInfo("Estrutura do arquivo AGENDA atualizada.")
	Endif
	
Endif

If !TCCanOpen(cFile,cFile+'_UNQ')
	// Se o Indice único da tabela nao existe, cria 
	USE (cFile) ALIAS (cFile) EXCLUSIVE NEW VIA "TOPCONN"
	IF NetErr()
		MsgSTop("Falha ao abrir a Agenda em modo EXCLUSIVO. Tente novamente mais tarde.")
		QUIT
	Endif
	nRet := TCUnique(cFile,"ID")
	If nRet < 0 
		MsgSTop(tcsqlerror(),"Falha ao criar índice único")
		QUIT
	Endif
	USE
EndIf

If !TCCanOpen(cFile,cFile+'1')
	// Se o Indice por ID nao existe, cria
	USE (cFile) ALIAS (cFile) EXCLUSIVE NEW VIA "TOPCONN"
	IF NetErr()
		MsgSTop("Falha ao abrir a Agenda em modo EXCLUSIVO. Tente novamente mais tarde.")
		QUIT
	Endif
	INDEX ON ID TO (cFile+'1')
	USE
EndIf

If !TCCanOpen(cFile,cFile+'2')
	// Se o indice por nome nao existe, cria
	USE (cFile) ALIAS (cFile) EXCLUSIVE NEW VIA "TOPCONN"
	IF NetErr()
		MsgSTop("Falha ao abrir a Agenda em modo EXCLUSIVO. Tente novamente mais tarde.")
		QUIT
	Endif
	INDEX ON NOME TO (cFile+'2')
	USE
EndIf

// Abra o arquivo de agenda em modo compartilhado
USE (cFile) ALIAS AGENDA SHARED NEW VIA "TOPCONN"

If NetErr()
	MsgSTop("Falha ao abrir a Agenda em modo compartilhado. Tente novamente mais tarde.")
	QUIT
Endif

// Liga o filtro para ignorar registros deletados 
SET DELETED ON 

// Abre os indices, seleciona ordem por ID
// E Posiciona no primeiro registro 
DbSetIndex(cFile+'1')
DbSetIndex(cFile+'2')
DbSetOrder(1)
DbGoTop()

// Solta o MUTEX 
GlbNmUnlock("AGENDA_DB")

Return .T.

// ----------------------------------------------------------------------
// Funcao de encerramento do contexto de dados da Agenda
// Fecha todos os alias abertos, encerra a conexão com o DBAccess

STATIC Function CloseDB()

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

STATIC Function ManAgenda(oDlg,aBtns,aGets,nAction,nMode,oSayOrd,oBmpFoto)
Local nI , nT
Local cNewId
Local lVolta

If nAction == 1
	
	// Inicio da Inclusao -- Novo contato 
	
	// Limpa todos os valores de tela,
	// habilitando os campos para edição
	ClearGets(aGets , .T. )

	// Release 1.1
	// Nao abre o campo ID para edição 
	// E nao gera o numero agora, somente na hora de gravar
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
	
	// Entrou no modo de Consulta / Navegação
	
	DBSelectArea("AGENDA")
	
	// Primeiro verifica se tem alguma coisa para ser mostrada
	If BOF() .and. EOF()
		MsgStop("Não há registros na Agenda.","Consultar")
		Return .F.
	Endif
	
	// Consulta em ordem alfabética, inicia no primeiro registro
	DbsetOrder(2)
	DBGotop()
	
	// Atualiza texto com a ordem 
	oSayOrd:SetText("Ordem .... "+ AGENDA->(IndexKey()))
	
	// Coloca os dados do registro na tela
	ReadRecord(aGets)

	// Mostra a imagem do contato 	
	ShowImg(oBmpFoto)
	
	// Seta que o modo atual é Consulta
	nMode := 4
	AdjustMode(oDlg,aBtns,aGets,nMode)
	
ElseIf nAction == 5  // Confirma
	
	IF nMode == 1
		
		// Confirmando uma inclusao

		// Release 1.1
		// Usa uma funcao para criar um novo ID para a Agenda
		cNewID := GetNewID()

		// Release 1.2
		// Se o ID está vazio, nao foi possivel obter o Semaforo
		// para a geração do ID -- Muitas operações de inserção concorentes
				
		While empty(cNewID)

			If MsgInfo("Falha ao obter um novo ID para inclusão. Deseja tentar novamente ?")
				cNewID := GetNewID()
				LOOP
			Endif   
			
			MsgStop("Não é possível incluir na agenda neste momento. Tente novamente mais tarde.")
			Return
			
		Enddo

		// Coloca o valor no GET 
		EVAL( aGets[1][2]:bSetGet , cNewId )
		
		DBSelectArea("AGENDA")

		// Inicia uma inserção
		DBAppend()   
		
		// Coloca o valor dos GETs nos respectivos campos do Alias
		PutRecord(aGets)
		
		// Solta o lock pego automaticamente na inclusao
		// fazendo o flush do registro
		DBRUnlock()

		// Volta para a ordem alfabética 		
		dbsetorder(2)

		// Atualiza texto com a ordem 
		oSayOrd:SetText("Ordem .... "+ AGENDA->(IndexKey()))
		
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
			
			If BOF()
			    // Se já estava no primeiro registro, 
			    // volta para o topo do arquivo 
				DbGoTOP()			
			Endif

			If BOF() .and. EOF()
				                              
				// Se nao tem mais registros visiveis, volta ao estado inicial
				MsgStop("Não há mais registros para visualização")
				
				// Volta ao modo inicial
				nMode := 0
				AdjustMode(oDlg,aBtns,aGets,nMode)
				
			Else
			
				// Coloca os dados do registro atual na tela
				ReadRecord(aGets)

				// Mostra a imagem do contato 	
				ShowImg(oBmpFoto)
				
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
			
			// Release 1.1
			// Atualiza os dados do registro atual na tela
			ReadRecord(aGets)

			// Mostra a imagem do contato 	
			ShowImg(oBmpFoto)

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

	// Mostra a imagem do contato 	
	ShowImg(oBmpFoto)
	
ElseIf nAction == 8  // Consulta - Registro anterior
	
	DbSelectArea("AGENDA")
	DbSkip(-1)
	
	IF BOF()
		// Bateu no início do arquivo
		MsgInfo("Não há registro anterior. Você está no primeiro registro da Agenda")
	ELSE

		// Coloca na tela o conteudo do registro atual
		ReadRecord(aGets)

		// Mostra a imagem do contato 	
		ShowImg(oBmpFoto)

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

	// Mostra a imagem do contato 	
	ShowImg(oBmpFoto)

ElseIf nAction == 10  // Consulta - Últmio  Registro
	
	DbSelectArea("AGENDA")
	DBGoBottom()
	
	// Coloca na tela o conteudo do registro atual
	ReadRecord(aGets)
	
	// Mostra a imagem do contato 	
	ShowImg(oBmpFoto)

ElseIf nAction == 11  // Pesquisa Indexada

	// Realiza a busca 
	PesqIndeX(oDlg)

	// Atualiza na tela o conteudo do registro atual 
	ReadRecord(aGets)
	
	// Mostra a imagem do contato 	
	ShowImg(oBmpFoto)

ElseIf nAction == 12  // Troca de Ordem

	IF ChangeOrd(oDlg)
		// Se a ordem foi trocada 
		// Atualiza texto com a chave do indice em uso 
		oSayOrd:SetText("Ordem .... "+ AGENDA->(IndexKey()))
    Endif
    

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
// Habilita ou desabilita os botões de navegação 
// do painel de botões do lado direito 
// -- Menos os botoes de e-Mail e Foto 3x4, que tem ativação condicional 
// -------------------------------------------------
STATIC Function SetNavBtn(aBtns,lEnable)
Local nI
For nI := 7 to 13
	aBtns[nI]:SetEnable(lEnable)
Next
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

	// Desliga a edição dos GETS	
	EnableGets(aGets,.F.)

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
	
	// Desliga a edição dos GETS	
	EnableGets(aGets,.F.)
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


// Release 1.2
// Usar cache em memória para gerar a sequencia

STATIC Function GetNewID()
Local cLastID,cNewId
Local nRetry := 0
While !GlbNmLock("AGENDA_ID")
	// Espera máxima de 1 segundo, 20 tentativas
	// com intervalos de 50 milissegundos
	nRetry++
	If nRetry > 20
		return ""
	Endif
	Sleep(50)
Enddo
cLastID := GetGlbValue("AGENDA_SEQ")
If Empty(cLastID)
	// Somente busco na Tabela se eu nao tenho o valor na memoria
	DBSelectArea("AGENDA")
	DbsetOrder(1)
	DBGobottom()
	cLastId := AGENDA->ID
Endif
cNewId := StrZero( val(cLastID) + 1 , 6 )
PutGlbValue("AGENDA_SEQ",cNewID)
GlbNmUnlock("AGENDA_ID")
Return cNewId


// Release 1.1 
// Exemplo de validação do campo cUF ( Estado ou Unidade da Federação ) 

STATIC Function VldUf(cUF)
If Empty(cUF)
	// Estado nao informado / vazio / em branco 
	Return .T.
Endif
If cUF $ "AC,AL,AP,AM,BA,CE,DF,ES,GO,MA,MT,MS,MG,PA,PB,PR,PE,PI,RJ,RN,RS,RO,RR,SC,SP,SE,TO"
	// Estado digitado está na lista ? Beleza
	Return .T.
Endif          

// Chegou até aqui ? O estado informado não é válido. 
// Mostra a mensagem e retorna .F., nao permitindo 
// que o foco seja removido do campo 
MsgSTop("Unidade da Federação inválida ou esconhecida : ["+cUF+"] - "+;
        "Informe um valor válido ou deixe este campo em branco.","Erro na Validação")

Return .F.



Static Function ChangeOrd(oDlg)
Local nOrdAtu := AGENDA->(IndexOrd())
Local nNewOrd := 0
If nOrdAtu == 1 
	If MsgYesNo("Deseja alterar para ordem de NOME ?")
		nNewOrd := 2
	Endif
Else
	If MsgYesNo("Deseja alterar para ordem de ID ?")
		nNewOrd := 1
	Endif
Endif 

if ( nNewOrd > 0 ) 
	AGENDA->(DBSETORDER(nNewOrd))
Endif

return nNewOrd > 0 


STATIC Function PesqIndeX(oDlgParent)
Local oDlgPesq 
Local cTitle
Local cStrBusca
Local nTamanho
Local nRecSave 
Local lFound := .F.
Local cIndexFld := AGENDA->(Indexkey())
Local oGet1 , oBtn1

// Monta titulo da janela de pesquisa
cTitle := 'Pesquisa por '+ cIndexFld

// Guarda numero do registro atual 
nRecSave := AGENDA->(Recno())

If indexord() == 1
	nTamanho := 6
	cStrBusca := space(nTamanho)
	cPicture := "@9"
ElseIf indexord() == 2
	nTamanho := 50
	cStrBusca := space(nTamanho)
	cPicture := "@!"
Endif

DEFINE DIALOG oDlgPesq TITLE (cTitle) ;
	FROM 0,0 TO 120,415 PIXEL;
	FONT oDlgParent:oFont ;
	OF oDlgParent ; 
	COLOR CLR_BLACK, CLR_LIGHTGRAY 

@ 05,05 GET oGet1 VAR cStrBusca   PICTURE (cPicture)   SIZE CALCSIZEGET(nTamanho) ,12 OF oDlgPesq PIXEL

@ 25,05 BUTTON oBtn1 PROMPT "Buscar" SIZE 60,15 ;
	ACTION IIF( SeekAgenda(cIndexFld,cStrBusca) , (lFound := .T. , oDlgPesq:End()) , oGet1:SetFocus() ) OF oDlgPesq PIXEL

ACTIVATE DIALOG oDlgPesq CENTER

If !lFound
	// Nao achou, volta ao registro antes da busca 
	AGENDA->(dbgoto(nRecSave))
Endif

Return 


STATIC Function SeekAgenda(cIndexFld,cStrBusca)

IF cIndexFld == 'ID'
	cStrBusca := strzero(val(cStrBusca),6)
ElseIF cIndexFld == 'NOME'
	cStrBusca := alltrim(cStrBusca)
Endif

If !DbSeek(cStrBusca)
	MsgStop("Informação não encontrada.","Busca por ["+cStrBusca+"]")
	return .F.
Endif

return .T. 


/* ---------------------------------------------------
Botcao para busca de CEP e preenchimento de campos 
de endereço automaticamente. 
--------------------------------------------------- */

Static Function BuscaCEP(oDlg,aBtns,aGets)

Local nPos , cCEP
Local cJsonCEP
Local oJsonObj
Local aJsonFields := {}
Local nRetParser := 0
Local oJHashMap
Local lOk
Local cCEPEnder  := ''
Local cCEPBairro := ''
Local cCepCidade := ''
Local cCEPUF     := ''
Local lCEPERRO   := .F.

// Busca o campo CEP nos Gets e recupera o valor informado
nPos := ascan(aGets , {|x| x[1] == "CEP" } )
cCep := Eval(aGets[nPos][2]:bSetGet)
cCep := alltrim(cCep)

// Verifica se o valor informado está completo - 8 digitos
IF len(cCEp)  < 8
	MsgStop("Digite o número do CEP completo para a busca.","CEP Inválido ou incompleto")
	Return
Endif

// Busca o CEP usando uma API WEB
// Em caso de sucesso, a API retorna um JSON
// Em caso de falha, uam string vazia
cJsonCEP := WebGetCep(cCEP)

If !empty(cJsonCEP)
	
	// Caso o CEP tenha sido encontrado, chama o parser JSON
	
	oJsonObj := tJsonParser():New()
	
	// Faz o Parser da mensagem JSon e extrai para Array (aJsonfields)
	// e cria tambem um HashMap para os dados da mensagem (oJHM)
	lOk := oJsonObj:Json_Hash(cJsonCEP, len(cJsonCEP), @aJsonfields, @nRetParser, @oJHashMap)
	
	
	If ( !Lok  )
		
		MsgStop(cJsonCEP,"Falha ao identificar CEP",cCEP)
		
	Else
		
		// Obtem o valor dos campos usando a chave
		HMGet(oJHashMap, "erro", lCEPERRO)
		
		if lCEPERRO
			
			MsgStop("CEP Inexistente na Base de Dados","Falha ao buscar CEP "+cCEP)
			
		Else
			
			
			HMGet(oJHashMap, "logradouro", cCEPEnder)
			HMGet(oJHashMap, "bairro", cCEPBairro)
			HMGet(oJHashMap, "localidade", cCepCidade)
			HMGet(oJHashMap, "uf", cCEPUF)
			
			cCEPEnder  := padr(upper(cCEPEnder) ,50)
			cCEPBairro := padr(upper(cCEPBairro),30)
			cCepCidade := padr(upper(cCepCidade),40)
			cCEPUF     := padr(Upper(cCEPUF)    ,2)
			
			IF MsgYesNo("Endereço ... "+cCEPEnder + chr(10) + ;
				"Bairro ..... "+cCEPBairro + chr(10) + ;
				"Cidade ..... "+cCEPCidade+ chr(10) + ;
				"Estado ..... "+cCepUF+ chr(10) + ;
				"Deseja atualizar o formulário com estes dados?","CEP encontrado")
				
				nPos := ascan(aGets , {|x| x[1] == "ENDER" } )
				Eval(aGets[nPos][2]:bSetGet , cCEPEnder )
				
				nPos := ascan(aGets , {|x| x[1] == "BAIRR" } )
				Eval(aGets[nPos][2]:bSetGet , cCEPBAirro )
				
				nPos := ascan(aGets , {|x| x[1] == "CIDADE" } )
				Eval(aGets[nPos][2]:bSetGet , cCepCidade )
				
				nPos := ascan(aGets , {|x| x[1] == "UF" } )
				Eval(aGets[nPos][2]:bSetGet , cCepUF )
				
			Endif
			
		Endif
		
	Endif
	
	FreeObj(oJsonObj)
	FreeObj(oJHashMap)
	
Endif

Return

/* ---------------------------------------------------
Busca na WEB o CEP Informado 
Usa API en JSON oferecida pelo site viacep.com.br
Retorno da API  : 
 
{
  "cep": "06709-300",
  "logradouro": "Estrada Aldeia",
  "complemento": "",
  "bairro": "Granja Viana",
  "localidade": "Cotia",
  "uf": "SP",
  "unidade": "",
  "ibge": "3513009",
  "gia": "2781"
}

--------------------------------------------------- */

STATIC Function WebGetCep(cCEP)
Local cUrl  , cJsonRet
Local nCode , cMsg := ''
// Montando a URL de pesquisa
cUrl := 'http://viacep.com.br/ws/'+cCEP+'/json/'
// Buscando o CEP
cJsonRet := httpget(cUrl)
// Verificando retorno 
If empty(cJsonRet)
	nCode := HTTPGETSTATUS(@cMsg)
	MsgStop(cMsg+" ( HTTP STATUS = "+cValToChar(nCode)+" )","Falha na Busca de CEP")
Endif
Return cJsonRet

              
/* --------------------------------------------------------------------------
Abre uma guia no navegador padrão da máquina onde está sendo executado 
o SmartClient, com o mapa do Google Maps pesquisando o endereço 
do contato atualmente em foco na tela da agenda.

URL de Exemplo:
https://www.google.com/maps/search/?api=1&query=Estrada da Aldeia,Cotia,SP,06709300
-------------------------------------------------------------------------- */

STATIC Function ShowMap(oDlg,aBtns,aGets)
Local nPos
Local cEndereco
Local cCidade
Local cUF
Local cCEP
Local cMapsURL := 'https://www.google.com/maps/search/?api=1&query='
Local cUrlQry := ''

nPos := ascan(aGets , {|x| x[1] == "ENDER" } )
cEndereco := alltrim(Eval( aGets[nPos][2]:bSetGet ))
If !empty(cEndereco)
	cUrlQry += UrlEscape(cEndereco+',')
Endif
				
nPos := ascan(aGets , {|x| x[1] == "CIDADE" } )
cCidade := alltrim(Eval( aGets[nPos][2]:bSetGet ))
If !empty(cCidade)
	cUrlQry += UrlEscape(cCidade+',')
Endif

nPos := ascan(aGets , {|x| x[1] == "UF" } )
cUF := Eval( aGets[nPos][2]:bSetGet )
If !empty(cUF)
	cUrlQry += UrlEscape(cUF+',')
Endif

nPos := ascan(aGets , {|x| x[1] == "CEP" } )
cCep := alltrim(Eval( aGets[nPos][2]:bSetGet ))
If !empty(cCidade)
	cUrlQry += UrlEscape(cCEP)
Endif
          
If Empty(cUrlQry)
	MsgStop("Nao há dados preenchidos suficientes para a busca.")
	Return
Endif

shellExecute("Open", cMapsURL+cUrlQry, "", "", 1 )

Return

/* -----------------------------------------------------------
Funcao de Escape de caracteres básica para informar 
valores de campos via URL 
----------------------------------------------------------- */

STATIC Function UrlEscape(cInfo)
cInfo := strtran(cInfo,'%',"%25")
cInfo := strtran(cInfo,'&',"%26")
cInfo := strtran(cInfo," ","+")
cInfo := strtran(cInfo,'"',"%22")
cInfo := strtran(cInfo,'#',"%23")
cInfo := strtran(cInfo,",","%2C")
cInfo := strtran(cInfo,'<',"%3C")
cInfo := strtran(cInfo,'>',"%3E")
cInfo := strtran(cInfo,"|","%7C")
Return cInfo


/* --------------------------------------------------------------------
Cria uma janela WEB com o componente TWebEngine() no Smartclient
e abre uma janela no envio de e-mail do GMail
Observação: É necessário ter uma conta no GMail 
-------------------------------------------------------------------- */

STATIC Function SendGMail(cEMAIL)
Local cMailURL := 'https://mail.google.com/mail/?view=cm&fs=1&tf=1&to='
Local oDlgMail
Local oWebBrowse
Local cTitle := "Enviar eMail ("+cEMAIL+")" 

DEFINE DIALOG oDlgMail TITLE (cTitle) ;
	FROM 0,0 TO 600,800 PIXEL

oWebBrowse := TWebEngine():New(oDlgMail, 0, 0, 100, 100)
oWebBrowse:Align := CONTROL_ALIGN_ALLCLIENT
oWebBrowse:Navigate(cMailURL+lower(cEMAIL))

ACTIVATE DIALOG oDlgMail CENTER 

Return


STATIC Function ShowImg(oBmpFoto)
Local cTmpPath
Local nH
Local cRAWImage 
Local cId 

If !Agenda->(Eof())
	cRAWImage := AGENDA->IMAGE
	cId := AGENDA->ID
Endif

If empty(cRawImage)
	
	// Carrega a imagem default 
	oBmpFoto:Load(,"\temp\tmp_photo_3x4.img")

Else
	
	// Carrega a imagem do contato
	// fazendo cache em disco na pasta TEMP 
	
	cTmpPath := "\temp\tmp_"
	cTmpPath += cID
	cTmpPath += ".img"
	
	if !file(cTmpPath)
		nH := fCreate(cTmpPath)
		fWrite(nH,cRawImage)
		fclose(nH)
	Endif
	
	oBmpFoto:Load(,cTmpPath)
	
Endif


Return     


STATIC Function OpenDB()

// Conecta com o DBAccess configurado no ambiente
nH := TCLink()

If nH < 0
	MsgStop("DBAccess - Erro de conexao "+cValToChar(nH))
	QUIT
Endif

Return


STATIC Function RAW3x4()
Local cRaw := ''
cRaw += HEx2Bin('89504E470D0A1A0A0000000D4948445200000078000000A00802000000486B3FC700000001735247')
cRaw += HEx2Bin('4200AECE1CE90000000467414D410000B18F0BFC61050000000970485973000012740000127401DE')
cRaw += HEx2Bin('661F78000003D749444154785EEDDC6D5AEA30140061F7D3F5B01FD7C37AD88F3760494E3E8A14CB')
cRaw += HEx2Bin('542E33BF34C4206F43451FEAC79721090D253494D0504243090D253494D0504243090D253494D050')
cRaw += HEx2Bin('4243090D253494D0504243090D253494D0504243090D253494D0504243090D253494D0504243090D')
cRaw += HEx2Bin('253494D0504243090D253494D0504243090D253494D0504243090D253494D0504243ED097D3A1E3E')
cRaw += HEx2Bin('BE9B3E4FF3D87FDB33A0B35F6C9AA6C3E7B1F23C7D4ED71B857EA0E2372A983E0DBA2C7C38CE43BB')
cRaw += HEx2Bin('8743A7AE8F5EE8DF15A0F3E33C1DA3FE3C2CF4EF1A41A7C2F0EC5A43A71F8D53FEBC3D9DCF9DD201')
cRaw += HEx2Bin('2BB352DF27FE3075F8F321558BFFBCCEE671D0C1A0874ECD1F96DADD980EC47C4BD794E72E4187E7')
cRaw += HEx2Bin('CC5DEB6CDEDFD8D1E302CE8F73AF77F5D38EBE779DCDC3CED1611B5D01EB079DF50B55910E7E69D7')
cRaw += HEx2Bin('5D4787ABA6CAC22DDBBA75B6ECB9D0C38A7E99181F5D19CEA3DD5927375C63117AE53A5B4643C747')
cRaw += HEx2Bin('31103DD70F2F4CBC14EE6D70046BE8B5EB6C19053DF8C570E971F7C36523F60623A03256CF5FBBCE')
cRaw += HEx2Bin('96713F0CFB7AD14BFDF0F2337EBC48195B82BE6F9D2D7B09E8B062AB30BCB332587F036BD7D9B297')
cRaw += HEx2Bin('808E4BA6C1F1AB85705FD57770993CFF2EB2729D2D7B0DE8F8AC1F57EFD07EF6F5F675EB6CD8AB40')
cRaw += HEx2Bin('9F89F2B26DFD2F749D67596AD53ADBF554E8F4EC9CC786A5676C9E18F7D1C270FA82157FA2389F0E')
cRaw += HEx2Bin('F2CC342D4EDAE14F1DCF80B6514243090D253494D0504243090D253494D0504243090D253494D050')
cRaw += HEx2Bin('4243090D253494D0504243090D253494D0504243090D253494D0504243090D253494D0504243ED07')
cRaw += HEx2Bin('7D7EEBECA1BA6276703DD123D56F806EDFFABB573B41DF7AE3FDEFDE0DDEAEFCDED0E1A280518F53')
cRaw += HEx2Bin('F70750E874A2086FB2AFE91F940E970AE40B9DDFFCD43128525FA1CB068D97979499D501A98683B9')
cRaw += HEx2Bin('D07501BAE08C06C7868DBED0C3D22B900CDAECD3B2A9E7F13C10A7759B5CE85CD8ACA1E6FAA954BD')
cRaw += HEx2Bin('A9F36711B04CC9A342E746D0E357D26153E7E2761EA10A9D1BEFE84B1D4E4B1D270CF7B8D00B9DAA')
cRaw += HEx2Bin('CBB17B9E463AECE71BC72BB43FF71F81BE14352B991EB3DC2CF4FA16A00365F8D348DED4ED6965D8')
cRaw += HEx2Bin('9B429FE99AABAFD3CB8978EE08300532D946F49B76F9ABF627FE6E37E89B957370993A8B850D1C5F')
cRaw += HEx2Bin('79B4097DE9E6D33DD084239255EFDBD442CFCDFFC921EEECFE7F3994E311B9EE9216FA5D131A4A68')
cRaw += HEx2Bin('28A1A18486121A4A6828A1A18486121A4A6828A1A18486121A4A6828A1A18486121A4A6828A1A184')
cRaw += HEx2Bin('86121A4A6828A1A18486121A4A6828A1A18486121A4A6828A1A18486121A4A6828A1A18486121A4A')
cRaw += HEx2Bin('6828A1A18486121A4A6828A1A18486121A4A6828A1A18486121AE9EBEB1FDCCEA468FA802AA60000')
cRaw += HEx2Bin('000049454E44AE426082')

Return cRaw



STATIC function HEx2Bin(cHex)
Local cBin := ''
For nI := 1 to len(cHex) STEP 2
	cBin += chr(__HEXTODEC(substr(cHex,nI,2)))
Next
Return cBin


STATIC Function ChgImage(oDlg,aBtns,aGets,oBmpFoto)
Local cTitle := 'Escolha uma imagem'
Local oDlgImg
Local cFile := space(40)
Local lOk := .F.
Local aFInfo

DEFINE DIALOG oDlgImg TITLE (cTitle) ;
	FROM 0,0 TO 120,415 PIXEL;
	FONT oDlg:oFont ;
	OF oDlg ; 
	COLOR CLR_BLACK, CLR_HBLUE

@ 05,05 GET oGet1 VAR cFile  SIZE CALCSIZEGET(40),12 OF oDlgImg PIXEL

@ 25,05 BUTTON oBtn1 PROMPT "Buscar" SIZE 60,15 ;
	ACTION (BuscaFile(@cFile)) OF oDlgImg PIXEL

@ 25,85 BUTTON oBtn2 PROMPT "Ok" SIZE 60,15 ;
    WHEN File(alltrim(cFile)) ; 
	ACTION ( lOk := .T. , oDlgImg:End() ) OF oDlgImg PIXEL

ACTIVATE DIALOG oDlgImg CENTER

IF lOk

	cFile := alltrim(cFile)
	aFInfo := Directory(cFile)
	If len(aFInfo) > 0 
		If aFInfo[1][2] > ( 128 * 1024) // Até 128 KB
			MsgStop("Arquivo muito grande ("+str(aFInfo[1][2]/2014,8,2)+" KB)","Imagem maior que 128 KB")
			return 
		Endif
	Else
		MsgStop('Arquivo não encontrado',cFile)
		return 
	Endif

	// Chegou ate aqui, atualiza o campo memo 
	
	cMemoImg := AGENDA->IMAGE
	
	If !empty(cMemoImg)
		lOk := MsgYEsNo("Este contato já tem uma foto. Deseja substituí-la ?")
	Endif

	If lOk

		// Lê a imagem do disco 			                         
		cMemoImg := ReadFile(cFile)
		
		If empty(cMemoImg)
			Return
		Endif

		DBSelectArea("AGENDA")

		If DbrLock(recno())

			AGENDA->IMAGE := cMemoImg

			DBRUnlock()
			
			MsgInfo("Imagem atualizada.")

			// Limpa ultima imagem desse ID do cache temporário 			
			CleanImg(AGENDA->ID)

			// Carrega a imagem default para limpar 
			// o cache do componente de imagem 
			oBmpFoto:Load(,"\temp\tmp_photo_3x4.img")

			// Agora Mostra a nova imagem do contato 	
			ShowImg(oBmpFoto)

		Else
			
			// Nao conseguiu bloqueio do registro
			// Mostra a mensagem e permanece no modo de alteração
			MsgStop("Registro não pode ser alterado, está sendo usado por outro usuário")
			
		Endif
		
	Endif

Endif

Return


STATIC Function BuscaFile(cFile)
Local cRet

cRet := cGetFile( 'Imagens PNG|*.png|Imagens JPEG|*.jpg|Imagens BITMAP|*.bmp' , ;
       'Imagens', 1, 'C:\', .F., 1  ,.T., .T. )

If !empty(cRet) 
	cFile := padr(cRet,len(cFile))
Endif

return 


STATIC Function ReadFile(cFile)
Local cBuffer := ''
Local nH , nTam
nH := Fopen(cFile)
IF nH != -1
	nTam := fSeek(nH,0,2)
	fSeek(nH,0)
	cBuffer := space(nTam)
	fRead(nH,@cBuffer,nTam)
	fClose(nH)
Else
	MsgStop("Falha na abertura do arquivo ["+cFile+"]","FERROR "+cValToChar(Ferror()))
Endif
Return cBuffer


STATIC Function ShowObj(oObj)
Local aInfo := ClassDataArr(oObj,.T.)
Local ni
For nI := 1 to len(aInfo)
	conout(aInfo[nI][1]+' = '+cValToChar(aInfo[nI][2]))
Next
Return

STATIC Function CleanImg(cID)
Local cTmpPath
cTmpPath := "\temp\tmp_"
cTmpPath += cID
cTmpPath += ".img"
ferase(cTmpPath)
return

