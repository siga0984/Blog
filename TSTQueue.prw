#include 'protheus.ch'

/* ===================================================================
Função     U_TSTQueue
Autor      Júlio Wittwer
Data       08/02/2015
Desrição   Teste de fila global com processamento 
           das requisições em JOB

Post relacionado

https://siga0984.wordpress.com/2015/02/17/escalabilidade-e-performance-fila-com-job/

Configuração de onstart para o Job de processamento

[ONSTART]
Jobs=JOBFILA01

[JOBFILA01]
Environment=NOMEDOSEUAMBIENTE
main=U_FILAPROC
nParms=1
Parm1=FILA01
=================================================================== */

User Function TSTQueue()
Local oDlg
Local oButton1
Local oSay1
Local oSay2
Local oQueue
Local nStatus
Local cMsg1 := space(40)
Local cMsg2 := space(40)
Local cMsg3 := space(40)
 
// Cria o objeto da fila global 
// Informa o nome da fila global e 
// a quantidade máxima de elementos
oQueue := APGlbQueue():New( "FILA01", 10 )
DEFINE DIALOG oDlg TITLE "Cliente da Fila" FROM 0,0 TO 160,300 PIXEL
// Botao para acrescentar na fila uma requisicao de processamento
@ 10,10 BUTTON oButton1 PROMPT "&Inserir requisição" ;
  ACTION ( InsertReq(oQueue,oSay1,oSay2) ) ;
  SIZE 080, 013 of oDlg PIXEL
@ 30,10 SAY oSay1 VAR cMsg1 SIZE 080, 013 of oDlg PIXEL
@ 40,10 SAY oSay2 VAR cMsg2 SIZE 080, 013 of oDlg PIXEL
ACTIVATE DIALOG oDlg CENTER ;
  VALID ( MsgYesNo("Deseja encerrar a aplicação ?") )
Return

/* ----------------------------------------------------------------
Inserção de requisição na fila, disparada pelo botão de interface
Atualiza valor na tela com o total de requisicoes enviadas 
---------------------------------------------------------------- */
STATIC Function InsertReq(oQueue,oSay1,oSay2)
Local nValue 
Local nStatus
// Requisicao fixa, 5 segundos ( 5000 ms ) 
nValue := 5000
nStatus := oQueue:Enqueue( nValue )
If nStatus < 0 
  // Falha ao inserir na fila
  MsgStop(oQueue:GetErrorStr(),"Falha ao acrescentar elemento na fila")
Else
  // Inseriu com sucesso, atualiza informaçoes na tela
  oSay1:SetText("Requisições inseridas ... "+cValToChar(oQueue:nEnqCnt))
  oSay2:SetText("Itens na fila ........... "+cValToChar(nStatus))
endif
Return

/* --------------------------------------------------------------
JOB dedicado para retirar e processar elementos colocados na fila
Deve ser iniciado na subida do servidor, no [ONSTART] como um
job, informando como parametro o ID da Fila Global
------------------------------------------------------------- */
USER Function FILAPROC( cQueueId )
Local nStatus
Local oQueue
Local xRequest := NIL

// Coloca observação do processo para Protheus Monitor
PtInternal(1,"Job de Processamento - Fila "+cQueueId)

If empty(cQueueId)
 // Se esta funcao foi chamada sem o ID da Fila ... 
 UserException("Missing QueueId configuration for U_FILAPROC")
Endif
conout("["+dtos(date())+" "+time()+"][JOB] Thread ["+cValToChar(ThreadId())+"] Iniciado.")

// Cria a instância para manutenção da fila
oQueue := APGlbQueue():New( cQueueId )
While !KillApp() 
 
  // Loop de processamento permanece em execução
  // desde que esta thread nao tenha recebido
  // uma notificação de finalização 
 
  // Tenta remover o primeiro item da fila
  nStatus := oQueue:Dequeue( @xRequest )
 
  If ( nStatus >= 0 )
 
    conout("["+dtos(date())+" "+time()+"][JOB] Thread ["+cValToChar(ThreadId())+"] Tamanho da fila = "+cValToChar(nStatus))
 
    // Pegou o primeiro item da fila
    // e retornou o numero de itens pendentes
    // neste momento na fila
 
    // Informa no console que vai fazer um "Sleep"
    conout( "["+dtos(date())+" "+time()+"][JOB] Thread ["+cValToChar(ThreadId())+"] Processando ... " )
 
    // Aqui eu poderia chamar qqer coisa
    // neste exemplo será feito um sleep mesmo 
    Sleep(xRequest)
 
    conout( "["+dtos(date())+" "+time()+"][JOB] Thread ["+cValToChar(ThreadId())+"] Fim de processamento " )
  Else
 
    // Nao pegou nenhum item ... 
    // Falha de lock ou Fila vazia
    conout("["+dtos(date())+" "+time()+"][JOB] Thread ["+cValToChar(ThreadId())+"] "+oQueue:GetErrorStr())
  Endif
 
Enddo

conout("["+dtos(date())+" "+time()+"][JOB] Thread ["+cValToChar(ThreadId())+"] Finalizado.")

Return

