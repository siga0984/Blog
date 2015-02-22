#include 'protheus.ch'

/* ----------------------------------------------------------
Classe    APGlbQueue
Autor     Júlio Wittwer
Data      08/02/2015
Desrição  Classe para manipular fila global. Uma fila global em memoria
          usa um cotainer de memoria global do Advpl, visivel por qualquer
          thread no mesmo servidor de aplicação. A classe garante uma
          identificação unica por ambiente ( environment ) para permitir
          mais de uma fila com mesmo nome no mesmo servidor onde as threads
          estao sendo executadas em environments diferentes no mesmo servidor. 
      
          O mecanismo de Dequeue() permite time-out de espera caso a lista
          esteja vazia, e caso seja feito um Enqueue() enquanto o processo 
          está em espera, a notificação e processamento são feitos diretamente
          via IpcGo() / IpcWaitEx() -- comunicação nomeada entre processos, e
          pode haver mais de um processo de Enqueue() e Dequeue() rodando ao mesmo
          tempo, pois o acesso à fila global é semaforizada.

Post relacionado

https://siga0984.wordpress.com/2015/02/17/escalabilidade-e-performance-fila-com-job/

Lista de Erros dos Métodos

-1 Nao foi possivel obter lock global 
-2 Nao existem itens na fila 
-3 Limite de requisicoes na fila atingido 

Referencias

http://tdn.totvs.com/display/tec/GlbLock
http://tdn.totvs.com/display/tec/GlbUnlock
http://tdn.totvs.com/display/tec/GetGlbVars
http://tdn.totvs.com/display/tec/PutGlbVars
http://tdn.totvs.com/display/tec/IpcWaitEx
http://tdn.totvs.com/display/tec/IpcGo

---------------------------------------------------------- */

CLASS APGlbQueue
                     
  DATA cErrorStr
  DATA nErrorCode
  DATA cGlbListId
  DATA nMaxQueue
  DATA nEnqCnt
  DATA nDeqCnt

  // Metodos de uso publico 
  METHOD NEW( cQueueId , nMaxQueue )
  METHOD Enqueue( xRequest )
  METHOD Dequeue( xRequest, nWait )
  METHOD Clear()
  METHOD GetErrorStr()

  // Uso interno da classe
  METHOD ClearError() 
  METHOD SetError() 

ENDCLASS

/* ----------------------------------------------------------
Metodo New()
Inicializa uma fila com um identificador no servidor atual 
nMaxQueue = Tamanho maximo da fila. Default = 0 ( sem limite ) 
cQueueId = Identificador globa da fila de elementos
Caso a fila já exista, ela não é limpa. 
---------------------------------------------------------- */
                                       
METHOD NEW( cQueueId , nMaxQueue ) CLASS APGlbQueue

If nMaxQueue == NIL
  // Default = sem limite
  nMaxQueue := 0
Endif

// Inicializa propriedades das classes

::cErrorStr := ''
::nErrorCode := 0
::nMaxQueue  := nMaxQueue                     
::nEnqCnt := 0
::nDeqCnt := 0 

// O identificador global para a lista na memoria usa um prefixo 
// exclusivo e o nome do ambiente, para permitir que mais de 
// um ambiente configurado no mesmo servidor tenha filas independentes
::cGlbListId   := '_GLBQLIST_'+Upper(GetEnvServer()) +'_'+ Upper(cQueueId)

Return self


/* ----------------------------------------------------------
Metodo Enqueue()
Acrescenta um item na fila de processamento
Caso exista um ou mais processos esperando uma notificação
um dos processos em espera arbitrariamente será notificado na hora
Retorna o numero de elementos na fila apos acrescentar o elemento informado
Em caso de falha na inserção, retorna um numero menor que zero.
---------------------------------------------------------- */

METHOD Enqueue( xRequest ) CLASS APGlbQueue
Local aItens := {}

::ClearError()

If IpcGo(::cGlbListId,xRequest)

  // Antes de tentar usar a fila, tenta enviar direto a notificação 
  // caso tenha conseguido, tinha algum processo aguardando
  // no dequeue(). Neste caso, retorna 0
  Return 0

Endif

// Se nao conseguiu fazer ipcgo(), nao tinha ninguem em espera
// neste caso, vai usar a lista global de espera

If !GlbLockRetry()
  
  // Nao conseguiu o lock global ...
  Return ::SetError(-1,'Unable to obtain Global Lock')
  
Endif

// Recupera fila global na memoria
GetGlbVars(::cGlbListId,@aItens)

If ::nMaxQueue > 0 .and. Len(aItens) >= ::nMaxQueue
  // Caso exista limite de itens em espera na fila
  // e o limite tenha sido atingido, nao permite 
  // acrescentar mais elementos.
  GlbUnlock()
  Return ::SetError(-3,'Too Many Requests. Queue Full.')
Endif

// Acrescenta o item na fila
Aadd(aItens,xRequest)

// Incrementa contador de itens acrescentados
::nEnqCnt++

// Salva a fila na variavel global novamente
PutGlbVars(::cGlbListId,aItens)

// Solta o lock global
GlbUnlock()

// Mostra no log de console quantos itens tem na fila
conout("Thread ["+cValToChar(ThreadId())+"] Enqueue("+cValToChar(xRequest)+")")
conout("Thread ["+cValToChar(ThreadId())+"] Queue Size = "+cValToChar(len(aItens)))

// Retorna a quantidade de itens na lista agora
Return len(aItens)

/* --------------------------------------------------------
Recupera e remove o primeiro item da lista. Caso nao haja ninguem
na lista, espera por um tempo  determinado em milissegundos pela
notificação de insert O tempo default são 5000 ms ( 5 segundos )
xRequest deve ser informado por referência. Em caso de sucesso,
retorna o numero de requisicoes que ainda estao na fila.
Quando for retirado o ultimo  elemento da fila, será retornado zero.
Em caso de falha na inserção, retorna um numero menor que zero
-------------------------------------------------------- */

METHOD Dequeue( xRequest, nWait ) CLASS APGlbQueue
Local aItens := {}

::ClearError()

xRequest := NIL

If nWait == NIL
  // Time-out de espera default = 5 segundos
  nWait := 5000
ElseIf nWait < 100
  // Time-out muito baixo, nao premite tempo menor que 1/10 de segundo
  // desse modo, também nao permite time-out zero ( esperar pra sempre ) 
  nWait := 100
Endif

If !GlbLockRetry()
  // Nao conseguiu o lock global ...
  Return ::SetError(-1,'Unable to obtain Global Lock')
Endif

// Os itens da fila estao em uma variavel global.
// Realiza um lock global neste serviço, e pega os itens da fila
GetGlbVars(::cGlbListId,@aItens)

If Len(aItens) > 0
  
  // Se existem itens na fila, pega o primeiro
  // da fila, remove da lista de pendencias
  // e atualiza a lista
  xRequest := aItens[1]
  aDel(aItens,1)
  aSize(aItens,len(aItens)-1)
  PutGlbVars(::cGlbListId,aItens)
  
  // Solta o lock global
  GlbUnlock()
  
  // E Retorna o numero de itens restantes na fila
  Return Len(aItens)
  
Endif

// Se chegou aqui, a lista esta vazia
// Solta o lock global
GlbUnlock()

// Se chegou aqui, não tem itens na fila 
// Aguarda por uma requisição de Enqueue()
// pelo time-out especificado.

If IpcWaitEx(::cGlbListId,nWait,@xRequest)
  
  // Caso tenha recebido, 
  // pega o item direto, nem usa a fila
  // retorna 0 para indicar que pegou uma requisicao
  // e que a fila esta vazia 
  Return 0
  
Endif

// Apos esperar ... se nao recebeu nada 
// Retorna que nao pegou nada, e não tem elementos na fila
Return ::SetError(-2,'Queue List Empty')


/* --------------------------------------------------------
Bloqueia e limpa todos os elementos da lista global
Retorna 0 em caso de sucesso, -1 caso nao consiga lock global
-------------------------------------------------------- */

Method Clear() CLASS APGlbQueue

::ClearError()

If !GlbLockRetry()
  // Nao conseguiu o lock global ...
  Return ::SetError(-1,'Unable to obtain Global Lock')
Endif

// Salva uma fila vazia na global
PutGlbVars(::cGlbListId, {} )

// Solta o lock global
GlbUnlock()

Return 0

/* ----------------------------------------------------
-- Uso interno da classe -- 
Limpa propriedades de registro de erro da classe
---------------------------------------------------- */

Method ClearError() CLASS APGlbQueue
::nErrorCode := 0
::cErrorStr := ''
Return

/* ----------------------------------------------------
-- Uso interno da classe -- 
Seta codigo e string de ocorrencia erro da classe
---------------------------------------------------- */

METHOD SetError(nCode,cErrorMsg) CLASS APGlbQueue
::nErrorCode := nCode
::cErrorStr := cErrorMsg
Return nCode

/* ----------------------------------------------------
Retorna string contendo codigo e descrição do ultimo erro 
---------------------------------------------------- */

METHOD GetErrorStr() CLASS APGlbQueue
Return '('+cValToChar(::nErrorCode)+') '+ ::cErrorStr







/* --------------------------------------------------------------
Funcao Auxiliar GlbLockRetry()

Espera por flag de acesso exclusivo no lock global
Caso o lock global nao esteja disponivel, tenta
novamente 50 vezes em intervalos randomicos 
de 15 a 25 milissegundos ( aprox 1 segundo ), e 
retorna .F. se nao conseguiu um lock global 
A reprodução desta ocorencia indica uma situação de 
stress de concorrencia pelo lock global ( muita 
manutenção na lista ao mesmo tempo ) 
-------------------------------------------------------------- */

STATIC Function GlbLockRetry()

Local lOk := .F.
Local nLoops := 50
Local nCount := 0

If !GlbLock()
  While ( !GlbLock() )
    nCount++
    If nCount > nLoops 
      Return .F.
    Endif
    Sleep( randomize (15,26) )
  Enddo
Endif

Return .T.

