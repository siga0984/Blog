
#include 'Protheus.ch'
#include 'zLibStr2HexDmp.ch'

// ---------------------------------------------------------------------------------
// Utiliza direto uma inistancia da classe ZMEMCACHED 
// para ler e gravar valores, testa todas as operações, expires e contador/sequenciador

#define TEST_HOST		'localhost'
#define TEST_PORT		11211

User Function MemTst1()

Local oClient
Local cVersion := ""
Local aStats := {}
Local nI , nX
Local xValue 
Local nNewValue
Local aMeuArray

oClient := ZMEMCACHED():New( TEST_HOST , TEST_PORT )

// Modo verbose apenas para depuração 
// oClient:lVerbose := .t.

IF !oClient:Connect()
   conout("Falha de conexão...")
   conout(oClient:GetErrorStr())
   return
Endif

// Recupera a versao da instancia atual
If !oClient:GetVersion(@cVersion)
   conout("Falha ao recuperar versao...")
   conout(oClient:GetErrorStr())
   return
endif

conout("Memcache Host ..........: ["+TEST_HOST+"]")
conout("Memcache Port ..........: ["+cValToChar(TEST_PORT)+"]")
conout("Memcache Version .......: ["+cVersion+"]")

// Pega as estatisticas da instancia atual 
If !oClient:GetStats(@aStats)
   conout("Falha ao recuperar estatisticas...")
   conout(oClient:GetErrorStr())
   return
endif

conout(padc(" CURRENT MEMCACHED STATISTICS ",79,"-"))
aEval(aStats , {|x| conout(x) })

// Apaga todas as chaves

If !oClient:Flush()
   conout("Falha no Flush")
   conout(oClient:GetErrorStr())
   return
endif

// Testando armazenamento de valores
cValue1 := RandomStr(64)
cValue2 := RandomStr(64)

// Acrescenta o valor 
// Fuciona apenas caso a chave nao exista 
If !oClient:Add( 'chave' , cValue1 )
	conout("Falha ao adicionar chave ...")
	conout(oClient:GetErrorStr())
	Return 
Endif

// Agora tenta acrescentar na mesma chave
// isso nao deveria ser possivel 
If oClient:Add( 'chave' , cValue1 )
	UserException("Permitiu adicionar valor de chave ja existente")
Endif

// Troca valor 
// apenas se a chave existe 

If !oClient:Replace( 'chave' , cValue2 )
	conout("Falha ao trocar chave ...")
	conout(oClient:GetErrorStr())
	Return
Endif

// Deleta a chave
If !oClient:Delete( 'chave')
	conout("Falha ao deletar chave ...")
	conout(oClient:GetErrorStr())
	Return
Endif

// agora tenta trocar o valor. 
// deveria falhar, pois a chave nao existe 
If oClient:Replace( 'chave' , cValue1 )
	UserException("Permitiu troca de valor de chave que nao existe")
Endif

// Acrescenta o valor de novo 
// Deve funcionar, pois a chave tinha sido deletada
If !oClient:Add( 'chave' , cValue1 )
	conout("Falha ao adicionar chave ...")
	conout(oClient:GetErrorStr())
	Return 
Endif

// Mostra no console o valor graavdo 
conout(padc(" STORED VALUE ",79,"-"))
Conout(Str2HexDmp(cValue1))

// Agora le o valor da chave
lOk := oClient:Get('chave' , @xValue )

If !lOk
	conout("Falha ao ler a chave ...")
	conout(oClient:GetErrorStr())
	Return 
Endif

conout(padc(" READED VALUE ",79,"-"))
Conout(Str2HexDmp(xValue))

If ! (xValue == cValue1 ) 
	UserException("Divergencia de valor")
Endif

// Grava um array na chave 
lOk := oClient:Set('meuarray' , { 123, "ABC" , date() , NIL } )

If !lOk
	conout("Falha ao gravar um array  ...")
	conout(oClient:GetErrorStr())
	Return 
Endif

// Agora lê 
aMeuArray := {}

lOk := oClient:Get('meuarray' , @aMeuArray)

If !lOk
	conout("Falha ao ler um array  ...")
	conout(oClient:GetErrorStr())
	Return 
Endif

conout(replicate("-",79))
aEval(aMeuArray,{|x,y| conout("["+cValToChar(y)+"] => "+valtype(x)+" ["+cValToChar(x)+"]" )})
conout(replicate("-",79))

// busca uma chave que nao existe 
lOk := oClient:Get('naoexiste' , @xValue )

If !lOk
	conout("Retorno inesperado")
	conout(oClient:GetErrorStr())
	Return 
Endif
             
// Cria um contador para incremento
// Ja inicializado com um valor

If !oClient:Add( 'contador' , '666' )
	conout("Falha ao adicionar contador ...")
	conout(oClient:GetErrorStr())
	Return 
Endif


// Agora testa o incremento 
nNewValue := 0 

If !oClient:Increment( 'contador' , @nNewValue )
	conout("Falha ao incrementar contador ...")
	conout(oClient:GetErrorStr())
	Return 
Endif

conout("nNewValue = "+cValToChaR(nNewValue))

If nNewValue != 667
	UserException("Incr Failed - Expected 667 Reveived "+cvaltochar(nNewValue))
Endif

If !oClient:Decrement( 'contador' , @nNewValue )
	conout("Falha ao incrementar contador ...")
	conout(oClient:GetErrorStr())
	Return 
Endif

conout("nNewValue = "+cValToChaR(nNewValue))

If nNewValue != 666
	UserException("Decr Failed - Expected 667 Reveived "+cvaltochar(nNewValue))
Endif

// Agora incrementa um contador que nao existe 
If oClient:Increment( 'contador2' , @nNewValue )
	UserException("Nao deveria incrementar algo que nao existe")
Else
	Conout("-- Falha esperada -- contador realmente nao existe ")
Endif

// teste de valor com timeout                       
// expira em (aproximadamente) 2 segundos

If !oClient:Add( 'timer' , 'teste' , 2 )
	conout("Falha ao adicionar contador ...")
	conout(oClient:GetErrorStr())
	Return 
Endif

// le o valor 4 vezes
// em intervalos de 1 segundo 
// A partir da terceira leitura o valor 
// já nao deveria existir 

For nX := 1 to 4

	// le direto o valor 
	lOk := oClient:Get('timer' , @xValue )

	conout(padc(" GET VALUE ",79,"-"))

	If xValue = NIL 
		conout("--- NIL --- ")	
	Else
		Conout(Str2HexDmp(xValue))
	Endif

	If !lOk
		conout("Falha ao ler a chave ...")
		conout(oClient:GetErrorStr())
		Return 
	Endif

	Sleep(1000)

Next

// Pega as estatisticas no fional do teste
If !oClient:GetStats(@aStats)
   conout("Falha ao recuperar estatisticas...")
   conout(oClient:GetErrorStr())
   return
endif

conout(padc(" MEMCACHED STATSISTICS ",79,"-"))
aEval(aStats , {|x| conout(x) })

oClient:Disconnect()

FreeObj(oClient)

return


// ---------------------------------------------------------------------------------
// Funcao RandomStr()
// Gera uma string com o tamanho espeficicado, sorteando caracteres 
// ASICI da faixa de 32 a 127, contemplando letras, numeros e simbolos 

STATIC Function RandomStr(nSize)
Local cRet := ''
While nSize>0
	cRet += chr(randomize(32,128))
	nSize--
enddo
Return cRet

