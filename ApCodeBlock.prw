#include 'protheus.ch'

/* ======================================================================
Função      U_OLA01 .... U_OLA05
Autor       Júlio Wittwer
Data        12/12/2014
Descrição   Fonte de teste e demonstração de uso de CodeBlocks

Post relacionado : https://siga0984.wordpress.com/2014/12/12/codeblocks-em-advpl-parte-01/
====================================================================== */

User Function Ola01()
Local bCB := {|| MsgInfo("Olá CodeBlock") }
Eval(bCB)
Return


User Function Ola02()
Local bCB := {| cMsg | MsgInfo(cMsg) }
Eval(bCB,"Olá CodeBlock")
Eval(bCB,"Adeus CodeBlock")
Return


User Function Ola03()
Local bCB := {| x1,x2 | IIF( X1 < x2 , x1 , x2 ) }
Local nRet
Local cRet
nRet := Eval(bCB,10,5)
MsgInfo( cValToChar(nRet),"Número")
cRet := Eval(bCB,"ABC","ACD")
MsgInfo( cRet,"String")
Return


User Function Ola04()
Local bCB := {| x,y,z | y := x/2 , z := x*2 , x%2 == 0 }
Local nTeste := 4
Local lPar
Local nY , nZ
// O bloco de codigo recebe em x o valor de nTeste
// e recebe em y e z a referência das variáveis 
// nY e nZ respectivamente
lPar := Eval( bCB , nTeste , @nY , @nZ )
MsgInfo(lPar , "O numero é par ? ")
MsgInfo(nY , "Numero / 2 ")
MsgInfo(nZ , "Numero * 2 ")
Return


User Function Ola05()
Local aValores := {}
Local nSoma := 0
Local bSoma := {| nValor , nPos | nSoma += nValor }
aadd(aValores,3)
aadd(aValores,4)
aadd(aValores,3)
aadd(aValores,10)
aEval(aValores,bSoma)
MsgInfo(nSoma,"Valores Somados")
Return


