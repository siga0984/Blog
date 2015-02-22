#include "protheus.ch"

/* ======================================================================
Função      U_TSTDUP
Autor       Júlio Wittwer
Data        16/12/2014
Descrição   Fonte de teste e demonstração de desempenho de uso 
            de hash em AdvPL em comparação com busca sequencial em array
            para a busca de elementos duplicados em um array 

Post relacionado : https://siga0984.wordpress.com/2014/12/21/performance-e-escalabilidade-hash-map-parte-2/
====================================================================== */

User Function TSTDup()
Local aDados := {}
Local aCheck := {}
Local nI
Local nKey
Local aValues
Local nTotal1,nTotal2
Local nElements
Local oHash
nElements := 2

// Testa de 2 a 8192 elementos, dobrando a quantidade de elementos 
// a cada teste ( 2,4,8,16,32,64,128,256,512,1024,2048,4096,8192 )
conout("Resultado ( Elementos / Tempo com Ascan() / Tempo com Hash")
While nElements <= 8192
 
  // Zera os arrays para fazer cada teste
  aSize(aDados,0)
 
  // Cria um array de dados sem valores repetidos
  For nI := 1 to nElements
    aadd(aDados, { nI , "Teste "+cValToChar(nI) } )
  Next
 
  // método 1: Verificar por repetições com Ascan()
  aSize(aCheck,0)
  nTotal1 := seconds()
  For nI := 1 to nElements
    nKey := aDados[nI][1]
    // Busca usando ASCAN
    If ascan( aCheck , {|x| x[1] == nKey }) == 0
      // Nao achou, acrescenta o elemento
      // no array de elementos nao repetidos
      AADD(aCheck,aDados[nI])
    Endif
  Next
  nTotal1 := seconds() - nTotal1
 
  // método 2: Usando Hash Map
  aSize(aCheck,0)
  nTotal2 := seconds()
  oHash := HMNew() // Cria um hash map sem dados 
  For nI := 1 to nElements
    nKey := aDados[nI][1]
    // Busca no Hash Map ( usando chave numérica ) 
    If !hmGet(oHash,nKey,@aValues)
      // Nao achou, acrescenta o elemento no hash 
      // e no array de elementos nao repetidos
      HMAdd(oHash,aDados[nI])
      AADD(aCheck,aDados[nI])
    Endif
  Next
  nTotal2 := seconds() - nTotal2

  // Mostra o resutado deste teste ( elementos, tempo ascan, tempo hash
  conout(str(nElements,5)+" / "+str(nTotal1,6,3)+" s. / "+str(nTotal2,6,3)+" s.")

  // Dobra a quantidade de elementos para o próximo teste
  nElements := nElements * 2
 
Enddo
Return
