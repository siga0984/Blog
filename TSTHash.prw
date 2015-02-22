#include "protheus.ch"

/* ======================================================================
Função      U_TSTHASH
Autor       Júlio Wittwer
Data        16/12/2014
Descrição   Fonte de teste e demonstração de desempenho de uso 
            de hash em AdvPL em comparação com busca sequencial em array

Post relacionado : https://siga0984.wordpress.com/2014/12/16/performance-e-escalabilidade-hash-map-em-advpl/
====================================================================== */

USER Function TstHash()
Local aDados := {}
Local nI , nJ 
Local nTimer, nTotal
Local cBusca 
Local nPos , xValue
Local oHash

// Cria um array de duas colunas, com 17 elementos
aadd(aDados , { "BC","Branco" } )
aadd(aDados , { "AZ","Azul" } )
aadd(aDados , { "VM","Vermelho" } )
aadd(aDados , { "VD","Verde" } )
aadd(aDados , { "RX","Roxo" } )
aadd(aDados , { "AM","Amarelo" } )
aadd(aDados , { "MA","Marrom" } )
aadd(aDados , { "AM","Azul Marinho" } )
aadd(aDados , { "AC","Azul Céu" } )
aadd(aDados , { "AQ","Amarelo Queimado" } )
aadd(aDados , { "AT","Azul Turquesa" } )
aadd(aDados , { "SA","Salmão" } )
aadd(aDados , { "VO","Verde Oliva" } )
aadd(aDados , { "VI","Violeta" } )
aadd(aDados , { "GR","Cinza" } )
aadd(aDados , { "PB","Chumbo" } )
aadd(aDados , { "PT","Preto" } )

// Faz um teste de desempenho buscando pelos elementos usando ASCAN()
// Faz 50 mil buscas para cada um dos elementos cadastrados
// O ultimo loop busca por um elemento que nao existe
nTotal := seconds()
For nI := 1 to len(aDados)+1
  If nI > len(aDados)
    // Busca por um elemento que nao existe 
    cBusca := "NE"
  Else
    // Busca por um elemento que existe na lista 
    cBusca := aDados[nI][1]
  Endif
  nTimer := seconds()
  For nJ := 1 to 50000
    // Realiza 50 mil buscas 
    nPos := ascan( aDados , {|x| x[1] == cBusca })
  Next
  nTimer := seconds()-nTimer
  conout("Busca por ["+cBusca+"] demorou "+cValToChar(nTimer)+" s.")
Next
nTotal := seconds() - nTotal
conout("Tempo Total (ASCAN) = "+cValToChar(nTotal)+" s.")
conout("")

// Agora usando o HASH
nTotal := seconds()
// Cria o Objeto de HASH a partir do Array
oHash := aToHM(aDados)
For nI := 1 to len(aDados)+1
  If nI > len(aDados)
    // Busca por um elemento que nao existe 
    cBusca := "NE"
  Else
    // Busca por um elemento que existe na lista 
    cBusca := aDados[nI][1]
  Endif
  nTimer := seconds()
  For nJ := 1 to 50000
    // Realiza 50 mil buscas 
    lFound := HMGet( oHash , cBusca , @xValue )
  Next
  nTimer := seconds()-nTimer
  conout("Busca por ["+cBusca+"] demorou "+cValToChar(nTimer)+" s.")
Next

nTotal := seconds() - nTotal
conout("Tempo Total (HASH) = "+cValToChar(nTotal)+" s.")

Return


