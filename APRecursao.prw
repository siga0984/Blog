#include 'protheus.ch'

/* ======================================================================
Função      U_TESTEFAT e U_TESTEDIR
Autor       Júlio Wittwer
Data        14/01/2015
Descrição   Fonte de teste e demonstração de recursao e percorrer
            uma estrutura de árvore em AdvPL

Post relacionado : https://siga0984.wordpress.com/2015/01/14/funcoes-recursivas-em-advpl/
====================================================================== */


User Function TesteFat()

msginfo(Fatorial(4) , "Resultado do Fatorial - Com recursao")
msginfo(Fatorial2(4) , "Resultado do Fatorial - Sem recursao")

Return

STATIC Function Fatorial( nNum )
Local nResult
If nNum > 1
  nResult := nNum * Fatorial( nNum - 1 ) 
Else
  nResult := 1
Endif
Return nResult

STATIC Function Fatorial2( nNumero )
Local nRetorno := nNumero
Local nI
For nI := nNumero - 1 to 1 STEP -1
  nRetorno := nRetorno * nNumero 
Next
Return nRetorno


User Function TesteDir()

MsgInfo( TamDir('\') , 'Tamanho do RootPath - com recursao' )
MsgInfo( TamDir2('\') , 'Tamanho do RootPath - sem recursao' )

Return


STATIC Function TamDir( cPath )
Local aFiles := Directory(cPath+'*.*')
Local aSubDir := Directory(cPath+'*.*','D')
Local nI
Local nSize := 0
// Acumula o tamanho dos arquivos desta pasta
For nI := 1 to len(aFiles)
  nSize += aFiles[nI][2]
Next
// Acumula o tamanho das pastas a partir desta, caso existam
// Ignora os diretorios "." ( atual ) e ".." ( anterior ) 
For nI := 1 to len(aSubDir)
  If aSubDir[nI][1] != "." .and. aSubDir[nI][1] != ".."
    nSize += TamDir( cPath + aSubDir[nI][1] + '\' )
  Endif
Next
Return nSize



STATIC Function TamDir2( cPath )
Local aFiles := {}
Local aSubDir := {}
Local aVerify := {}
Local nI
Local nSize := 0
// Acrescenta a pasta atual como pendencia
aadd( aVerify , cPath )
While len(aVerify) > 0
 
  // Loop de processamento, enquanto houver subdiretorios
  // pendentes a serem verificados
 
  // Pega o path da ultima pendencia
  cPath := aTail(aVerify)
 
  // remove a ultima pendencia encolhendo o array
  aSize( aVerify , len(aVerify)-1 )
 
  // Identifica os arquivos da pasta
  aFiles := Directory(cPath+'*.*')
 
  // Acumula o tamanho dos arquivos desta pasta
  For nI := 1 to len(aFiles)
    nSize += aFiles[nI][2]
  Next
 
  // Verifica os sub-diretorios a partir desta pasta
  aSubDir := Directory(cPath+'*.*','D')
 
  // Acrescenta os sub-diretorios como pendencias
  For nI := 1 to len(aSubDir)
    If aSubDir[nI][1] != "." .and. aSubDir[nI][1] != ".."
      aAdd( aVerify , cPath + aSubDir[nI][1] + '\' )
    Endif
  Next
 
Enddo
Return nSize

