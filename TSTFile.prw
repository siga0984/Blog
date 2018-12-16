#include 'protheus.ch'
#include 'fileio.ch' 

USER Function TSTFILE()
Local nHnd 
Local cFile := '\temp\teste.txt'
Local cLine , nTamFile
Local cNewLine , nRead, nWrote
            
// Cria o arquivo 
// Automaticamente o arquivo é aberto em modo exclusivo para gravação
nHnd := fCreate(cFile)

If nHnd == -1
  MsgStop("Falha ao criar arquivo ["+cFile+"]","FERROR "+cValToChar(fError()))
  Return
Endif

// Cria uma linha em memória
cLine := "Olá sistema de arquivos" + CRLF

// Grava três linhas iguais, com 25 bytes cada 
fWrite(nHnd,cLine)
fWrite(nHnd,cLine)
fWrite(nHnd,cLine)

// Fecha o arquivo 
fClose(nHnd)

// Abre novamente para leitura e ecrita em modo exclusivo 
nHnd := fOpen(cFile,FO_READWRITE + FO_EXCLUSIVE )
If nHnd == -1
   MsgStop("Falha ao abrir ["+cFile+"]","Ferror " + cValToChar(fError()) )
   Return
Endif

// Identifica o tamanho do arquivo 
nTamFile := fSeek(nHnd,0,FS_END)
fSeek(nHnd,0,FS_SET)

conout("Tamanho do Arquivo = "+cValToChaR(nTamFile)+" byte(s)." )
                
// Lê a primeira linha do arquivo 
cBuffer := ""
nRead := FRead( nHnd, @cBuffer, 25 )
conout("Byte(s) lido(s) : "+cValToChar(nRead))

If nRead < 25
	MsgStop("Falha na leitura da primeira linha.","Ferror "+cValToChar(ferror()))
	Return
Endif

// Agora vamos trocar a segunda linha 
cNewLine := replicate('-',23)

nWrote := fWrite(nHnd , cNewLine )
conout("Byte(s) gravado(s) : "+cValToChar(nWrote))

If nWrote < 23
	MsgStop("Falha na gravação da segunda linha.","Ferror "+cValToChar(ferror()))
	Return
Endif

// fecha o arquivo 
fClose(nHnd)

Return
