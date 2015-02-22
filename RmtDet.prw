#include 'protheus.ch'

/* ======================================================================
Função    U_RmtDet()
Autor     Júlio Wittwer
Data      01/02/2015
Descrição Monta uma caixa de diálogo com todas as informações possíveis
          de se obter do SmartClient que iniciou este programa.

Post relacionado: https://siga0984.wordpress.com/2015/02/02/interface-visual-do-advpl-smartclient/
====================================================================== */

User Function RmtDet()
Local oDlg
Local nRmtType
Local cRmtType
Local cRmtLib
Local oFont
Local cInfo := ''

// Habilita interface com data mostrada com 4 digitos no ano
// e Habilita data em formato britânico ( Dia/Mes/Ano )
SET CENTURY ON
SET DATE BRITISH

cRmtLib := ''
nRmtType := GetRemoteType ( @cRmtLib )
DO CASE
  CASE nRmtType == -1
    UserException("Invalid JOB/BLIND call to U_RmtDet")
  CASE nRmtType == 0
    cRmtType := 'SmartClient Delphi'
  CASE nRmtType == 1
    cRmtType := 'SmartClient QT Windows'
  CASE nRmtType == 2
    cRmtType := 'SmartClient QT Linux/Mac'
  CASE nRmtType == 5 
    cRmtType := 'SmartClient HTML'
  OTHERWISE
    cRmtType := 'Remote Type '+cValToChar(nRmtType)
ENDCASE
If !empty(cRmtLib)
  cRmtType += ' ('+cRmtLib+')'
Endif

// Usa uma fonte mono-espaçada 
DEFINE FONT oFont NAME 'Courier New'
// Cria uma caixa de diálogo com área util de 640x480 PIXELs
DEFINE DIALOG oDlg TITLE (cRmtType) FROM 0,0 TO 480,640 PIXEL

// Informações da Interface remota
cRmtBuild := GetBuild(.T.)
cRmtIp := GetclientIP()
cUsrName := LogUserName()
dRmtDate := GetRmtDate()
cRmtTime := GetRmtTime()
aRmtInfo := GetRmtInfo()
cRmtTmp := GetTempPath(.T.)
lActivex := IsPlugin()
lSSLConn := IsSecure()

cInfo += 'SmartClient Build ....... ' + cRmtBuild + CRLF
cInfo += 'SmartClient Activex ..... ' + IIF(lActivex,"SIM","NAO") + CRLF 
cInfo += 'SmartClient Connection .. ' + IIF(lSSLConn ,"SSL","TCP") + CRLF 
cInfo += 'Remote IP ............... ' + cRmtIp + CRLF 
cInfo += 'Remote User Name ........ ' + cUsrName + CRLF 
cInfo += 'Remote DateTime ......... ' + dtoc(dRmtDate)+' '+cRmtTime + CRLF
cInfo += 'Remote Temp Path ........ ' + cRmtTmp + CRLF
cInfo += 'Remote Computer Name .... ' + aRmtInfo[1] + CRLF
cInfo += 'Remote O.S. ............. ' + aRmtInfo[2] + CRLF
cInfo += 'Remote O.S. Detais ...... ' + aRmtInfo[3] + CRLF
cInfo += 'Remote Memory (MB) ...... ' + aRmtInfo[4] + CRLF
cInfo += 'Remote CPU Count ........ ' + aRmtInfo[5] + CRLF
cInfo += 'Remote CPU MHZ .......... ' + aRmtInfo[6] + CRLF
cInfo += 'Remote CPU String ....... ' + aRmtInfo[7] + CRLF
cInfo += 'Remote O.S. Language .... ' + aRmtInfo[8] + CRLF
cInfo += 'Remote Web Browser ...... ' + aRmtInfo[9] + CRLF

// Coloca a string com os dados dentro de um Get Multiline
@ 5,5 GET oGet1 VAR cInfo MULTILINE FONT oFont SIZE 310,230 OF oDlg PIXEL

ACTIVATE DIALOG oDlg CENTER

Return
