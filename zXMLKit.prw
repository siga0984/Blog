#include 'protheus.ch'

/*
Funcao 		U_zXMLKit()
Autor		Julio Wittwer
Data 		14/07/1029
Descrição 	Interface para teste de Parser de XML

			Realiza o parser de uma string ou arquivo XML, e levanta todas as informações dos nodes 
			que podem ser extraídas do XML usando DOM 
*/


User Function zXMLKit()
Local oDlg
Local oGetXML
Local cXML := ''
Local oXml                    
Local cXmlFile := space(80)              
Local nOpList
Local aNodeList := {''}
Local oListResult 

oXml := tXmlManager():New()

// Cria uma caixa de diálogo com área util de 800x600  PIXELs
DEFINE DIALOG oDlg TITLE "AdvPL XML Kit" FROM 0,0 TO 600,800 PIXEL

@ 0,0 FOLDER oFolders PROMPTS "XML String","XML File","Resultado"  OF oDlg  PIXEL
oFolders:ALIGN := CONTROL_ALIGN_ALLCLIENT

// Folder 1 . Parser da String informada

@ 10,10 SAY "Informe a String XML para Parser" OF oFolders:aDialogs[1] PIXEL
@ 20,10 GET oGetXML VAR cXML MULTILINE SIZE 380, 200 of oFolders:aDialogs[1]  PIXEL

@ 230,10 BUTTON oBtmParse PROMPT "&Parser" ;
  ACTION (DoPArser(oDlg,oListResult,oXml,cXml)) ;
  SIZE 040, 013 OF oFolders:aDialogs[1]  PIXEL

@ 230,350 BUTTON oBtmParse PROMPT "Sai&r" ;
  ACTION (oDlg:End()) ;
  SIZE 040, 013 OF oFolders:aDialogs[1]  PIXEL


// Folder 2 . Parser do Arquivo XML informado

@ 10,10 SAY "Informe o arquivo XML para Parser" OF oFolders:aDialogs[2] PIXEL
@ 20,10 GET oGetFile VAR cXmlFile SIZE 380, 13 of oFolders:aDialogs[2]  PIXEL

@ 230,10 BUTTON oBtnParseF PROMPT "&Parser" ;
  ACTION (DoParserFile(oDlg,oListResult,oXml,cXmlFile)) ;
  SIZE 040, 013 of oFolders:aDialogs[2]  PIXEL

@ 230,350 BUTTON oBtmParse PROMPT "Sai&r" ;
  ACTION (oDlg:End()) ;
  SIZE 040, 013 of oFolders:aDialogs[2]  PIXEL

// Folder 3 - ListBox com o resultado 

@0,0 LISTBOX oListResult VAR nOpList;
	ITEMS aNodeList ;
	OF oFolders:aDialogs[3]  PIXEL

oListResult:ALIGN := CONTROL_ALIGN_ALLCLIENT

ACTIVATE DIALOG oDlg CENTER 

Return


STATIC Function DoPArser(oDlg,oListResult,oXml,cXml)
Local lOk
Local nTimer 
Local cMsg

If Empty(cXml)
	MsgInfo('Infome um XML para executar o Parser')
	Return
Endif

// remove quebras de linha e tabulações
cXml := Strtran(cXml,chr(13),'')
cXml := Strtran(cXml,chr(10),'')
cXml := Strtran(cXml,chr(9),'')

nTimer := seconds()
lOk := oXml:Parse(cXml)	
nTimer := seconds()-nTimer

If !lOk
	cMsg := oXml:Error()
	oListResult:SetArray({' '})
	MsgStop(cMsg,"Parse Error")     
Else
	cMsg := "Parser executado em "+str(nTimer,12,2)+' s.'
	BuildResult(oXml,oListResult)
	MsgInfo(cMsg,"Parse OK")     
Endif
	
Return

STATIC Function DoParserFile(oDlg,oListResult,oXml,cXmlFile)
Local lOk
Local nTimer 
Local cMsg
                                       
cXmlFile := alltrim(cXmlFile)

If Empty(cXmlFile)
	MsgStop('Infome um arquivo XML para executar o Parser')
	Return
Endif

If !file(cXmlFile)
	MsgStop('Arquivo ['+cXmlFile+'] não encontrado.')
	Return
Endif

nTimer := seconds()
lOk := oXml:ParseFile(cXmlFile)	
nTimer := seconds()-nTimer

If !lOk
	cMsg := oXml:Error()
	oListResult:SetArray({' '})
	MsgStop(cMsg,"Parse Error")     
Else
	cMsg := "Parser executado em "+str(nTimer,12,2)+' s.'
	BuildResult(oXml,oListResult)
	MsgInfo(cMsg,"Parse OK")     
Endif
	
Return
                                 

STATIC Function BuildResult(oXml,oListResult)
Local cRow          
Local aResult := {}

While .T.

	If !XMLempty(oXML:cText)
		//conout("Name ... "+oXML:cName)
		//conout("Path ... "+oXML:cPath)
		//conout("Text ... "+oXML:cText)
		cRow := 'Path ['+oXML:cPath+'] Value ['+oXML:cText+']'
		aadd(aResult,cRow)
	Endif
	
	IF oXml:DOMHASATT()
		aAtt := oXml:DOMGETATTARRAY()
		For nI := 1 to len(aAtt)
			cRow := 'Path ['+oXML:cPath+'] Att ['+aAtt[nI][1]+'] Value ['+aAtt[nI][2]+']'
			aadd(aResult,cRow)
		Next
	Endif
	
	If oXml:DOMHasNextNode()
		oxml:DOMNextNode()
		LOOP
	ElseIf oXml:DOMHasChildNode()
		oxml:DOMChildNode()
		LOOP
	Endif
	
	EXIT
	
Enddo

If len(aResult) > 0 
	oListResult:SetArray(aResult)
Else
	oListResult:SetArray({' *** NO RESULT *** '})
Endif

Return


Static Function XMLempty(cNodeValue)

cNodeValue := strtran(cNodeValue,chr(13),'')
cNodeValue := strtran(cNodeValue,chr(10),'')
cNodeValue := strtran(cNodeValue,chr(9),'')
cNodeValue := strtran(cNodeValue,' ','')

Return empty(cNodeValue)



