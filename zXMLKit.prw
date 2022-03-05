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

// Faz parser de um XML lido do disco a partir do RootPAth

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
                                 
// Monta os dados para um ListBox com os resultados obtidos 

STATIC Function BuildResult(oXml,oListResult,aResult,nStack)
Local cRow          
Local nI 

DEFAULT aResult := {}
DEFAULT nStack := 1
 
While .T.

	// Se o nó atual tem um dado, armazena
	If !XMLempty(oXML:cText)
		cRow := 'Path ['+oXML:cPath+'] Value ['+oXML:cText+']'
		aadd(aResult,cRow)
	Endif
	
	IF oXml:DOMHASATT()
		// Se o nó atual tem atrubitos, lê todos
		aAtt := oXml:DOMGETATTARRAY()
		For nI := 1 to len(aAtt)
			cRow := 'Path ['+oXML:cPath+'] Att ['+aAtt[nI][1]+'] Value ['+aAtt[nI][2]+']'
			aadd(aResult,cRow)
		Next
	Endif
	
	If oXml:DOMHasChildNode()
		// Se o nó atual tem um filho, entra nele para avaliar o conteudo
		oxml:DOMChildNode()
		BuildResult(oXml,oListResult,aResult,nStack+1)
		oxml:DOMParentNode()
	Endif
	
	IF oXml:DOMHasNextNode()
		// Se existe um próximo nó neste nivel, 
		// vai pra ele e continua analizando 
		oxml:DOMNextNode()
		LOOP
	Endif

	// Este nó já foi analizado inteiro, sai do loop 
	EXIT
	
Enddo

If nStack == 1
	// Seta o array de resultado para o ListBox
	If len(aResult) > 0
		oListResult:SetArray(aResult)
	Else
		oListResult:SetArray({' *** NO RESULT *** '})
	Endif
Endif

Return

// Remove caracteres irrelevantes do valor de um node 
// para ver se ele é realmente vazio

Static Function XMLempty(cNodeValue)

cNodeValue := strtran(cNodeValue,chr(13),'')
cNodeValue := strtran(cNodeValue,chr(10),'')
cNodeValue := strtran(cNodeValue,chr(9),'')
cNodeValue := strtran(cNodeValue,' ','')

Return empty(cNodeValue)

