#include 'protheus.ch'

/* ===================================================================================
Funcao 		U_ZJSONKIT()
Autor		Julio Wittwer
Data 		01/12/2019
Descrição 	Interface para identificar informações de Objeto JSON

			Transforma uma String JSON em um objeto AdvPL, e monta os nodes  
			com o caminho para obtenção das informações . Usa uma string fornecida 
			na hora ou o conteudo de um arquivo indicado a partir do RootPath 
			ou na máquina cliente ( onde está o SmartClient ) 
==================================================================================== */
    
User Function ZJSONKIT()
Local oDlg
Local oGetJSON
Local cJSON := ''
Local oJSON                    
Local cJSONFile := space(80)              
Local nOpList
Local aNodeList := {''}
Local oListResult 
Local oFont
Local lIgnoreNull := .F. 
Local lOnlyStruct := .F. 
Local aResult := {}

// Define Formato de data DD/MM/AAAA
SET DATE BRITISH
SET CENTURY ON

// Fonte Default tamanho 10, Monoespaçada
oFont := TFont():New('Courier new',,-12,.T.)
SETDEFFONT(oFont)

// Cria o objeto JSON 
oJSON := JsonObject():New()

// Cria uma caixa de diálogo com área util de 800x600  PIXELs
DEFINE DIALOG oDlg TITLE "AdvPL JSON Kit" FROM 0,0 TO 600,800 PIXEL

@ 0,0 FOLDER oFolders PROMPTS "JSON String","JSON File","Resultado"  OF oDlg  PIXEL
oFolders:ALIGN := CONTROL_ALIGN_ALLCLIENT

// Folder 1 . Parser da String informada

@ 10,10 SAY "Informe a String JSON para avaliar" OF oFolders:aDialogs[1] PIXEL
@ 20,10 GET oGetJSON VAR cJSON MULTILINE SIZE 380, 200 of oFolders:aDialogs[1]  PIXEL

@ 230,10 BUTTON oBtmParse PROMPT "&Parser" ;
  ACTION (DoPArser(oDlg,oJSON,cJSON,aResult),ShowResult(oFolders,oListResult,aResult,lIgnoreNull,lOnlyStruct)) ;
  SIZE 040, 013 OF oFolders:aDialogs[1]  PIXEL

@ 230,350 BUTTON oBtmParse PROMPT "Sai&r" ;
  ACTION (oDlg:End()) ;
  SIZE 040, 013 OF oFolders:aDialogs[1]  PIXEL

// Folder 2 . Parser do Arquivo JSON informado

@ 10,10 SAY "Informe o arquivo JSON para avaliar" OF oFolders:aDialogs[2] PIXEL
@ 20,10 GET oGetFile VAR cJSONFile SIZE 380, 13 of oFolders:aDialogs[2]  PIXEL

@ 230,10 BUTTON oBtnParseF PROMPT "&Parser" ;
  ACTION (DoParserFile(oDlg,oJSON,cJSONFile,aResult),ShowResult(oFolders,oListResult,aResult,lIgnoreNull,lOnlyStruct)) ;
  SIZE 040, 013 of oFolders:aDialogs[2]  PIXEL

@ 230,350 BUTTON oBtmParse PROMPT "Sai&r" ;
  ACTION (oDlg:End()) ;
  SIZE 040, 013 of oFolders:aDialogs[2]  PIXEL

// Folder 3 - ListBox com o resultado 

@ 230,60 CHECKBOX oChkBox1 VAR lIgnoreNull PROMPT "Ignorar NULL" SIZE 80,12 OF oFolders:aDialogs[3] PIXEL

@ 230,120 CHECKBOX oChkBox2 VAR lOnlyStruct PROMPT "Somente Estrutura" SIZE 80,12 OF oFolders:aDialogs[3] PIXEL

@ 230,210 BUTTON oBtnUpd PROMPT "&Atualizar" ;
  ACTION (ShowResult(oFolders,oListResult,aResult,lIgnoreNull,lOnlyStruct)) ;
  SIZE 040, 013 of oFolders:aDialogs[3]  PIXEL

@ 230,280 BUTTON oBtnExp PROMPT "&Copiar" ;
  ACTION (ToClipboard(oListResult)) ;
  SIZE 040, 013 of oFolders:aDialogs[3]  PIXEL

@ 230,350 BUTTON oBtmParse PROMPT "Sai&r" ;
  ACTION (oDlg:End()) ;
  SIZE 040, 013 of oFolders:aDialogs[3]  PIXEL

@ 20,10 LISTBOX oListResult VAR nOpList;
	ITEMS aNodeList ;
	SIZE 380, 200;
	OF oFolders:aDialogs[3]  PIXEL

// Listbox ocupa o frame inteiro 
// oListResult:ALIGN := CONTROL_ALIGN_ALLCLIENT

ACTIVATE DIALOG oDlg CENTER 

Return

// ----------------------------------------------------------------------------                                 
// Realiza o parser da string informada na interface

STATIC Function DoPArser(oDlg,oJSON,cJSON,aResult)
Local nTimer 
Local cMsg

If Empty(cJSON)
	MsgInfo('Infome um JSON para executar o Parser')
	Return
Endif

// Limpa o resultado 
aSize(aResult,0)

nTimer := seconds()
cMsg := oJSON:fromJson(cJSON)	
nTimer := seconds()-nTimer

If !empty(cMsg)
	If len(cMsg)>512
		cMsg := left(cMsg,512)
	Endif
	MsgStop(cMsg,"JSON Parse Error")     
Else
	cMsg := "Parser executado em "+str(nTimer,12,2)+' s.'
	BuildResult(oJSON,aResult,"",left(cJson,1)=='[')
	MsgInfo(cMsg,"Parse OK")     
	// Ordena o resutado alfabeticamente
	aSort(aResult)
Endif
	
Return

// ----------------------------------------------------------------------------                                 
// Faz o parser de um arquivo JSON

STATIC Function DoParserFile(oDlg,oJSON,cJSONFile,aResult)
Local nTimer 
Local cMsg      
Local cJSonStr  := ''
Local nH
Local nTam
                                       
cJSONFile := alltrim(cJSONFile)

If Empty(cJSONFile)
	MsgStop('Infome um arquivo JSON para executar o Parser')
	Return
Endif

If !file(cJSONFile)
	MsgStop('Arquivo ['+cJSONFile+'] não encontrado.')
	Return
Endif

// Limpa o resultado 
aSize(aResult,0)

// Carrega o conteudo do arquivo para a mémória 
nH := Fopen(cJsonFile)
nTam := fSeek(nh,0,2)
fSeek(nH,0)
fRead(nH,@cJSonStr,nTam)
fClose(nH)

// Popula o objeto JSON a partir da string
nTimer := seconds()
cMsg := oJSON:FromJson(cJSonStr)
nTimer := seconds()-nTimer

If !empty(cMsg)
	IF len(cMsg) > 512
		cMsg := left(cMsg,512)
	Endif
	MsgStop(cMsg,"JSON PARSE ERROR")     
Else
	cMsg := "Parser executado em "+str(nTimer,12,2)+' s.'
	BuildResult( oJSON , aResult,"",left(cJSonStr,1)=='[')
	MsgInfo(cMsg,"JSON Parse OK")     
	// Ordena o resutado alfabeticamente
	aSort(aResult)
Endif
	
Return

// ----------------------------------------------------------------------------                                 
// Monta um Array de resultados com os dados obtidos do Objeto 
// Gera e varre o Objeto JSON recursivamente, gerando os caminhos de acesso 
// aos dados e estrutura de objetos do JSON    

STATIC Function BuildResult(oJSON,aResult,cPath,lBeginArray)
Local cRow          
Local aNames , nI 
Local lIsJson := .F.
Local lIsArray := .F. 
Local xValue

DEFAULT aResult := {}
DEFAULT cPath := ''  
DEFAULT lBeginArray := .F. 

If !empty(cPath)
	cPath += ']['
Endif

IF valtype(oJson) == 'A' .or. ( valtype(oJson) == 'J' .and. lBeginArray ) 

	// O Objeto é um array, varre todos os elementos
	
	For nI := 1 to len(oJson)
		xValue := oJson[nI]               
		lIsJson := ( valtype(xValue) = 'J' )
		lIsArray := ( valtype(xValue) = 'A' )
		cRow := 'oJson['+cPath+cValToChar(nI)+'] // Tipo ['+valtype(xValue)+']'
		If lIsJson .or. lIsArray
			If lIsArray
				cRow += ' Elementos ['+cValToChar(len(xValue))+']'
			Endif
			BuildResult(xValue,aResult,cPath+str(nI,4),.F.)
		ElseIf valtype(xValue) != 'U'
			cRow += ' Conteudo ['+cValToChar(xValue)+']'
			aadd(aResult,cRow)
		Endif
	Next

ElseIf valtype(oJson) == 'J'

	// Identifica a lista de propriedades do objeto JSOB
	// Varre a lista avaliando todas as propriedades
	
	aNames := oJson:GetNames()
	For nI := 1 to len(aNames)
		xValue := oJson[aNames[nI]]
		lIsJson := ( valtype(xValue) = 'J' )
		lIsArray := ( valtype(xValue) = 'A' )
		cRow := 'oJson['+cPath+'"'+aNames[nI]+'"] // Tipo ['+valtype(xValue)+']'
		If lIsJson .or. lIsArray
			If lIsArray
				cRow += ' Elementos ['+cValToChar(len(xValue))+']'
			Endif
			aadd(aResult,cRow)
			BuildResult(xValue,aResult,cPath+'"'+aNames[nI]+'"',.F.)
		ElseIf valtype(xValue) != 'U'
			cRow += ' Conteudo ['+cValToChar(xValue)+']'
			aadd(aResult,cRow)
		Endif
	Next

Endif

Return 


// ----------------------------------------------------------------------------                                 
// Mosrta o resultado considerando os parametros de filtro 

STATIC Function ShowResult(oFolders,oListResult,aResult,lIgnoreNull,lOnlyStruct)
Local aShowResult := {}
Local nI , cRow

If len(aResult) > 0  
	For nI := 1 to len(aResult)
		cRow := aResult[nI]
		IF lIgnoreNull .and. "Tipo [U]" $ cRow 
			// Tipo NIL / NULL ignora
			loop
		ElseIF !lOnlyStruct          
			// Nao é somente a estrutura ? Acrescenta no resultado 
			aadd(aShowResult,cRow)
		ElseIf  "Tipo [J]" $ cRow .or. "Tipo [A]" $ cRow 
			//  Somente estrutura , é um objeto ou array, mostra 
			aadd(aShowResult,cRow)              
		Endif
	Next
	conout('Resultado : '+cValtoChar(len(aShowResult)))
	oListResult:SetArray(aShowResult)

	// Joga o foco para a aba de resultados em caso de sucesso 
	oFolders:SHOWPAGE(3)

Else

	// Em caso de erro apenas limpa a aba de resultados
	oListResult:SetArray({' '})

Endif

Return

// ----------------------------------------------------------------------------                                 
// Gera uma string com o conteúdo visivel de objetos na tela
// E copia para a área de transferencia

STATIC Function ToClipboard(oListResult)
Local aData := oListResult:aItems
LOcal cStr := '' ,nI 
For nI := 1 to len(aData)
	cStr += aData[nI] + chr(13)+chr(10)
Next
COPYTOCLIPBOARD(cStr)
MsgInfo('Conteúdo copiado para a área de transferencia')
Return                         


