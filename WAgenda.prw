#include "protheus.ch" 

User Function WAgenda()
Local cHtml := ''
Local cFile := 'AGENDA'
   
HTTPPOST->_SHOWRECORD := .F. 
            
If empty(HTTPSESSION->LOGIN)
	// Usuário ainda não está logado. 
	// Retorna para ele a tela de login
	// Antes, passa internamente um valor para identificar 
	// qual a funcao a ser executada apos o login ser realizado 
	HTTPGET->REQUEST := 'agenda'
	Return H_WLogin()
Endif

If empty(HTTPPOST->OP)
	// Caso nao tenha sido informada operação, a operação 
	// default é posicionar no primeiro registro da tabela
	// em ordem alfabérica
	HTTPPOST->OP := '4'
Endif

If Select("AGENDA") == 0 
	USE (cFile) ALIAS AGENDA SHARED NEW VIA "TOPCONN"
	DbSetIndex(cFile+'1')
	DbSetIndex(cFile+'2')
Endif

DbSelectArea("AGENDA")

If HTTPPOST->OP == '4' // Primeiro
      
	DBSetOrder(2)
	Dbgotop()
	HTTPPOST->_SHOWRECORD := .T.
	
ElseIf HTTPPOST->OP == '5' // Anterior
      
	DBSetOrder(2)
	Dbgoto( val(HTTPPOST->RECNO) )
	DbSkip(-1)
	IF bof()
			HTTPPOST->_ERRORMSG := 'Este é o primeiro contato.'
	Endif
	HTTPPOST->_SHOWRECORD := .T.
	
ElseIf HTTPPOST->OP == '6' // Próximo

	DbSetOrder(2)      
	Dbgoto( val(HTTPPOST->RECNO) )
	DbSkip()   
	If Eof()
		Dbgobottom()
		HTTPPOST->_ERRORMSG := 'Este é o último contato.'
	Endif
	HTTPPOST->_SHOWRECORD := .T.

ElseIf HTTPPOST->OP == '7' // Último
      
	DBSetOrder(2)
	Dbgobottom()	
	HTTPPOST->_SHOWRECORD := .T.

Endif

cHtml := H_WAgenda()

Return cHtml




