#include 'protheus.ch'

/* ======================================================================
Função      U_APPINT01 e U_APPINT02
Autor       Júlio Wittwer
Data        21/01/2015
Descrição   Fonte de demonstração de uso de classes de Interface AdvPL

Post relacionado : https://siga0984.wordpress.com/2015/01/21/interface-visual-do-advpl-parte-01/
====================================================================== */

User Function APPINT01()
Local oDlg
Local oBtn1, oSay1
 
DEFINE DIALOG oDlg TITLE "Exemplo" FROM 0,0 TO 150,300 COLOR CLR_BLACK,CLR_WHITE PIXEL
@ 25,05 SAY oSay1 PROMPT "Apenas uma mensagem" SIZE 60,12 OF oDlg PIXEL 
 
@ 50,05 BUTTON oBtn1 PROMPT 'Sair' ACTION ( oDlg:End() ) SIZE 40, 013 OF oDlg PIXEL
ACTIVATE DIALOG oDlg CENTER
Return
  
// Abaixo segue a mesma funcao, escrita usando diretamente as classes
// de interface visual, ao inves de usar os comandos de interface

/*

User Function APPINT01() 
Local oDlg
Local oBtn1, oSay1
 
oDlg = TDialog():New( 0, 0, 150, 300, "Exemplo" ,,,,, CLR_BLACK,CLR_WHITE ,,, .T. )
oSay1 := TSay():New( 25, 05, {|| "Apenas uma mensagem"} ,oDlg,,,,,,.T.,,, 60, 12 )
oBtn1 := TButton():New( 50, 05, "Sair", oDlg,{|| oDlg:End() }, 40, 013,,,,.T. )
oDlg:Activate( , , , .T. )
Return

*/

