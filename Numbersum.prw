#include "protheus.ch"

/* ---------------------------------------------------
Função    U_NumberSum
Autor     Júlio Wittwer
Data      08/2024
Versão    1.240818
Descrição Interface e core de processamento para solucionar fases do jogo Number Sums 
--------------------------------------------------- */

#define SOLVE_ECHO 	.F.

USER  Function NumberSum()

	Local cTitle := 'NumberSum - Interface'
	Local nLinhas := 6
	Local nColunas := 6

	DEFINE DIALOG oDlg TITLE (cTitle) ;
		FROM 0,0 TO 240,240  PIXEL

	@ 14,05 SAY "Linhas" SIZE 60,12 OF oDlg  PIXEL

	@ 12,50 GET nLinhas PICTURE "99" SIZE 30,12 OF oDlg  PIXEL

	@ 28,05 SAY "Colunas" SIZE 60,12 OF oDlg  PIXEL

	@ 26,50 GET nColunas PICTURE "99" SIZE 30,12 OF oDlg  PIXEL

	@ 44,05  BUTTON oBtn1 PROMPT "Proximo" SIZE 60,15 ;
		ACTION (GuiData(nLinhas,nColunas)) OF oDlg  PIXEL

	ACTIVATE DIALOG oDlg CENTER

return

/**
* Monta interface dinamica para obter as informações do Grid de dados 
*/

STATIC Function GuiData(nLinhas,nColunas)
	Local oDlg2
	Local nC , nL , x := 0
	Local cTitle := 'Informe os valores'
	Local aGuiData := {}
	Local aGuiCols := {}
	Local aGuiRows := {}

	For nL := 1 to nLinhas
		aadd(aGuiRows,0)
	Next
	For nC := 1 to nColunas
		aadd(aGuiCols,0)
	Next
	For nL := 1 to nLinhas
		aadd(aGuiData,aclone(aGuiCols))
	Next

	DEFINE DIALOG oDlg2 TITLE (cTitle) ;
		FROM 0,0 TO 480,480  PIXEL

	// Input dos valores das somas das colunas
	For nC := 1 to nColunas

		@ 02, (nC*20)+5 GET oGet VAR x PICTURE "999" SIZE 10,12 OF oDlg2  PIXEL
		oGet:bSetGet := &('{|x| if(pcount()=0, aGuiCols['+cvaltochar(nC)+'] ,  aGuiCols['+cvaltochar(nC)+'] := x )}')

	Next

	// Input das linhas
	For nL := 1 to nLinhas

		// A primeira coluna, mais à esquerda, recebe o valor esperado da soma
		@ nL*20, 1 GET oGet VAR x PICTURE "999" SIZE 10,12 OF oDlg2  PIXEL
		oGet:bSetGet := &('{|x| if(pcount()=0, aGuiRows['+cvaltochar(nL)+'] ,  aGuiRows['+cvaltochar(nL)+'] := x )}')

		// As demais colunas são os valores daquela linha
		For nC := 1 to nColunas

			@ nL*20, (nC*20)+5 GET oGet VAR x PICTURE "999" SIZE 10,12 OF oDlg2  PIXEL
			oGet:bSetGet := &('{|x| if(pcount()=0, aGuiData['+cvaltochar(nL)+']['+cvaltochar(nC)+'] ,  aGuiData['+cvaltochar(nL)+']['+cvaltochar(nC)+'] := x )}')

		Next

	Next

	@ nL*20,05  BUTTON oBtn1 PROMPT "Resolver" SIZE 60,15 ;
		ACTION ( SolveGrid(aGuiData,aGuiCols,aGuiRows) , oDlg2:Refresh() ) OF oDlg2  PIXEL

	ACTIVATE DIALOG oDlg2 CENTER

return


/*
Faz uma verificação mínima de validade dos dados informados
*/
STATIC Function ValidGrid(aGrid,aSumCol,aSumRow)
	Local nL, nC, nTam
	nTam := len(aSumCol)
	For nL := 1 to nTam
		If aSumRow[nL] < 1
			MsgInfo("VErifique os dados informados.","Atenção")
			return .F.
		Endif
	Next
	For nC := 1 to nTam
		If aSumCol[nC] < 1
			MsgInfo("VErifique os dados informados.","Atenção")
			return .F.
		Endif
	Next
	For nL := 1 to nTam
		For nC := 1 to nTam
			if ( aGrid[nL][nC] < 1  )
				MsgInfo("VErifique os dados informados.","Atenção")
				return .F.
			Endif
		Next
	Next
return .T.


/*
Função de resolução do Grid de Dados
*/

STATIC Function SolveGrid(aGrid,aSumCol,aSumRow)
	Local nL, nC, nTam
	Local nSoma
	Local aData,aNewData
	Local nPend, lSolved := .F.
	Local lFailed := .F.
	Local aGrid1 := aclone(aGrid)
	Local aSumCol1 := aclone(aSumCol)
	Local aSumRow1 := aclone(aSumRow)

	IF !ValidGrid(aGrid,aSumCol,aSumRow)
		Return
	Endif

// O grid é sempre "quadrado", então o numero de linhas e colunas é sempre igual 
	nTam := len(aSumCol)

// O numero de pendencias a resolver sao os conjuntos de linhas e colunas, ou 
// seja, 2 x nTam 
	nPend := nTam*2

	If SOLVE_ECHO
		conout("Grid Inicial : ")
		ShowGrid(aGrid)
	Endif

	While nPend > 0

		// Enquanto houver pendencias

		For nL := 1 to nTam

			// Avalia as linhas do Grid
			// Primeiro pega o valor da soma esperado dessa linha

			nSoma := aSumRow[nL]

			If nSoma > 0
				// Quando a soma é maior que zero, esse conjunto
				// anda está pendente. Se a soma for zero, já resolveu

				// Agora determina os numeros do conjunto ( colunas )
				aData := {}
				For nC := 1 to nTam
					aadd(aData,aGrid[nL][nC])
				Next

				if SOLVE_ECHO
					conout("Analizando Linha "+cvaltochar(nL))
				Endif

				// Faz a analise considerando os dados
				// E retorna o novo conjunto de dados.
				// Caso esse conjunto foi "resolvido", lSolved é atualizado com .t.
				aNewData := Analyze(nSoma,aData,@lSolved)

				// Se o novo array de dados está vazio, nao foi possivel solucionar
				IF empty(aNewData)
					lFailed := .T.
					EXIT
				Endif

				// com o conjunto de dados retornado, atualiza o grid
				For nC := 1 to nTam
					aGrid[nL][nC] := aNewData[nC]
				Next

				If lSolved
					aSumRow[nL] := 0
					nPend--
				Endif

			Endif

		Next

		IF lFailed
			EXIT
		Endif

// Faz agora a mesma coisa para as colunas

		For nC := 1 to nTam

			// Avalia as colunas do Grid
			// Peimeiro pega o valor da soma esperado dessa coluna
			nSoma := aSumCol[nC]

			If nSoma > 0

				// Agora determina os numeros do conjunto ( linhas )
				aData := {}
				For nL := 1 to nTam
					aadd(aData,aGrid[nL][nC])
				Next

				if SOLVE_ECHO
					conout("Analizando Coluna "+cvaltochar(nC))
				Endif

				// Faz a analise considerando os dados
				// E retorna o novo conjunto de dados
				aNewData := Analyze(nSoma,aData,@lSolved)

				// Se o novo array de dados está vazio, nao foi possivel solucionar
				IF empty(aNewData)
					lFailed := .T.
					EXIT
				Endif

				// com o conjunto de dados retornado, atualiza o grid
				For nL := 1 to nTam
					aGrid[nL][nC] := aNewData[nL]
				Next

				If lSolved
					aSumCol[nC] := 0
					nPend--
				Endif

			Endif

		Next

		IF lFailed
			EXIT
		Endif

	Enddo

	IF lFailed

		MsgStop("Falha ao determinar solução. Verifique os dados informados.","Atenção")

		aGrid := aclone(aGrid1)
		aSumCol := aclone(aSumCol1)
		aSumRow := aclone(aSumRow1)

		RETURN

	Endif

	If SOLVE_ECHO
		conout("Grid Final : ")
		ShowGrid(aGrid)
	Endif

return

/*
Função de Análise de Conjuntos 
Recebe o valor de soma esperado, e os números de um conjunto 
Retorna um novo conjunto de numeros, confirmando ou removendo os numeros
para satisfazer a condição de soma. Se um conjunto for resolvido, isto é, 
no retorno ele somente tiver numeros confirmados, o 3o parametro lSolved 
por referência é alimentado com .t. 
*/


STATIC Function Analyze(nSoma,aData,lSolved)
	Local nI
	Local nTam := len(aData)
	Local aBitCmp := {1,2,4,8,16,32,64,128}
	Local nMaxBit := 2 ** nTam  // Total de combinações
	Local nTemp , cBitMask
	Local aPossible := {}
	Local nNum , nPos
	Local aNewData := {}

	lSolved := .F.

	if SOLVE_ECHO
		Conout("Soma Esperada : "+cvaltochar(nSoma) )
		cMsg := "Conjunto : "
		For nI := 1 to len(aData)
			cMsg += " "+cvaltochar(aData[nI])
		Next
		conout(cMsg)
	Endif

	// Avalia do numero 1 ao numero de combinações possivel
	For nNum := 1 to nMaxBit

		// Variavel temporaria para avaliar a soma dessa possibilidade
		// E a mascara de bits dessa possibilidade
		nTemp := 0
		cBitMask := ''

		// Conta de 1 até a quantidade de elementos do conjunto
		For nPos := 1 to nTam

			// Pega o valor do elemento do conjunto
			nValor :=  aData[nPos]

			if ( NAND(nNum , aBitCmp[nPos] ) > 0 )

				cBitMask += '1'

				// Se o bit dessa posição está ligada, o valor deve ser considerado
				// Se o valor nao foi removido, considera . Se foi removido, essa possibilidade não é válida
				If nValor != 0
					nTemp += ABS(nValor)
				Else
					nTemp := 0
					EXIT
				Endif

			Else

				cBitMask += '0'

				// Se o bit dessa posição está ligada, essa possibilidade nao considera esse valor
				// Se esse valor já está confirmado ( valor negativo ), essa possibilidade não é válida
				if nValor < 0
					nTemp := 0
					EXIT
				Endif

			Endif

			if nTemp > nSoma
				// Se o valor somado até agora já passou o total
				// nao precisa continuar, essa possibilidade nao é válida
				nTemp := 0
				EXIT
			Endif

		Next

		If nTemp = nSoma
			// Essa combinação de numeros atingiu a soma
			// acrescenta essa mascara de bits no array de possibilidades
			aadd( aPossible , cBitMask )
		Endif

	Next

	nPossible := len(aPossible)

	if SOLVE_ECHO
		conout("Possibilidades : "+cvaltochar(nPossible))
		for nI := 1 to len(aPossible)
			conout(cvaltochar(nI)+". "+aPossible[nI])
		Next
	Endif

	If nPossible < 1

		// Nao tem nenhuma possibilidade válida ?
		// Algo está errado .. provavelmente os dados
		// do grid nao foram informados corretamente
		If SOLVE_ECHO
			conout("Nenhuma possibilidade encontrada")
		Endif

		REturn {}

	ElseIF nPossible == 1

		// So tem uma possibilidade,
		// efetiva essa possibilidade no retorno
		// "1" confirma o numero, "0" remove
		cBitMask := aPossible[1]
		For nPos := 1 to nTam
			if substr(cBitMask,nPos,1) == '1'
				// Confirma o numero, tornando ele negativo
				nValor := aData[nPos]
				nValor := ABS(nValor) * (-1)
				aadd(aNewData , nValor )
			Else
				// remove o numero, acrescentando 0 no retorno
				aadd(aNewData , 0 )
			Endif
		Next

		// Este conjunto foi resolvido
		lSolved := .T.

	ElseIF nPossible > 1

		// Mais de uma possibilidade,
		// avalia cada numero do conjunto
		For nPos := 1 to nTam
			nUso := 0
			for nI := 1 to len(aPossible)
				cBitMask := aPossible[nI]
				if substr(cBitMask,nPos,1) == '1'
					nUso++
				Endif
			Next
			// Recupera o valor do conjunto
			nValor := aData[nPos]
			if nUso == len(aPossible)
				// Confirma o numero, tornando ele negativo
				nValor := ABS(nValor) * (-1)
				aadd(aNewData , nValor )
			ElseIf nUso == 0
				// remove o numero, acrescentando 0 no retorno
				aadd(aNewData , 0 )
			Else
				// valor inalterado, acrescenta no retorno
				aadd(aNewData , nValor )
			Endif
		Next

	Endif

	if SOLVE_ECHO
		cMsg := "Resultado : "
		For nI := 1 to len(aNewData)
			cMsg += " "+cvaltochar(aNewData[nI])
		Next
		conout(cMsg)
	Endif

return aNewData

/*
Função que recebe o array de dados de um Grid, 
e mostra no log de console do AppServer
*/

STATIC Function ShowGrid(aGrid)
	Local nL, nC, nTam
	Local cMsg := ''

	nTam := len(aGrid)
	For nL := 1 to nTam
		cMsg := ''
		For nC := 1 to nTam
			cMsg += str(aGrid[nL][nC],5)
		Next
		conout(cMsg)
	Next

return
