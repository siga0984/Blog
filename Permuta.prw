/* -----------------------------------------------------------------
Funcao 		U_PERMUTA
Autor		Julio Wittwer
Data		07/12/2018
Descrição	Fonte de exemplo do algoritmo de Heap para permutação 
----------------------------------------------------------------- */

User Function Permuta()
Generate( 4 , {'A' ,'B' ,'C' ,'D' } )
Return


STATIC Function Generate( n , A )
Local c := {}
Local i := 0 
For i := 1 to n
	aadd(c,0)
Next
output(A)
i := 0 
While i < n
	If  c[i+1] < i
		if ( i % 2 ) == 0 
			swap(A, 1 , i+1)
		else
			swap(A, c[i+1]+1, i+1)
		end if
		output(A)
		c[i+1]++
		i := 0
	Else
		c[i+1] := 0
		i++
	EndIf
Enddo

STATIC Function swap(aData,nPos1,nPos2)
Local nTemp := aData[nPos1]
aData[nPos1] := aData[nPos2] 
aData[nPos2] := nTemp
Return

STATIC Function output(A)
Local i, R := ''
For i := 1 to len(A)
	If !empty(R)
		R += ', '
	Endif
	R += cValToChaR(A[i])
Next
conout(R)
Return
	
/* -----------------------------------------------------------------

Pseudocodigo original -- sem recursão

ALGORITMO DE HEAP. In: WIKIPÉDIA, a enciclopédia livre. Flórida: Wikimedia Foundation, 2018. 
Disponível em: <https://pt.wikipedia.org/w/index.php?title=Algoritmo_de_Heap&oldid=53519558>. 
Acesso em: 5 nov. 2018.

procedure generate(n : integer, A : array of any):
    c : array of int

    for i := 0; i < n; i += 1 do
        c[i] := 0
    end for

    output(A)

    i := 0;
    while i < n do
        if  c[i] < i then
            if i is even then
                swap(A[0], A[i])
            else
                swap(A[c[i]], A[i])
            end if
            output(A)
            c[i] += 1
            i := 0
        else
            c[i] := 0
            i += 1
        end if
    end while

----------------------------------------------------------------- */
    
    