#include 'protheus.ch'

/* ============================================================
Arquivo    zLibNBin.prw
Autor      Julio Wittwer
Data       01/2019
Descrição  Arquivo de funções auxiliares de conversao entre
           valores numericos em ASCII ( Binary String ) , com 2 e 4 bytes 
           
Referência

Um valor numérico dentro de um arquivo não necessariamente possui a mesma 
representação deste valor na memória. A plataforma IBM/PC e compatíveis 
(Intel, AMD64) usa uma disposição de bytes na memória chamada Little Endian, 
o que significa que o BIT do byte com o maior valor está no final -- lado direito
da sequencia. 

Com 2 bytes da memória, podemos representar 65536 valores distintos, 
do valor 0 ao 65535.  O valor 16737 é representado em hexadecimal 
( base 16 ) pela sequencia 0x4161. Cada par de digitos pode ser colocado 
em um byte. Logo, temos dois pares: 0x41 e 0x61

Usando a representação Big Endian, este número seria armazenado 
nesta ordem, começando da esquerda para a direita, o primeiro 
byte é o 0x41, e o segundo é 0x51. 

A representação binária deste numero em Little Endian é ao contrário, 
o byte com o bit de maior valor ( 0x41 ) vai no final da sequência. Logo, ele 
é representado na sequência 0x61 0x41 
                                      
As funções nativas da linguagem AdvPL para conversão de valores inteiros de 16 bits ( 2 Bytes ) 
e 32 Bits ( 4 Bytes ) entre os formatos numérico decimal e string binária são : 

I2BBin e Bin2I -- Número 16 bits para string binária e vice-versa 
L2BBin e Bin2L -- Número 32 bits para string binária e vice-versa 

As funções nativas de conversão do ADvPL trabalham com a representação 
binária em Little Endian. Já as funções de conversão feitas nesta lib 
trabalham com big Endian -- bit de maior valor primeiro ( a esquerda ) 

============================================================ */

User Function ZLibNBin()

cValor := chr(65)+chr(97)    // 0x41 0x61

// 24897 -- valor esperado considerando a sequencia em little endian
nValor := Bin2I(cValor)
conout(nValor)             

// 16737 -- valor esperado considerando a sequencia como big-endian
nValor := Bin2toN(cValor)
conout(nValor)             

Return


// ------------------------------------------------------------
// Converte buffer de 4 bytes ( 32 Bits ) no seu valor numerico  

STATIC Function Bin4toN(cBin4)
Local nByte1,nByte2,nByte3,nByte4

nByte1 := asc(substr(cBin4,1,1))
nByte2 := asc(substr(cBin4,2,1))
nByte3 := asc(substr(cBin4,3,1))
nByte4 := asc(substr(cBin4,4,1))

If nByte3 > 0
	nByte4 += ( nByte3 * 256 )
Endif
If nByte2 > 0
	nByte4 += ( nByte2 * 65536 )
Endif
If nByte1 > 0
	nByte4 += ( nByte1 * 16777216 )
Endif

Return nByte4


// ------------------------------------------------------------
// Converte valor numérico em buffer de 4 bytes ( 32 Bits ) 
// ( High Byte First )

STATIC Function NtoBin4(nNum)
Local cBin4 := '' , nTmp
While nNum > 0
	nTmp := nNum % 256 
	cBin4 := chr(nTmp) + cBin4
	nNum := ( ( nNum - nTmp ) / 256 )
Enddo
While len(cBin4) < 4
	cBin4 := CHR(0) + cBin4
Enddo
Return cBin4


// ------------------------------------------------------------
// Converte buffer de 2 bytes ( 16 Bits ) no seu valor numerico  
// ( High Byte First ) 

STATIC Function Bin2toN(cBin4)
Local nByte1,nByte2

nByte1 := asc(substr(cBin4,1,1))
nByte2 := asc(substr(cBin4,2,1))

If nByte1 > 0
	nByte2 += ( nByte1 * 256 )
Endif

Return nByte2


// ------------------------------------------------------------
// Converte valor numérico em buffer de 2 bytes ( 16 Bits ) 
// ( High Byte First ) 

STATIC Function NtoBin2(nNum)
Local cBin2 := '' , nTmp
Local nL := ( nNum % 256 ) 
Local nH := ( nNum-nL ) / 256 
Return chr(nH) + chr(nL)

