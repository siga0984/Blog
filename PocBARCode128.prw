#include 'protheus.ch'
#include 'zlib.ch'  

User Function PocBAR128()

Local oBar := zBarCode128():New()
Local aSeq, oBmp

// Gera a sequencia de valores em array 
aSeq := oBar:Generate('http://siga0984.wordpress.com')

// Monta o bitmap usando os valores
oBmp := oBar:BuildBmp(aSeq)

// Salva a imagem em disco 
oBmp:SaveToBMP('\barcode.bmp')               

Return



