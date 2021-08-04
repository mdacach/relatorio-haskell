---------------------------------------------------------
-- Relatorio.hs
-- Para executar digitar: runghc Relatorio.hs
---------------------------------------------------------

module Relatorio where

main :: IO()
main = putStr (relatorio 6)

tamanho :: Int 
tamanho = 30

preco::Double
preco= 3450.30

-- recebe o "tipo" do relatorio
-- 3 -> trimestral
-- 6 -> semestral
-- 12 -> anual
relatorio :: Int -> String
relatorio tipo 
    | tipo == 3 = cabecalho ++ "\n" ++ corpo3 1 3 ++ "\n" ++ rodape 1 3 ++ "\n" ++ corpo3 4 6 ++ "\n" ++ rodape 4 6 ++ "\n" ++
                                       corpo3 7 9 ++ "\n" ++ rodape 7 9 ++ "\n" ++ corpo3 10 12 ++ "\n" ++ rodape 10 12
    | tipo == 6 = cabecalho ++ "\n" ++ corpo6 1 6 ++ "\n" ++ rodape 1 6 ++ "\n" ++ corpo6 7 12 ++ "\n" ++ rodape 7 12
    | tipo == 12 = cabecalho ++ "\n" ++ corpo12 ++ "\n" ++ rodape 1 12
    | otherwise = "Tipo de relatorio invalido"

cabecalho :: String
cabecalho = (imprimirSimbolo tamanho '-') ++ "\n" ++ imprimirSimbolo 8 '-' ++ 
            "Empresa Modelo" ++ imprimirSimbolo 8 '-' ++  "\n" ++
            (imprimirSimbolo tamanho '-') ++ "\n"

imprimirSimbolo :: Int -> Char -> String
imprimirSimbolo 0 ch = ""
imprimirSimbolo n ch = [ch] ++ imprimirSimbolo (n-1) ch

tituloTabela :: String
tituloTabela = "Meses" ++ imprimirSimbolo 10 '.' ++ "Vendas" ++ imprimirSimbolo 4 '.' ++ "Valor"

corpo12 :: String
corpo12 = (imprimirSimbolo 5 ' ') ++ "Relatorio anual\n" ++ (imprimirSimbolo tamanho '-') ++ "\n"++ tituloTabela ++ "\n" ++imprimeMeses 1 12

corpo6 :: Int -> Int -> String
corpo6 l r = (imprimirSimbolo 5 ' ') ++ "Relatorio semestral\n" ++ (imprimirSimbolo tamanho '-') ++ "\n" ++ tituloTabela ++ "\n" ++ imprimeMeses l r 

corpo3 :: Int -> Int -> String
corpo3 l r = (imprimirSimbolo 7 ' ') ++ "Relatorio trimestral\n" ++ tituloTabela ++ "\n" ++ imprimeMeses l r ++ (imprimirSimbolo tamanho '-') ++ "\n"

imprimeMeses :: Int -> Int -> String
imprimeMeses l r 
    | l <= r = imprimeMeses l (r-1) ++ imprimeMes r
    | otherwise = ""

imprimeMes :: Int -> String
imprimeMes n = mes n ++ "  " ++ show(vendas n) ++ "  " ++ show (valorVendasMes n) ++ "\n"

imprimePadding :: Int -> String
imprimePadding 0 = ""
imprimePadding n = " " ++ imprimePadding (n-1)

mes :: Int -> String
mes 1 = "Janeiro" ++ imprimePadding 2
mes 2 = "Fevereiro" ++ imprimePadding 0
mes 3 = "Marco" ++ imprimePadding 4
mes 4 = "Abril" ++ imprimePadding 4
mes 5 = "Maio" ++ imprimePadding 5
mes 6 = "Junho" ++ imprimePadding 4
mes 7 = "Julho" ++ imprimePadding 4
mes 8 = "Agosto" ++ imprimePadding 3
mes 9 = "Setembro" ++ imprimePadding 1
mes 10 = "Outubro" ++ imprimePadding 2
mes 11 = "Novembro" ++ imprimePadding 1
mes 12 = "Dezembro" ++ imprimePadding 1

imprimirTracos :: Int -> String
imprimirTracos 0 = ""
imprimirTracos n = "-" ++ imprimirTracos (n-1)

rodape :: Int -> Int -> String
rodape l r = imprimirTracos tamanho ++ "\n" ++
          plotarGrafico l r ++ "\n" ++
          "Soma Total:" ++ show (somaVendas l r) ++ "\n" ++
          "Total vendas: " ++ show (valorVendas l r) ++ "\n" ++
          "Maior Venda: " ++ show (maiorVenda l r) ++ "\n" ++
          "Venda Zerada? " ++ show (vendaZerada l r) ++ "\n" ++
          "Quantidade Vendas Zeradas? " ++ show (quantidadeVendaZerada l r) ++ "\n" ++
          "Media de Vendas: " ++ show (mediaVendas l r) ++ "\n" ++
          "Desvio Padrao: " ++ show (desvioPadraoVendas l r) ++ "\n"
          


vendas :: Int -> Int
vendas 1 = 10
vendas 2 = 12
vendas 3 = 8
vendas 4 = 0
vendas 5 = 30
vendas 6 = 23
vendas 7 = 23
vendas 8 = 9
vendas 9 = 19
vendas 10 = 13
vendas 11 = 22
vendas 12 = 18
vendas _  = 0

somaVendas :: Int -> Int -> Int
somaVendas l r 
    | l <= r = vendas r + somaVendas l (r-1)
    | otherwise = 0

valorVendas :: Int -> Int -> Double
valorVendas l r
    | l <= r = fromIntegral (vendas r) * preco  + valorVendas l (r-1)
    | otherwise = 0

valorVendasMes :: Int -> Double
valorVendasMes n = fromIntegral (vendas n) * preco

--------------------------------------------------------
-- Implementar as funções abaixo utilizando recursão
-- Incluir as informações no relatório de Vendas
--------------------------------------------------------

-- Calcular a maior venda entre o mês l e r, inclusive
maiorVenda :: Int -> Int -> Int
maiorVenda l r 
    | l <= r = max (vendas r) (maiorVenda l (r-1)) 
    | otherwise = 0 

-- Verificar se há venda zerada entre o mês l e r, inclusive.
vendaZerada :: Int -> Int -> Bool
vendaZerada l r 
    | l <= r = (vendas r == 0) || vendaZerada l (r-1)
    | otherwise = False

-- Retorna a quantidade de vendas zeradas entre o mês l e r, inclusive
quantidadeVendaZerada :: Int -> Int -> Int
quantidadeVendaZerada l r 
    | l <= r = fromEnum (vendas r == 0) + quantidadeVendaZerada l (r-1)   
    | otherwise = 0

-- Calcular a média de vendas no entre o mês 1 e n, inclusive.
mediaVendas :: Int -> Int -> Double
mediaVendas l r = mediaVendasAux l r (r-l+1)

-- l -> mes inicial
-- r -> mes final (e que esta sendo contado agora)
-- d -> valor para dividir (numero de meses originalmente)
mediaVendasAux :: Int -> Int -> Int -> Double
mediaVendasAux l r d 
    | l <= r = (fromIntegral (vendas r)) / (fromIntegral d) + mediaVendasAux l (r-1) d
    | otherwise = 0

-- Calcular desvio padrao de vendas no entre o mês 1 e n, inclusive.
desvioPadraoVendas :: Int -> Int -> Double
desvioPadraoVendas l r = sqrt (desvioPadraoVendasAux l r (r-l+1))

desvioPadraoVendasAux :: Int -> Int -> Int -> Double
desvioPadraoVendasAux l r n 
    | l <= r = (fromIntegral (vendas r) - mediaVendas l n) * (fromIntegral (vendas r) - mediaVendas l n) / fromIntegral n + desvioPadraoVendasAux l (r-1) n
    | otherwise = 0

imprimeHashtag :: Int -> String
imprimeHashtag 0 = ""
imprimeHashtag n = "#" ++ imprimeHashtag (n-1)

plotarGrafico :: Int -> Int -> String
plotarGrafico l r 
    | l <= r = plotarGrafico l (r-1) ++ mes r ++ " " ++ imprimeHashtag (vendas r) ++ "\n"
    | otherwise = ""


------------------------------------------------
-- TO DO:

-- Melhorar layout do relatório conforme tamanho
-- Centralizar os títulos 
------------------------------------------------
