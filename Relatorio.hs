---------------------------------------------------------
-- Relatorio.hs
-- Para executar digitar: runghc Relatorio.hs
---------------------------------------------------------

module Relatorio where

main :: IO()
main = putStr (relatorio 12)

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
    | tipo == 3 = cabecalho ++ "\n" ++ corpo3 ++ "\n" ++ rodape 3
    | tipo == 6 = cabecalho ++ "\n" ++ corpo6 ++ "\n" ++ rodape 6
    | tipo == 12 = cabecalho ++ "\n" ++ corpo12 ++ "\n" ++ rodape 12
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
corpo12 = (imprimirSimbolo 5 ' ') ++ "Relatorio anual\n" ++ (imprimirSimbolo tamanho '-') ++ "\n"++ tituloTabela ++ "\n" ++imprimeMeses 12

corpo6 :: String
corpo6 = (imprimirSimbolo 5 ' ') ++ "Relatorio semestral\n" ++ (imprimirSimbolo tamanho '-') ++ "\n" ++ tituloTabela ++ "\n" ++ imprimeMeses 6 

corpo3 :: String
corpo3 = (imprimirSimbolo 7 ' ') ++ "Relatorio trimestral\n" ++ tituloTabela ++ "\n" ++ imprimeMeses 3 ++ (imprimirSimbolo tamanho '-') ++ "\n"

imprimeMeses :: Int -> String
imprimeMeses 1 = imprimeMes 1
imprimeMeses n = imprimeMeses (n-1) ++ imprimeMes n

imprimeMes :: Int -> String
imprimeMes n = mes n ++ "  " ++ show(vendas n) ++ "  " ++ show (valorVendasMes n) ++ "\n"

imprimePadding :: Int -> String
imprimePadding 0 = ""
imprimePadding n = " " ++ imprimePadding (n-1)

mes :: Int -> String
<<<<<<< HEAD
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

rodape :: Int -> String
rodape n = imprimirTracos tamanho ++ "\n" ++
          plotarGrafico n ++ "\n" ++
          "Soma Total:" ++ show (somaVendas n) ++ "\n" ++
          "Total vendas: " ++ show (valorVendas n) ++ "\n" ++
          "Maior Venda: " ++ show (maiorVenda n) ++ "\n" ++
          "Venda Zerada? " ++ show (vendaZerada n) ++ "\n" ++
          "Quantidade Vendas Zeradas? " ++ show (quantidadeVendaZerada n) ++ "\n" ++
          "Media de Vendas: " ++ show (mediaVendas n) ++ "\n" ++
          "Desvio Padrao: " ++ show (desvioPadraoVendas n) ++ "\n"
          


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

somaVendas :: Int -> Int
somaVendas 1 = vendas 1
somaVendas n = vendas n + somaVendas (n-1)

valorVendas :: Int -> Double
valorVendas 0 = 0
valorVendas n = fromIntegral (vendas n) * preco  + valorVendas (n-1)

valorVendasMes :: Int -> Double
valorVendasMes n = fromIntegral (vendas n) * preco

--------------------------------------------------------
-- Implementar as funções abaixo utilizando recursão
-- Incluir as informações no relatório de Vendas
--------------------------------------------------------

-- Calcular a maior venda entre o mês 1 e n, inclusive
maiorVenda :: Int -> Int
maiorVenda 1 = vendas 1
maiorVenda n = max (vendas n) (maiorVenda (n-1)) 

-- Verificar se há venda zerada entre o mês 1 e n, inclusive.
vendaZerada :: Int -> Bool
vendaZerada 1 = (vendas 1 == 0)
vendaZerada n = (vendas n == 0) || vendaZerada (n-1)

-- Retorna a quantidade de vendas zeradas entre o mês 1 e n, inclusive
quantidadeVendaZerada :: Int -> Int
quantidadeVendaZerada 1 = fromEnum (vendas 1 == 0)
quantidadeVendaZerada n = fromEnum (vendas n == 0) + quantidadeVendaZerada (n-1) 

-- Calcular a média de vendas no entre o mês 1 e n, inclusive.
mediaVendas :: Int -> Double
mediaVendas n = mediaVendasAux n n

mediaVendasAux :: Int -> Int -> Double
mediaVendasAux 0 n = 0
mediaVendasAux i n = (fromIntegral (vendas i)) / (fromIntegral n) + mediaVendasAux (i-1) n

-- Calcular desvio padrao de vendas no entre o mês 1 e n, inclusive.
desvioPadraoVendas :: Int -> Double
desvioPadraoVendas n = sqrt (desvioPadraoVendasAux n n)

desvioPadraoVendasAux :: Int -> Int -> Double
desvioPadraoVendasAux 0 n = 0.0
desvioPadraoVendasAux i n = (fromIntegral (vendas i) - mediaVendas n) * (fromIntegral (vendas i) - mediaVendas n) / fromIntegral n + desvioPadraoVendasAux (i-1) n

imprimeHashtag :: Int -> String
imprimeHashtag 0 = ""
imprimeHashtag n = "#" ++ imprimeHashtag (n-1)

plotarGrafico :: Int -> String
plotarGrafico 0 = ""
plotarGrafico n = plotarGrafico (n-1) ++ mes n ++ " " ++ imprimeHashtag (vendas n) ++ "\n"


------------------------------------------------
-- TO DO:

-- Melhorar layout do relatório conforme tamanho
-- Centralizar os títulos 
------------------------------------------------
