---------------------------------------------------------
-- Relatorio.hs
-- Para executar digitar: runghc Relatorio.hs
---------------------------------------------------------

module Relatorio where

main :: IO()
main = do -- pegar input do usuario
    putStrLn ("Digite o tipo do relatorio:" ++ "\n" ++ "3: trimestral \n6: semestral \n12: anual")
    input <- getLine
    let tipo = (read input :: Int) -- input eh lido como string
    putStr (relatorio tipo)

tamanhoHeader :: Int 
tamanhoHeader = 30

precoProduto :: Double
precoProduto = 3450.30

-- recebe o "tipo" do relatorio
-- 3 -> trimestral
-- 6 -> semestral
-- 12 -> anual
relatorio :: Int -> String
relatorio tipo 
    | tipo == 3 = cabecalho ++ "\n" ++ corpo3 1 3 ++ "\n" ++ rodape 1 3 ++ "\n" ++
                                       imprimirSimbolo tamanhoHeader '-' ++ "\n" ++ 
                                       corpo3 4 6 ++ "\n" ++ rodape 4 6 ++ "\n" ++
                                       imprimirSimbolo tamanhoHeader '-' ++ "\n" ++ 
                                       corpo3 7 9 ++ "\n" ++ rodape 7 9 ++ "\n" ++
                                       imprimirSimbolo tamanhoHeader '-' ++ "\n" ++ 
                                       corpo3 10 12 ++ "\n" ++ rodape 10 12 ++
                                       imprimirSimbolo tamanhoHeader '-' ++ "\n"
                                        
    | tipo == 6 = cabecalho ++ "\n" ++ corpo6 1 6 ++ "\n" ++ rodape 1 6 ++ "\n" ++
                                       imprimirSimbolo tamanhoHeader '-' ++ "\n" ++ 
                                       corpo6 7 12 ++ "\n" ++ rodape 7 12 ++
                                       imprimirSimbolo tamanhoHeader '-' ++ "\n"

    | tipo == 12 = cabecalho ++ "\n" ++ corpo12 ++ "\n" ++ rodape 1 12 ++
                                       imprimirSimbolo tamanhoHeader '-' ++ "\n"

    | otherwise = "Tipo de relatorio invalido"

cabecalho :: String
cabecalho = (imprimirSimbolo tamanhoHeader '-') ++ "\n" ++ imprimirSimbolo 8 '-' ++ 
            "Empresa Modelo" ++ imprimirSimbolo 8 '-' ++  "\n" ++
            (imprimirSimbolo tamanhoHeader '-') ++ "\n"

-- helper para imprimir simbolos
imprimirSimbolo :: Int -> Char -> String
imprimirSimbolo 0 ch = ""
imprimirSimbolo n ch = [ch] ++ imprimirSimbolo (n-1) ch

tituloTabela :: String
tituloTabela = "Meses" ++ imprimirSimbolo 10 '.' ++ "Vendas" ++ imprimirSimbolo 4 '.' ++ "Valor"

-- funcoes para corpo do relatorio
-- como precisamos diferenciar os tipos de relatorio (trimestral/semestral/anual), 
-- temos uma funcao para cada
corpo3 :: Int -> Int -> String
corpo3 l r = (imprimirSimbolo 7 ' ') ++ "Relatorio trimestral\n" ++ tituloTabela ++ "\n" ++ imprimeMeses l r ++ (imprimirSimbolo tamanhoHeader '-') ++ "\n"

corpo6 :: Int -> Int -> String
corpo6 l r = (imprimirSimbolo 5 ' ') ++ "Relatorio semestral\n" ++ (imprimirSimbolo tamanhoHeader '-') ++ "\n" ++ tituloTabela ++ "\n" ++ imprimeMeses l r 

corpo12 :: String
corpo12 = (imprimirSimbolo 5 ' ') ++ "Relatorio anual\n" ++ (imprimirSimbolo tamanhoHeader '-') ++ "\n"++ tituloTabela ++ "\n" ++imprimeMeses 1 12

-- funcoes recebem dois parametros L e R para indicar o intervalo de meses correspondentes 
-- precisamos disso para imprimir somente as informacoes correspondentes ao relatorio trimestral
-- de 4 a 6, por exemplo
imprimeMeses :: Int -> Int -> String
imprimeMeses l r 
    | l <= r = imprimeMeses l (r-1) ++ imprimeMes r
    | otherwise = ""

imprimeMes :: Int -> String
imprimeMes n = mes n ++ "  " ++ show(vendas n) ++ "  " ++ show (valorVendasMes n) ++ "\n"


mes :: Int -> String
mes 1 = "Janeiro" ++ imprimirSimbolo 2 ' '
mes 2 = "Fevereiro" ++ imprimirSimbolo 0 ' '
mes 3 = "Marco" ++ imprimirSimbolo 4 ' '
mes 4 = "Abril" ++ imprimirSimbolo 4 ' '
mes 5 = "Maio" ++ imprimirSimbolo 5 ' '
mes 6 = "Junho" ++ imprimirSimbolo 4 ' '
mes 7 = "Julho" ++ imprimirSimbolo 4 ' '
mes 8 = "Agosto" ++ imprimirSimbolo 3 ' '
mes 9 = "Setembro" ++ imprimirSimbolo 1 ' '
mes 10 = "Outubro" ++ imprimirSimbolo 2 ' '
mes 11 = "Novembro" ++ imprimirSimbolo 1 ' '
mes 12 = "Dezembro" ++ imprimirSimbolo 1 ' '

-- TODO deixar mais bonito
rodape :: Int -> Int -> String
rodape l r = imprimirSimbolo tamanhoHeader '-' ++ "\n" ++
          "Grafico de vendas:\n" ++ plotarGrafico l r ++ "\n" ++
           imprimirSimbolo tamanhoHeader '-' ++ "\n" ++ 
          "Soma Total: " ++ show (somaVendas l r) ++ "\n" ++
          "Total vendas: " ++ show (valorVendas l r) ++ "\n" ++
          "Maior Venda: " ++ show (maiorVenda l r) ++ " em " ++ maiorVendaMes l r ++ "\n" ++
          "Venda Zerada? " ++ show (vendaZerada l r) ++ "\n" ++
          "Quantidade Vendas Zeradas " ++ show (quantidadeVendaZerada l r) ++ "\n" ++
          "Media de Vendas: " ++ show (mediaVendas l r) ++ "\n" ++
          "Desvio Padrao: " ++ show (desvioPadraoVendas l r) ++ "\n"
          

-- guardar informacoes das vendas
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
    | l <= r = fromIntegral (vendas r) * precoProduto  + valorVendas l (r-1)
    | otherwise = 0

valorVendasMes :: Int -> Double
valorVendasMes n = fromIntegral (vendas n) * precoProduto

-- Calcular a maior venda entre o mês l e r, inclusive
maiorVenda :: Int -> Int -> Int
maiorVenda l r 
    | l <= r = max (vendas r) (maiorVenda l (r-1)) 
    | otherwise = 0 

-- Mes onde ocorre maior venda
maiorVendaMes :: Int -> Int -> String
maiorVendaMes l r = maiorVendaMesAux l r r

maiorVendaMesAux :: Int -> Int -> Int -> String
maiorVendaMesAux l r cur
    | l <= r && (vendas cur == maiorVenda l r) = mes cur -- usa funcao ja feita
    | l <= r = maiorVendaMesAux l r (cur-1)
    | otherwise = ""



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

-- Calcular a média de vendas no entre o mês l e r, inclusive.
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

-- auxiliar para guardar o quanto temos que dividir 
-- TODO talvez fazer melhor, so dividindo no final
desvioPadraoVendasAux :: Int -> Int -> Int -> Double
desvioPadraoVendasAux l r n 
    | l <= r = (fromIntegral (vendas r) - mediaVendas l n) * (fromIntegral (vendas r) - mediaVendas l n) / fromIntegral n + desvioPadraoVendasAux l (r-1) n
    | otherwise = 0

plotarGrafico :: Int -> Int -> String
plotarGrafico l r 
    | l <= r = plotarGrafico l (r-1) ++ mes r ++ " " ++ imprimirSimbolo (vendas r) '#' ++ "\n"
    | otherwise = ""

------------------------------------------------
-- TO DO:

-- Melhorar layout do relatório conforme tamanhoHeader
-- Centralizar os títulos 
------------------------------------------------
