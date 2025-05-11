-- Integrante
-- Matheus Muzza Pires Ferreira Nusp: 15479468
-- Marcelo Martins Conti Nusp: 15474629

main = do

    -- Pegar a Linha de entrada do meu programa
    linha <- getLine

    -- fazer o parse dessa string (separar pelos espaços)
    let partes = words linha
    
    putStrLn $ saida_rodadas (rodadas partes) 1 ++ show ( pontos $ rodadas $ partes)
    
-- Função que monta as duplas das saídas do programa
rodadas :: [String] ->[(String,String)]
rodadas [] = [] -- se recebo uma lista vazia, retorno uma dupla vazia
rodadas [x] = [] -- se recebo uma lista com um elemento, retorno uma dupla vazia
rodadas [x,y,z] -- se recebo uma lista com três elements (estou no final da minha lista, sendo possivel três jogos)
    |charToInt x + charToInt y == 10 && charToInt x/= 10 = [(x,"/"),(z,"0")] -- fiz um parse nesses três jogos com (x e y)
    |charToInt x == 10 = [(x,"_"),(y,z)]
    |charToInt y + charToInt z == 10 = [(x,y),("/","0")] -- fiz um parse nesses jogos com (y e z)
    |otherwise = [(x,y),(z,"0")] -- não fiz um parse
rodadas (x:y:xs) 
    |charToInt x == 10 = ("10","_") : rodadas (y:xs) -- se fiz um strike, monto minha dupla ("10", "_")
    |charToInt x + charToInt y < 10 = (x,y) : rodadas xs -- se fiz um jogo, sem ser strike e Sparse
    |charToInt x + charToInt y > 10 = (x,y) : rodadas xs 
    |otherwise = (x,"/") : rodadas xs -- fiz um parse no jogo

-- montar a saída do meu programa
saida_rodadas :: [(String,String)] -> Int -> String
saida_rodadas [] _ = "" --se recebo uma lista vazia, coloco nada na string
saida_rodadas [(r1,r2)] _  
    |(charToInt r1 + charToInt r2 == 10 && charToInt r2 /= 0 )= ""++r1++" "++"/"++" | "
    |otherwise = ""++eh10(r1)++" "++eh10(r2)++" | " --se recebo uma lista, com uma tupla
    
saida_rodadas ((r1,r2):(r3,r4):xs) i -- se recebo uma lista com mais de uma tupla como elemento
    |i /= 10 && r1 =="10" = "" ++ "X" ++ " " ++ r2 ++ " | " ++ saida_rodadas ((r3,r4):xs) (i+1)
    |i /= 10 && r1 /="10" = "" ++ r1 ++ " " ++ r2 ++" | " ++ saida_rodadas ((r3,r4):xs) (i+1)
    |i == 10 && r1 == "10" ="" ++ "X" ++ " " ++ saida_rodadas((r3,r4):xs) (i+1)
    |otherwise = "" ++ (eh10 r1) ++ " " ++ (eh10 r2) ++ " " ++ (eh10 r3) ++ " | " ++ saida_rodadas (xs) (i+1)
    
eh10 :: String -> String
eh10 x
    |x =="10" = "X"
    |otherwise = x

--converter um caracter para inteiro
charToInt :: String -> Int
charToInt "X" = 10 
charToInt "_" = 0
charToInt c   = read(c)


--Converter a Tupla de String para um array de inteiros
tuplaToArrayInt :: (String, String) -> [Int]
tuplaToArrayInt ("10", "_") = [10]
tuplaToArrayInt (a, "/") = [charToInt a, 10 - charToInt a]
tuplaToArrayInt (a, b) = [charToInt a, charToInt b]

--Calcula os pontos
pontos::[(String, String)] -> Int
pontos frames = pontuarFrames 1 frames

--Pontuar cada frame e ir calculando a soma
pontuarFrames :: Int -> [(String, String)] -> Int
pontuarFrames frame [] = 0 --se tenho uma lista vazia, retorno 0
pontuarFrames 10 ((a, b):(c, d):xs) 
    | b == "/" = charToInt c + 10
    | c == "/" = charToInt a + 10 
    | otherwise = charToInt a + charToInt b + charToInt c + charToInt d
pontuarFrames frame ((a,b):xs)
    | frame >= 11 = 0 -- só tenho no maximo 10 frames
    | a == "10"   = 10 + sum bonus + pontuarFrames (frame + 1) xs -- Tive um Strike
    | b == "/"    = spare -- deu um spare
    | otherwise    = charToInt a + charToInt b + pontuarFrames (frame + 1) xs
    where
        -- vou criar um array de inteiros, que é a concatenação dos dois elementos do vetor de tuplas, 
        -- depois de transformados em inteiros
        bonus = take 2 (concatMap tuplaToArrayInt xs)

        spare = case xs of
            --eu tenho algum elemento no meu xs, logo, eu pego apenas a cabeça desse elemento
            (x:_) -> let prox = head (tuplaToArrayInt x)
                     in 10 + prox + pontuarFrames (frame + 1) xs

            -- caso que eu não tenho mais elemento na minha lista, retorno 10
            _     -> 10