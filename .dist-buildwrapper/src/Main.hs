module Main where

import Text.Printf 
import System.Random
import Data.List
import System.IO.Unsafe

type Point     = (Float,Float)
type Color     = (Int,Int,Int)
type Circle    = (Point,Float)

imageWidth :: Int
imageWidth = 500

imageHeight :: Int
imageHeight = 500


-- Funcao principal que faz leitura do dataset e gera arquivo SVG
main :: IO ()
main = do 
        strcontent <- readFile infile
        let pairs = map (span (/= ' ')) (lines strcontent)
            freqs = readInts (map snd pairs)
        writeFile outfile (svgCloudGen imageWidth imageHeight freqs)
        putStrLn "Ok!"
        where 
                infile = "dataset.txt"
                outfile = "tagcloud.svg"


-- Transforma lista de strings em lista de inteiros
readInts :: [String] -> [Int]
readInts ss = map read ss


-- Gera o documento SVG da tag cloud, concatenando cabecalho, conteudo e rodape
svgCloudGen :: Int -> Int -> [Int] -> String
svgCloudGen w h dataset = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++ 
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ++
        (svgViewBox w h) ++
        (concat (svgBubbleGen w h dataset)) ++ "</svg>\n"


-- Funcao que gera a lista de circulos em formato SVG.
-- uso da funcao reverse: cria uma nova sequ�ncia do original com itens na ordem inversa 
-- uso da funcao sort: serve para ordenar
svgBubbleGen:: Int -> Int -> [Int] -> [String]
svgBubbleGen w h dataset = [funcJunta (fromIntegral w/2) (fromIntegral h/2) (reverse (sort raio))] 
        where raio = funcRaio dataset


-- Configura o viewBox da imagem e coloca retangulo branco no fundo
svgViewBox :: Int -> Int -> String
svgViewBox w h =
        printf "<svg width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\"" w h w h ++ 
                " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
        printf "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" style=\"fill:white;stroke:purple;stroke-width:4\"/>\n" w h
            
            
-- Gera string representando um circulo em SVG.
-- Demorei para encontrar uma funcao que transforme IO Int -> Int. Quem faz isso � a unsafePerformIO
svgCircle :: Circle -> String
svgCircle ((x,y),r) = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(%d,%d,%d)\" />\n" x y r (unsafePerformIO funcCor) (unsafePerformIO funcCor) (unsafePerformIO funcCor)
        

-- Esta fun��o gera um n�mero rand�mico no intervalo de 0 a 255        
funcCor :: IO Int
funcCor = randomRIO (0, 255)
        
        
-- Fun��o para calcular o tamanho do raio atrav�s das frequencias do dataset, e transforma a lista em float
-- Peguei 5% (por isso a divis�o por 20) de cada frequ�ncia do dataset para o raio.
-- Somei 3, pois tendo como entrada: funcRaio [1,5,2,6,1], a sa�da: [5.0e-2,0.25,0.1,0.3,5.0e-2]
funcRaio :: [Int] -> [Float]
funcRaio [] = []
funcRaio dataset = (fromIntegral (head dataset)/20)+3 : funcRaio (tail dataset) 
            

-- Fun��o que gera os circulos
-- Ela chama outra fun��o enviando todos os elementos do c�rculo, al�m de um valor para o a e o t
funcJunta :: Float -> Float -> [Float] -> String
funcJunta  _ _ [] = []
funcJunta x y raio = 
        let monta = funcDadoscirc circulo1 (x,y) raio t  
                where circulo1 = ((x,y),head raio)
                      -- a = 1 -- Comentei essa linha, pois deixei o a fixado em 1, logo nao influenciar� nos c�lculos a*t*cos t e a*t*sen t
                      t = 0
        in lisCircle monta -- Aqui manda a lista de c�rculos gerada pela funcDadoscirc para a fun��o lisCircle que gera o svg    
        
        
-- Fun��o que cria uma lista de c�rculos
-- Vai inserindo os c�rculos na lista, mas antes � necessario chamar a funcao da espiral e verificar se possui interseccao
funcDadoscirc :: Circle -> Point -> [Float] -> Float -> [Circle]
funcDadoscirc _ _ [] _ = [] -- A condi��o de parada � quando a lista de raios for vazia
funcDadoscirc circulo1 (x2,y2) raio t = 
        let ponto = funcEspiral circulo1 circulo2 t
                where circulo2 = ((x2,y2),head(tail raio))
        in circulo1 : funcDadoscirc ponto (x2,y2) (tail raio) t
                                                                                                                                                                   
                                                                                
-- Fun��o que lista todos os c�rculos em svg
lisCircle :: [Circle] -> String
lisCircle [] = []
lisCircle (h:t) = svgCircle (h) ++ lisCircle t 
 
       
-- Esta fun��o verifica se 2 c�rculos possuem intersec��o
funcInterseccao :: Circle -> Circle -> Bool
funcInterseccao ((x1,y1),r1) ((x2,y2),r2)
        |distancia > r1+r2 = False
        |otherwise         = True
        where distancia = sqrt (xC+yC)
              xC = (x2 - x1) ^ 2 
              yC = (y2 - y1) ^ 2 
              
              
-- Esta fun��o coloca um ponto na espiral que n�o tenha intersec��o
funcEspiral :: Circle -> Circle -> Float -> (Circle) 
funcEspiral ((x,y),r) (novoPonto,novoR) t
        |k == True = funcEspiral circulo1 circulo2 (t+0.01) -- Se tem interseccao, � necess�rio deslocar este ponto, alterando o t.
        |otherwise = (circulo2)
        where k = funcInterseccao circulo1 circulo2 
              circulo1 = ((x,y),r)
              circulo2 = ((novoX,novoY),novoR)
              novoX = (fst novoPonto) + (t*(cos t))
              novoY = (snd novoPonto) + (t*(sin t))