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
-- uso da funcao reverse: cria uma nova sequ�ncia da original com itens na ordem inversa 
-- uso da funcao sort: serve para ordenar
svgBubbleGen:: Int -> Int -> [Int] -> [String]
svgBubbleGen w h dataset = lisCircle (funcJunta [] (fromIntegral w/2) (fromIntegral h/2) (reverse (sort raio)))
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
-- Somei 3 (para o menor raio ficar vis�vel), pois tendo como entrada: funcRaio [1,5,2,6,1], a sa�da: [5.0e-2,0.25,0.1,0.3,5.0e-2]
funcRaio :: [Int] -> [Float]
funcRaio [] = []
funcRaio dataset = (fromIntegral (head dataset)/20)+3 : funcRaio (tail dataset) 
            

-- Fun��o que gera a lista de circulos
funcJunta :: [Circle] -> Float -> Float -> [Float] -> [Circle]
funcJunta lista _ _ [] = lista
funcJunta lista x y raio = funcJunta (lista ++ (funcEspiral lista circulo t)) x y (tail raio)
        where circulo = ((x,y),head raio)     
              -- a = 1 -- Comentei essa linha, pois deixei o a fixado em 1, logo nao influenciar� nos c�lculos a*t*cos t e a*t*sen t
              t = 0                                                                                                                                                                              
        
                          
-- Esta fun��o coloca um ponto na espiral que n�o tenha intersec��o
funcEspiral :: [Circle] -> Circle -> Float -> [Circle]
funcEspiral lista ((x,y),r) t
        |k == True = funcEspiral lista ((novoX,novoY),r) (t+0.2) -- Se tem interseccao, � necess�rio deslocar este ponto, alterando o t.
        |otherwise = [((x,y),r)] -- Este � o c�rculo sem intersec��o
        where k = funcInterseccao lista ((x,y),r) 
              novoX = abs(x + (t*(cos t)))
              novoY = abs(y + (t*(sin t)))   
              
                                                                                              
-- Fun��o que lista todos os c�rculos em svg
lisCircle :: [Circle] -> [String]
lisCircle [] = []
lisCircle (h:t) = svgCircle (h) : lisCircle t 
 
       
-- Esta fun��o analisa se possui intersec��o entre o c�rculo que quer ser inserido e a lista de c�rculos
funcInterseccao :: [Circle] -> Circle -> Bool
funcInterseccao [] _ = False
funcInterseccao lista circulo = funcDistancia circulo (head lista) || (funcInterseccao (tail lista) circulo) 
-- Ex: Tem intersecc�o e lista de circulos est� vazia ent�o (True || False) � igual a True 


-- Esta fun��o calcula a dist�ncia entre 2 c�rculos
funcDistancia :: Circle -> Circle -> Bool
funcDistancia ((x1,y1),r1) ((x2,y2),r2)
        |distancia > r1+r2 = False
        |otherwise         = True
        where distancia = sqrt (xC+yC)
              xC = (x2 - x1) ^ 2 
              yC = (y2 - y1) ^ 2    