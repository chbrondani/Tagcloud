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
            --freq = head freqs
        --print freq
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
-- uso da funcao reverse: cria uma nova sequência do original com itens na ordem inversa 
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
-- Demorei para encontrar uma funcao que transforme IO Int -> Int. Quem faz isso é a unsafePerformIO
svgCircle :: Circle -> String
svgCircle ((x,y),r) = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(%d,%d,%d)\" />\n" x y r (unsafePerformIO funcCor) (unsafePerformIO funcCor) (unsafePerformIO funcCor)
        

-- Esta função gera um número randômico no intervalo de 0 a 255        
funcCor :: IO Int
funcCor = randomRIO (0, 255)
        
        
-- Função para calcular o tamanho do raio através das frequencias do dataset, e transforma a lista em float
-- Peguei 5% (por isso a divisão por 20) de cada frequência do dataset para o raio.
-- Somei 3, pois tendo como entrada: funcRaio [1,5,2,6,1], a saída: [5.0e-2,0.25,0.1,0.3,5.0e-2]
funcRaio :: [Int] -> [Float]
funcRaio [] = []
funcRaio dataset = (fromIntegral (head dataset)/20)+3 : funcRaio (tail dataset) 
            

-- Função que gera os circulos do tipo Circle ((x,y),raio)
-- Ela chama outra função enviando todos os elementos do círculo, além de um valor para o a e o t
funcJunta :: Float -> Float -> [Float] -> String
funcJunta  _ _ [] = []
funcJunta x y raio = 
        let monta = funcDadoscirc circulo1 (x,y) raio a t  
                where circulo1 = ((x,y),head raio)
                      a = 2
                      t = 1
        in lisCircle monta 
        
        
-- Função que gera os circulos do tipo Circle ((x,y),raio)
-- Vai inserindo os círculos na lista, mas antes é necessario chamar a funcao da espiral e verificar se possui interseccao
funcDadoscirc :: Circle -> Point -> [Float] -> Float -> Float -> [Circle]
funcDadoscirc _ _ [] _ _ = []
funcDadoscirc circulo1 (x,y) raio a t = 
        let ponto = funcEspiral circulo1 circulo2 a t
                where circulo2 = ((x,y),head(tail raio))
        in circulo1 : funcDadoscirc (fst ponto) (x,y) (tail raio) a (snd ponto)    


-- Função que lista todos os círculos
lisCircle :: [Circle] -> String
lisCircle [] = []
lisCircle (h:t) = svgCircle (h) ++ lisCircle t 
 
       
-- Esta função verifica se 2 círculos possuem intersecção
funcInterseccao :: Circle -> Circle -> Bool
funcInterseccao ((x1,y1),r1) ((x2,y2),r2)
        |distancia > r1+r2 = False
        |otherwise         = True
        where distancia = sqrt (xC+yC)
              xC = (x2 - x1) ^ 2 
              yC = (y2 - y1) ^ 2 
              
              
-- Esta função coloca um ponto na espiral que não tenha intersecção
funcEspiral :: Circle -> Circle -> Float -> Float-> (Circle, Float) -- (Circle, Float): o Circle corresponde ao círculo que será criado na espiral e o Float é o t.
funcEspiral ((x,y),r) (novoPonto,novoR) a t
        |k == True = funcEspiral ((x,y),r) ((novoX,novoY),novoR) a (t + (0.3)) -- Se tem interseccao, é necessário deslocar este ponto, alterando o t.
        |otherwise = (((novoX, novoY),novoR),t)
        where k = funcInterseccao ((x,y),r) ((novoX,novoY),novoR) 
              novoX = (fst novoPonto) + (a*t*(cos t))
              novoY = (snd novoPonto) + (a*t*(sin t))