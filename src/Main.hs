module Main where

import Text.Printf 
import System.Random
import Data.List

type Point     = (Float,Float)
type Color     = (Int,Int,Int)
type Circle    = (Point,Float,Color)

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


-- Esta funcao deve gerar a lista de circulos em formato SVG.
-- A implementacao atual eh apenas um teste que gera um circulo posicionado no meio da figura.
-- TODO: Alterar essa funcao para usar os dados do dataset.
svgBubbleGen:: Int -> Int -> [Int] -> [String]
svgBubbleGen w h dataset = [funcJunta (fromIntegral w/2) (fromIntegral h/2) (reverse (sort dataset)) (0,255,0)]
        --where raio = funcRaio dataset
        
        
-- Função para calcular o tamanho do raio através das frequencias do dataset
funcRaio :: [Int] -> [Float]
funcRaio [] = []
funcRaio dataset = (fromIntegral (head dataset)/20) : funcRaio (tail dataset) -- Peguei 5% (por isso a divisão por 20) de cada frequência do dataset para o raio.


-- Função que gera os circulos do tipo Circle ((x,y),raio,(r,g,b))
funcJunta :: Float -> Float -> [Int] -> Color -> String
funcJunta  _ _ [] _ = []
funcJunta x y dataset (_) = svgCircle ((x,y), raio ,(0,255,0)) ++ (funcJunta ponto_x ponto_y (tail dataset) (255,0,0))
        where
        ponto_x = x*0.1*(cos 0.1)
        ponto_y = y*0.1*(sin 0.1)
        raio = fromIntegral (head dataset)/20
        

-- Gera string representando um circulo em SVG. A cor do circulo esta fixa. 
-- TODO: Alterar esta funcao para mostrar um circulo de uma cor fornecida como parametro.
funcCor :: IO Int
funcCor = randomRIO (0, 255)

-- Função que lista todos os círculos
lisCircle :: [Circle] -> [String]
lisCircle [] = []
lisCircle (h:t) = svgCircle (h) : lisCircle t 

svgCircle :: Circle -> String
svgCircle ((x,y),raiocirc,(r,g,b)) = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(%d,%d,%d)\" />\n" x y raiocirc r g b


-- Configura o viewBox da imagem e coloca retangulo branco no fundo
svgViewBox :: Int -> Int -> String
svgViewBox w h =
        printf "<svg width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\"" w h w h ++ 
                " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
        printf "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" style=\"fill:white;stroke:purple;stroke-width:4\"/>\n" w h
        
        
-- Esta função calcula a distância entre 2 círculos
--funcDistancia :: Circle -> Circle -> Float
--funcDistancia ((x1,y1),r1,_) ((x2,y2),r2,_) = sqrt (((x2-x1)^2)+((y2-y1)^2))

-- Esta função verifica se 2 círculos possuem intersecção
funcInterseccao :: Circle -> Circle -> Bool
funcInterseccao ((x1,y1),r1,_)((x2,y2),r2,_)
        |distancia > r1+r2        = False
        |distancia < r1-r2        = False
        |distancia == 0 && r1==r2 = True
        where distancia = sqrt (((x2-x1)^2)+((y2-y1)^2))