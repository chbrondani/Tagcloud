module Main where

import Text.Printf 

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


-- Esta funcao deve gerar a lista de circulos em formato SVG.
-- A implementacao atual eh apenas um teste que gera um circulo posicionado no meio da figura.
-- TODO: Alterar essa funcao para usar os dados do dataset.
dadosDataset :: IO()
dadosDataset = do 
        strcontent <- readFile "dataset.txt"
        let pairs = map (span (/= ' ')) (lines strcontent)
            freqs = readInts (map snd pairs)
        print freqs
        
svgBubbleGen:: Int -> Int -> [Int] -> [String]
svgBubbleGen w h dataset = [svgCircle ((fromIntegral w/2, fromIntegral h/2), fromIntegral 2249/50) (0,255,0)]


-- Gera string representando um circulo em SVG. A cor do circulo esta fixa. 
-- TODO: Alterar esta funcao para mostrar um circulo de uma cor fornecida como parametro.

--funcCor :: Int -> Int -> Int -> Color
--funcCor r g b = (r,g,b)
  --      where r >= 0 && r <=255 
    --          g >= 0 && g <=255 
      --        b >= 0 && b <=255 

svgCircle :: Circle -> Color -> String
svgCircle ((x,y),raio)(r,g,b)= printf "<circle cx=\"%f\" cy=\"%f\" raio=\"%f\" fill=\"rgb(%d,%d,%d)\" />\n" x y raio r g b 


-- Configura o viewBox da imagem e coloca retangulo branco no fundo
svgViewBox :: Int -> Int -> String
svgViewBox w h =
        printf "<svg width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\"" w h w h ++ 
                " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
        printf "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" style=\"fill:white;stroke:purple;stroke-width:4\"/>\n" w h
        
        
-- Esta fun��o calcula a dist�ncia entre 2 c�rculos
funcDistancia :: Circle -> Circle -> Float
funcDistancia ((x1,y1),r1) ((x2,y2),r2) = sqrt (((x2-x1)^2)+((y2-y1)^2))

-- Esta fun��o verifica se 2 c�rculos possuem intersec��o
funcInterseccao :: Float -> Circle -> Circle -> Bool
funcInterseccao funcDistancia (_,r1) (_,r2)
        |funcDistancia > r1+r2        = False
        |funcDistancia < r1-r2        = False
        |funcDistancia == 0 && r1==r2 = True
        