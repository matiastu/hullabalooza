module Lib where
import Text.Show.Functions

laVerdad = True

-- punto 1,2 y 3

type Nombre = String
type CantPub = Float
type Bandas = String
type Animo = String
type Concierto = Festival->Festival

data Festival = UnFestival {nombre :: Nombre,publico :: CantPub,animo :: Animo,bandas :: [Banda]} deriving (Show, Eq)

data Banda = UnaBanda {nombreBanda :: Nombre, genero :: Genero , descripcion :: [String], decibeles :: Int} deriving (Show, Eq)

data Genero = RockNacional | Pop | HeavyMetal | TrashMetal | PopMetal deriving (Show, Eq)

losRedondos :: Banda
losRedondos = UnaBanda "Los Redondos" RockNacional ["legendaria", "pogosa"] 45

soda :: Banda
soda = UnaBanda "Soda" RockNacional ["irrepetible"] 40

miranda :: Banda
miranda = UnaBanda "Miranda" Pop ["insipida","incolora","inodora"] 60

metallica :: Banda
metallica = UnaBanda "Metallica" HeavyMetal ["legendaria","vendida"] 60

hullabalooza :: Festival
hullabalooza = UnFestival "Hulabalooza" 20000 "indiferente" [miranda, losRedondos, metallica, soda]

tocar :: Festival -> Banda -> Festival
tocar festival banda | any (estaEnFest banda) (bandas festival) = tocar1 festival banda
                     | otherwise = festival

estaEnFest :: Banda -> Banda -> Bool
estaEnFest banda festival = banda == festival

tocar1 :: Festival -> Banda -> Festival
tocar1 festival banda | genero banda == RockNacional = modificarRock festival
                      | genero banda == Pop = modificarPop festival 
                      | otherwise = modificarMetal banda festival

modificarRock :: Concierto
modificarRock festival = festival{publico = publico festival + 100}

modificarPop :: Concierto
modificarPop festival | animo festival == "indiferente" = euforicoPop festival 
                      | otherwise = festival

euforicoPop :: Concierto
euforicoPop festival = festival {publico = publico festival * 2, animo = "euforico"} 

modificarMetal :: Banda -> Concierto
modificarMetal banda festival | genero banda == HeavyMetal = aumentarMetal " pesado" festival
                              | genero banda == TrashMetal = aumentarMetal " basura" festival
                              | otherwise = aumentarMetal " pop" festival

aumentarMetal :: String -> Concierto 
aumentarMetal tipo festival = festival{publico= publico festival + publico festival *0.01, animo = animo festival ++ tipo}

kiss :: Banda
kiss = UnaBanda "Kiss" TrashMetal ["clasico","inigualable"] 80

theStrokes :: Banda
theStrokes = UnaBanda "The Strokes" PopMetal ["suicidio asistido", "emocional", "linda"] 45

-- punto 4

suceder :: Festival -> Festival
suceder festival = foldl (tocar) festival (bandas festival) 

-- punto 5

clasificarBanda :: Banda -> Int
clasificarBanda banda | esRePolenta banda = 3
                      | estaBuenarda banda = 2
                      | estaBuenarda2 banda = 2
                      | estaBuenarda3 banda = 2
                      | esVendida banda = 1
                      | esAcustica banda = 1
                      | esLegendaria banda = 1
                      | otherwise = 0

esRePolenta :: Banda -> Bool
esRePolenta banda = esVendida banda && esAcustica banda && esLegendaria banda

estaBuenarda :: Banda -> Bool
estaBuenarda banda = esVendida banda && esAcustica banda

estaBuenarda2 :: Banda -> Bool
estaBuenarda2 banda = esAcustica banda && esLegendaria banda

estaBuenarda3 :: Banda -> Bool
estaBuenarda3 banda = esVendida banda && esLegendaria banda

esVendida :: Banda -> Bool
esVendida banda = length (descripcion banda) >= 3 || any (== "vendida") (descripcion banda)

esAcustica :: Banda -> Bool
esAcustica banda = decibeles banda >= 55

esLegendaria :: Banda -> Bool
esLegendaria banda = any (== "legendaria") (descripcion banda) && decibeles banda >= 40

popularidad :: Banda -> Int
popularidad banda = (clasificarBanda banda) * 100