{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Test.HUnit

{-- Tipos --}

import Data.Either
import Data.List

data Dirección = Norte | Sur | Este | Oeste
  deriving (Eq, Show)
type Posición = (Float, Float)

data Personaje = Personaje Posición String  -- posición inicial, nombre
  | Mueve Personaje Dirección               -- personaje que se mueve, dirección en la que se mueve
  | Muere Personaje                         -- personaje que muere
  deriving (Eq, Show)
data Objeto = Objeto Posición String        -- posición inicial, nombre
  | Tomado Objeto Personaje                 -- objeto que es tomado, personaje que lo tomó
  | EsDestruido Objeto                      -- objeto que es destruido
  deriving (Eq, Show)
type Universo = [Either Personaje Objeto]

{-- Observadores y funciones básicas de los tipos --}

siguiente_posición :: Posición -> Dirección -> Posición
siguiente_posición p Norte = (fst p, snd p + 1)
siguiente_posición p Sur = (fst p, snd p - 1)
siguiente_posición p Este = (fst p + 1, snd p)
siguiente_posición p Oeste = (fst p - 1, snd p)

posición :: Either Personaje Objeto -> Posición
posición (Left p) = posición_personaje p
posición (Right o) = posición_objeto o

posición_objeto :: Objeto -> Posición
posición_objeto = foldObjeto const (const posición_personaje) id

nombre :: Either Personaje Objeto -> String
nombre (Left p) = nombre_personaje p
nombre (Right o) = nombre_objeto o

nombre_personaje :: Personaje -> String
nombre_personaje = foldPersonaje (const id) const id

está_vivo :: Personaje -> Bool
está_vivo = foldPersonaje (const (const True)) (const (const True)) (const False)

fue_destruido :: Objeto -> Bool
fue_destruido = foldObjeto (const (const False)) const (const True)

universo_con :: [Personaje] -> [Objeto] -> [Either Personaje Objeto]
universo_con ps os = map Left ps ++ map Right os

es_un_objeto :: Either Personaje Objeto -> Bool
es_un_objeto (Left o) = False
es_un_objeto (Right p) = True

es_un_personaje :: Either Personaje Objeto -> Bool
es_un_personaje (Left o) = True
es_un_personaje (Right p) = False

-- Asume que es un personaje
personaje_de :: Either Personaje Objeto -> Personaje
personaje_de (Left p) = p

-- Asume que es un objeto
objeto_de :: Either Personaje Objeto -> Objeto
objeto_de (Right o) = o

en_posesión_de :: String -> Objeto -> Bool
en_posesión_de n = foldObjeto (const (const False)) (\ r p -> nombre_personaje p == n) (const False)

objeto_libre :: Objeto -> Bool
objeto_libre = foldObjeto (const (const True)) (const (const False)) (const False)

norma2 :: (Float, Float) -> (Float, Float) -> Float
norma2 p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

cantidad_de_objetos :: Universo -> Int
cantidad_de_objetos = length . objetos_en

cantidad_de_personajes :: Universo -> Int
cantidad_de_personajes = length . personajes_en

distancia :: (Either Personaje Objeto) -> (Either Personaje Objeto) -> Float
distancia e1 e2 = norma2 (posición e1) (posición e2)

objetos_libres_en :: Universo -> [Objeto]
objetos_libres_en u = filter objeto_libre (objetos_en u)

está_el_personaje :: String -> Universo -> Bool
está_el_personaje n = foldr (\x r -> es_un_personaje x && nombre x == n && (está_vivo $ personaje_de x) || r) False

está_el_objeto :: String -> Universo -> Bool
está_el_objeto n = foldr (\x r -> es_un_objeto x && nombre x == n && not (fue_destruido $ objeto_de x) || r) False

-- Asume que el personaje está
personaje_de_nombre :: String -> Universo -> Personaje
personaje_de_nombre n u = foldr1 (\x1 x2 -> if nombre_personaje x1 == n then x1 else x2) (personajes_en u)

-- Asume que el objeto está
objeto_de_nombre :: String -> Universo -> Objeto
objeto_de_nombre n u = foldr1 (\x1 x2 -> if nombre_objeto x1 == n then x1 else x2) (objetos_en u)

es_una_gema :: Objeto -> Bool
es_una_gema o = isPrefixOf "Gema de" (nombre_objeto o)

{-Ejercicio 1-}
--foldPersonaje/foldObjeto toma 3 funciones (una para cada constructor),
--un Personaje/Objeto y devuelve el resultado del plegado

foldPersonaje :: (Posición -> String -> a) -> (a -> Dirección -> a) -> (a -> a) -> Personaje -> a
foldPersonaje fBase fMueve fMuere personaje = case personaje of
  Personaje pos nombre -> fBase pos nombre
  Mueve per dir -> fMueve (fold per) dir
  Muere per -> fMuere (fold per)
  where fold = foldPersonaje fBase fMueve fMuere

foldObjeto :: (Posición -> String -> a) -> (a -> Personaje -> a) -> (a -> a) -> Objeto -> a
foldObjeto fBase fTomado fDestruido objeto = case objeto of
  Objeto pos nombre -> fBase pos nombre
  Tomado obj per -> fTomado (fold obj) per
  EsDestruido obj -> fDestruido (fold obj)
  where fold = foldObjeto fBase fTomado fDestruido

{-Ejercicio 2-}
--posición_personaje devuelve la nueva posición de un personaje que se movió,
--aplicando siguiente_posición por cada constructor Mueve

posición_personaje :: Personaje -> Posición
posición_personaje = foldPersonaje (\pos _ -> pos) (\pos dir -> siguiente_posición pos dir) id 

nombre_objeto :: Objeto -> String
nombre_objeto = foldObjeto (const id) const id

{-Ejercicio 3-}

objetos_en :: Universo -> [Objeto]
objetos_en u = map objeto_de (filter es_un_objeto u)

personajes_en :: Universo -> [Personaje]
personajes_en u = map personaje_de (filter es_un_personaje u)

{-Ejercicio 4-}

objetos_en_posesión_de :: String -> Universo -> [Objeto]
objetos_en_posesión_de s u = filter (en_posesión_de s) (objetos_en u)

{-Ejercicio 5-}

-- Asume que hay al menos un objeto
objeto_libre_mas_cercano :: Personaje -> Universo -> Objeto
objeto_libre_mas_cercano p u = foldl1 (menor_distancia_con p) (objetos_libres_en u)


-- Compara la distancia de dos objetos con respecto a un personaje y devuelve el más cercano a este
menor_distancia_con :: Personaje -> Objeto -> Objeto -> Objeto
menor_distancia_con p x y | distancia (Right x) (Left p) < distancia (Right y) (Left p) = x
                          | otherwise = y
-- Es necesario usar Right x/y y Left p para que el tipo coincida con el de la función distancia

{-Ejercicio 6-}
-- Sumo todos los objetos de Thanos que son gemas, y si cuento 6 o más devuelvo true, sino false
tiene_thanos_todas_las_gemas :: Universo -> Bool
tiene_thanos_todas_las_gemas u  | (foldr (\obj acum -> (if es_una_gema obj then 1 else 0) + acum) 0 (objetos_en_posesión_de "Thanos" u)) == 6 = True
                                | otherwise = False


{-Ejercicio 7-}

podemos_ganarle_a_thanos :: Universo -> Bool
podemos_ganarle_a_thanos u = (not (tiene_thanos_todas_las_gemas u)) && 
  ((está_el_objeto "StormBreaker" u && está_el_personaje "Thor" u) || (está_el_personaje "Wanda" u && está_el_personaje "Visión" u && en_posesión_de "Visión" (objeto_de_nombre "Gema de la Mente" u)))

{-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7
  ]

--Personajes

phil = Personaje (0,0) "Phil"
thanos = Personaje (10,10) "Thanos"
thor = Personaje (5,5) "Thor"
vision = Personaje (-5, 3) "Visión"
wanda = Personaje (-5, 2) "Wanda"
dr_strange = Personaje (100,-4) "Dr Strange"

--Objetos

mjölnir = Objeto (2,2) "Mjölnir"
escudo_capitán_américa = Objeto (2,2) "Escudo del Capitán América"
stormbreaker = Objeto (-3,3) "StormBreaker"

stormbreaker_en_thor = Tomado stormbreaker thor

gema_tiempo = Objeto (1,3) "Gema del Tiempo"
gema_mente = Objeto (7,9) "Gema de la Mente"
gema_espacio = Objeto (0,6) "Gema del Espacio"
gema_realidad = Objeto (0,4) "Gema de la Realidad"
gema_poder = Objeto (14,28) "Gema del Poder"
gema_alma = Objeto (200,1) "Gema del Alma"

gema_mente_vision = Tomado (Objeto (7,9) "Gema de la Mente") vision

gemas_sueltas = [gema_tiempo, gema_mente, gema_espacio, gema_realidad, gema_poder, gema_alma]

--una lista de los objetos gema pero en posesión de Thanos
gemas_en_thanos = map (\obj -> Tomado obj thanos) gemas_sueltas

-- Universos

universo_sin_thanos = universo_con [phil] [mjölnir]
universo_thanos_gana_solo = universo_con [thanos] gemas_en_thanos

universo_thanos_gana_stormbreaker = universo_con [thanos, thor] (stormbreaker : gemas_en_thanos)

universo_wanda_vision = (universo_con [thanos, vision, wanda] [gema_tiempo, gema_mente_vision, gema_espacio, gema_realidad, gema_poder, gema_alma])
universo_wanda_vision_sin_mente = (universo_con [thanos, vision, wanda] gemas_sueltas)


testsEj1 = test [ -- Casos de test para el ejercicio 1
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) phil             -- Caso de test 1 - expresión a testear
    ~=? 0                                                               -- Caso de test 1 - resultado esperado
  ,
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) (Muere phil)     -- Caso de test 2 - expresión a testear
    ~=? 1                                                               -- Caso de test 2 - resultado esperado
  ,
  foldPersonaje (\p s -> s ++ " en posicion " ++ show p) (\r d -> r ++ " se movio hacia " ++ (show d)) (\r -> r ++ ", se murio") (Muere (Mueve phil Norte))
    ~=?  "Phil en posicion (0.0,0.0) se movio hacia Norte, se murio"
  ,
  foldObjeto (\p s -> s ++ " en posicion " ++ show p) (\r p -> r ++ " fue tomado por " ++ (nombre_personaje p)) (\r -> r ++ ", fue destruido") (EsDestruido (mjölnir))
    ~=? "Mjölnir en posicion (2.0,2.0), fue destruido"
  ,
  foldObjeto (\p s -> s ++ " en posicion " ++ show p) (\r p -> r ++ " fue tomado por " ++ (nombre_personaje p)) (\r -> r ++ ", fue destruido") (EsDestruido (Tomado mjölnir phil))
    ~=? "Mjölnir en posicion (2.0,2.0) fue tomado por Phil, fue destruido"
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  posición_personaje phil       -- Caso de test 1 - expresión a testear
    ~=? (0,0)                   -- Caso de test 1 - resultado esperado
  ,
  posición_personaje (Mueve phil Norte)
    ~=? (0,1)
  ,
  posición_personaje (Mueve (Mueve phil Norte) Sur)
    ~=? (0,0)
  ,
  posición_personaje (Mueve phil Oeste)
    ~=? (-1,0)
  ,
  posición_personaje (Mueve (Muere (Mueve phil Norte)) Este)
    ~=? (1,1)
  ,
  nombre_objeto mjölnir
    ~=? "Mjölnir"
  ,
  nombre_objeto (Tomado mjölnir (Mueve phil Norte))
    ~=? "Mjölnir"
  ,
  nombre_objeto (EsDestruido (Tomado mjölnir (Mueve phil Norte)))
    ~=? "Mjölnir"
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  objetos_en []       -- Caso de test 1 - expresión a testear
    ~=? []            -- Caso de test 1 - resultado esperado
  ,
  personajes_en []
    ~=? []
  ,
  --los objetos deberían figurar ya sea que estén o no en manos de un personaje o destruidos
  objetos_en (universo_con [] gemas_sueltas)
    ~=? gemas_sueltas
  ,
  objetos_en (universo_thanos_gana_stormbreaker)
    ~=? (stormbreaker : gemas_en_thanos)
  ,
  objetos_en (universo_con [thanos] [Tomado (EsDestruido gema_poder) thanos])
    ~=? [Tomado (EsDestruido gema_poder) thanos]
  ,
  --ídem para los distintos constructores de personajes
  personajes_en universo_wanda_vision
    ~=? [thanos, vision, wanda]
  ,
  personajes_en (universo_con [Muere thanos, Mueve vision Norte] [])
    ~=? [Muere thanos, Mueve vision Norte]
  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  objetos_en_posesión_de "Phil" []       -- Caso de test 1 - expresión a testear
    ~=? []                             -- Caso de test 1 - resultado esperado
  ,
  objetos_en_posesión_de "Thanos" universo_thanos_gana_solo
    ~=? gemas_en_thanos
  ,
  objetos_en_posesión_de "Thor" (universo_con [thor, thanos, wanda] (stormbreaker_en_thor : gemas_en_thanos))
    ~=? [stormbreaker_en_thor]
  ]

-- Para el test de dos objetos a la misma distancia
mas_cercano_dos_iguales = objeto_libre_mas_cercano phil (universo_con [phil, thor, dr_strange] [mjölnir, escudo_capitán_américa, gema_mente, gema_poder])

testsEj5 = test [ -- Casos de test para el ejercicio 5
  objeto_libre_mas_cercano phil [Right mjölnir]       -- Caso de test 1 - expresión a testear
    ~=? mjölnir                                       -- Caso de test 1 - resultado esperado
  ,
  -- Todos los objetos están libres
  objeto_libre_mas_cercano wanda (universo_wanda_vision_sin_mente)
    ~=? gema_realidad
  ,
  -- Dos objetos a la misma distancia
  (elem mas_cercano_dos_iguales [escudo_capitán_américa, mjölnir]) ~=? True
  ,

  -- Un solo objeto libre
  objeto_libre_mas_cercano dr_strange (universo_con [thanos, dr_strange] (escudo_capitán_américa : gemas_en_thanos))
    ~=? escudo_capitán_américa
  ,
  --Algunos objetos libres
  objeto_libre_mas_cercano dr_strange (universo_con [thanos, vision, thor, dr_strange] [gema_poder, gema_alma, stormbreaker_en_thor, gema_mente_vision]) 
    ~=? gema_poder
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  tiene_thanos_todas_las_gemas universo_sin_thanos       -- Caso de test 1 - expresión a testear
    ~=? False                                            -- Caso de test 1 - resultado esperado
  ,
  tiene_thanos_todas_las_gemas (universo_con [thanos] gemas_sueltas)
    ~=? False
  ,
  tiene_thanos_todas_las_gemas (universo_con [thanos] [])
    ~=? False
  ,
  tiene_thanos_todas_las_gemas universo_thanos_gana_solo
    ~=? True
  ,
  tiene_thanos_todas_las_gemas universo_thanos_gana_stormbreaker
    ~=? True
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  podemos_ganarle_a_thanos universo_sin_thanos         -- Caso de test 1 - expresión a testear
    ~=? False                                          -- Caso de test 1 - resultado esperado
  ,
  podemos_ganarle_a_thanos universo_thanos_gana_solo
    ~=? False
  ,
  
  --thanos tiene que ganar si tiene las 6 gemas, indpendientemente de si Thor tiene stormbreaker
  podemos_ganarle_a_thanos universo_thanos_gana_stormbreaker
    ~=? False
  ,
  -- stormbreaker y Thor no tienen que estar necesariamente juntos para que tengamos la posiblidad de ganar
  podemos_ganarle_a_thanos (universo_con [thanos, thor] (stormbreaker : gemas_sueltas))
    ~=? True
  ,
  podemos_ganarle_a_thanos (universo_con [thanos, thor] (stormbreaker_en_thor : gemas_sueltas))
    ~=? True
  ,


  podemos_ganarle_a_thanos universo_wanda_vision
    ~=? True
  ,
  podemos_ganarle_a_thanos universo_wanda_vision_sin_mente
    ~=? False
  ]
