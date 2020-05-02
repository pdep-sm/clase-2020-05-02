module Library where
import PdePreludat
-- Clase 05 (parapaaaaaaa parapaaaaaaa) Jurassic Park - La venganza
trex = Carnivoro "TRex" 6 10000 10
triceratops = Herbivoro "Triceratops" 3 8000
velociraptor = Carnivoro "Velociraptor" 2 4500 6

dinos = [trex, triceratops, velociraptor]

data Dinosaurio = Carnivoro {
        nombre :: String,
        altura :: Metros,
        peso :: Kg,
        cantidadDientes :: Number
} | Herbivoro {
    nombre :: String,
    altura :: Metros,
    peso :: Kg
} deriving Show

type Kg = Number
type Metros = Number

esCarnivoro (Carnivoro _ _ _ _ ) = True
esCarnivoro _ = False

esHerbivoro (Herbivoro _ _ _ ) = True
esHerbivoro _ = False

carnivoros  = filter esCarnivoro

type DinoAccion = Dinosaurio -> Dinosaurio
--1 Alimentamos un dinosaurio y sube 30kg de peso
alimentar :: DinoAccion
alimentar dino = dino{ peso = peso dino + 30 }

--2 Hacemoos trotar unos kilometros un dinosaurio, pierde kg por km, hasta 10kg 
trotar :: Number -> DinoAccion
trotar km dino = dino{ peso = max 10 (peso dino - km) }

--3 irAlDentista hace que el carnivoro pierda dientes, al herbivoro no le pasa nada
irAlDentista dino@(Carnivoro _ _ _ _ ) = dino{ cantidadDientes = cantidadDientes dino - 1} 
irAlDentista dino@(Herbivoro _ _ _ ) = dino

--4 Hacer que un dinosaurio realice acciones
realizar acciones dino = foldl (flip ($)) dino acciones
realizar' acciones dino = foldr ($) dino acciones
realizar'' acciones dino = (foldl (.) id acciones) dino
--funcionFold dino accion => accion dino


-- 5 - Hacer un torneo de dinosaurios donde los dinosaurios combatan y quede un ganador
-- sabiendo que:
-- h1 vs h2 => h1
-- c vs h = c
-- c1 vs c2 => carnivoro c/mayor cantidadDientes

combatir :: Dinosaurio -> Dinosaurio -> Dinosaurio
combatir herbivoro@(Herbivoro _ _ _) (Herbivoro _ _ _) = herbivoro

combatir carnivoro@(Carnivoro _ _ _ _) (Herbivoro _ _ _) = carnivoro
combatir (Herbivoro _ _ _) carnivoro@(Carnivoro _ _ _ _) = carnivoro

combatir c1 c2
    | cantidadDientes c1 > cantidadDientes c2 = c1
    | otherwise = c2


torneo = foldl1 combatir  
--es lo mismo en este caso usar foldr1




------------------------------------
multiplicar n numeros = (map ($n). map (*)) numeros
--multiplicar n numeros = map (n*) numeros
-- ($) n (*5) -- NO FUNCIONA
    -- flip ($) n (*5) -- SI FUNCIONA
-- ($) (*5) n -- SI FUNCIONA
-- ($5) (*4) -- SI FUNCIONA
-- (5$) (*4) -- NO FUNCIONA

aplicarFuncion :: (a -> b) -> a -> b
aplicarFuncion funcion parametro = funcion parametro
--($) funcion parametro = funcion parametro

-- (f . g . c) parametro
-- f . g . c `aplicarFuncion` parametro
-- f . g . c $ parametro