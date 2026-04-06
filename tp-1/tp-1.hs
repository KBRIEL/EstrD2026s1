-- practica 1

-- Números enteros

sucesor :: Int -> Int
sucesor     n = n+1


sumar :: Int -> Int -> Int
--Dados dos números devuelve su suma utilizando la operación +
sumar n m = n+m


divisionYResto :: Int -> Int -> (Int, Int)
-- Dado dos números, devuelve un par donde la primera componente es la división del
-- primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
-- para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
-- provista por Haskell.

divisionYResto n m = (div n m, mod n m)

maxDelPar :: (Int,Int) -> Int
--Dado un par de números devuelve el mayor de estos.
maxDelPar (a,b) = if a > b 
                  then a    
                  else b


{-4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresión a todas las funciones del punto anterior.
Ejemplo: maxDePar (divisionYResto (suma 5 5) (sucesor 0))

-- sumar (maxDelPar (divisionYResto 20 (sucesor 3))) (sucesor 4)
-- maxDelPar (divisionYResto (sumar (sucesor 9) (sucesor 10)) (sucesor 1))
-- sucesor (maxDelPar (divisionYResto 81 (sumar 5 4)))
-- sucesor (maxDelPar (divisionYResto 63 (sumar 6 1)))
-}


{-Tipos enumerativos-}

data Dir = Norte |Sur |Este| Oeste deriving Show

opuesto :: Dir -> Dir
--Dada una dirección devuelve su opuesta.
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este


iguales :: Dir -> Dir -> Bool
--Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False


siguiente :: Dir -> Dir
{--Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
la siguiente dirección a Oeste. Posee una precondición esta función? Es una función
total o parcial? Por qué?-}
siguiente Norte = Este
siguiente Este = Sur 
siguiente Sur = Oeste

-- Rta: no posee precondicion, porque la función cubre todas las posibilidades de Dir, es una función total.


data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo 
                   deriving Show

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
-- Devuelve un par donde la primera componente es el primer día de la semana, y la
-- segunda componente es el último día de la semana. Considerar definir subtareas útiles
-- que puedan servir después.
primeroYUltimoDia = (primerDiaDeLaSemana, ultimoDiaDeLaSemana)

primerDiaDeLaSemana :: DiaDeSemana
primerDiaDeLaSemana = Lunes

ultimoDiaDeLaSemana :: DiaDeSemana
ultimoDiaDeLaSemana = Domingo



empiezaConM :: DiaDeSemana -> Bool
--Dado un día de la semana indica si comienza con la letra M.
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False


vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
-- Dado dos días de semana, indica si el primero viene después que el segundo. Analizar
-- la calidad de la solución respecto de la cantidad de casos analizados (entre los casos
-- analizados en esta y cualquier subtarea, deberían ser no más de 9 casos).
-- Ejemplo: vieneDespues Jueves Lunes = True
vieneDespues d1 d2 = valorXDia d1 > valorXDia d2

valorXDia :: DiaDeSemana-> Int
valorXDia Lunes = 1
valorXDia Martes = 2
valorXDia Miercoles = 3
valorXDia Jueves = 4
valorXDia Viernes = 5
valorXDia Sabado = 6
valorXDia Domingo = 7



estaEnElMedio :: DiaDeSemana -> Bool
-- Dado un día de la semana indica si no es ni el primer ni el ultimo dia.
estaEnElMedio d = not (esPrimerDiaDeSemana d || esUltimoDiaDeSemana d)

esPrimerDiaDeSemana :: DiaDeSemana -> Bool
esPrimerDiaDeSemana Lunes  = True
esPrimerDiaDeSemana _ = False

esUltimoDiaDeSemana :: DiaDeSemana -> Bool
esUltimoDiaDeSemana Domingo  = True
esUltimoDiaDeSemana _ = False


negar :: Bool -> Bool
-- Dado un booleano, si es True devuelve False, y si es False devuelve True.
negar True = False
negar False = True 


oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien False b = b

yTambien :: Bool -> Bool -> Bool
yTambien True b = b
yTambien False _ = False

implica :: Bool -> Bool -> Bool
implica True b = b
implica False _ = True


{-  Registros  -}

-- Definnir el tipo de dato Persona, como un nombre y la edad de la persona. 
data Persona = P String Int
               deriving Show

nombre :: Persona -> String
-- Devuelve el nombre de una persona
nombre (P n e) = n


edad :: Persona -> Int
-- Devuelve la edad de una persona
edad (P n e) = e


crecer :: Persona -> Persona
-- Aumenta en uno la edad de la persona.
crecer (P n e) = P n (e + 1)


cambioDeNombre :: String -> Persona -> Persona
-- Dados un nombre y una persona, devuelve una persona con la edad de la persona y el
-- nuevo nombre.
cambioDeNombre m (P n e) = (P m e)

esMayorQueLaOtra :: Persona -> Persona -> Bool
-- Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra p1 p2 = edad p1 > edad p2


laQueEsMayor :: Persona -> Persona -> Persona
-- Dadas dos personas devuelve a la persona que sea mayor.
laQueEsMayor p1 p2 = if (edad p1 > edad p2) 
                     then p1
                     else p2 
-- Personas --
j :: Persona
j = P "Juan" 25
p :: Persona
p = P "Pepe" 95 


-- Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
-- porcentaje de energía; y Entrenador, como un nombre y dos Pokémon. 
data Pokemon = PK TipoDePokemon Int deriving Show
data TipoDePokemon = Agua | Fuego | Planta deriving Show 
data Entrenador = E String Pokemon Pokemon deriving Show

superaA :: Pokemon -> Pokemon -> Bool
-- Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. 
superaA pk1 pk2 = esSuperiorTipo (tipoDePokemon pk1) (tipoDePokemon pk2)

tipoDePokemon :: Pokemon -> TipoDePokemon
-- Dado un Pokemon, devuelve su TipoDePokemon
tipoDePokemon (PK tp _) = tp

esSuperiorTipo :: TipoDePokemon -> TipoDePokemon -> Bool
--Dados dos TipoDePokemon si el primero supera al segundo.
-- Agua supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
esSuperiorTipo Agua Fuego  = True
esSuperiorTipo Fuego  Planta = True
esSuperiorTipo Planta Agua   = True
esSuperiorTipo _      _      = False

--Pokemones--
pk1 :: Pokemon
pk1 = PK Agua 35

pk2 :: Pokemon
pk2 = PK Fuego 55

pk3 :: Pokemon
pk3 = PK Planta 28

pk4 :: Pokemon
pk4 = PK Planta 2

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
-- Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantidadDePokemonDe tp (E _ pk1 pk2) = (sumarSiEsIgualTipo tp pk1) + (sumarSiEsIgualTipo tp pk2)

sumarSiEsIgualTipo :: TipoDePokemon -> Pokemon -> Int
-- Dados un TipoDePokemon y un Pokemon, devuelve uno si son iguales, sino devuelve 0
sumarSiEsIgualTipo tp1 pk = if (esIgualTipo tp1 (tipoDePokemon pk))
                            then 1
                            else 0 

esIgualTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esIgualTipo Agua   Agua   = True
esIgualTipo Fuego  Fuego  = True
esIgualTipo Planta Planta = True
esIgualTipo _      _      = False

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
-- Dado un par de entrenadores, devuelve a sus Pokémon en una lista.
juntarPokemon (e1, e2) = (pokemones e1) ++ (pokemones e2) 

pokemones :: Entrenador ->  [Pokemon]
pokemones (E _ pk1 pk2) = pk1 : pk2: []

-- Entrnadores --
entrenador1 = E "Jose" pk1 pk2
entrenador2 = E "Pepe" pk3 pk4

-- Funciones polimórfincas--

loMismo :: a -> a
-- Dado un elemento de algún tipo devuelve ese mismo elemento.
loMismo a = a

siempreSiete :: a -> Int
-- Dado un elemento de algún tipo devuelve el número 7.
siempreSiete x = 7

swap :: (a,b) -> (b, a)
-- Dadas una tupla, invierte sus componentes.
swap (x,y) = (y,x)

-- Por qué existen dos variables de tipo diferentes?
-- Rta: Cada letra representa un tipo de dato diferente 
-- Responda la siguiente pregunta: Por qué estas funciones son polimórficas?
-- Rta: porque funcionan para cualquier tipo de dato

estaVacia :: [a] -> Bool
-- Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.
estaVacia [] = True
estaVacia (_:_) = False

elPrimero :: [a] -> a
-- Dada una lista devuelve su primer elemento.
elPrimero xs = head xs

elPrimero' :: [a] -> a
-- Dada una lista devuelve su primer elemento.
elPrimero' (x:xs) = x
-- la lista no tiene que estar vacia

sinElPrimero :: [a] -> [a]
-- Dada una lista devuelve esa lista menos el primer elemento.
sinElPrimero [] = []
sinElPrimero (x:xs) = xs
-- la lista no tiene que estar vacia
splitHead :: [a] -> (a, [a])
-- Dada una lista devuelve un par, donde la primera componente es el primer 
-- elemento de la lista, y la segunda componente es esa lista pero sin el primero. 
-- Nota: tener en cuenta que el constructor de listas es :
splitHead  (x:xs) = (x, xs)
-- la lista no tiene que estar vacia