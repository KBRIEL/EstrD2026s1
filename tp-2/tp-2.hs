
sumatoria :: [Int] -> Int
sumatoria     []   = 0
sumatoria    (x:xs)= x + sumatoria xs

longitud :: [a]  -> Int
longitud     []  =  0
longitud  (x:xs) =  1 + longitud xs

sucesores :: [Int]  -> [Int]
sucesores      []   =   []
sucesores    (x:xs) =  (x+1): sucesores xs

conjuncion :: [Bool]-> Bool
conjuncion      []   = True
conjuncion    (x:xs) = x && conjuncion xs

disyuncion :: [Bool]-> Bool
disyuncion      []   = False
disyuncion    (x:xs) = x || disyuncion xs 

aplanar :: [[a]]-> [a]
aplanar    []    = []
aplanar   (x:xs) = x ++ aplanar xs 

aplanar2 :: [[a]]-> [a]
aplanar2    []    = []
aplanar2   (x:xs) = agregar x  (aplanar2 xs) 

pertenece :: Eq a => a-> [a]-> Bool 
pertenece            a   []  = False
pertenece            a   (x:xs) =  a == x || pertenece a xs

apariciones :: Eq a => a ->  [a]  -> Int 
apariciones            a     []   =  0
apariciones            a   (x:xs) =  if a == x
                                      then 1 + apariciones a xs
                                      else     apariciones a xs

losMenoresA :: Int-> [Int]-> [Int] 
losMenoresA     n    []  = []
losMenoresA     n    (x:xs) = if n > x
                                then x: losMenoresA n xs
                                else losMenoresA n xs


lasDeLongitudMayorA :: Int-> [[a]]-> [[a]]
lasDeLongitudMayorA     n     []     = []
lasDeLongitudMayorA     n    (xs:xss) = if longitud xs > n
                                            then xs : lasDeLongitudMayorA n xss
                                            else lasDeLongitudMayorA n xss


agregarAlFinal :: [a]-> a-> [a]
agregarAlFinal [] a = a:[]
agregarAlFinal (x:xs) a = x : agregarAlFinal xs a
 

agregar :: [a]-> [a]-> [a]
agregar     []    ys =  ys
agregar   (x:xs)  ys = x: agregar xs ys 


reversa :: [a]-> [a]
reversa    [] =  []
reversa    (x:xs) = agregar (reversa xs)  [x]
                                  

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos     []        ys  = ys
zipMaximos     xs       []   = xs
zipMaximos     (x:xs)   (y:ys) = maximoDe x y : zipMaximos xs ys

maximoDe :: Int -> Int -> Int
maximoDe      x     y  =  if x > y
                            then x
                            else y

elMinimo :: Ord a => [a] -> a
--no debe haber un lista vacia
elMinimo       []   = error "El mínimo de una lista vacía es error"
elMinimo   (x:[])   = x  
elMinimo   (x:xs)   = if x < (elMinimo xs)
                                 then x   
                                 else (elMinimo xs)   


--- Recursión sobre números

factorial :: Int-> Int
factorial    0    = 1
factorial    n    =  factorial (n-1) * n

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if n < 1 
                      then [] 
                      else n : cuentaRegresiva (n-1)


repetir :: Int-> a-> [a]
repetir     0    e  = []
repetir     n    e  = e : repetir (n-1) e


losPrimeros :: Int-> [a]-> [a]
losPrimeros     0     xs  = []
losPrimeros     n     []  = []
losPrimeros     n  (x:xs) = x: losPrimeros (n-1) xs

sinLosPrimeros :: Int-> [a]-> [a]
sinLosPrimeros     0    xs  = xs  
sinLosPrimeros     n    []  = []
sinLosPrimeros     n (x:xs) = sinLosPrimeros (n-1) xs


-- Registros

---------info -------
data Persona = P String Int 
                deriving Show

juan :: Persona  
juan = P "juan" 24
maria :: Persona  
maria = P "maria" 42      
pedro :: Persona  
pedro = P "pedro" 44
ana :: Persona  
ana = P "ana" 22     

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = PK TipoDePokemon Int
data Entrenador = E String [Pokemon]

entrenador1 :: Entrenador
entrenador1 = E "Nico" [pok1, pok2]
entrenador2 :: Entrenador
entrenador2 = E "Gus" [pok2, pok3]
entrenador3 :: Entrenador
entrenador3 = E "Isra" [pok1, pok4]
entWin :: Entrenador
entWin = E "winner" [pok1, pok1, pok4, pok2, pok3]
entLose :: Entrenador
entLose = E "loser" [pok2, pok2]
entMix :: Entrenador
entMix = E "loser" [pok2, pok3]

pok1 :: Pokemon 
pok1 = PK Agua 100 
pok2 :: Pokemon 
pok2 = PK Fuego 80
pok3 :: Pokemon 
pok3 = PK Planta 90 
pok4 :: Pokemon 
pok4 = PK Agua 70
------------------------------------------------------------
----funciones aux ----------
edad :: Persona -> Int
edad     (P n e) = e

edades :: [Persona]-> [Int]
edades    []       = []
edades    (x:xs)   = (edad x): edades xs

dameElTipo :: Pokemon -> TipoDePokemon
dameElTipo   (PK tp ent) = tp
                                               

mismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
mismoTipo    Agua              Agua         = True
mismoTipo    Fuego             Fuego        = True
mismoTipo    Planta            Planta       = True
mismoTipo    _                 _            = False   

tipoDePokemonEsSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
tipoDePokemonEsSuperior Agua  Fuego  = True
tipoDePokemonEsSuperior Fuego Planta = True
tipoDePokemonEsSuperior Planta Agua  = True
tipoDePokemonEsSuperior _       _    = False

superaA :: Pokemon -> Pokemon -> Bool
superaA    p1          p2     = tipoDePokemonEsSuperior (dameElTipo p1) (dameElTipo p2 )
------------------------------------------------------------

mayoresA :: Int-> [Persona]-> [Persona]
-- la edad no puede ser negativa 
mayoresA    n      []      =  []
mayoresA    n      (x:xs)  =  if  (edad x) > n 
                                then x: mayoresA n xs
                                else mayoresA n xs

promedioEdad :: [Persona]-> Int
-- debe haber al menos una persona en la lista
promedioEdad     (x:[])      = edad x
promedioEdad     xs          = div (sumatoriaEdades xs)  (length xs)

sumatoriaEdades :: [Persona] -> Int
sumatoriaEdades     []       = 0
sumatoriaEdades     (x:xs)   = edad x + sumatoriaEdades xs

elMasViejo :: [Persona]-> Persona
elMasViejo    (x:[])    = x
elMasViejo    (x:xs)    = if (edad x) > edad (elMasViejo xs)
                            then x
                            else elMasViejo xs
----------------------------------------------------------------------------------------------------------------

cantPokemon :: Entrenador-> Int
cantPokemon   (E n ps)   = longitud ps

-----------------------------------------------------------------------------------------------------------
cantPokemonDe :: TipoDePokemon-> Entrenador-> Int
cantPokemonDe          tp        (E n ps)  = soloDelMismoTipo tp ps

soloDelMismoTipo :: TipoDePokemon -> [Pokemon] -> Int
soloDelMismoTipo      tp                []    =  0
soloDelMismoTipo      tp              (p:ps)  =  if mismoTipo tp (dameElTipo p) 
                                                    then 1 + soloDelMismoTipo tp ps
                                                    else soloDelMismoTipo tp ps
-----------------------------------------------------------------------------------------------------

lesGananALaListaDelEntrenador :: TipoDePokemon -> [Pokemon] -> Entrenador -> Int
lesGananALaListaDelEntrenador   tp  []          e          = 0
lesGananALaListaDelEntrenador   tp  (p:ps)      e          = if leGanaATodos tp p e
                                                then 1 + lesGananALaListaDelEntrenador tp ps e
                                                else lesGananALaListaDelEntrenador tp ps e

leGanaATodos :: TipoDePokemon -> Pokemon -> Entrenador -> Bool
leGanaATodos      tp             p          (E n ps)   = esGanador tp p ps

esGanador ::  TipoDePokemon -> Pokemon -> [Pokemon] ->Bool
esGanador          tp          p1        []     = True
esGanador          tp          p1        (p:ps)  =  if mismoTipo tp ( dameElTipo p1)
                                                      then (superaA p1 p)  && esGanador tp p1 ps 
                                                      else  False
                                                                         

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon-> Entrenador-> Entrenador-> Int
cuantosDeTipo_De_LeGananATodosLosDe_    tp               (E n ps)     e         = lesGananALaListaDelEntrenador tp ps e
                                                                            
-----------------------------------------------------------------------------------------------------------------------------------------------

esDeTipo :: TipoDePokemon -> Pokemon -> Bool
esDeTipo        tp1           (PK tp n) = mismoTipo tp1 tp

tieneTipoDe :: TipoDePokemon -> [Pokemon] -> Bool
tieneTipoDe     tipo [] = False
tieneTipoDe     tipo (p:ps) = esDeTipo  tipo p || tieneTipoDe tipo ps

esMaestroPokemon :: Entrenador-> Bool
esMaestroPokemon    (E n ps)   = tieneTipoDe Agua ps && tieneTipoDe Fuego ps && tieneTipoDe Planta ps

---------------------------------------------------------------------------------------------------------------------------------

data Seniority = Junior | SemiSenior | Senior deriving Show
data Proyecto = ConsProyecto String deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
data Empresa = ConsEmpresa [Rol] deriving Show

proyecto1 :: Proyecto
proyecto1 = ConsProyecto "proyecto1"
proyecto2 :: Proyecto
proyecto2 = ConsProyecto "proyecto2"
proyecto3 :: Proyecto
proyecto3 = ConsProyecto "proyecto3"

rol1 :: Rol
rol1 = Developer Junior proyecto1
rol2 :: Rol
rol2 = Developer Junior proyecto2
rol3 :: Rol
rol3 = Developer Senior proyecto3
rol4 :: Rol
rol4 = Developer Senior proyecto3

empresa1 :: Empresa
empresa1 = ConsEmpresa [rol1, rol2]
empresa2 :: Empresa
empresa2 = ConsEmpresa [rol1, rol3]
empresa3 :: Empresa
empresa3 = ConsEmpresa [rol1, rol2, rol3]
empresa4 :: Empresa
empresa4 = ConsEmpresa [rol1, rol2, rol3, rol4]
empRep :: Empresa
empRep = ConsEmpresa [rol1, rol2, rol1, rol4]


dameElNombre :: Proyecto -> String
dameElNombre   (ConsProyecto s)= s

contiene :: [Char] -> [Proyecto] -> Bool
contiene            a     []    =   False
contiene            a    (x:xs) =   (a == dameElNombre x )|| contiene a xs



proyectosSinRepetir ::[Proyecto]-> [Proyecto]
proyectosSinRepetir [] = []
proyectosSinRepetir   (x:xs) = if contiene (dameElNombre x) (proyectosSinRepetir xs)
                            then proyectosSinRepetir xs
                            else x: proyectosSinRepetir xs 


listaDeRolesAListaDeProyectos :: [Rol] -> [Proyecto]
listaDeRolesAListaDeProyectos    [] = []
listaDeRolesAListaDeProyectos    (r:rs) = proyectoDelRol r : listaDeRolesAListaDeProyectos rs

proyectoDelRol ::   Rol           -> Proyecto
proyectoDelRol    (Developer s p) = p
proyectoDelRol    (Management s p) = p

proyectos ::    Empresa        -> [Proyecto]
proyectos    (ConsEmpresa rs)  =   proyectosSinRepetir (listaDeRolesAListaDeProyectos rs) 

-----------------------------------------------------------------------------------------------------------


estaEnAlmenosUnProyecto :: Rol -> [Proyecto]-> Bool
estaEnAlmenosUnProyecto    r      []        = False
estaEnAlmenosUnProyecto    r      (p:ps)   = dameElNombre (proyectoDelRol r) == dameElNombre p || estaEnAlmenosUnProyecto r ps

esSr :: Seniority -> Bool
esSr    Senior    = True
esSr    _         = False

seniorityDelRol ::   Rol           -> Seniority
seniorityDelRol    (Developer s p) = s
seniorityDelRol   (Management s p) = s

esDeveloper :: Rol -> Bool
esDeveloper     (Developer s p)= True
esDeveloper    _               = False


quienesEstanEnLosProyectos :: [Rol] -> [Proyecto] -> Int
quienesEstanEnLosProyectos      []     ps       = 0
quienesEstanEnLosProyectos    (r:rs)   ps       = if estaEnAlmenosUnProyecto r ps && esSr (seniorityDelRol r) && esDeveloper r
                                                    then  1 + quienesEstanEnLosProyectos rs ps
                                                    else quienesEstanEnLosProyectos rs ps


losDevSenior :: Empresa-> [Proyecto]-> Int 
losDevSenior  (ConsEmpresa rs)  ps  =  quienesEstanEnLosProyectos rs ps
--------------------------------------------------------------------------------------------

quienesSonDeLosProyectos :: [Rol] -> [Proyecto] -> Int
quienesSonDeLosProyectos      []     ps       = 0
quienesSonDeLosProyectos    (r:rs)   ps       = if estaEnAlmenosUnProyecto r ps
                                                    then  1 + quienesSonDeLosProyectos rs ps
                                                    else quienesSonDeLosProyectos rs ps

cantQueTrabajanEn :: [Proyecto]-> Empresa-> Int 
cantQueTrabajanEn    ps      (ConsEmpresa rs)     =    quienesSonDeLosProyectos rs ps 

----------------------------------------------------------------------------------------------------


asignadosPorProyecto :: Empresa-> [(Proyecto, Int)]
asignadosPorProyecto   (ConsEmpresa rs) = cantRolesPorProyecto rs

cantRolesPorProyecto :: [Rol] -> [(Proyecto, Int)]
cantRolesPorProyecto    []   = []
cantRolesPorProyecto     (r:rs) = if estaEnLista r  rs
                                    then  (armarTupla (dameElProyecto r)rs): cantRolesPorProyecto (eliminarDeLaLista r rs)
                                    else ((dameElProyecto r), 1) : cantRolesPorProyecto rs


dameLaCant :: Proyecto -> [Rol] -> Int
dameLaCant      p1          []       = 0
dameLaCant      p1          (r:rs)   =  if (dameElNombre p1) == dameElNombre (dameElProyecto r)
                                               then 1 + dameLaCant p1 rs
                                               else dameLaCant p1 rs

armarTupla :: Proyecto -> [Rol] -> (Proyecto, Int)
armarTupla     p           rs= (p, (dameLaCant p rs)+1)

eliminarDeLaLista :: Rol -> [Rol] -> [Rol]
eliminarDeLaLista    r1      []  = []
eliminarDeLaLista    r1      (r:rs )= if dameElNombre (dameElProyecto r1) == dameElNombre (dameElProyecto r)
                                        then eliminarDeLaLista r1 rs
                                        else r: eliminarDeLaLista r1 rs

estaEnLista :: Rol ->  [Rol]  -> Bool 
estaEnLista    r1       []    = False
estaEnLista    r1     (r:rs)  =  (dameElNombre (dameElProyecto r1)) == dameElNombre(dameElProyecto r) || estaEnLista r1 rs                         

dameElProyecto :: Rol -> Proyecto
dameElProyecto (Developer s p) = p
dameElProyecto (Management s p) = p


