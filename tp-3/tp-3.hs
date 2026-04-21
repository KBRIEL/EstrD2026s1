data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show



celda1 :: Celda
celda1 = CeldaVacia

celda2= Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))

nroBolitas :: Color  ->   Celda      -> Int 
nroBolitas      c        CeldaVacia  =  0
nroBolitas      c (Bolita col celda) = unoColorSi c col + nroBolitas c celda

unoColorSi :: Color -> Color -> Int
unoColorSi    Azul    Azul   = 1
unoColorSi    Rojo    Rojo   = 1
unoColorSi    _       _      = 0

-------------------------------------------------------------------------
poner :: Color-> Celda-> Celda 
poner     c      cel = Bolita c cel
---------------------------------------------------------------------------
sacar :: Color-> Celda-> Celda
sacar    c       CeldaVacia = CeldaVacia
sacar    c       (Bolita col celda) = if mismoColor c col
                                        then celda
                                        else (Bolita col (sacar c celda))


mismoColor :: Color -> Color -> Bool
mismoColor    Azul    Azul   = True
mismoColor    Rojo    Rojo   = True
mismoColor    _       _      = False

-----------------------------------------------------------------------------------

ponerN :: Int-> Color-> Celda-> Celda
ponerN     0    c      cel  =  cel
ponerN     n    c      cel = ponerN (n-1) c (Bolita c cel)

-----------------------------------------------------------------------------------------

data Objeto = Cacharro | Tesoro 
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

cam1 = (Cofre [Tesoro] Fin)
cam2 = Fin
cam3 = Nada Fin
cam4 = (Cofre [Cacharro](Cofre [Tesoro](Nada Fin)))
cam5 = (Cofre [Cacharro, Tesoro](Cofre [Tesoro](Nada Fin)))
cam6 =  (Nada (Cofre [Tesoro] (Cofre [Tesoro] (Nada Fin))))
cam7 =  (Nada (Nada (Nada (Nada Fin))))
cam8 =  (Nada (Nada (Nada (Cofre [Tesoro] (Nada Fin)))))

hayTesoro :: Camino-> Bool 
hayTesoro      Fin    = False
hayTesoro    (Nada c) = False || hayTesoro c
hayTesoro     ( Cofre os c) = cofreConTesoro os  || hayTesoro c


cofreConTesoro :: [Objeto] -> Bool
cofreConTesoro     []      = False
cofreConTesoro     (o:os)  = esTesoro o || cofreConTesoro os

esTesoro :: Objeto -> Bool
esTesoro    Tesoro = True
esTesoro    _      = False


pasosHastaTesoro :: Camino-> Int 
--Precondición: tiene que haber al menos un tesoro
pasosHastaTesoro    Fin = 0
pasosHastaTesoro    (Nada c) = 1 + (pasosHastaTesoro c)
pasosHastaTesoro    (Cofre os c) = if (cofreConTesoro os)
                                    then  0
                                    else 1 + pasosHastaTesoro c

-------------------------------------------------------------------------
moverN :: Int -> Camino -> Camino
moverN     0      c      =  c
moverN     n      c      = moverN (n-1) ( mover c)

mover ::  Camino -> Camino
mover      Fin           = Fin
mover     (Nada c)       =  c
mover     (Cofre os c)   =  c

hayTesoroAqui :: Camino -> Bool
hayTesoroAqui    (Cofre os c)= cofreConTesoro os
hayTesoroAqui     _        = False

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn    n       ca     = hayTesoroAqui (moverN n ca )


 

sumarTesoros :: [Objeto] -> Int
sumarTesoros   []       = 0
sumarTesoros   (t:ts)   = if (esTesoro t) 
                            then 1 + sumarTesoros ts 
                            else sumarTesoros ts


alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros    0      _     =  True
alMenosNTesoros    n      Fin   =  False
alMenosNTesoros    n      (Cofre os c) = alMenosNTesoros (max 0 (n-(sumarTesoros os))) c
alMenosNTesoros    n      (Nada c)   = alMenosNTesoros n c
 
---------------------------------------------------------------------------------------------------
cantTesoroHasta :: Int -> Camino -> Int
cantTesoroHasta    0      _      = 0 
cantTesoroHasta    n     Fin       = 0
cantTesoroHasta    n    (Cofre os c) =  sumarTesoros os + cantTesoroHasta (n-1) c                                      
cantTesoroHasta    n    (Nada c)     = cantTesoroHasta (n-1) c

cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre     n     m     ca      = cantTesoroHasta m (moverN n ca)

                                                
----------------------------------------------------------------------------------------
--Tipos arbóreos--------------------------------------------------------
arbol0 = EmptyT
arbol1 :: Tree Int
arbol1 = NodeT 2
                (NodeT 3 
                        EmptyT 
                        EmptyT
                        ) 
                (NodeT 1 
                    (NodeT 2 
                            EmptyT 
                            EmptyT
                            )  
                    EmptyT
                    )
arbol2 :: Tree Int
arbol2 = NodeT 1 
                (NodeT 3 
                        EmptyT 
                        EmptyT
                        ) 
                (NodeT 2 
                        EmptyT 
                        EmptyT
                        )

arbol3 :: Tree Int
arbol3 = NodeT 1
                (NodeT 2 
                        (NodeT 4 
                                EmptyT 
                                EmptyT) 
                                
                        EmptyT
                        )
                (NodeT 3 
                        (NodeT 5 
                              EmptyT 
                              EmptyT)  
                        EmptyT
                    ) 


arbol4 :: Tree Int
arbol4 = NodeT 1
                (NodeT 2 
                        (NodeT 4 
                                EmptyT 
                                EmptyT) 
                                
                        EmptyT
                        )
                (NodeT 3 
                        (NodeT 5 
                              EmptyT 
                              EmptyT)  
                       ( NodeT 6
                              (NodeT 7 
                                    EmptyT 
                                    EmptyT)  
                                EmptyT)
                                   
                    ) 
--Árboles binarios-----------------------------------------------------
   
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

sumarT :: Tree Int -> Int
sumarT        EmptyT    = 0
sumarT        (NodeT n tl tr)     = n + sumarT (tl) + sumarT (tr)

-------------------------------------------------------------------------

sizeT :: Tree a -> Int
sizeT        EmptyT    = 0
sizeT        (NodeT a tl tr)     = 1 + sizeT (tl) + sizeT (tr)

-------------------------------------------------------------------------


mapDobleT :: Tree Int -> Tree Int
mapDobleT        EmptyT    = EmptyT
mapDobleT        (NodeT n tl tr)     = NodeT (n * 2) (mapDobleT tl) (mapDobleT tr)


-------------------------------------------------------------------------

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT        n  EmptyT          = False
perteneceT        n  (NodeT n1 tl tr) = (n == n1)  || (perteneceT n tl) || (perteneceT n tr)


-------------------------------------------------------------------------

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT        a EmptyT    = 0
aparicionesT        a (NodeT b tl tr)     = if a == b 
                                                then 1 + aparicionesT a tl + aparicionesT a tr 
                                                else aparicionesT a tl + aparicionesT a tr 

-------------------------------------------------------------------------

esHoja :: Tree a -> Bool
esHoja     EmptyT = True
esHoja     _      = False

leaves :: Tree a -> [a]
leaves        EmptyT    = []
leaves        (NodeT a tl tr)     = if esHoja tl && esHoja tr
                                     then [a] 
                                     else leaves tl ++ leaves tr

-------------------------------------------------------------------------


heightT :: Tree a -> Int
heightT     EmptyT  = 0
heightT     (NodeT a tl tr)     = 1 + max (heightT tl) (heightT tr)

-------------------------------------------------------------------------

mirrorT :: Tree a -> Tree a
mirrorT     EmptyT    = EmptyT
mirrorT     (NodeT a tl tr)     = NodeT a  (mirrorT tr) (mirrorT tl)

-------------------------------------------------------------------------

toList :: Tree a -> [a]
toList    EmptyT = []
toList    (NodeT a tl tr)     =  toList tl ++ [a] ++ toList tr

-------------------------------------------------------------------------

levelN :: Int -> Tree a -> [a]
levelN     _      EmptyT          =  [] 
levelN     0     (NodeT x _  _ )  =  x : []
levelN     n     (NodeT _ t1 t2)  =  levelN (n-1) t1 ++ levelN (n-1) t2

-------------------------------------------------------------------------

nivel :: [[a]] -> [[a]] -> [[a]]
nivel []       ys      = ys
nivel xs      []       = xs
nivel (x:xs) (y:ys) = (x ++ y) : nivel xs ys

listPerLevel :: Tree a -> [[a]]
listPerLevel    EmptyT  = []
listPerLevel    (NodeT x t1 t2) = [x] : nivel (listPerLevel t1) (listPerLevel t2)

-------------------------------------------------------------------------


ramaMasLarga :: Tree a -> [a]
ramaMasLarga    EmptyT  = []
ramaMasLarga    (NodeT x tl tr) = if length (ramaMasLarga tl) > length (ramaMasLarga tr)
                                     then  x: ramaMasLarga tl 
                                     else  x: ramaMasLarga tr

-------------------------------------------------------------------------

consACada :: a -> [[a]] -> [[a]]
consACada    x    []       = []
consACada    x   (xs:xss) = (x:xs) : consACada x xss

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos    EmptyT  = []
todosLosCaminos    (NodeT x t1 t2) =[x] : consACada x (todosLosCaminos t1) ++ consACada x (todosLosCaminos t2)



--Expresiones Aritméticas-------------------------------------------------------------------------
data ExpA = Valor Int
                    | Sum ExpA ExpA
                    | Prod ExpA ExpA
                    | Neg ExpA    deriving Show




eval :: ExpA -> Int
eval    (Valor n ) =  n
eval    (Sum  n m) = eval n + eval m 
eval    (Prod n m) = eval n * eval m
eval    (Neg   n ) = - eval n 


-------------------------------------------------------------------------

simplificar ::  ExpA -> ExpA
simplificar (Valor x) =  Valor x 
simplificar (Sum x y) = simplificarSum (simplificar x) (simplificar y)
simplificar (Prod x y) = simplificarProd (simplificar x) (simplificar y)
simplificar (Neg x) = simplificarNeg (simplificar x) 

simplificarSum :: ExpA -> ExpA -> ExpA
simplificarSum    (Valor 0) ex = ex
simplificarSum    ex (Valor 0) = ex
simplificarSum    ex1 ex2      = Sum ex1 ex2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd    (Valor 0) ex  = Valor 0 
simplificarProd    ex (Valor 0)  = Valor 0
simplificarProd    ex (Valor 1)  = ex 
simplificarProd      (Valor 1) ex = ex
simplificarProd    ex1      ex2  = Prod ex1 ex2



simplificarNeg :: ExpA -> ExpA
simplificarNeg    (Neg  ex) = ex
simplificarNeg    ex        = Neg ex



