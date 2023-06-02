
-- Defino nuevo tipo de dato Figura, la cual contiene informacion sobre una figura geometrica

data Forma = Triangulo | Circulo | Cuadrado | Rombo deriving (Show, Eq)
data Color = Rojo | Amarillo | Azul | Verde deriving (Show, Eq) --Significa que derive de Eq para usar ==, y de Show para poder mostrarlo por pantalla
type Figura = (Forma, Color, Int) --Una Figura es una tupla de tres elementos (ternas)

-- 1 Asociar diferentes predicados que indiquen el color y la forma

rojo :: Figura -> Bool
rojo (f,c,t) = c == Rojo

azul :: Figura -> Bool
azul (f,c,t) = c == Azul

amarillo:: Figura -> Bool
amarillo (f,c,t) = c == Amarillo

verde :: Figura -> Bool
verde (f,c,t) = c == Verde

circulo :: Figura -> Bool
circulo (f,c,t) = f == Circulo

rombo :: Figura -> Bool
rombo (f,c,t) = f == Circulo

cuadrado :: Figura -> Bool
cuadrado (f,c,t) = f == Cuadrado

triangulo :: Figura -> Bool
triangulo (f,c,t) = f == Triangulo

-- 2 Defini la funcion tam :: Figura -> Int, que dada una figura devuelve su tamaño

tam :: Figura -> Int
tam (f,c,t) = t

-- 4 P\cada propiedad del 3 da un ej de una lista xs que la cumpla y un ej de una lista xs′ que no la cumpla

-- Todas las figuras de xs son rojas.
xsa :: [Figura]
xsa = [(Triangulo,Rojo,5 ),(Cuadrado,Rojo,10),(Circulo,Rojo,2)]
xsA :: [Figura]
xsA = [(Cuadrado,Azul,15),(Circulo,Rojo,1),(Triangulo,Azul,2)]

-- Todas las figuras de xs son de tamaño menor a 5.
xsb :: [Figura]
xsb = [(Triangulo,Rojo,4 ),(Cuadrado,Rojo,2),(Circulo,Rojo,3)]
xsB :: [Figura]
xsB = [(Cuadrado,Azul,15),(Circulo,Rojo,1),(Triangulo,Azul,23)]

-- Todos los triangulos de xs son rojos
xsc :: [Figura]
xsc = [(Triangulo,Rojo,5 ),(Cuadrado,Rojo,10),(Triangulo,Rojo,7)]
xsC :: [Figura]
xsC = [(Triangulo,Azul,15),(Circulo,Rojo,1),(Triangulo,Rojo,2)]

-- Existe un cuadrado verde en xs.
xsd :: [Figura]
xsd = [(Cuadrado,Verde,5 ),(Cuadrado,Rojo,10),(Triangulo,Rojo,7)]
xsD :: [Figura]
xsD = [(Triangulo,Azul,15),(Cuadrado,Rojo,1),(Triangulo,Rojo,2)]

-- Todos los c´ırculos de xs son azules y de tama˜no menor a 10.
xse :: [Figura]
xse = [(Circulo,Azul,5 ),(Cuadrado,Rojo,10),(Circulo,Azul,9)]
xsE :: [Figura]
xsE = [(Circulo,Azul,76 ),(Cuadrado,Rojo,10),(Circulo,Rojo,9)]

--  Ningun triangulo de xs es azul.
xsf :: [Figura]
xsf = [(Cuadrado,Verde,5 ),(Triangulo,Verde,10),(Triangulo,Rojo,7)]
xsF :: [Figura]
xsF = [(Triangulo,Azul,15),(Cuadrado,Rojo,1),(Triangulo,Rojo,2)]

-- En xs no hay c´ırculos amarillos ni verdes.
xsg :: [Figura]
xsg = [(Circulo,Azul,5 ),(Triangulo,Verde,10),(Circulo,Rojo,7)]
xsG :: [Figura]
xsG = [(Circulo,Amarillo,15),(Cuadrado,Rojo,1),(Triangulo,Rojo,2)]

-- Existe (al menos) un cuadrado de tama˜no menor a 5 en xs.
xsh :: [Figura]
xsh = [(Circulo,Azul,5 ),(Cuadrado,Verde,3),(Circulo,Rojo,7)]
xsH :: [Figura]
xsH = [(Circulo,Amarillo,15),(Cuadrado,Rojo,10),(Triangulo,Rojo,2)]

-- Si hay cırculos rojos en xs entonces hay cuadrados rojos.
xsi :: [Figura]
xsi = [(Cuadrado,Rojo,5 ),(Triangulo,Verde,10),(Circulo,Rojo,7)]
xsI :: [Figura]
xsI = [(Circulo,Amarillo,15),(Cuadrado,Verde,1),(Circulo,Rojo,2)]

-- 5 P\cada propiedad del ej 3 defini una funcion recursiva que dada una lista devuelva true si la prop se cumple para esa lista y falso en caso contrario

-- Todas las figuras de xs son rojas
propA :: [Figura] -> Bool
propA [ ] = True
propA (x : xs) = rojo x && propA xs

-- Todas las figuras de xs son de tamaño menor a 5
propB :: [Figura] -> Bool
propB [ ] = True
propB (x : xs) = tam x < 5 && propB xs

-- Todos los triangulos de xs son rojos
propC :: [Figura] -> Bool
propC [ ] = False
propC (x : xs) = if triangulo x
                    then rojo x && propC xs
                 else propC xs
                 
-- Existe un cuadrado verde en xs
propD :: [Figura] -> Bool
propD [ ] = False
propD (x : xs) = if cuadrado x
                    then verde x || propD xs
                 else propD xs  

-- Todos los circulos de xs son azules y de tamaño menor a 10
propE :: [Figura] -> Bool
propE [ ] = True
propE (x : xs) = if circulo x && azul x 
                    then tam x < 10 && propE xs
                 else propE xs

-- Ningun triangulo de xs es azul
propF :: [Figura] -> Bool
propF [ ] = True
propF (x : xs) = if triangulo x
                    then (azul x == False) && propF xs
                 else propF xs

-- En xs no hay circulos amarillos ni verdes
propG :: [Figura] -> Bool
propG [ ] = True
propG (x : xs) = if circulo x
                    then ((amarillo x || verde x) == False) && propG xs
                 else propG xs

-- Existe al menos un cuadrado de tamañó menor a 5 en xs
propH :: [Figura] -> Bool
propH [ ] = True
propH (x : xs) = if cuadrado x 
                    then tam x < 5 && propH xs
                 else propH xs
                 

-- Si hay circulos rojos en xs entonces hay cuadrados rojos 
-- Hay que rehacerla
propI :: [Figura] -> Bool
propI [ ] = True
propI (x : xs) = if circulo x && rojo x
                    then cuadrado x && rojo x && propI xs
                 else propH xs

