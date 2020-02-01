{-
BLACKJACK
Autores:
-- David Segura #13-11341
-- Ian Goldberg

-}

-- CARTAS
data Palo = Treboles | Diamantes | Picas | Corazones deriving (Enum)

instance Show Palo where
    show Treboles = "♣"
    show Diamantes = "♦"
    show Picas = "♠"
    show Corazones = "♥"

data Rango = N Int | Jack | Queen | King | Ace deriving (Eq)

instance Show Rango where
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show Ace = "A"
    show (N x) = show x

data Carta = Carta {
    rango :: Rango,
    palo :: Palo
}

instance Show Carta where
    show (Carta rango palo) = show palo ++ show rango

-- JUGADOR

data Jugador = Dealer | Player deriving(Show,Read) -- PENDIENTE

-- MANO

newtype Mano = Mano [Carta] deriving Show

-- Devuelve mano vacio
vacia :: Mano
vacia = Mano []

-- Listas con los palos de las cartas y el rango de las mismas
paloc = [Treboles .. Corazones]
rangoc =  [N x | x <- [2..10] ] ++ [Jack,Queen,King,Ace] 

baraja :: Mano
baraja = Mano[ Carta x y | x <- rangoc, y <- paloc ]

cantidadCartas :: Mano -> Int
cantidadCartas (Mano a) = length a

-- Auxiliar
valorUnaCarta :: Carta -> Int
valorUnaCarta (Carta rango palo) = case rango of
    Ace -> 11
    (N x) -> x
    _ -> 10

-- Auxiliar
numberOfA :: Mano -> Int
numberOfA (Mano []) = 0
numberOfA (Mano ((Carta Ace palo):xs)) = 1 + numberOfA (Mano xs)
numberOfA (Mano ((Carta _ palo):xs)) = numberOfA (Mano xs)

-- Auxiliar
valorSinA :: Mano -> Int
valorSinA (Mano []) = 0
valorSinA (Mano (x:xs)) = valorUnaCarta x + valorSinA (Mano xs)

{- Función que aproxima la mano a la realidad, donde con AA9 se puede conseguir 21
valorMasExacto :: Int -> Int -> Int
valorA m 0 = m
valorA m a = if m > 21 then (valorA (m-10) (a-1)) else m
-}

valor :: Mano -> Int
valor m = if valorSinA m > 21 && numberOfA m > 0 then valorSinA m - 10 * numberOfA m else valorSinA m

busted :: Mano -> Bool
busted a = if valor a > 21 then True else False

blackjack :: Mano -> Bool
blackjack m = if courtCards m && valor m == 21 && cantidadCartas m == 2 then True else False

-- Auxiliar
courtCards :: Mano -> Bool
courtCards (Mano []) = False
courtCards (Mano ((Carta rango palo):xs)) | rango == Jack = True
                                          | rango == Queen = True
                                          | rango == King = True
                                          | otherwise = courtCards(Mano xs)
