{-
BLACKJACK
Autores:
-- David Segura #13-11341
-- Ian Goldberg

-}

import System.Random
import Data.List

-- CARTAS
data Palo = Treboles | Diamantes | Picas | Corazones deriving (Enum,Eq)

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
}deriving Eq

instance Show Carta where
    show (Carta rango palo) = show palo ++ show rango

-- JUGADOR

data Jugador = Dealer | Player deriving Read -- PENDIENTE

instance Show Jugador where
    show Dealer = "Dealer"
    show Player = "Player"

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

ganador :: Mano -> Mano -> Jugador
ganador dealer jugador | valor dealer > valor jugador = Dealer
                       | valor jugador > valor dealer = Player
                       | valor dealer == valor jugador = if blackjack jugador then Player else Dealer

separar :: Mano -> (Mano, Carta, Mano)
separar (Mano cartas) = if odd c 
                        then (Mano (tomar1Mitad cartas (div c 2)),cartas !! (div c 2),Mano (tomar2Mitad cartas (div c 2))) 
                        else (Mano (tomar1Mitad cartas (div c 2)),cartas !! (div c 2),Mano (tomar2Mitad cartas ((div c 2)-1)))
                        where c = cantidadCartas (Mano cartas)

-- Auxiliar
tomar1Mitad :: [Carta] -> Int -> [Carta]
tomar1Mitad list n = if length list > n then tomar1Mitad (init list) n else list
-- Auxiliar
tomar2Mitad :: [Carta] -> Int -> [Carta]
tomar2Mitad list n = if length list > n then tomar2Mitad (tail list) n else list

barajar :: StdGen -> Mano -> Mano
barajar gen m = barajarListas gen m (Mano [])

-- Auxiliar
barajarListas :: StdGen -> Mano -> Mano -> Mano
barajarListas gen (Mano []) acum = acum
barajarListas gen (Mano m) (Mano acum) = barajarListas gen (Mano newM) (Mano newAcum)
    where value = randomR (0,(cantidadCartas (Mano m))-1) gen
          cartaAleatoria = m !! fst value
          newAcum = cartaAleatoria:acum
          newM = delete cartaAleatoria m

inicialLambda :: Mano -> (Mano, Mano)
inicialLambda (Mano m) = (Mano (take 2 m), Mano (drop 2 m))