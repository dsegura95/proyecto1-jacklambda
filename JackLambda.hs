{-
BLACKJACK MAIN
Autores:
-- Jesus Kauze #12-10273
-- David Segura #13-11341
-- Ian Goldberg #14-10406
-}

import Carta
import System.Random
import System.IO

data GameState = GS {
    juegosJugados :: Int,
    victoriasLambda :: Int,
    nombre :: String,
    generador :: StdGen,
    dinero :: Int,
    objetivo :: Int,
    apuesta :: Int
    } deriving (Show, Read)

main :: IO ()
main = mainLoop

mainLoop :: IO ()
mainLoop = do 
    putStrLn (" ")
    putStrLn "<----------------- CONFIGURACION DEL JUEGO ----------------->"
    putStrLn (" ")
    putStrLn "¿Desea cargar una partida? (Si/No)"
    answer <- getLine
    if answer == "Si" then 
        cargarPartida
    else if answer == "No" then
        iniciarPartida
    else do
        putStrLn "Ingrese una respuesta válida:"
        mainLoop

iniciarPartida :: IO () 
iniciarPartida = do 
    putStrLn "Ingrese un nombre de usuario:"
    nombreJ <- getLine
    putStrLn "Ingrese un monto de dinero:"
    dineroJ <- readLn
    if dineroJ < 1 then do
        putStrLn "ALERTA! Ingrese una cantidad positiva como monto de dinero:"
        iniciarPartida
    else do
        putStrLn "Ingrese el monto de dinero necesario para ganar:"
        objetivoJ <- readLn
        if objetivoJ < dineroJ then do
            putStrLn "ALERTA! Ingrese una cantidad objetivo mayor que la cantidad de dinero que se posee:"
            iniciarPartida
        else do
            putStrLn "Ingrese cuánto dinero desea apostar por ronda:"
            apuestaJ <- readLn
            if apuestaJ > dineroJ || apuestaJ < 1 then do
                putStrLn "ALERTA! Ingrese una apuesta positiva menor a la cantidad de dinero inicial:"
                iniciarPartida
            else do
                gen <- getStdGen
                let zero = 0
                let zero1 = 0
                let state = GS zero zero1 nombreJ gen dineroJ objetivoJ apuestaJ
                gameMenu state

gameMenu :: GameState -> IO () 
gameMenu state = do
    putStrLn (" ")
    putStrLn "<----------------- MENU DE JUEGO ----------------->"
    let jugadas = juegosJugados state
    let vLambda = victoriasLambda state
    let vJugador = (juegosJugados state) - (victoriasLambda state)
    let nombreJ = nombre state
    let dineroJ = dinero state
    putStrLn " "
    putStrLn ("Se han jugado " ++ (show (jugadas)) ++ " partidas.")
    putStrLn ("Jack Lambda ha ganado " ++ (show (vLambda)) ++ " partidas.")
    putStrLn (nombreJ ++ " ha ganado " ++ (show (vJugador)) ++ " partidas.")
    putStrLn (nombreJ ++ " tiene " ++ (show (dineroJ)) ++ " de dinero.")
    putStrLn " "
    putStrLn "1.- Jugar Ronda"
    putStrLn "2.- Guardar Partida"
    putStrLn "3.- Cargar Partida"
    putStrLn "4.- Salir"
    putStrLn "Ingrese el número asociado a la opción deseada: "
    answer <- getLine

    case answer of
        "1" -> jugarRonda state
        "2" -> guardarPartida state
        "3" -> cargarPartida 
        "4" -> salirJuego

jugarRonda :: GameState -> IO ()
jugarRonda state = do 
    putStrLn (" ")
    putStrLn "<----------------- INICIO DE RONDA ----------------->"
    putStrLn (" ")
    let newDinero = (dinero state) - (apuesta state)
    let newJuegosJugados = (juegosJugados state) + 1
    newGen <- newStdGen
    let newState = GS (newJuegosJugados) (victoriasLambda state) (nombre state) (newGen) (newDinero) (objetivo state) (apuesta state)
    let barajaJ = baraja
    let barajaJuego = barajar (generador newState) barajaJ
    let manosIniciales = inicialLambda barajaJuego
    let manoLambda = fst(manosIniciales)
    let manoJugador = vacia
    let resto = snd(manosIniciales) 
    putStrLn ((nombre newState) ++ ", esta es mi primera carta: " ++ (show ((manoLista (manoLambda)) !! 0 )))
    putStrLn (" ")
    if blackjack manoLambda then do
        putStrLn ((nombre newState) ++ " he sacado Blackjack. Yo gano.")
        if (dinero newState) >= (apuesta newState) then
            gameMenu newState
        else do 
            putStrLn ((nombre newState) ++ ", no te queda dinero. Es el fin del juego para ti.")
            salirJuego
    else do
        manoMT <- robarFase manoJugador resto
        let manoMazoTupla = justToValue manoMT
        manoMT1 <- robarFase (snd manoMazoTupla) (aplanar (fst manoMazoTupla))
        let manoMazoTupla1 = justToValue manoMT1
        putStrLn ((nombre newState) ++ ", tu mano es: " ++ (show (snd manoMazoTupla1)))
        if (blackjack (snd manoMazoTupla1)) then do
            putStrLn ((nombre newState) ++ ", tu mano es un blackjack")
        else do
            putStrLn ("Suma " ++ show (valor (snd manoMazoTupla1)))
        menuPartida newState (fst manoMazoTupla1) (snd manoMazoTupla1) manoLambda resto

menuPartida :: GameState -> Mazo -> Mano -> Mano -> Mano -> IO ()
menuPartida state mazoContinue manoJugador manoLambda manoOriginal = do
    putStrLn (" ")
    putStrLn "<----------------- ACCION ----------------->"
    putStrLn (" ")
    putStrLn ("Ingrese el número asociado a la opción deseada:")
    putStrLn ("1.- Hit (Pedir Carta)")
    putStrLn ("2.- Stand (No pedir más cartas)")
    if ((dinero state) >= (apuesta state)) then do
        putStrLn ("3.- Double Down (Redoblar Apuesta)")
        let newDinero = (dinero state) - (apuesta state)
        let newState = GS (juegosJugados state) (victoriasLambda state) (nombre state) (generador state) (newDinero) (objetivo state) (apuesta state)
        if (cantidadCartas manoJugador) == 2 then do
            putStrLn ("4.- Surrender (Rendirse)")
            putStrLn (" ")
            answer <- getLine
            case answer of 
                "1" -> hit state mazoContinue manoJugador manoLambda manoOriginal False
                "2" -> stand state mazoContinue manoJugador manoLambda manoOriginal 0
                "3" -> doubleDown newState mazoContinue manoJugador manoLambda manoOriginal
                "4" -> surrender state mazoContinue manoJugador manoLambda manoOriginal
        else do
            putStrLn (" ")
            answer <- getLine
            case answer of 
                "1" -> hit state mazoContinue manoJugador manoLambda manoOriginal False
                "2" -> stand state mazoContinue manoJugador manoLambda manoOriginal 0
                "3" -> doubleDown newState mazoContinue manoJugador manoLambda manoOriginal
    else 
        if (cantidadCartas manoJugador) == 2 then do
            putStrLn ("3.- Surrender (Rendirse)")
            putStrLn (" ")
            answer <- getLine
            case answer of 
                "1" -> hit state mazoContinue manoJugador manoLambda manoOriginal False
                "2" -> stand state mazoContinue manoJugador manoLambda manoOriginal 0
                "3" -> surrender state mazoContinue manoJugador manoLambda manoOriginal
        else do
            putStrLn (" ")
            answer <- getLine
            case answer of 
                "1" -> hit state mazoContinue manoJugador manoLambda manoOriginal False
                "2" -> stand state mazoContinue manoJugador manoLambda manoOriginal 0


hit :: GameState -> Mazo -> Mano -> Mano -> Mano -> Bool -> IO ()
hit state mazoContinue manoJugador manoLambda manoOriginal b = do
    if (puedePicar mazoContinue) then do
        rT <- robarFase manoJugador (aplanar mazoContinue)
        let robarTupla = justToValue rT
        let manoJ = snd robarTupla
        let resto = fst robarTupla
        if (busted manoJ) then do
            putStrLn (" ")
            putStrLn ((nombre state) ++ ", tu mano es " ++ (show manoJ))
            putStrLn ("Suma " ++ (show (valor manoJ)) ++ ". Perdiste.")
            perdiste state
        else do
            putStrLn (" ")
            putStrLn ((nombre state) ++ ", tu mano es " ++ (show manoJ))
            putStrLn ("Suma " ++ (show (valor manoJ)) ++ ".")
            if (b == False) then
                menuPartida state resto manoJ manoLambda manoOriginal
            else 
                stand state resto manoJ manoLambda manoOriginal 1
    else do
        let newMazo = reconstruir (desdeMano manoOriginal) manoJugador
        hit state newMazo manoJugador manoLambda manoOriginal b

stand :: GameState -> Mazo -> Mano -> Mano -> Mano -> Int -> IO ()
stand state mazoContinue manoJugador manoLambda manoOriginal b = do
    let newManoLambda = juegaLambda mazoContinue manoLambda
    if (newManoLambda == Nothing) then do
        let newMazo = reconstruir (desdeMano manoOriginal) manoJugador
        stand state newMazo manoJugador manoLambda manoOriginal b
    else do
        let nManoLambda = justToValueMano newManoLambda
        putStrLn (" ")
        putStrLn ("Es mi turno ahora.")
        putStrLn ("Mi mano es " ++ (show nManoLambda))
        putStrLn ("Suma " ++ (show (valor nManoLambda)))
        if (busted nManoLambda) then do
            ganaste state b
        else do
            let winner = ganador nManoLambda manoJugador
            if ((valor manoJugador) == (valor nManoLambda)) then do
                if ((show winner) == "Dealer") then do
                    putStrLn ("Empatamos, así que yo gano.")
                    perdiste state
                else
                    ganaste state b
            else do
                if ((show winner) == "Dealer") then do
                    putStrLn ("Yo gano.")
                    perdiste state
                else
                    ganaste state b

doubleDown :: GameState -> Mazo -> Mano -> Mano -> Mano -> IO ()
doubleDown state mazoContinue manoJugador manoLambda manoOriginal = do
    hit state mazoContinue manoJugador manoLambda manoOriginal True

ganaste :: GameState -> Int -> IO ()
ganaste state b = do 
    putStrLn ("Tu ganas.")
    putStrLn (" ")
    let ganancia = (apuesta state) * 2 * (1 + b)        
    let newDinero = (dinero state) + ganancia
    if (newDinero > (objetivo state)) then do
        putStrLn ("Felicidades, " ++ (nombre state) ++ ", me has derrotado. Es el fin del juego para mí.")
        salirJuego
    else do
        let newState = GS (juegosJugados state) (victoriasLambda state) (nombre state) (generador state) (newDinero) (objetivo state) (apuesta state)
        gameMenu newState

perdiste :: GameState -> IO ()
perdiste state = 
    if ((dinero state) > (apuesta state)) then do
        let newVictoriasLambda = (victoriasLambda state) + 1
        let newState = GS (juegosJugados state) (newVictoriasLambda) (nombre state) (generador state) (dinero state) (objetivo state) (apuesta state)
        gameMenu newState
    else do
        putStrLn ((nombre state) ++ ", no te queda dinero. Es el fin del juego para ti.")
        salirJuego

surrender :: GameState -> Mazo -> Mano -> Mano -> Mano -> IO ()
surrender state mazoContinue manoJugador manoLambda manoOriginal = do
    let apuestaJ = div (apuesta state) 2
    let newDinero = ((dinero state) + (div (apuesta state) 2))
    let newState = GS (juegosJugados state) (victoriasLambda state) (nombre state) (generador state) newDinero (objetivo state) (apuesta state)
    putStrLn (" ")
    putStrLn ((nombre newState) ++ ", te has rendido. Yo gano.")
    if ((dinero newState) < (apuesta newState)) then do
        putStrLn ((nombre newState) ++ ", no te queda dinero. Es el fin del juego para ti")
        salirJuego
    else do
        menuPartida newState mazoContinue manoJugador manoLambda manoOriginal

robarFase :: Mano -> Mano -> IO (Maybe (Mazo, Mano))
robarFase manoJugador resto = do 
    let mazo = desdeMano resto
    putStrLn ("El mazo fue dividido. Qué mitad escoge? (Derecho/Izquierdo)")
    elec <- getLine 
    putStrLn (" ")
    if ((elec /= "Derecho") && (elec /= "Izquierdo")) then do
            putStrLn ("Ingrese una opción válida. (Derecho/Izquierdo)")
            robarFase manoJugador resto
    else do
        let eleccion = read (elec) :: Eleccion
        let robarTupla1 = robar mazo manoJugador eleccion
        return robarTupla1

justToValueMano :: Maybe Mano -> Mano
justToValueMano (Just a) = a

justToValue :: Maybe (Mazo,Mano) -> (Mazo, Mano)
justToValue (Just a) = a

guardarPartida :: GameState -> IO ()
guardarPartida state = do 
    putStrLn "Ingrese el nombre del fichero donde quiere guardar la partida: "
    ficheroAux <- getLine
    let fichero = ficheroAux ++ ".txt"
    handle <- openFile fichero WriteMode
    hPutStrLn handle (show state)
    hClose handle

cargarPartida :: IO ()
cargarPartida = do  
    putStrLn "Ingrese el nombre del fichero que contiene la partida: "
    ficheroAux <- getLine
    let fichero = ficheroAux ++ ".txt"
    handle <- openFile fichero ReadMode
    contents <- hGetContents handle
    let state = read contents
    gameMenu state
    hClose handle
    

salirJuego :: IO ()
salirJuego = do 
    putStrLn "Gracias por jugar."