{-
BLACKJACK MAIN
Autores:
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
                    }

main :: IO ()
main = mainLoop

mainLoop :: IO ()
mainLoop = do 
		  putStrLn "¿Desea cargar una partida? (Si/No)"
		  answer <- getLine
		  if answer == "Si" then 
		  		cargarPartida
		  else if answer == "No" then
		  			iniciarPartida
		  else 
		  	   do
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
	let newDinero = (dinero state) - (apuesta state)
	let newJuegosJugados = (juegosJugados state) - 1
	let newState = GS (newJuegosJugados) (victoriasLambda state) (nombre state) (generador state) (newDinero) (objetivo state) (apuesta state)
	let barajaJ = baraja
	let barajaJuego = barajar (generador state) barajaJ
	let manosIniciales = inicialLambda barajaJuego
	let manoLambda = fst(manosIniciales)
	let resto = snd(manosIniciales) 
	putStrLn ((nombre state) ++ ", esta es mi primera carta: " ++ (show (manoLambda)))

guardarPartida :: GameState -> IO ()
guardarPartida state = do 
	putStrLn "Ingrese el nombre del fichero donde quiere guardar la partida: "
	ficheroAux <- getLine
	let fichero = ficheroAux ++ ".txt"
	handle <- openFile fichero WriteMode
	hPutStrLn handle (show (juegosJugados state) ++ " " ++ show (victoriasLambda state) ++ " " ++ nombre state ++ " " ++ show (dinero state) ++ " " ++ show (objetivo state) ++ " " ++ show (apuesta state))
	hClose handle

cargarPartida :: IO ()
cargarPartida = do  
	putStrLn "Ingrese el nombre del fichero que contiene la partida: "
	ficheroAux <- getLine
	let fichero = ficheroAux ++ ".txt"
	handle <- openFile fichero ReadMode
	contents <- hGetContents handle
	let allWords = words contents
	gen <- getStdGen
	let victoriasJ = read (allWords !! 0)
	let victoriasL = read (allWords !! 1)
	let nombreJ = allWords !! 2
	let dineroJ = read (allWords !! 3)
	let objetivoJ = read (allWords !! 4)
	let apuestaJ = read (allWords !! 5)
	let state = GS victoriasJ victoriasL nombreJ gen dineroJ objetivoJ apuestaJ
	gameMenu state
	hClose handle
	

salirJuego :: IO ()
salirJuego = do 
	putStrLn "Gracias por jugar."