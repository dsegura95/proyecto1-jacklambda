# Proyecto 1 - Jack Lambda

Proyecto Simulacion BlackJack Vs PC:: JackLambda

## README

Con estas instrucciones podras ejecutar y correr el codigo para empezar a jugar contra el famoso JackLambda. Para ver detalles del codigo ver notas del desarrollo


### Prerequisitos

Lo que necesitaras para poder ejecutar el proyecto

```
sudo apt install ghc-platform
```

### Pasos para ejecutar

Para poder ejecutar y correr el programa debe hacer lo siguiente:


```
$ make && make clean
$ ./JackLambda
```

Posteriormente siga los pasos indicados por el programa para empezar a jugar (Recuerde que existe distincion entre MAYUS y minus)

## Implementacion de funciones

Para el desarrollo del programa se utilizaron distintas firmas de funciones externas en conjunto con las requeridas en el PDF.

A continuacion se detallaran las mismas:

- mainLoop :: IO ()

Funciones de Mano:

- numberOfA :: Mano -> Int

- valorSinA :: Mano -> Int

- courtCards :: Mano -> Bool

- tomar1Mitad :: [Carta] -> Int -> [Carta]

- tomar2Mitad :: [Carta] -> Int -> [Carta]

- barajarListas :: StdGen -> Mano -> Mano -> Mano

Funciones de Mazo:

- fst3 :: (Mano,Carta,Mano) -> Mano

- snd3 :: (Mano,Carta,Mano) -> Carta

- trd3 :: (Mano,Carta,Mano) -> Mano

- aplanarM :: Mazo -> [Carta] -> [Carta]

-  incluir :: Carta -> Mano ->  Mano

- primeraCartaMazo :: Mazo -> Carta


## Equipo Desarrollador

* [David Segura](https://github.com/dsegura95) - 13-11341
* [Ian Goldbergs](https://github.com/IanKz) - 14-10406
* [Jesus Kauze](https://github.com/jkauze) - 12-10273
