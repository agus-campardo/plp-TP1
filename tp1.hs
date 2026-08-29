module TP1 where

data Caja = Bombilla Bool | Nada
              deriving Eq
instance Show Caja where
    show = showDeCaja

showDeCaja :: Caja -> String 
showDeCaja (Bombilla True) = "💡"
showDeCaja (Bombilla False) = "⚪️"
showDeCaja (Nada) = "🛑"

data Circuito = Caja     Caja
              | Serie    Circuito Circuito
              | Paralelo Caja Circuito Circuito Caja
                  deriving Eq
instance Show Circuito where
    show = showDeCircuito


showDeCircuito :: Circuito -> String
showDeCircuito (Caja caja) = showDeCaja caja
showDeCircuito (Serie circuitoInicial circuitoFinal) =
  (showDeCircuito circuitoInicial) ++ "-" ++ (showDeCircuito circuitoFinal)
showDeCircuito (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuito circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuito circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

showDeCircuitoConEstructura :: Circuito -> String
showDeCircuitoConEstructura (Caja caja) = showDeCaja caja
showDeCircuitoConEstructura (Serie circuitoInicial circuitoFinal) = "(" ++
  (showDeCircuitoConEstructura circuitoInicial) ++
    "-" ++
  (showDeCircuitoConEstructura circuitoFinal) ++ ")"
showDeCircuitoConEstructura (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuitoConEstructura circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuitoConEstructura circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

on  = Bombilla True
off = Bombilla False

cajaOn   = Caja on
cajaOff  = Caja off
cajaNada = Caja Nada

{-
data Circuito = Caja     Caja
              | Serie    Circuito Circuito
              | Paralelo Caja Circuito Circuito Caja

data Caja = Bombilla Bool | Nada

-}

-- 1: recCircuito
recCircuito :: (Caja -> b) ->                                           -- fCaja 
               (Circuito -> b -> Circuito -> b -> b) ->                 -- fSerie 
               (Caja -> Circuito -> b -> Circuito -> b -> Caja -> b) -> -- fParalelo
               Circuito -> b 
recCircuito fCaja fSerie fParalelo c = case c of 
  Caja caja            -> fCaja caja 
  Serie ci cf          -> fSerie ci (rec ci) cf (rec cf) 
  Paralelo ce ci cd cs -> fParalelo ce ci (rec ci) cd (rec cd) cs 

  where rec = recCircuito fCaja fSerie fParalelo

-- 2: foldCircuito

foldCircuito :: (Caja -> b) -> (b -> b -> b) -> (Caja -> b -> b -> Caja -> b) -> Circuito -> b
foldCircuito fCaja fSerie fParalelo = 
  recCircuito 
    fCaja
    (\_ recCi _ recCd -> fSerie recCi recCd)
    (\ce _ recCi _ recCd cs -> fParalelo ce recCi recCd cs)


-- 3 invertido
invertido :: Circuito -> Circuito
invertido = foldCircuito 
              (\caja -> Caja caja)
              (\recCi recCd -> Serie recCd recCi)
              (\ce recCi recCd cs -> Paralelo cs recCd recCi ce) 

-- 4: hayCaminoIluminado
-- solo considera las cajas con bombillas prendidas 
hayCaminoIluminado :: Circuito -> Bool 
hayCaminoIluminado = 
  recCircuito 
    esCajaIluminada
    (\_ recCi _ recCd -> recCi && recCd)
    (\ce ci recCi cd recCd cs -> 
      esCajaIluminada ce && (recCi || recCd) && esCajaIluminada cs)

  
esCajaIluminada :: Caja -> Bool 
esCajaIluminada = (== on)


-- 5: cantidadPrendidas

cantidadPrendidas :: Circuito -> Int 
cantidadPrendidas =
  foldCircuito
    (\caja -> if esCajaIluminada caja then 1 else 0) 
    (+)
    (\ce recCi recCd cs -> 
      cantidadPrendidasPorLado ce recCi + 
      cantidadPrendidasPorLado cs recCd   
    )

    where 
      cantidadPrendidasPorLado :: Caja -> Int -> Int
      cantidadPrendidasPorLado caja rec =
        if caja == on then 1 + rec else rec 

-- 6: cajasDeCircuito

cajasDeCircuito :: Circuito -> [Caja]
cajasDeCircuito = foldCircuito
  (\caja -> [caja])
  (++)
  (\ce recCi recCd cs -> [ce] ++ recCi ++ recCd ++ [cs])


-- 7: esCircuitoProlijo

esCircuitoProlijo :: Circuito -> Bool
esCircuitoProlijo = recCircuito
  (const True)
  (\_ recCi cd recCd -> 
      case cd of 
        Serie _ _ -> False 
        _         -> recCi && recCd)
  (\_ _ recCi _ recCd _ -> recCi && recCd)

-- 8: circuitoEmprolijado 
-- PREGUNTAR ¡!¡!¡!¡!

-- de clase del 28/6: hay que ordenar usando lógica pre-oreden. 
-- tirar todas las series la la izq 
-- (no se puede usar Paralelo ni modificar la estructura en si. se debe preservar 
-- el "camino" de las luces y el estado y cantindad de las bombillas en las cajas )

circuitoEmprolijado :: Circuito -> Circuito
circuitoEmprolijado c = 
  if esCircuitoProlijo c 
  then c 
  else foldCircuito
        (\caja -> Caja caja)
        (\recCi recCd -> Serie recCd recCi) 
        -- pero qué pasa si tengo 
        -- Serie ci cd, donde ambos son Series?
        -- o sea, Serie (Serie 1) (Serie 2)

        -- cuando lo de vuelta no va a ser prolijo porque en 
        -- la segunda posición aún voy a tener una Serie 😛
        (\ce recCi recCd cs -> Paralelo ce recCi recCd cs) c

-- 9: tienenLaMismaEstructura 
{-
Para resolver el ejercicio, decidimos tomar las ideas aportadas para la función "take" vistas en clase. 
Tanto recCI y como recCd será una función de tipo Circuito -> Bool, para que podamos pasarle el segundo circuito (c2)
y así comparar su estructura. 
-}

tienenLaMismaEstructura :: Circuito -> (Circuito -> Bool)
tienenLaMismaEstructura = 
  foldCircuito 
    (\caja -> \c2 -> case c2 of 
                    Caja _ -> True 
                    _      -> False 
    ) 
    (\recCi recCd -> \c2 -> case c2 of 
                        Serie ci cd -> (recCi ci) && (recCd cd) 
                        _           -> False
    )
    (\_ recCi recCd _ -> \c2 -> case c2 of 
                              Paralelo _ ci cd _  -> (recCi ci) && (recCd cd)
                              _                   -> False 
    ) 

-- 10: subCircuitoMásResistente
subCircuitoMásResistente = undefined -- TODO: COMPLETAR

{-- 11: Demostrar: alternado . alternado = id

alternado :: Circuito -> Circuito
{AC} alternado (Caja caja) = Caja (cajaAlternada caja)
{AS} alternado (Serie ci cf) = Serie (alternado ci) (alternado cf)
{AP} alternado (Paralelo ce ci cd cs) =
       Paralelo (cajaAlternada ce) (alternado ci) (alternado cd) (cajaAlternada cs)

cajaAlternada :: Caja -> Caja
{CAN} cajaAlternada Nada = Nada
{CAB} cajaAlternada Bombilla booleano = Bombilla not booleano

(.) :: (b -> c) -> (a -> b) -> a -> c
{C} (f . f) x = f (f x)

id :: a -> a
{I} id x = x

not :: Bool -> Bool
{NT} not True = False
{NF} not False = True

Principio de inducción sobre Circuitos




--}




ejemplo = Serie
            ( Paralelo
                on
                (Paralelo off cajaNada cajaOn on)
                (Paralelo Nada cajaOn cajaOff Nada)
                on
            )
            cajaOn
ejemplo2 = Serie cajaOn (Serie cajaOff cajaOn)

ejemplo3 = Serie
            ( Paralelo
                off
                (Paralelo on cajaOn cajaOn off)
                (Paralelo on cajaOn cajaOff on)
                on
            )
            cajaNada


