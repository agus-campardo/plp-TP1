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
-- PREGUNTAR ¡!¡!¡!¡!¡!
{-
hayCaminoIluminado :: Circuito -> Bool 
hayCaminoIluminado = 
  foldCircuito 
    (\caja -> caja == on)
    (\recCi recCd -> recCi && recCd)
    (\ce recCi recCd cs -> 
      ce == on && on -> 
        offrecCi && recCd && cs == on)
-}  
  
  
{--  
  recCircuito 
    (\caja -> lol)
    (\ci recCi cd recCd -> recCi && recCd)
    (\ce ci recCi cd recCd cs -> 
      case ci of   

    )jemplo = Serie
            ( Paraleloon
                on
                (Paralelo off cajaNada cajaOn on)
                (Paralelo Nada cajaOn cajaOff Nada)
                on
            )
            cajaOn
-}

-- CONSIDERÉ QU EL CAMINO ES SOLO DE PRENDIDAS
-- PREGUNTAR A QUÉ SE CONSIDERA CAMINO 
hayCaminoIluminado :: Circuito -> Bool 
hayCaminoIluminado = 
  recCircuito 
    (\caja -> caja == on)
    (\_ recCi _recCd -> recCi && recCd)
    (\ce ci recCi cd recCd cs -> 
      if apagadaOVacia ce || apagadaOVacia cs then False else 
      puedoContinuarCamino ci recCi || puedoContinuarCamino cd recCd)

    where
      apagadaOVacia :: Caja -> Bool 
      apagadaOVacia caja = 
        if caja == on then False else True 

      puedoContinuarCamino :: Circuito -> Bool -> Bool 
      puedoContinuarCamino c recC = case c of 
        Caja  caja           -> if caja == on then True else False -- transfomar e funcion aux!¡!¡!
        Serie ci cd          -> recC      
        Paralelo ce ci cd cs -> if apagadaOVacia ce || apagadaOVacia cs then False else recC


-- 5: cantidadPrendidas

cantidadPrendidas :: Circuito -> Int 
cantidadPrendidas =
  foldCircuito
    (\caja -> 
      if caja == on then 1 else 0)
    (\recCi recCd -> recCd + recCi)
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
  (\recCi recCd -> recCi ++ recCd)
  (\ce recCi recCd cs -> [ce] ++ recCi ++ recCd ++ [cs])

-- 7: esCircuitoProlijo
esCircuitoProlijo :: Circuito -> Bool
esCircuitoProlijo = recCircuito
  (\caja -> True)
  (\ci recCi cd recCd -> 
    case ci of
    Caja _ -> False    
    _ -> True && recCi && recCd -- PREGUNTAR ¡!¡!¡!
  ) 
  (\_ _ recCi _ recCd _ -> recCi && recCd)

-- 8: circuitoEmprolijado 
-- PREGUNTAR ¡!¡!¡!¡!
circuitoEmprolijado :: Circuito -> Circuito
circuitoEmprolijado c = 
  if esCircuitoProlijo c 
  then c 
  else foldCircuito
        (\caja -> Caja caja)
        (\recCi recCd -> Serie recCd recCi)
        (\ce recCi recCd cs -> Paralelo ce recCi recCd cs) c

-- 9: tienenLaMismaEstructura 

tienenLaMismaEstructura :: Circuito -> Circuito -> Bool
tienenLaMismaEstructura = undefined

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

-- TODO: COMPLETAR

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

