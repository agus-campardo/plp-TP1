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
------------------------------------------------------------------------------------------------

Principio de inducción sobre cajas 

data Caja = Bombilla Bool | Nada

Sea P una propiedad sobre expresiones del tipo Caja, basta mostrar que vale: 
  ▷ ∀b :: Bool. P(b), entonces P(Bombilla b) 
  ▷ P(Nada)
Entonces, vale ∀x :: Caja. P(x). 

------------------------------------------------------------------------------------------------

Principio de inducción sobre circuitos

data Circuito = Caja     Caja
              | Serie    Circuito Circuito
              | Paralelo Caja Circuito Circuito Caja

(ↈ)
Sea P una propiedad sobre expresiones de tipo Circuito basta mostrar que vale: 
  ▷ ∀caja :: Caja. P(Caja caja)
  ▷ ∀ci :: Circuito. ∀cd :: Circuito. P(ci) ∧ P(cd), entonces P(Serie ci cd)
  ▷ ∀ce :: Caja. ∀ci :: Circuito. ∀cd :: Circuito. ∀cd :: Caja. P(ci) ∧ P(cd), entonces P(Paralelo ce ci cd cs)
Entonces, vale ∀x :: Circuito. P(x). 

Queremos demostrar la siguiente propiedad: alternado . alternado = id

Por extensionalidad funcional, nos bastaría demostrar que ∀x :: Circuito. (alternado . alternado) x = id x
Por {C}, esto es lo mismo que demostar que ∀x :: Circuito. alternado(alternado x) = id x

Por inducción sobre Circuitos, bastaría con demostrar (ↈ). 
Siendo P(x): alternado(alternado x) = id x. 

  ▷ ∀caja :: Caja. 
    P(Caja caja): alternado(alternado (Caja caja)) = id (Caja caja)

      - alternado(alternado (Caja caja)) 
        {AC} = alternado(Caja (cajaAlternada caja))
        {AC} = Caja (cajaAlternada (cajaAlternada caja))

        Sea data Caja = Bombilla Bool | Nada

        Sea Q una propiedad sobre expresiones del tipo Caja, basta mostrar que vale: 
          ▷ ∀b :: Bool. Q(Bombilla b) 
          ▷ Q(Nada)
        Entonces, vale ∀x :: Caja. Q(x). 
        
        Q(x): Caja (cajaAlternada (cajaAlternada x)) = id (Caja x)

        Probemos 
          ▷ ∀b :: Bool. Q(Bombilla b) 
            Por inducción sobre booleanos, basta probar que Q(Bombilla True) y Q(Bombilla False)

            - Q(Bombilla True): Caja (cajaAlternada (cajaAlternada (Bombilla True))) = id (Caja (Bombilla True))

              -- Caja (cajaAlternada (cajaAlternada (Bombilla True))) 
                {CAB} = Caja (cajaAlternada (Bombilla not True)) 
                {NT}  = Caja (cajaAlternada (Bombilla False)) 
                {CAB} = Caja (Bombilla not False) 
                {NF}  = Caja (Bombilla True) 
                {I}   = id (Caja (Bombilla True)) 

                Que era lo que queríamos probar. 
            
            - Q(Bombilla False): Caja (cajaAlternada (cajaAlternada (Bombilla False))) = id (Caja (Bombilla False))

              -- Caja (cajaAlternada (cajaAlternada (Bombilla False))) 
                {CAB} = Caja (cajaAlternada (Bombilla not False)) 
                {NF}  = Caja (cajaAlternada (Bombilla True)) 
                {CAB} = Caja (Bombilla not True) 
                {NT}  = Caja (Bombilla False) 
                {I}   = id (Caja (Bombilla False)) 

                Que era lo que queríamos probar. 
            
          ▷ Q(Nada): Caja (cajaAlternada (cajaAlternada Nada)) = id (Caja Nada)  

            - Caja (cajaAlternada (cajaAlternada (Nada)))
              {CAN} = Caja (cajaAlternada Nada)
              {CAN} = Caja Nada          
              {I}   = id (Caja Nada)

              Que era lo que queríamos probar. 


  ▷ ∀ci :: Circuito. ∀cd :: Circuito. P(ci) ∧ P(cd), entonces P(Serie ci cd)
    P(Serie ci cd): alternado(alternado (Serie ci cd)) = id (Serie ci cd)

    - alternado(alternado (Serie ci cd))
      {AS} = alternado(Serie (alternado ci) (alternado cd)) 
      {AS} = Serie (alternado(alternado ci)) (alternado(alternado cd))
      {HI} = Serie (id ci) (id cd) 
      {I}  = Serie ci cd 
      {I}  = id (Serie ci cd) 

  ▷ ∀ce :: Caja. ∀ci :: Circuito. ∀cd :: Circuito. ∀cd :: Caja. P(ci) ∧ P(cd), entonces P(Paralelo ce ci cd cs)
    P(Paralelo ce ci cd cs): alternado(alternado (Paralelo ce ci cd cs)) = id (Paralelo ce ci cd cs)

    - alternado(alternado (Paralelo ce ci cd cs))
      {AP} = alternado(Paralelo (cajaAlternada ce) (alternado ci) (alternado cd) (cajaAlternada cs))
      {AP} = Paralelo (cajaAlternado(cajaAlternada ce)) (alternado(alternado ci)) (alternado(alternado cd)) (cajaAlternado(cajaAlternada cs))
      {HI} = Paralelo (cajaAlternado(cajaAlternada ce)) (id ci) (id cd) (cajaAlternado(cajaAlternada cs))
      {por haber probado Q(x) en inducción sobre cajas} 
          = Paralelo (id ce) (id ci) (id cd) (id cs)
      {I}  = Paralelo ce ci cd cs 
      {I}  = id (Paralelo ce ci cd cs)

    
  



                                      
     


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


