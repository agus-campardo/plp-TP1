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
comentarios y consideraciones del viernes 4/9: 
  ▶ una caja vacia conduce 
  ▶ "los bugs están en los casos más pequeños". 
      3 <= cantidadTests <= 5 
      ver de hacer uno por cada constructor? 
  ▶ en el ej10: quiero que me de el resultado para todas las combinaciones de funciones posibles 
    darle definicion a la funcion de resistencia

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
    (\_ recCi _ recCd       -> fSerie recCi recCd)
    (\ce _ recCi _ recCd cs -> fParalelo ce recCi recCd cs)


-- 3 invertido
invertido :: Circuito -> Circuito
invertido = foldCircuito 
              Caja
              (\recCi recCd       -> Serie recCd recCi)
              (\ce recCi recCd cs -> Paralelo cs recCd recCi ce) 

-- 4: hayCaminoIluminado
-- solo considera las cajas con bombillas prendidas 
hayCaminoIluminado :: Circuito -> Bool 
hayCaminoIluminado = 
  recCircuito 
    esCajaIluminada
    (\_ recCi _ recCd         -> recCi && recCd)
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
  (\caja              -> [caja])
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

{-
TODO: BORRAR
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
-}

-- 9: tienenLaMismaEstructura 
{-
Para resolver el ejercicio, decidimos tomar las ideas aportadas para la función "take" vistas en clase. 
Tanto recCI y como recCd serán una función de tipo Circuito -> Bool, para que podamos pasarle el segundo circuito (c2)
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
resistenciaCircuito :: Circuito -> Float
resistenciaCircuito = undefined

subCircuitoMásResistente :: Circuito -> Circuito
subCircuitoMásResistente = recCircuito
  Caja 
  (\ci recCi cd recCd -> comparar (comparar (comparar ci recCi) (comparar cd recCd)) (Serie ci cd))
  (\ce ci recCi cd recCd cs -> comparar (comparar (comparar ci recCi) (comparar cd recCd)) (Paralelo ce ci cd cs))
    where
      comparar :: Circuito -> Circuito -> Circuito 
      comparar c1 c2 = if resistenciaCircuito c1 >= resistenciaCircuito c2 then c1 else c2
      
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




{-
HECHO CON EL LEMA DE GENERACIÓN 

(ↈ) PRINCIPIO DE INDUCCIÓN ESTRUCTURAL PARA CIRCUITOS: 

data Circuito = Caja     Caja
              | Serie    Circuito Circuito
              | Paralelo Caja Circuito Circuito Caja

Para probar P sobre todas las instancias de tipo T, basta probar P para cada uno de los constructores. 

Sea P una propiedad sobre expresiones de tipo Circuito, basta mostrar que vale:
  ▷ ∀caja :: Caja. P(Caja caja)                                               [caso base]
  ▷ ∀ci :: Circuito. ∀cd :: Circuito. P(ci) ∧ P(cd), entonces P(Serie ci cd)  [caso inductivo]
  ▷ ∀ce :: Caja. ∀ci :: Circuito. ∀cd :: Circuito. ∀cs :: Caja.
    P(ci) ∧ P(cd), entonces P(Paralelo ce ci cd cs)                            [caso inductivo]
Entonces, vale ∀x :: Circuito. P(x).


LEMAS DE GENERACIÓN: 
Los usaremos para hacer análisis de casos sin inducción . 

Lema de generación para cajas: 

data Caja = Bombilla Bool | Nada

Si caja :: Caja, entonces: 
  - o bien, caja = Nada 
  - o bien, ∃b :: Bool. caja = Bombilla b


Lemas de generación para booleanos:

data Bool = True | False 

Si b :: Bool, entonces:
  - o bien, b = True
  - o bien, b = False 


DEMO: 

Queremos demostrostrar: alternado . alternado = id 

Por extensionalidad funcional, nos bastaría demostrar que: 
  ∀x :: Circuito. (alternado . alternado) x = id x

Por {C}, esto es lo mismo que: 
  ∀x :: Circuito. alternado(alternado x) = id x

Definimos el predicado P(x) ≡ alternado (alternado x) = id x

Por inducción estructural sobre Circuitos, basta demostrar (ↈ). 


CASO BASE: ∀caja :: Caja. P(Caja caja)
  P(Caja caja): alternado(alternado (Caja caja)) = id (Caja caja)

  - alternado(alternado (Caja caja)) 
    {AC} = alternado(Caja (cajaAlternada caja))
    {AC} = Caja (cajaAlternada (cajaAlternada caja))
  
  Por lema de generación para cajas, hay dos posibilidades: 

    Caso 1: caja = Nada 
      
      Caja (cajaAlternada (cajaAlternada Nada))
      {CAN} = Caja (cajaAlternada Nada)
      {CAN} = Caja Nada 
      {ID}  = id (Caja Nada)

      Entonces, P(Caja Nada) se cumple. 

    Caso 2: ∃b :: Bool. caja = Bombilla b

      Caja (cajaAlternada (cajaAlternada Bombilla b))
      {CAB} = Caja (cajaAlternada (Bombilla (not b)))
      {CAB} = Caja (Bombilla (not (not b)))

      Por lema de generación para booleanos, hay dos posibilidades_ 

        Caso 2.1: b = True 

          Caja (Bomnilla (not (not True)))
          {NT} = Caja (Bombilla (not False))
          {NT} = Caja (Bombilla True)
          {ID} = id (Caja (Bombilla True))
        
        Caso 2.2: b = False 

          Caja (Bombilla (not (not False)))
          {NF} = Caja (Bombilla (not True))
          {NF} = Caja (Bombilla False)
          {ID} = id (Caja (Bombilla False))

  Por lo tanto, ∀caja :: Caja. P(Caja caja) queda demostrado

  CASO INDUCTIVO: Serie (igual)
  CASO INDUCTIVO: Paralelo (igual tambien)

-}







ejemplo1 = Serie
            ( Paralelo
                on
                (Paralelo off cajaNada cajaOn on)
                (Paralelo Nada cajaOn cajaOff Nada)
                on
            )
            cajaOn

ejemplo1_invertido = Serie
                      cajaOn
                      ( Paralelo
                          on
                          (Paralelo Nada cajaOff cajaOn Nada)
                          (Paralelo on cajaOn cajaNada off)
                          on
                      )

ejemplo2 = Serie cajaOn (Serie cajaOff cajaOn)

ejemplo3 = Serie
            ( Paralelo
                on
                (Paralelo on cajaOn cajaOn on)
                (Paralelo on cajaOn cajaOff on)
                on
            )
            cajaOn


