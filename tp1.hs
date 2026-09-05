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
    esCajaConductora 
    (\_ recCi _ recCd         -> recCi && recCd)
    (\ce ci recCi cd recCd cs -> 
      esCajaConductora ce && (recCi || recCd) && esCajaConductora cs)

  where 
    esCajaConductora :: Caja -> Bool 
    esCajaConductora caja = caja == on || caja == Nada


-- 5: cantidadPrendidas

cantidadPrendidas :: Circuito -> Int 
cantidadPrendidas =
  foldCircuito
    (\caja -> if caja == on then 1 else 0) 
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
resistenciaCircuito c = 
  if cantidadTotales c == 0
    then 0
    else fromIntegral (cantidadPrendidas c) / fromIntegral (cantidadTotales c)
  where
    cantidadTotales :: Circuito -> Int
    cantidadTotales = foldCircuito
      (\caja -> if caja /= Nada then 1 else 0)
      (+)
      (\ce recCi recCd cs -> contarCaja ce + recCi + recCd + contarCaja cs)

    contarCaja :: Caja -> Int
    contarCaja caja = if caja /= Nada then 1 else 0

subCircuitoMásResistente :: Circuito -> Circuito
subCircuitoMásResistente = recCircuito
  Caja 
  (\ci recCi cd recCd -> comparar (comparar (comparar ci recCi) (comparar cd recCd)) (Serie ci cd))
  (\ce ci recCi cd recCd cs -> comparar (comparar (comparar ci recCi) (comparar cd recCd)) (Paralelo ce ci cd cs))
    where
      comparar :: Circuito -> Circuito -> Circuito 
      comparar c1 c2 = if resistenciaCircuito c1 >= resistenciaCircuito c2 then c1 else c2

{-- OPCIÓN PARA TESTEAR
subCircuitoMásResistente' :: (Circuito -> Float) -> Circuito -> Circuito
subCircuitoMásResistente' f = recCircuito
  Caja 
  (\ci recCi cd recCd -> comparar (comparar (comparar ci recCi) (comparar cd recCd)) (Serie ci cd))
  (\ce ci recCi cd recCd cs -> comparar (comparar (comparar ci recCi) (comparar cd recCd)) (Paralelo ce ci cd cs))
    where
      comparar :: Circuito -> Circuito -> Circuito 
      comparar c1 c2 = if f c1 >= f c2 then c1 else c2

subCircuitoMásResistente :: Circuito -> Circuito
subCircuitoMásResistente = subCircuitoMásResistente' resistenciaCircuito
--}

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

PRINCIPIO DE INDUCCIÓN SOBRE CAJAS: 

data Caja = Bombilla Bool | Nada

Sea P una propiedad sobre expresiones del tipo Caja, basta mostrar que vale: 
  ▷ ∀b :: Bool. 
    P(b), entonces P(Bombilla b) 
  ▷ P(Nada)
Entonces, vale ∀x :: Caja. P(x). 

------------------------------------------------------------------------------------------------

PRINCIPIO DE INDUCCIÓN SOBRE CIRCUITOS: (ↈ)

data Circuito = Caja     Caja
              | Serie    Circuito Circuito
              | Paralelo Caja Circuito Circuito Caja

Sea P una propiedad sobre expresiones de tipo Circuito basta mostrar que vale: 
  ▷ ∀caja :: Caja. 
    P(Caja caja)                                                  [caso base]
  ▷ ∀ci :: Circuito. ∀cd :: Circuito. 
    P(ci) ∧ P(cd), entonces P(Serie ci cd)                        [caso recursivo]
  ▷ ∀ce :: Caja. ∀ci :: Circuito. ∀cd :: Circuito. ∀cs :: Caja.                     
    P(ci) ∧ P(cd), entonces P(Paralelo ce ci cd cs)               [caso recursivo] 
Entonces, vale ∀x :: Circuito. P(x). 

------------------------------------------------------------------------------------------------

DEMO: 

Queremos demostrar la siguiente propiedad: alternado . alternado = id

Por extensionalidad funcional, nos bastaría demostrar que:  
  ∀x :: Circuito. (alternado . alternado) x = id x

Por {C}, esto es lo mismo que demostar que: 
  ∀x :: Circuito. alternado (alternado x) = id x

Definimos P(x) ≡ alternado (alternado x) = id x

Por inducción sobre Circuitos, bastaría con demostrar (ↈ). 



▷ CASO BASE: Caja 
  ∀caja :: Caja. P(Caja caja) ≡ alternado(alternado (Caja caja)) = id (Caja caja)

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
            Por lema de generación sobre booleanos, b = True o b = False.
            Basta probar que Q(Bombilla True) y Q(Bombilla False)

            - Q(Bombilla True): Caja (cajaAlternada (cajaAlternada (Bombilla True))) = id (Caja (Bombilla True))

              -- Caja (cajaAlternada (cajaAlternada (Bombilla True))) 
                {CAB} = Caja (cajaAlternada (Bombilla (not True))) 
                {CAB} = Caja (Bombilla (not (not True)))) 
                {NT}  = Caja (Bombilla (not False))
                {NF}  = Caja (Bombilla True) 
                {I}   = id (Caja (Bombilla True)) 

                Que era lo que queríamos probar. 
            
            - Q(Bombilla False): Caja (cajaAlternada (cajaAlternada (Bombilla False))) = id (Caja (Bombilla False))

             -- Caja (cajaAlternada (cajaAlternada (Bombilla False))) 
                {CAB} = Caja (cajaAlternada (Bombilla (not False))) 
                {CAB} = Caja (Bombilla (not (not False)))) 
                {NF}  = Caja (Bombilla (not True))
                {NT}  = Caja (Bombilla False) 
                {I}   = id (Caja (Bombilla False)) 

                Que era lo que queríamos probar. 
            
          ▷ Q(Nada): Caja (cajaAlternada (cajaAlternada Nada)) = id (Caja Nada)  

            - Caja (cajaAlternada (cajaAlternada (Nada)))
              {CAN} = Caja (cajaAlternada Nada)
              {CAN} = Caja Nada          
              {I}   = id (Caja Nada)

              Que era lo que queríamos probar. 

  Por lo tanto, P(Caja caja) queda demostrado. 



▷ CASO RECURSIVO: Serie 
    ∀ci :: Circuito. ∀cd :: Circuito. 
      P(ci) ∧ P(cd), entonces P(Serie ci cd)

    Hipótesis inductiva: 
      P(ci) ≡ alternado (alternado ci) = id ci
      P(cd) ≡ alternado (alternado cd) = id cd
    
    Queremos probar: 
    P(Serie ci cd): alternado(alternado (Serie ci cd)) = id (Serie ci cd)

    Entonces: 
      - alternado(alternado (Serie ci cd))
        {AS} = alternado(Serie (alternado ci) (alternado cd)) 
        {AS} = Serie (alternado(alternado ci)) (alternado(alternado cd))
        {HI} = Serie (id ci) (id cd) 
        {I}  = Serie ci (id cd) 
        {I}  = Serie ci cd 
        {I}  = id (Serie ci cd) 
  
  Por lo tanto, P(Serie ci cd) queda demostrado. 



▷ CASO RECURSIVO: Paralelo
    ∀ce :: Caja. ∀ci :: Circuito. ∀cd :: Circuito. ∀cd :: Caja. 
      P(ci) ∧ P(cd), entonces P(Paralelo ce ci cd cs)

    Hipótesis inductiva: 
      P(ci) ≡ alternado (alternado ci) = id ci
      P(cd) ≡ alternado (alternado cd) = id cd
    
    Queremos probar: 
    P(Paralelo ce ci cd cs) ≡ alternado(alternado (Paralelo ce ci cd cs)) = id (Paralelo ce ci cd cs)

      - alternado(alternado (Paralelo ce ci cd cs))
        {AP}   = alternado(Paralelo (cajaAlternada ce) (alternado ci) (alternado cd) (cajaAlternada cs))
        {AP}   = Paralelo (cajaAlternada (cajaAlternada ce)) 
                          (alternado (alternado ci)) 
                          (alternado (alternado cd)) 
                          (cajaAlternada (cajaAlternada cs))
        {HI}   = Paralelo (cajaAlternada (cajaAlternada ce)) (id ci) (id cd) (cajaAlternada (cajaAlternada cs))
        {LEMA} = Paralelo (id ce) (id ci) (id cd) (id cs)
        {I}    = Paralelo ce (id ci) (id cd) (id cs)
        {I}    = Paralelo ce  ci (id cd) (id cs)
        {I}    = Paralelo ce  ci cd (id cs)
        {I}    = Paralelo ce ci cd cs 
        {I}    = id (Paralelo ce ci cd cs)

  Por lo tanto, P(Paralelo ce ci cd cs) queda demostrado. 



DEMOSTRACIÓN DEL LEMA:  

  Lema: ∀c :: Caja. cajaAlternada(cajaAlternada c) = id c
    
    Sea R(c) ≡ cajaAlternada(cajaAlternada c) = id c
    
    Por inducción estructural sobre cajas, para probar R(x), basta probar:
      ▷ ∀b :: Bool. R(Bombilla b)
      ▷ R(Nada)

      ▷ CASO: Bombilla 
          ∀b :: Bool. R(Bombilla b)

          Por lema de generación de booleanos, b = True o b = False. 
          Luego para probar R(Bombilla b), basta probar R(Bombilla True) y R(Bombilla False).

            ▷ Caso b = True
              R(Bombilla True) ≡ cajaAlternada (cajaAlternada (Bombilla True)) = id (Bombilla True)

              - cajaAlternada(cajaAlternada (Bombilla True))
                {CAB} = cajaAlternada (Bombilla (not True))
                {CAB} = Bombilla (not (not True))
                {NT}  = Bombilla (not False)
                {NF}  = Bombilla True
                {I}   = id (Bombilla True)

            ▷ Caso b = False 
              R(Bombilla False) ≡ cajaAlternada(cajaAlternada (Bombilla False)) = id (Bombilla False)

              - cajaAlternada(cajaAlternada (Bombilla False))
                {CAB} = cajaAlternada (Bombilla (not False))
                {CAB} = Bombilla (not (not False))
                {NF}  = Bombilla (not True)
                {NT}  = Bombilla False
                {I}   = id (Bombilla False)

      ▷ CASO: Nada 
          R(Nada) ≡ cajaAlternada (cajaAlternada Nada) = id Nada

          - cajaAlternada (cajaAlternada Nada)
            {CAN} = cajaAlternada Nada 
            {CAN} = Nada
            {I}   = id Nada

  Por lo tanto, ∀c :: Caja. cajaAlternada (cajaAlternada c) = id c

Hemos demostrado los tres casos del principio de inducción sobre circuitos (ↈ).

Por lo tanto, ∀x :: Circuito. P(x) se cumple.                           
--}


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


{-
Eliminar 'esIluminado' del ejercicio 4/5 
Hacer resistenciaCircuito de ejercicio 10
-}


