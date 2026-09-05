import Test.HUnit
import TP1

-- TESTS

testsInvertido :: Test
testsInvertido = TestList -- TODO: AGREGAR
  [ "Caja invertida (1)"
    ~: invertido cajaOn
    ~?= cajaOn
  , "Caja invertida (2)"
    ~: invertido cajaOff
    ~?= cajaOff
  , "Caja invertida (3)"
    ~: invertido cajaNada
    ~?= cajaNada
  , "Invertido de circuito2"
    ~: invertido circuito2
    ~?= circuito2_invertido
  , "Invertido de circuito3"
    ~: invertido circuito3
    ~?= circuito3_invertido
  , "Invertido de circuito4"
    ~: invertido circuito4
    ~?= circuito4_invertido
  ]

testsHayCaminoIluminado :: Test
testsHayCaminoIluminado = TestList -- TODO: AGREGAR
  [ "En una caja con bombilla encendida hay camino iluminado"
    ~: hayCaminoIluminado cajaOn
    ~?= True
  , "circuito2 no tiene camino iluminado"
    ~: hayCaminoIluminado circuito2 
    ~?= False
  , "circuito3 no tiene camino iluminado"
    ~: hayCaminoIluminado circuito3
    ~?= False
  , "circuito4 tiene camino iluminado"
    ~: hayCaminoIluminado circuito4
    ~?= True
  ]

testsCantidadPrendidas :: Test
testsCantidadPrendidas = TestList -- TODO: AGREGAR
  [ "Cantidad prendidas en caja prendida es 1"
    ~: cantidadPrendidas cajaOn
    ~?= 1
  , "circuito2 tiene 1 prendida"
    ~: cantidadPrendidas circuito2
    ~?= 1
  , "circuito3 tiene 1 prendida"
    ~: cantidadPrendidas circuito3
    ~?= 1
  , "circuito4 tiene 6 prendidas"
    ~: cantidadPrendidas circuito4
    ~?= 6  
  ]

testsCajasDeCircuito :: Test
testsCajasDeCircuito = TestList -- TODO: AGREGAR
  [ "La lista de cajas de un circuito con una única caja es la lista con esa caja"
    ~: cajasDeCircuito cajaOn
    ~?= [on]
  , "circuito2"
    ~: cajasDeCircuito circuito2
    ~?= [on, off]
  , "circuito3"
    ~: cajasDeCircuito circuito3
    ~?= [on, Nada, off, off]
  , "circuito4"
    ~: cajasDeCircuito circuito4
    ~?= [on, off, Nada, on, on, Nada, on, off, Nada, on, on]
  ]

testsEsCircuitoProlijo :: Test
testsEsCircuitoProlijo = TestList -- TODO: AGREGAR
  [ "Una caja es prolija"
    ~: esCircuitoProlijo cajaOn
    ~?= True
  , "circuito2 es prolijo"
    ~: esCircuitoProlijo circuito2
    ~?= True
  , "circuito3 es prolijo"
    ~: esCircuitoProlijo circuito3
    ~?= True
  , "circuito4 es prolijo"
    ~: esCircuitoProlijo circuito4
    ~?= True
  , "circuito 5 (con serie a la derecha) no es prolijo"
    ~: esCircuitoProlijo circuito5
    ~?= False
  ]

testsTienenLaMismaEstructura :: Test
testsTienenLaMismaEstructura = TestList -- TODO: AGREGAR
  [ "Dos cajas tienen la misma estructura"
    ~: tienenLaMismaEstructura cajaOn cajaOff
    ~?= True
  , "acá, circuito2 y su invertido tienen misma estructura"
    ~: tienenLaMismaEstructura circuito2 circuito2_invertido
    ~?= True
  , "acá, circuito3 y su invertido tienen misma estructura"
    ~: tienenLaMismaEstructura circuito3 circuito3_invertido
    ~?= True
  , "circuito4 y su invertido no tienen la misma estructura"
    ~: tienenLaMismaEstructura circuito4 circuito4_invertido
    ~?= False
  , "circuito4 y circuito3 no tienen la misma estructura"
    ~: tienenLaMismaEstructura circuito4 circuito3
    ~?= False
  ]

testsSubCircuitoMásResistente :: Test
testsSubCircuitoMásResistente = TestList -- TODO: AGREGAR
  [
    
  ]

tests :: Test
tests = TestList
  [ TestLabel "invertido"                testsInvertido
  , TestLabel "hayCaminoIluminado"       testsHayCaminoIluminado
  , TestLabel "cantidadPrendidas"        testsCantidadPrendidas
  , TestLabel "cajasDeCircuito"          testsCajasDeCircuito
  , TestLabel "esCircuitoProlijo"        testsEsCircuitoProlijo
  --, TestLabel "circuitoEmprolijado"      testsCircuitoEmprolijado
  , TestLabel "tienenLaMismaEstructura"  testsTienenLaMismaEstructura
  , TestLabel "subCircuitoMásResistente" testsSubCircuitoMásResistente
  ]

main :: IO ()
main = runTestTT tests >>= print