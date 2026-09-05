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
  , "Caja invertida (4)"
    ~: invertido ejemplo1 
    ~?= ejemplo1_invertido
  ]

testsHayCaminoIluminado :: Test
testsHayCaminoIluminado = TestList -- TODO: AGREGAR
  [ "En una caja con bombilla encendida hay camino iluminado"
    ~: hayCaminoIluminado cajaOn
    ~?= True
  , "Camino iluminado (2)"
    ~: hayCaminoIluminado ejemplo1 
    ~?= False
  , "Camino ilumnidado (3)"
    ~: hayCaminoIluminado ejemplo3
    ~?= True
  ]

testsCantidadPrendidas :: Test
testsCantidadPrendidas = TestList -- TODO: AGREGAR
  [ "Cantidad prendidas en caja prendida es 1"
    ~: cantidadPrendidas cajaOn
    ~?= 1
  , "Cantidad prendidas (2)"
    ~: cantidadPrendidas ejemplo1 
    ~?= 6
  , "Cantidad prendidas (4)"
    ~: cantidadPrendidas ejemplo3
    ~?= 10
  ]

testsCajasDeCircuito :: Test
testsCajasDeCircuito = TestList -- TODO: AGREGAR
  [ "La lista de cajas de un circuito con una única caja es la lista con esa caja"
    ~: cajasDeCircuito cajaOn
    ~?= [on]
  , "Cajas (2)"
    ~: cajasDeCircuito ejemplo1
    ~?= [on, off, Nada, on, on, Nada, on, off, Nada, on, on]
  , "Cajas (3)"
    ~: cajasDeCircuito ejemplo3
    ~?= [on, on, on, on, on, on, on, off, on, on, on]
  ]

testsEsCircuitoProlijo :: Test
testsEsCircuitoProlijo = TestList -- TODO: AGREGAR
  [ "Una caja es prolija"
    ~: esCircuitoProlijo cajaOn
    ~?= True
  , "Un circuito sin serie a la derecha"
    ~: esCircuitoProlijo ejemplo1
    ~?= True
  , "Un circuito con serie a la derecha"
    ~: esCircuitoProlijo ejemplo2
    ~?= False
  ]

testsTienenLaMismaEstructura :: Test
testsTienenLaMismaEstructura = TestList -- TODO: AGREGAR
  [ "Las cajas tienen la misma estructura"
    ~: tienenLaMismaEstructura cajaOn cajaOff
    ~?= True
  , "Ambos circuitos tienen la misma estructura, más no así lo de adentro de las cajas"
    ~: tienenLaMismaEstructura ejemplo1 ejemplo1_misma_estructura
    ~?= True
  , "En este caso, el inverso no tiene la misma estructura"
    ~: tienenLaMismaEstructura ejemplo1 ejemplo1_invertido
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