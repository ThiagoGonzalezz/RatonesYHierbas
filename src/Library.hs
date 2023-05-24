module Library where
import PdePreludat

type Enfermedad = String

type Hierbas = (Raton->Raton)

type Medicamento = [Hierbas]

type Comunidad = [Raton]

data Raton = UnRaton {
    nombre::String,
    edad::Number,
    peso::Number,
    enfermedades::[Enfermedad]
}deriving (Show, Eq)

sinusitis::Enfermedad
sinusitis = "Sinusitis"

brucelosis::Enfermedad 
brucelosis = "Brucelosis"

sarampion::Enfermedad
sarampion = "Sarampion"

tuberculosis::Enfermedad
tuberculosis = "Tuberculosis"

obesidad::Enfermedad
obesidad = "Obesidad"

--1
cerebro::Raton
cerebro = UnRaton "Cerebro" 9 0.2 [brucelosis, sarampion, tuberculosis]

bincenterrata::Raton
bincenterrata = UnRaton "Bicenterrata" 256 0.2 []

huesudo::Raton
huesudo = UnRaton "Huesudo" 4 10 [obesidad, sinusitis]

--2

hierbaBuena::Raton->Raton
hierbaBuena ratoncito = ratoncito{edad = sqrt (edad ratoncito)}

hierbaVerde::String->Raton->Raton
hierbaVerde terminacion raton  = raton{ enfermedades = filter (not . terminaCon terminacion) (enfermedades raton)} 

terminaCon::String->Enfermedad->Bool
terminaCon terminacion enfermedad = drop (length enfermedad - length terminacion) enfermedad == terminacion

alcachofa::Raton->Raton
alcachofa raton 
    |peso raton > 2 = raton{peso = 0.9 * peso raton}
    |otherwise =  raton{peso = 0.95 * peso raton}

hierbaZort::Raton->Raton
hierbaZort raton = raton{edad = 0, enfermedades = []}

hierbaDelDiablo :: Raton -> Raton
hierbaDelDiablo raton = raton {peso = max 0 (peso raton - 0.1), enfermedades = filter mas10Letras (enfermedades raton)}

mas10Letras::Enfermedad->Bool
mas10Letras enfermedad = length enfermedad >= 10

--3
administrarMedicamentos:: [Medicamento] -> Raton -> Raton
administrarMedicamentos medicamentos raton = foldl (foldr ($)) raton medicamentos 

pondsAntiAge::Medicamento
pondsAntiAge = [hierbaBuena, hierbaBuena, hierbaBuena, alcachofa]

reduceFatFast:: Number -> Medicamento
reduceFatFast potencia = hierbaVerde "Obesidad" : replicate potencia alcachofa

pdepCilina :: Medicamento
pdepCilina = [hierbaVerde (head sufijosInfecciosas) , hierbaVerde (sufijosInfecciosas !! 1), hierbaVerde (sufijosInfecciosas !! 2), hierbaVerde (last sufijosInfecciosas)] 

sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

--4
--a
cantidadIdeal::(Number->Bool) -> Number
cantidadIdeal condicion =  buscarCualCumple condicion [1 ..]

buscarCualCumple::(Number->Bool)->[Number]->Number
buscarCualCumple condicion (x:xs) 
    |condicion x = x
    |otherwise = buscarCualCumple condicion xs

--b
lograEstabilizar::Comunidad->Medicamento->Bool
lograEstabilizar comunidad medicamento  = (all estabilizo . map (administrarMedicamentos [medicamento])) comunidad 

estabilizo::Raton->Bool
estabilizo raton = peso raton < 1 && length (enfermedades raton) < 3

--c

potenciaIdeal::Comunidad->Number
potenciaIdeal comunidad = cantidadIdeal (lograEstabilizar  comunidad . reduceFatFast) 
