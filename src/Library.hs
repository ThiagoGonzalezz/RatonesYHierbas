module Library where
import PdePreludat

type Enfermedad = String

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

hiervaVerde::Raton->String->Raton
hiervaVerde raton terminacion = raton{ enfermedades = filter (noTerminaCon terminacion) (enfermedades raton)} 

noTerminaCon::String->Enfermedad->Bool
noTerminaCon terminacion