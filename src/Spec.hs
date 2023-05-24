module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Hierbas" $ do
    it "hierbaBuena" $ do
      hierbaBuena huesudo `shouldBe` UnRaton {nombre = "Huesudo", edad = 2, peso = 10, enfermedades = ["Obesidad","Sinusitis"]}
    it "hierbaVerde" $ do
      hierbaVerde "dad" huesudo `shouldBe` UnRaton {nombre = "Huesudo", edad = 4, peso = 10, enfermedades = ["Sinusitis"]}
    it "alcachofa > 2 kilos" $ do
      alcachofa huesudo `shouldBe` UnRaton "Huesudo" 4 9 [obesidad, sinusitis]
    it "alcachofa < 2 kilos" $ do
      alcachofa cerebro `shouldBe` UnRaton {nombre = "Cerebro", edad = 9, peso = 0.19, enfermedades = ["Brucelosis","Sarampion","Tuberculosis"]}
    it "hierbaZort" $ do
      hierbaZort huesudo `shouldBe` UnRaton "Huesudo" 0 10 []
    it "hierbaDelDiablo peso > 0" $ do
      hierbaDelDiablo cerebro `shouldBe` UnRaton "Cerebro" 9 0.1 [brucelosis, tuberculosis] 
    it "hierbaDelDiablo peso = 0" $ do
      hierbaDelDiablo (UnRaton "Huesudo" 4 0 [obesidad, sinusitis]) `shouldBe`  UnRaton "Huesudo" 4 0 []
  
  describe "Medicamentos" $ do
    it "reduceFatFast" $ do
      administrarMedicamentos [pondsAntiAge] bincenterrata `shouldBe` UnRaton "Bicenterrata" 2 0.19 []
    it "reduceFatFast 1" $ do
      administrarMedicamentos [reduceFatFast 1] huesudo `shouldBe` UnRaton "Huesudo" 4 9 [sinusitis]
    it "reduceFatFast 2" $ do
      administrarMedicamentos [reduceFatFast 2] huesudo `shouldBe` UnRaton "Huesudo" 4 8.1 [sinusitis]
    it "pdepCilina" $ do
      administrarMedicamentos [pdepCilina] huesudo `shouldBe` UnRaton "Huesudo" 4 10 [obesidad]
  
  describe "Experimentos" $ do
    it "cantidadIdeal even" $ do
      cantidadIdeal even `shouldBe` 2
    it "cantidadIdeal even" $ do
      cantidadIdeal (>7) `shouldBe` 8
    it "lograEstabilizar caso verdadero" $ do
      lograEstabilizar [huesudo] (reduceFatFast 30) `shouldBe` True
    it "lograEstabilizar caso falso" $ do
      lograEstabilizar [huesudo] (reduceFatFast 1) `shouldBe` False
    it "potenciaIdeal huesudo" $ do
      potenciaIdeal [huesudo] `shouldBe` 29
    it "potenciaIdeal cerebro" $ do
      potenciaIdeal [bincenterrata] `shouldBe` 1