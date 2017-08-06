module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Test.Control.Alt.Free (main) as Free
import Test.Spec.Runner (RunnerEffects)

main :: Eff (RunnerEffects ()) Unit
main = do
  Free.main
