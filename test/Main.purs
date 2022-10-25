module Test.Main where

import Prelude

import Control.Monad.Gen as Gen
import Data.Maybe (Maybe(..))
import Data.UUID.Random as UUID
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Assert (assertTrue)
import Test.QuickCheck as QC

main :: Effect Unit
main = do
  log ""
  log "Checking some existing UUIDv4s parse"
  checkUUID "e8062314-0bf4-47a0-88bd-99e8e0252965"
  checkUUID "5d94b56a-0f1f-4d6b-b2c9-1c1839fdf2b5"
  checkUUID "c6969b52-fd41-4fde-b791-f566140e4284"
  checkUUID "35214b37-4f36-4c4f-a47e-0ee3be4a2ab0"
  log ""
  log "Checking that `fromString`/`toString` roundtrips"
  QC.quickCheckGen' 10000 (checkRoundtrip <<< UUID.toString <$> UUID.make' Gen.chooseInt)
  log ""

checkUUID :: String -> Effect Unit
checkUUID input = log ("- " <> input) *> assertTrue (checkRoundtrip input)

checkRoundtrip :: String -> Boolean
checkRoundtrip input = (UUID.toString <$> UUID.fromString input) == Just input
