module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (intercalate)
import Data.List (List, (:))
import Data.Monoid (mempty)
import Data.Record (get)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Row (class RowToList, Cons, Nil, RLProxy(..), kind RowList)

class WriteJSON a where
  writeJSON :: a -> String

instance stringWriteJSON :: WriteJSON String where
  writeJSON s = s

instance intWriteJSON :: WriteJSON Int where
  writeJSON i = show i

instance arrayWriteJSON :: WriteJSON a => WriteJSON (Array a) where
  writeJSON xs = "[" <> contents <> "]"
    where
      contents = intercalate "," $ writeJSON <$> xs

instance recordWriteJSON ::
  ( RowToList row rl
  , WriteJSONFields rl row
  ) => WriteJSON (Record row) where
  writeJSON rec = "{" <> fields <> "}"
    where
      rlp = RLProxy :: RLProxy rl
      fields = intercalate "," $ writeJSONFields rlp rec

class WriteJSONFields (rl :: RowList) row | rl -> row where
  writeJSONFields :: forall g. g rl -> Record row -> List String

instance consWriteJSONFields ::
  ( IsSymbol name
  , WriteJSON ty
  , WriteJSONFields tail row
  , RowCons name ty whatever row
  ) => WriteJSONFields (Cons name ty tail) row where
  writeJSONFields _ rec = head : rest
    where
      namep = SProxy :: SProxy name
      key = reflectSymbol namep
      value = writeJSON $ get namep rec
      head = "\"" <> key <> "\":" <> value
      tailp = RLProxy :: RLProxy tail
      rest = writeJSONFields tailp rec

instance nilWriteJSONFields :: WriteJSONFields Nil row where
  writeJSONFields _ _ = mempty

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ writeJSON "asdf"
  log $ writeJSON 1
  log $ writeJSON [1,2,3]
  log $ writeJSON {a: 123, b: "asdf", c: {d: 123}}
  log "Hello sailor!"
