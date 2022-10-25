module Data.UUID.Random
  ( UUIDv4
  , make
  , make'
  , toString
  , fromString
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe, fromJust)
import Data.String as String
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.String.Regex.Unsafe as RXU
import Data.Traversable (sequence, traverse)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random as Random
import Partial.Unsafe (unsafePartial)

-- | A type for version 4 (random) UUIDs.
data UUIDv4 = UUIDv4 String (Array Int)

instance Eq UUIDv4 where
  eq (UUIDv4 a _) (UUIDv4 b _) = a == b

instance Ord UUIDv4 where
  compare (UUIDv4 a _) (UUIDv4 b _) = compare a b

instance Show UUIDv4 where
  show (UUIDv4 s _) = "(UUIDv4 " <> s <> ")"

-- | Creates a `UUIDv4` using the standard `Math.random`.
make :: forall m. MonadEffect m => m UUIDv4
make = liftEffect $ make' Random.randomInt

-- | Creates a `UUIDv4` using a custom function for generating random numbers.
make' :: forall m. Applicative m => (Int -> Int -> m Int) -> m UUIDv4
make' rand = unsafeFromInts <$> sequence uuid
  where
  uuid = x8 <> x4 <> [ pure 4 ] <> x3 <> y <> x3 <> x12
  y = [ rand 8 11 ]
  x = [ rand 0 15 ]
  x3 = x <> x <> x
  x4 = x3 <> x
  x8 = x4 <> x4
  x12 = x8 <> x4

-- | Prints a `UUIDv4` as a string
toString :: UUIDv4 -> String
toString (UUIDv4 s _) = s

-- | Attempts to parse a `UUIDv4` from a string.
fromString :: String -> Maybe UUIDv4
fromString str = fromInts =<< traverse (Int.fromStringAs hexRadix) (RX.split rxChars str)

rxChars :: RX.Regex
rxChars = unsafePartial (RXU.unsafeRegex "-?" RXF.global)

fromInts :: Array Int -> Maybe UUIDv4
fromInts is = do
  guard (Array.length is == 32)
  rd <- Array.index is 12
  guard (rd == 4)
  yd <- Array.index is 16
  guard (yd >= 8 && yd < 12)
  pure (unsafeFromInts is)

unsafeFromInts :: Array Int -> UUIDv4
unsafeFromInts is = UUIDv4 (render is) is

render :: Array Int -> String
render is = String.joinWith "-" [ chunk 0 8, chunk 8 12, chunk 12 16, chunk 16 20, chunk 20 32 ]
  where
  chunk i j = String.joinWith "" (map (Int.toStringAs hexRadix) (Array.slice i j is))

hexRadix :: Int.Radix
hexRadix = unsafePartial (fromJust (Int.radix 16))
