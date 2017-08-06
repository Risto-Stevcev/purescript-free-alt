module Control.Alt.Free where

import Prelude (type (~>), (<<<), ($), map, id)
import Control.Alt (class Alt, (<|>))
import Data.Foldable (foldr)
import Data.Functor.Compose (Compose(..))
import Data.NonEmpty ((:|))
import Data.List.NonEmpty (NonEmptyList(..), singleton)

type FreeAlt f a = Compose NonEmptyList f a

liftFreeAlt ∷ ∀ f a. f a → FreeAlt f a
liftFreeAlt = Compose <<< singleton

lowerFreeAlt ∷ ∀ f a. Alt f => FreeAlt f a → f a
lowerFreeAlt a = foldFreeAlt id a

foldFreeAlt ∷ ∀ f g a. Alt g => (f ~> g) → FreeAlt f a → g a
foldFreeAlt f (Compose (NonEmptyList (x :| xs))) = foldr (\elem acc → f elem <|> acc) (f x) xs

hoistFreeAlt ∷ ∀ f g a. (f ~> g) → FreeAlt f a → FreeAlt g a
hoistFreeAlt f (Compose a) = Compose $ map f a
