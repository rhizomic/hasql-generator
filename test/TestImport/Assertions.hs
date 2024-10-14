module TestImport.Assertions
  ( assertRight,
    assertJust,
    assertJustWith,
  )
where

import Data.Either (Either (Left, Right))
import Data.Function (($))
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid ((<>))
import GHC.Base (error)
import GHC.Show (Show, show)
import GHC.Stack (HasCallStack)

-- | Asserts that the value is 'Right' and 'error's otherwise.
assertRight :: (HasCallStack, Show a) => Either a b -> b
assertRight (Right b) = b
assertRight (Left a) = error $ "Expected Right, got Left: " <> show a

-- | Asserts that the value is 'Just' and 'error's otherwise.
assertJust :: (HasCallStack) => Maybe a -> a
assertJust (Just a) = a
assertJust Nothing = error "Expected Just, got Nothing"

-- | Asserts that the value is 'Just' and 'error's otherwise with the provided
--   message.
assertJustWith :: (HasCallStack, Show b) => b -> Maybe a -> a
assertJustWith _msg (Just a) = a
assertJustWith msg Nothing = error ("Expected Just, got Nothing. " <> show msg)
