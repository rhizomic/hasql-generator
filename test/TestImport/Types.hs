module TestImport.Types
  ( Hobby (..),
  )
where

import GHC.Read (Read)
import GHC.Show (Show)
import Hasql.Generator.Types (HasqlEnum)

data Hobby
  = Reading
  | Writing
  | PlayingSports
  deriving stock (Show, Read)

instance HasqlEnum Hobby
