{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Resource where

import Control.Applicative

import Data.Serialize
import qualified Data.ByteString.Lazy as L


-- | @Resource r m a@ represents a resource @r@ that may store and restore
-- a value of type @a@ in a monad @m@.
class Resource r m a | r -> m, r -> a where
    -- | @store r a@ commits the value @a@ to the resource location @r@.
    -- User code may assume that 'store' succeeds, so this should be
    -- implemented as 'mzero' on failure.
    store   :: r -> a -> m ()
    -- | Restore a previously stored value. 
    --
    -- @
    -- x <- do { store r a; restore r }
    -- @
    --
    -- implies
    --
    -- @
    -- x == Just a || x == Nothing
    -- @
    --
    restore :: r -> m (Maybe a)


-- | @Serial a@ represents the FilePath of a stored serialized value of type
-- @a@.
newtype Serial a = Serial { serialPath :: FilePath }

instance (Serialize a) => Resource (Serial a) IO a where
    store   (Serial fp) = L.writeFile fp . encodeLazy
    restore (Serial fp) =
        either (const Nothing) Just . decodeLazy <$> L.readFile fp
