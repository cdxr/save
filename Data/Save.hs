{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Data.Save
(
  Save
, Saved ( Fresh, Changed )
, new
, condition
, resource
, value
, isDirty
, fromSave
, setResource
, update
, sully
, load
, save
, saveAs
)
where

import Control.Applicative
import Control.Monad
import Data.Copointed

import Data.Resource


-- | A value indicating whether information has been modified since it was
-- last stored on disk.
-- Isomorphic to Bool.
--
data Saved = Fresh | Changed
  deriving (Show, Eq, Ord)

-- | @Save s r a@ is a value @a@ annotated with a resource location @r@ and
-- an @s@ that indicates whether it has been modified since it was last
-- stored on @r@.
--
data Save (s :: Saved) r a where
    Clean :: r -> a -> Save Fresh r a
    Dirty :: r -> a -> Save Changed r a

deriving instance (Show r, Show a) => Show (Save s r a)
deriving instance (Eq r, Eq a)     => Eq   (Save s r a)


-- | Construct a fresh value.
new :: a -> Save Fresh () a
new = Clean ()

condition :: Save s r a -> Saved
condition (Clean _ _) = Fresh
condition (Dirty _ _) = Changed

resource :: Save s r a -> r
resource (Clean r _) = r
resource (Dirty r _) = r

value :: Save s r a -> a
value (Clean _ a) = a
value (Dirty _ a) = a

isDirty :: Save s r a -> Bool
isDirty (Clean _ _) = False
isDirty (Dirty _ _) = True

-- | A catamorphism for 'Save' in the spirit of 'maybe' and 'either'
fromSave :: (Saved -> r -> a -> b) -> Save s r a -> b
fromSave f = f <$> condition <*> resource <*> value


update :: (a -> a) -> Save s r a -> Save Changed r a
update f = Dirty <$> resource <*> f . value

sully :: Save s r a -> Save Changed r a
sully = update id


instance Copointed (Save s r) where
    copoint = value

instance Functor (Save Changed r) where
    fmap f = Dirty <$> resource <*> f . value

instance Applicative (Save Changed ()) where
    pure = Dirty ()
    Dirty () f <*> Dirty () a = Dirty () (f a)

instance Monad (Save Changed ()) where
    return = Dirty ()
    Dirty () x >>= f = f x

-- | Restore a 'Save' from a 'Resource'.
load :: (Monad m, Resource r m a) => r -> m (Maybe (Save Fresh r a))
load r = fmap (Clean r) `liftM` restore r

-- | Store a 'Save', if necessary. This is implemented in terms of 'store'.
-- The 'Resource' instance is responsible for ensuring that this does not
-- return unless the value is successfully commited.
-- 
save :: (Monad m, Resource r m a) => Save s r a -> m (Save Fresh r a)
save (Clean r a) = return $ Clean r a
save (Dirty r a) = store r a >> return (Clean r a)

-- | Store a 'Save' to a secondary resource.
saveAs :: (Monad m, Resource r' m a) => r' -> Save s r a -> m (Save Fresh r' a)
saveAs r = save . setResource r

setResource :: r' -> Save s r a -> Save Changed r' a
setResource r = Dirty r . value
