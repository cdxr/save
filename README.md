save
====

`save` is an experimental Haskell library for managing application state
between sessions. It provides two modules: `Data.Save`and `Data.Resource.
This package depends on several GHC language extensions.

## Data.Resource

`Data.Resource` defines the typeclass `Resource r m a`, representing a resource
`r` that stores and recalls an `a` in the monad `m`. Often times `m` will be
`IO` and `r` will be some sort of handle to the resource. The type `Serial`
represents a filepath to a value, and provides an instance
`(Serialize a) => Resource (Serial a) IO a`.

## Data.Save

`Data.Save` provides the type `Save s r a` where `s` is either the type
`Fresh` or `Changed` (this is enforced with the DataKinds and KindSignatures
extensions) and `r` is an instance of `Resource` (see Data.Resource).

A `Save s r a` value contains a value of `a` as well as a value of `r`, a
resource which points to an `a`, and the type-level `s` (either `Fresh` or
`Changed`) which tracks whether `a` has been modified since it was last read
from or written to the resource.

A user of the library encapsulates a value `a` in a `Save Fresh r a` with the
function `new`. The function `update` is then used to apply a function of type
`a -> a` to the encapsulated value, resulting in a `Save Changed r a`.

A value of `Save Changed r a` can only be changed to `Save Fresh r a` with the
function `save`, which depends on a `Resource r m a` instance. Library users
may therefore use the `Save` type in function signatures to statically enforce
properties pertaining to saved data. For instance, one might write a function
`quitApplication :: Save Fresh IO a -> IO ()`.
