module Control.Disposable
    ( Disposable -- constructor not exported
    , runDisposable
    , Dispose(..)
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.IORef
import Data.Semigroup
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Foreign.Export as J

-- | A wrapper around authorized IO actions.
newtype Disposable = Disposable { runDisposable :: IO () }

instance Semigroup Disposable where
    (Disposable f) <> (Disposable g) = Disposable (f >> g)

instance Monoid Disposable where
    mempty = Disposable (pure ())
    mappend = (<>)

-- | A 'Dispose' is something with some resources to release
class Dispose a where
    dispose :: a -> Disposable

instance Dispose Disposable where
    dispose = id

instance Dispose (J.Callback a) where
    dispose = Disposable . J.releaseCallback

instance Dispose (J.Export a) where
    dispose = Disposable . J.releaseExport

instance Dispose a => Dispose (TVar a) where
    dispose a = Disposable . join $ (runDisposable . dispose) <$> atomically (readTVar a)

instance Dispose a => Dispose (TMVar a) where
    dispose a = Disposable . join $
        (runDisposable . dispose) <$> atomically (readTMVar a)

instance Dispose a => Dispose (IORef a) where
    dispose a = Disposable . join $
        (runDisposable . dispose) <$> readIORef a

instance Dispose a => Dispose (MVar a) where
    dispose a = Disposable . join $
        (runDisposable . dispose) <$> readMVar a
