This is a literate Haskell file.
First of all, we will need a few imports and language extensions.

\begin{code}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Writer.Class
import Data.Kind

import qualified Control.Monad.Except as T
import qualified Control.Monad.Reader as T
import qualified Control.Monad.Writer as T
\end{code}

`Elevator` is a newtype wrapper for monad transformers.
We can use this to derive instances in a transformer stack.

\begin{code}
newtype Elevator
  (t :: (Type -> Type) -> Type -> Type)
  (m :: Type -> Type)
  (a :: Type)
    = Elevator { unElevator :: t m a }
  deriving newtype (Applicative, Functor, Monad)
\end{code}

As a prerequisite I will mention, that `MonadTrans t` and `MonadTransControl t` instances will be required.
Now we will implement a few instances for `Elevator t m`.

\begin{code}
instance (Monad (t m), MonadTransControl t, MonadError e m) => MonadError e (Elevator t m) where
  throwError = Elevator . lift . throwError
  catchError throwing catching = Elevator $ (restoreT . pure =<<) $ liftWith $ \ runT ->
    catchError (runT $ unElevator throwing) (runT . unElevator . catching)

instance (Monad (t m), MonadTransControl t, MonadReader r m) => MonadReader r (Elevator t m) where
  ask = Elevator $ lift ask
  local f tma = Elevator $ (restoreT . pure =<<) $ liftWith $ \ runT ->
    local f $ runT $ unElevator tma

instance (Monad (t m), MonadTrans t, MonadState s m) => MonadState s (Elevator t m) where
  get = Elevator $ lift get
  put = Elevator . lift . put

instance (Monad (t m), MonadTransControl t, MonadWriter w m) => MonadWriter w (Elevator t m) where
  tell = Elevator . lift . tell
  listen tma = Elevator $ liftWith (\ runT -> listen $ runT $ unElevator tma) >>= \ (sta, w) ->
    (, w) <$> restoreT (pure sta)
  pass tma = Elevator $ lift . pass . pure =<< unElevator tma
\end{code}

Everything up to this point should end up in a library.
Since `MonadTransControl` is not part of the "transformers" package, "monad-control" or a new package would be suitable I guess.

Now we will look at an example, that you might find in a lot of applications.
We will use a transformer stack with 3 transformers.
The exact choice of transformers is not important, as long as they have `MonadTransControl` instances.

\begin{code}
newtype AppT m a = AppT { unAppT :: T.ReaderT Int (T.WriterT [String] (T.ExceptT Bool m)) a}
  deriving newtype (Functor, Applicative, Monad)
\end{code}

We were able to derive `Functor`, `Applicative` and `Monad` instances as usual.
Now let's look at some other instances.
Unless noted, all of these instances can be derived (just not all together).

\begin{code}
  -- We can easily derive `MonadState s m => MonadState s (AppT m)`.
  -- Keep in mind, that this just works, because all of the transformers implement all of the
  -- classes as instances in mtl.
  -- This is very annoying to do for your own transformers and classes.
  -- This wouldn't work, if we had `StateT` in our transformer stack.
  deriving newtype (MonadState s)

  -- These instances cannot be derived with GeneralizedNewtypeDeriving,
--  deriving newtype (MonadReader r) -- Error: Can't match `r ~ Int`
--  deriving newtype (MonadWriter w) -- Error: Can't match `w ~ [String]`
--  deriving newtype (MonadError e) -- Error: Can't match `e ~ Bool`

  -- because our transformer stack already implements different instances, that we actually can
  -- derive:
--  deriving newtype (MonadReader Int)
--  deriving newtype (MonadWriter [String])
  deriving newtype (MonadError Bool)

  -- A lot of times we don't want these instances out of our transformer stack, but the ones from
  -- our base monad `m`.
  -- We can derive these using our `Elevator` instances.
  deriving (MonadReader r) via Elevator AppT m
  deriving (MonadWriter w) via Elevator AppT m
--  deriving (MonadError e) via Elevator AppT m
\end{code}

Now we can clearly see whether we get an instance out of our transformer stack, or from the base monad `m`.

If we want `MonadTrans AppT` and `MonadTransControl AppT` instances, we still have to implement those manually.

\begin{code}
instance MonadTrans AppT where
  lift = AppT . lift . lift . lift

instance MonadTransControl AppT where
  type StT AppT a = StT (T.ExceptT Bool) (StT (T.WriterT [String]) (StT (T.ReaderT Int) a))
  liftWith f = AppT $
    liftWith $ \ runT ->
      liftWith $ \ runT' ->
        liftWith $ \ runT'' ->
          f (runT'' . runT' . runT . unAppT)
  restoreT = AppT . restoreT . restoreT . restoreT
\end{code}

These instances can only be derived when actually composing transformers with `ComposeT`.

\begin{code}
-- | This main function is here, just to make cabal happy.
main :: IO ()
main = undefined
\end{code}
