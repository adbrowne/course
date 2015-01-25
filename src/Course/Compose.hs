{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  (<$>) f c =
    let 
      Compose i = c
      cb =(\ga -> f <$> ga) <$> i
    in 
      Compose cb

instance (Apply f, Apply g) =>
  Apply (Compose f g) where
-- Implement the (<*>) function for an Apply instance for Compose
  Compose f <*> Compose a =
      let 
        bar = (<*>)
        foo = bar <$> f
      in
        Compose (foo <*> a)

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure a =
    Compose $ pure (pure a) 

instance (Bind f, Bind g) =>
  Bind (Compose f g) where
-- Implement the (=<<) function for a Bind instance for Compose
-- I don't think this can be done because we have no way to break
-- down f to build it up gradually
  (=<<) =
    error "todo"
    {--let 
      Compose fga = ma
    in
      Compose $ f =<< fga --}
