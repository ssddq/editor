{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE RankNTypes #-}

module Common.Utils where

import GHC.Types

{-# INLINE (|*) #-}
(|*)
  :: forall r a (b :: TYPE r)
   . (a -> b)
  -> a
  -> b
(|*) f x =  f x

{-# INLINE (|-) #-}
(|-)
  :: forall r a (b :: TYPE r)
   . (a -> b)
  -> a
  -> b
(|-) f x =  f x

infixl 2 |*
infixl 3 |-
