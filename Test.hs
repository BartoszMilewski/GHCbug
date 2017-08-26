{-# LANGUAGE RankNTypes #-}
module Test where
  
import Data.Profunctor
  
proj :: Profunctor p => forall c. (forall a. p a a) -> p c c
proj e = e

f1 :: Profunctor p => (a -> b) -> (forall c. p c c) -> p a b 
f1 f e = dimap f id (proj e)

{-
• Couldn't match type ‘p c0 c0’ with ‘forall a1. p a1 a1’
  Expected type: p c0 c0 -> p a a
    Actual type: (forall a1. p a1 a1) -> p a a
• In the second argument of ‘(.)’, namely ‘proj’
  In the expression: dimap id f . proj
  In an equation for ‘f2’: f2 f = dimap id f . proj
• Relevant bindings include
    f2 :: (a -> b) -> (forall c. p c c) -> p a b
      (bound at 24:1)
-}
f2 :: Profunctor p => (a -> b) -> (forall c. p c c) -> p a b 
f2 f = dimap id f . proj

{-
• Cannot instantiate unification variable ‘a0’
  with a type involving foralls: (forall c. p c c) -> p a b
    GHC doesn't yet support impredicative polymorphism
• In the expression: undefined
  In an equation for ‘f3’: f3 f = undefined
• Relevant bindings include
    f :: a -> b
      (bound at 39:4)
    f3 :: (a -> b) -> (forall c. p c c) -> p a b
      (bound at 39:1)
-}

f3 :: Profunctor p => (a -> b) -> (forall c. p c c) -> p a b 
f3 f = undefined -- dimap id f . proj
