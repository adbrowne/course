{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind
import Course.Monad
import qualified Data.Set as S

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- | Implement the `Functor` instance for `State s`.
-- >>> runState ((+1) <$> pure 0) 0
-- (1,0)
instance Functor (State s) where
  (<$>) ::
    (a -> b)
    -> State s a
    -> State s b
  (<$>) f v =
      State applyState
   where
       applyState sa =
            let (a,s) = runState v sa in
            (f a, s)


-- | Implement the `Apply` instance for `State s`.
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> import qualified Prelude as P
-- >>> runState (State (\s -> ((+3), s P.++ ["apple"])) <*> State (\s -> (7, s P.++ ["banana"]))) []
-- (10,["apple","banana"])
instance Apply (State s) where
  (<*>) ::
    State s (a -> b)
    -> State s a
    -> State s b
  (<*>) sf sa =
    State applyState
   where
     applyState s =
       let (f,s1) = runState sf s in
       let (a,s2) = runState sa s1 in
       (f a, s2)

-- | Implement the `Applicative` instance for `State s`.
-- >>> runState (pure 2) 0
-- (2,0)
instance Applicative (State s) where
  pure ::
    a
    -> State s a
  pure a =
    State (\s -> (a,s))

-- | Implement the `Bind` instance for `State s`.
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
instance Bind (State s) where
  (=<<) ::
    (a -> State s b)
    -> State s a
    -> State s b
  (=<<) f sa =
    State fa
     where
       fa s =
         let (a,s2) = runState sa s in
         let sb = f a in
         runState sb s2

instance Monad (State s) where

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) -> exec (State f) s == snd (runState (State f) s)
exec ::
  State s a
  -> s
  -> s
exec state s =
  snd $ runState state s

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) -> eval (State f) s == fst (runState (State f) s)
eval ::
  State s a
  -> s
  -> a
eval state s =
  fst $ runState state s

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get ::
  State s s
get =
  State (\s -> (s,s))

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put ::
  s
  -> State s ()
put s =
  State (\_ -> ((), s))

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
findM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM _ Nil = return Empty
findM pred (x :. xs) =
  let mt = pred x in
  loop =<< mt
   where loop True = return $ Full x
         loop False = findM pred xs

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
firstRepeat ::
  Ord a =>
  List a
  -> Optional a
firstRepeat xs =
  let findResult = (findM pred xs)
  in eval findResult S.empty
   where
        pred :: Ord a => a -> State (S.Set a) Bool
        pred a = State (stateFunc a)
        stateFunc a s =
          case S.member a s of
            True -> (True, s)
            False -> (False, S.insert a s)

filterM' ::
  Monad f =>
  List a
  -> (a -> f Bool)
  -> List a
  -> f (List a)
filterM' acc _ Nil = return $ reverse acc
filterM' acc pred (x :. xs) =
  let mt = pred x in
  loop =<< mt
   where loop True = filterM' (x :. acc) pred xs
         loop False = filterM' acc pred xs

filterM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (List a)
filterM pred xs =
  filterM' Nil pred xs

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> firstRepeat (distinct xs) == Empty
--
-- prop> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
distinct ::
  Ord a =>
  List a
  -> List a
distinct xs =
  let blah = (filterM pred xs)
  in eval blah S.empty
   where
        pred :: Ord a => a -> State (S.Set a) Bool
        pred a = State (stateFunc a)
        stateFunc a s =
           case S.member a s of
             True -> (False, s)
             False -> (True, S.insert a s)

sumOfDigitsSquare :: Integer -> Integer
sumOfDigitsSquare a =
  let digits = map (\x -> ord x - ord '0') $ show' a in
  let squared = map (\x -> x * x) digits in
  toInteger $ foldRight (+) 0 squared

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `findM` with `State` and `produce`.
--
-- /Tip:/ Use `flatten` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
isHappy ::
  Integer
  -> Bool
isHappy a =
   let sequence = produce sumOfDigitsSquare a in
   let result = findM pred sequence in
   case eval result S.empty of
      Full 1 -> True
      Full _ -> False
      Empty -> False
     where
        pred :: Ord a => a -> State (S.Set a) Bool
        pred a = State (stateFunc a)
        stateFunc a s =
           case S.member a s of
             True -> (True, s)
             False -> (False, S.insert a s)
