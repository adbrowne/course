{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateT where

import Course.Core
import Course.Id
import Course.Optional
import Course.List
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor f => Functor (StateT s f) where
  (<$>) ::
    (a -> b)
    -> StateT s f a
    -> StateT s f b
  (<$>) f sa =
    StateT applyState
     where
        f2 (a,s) = (f a,s)
        applyState s =
              let fas = runStateT sa s in
              f2 <$> fas

mapFst :: forall t t1 t2. (t -> t1) -> (t, t2) -> (t1, t2)
mapFst f (a,b) = (f a, b)

-- | Implement the `Apply` instance for @StateT s f@ given a @Bind f@.
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Bind f => Apply (StateT s f) where
  (<*>) ::
    StateT s f (a -> b)
    -> StateT s f a
    -> StateT s f b
  (<*>) sf sa =
    StateT applyState
     where
       blah (f,s2) =
         let fas3 = runStateT sa s2 in
         mapFst f <$> fas3
       applyState s =
         let ffs2 = runStateT sf s in
         let blah2 = blah <$> ffs2 in
         join blah2

-- | Implement the `Applicative` instance for @StateT s f@ given a @Applicative f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
instance Monad f => Applicative (StateT s f) where
  pure ::
    a
    -> StateT s f a
  pure a =
    StateT (\s -> pure (a,s))

-- | Implement the `Bind` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
instance Monad f => Bind (StateT s f) where
  (=<<) ::
    (a -> StateT s f b)
    -> StateT s f a
    -> StateT s f b
  (=<<) f sa =
    StateT applyState
     where
       blah (a,s2) =
         runStateT (f a) s2
       applyState s1 =
         let fas = runStateT sa s1 in
         let blah2 = blah <$> fas in
         join blah2

instance Monad f => Monad (StateT s f) where

-- | A `State'` is `StateT` specialised to the `Id` functor.
type State' s a =
  StateT s Id a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- Id ((),1)
state' ::
  (s -> (a, s))
  -> State' s a
state' f =
  StateT applyState
   where applyState s = Id $ f s

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' sa s =
  let Id (a,s0) = runStateT sa s in
  (a, s0)

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  Functor f =>
  StateT s f a
  -> s
  -> f s
execT sa s =
  snd <$> runStateT sa s

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a
  -> s
  -> s
exec' sa s =
  let Id s = execT sa s in
  s

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT sa s =
  fst <$> runStateT sa s

-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a
  -> s
  -> a
eval' sa s =
  let Id a = evalT sa s in
  a

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  Monad f =>
  StateT s f s
getT =
  StateT (\s -> pure (s,s))

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT ::
  Monad f =>
  s
  -> StateT s f ()
putT s =
  StateT applyState
   where applyState _ =
           pure ((),s)

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' xs =
  let blah = (filterM pred xs)
  in eval blah S.empty
   where
        pred :: Ord a => a -> State (S.Set a) Bool
        pred a = State (stateFunc a)
        stateFunc a s =
           case S.member a s of
             True -> (False, s)
             False -> (True, S.insert a s)

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF xs =
  evalT (filtering pred xs) S.empty
   where
    pred a = StateT (applyState a)
    applyState a s =
        case a > 100 of
        True -> Empty
        _ ->
            case S.member a s of
            True -> pure (False, s)
            False -> pure (True, S.insert a s)

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  (<$>) f a =
    OptionalT runOptional'
     where
       fo (Full x) = Full (f x)
       fo Empty = Empty
       runOptional' =
        let fa = runOptionalT a in
        let fb = fo <$> fa in
        fb

-- | Implement the `Apply` instance for `OptionalT f` given a Apply f.
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Apply f => Apply (OptionalT f) where
  (<*>) af aa =
    OptionalT runOptional'
     where
       appF (Full f) (Full a) = Full (f a)
       appF _ _ = Empty
       runOptional' =
        let aff = runOptionalT af in
        let aaa = runOptionalT aa in
        appF <$> aff <*> aaa

-- | Implement the `Applicative` instance for `OptionalT f` given a Applicative f.
instance Applicative f => Applicative (OptionalT f) where
  pure a =
    OptionalT (pure (Full a))

-- | Implement the `Bind` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Bind (OptionalT f) where
  (=<<) (f :: a -> OptionalT f b) ma =
    OptionalT runOptional'
     where
       blah :: Optional a -> f (Optional b)
       blah (Full a) = runOptionalT (f a)
       blah Empty = pure Empty
       runOptional' =
        let aaa = runOptionalT ma in
        let b = blah <$> aaa in
        join b

instance Monad f => Monad (OptionalT f) where

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  (<$>) f x =
    let Logger list a = x
    in Logger list (f a)

-- | Implement the `Apply` instance for `Logger`.
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Apply (Logger l) where
  (<*>) lf lx =
    let Logger xs f = lf in
    let Logger ys x = lx in
    Logger (xs ++ ys) (f x)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
instance Applicative (Logger l) where
  pure =
    Logger Nil

-- | Implement the `Bind` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Bind (Logger l) where
  (=<<) f ma =
    let Logger lista a = ma in
    let Logger listb b = f a in
    Logger (lista ++ listb) b

instance Monad (Logger l) where

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l
  -> a
  -> Logger l a
log1 l1 =
  Logger (l1 :. Nil)

liftOptional :: f (a -> b) -> OptionalT f (a -> b)
liftOptional f =
  OptionalT $ (\x -> Full x) <$> f

addLog :: Chars -> OptionalT (Logger Chars) (Bool, S.Set a) -> OptionalT (Logger Chars) (Bool, S.Set a)
addLog msg a =
  foo <*> a
  where
    foo = OptionalT $ log1 msg (Full id)

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG ::
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG xs =
  let
    p a = StateT (applyState a)
    opt = evalT (filtering p xs) S.empty
  in  runOptionalT opt
   where
     normal a s =
       let
         result = 
            case S.member a s of
            True -> pure (False, s)
            False -> pure (True, S.insert a s) 
       in
         case even a of
         True ->
           let 
             msg = ("even number: " :: List Char) ++ (show' a)
           in addLog msg result
         False -> result
     applyState a s =
       case a > 100 of
       False -> normal a s
       True ->
         let msg = "aborting > 100: " ++ (show' a)
         in OptionalT $ log1 msg Empty
