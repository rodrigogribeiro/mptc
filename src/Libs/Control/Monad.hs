module Control.Monad
    (
    -- * Functor and monad classes

      Functor(fmap)
    , Monad((>>=), (>>), return, fail)

    , MonadPlus (   -- class context: Monad
          mzero     -- :: (MonadPlus m) => m a
        , mplus     -- :: (MonadPlus m) => m a -> m a -> m a
        )
    -- * Functions

    -- ** Naming conventions
    -- $naming

    -- ** Basic @Monad@ functions

    , mapM          -- :: (Monad m) => (a -> m b) -> [a] -> m [b]
    , mapM_         -- :: (Monad m) => (a -> m b) -> [a] -> m ()
    , forM          -- :: (Monad m) => [a] -> (a -> m b) -> m [b]
    , forM_         -- :: (Monad m) => [a] -> (a -> m b) -> m ()
    , sequence      -- :: (Monad m) => [m a] -> m [a]
    , sequence_     -- :: (Monad m) => [m a] -> m ()
    , (=<<)         -- :: (Monad m) => (a -> m b) -> m a -> m b
    , (>=>)         -- :: (Monad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
    , (<=<)         -- :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
    , forever       -- :: (Monad m) => m a -> m b
    , void

    -- ** Generalisations of list functions

    , join          -- :: (Monad m) => m (m a) -> m a
    , msum          -- :: (MonadPlus m) => [m a] -> m a
    , mfilter       -- :: (MonadPlus m) => (a -> Bool) -> m a -> m a
    , filterM       -- :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
    , mapAndUnzipM  -- :: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
    , zipWithM      -- :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
    , zipWithM_     -- :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m ()
    , foldM         -- :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a 
    , foldM_        -- :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m ()
    , replicateM    -- :: (Monad m) => Int -> m a -> m [a]
    , replicateM_   -- :: (Monad m) => Int -> m a -> m ()

    -- ** Conditional execution of monadic expressions

    , guard         -- :: (MonadPlus m) => Bool -> m ()
    , when          -- :: (Monad m) => Bool -> m () -> m ()
    , unless        -- :: (Monad m) => Bool -> m () -> m ()

    -- ** Monadic lifting operators

    , liftM         -- :: (Monad m) => (a -> b) -> (m a -> m b)
    , liftM2        -- :: (Monad m) => (a -> b -> c) -> (m a -> m b -> m c)
    , liftM3        -- :: ...
    , liftM4        -- :: ...
    , liftM5        -- :: ...

    , ap            -- :: (Monad m) => m (a -> b) -> m a -> m b

    ) where

import Data.Maybe
import Base

infixr 1 =<<


-- -----------------------------------------------------------------------------
-- The MonadPlus class definition

-- | Monads that also support choice and failure.
class Monad m => MonadPlus m where
   -- | the identity of 'mplus'.  It should also satisfy the equations
   --
   -- > mzero >>= f  =  mzero
   -- > v >> mzero   =  mzero
   --
   mzero :: m a 
   -- | an associative operation
   mplus :: m a -> m a -> m a

instance MonadPlus [] where
   mzero = []
   mplus = (++)

instance MonadPlus Maybe where
   mzero = Nothing

   Nothing `mplus` ys  = ys
   xs      `mplus` _ys = xs

-- -----------------------------------------------------------------------------
-- Functions mandated by the Prelude

-- | @'guard' b@ is @'return' ()@ if @b@ is 'True',
-- and 'mzero' if @b@ is 'False'.

guard           :: (MonadPlus m) => Bool -> m ()
guard True      =  return ()
guard False     =  mzero

-- | This generalizes the list-based 'filter' function.

filterM          :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM _ []     =  return []
filterM p (x:xs) =  do
   flg <- p x
   ys  <- filterM p xs
   return (if flg then x:ys else ys)

-- | 'forM' is 'mapM' with its arguments flipped
forM            :: Monad m => [a] -> (a -> m b) -> m [b]
{-# INLINE forM #-}
forM            = flip mapM

-- | 'forM_' is 'mapM_' with its arguments flipped
forM_           :: Monad m => [a] -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_           = flip mapM_

-- | This generalizes the list-based 'concat' function.

msum        :: MonadPlus m => [m a] -> m a
{-# INLINE msum #-}
msum        =  foldr mplus mzero

infixr 1 <=<, >=>

-- | Left-to-right Kleisli composition of monads.
(>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g     = \x -> f x >>= g

-- | Right-to-left Kleisli composition of monads. @('>=>')@, with the arguments flipped
(<=<)       :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<)       = flip (>=>)

-- | @'forever' act@ repeats the action infinitely.
forever     :: (Monad m) => m a -> m b
forever a   = a >> forever a

-- | @'void' value@ discards or ignores the result of evaluation, such as the return value of an 'IO' action.
void :: Functor f => f a -> f ()
void = fmap (const ())

-- -----------------------------------------------------------------------------
-- Other monad functions

-- | The 'join' function is the conventional monad join operator. It is used to
-- remove one level of monadic structure, projecting its bound argument into the
-- outer level.
join              :: (Monad m) => m (m a) -> m a
join x            =  x >>= id

-- | The 'mapAndUnzipM' function maps its first argument over a list, returning
-- the result as a pair of lists. This function is mainly used with complicated
-- data structures or a state-transforming monad.
mapAndUnzipM      :: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
mapAndUnzipM f xs =  (sequence (map f xs)) >>= (return . unzip)

-- | The 'zipWithM' function generalizes 'zipWith' to arbitrary monads.
zipWithM          :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys  =  sequence (zipWith f xs ys)

-- | 'zipWithM_' is the extension of 'zipWithM' which ignores the final result.
zipWithM_         :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs ys =  sequence_ (zipWith f xs ys)

{- | The 'foldM' function is analogous to 'foldl', except that its result is
encapsulated in a monad. Note that 'foldM' works from left-to-right over
the list arguments. This could be an issue where @('>>')@ and the `folded
function' are not commutative.


>       foldM f a1 [x1, x2, ..., xm]

==  

>       do
>         a2 <- f a1 x1
>         a3 <- f a2 x2
>         ...
>         f am xm

If right-to-left evaluation is required, the input list should be reversed.
-}

foldM             :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM _ a []      =  return a
foldM f a (x:xs)  =  f a x >>= \fax -> foldM f fax xs

-- | Like 'foldM', but discards the result.
foldM_            :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m ()
foldM_ f a xs     = foldM f a xs >> return ()

-- | @'replicateM' n act@ performs the action @n@ times,
-- gathering the results.
replicateM        :: (Monad m) => Int -> m a -> m [a]
replicateM n x    = sequence (replicate n x)

-- | Like 'replicateM', but discards the result.
replicateM_       :: (Monad m) => Int -> m a -> m ()
replicateM_ n x   = sequence_ (replicate n x)

{- | Conditional execution of monadic expressions. For example, 

>       when debug (putStr "Debugging\n")

will output the string @Debugging\\n@ if the Boolean value @debug@ is 'True',
and otherwise do nothing.
-}

when              :: (Monad m) => Bool -> m () -> m ()
when p s          =  if p then s else return ()

-- | The reverse of 'when'.

unless            :: (Monad m) => Bool -> m () -> m ()
unless p s        =  if p then return () else s

-- | Promote a function to a monad.
liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM f m1              = do { x1 <- m1; return (f x1) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right.  For example,
--
-- >    liftM2 (+) [0,1] [0,2] = [0,2,1,3]
-- >    liftM2 (+) (Just 1) Nothing = Nothing
--
liftM2  :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f m1 m2          = do { x1 <- m1; x2 <- m2; return (f x1 x2) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2').
liftM3  :: (Monad m) => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM3 f m1 m2 m3       = do { x1 <- m1; x2 <- m2; x3 <- m3; return (f x1 x2 x3) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2').
liftM4  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
liftM4 f m1 m2 m3 m4    = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; return (f x1 x2 x3 x4) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2').
liftM5  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
liftM5 f m1 m2 m3 m4 m5 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; return (f x1 x2 x3 x4 x5) }

{- | In many situations, the 'liftM' operations can be replaced by uses of
'ap', which promotes function application. 

>       return f `ap` x1 `ap` ... `ap` xn

is equivalent to 

>       liftMn f x1 x2 ... xn

-}

ap                :: (Monad m) => m (a -> b) -> m a -> m b
ap                =  liftM2 id


-- -----------------------------------------------------------------------------
-- Other MonadPlus functions

-- | Direct 'MonadPlus' equivalent of 'filter'
-- @'filter'@ = @(mfilter:: (a -> Bool) -> [a] -> [a]@
-- applicable to any 'MonadPlus', for example
-- @mfilter odd (Just 1) == Just 1@
-- @mfilter odd (Just 2) == Nothing@

mfilter :: (MonadPlus m) => (a -> Bool) -> m a -> m a
mfilter p ma = do
  a <- ma
  if p a then return a else mzero

-- XXX mfilter and guard
