module SK where

undefined :: a

data K0
data S0
data App x y
data Other a

data Done
data More

class CombineDone d1 d2 d | d1 d2 -> d
instance CombineDone Done Done Done
instance CombineDone Done More More
instance CombineDone More Done More
instance CombineDone More More More


class Eval1 x y d | x -> y d

instance Eval1 S0        S0         Done
instance Eval1 K0        K0         Done
instance Eval1 (Other a) (Other a)  Done

instance Eval1 x x' d => Eval1 (App K0 x) (App K0 x') d
instance Eval1 x x' d => Eval1 (App S0 x) (App S0 x') d
instance ( Eval1 x x' d1
         , Eval1 y y' d2
         , CombineDone d1 d2 d
         ) => Eval1 (App (App S0 x) y) (App (App S0 x') y') d

instance Eval1 x x' d => Eval1 (App (Other a) x) (App (Other a) x') d
instance ( Eval1 x x' d1
         , Eval1 y y' d2
         , CombineDone d1 d2 d
         ) => Eval1 (App (App (Other a) x ) y )
                    (App (App (Other a) x') y') d
instance ( Eval1 x x' d1
         , Eval1 y y' d2
         , Eval1 z z' d3
         , CombineDone d1 d2 d4
         , CombineDone d3 d4 d
         ) => Eval1 (App (App (App (Other a) x ) y ) z )
                    (App (App (App (Other a) x') y') z') d

instance Eval1      (App (App K0 x) y)         x     More
instance Eval1 (App (App (App K0 x) y) z) (App x z)  More


instance Eval1 
            (App (App (App S0 f) g) x)
            (App (App f x) (App g x))
            More
instance (    Eval1      (App (App (App p q) x) y)         a     d
         ) => Eval1 (App (App (App (App p q) x) y) z) (App a z)  d


class EvalAux x y q1 | x q1 -> y

instance EvalAux x x Done

instance (    Eval1   x y q
         ,    EvalAux y z q
         ) => EvalAux x z More

class Eval x y | x -> y
instance EvalAux x y More => Eval x y


eval1 :: Eval1 x y q => x -> y
eval1 = undefined

eval :: Eval x y => x -> y
eval = undefined

bot :: a
bot = undefined

data P0
data Q0
data R0

type P = Other P0
type Q = Other Q0
type R = Other R0

testK = eval (bot :: K P Q)   :: P
testS = eval (bot :: S P Q R) :: App (App P R) (App Q R)

test1 = eval (bot :: I P)            :: P
test2 = eval (bot :: C K0 I0 P)      :: P
test3 = eval (bot :: App (Y KI0) P)  :: P

test4 = eval (bot :: M P)     :: App P P
test5 = eval (bot :: W P Q)   :: App (App P Q) Q
test6 = eval (bot :: C P Q R) :: App (App P R) Q
test7 = eval (bot :: B P Q R) :: App P (App Q R)

-- Uncomment the following line to cause GHC to diverge.

--testOmega = eval (bot :: OMEGA)

--undefined :: a

type FoldN n = App (App n P) Q

type Z      = SK0
type Succ0  = App S0 (App (App S0 KS0) K0)

type Plus0  = App (App S0 KS0) (App (App S0 (App K0 (App S0 (App K0 S0))))
                                    (App S0 KK0))
type Mult0  = App (App S0 KS0) K0

type Succ a   = App Succ0 a
type Mult n m = App (App Mult0 n) m
type Plus n m = App (App Plus0 n) m

type One    = Succ Z
type Two    = Succ One
type Three  = Succ Two
type Four   = Succ Three
type Five   = Succ Four

test_n1 = eval (bot :: FoldN Z)   :: Q
test_n2 = eval (bot :: FoldN One) :: App P Q

test_n3 = eval (bot :: FoldN (Plus Two Three))
test_n3 :: App P (App P (App P (App P (App P Q))))

test_n4 = eval (bot :: FoldN (Mult Two Four))
test_n4 :: App P (App P (App P (App P (App P (App P (App P (App P Q)))))))


type K x y   = App (App K0 x) y
type S f g x = App (App (App S0 f) g) x
type I x     = App SKK0 x
type B x y z = App (App (App B0 x) y) z
type C x y z = App (App (App C0 x) y) z
type M x     = App M0 x
type W f x   = App (App W0 f) x
type Y f     = App Y0 f

type I0      = SKK0
type B0      = App (App S0 KS0) K0
type C0      = App (App S0 (App (App B0 B0) S0)) KK0
type W0      = App C0 (App (App B0 M0) (App (App B0 B0) (App C0 I0)))
type L0      = App (App C0 B0) M0
type Y0      = App (App S0 L0) L0
type KI0     = App K0 I0

type M0      = App (App S0 I0) I0
type OMEGA   = App M0 M0

type KK0     = App K0 K0
type KS0     = App K0 S0
type KK x    = App KK0 x
type KS x    = App KS0 x

type SK0     = App S0 K0
type SS0     = App S0 S0
type SKK0    = App (App S0 K0) K0
type SKS0    = App (App S0 K0) S0
type SSK0    = App (App S0 S0) K0
type SSS0    = App (App S0 S0) S0
type SK f x  = App (App SK0 f) x
type SS f x  = App (App SS0 f) x
type SKK x   = App SKK0 x
type SKS x   = App SKS0 x
type SSK x   = App SSK0 x
type SSS x   = App SSS0 x
