
This message is literate Haskell so you can all follow along at home.
The program developed in this message was tested on GHC 6.4.2.

My purpose today is to show that the GHC typechecker with multi-parameter
typeclasses, functional dependencies, and undecidable instances is Turing-
complete.  Some time ago on this mailing list, there was a brief discussion
about a lambda calculus and a direct Turing-machine implementation; either
would be sufficent to demonstrate the Turing-completeness of the typechecker.
However, neither implementation was preserved on-list, and the links are
now dead.

My strategy will be to embed the SK combinator calculus.  The SK combinator
calculus is capable of embedding the lambda calculus, which is well-known
to be Turing-complete. Furthermore, the SK calculus is simpler to implement
than the lambda calculus.

For a complete proof of Turing-completeness, one should have a correctness
proof for the embedding.  I do not undertake such a proof here, but I will
demonstrate what I hope to be convincing evidence for the correctness of
this embedding.


I begin by throwing the switches that give me the needed extensions.

> {-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
> module SK where

Next I define the terms of the SK combinator basis.  This includes symbols for
the combinators themselves, a symbol for application, and a way to introduce
arbitrary uninterpreted types into computations.

> data K0
> data S0
> data App x y
> data Other a

Now I proceed to create the evaluator by first defining a single-step reduction
relation.  This relation is carefully designed to be reflexive on normal forms,
and always reduces the leftmost-outermost redex.  I then form the final
evaluation relation by taking the transitive closure of single-step reduction.

The semantics of typeclass constraint satisfaction are basicly strict.  That
is, the typechecker will make sure to fully evaluate all class constraints
and fully instantiate all types whether or not those types turn out to be
needed.  Because of this, I need to be careful to form the transitive closure
in a way that doesn't cause the typechecker to diverge on evaluation when it
should not (of course, the typecheker will diverge on divergent terms).

In order to do this, I define the single-step reduction relation so that it
returns an additional result in addition to the reduced term.  This result
indicates 'More' whenever a redex was found and reduced and 'Done' otherwise.

A few reduction rules perform reduction in parallel.  These rules will return
'More' if reduction was performed on any subterm.

> data Done
> data More

> class CombineDone d1 d2 d | d1 d2 -> d
> instance CombineDone Done Done Done
> instance CombineDone Done More More
> instance CombineDone More Done More
> instance CombineDone More More More


The 'Eval1' relation performs a single step of reduction.  The presentation
of this relation is somewhat tedious because we are forced to enumerate each
possible spine shape up to 4 'App's deep.  I could possibly do this more
concisely if I enabled overlapping instances.

> class Eval1 x y d | x -> y d

I start by providing axioms stating that atomic terms evaluate to themselves
with no reduction.

> instance Eval1 S0        S0         Done
> instance Eval1 K0        K0         Done
> instance Eval1 (Other a) (Other a)  Done

There are a number of cases which just propagate evaluation contexts underneath
the right-hand side of of 'App' when the spine shape cannot be reduced any
further.  These fairly uninteresting cases are collected together here.

> instance Eval1 x x' d => Eval1 (App K0 x) (App K0 x') d
> instance Eval1 x x' d => Eval1 (App S0 x) (App S0 x') d
> instance ( Eval1 x x' d1
>          , Eval1 y y' d2
>          , CombineDone d1 d2 d
>          ) => Eval1 (App (App S0 x) y) (App (App S0 x') y') d
>
> instance Eval1 x x' d => Eval1 (App (Other a) x) (App (Other a) x') d
>
> instance ( Eval1 x x' d1
>          , Eval1 y y' d2
>          , CombineDone d1 d2 d
>          ) => Eval1 (App (App (Other a) x ) y )
>                     (App (App (Other a) x') y') d
>
> instance ( Eval1 x x' d1
>          , Eval1 y y' d2
>          , Eval1 z z' d3
>          , CombineDone d1 d2 d4
>          , CombineDone d3 d4 d
>          ) => Eval1 (App (App (App (Other a) x ) y ) z )
>                     (App (App (App (Other a) x') y') z') d

Now we get to the real meat.  Here are the rules for reducing the 'K'
combinator.  There are rules for reducing under both 2 and 3 'App's.

> instance Eval1      (App (App K0 x) y)         x     More
> instance Eval1 (App (App (App K0 x) y) z) (App x z)  More

And here, the rule for reducing the 'S' combinator.

> instance Eval1 
>             (App (App (App S0 f) g) x)
>             (App (App f x) (App g x))
>             More

Finally, a rule to decompose an arbitrary 'App' context. I propagate
the evaluation context down the left side of an 'App' only when that
'App' is not involved in any redexes. This implements the leftmost-
outermost strategy.  In any leftmost spine consisting of 4 'App's,
the leftmost 'App' can never be involved in any redex. The cases for
other levels of 'App's are folded into the above rules.

> instance (    Eval1      (App (App (App p q) x) y)         a     d
>          ) => Eval1 (App (App (App (App p q) x) y) z) (App a z)  d


Now I need to take the transitive closure of the 'Eval1' relation.  I do
this with the auxiliary 'EvalAux' class.  The first instance represents
the base case, where 'Eval1' performed no reductions.  The second instance
implements the transitivity and does bookkeeping with the termination markers.

> class EvalAux x y q1 | x q1 -> y

> instance EvalAux x x Done

> instance (    Eval1   x y q
>          ,    EvalAux y z q
>          ) => EvalAux x z More


Now at last I can define the evaluation relation.  It is defined in terms of
'EvalAux' and hides the termination bookkeeping.

> class Eval x y | x -> y
> instance EvalAux x y More => Eval x y


I define convenient functions to allow me to invoke the type evaluators
and a synonym for 'undefined' to cut down on typing.

> eval1 :: Eval1 x y q => x -> y
> eval1 = undefined

> eval :: Eval x y => x -> y
> eval = undefined

> bot :: a
> bot = undefined

Before showing off the evaluator, I'm going to need several dummy types
to work with.

> data P0
> data Q0
> data R0

> type P = Other P0
> type Q = Other Q0
> type R = Other R0

Now, some examples of the SK evaluator in action.  First I show
that the S and K combinators have their expected axiomatic behavior.
(Note, the following demonstrations rely on definitions found in the
appendix at the end).

> testK = eval (bot :: K P Q)   :: P
> testS = eval (bot :: S P Q R) :: App (App P R) (App Q R)

Now I demonstrate several different ways of writing the identity combinator.

> test1 = eval (bot :: I P)            :: P
> test2 = eval (bot :: C K0 I0 P)      :: P
> test3 = eval (bot :: App (Y KI0) P)  :: P

Axiomatic demonstrations of several other important combinators.

> test4 = eval (bot :: M P)     :: App P P
> test5 = eval (bot :: W P Q)   :: App (App P Q) Q
> test6 = eval (bot :: C P Q R) :: App (App P R) Q
> test7 = eval (bot :: B P Q R) :: App P (App Q R)

Uncomment the following line to cause GHC to diverge.

--testOmega = eval (bot :: OMEGA)

Finally, a presentation of the church numerals in combinatory form.

> type FoldN n = App (App n P) Q

> type Z      = SK0
> type Succ0  = App S0 (App (App S0 KS0) K0)

> type Plus0  = App (App S0 KS0) (App (App S0 (App K0 (App S0 (App K0 S0))))
>                                     (App S0 KK0))
> type Mult0  = App (App S0 KS0) K0

> type Succ a   = App Succ0 a
> type Mult n m = App (App Mult0 n) m
> type Plus n m = App (App Plus0 n) m

> type One    = Succ Z
> type Two    = Succ One
> type Three  = Succ Two
> type Four   = Succ Three
> type Five   = Succ Four

> test_n1 = eval (bot :: FoldN Z)   :: Q
> test_n2 = eval (bot :: FoldN One) :: App P Q

> test_n3 = eval (bot :: FoldN (Plus Two Three))
> test_n3 :: App P (App P (App P (App P (App P Q))))

> test_n4 = eval (bot :: FoldN (Mult Two Four))
> test_n4 :: App P (App P (App P (App P (App P (App P (App P (App P Q)))))))


=================================================
Appendix:  definitions of additional combinators


> type K x y   = App (App K0 x) y
> type S f g x = App (App (App S0 f) g) x
> type I x     = App SKK0 x
> type B x y z = App (App (App B0 x) y) z
> type C x y z = App (App (App C0 x) y) z
> type M x     = App M0 x
> type W f x   = App (App W0 f) x
> type Y f     = App Y0 f

> type I0      = SKK0
> type B0      = App (App S0 KS0) K0
> type C0      = App (App S0 (App (App B0 B0) S0)) KK0
> type W0      = App C0 (App (App B0 M0) (App (App B0 B0) (App C0 I0)))
> type L0      = App (App C0 B0) M0
> type Y0      = App (App S0 L0) L0
> type KI0     = App K0 I0

> type M0      = App (App S0 I0) I0
> type OMEGA   = App M0 M0

> type KK0     = App K0 K0
> type KS0     = App K0 S0
> type KK x    = App KK0 x
> type KS x    = App KS0 x

> type SK0     = App S0 K0
> type SS0     = App S0 S0
> type SKK0    = App (App S0 K0) K0
> type SKS0    = App (App S0 K0) S0
> type SSK0    = App (App S0 S0) K0
> type SSS0    = App (App S0 S0) S0
> type SK f x  = App (App SK0 f) x
> type SS f x  = App (App SS0 f) x
> type SKK x   = App SKK0 x
> type SKS x   = App SKS0 x
> type SSK x   = App SSK0 x
> type SSS x   = App SSS0 x
