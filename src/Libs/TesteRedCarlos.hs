{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, NoMonomorphismRestriction, UndecidableInstances #-}

--import Base

class App a b where
  app:: a -> Either b (a->b) -> b

data A a = A [a]
data B a = B [a]

appA a@(A _) = app a -- appA: App (A a) b => A a -> Either b (A a -> b) -> b
appB b@(B _) = app b -- appB: App (B a) b => B a -> Either b (B a -> b) -> b

instance App (A a) b => App (B a) b where
  app bx@(B x) rightf@(Right _) = appB bx rightf
  app    (B x)        (Left b)  = let y = app (A x) (Left b) in y

instance App (B a) Bool => App (A a) Char where
  app (A x) (Left '1') = let y = appB (B x) (Left True)
                          in if y then '2' else '0'

instance App (B a) Char => App (A a) Bool where
  app (A x) (Left True) = let y = appB (B x) (Left '1')
                           in y=='1'

x = app (B undefined)    -- (App (A a) b) => Either b (B a -> b) -> b -- ok
                         -- Note: context reduction App (B a) b ~> App (A a) b)
y = x (Left '1')         -- Não entendo como constraint App (A a) Char pode ter sido descartado!
z1 = x undefined         -- App (A a) b => b -- ok
z2 = x (Left undefined)  -- App (A a1) a => a -- ok
z3 = x (Right undefined) -- App (A a) b => b -- ok

main = print y
