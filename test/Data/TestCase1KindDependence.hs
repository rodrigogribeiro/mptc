
module Data.TestCase1KindDependence where


class C a where
    op :: T a d
          
data C a => T a b = MkT (a b)
