
module Tests.Data.TestCase2KindDependence where


data GRose f a = Node a (f (GRose f a))

type LNode a = GRose [] a 