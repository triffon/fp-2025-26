newtype Predicate a = Predicate { getPredicate :: a -> Bool }

instance Semigroup (Predicate a) where  
  (<>) :: Predicate a -> Predicate a -> Predicate a
  (Predicate p1) <> (Predicate p2) = Predicate $ \a -> p1 a && p2 a

instance Monoid (Predicate a) where
  mempty :: Predicate a
  mempty = Predicate $ const True

filterAll :: [Predicate a] -> [a] -> [a]
filterAll p = filter (getPredicate $ mconcat p)

liftB :: (Bool -> Bool -> Bool) -> Predicate a -> Predicate a -> Predicate a
liftB f (Predicate p1) (Predicate p2) = Predicate $ \a -> f (p1 a) (p2 a)
