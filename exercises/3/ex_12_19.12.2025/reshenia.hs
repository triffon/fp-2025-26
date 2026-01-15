integerToNat' :: Int -> Maybe Nat
integerToNat' 0 = Just Zero
integerToNat' z
    | z < 0 = Nothing
    | otherwise = Just (Succ (integerToNat (z - 1)))

safeDiv :: Int -> Int -> Maybe (Int, Int)
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `quot` y, x `rem` y)
