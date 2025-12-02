module Tuples where

type Point = (Double, Double)
type Triangle = (Point, Point, Point)

-- >>> : Triangle
-- type Triangle :: *
-- type Triangle = (Point, Point, Point)
--   	-- Defined at /home/trifon/fmisync/Courses/2025_26/FP_2025_26/fp-2025-26/lectures/haskell/Tuples.hs:4:1
triangle :: Triangle
triangle = ((1, 2), (5, 2), (5, 7))

-- >>> :t ()
-- () :: ()
