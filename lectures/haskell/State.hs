module State where

newtype State state result = State { runState :: state -> (result, state) } 

instance Functor (State state) where
    fmap :: (a -> b) -> State state a -> State state b
    fmap f (State p) = State (\state -> let (result, newState) = p state in (f result, newState))

instance Applicative (State state) where
    pure :: a -> State state a
--    pure r = State (\state -> (r, state))
--   pure r = State ((,) r)
    pure = State . (,)

    (<*>) :: State state (a -> b) -> State state a -> State state b
    (State p) <*> (State q) = State (\state -> let (f, newState)  = p state; 
                                                   (x, newState2) = q newState
                                                in  (f x, newState2))

instance Monad (State state) where
    (>>=) :: State state a -> (a -> State state b) -> State state b
    (State p) >>= f = State (\state -> let (resultP, newState) = p state
                                       in runState (f resultP) newState)


type S = State [Int] Int

getState :: State state state
getState = State (\state -> (state, state))

setState :: state -> State state ()
setState newState = State (\_ -> ((), newState))  

p :: State Int Int 
p = do x <- getState
       setState (2*x)
       y <- getState
       return $ y + 2

-- >>> runState p 5
-- (12,10)

q :: State [Int] Int
q = do x <- getState
       setState $ doubleHead x
       y <- getState
       return $ sum $ take 3 y
   where doubleHead [] = []
         doubleHead (x:xs) = 2*x : xs

-- >> runState q [1..5]
-- (7,[2,2,3,4,5])
