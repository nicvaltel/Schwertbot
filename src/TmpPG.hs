module TmpPG where


-- type XXX = Int -> String -> String

-- func :: Int -> IO Int
-- func n = pure $ n + 1


-- func' :: Int -> IO (Int -> Int)
-- func' n = 


-- incexpApp :: IO (Int -> String -> String)
-- incexpApp = do 
--   f <- pure func
--   pure undefined


-- func :: (a -> IO a) -> IO (a -> a)
-- func f = do
--   x <- f undefined -- get an initial value of type 'a'
--   pure (\y -> do
--     x' <- f x      -- apply f to the previous value
--     pure (y `seq` x') -- evaluate y first to ensure strictness
--     )