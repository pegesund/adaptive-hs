module Simple where

import ACE.Html

-- import Control.Monad
-- import Control.Monad.Trans
--import Control.Monad.Trans.Maybe

--mgreet :: MaybeT IO ()                             -- types:
---mgreet = do liftIO $ putStr "What is your name? "  -- MaybeT IO ()
--            n <- liftIO getLine                    -- MaybeT IO String
--            liftIO $ putStrLn $ "Hello, " ++ n     -- MaybeT IO ()
--
--test = runMaybeT mgreet