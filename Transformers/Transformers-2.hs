import Data.Char (isUpper, isLower)
import Control.Monad.Trans.Maybe
import Control.Monad.Reader

-- "Monad transformers are like onions. At first, they make you cry but then you learn to appreciate
--   them. Like onions, they're also made of layers. Each layer is the functionality of a new monad,
--   you lift monadic functions to get into the inner monads and you have transformerised functions
--   to unwrap each layer."

-- *** Transformers in Practice: User Login ***
-- Reference: https://mmhaskell.com/monads/transformers
-- Let's say we're creating some login functionality, which needs to read in user input from the
--   terminal and validate the input before logging in.
-- We want to use the functionality of the Maybe monad, but we're already inside another monad (IO).

-- Ugly version without transformers:
main1 :: IO ()
main1 = do
  maybeUserName <- readUserName
  -- This "stairway to hell" results from the nested monadic contexts...
  case maybeUserName of
    Nothing -> print "Invalid user name!"
    Just uName -> do
      maybeEmail <- readEmail
      case maybeEmail of
        Nothing -> print "Invalid email!"
        Just email -> do
          maybePassword <- readPassword
          case maybePassword of
            Nothing -> print "Invalid Password"
            Just password -> login uName email password
-- Does this look familiar? It's similar to our naive approach to sequencing computations that
--   return `Maybe` values before we learned how to leverage the "bind" operator.
-- Once we discovered (>>=) (and the even cleaner do-notation that sugars over it), we replaced the
--   stairway with a neat sequence of computations resembling imperative code:
sequenceMaybeComps :: String -> String -> String -> Maybe (String, String, String)
sequenceMaybeComps u e p = do
  uName <- validateUserName u
  email <- validateEmail e
  pw <- validateEmail p
  Just (uName, email, pw)
  -- equivalent to:
  -- validateUserName u >>=
  --   (\u' -> validateEmail e >>=
  --     (\e' -> validatePassword p >>=
  --        (\p' -> Just (u', e', p'))))

-- When the complexity of our application grows to require computations in multiple composed
--   contexts, we lose the elegant syntax above and are back to manually pattern matching `Maybe`
--   computations within the context of the "base" monad (IO). This is because monads aren't closed
--   under composition, so we don't automatically get a new monad when we compose two together. That
--   means we can't use (>>=)/do-notation to chain computations within the "double-context".

--- *** Read functions ***
readUserName :: IO (Maybe String)
readUserName = do
  putStrLn "Please enter your username:"
  validateUserName <$> getLine

readEmail :: IO (Maybe String)
readEmail = do
  putStrLn "Please enter your email:"
  validateEmail <$> getLine

readPassword :: IO (Maybe String)
readPassword = do
  putStrLn "Please enter your Password:"
  validatePassword <$> getLine

-- *** Validators ***
validateUserName :: String -> Maybe String
validateUserName = validateStr (\s -> length s > 5)
validateEmail :: String -> Maybe String
validateEmail = validateStr (\s -> '@' `elem` s && '.' `elem` s)
validatePassword :: String -> Maybe String
validatePassword = validateStr (\s -> all ($ s) [(> 8) . length, any isUpper, any isLower])

validateStr :: (String -> Bool) -> String -> Maybe String
validateStr p s = if p s then Just s else Nothing


-- *** Refactor functions from IO (Maybe String) to MaybeT IO String ***

-- newtype MaybeT n a = MaybeT { runMaybeT :: n (Maybe a) }
readUserNameT :: MaybeT IO String
--               ^ type constructor (named after the inner context)
--                      ^ "base" (outer) context
--                         ^ contextualized data
readUserNameT = MaybeT $ do
  putStrLn "Please enter your username:"
  validateUserName <$> getLine

readEmailT :: MaybeT IO String
readEmailT = MaybeT $ do
  putStrLn "Please enter your email:"
  validateEmail <$> getLine

readPasswordT :: MaybeT IO String
readPasswordT = MaybeT $ do
  putStrLn "Please enter your password:"
  validatePassword <$> getLine

-- *** Refactor main action ***
main2 :: IO ()
main2 = do -- 1. Enter the IO context
  maybeCreds <- runMaybeT $ -- 3. Apply the MaybeT "getter" to get an `IO Maybe` value, which we
                            --    can then bind into a Maybe value (`maybeCreds`)
    do -- 2. Enter our new MaybeT context, where we can now bind the raw strings (if valid) via `<-`
       --    instead of `Maybe String` values:
      usr <- readUserNameT
      email <- readEmailT
      pass <- readPasswordT
      -- If any of these functions fail, the code will short-circuit and immediately return Nothing
      pure (usr, email, pass) -- Promote the credentials back into MaybeT context
  case maybeCreds of -- 4. We now do a single case-match on the Maybe credential value...
  -- ...and say goodbye stairway to hell!
    Nothing -> print "Login failed!" >> main2
    Just (u, e, p) -> login u e p -- If valid, destructure the Maybe credentials and login


-- *** Add a third context with ReaderT ***

-- Create a type synonym for some cached user data:
type Cookie = (Maybe String, Maybe String, Maybe String)

-- The Reader transformer adds a read-only environment (`r`) to the base monad (`m`):

-- newtype Reader  r a   = Reader  { runReader  :: r -> a }
-- newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

readUserNameR :: MaybeT (ReaderT Cookie IO) String
--                       ^ the base monad for MaybeT is now itself a transformer, ReaderT
--                               ^ the type of the environment data
--                                      ^ the base monad for ReaderT
readUserNameR = MaybeT $ do
  -- ask :: ReaderT Cookie IO a -> ReaderT Cookie IO Cookie
  (mCachedUser, _, _) <- ask -- `ask` is a Reader utility function that retrieves the
                             -- environment data.
  case mCachedUser of
    Nothing -> do
      -- lift takes a value in a base context and lifts it to a transformer context
      -- lift :: (Monad m) => m a -> t m a
      lift $ putStrLn "Please enter your username:" -- IO () -> ReaderT Cookie IO ()
      validateUserName <$> lift getLine
    ju -> pure ju -- if username is cached, promote it to `ReaderT IO` context

readEmailR :: MaybeT (ReaderT Cookie IO) String
readEmailR = MaybeT $ do
  (_, mCachedEmail, _) <- ask
  case mCachedEmail of
    Nothing -> do
      lift $ putStrLn "Please enter your email:"
      validateEmail <$> lift getLine
    je -> pure je

readPasswordR :: MaybeT (ReaderT Cookie IO) String
readPasswordR = MaybeT $ do
  (_, _, mCachedPassword) <- ask
  case mCachedPassword of
    Nothing -> do
      lift $ putStrLn "Please enter your password:"
      validatePassword <$> lift getLine
    jp -> pure jp

emptyCookie = (Nothing, Nothing, Nothing)
fullCookie = (Just "curriedFunctions", Just "ian.burzynski@emurgo.io", Just "LoneBurrito")

main3 :: IO ()
main3 = do
  maybeCreds <- (runReaderT . runMaybeT $ do
    user <- readUserNameR
    email <- readEmailR
    pass <- readPasswordR
    pure (user, email, pass)) fullCookie
  case maybeCreds of
    Nothing -> print "Login failed!" >> main3
    Just (u, e, p) -> login u e p

main3' :: IO ()
main3' = do
  maybeCreds <- (runReaderT . runMaybeT $ do
    user <- readUserNameR
    email <- readEmailR
    pass <- readPasswordR
    pure (user, email, pass)) emptyCookie
  case maybeCreds of
    Nothing -> print "Login failed!" >> main3'
    Just (u, e, p) -> login u e p

-- Placeholder function for some login operation:
login :: String -> String -> String -> IO ()
login u e p = putStrLn . concat $ ["Logged in as "
                                  , u
                                  , " "
                                  , "("
                                  , e
                                  , ")!"]