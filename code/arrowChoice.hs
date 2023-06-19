import Control.Applicative
import Control.Arrow
import Control.Selective

newtype SelectiveParser a = SelectiveParser { runSelectiveParser :: String -> Maybe (a, String) }

instance Functor SelectiveParser where
  fmap f (SelectiveParser p) = SelectiveParser $ \input -> do
    (x, rest) <- p input
    return (f x, rest)

instance Applicative SelectiveParser where
  pure x = SelectiveParser $ \input -> Just (x, input)
  SelectiveParser pf <*> SelectiveParser px = SelectiveParser $ \input -> do
    (f, rest1) <- pf input
    (x, rest2) <- px rest1
    return (f x, rest2)

instance Alternative SelectiveParser where
  empty = SelectiveParser $ const Nothing
  SelectiveParser p1 <|> SelectiveParser p2 = SelectiveParser $ \input ->
    p1 input <|> p2 input

instance Selective SelectiveParser where
  select (SelectiveParser f) (SelectiveParser g) = SelectiveParser $ \input ->
    case f input of
      Just (Left x, rest) -> g x rest
      Just (Right y, rest) -> Just (y, rest)
      Nothing -> Nothing

-- Example selective parser
intParser :: SelectiveParser Int
intParser = SelectiveParser $ \input -> case reads input of
  [(x, rest)] -> Just (x, rest)
  _ -> Nothing

stringParser :: String -> SelectiveParser String
stringParser expected = SelectiveParser $ \input -> if expected == input
  then Just (expected, "")
  else Nothing

ifParser :: SelectiveParser Bool -> SelectiveParser a -> SelectiveParser a -> SelectiveParser a
ifParser condParser thenParser elseParser = select (const <$> thenParser) elseParser <*> condParser

exampleParser :: SelectiveParser String
exampleParser = ifParser (stringParser "abc") (stringParser "then") (stringParser "else")

main :: IO ()
main = do
  let result = runSelectiveParser exampleParser "abc"
  putStrLn $ show result
