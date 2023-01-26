-- After all, this is one of those things that is interesting in theory, 
-- but in practice amounts to an awful hack that no one would ever want 
-- to use in real code.

-- Applicative can only parse context-free languages as long as we restrict 
-- ourselves to finite grammars

-- for not context-free(T0 & T1)

-- https://byorgey.wordpress.com/2012/01/05/parsing-context-sensitive-languages-with-applicative/

-- http://www.cse.chalmers.se/~nad/publications/danielsson-parser-combinators.pdf


import Text.Parsec
import Text.Parsec.String
import Control.Arrow ((&&&))
import Control.Applicative hiding ((<|>))
import Data.List (group)

guard' :: Alternative f => Bool -> f ()
guard' True = pure()
guard' False = empty

parseNew :: (String -> Bool) -> Parser()
parseNew p = (eof <* guard' (p []))
    <|> foldr (<|>) parserZero
            (map (\c -> char c *> parseNew (p . (c:))) ['a'..'z'])

sensitiveGrammar :: String -> Bool
sensitiveGrammar s 
    | [('a', na), ('b', nb), ('c', nc)] <- map (head &&& length). group $ s
        = na == nb && nb == nc
    | otherwise = False                     

sensitiveParser = parseNew sensitiveGrammar

freeGrammar :: String -> Bool
freeGrammar s 
    | [('a', na), ('b', nb)] <- map (head &&& length). group $ s
        = na == nb
    | otherwise = False
freeParser = parseNew freeGrammar    

main = do
    parseTest freeParser "aabb"
    parseTest freeParser "abb"
    parseTest sensitiveParser "aaa"
    parseTest sensitiveParser "abbc"
    parseTest sensitiveParser "ABC"
    parseTest sensitiveParser "aaabbbccc"