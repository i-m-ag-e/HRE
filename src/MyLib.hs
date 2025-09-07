module MyLib(module NFA, module Parse, module AST, matchRE, searchRE) where
import AST
import NFA
import Parse
import qualified Data.Set as S

checkMatch :: RExpr -> String -> Bool
checkMatch re s = any (`S.member` finalStates nfa) finals
    where
        nfa = build re
        finals = trans nfa s

matchRE :: String -> String -> Maybe Bool
matchRE re s = checkMatch <$> parsed <*> Just s
    where
        parsed = runParser parseRE re

searchRE :: String -> String -> Maybe Bool
searchRE re s = (checkMatch . wrapped <$> parsed) <*> Just s
    where
        parsed = runParser parseRE re
        wrapped reg = Concat (Concat (Star Wildcard) reg) (Star Wildcard)
