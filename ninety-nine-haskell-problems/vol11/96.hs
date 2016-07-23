import Text.RegexPR
import Data.Maybe

identifier = isJust . matchRegexPR "^[a-zA-Z](-?[a-zA-Z0-9])*$"

main = do
    print $ identifier "this-is-a-long-identifier"
    print $ identifier "this-ends-in-"
    print $ identifier "two--hyphens"
