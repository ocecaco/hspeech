{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Grammar
  ( Element
  , Rule
  , RuleToken
  , Grammar
  , DragonSyntax

  , word
  , phrase
  , list
  , dictation
  , dictationWord
  , spellingLetter
  , wordContext

  , let_
  , createRule
  , withRule
  , ruleRef

  , createGrammar
  , grammarSyntax
  )
where

import Control.Applicative
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import Grammar.Parse
import Grammar.Dragon

data Rules = Rules
  { _ruleDefinitions :: [(Text, Bool, DragonSyntax)]
  , _ruleCounter :: Int
  }

newtype Builder a = Builder { runBuilder :: State Rules a }
                  deriving (Functor, Applicative, Monad)

newRule :: Bool -> DragonSyntax -> Builder Text
newRule exported e = Builder $ do
  Rules definitions counter <- get
  let name = T.pack ("rule" ++ show counter)
  put (Rules ((name, exported, e) : definitions) (counter + 1))
  return name

type Parser a = ParseTree -> Either Text a

newtype Grammar t a = Grammar { buildGrammar :: Builder [(t, Maybe (Text, Parser a))] }

newtype Element a = Element { buildElement :: Builder (GrammarElement a) }

newtype Rule a = Rule (Element a)

createRule :: Element a -> Rule a
createRule = Rule

newtype RuleToken a = RuleToken { unwrapRuleToken :: Element a }

data GrammarElement a = Syntax DragonSyntax (Parser a)
                      | Pure a
                      | Fail

grammarSyntax :: Grammar t a -> ([(Text, Bool, DragonSyntax)], [(t, Maybe Text)])
grammarSyntax g = (reverse definitions, fmap (\(t, name) -> (t, fmap fst name)) mapping)
  where (mapping, Rules definitions _) = runState (runBuilder (buildGrammar g)) (Rules [] 1)

simple :: GrammarElement a -> Element a
simple = Element . return

chain :: Element (a -> b) -> Element a -> Element b
chain first second = Element (helper <$> buildElement first <*> buildElement second)
  where helper (Syntax firstSyntax firstParser) (Syntax secondSyntax secondParser) = Syntax (Sequence firstSyntax secondSyntax) newParser
          where newParser (ParseTree _ (PSequence leftParse rightParse)) = firstParser leftParse <*> secondParser rightParse
                newParser _ = Left "expected sequence"

        helper (Syntax syntax parser) (Pure v) = Syntax syntax (\tree -> fmap ($ v) (parser tree))
        helper (Pure f) (Syntax syntax parser) = Syntax syntax (\tree -> fmap f (parser tree))
        helper (Pure f) (Pure v) = Pure (f v)

        helper Fail _ = Fail
        helper _ Fail = Fail

alt :: Element a -> Element a -> Element a
alt left right = Element (helper <$> buildElement left <*> buildElement right)
  where helper (Syntax firstSyntax firstParser) (Syntax secondSyntax secondParser) = Syntax (Alternative firstSyntax secondSyntax) newParser
          where newParser (ParseTree _ (PAlternative (Left p))) = firstParser p
                newParser (ParseTree _ (PAlternative (Right p))) = secondParser p
                newParser _ = Left "expected alternative"

        helper (Syntax syntax parser) (Pure v) = Syntax (Optional syntax) newParser
          where newParser (ParseTree _ (POptional Nothing)) = Right v
                newParser (ParseTree _ (POptional (Just p))) = parser p
                newParser _ = Left "expected optional"

        helper (Pure v) (Syntax syntax parser) = Syntax (Optional syntax) newParser
          where newParser (ParseTree _ (POptional Nothing)) = Right v
                newParser (ParseTree _ (POptional (Just p))) = parser p
                newParser _ = Left "expected optional"

        helper (Pure a) (Pure _) = Pure a
        helper Fail x = x
        helper x Fail = x

repetition :: Element a -> Element [a]
repetition e = Element (helper <$> buildElement e)
  where helper (Syntax syntax parser) = Syntax (Repetition syntax) newParser
          where newParser (ParseTree _ (PRepetition p)) = sequence (fmap parser p)
                newParser _ = Left "expected repetition"

        helper (Pure _) = Pure []

        helper Fail = Fail

getWords :: ParseTree -> [Text]
getWords (ParseTree ws _) = ws

phrase :: Text -> Element [Text]
phrase sentence = sequenceA (fmap word (T.words sentence))

firstWord :: [Text] -> Either Text Text
firstWord (t:_) = Right t
firstWord [] = Left "failed to get first word"

word :: Text -> Element Text
word text = simple (Syntax (Word text) (firstWord . getWords))

list :: Text -> Element Text
list name = simple (Syntax (List name) (firstWord . getWords))

ruleRef :: RuleToken a -> Element a
ruleRef = unwrapRuleToken

dictation :: Element [Text]
dictation = simple (Syntax Dictation (Right . getWords))

dictationWord :: Element Text
dictationWord = simple (Syntax DictationWord (firstWord . getWords))

spellingLetter :: Element Text
spellingLetter = simple (Syntax SpellingLetter (firstWord . getWords))

wordContext :: ([Text] -> a -> b) -> Element a -> Element b
wordContext f e = Element (helper <$> buildElement e)
  where helper (Syntax syntax parser) = Syntax syntax newParser
          where newParser tree = f (getWords tree) <$> parser tree

        helper (Pure v) = Pure (f [] v)

        helper Fail = Fail

intern :: Bool -> Element a -> Builder (GrammarElement a)
intern exported shared = do
  child <- buildElement shared
  newChild <- case child of
    Syntax childSyntax childParser -> do
      name <- newRule exported childSyntax
      return (Syntax (RuleRef name) childParser)
    p@(Pure _) -> return p
    f@Fail -> return f
  return newChild


createGrammar :: [(t, Element a)] -> Grammar t a
createGrammar exportedRules = Grammar $ do
  forM exportedRules $ \(tag, definition) -> do
    rule <- intern True definition
    return (tag, extract rule)
  where extract (Syntax (RuleRef name) parser) = Just (name, parser)
        extract _ = Nothing


withRule :: Rule a -> (RuleToken a -> Grammar t b) -> Grammar t b
withRule (Rule shared) create = Grammar $ do
  i <- intern False shared
  buildGrammar (create (RuleToken (simple i)))

let_ :: Element a -> (Element a -> Element b) -> Element b
let_ shared create = Element $ do
  i <- intern False shared
  buildElement (create (simple i))


instance Functor Element where
  fmap f e = Element (helper <$> buildElement e)
    where helper (Syntax syntax parser) = Syntax syntax $ \tree -> fmap f (parser tree)
          helper (Pure v) = Pure (f v)
          helper Fail = Fail

instance Applicative Element where
  pure x = simple (Pure x)
  (<*>) = chain

instance Alternative Element where
  empty = simple Fail
  (<|>) = alt
  some = repetition
  many x = fmap unwrap (optional (repetition x))
    where unwrap Nothing = []
          unwrap (Just rs) = rs
