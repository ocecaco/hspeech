{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Grammar
  ( Element(..)
  , DragonSyntax

  , word
  , list
  , dictation
  , dictationWord
  , spellingLetter
  , wordContext
  , let_
  )
where

import Control.Applicative
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Grammar.Parse
import Grammar.Dragon

data Rules = Rules
  { _ruleDefinitions :: Map Int DragonSyntax
  , _ruleCounter :: Int
  }

newtype Builder a = Builder { runBuilder :: State Rules a }
                  deriving (Functor, Applicative, Monad)

newRule :: GrammarElement a -> Builder (GrammarElement a)
newRule e = Builder $ do
  let Syntax newDefinition parser = e -- TODO: handle other cases
  Rules definitions counter <- get
  put (Rules (M.insert counter newDefinition definitions) (counter + 1))
  return (Syntax (RuleRef counter) parser)

newtype Element a = Element { buildElement :: Builder (GrammarElement a) }

data GrammarElement a = Syntax DragonSyntax (ParseTree -> Either Text a)
                      | Pure a
                      | Fail

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

        helper (Pure x) = (Pure (repeat x))

        helper Fail = Fail

word :: Text -> Element ()
word text = Element (return (Syntax (Word text) (const $ Right ())))

list :: Text -> Element ()
list name = Element (return (Syntax (List name) (const $ Right ())))

dictation :: Element ()
dictation = Element (return (Syntax Dictation (const $ Right ())))

dictationWord :: Element ()
dictationWord = Element (return (Syntax DictationWord (const $ Right ())))

spellingLetter :: Element ()
spellingLetter = Element (return (Syntax SpellingLetter (const $ Right ())))

wordContext :: ([Text] -> a -> b) -> Element a -> Element b
wordContext f e = Element (helper <$> buildElement e)
  where getWords (ParseTree ws _) = ws

        helper (Syntax syntax parser) = Syntax syntax newParser
          where newParser tree = f (getWords tree) <$> parser tree

        helper (Pure v) = Pure (f [] v)

        helper Fail = Fail

let_ :: Element a -> (Element a -> Element b) -> Element b
let_ shared create = Element $ do
  child <- buildElement shared
  reference <- newRule child
  buildElement $ create (Element (return reference))

instance Functor Element where
  fmap f e = Element (helper <$> buildElement e)
    where helper (Syntax syntax parser) = Syntax syntax $ \tree -> fmap f (parser tree)
          helper (Pure v) = Pure (f v)
          helper Fail = Fail

instance Applicative Element where
  pure x = Element (return (Pure x))
  (<*>) = chain

instance Alternative Element where
  empty = Element (return Fail)
  (<|>) = alt
  some = repetition
  many x = fmap unwrap (optional (repetition x))
    where unwrap Nothing = []
          unwrap (Just rs) = rs
