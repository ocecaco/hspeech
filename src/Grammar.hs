{-# LANGUAGE OverloadedStrings #-}
module Grammar
  ( Element(..)
  , DragonElement

  , word
  , list
  , ruleRef
  , dictation
  , dictationWord
  , spellingLetter
  , wordContext

  , elementSyntax
  )
where

import Control.Applicative
import Data.Text (Text)
import Grammar.Parse
import Grammar.Dragon

data RuleToken a = RuleToken Text (ParseTree -> Either Text a)

data Element a = Element DragonElement (ParseTree -> Either Text a)
               | Pure a
               | Fail

elementSyntax :: Element a -> Maybe DragonElement
elementSyntax (Element s _) = Just s
elementSyntax (Pure _) = Nothing
elementSyntax Fail = Nothing

chain :: Element (a -> b) -> Element a -> Element b
chain (Element firstSyntax firstParser) (Element secondSyntax secondParser) = Element (Sequence firstSyntax secondSyntax) newParser
  where newParser (ParseTree _ (PSequence leftParse rightParse)) = firstParser leftParse <*> secondParser rightParse
        newParser _ = Left "expected sequence"

chain (Element syntax parser) (Pure v) = Element syntax (\tree -> fmap ($ v) (parser tree))
chain (Pure f) (Element syntax parser) = Element syntax (\tree -> fmap f (parser tree))
chain (Pure f) (Pure v) = Pure (f v)

chain Fail _ = Fail
chain _ Fail = Fail

alt :: Element a -> Element a -> Element a
alt (Element firstSyntax firstParser) (Element secondSyntax secondParser) = Element (Alternative firstSyntax secondSyntax) newParser
  where newParser (ParseTree _ (PAlternative (Left p))) = firstParser p
        newParser (ParseTree _ (PAlternative (Right p))) = secondParser p
        newParser _ = Left "expected alternative"

alt (Element syntax parser) (Pure v) = Element (Optional syntax) newParser
  where newParser (ParseTree _ (POptional Nothing)) = Right v
        newParser (ParseTree _ (POptional (Just p))) = parser p
        newParser _ = Left "expected optional"

alt (Pure v) (Element syntax parser) = Element (Optional syntax) newParser
  where newParser (ParseTree _ (POptional Nothing)) = Right v
        newParser (ParseTree _ (POptional (Just p))) = parser p
        newParser _ = Left "expected optional"

alt (Pure a) (Pure _) = Pure a
alt Fail x = x
alt x Fail = x

repetition :: Element a -> Element [a]
repetition (Element syntax parser) = Element (Repetition syntax) newParser
  where newParser (ParseTree _ (PRepetition p)) = sequence (fmap parser p)
        newParser _ = Left "expected repetition"

repetition (Pure x) = (Pure (repeat x))

repetition Fail = Fail

word :: Text -> Element ()
word text = Element (Word text) (const $ Right ())

list :: Text -> Element ()
list text = Element (List text) (const $ Right ())

ruleRef :: RuleToken a -> Element a
ruleRef (RuleToken ruleName parser)  = Element (RuleRef ruleName) parser

dictation :: Element ()
dictation = Element Dictation (const $ Right ())

dictationWord :: Element ()
dictationWord = Element DictationWord (const $ Right ())

spellingLetter :: Element ()
spellingLetter = Element SpellingLetter (const $ Right ())

wordContext :: ([Text] -> a -> b) -> Element a -> Element b
wordContext f (Element syntax parser) = Element syntax newParser
  where getWords (ParseTree ws _) = ws

        newParser tree = f (getWords tree) <$> parser tree

wordContext f (Pure v) = Pure (f [] v)

wordContext _ Fail = Fail

instance Functor Element where
  fmap f (Element syntax parser) = Element syntax $ \tree -> fmap f (parser tree)
  fmap f (Pure v) = Pure (f v)
  fmap _ Fail = Fail

instance Applicative Element where
  pure = Pure
  (<*>) = chain

instance Alternative Element where
  empty = Fail
  (<|>) = alt
  some = repetition
  many x = fmap unwrap (optional (repetition x))
    where unwrap Nothing = []
          unwrap (Just rs) = rs
