{-# LANGUAGE OverloadedStrings #-}
module Grammar.Dragon
  ( DragonSyntax(..)
  )
where

import Data.Aeson
import Data.Text (Text)
import Grammar.Parse

data DragonSyntax = Sequence DragonSyntax DragonSyntax
                  | Alternative DragonSyntax DragonSyntax
                  | Repetition DragonSyntax
                  | Optional DragonSyntax
                  | Word Text
                  | List Text
                  | RuleRef Text
                  | Dictation
                  | DictationWord
                  | SpellingLetter
                  deriving (Show)

typedObject :: Text -> [(Text, Value)] -> Value
typedObject tag fields = object (typeTag tag : fields)
  where typeTag t = ("type" :: Text, toJSON t)

capture :: ToJSON v => CaptureTag -> v -> Value
capture tag child = typedObject "capture" ["name" .= renderTag tag, "child" .= child]

instance ToJSON DragonSyntax where
  toJSON (Sequence left right) = capture TSequence $ typedObject "sequence" ["children" .= [left, right]]
  toJSON (Alternative left right) = typedObject "alternative" ["children" .= [capture TAlternativeLeft left, capture TAlternativeRight right]]
  toJSON (Repetition x) = capture TRepetition $ typedObject "repetition" ["child" .= x]
  toJSON (Optional x) = capture TOptional $ typedObject "optional" ["child" .= x]

  toJSON (Word w) = capture TLeaf $ typedObject "word" ["text" .= w]
  toJSON (List n) = capture TLeaf $ typedObject "list" ["name" .= n]
  toJSON (RuleRef n) = capture TLeaf $ typedObject "rule_ref" ["name" .= n]

  toJSON Dictation = capture TLeaf $ typedObject "dictation" []
  toJSON DictationWord = capture TLeaf $ typedObject "dictation_word" []
  toJSON SpellingLetter = capture TLeaf $ typedObject "spelling_letter" []

