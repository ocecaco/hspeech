{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Foldable
import Control.Applicative
import Data.Text (Text)
import Data.Aeson

import Grammar

meaning :: Text -> a -> Element a
meaning w v = word w *> pure v

data Key = A | B | C
  deriving (Show)

ruleKey :: Rule Key
ruleKey = createRule $ asum
  [ "arch" `meaning` A
  , "brov" `meaning` B
  , "char" `meaning` C
  ]

main :: IO ()
main = print (grammarSyntax grammar)
  where grammar =
          withRule ruleKey $ \key ->
            createGrammar [(A, word "hello" *> ruleRef key *> ruleRef key)]
