{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Foldable
import Control.Applicative
import Data.Text (Text)
import Data.Maybe
import qualified Data.Text.IO as TIO

import Grammar

meaning :: Text -> a -> Element a
meaning w v = word w *> pure v

data Key = A | B | C
  deriving (Show)

key :: Element Key
key = asum
  [ "arch" `meaning` A
  , "brov" `meaning` B
  , "char" `meaning` C
  ]

pressKey :: Element (IO ())
pressKey = print <$> (word "press" *> key)

number :: Element Int
number = asum
  [ "one" `meaning` 1
  , "two" `meaning` 2
  , "three" `meaning` 3
  ]

pressNumbers :: Element (IO ())
pressNumbers = mapM_ print <$> (word "numbers" *> optional (word "hello") *> (some number <* word "bow"))

test :: Element (IO ())
test = fmap print $ some (word "say" *> dictated <* word "stop")
  where dictated = wordContext (\t _ -> t) dictation

myRule :: Element (IO ())
myRule = word "Quinn" *> (sequence_ <$> some (pressKey <|> pressNumbers <|> test))

main :: IO ()
main = fromMaybe (return ()) $ fmap TIO.putStrLn (pretty $ myRule)
