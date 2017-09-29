{-# LANGUAGE OverloadedStrings #-}
module Grammar.Parse
  ( ParseTree(..)
  , ParseType(..)
  , CaptureTree
  , CaptureTag(..)
  , parseFromCaptures
  , renderTag
  )
where

import Data.Text (Text)
import Data.Aeson

data ParseTree = ParseTree [Text] ParseType

data ParseType = PSequence ParseTree ParseTree
               | PAlternative (Either ParseTree ParseTree)
               | PRepetition [ParseTree]
               | POptional (Maybe ParseTree)
               | PLeaf

data CaptureTag = TSequence
                | TAlternativeLeft
                | TAlternativeRight
                | TRepetition
                | TOptional
                | TLeaf

readTag :: Text -> Maybe CaptureTag
readTag "sequence" = Just TSequence
readTag "alternative_left" = Just TAlternativeLeft
readTag "alternative_right" = Just TAlternativeRight
readTag "repetition" = Just TRepetition
readTag "optional" = Just TOptional
readTag "leaf" = Just TLeaf
readTag _ = Nothing

renderTag :: CaptureTag -> Text
renderTag TSequence = "sequence"
renderTag TAlternativeLeft = "alternative_left"
renderTag TAlternativeRight = "alternative_right"
renderTag TRepetition = "repetition"
renderTag TOptional = "optional"
renderTag TLeaf = "leaf"

data CaptureTree = CaptureTree
  { _captureName :: Text
  , _captureRule :: Text
  , _captureSlice :: (Int, Int)
  , _captureChildren :: [CaptureTree]
  }

instance FromJSON CaptureTree where
  parseJSON = withObject "CaptureTree" $ \v ->
    CaptureTree <$> v .: "name" <*> v .: "rule" <*> v .: "slice" <*> v .: "children"

parseFromCaptures :: [Text] -> CaptureTree -> Either Text ParseTree
parseFromCaptures ws capture = helper capture
  where slice :: Int -> Int -> [Text]
        slice a b = take (b - a) (drop a ws)

        helper :: CaptureTree -> Either Text ParseTree
        helper (CaptureTree name _ (a, b) children) = case readTag name of
          Nothing -> Left "unknown capture tag"
          Just t -> ParseTree (slice a b) <$> go t children

        go :: CaptureTag -> [CaptureTree] -> Either Text ParseType
        go TSequence [x, y] = PSequence <$> helper x <*> helper y

        go TAlternativeLeft [x] = PAlternative . Left <$> helper x
        go TAlternativeRight [x] = PAlternative . Right <$> helper x

        go TOptional [x] = POptional . Just <$> helper x
        go TOptional [] = Right $ POptional Nothing

        go TRepetition xs = PRepetition <$> sequence (fmap helper xs)

        go TLeaf [] = Right PLeaf

        go _ _ = Left "invalid number of children in capture tree"
