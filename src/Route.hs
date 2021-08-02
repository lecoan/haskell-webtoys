{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Route where

import BasicPrelude
import Data.Attoparsec.ByteString.Char8

data Route = Home | Message Int

parserRoute :: ByteString -> Either String Route
parserRoute = parseOnly parser

parser :: Parser Route
parser =
  choice
    [ string "/" <* endOfInput >> return Home,
      string "/messages/" >> fmap Message (decimal <* endOfInput)
    ]