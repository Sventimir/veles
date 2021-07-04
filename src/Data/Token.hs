module Data.Token (
  Token(..),
  tokenise
) where

import Data.Text (Text)


data Token = Quote
           | OpenParam
           | CloseParam
           | Symbol Text
           | Char Char
           | String Text
           | Number Text

tokenise :: Text -> [Token]
tokenise = undefined
