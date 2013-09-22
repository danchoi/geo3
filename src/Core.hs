{-# LANGUAGE OverloadedStrings #-}
module Core where
import Data.Attoparsec.Char8
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T

type LatLng = (Double, Double)

data Event = Rename Text 
           | Center LatLng
           | Chat Text
           deriving (Show, Eq)


eventParser :: Parser Event
eventParser = undefined

