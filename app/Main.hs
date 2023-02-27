{-# LANGUAGE OverloadedStrings #-}

module Main where

import Converter
import qualified Data.Text as T
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax (LaTeX, getBody)

outputBody :: LaTeX -> T.Text
outputBody = maybe "" (stripExtraneousSpaces . texToSpeech) . getBody

main :: IO ()
main = interact (either show (T.unpack . outputBody . filterCommands saneCommandSkip . filterEnvironments saneSkip) . parseLaTeX . T.pack)
