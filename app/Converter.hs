{-# LANGUAGE OverloadedStrings #-}

module Converter where

import Control.Monad
import Control.Monad.State.Lazy
import qualified Data.Set as S
import qualified Data.Text as T
import Text.LaTeX (tableofcontents)
import Text.LaTeX.Base.Syntax

verbal :: S.Set String
verbal = S.fromList ["LaTeX", "alpha", "beta", "gamma", "delta", "epsilon", "eta", "theta", "iota", "kappa", "lambda", "mu", "nu", "xi", "omicron", "pi", "rho", "sigma", "tau", "upsilon", "phi", "chi", "xi", "omega"]

saneSkip :: S.Set String
saneSkip = S.fromList ["figure", "tabular"]

saneCommandSkip = S.fromList ["label", "tableofcontents"]

mathsEnvironment :: S.Set String
mathsEnvironment = S.fromList ["equation", "equation*", "align", "align*", "aligned", "aligned*"]

mathsToSpeech :: LaTeX -> T.Text
mathsToSpeech = const "blah"

argToSpeech :: TeXArg -> T.Text
argToSpeech (FixArg contents) = texToSpeech contents
argToSpeech _ = ""

argsToSpeech :: [TeXArg] -> T.Text
argsToSpeech = T.unwords . filter (/= "") . map argToSpeech

texToSpeech :: LaTeX -> T.Text
texToSpeech (TeXRaw text) = text
texToSpeech (TeXCommS comm) = if comm `S.member` verbal then T.pack comm else ""
texToSpeech (TeXComm "chapter" title) = "Chapter " <> argsToSpeech title <> "."
texToSpeech (TeXComm "section" title) = "Section " <> argsToSpeech title <> "."
texToSpeech (TeXComm "subsection" title) = "Subsection " <> argsToSpeech title <> "."
texToSpeech (TeXComm "subsubsection" title) = "Subsection " <> argsToSpeech title <> "."
texToSpeech (TeXComm "cite" key) = argsToSpeech key -- Will be handled specially later along with other cite commands
texToSpeech (TeXComm comm args)
  | comm `S.member` verbal = T.pack comm <> " " <> argsToSpeech args
  | otherwise = T.unwords . map argToSpeech $ args
texToSpeech (TeXMath _ maths) = mathsToSpeech maths
texToSpeech (TeXEnv "figure" _ _) = ""
texToSpeech (TeXEnv "enumerate" _ contents) = texToSpeech . countUp $ contents
texToSpeech (TeXEnv env _ contents) = if env `S.member` mathsEnvironment then mathsToSpeech contents else texToSpeech contents
texToSpeech (TeXBraces contents) = texToSpeech contents
texToSpeech (TeXSeq (TeXCommS "chapter") (TeXSeq (TeXRaw "*") contents)) = "Chapter " <> texToSpeech contents
texToSpeech (TeXSeq (TeXCommS "section") (TeXSeq (TeXRaw "*") contents)) = "Section " <> texToSpeech contents
texToSpeech (TeXSeq (TeXCommS "subsection") (TeXSeq (TeXRaw "*") contents)) = "Subsection " <> texToSpeech contents
texToSpeech (TeXSeq (TeXCommS "subsubsection") (TeXSeq (TeXRaw "*") contents)) = "Subsection " <> texToSpeech contents
texToSpeech (TeXSeq left right) = texToSpeech left <> texToSpeech right
texToSpeech _ = ""

tshow :: Show a => a -> T.Text
tshow = T.pack . show

countUp :: LaTeX -> LaTeX
countUp latex = evalState (go latex) 1
  where
    go (TeXSeq left right) = TeXSeq <$> go left <*> go right
    go (TeXCommS "item") = do
      i <- get
      modify (+ 1)
      return (TeXRaw $ tshow i <> ".")
    go other = pure other

filterEnvironments :: S.Set String -> LaTeX -> LaTeX
filterEnvironments skipped (TeXEnv name args contents)
  | name `S.member` skipped = TeXEmpty
  | otherwise = TeXEnv name args (filterEnvironments skipped contents)
filterEnvironments skipped (TeXSeq left right) = filterEnvironments skipped left <> filterEnvironments skipped right
filterEnvironments skipped other = other

filterArgs :: S.Set String -> [TeXArg] -> [TeXArg]
filterArgs skipped = map (go skipped)
  where
    go skipped (FixArg contents) = FixArg (filterCommands skipped contents)
    go skipped (OptArg contents) = OptArg (filterCommands skipped contents)
    go skipped (MOptArg contents) = MOptArg (map (filterCommands skipped) contents)
    go skipped (SymArg contents) = SymArg (filterCommands skipped contents)
    go skipped (MSymArg contents) = MSymArg (map (filterCommands skipped) contents)
    go skipped (ParArg contents) = ParArg (filterCommands skipped contents)
    go skipped (MParArg contents) = MParArg (map (filterCommands skipped) contents)

filterCommands :: S.Set String -> LaTeX -> LaTeX
filterCommands skipped comm@(TeXCommS name)
  | name `S.member` skipped = TeXEmpty
  | otherwise = comm
filterCommands skipped (TeXComm name args)
  | name `S.member` skipped = TeXEmpty
  | otherwise = TeXComm name (filterArgs skipped args)
filterCommands skipped (TeXSeq left right) = filterCommands skipped left <> filterCommands skipped right
filterCommands skipped (TeXEnv env args contents) = TeXEnv env args (filterCommands skipped contents)
filterCommands skipped other = other
