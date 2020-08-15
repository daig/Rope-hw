{-# language LambdaCase #-}
{-# language ViewPatterns #-}
module Main where
import Prelude hiding (head)
import Env
import Cmd
import Rope
import Options.Applicative
import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as BS

data InputOpts = StdIn | FromFile String
optParseFile :: Parser (Maybe String)
optParseFile =
  Just <$>
    strOption ( long "file"
             <> short 'f'
             <> metavar "FILE"
             <> help "read script from a filepath rather than stdin"
             <> action "file")
  <|> pure Nothing
main :: IO ()
main = do
  script <- execParser (info optParseFile fullDesc) >>= \case
    Nothing -> BS.getContents
    Just file ->  BS.readFile file
  case parseCmd script of
    Left _ -> print "invalid script format"
    Right (runCmds -> (output, head -> finalT)) -> do
      mapM_ (\c -> putStrLn [c]) output
      putStr "Final text: "; BS.putStrLn (toBS finalT)
