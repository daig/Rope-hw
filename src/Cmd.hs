--------------------------------------------------------------------
-- |
-- Module    :  Cmd
-- Copyright :  (c) <David Girardo> 2017
-- License   :  MIT
-- Maintainer:  <david.girardo@mit.edu>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
{-# language LambdaCase #-}
module Cmd (Cmd(..),parseCmd) where
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8 as P
import Data.Char (isLower)
import Control.Monad (guard)

-- | A Parsed command
data Cmd = Append ByteString | Delete Int | Print Int | Undo deriving Show

-- | Parser for an isolated @Cmd@
pCmd :: Parser Cmd
pCmd = choice [pAppend,pDelete,pPrint,pUndo]
  where
    pAppend = Append <$> (char '1' *> skipSpace *> takeWhile1 isLower)
    pDelete = Delete <$> (char '2' *> skipSpace *> decimal)
    pPrint  = Print  <$> (char '3' *> skipSpace *> decimal)
    pUndo   = Undo   <$   char '4'

-- | Parser for a script of commands with header
pCmds :: Parser [Cmd]
pCmds = do
  q <- decimal <* skipSpace
  guard $ q <= 1000000
  count q (pCmd <* skipSpace)

-- | Parse a list of commands
parseCmd :: ByteString -> Either String [Cmd]
parseCmd = parseOnly pCmds
