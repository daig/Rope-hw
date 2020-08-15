--------------------------------------------------------------------
-- |
-- Module    :  Env
-- Copyright :  (c) <David Girardo> 2017
-- License   :  MIT
-- Maintainer:  <david.girardo@mit.edu>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
{-# language LambdaCase #-}
module Env where

import Cmd (Cmd(..))
import Rope
import Prelude hiding (head)
import Control.Monad.State (State,gets,modify,runState)
import Control.Monad.Writer
import Data.DList (DList)
import Data.Foldable (traverse_)

-- | The Env includes the history of all buffers as a State layer, and the Print outputs as a WriterT layer. Output are built in a DList for O(1) append
type Env = WriterT (DList Char) (State [Rope])
-- | Run an environment with an initial history, and retrieve the output and final history
runEnv :: Env a -> [Rope] -> (DList Char,[Rope])
runEnv e s = runState (execWriterT e) s
-- | Safely extract the head of a list, supplying the monoid unit if empty
head :: Monoid a => [a] -> a
head = \case
  [] -> mempty
  (a:_) -> a
-- | Evaluate a @Cmd@ as an action in the @Env@ monad, acting on internal state/history or printing to stdout
eval :: Cmd -> Env ()
eval = \case
  Print k -> gets (charAt (k-1) . head) >>= \case 
    Nothing -> do
      s <- gets head
      fail $ "invalid index:" ++ show (k-1) ++ " into " ++ show s
    Just c -> tell (pure c)
  Undo -> modify tail
  Append s -> modify (save (:> s))
  Delete n -> modify (save (dropTail n))

-- | Run a sequence of commands to extract the output and final history
runCmds :: [Cmd] -> (DList Char,[Rope])
runCmds cmds = runEnv (traverse_ eval cmds) mempty 
      

-- | Push a modified head onto the top of a list
save :: Monoid a => (a -> a) -> [a] -> [a]
save f xs = f (head xs) : xs
