{-# language TemplateHaskell #-}
{-# language LambdaCase #-}
{-# language ViewPatterns #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
import Prelude hiding (length,splitAt)
import Rope as R
import Cmd
import Env
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Control.Monad (guard)
import Data.Monoid ((<>))
import Test.QuickCheck
import Test.QuickCheck.Instances () -- instance Arbitrary ByteString
import Test.HUnit
import qualified Data.Foldable as F (toList)

-- Ideally, an instance for FingerTree should be upstreamed into quickcheck-instances or similar
instance Arbitrary Rope where
  arbitrary = fromList <$> arbitrary
  shrink = map fromList . shrink . R.toList

-- | Data representing a Rope with a valid split position in it
data Split = Split Rope Int deriving Show
instance Arbitrary Split where
  arbitrary = do
    r <- arbitrary
    n <- choose (0,length r)
    return $ Split r n
-- | Data representing a Rope with a valid index into it
data Indexed = Indexed Rope Int deriving Show
instance Arbitrary Indexed where
  arbitrary = do
    r <- arbitrary
    n <- choose (0,length r - 1)
    return $ Indexed r n

-- Check that cons/snoc pattern match is inverse to pattern construction
prop_snoc r bs = not (BS.null bs) ==> r == r' && bs == bs' where r' :> bs' = r :> bs
prop_cons bs r = not (BS.null bs) ==> r == r' && bs == bs' where bs' :< r' = bs :< r
-- check that splitAt works on Rope like the related function on ByteStrings
prop_splitAt (Split rope n) = toBS l === l' .&&. toBS r === r'
  where (l,r) = splitAt n rope
        (l',r') = BS.splitAt n $ toBS rope
-- check that charAt indexes into a Rope the same way `index` does in a ByteString
prop_charAt (Indexed r n) = length r > 0 ==> charAt n r === Just (BS.index (toBS r) n)

hunits = runTestTT . test $
  [(parseCmd <$> BS.readFile "test/test.txt") >>= \case
        Left _ -> assertString "example script failed to parse"
        Right (runCmds -> (F.toList -> output,_))
          -> assertEqual "check example script output" output "cya"
  ]

return [] -- Needed to put props in scope for template haskell
main :: IO ()
main = do
  _ <- hunits
  guard =<< $quickCheckAll

