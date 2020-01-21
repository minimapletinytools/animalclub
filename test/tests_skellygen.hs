import           AnimalClub.Skellygen
import           Test.Hspec
import           Test.QuickCheck

--prop_TRS

prop_justfortesting :: Int -> Bool
prop_justfortesting x = (read . show) x == x


main :: IO ()
main = hspec $ do
  describe "quickcheck" $ do
    it "does in fact work with spec" $ property $
      prop_justfortesting
