module Main where
import qualified Symm as S
import qualified Rubik as R
import Data.Monoid

main :: IO ()
main = do {
        print $ R.defaultRubik;
        print $ R.trans R.defaultRubik $ S.inverse R.turnR `mappend` S.inverse R.turnD
}
