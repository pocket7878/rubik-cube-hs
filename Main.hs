module Main where
import qualified Perm as P
import qualified Rubik as R
import Data.Monoid

main :: IO ()
main = do {
        print $ R.defaultRubik;
        print $ R.trans R.defaultRubik $ P.inverse R.turnR `mappend` P.inverse R.turnD
}
