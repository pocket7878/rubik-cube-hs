module Main where
import qualified Perm as P
import qualified Rubik as R
import Data.Monoid

action1 = mconcat $ take 3 $ repeat (P.inverse R.turnM `mappend` P.inverse R.turnU)
action2 = mconcat $ take 3 $ repeat (R.turnM `mappend` P.inverse R.turnU)
action = mconcat [action1, P.inverse R.turnU, action2, P.inverse R.turnU]

main :: IO ()
main = do {
        print $ R.defaultRubik;
        print $ P.clean action;
        print $ foldl1 lcm $ map (\x -> P.cycleLength x) $ P.findCycles action;
}
