{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main where
  
import Retrie
import Debug.Trace
import Data.Data
import qualified GHC
main :: IO ()
main = runScript $ \opts -> do
  [rewrite] <- parseRewrites opts [Adhoc "forall f x xs g a s. f (\\x -> x ^. xs) (g (a s)) = f (xs) (g (Just s))"]
  return $ apply [setRewriteTransformer stringToFooArg rewrite]

argMapping :: FastString -> String
argMapping val = "StorageType." <> (snd $ splitAt 3 $ unpackFS val)

stringToFooArg :: MatchResultTransformer
stringToFooArg _ctxt match
  | MatchResult substitution template <- match
  , Just (HoleExpr expr) <- traceShowId $ lookupSubst "xs" substitution 
  , L _ b@(HsVar x p) <- astA expr = do
    let !k = traceShowId $ rdrFS $ GHC.unLoc p
    newExpr <- parseExpr $ traceShowId $ argMapping k
    return $
      MatchResult (extendSubst substitution "xs" (HoleExpr newExpr)) template
  | otherwise = return NoMatch
