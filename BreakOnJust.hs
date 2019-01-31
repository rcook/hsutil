#!/usr/bin/env stack
-- stack --resolver=lts-12.6 script

module BreakOnJust (main) where

import Test.Hspec
import Text.Read

-- | 'breakOnJust', applied to a function @f@ and a list @xs@, returns a triple
-- where the first element is the prefix (possibly empty) of @xs@ of elements
-- for which the function @f@ evaluates to @Nothing@, the second element is the
-- result of applying @f@ to the first element in @xs@ for which it evaluates to
-- non-@Nothing@ and the third element is the (possibly empty) remainder of the
-- list.
breakOnJust ::
    (a -> Maybe b)          -- ^ function @f@
    -> [a]                  -- ^ list @xs@
    -> ([a], Maybe b, [a])  -- ^ result
breakOnJust _ [] = ([], Nothing, [])
breakOnJust f (x : xs) =
    case f x of
        result@(Just _) -> ([], result, xs)
        _ -> let (gs, result, hs) = breakOnJust f xs in (x : gs, result, hs)

main :: IO ()
main = spec

spec :: IO ()
spec = hspec $
    describe "breakOnJust" $ do
        it "should work on empty" $
            (flip breakOnJust) [] (\s ->
                if s == "123"
                    then Just ("[" ++ s ++ "]")
                    else Nothing)
                `shouldBe` ([], Nothing, [])
        it "should work with no match" $
            (flip breakOnJust) ["foo", "bar", "123", "xyz", "pqr"] (const (Nothing :: Maybe String))
                `shouldBe` (["foo", "bar", "123", "xyz", "pqr"], Nothing, [])
        it "should work" $
            (flip breakOnJust) ["foo", "bar", "123", "xyz", "pqr"] (\s ->
                if s == "123"
                    then Just ("[" ++ s ++ "]")
                    else Nothing)
                `shouldBe` (["foo", "bar"], Just "[123]", ["xyz", "pqr"])
        it "should break on first match" $
            (flip breakOnJust) ["foo", "bar", "123", "xyz", "123", "pqr"] (\s ->
                if s == "123"
                    then Just ("[" ++ s ++ "]")
                    else Nothing)
                `shouldBe` (["foo", "bar"], Just "[123]", ["xyz", "123", "pqr"])
