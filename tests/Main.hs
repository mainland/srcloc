module Main (main) where

import           Control.Monad         (forM_)
import           Data.Data             (gmapT)
import qualified Data.List             as List
import           Data.List.NonEmpty    (NonEmpty (..))
import           Data.Loc
import qualified Data.Semigroup        as Semigroup
import           System.Environment    (getArgs, getExecutablePath)
import           System.Exit           (ExitCode (ExitSuccess))
import           System.Process        (readProcessWithExitCode)
import           Test.Tasty            (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit      (Assertion, assertEqual, testCase, (@?=))
import           Test.Tasty.QuickCheck (Arbitrary (..), UnicodeString (..),
                                        choose, frequency, testProperty)
import           Text.Read             (readMaybe)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--stress", mode] -> stressAggregation mode
        _                  -> defaultMain tests

tests :: TestTree
tests = testGroup "srcloc"
    [ testGroup "positions"
        [ testCase "starting position has known offset zero" $
            assertPos (startPos "input.hs") (Pos "input.hs" 1 1 (Just 0))
        , testCase "linePos has an unknown offset" $
            forM_ [1, 7] $ \line ->
                assertPos (linePos "input.hs" line) (Pos "input.hs" line 1 Nothing)
        , testCase "accessors" $
            posFields (Pos "input.hs" 7 12 (Just 40))
                @?= ("input.hs", 7, 12, Just 40)
        , testCase "newline resets the column" $
            assertPos (advancePos (Pos "input.hs" 7 12 (Just 40)) '\n')
                (Pos "input.hs" 8 1 (Just 41))
        , testCase "tabs advance to the next stop from every nearby column" $
            forM_ (zip [1..24] (replicate 8 9 ++ replicate 8 17 ++ replicate 8 25)) $ \(col, next) ->
                assertPos (advancePos (Pos "input.hs" 1 col (Just 10)) '\t')
                    (Pos "input.hs" 1 next (Just 11))
        , testCase "ordinary characters, Unicode, and CR advance one column" $
            forM_ ['x', '\NUL', '\x3bb', '\x1f600', '\r'] $ \c ->
                assertPos (advancePos (Pos "input.hs" 7 12 (Just 40)) c)
                    (Pos "input.hs" 7 13 (Just 41))
        , testCase "mixed input tracks coordinates and a known offset" $
            assertPos (List.foldl' advancePos (startPos "input.hs") "a\t\x3bb\n\tZ")
                (Pos "input.hs" 2 10 (Just 6))
        , testCase "mixed input preserves an unknown offset" $
            assertPos (List.foldl' advancePos (linePos "input.hs" 7) "a\t\x3bb\n\tZ")
                (Pos "input.hs" 8 10 Nothing)
        , testProperty "advancement increments known offsets and preserves unknowns" $
            \(SamplePos p) c ->
                let q = advancePos p c
                in posCoff q == fmap (+1) (posCoff p) && posFile q == posFile p
        , testProperty "tabs move forward by at most eight columns to a stop" $
            \(SamplePos p) ->
                let c = posCol (advancePos p '\t')
                in c > posCol p && c <= posCol p + 8 && (c - 1) `mod` 8 == 0
        , testCase "ordering prioritizes file, then line, then column" $ do
            compare (Pos "a" 9 9 (Just 99)) (Pos "b" 1 1 Nothing) @?= LT
            compare (Pos "a" 1 9 Nothing) (Pos "a" 2 1 (Just 0)) @?= LT
            compare (Pos "a" 1 1 (Just 99)) (Pos "a" 1 2 (Just 0)) @?= LT
        , testCase "positions from different helpers compare by coordinates" $ do
            let a = linePos "input.hs" 2
                b = advancePos (startPos "input.hs") '\n'
            posCoff a @?= Nothing
            posCoff b @?= Just 1
            (a == b) @?= True
            compare a b @?= EQ
        , testProperty "offsets do not affect equality or ordering" $
            \(SamplePos (Pos file line col _)) a b ->
                let p = Pos file line col a
                    q = Pos file line col b
                in p == q && compare p q == EQ && p <= q && q <= p
        , testProperty "comparison agrees with equality" $
            \(SamplePos p) (SamplePos q) -> (compare p q == EQ) == (p == q)
        , testProperty "ordering is transitive" $
            \(SamplePos p) (SamplePos q) (SamplePos r) ->
                not (p <= q && q <= r) || p <= r
        , testProperty "Read and Show round-trip all fields including offsets" $
            \(SamplePos p) -> fmap posFields (readMaybe (show p)) == Just (posFields p)
        , testProperty "generic identity preserves all fields including offsets" $
            \(SamplePos p) -> posFields (gmapT id p) == posFields p
        , testCase "Read accepts both offset forms" $ do
            fmap posFields (readMaybe "Pos \"input.hs\" 1 1 Nothing")
                @?= Just ("input.hs", 1, 1, Nothing)
            fmap posFields (readMaybe "Pos \"input.hs\" 1 1 (Just 0)")
                @?= Just ("input.hs", 1, 1, Just 0)
        ]
    , testGroup "locations"
        [ testCase "absent endpoints remain absent" $ do
            assertLoc (locStart NoLoc) NoLoc
            assertLoc (locEnd NoLoc) NoLoc
        , testProperty "endpoint projections preserve offsets" $
            \(SamplePos p) (SamplePos q) ->
                sameLoc (locStart (Loc p q)) (Loc p p)
                && sameLoc (locEnd (Loc p q)) (Loc q q)
        , testCase "combination selects outer endpoints with their offsets" $ do
            let a = Loc (Pos "input.hs" 1 2 (Just 1)) (Pos "input.hs" 1 4 Nothing)
                b = Loc (Pos "input.hs" 1 3 Nothing) (Pos "input.hs" 1 7 (Just 6))
                expected = Loc (point 2) (point 7)
            assertLoc (a `mappend` b) expected
            assertLoc (b `mappend` a) expected
        , testProperty "NoLoc is an identity including offset information" $ \(SampleLoc l) ->
            sameLoc (mempty `mappend` l) l && sameLoc (l `mappend` mempty) l
        , testProperty "Semigroup and Monoid combination agree including offsets" $
            \(SampleLoc a) (SampleLoc b) -> sameLoc (a Semigroup.<> b) (a `mappend` b)
        , testCase "NoLoc sorts before a concrete location" $
            compare NoLoc (Loc (point 1) (point 2)) @?= LT
        , testCase "location comparisons ignore offsets at both endpoints" $ do
            let a = Loc (Pos "input.hs" 1 1 Nothing) (Pos "input.hs" 1 2 (Just 10))
                b = Loc (Pos "input.hs" 1 1 (Just 0)) (Pos "input.hs" 1 2 (Just 20))
            (a == b) @?= True
            compare a b @?= EQ
        , testProperty "comparison agrees with equality" $
            \(SampleLoc a) (SampleLoc b) -> (compare a b == EQ) == (a == b)
        , testProperty "Read and Show round-trip endpoint offsets" $
            \(SampleLoc l) -> fmap locFields (readMaybe (show l)) == Just (locFields l)
        , testProperty "generic identity preserves endpoint offsets" $
            \(SampleLoc l) -> sameLoc (gmapT id l) l
        ]
    , testGroup "conservative offset merging"
        [ testCase "matching coordinates retain only agreeing known offsets" $
            forM_ offsetCases $ \(a, b, expectedOffset) -> do
                let p = Pos "input.hs" 1 1 a
                    q = Pos "input.hs" 1 1 b
                    result = Pos "input.hs" 1 1 expectedOffset
                    expected = Loc result result
                assertLoc (p <--> q) expected
                assertLoc (q <--> p) expected
                assertLoc (locOf (p `srcspan` q)) expected
                assertLoc (locOf (q `srcspan` p)) expected
                assertLoc (locOf [p, q]) expected
        , testCase "a conflict stays unknown when combined with a known offset" $ do
            let a = locOf (Pos "input.hs" 1 1 (Just 10))
                b = locOf (Pos "input.hs" 1 1 (Just 20))
                expected = locOf (Pos "input.hs" 1 1 Nothing)
            assertLoc ((a `mappend` b) `mappend` b) expected
            assertLoc (a `mappend` (b `mappend` b)) expected
        , testProperty "associativity includes offset information" $
            \(SampleLoc a) (SampleLoc b) (SampleLoc c) ->
                sameLoc ((a `mappend` b) `mappend` c) (a `mappend` (b `mappend` c))
        , testProperty "commutativity includes offset information" $
            \(SampleLoc a) (SampleLoc b) -> sameLoc (a `mappend` b) (b `mappend` a)
        , testProperty "idempotency includes offset information" $
            \(SampleLoc l) -> sameLoc (l `mappend` l) l
        , testCase "associativity for all tied endpoint offset combinations" $
            forM_ [(a, b, c) | a <- tiedSpans, b <- tiedSpans, c <- tiedSpans] $ \(a, b, c) ->
                assertLoc ((a `mappend` b) `mappend` c) (a `mappend` (b `mappend` c))
        , testCase "commutativity for all tied endpoint offset combinations" $
            forM_ [(a, b) | a <- tiedSpans, b <- tiedSpans] $ \(a, b) ->
                assertLoc (a `mappend` b) (b `mappend` a)
        , testCase "idempotency for all tied endpoint offset combinations" $
            forM_ tiedSpans $ \l -> assertLoc (l `mappend` l) l
        ]
    , testGroup "aggregation"
        [ testProperty "mconcat preserves right-fold results including offsets" $
            \samples ->
                let locations = [l | SampleLoc l <- samples]
                    expected = foldr mappend NoLoc locations
                in sameLoc (mconcat locations) expected
                    && sameLoc (locOf (mconcat (map SrcLoc locations))) expected
        , testProperty "sconcat preserves right-fold results including offsets" $
            \(SampleLoc first) samples ->
                let locations = [l | SampleLoc l <- samples]
                    expected = foldr mappend NoLoc (first : locations)
                in sameLoc (Semigroup.sconcat (first :| locations)) expected
                    && sameLoc (locOf (Semigroup.sconcat
                        (SrcLoc first :| map SrcLoc locations))) expected
        , testGroup "bounded stack"
            [ testCase mode (checkAggregationStack mode)
            | mode <- ["locOf", "locOfList", "mconcat-Loc", "mconcat-SrcLoc",
                       "sconcat-Loc", "sconcat-SrcLoc"]
            ]
        ]
    , testGroup "conversion and Located instances"
        [ testProperty "Loc and SrcLoc conversions preserve offsets" $
            \(SampleLoc l) ->
                sameLoc (fromLoc l :: Loc) l && sameLoc (locOf (fromLoc l :: SrcLoc)) l
                && sameLoc (locOf l) l && sameLoc (locOf (srclocOf l)) l
        , testProperty "position conversions produce point locations with offsets" $
            \(SamplePos p) ->
                sameLoc (fromPos p :: Loc) (Loc p p)
                && sameLoc (locOf (fromPos p :: SrcLoc)) (Loc p p)
                && sameLoc (locOf p) (Loc p p)
        , testCase "noLoc converts to both location types" $ do
            assertLoc (noLoc :: Loc) NoLoc
            assertLoc (locOf (noLoc :: SrcLoc)) NoLoc
        , testCase "absent Maybe and empty lists have no location" $ do
            assertLoc (locOf (Nothing :: Maybe Pos)) NoLoc
            assertLoc (locOf ([] :: [Pos])) NoLoc
            assertLoc (locOfList ([] :: [Loc])) NoLoc
        , testProperty "Just preserves the location and offsets" $ \(SampleLoc l) ->
            sameLoc (locOf (Just l)) l
        , testCase "lists merge all locations including absent entries" $ do
            let a = Loc (point 1) (point 3)
                b = Loc (point 2) (point 5)
                expected = Loc (point 1) (point 5)
            assertLoc (locOf [NoLoc, b, a, NoLoc]) expected
            assertLoc (locOfList [b, a]) expected
            assertLoc (locOf [[NoLoc, b], [], [a]]) expected
        , testCase "list instance honors a custom locOfList" $ do
            let a = Loc (point 1) (point 2)
                b = Loc (point 4) (point 5)
            assertLoc (locOf [FirstLocation a, FirstLocation b]) a
        , testCase "span operators accept different Located types" $ do
            let expected = Loc (point 1) (point 5)
            assertLoc (point 1 <--> L (Loc (point 3) (point 5)) ()) expected
            assertLoc (locOf (Just (point 1) `srcspan` [point 3, point 5])) expected
        ]
    , testGroup "SrcLoc"
        [ testProperty "equality and ordering erase locations" $
            \(SampleLoc a) (SampleLoc b) ->
                SrcLoc a == SrcLoc b && compare (SrcLoc a) (SrcLoc b) == EQ
        , testProperty "Show erases the location at every precedence" $
            \(SampleLoc l) -> all (\d -> showsPrec d (SrcLoc l) "!" == "noLoc!") [0..11]
        , testProperty "reading displayed SrcLoc yields NoLoc" $
            \(SampleLoc l) -> fmap (locFields . locOf) (readMaybe (show (SrcLoc l)) :: Maybe SrcLoc)
                == Just (locFields NoLoc)
        , testProperty "explicit Read preserves the underlying location and offsets" $
            \(SampleLoc l) ->
                fmap (locFields . locOf) (readMaybe ("SrcLoc (" ++ show l ++ ")") :: Maybe SrcLoc)
                    == Just (locFields l)
        , testCase "Read accepts parenthesized forms" $
            forM_ ["noLoc", "(noLoc)", "((noLoc))", "SrcLoc NoLoc", "(SrcLoc NoLoc)"] $ \s ->
                fmap (locFields . locOf) (readMaybe s :: Maybe SrcLoc) @?= Just (locFields NoLoc)
        , testCase "Read respects constructor application precedence" $ do
            map (\(l, rest) -> (locFields (locOf l), rest)) (readsPrec 10 "SrcLoc NoLoc tail" :: [(SrcLoc, String)])
                @?= [(locFields NoLoc, " tail")]
            (readsPrec 11 "SrcLoc NoLoc" :: [(SrcLoc, String)]) @?= []
            map (\(l, rest) -> (locFields (locOf l), rest)) (readsPrec 11 "(SrcLoc NoLoc) tail" :: [(SrcLoc, String)])
                @?= [(locFields NoLoc, " tail")]
        , testCase "Read rejects malformed input" $
            forM_ ["", "NoLoc", "SrcLoc", "SrcLoc junk", "noLoc trailing"] $ \s ->
                (readMaybe s :: Maybe SrcLoc) @?= Nothing
        , testProperty "combination preserves the merged offsets" $
            \(SampleLoc a) (SampleLoc b) ->
                sameLoc (locOf (SrcLoc a `mappend` SrcLoc b)) (a `mappend` b)
                && sameLoc (locOf (SrcLoc a Semigroup.<> SrcLoc b)) (a `mappend` b)
        , testProperty "identities preserve the underlying offsets" $ \(SampleLoc l) ->
            sameLoc (locOf (mempty `mappend` SrcLoc l)) l
            && sameLoc (locOf (SrcLoc l `mappend` mempty)) l
        , testCase "mconcat combines underlying spans" $
            assertLoc (locOf (mconcat [srclocOf (point 5), mempty, srclocOf (point 1)]))
                (Loc (point 1) (point 5))
        , testProperty "generic identity preserves the underlying offsets" $
            \(SampleLoc l) -> sameLoc (locOf (gmapT id (SrcLoc l))) l
        ]
    , testGroup "located payloads"
        [ testProperty "equality and ordering use only the payload" $
            \(SampleLoc a) (SampleLoc b) x y ->
                let lhs = L a (x :: Int)
                    rhs = L b y
                in (lhs == rhs) == (x == y) && compare lhs rhs == compare x y
        , testCase "Show preserves payload precedence in nested applications" $
            show (Just (L NoLoc (Just (1 :: Int)))) @?= "Just (Just 1)"
        , testProperty "Show delegates precedence and preserves suffixes" $
            \(SampleLoc l) x (UnicodeString suffix) ->
                all (\d -> showsPrec d (L l (Just (x :: Int))) suffix == showsPrec d (Just x) suffix) [0..11]
        , testProperty "relocation changes location and preserves payload" $
            \(SampleLoc a) (SampleLoc b) x ->
                let moved = reloc b (L a (x :: Int))
                in sameLoc (locOf moved) b && unLoc moved == x
        , testProperty "Functor changes payload and preserves location and offsets" $
            \(SampleLoc l) x ->
                let mapped = fmap (+1) (L l (x :: Int))
                in unLoc mapped == x + 1 && sameLoc (locOf mapped) l
        , testCase "payload operations do not force the location" $ do
            let x = L (error "location forced") (3 :: Int)
            unLoc x @?= 3
            show x @?= "3"
            (x == L NoLoc 3) @?= True
            compare x (L NoLoc 4) @?= LT
            assertLoc (locOf (reloc NoLoc x)) NoLoc
        , testCase "location operations do not force the payload" $ do
            let l = Loc (point 1) (point 2)
                x = L l (error "payload forced" :: Int)
            assertLoc (locOf x) l
            assertLoc (locOf (fmap (+1) x)) l
        , testProperty "generic identity preserves location, offsets, and payload" $
            \(SampleLoc l) x ->
                let rebuilt = gmapT id (L l (x :: Int))
                in sameLoc (locOf rebuilt) l && unLoc rebuilt == x
        ]
    , testGroup "display"
        [ testCase "NoLoc" $ displayLoc NoLoc @?= "<no location>"
        , testCase "point" $ displayPos (Pos "input.hs" 2 3 (Just 10)) @?= "input.hs:2:3"
        , testCase "same-line span" $
            displayLoc (Loc (Pos "input.hs" 2 3 (Just 10)) (Pos "input.hs" 2 5 (Just 12)))
                @?= "input.hs:2:3-5"
        , testCase "multi-line span" $
            displayLoc (Loc (Pos "input.hs" 2 3 (Just 10)) (Pos "input.hs" 4 5 (Just 40)))
                @?= "input.hs:2:3-4:5"
        , testCase "cross-file span with matching line and column" $
            displayLoc (startPos "a.hs" <--> startPos "b.hs")
                @?= "a.hs:1:1-b.hs:1:1"
        , testCase "cross-file span with matching line" $
            displayLoc (Loc (Pos "a.hs" 2 3 (Just 10)) (Pos "b.hs" 2 5 Nothing))
                @?= "a.hs:2:3-b.hs:2:5"
        , testCase "cross-file span with different lines" $
            displayLoc (Loc (Pos "a.hs" 2 3 Nothing) (Pos "dir/b \x3bb.hs" 4 5 (Just 40)))
                @?= "a.hs:2:3-dir/b \x3bb.hs:4:5"
        , testCase "cross-file display preserves endpoint order" $
            displayLoc (Loc (Pos "b.hs" 4 5 Nothing) (Pos "a.hs" 2 3 Nothing))
                @?= "b.hs:4:5-a.hs:2:3"
        , testCase "filename contents are preserved" $
            displayPos (startPos "dir/a b\x3bb.hs") @?= "dir/a b\x3bb.hs:1:1"
        , testProperty "offsets do not affect position display" $
            \(SamplePos (Pos file line col _)) a b ->
                displayPos (Pos file line col a) == displayPos (Pos file line col b)
        , testProperty "position ShowS agrees with String and preserves suffix" $
            \(SamplePos p) (UnicodeString suffix) -> displaySPos p suffix == displayPos p ++ suffix
        , testProperty "location ShowS agrees with String and preserves suffix" $
            \(SampleLoc l) (UnicodeString suffix) -> displaySLoc l suffix == displayLoc l ++ suffix
        ]
    ]

-- Run stress cases in a separate process so the stack limit applies regardless
-- of the surrounding test runner, and a stack overflow becomes a test failure.
checkAggregationStack :: String -> Assertion
checkAggregationStack mode = do
    executable <- getExecutablePath
    (code, out, err) <- readProcessWithExitCode executable
        ["--stress", mode, "+RTS", "-K8m", "-RTS"] ""
    assertEqual (mode ++ " failed with an 8 MiB stack:\n" ++ out ++ err)
        ExitSuccess code

stressAggregation :: String -> Assertion
stressAggregation mode = assertLoc actual (Loc (point 1) (point count))
  where
    count = 1000000
    positions = map point [1..count]
    actual = case mode of
        "locOf" -> locOf positions
        "locOfList" -> locOfList positions
        "mconcat-Loc" -> mconcat (map locOf positions)
        "mconcat-SrcLoc" -> locOf (mconcat (map srclocOf positions))
        "sconcat-Loc" -> Semigroup.sconcat
            (locOf (point 1) :| map (locOf . point) [2..count])
        "sconcat-SrcLoc" -> locOf (Semigroup.sconcat
            (srclocOf (point 1) :| map (srclocOf . point) [2..count]))
        _ -> error ("Unknown stress case: " ++ mode)

-- Inspect all fields explicitly because Eq ignores offsets (and SrcLoc ignores
-- the entire location). Use these observations for preservation and merge laws.
type PosFields = (FilePath, Int, Int, Maybe Int)

posFields :: Pos -> PosFields
posFields p = (posFile p, posLine p, posCol p, posCoff p)

locFields :: Loc -> Maybe (PosFields, PosFields)
locFields NoLoc     = Nothing
locFields (Loc p q) = Just (posFields p, posFields q)

sameLoc :: Loc -> Loc -> Bool
sameLoc a b = locFields a == locFields b

assertPos :: Pos -> Pos -> Assertion
assertPos actual expected = posFields actual @?= posFields expected

assertLoc :: Loc -> Loc -> Assertion
assertLoc actual expected = locFields actual @?= locFields expected

-- An explicit truth table includes unknowns and conflicting known offsets.
offsetCases :: [(Maybe Int, Maybe Int, Maybe Int)]
offsetCases =
    [ (Nothing, Nothing, Nothing)
    , (Nothing, Just 10, Nothing)
    , (Nothing, Just 20, Nothing)
    , (Just 10, Nothing, Nothing)
    , (Just 10, Just 10, Just 10)
    , (Just 10, Just 20, Nothing)
    , (Just 20, Nothing, Nothing)
    , (Just 20, Just 10, Nothing)
    , (Just 20, Just 20, Just 20)
    ]

-- Vary offsets independently at both tied endpoints. Include NoLoc to exercise
-- the distinction between an absent span and an unknown endpoint offset.
tiedSpans :: [Loc]
tiedSpans = NoLoc :
    [ Loc (Pos "input.hs" 1 1 a) (Pos "input.hs" 1 2 b)
    | a <- offsets, b <- offsets
    ]
  where
    offsets = [Nothing, Just 0, Just 10, Just 20]

point :: Int -> Pos
point col = Pos "input.hs" 1 col (Just (col - 1))

-- Bound coordinates and known offsets to avoid Int overflow in advancement.
newtype SamplePos = SamplePos Pos
    deriving Show

instance Arbitrary SamplePos where
    arbitrary = do
        UnicodeString file <- arbitrary
        line <- choose (1, 1000)
        col <- choose (1, 1000)
        offset <- frequency [(1, return Nothing), (3, fmap Just (choose (0, 100000)))]
        return (SamplePos (Pos file line col offset))
    shrink (SamplePos (Pos file line col offset)) =
        [ SamplePos (Pos f l c o)
        | (f, l, c, o) <- shrink (file, line, col, offset)
        , l > 0, c > 0, maybe True (>= 0) o
        ]

-- Constructors allow arbitrary endpoints, including reversed and cross-file
-- spans. The general laws cover those too, not just well-formed source ranges.
newtype SampleLoc = SampleLoc Loc
    deriving Show

instance Arbitrary SampleLoc where
    arbitrary = frequency
        [ (1, return (SampleLoc NoLoc))
        , (4, do
            SamplePos p <- arbitrary
            SamplePos q <- arbitrary
            return (SampleLoc (Loc p q)))
        ]
    shrink (SampleLoc NoLoc) = []
    shrink (SampleLoc (Loc p q)) = SampleLoc NoLoc :
        [SampleLoc (Loc a b) | (SamplePos a, SamplePos b) <- shrink (SamplePos p, SamplePos q)]

newtype FirstLocation = FirstLocation Loc

instance Located FirstLocation where
    locOf (FirstLocation l) = l
    locOfList []    = NoLoc
    locOfList (x:_) = locOf x
