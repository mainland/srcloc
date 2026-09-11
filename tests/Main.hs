module Main (main) where

import           Control.Monad         (forM_)
import           Data.Data             (gmapT)
import qualified Data.List             as List
import           Data.Loc
import qualified Data.Semigroup        as Semigroup
import           Test.Tasty            (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))
import           Test.Tasty.QuickCheck (Arbitrary (..), UnicodeString (..),
                                        choose, frequency, testProperty)
import           Text.Read             (readMaybe)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "srcloc"
    [ testGroup "positions"
        [ testCase "starting conventions" $
            startPos "input.hs" @?= Pos "input.hs" 1 1 0
        , testCase "linePos resets column and character offset" $
            linePos "input.hs" 7 @?= Pos "input.hs" 7 1 0
        , testCase "accessors" $ do
            let p = Pos "input.hs" 7 12 40
            (posFile p, posLine p, posCol p, posCoff p)
                @?= ("input.hs", 7, 12, 40)
        , testCase "newline resets the column" $
            advancePos (Pos "input.hs" 7 12 40) '\n'
                @?= Pos "input.hs" 8 1 41
        , testCase "tabs advance to the next stop from every nearby column" $
            forM_ (zip [1..24] (replicate 8 9 ++ replicate 8 17 ++ replicate 8 25)) $ \(col, next) ->
                advancePos (Pos "input.hs" 1 col 10) '\t'
                    @?= Pos "input.hs" 1 next 11
        , testCase "ordinary characters, Unicode, and CR each advance one column" $
            forM_ ['x', '\NUL', '\x3bb', '\x1f600', '\r'] $ \c ->
                advancePos (Pos "input.hs" 7 12 40) c
                    @?= Pos "input.hs" 7 13 41
        , testCase "mixed input tracks lines, columns, and character offsets" $
            List.foldl' advancePos (startPos "input.hs") "a\t\x3bb\n\tZ"
                @?= Pos "input.hs" 2 10 6
        , testProperty "every character advances the offset and preserves the file" $
            \(SamplePos p) c ->
                let q = advancePos p c
                in posCoff q == posCoff p + 1 && posFile q == posFile p
        , testProperty "tabs move forward by at most eight columns to a stop" $
            \(SamplePos p) ->
                let c = posCol (advancePos p '\t')
                in c > posCol p && c <= posCol p + 8 && (c - 1) `mod` 8 == 0
        , testCase "ordering prioritizes file, then line, then column" $ do
            compare (Pos "a" 9 9 99) (Pos "b" 1 1 0) @?= LT
            compare (Pos "a" 1 9 99) (Pos "a" 2 1 0) @?= LT
            compare (Pos "a" 1 1 99) (Pos "a" 1 2 0) @?= LT
        , testProperty "Read and Show round-trip all fields" $
            \(SamplePos p) -> readMaybe (show p) == Just p
        , testProperty "generic identity preserves all fields" $
            \(SamplePos p) -> gmapT id p == p
        ]
    , testGroup "locations"
        [ testCase "absent endpoints remain absent" $ do
            locStart NoLoc @?= NoLoc
            locEnd NoLoc @?= NoLoc
        , testProperty "endpoint projections preserve their position" $
            \(SamplePos p) (SamplePos q) ->
                locStart (Loc p q) == Loc p p && locEnd (Loc p q) == Loc q q
        , testCase "combination covers both spans in either order" $ do
            let a = Loc (point 2) (point 4)
                b = Loc (point 3) (point 7)
                expected = Loc (point 2) (point 7)
            a `mappend` b @?= expected
            b `mappend` a @?= expected
        , testProperty "left and right identities" $ \(SampleLoc l) ->
            mempty `mappend` l == l && l `mappend` mempty == l
        , testProperty "combination is associative" $
            \(SampleLoc a) (SampleLoc b) (SampleLoc c) ->
                (a `mappend` b) `mappend` c == a `mappend` (b `mappend` c)
        , testProperty "combination is idempotent" $ \(SampleLoc l) ->
            l `mappend` l == l
        , testProperty "Semigroup and Monoid combination agree" $
            \(SampleLoc a) (SampleLoc b) -> (a Semigroup.<> b) == a `mappend` b
        , testCase "NoLoc sorts before a concrete location" $
            compare NoLoc (Loc (point 1) (point 2)) @?= LT
        , testProperty "Read and Show round-trip endpoints" $
            \(SampleLoc l) -> readMaybe (show l) == Just l
        , testProperty "generic identity preserves endpoints" $
            \(SampleLoc l) -> gmapT id l == l
        ]
    , testGroup "conversion and Located instances"
        [ testProperty "Loc and SrcLoc conversions preserve locations" $
            \(SampleLoc l) ->
                (fromLoc l :: Loc) == l && locOf (fromLoc l :: SrcLoc) == l
                && locOf l == l && locOf (srclocOf l) == l
        , testProperty "position conversions produce point locations" $
            \(SamplePos p) ->
                (fromPos p :: Loc) == Loc p p
                && locOf (fromPos p :: SrcLoc) == Loc p p
                && locOf p == Loc p p
        , testCase "noLoc converts to both location types" $ do
            (noLoc :: Loc) @?= NoLoc
            locOf (noLoc :: SrcLoc) @?= NoLoc
        , testCase "absent Maybe and empty lists have no location" $ do
            locOf (Nothing :: Maybe Pos) @?= NoLoc
            locOf ([] :: [Pos]) @?= NoLoc
            locOfList ([] :: [Loc]) @?= NoLoc
        , testProperty "Just preserves the location" $ \(SampleLoc l) ->
            locOf (Just l) == l
        , testCase "lists merge all locations including absent entries" $ do
            let a = Loc (point 1) (point 3)
                b = Loc (point 2) (point 5)
                expected = Loc (point 1) (point 5)
            locOf [NoLoc, b, a, NoLoc] @?= expected
            locOfList [b, a] @?= expected
            locOf [[NoLoc, b], [], [a]] @?= expected
        , testCase "list instance honors a custom locOfList" $ do
            let a = Loc (point 1) (point 2)
                b = Loc (point 4) (point 5)
            locOf [FirstLocation a, FirstLocation b] @?= a
        , testCase "span operators accept different Located types" $ do
            let expected = Loc (point 1) (point 5)
            point 1 <--> L (Loc (point 3) (point 5)) () @?= expected
            locOf (Just (point 1) `srcspan` [point 3, point 5]) @?= expected
        ]
    , testGroup "SrcLoc"
        [ testProperty "equality and ordering erase locations" $
            \(SampleLoc a) (SampleLoc b) ->
                SrcLoc a == SrcLoc b && compare (SrcLoc a) (SrcLoc b) == EQ
        , testProperty "Show erases the location at every precedence" $
            \(SampleLoc l) -> all (\d -> showsPrec d (SrcLoc l) "!" == "noLoc!") [0..11]
        , testProperty "reading displayed SrcLoc yields NoLoc" $
            \(SampleLoc l) -> fmap locOf (readMaybe (show (SrcLoc l)) :: Maybe SrcLoc) == Just NoLoc
        , testProperty "explicit Read preserves the underlying location" $
            \(SampleLoc l) ->
                fmap locOf (readMaybe ("SrcLoc (" ++ show l ++ ")") :: Maybe SrcLoc) == Just l
        , testCase "Read accepts parenthesized forms" $
            forM_ ["noLoc", "(noLoc)", "((noLoc))", "SrcLoc NoLoc", "(SrcLoc NoLoc)"] $ \s ->
                fmap locOf (readMaybe s :: Maybe SrcLoc) @?= Just NoLoc
        , testCase "Read respects constructor application precedence" $ do
            map (\(l, rest) -> (locOf l, rest)) (readsPrec 10 "SrcLoc NoLoc tail" :: [(SrcLoc, String)])
                @?= [(NoLoc, " tail")]
            (readsPrec 11 "SrcLoc NoLoc" :: [(SrcLoc, String)]) @?= []
            map (\(l, rest) -> (locOf l, rest)) (readsPrec 11 "(SrcLoc NoLoc) tail" :: [(SrcLoc, String)])
                @?= [(NoLoc, " tail")]
        , testCase "Read rejects malformed input" $
            forM_ ["", "NoLoc", "SrcLoc", "SrcLoc junk", "noLoc trailing"] $ \s ->
                (readMaybe s :: Maybe SrcLoc) @?= Nothing
        , testProperty "combination preserves actual endpoints" $
            \(SampleLoc a) (SampleLoc b) ->
                locOf (SrcLoc a `mappend` SrcLoc b) == a `mappend` b
                && locOf (SrcLoc a Semigroup.<> SrcLoc b) == a `mappend` b
        , testProperty "identities preserve actual endpoints" $ \(SampleLoc l) ->
            locOf (mempty `mappend` SrcLoc l) == l
            && locOf (SrcLoc l `mappend` mempty) == l
        , testCase "mconcat combines underlying spans" $
            locOf (mconcat [srclocOf (point 5), mempty, srclocOf (point 1)])
                @?= Loc (point 1) (point 5)
        , testProperty "generic identity preserves the underlying location" $
            \(SampleLoc l) -> locOf (gmapT id (SrcLoc l)) == l
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
                in locOf moved == b && unLoc moved == x
        , testProperty "Functor changes payload and preserves location" $
            \(SampleLoc l) x ->
                let mapped = fmap (+1) (L l (x :: Int))
                in unLoc mapped == x + 1 && locOf mapped == l
        , testCase "payload operations do not force the location" $ do
            let x = L (error "location forced") (3 :: Int)
            unLoc x @?= 3
            show x @?= "3"
            (x == L NoLoc 3) @?= True
            compare x (L NoLoc 4) @?= LT
            locOf (reloc NoLoc x) @?= NoLoc
        , testCase "location operations do not force the payload" $ do
            let l = Loc (point 1) (point 2)
                x = L l (error "payload forced" :: Int)
            locOf x @?= l
            locOf (fmap (+1) x) @?= l
        , testProperty "generic identity preserves location and payload" $
            \(SampleLoc l) x ->
                let rebuilt = gmapT id (L l (x :: Int))
                in locOf rebuilt == l && unLoc rebuilt == x
        ]
    , testGroup "display"
        [ testCase "NoLoc" $ displayLoc NoLoc @?= "<no location>"
        , testCase "point" $ displayPos (Pos "input.hs" 2 3 10) @?= "input.hs:2:3"
        , testCase "same-line span" $
            displayLoc (Loc (Pos "input.hs" 2 3 10) (Pos "input.hs" 2 5 12))
                @?= "input.hs:2:3-5"
        , testCase "multi-line span" $
            displayLoc (Loc (Pos "input.hs" 2 3 10) (Pos "input.hs" 4 5 40))
                @?= "input.hs:2:3-4:5"
        , testCase "filename contents are preserved" $
            displayPos (startPos "dir/a b\x3bb.hs") @?= "dir/a b\x3bb.hs:1:1"
        , testProperty "position ShowS agrees with String and preserves suffix" $
            \(SamplePos p) (UnicodeString suffix) -> displaySPos p suffix == displayPos p ++ suffix
        , testProperty "location ShowS agrees with String and preserves suffix" $
            \(SampleLoc l) (UnicodeString suffix) -> displaySLoc l suffix == displayLoc l ++ suffix
        ]
    ]

-- Positions on one coherent line for concrete span examples.
point :: Int -> Pos
point col = Pos "input.hs" 1 col (col - 1)

-- Public constructors permit independent offsets and arbitrary span endpoints.
-- Keep coordinates bounded to avoid Int overflow in advancement properties.
newtype SamplePos = SamplePos Pos
    deriving Show

instance Arbitrary SamplePos where
    arbitrary = do
        UnicodeString file <- arbitrary
        line <- choose (1, 1000)
        col <- choose (1, 1000)
        offset <- choose (0, 100000)
        return (SamplePos (Pos file line col offset))
    shrink (SamplePos (Pos file line col offset)) =
        [ SamplePos (Pos f l c o)
        | (f, l, c, o) <- shrink (file, line, col, offset)
        , l > 0, c > 0, o >= 0
        ]

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
