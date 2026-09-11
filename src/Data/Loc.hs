{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE Safe               #-}

-- |
-- Module      :  Data.Loc
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2015
-- License     :  BSD-style
-- Maintainer  :  Geoffrey Mainland <mainland@cs.drexel.edu>
--
-- Source positions and spans, with optional character offsets.
--
-- = Positions and offsets
--
-- @Pos@ stores a filename, a line and column starting at 1, and an optional
-- character offset starting at 0. @Just n@ means the offset is known.
-- @Nothing@ means it is unknown. Offsets count characters, including tabs and
-- newlines, rather than bytes.
--
-- 'startPos' supplies @Just 0@. 'linePos' supplies @Nothing@ because it has no
-- information about preceding line lengths. 'advancePos' increments known
-- offsets and preserves unknown offsets. Tabs use stops of width 8 and newlines
-- follow UNIX conventions.
--
-- Position equality and ordering compare the filename, line, and column.
-- Offsets do not affect comparisons. Inspect 'posCoff' explicitly when offset
-- information matters.
--
-- = Combining locations
--
-- Location combination takes the earliest beginning and latest end. Endpoints
-- with different coordinates retain the selected position's offset. For
-- endpoints with matching filename, line, and column:
--
-- * Two identical known offsets combine to that known offset.
-- * Conflicting known offsets combine to @Nothing@.
-- * An unknown offset combined with any offset produces @Nothing@.
--
-- Combination is associative, commutative, and idempotent, including offset
-- information. Grouping and input order do not change the result. Combining a
-- span with itself preserves all its fields. An unknown offset remains unknown
-- when combined with a known offset at the same coordinates, so an offset lost
-- to a conflict cannot be restored by regrouping. 'NoLoc' is the identity.
--
-- These rules apply to the @Loc@ and @SrcLoc@ monoid and semigroup instances,
-- '<-->', 'srcspan', and the default aggregation of located lists.
--
-- List aggregation through @mconcat@, @sconcat@, and the default 'locOfList'
-- uses strict left folds. It consumes finite lists without stack usage growing
-- with the number of locations. Evaluating individual values or comparing their
-- filenames may have additional costs.
--
-- = Migrating from integer offsets
--
-- The offset field of @Pos@ and the result of 'posCoff' use @Maybe Int@.
-- Code written for the former @Int@ API should wrap known offsets in @Just@
-- and use @Nothing@ for unknown offsets. Code reading offsets must handle both
-- cases. The @Read@, @Show@, and generic @Data@ representations also change.
-- Position equality now ignores offsets, and merging tied endpoints may discard
-- offset information when it is unknown or conflicting.

module Data.Loc (
    Pos(..),
    posFile,
    posLine,
    posCol,
    posCoff,
    startPos,
    linePos,
    advancePos,
    displayPos,
    displaySPos,

    Loc(..),
    locStart,
    locEnd,

    (<-->),

    displayLoc,
    displaySLoc,

    SrcLoc(..),
    srclocOf,
    srcspan,

    IsLocation(..),
    noLoc,

    Located(..),

    Relocatable(..),

    L(..),
    unLoc
  ) where

import           Data.Data          (Data (..))
import qualified Data.List          as List
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Semigroup     (Semigroup (..))

-- | Position type.
--
-- Equality and ordering use only the file name, line, and column, in that
-- order. The optional character offset is additional information and does not
-- affect comparisons.
data Pos = -- | Source file name, line, column, and optional character offset.
           --
           -- Line numbering starts at 1, column offset starts at 1, and
           -- known character offsets start at 0. 'Nothing' denotes an unknown
           -- offset.
           Pos !FilePath
               {-# UNPACK #-} !Int
               {-# UNPACK #-} !Int
               !(Maybe Int)
  deriving (Read, Show, Data)

instance Eq Pos where
    Pos f1 l1 c1 _ == Pos f2 l2 c2 _ =
        (f1, l1, c1) == (f2, l2, c2)

instance Ord Pos where
    compare (Pos f1 l1 c1 _) (Pos f2 l2 c2 _) =
        compare (f1, l1, c1) (f2, l2, c2)

-- | Position file.
posFile :: Pos -> FilePath
posFile (Pos f _ _ _) = f

-- | Position line.
posLine :: Pos -> Int
posLine (Pos _ l _ _) = l

-- | Position column.
posCol :: Pos -> Int
posCol (Pos _ _ c _) = c

-- | Position character offset, or 'Nothing' when unknown.
posCoff :: Pos -> Maybe Int
posCoff (Pos _ _ _ coff) = coff

-- | Starting position for given file, with known character offset 0.
startPos :: FilePath -> Pos
startPos f = Pos f startLine startCol startCoff

startLine :: Int
startLine = 1

startCol :: Int
startCol = 1

startCoff :: Maybe Int
startCoff = Just 0

-- | Position corresponding to given file and line.
--
-- The character offset is unknown because preceding line lengths are not given.
linePos :: FilePath -> Int -> Pos
linePos f l = Pos f l startCol Nothing

-- | Advance a position by a single character. Newlines increment the line
-- number, tabs increase the position column following a tab stop width of 8,
-- and all other characters increase the position column by one. All characters,
-- including newlines and tabs, increase a known character offset by 1. Unknown
-- offsets remain unknown.
--
-- Note that 'advancePos' assumes UNIX-style newlines.
advancePos :: Pos -> Char -> Pos
advancePos (Pos f l _ coff) '\n' = Pos f (l+1) startCol     (advanceCoff coff)
advancePos (Pos f l c coff) '\t' = Pos f l     nextTabStop  (advanceCoff coff)
  where nextTabStop = ((c+7) `div` 8) * 8 + 1
advancePos (Pos f l c coff) _    = Pos f l     (c + 1)      (advanceCoff coff)

-- Force known offsets so repeated advancement does not accumulate additions
-- inside Just, even when only the position itself is evaluated.
advanceCoff :: Maybe Int -> Maybe Int
advanceCoff Nothing     = Nothing
advanceCoff (Just coff) = let next = coff + 1 in next `seq` Just next

-- | Location type, consisting of a beginning position and an end position.
--
-- Comparisons ignore character offsets, as for @Pos@. Combining locations takes
-- the earliest beginning and latest end. When endpoint coordinates match, their
-- offset is retained only if both offsets are known and equal. Otherwise the
-- merged offset is unknown. 'NoLoc' is the identity for combination.
--
-- Combination is associative, commutative, and idempotent, including the
-- resulting offset information. An unknown offset at a tied endpoint remains
-- unknown when combined with a known offset, so regrouping cannot restore
-- information lost to a conflict.
data Loc =  NoLoc
         |  -- | Beginning and end positions
            Loc  {-# UNPACK #-} !Pos
                 {-# UNPACK #-} !Pos
  deriving (Eq, Ord, Read, Show, Data)

-- | Starting position of the location.
locStart :: Loc -> Loc
locStart  NoLoc     = NoLoc
locStart  (Loc p _) = Loc p p

-- | Ending position of the location.
locEnd :: Loc -> Loc
locEnd  NoLoc     = NoLoc
locEnd  (Loc _ p) = Loc p p

-- | Append two locations.
locAppend :: Loc -> Loc -> Loc
locAppend NoLoc       l           = l
locAppend l           NoLoc       = l
locAppend (Loc b1 e1) (Loc b2 e2) = Loc (minPos b1 b2) (maxPos e1 e2)

minPos :: Pos -> Pos -> Pos
minPos p q = case compare p q of
    LT -> p
    EQ -> mergePosOffsets p q
    GT -> q

maxPos :: Pos -> Pos -> Pos
maxPos p q = case compare p q of
    LT -> q
    EQ -> mergePosOffsets p q
    GT -> p

-- Called only for positions at the same source coordinates. Unknown offsets
-- absorb known offsets so conflicts cannot be undone by later combinations.
mergePosOffsets :: Pos -> Pos -> Pos
mergePosOffsets (Pos f l c o1) (Pos _ _ _ o2) =
    Pos f l c (if o1 == o2 then o1 else Nothing)

instance Semigroup Loc where
    (<>) = locAppend
    sconcat (l :| ls) = List.foldl' locAppend l ls

instance Monoid Loc where
    mempty = NoLoc
    mconcat = List.foldl' locAppend NoLoc
#if !(MIN_VERSION_base(4,11,0))
    mappend = locAppend
#endif

-- | Merge the locations of two 'Located' values.
(<-->) :: (Located a, Located b) => a -> b -> Loc
x <--> y = locOf x `mappend` locOf y

infixl 6 <-->

-- | Source location type. Source location are all equal, which allows AST nodes
-- to be compared modulo location information.
newtype SrcLoc = SrcLoc Loc
  deriving (Data)

instance Monoid SrcLoc where
    mempty = SrcLoc mempty
    mconcat = List.foldl' mappend mempty
#if !(MIN_VERSION_base(4,11,0))
    mappend (SrcLoc l1) (SrcLoc l2) = SrcLoc (l1 `mappend` l2)
#endif

instance Semigroup SrcLoc where
  SrcLoc l1 <> SrcLoc l2 = SrcLoc (l1 <> l2)
  sconcat (l :| ls) = List.foldl' (<>) l ls

instance Eq SrcLoc where
    _ == _ = True

instance Ord SrcLoc where
    compare _ _ = EQ

instance Show SrcLoc where
    showsPrec _ _ = showString "noLoc"

instance Read SrcLoc where
    readsPrec p input =
        readParen False
          (\s -> [(SrcLoc NoLoc, s') |
                  ("noLoc", s') <- lex s])
          input
        ++
        readParen (p > app_prec)
          (\s -> [(SrcLoc l, s'') |
                  ("SrcLoc", s') <- lex s,
                  (l, s'') <- readsPrec (app_prec+1) s'])
          input
      where
        app_prec = 10

-- | The @SrcLoc@ of a 'Located' value.
srclocOf :: Located a => a -> SrcLoc
srclocOf = fromLoc . locOf

-- | A @SrcLoc@ with (minimal) span that includes two 'Located' values.
srcspan :: (Located a, Located b) => a -> b -> SrcLoc
x `srcspan` y = SrcLoc (locOf x `mappend` locOf y)

infixl 6 `srcspan`

-- | Locations
class IsLocation a where
    fromLoc :: Loc -> a
    fromPos :: Pos -> a
    fromPos p = fromLoc (Loc p p)

instance IsLocation Loc where
    fromLoc = id

instance IsLocation SrcLoc where
    fromLoc = SrcLoc

-- | No location.
noLoc :: IsLocation a => a
noLoc = fromLoc NoLoc

-- | Located values have a location.
class Located a where
    locOf :: a -> Loc

    -- | Combine the locations of a finite list. The default implementation
    -- uses a strict left fold through the @Loc@ monoid, retaining only the
    -- accumulated span as it traverses the list. Instances may override this
    -- method to customize how lists are located.
    locOfList :: [a] -> Loc
    locOfList xs = mconcat (map locOf xs)

instance Located a => Located [a] where
    locOf = locOfList

instance Located a => Located (Maybe a) where
    locOf Nothing  = NoLoc
    locOf (Just x) = locOf x

instance Located Pos where
    locOf p = Loc p p

instance Located Loc where
    locOf = id

instance Located SrcLoc where
    locOf (SrcLoc loc) = loc

-- | Values that can be relocated
class Relocatable a where
    reloc :: Loc -> a -> a

-- | A value of type @L a@ is a value of type @a@ with an associated @Loc@, but
-- this location is ignored when performing comparisons.
data L a = L Loc a
  deriving (Functor, Data)

-- | Extract the value, discarding its location.
unLoc :: L a -> a
unLoc (L _ a) = a

instance Eq x => Eq (L x) where
    (L _ x) == (L _ y) = x == y

instance Ord x => Ord (L x) where
    compare (L _ x) (L _ y) = compare x y

instance Show x => Show (L x) where
    showsPrec d (L _ x) = showsPrec d x

instance Located (L a) where
    locOf (L loc _) = loc

instance Relocatable (L a) where
    reloc loc (L _ x) = L loc x

-- | Format a position in a human-readable way, returning an ordinary
-- 'String'.
displayPos :: Pos -> String
displayPos p = displayLoc (Loc p p)

-- | Format a position in a human-readable way.
displaySPos :: Pos -> ShowS
displaySPos p = displaySLoc (Loc p p)

-- | Format a location in a human-readable way, returning an ordinary
-- 'String'.
--
-- Same-file spans omit repeated filename and line information:
-- @input.hs:2:3@, @input.hs:2:3-5@, or @input.hs:2:3-4:5@.
-- Cross-file spans show both complete endpoints, as in @a.hs:2:3-b.hs:4:5@,
-- even when their line and column numbers match. Endpoints are displayed in
-- their stored order. Filenames are emitted verbatim and offsets are omitted.
-- 'NoLoc' is displayed as @<no location>@.
displayLoc :: Loc -> String
displayLoc loc = displaySLoc loc ""

-- | Format a location as described by 'displayLoc', prepending the result to
-- the supplied suffix.
displaySLoc :: Loc -> ShowS
displaySLoc NoLoc =
    showString "<no location>"

displaySLoc (Loc p1@(Pos src line1 col1 _) p2@(Pos src2 line2 col2 _))
  | src /= src2 =
      displaySPos p1 . dash . displaySPos p2
  | (line1, col1) == (line2, col2) =
      -- filename.txt:2:3
      showString src . colon . shows line1 . colon . shows col1
  | line1 == line2 =
      -- filename.txt:2:3-5
      showString src .
      colon . shows line1 .
      colon . shows col1 .
      dash  . shows col2
  | otherwise =
      -- filename.txt:2:3-4:5
      showString src .
      colon . shows line1 .
      colon . shows col1 .
      dash  . shows line2 .
      colon . shows col2
  where
    colon = (':' :)
    dash  = ('-' :)
