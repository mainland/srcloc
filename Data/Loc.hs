-- |
-- Module      :  Data.Loc
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2012
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Loc (
    Pos(..),
    posFile,
    posLine,
    posCol,
    posCoff,
    startPos,
    linePos,
    advancePos,

    Loc(..),
    locStart,
    locEnd,

    (<-->),

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

#ifdef __GLASGOW_HASKELL__
import Data.Data (Data(..))
import Data.Typeable (Typeable(..))
#endif
import Data.List (foldl')
import Data.Monoid (Monoid(..))

-- | Position type.
data Pos = -- | Source file name, line, column, and character offset
           Pos !String
               {-# UNPACK #-} !Int
               {-# UNPACK #-} !Int
               {-# UNPACK #-} !Int
#ifdef __GLASGOW_HASKELL__
  deriving (Eq, Read, Show, Data, Typeable)
#else
  deriving (Eq, Read, Show)
#endif

instance Ord Pos where
    compare (Pos f1 l1 c1 _) (Pos f2 l2 c2 _) =
        compare (f1, l1, c1) (f2, l2, c2)

-- | Position file.
posFile :: Pos -> String
posFile (Pos f _ _ _) = f

-- | Position line.
posLine :: Pos -> Int
posLine (Pos _ l _ _) = l

-- | Position column.
posCol :: Pos -> Int
posCol (Pos _ _ c _) = c

-- | Position character offset.
posCoff :: Pos -> Int
posCoff (Pos _ _ _ coff) = coff

startLine :: Int
startLine = 1

startCol :: Int
startCol = 0

startCoff :: Int
startCoff = 0

startPos :: String -> Pos
startPos f = Pos f startLine startCol startCoff

linePos :: String -> Int -> Pos
linePos f l = Pos f l startCol startCoff

advancePos :: Pos -> Char -> Pos
advancePos (Pos f l _ coff) '\n' = Pos f (l+1) startCol (coff + 1)
advancePos (Pos f l c coff) _    = Pos f l     (c + 1)  (coff + 1)

-- | Location type, consisting of a beginning position and an end position.
data Loc =  NoLoc
         |  -- | Beginning and end positions
            Loc  {-# UNPACK #-} !Pos
                 {-# UNPACK #-} !Pos
#ifdef __GLASGOW_HASKELL__
  deriving (Eq, Read, Show, Data, Typeable)
#else
  deriving (Eq, Read, Show)
#endif

-- | Starting position of the location.
locStart :: Loc -> Loc
locStart  NoLoc      = NoLoc
locStart  (Loc p _)  = Loc p p

-- | Ending position of the location.
locEnd :: Loc -> Loc
locEnd  NoLoc      = NoLoc
locEnd  (Loc _ p)  = Loc p p

instance Monoid Loc where
    mempty = NoLoc

    NoLoc     `mappend` l         = l
    l         `mappend` NoLoc     = l
    Loc b1 e1 `mappend` Loc b2 e2 = Loc (min b1 b2) (max e1 e2)

-- | Merge the locations of two 'Located' values.
(<-->) :: (Located a, Located b) => a -> b -> Loc
x <--> y = locOf x `mappend` locOf y

infixl 6 <-->

-- | Source location type. Source location are all equal, which allows AST nodes
-- to be compared modulo location information.
newtype SrcLoc = SrcLoc Loc
  deriving (Data, Typeable)

instance Eq SrcLoc where
    _ == _ = True

instance Ord SrcLoc where
    compare _ _ = EQ

instance Show SrcLoc where
    showsPrec _ _ = showString "noLoc"

instance Read SrcLoc where
    readsPrec p s =
        readParen False
          (\s -> [(SrcLoc NoLoc, s') |
                  ("noLoc", s') <- lex s])
          s
        ++
        readParen (p > app_prec)
          (\s -> [(SrcLoc l, s'') |
                  ("SrcLoc", s') <- lex s,
                  (l, s'') <- readsPrec (app_prec+1) s'])
          s
      where
        app_prec = 10

-- | The 'SrcLoc' of a 'Located' value.
srclocOf :: Located a => a -> SrcLoc
srclocOf = fromLoc . locOf

-- | A 'SrcLoc' with (minimal) span that includes two 'Located' values.
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

    locOfList :: [a] -> Loc
    locOfList xs = mconcat (map locOf xs)

instance Located a => Located [a] where
    locOf = locOfList

instance Located a => Located (Maybe a) where
    locOf Nothing   = NoLoc
    locOf (Just x)  = locOf x

instance Located Pos where
    locOf p = Loc p p

instance Located Loc where
    locOf = id

instance Located SrcLoc where
    locOf (SrcLoc loc) = loc

-- | Values that can be relocated
class Relocatable a where
    reloc :: Loc -> a -> a

-- | An 'L a' is an 'a' with an associated 'Loc', but this location is ignored
-- when performing comparisons.
data L a = L Loc a

unLoc :: L a -> a
unLoc (L _ a) = a

instance Eq x => Eq (L x) where
    (L _ x) == (L _ y) = x == y

instance Ord x => Ord (L x) where
    compare (L _ x) (L _ y) = compare x y

instance Show x => Show (L x) where
    show (L _ x) = show x

instance Located (L a) where
    locOf (L loc _) = loc

instance Relocatable (L a) where
    reloc loc (L _ x) = L loc x
