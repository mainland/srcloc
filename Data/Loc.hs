-- Copyright (c) 2006-2012
--         The President and Fellows of Harvard College.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Loc
-- Copyright   :  (c) Harvard University 2006-2012
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

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

    mergeLoc,
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
import Data.Generics (Data(..),Typeable(..))
#endif
import Data.List (foldl')

-- | Position type.
data Pos = -- | Source file name, line, column, and character offset
           Pos !String
               {-# UNPACK #-} !Int
               {-# UNPACK #-} !Int
               {-# UNPACK #-} !Int
#ifdef __GLASGOW_HASKELL__
  deriving (Eq, Show, Data, Typeable)
#else
  deriving (Eq, Show)
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
  deriving (Eq, Show, Data, Typeable)
#else
  deriving (Eq, Show)
#endif

-- | Starting position of the location.
locStart :: Loc -> Loc
locStart  NoLoc      = NoLoc
locStart  (Loc p _)  = Loc p p

-- | Ending position of the location.
locEnd :: Loc -> Loc
locEnd  NoLoc      = NoLoc
locEnd  (Loc _ p)  = Loc p p

-- | Calculate a 'Loc' with (minimal) span that includes both 'Loc' values.
mergeLoc :: Loc -> Loc -> Loc
mergeLoc  NoLoc        l            = l
mergeLoc  l            NoLoc        = l
mergeLoc  (Loc b1 e1)  (Loc b2 e2)  = Loc (min b1 b2) (max e1 e2)

-- | Merge the locations of two 'Located' values.
(<-->) :: (Located a, Located b) => a -> b -> Loc
x <--> y = mergeLoc (locOf x) (locOf y)

infixl 6 <-->

-- | Source location type. Source location are all equal, which allows AST nodes
-- to be compared modulo location information.
newtype SrcLoc = SrcLoc Loc
  deriving (Data, Typeable)

instance Eq SrcLoc where
    _ == _ = True

instance Ord SrcLoc where
    compare _ _ = EQ

-- | The 'SrcLoc' of a 'Located' value.
srclocOf :: Located a => a -> SrcLoc
srclocOf = fromLoc . locOf

-- | A 'SrcLoc' with (minimal) span that includes two 'Located' values.
srcspan :: (Located a, Located b) => a -> b -> SrcLoc
x `srcspan` y = SrcLoc (mergeLoc (locOf x) (locOf y))

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
    locOfList xs = foldl' mergeLoc NoLoc (map locOf xs)

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
