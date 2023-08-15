{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Nock (tar, hax, fas, cell, atom, Annotation (..), Noun, RawNoun (..)) where

import Control.DeepSeq (NFData)
import Data.Hashable
import Data.MemoTrie (memo)
import Debug.Trace (traceShowId)
import GHC.Generics (Generic)
import GHC.Word (Word64)
import Numeric.Natural

data Annotation = Annotation
  { hash :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

atom :: Natural -> Noun
atom nat = Atom nat Annotation {hash = hash nat}

cell :: Noun -> Noun -> Noun
cell lhs rhs =
  Cell
    lhs
    rhs
    Annotation
      { hash = hash ((annotation lhs).hash, (annotation rhs).hash)
      }

sig :: Noun
sig = atom 0

one :: Noun
one = atom 1

two :: Noun
two = atom 2

three :: Noun
three = atom 3

four :: Noun
four = atom 4

twoThree :: Noun
twoThree = cell two three

sigOne :: Noun
sigOne = cell sig one

annotation :: RawNoun ann -> ann
annotation (Atom _ ann) = ann
annotation (Cell _ _ ann) = ann

data RawNoun ann
  = Atom !Natural ann
  | Cell (RawNoun ann) (RawNoun ann) ann
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

instance Eq (RawNoun ann) where
  (Atom lhs _) == (Atom rhs _) = lhs == rhs
  (Cell lhslhs lhsrhs _) == (Cell rhslhs rhsrhs _) = lhslhs == rhslhs && lhsrhs == rhsrhs
  _ == _ = False

hashEq :: Noun -> Noun -> Bool
hashEq (Atom lhsNat lhsAnn) (Atom rhsNat rhsAnn) =
  lhsAnn.hash == rhsAnn.hash && lhsNat == rhsNat
hashEq (Cell lhslhs lhsrhs lhsAnn) (Cell rhslhs rhsrhs rhsAnn) =
  lhsAnn.hash == rhsAnn.hash && hashEq lhslhs rhslhs && hashEq lhsrhs rhsrhs
hashEq _ _ = False

type Noun = RawNoun Annotation

wut :: Noun -> Noun
wut Cell {} = sig
wut Atom {} = one

lus :: Noun -> Noun
lus ~(Atom nat _) = atom (nat + 1)

tis :: Noun -> Noun -> Noun
tis lhs rhs = if hashEq lhs rhs then sig else one

fas' :: Word64 -> Noun -> Noun
fas' n = case (traceShowId n) of
  1 -> id
  2 -> \ ~(Cell x _ _) -> x
  3 -> \ ~(Cell _ x _) -> x
  _ ->
    let (d, m) = divMod n 2
        next = fas d
     in if m == 0
          then \x ->
            ( case next x of
                ~(Cell y _ _) -> y
            )
          else \x ->
            ( case next x of
                ~(Cell _ y _) -> y
            )

fas :: Word64 -> Noun -> Noun
fas = memo fas'

hax :: Natural -> Noun -> Noun -> Noun
hax n b c =
  let (a, m) = divMod n 2
   in case n of
        1 -> b
        _ | m == 0 -> hax a (cell b (fas (fromIntegral $ a + a + 1) c)) c
        _ -> hax a (cell (fas (fromIntegral $ a + a) c) b) c

tar :: Noun -> Noun
tar ~(Cell subject ~(Cell a b _) _) = tar' a b subject

tar' :: Noun -> Noun -> Noun -> Noun
tar' b c subject = case b of
  Cell x y _ -> case c of
    ~(Cell l k _) -> cell (tar' x y subject) (tar' l k subject)
  Atom n _ -> case n of
    0 -> case c of
      ~(Atom nat _) -> fas (fromIntegral nat) subject
    1 -> c
    3 -> case c of
      ~(Cell l k _) -> wut $ tar' l k subject
    4 -> case c of
      ~(Cell l k _) -> lus $ tar' l k subject
    _ -> case c of
      ~(Cell x ~(Cell h j _) _) -> case n of
        2 -> case (tar' h j subject, x) of
          ~(~(Cell u v _), ~(Cell l k _)) -> tar' u v (tar' l k subject)
        5 -> case x of
          ~(Cell l k _) -> tis (tar' h j subject) (tar' l k subject)
        7 -> case x of
          ~(Cell l k _) -> tar' h j (tar' l k subject)
        8 -> case x of
          ~(Cell l k _) -> tar' h j (cell (tar' l k subject) subject)
        9 -> tar' two (cell sigOne (cell sig x)) (tar' h j subject)
        6 -> case tar' sig (tar' sig (tar' four (cell four x) subject) twoThree) (cell h j) of
          ~(Cell u v _) -> tar' u v subject
        10 -> case x of
          ~(Cell ~(Atom b' _) ~(Cell u v _) _) -> hax b' (tar' u v subject) (tar' h j subject)
        11 -> case x of
          Cell _ ~(Cell u v _) _ -> tar' sig three (cell (tar' u v subject) (tar' h j subject))
          Atom _ _ -> tar' h j subject
        u -> error $ show u
