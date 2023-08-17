{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Nock (tar, hax, fas, cell, atom, Annotation (..), Noun, RawNoun (..)) where

import Control.DeepSeq (NFData, force)
import Data.Bits
import Data.Hashable
import GHC.Base
import GHC.Conc (par)
import GHC.Generics (Generic)
import GHC.Num

newtype Annotation = Annotation
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
  (Cell lhslhs lhsrhs _) == (Cell rhslhs rhsrhs _) =
    let rhsRes = lhsrhs == rhsrhs
     in (force rhsRes `par` lhslhs == rhslhs) && rhsRes
  _ == _ = False

hashEq :: Noun -> Noun -> Bool
hashEq (Atom lhsNat lhsAnn) (Atom rhsNat rhsAnn) =
  let normalEq = lhsNat == rhsNat
   in (force normalEq `par` lhsAnn.hash == rhsAnn.hash) && normalEq
hashEq (Cell lhslhs lhsrhs lhsAnn) (Cell rhslhs rhsrhs rhsAnn) =
  let normalEq = hashEq lhslhs rhslhs && hashEq lhsrhs rhsrhs
   in (force normalEq `par` lhsAnn.hash == rhsAnn.hash) && normalEq
hashEq _ _ = False

type Noun = RawNoun Annotation

wut :: Noun -> Noun
wut Cell {} = sig
wut Atom {} = one

lus :: Noun -> Noun
lus ~(Atom nat _) = atom (nat + 1)

tis :: Noun -> Noun -> Noun
tis lhs rhs = if hashEq lhs rhs then sig else one

fasWord :: Word# -> Noun -> Noun
fasWord lhs rhs =
  case lhs of
    1## -> rhs
    2## -> case rhs of
      ~(Cell lhs' _ _) -> lhs'
    3## -> case rhs of
      ~(Cell _ rhs' _) -> rhs'
    n ->
      let rest = fasWord (uncheckedShiftRL# n 1#) rhs
       in case and# 1## n of
            1## -> fasWord 3## rest
            _ -> fasWord 2## rest

fas :: Natural -> Noun -> Noun
fas (NS w) rhs = fasWord w rhs
fas n rhs =
  let rest = fas (unsafeShiftR n 1) rhs
   in if testBit n 0
        then fas 3 rest
        else fas 2 rest

haxWord :: Word# -> Noun -> Noun -> Noun
haxWord 1## newValue _ = newValue
haxWord place newValue ~(Cell lhs rhs _) =
  let a = uncheckedShiftRL# place 1#
   in case and# place 1## of
        1## -> cell (haxWord a newValue lhs) rhs
        _ -> cell lhs (haxWord a newValue rhs)

hax :: Natural -> Noun -> Noun -> Noun
hax (NS w) newValue rhs = haxWord w newValue rhs
hax place newValue ~(Cell lhs rhs _) =
  let a = unsafeShiftR place 1
   in if testBit place 0
        then cell (hax a newValue lhs) rhs
        else cell lhs (hax a newValue rhs)

tar :: Noun -> Noun
tar ~(Cell subject ~(Cell a b _) _) = tar' a b subject

tar' :: Noun -> Noun -> Noun -> Noun
tar' b c subject = case b of
  Cell x y _ -> case c of
    ~(Cell l k _) -> cell (tar' x y subject) (tar' l k subject)
  Atom (NB _) _ -> undefined
  Atom (NS n) _ -> case n of
    0## -> case c of
      ~(Atom nat _) -> fas nat subject
    1## -> c
    3## -> case c of
      ~(Cell l k _) -> wut $ tar' l k subject
    4## -> case c of
      ~(Cell l k _) -> lus $ tar' l k subject
    _ -> case c of
      ~(Cell x ~(Cell h j _) _) -> case n of
        2## -> case (tar' h j subject, x) of
          ~(~(Cell u v _), ~(Cell l k _)) -> tar' u v (tar' l k subject)
        5## -> case x of
          ~(Cell l k _) -> tis (tar' h j subject) (tar' l k subject)
        7## -> case x of
          ~(Cell l k _) -> tar' h j (tar' l k subject)
        8## -> case x of
          ~(Cell l k _) -> tar' h j (cell (tar' l k subject) subject)
        9## -> tar' two (cell sigOne (cell sig x)) (tar' h j subject)
        6## -> case tar' sig (tar' sig (tar' four (cell four x) subject) twoThree) (cell h j) of
          ~(Cell u v _) -> tar' u v subject
        10## -> case x of
          ~(Cell ~(Atom b' _) ~(Cell u v _) _) -> hax b' (tar' u v subject) (tar' h j subject)
        11## -> case x of
          Cell _ ~(Cell u v _) _ -> tar' sig three (cell (tar' u v subject) (tar' h j subject))
          Atom _ _ -> tar' h j subject
        _ -> undefined
