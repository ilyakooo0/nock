{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Nock (tar, hax, fas, cell, atom, Annotation (..), Noun, RawNoun (..)) where

import Data.Hashable
import Debug.Trace (trace, traceShowId)
import GHC.Generics (Generic)
import Numeric.Natural
import Test.QuickCheck.Arbitrary.Generic

data Annotation = Annotation
  { hash :: Int
  }
  deriving stock (Show)

zero :: Natural
zero = 0

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

annotation :: RawNoun ann -> ann
annotation (Atom _ ann) = ann
annotation (Cell _ _ ann) = ann

data RawNoun ann
  = Atom !Natural ann
  | Cell (RawNoun ann) (RawNoun ann) ann
  deriving stock (Generic, Show)

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

tis :: Noun -> Noun
tis ~(Cell lhs rhs _) = if hashEq lhs rhs then sig else one

fas :: Noun -> Noun
fas ~(Cell ~(Atom lhs _) rhs _) =
  case lhs of
    1 -> rhs
    2 -> case rhs of
      ~(Cell lhs' _ _) -> lhs'
    3 -> case rhs of
      ~(Cell _ rhs' _) -> rhs'
    n
      | n > 0 ->
          let (d, m) = divMod n 2
           in if m == 0
                then fas (cell (atom 2) (fas (cell (atom d) rhs)))
                else fas (cell (atom 3) (fas (cell (atom d) rhs)))
    _ -> undefined

hax :: Noun -> Noun
hax ~(Cell ~(Atom n _) ~(Cell a b _) _) =
  let (d, m) = divMod n 2
   in case n of
        1 -> a
        _ | m == 0 -> hax (cell (atom d) (cell (cell a (fas (cell (atom $ n + 1) b))) b))
        _ -> hax (cell (atom d) (cell (cell (fas (cell (atom $ n - 1) b)) a) b))

tar :: Noun -> Noun
tar ~(Cell a ~(Cell b c _) _) = case b of
  Cell x y _ -> cell (tar (cell a (cell x y))) (tar (cell a c))
  Atom n _ -> case n of
    0 -> fas $ cell c a
    1 -> c
    3 -> wut $ tar $ cell a c
    4 -> lus $ tar $ cell a c
    _ -> case c of
      ~(Cell x y _) -> case n of
        2 -> tar $ cell (tar $ cell a x) (tar $ cell a y)
        5 -> tis $ cell (tar $ cell a x) (tar $ cell a y)
        7 -> tar $ cell (tar $ cell a x) y
        8 -> tar $ cell (cell (tar $ cell a x) a) y
        9 -> tar $ cell (tar $ cell a y) $ cell (atom 2) (cell (cell sig one) (cell sig x))
        6 -> case y of
          ~(Cell l k _) -> tar $ cell a $ tar $ cell (cell l k) $ cell sig $ tar $ cell (cell (atom 2) (atom 3)) $ cell sig $ tar $ cell a (cell (atom 4) (cell (atom 4) x))
        10 -> case x of
          ~(Cell l k _) -> hax $ cell l $ cell (tar (cell a k)) $ tar $ cell a y
        11 -> case x of
          Cell l k _ -> tar $ cell (cell (tar $ cell a k) (tar $ cell a y)) $ cell sig (atom 3)
          Atom _ _ -> tar $ cell a y
        u -> error $ show u
