{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Nock (tar, hax, fas, cell, atom, Annotation (..), Noun, RawNoun (..)) where

import Control.DeepSeq (NFData)
import Data.Hashable
import GHC.Generics (Generic)
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

sigThree :: Noun
sigThree = cell sig three

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
hax ~(Cell ~(Atom n _) ~(Cell b c _) _) =
  let (a, m) = divMod n 2
   in case n of
        1 -> b
        _ | m == 0 -> hax (cell (atom a) (cell (cell b (fas (cell (atom $ a + a + 1) c))) c))
        _ -> hax (cell (atom a) (cell (cell (fas (cell (atom $ a + a) c)) b) c))

tar :: Noun -> Noun
tar ~(Cell subject x _) = tar' x subject

tar' :: Noun -> Noun -> Noun
tar' ~(Cell b c _) subject = case b of
  Cell x y _ -> cell (tar' (cell x y) subject) (tar' c subject)
  Atom n _ -> case n of
    0 -> fas $ cell c subject
    1 -> c
    3 -> wut $ tar' c subject
    4 -> lus $ tar' c subject
    _ -> case c of
      ~(Cell x y _) -> case n of
        2 -> tar' (tar' y subject) (tar' x subject)
        5 -> tis $ cell (tar' y subject) (tar' x subject)
        7 -> tar' y (tar' x subject)
        8 -> tar' y (cell (tar' x subject) subject)
        9 -> tar' (cell two (cell sigOne (cell sig x))) (tar' y subject)
        6 -> case y of
          ~(Cell c' d' _) -> tar' (tar' (cell sig $ tar' (cell sig $ tar' (cell four (cell four x)) subject) twoThree) (cell c' d')) subject
        10 -> case x of
          ~(Cell b' c' _) -> hax $ cell b' $ cell (tar' c' subject) $ tar' y subject
        11 -> case x of
          Cell _ c' _ -> tar' sigThree (cell (tar' c' subject) (tar' y subject))
          Atom _ _ -> tar' y subject
        u -> error $ show u
