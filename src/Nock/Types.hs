module Nock.Types (pretty, hashEq, Noun, RawNoun (..), Annotation (..), atom, cell) where

import Control.DeepSeq
import Data.Hashable
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Builder.Int qualified as TLB
import GHC.Conc
import GHC.Generics
import GHC.Natural

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

annotation :: RawNoun ann -> ann
annotation (Atom _ ann) = ann
annotation (Cell _ _ ann) = ann

data RawNoun ann
  = Atom !Natural ann
  | Cell !(RawNoun ann) !(RawNoun ann) ann
  deriving stock (Generic)
  deriving anyclass (NFData)

instance Show (RawNoun ann) where
  show = TL.unpack . pretty

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

pretty :: RawNoun ann -> TL.Text
pretty noun = TLB.toLazyText $ pretty' noun False

pretty' :: RawNoun ann -> Bool -> TLB.Builder
pretty' (Atom a _) _ = TLB.decimal a
pretty' (Cell lhs rhs _) True = pretty' lhs False <> " " <> pretty' rhs True
pretty' (Cell lhs rhs _) False = "[" <> pretty' lhs False <> " " <> pretty' rhs True <> "]"
