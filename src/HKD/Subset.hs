{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module HKD.Subset where

import Data.Void
import Data.Kind

newtype Age = Age Int
newtype Name = Name String
newtype Country = Country String

-- type family X t where
--   X Age = Age
--   X Name = Name
--   X _ = Void

data family X a

data instance X Age = X Age
data instance X Name = Y
data instance X Country = Z


data User f =
    User
    { age     :: f Age
    , name    :: f Name
    , country :: f Country
    }

-- x :: User X
-- x = User{ age=X (Age 5)
--         , name= Y
--         , country= Z
--         }

newtype Tagged t a = Tagged {untag :: a}

type family Contains (xs :: [k]) t where
  Contains '[] _ = 'False
  Contains (x ':xs) x = 'True
  Contains (x ':xs) t = Contains xs t

data R xs t where
  Has :: (Contains xs t ~ 'True) => Val t -> R xs t
  Hasn't :: (Contains xs t ~ 'False) => Void -> R xs t

type family Val x = val | val -> x where
  Val Lit' = Int
  Val Var' = String
  Val Add' = (Int, Int)

data Ts = Lit' |  Var' | Add'
data AST xs = Literal (R xs Lit')  | Var (R xs Var') | Add (R xs Add')

handleAST :: AST [Lit', Add'] -> Int
handleAST (Literal (Has x)) = x
handleAST (Add (Has (a, b))) = a + b
handleAST (Var (Hasn't v)) = absurd v

-- data family NoVars x t
-- data instance NoVars Var' = V Void
-- data instance NoVars Lit' = L Lit'
-- data instance NoVars BinOp' = B BinOp'
-- newtype Var' = Var' String
-- newtype Lit' = Lit' Int
-- data BinOp' xs = BinOp' Op (AST xs) (AST xs)
-- data AST f = Literal (f Lit') | Var (f Var') | BinOp (f BinOp')

-- interpAST :: AST NoVars -> Int
-- interpAST (Literal (L (Lit' n))) = n
-- interpAST (Var (V v)) = absurd v
-- interpAST (BinOp (O Add)) =

