module Extended.Language.Haskell.TH (
    module X
  , debugQ
  , arrowType
  , tupleType
  , simpleClause
  , simpleFunDec
    ) where

import           Language.Haskell.TH as X

debugQ :: (Show a, Ppr a) => Q a -> IO ()
debugQ q = do
    ret <- runQ q
    print ret
    putStrLn $ pprint ret

-- | [a, b, c] ==> a -> b -> c
arrowType :: [Type] -> Type
arrowType = foldr1 $ \t1 -> AppT (AppT ArrowT t1)

-- | [a, b, c] ==> (a, b, c)
tupleType :: [Type] -> Type
tupleType ts = foldl AppT (TupleT $ length ts) ts

-- | f :: Type
--   f = exp
simpleFunDec :: Name -> Type -> Q Exp -> Q [Dec]
simpleFunDec n t qe = do
    f <- funD n [simpleClause qe]
    return [SigD n t, f]

simpleClause :: ExpQ -> ClauseQ
simpleClause expq = clause [] (normalB expq) []
