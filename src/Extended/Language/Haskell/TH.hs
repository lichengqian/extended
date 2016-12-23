{-# LANGUAGE TemplateHaskell #-}
module Extended.Language.Haskell.TH (
    module X
  , debugQ
  , arrowType
  , tupleType
  , simpleClause
  , simpleFunDec
  , liftQ
  , liftClassQ
  , liftReaderT
    ) where

import           Control.Monad
import           Control.Monad.Reader
import qualified Control.Monad.Trans  as Trans
import           Language.Haskell.TH  as X

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

fromArrowT (AppT (AppT ArrowT t1) t2) = t1 : fromArrowT t2
fromArrowT t                          = [t]

-- f a = lift (f a)
liftQ :: Name -> Type -> DecQ
liftQ n t = do
    args <- replicateM nargs $ newName "p"
    funD n [clause (fmap varP args) (body args) []]
    where
        body args = normalB (appE [| Trans.lift |] $ appsE (varE n : fmap varE args))
        nargs = length (fromArrowT t) - 1

liftClassQ :: Name -> DecsQ
liftClassQ n = do
    ClassI (ClassD _ _ _ _ funs) _ <- reify n
    concat <$> mapM f funs
    where
        f (SigD n t) = do
            d <- liftQ n t
            i <- pragInlD n Inline FunLike AllPhases
            return [i, d]
        f _ = return []

-- [ (AppT (ConT GHC.Show.Show) (AppT (AppT (ConT Control.Monad.Trans.Reader.ReaderT) (VarT a_1)) (VarT m_0))) []]
liftReaderT :: Name -> DecsQ
liftReaderT n = do
    let m = mkName "m"
        a = varT $ mkName "a"
        ctx = [AppT (ConT n) (VarT m)]
        readerT = [t| $(conT n) (ReaderT $a $(varT m)) |]
    d <- InstanceD Nothing ctx <$> readerT <*> liftClassQ n
    return [d]
