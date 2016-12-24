{-# LANGUAGE TemplateHaskell #-}
module Test where

import           Control.Monad.Reader
import           Extended.Language.Haskell.TH

class (Monad m) => MonadExt m where
    extLog :: Int -> m ()

liftReaderT ''MonadExt

{-
======>
    instance MonadExt m => MonadExt (ReaderT a m) where
      {-# INLINE extLog #-}
      extLog p_aFY7 = lift (extLog p_aFY7)
-}

-- template haskel 非常强大，可以任意生成重复代码！
forM [1..10] $ \i ->
    valD (varP $ mkName $ "f" ++ show i) (normalB (litE (IntegerL i))) []

-- 可以直接使用do表达式！！！可以实现任意复杂的代码生成逻辑
do
    let f i = valD (varP $ mkName $ "f" ++ show i) (normalB (litE (IntegerL i))) []
    mapM f [20..30]
{-
======>
    f1 = 1
    f2 = 2
    f3 = 3
    f4 = 4
    f5 = 5
    f6 = 6
    f7 = 7
    f8 = 8
    f9 = 9
    f10 = 10
-}
