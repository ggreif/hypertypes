{-# language TypeSynonymInstances, FlexibleInstances, LambdaCase #-}

newtype Hy a b = Hy { invoke :: Hy b a -> b }

data Ty' = Top | Int | Bool | Err | Ty' `App` Ty' | Arr
  deriving (Eq, Show)
type Ty = Hy Ty' Ty'


instance Monoid Ty where
  mempty = Hy (\(Hy f) -> f mempty)

instance Semigroup Ty where
  t <> u = Hy (\_ -> t `invoke` u)

ty :: Ty' -> Ty
ty accept = Hy $
   \(Hy f) -> case f (Hy (\_ -> accept)) of
                Top -> accept
                t | t == accept -> accept
                _ -> Err

int, func :: Ty
[int, func] = ty <$> [Int, Arr `App` Int `App` Int]

unif :: Ty -> Ty -> Ty
unif a b = a <> b <> a <> a

obtain :: Ty -> Ty'
obtain (Hy f) = f (Hy (\_ -> Top))

main = print (obtain (func <> func))
