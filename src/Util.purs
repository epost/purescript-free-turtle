module Util where

infixl 1 >>

(>>) :: forall m a b. (Monad m) => m a -> m b -> m b
(>>) ma mb = ma >>= (\_ -> mb)

