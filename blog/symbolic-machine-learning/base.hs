mempty :: a
mappend :: a -> a -> a
mconcat :: [a] -> a
fmap :: (a -> b) -> f a -> f b
(<$) :: a -> f b -> f a
pure :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b
(*>) :: f a -> f b -> f b
(<*) :: f a -> f b -> f a
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
join :: (Monad m) => m (m a) -> m a
(>>=) :: forall a b. m a -> (a -> m b) -> m b
(>>) :: forall a b. m a -> m b -> m b
return :: a -> m a
fail :: String -> m a
(=<<) :: Monad m => (a -> m b) -> m a -> m b
when :: (Applicative f) => Bool -> f () -> f ()
sequence :: Monad m => [m a] -> m [a]
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
liftM :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM2 :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM3 :: (Monad m) => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM4 :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
liftM5 :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
ap :: (Monad m) => m (a -> b) -> m a -> m b
empty :: f a
(<|>) :: f a -> f a -> f a
some :: f a -> f [a]
many :: f a -> f [a]
mzero :: m a
mplus :: m a -> m a -> m a
foldr :: (a -> b -> b) -> b -> [a] -> b
build :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
augment :: forall a. (forall b. (a->b->b) -> b -> b) -> [a] -> [a]
map :: (a -> b) -> [a] -> [b]
mapFB :: (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
(++) :: [a] -> [a] -> [a]
otherwise :: Bool
unsafeChr :: Int -> Char
ord :: Char -> Int
eqString :: String -> String -> Bool
id :: a -> a
assert :: Bool -> a -> a
breakpoint :: a -> a
breakpointCond :: Bool -> a -> a
const :: a -> b -> a
(.) :: (b -> c) -> (a -> b) -> a -> c
flip :: (a -> b -> c) -> b -> a -> c
($) :: (a -> b) -> a -> b
($!) :: (a -> b) -> a -> b
until :: (a -> Bool) -> (a -> a) -> a -> a
asTypeOf :: a -> a -> a
returnIO :: a -> IO a
bindIO :: IO a -> (a -> IO b) -> IO b
thenIO :: IO a -> IO b -> IO b
quotRemInt :: Int -> Int -> (Int, Int)
divModInt :: Int -> Int -> (Int, Int)