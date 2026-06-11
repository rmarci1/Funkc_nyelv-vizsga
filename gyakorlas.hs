import Data.Functor.Classes
import Control.Monad
import Control.Monad.Except
import Data.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
--Functorok

data Tree a = Leaf (Maybe a) | Node (Tree a) a (Tree a) deriving (Eq, Show)
instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Leaf Nothing) = Leaf Nothing
    fmap f (Leaf (Just a)) = Leaf $ Just $ f a
    fmap f (Node tr a tr2) = Node (fmap f tr) (f a) (fmap f tr2)

data Gofri f a
    = MkGofri (f a) (f (Gofri f a))
deriving instance (Eq a, Eq1 f) => Eq (Gofri f a)
deriving instance (Show a, Show1 f) => Show (Gofri f a)

instance Functor f => Functor (Gofri f) where 
    fmap :: (a -> b) -> Gofri f a -> Gofri f b
    fmap f (MkGofri fa ffa) = MkGofri (fmap f fa) (fmap (\g -> fmap f g) ffa)

data CrazyType3 a b
    = CrazyCon1 a b a 
    | CrazyCon2 (CrazyType3 a b) [b] [a]
    | CrazyCon3 (CrazyType3 Int b) (CrazyType3 a a) [[b]]
    deriving (Eq, Show)

instance Functor (CrazyType3 fixed) where
    fmap :: (a -> b) -> CrazyType3 fixed a -> CrazyType3 fixed b
    fmap f (CrazyCon1 fixed a fixed2) = CrazyCon1 fixed (f a) fixed2   
    fmap f (CrazyCon2 a xs fixed) = CrazyCon2 (fmap f a) (fmap f xs) fixed
    fmap f (CrazyCon3 a fixed xs) = CrazyCon3 (fmap f a) fixed (fmap (\a -> fmap f a) xs)


--Foldable

-- data Tree a = Leaf (Maybe a) | Node (Tree a) a (Tree a) deriving (Eq, Show)
instance Foldable Tree where
    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr f b (Leaf Nothing) = b
    foldr f b (Leaf (Just a)) = f a b
    foldr f b (Node tr a tr2) = foldr f (f a (foldr f b tr2)) tr
    
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap f (Leaf Nothing) = mempty
    foldMap f (Leaf (Just a)) = f a
    foldMap f (Node tr a tr2) = foldMap f tr <> f a <> foldMap f tr2

--instance Functor f => Functor (Gofri f) where 
--    fmap :: (a -> b) -> Gofri f a -> Gofri f b
--    fmap f (MkGofri fa ffa) = MkGofri (fmap f fa) (fmap (\g -> fmap f g) ffa)

instance (Foldable f) => Foldable (Gofri f) where
    foldr :: (a -> b -> b) -> b -> Gofri f a -> b
    foldr f b (MkGofri fa ffa) = foldr f (foldr (\x y -> foldr f y x) b ffa) fa 

    foldMap :: Monoid m => (a -> m) -> Gofri f a -> m
    foldMap f (MkGofri fa ffa) = foldMap f fa <> foldMap (\x -> foldMap f x) ffa

--data CrazyType3 a b
--   = CrazyCon1 a b a 
--   | CrazyCon2 (CrazyType3 a b) [b] [a]
--   | CrazyCon3 (CrazyType3 Int b) (CrazyType3 a a) [[b]]
--   deriving (Eq, Show)

instance Foldable (CrazyType3 fixed) where
    foldr :: (a -> b -> b) -> b -> CrazyType3 f a -> b
    foldr f b (CrazyCon1 _ a _) = f a b
    foldr f b (CrazyCon2 a xs _) = foldr f (foldr f b xs) a
    foldr f b (CrazyCon3 a _ xs) = foldr f (foldr (\x y -> foldr f y x) b xs) a 

    foldMap :: Monoid m => (a -> m) -> CrazyType3 f a -> m
    foldMap f (CrazyCon1 _ a _) = f a
    foldMap f (CrazyCon2 a xs _) = foldMap f a <> foldMap f xs
    foldMap f (CrazyCon3 a _ xs) = foldMap f a <> foldMap (\x -> foldMap f x) xs



{-                 
                 ---------    Jegy / Zöld    ----------
             /---|       |------------------>|        |
 Tol / Piros |   | Zárva |    Tol / Zöld     | Nyitva |
             \-->|       |<------------------|        |
                 ---------                   ----------
                   |  ^                          | 
                   |  |       ----------         | Tilt / Piros
                   |  |       |        |         |
      Tilt / Piros |  \-------| Tiltva |<--------/
                   |  Nyit /  |        |
                   |  Zöld    ----------
                   |            ^  | ^
                   |            |  | | Tilt / Piros
                   |            |  | |
                   \------------/  \-/ 
    -}
data MachineState = Open | Closed | Locked
  deriving (Eq, Show)

data LightColour = Red | Yellow | Green
  deriving (Eq, Show)

push, ticket, lock, open :: State MachineState LightColour
push = do
    state <- get
    case state of
        Open -> do 
            put Closed
            return Green
        Closed -> return Red
        _ -> return Yellow
ticket = do
    state <- get
    case state of
        Closed -> do 
            put Open
            return Green
        _ -> return Yellow  
lock = do
    state <- get
    case state of
        Open -> put Locked
        Closed -> put Locked
    return Red
open = do
    state <- get
    case state of 
        Locked -> do
            put Closed
            return Green
        _ -> return Yellow 

pistike :: State MachineState [LightColour]
pistike = do
    state <- get
    l1 <- push 
    l2 <- push 
    l3 <- ticket 
    l4 <- push
    l5 <- open
    l6 <- ticket
    l7 <- push
    return [l1,l2,l3,l4,l5,l6,l7]

data Tree2 a = Leaf2 a | Node2 (Tree2 a) a (Tree2 a) deriving (Show, Eq)

labelTree :: Num b => Tree2 a -> State b (Tree2 (b, a))
labelTree tr = do
    ind <- get
    case tr of
        Leaf2 a -> do
            put $ ind+1
            return (Leaf2 (ind,a))
        Node2 tr2 a tr3 -> do
            let x = (ind,a)
            put (ind+1) 
            res1 <- labelTree tr2
            res2 <- labelTree tr3
            return $ Node2 res1 x res2

data SequenceError = LoopDetected Int Int deriving (Show,Eq)

nonLoopingSequence :: Eq a => (a -> a) -> a -> Int -> Except SequenceError [a]
nonLoopingSequence f start length = helpLooping f start 0 length [] 
    where 
        helpLooping f curr ind length xs = do 
            if ((ind+1) == length) then return $ reverse $ curr : xs 
            else case elemIndex (f curr) (reverse xs) of 
                Nothing -> helpLooping f (f curr) (ind+1) length (curr : xs) 
                Just a -> throwError $ LoopDetected (ind+1) (ind+1-a)

-- data Tree a = Leaf (Maybe a) | Node (Tree a) a (Tree a) deriving (Eq, Show, Foldable, Functor)

instance Traversable Tree where
    traverse :: (Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
    traverse f (Leaf Nothing) = pure $ Leaf Nothing
    traverse f (Leaf (Just a)) = Leaf <$> Just <$> f a
    traverse f (Node tr a tr2) = Node <$> traverse f tr <*> f a <*> traverse f tr2

{- data Gofri f a
    = MkGofri (f a) (f (Gofri f a))
    deriving (Foldable, Functor)

deriving instance (Eq a, Eq1 f) => Eq (Gofri f a)
deriving instance (Show a, Show1 f) => Show (Gofri f a) -}

instance (Traversable g) => Traversable (Gofri g) where
    traverse :: (Traversable g, Applicative f) => (a -> f b) -> Gofri g a -> f (Gofri g b)
    traverse f (MkGofri fa ffa) = MkGofri <$> traverse f fa <*> traverse (\a -> traverse f a) ffa

{- data CrazyType3 a b
    = CrazyCon1 a b a 
    | CrazyCon2 (CrazyType3 a b) [b] [a]
    | CrazyCon3 (CrazyType3 Int b) (CrazyType3 a a) [[b]]
    deriving (Eq, Show, Functor, Foldable) -}

instance Traversable (CrazyType3 fixed) where
    traverse :: (Applicative f) => (a -> f b) -> CrazyType3 fixed a -> f (CrazyType3 fixed b)
    traverse f (CrazyCon1 b a c) = CrazyCon1 <$> pure b <*> f a <*> pure c
    traverse f (CrazyCon2 a xs l) = CrazyCon2 <$> traverse f a <*> traverse f xs <*> pure l 
    traverse f (CrazyCon3 a l xs) = CrazyCon3 <$> traverse f a <*> pure l <*> traverse (\x -> traverse f x) xs

