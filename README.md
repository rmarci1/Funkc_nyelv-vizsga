# 🚀🚀🚀🚀🚀 Haskell ZH / Vizsga

## 🔗 Hasznos linkek

| Rész | Link |
|-----------|--------|
| Gyakorlatok | https://github.com/akaposi/ELTE-func-lang/tree/master/2025-26-2/3 |
| Hoogle | https://hoogle.haskell.org/ |
| Öcsisajt🧀 | (Bárcsak:D) |

---

# 🎨 Functor (`fmap`)

## Alapszabály

Ahol az `a` szerepel a típusparaméter helyén, ott kell alkalmazni az `f` függvényt.

```haskell
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
```

---

# 📦 Foldable / Monoid


## Példa

```haskell
data CrazyType3 a b
    = CrazyCon1 a b a 
    | CrazyCon2 (CrazyType3 a b) [b] [a]
    | CrazyCon3 (CrazyType3 Int b) (CrazyType3 a a) [[b]]
    deriving (Eq, Show)

instance Foldable (CrazyType3 fixed) where
    foldr :: (a -> b -> b) -> b -> CrazyType3 f a -> b
    foldr f b (CrazyCon1 _ a _) = f a b
    foldr f b (CrazyCon2 a xs _) = foldr f (foldr f b xs) a
    foldr f b (CrazyCon3 a _ xs) = foldr f (foldr (\x y -> foldr f y x) b xs) a 

    foldMap :: Monoid m => (a -> m) -> CrazyType3 f a -> m
    foldMap f (CrazyCon1 _ a _) = f a
    foldMap f (CrazyCon2 a xs _) = foldMap f a <> foldMap f xs
    foldMap f (CrazyCon3 a _ xs) = foldMap f a <> foldMap (\x -> foldMap f x) xs
```

---

# 🧙 Monad

## Szükséges importok

```haskell
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except

import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.Error.Class
import Control.Monad.IO.Class
```

---

## 📋 Monad műveletek
```haskell
+---------------------------+---------------------------------------------+-------------------------------------------------------------+
| Monad                     | Primitive Function #1                       | Primitive Function #2                                       |
+---------------------------+---------------------------------------------+-------------------------------------------------------------+
| State s a                 | get :: State s s                            | put :: s -> State s ()                                      |
+---------------------------+---------------------------------------------+-------------------------------------------------------------+
| Monoid w => Writer w a    |                                             | tell :: w -> Writer w ()                                    |
+---------------------------+---------------------------------------------+-------------------------------------------------------------+
| Reader r a                | ask :: Reader r r                           | local :: (r -> r) -> Reader r a -> Reader r a               |
+---------------------------+---------------------------------------------+-------------------------------------------------------------+
| Except e a                | throwError :: e -> Except e a               | catchError :: Except e a -> (e -> Except e a) -> Except e a |
+---------------------------+---------------------------------------------+-------------------------------------------------------------+
```
---

## 🔄 Transformer megfeleltetés

| Normál | Transformer |
|----------|-------------|
| `State s a` | `StateT s m a` |
| `Reader r a` | `ReaderT r m a` |
| `Writer w a` | `WriterT w m a` |
| `Except e a` | `ExceptT e m a` |

```haskell
newtype State  s a = State  { runState  :: s -> (a,s) }
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

newtype Reader  r a = Reader  { runReader  :: r -> a }
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

newtype Writer  w a = Writer  { runWriter  :: (a,w) }
newtype WriterT w m a = WriterT { runWriterT :: m (a,w) }

newtype Except  e a = Except  { runExcept  :: Either e a }
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
```

---

## 🎯 Monad stack példa

```haskell
type Scheduler a =
    ReaderT Int
        (StateT [(String, Int)]
            (Writer [String]))
        a
```

Ugyanez absztrakcióval:

```haskell
type SchedulerM m a =
    ( MonadReader Int m
    , MonadState [(String, Int)] m
    , MonadWriter [String] m
    ) => m a
```

---

## 💡 IO Műveletek (`liftIO,putStr,putStrLn`)

```haskell
liftIO :: MonadIO m => IO a -> m a -- Ha az IO műveletet fel kell liftelni egy másik monadra
putStr :: String -> IO () -- Egy Stringet ír ki új sor kezdése nélkül
putStrLn :: String -> IO () -- Egy Stringet ír ki új sor kezdéssel
```

Példa:

```haskell
drawLawn :: (Int,Int) -> (Int,Int) -> Lawnmover ()
drawLawn x y = do
    liftIO $ putStrLn "Hello"
```

---

# 🔄 Traversable

```haskell
data Gofri f a
    = MkGofri (f a) (f (Gofri f a))
    deriving (Foldable, Functor)

deriving instance (Eq a, Eq1 f) => Eq (Gofri f a)
deriving instance (Show a, Show1 f) => Show (Gofri f a)

instance (Traversable g) => Traversable (Gofri g) where
    traverse :: (Traversable g, Applicative f) => (a -> f b) -> Gofri g a -> f (Gofri g b)
    traverse f (MkGofri fa ffa) = MkGofri <$> traverse f fa <*> traverse (\a -> traverse f a) ffa

data CrazyType3 a b
    = CrazyCon1 a b a 
    | CrazyCon2 (CrazyType3 a b) [b] [a]
    | CrazyCon3 (CrazyType3 Int b) (CrazyType3 a a) [[b]]
    deriving (Eq, Show, Functor, Foldable)

instance Traversable (CrazyType3 fixed) where
    traverse :: (Applicative f) => (a -> f b) -> CrazyType3 fixed a -> f (CrazyType3 fixed b)
    traverse f (CrazyCon1 b a c) = CrazyCon1 <$> pure b <*> f a <*> pure c
    traverse f (CrazyCon2 a xs l) = CrazyCon2 <$> traverse f a <*> traverse f xs <*> pure l 
    traverse f (CrazyCon3 a l xs) = CrazyCon3 <$> traverse f a <*> pure l <*> traverse (\x -> traverse f x) xs

```



---

# 📝 Parser

## Új operátor hozzáadása

Ha ugyanazon a precedenciaszinten szeretnél operátort hozzáadni:

```haskell
pDiv :: Parser Exp
pDiv =
    chainl1 pMul ((:!!) <$ string' "!!")
    <|>
    chainl1 pMul ((:/) <$ char' '/')
```

### Példa

`!!` ugyanazon a precedenciaszinten van mint a `/`.

---
### Minden is benne van: https://github.com/akaposi/ELTE-func-lang/blob/master/2025-26-2/3/Gy11.hs

# 🎯 Vizsga Hajrá! - Motivációs Röplap

Figyelj, ide figyelj, mert ezt most komolyan mondom:

Nem vagy egyedül.

Tudom, hogy most este van, és lehet, hogy fáradt vagy. Lehet, hogy a képernyő előtt ülsz, és a monád transzformerek olyanok, mintha kínaiul lennének. De figyelj: ezerszámra voltak előtted diákok, akik ugyanígy éreztek. És átmentek. És te is át fogsz menni.

Emlékezz, mit tudsz:

A Functor? Csak annyi, hogy megyünk az a paraméterre és alkalmazzuk f-et. Pont.

A Foldable? Összegyűjtjük az a-kat.

A Traversable? Ugyanaz mint a Functor, csak Applicative-ba csomagolva.

A monád transzformerek? Csak egymásra pakolt dobozok. A lift a barátod.

És ami a legfontosabb: A vizsga nem azt méri, hogy tökéletes vagy-e. Azt méri, hogy elég jó vagy-e. És az vagy. Mert itt vagy, mert tanultál, mert próbálkoztál.

Vegyél egy mély levegőt. Igyál egy korty vizet. És emlékezz: a típusod a barátod. Hagyd, hogy a típus vezessen. Ha elakadsz, kövesd a típusokat - ők tudják az utat.

Menni fog. Tényleg. 🚀

És ha holnap bent ülsz a vizsgán, és meglátod azt a CrazyType3-at, mosolyogj rá egyet. Mert te tudod, mit kell csinálni.

Hajrá! 💪

