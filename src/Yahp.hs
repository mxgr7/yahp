{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yahp
  (module Yahp
  ,module S
  ) where

import           Control.Arrow as S
import           Control.Exception as S hiding (throwIO, throwTo)
import qualified Control.Lens as Lens
import           Control.Monad as S
-- import           Control.Monad.Except as S
-- import           Control.Monad.RWS as S hiding (pass)
-- import           Control.Monad.Reader as S
import           Control.Monad.Writer as S (MonadWriter, Writer)
import           Control.Monad.Writer (tell, runWriter)
import qualified Data.ByteString as B
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy as LB
import           Data.Char as S
import           Data.Ord as S
import           Data.Fixed as S
import qualified Data.Foldable as UnsafeFoldable
import           Data.Function as S
import           Data.Functor as S
import           Data.Int as S
import           Data.List as S
  ( lookup
  , zip3
  , zip4
  , zip5
  , zipWith3
  , zipWith4
  , zipWith5
  , unzip3
  , unzip4
  , unzip5
  , delete
  , partition)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe as S hiding (fromJust)
import           Data.Monoid ()
import           Data.String as S (String, fromString)
import           Data.Typeable as S
import qualified GHC.Exts as G
import           GHC.Exts as S (IsList(Item), fromList)
import qualified Prelude
import qualified Protolude as P
import Protolude as S hiding (
  -- | GHC.OverloadedLabels
  IsLabel(..)
  -- | Data.Text.Encoding.Error
  ,lenientDecode , ignore , strictDecode , replace
  -- | NE.head
  , head
  -- | import Control.Exception
  ,catch
  -- | https://github.com/sdiehl/protolude/issues/110
  , minimum, minimumBy, maximum, maximumBy
  -- | GHC.generics
  ,from, to
  -- | GHC.Types
  , Symbol
  -- | bifunctor
  ,first,second, show, map)
import           Protolude.Error as S
import           System.IO.Unsafe
import           Text.Show as S
import           Relude.Extra.Tuple as S


-- {-# INLINE pshow #-}
-- pshow :: (Show a, StringConv String b) => a -> b
-- pshow = P.show

{-# INLINE minimum #-}
minimum :: Ord a => NonEmpty a -> a
minimum = UnsafeFoldable.minimum

{-# INLINE minimumBy #-}
minimumBy :: (a -> a -> Ordering) -> NonEmpty a -> a
minimumBy = UnsafeFoldable.minimumBy

{-# INLINE maximum #-}
maximum :: Ord a => NonEmpty a -> a
maximum = UnsafeFoldable.maximum

{-# INLINE maximumBy #-}
maximumBy :: (a -> a -> Ordering) -> NonEmpty a -> a
maximumBy = UnsafeFoldable.maximumBy


{-# INLINE foldr1 #-}
foldr1 :: (a -> a -> a) -> NonEmpty a -> a
foldr1 = UnsafeFoldable.foldr1

{-# INLINE foldl1' #-}
foldl1' :: (a -> a -> a) -> NonEmpty a -> a
foldl1' f (a :| b) = UnsafeFoldable.foldl' f a b

{-# INLINE foldl1 #-}
foldl1 :: (a -> a -> a) -> NonEmpty a -> a
foldl1 = UnsafeFoldable.foldl1
  
{-# INLINE headMaybe #-}
headMaybe :: Foldable f => f a -> Maybe a
headMaybe = P.head

{-# INLINE head #-}
head :: NonEmpty a -> a
head = NE.head


{-# INLINE toList' #-}
toList' :: G.IsList l => l -> [G.Item l]
toList' = G.toList

fmap2 :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
fmap2 = fmap . fmap
{-# INLINE fmap2 #-}

fmap3 :: (Functor f1, Functor f2, Functor f) => (a -> b) -> f1 (f2 (f a)) -> f1 (f2 (f b))
fmap3 = fmap2 . fmap
{-# INLINE fmap3 #-}

pure3 :: (Applicative f1, Applicative f2, Applicative f3) => a -> f1 (f2 (f3 a))
pure3 = pure . pure . pure
{-# INLINE pure3 #-}

pure2 :: (Applicative f1, Applicative f2) => a -> f1 (f2 a)
pure2 = pure . pure
{-# INLINE pure2 #-}

traverse2 :: (Applicative f, Traversable t1, Traversable t2) => (a -> f b) -> t1 (t2 a) -> f (t1 (t2 b))
traverse2 = traverse . traverse
{-# INLINE traverse2 #-}

ffor :: Functor f => f a -> (a -> b) -> f b
ffor = flip fmap
{-# INLINE ffor #-}

chain :: Monad m => (a -> m b) -> m a -> m b
chain = (=<<)
{-# INLINE chain #-}

bind :: Monad m => m a -> (a -> m b) -> m b
bind = (>>=)
{-# INLINE bind #-}

both :: (a -> b) -> (a, a) -> (b, b)
both f ~(x,y) = (f x, f y)
{-# INLINE both #-}

enumAll :: (Bounded a, Enum a) => [a]
enumAll = [minBound..]
{-# INLINABLE enumAll #-}

maybeThrow :: MonadError e m => e -> Maybe a -> m a
maybeThrow msg = maybe (throwError msg) return
{-# INLINE maybeThrow #-}

lookupThrow'
  :: (MonadError e m, ConvertText a1 String, ConvertText String e,
      Lens.Ixed (t a2), Typeable (Lens.Index (t a2)),
      Show (Lens.Index (t a2)), Foldable t) =>
     a1 -> Lens.Index (t a2) -> t a2 -> m (Lens.IxValue (t a2))
lookupThrow' msg k c = maybeThrow (toS $ err <> toS msg) $ c Lens.^? Lens.ix k
  where err = show (typeOf k) <> " not found: " <> show k <> ". Container count " <>
          show (length c)
{-# INLINEABLE lookupThrow' #-}

lookupThrow :: (MonadError e m, ConvertText String e,
                 Lens.At (t a2), Typeable (Lens.Index (t a2)),
                 Show (Lens.Index (t a2)), Foldable t) =>
                Lens.Index (t a2) -> t a2 -> m (Lens.IxValue (t a2))
lookupThrow = lookupThrow' ""
{-# INLINE lookupThrow #-}

tellErrors :: (Foldable f, MonadWriter [a] m) => f (Either a b) -> m [b]
tellErrors = mapTellErrors id
{-# INLINE tellErrors #-}

mapTellErrors :: (Foldable f, MonadWriter [a] m) => (v -> Either a b) -> f v -> m [b]
mapTellErrors f xs = let (errs, vals) = partitionEithers $ fmap f $ toList xs in vals <$ tell errs
{-# INLINEABLE mapTellErrors #-}

noErrors :: (IsString e, Monoid e, MonadError e m) => Writer [e] a -> m a
noErrors = (\(a, es) -> if null es then pure a else unlineErrors es) . runWriter
{-# INLINEABLE noErrors #-}

sequenceErrors :: (MonadError e f, Monoid e, IsString e, Foldable t) => t (Either e b) -> f [b]
sequenceErrors = mapErrors id
{-# INLINE sequenceErrors #-}

mapErrors :: (MonadError e f, Monoid e, IsString e, Foldable t) => (a -> Either e b) -> t a -> f [b]
mapErrors f xs = do
  let (errs, vals) = partitionEithers $ f <$> toList xs
  vals <$ when (not $ null errs) (unlineErrors errs)
{-# INLINEABLE mapErrors #-}

unlineErrors :: (MonadError e m, Monoid e, IsString e, Foldable t) => t e -> m a
unlineErrors = throwError . mconcat . intersperse (fromString "\n") . toList
{-# INLINEABLE unlineErrors #-}

modifyError :: MonadError e m => (e -> e) -> m a -> m a
modifyError f = flip catchError (throwError . f)
{-# INLINE modifyError #-}

eitherException :: ConvertText s String => Either s a -> a
eitherException = either (Prelude.error . toS) id
{-# INLINE eitherException #-}

catchWhnf :: (Exception ex, MonadError er m) => (ex -> er) -> a -> m a
catchWhnf f a = unsafePerformIO $ catch (seq a $ pure2 a) (pure . throwError . f)
{-# INLINEABLE catchWhnf #-}

forMaybe :: c -> Maybe a -> (a -> c) -> c
forMaybe d = flip $ maybe d
{-# INLINE forMaybe #-}


fromUtf8 :: B.ByteString -> Text
fromUtf8 = decodeUtf8With T.strictDecode

fromUtf8Lenient :: B.ByteString -> Text
fromUtf8Lenient = decodeUtf8With T.lenientDecode

fromUtf8Lazy :: LB.ByteString -> LText
fromUtf8Lazy = TL.decodeUtf8With T.strictDecode

fromUtf8LazyLenient :: LB.ByteString -> LText
fromUtf8LazyLenient = TL.decodeUtf8With T.lenientDecode


-- fcheck :: IO ()
-- fcheck = let fl x y = trace ("trace: " <> [Prelude.head y]) $ concat ["(",x,"+",y,")"]
--              fr x y = trace ("trace: " <> [Prelude.head x]) $ concat ["(",x,"+",y,")"]
--              l = ["a","b","c","d","e"]
--   in putStrLn $ unlines $ fmap ((\x -> seq x (x::Text)) . toS. trace "")
--   [foldl   fl       "0"     l
--   ,foldl1  fl $     "0" :|  l
--   ,foldl'  fl       "0"     l
--   ,foldl1' fl $     "0" :|  l
--   ,foldr   fr       "0"     l
--   ,foldr'  fr       "0"     l
--   ,foldr1  fr $     "0" :|  l
--   ]

-- trace: e
-- trace: d
-- trace: c
-- trace: b
-- trace: a

-- trace: e
-- trace: d
-- trace: c
-- trace: b
-- trace: a

-- trace: a
-- trace: b
-- trace: c
-- trace: d
-- trace: e

-- trace: a
-- trace: b
-- trace: c
-- trace: d
-- trace: e

-- trace: a
-- trace: b
-- trace: c
-- trace: d
-- trace: e

-- trace: e
-- trace: d
-- trace: c
-- trace: b
-- trace: a

-- trace: 0
-- trace: a
-- trace: b
-- trace: c
-- trace: d
-- (((((0+a)+b)+c)+d)+e)
-- (((((0+a)+b)+c)+d)+e)
-- (((((0+a)+b)+c)+d)+e)
-- (((((0+a)+b)+c)+d)+e)
-- (a+(b+(c+(d+(e+0)))))
-- (a+(b+(c+(d+(e+0)))))
-- (0+(a+(b+(c+(d+e)))))

class IsLabel (x :: P.Symbol) a where
  fromLabel :: HasCallStack => a

instance {-# OVERLAPPABLE #-} P.IsLabel m a => IsLabel m a where
  fromLabel = P.fromLabel @m
  
ifThenElse :: Bool -> a -> a -> a
ifThenElse True  a _ = a
ifThenElse False _ b = b
{-# INLINE ifThenElse #-}
