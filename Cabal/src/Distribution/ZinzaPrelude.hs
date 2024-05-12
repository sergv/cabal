-- | A small prelude used in @zinza@ generated
-- template modules.
module Distribution.ZinzaPrelude
  ( Writer
  , execWriter
  , tell

    -- * Re-exports
  , forM_
  , Generic
  , PackageName
  , Version
  , prettyShow

  , Builder
  , tellC
  , tellB
  , tellS
  , tellI
  ) where

import Data.ByteString.Builder        (Builder)

import Distribution.Compat.Prelude
import Prelude ()

import Control.Monad (forM_)
import Distribution.Pretty (prettyShow)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.Version (Version)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB

newtype Writer a = W { unW :: Builder -> (Builder, a) }

instance Functor Writer where
  fmap = liftM

instance Applicative Writer where
  pure x = W $ \ss -> (ss, x)
  (<*>) = ap

instance Monad Writer where
  return = pure
  m >>= k = W $ \s1 ->
    let (s2, x) = unW m s1
     in unW (k x) s2
  {-# INLINE (>>=) #-}

execWriter :: Writer a -> Builder
execWriter w = fst (unW w mempty)

tell :: Builder -> Writer ()
tell s = W $ \s' -> (s' <> s, ())

tellC :: Char -> Writer ()
tellC = tell . BSB.char8

tellB :: BS.ByteString -> Writer ()
tellB = tell . BSB.byteString

tellS :: String -> Writer ()
tellS = tell . BSB.string8

tellI :: Int -> Writer ()
tellI = tell . BSB.intDec
