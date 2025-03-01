{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Viking.Stream.Binary where

import           Control.Monad.Trans.Either (EitherT, runEitherT)

import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Functor.Identity (runIdentity)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Viking.Prelude

import           Viking
import qualified Viking.ByteStream as ByteStream
import qualified Viking.Stream as Stream
import           Viking.Stream.Binary (BinaryError)
import qualified Viking.Stream.Binary as Stream

prop_get :: Property
prop_get =
  property $ do
    n <- forAll $ Gen.int (Range.linearFrom 8 1 100)
    x0 <- forAll $ Gen.int64 Range.linearBounded
    tl0 <- forAll $ Gen.bytes (Range.linear 0 100)

    let
      hd0 =
        Lazy.toStrict . Put.runPut $ Put.putInt64be x0

      (bs0, bs1) =
        Strict.splitAt n (hd0 <> tl0)

    annotateShow bs0
    annotateShow bs1

    (x, tl1) <- evalEither . runIdentity . runEitherT $
      Stream.runGet Get.getInt64be . ByteStream.fromChunks $ Stream.each [bs0, bs1]

    let
      tl =
        runIdentity $ ByteStream.toStrict_ tl1

    x0 === x
    tl0 === tl

test_get_multiple ::
     (forall m a r. Monad m => Get a -> ByteStream m r -> Stream (Of a) (EitherT BinaryError m) r)
  -> Range Int
  -> Property
test_get_multiple getMultiple n =
  property $ do
    xs0 <- forAll . Gen.list n $ Gen.int64 Range.linearBounded

    let
      lbs0 =
        Put.runPut $ traverse_ Put.putInt64be xs0

    annotateShow lbs0

    xs <- evalEither . runIdentity . runEitherT $
      Stream.toList_ . getMultiple Get.getInt64be $ ByteStream.fromLazy lbs0

    xs0 === xs

prop_get_some :: Property
prop_get_some =
  test_get_multiple Stream.runGetSome (Range.linear 1 100)

prop_get_many :: Property
prop_get_many =
  test_get_multiple Stream.runGetMany (Range.linear 0 100)

tests :: IO Bool
tests =
  checkParallel $$(discover)
