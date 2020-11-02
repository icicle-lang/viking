{-# LANGUAGE NoImplicitPrelude #-}
module Viking (
  -- $overview

  -- * General
  -- $general
    Stream
  , Of(..)
  ) where

import           Streaming (Stream, Of(..))

-- $overview
--
-- This library is intended to be used with the one or more of the following
-- imports:
--
-- @
-- import           Viking (Of(..), Stream)
-- import qualified Viking.Stream as Stream
-- import qualified Viking.ByteStream as ByteStream
-- import qualified Viking.Char8Stream as Char8Stream
-- @
--

-- $general
--
-- The 'Stream' data type can represent any effectful succession of steps,
-- where the form of the steps or 'commands' is specified by the first
-- parameter.
--
-- @
-- data Stream f m r =
--     Step !(f (Stream f m r))
--   | Effect (m (Stream f m r))
--   | Return r
-- @
--
-- In the common case, effectful linked-lists, the first parameter is
-- instantiated as @Of a@ where 'Of' is a left-strict tuple.
--
-- @
-- data Of a b =
--   !a :> b
--
-- data Stream (Of a) m r =
--     Step !(Of a (Stream (Of a) m r))
--   | Effect (m (Stream (Of a) m r))
--   | Return r
-- @
--
