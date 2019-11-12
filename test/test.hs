import           Hedgehog.Main

import qualified Test.Viking.ByteStream
import qualified Test.Viking.Char8Stream
import qualified Test.Viking.Stream
import qualified Test.Viking.Stream.Binary


main :: IO ()
main =
  defaultMain [
      Test.Viking.ByteStream.tests
    , Test.Viking.Char8Stream.tests
    , Test.Viking.Stream.tests
    , Test.Viking.Stream.Binary.tests
    ]
