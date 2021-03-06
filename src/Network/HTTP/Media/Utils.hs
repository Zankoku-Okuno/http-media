-----------------------------------------------------------------------------
-- | Common utilities.
module Network.HTTP.Media.Utils
    ( breakByte
    , trimBS

    , validChars
    , isValidChar

    , slash
    , semi
    , comma
    , space
    , equal
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString as BS

------------------------------------------------------------------------------
import Data.ByteString (ByteString)
import Data.Word       (Word8)


------------------------------------------------------------------------------
-- | Equivalent to 'Data.ByteString.breakByte', but leaves out the byte the
-- string is broken on.
breakByte :: Word8 -> ByteString -> (ByteString, ByteString)
breakByte w = fmap BS.tail . BS.breakByte w


------------------------------------------------------------------------------
-- | Trims space characters from both ends of a ByteString.
trimBS :: ByteString -> ByteString
trimBS = BS.reverse . dropSpace . BS.reverse . dropSpace
  where
    dropSpace = BS.dropWhile (== space)


------------------------------------------------------------------------------
-- | List of the valid characters for a media-type `reg-name` as per RFC 4288.
validChars :: [Word8]
validChars =
    [33, 35, 36, 37, 43, 45, 46, 94, 95] ++ [48..57] ++ [65..90] ++ [97..122]


------------------------------------------------------------------------------
-- | Evaluates whether the given character is valid in a media type `reg-name`
-- as per RFC 4288.
isValidChar :: Word8 -> Bool
isValidChar c = c >= 97 && c <= 122 || c >= 48 && c <= 57 ||
    c >= 65 && c <= 90 || c `elem` [33, 35, 36, 37, 43, 45, 46, 94, 95]


------------------------------------------------------------------------------
-- | 'ByteString' compatible characters.
slash, semi, comma, space, equal :: Word8
[slash, semi, comma, space, equal] = [47, 59, 44, 32, 61]

