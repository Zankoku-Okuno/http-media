------------------------------------------------------------------------------
-- | A framework for parsing HTTP media type headers.
module Network.HTTP.Media
    (
    -- * Media types
      MediaType
    , (//)
    , (/:)
    , mainType
    , subType
    , parameters
    , (/?)
    , (/.)

    -- * Quality Values
    , Quality
    , parseQuality

    -- * Accept matching
    , matchAccept
    , mapAccept
    , mapAcceptMedia
    , mapAcceptBytes

    -- * Content matching
    , matchContent
    , mapContent
    , mapContentMedia

    -- * Accept
    , Accept (..)
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString as BS

------------------------------------------------------------------------------
import Control.Applicative  (pure, (<$>), (<*>), (<|>))
import Control.Monad        (guard)
import Data.ByteString      (ByteString, split)
import Data.ByteString.UTF8 (toString)

------------------------------------------------------------------------------
import Network.HTTP.Media.Accept    as Accept
import Network.HTTP.Media.MediaType as MediaType
import Network.HTTP.Media.Quality
import Network.HTTP.Media.Utils


------------------------------------------------------------------------------
-- | Matches a list of server-side resource options against a quality-marked
-- list of client-side preferences. A result of 'Nothing' means that nothing
-- matched (which should indicate a 406 error). If two or more results arise
-- with the same quality level and specificity, then the first one in the
-- server list is chosen.
--
-- The use of the 'Accept' type class allows the application of either
-- 'MediaType' for the standard Accept header or 'ByteString' for any other
-- Accept header which can be marked with a quality value.
--
-- > getHeader >>= parseQuality >>= matchAccept ["text/html", "application/json"]
--
-- For more information on the matching process see RFC 2616, section 14.1-4.
matchAccept
    :: Accept a
    => [a]         -- ^ The server-side options
    -> [Quality a]  -- ^ The client-side header value
    -> Maybe a
matchAccept options acceptq = do
    let merge (Quality c q) = map (`Quality` q) $ filter (`matches` c) options
        matched = concatMap merge acceptq
        (hq, qs) = foldr qfold (0, []) matched
        qfold (Quality v q) (mq, vs) = case compare q mq of
            GT -> (q, [v])
            EQ -> (mq, v : vs)
            LT -> (mq, vs)
        specific (a : ms) = Just $ foldl mostSpecific a ms
        specific []       = Nothing
    guard (hq /= 0)
    specific qs


------------------------------------------------------------------------------
-- | The equivalent of 'matchAccept' above, except the resulting choice is
-- mapped to another value. Convenient for specifying how to translate the
-- resource into each of its available formats.
--
-- > maybe render406Error renderResource $ getHeader >>= parseQuality >>= mapAccept
-- >     [ ("text" // "html",        asHtml)
-- >     , ("application" // "json", asJson)
-- >     ]
mapAccept
    :: Accept a
    => [(a, b)]    -- ^ The map of server-side preferences to values
    -> [Quality a] -- ^ The client-side header value
    -> Maybe b
mapAccept options accept =
    matchAccept (map fst options) accept >>= lookupMatches options


------------------------------------------------------------------------------
-- | A specialisation of 'mapAccept' that only takes MediaType as its input,
-- to avoid ambiguous-type errors when using string literal overloading.
--
-- > maybe render406Error renderResource $ getHeader >>= parseQuality >>= mapAcceptMedia
-- >     [ ("text/html",        asHtml)
-- >     , ("application/json", asJson)
-- >     ]
mapAcceptMedia ::
    [(MediaType, b)]       -- ^ The map of server-side preferences to values
    -> [Quality MediaType] -- ^ The client-side header value
    -> Maybe b
mapAcceptMedia = mapAccept


------------------------------------------------------------------------------
-- | A specialisation of 'mapAccept' that only takes ByteString as its input,
-- to avoid ambiguous-type errors when using string literal overloading.
--
-- > maybe render406Error encodeResourceWith $ getHeader >>= parseQuality >>= mapAcceptBytes
-- >     [ ("compress", compress)
-- >     , ("gzip",     gzip)
-- >     ]
mapAcceptBytes ::
    [(ByteString, b)]       -- ^ The map of server-side preferences to values
    -> [Quality ByteString] -- ^ The client-side header value
    -> Maybe b
mapAcceptBytes = mapAccept


------------------------------------------------------------------------------
-- | Matches a list of server-side parsing options against a the client-side
-- content value. A result of 'Nothing' means that nothing matched (which
-- should indicate a 415 error).
--
-- > matchContent ["application/json", "text/plain"] =<< parseAccept =<< getContentType
--
-- For more information on the matching process see RFC 2616, section 14.17.
matchContent
    :: Accept a
    => [a]         -- ^ The server-side response options
    -> a  -- ^ The client's request value
    -> Maybe a
matchContent options ctype = foldl choose Nothing options
  where
    choose m server = m <|> do
        guard $ ctype `matches` server
        Just server


------------------------------------------------------------------------------
-- | The equivalent of 'matchContent' above, except the resulting choice is
-- mapped to another value.
--
-- > maybe send415Error readRequestBodyWith $ getContentType >>= parseAccept >>= mapContent
-- >     [ ("application" // "json", parseJson)
-- >     , ("text" // "plain",       parseText)
-- >     ]
mapContent
    :: Accept a
    => [(a, b)]    -- ^ The map of server-side responses
    -> a           -- ^ The client request's header value
    -> Maybe b
mapContent options ctype =
    matchContent (map fst options) ctype >>= lookupMatches options


------------------------------------------------------------------------------
-- | A specialisation of 'mapContent' that only takes MediaType as its input,
-- to avoid ambiguous-type errors when using string literal overloading.
--
-- > getContentType >>= parseAccept >>=
-- >     maybe send415Error readRequestBodyWith . mapContentMedia
-- >         [ ("application/json", parseJson)
-- >         , ("text/plain",       parseText)
-- >         ]
mapContentMedia
    :: [(MediaType, b)]  -- ^ The map of server-side responses
    -> MediaType         -- ^ The client request's header value
    -> Maybe b
mapContentMedia = mapContent


------------------------------------------------------------------------------
-- | Parses a full Accept-* header into a list of quality-valued media types.
parseQuality :: Accept a => ByteString -> Maybe [Quality a]
parseQuality = (. split comma) . mapM $ \bs ->
    let (accept, q) = BS.breakSubstring ";q=" $ BS.filter (/= space) bs
    in (<*> parseAccept accept) $ if BS.null q
        then pure maxQuality else flip Quality <$> readQ
            (toString $ BS.takeWhile (/= semi) $ BS.drop 3 q)


------------------------------------------------------------------------------
-- | The equivalent of 'lookupBy matches'.
lookupMatches :: Accept a => [(a, b)] -> a -> Maybe b
lookupMatches ((k, v) : r) a
    | Accept.matches k a = Just v
    | otherwise         = lookupMatches r a
lookupMatches [] _ = Nothing

