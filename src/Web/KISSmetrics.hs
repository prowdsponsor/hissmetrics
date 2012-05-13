-- | This module is meant to be imported qualified:
--
-- @
-- import qualified Web.KISSmetrics as KISSmetrics
-- @
module Web.KISSmetrics
    (
    ) where

import Control.Arrow (second)
import Data.Text (Text)
import Data.Time (UTCTime, formatTime)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as H


-- | Your KISSmetrics API key.
type APIKey = H.Ascii


-- | KISSmetrics names and identities are limited to at most 255
-- characters and all commas (@,@) and colons (@:@) are changed
-- to spaces (@ @).  Nothing is checked by this Haskell library,
-- so be careful =).
type SimpleText = H.Ascii


-- | A KISSmetrics property.  The property names needs to follow
-- the rules outlined on 'SimpleText'@'s@ documentation.  The
-- property value, on the other hand, are only limited to 8 KiB
-- and don't have any other restrictions.
type Property = (SimpleText, Text)


-- | A timestamp used only to ignore duplicated events.
data Timestamp =
    Automatic
    -- ^ Use KISSmetrics' servers time as the timestamp.
  | Manual UTCTime
    -- ^ Use given time as the timestamp.


data CallType =
    -- | Record an event.
    Record { eventName :: SimpleText
             -- ^ Name of the event being recorded.
           , identity :: SimpleText
             -- ^ Identity of the person doing the event.
           , timestamp :: Timestamp
             -- ^ See 'Timestamp'.
           , properties :: [Property]
             -- ^ Any additional properties you may want.
           }
    -- | Set user properties without recording an event.
  | SetProps { identity :: SimpleText
               -- ^ Identity of the person whose properties will
               -- to be changed.
             , timestamp :: Timestamp
               -- ^ See 'Timestamp'.
             , properties :: [Property]
               -- ^ Properties to be set.
             }
    -- | Alias two identities as the same one.
  | Alias { identity :: SimpleText
            -- ^ Identity of the person you're aliasing.
          , identity' :: SimpleText
            -- ^ Other identity you want to alias.
          }


-- | Call KISSmetrics' API.  See 'CallType' for documentation
-- about which calls you may make.
--
-- Note that official KISSmetrics' APIs provide many functions
-- (usually four) while we provide just this one and a sum data
-- type.  This function alone does the work of @record@, @set@,
-- @identify@ and @alias@.
--
-- TODO: Currently there's no support for automatically retrying
-- failed request, you need to retry yourself.
call :: H.Manager  -- ^ HTTP connection manager (cf. 'H.newManager').
     -> APIKey     -- ^ Your KISSmetrics API key.
     -> CallType   -- ^ Which call you would like to make.
     -> IO ()
call manager apikey callType =
  C.runResourceT $ do
    -- Create the request
    let (path, args) = callInfo callType
        request =
          H.def { H.method = "GET"
                , H.secure = True
                , H.host   = "trk.kissmetrics.com"
                , H.path   = path
                , H.queryString =
                    H.renderSimpleQuery False $
                    ("_k", apikey) : args
                , H.redirectCount = 0
                }

    -- Make the call
    H.Response {..} <- H.http request manager

    -- By default http-conduit will already throw an exception on
    -- anything other than 200 Ok, so we don't need to check the
    -- response.  We consume it just to free the resources as early
    -- as possible.  (If we just closed and KISSmetrics decided to
    -- give an OK message in the body, the connection would not be
    -- keep-alived correctly.)
    responseBody C.$$ CL.sinkNull



-- | Internal function.  Given a 'CallType', return the URL to be
-- used and generate a list of arguments.
callInfo :: CallType -> (H.Ascii, H.SimpleQuery)
callInfo Record {..} =
  ( "/e"
  , (:) ("_n", eventName) $
    (:) ("_p", identity) $
    timestampInfo timestamp $
    propsInfo properties
  )
callInfo SetProps {..} =
  ( "/s"
  , (:) ("_p", identity) $
    timestampInfo timestamp $
    propsInfo properties
  )
callInfo Alias {..} =
  ( "/a"
  , [("_p", identity), ("_n", identity')]
  )


-- | Generate a difference list of arguments for a timestamp.
timestampInfo :: Timestamp
              -> (H.SimpleQuery -> H.SimpleQuery) -- ^ Difference list.
timestampInfo Automatic = id
timestampInfo (Manual t) =
  (:) ("_d", "1") .
  (:) ("_t", B8.pack $ formatTime locale "%s" t)
    where locale = error "Web.KISSmetrics.timestampInfo: locale shouldn't be needed."


-- | Generate a list of arguments for a list of properties.
propsInfo :: [Property] -> H.SimpleQuery
propsInfo = map (second TE.encodeUtf8)
