-- | Snap integration for the WebSockets library
module Network.WebSockets.Snap
    ( runWebSocketsSnap
    ) where

import qualified Network.WebSockets as WS
import qualified Snap.Core as Snap
import qualified Snap.Internal.Http.Types as Snap
import qualified Snap.Types.Headers as Headers

-- | The following function escapes from the current 'Snap.Snap' handler, and
-- continues processing the 'WS.WebSockets' action. The action to be executed
-- takes the 'WS.Request' as a parameter, because snap has already read this
-- from the socket.
runWebSocketsSnap :: (WS.Request -> WS.WebSockets ())
                  -> Snap.Snap ()
runWebSocketsSnap = runWebSocketsSnapWith WS.defaultWebSocketsOptions

-- | Variant of 'runWebSocketsSnap' which allows custom options
runWebSocketsSnapWith :: WS.WebSocketsOptions
                      -> (WS.Request -> WS.WebSockets ())
                      -> Snap.Snap ()
runWebSocketsSnapWith options ws = do
    rq <- Snap.getRequest
    Snap.escapeHttp $ \tickle writeEnd ->
        let options' = options
                { WS.onPong = tickle 30 >> WS.onPong options
                }

        in WS.runWebSocketsWith options' (ws (fromSnapRequest rq)) writeEnd

-- | Convert a snap request to a websockets request
fromSnapRequest :: Snap.Request -> WS.Request
fromSnapRequest rq = WS.Request
    { WS.requestPath    = Snap.rqURI rq
    , WS.requestHeaders = Headers.toList (Snap.rqHeaders rq)
    }
