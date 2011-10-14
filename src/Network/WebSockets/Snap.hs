-- | Snap integration for the WebSockets library
module Network.WebSockets.Snap
    ( runWebSocketsSnap
    ) where

import qualified Network.WebSockets as WS
import qualified Snap.Core as Snap
import qualified Snap.Internal.Http.Types as Snap
import qualified Snap.Types.Headers as Headers

-- | The following function escapes from the current 'Snap.Snap' handler, and
-- continues processing the 'WS.WebSockets' action.
--
-- The provided WebSockets action has to make sure that at least every 30
-- seconds, a @getOptions >>= liftIO . onPong@ action is executed to prevent
-- the connection from timing out. For protocols that support ping (hybi10),
-- that can be easily done using 'spawnPingThread'. For hybi00, you have to
-- implement your own mechanism.
runWebSocketsSnap :: (WS.Request -> WS.WebSockets ())
                  -> Snap.Snap ()
runWebSocketsSnap = runWebSocketsSnapWith WS.defaultWebSocketsOptions

-- | Variant of 'runWebSocketsSnap' which allows custom options.
runWebSocketsSnapWith :: WS.WebSocketsOptions
                      -> (WS.Request -> WS.WebSockets ())
                      -> Snap.Snap ()
runWebSocketsSnapWith options ws = do
    rq <- Snap.getRequest
    Snap.escapeHttp $ \tickle writeEnd ->
        let options' = options
                { WS.onPong = tickle 30 >> WS.onPong options
                }

        in WS.runWebSocketsWith options' (fromSnapRequest rq) ws writeEnd

-- | Convert a snap request to a websockets RequestHttpPart
fromSnapRequest :: Snap.Request -> WS.RequestHttpPart
fromSnapRequest rq = WS.RequestHttpPart
    { WS.requestHttpPath    = Snap.rqURI rq
    , WS.requestHttpHeaders = Headers.toList (Snap.rqHeaders rq)
    }

