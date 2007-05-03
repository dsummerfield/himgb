module Main where

import Network.CGI
import Control.Monad
import Graphics.GD
import Text.XHtml
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import System.Posix.Time
import System.IO

data Post = Post { up_name :: String
                 , file    :: Maybe String
                 , up_time :: Int
                 , txt     :: String
    } deriving (Show, Read)

data Thread = Thread { posts :: [Post]
                     , eyde  :: Int
    } deriving (Show, Read)

data HState = HState { ibtitle :: String
                     , threads :: [Thread]
    } deriving (Show, Read)

-- -------------------------------------------------------------------------

addPost :: Thread -> Post -> Thread
addPost t p = Thread (posts t ++ [p]) (eyde t) 
 
newThread :: HState -> Post -> HState
newThread h po = h { threads = (Thread [po] (up_time po)): threads h }

addPost' :: HState -> Int -> Post -> HState
addPost' hs idx po = 
    let t  = filter (\th -> idx == eyde th) (threads hs)
        nt = addPost (head t) po
        t' = filter (\th -> idx /= eyde th) (threads hs)
     in hs { threads = nt:t' }

addSagedPost :: HState -> Int -> Post -> HState
addSagedPost hs idx po = hs { threads = map (\th -> 
    if idx == eyde th then addPost th po else th) (threads hs) }

-- -------------------------------------------------------------------------

formatImage :: Maybe String -> Html
formatImage Nothing = noHtml
formatImage (Just l)= lnk (image ! [src ("src/"++l++"s")]) ("src/"++l)

formatPost :: Post -> Html
formatPost po = p (toHtml (replicate 70 '-')) +++
                p (bold (toHtml "author:")) +++
                (toHtml (up_name po)) +++
                p (formatImage (file po)) +++
                p (bold (toHtml "txt:")) +++
                (toHtml (txt po)) +++ 
                br +++ 
                br

replyForm eeydee =
  form (hidden "id" eeydee +++
        (case eeydee of
           "1" -> p (bold (toHtml "create new thread:"))
           _   -> p (bold (toHtml "reply to thread:"))) +++
        p (toHtml "name:") +++
        textfield "name" +++
        p (toHtml "Text:") +++
        textfield "txt" +++
        p (toHtml "file") +++
        afile "upfile" +++
        submit "send" "submit") ! [ method "POST"
                                  , enctype "multipart/form-data"]

formatThread :: Thread -> Html
formatThread t = hr +++ 
                 foldr ((+++).formatPost) noHtml (posts t) +++
                 replyForm (show (eyde t)) +++
                 hr

fiatImgB :: HState -> Html
fiatImgB h = (thetitle (toHtml (ibtitle h))) +++
             h1 (toHtml (ibtitle h)) +++ 
             hr +++
             replyForm "1" +++
             (foldr ((+++).formatThread) noHtml $ threads h) +++
             (p $ toHtml $ lnk "source" "hImgb.hs")

lnk txt hrf = anchor (toHtml txt) ! [href hrf]

-- ------------------------------------------------------------------------

makethumb :: FilePath -> IO ()
makethumb file = do
    img <- loadJpegFile file
    (sizex, sizey) <- imageSize img
    let ratiox = fromIntegral sizex / 300.0
        ratioy = fromIntegral sizey / 300.0
        ratio = max ratiox ratioy
    img' <- resizeImage (round (fromIntegral sizex / ratio))
                        (round (fromIntegral sizey / ratio)) img
    saveJpegFile (-1) (file ++ "s") img'

-- ------------------------------------------------------------------------

main :: IO ()
main = do
    runCGI (handleErrors imgb)

imgb :: CGI CGIResult
imgb = do
    efile   <- liftIO $ openFile "stateh" ReadWriteMode
    idx     <- liftM  (fromMaybe               "") $ getInput "id"
    text    <- liftM  (fromMaybe        "silence") $ getInput "txt"
    bslfile <- liftM  (fromMaybe          B.empty) $ getInputFPS "upfile"
    name    <- liftM  (fromMaybe "Anonymous Hero") $ getInput "name"
    purge   <- liftM  (fromMaybe              "f") $ getInput "purge"

    hst <- (case purge of 
       "t" -> return $ HState "hImgB" []
       _   -> liftM read (liftIO (hGetLine efile)))

    case null idx of
      True  -> do
        liftIO (hClose efile)
        output $ prettyHtml $ fiatImgB hst
      False ->
        case read idx of
          1 -> 
            if B.null bslfile 
              then fail "must supply image with new threads"
              else do liftIO $ do 
                        time <- liftM show epochTime
                        let img = "src/"++time
                        B.writeFile img bslfile
                        makethumb img
                        let nt = newThread hst (
                                   Post { up_name = name, file = Just time
                                        , up_time = read time, txt = text } )
                        hSeek efile AbsoluteSeek 0
                        hPutStrLn efile (show nt)
                        hClose efile
                      output $ prettyHtml $ "Shaved!"

          _ -> do liftIO $ do -- replying to thread.
                              -- warning: we assume idx a valid thread.
                              -- fix this.
                    time <- liftM show epochTime
                    if B.null bslfile 
                      then do when (null text) (fail 
                                "reply with either an image or text or both")
                              hSeek efile AbsoluteSeek 0
                              hPutStrLn efile (show (addPost' hst (read idx) (
                                Post { up_name = name, file = Nothing
                                     , up_time = read time, txt = text })))
                              hClose efile
                      else do let img = "src/"++time
                              B.writeFile img bslfile
                              makethumb img
                              hSeek efile AbsoluteSeek 0
                              hPutStrLn efile (show (addPost' hst (read idx) (
                                Post { up_name = name, file = Just time
                                     , up_time = read time, txt = text })))
                              hClose efile
                  output $ prettyHtml $ "Shaved!"

