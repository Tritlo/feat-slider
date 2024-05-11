{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE LambdaCase #-} 
{-# LANGUAGE CPP #-} 
module Main where

import Brick
import Brick.AttrMap as A
import Brick.Widgets.ProgressBar as P
import Test.Feat
import Data.Dynamic
import Brick.Types as T
import Brick.Widgets.Border (borderWithLabel, vBorder, hBorder)

import Brick.Widgets.Center
import qualified Graphics.Vty as V
import Control.Monad (void, unless)
import Type.Reflection (SomeTypeRep)

import Lens.Micro.Mtl
import Lens.Micro.TH
import Lens.Micro
import Data.Maybe (fromJust)
import Control.Monad.IO.Class (liftIO)
import Text.Wrap
import Data.Proxy

data AppState a = AS { _sz :: Int,
                       _ind :: Int,
                       _ty :: SomeTypeRep,
                       _indMax :: Int,
                       _vals :: Dynamic,
                       _pty :: Proxy [a]
                     }

makeLenses ''AppState


theApp :: (Show a, Enumerable a) => App (AppState a) e String
theApp = App { appDraw = drawUi
             , appAttrMap = const theMap
             , appChooseCursor = neverShowCursor
             , appHandleEvent = appEvent
             , appStartEvent = do vty <- getVtyHandle
                                  liftIO $ V.setMode (V.outputIface vty) V.Mouse True
}

  where theMap = A.attrMap V.defAttr [ (P.progressCompleteAttr, V.black `on` V.brightWhite),
                                       (P.progressIncompleteAttr, bg V.black)]

        sizeDown = do vz <- use sz
                      px <- use pty
                      unless (vz <= 0) $ do
                        sz -= 1
                        vz' <- use sz
                        let (ni,vs) =  (filter (\(n, _) -> n /= 0) $ values ) !! vz'
                        indMax .= (fromInteger ni)
                        vals .= toDyn (vs `asProxyTypeOf` px)
        sizeUp = do sz += 1
                    vz' <- use sz
                    px <- use pty
                    let (ni,vs) = (filter (\(n, _) -> n /= 0) $ values) !! vz'
                    indMax .= (fromInteger ni)
                    vals .= toDyn (vs `asProxyTypeOf` px)
        appEvent (T.VtyEvent e)
            = case e of 
                V.EvKey (V.KChar 'q') [] -> halt
                _ -> return ()
        appEvent (MouseDown vpnm _ _ loc) =
                    case vpnm of
                      "sizeUp" -> sizeUp
                      "sizeDown" -> sizeDown
                      "quit" -> halt
                      "pBar" -> lookupViewport vpnm >>= \case
                                     Just v -> do
                                         im <- use indMax
                                         let w = V.regionWidth $ (v ^. vpSize)
                                             c = loc ^. locationColumnL 
                                             ratio = floor @Float ((fromIntegral c / fromIntegral w) * fromIntegral im)
                                         ind .= ratio
                                     _ -> return ()
                      _ -> return ()
        appEvent _ = return ()
        drawUi as = 

                    [ do let pbar = P.progressBar (Just $ show (1 + as ^. ind) ++ "/" ++ show (as ^. indMax))
                                                  ((fromIntegral $ 1 + as ^. ind) / (fromIntegral (as ^. indMax)))
                         borderWithLabel (str "Generate a value:") $ 
                           vBox [ 
                              vLimit 5 $ center $   
                                  hBox $  [ clickable "quit" $ hLimit 7 $ center $ str "Quit",
                                           vBorder,
                                           center $ hBox [ (str "Type: "),
                                                           str (show $ as ^. ty)],
                                           vBorder,
                                           center $ hBox [(str "Size:"),
                                                          str (show $ as ^. sz)],
                                           vBorder,
                                           hLimit 5 $ center $ vBox  [
                                                     clickable "sizeUp" $ center $ str "↑",
                                                     hBorder,
                                                     clickable "sizeDown" $ center $ str "↓"
                                                     ]  ]
                              , hBorder
                              , vLimit 1 $ viewport "pBar" Vertical $ clickable "pBar" pbar
                              , hBorder
                              , center $ strWrapWith (defaultWrapSettings {breakLongWords =True}) (show v)]
                         ]
          where 
                v = ((fromJust $ fromDynamic (as ^. vals)) `asProxyTypeOf` (as ^. pty)) !! (as ^. ind)


main :: IO ()
main = void $ defaultMain theApp
                    (AS 1 0 (dynTypeRep $ toDyn $ head $ vs)
                            (fromInteger indMaxInit)
                            (toDyn (vs `asProxyTypeOf` px))
                            px)

    where (indMaxInit, vs) = (filter (\(n, _) -> n /= 0) $ values) !! 0
#ifdef TYPE
          px :: Proxy [TYPE]
#else
          px :: Proxy [[Bool]]
#endif
          px = Proxy
