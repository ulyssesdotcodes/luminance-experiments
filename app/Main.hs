{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when, void)
import Control.Monad.Trans
import qualified Control.Monad.Error.Class as CME
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Maybe
import Data.Traversable (sequenceA)
import qualified Graphics.Rendering.OpenGL as GO
import qualified Graphics.UI.GLFW as G
import System.Exit
import System.IO
import Graphics.Luminance
import Graphics.Luminance.RW
import Data.Maybe

vertices :: [V 2 Float]
vertices =
  [
    vec2 (-1) (-1)
  , vec2 (-1) 1
  , vec2 1 (-1)

  , vec2 1 (-1)
  , vec2 1 1
  , vec2 (-1) 1
  ]

data Error = ErrorStage StageError | ErrorProgram ProgramError deriving (Eq, Show)
instance HasStageError Error where
  fromStageError = ErrorStage

instance HasProgramError Error where
  fromProgramError = ErrorProgram

main :: IO ()
main = do
  successfulInit <- G.init
  when successfulInit $ do
    G.windowHint $ G.WindowHint'ContextVersionMajor 3
    G.windowHint $ G.WindowHint'ContextVersionMinor 3
    runMaybeT (MaybeT G.getPrimaryMonitor >>= (MaybeT . G.getVideoMode)) >>= print
    mw <- G.getPrimaryMonitor >>= \m -> G.createWindow 1920 1080 "Simple example" Nothing Nothing
    case mw of
      Just window' -> do
        vs <- G.getVersionString
        print vs
        G.makeContextCurrent mw
        G.swapInterval 1
        vertexShader <- createStageFromFile "app/passthrough.vert" VertexShader
        fragmentShader <- createStageFromFile "app/circle.frag" FragmentShader
        (x::Either Error ()) <- runExceptT . runResourceT $ do
          p <- sequenceA [vertexShader, fragmentShader] >>= createProgram_
          quad <- createGeometry vertices Nothing Triangle
          liftIO $ mainLoop window' p quad
        either (print . show) mempty x
        G.destroyWindow window'
      Nothing -> liftIO $ hPutStrLn stderr "unable to cereate window"
    G.terminate

createStageFromFile :: (CME.MonadError e m, MonadResource m, HasStageError e) => FilePath -> StageType -> IO (m Stage)
createStageFromFile f s =
  readFile f >>= \shader -> return $ createStage s shader

mainLoop :: G.Window -> Program () -> Geometry -> IO ()
mainLoop window prog geo =
  let
    rcmd = renderCmd Nothing False
    sbp geo = pureDraw $ rcmd geo
    fbb = defaultFrameCmd [ShadingCmd prog (\a -> mempty) [sbp geo]]
  in
    do
      void . draw $ fbb
      G.swapBuffers window
      mainLoop window prog geo
