{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when, void)
import Control.Monad.Trans
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Resource
import Data.Traversable (sequenceA)
import qualified Graphics.Rendering.OpenGL as GO
import qualified Graphics.UI.GLFW as G
import System.Exit
import System.IO
import Graphics.Luminance
import Graphics.Luminance.RW
import Data.Maybe

type App = ExceptT AppError (ResourceT IO)
newtype AppError = AppError String deriving (Eq, Show)


vertices :: [V 2 Float]
vertices =
  [
    vec2 (-0.5) (-0.5)
  , vec2 0 0.5
  , vec2 (0.5) (-0.5)
  ]

vsSrc = unlines
  [ "in vec2 co;"
  , "out vec4 vertexColor;"
  , "vec4 color[3] = vec4[]("
  ,    "vec4(1., 0., 0., 1.)"
  ,  ", vec4(0., 1., 0., 1.)"
  ,  ", vec4(0., 0., 1., 1.)"
  ,  ");"
  ,"void main() {"
  ,  "gl_Position = vec4(co, 0., 1.);"
  ,  "vertexColor = color[gl_VertexID];"
  ,"}"
  ]

fsSrc = unlines
  [ "in vec4 vertexColor;"
  , "out vec4 frag;"
  , "void main() {"
  , "  frag = vertexColor;"
  , "}"
  ]

data Error = ErrorStage StageError | ErrorProgram ProgramError deriving (Show)
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
    mw <- G.createWindow 640 480 "Simple example" Nothing Nothing
    case mw of
      Just window' -> do
        vs <- G.getVersionString
        print vs
        G.makeContextCurrent mw
        G.swapInterval 1
        mainLoop window'
        G.destroyWindow window'
      Nothing -> liftIO $ hPutStrLn stderr "unable to cereate window"
    G.terminate

mainLoop :: G.Window -> IO ()
mainLoop window =
  let
    rcmd geo = renderCmd Nothing False geo
    sbp geo = pureDraw $ rcmd geo
    fbb prog geo = defaultFrameCmd [ShadingCmd prog (\a -> mempty) [sbp geo]]
  in
    do
      (x::Either Error ()) <- runExceptT . runResourceT $ do
        program <- sequenceA [createStage VertexShader vsSrc, createStage FragmentShader fsSrc] >>= createProgram_
        triangle <- createGeometry vertices Nothing Triangle
        liftIO . void . draw $ fbb program triangle
      G.swapBuffers window
      mainLoop window
