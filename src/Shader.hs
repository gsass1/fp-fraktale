module Shader where


import Control.Monad
import qualified Data.ByteString as BS
import qualified Graphics.Rendering.OpenGL as GL
import System.IO
import System.Exit (exitFailure)
import SDL (($=))



data Shader = Shader
  { prog :: GL.Program
  , attrib :: GL.AttribLocation
  }



loadShader :: FilePath -> FilePath -> IO (Shader)
loadShader vsSourcePath fsSourcePath = do
    -- Vertex-Shader laden
    vsSource <- BS.readFile vsSourcePath

    vs <- GL.createShader GL.VertexShader
    GL.shaderSourceBS vs $= vsSource
    GL.compileShader vs

    vsOK <- GL.get $ GL.compileStatus vs
    unless vsOK $ do
        vlog <- GL.get $ GL.shaderInfoLog vs
        hPutStrLn stderr vlog
        exitFailure

    -- Fragment-Shader laden
    fsSource <- BS.readFile fsSourcePath

    fs <- GL.createShader GL.FragmentShader
    GL.shaderSourceBS fs $= fsSource
    GL.compileShader fs
    fsOK <- GL.get $ GL.compileStatus fs
    unless fsOK $ do
        flog <- GL.get $ GL.shaderInfoLog fs
        hPutStrLn stderr flog
        exitFailure

    -- Shader-Programm erstellen und den Vertex- und Fragment-Shader assoziieren
    program <- GL.createProgram
    GL.attachShader program vs
    GL.attachShader program fs

    -- Input der Vertex-Position
    GL.attribLocation program "vp" $= GL.AttribLocation 0

    -- Programm linken
    GL.linkProgram program
    linkOK <- GL.get $ GL.linkStatus program
    GL.validateProgram program
    status <- GL.get $ GL.validateStatus program
    unless (linkOK && status) $ do
        plog <- GL.get $ GL.programInfoLog program
        putStrLn plog
        exitFailure

    -- Programm "benutzen". In C wuerde man hier glUseProgram benutzen
    GL.currentProgram $= Just program

    return (Shader program (GL.AttribLocation 0))

