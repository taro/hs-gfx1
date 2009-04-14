module Main where
import Prelude hiding (init)
import Graphics.UI.SDL.Events
import Graphics.UI.SDL.General
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.WindowManagement 

gfxInit w h cap = do
	init [InitEverything]

	let attrs = [
		(glRedSize, 5),
		(glGreenSize, 5),
		(glBlueSize, 5),
		(glDepthSize, 16),
		(glDoubleBuffer, 1)]

	mapM_ (uncurry (glSetAttribute $)) attrs

	screen <- setVideoMode w h 0 [AnyFormat, OpenGL]

	setCaption cap ""

evtLoop = do
	ev <- waitEvent

	case ev of
		Quit -> return ()
		_ -> evtLoop

main = do
	gfxInit 640 480 "hello"
	evtLoop
