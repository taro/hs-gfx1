module Main where
import Prelude hiding (init)
import Graphics.UI.SDL hiding (SrcAlpha)
import Graphics.Rendering.OpenGL.GL

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

	blendEquation $= FuncAdd
	blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
	lighting $= Disabled
	textureFunction $= Modulate
	texture Texture2D $= Enabled
	depthFunc $= Just Less

evtLoop = do
	ev <- waitEvent

	case ev of
		Quit -> return ()
		_ -> evtLoop

main = do
	gfxInit 640 480 "hello"
	evtLoop
