module Main where
import Prelude hiding (init)
import Graphics.UI.SDL hiding (SrcAlpha)
import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.GLU.Matrix (ortho2D)

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

	viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

	matrixMode $= Projection
	loadIdentity
	ortho2D 0 (fromIntegral w) (fromIntegral h) 0

	matrixMode $= Modelview 0
	loadIdentity

evtLoop = do
	ev <- pollEvent

	case ev of
		Quit -> undefined
		NoEvent -> return ()
		_ -> evtLoop

gfxLoop = do
	clear [ColorBuffer, DepthBuffer]

	renderPrimitive Triangles $ do
		color $ Color3 1 0 (0 :: GLfloat)
		vertex $ Vertex3 0 0 (0 :: GLfloat)
		vertex $ Vertex3 100 0 (0 :: GLfloat)
		vertex $ Vertex3 0 100 (0 :: GLfloat)
		vertex $ Vertex3 100 0 (0 :: GLfloat)
		vertex $ Vertex3 100 100 (0 :: GLfloat)
		vertex $ Vertex3 0 100 (0 :: GLfloat)

	glSwapBuffers

main = do
	gfxInit 640 480 "hello"
	sequence_ $ cycle [evtLoop, gfxLoop]
