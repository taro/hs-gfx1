module Main where
import Prelude hiding (init)
import Control.Monad (liftM)
import Data.Array.MArray (newListArray)
import Data.Array.Storable (withStorableArray)
import Foreign.Ptr (nullPtr, plusPtr)
import Graphics.UI.SDL hiding (SrcAlpha)
import Graphics.UI.SDL.Image (load)
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
	texture Texture2D $= Enabled
	clientState VertexArray $= Enabled

	viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

	matrixMode $= Projection
	loadIdentity
	ortho2D 0 (fromIntegral w) (fromIntegral h) 0

	matrixMode $= Modelview 0
	loadIdentity

gfxLoad path = do
	src <- load path

	let srcW = surfaceGetWidth src
	let srcH = surfaceGetHeight src
	let maskR = 0x000000FF
	let maskG = 0x0000FF00
	let maskB = 0x00FF0000
	let maskA = 0xFF000000

	dst <- createRGBSurface [SWSurface] srcW srcH 32 maskR maskG maskB maskA 
		
	let dstFormat = surfaceGetPixelFormat dst
	magenta <- mapRGB dstFormat 255 0 255
	alpha <- mapRGBA dstFormat 0 0 0 0

	dstPixels <- surfaceGetPixels dst
	setColorKey src [SrcColorKey] magenta 
	fillRect dst Nothing alpha 
	blitSurface src Nothing dst Nothing
	
	[tex] <- genObjectNames 1

	textureBinding Texture2D $= Just tex
	textureWrapMode Texture2D S $= (Repeated, Repeat)
	textureWrapMode Texture2D T $= (Repeated, Repeat)
	textureFilter Texture2D $= ((Linear', Nothing), Linear')

	let texSize = TextureSize2D (fromIntegral srcW) (fromIntegral srcH)
	let texPixels = PixelData RGBA UnsignedByte dstPixels

	texImage2D Nothing NoProxy 0 RGBA' texSize 0 texPixels 

	freeSurface src
	freeSurface dst

	return tex

listToVBO :: [Float] -> IO BufferObject
listToVBO elems = do
	let size = length elems
	let ptrSize = toEnum $ size * 4
	
	[buf] <- genObjectNames 1
	bindBuffer ElementArrayBuffer $= Just buf

	arr <- newListArray (0, size - 1) elems

	withStorableArray arr (\ptr ->
		bufferData ElementArrayBuffer $= (ptrSize, ptr, StaticDraw))

	bindBuffer ArrayBuffer $= Nothing
	return buf

displayVbo buf size = do
	let stride = 9
	let offset = plusPtr nullPtr
	let vxPos = VertexArrayDescriptor 3 Float stride $ offset 0
	let vxClr = VertexArrayDescriptor 4 Float stride $ offset 12
	let vxTex = VertexArrayDescriptor 2 Float stride $ offset (12 + 16)

	bindBuffer ArrayBuffer $= Just buf

	arrayPointer VertexArray $= vxPos
	arrayPointer ColorArray $= vxClr
	arrayPointer TextureCoordArray $= vxTex

	drawArrays Triangles 0 size

	bindBuffer ArrayBuffer $= Nothing

evtLoop = do
	ev <- pollEvent

	case ev of
		Quit -> undefined
		NoEvent -> return ()
		_ -> evtLoop

gfxLoop vbo = do
	clear [ColorBuffer, DepthBuffer]

	displayVbo vbo 3

	glSwapBuffers

main = do
	gfxInit 640 480 "hello"
	testImage <- gfxLoad "test-image.jpg"
	testData <- listToVBO [
		0, 0, 0, 1, 1, 1, 0, 0,
		100, 0, 0, 1, 1, 1, 1, 0,
		0, 100, 0, 1, 1, 1, 0, 1,
		100, 0, 0, 1, 1, 1, 1, 0,
		100, 100, 0, 1, 1, 1, 1, 1,
		0, 100, 0, 1, 1, 1, 0, 1]
	sequence_ $ cycle [evtLoop, gfxLoop testData]
