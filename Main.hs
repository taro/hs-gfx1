module Main where
import Prelude hiding (init)
import Control.Monad (liftM)
import Data.Array.Storable (newListArray, withStorableArray)
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
	clearColor $= Color4 1 0 1 0
	texture Texture2D $= Enabled

	clientState VertexArray $= Enabled
	clientState ColorArray $= Enabled
	clientState TextureCoordArray $= Enabled

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
	let stride = 36
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

displayVA va = do
	arr <- newListArray (0, length va - 1) va

	withStorableArray arr (\ptr -> do
		interleavedArrays T2fC3fV3f 0 ptr)

	drawArrays Triangles 0 18

evtLoop = do
	ev <- pollEvent

	case ev of
		Quit -> undefined
		NoEvent -> return ()
		_ -> evtLoop

gfxLoop :: [Float] -> IO ()
gfxLoop verts = do
	clear [ColorBuffer, DepthBuffer]

	displayVA verts

	glSwapBuffers

main = do
	gfxInit 640 480 "hello"
	testImage <- gfxLoad "test-image.jpg"

	let testVerts = [
		0, 0, 1, 1, 1, 0, 0, 0,
		1, 0, 1, 1, 1, 100, 0, 0,
		0, 1, 1, 1, 1, 0, 100, 0,
		1, 0, 1, 1, 1, 100, 0, 0,
		1, 1, 1, 1, 1, 100, 100, 0,
		0, 1, 1, 1, 1, 0, 100, 0]

	sequence_ $ cycle [evtLoop, gfxLoop testVerts]
