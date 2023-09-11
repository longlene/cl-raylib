(in-package :rlgl)

;; Texture parameters (equivalent to OpenGL defines)
(defconstant +texture-wrap-s+ #x2802)
(defconstant +texture-wrap-t+ #x2803)
(defconstant +texture-mag-filter+ #x2800)
(defconstant +texture-min-filter+ #x2801)

(defconstant +texture-filter-nearest+ #x2600)
(defconstant +texture-filter-linear+ #x2601)
(defconstant +texture-filter-mip-nearest+ #x2700)
(defconstant +texture-filter-nearest-mip-linear+ #x2702)
(defconstant +texture-filter-linear-mip-nearest+ #x2701)
(defconstant +texture-filter-mip-linear+ #x2703)
(defconstant +texture-filter-anisotropic+ #x3000 "Anisotropic filter (custom identifier)") 
(defconstant +texture-mipmap-bias-ratio+ #x4000 "Texture mipmap bias, percentage ratio (custom identifier)")

(defconstant +texture-wrap-repeat+ #x2901)
(defconstant +texture-wrap-clamp+ #x812F)
(defconstant +texture-wrap-mirror-repeat+ #x8370)
(defconstant +texture-wrap-mirror-clamp+ #x8742)

;; Matrix modes (equivalent to OpenGL)
(defconstant +modelview+ #x1700)
(defconstant +projection+ #x1701)
(defconstant +texture+ #x1702)

;; Primitive assembly draw modes
(defconstant +lines+ #x0001)
(defconstant +triangles+ #x0004)
(defconstant +quads+ #x0007)

;; GL equivalent data types
(defconstant +unsigned-byte+ #x1401)
(defconstant +float+ #x1406)

;; GL buffer usage hint
(defconstant +stream-draw+ #x88E0)
(defconstant +stream-read+ #x88E1)
(defconstant +stream-copy+ #x88E2)
(defconstant +static-draw+ #x88E4)
(defconstant +static-read+ #x88E5)
(defconstant +static-copy+ #x88E6)
(defconstant +dynamic-draw+ #x88E8)
(defconstant +dynamic-read+ #x88E9)
(defconstant +dynamic-copy+ #x88EA)

;; GL Shader type
(defconstant +fragment-shader+ #x8B30)
(defconstant +vertex-shader+ #x8B31)
(defconstant +compute-shader+ #x91B9)

;; GL blending factors
(defconstant +zero+ #x0 "GL_ZERO")
(defconstant +one+ #x1 "GL_ONE")
(defconstant +src-color+ #x0300 "GL_SRC_COLOR")
(defconstant +one-minus-src-color+ #x0301 "GL_ONE_MINUS_SRC_COLOR")
(defconstant +src-alpha+ #x0302 "GL_SRC_ALPHA")
(defconstant +one-minus-src-alpha+ #x0303 "GL_ONE_MINUS_SRC_ALPHA")
(defconstant +dst-alpha+ #x0304 "GL_DST_ALPHA")
(defconstant +one-minus-dst-alpha+ #x0305 "GL_ONE_MINUS_DST_ALPHA")
(defconstant +dst-color+ #x0306 "GL_DST_COLOR")
(defconstant +one-minus-dst-color+ #x0307 "GL_ONE_MINUS_DST_COLOR")
(defconstant +src-alpha-saturate+ #x0308 "GL_SRC_ALPHA_SATURATE")
(defconstant +constant-color+ #x8001 "GL_CONSTANT_COLOR")
(defconstant +one-minus-constant_color+ #x8002 "GL_ONE_MINUS_CONSTANT_COLOR")
(defconstant +constant-alpha+ #x8003 "GL_CONSTANT_ALPHA")
(defconstant +one-minus-constant-alpha+ #x8004 "GL_ONE_MINUS_CONSTANT_ALPHA")

;; gl blending functions/equations
(defconstant +func-add+ #x8006 "GL_FUNC_ADD")
(defconstant +min+ #x8007 "GL_MIN")
(defconstant +max+ #x8008 "GL_MAX")
(defconstant +func-subtract+ #x800A "GL_FUNC_SUBTRACT")
(defconstant +func-reverse_subtract+ #x800B "GL_FUNC_REVERSE_SUBTRACT")
(defconstant +blend-equation+ #x8009 "GL_BLEND_EQUATION")
(defconstant +blend-equation-rgb+ #x8009 "GL_BLEND_EQUATION_RGB)   (Same as BLEND_EQUATION")
(defconstant +blend-equation-alpha+ #x883D "GL_BLEND_EQUATION_ALPHA")
(defconstant +blend-dst-rgb+ #x80C8 "GL_BLEND_DST_RGB")
(defconstant +blend-src-rgb+ #x80C9 "GL_BLEND_SRC_RGB")
(defconstant +blend-dst-alpha+ #x80CA "GL_BLEND_DST_ALPHA")
(defconstant +blend-src-alpha+ #x80CB "GL_BLEND_SRC_ALPHA")
(defconstant +blend-color+ #x8005 "GL_BLEND_COLOR")

(defcstruct (%matrix :class %matrix-class)
  "Matrix, 4x4 components, column major, OpenGL style, right handed"
  (m0 :float) (m4 :float) (m8 :float)  (m12 :float)
  (m1 :float) (m5 :float) (m9 :float)  (m13 :float)
  (m2 :float) (m6 :float) (m10 :float) (m14 :float)
  (m3 :float) (m7 :float) (m11 :float) (m15 :float))

(define-conversion-into-foreign-memory ((object 3d-matrices:mat4) (type %matrix-class) pointer)
  (with-foreign-slots ((m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15) pointer (:struct %matrix))
    (setf
     ;; row 1
     m0  (3d-matrices:miref4 object 0)
     m4  (3d-matrices:miref4 object 1)
     m8  (3d-matrices:miref4 object 2)
     m12 (3d-matrices:miref4 object 3)
     ;; row 2
     m1  (3d-matrices:miref4 object 4)
     m5  (3d-matrices:miref4 object 5)
     m9  (3d-matrices:miref4 object 6)
     m13 (3d-matrices:miref4 object 7)
     ;; row 3
     m2  (3d-matrices:miref4 object 8)
     m6  (3d-matrices:miref4 object 9)
     m10 (3d-matrices:miref4 object 10)
     m14 (3d-matrices:miref4 object 11)
     ;; row 4
     m3  (3d-matrices:miref4 object 12)
     m7  (3d-matrices:miref4 object 13)
     m11 (3d-matrices:miref4 object 14)
     m15 (3d-matrices:miref4 object 15))))

(define-conversion-from-foreign (pointer (type %matrix-class))
  (with-foreign-slots ((m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15) pointer (:struct %matrix))
    (3d-matrices:mat m0 m4 m8 m12
                     m1 m5 m9 m13
                     m2 m6 m10 m14
                     m3 m7 m11 m15)))

(defcstruct %VertexBuffer
  "Dynamic vertex buffers (position + texcoords + colors + indices arrays)"
  (element-count :int)
  (vertices (:pointer :float))
  (texcoords (:pointer :float))
  (colors (:pointer :unsigned-char))
  (indices (:pointer :unsigned-int))
  (vao-id :unsigned-int)
  (vbo-id :unsigned-int :count 4))

(defcstruct %DrawCall
  "Draw call type
NOTE: Only texture changes register a new draw, other state-change-related elements are not
used at this moment (vaoId, shaderId, matrices), raylib just forces a batch draw call if any
of those state-change happens (this is done in core module)"
  (mode :int)
  (vertex-count :int)
  (vertex-alignment :int)
  (texture-id :unsigned-int))

(defcstruct %RenderBatch
  (buffer-count :int)
  (current-buffer :int)
  (vertex-buffer (:pointer (:struct %VertexBuffer)))
  (draws (:pointer (:struct %DrawCall)))
  (draw-counter :int)
  (current-depth :float))

(defcenum GLVersion
  (:opengl-11 1)
  (:opengl-21 2)
  (:opengl-33 3)
  (:opengl-43 4)
  (:opengl-es-20 5))

(defcenum FramebufferAttachType
  (:attachment-color-channel0 0)
  (:attachment-color-channel1 1)
  (:attachment-color-channel2 2)
  (:attachment-color-channel3 3)
  (:attachment-color-channel4 4)
  (:attachment-color-channel5 5)
  (:attachment-color-channel6 6)
  (:attachment-color-channel7 7)
  (:attachment-depth 100)
  (:attachment-stencil 200))

(defcenum FramebufferAttachTextureType
  (:attachment-cubemap-positive-x 0)
  (:attachment-cubemap-negative-x 1)
  (:attachment-cubemap-positive-y 2)
  (:attachment-cubemap-negative-y 3)
  (:attachment-cubemap-positive-z 4)
  (:attachment-cubemap-negative-z 5)
  (:attachment-texture2d 100)
  (:attachment-renderbuffer 200))

(defcenum TracelogLevel
  (:log-all 0)
  (:log-trace 1)
  (:log-debug 2)
  (:log-info 3)
  (:log-warning 4)
  (:log-error 5)
  (:log-fatal 6)
  (:log-none 7))

(defcenum PixelFormat
  (:pixelformat-uncompressed-grayscale 1)
  (:pixelformat-uncompressed-gray-alpha 2)
  (:pixelformat-uncompressed-r5g6b5 3)
  (:pixelformat-uncompressed-r8g8b8 4)
  (:pixelformat-uncompressed-r5g5b5a1 5)
  (:pixelformat-uncompressed-r4g4b4a4 6)
  (:pixelformat-uncompressed-r8g8b8a8 7)
  (:pixelformat-uncompressed-r32 8)
  (:pixelformat-uncompressed-r32g32b32 9)
  (:pixelformat-uncompressed-r32g32b32a32 10)
  (:pixelformat-compressed-dxt1-rgb 11)
  (:pixelformat-compressed-dxt1-rgba 12)
  (:pixelformat-compressed-dxt3-rgba 13)
  (:pixelformat-compressed-dxt5-rgba 14)
  (:pixelformat-compressed-etc1-rgb 15)
  (:pixelformat-compressed-etc2-rgb 16)
  (:pixelformat-compressed-etc2-eac-rgba 17)
  (:pixelformat-compressed-pvrt-rgb 18)
  (:pixelformat-compressed-pvrt-rgba 19)
  (:pixelformat-compressed-astc-4x4-rgba 20)
  (:pixelformat-compressed-astc-8x8-rgba 21))

(defcenum TextureFilter
  (:texture-filter-point 0)
  (:texture-filter-bilinear 1)
  (:texture-filter-trilinear 2)
  (:texture-filter-anisotropic-4x 3)
  (:texture-filter-anisotropic-8x 4)
  (:texture-filter-anisotropic-16x 5))

(defcenum BlendMode
  (:blend-alpha 0)
  (:blend-additive 1)
  (:blend-multiplied 2)
  (:blend-add-colors 3)
  (:blend-subtract-colors 4)
  (:blend-alpha-premul 5)
  (:blend-custom 6))

(defcenum ShaderLocationIndex
  (:shader-loc-vertex-position 0)
  (:shader-loc-vertex-texcoord01 1)
  (:shader-loc-vertex-texcoord02 2)
  (:shader-loc-vertex-normal 3)
  (:shader-loc-vertex-tangent 4)
  (:shader-loc-vertex-color 5)
  (:shader-loc-matrix-mvp 6)
  (:shader-loc-matrix-view 7)
  (:shader-loc-matrix-projection 8)
  (:shader-loc-matrix-model 9)
  (:shader-loc-matrix-normal 10)
  (:shader-loc-vector-view 11)
  (:shader-loc-color-diffuse 12)
  (:shader-loc-color-specular 13)
  (:shader-loc-color-ambient 14)
  (:shader-loc-map-albedo 15)
  (:shader-loc-map-metalness 16)
  (:shader-loc-map-normal 17)
  (:shader-loc-map-roughness 18)
  (:shader-loc-map-occlusion 19)
  (:shader-loc-map-emission 20)
  (:shader-loc-map-height 21)
  (:shader-loc-map-cubemap 22)
  (:shader-loc-map-irradiance 23)
  (:shader-loc-map-prefilter 24)
  (:shader-loc-map-brdf 25))

(defcenum ShaderUniformDataType
  (:shader-uniform-float 0)
  (:shader-uniform-vec2 1)
  (:shader-uniform-vec3 2)
  (:shader-uniform-vec4 3)
  (:shader-uniform-int 4)
  (:shader-uniform-ivec2 5)
  (:shader-uniform-ivec3 6)
  (:shader-uniform-ivec4 7)
  (:shader-uniform-sampler2d 8))

(defcenum ShaderAttributeDataType
  (:shader-attrib-float 0)
  (:shader-attrib-vec2 1)
  (:shader-attrib-vec3 2)
  (:shader-attrib-vec4 3))

(defcenum RlDataType
  (:unsigned-byte #x1401)
  (:float #x1406))

(defcfun ("rlMatrixMode" matrix-mode) :void
  "Choose the current matrix to be transformed"
  (mode :int))

(defcfun ("rlPushMatrix" push-matrix) :void
  "Push the current matrix to stack")

(defcfun ("rlPopMatrix" pop-matrix) :void
  "Pop latest inserted matrix from stack")

(defcfun ("rlLoadIdentity" load-identity) :void
  "Reset current matrix to identity matrix")

(defcfun ("rlTranslatef" translate-f) :void
  "Multiply the current matrix by a translation matrix"
  (x :float)
  (y :float)
  (z :float))

(defcfun ("rlRotatef" rotate-f) :void
  "Multiply the current matrix by a rotation matrix"
  (angle :float)
  (x :float)
  (y :float)
  (z :float))

(defcfun ("rlScalef" scale-f) :void
  "Multiply the current matrix by a scaling matrix"
  (x :float)
  (y :float)
  (z :float))

(defcfun ("rlMultMatrixf" mult-matrix-f) :void
  "Multiply the current matrix by another matrix"
  (matf (:pointer :float)))

(defcfun ("rlFrustum" frustum) :void
  (left :double)
  (right :double)
  (bottom :double)
  (top :double)
  (znear :double)
  (zfar :double))

(defcfun ("rlOrtho" ortho) :void
  (left :double)
  (right :double)
  (bottom :double)
  (top :double)
  (znear :double)
  (zfar :double))

(defcfun ("rlViewport" viewport) :void
  "Set the viewport area"
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defcfun ("rlBegin" begin) :void
  "Initialize drawing mode (how to organize vertex)"
  (mode :int))

(defcfun ("rlEnd" end) :void
  "Finish vertex providing")

(defcfun ("rlVertex2i" vertex-2i) :void
  "Define one vertex (position) - 2 int"
  (x :int)
  (y :int))

(defcfun ("rlVertex2f" vertex-2f) :void
  "Define one vertex (position) - 2 float"
  (x :float)
  (y :float))

(defcfun ("rlVertex3f" vertex-3f) :void
  "Define one vertex (position) - 3 float"
  (x :float)
  (y :float)
  (z :float))

(defcfun ("rlTexCoord2f" texcoord-2f) :void
  "Define one vertex (texture coordinate) - 2 float"
  (x :float)
  (y :float))

(defcfun ("rlNormal3f" normal-3f) :void
  "Define one vertex (normal) - 3 float"
  (x :float)
  (y :float)
  (z :float))

(defcfun ("rlColor4ub" color-4ub) :void
  "Define one vertex (color) - 4 byte"
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char)
  (a :unsigned-char))

(defcfun ("rlColor3f" color-3f) :void
  "Define one vertex (color) - 3 float"
  (x :float)
  (y :float)
  (z :float))

(defcfun ("rlColor4f" color-4f) :void
  "Define one vertex (color) - 4 float"
  (x :float)
  (y :float)
  (z :float)
  (w :float))

(defcfun ("rlEnableVertexArray" enable-vertex-array) :bool
  "Enable vertex array (VAO, if supported)"
  (vaoid :unsigned-int))

(defcfun ("rlDisableVertexArray" disable-vertex-array) :void
  "Disable vertex array (VAO, if supported)")

(defcfun ("rlEnableVertexBuffer" enable-vertex-buffer) :void
  "Enable vertex buffer (VBO)"
  (id :unsigned-int))

(defcfun ("rlDisableVertexBuffer" disable-vertex-buffer) :void
  "Disable vertex buffer (VBO)")

(defcfun ("rlEnableVertexBufferElement" enable-vertex-buffer-element) :void
  "Enable vertex buffer element (VBO element)"
  (id :unsigned-int))

(defcfun ("rlDisableVertexBufferElement" disable-vertex-buffer-element) :void
  "Disable vertex buffer element (VBO element)")

(defcfun ("rlEnableVertexAttribute" enable-vertex-attribute) :void
  "Enable vertex attribute index"
  (index :unsigned-int))

(defcfun ("rlDisableVertexAttribute" disable-vertex-attribute) :void
  "Disable vertex attribute index"
  (index :unsigned-int))

(defcfun ("rlActiveTextureSlot" active-texture-slot) :void
  "Select and active a texture slot"
  (slot :int))

(defcfun ("rlEnableTexture" enable-texture) :void
  "Enable texture"
  (id :unsigned-int))

(defcfun ("rlDisableTexture" disable-texture) :void
  "Disable texture")

(defcfun ("rlEnableTextureCubemap" enable-texture-cubemap) :void
  "Enable texture cubemap"
  (id :unsigned-int))

(defcfun ("rlDisableTextureCubemap" disable-texture-cubemap) :void
  "Disable texture cubemap")

(defcfun ("rlTextureParameters" texture-parameters) :void
  "Set texture parameters (filter, wrap)"
  (id :unsigned-int)
  (param :int)
  (value :int))

(defcfun ("rlCubemapParameters" cubemap-parameters) :void
  "Set cubemap parameters (filter, wrap)"
  (id :unsigned-int)
  (param :int)
  (value :int))

(defcfun ("rlEnableShader" enable-shader) :void
  "Enable shader program"
  (id :unsigned-int))

(defcfun ("rlDisableShader" disable-shader) :void
  "Disable shader program")

(defcfun ("rlEnableFramebuffer" enable-framebuffer) :void
  "Enable render texture (fbo)"
  (id :unsigned-int))

(defcfun ("rlDisableFramebuffer" disable-framebuffer) :void
  "Disable render texture (fbo), return to default framebuffer")

(defcfun ("rlActiveDrawBuffers" active-draw-buffers) :void
  "Activate multiple draw color buffers"
  (count :int))

(defcfun ("rlEnableColorBlend" enable-color-blend) :void
  "Enable color blending")

(defcfun ("rlDisableColorBlend" disable-color-blend) :void
  "Disable color blending")

(defcfun ("rlEnableDepthTest" enable-depth-test) :void
  "Enable depth test")

(defcfun ("rlDisableDepthTest" disable-depth-test) :void
  "Disable depth test")

(defcfun ("rlEnableDepthMask" enable-depth-mask) :void
  "Enable depth write")

(defcfun ("rlDisableDepthMask" disable-depth-mask) :void
  "Disable depth write")

(defcfun ("rlEnableBackfaceCulling" enable-backface-culling) :void
  "Enable backface culling")

(defcfun ("rlDisableBackfaceCulling" disable-backface-culling) :void
  "Disable backface culling")

(defcfun ("rlSetCullFace" set-cull-face) :void
  "Set face culling mode"
  (mode :int))

(defcfun ("rlEnableScissorTest" enable-scissor-test) :void
  "Enable scissor test")

(defcfun ("rlDisableScissorTest" disable-scissor-test) :void
  "Disable scissor test")

(defcfun ("rlScissor" scissor) :void
  "Scissor test"
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defcfun ("rlEnableWireMode" enable-wire-mode) :void
  "Enable wire mode")

(defcfun ("rlDisableWireMode" disable-wire-mode) :void
  "Disable wire mode")

(defcfun ("rlSetLineWidth" set-line-width) :void
  "Set the line drawing width"
  (width :float))

(defcfun ("rlGetLineWidth" get-line-width) :float
  "Get the line drawing width")

(defcfun ("rlEnableSmoothLines" enable-smooth-lines) :void
  "Enable line aliasing")

(defcfun ("rlDisableSmoothLines" disable-smooth-lines) :void
  "Disable line aliasing")

(defcfun ("rlEnableStereoRender" enable-stereo-render) :void
  "Enable stereo rendering")

(defcfun ("rlDisableStereoRender" disable-stereo-render) :void
  "Disable stereo rendering")

(defcfun ("rlIsStereoRenderEnabled" is-stereo-render-enabled) :bool
  "Check if stereo render is enabled")

(defcfun ("rlClearColor" clear-color) :void
  "Clear color buffer with color"
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char)
  (a :unsigned-char))

(defcfun ("rlClearScreenBuffers" clear-screen-buffers) :void
  "Clear used screen buffers (color and depth)")

(defcfun ("rlCheckErrors" check-errors) :void
  "Check and log OpenGL error codes")

(defcfun ("rlSetBlendMode" set-blend-mode) :void
  "Set blending mode"
  (mode :int))

(defcfun ("rlSetBlendFactors" set-blend-factors) :void
  "Set blending mode factor and equation (using OpenGL factors)"
  (glsrcfactor :int)
  (gldstfactor :int)
  (glequation :int))

(defcfun ("rlSetBlendFactorsSeparate" set-blend-factors-separate) :void
  "Set blending mode factors and equations separately (using OpenGL factors)"
  (glSrcRGB :int)
  (glDstRGB :int)
  (glSrcAlpha :int)
  (glDstAlpha :int)
  (glEqRGB :int)
  (glEqAlpha :int))

(defcfun ("rlglInit" rlgl-init) :void
  "Initialize rlgl (buffers, shaders, textures, states)"
  (width :int)
  (height :int))

(defcfun ("rlglClose" rlgl-close) :void
  "De-initialize rlgl (buffers, shaders, textures)")

(defcfun ("rlLoadExtensions" load-extensions) :void
  "Load OpenGL extensions (loader function required)"
  (loader (:pointer :void)))

(defcfun ("rlGetVersion" get-version) :int
  "Get current OpenGL version")

(defcfun ("rlSetFramebufferWidth" set-framebuffer-width) :void
  "Set current framebuffer width"
  (width :int))

(defcfun ("rlGetFramebufferWidth" get-framebuffer-width) :int
  "Get default framebuffer width")

(defcfun ("rlSetFramebufferHeight" set-framebuffer-height) :void
  "Set current framebuffer width"
  (height :int))

(defcfun ("rlGetFramebufferHeight" get-framebuffer-height) :int
  "Get default framebuffer height")

(defcfun ("rlGetTextureIdDefault" get-texture-id-default) :unsigned-int
  "Get default texture id")

(defcfun ("rlGetShaderIdDefault" get-shader-id-default) :unsigned-int
  "Get default shader id")

(defcfun ("rlGetShaderLocsDefault" get-shader-locs-default) (:pointer :int)
  "Get default shader locations")

(defcfun ("rlLoadRenderBatch" load-render-batch) (:struct %RenderBatch)
  "Load a render batch system"
  (numbuffers :int)
  (bufferelements :int))

(defcfun ("rlUnloadRenderBatch" unload-render-batch) :void
  "Unload render batch system"
  (batch (:struct %RenderBatch)))

(defcfun ("rlDrawRenderBatch" draw-render-batch) :void
  "Draw render batch data (Update->Draw->Reset)"
  (batch (:pointer (:struct %RenderBatch))))

(defcfun ("rlSetRenderBatchActive" set-render-batch-active) :void
  "Set the active render batch for rlgl (NULL for default internal)"
  (batch (:pointer (:struct %RenderBatch))))

(defcfun ("rlDrawRenderBatchActive" draw-render-batch-active) :void
  "Update and draw internal render batch")

(defcfun ("rlCheckRenderBatchLimit" check-render-batch-limit) :bool
  "Check internal buffer overflow for a given number of vertex"
  (vcount :int))

(defcfun ("rlSetTexture" set-texture) :void
  "Set current texture for render batch and check buffers limits"
  (id :unsigned-int))

(defcfun ("rlLoadVertexArray" load-vertex-array) :unsigned-int)

(defcfun ("rlLoadVertexBuffer" load-vertex-buffer) :unsigned-int
  (buffer (:pointer :void))
  (size :int)
  (dynamic :bool))

(defcfun ("rlLoadVertexBufferElement" load-vertex-buffer-element) :unsigned-int
  (buffer (:pointer :void))
  (size :int)
  (dynamic :bool))

(defcfun ("rlUpdateVertexBuffer" update-vertex-buffer) :void
  (bufferid :unsigned-int)
  (data (:pointer :void))
  (datasize :int)
  (offset :int))

(defcfun ("rlUpdateVertexBufferElements" update-vertex-buffer-elements) :void
  (id :unsigned-int)
  (data (:pointer :void))
  (datasize :int)
  (offset :int))

(defcfun ("rlUnloadVertexArray" unload-vertex-array) :void
  (vaoid :unsigned-int))

(defcfun ("rlUnloadVertexBuffer" unload-vertex-buffer) :void
  (vboid :unsigned-int))

(defcfun ("rlSetVertexAttribute" set-vertex-attribute) :void
  (index :unsigned-int)
  (compsize :int)
  (type RlDataType)
  (normalized :bool)
  (stride :int)
  (pointer :size))

(defcfun ("rlSetVertexAttributeDivisor" set-vertex-attribute-divisor) :void
  (index :unsigned-int)
  (divisor :int))

(defcfun ("rlSetVertexAttributeDefault" set-vertex-attribute-default) :void
  (locindex :int)
  (value (:pointer :void))
  (attribtype :int)
  (count :int))

(defcfun ("rlDrawVertexArray" draw-vertex-array) :void
  (offset :int)
  (count :int))

(defcfun ("rlDrawVertexArrayElements" draw-vertex-array-elements) :void
  (offset :int)
  (count :int)
  (buffer (:pointer :void)))

(defcfun ("rlDrawVertexArrayInstanced" draw-vertex-array-instanced) :void
  (offset :int)
  (count :int)
  (instances :int))

(defcfun ("rlDrawVertexArrayElementsInstanced" draw-vertex-array-elements-instanced) :void
  (offset :int)
  (count :int)
  (buffer (:pointer :void))
  (instances :int))

(defcfun ("rlLoadTexture" load-texture) :unsigned-int
  (data (:pointer :void))
  (width :int)
  (height :int)
  (format :int)
  (mipmapcount :int))

(defcfun ("rlLoadTextureDepth" load-texture-depth) :unsigned-int
  (width :int)
  (height :int)
  (userenderbuffer :bool))

(defcfun ("rlLoadTextureCubemap" load-texture-cubemap) :unsigned-int
  (data (:pointer :void))
  (size :int)
  (format :int))

(defcfun ("rlUpdateTexture" update-texture) :void
  (id :unsigned-int)
  (offsetx :int)
  (offsety :int)
  (width :int)
  (height :int)
  (format :int)
  (data (:pointer :void)))

(defcfun ("rlGetGlTextureFormats" get-gl-texture-formats) :void
  (format :int)
  (glinternalformat (:pointer :int))
  (glformat (:pointer :int))
  (gltype (:pointer :int)))

(defcfun ("rlGetPixelFormatName" get-pixel-format-name) :string
  (format :unsigned-int))

(defcfun ("rlUnloadTexture" unload-texture) :void
  (id :unsigned-int))

(defcfun ("rlGenTextureMipmaps" gen-texture-mipmaps) :void
  (id :unsigned-int)
  (width :int)
  (height :int)
  (format :int)
  (mipmaps (:pointer :int)))

(defcfun ("rlReadTexturePixels" read-texture-pixels) (:pointer :void)
  (id :unsigned-int)
  (width :int)
  (height :int)
  (format :int))

(defcfun ("rlReadScreenPixels" read-screen-pixels) (:pointer :unsigned-char)
  (width :int)
  (height :int))

(defcfun ("rlLoadFramebuffer" load-framebuffer) :unsigned-int
  (width :int)
  (height :int))

(defcfun ("rlFramebufferAttach" framebuffer-attach) :void
  (fboid :unsigned-int)
  (texid :unsigned-int)
  (attachtype FrameBufferAttachType)
  (textype :int)
  (miplevel :int))

(defcfun ("rlFramebufferComplete" framebuffer-complete) :bool
  (id :unsigned-int))

(defcfun ("rlUnloadFramebuffer" unload-framebuffer) :void
  (id :unsigned-int))

(defcfun ("rlLoadShaderCode" load-shader-code) :unsigned-int
  (vscode :string)
  (fscode :string))

(defcfun ("rlCompileShader" compile-shader) :unsigned-int
  (shadercode :string)
  (type :int))

(defcfun ("rlLoadShaderProgram" load-shader-program) :unsigned-int
  (vshaderid :unsigned-int)
  (fshaderid :unsigned-int))

(defcfun ("rlUnloadShaderProgram" unload-shader-program) :void
  (id :unsigned-int))

(defcfun ("rlGetLocationUniform" get-location-uniform) :int
  (shaderid :unsigned-int)
  (uniformname :string))

(defcfun ("rlGetLocationAttrib" get-location-attrib) :int
  (shaderid :unsigned-int)
  (attribname :string))

(defcfun ("rlSetUniform" set-uniform) :void
  (locindex :int)
  (value (:pointer :void))
  (uniformtype :int)
  (count :int))

(defcfun ("rlSetUniformMatrix" set-uniform-matrix) :void
  (locindex :int)
  (mat (:struct %Matrix)))

(defcfun ("rlSetUniformSampler" set-uniform-sampler) :void
  (locindex :int)
  (textureid :unsigned-int))

(defcfun ("rlSetShader" set-shader) :void
  (id :unsigned-int)
  (locs (:pointer :int)))

(defcfun ("rlLoadComputeShaderProgram" load-compute-shader-program) :unsigned-int
  (shaderid :unsigned-int))

(defcfun ("rlComputeShaderDispatch" compute-shader-dispatch) :void
  (groupx :unsigned-int)
  (groupy :unsigned-int)
  (groupz :unsigned-int))

(defcfun ("rlLoadShaderBuffer" load-shader-buffer) :unsigned-int
  (size :unsigned-long-long)
  (data (:pointer :void))
  (usagehint :int))

(defcfun ("rlUnloadShaderBuffer" unload-shader-buffer) :void
  (ssboid :unsigned-int))

(defcfun ("rlUpdateShaderBuffer" update-shader-buffer) :void
  (id :unsigned-int)
  (data (:pointer :void))
  (datasize :unsigned-int)
  (offset :unsigned-int))

(defcfun ("rlBindShaderBuffer" bind-shader-buffer) :void
  (id :unsigned-int)
  (index :unsigned-int))

(defcfun ("rlReadShaderBuffer" read-shader-buffer) :void
  (id :unsigned-int)
  (dest (:pointer :void))
  (count :unsigned-int)
  (offset :unsigned-int))

(defcfun ("rlCopyShaderBuffer" copy-shader-buffers) :void
  (destid :unsigned-int)
  (srcid :unsigned-int)
  (destoffset :unsigned-int)
  (srcoffset :unsigned-int)
  (count :unsigned-int))

(defcfun ("rlGetShaderBufferSize" get-shader-buffer-size) :unsigned-int
  (id :unsigned-int))

(defcfun ("rlBindImageTexture" bind-image-texture) :void
  (id :unsigned-int)
  (index :unsigned-int)
  (format :unsigned-int)
  (readonly :int))

(defcfun ("rlGetMatrixModelview" get-matrix-modelview) (:struct %Matrix))

(defcfun ("rlGetMatrixProjection" get-matrix-projection) (:struct %Matrix))

(defcfun ("rlGetMatrixTransform" get-matrix-transform) (:struct %Matrix))

(defcfun ("rlGetMatrixProjectionStereo" get-matrix-projection-stereo) (:struct %Matrix)
  (eye :int))

(defcfun ("rlGetMatrixViewOffsetStereo" get-matrix-view-offset-stereo) (:struct %Matrix)
  (eye :int))

(defcfun ("rlSetMatrixProjection" set-matrix-projection) :void
  (proj (:struct %Matrix)))

(defcfun ("rlSetMatrixModelview" set-matrix-modelview) :void
  (view (:struct %Matrix)))

(defcfun ("rlSetMatrixProjectionStereo" set-matrix-projection-stereo) :void
  (right (:struct %Matrix))
  (left (:struct %Matrix)))

(defcfun ("rlSetMatrixViewOffsetStereo" set-matrix-view-offset-stereo) :void
  (right (:struct %Matrix))
  (left (:struct %Matrix)))

(defcfun ("rlLoadDrawCube" load-draw-cube) :void)

(defcfun ("rlLoadDrawQuad" load-draw-quad) :void)
