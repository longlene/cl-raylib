(in-package #:cl-raylib)

;;;===================================================================================
;;; rlgl v6.0 - A multi-OpenGL abstraction layer with an immediate-mode style API
;;; Port of raylib/src/rlgl.h, GRAPHICS_API_OPENGL_33 path (raylib desktop default)
;;;
;;; NOTE: OpenGL functions are loaded with the loader provided to rlLoadExtensions()
;;; (glfwGetProcAddress), like glad does, see the GL loader section below
;;; NOTE: Matrix values are 3d-matrices mat4 (see math.lisp), treated as immutable values
;;; NOTE: Functions receiving a C data pointer (const void *) accept a foreign pointer,
;;; a specialized Lisp vector (pinned while the GL call runs) or NIL (NULL)
;;;===================================================================================

(defparameter +rlgl-version+ "6.0")

;;;----------------------------------------------------------------------------------
;;; Defines and Macros
;;;----------------------------------------------------------------------------------
;; Default internal render batch elements limits
(defconstant +rl-default-batch-buffer-elements+ 8192 "Maximum amount of elements (quads) per batch")
(defconstant +rl-default-batch-buffers+ 1 "Default number of batch buffers (multi-buffering)")
(defconstant +rl-default-batch-drawcalls+ 256 "Default number of batch draw calls (by state changes: mode, texture)")
(defconstant +rl-default-batch-max-texture-units+ 4 "Maximum number of textures units that can be activated on batch drawing")

;; Internal Matrix stack
(defconstant +rl-max-matrix-stack-size+ 32 "Maximum size of Matrix stack")

;; Shader limits
(defconstant +rl-max-shader-locations+ 32 "Maximum number of shader locations supported")

;; Projection matrix culling
(defconstant +rl-cull-distance-near+ 0.05d0 "Default near cull distance")
(defconstant +rl-cull-distance-far+ 4000.0d0 "Default far cull distance")

;; Texture parameters (equivalent to OpenGL defines)
(defconstant +rl-texture-wrap-s+ #x2802 "GL_TEXTURE_WRAP_S")
(defconstant +rl-texture-wrap-t+ #x2803 "GL_TEXTURE_WRAP_T")
(defconstant +rl-texture-mag-filter+ #x2800 "GL_TEXTURE_MAG_FILTER")
(defconstant +rl-texture-min-filter+ #x2801 "GL_TEXTURE_MIN_FILTER")

(defconstant +rl-texture-filter-nearest+ #x2600 "GL_NEAREST")
(defconstant +rl-texture-filter-linear+ #x2601 "GL_LINEAR")
(defconstant +rl-texture-filter-mip-nearest+ #x2700 "GL_NEAREST_MIPMAP_NEAREST")
(defconstant +rl-texture-filter-nearest-mip-linear+ #x2702 "GL_NEAREST_MIPMAP_LINEAR")
(defconstant +rl-texture-filter-linear-mip-nearest+ #x2701 "GL_LINEAR_MIPMAP_NEAREST")
(defconstant +rl-texture-filter-mip-linear+ #x2703 "GL_LINEAR_MIPMAP_LINEAR")
(defconstant +rl-texture-filter-anisotropic+ #x3000 "Anisotropic filter (custom identifier)")
(defconstant +rl-texture-mipmap-bias-ratio+ #x4000 "Texture mipmap bias, percentage ratio (custom identifier)")

(defconstant +rl-texture-wrap-repeat+ #x2901 "GL_REPEAT")
(defconstant +rl-texture-wrap-clamp+ #x812F "GL_CLAMP_TO_EDGE")
(defconstant +rl-texture-wrap-mirror-repeat+ #x8370 "GL_MIRRORED_REPEAT")
(defconstant +rl-texture-wrap-mirror-clamp+ #x8742 "GL_MIRROR_CLAMP_EXT")

;; Matrix modes (equivalent to OpenGL)
(defconstant +rl-modelview+ #x1700 "GL_MODELVIEW")
(defconstant +rl-projection+ #x1701 "GL_PROJECTION")
(defconstant +rl-texture+ #x1702 "GL_TEXTURE")

;; Primitive assembly draw modes
(defconstant +rl-lines+ #x0001 "GL_LINES")
(defconstant +rl-triangles+ #x0004 "GL_TRIANGLES")
(defconstant +rl-quads+ #x0007 "GL_QUADS")

;; GL equivalent data types
(defconstant +rl-unsigned-byte+ #x1401 "GL_UNSIGNED_BYTE")
(defconstant +rl-float+ #x1406 "GL_FLOAT")

;; GL buffer usage hint
(defconstant +rl-stream-draw+ #x88E0 "GL_STREAM_DRAW")
(defconstant +rl-stream-read+ #x88E1 "GL_STREAM_READ")
(defconstant +rl-stream-copy+ #x88E2 "GL_STREAM_COPY")
(defconstant +rl-static-draw+ #x88E4 "GL_STATIC_DRAW")
(defconstant +rl-static-read+ #x88E5 "GL_STATIC_READ")
(defconstant +rl-static-copy+ #x88E6 "GL_STATIC_COPY")
(defconstant +rl-dynamic-draw+ #x88E8 "GL_DYNAMIC_DRAW")
(defconstant +rl-dynamic-read+ #x88E9 "GL_DYNAMIC_READ")
(defconstant +rl-dynamic-copy+ #x88EA "GL_DYNAMIC_COPY")

;; GL Shader type
(defconstant +rl-fragment-shader+ #x8B30 "GL_FRAGMENT_SHADER")
(defconstant +rl-vertex-shader+ #x8B31 "GL_VERTEX_SHADER")
(defconstant +rl-compute-shader+ #x91B9 "GL_COMPUTE_SHADER")

;; GL blending factors
(defconstant +rl-zero+ 0 "GL_ZERO")
(defconstant +rl-one+ 1 "GL_ONE")
(defconstant +rl-src-color+ #x0300 "GL_SRC_COLOR")
(defconstant +rl-one-minus-src-color+ #x0301 "GL_ONE_MINUS_SRC_COLOR")
(defconstant +rl-src-alpha+ #x0302 "GL_SRC_ALPHA")
(defconstant +rl-one-minus-src-alpha+ #x0303 "GL_ONE_MINUS_SRC_ALPHA")
(defconstant +rl-dst-alpha+ #x0304 "GL_DST_ALPHA")
(defconstant +rl-one-minus-dst-alpha+ #x0305 "GL_ONE_MINUS_DST_ALPHA")
(defconstant +rl-dst-color+ #x0306 "GL_DST_COLOR")
(defconstant +rl-one-minus-dst-color+ #x0307 "GL_ONE_MINUS_DST_COLOR")
(defconstant +rl-src-alpha-saturate+ #x0308 "GL_SRC_ALPHA_SATURATE")
(defconstant +rl-constant-color+ #x8001 "GL_CONSTANT_COLOR")
(defconstant +rl-one-minus-constant-color+ #x8002 "GL_ONE_MINUS_CONSTANT_COLOR")
(defconstant +rl-constant-alpha+ #x8003 "GL_CONSTANT_ALPHA")
(defconstant +rl-one-minus-constant-alpha+ #x8004 "GL_ONE_MINUS_CONSTANT_ALPHA")

;; GL blending functions/equations
(defconstant +rl-func-add+ #x8006 "GL_FUNC_ADD")
(defconstant +rl-min+ #x8007 "GL_MIN")
(defconstant +rl-max+ #x8008 "GL_MAX")
(defconstant +rl-func-subtract+ #x800A "GL_FUNC_SUBTRACT")
(defconstant +rl-func-reverse-subtract+ #x800B "GL_FUNC_REVERSE_SUBTRACT")
(defconstant +rl-blend-equation+ #x8009 "GL_BLEND_EQUATION")
(defconstant +rl-blend-equation-rgb+ #x8009 "GL_BLEND_EQUATION_RGB (Same as BLEND_EQUATION)")
(defconstant +rl-blend-equation-alpha+ #x883D "GL_BLEND_EQUATION_ALPHA")
(defconstant +rl-blend-dst-rgb+ #x80C8 "GL_BLEND_DST_RGB")
(defconstant +rl-blend-src-rgb+ #x80C9 "GL_BLEND_SRC_RGB")
(defconstant +rl-blend-dst-alpha+ #x80CA "GL_BLEND_DST_ALPHA")
(defconstant +rl-blend-src-alpha+ #x80CB "GL_BLEND_SRC_ALPHA")
(defconstant +rl-blend-color+ #x8005 "GL_BLEND_COLOR")

(defconstant +rl-read-framebuffer+ #x8CA8 "GL_READ_FRAMEBUFFER")
(defconstant +rl-draw-framebuffer+ #x8CA9 "GL_DRAW_FRAMEBUFFER")

;; Default shader vertex attribute locations
(defconstant +rl-default-shader-attrib-location-position+ 0)
(defconstant +rl-default-shader-attrib-location-texcoord+ 1)
(defconstant +rl-default-shader-attrib-location-normal+ 2)
(defconstant +rl-default-shader-attrib-location-color+ 3)
(defconstant +rl-default-shader-attrib-location-tangent+ 4)
(defconstant +rl-default-shader-attrib-location-texcoord2+ 5)
(defconstant +rl-default-shader-attrib-location-indices+ 6)
(defconstant +rl-default-shader-attrib-location-boneindices+ 7)
(defconstant +rl-default-shader-attrib-location-boneweights+ 8)
(defconstant +rl-default-shader-attrib-location-instancetransform+ 9)

;;;----------------------------------------------------------------------------------
;;; Types and Structures Definition
;;;----------------------------------------------------------------------------------

;; Dynamic vertex buffers (position + texcoords + colors + indices arrays)
(defstruct rl-vertex-buffer
  (element-count 0 :type fixnum)        ; Number of elements in the buffer (QUADS)
  (vertices (make-array 0 :element-type 'single-float) :type (simple-array single-float (*)))  ; Vertex position (XYZ - 3 components per vertex) (shader-location = 0)
  (texcoords (make-array 0 :element-type 'single-float) :type (simple-array single-float (*))) ; Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
  (normals (make-array 0 :element-type 'single-float) :type (simple-array single-float (*)))   ; Vertex normal (XYZ - 3 components per vertex) (shader-location = 2)
  (colors (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*))) ; Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
  (indices (make-array 0 :element-type '(unsigned-byte 32)) :type (simple-array (unsigned-byte 32) (*))) ; Vertex indices (in case vertex data comes indexed) (6 indices per quad)
  (vao-id 0 :type (unsigned-byte 32))   ; OpenGL Vertex Array Object id
  (vbo-id (make-array 5 :element-type '(unsigned-byte 32) :initial-element 0)
   :type (simple-array (unsigned-byte 32) (5)))) ; OpenGL Vertex Buffer Objects id (5 types of vertex data)

;; Draw call type
;; NOTE: Only texture changes register a new draw, other state-change-related elements are not
;; used at this moment (vaoId, shaderId, matrices), raylib just forces a batch draw call if any
;; of those state-change happens (this is done in core module)
(defstruct rl-draw-call
  (mode 0 :type fixnum)                 ; Drawing mode: LINES, TRIANGLES, QUADS
  (vertex-count 0 :type fixnum)         ; Number of vertex of the draw
  (vertex-alignment 0 :type fixnum)     ; Number of vertex required for index alignment (LINES, TRIANGLES)
  (texture-id 0 :type (unsigned-byte 32))) ; Texture id to be used on the draw -> Use to create new draw call if changes

;; rlRenderBatch type
(defstruct rl-render-batch
  (buffer-count 0 :type fixnum)         ; Number of vertex buffers (multi-buffering support)
  (current-buffer 0 :type fixnum)       ; Current buffer tracking in case of multi-buffering
  (vertex-buffer #() :type simple-vector) ; Dynamic buffer(s) for vertex data
  (draws #() :type simple-vector)       ; Draw calls array, depends on textureId
  (draw-counter 0 :type fixnum)         ; Draw calls counter
  (current-depth 0.0 :type single-float)) ; Current depth value for next draw

;; OpenGL version
(defconstant +rl-opengl-software+ 0 "Software rendering")
(defconstant +rl-opengl-11+ 1 "OpenGL 1.1")
(defconstant +rl-opengl-21+ 2 "OpenGL 2.1 (GLSL 120)")
(defconstant +rl-opengl-33+ 3 "OpenGL 3.3 (GLSL 330)")
(defconstant +rl-opengl-43+ 4 "OpenGL 4.3 (using GLSL 330)")
(defconstant +rl-opengl-es-20+ 5 "OpenGL ES 2.0 (GLSL 100)")
(defconstant +rl-opengl-es-30+ 6 "OpenGL ES 3.0 (GLSL 300 es)")

;; Trace log level
(defconstant +rl-log-all+ 0 "Display all logs")
(defconstant +rl-log-trace+ 1 "Trace logging, intended for internal use only")
(defconstant +rl-log-debug+ 2 "Debug logging, used for internal debugging")
(defconstant +rl-log-info+ 3 "Info logging, used for program execution info")
(defconstant +rl-log-warning+ 4 "Warning logging, used on recoverable failures")
(defconstant +rl-log-error+ 5 "Error logging, used on unrecoverable failures")
(defconstant +rl-log-fatal+ 6 "Fatal logging, used to abort program")
(defconstant +rl-log-none+ 7 "Disable logging")

;; Texture pixel formats
(defconstant +rl-pixelformat-uncompressed-grayscale+ 1 "8 bit per pixel (no alpha)")
(defconstant +rl-pixelformat-uncompressed-gray-alpha+ 2 "8*2 bpp (2 channels)")
(defconstant +rl-pixelformat-uncompressed-r5g6b5+ 3 "16 bpp")
(defconstant +rl-pixelformat-uncompressed-r8g8b8+ 4 "24 bpp")
(defconstant +rl-pixelformat-uncompressed-r5g5b5a1+ 5 "16 bpp (1 bit alpha)")
(defconstant +rl-pixelformat-uncompressed-r4g4b4a4+ 6 "16 bpp (4 bit alpha)")
(defconstant +rl-pixelformat-uncompressed-r8g8b8a8+ 7 "32 bpp")
(defconstant +rl-pixelformat-uncompressed-r32+ 8 "32 bpp (1 channel - float)")
(defconstant +rl-pixelformat-uncompressed-r32g32b32+ 9 "32*3 bpp (3 channels - float)")
(defconstant +rl-pixelformat-uncompressed-r32g32b32a32+ 10 "32*4 bpp (4 channels - float)")
(defconstant +rl-pixelformat-uncompressed-r16+ 11 "16 bpp (1 channel - half float)")
(defconstant +rl-pixelformat-uncompressed-r16g16b16+ 12 "16*3 bpp (3 channels - half float)")
(defconstant +rl-pixelformat-uncompressed-r16g16b16a16+ 13 "16*4 bpp (4 channels - half float)")
(defconstant +rl-pixelformat-compressed-dxt1-rgb+ 14 "4 bpp (no alpha)")
(defconstant +rl-pixelformat-compressed-dxt1-rgba+ 15 "4 bpp (1 bit alpha)")
(defconstant +rl-pixelformat-compressed-dxt3-rgba+ 16 "8 bpp")
(defconstant +rl-pixelformat-compressed-dxt5-rgba+ 17 "8 bpp")
(defconstant +rl-pixelformat-compressed-etc1-rgb+ 18 "4 bpp")
(defconstant +rl-pixelformat-compressed-etc2-rgb+ 19 "4 bpp")
(defconstant +rl-pixelformat-compressed-etc2-eac-rgba+ 20 "8 bpp")
(defconstant +rl-pixelformat-compressed-pvrt-rgb+ 21 "4 bpp")
(defconstant +rl-pixelformat-compressed-pvrt-rgba+ 22 "4 bpp")
(defconstant +rl-pixelformat-compressed-astc-4x4-rgba+ 23 "8 bpp")
(defconstant +rl-pixelformat-compressed-astc-8x8-rgba+ 24 "2 bpp")

;; Texture parameters: filter mode
(defconstant +rl-texture-filter-point+ 0 "No filter, pixel approximation")
(defconstant +rl-texture-filter-bilinear+ 1 "Linear filtering")
(defconstant +rl-texture-filter-trilinear+ 2 "Trilinear filtering (linear with mipmaps)")
(defconstant +rl-texture-filter-anisotropic-4x+ 3 "Anisotropic filtering 4x")
(defconstant +rl-texture-filter-anisotropic-8x+ 4 "Anisotropic filtering 8x")
(defconstant +rl-texture-filter-anisotropic-16x+ 5 "Anisotropic filtering 16x")

;; Color blending modes (pre-defined)
(defconstant +rl-blend-alpha+ 0 "Blend textures considering alpha (default)")
(defconstant +rl-blend-additive+ 1 "Blend textures adding colors")
(defconstant +rl-blend-multiplied+ 2 "Blend textures multiplying colors")
(defconstant +rl-blend-add-colors+ 3 "Blend textures adding colors (alternative)")
(defconstant +rl-blend-subtract-colors+ 4 "Blend textures subtracting colors (alternative)")
(defconstant +rl-blend-alpha-premultiply+ 5 "Blend premultiplied textures considering alpha")
(defconstant +rl-blend-custom+ 6 "Blend textures using custom src/dst factors (use rlSetBlendFactors())")
(defconstant +rl-blend-custom-separate+ 7 "Blend textures using custom src/dst factors (use rlSetBlendFactorsSeparate())")

;; Shader location point type
(defconstant +rl-shader-loc-vertex-position+ 0 "Shader location: vertex attribute: position")
(defconstant +rl-shader-loc-vertex-texcoord01+ 1 "Shader location: vertex attribute: texcoord01")
(defconstant +rl-shader-loc-vertex-texcoord02+ 2 "Shader location: vertex attribute: texcoord02")
(defconstant +rl-shader-loc-vertex-normal+ 3 "Shader location: vertex attribute: normal")
(defconstant +rl-shader-loc-vertex-tangent+ 4 "Shader location: vertex attribute: tangent")
(defconstant +rl-shader-loc-vertex-color+ 5 "Shader location: vertex attribute: color")
(defconstant +rl-shader-loc-matrix-mvp+ 6 "Shader location: matrix uniform: model-view-projection")
(defconstant +rl-shader-loc-matrix-view+ 7 "Shader location: matrix uniform: view (camera transform)")
(defconstant +rl-shader-loc-matrix-projection+ 8 "Shader location: matrix uniform: projection")
(defconstant +rl-shader-loc-matrix-model+ 9 "Shader location: matrix uniform: model (transform)")
(defconstant +rl-shader-loc-matrix-normal+ 10 "Shader location: matrix uniform: normal")
(defconstant +rl-shader-loc-vector-view+ 11 "Shader location: vector uniform: view")
(defconstant +rl-shader-loc-color-diffuse+ 12 "Shader location: vector uniform: diffuse color")
(defconstant +rl-shader-loc-color-specular+ 13 "Shader location: vector uniform: specular color")
(defconstant +rl-shader-loc-color-ambient+ 14 "Shader location: vector uniform: ambient color")
(defconstant +rl-shader-loc-map-albedo+ 15 "Shader location: sampler2d texture: albedo (same as: RL_SHADER_LOC_MAP_DIFFUSE)")
(defconstant +rl-shader-loc-map-metalness+ 16 "Shader location: sampler2d texture: metalness (same as: RL_SHADER_LOC_MAP_SPECULAR)")
(defconstant +rl-shader-loc-map-normal+ 17 "Shader location: sampler2d texture: normal")
(defconstant +rl-shader-loc-map-roughness+ 18 "Shader location: sampler2d texture: roughness")
(defconstant +rl-shader-loc-map-occlusion+ 19 "Shader location: sampler2d texture: occlusion")
(defconstant +rl-shader-loc-map-emission+ 20 "Shader location: sampler2d texture: emission")
(defconstant +rl-shader-loc-map-height+ 21 "Shader location: sampler2d texture: height")
(defconstant +rl-shader-loc-map-cubemap+ 22 "Shader location: samplerCube texture: cubemap")
(defconstant +rl-shader-loc-map-irradiance+ 23 "Shader location: samplerCube texture: irradiance")
(defconstant +rl-shader-loc-map-prefilter+ 24 "Shader location: samplerCube texture: prefilter")
(defconstant +rl-shader-loc-map-brdf+ 25 "Shader location: sampler2d texture: brdf")

(defconstant +rl-shader-loc-map-diffuse+ +rl-shader-loc-map-albedo+)
(defconstant +rl-shader-loc-map-specular+ +rl-shader-loc-map-metalness+)

;; Shader uniform data type
(defconstant +rl-shader-uniform-float+ 0 "Shader uniform type: float")
(defconstant +rl-shader-uniform-vec2+ 1 "Shader uniform type: vec2 (2 float)")
(defconstant +rl-shader-uniform-vec3+ 2 "Shader uniform type: vec3 (3 float)")
(defconstant +rl-shader-uniform-vec4+ 3 "Shader uniform type: vec4 (4 float)")
(defconstant +rl-shader-uniform-int+ 4 "Shader uniform type: int")
(defconstant +rl-shader-uniform-ivec2+ 5 "Shader uniform type: ivec2 (2 int)")
(defconstant +rl-shader-uniform-ivec3+ 6 "Shader uniform type: ivec3 (3 int)")
(defconstant +rl-shader-uniform-ivec4+ 7 "Shader uniform type: ivec4 (4 int)")
(defconstant +rl-shader-uniform-uint+ 8 "Shader uniform type: unsigned int")
(defconstant +rl-shader-uniform-uivec2+ 9 "Shader uniform type: uivec2 (2 unsigned int)")
(defconstant +rl-shader-uniform-uivec3+ 10 "Shader uniform type: uivec3 (3 unsigned int)")
(defconstant +rl-shader-uniform-uivec4+ 11 "Shader uniform type: uivec4 (4 unsigned int)")
(defconstant +rl-shader-uniform-sampler2d+ 12 "Shader uniform type: sampler2d")

;; Shader attribute data types
(defconstant +rl-shader-attrib-float+ 0 "Shader attribute type: float")
(defconstant +rl-shader-attrib-vec2+ 1 "Shader attribute type: vec2 (2 float)")
(defconstant +rl-shader-attrib-vec3+ 2 "Shader attribute type: vec3 (3 float)")
(defconstant +rl-shader-attrib-vec4+ 3 "Shader attribute type: vec4 (4 float)")

;; Framebuffer attachment type
;; NOTE: By default up to 8 color channels defined, but it can be more
(defconstant +rl-attachment-color-channel0+ 0 "Framebuffer attachment type: color 0")
(defconstant +rl-attachment-color-channel1+ 1 "Framebuffer attachment type: color 1")
(defconstant +rl-attachment-color-channel2+ 2 "Framebuffer attachment type: color 2")
(defconstant +rl-attachment-color-channel3+ 3 "Framebuffer attachment type: color 3")
(defconstant +rl-attachment-color-channel4+ 4 "Framebuffer attachment type: color 4")
(defconstant +rl-attachment-color-channel5+ 5 "Framebuffer attachment type: color 5")
(defconstant +rl-attachment-color-channel6+ 6 "Framebuffer attachment type: color 6")
(defconstant +rl-attachment-color-channel7+ 7 "Framebuffer attachment type: color 7")
(defconstant +rl-attachment-depth+ 100 "Framebuffer attachment type: depth")
(defconstant +rl-attachment-stencil+ 200 "Framebuffer attachment type: stencil")

;; Framebuffer texture attachment type
(defconstant +rl-attachment-cubemap-positive-x+ 0 "Framebuffer texture attachment type: cubemap, +X side")
(defconstant +rl-attachment-cubemap-negative-x+ 1 "Framebuffer texture attachment type: cubemap, -X side")
(defconstant +rl-attachment-cubemap-positive-y+ 2 "Framebuffer texture attachment type: cubemap, +Y side")
(defconstant +rl-attachment-cubemap-negative-y+ 3 "Framebuffer texture attachment type: cubemap, -Y side")
(defconstant +rl-attachment-cubemap-positive-z+ 4 "Framebuffer texture attachment type: cubemap, +Z side")
(defconstant +rl-attachment-cubemap-negative-z+ 5 "Framebuffer texture attachment type: cubemap, -Z side")
(defconstant +rl-attachment-texture2d+ 100 "Framebuffer texture attachment type: texture2d")
(defconstant +rl-attachment-renderbuffer+ 200 "Framebuffer texture attachment type: renderbuffer")

;; Face culling mode
(defconstant +rl-cull-face-front+ 0)
(defconstant +rl-cull-face-back+ 1)

;;;===================================================================================
;;; RLGL IMPLEMENTATION
;;;===================================================================================

;;;----------------------------------------------------------------------------------
;;; OpenGL functions loader (replaces glad)
;;;
;;; DEFGLFUN defines a Lisp function calling the GL entry point through a pointer
;;; obtained from the rlLoadExtensions() loader, with float traps masked (C semantics)
;;;----------------------------------------------------------------------------------
(defvar *gl-functions* nil "List of (gl-name . pointer-variable) loaded by %gl-load-functions")

(defmacro defglfun ((cname lname) result-type &rest args)
  (let ((pointer (alexandria:symbolicate "*" lname "-POINTER*")))
    `(progn
       (defvar ,pointer (cffi:null-pointer))
       (pushnew '(,cname . ,pointer) *gl-functions* :test #'equal)
       (declaim (inline ,lname))
       (defun ,lname ,(mapcar #'first args)
         (float-features:with-float-traps-masked t
           (cffi:foreign-funcall-pointer ,pointer ()
                                         ,@(loop for (name type) in args collect type collect name)
                                         ,result-type))))))

(defun %gl-load-functions (loader)
  "Load all GL entry points with LOADER (a C function pointer: void *(*)(const char *)), returns T on success"
  (let ((success t))
    (dolist (entry *gl-functions* success)
      (let ((address (cffi:foreign-funcall-pointer loader () :string (car entry) :pointer)))
        (when (cffi:null-pointer-p address) (setf success nil))
        (setf (symbol-value (cdr entry)) address)))))

;; GL 1.0 - 3.3 core functions used by rlgl
(defglfun ("glViewport" %gl-viewport) :void (x :int) (y :int) (width :int) (height :int))
(defglfun ("glEnable" %gl-enable) :void (cap :uint))
(defglfun ("glDisable" %gl-disable) :void (cap :uint))
(defglfun ("glDepthFunc" %gl-depth-func) :void (func :uint))
(defglfun ("glDepthMask" %gl-depth-mask) :void (flag :uchar))
(defglfun ("glColorMask" %gl-color-mask) :void (r :uchar) (g :uchar) (b :uchar) (a :uchar))
(defglfun ("glCullFace" %gl-cull-face) :void (mode :uint))
(defglfun ("glFrontFace" %gl-front-face) :void (mode :uint))
(defglfun ("glScissor" %gl-scissor) :void (x :int) (y :int) (width :int) (height :int))
(defglfun ("glPolygonMode" %gl-polygon-mode) :void (face :uint) (mode :uint))
(defglfun ("glLineWidth" %gl-line-width) :void (width :float))
(defglfun ("glClearColor" %gl-clear-color) :void (r :float) (g :float) (b :float) (a :float))
(defglfun ("glClearDepth" %gl-clear-depth) :void (depth :double))
(defglfun ("glClear" %gl-clear) :void (mask :uint))
(defglfun ("glGetError" %gl-get-error) :uint)
(defglfun ("glGetIntegerv" %gl-get-integerv) :void (pname :uint) (data :pointer))
(defglfun ("glGetFloatv" %gl-get-floatv) :void (pname :uint) (data :pointer))
(defglfun ("glGetString" %gl-get-string) :pointer (name :uint))
(defglfun ("glGetStringi" %gl-get-stringi) :pointer (name :uint) (index :uint))
(defglfun ("glBlendFunc" %gl-blend-func) :void (sfactor :uint) (dfactor :uint))
(defglfun ("glBlendEquation" %gl-blend-equation) :void (mode :uint))
(defglfun ("glBlendFuncSeparate" %gl-blend-func-separate) :void (src-rgb :uint) (dst-rgb :uint) (src-alpha :uint) (dst-alpha :uint))
(defglfun ("glBlendEquationSeparate" %gl-blend-equation-separate) :void (mode-rgb :uint) (mode-alpha :uint))
(defglfun ("glPixelStorei" %gl-pixel-storei) :void (pname :uint) (param :int))
(defglfun ("glGenTextures" %gl-gen-textures) :void (n :int) (textures :pointer))
(defglfun ("glDeleteTextures" %gl-delete-textures) :void (n :int) (textures :pointer))
(defglfun ("glBindTexture" %gl-bind-texture) :void (target :uint) (texture :uint))
(defglfun ("glActiveTexture" %gl-active-texture) :void (texture :uint))
(defglfun ("glTexParameteri" %gl-tex-parameteri) :void (target :uint) (pname :uint) (param :int))
(defglfun ("glTexParameterf" %gl-tex-parameterf) :void (target :uint) (pname :uint) (param :float))
(defglfun ("glTexParameteriv" %gl-tex-parameteriv) :void (target :uint) (pname :uint) (params :pointer))
(defglfun ("glTexImage2D" %gl-tex-image-2d) :void (target :uint) (level :int) (internal-format :int) (width :int) (height :int) (border :int) (format :uint) (type :uint) (pixels :pointer))
(defglfun ("glCompressedTexImage2D" %gl-compressed-tex-image-2d) :void (target :uint) (level :int) (internal-format :uint) (width :int) (height :int) (border :int) (image-size :int) (data :pointer))
(defglfun ("glTexSubImage2D" %gl-tex-sub-image-2d) :void (target :uint) (level :int) (xoffset :int) (yoffset :int) (width :int) (height :int) (format :uint) (type :uint) (pixels :pointer))
(defglfun ("glGenerateMipmap" %gl-generate-mipmap) :void (target :uint))
(defglfun ("glGetTexImage" %gl-get-tex-image) :void (target :uint) (level :int) (format :uint) (type :uint) (pixels :pointer))
(defglfun ("glReadPixels" %gl-read-pixels) :void (x :int) (y :int) (width :int) (height :int) (format :uint) (type :uint) (pixels :pointer))
(defglfun ("glGenFramebuffers" %gl-gen-framebuffers) :void (n :int) (ids :pointer))
(defglfun ("glDeleteFramebuffers" %gl-delete-framebuffers) :void (n :int) (ids :pointer))
(defglfun ("glBindFramebuffer" %gl-bind-framebuffer) :void (target :uint) (framebuffer :uint))
(defglfun ("glBlitFramebuffer" %gl-blit-framebuffer) :void (src-x0 :int) (src-y0 :int) (src-x1 :int) (src-y1 :int) (dst-x0 :int) (dst-y0 :int) (dst-x1 :int) (dst-y1 :int) (mask :uint) (filter :uint))
(defglfun ("glDrawBuffers" %gl-draw-buffers) :void (n :int) (bufs :pointer))
(defglfun ("glFramebufferTexture2D" %gl-framebuffer-texture-2d) :void (target :uint) (attachment :uint) (textarget :uint) (texture :uint) (level :int))
(defglfun ("glFramebufferRenderbuffer" %gl-framebuffer-renderbuffer) :void (target :uint) (attachment :uint) (renderbuffertarget :uint) (renderbuffer :uint))
(defglfun ("glCheckFramebufferStatus" %gl-check-framebuffer-status) :uint (target :uint))
(defglfun ("glGetFramebufferAttachmentParameteriv" %gl-get-framebuffer-attachment-parameteriv) :void (target :uint) (attachment :uint) (pname :uint) (params :pointer))
(defglfun ("glGenRenderbuffers" %gl-gen-renderbuffers) :void (n :int) (ids :pointer))
(defglfun ("glDeleteRenderbuffers" %gl-delete-renderbuffers) :void (n :int) (ids :pointer))
(defglfun ("glBindRenderbuffer" %gl-bind-renderbuffer) :void (target :uint) (renderbuffer :uint))
(defglfun ("glRenderbufferStorage" %gl-renderbuffer-storage) :void (target :uint) (internal-format :uint) (width :int) (height :int))
(defglfun ("glGenVertexArrays" %gl-gen-vertex-arrays) :void (n :int) (arrays :pointer))
(defglfun ("glDeleteVertexArrays" %gl-delete-vertex-arrays) :void (n :int) (arrays :pointer))
(defglfun ("glBindVertexArray" %gl-bind-vertex-array) :void (array :uint))
(defglfun ("glGenBuffers" %gl-gen-buffers) :void (n :int) (buffers :pointer))
(defglfun ("glDeleteBuffers" %gl-delete-buffers) :void (n :int) (buffers :pointer))
(defglfun ("glBindBuffer" %gl-bind-buffer) :void (target :uint) (buffer :uint))
(defglfun ("glBufferData" %gl-buffer-data) :void (target :uint) (size :long) (data :pointer) (usage :uint))
(defglfun ("glBufferSubData" %gl-buffer-sub-data) :void (target :uint) (offset :long) (size :long) (data :pointer))
(defglfun ("glEnableVertexAttribArray" %gl-enable-vertex-attrib-array) :void (index :uint))
(defglfun ("glDisableVertexAttribArray" %gl-disable-vertex-attrib-array) :void (index :uint))
(defglfun ("glVertexAttribPointer" %gl-vertex-attrib-pointer) :void (index :uint) (size :int) (type :uint) (normalized :uchar) (stride :int) (pointer :pointer))
(defglfun ("glVertexAttribDivisor" %gl-vertex-attrib-divisor) :void (index :uint) (divisor :uint))
(defglfun ("glVertexAttrib1fv" %gl-vertex-attrib-1fv) :void (index :uint) (v :pointer))
(defglfun ("glVertexAttrib2fv" %gl-vertex-attrib-2fv) :void (index :uint) (v :pointer))
(defglfun ("glVertexAttrib3fv" %gl-vertex-attrib-3fv) :void (index :uint) (v :pointer))
(defglfun ("glVertexAttrib4fv" %gl-vertex-attrib-4fv) :void (index :uint) (v :pointer))
(defglfun ("glDrawArrays" %gl-draw-arrays) :void (mode :uint) (first :int) (count :int))
(defglfun ("glDrawElements" %gl-draw-elements) :void (mode :uint) (count :int) (type :uint) (indices :pointer))
(defglfun ("glDrawArraysInstanced" %gl-draw-arrays-instanced) :void (mode :uint) (first :int) (count :int) (instancecount :int))
(defglfun ("glDrawElementsInstanced" %gl-draw-elements-instanced) :void (mode :uint) (count :int) (type :uint) (indices :pointer) (instancecount :int))
(defglfun ("glCreateShader" %gl-create-shader) :uint (type :uint))
(defglfun ("glShaderSource" %gl-shader-source) :void (shader :uint) (count :int) (string :pointer) (length :pointer))
(defglfun ("glCompileShader" %gl-compile-shader) :void (shader :uint))
(defglfun ("glGetShaderiv" %gl-get-shaderiv) :void (shader :uint) (pname :uint) (params :pointer))
(defglfun ("glGetShaderInfoLog" %gl-get-shader-info-log) :void (shader :uint) (buf-size :int) (length :pointer) (info-log :pointer))
(defglfun ("glDeleteShader" %gl-delete-shader) :void (shader :uint))
(defglfun ("glCreateProgram" %gl-create-program) :uint)
(defglfun ("glAttachShader" %gl-attach-shader) :void (program :uint) (shader :uint))
(defglfun ("glDetachShader" %gl-detach-shader) :void (program :uint) (shader :uint))
(defglfun ("glBindAttribLocation" %gl-bind-attrib-location) :void (program :uint) (index :uint) (name :string))
(defglfun ("glLinkProgram" %gl-link-program) :void (program :uint))
(defglfun ("glGetProgramiv" %gl-get-programiv) :void (program :uint) (pname :uint) (params :pointer))
(defglfun ("glGetProgramInfoLog" %gl-get-program-info-log) :void (program :uint) (buf-size :int) (length :pointer) (info-log :pointer))
(defglfun ("glDeleteProgram" %gl-delete-program) :void (program :uint))
(defglfun ("glUseProgram" %gl-use-program) :void (program :uint))
(defglfun ("glGetUniformLocation" %gl-get-uniform-location) :int (program :uint) (name :string))
(defglfun ("glGetAttribLocation" %gl-get-attrib-location) :int (program :uint) (name :string))
(defglfun ("glUniform1i" %gl-uniform-1i) :void (location :int) (v0 :int))
(defglfun ("glUniform4f" %gl-uniform-4f) :void (location :int) (v0 :float) (v1 :float) (v2 :float) (v3 :float))
(defglfun ("glUniform1fv" %gl-uniform-1fv) :void (location :int) (count :int) (value :pointer))
(defglfun ("glUniform2fv" %gl-uniform-2fv) :void (location :int) (count :int) (value :pointer))
(defglfun ("glUniform3fv" %gl-uniform-3fv) :void (location :int) (count :int) (value :pointer))
(defglfun ("glUniform4fv" %gl-uniform-4fv) :void (location :int) (count :int) (value :pointer))
(defglfun ("glUniform1iv" %gl-uniform-1iv) :void (location :int) (count :int) (value :pointer))
(defglfun ("glUniform2iv" %gl-uniform-2iv) :void (location :int) (count :int) (value :pointer))
(defglfun ("glUniform3iv" %gl-uniform-3iv) :void (location :int) (count :int) (value :pointer))
(defglfun ("glUniform4iv" %gl-uniform-4iv) :void (location :int) (count :int) (value :pointer))
(defglfun ("glUniform1uiv" %gl-uniform-1uiv) :void (location :int) (count :int) (value :pointer))
(defglfun ("glUniform2uiv" %gl-uniform-2uiv) :void (location :int) (count :int) (value :pointer))
(defglfun ("glUniform3uiv" %gl-uniform-3uiv) :void (location :int) (count :int) (value :pointer))
(defglfun ("glUniform4uiv" %gl-uniform-4uiv) :void (location :int) (count :int) (value :pointer))
(defglfun ("glUniformMatrix4fv" %gl-uniform-matrix-4fv) :void (location :int) (count :int) (transpose :uchar) (value :pointer))

;; OpenGL constants used by rlgl (GL_*)
(defconstant +gl-false+ 0)
(defconstant +gl-true+ 1)
(defconstant +gl-no-error+ 0)
(defconstant +gl-lines+ #x0001)
(defconstant +gl-triangles+ #x0004)
(defconstant +gl-triangle-strip+ #x0005)
(defconstant +gl-lequal+ #x0203)
(defconstant +gl-front+ #x0404)
(defconstant +gl-back+ #x0405)
(defconstant +gl-front-and-back+ #x0408)
(defconstant +gl-ccw+ #x0901)
(defconstant +gl-cull-face+ #x0B44)
(defconstant +gl-depth-test+ #x0B71)
(defconstant +gl-blend+ #x0BE2)
(defconstant +gl-scissor-test+ #x0C11)
(defconstant +gl-line-smooth+ #x0B20)
(defconstant +gl-line-width+ #x0B21)
(defconstant +gl-unpack-alignment+ #x0CF5)
(defconstant +gl-pack-alignment+ #x0D05)
(defconstant +gl-texture-2d+ #x0DE1)
(defconstant +gl-unsigned-byte+ #x1401)
(defconstant +gl-unsigned-short+ #x1403)
(defconstant +gl-unsigned-int+ #x1405)
(defconstant +gl-float+ #x1406)
(defconstant +gl-half-float+ #x140B)
(defconstant +gl-texture+ #x1702)
(defconstant +gl-depth-component+ #x1902)
(defconstant +gl-red+ #x1903)
(defconstant +gl-green+ #x1904)
(defconstant +gl-rgb+ #x1907)
(defconstant +gl-rgba+ #x1908)
(defconstant +gl-one+ 1)
(defconstant +gl-point+ #x1B00)
(defconstant +gl-line+ #x1B01)
(defconstant +gl-fill+ #x1B02)
(defconstant +gl-vendor+ #x1F00)
(defconstant +gl-renderer+ #x1F01)
(defconstant +gl-version+ #x1F02)
(defconstant +gl-extensions+ #x1F03)
(defconstant +gl-nearest+ #x2600)
(defconstant +gl-linear+ #x2601)
(defconstant +gl-linear-mipmap-linear+ #x2703)
(defconstant +gl-texture-mag-filter+ #x2800)
(defconstant +gl-texture-min-filter+ #x2801)
(defconstant +gl-texture-wrap-s+ #x2802)
(defconstant +gl-texture-wrap-t+ #x2803)
(defconstant +gl-repeat+ #x2901)
(defconstant +gl-color-buffer-bit+ #x00004000)
(defconstant +gl-depth-buffer-bit+ #x00000100)
(defconstant +gl-func-add+ #x8006)
(defconstant +gl-func-subtract+ #x800A)
(defconstant +gl-unsigned-short-4-4-4-4+ #x8033)
(defconstant +gl-unsigned-short-5-5-5-1+ #x8034)
(defconstant +gl-rgb8+ #x8051)
(defconstant +gl-rgba4+ #x8056)
(defconstant +gl-rgb5-a1+ #x8057)
(defconstant +gl-rgba8+ #x8058)
(defconstant +gl-texture-wrap-r+ #x8072)
(defconstant +gl-clamp-to-edge+ #x812F)
(defconstant +gl-texture-base-level+ #x813C)
(defconstant +gl-texture-max-level+ #x813D)
(defconstant +gl-rg+ #x8227)
(defconstant +gl-r8+ #x8229)
(defconstant +gl-rg8+ #x822B)
(defconstant +gl-r16f+ #x822D)
(defconstant +gl-r32f+ #x822E)
(defconstant +gl-unsigned-short-5-6-5+ #x8363)
(defconstant +gl-texture0+ #x84C0)
(defconstant +gl-texture-max-anisotropy-ext+ #x84FE)
(defconstant +gl-max-texture-max-anisotropy-ext+ #x84FF)
(defconstant +gl-texture-lod-bias+ #x8501)
(defconstant +gl-texture-cube-map+ #x8513)
(defconstant +gl-texture-cube-map-positive-x+ #x8515)
(defconstant +gl-program-point-size+ #x8642)
(defconstant +gl-rgba32f+ #x8814)
(defconstant +gl-rgb32f+ #x8815)
(defconstant +gl-rgba16f+ #x881A)
(defconstant +gl-rgb16f+ #x881B)
(defconstant +gl-texture-cube-map-seamless+ #x884F)
(defconstant +gl-array-buffer+ #x8892)
(defconstant +gl-element-array-buffer+ #x8893)
(defconstant +gl-static-draw+ #x88E4)
(defconstant +gl-dynamic-draw+ #x88E8)
(defconstant +gl-fragment-shader+ #x8B30)
(defconstant +gl-vertex-shader+ #x8B31)
(defconstant +gl-compile-status+ #x8B81)
(defconstant +gl-link-status+ #x8B82)
(defconstant +gl-info-log-length+ #x8B84)
(defconstant +gl-shading-language-version+ #x8B8C)
(defconstant +gl-draw-framebuffer-binding+ #x8CA6)
(defconstant +gl-framebuffer-attachment-object-type+ #x8CD0)
(defconstant +gl-framebuffer-attachment-object-name+ #x8CD1)
(defconstant +gl-framebuffer-complete+ #x8CD5)
(defconstant +gl-framebuffer-incomplete-attachment+ #x8CD6)
(defconstant +gl-framebuffer-incomplete-missing-attachment+ #x8CD7)
(defconstant +gl-framebuffer-unsupported+ #x8CDD)
(defconstant +gl-color-attachment0+ #x8CE0)
(defconstant +gl-depth-attachment+ #x8D00)
(defconstant +gl-stencil-attachment+ #x8D20)
(defconstant +gl-framebuffer+ #x8D40)
(defconstant +gl-renderbuffer+ #x8D41)
(defconstant +gl-texture-swizzle-rgba+ #x8E46)
(defconstant +gl-num-extensions+ #x821D)
(defconstant +gl-rgb565+ #x8D62)
(defconstant +gl-compute-shader+ #x91B9)
(defconstant +gl-compressed-rgb-s3tc-dxt1-ext+ #x83F0)
(defconstant +gl-compressed-rgba-s3tc-dxt1-ext+ #x83F1)
(defconstant +gl-compressed-rgba-s3tc-dxt3-ext+ #x83F2)
(defconstant +gl-compressed-rgba-s3tc-dxt5-ext+ #x83F3)
(defconstant +gl-etc1-rgb8-oes+ #x8D64)
(defconstant +gl-compressed-rgb8-etc2+ #x9274)
(defconstant +gl-compressed-rgba8-etc2-eac+ #x9278)
(defconstant +gl-compressed-rgb-pvrtc-4bppv1-img+ #x8C00)
(defconstant +gl-compressed-rgba-pvrtc-4bppv1-img+ #x8C02)
(defconstant +gl-compressed-rgba-astc-4x4-khr+ #x93b0)
(defconstant +gl-compressed-rgba-astc-8x8-khr+ #x93b7)

;; Default shader vertex attribute names to set location points
;; WARNING: Pre-defined names can not be changed, they are used by default shaders and all raylib examples shaders
(defparameter +rl-default-shader-attrib-name-position+ "vertexPosition")    ; Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_LOCATION_POSITION
(defparameter +rl-default-shader-attrib-name-texcoord+ "vertexTexCoord")    ; Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_LOCATION_TEXCOORD
(defparameter +rl-default-shader-attrib-name-normal+ "vertexNormal")        ; Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_LOCATION_NORMAL
(defparameter +rl-default-shader-attrib-name-color+ "vertexColor")          ; Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_LOCATION_COLOR
(defparameter +rl-default-shader-attrib-name-tangent+ "vertexTangent")      ; Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_LOCATION_TANGENT
(defparameter +rl-default-shader-attrib-name-texcoord2+ "vertexTexCoord2")  ; Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_LOCATION_TEXCOORD2
(defparameter +rl-default-shader-attrib-name-boneindices+ "vertexBoneIndices") ; Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_LOCATION_BONEINDICES
(defparameter +rl-default-shader-attrib-name-boneweights+ "vertexBoneWeights") ; Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_LOCATION_BONEWEIGHTS
(defparameter +rl-default-shader-attrib-name-instancetransform+ "instanceTransform") ; Bound by default to shader location: RL_DEFAULT_SHADER_ATTRIB_LOCATION_INSTANCETRANSFORM

(defparameter +rl-default-shader-uniform-name-mvp+ "mvp")                ; model-view-projection matrix
(defparameter +rl-default-shader-uniform-name-view+ "matView")           ; view matrix
(defparameter +rl-default-shader-uniform-name-projection+ "matProjection") ; projection matrix
(defparameter +rl-default-shader-uniform-name-model+ "matModel")         ; model matrix
(defparameter +rl-default-shader-uniform-name-normal+ "matNormal")       ; normal matrix (transpose(inverse(matModelView))
(defparameter +rl-default-shader-uniform-name-color+ "colDiffuse")       ; color diffuse (base tint color, multiplied by texture color)
(defparameter +rl-default-shader-uniform-name-bonematrices+ "boneMatrices") ; bone matrices (required for GPU skinning)
(defparameter +rl-default-shader-sampler2d-name-texture0+ "texture0")    ; texture0 (texture slot active 0)
(defparameter +rl-default-shader-sampler2d-name-texture1+ "texture1")    ; texture1 (texture slot active 1)
(defparameter +rl-default-shader-sampler2d-name-texture2+ "texture2")    ; texture2 (texture slot active 2)

;;;----------------------------------------------------------------------------------
;;; Module Types and Structures Definition
;;;----------------------------------------------------------------------------------

;; Renderer state (RLGL.State)
(defstruct (rlgl-state (:conc-name rls-))
  (vertex-counter 0 :type fixnum)       ; Current active render batch vertex counter (generic, used for all batches)
  (texcoordx 0.0 :type single-float)    ; Current active texture coordinate (added on glVertex*())
  (texcoordy 0.0 :type single-float)
  (normalx 0.0 :type single-float)      ; Current active normal (added on glVertex*())
  (normaly 0.0 :type single-float)
  (normalz 0.0 :type single-float)
  (colorr 0 :type (unsigned-byte 8))    ; Current active color (added on glVertex*())
  (colorg 0 :type (unsigned-byte 8))
  (colorb 0 :type (unsigned-byte 8))
  (colora 0 :type (unsigned-byte 8))
  (current-matrix-mode 0 :type fixnum)  ; Current matrix mode
  (current-matrix :modelview :type keyword) ; Current matrix pointer (:modelview, :projection or :transform)
  (modelview (meye 4))                  ; Default modelview matrix
  (projection (meye 4))                 ; Default projection matrix
  (transform (meye 4))                  ; Transform matrix to be used with rlTranslate, rlRotate, rlScale
  (transform-required nil)              ; Require transform matrix application to current draw-call vertex (if required)
  (stack (make-array +rl-max-matrix-stack-size+ :initial-element (meye 4))) ; Matrix stack for push/pop
  (stack-counter 0 :type fixnum)        ; Matrix stack counter
  (current-texture-id 0 :type (unsigned-byte 32)) ; Current texture id to be used on glBegin
  (default-texture-id 0 :type (unsigned-byte 32)) ; Default texture used on shapes/poly drawing (required by shader)
  (active-texture-id (make-array +rl-default-batch-max-texture-units+ :element-type '(unsigned-byte 32) :initial-element 0)) ; Active texture ids to be enabled on batch drawing (0 active by default)
  (default-vshader-id 0)                ; Default vertex shader id (used by default shader program)
  (default-fshader-id 0)                ; Default fragment shader id (used by default shader program)
  (default-shader-id 0)                 ; Default shader program id, supports vertex color and diffuse texture
  (default-shader-locs nil)             ; Default shader locations pointer to be used on rendering
  (current-shader-id 0)                 ; Current shader id to be used on rendering (by default, defaultShaderId)
  (current-shader-locs nil)             ; Current shader locations pointer to be used on rendering (by default, defaultShaderLocs)
  (stereo-render nil)                   ; Stereo rendering flag
  (projection-stereo (vector (meye 4) (meye 4))) ; VR stereo rendering eyes projection matrices
  (view-offset-stereo (vector (meye 4) (meye 4))) ; VR stereo rendering eyes view offset matrices
  ;; Blending variables
  (current-blend-mode 0)                ; Blending mode active
  (gl-blend-src-factor 0)               ; Blending source factor
  (gl-blend-dst-factor 0)               ; Blending destination factor
  (gl-blend-equation 0)                 ; Blending equation
  (gl-blend-src-factor-rgb 0)           ; Blending source RGB factor
  (gl-blend-dest-factor-rgb 0)          ; Blending destination RGB factor
  (gl-blend-src-factor-alpha 0)         ; Blending source alpha factor
  (gl-blend-dest-factor-alpha 0)        ; Blending destination alpha factor
  (gl-blend-equation-rgb 0)             ; Blending equation for RGB
  (gl-blend-equation-alpha 0)           ; Blending equation for alpha
  (gl-custom-blend-mode-modified nil)   ; Custom blending factor and equation modification status
  (framebuffer-width 0)                 ; Current framebuffer width
  (framebuffer-height 0))               ; Current framebuffer height

;; Extensions supported flags (RLGL.ExtSupported)
(defstruct (rlgl-ext-supported (:conc-name rlext-))
  (vao nil)                             ; VAO support (OpenGL ES2 could not support VAO extension) (GL_ARB_vertex_array_object)
  (instancing nil)                      ; Instancing supported (GL_ANGLE_instanced_arrays, GL_EXT_draw_instanced + GL_EXT_instanced_arrays)
  (tex-npot nil)                        ; NPOT textures full support (GL_ARB_texture_non_power_of_two, GL_OES_texture_npot)
  (tex-depth nil)                       ; Depth textures supported (GL_ARB_depth_texture, GL_OES_depth_texture)
  (tex-depth-webgl nil)                 ; Depth textures supported WebGL specific (GL_WEBGL_depth_texture)
  (tex-float32 nil)                     ; float textures support (32 bit per channel) (GL_OES_texture_float)
  (tex-float16 nil)                     ; half float textures support (16 bit per channel) (GL_OES_texture_half_float)
  (tex-comp-dxt nil)                    ; DDS texture compression support (GL_EXT_texture_compression_s3tc, GL_WEBGL_compressed_texture_s3tc, GL_WEBKIT_WEBGL_compressed_texture_s3tc)
  (tex-comp-etc1 nil)                   ; ETC1 texture compression support (GL_OES_compressed_ETC1_RGB8_texture, GL_WEBGL_compressed_texture_etc1)
  (tex-comp-etc2 nil)                   ; ETC2/EAC texture compression support (GL_ARB_ES3_compatibility)
  (tex-comp-pvrt nil)                   ; PVR texture compression support (GL_IMG_texture_compression_pvrtc)
  (tex-comp-astc nil)                   ; ASTC texture compression support (GL_KHR_texture_compression_astc_hdr, GL_KHR_texture_compression_astc_ldr)
  (tex-mirror-clamp nil)                ; Clamp mirror wrap mode supported (GL_EXT_texture_mirror_clamp)
  (tex-aniso-filter nil)                ; Anisotropic texture filtering support (GL_EXT_texture_filter_anisotropic)
  (compute-shader nil)                  ; Compute shaders support (GL_ARB_compute_shader)
  (ssbo nil)                            ; Shader storage buffer object support (GL_ARB_shader_storage_buffer_object)
  (max-anisotropy-level 0.0 :type single-float) ; Maximum anisotropy level supported (minimum is 2.0f)
  (max-depth-bits 0))                   ; Maximum bits for depth component

;; rlglData
(defstruct (rlgl-data (:conc-name rlgl-))
  (current-batch nil)                   ; Current render batch
  (default-batch nil)                   ; Default internal render batch
  (loader nil)                          ; OpenGL function loader
  (state (make-rlgl-state))             ; Renderer state
  (ext-supported (make-rlgl-ext-supported))) ; Extensions supported flags

;;;----------------------------------------------------------------------------------
;;; Global Variables Definition
;;;----------------------------------------------------------------------------------
(defvar *is-gpu-ready* nil)
(defvar *rl-cull-distance-near* +rl-cull-distance-near+)
(defvar *rl-cull-distance-far* +rl-cull-distance-far+)

(defvar *rlgl* (make-rlgl-data))

(defmacro %rls (accessor)
  "RLGL.State.<accessor>"
  `(,accessor (rlgl-state *rlgl*)))

(defmacro %rlext (accessor)
  "RLGL.ExtSupported.<accessor>"
  `(,accessor (rlgl-ext-supported *rlgl*)))

;;;----------------------------------------------------------------------------------
;;; C data helpers
;;;----------------------------------------------------------------------------------

(defun %lisp-data-element-type (type)
  (ecase type
    (:float 'single-float)
    (:int '(signed-byte 32))
    (:uint '(unsigned-byte 32))
    (:ushort '(unsigned-byte 16))
    (:uchar '(unsigned-byte 8))))

(defun %to-lisp-data (data type)
  "Convert DATA (number, vec2/3/4, mat4, list, vector) into a specialized vector of C TYPE elements"
  (let ((element-type (%lisp-data-element-type type)))
    (flet ((elements (x)
             (typecase x
               (vec2 (list (vx2 x) (vy2 x)))
               (vec3 (list (vx3 x) (vy3 x) (vz3 x)))
               (vec4 (list (vx4 x) (vy4 x) (vz4 x) (vw4 x)))
               (mat4 (coerce (marr4 x) 'list))
               (sequence (coerce x 'list))
               (t (list x)))))
      (let ((items (if (and (typep data 'sequence) (not (stringp data)))
                       (loop for x across (coerce data 'vector) append (elements x))
                       (elements data))))
        (make-array (length items) :element-type element-type
                                   :initial-contents (mapcar (lambda (v) (coerce (if (eq element-type 'single-float) v (truncate v))
                                                                                 element-type))
                                                             items))))))

(defmacro %with-c-data ((pointer data &optional (type :float)) &body body)
  "Bind POINTER to a C pointer to DATA: NIL -> NULL, foreign pointer, specialized vector (pinned),
other data converted to a vector of C TYPE elements"
  (let ((d (gensym "DATA")) (f (gensym "BODY")))
    `(let ((,d ,data))
       (flet ((,f (,pointer) ,@body))
         (cond ((null ,d) (,f (cffi:null-pointer)))
               ((cffi:pointerp ,d) (,f ,d))
               ((and (typep ,d '(simple-array * (*)))
                     (not (eq (array-element-type ,d) t))
                     (not (stringp ,d)))
                (cffi:with-pointer-to-vector-data (p ,d) (,f p)))
               (t (let ((v (%to-lisp-data ,d ,type)))
                    (cffi:with-pointer-to-vector-data (p v) (,f p)))))))))

(defun %c-data-size (data)
  "Size in bytes of a specialized vector"
  (* (length data)
     (let ((type (array-element-type data)))
       (cond ((subtypep type 'single-float) 4)
             ((subtypep type 'double-float) 8)
             ((subtypep type '(unsigned-byte 8)) 1)
             ((subtypep type '(signed-byte 8)) 1)
             ((subtypep type '(unsigned-byte 16)) 2)
             ((subtypep type '(signed-byte 16)) 2)
             ((subtypep type '(unsigned-byte 32)) 4)
             ((subtypep type '(signed-byte 32)) 4)
             (t 8)))))

(defmacro %gl-gen-one (gen-function)
  "Call glGen*(1, &id) and return id"
  `(cffi:with-foreign-object (id :uint)
     (setf (cffi:mem-ref id :uint) 0)
     (,gen-function 1 id)
     (cffi:mem-ref id :uint)))

(defmacro %gl-delete-one (delete-function id)
  "Call glDelete*(1, &id)"
  `(cffi:with-foreign-object (p :uint)
     (setf (cffi:mem-ref p :uint) ,id)
     (,delete-function 1 p)))

(defun %gl-get-integer (pname)
  (cffi:with-foreign-object (v :int)
    (setf (cffi:mem-ref v :int) 0)
    (%gl-get-integerv pname v)
    (cffi:mem-ref v :int)))

(defun %gl-get-float (pname)
  (cffi:with-foreign-object (v :float)
    (setf (cffi:mem-ref v :float) 0.0)
    (%gl-get-floatv pname v)
    (cffi:mem-ref v :float)))

(defun %gl-string (name)
  (let ((p (%gl-get-string name)))
    (if (cffi:null-pointer-p p) "" (cffi:foreign-string-to-lisp p))))

;;;----------------------------------------------------------------------------------
;;; Auxiliar matrix math functions
;;;----------------------------------------------------------------------------------

;; Get identity matrix
(defun %rl-matrix-identity ()
  (%matrix 1.0 0.0 0.0 0.0  0.0 1.0 0.0 0.0  0.0 0.0 1.0 0.0  0.0 0.0 0.0 1.0))

;; Get float array of matrix data
;; Explicit conversion to column-major memory layout
(defun %rl-matrix-to-float (mat)
  (let ((result (make-array 16 :element-type 'single-float)))
    (dotimes (i 16 result)
      (setf (aref result i) (mcref4 mat (mod i 4) (floor i 4))))))

;; Get two matrix multiplication
;; NOTE: When multiplying matrices... the order matters!
(defun %rl-matrix-multiply (left right)
  (matrix-multiply left right))

;; Transposes provided matrix
(defun %rl-matrix-transpose (mat)
  (matrix-transpose mat))

;; Invert provided matrix
(defun %rl-matrix-invert (mat)
  (matrix-invert mat))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition - Matrix operations
;;;----------------------------------------------------------------------------------

(declaim (inline %rl-current-matrix (setf %rl-current-matrix)))
(defun %rl-current-matrix ()
  "*RLGL.State.currentMatrix"
  (let ((state (rlgl-state *rlgl*)))
    (ecase (rls-current-matrix state)
      (:modelview (rls-modelview state))
      (:projection (rls-projection state))
      (:transform (rls-transform state)))))

(defun (setf %rl-current-matrix) (mat)
  (let ((state (rlgl-state *rlgl*)))
    (ecase (rls-current-matrix state)
      (:modelview (setf (rls-modelview state) mat))
      (:projection (setf (rls-projection state) mat))
      (:transform (setf (rls-transform state) mat)))))

;; Choose the current matrix to be transformed
(defun rl-matrix-mode (mode)
  (cond ((= mode +rl-projection+) (setf (%rls rls-current-matrix) :projection))
        ((= mode +rl-modelview+) (setf (%rls rls-current-matrix) :modelview)))
  ;;else if (mode == RL_TEXTURE) // Not supported
  (setf (%rls rls-current-matrix-mode) mode))

;; Push the current matrix into RLGL.State.stack
(defun rl-push-matrix ()
  (when (>= (%rls rls-stack-counter) +rl-max-matrix-stack-size+)
    (trace-log +log-error+ "RLGL: Matrix stack overflow (RL_MAX_MATRIX_STACK_SIZE)")
    (return-from rl-push-matrix nil))
  (when (= (%rls rls-current-matrix-mode) +rl-modelview+)
    (setf (%rls rls-transform-required) t
          (%rls rls-current-matrix) :transform))
  (setf (aref (%rls rls-stack) (%rls rls-stack-counter)) (%rl-current-matrix))
  (incf (%rls rls-stack-counter)))

;; Pop latest inserted matrix from RLGL.State.stack
(defun rl-pop-matrix ()
  (when (> (%rls rls-stack-counter) 0)
    (setf (%rl-current-matrix) (aref (%rls rls-stack) (1- (%rls rls-stack-counter))))
    (decf (%rls rls-stack-counter)))
  (when (and (= (%rls rls-stack-counter) 0) (= (%rls rls-current-matrix-mode) +rl-modelview+))
    (setf (%rls rls-current-matrix) :modelview
          (%rls rls-transform-required) nil)))

;; Reset current matrix to identity matrix
(defun rl-load-identity ()
  (setf (%rl-current-matrix) (%rl-matrix-identity)))

;; Multiply the current matrix by a translation matrix
(defun rl-translatef (x y z)
  (let ((mat-translation (%matrix 1.0 0.0 0.0 0.0  0.0 1.0 0.0 0.0  0.0 0.0 1.0 0.0
                                  (float x 1.0) (float y 1.0) (float z 1.0) 1.0)))
    ;; NOTE: Transposing matrix by multiplication order
    (setf (%rl-current-matrix) (%rl-matrix-multiply mat-translation (%rl-current-matrix)))))

;; Multiply the current matrix by a rotation matrix
;; NOTE: The provided angle must be in degrees
(defun rl-rotatef (angle x y z)
  (let ((angle (float angle 1.0)) (x (float x 1.0)) (y (float y 1.0)) (z (float z 1.0)))
    ;; Axis vector (x, y, z) normalization
    (let ((length-squared (+ (* x x) (* y y) (* z z))))
      (when (and (/= length-squared 1.0) (/= length-squared 0.0))
        (let ((inverse-length (/ 1.0 (sqrt length-squared))))
          (setf x (* x inverse-length)
                y (* y inverse-length)
                z (* z inverse-length)))))
    ;; Rotation matrix generation
    (let* ((sinres (%sinf (* (/ +pi+ 180.0) angle)))
           (cosres (%cosf (* (/ +pi+ 180.0) angle)))
           (tt (- 1.0 cosres))
           (mat-rotation (%matrix (+ (* x x tt) cosres)
                                  (+ (* y x tt) (* z sinres))
                                  (- (* z x tt) (* y sinres))
                                  0.0
                                  (- (* x y tt) (* z sinres))
                                  (+ (* y y tt) cosres)
                                  (+ (* z y tt) (* x sinres))
                                  0.0
                                  (+ (* x z tt) (* y sinres))
                                  (- (* y z tt) (* x sinres))
                                  (+ (* z z tt) cosres)
                                  0.0
                                  0.0 0.0 0.0 1.0)))
      ;; NOTE: Transposing matrix by multiplication order
      (setf (%rl-current-matrix) (%rl-matrix-multiply mat-rotation (%rl-current-matrix))))))

;; Multiply the current matrix by a scaling matrix
(defun rl-scalef (x y z)
  (let ((mat-scale (%matrix (float x 1.0) 0.0 0.0 0.0  0.0 (float y 1.0) 0.0 0.0
                            0.0 0.0 (float z 1.0) 0.0  0.0 0.0 0.0 1.0)))
    ;; NOTE: Transposing matrix by multiplication order
    (setf (%rl-current-matrix) (%rl-matrix-multiply mat-scale (%rl-current-matrix)))))

;; Multiply the current matrix by another matrix
;; NOTE: MATF is a column-major float array (16 floats), a mat4 is also accepted
(defun rl-mult-matrixf (matf)
  ;; Matrix creation from array
  ;; Conversion from column-major to row-major memory order
  (let ((mat (if (typep matf 'mat4)
                 matf
                 (apply #'%matrix (loop for i below 16 collect (float (elt matf i) 1.0))))))
    (setf (%rl-current-matrix) (%rl-matrix-multiply mat (%rl-current-matrix)))))

;; Multiply the current matrix by a perspective matrix generated by parameters
(defun rl-frustum (left right bottom top znear zfar)
  (let* ((left (float left 1d0)) (right (float right 1d0)) (bottom (float bottom 1d0))
         (top (float top 1d0)) (znear (float znear 1d0)) (zfar (float zfar 1d0))
         (rl (float (- right left) 1.0))
         (tb (float (- top bottom) 1.0))
         (fn (float (- zfar znear) 1.0))
         (mat-frustum (%matrix (/ (* (float znear 1.0) 2.0) rl) 0.0 0.0 0.0
                               0.0 (/ (* (float znear 1.0) 2.0) tb) 0.0 0.0
                               (/ (+ (float right 1.0) (float left 1.0)) rl)
                               (/ (+ (float top 1.0) (float bottom 1.0)) tb)
                               (/ (- (+ (float zfar 1.0) (float znear 1.0))) fn)
                               -1.0
                               0.0 0.0
                               (/ (- (* (float zfar 1.0) (float znear 1.0) 2.0)) fn)
                               0.0)))
    (setf (%rl-current-matrix) (%rl-matrix-multiply (%rl-current-matrix) mat-frustum))))

;; Multiply the current matrix by an orthographic matrix generated by parameters
(defun rl-ortho (left right bottom top znear zfar)
  ;; NOTE: If left-right and top-botton values are equal it could create a division by zero,
  ;; response to it is platform/compiler dependent
  (let* ((left (float left 1d0)) (right (float right 1d0)) (bottom (float bottom 1d0))
         (top (float top 1d0)) (znear (float znear 1d0)) (zfar (float zfar 1d0))
         (rl (float (- right left) 1.0))
         (tb (float (- top bottom) 1.0))
         (fn (float (- zfar znear) 1.0))
         (mat-ortho (%matrix (/ 2.0 rl) 0.0 0.0 0.0
                             0.0 (/ 2.0 tb) 0.0 0.0
                             0.0 0.0 (/ -2.0 fn) 0.0
                             (/ (- (+ (float left 1.0) (float right 1.0))) rl)
                             (/ (- (+ (float top 1.0) (float bottom 1.0))) tb)
                             (/ (- (+ (float zfar 1.0) (float znear 1.0))) fn)
                             1.0)))
    (setf (%rl-current-matrix) (%rl-matrix-multiply (%rl-current-matrix) mat-ortho))))

;; Set the viewport area (transformation from normalized device coordinates to window coordinates)
(defun rl-viewport (x y width height)
  (%gl-viewport x y width height))

;; Set clip planes distances
(defun rl-set-clip-planes (near-plane far-plane)
  (setf *rl-cull-distance-near* (float near-plane 1d0)
        *rl-cull-distance-far* (float far-plane 1d0)))

;; Get cull plane distance near
(defun rl-get-cull-distance-near ()
  *rl-cull-distance-near*)

;; Get cull plane distance far
(defun rl-get-cull-distance-far ()
  *rl-cull-distance-far*)

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition - Vertex level operations
;;;----------------------------------------------------------------------------------

(declaim (inline %rl-last-draw))
(defun %rl-last-draw (batch)
  "batch->draws[batch->drawCounter - 1]"
  (svref (rl-render-batch-draws batch) (1- (rl-render-batch-draw-counter batch))))

(defun %rl-align-last-draw (batch)
  "Make sure current draw vertexCount is aligned a multiple of 4 (rlBegin()/rlSetTexture())
Returns T if a new draw call was registered"
  (let ((draw (%rl-last-draw batch)))
    ;; Make sure current RLGL.currentBatch->draws[i].vertexCount is aligned a multiple of 4,
    ;; that way, following QUADS drawing will keep aligned with index processing
    ;; It implies adding some extra alignment vertex at the end of the draw,
    ;; those vertex are not processed but they are considered as an additional offset
    ;; for the next set of vertex to be drawn
    (setf (rl-draw-call-vertex-alignment draw)
          (cond ((= (rl-draw-call-mode draw) +rl-lines+)
                 (if (< (rl-draw-call-vertex-count draw) 4)
                     (rl-draw-call-vertex-count draw)
                     (mod (rl-draw-call-vertex-count draw) 4)))
                ((= (rl-draw-call-mode draw) +rl-triangles+)
                 (if (< (rl-draw-call-vertex-count draw) 4)
                     1
                     (- 4 (mod (rl-draw-call-vertex-count draw) 4))))
                (t 0)))
    (unless (rl-check-render-batch-limit (rl-draw-call-vertex-alignment draw))
      (incf (%rls rls-vertex-counter) (rl-draw-call-vertex-alignment draw))
      (incf (rl-render-batch-draw-counter batch))
      t)))

;; Initialize drawing mode (how to organize vertex)
(defun rl-begin (mode)
  ;; Draw mode can be RL_LINES, RL_TRIANGLES and RL_QUADS
  ;; NOTE: In all three cases, vertex are accumulated over default internal vertex buffer
  (let ((batch (rlgl-current-batch *rlgl*)))
    (when (/= (rl-draw-call-mode (%rl-last-draw batch)) mode)
      (when (> (rl-draw-call-vertex-count (%rl-last-draw batch)) 0)
        (%rl-align-last-draw batch))
      (when (>= (rl-render-batch-draw-counter batch) +rl-default-batch-drawcalls+)
        (rl-draw-render-batch batch))
      (let ((draw (%rl-last-draw batch)))
        (setf (rl-draw-call-mode draw) mode
              (rl-draw-call-texture-id draw) (%rls rls-current-texture-id)
              (%rls rls-current-texture-id) (%rls rls-default-texture-id))))))

;; Finish vertex providing
(defun rl-end ()
  ;; NOTE: Depth increment is dependent on rlOrtho(): z-near and z-far values,
  ;; as well as depth buffer bit-depth (16bit or 24bit or 32bit)
  ;; Correct increment formula would be: depthInc = (zfar - znear)/pow(2, bits)
  (let ((batch (rlgl-current-batch *rlgl*)))
    (setf (rl-render-batch-current-depth batch) (+ (rl-render-batch-current-depth batch) (/ 1.0 20000.0)))))

;; Define one vertex (position)
;; NOTE: Vertex position data is the basic information required for drawing
(defun rl-vertex3f (x y z)
  (let* ((x (float x 1.0)) (y (float y 1.0)) (z (float z 1.0))
         (state (rlgl-state *rlgl*))
         (tx x) (ty y) (tz z))
    (declare (type single-float x y z tx ty tz))
    ;; Transform provided vector if required
    (when (rls-transform-required state)
      (%with-matrix (m (rls-transform state))
        (setf tx (+ (* m0 x) (* m4 y) (* m8 z) m12)
              ty (+ (* m1 x) (* m5 y) (* m9 z) m13)
              tz (+ (* m2 x) (* m6 y) (* m10 z) m14))))
    (let ((batch (rlgl-current-batch *rlgl*)))
      ;; WARNING: Be careful with primitives breaking when launching a new batch!
      ;; RL_LINES comes in pairs, RL_TRIANGLES come in groups of 3 vertices and RL_QUADS come in groups of 4 vertices
      ;; Checking current draw.mode when a new vertex is required and finish the batch only if the draw.mode draw.vertexCount is %2, %3 or %4
      (when (> (rls-vertex-counter state)
               (- (* (rl-vertex-buffer-element-count
                      (svref (rl-render-batch-vertex-buffer batch) (rl-render-batch-current-buffer batch)))
                     4)
                  4))
        (let ((draw (%rl-last-draw batch)))
          (cond ((and (= (rl-draw-call-mode draw) +rl-lines+)
                      (= (mod (rl-draw-call-vertex-count draw) 2) 0))
                 ;; Reached the maximum number of vertices for RL_LINES drawing
                 ;; Launch a draw call but keep current state for next vertices comming
                 ;; NOTE: Adding +1 vertex to the check for some safety
                 (rl-check-render-batch-limit (+ 2 1)))
                ((and (= (rl-draw-call-mode draw) +rl-triangles+)
                      (= (mod (rl-draw-call-vertex-count draw) 3) 0))
                 (rl-check-render-batch-limit (+ 3 1)))
                ((and (= (rl-draw-call-mode draw) +rl-quads+)
                      (= (mod (rl-draw-call-vertex-count draw) 4) 0))
                 (rl-check-render-batch-limit (+ 4 1))))))
      (let* ((buffer (svref (rl-render-batch-vertex-buffer batch) (rl-render-batch-current-buffer batch)))
             (vertices (rl-vertex-buffer-vertices buffer))
             (texcoords (rl-vertex-buffer-texcoords buffer))
             (normals (rl-vertex-buffer-normals buffer))
             (colors (rl-vertex-buffer-colors buffer))
             (counter (rls-vertex-counter state)))
        (declare (type fixnum counter))
        ;; Add vertices
        (setf (aref vertices (* 3 counter)) tx
              (aref vertices (+ (* 3 counter) 1)) ty
              (aref vertices (+ (* 3 counter) 2)) tz)
        ;; Add current texcoord
        (setf (aref texcoords (* 2 counter)) (rls-texcoordx state)
              (aref texcoords (+ (* 2 counter) 1)) (rls-texcoordy state))
        ;; Add current normal
        (setf (aref normals (* 3 counter)) (rls-normalx state)
              (aref normals (+ (* 3 counter) 1)) (rls-normaly state)
              (aref normals (+ (* 3 counter) 2)) (rls-normalz state))
        ;; Add current color
        (setf (aref colors (* 4 counter)) (rls-colorr state)
              (aref colors (+ (* 4 counter) 1)) (rls-colorg state)
              (aref colors (+ (* 4 counter) 2)) (rls-colorb state)
              (aref colors (+ (* 4 counter) 3)) (rls-colora state))
        (setf (rls-vertex-counter state) (1+ counter))
        (incf (rl-draw-call-vertex-count (%rl-last-draw batch)))))))

;; Define one vertex (position)
(defun rl-vertex2f (x y)
  (rl-vertex3f x y (rl-render-batch-current-depth (rlgl-current-batch *rlgl*))))

;; Define one vertex (position)
(defun rl-vertex2i (x y)
  (rl-vertex3f (float x 1.0) (float y 1.0) (rl-render-batch-current-depth (rlgl-current-batch *rlgl*))))

;; Define one vertex (texture coordinate)
;; NOTE: Texture coordinates are limited to QUADS only
(defun rl-tex-coord2f (x y)
  (setf (%rls rls-texcoordx) (float x 1.0)
        (%rls rls-texcoordy) (float y 1.0)))

;; Define one vertex (normal)
;; NOTE: Normals limited to TRIANGLES only?
(defun rl-normal3f (x y z)
  (let ((normalx (float x 1.0)) (normaly (float y 1.0)) (normalz (float z 1.0)))
    (when (%rls rls-transform-required)
      (%with-matrix (m (%rls rls-transform))
        (let ((x normalx) (y normaly) (z normalz))
          (setf normalx (+ (* m0 x) (* m4 y) (* m8 z))
                normaly (+ (* m1 x) (* m5 y) (* m9 z))
                normalz (+ (* m2 x) (* m6 y) (* m10 z))))))
    ;; NOTE: Default behavior assumes the normal vector is in the correct space for what the shader expects,
    ;; it could be not normalized to 0.0f..1.0f, magnitud can be useed for some effects
    (setf (%rls rls-normalx) normalx
          (%rls rls-normaly) normaly
          (%rls rls-normalz) normalz)))

;; Define one vertex (color)
(defun rl-color4ub (r g b a)
  (setf (%rls rls-colorr) (logand r #xff)
        (%rls rls-colorg) (logand g #xff)
        (%rls rls-colorb) (logand b #xff)
        (%rls rls-colora) (logand a #xff)))

(declaim (inline %rl-float-to-u8))
(defun %rl-float-to-u8 (x)
  "C (unsigned char)(x*255) conversion"
  (logand (truncate (* (float x 1.0) 255)) #xff))

;; Define one vertex (color)
(defun rl-color4f (r g b a)
  (rl-color4ub (%rl-float-to-u8 r) (%rl-float-to-u8 g) (%rl-float-to-u8 b) (%rl-float-to-u8 a)))

;; Define one vertex (color)
(defun rl-color3f (x y z)
  (rl-color4ub (%rl-float-to-u8 x) (%rl-float-to-u8 y) (%rl-float-to-u8 z) 255))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition - OpenGL style functions (common to 1.1, 3.3+, ES2)
;;;----------------------------------------------------------------------------------

;; Set current texture to use
(defun rl-set-texture (id)
  (let ((batch (rlgl-current-batch *rlgl*)))
    (if (= id 0)
        (progn
          ;; NOTE: If quads batch limit is reached, force a draw call and next batch starts
          (when (>= (%rls rls-vertex-counter)
                    (* (rl-vertex-buffer-element-count
                        (svref (rl-render-batch-vertex-buffer batch) (rl-render-batch-current-buffer batch)))
                       4))
            (rl-draw-render-batch batch))
          (setf (%rls rls-current-texture-id) (%rls rls-default-texture-id)))
        (progn
          (setf (%rls rls-current-texture-id) id)
          (when (/= (rl-draw-call-texture-id (%rl-last-draw batch)) id)
            (when (> (rl-draw-call-vertex-count (%rl-last-draw batch)) 0)
              (when (%rl-align-last-draw batch)
                (setf (rl-draw-call-mode (%rl-last-draw batch))
                      (rl-draw-call-mode (svref (rl-render-batch-draws batch)
                                                (- (rl-render-batch-draw-counter batch) 2))))))
            (when (>= (rl-render-batch-draw-counter batch) +rl-default-batch-drawcalls+)
              (rl-draw-render-batch batch))
            (setf (rl-draw-call-texture-id (%rl-last-draw batch)) id
                  (rl-draw-call-vertex-count (%rl-last-draw batch)) 0))))))

;; Select and active a texture slot
(defun rl-active-texture-slot (slot)
  (%gl-active-texture (+ +gl-texture0+ slot)))

;; Enable texture
(defun rl-enable-texture (id)
  (%gl-bind-texture +gl-texture-2d+ id))

;; Disable texture
(defun rl-disable-texture ()
  (%gl-bind-texture +gl-texture-2d+ 0))

;; Enable texture cubemap
(defun rl-enable-texture-cubemap (id)
  (%gl-bind-texture +gl-texture-cube-map+ id))

;; Disable texture cubemap
(defun rl-disable-texture-cubemap ()
  (%gl-bind-texture +gl-texture-cube-map+ 0))

(defun %rl-texture-target-parameters (target id param value)
  "rlTextureParameters()/rlCubemapParameters() switch on param"
  (cond ((or (= param +rl-texture-wrap-s+) (= param +rl-texture-wrap-t+))
         (if (= value +rl-texture-wrap-mirror-clamp+)
             (if (%rlext rlext-tex-mirror-clamp)
                 (%gl-tex-parameteri target param value)
                 (trace-log +log-warning+ "GL: Clamp mirror wrap mode not supported (GL_MIRROR_CLAMP_EXT)"))
             (%gl-tex-parameteri target param value)))
        ((or (= param +rl-texture-mag-filter+) (= param +rl-texture-min-filter+))
         (%gl-tex-parameteri target param value))
        ((= param +rl-texture-filter-anisotropic+)
         (cond ((<= value (%rlext rlext-max-anisotropy-level))
                (%gl-tex-parameterf target +gl-texture-max-anisotropy-ext+ (float value 1.0)))
               ((> (%rlext rlext-max-anisotropy-level) 0.0)
                ;; NOTE: C passes id as the format argument (prints id)
                (trace-log +log-warning+ "GL: Maximum anisotropic filter level supported is ~dX" id)
                (%gl-tex-parameterf target +gl-texture-max-anisotropy-ext+ (float value 1.0)))
               (t (trace-log +log-warning+ "GL: Anisotropic filtering not supported"))))
        ((= param +rl-texture-mipmap-bias-ratio+)
         (%gl-tex-parameterf target +gl-texture-lod-bias+ (/ value 100.0)))))

;; Set texture parameters (wrap mode/filter mode)
(defun rl-texture-parameters (id param value)
  (%gl-bind-texture +gl-texture-2d+ id)
  ;; Reset anisotropy filter, in case it was set
  (when (= param +rl-texture-filter-anisotropic+)
    (%gl-tex-parameterf +gl-texture-2d+ +gl-texture-max-anisotropy-ext+ 1.0))
  (%rl-texture-target-parameters +gl-texture-2d+ id param value)
  (%gl-bind-texture +gl-texture-2d+ 0))

;; Set cubemap parameters (wrap mode/filter mode)
(defun rl-cubemap-parameters (id param value)
  (%gl-bind-texture +gl-texture-cube-map+ id)
  ;; Reset anisotropy filter, in case it was set
  (%gl-tex-parameterf +gl-texture-cube-map+ +gl-texture-max-anisotropy-ext+ 1.0)
  (%rl-texture-target-parameters +gl-texture-cube-map+ id param value)
  (%gl-bind-texture +gl-texture-cube-map+ 0))

;; Enable shader program
(defun rl-enable-shader (id)
  (%gl-use-program id))

;; Disable shader program
(defun rl-disable-shader ()
  (%gl-use-program 0))

;; Enable rendering to texture (fbo)
(defun rl-enable-framebuffer (id)
  (%gl-bind-framebuffer +gl-framebuffer+ id))

;; return the active render texture (fbo)
(defun rl-get-active-framebuffer ()
  (%gl-get-integer +gl-draw-framebuffer-binding+))

;; Disable rendering to texture
(defun rl-disable-framebuffer ()
  (%gl-bind-framebuffer +gl-framebuffer+ 0))

;; Blit active framebuffer to main framebuffer
(defun rl-blit-framebuffer (src-x src-y src-width src-height dst-x dst-y dst-width dst-height buffer-mask)
  (%gl-blit-framebuffer src-x src-y src-width src-height dst-x dst-y dst-width dst-height buffer-mask +gl-nearest+))

;; Bind framebuffer object (fbo)
(defun rl-bind-framebuffer (target framebuffer)
  (%gl-bind-framebuffer target framebuffer))

;; Activate multiple draw color buffers
;; NOTE: One color buffer is always active by default
(defun rl-active-draw-buffers (count)
  ;; NOTE: Maximum number of draw buffers supported is implementation dependent,
  ;; it can be queried with glGet*() but it must be at least 8
  (if (> count 0)
      (if (> count 8)
          (trace-log +log-warning+ "GL: Max color buffers limited to 8")
          (cffi:with-foreign-object (buffers :uint 8)
            (dotimes (i 8) (setf (cffi:mem-aref buffers :uint i) (+ +gl-color-attachment0+ i)))
            (%gl-draw-buffers count buffers)))
      (trace-log +log-warning+ "GL: One color buffer active by default")))

;;;----------------------------------------------------------------------------------
;;; General render state configuration
;;;----------------------------------------------------------------------------------

;; Enable color blending
(defun rl-enable-color-blend () (%gl-enable +gl-blend+))

;; Disable color blending
(defun rl-disable-color-blend () (%gl-disable +gl-blend+))

;; Enable depth test
(defun rl-enable-depth-test () (%gl-enable +gl-depth-test+))

;; Disable depth test
(defun rl-disable-depth-test () (%gl-disable +gl-depth-test+))

;; Enable depth write
(defun rl-enable-depth-mask () (%gl-depth-mask +gl-true+))

;; Disable depth write
(defun rl-disable-depth-mask () (%gl-depth-mask +gl-false+))

;; Enable backface culling
(defun rl-enable-backface-culling () (%gl-enable +gl-cull-face+))

;; Disable backface culling
(defun rl-disable-backface-culling () (%gl-disable +gl-cull-face+))

;; Set color mask active for screen read/draw
(defun rl-color-mask (r g b a)
  (%gl-color-mask (if r 1 0) (if g 1 0) (if b 1 0) (if a 1 0)))

;; Set face culling mode
(defun rl-set-cull-face (mode)
  (cond ((= mode +rl-cull-face-back+) (%gl-cull-face +gl-back+))
        ((= mode +rl-cull-face-front+) (%gl-cull-face +gl-front+))))

;; Enable scissor test
(defun rl-enable-scissor-test () (%gl-enable +gl-scissor-test+))

;; Disable scissor test
(defun rl-disable-scissor-test () (%gl-disable +gl-scissor-test+))

;; Scissor test
(defun rl-scissor (x y width height) (%gl-scissor x y width height))

;; Enable wire mode
(defun rl-enable-wire-mode ()
  ;; NOTE: glPolygonMode() not available on OpenGL ES
  (%gl-polygon-mode +gl-front-and-back+ +gl-line+))

;; Disable wire mode
(defun rl-disable-wire-mode ()
  ;; NOTE: glPolygonMode() not available on OpenGL ES
  (%gl-polygon-mode +gl-front-and-back+ +gl-fill+))

;; Enable point mode
(defun rl-enable-point-mode ()
  ;; NOTE: glPolygonMode() not available on OpenGL ES
  (%gl-polygon-mode +gl-front-and-back+ +gl-point+)
  (%gl-enable +gl-program-point-size+))

;; Disable point mode
(defun rl-disable-point-mode ()
  ;; NOTE: glPolygonMode() not available on OpenGL ES
  (%gl-polygon-mode +gl-front-and-back+ +gl-fill+))

;; Set the line drawing width
(defun rl-set-line-width (width) (%gl-line-width (float width 1.0)))

;; Get the line drawing width
(defun rl-get-line-width ()
  (%gl-get-float +gl-line-width+))

;; Set the point drawing size
(defun rl-set-point-size (size)
  (declare (ignore size)))

;; Get the point drawing size
(defun rl-get-point-size ()
  1.0)

;; Enable line aliasing
(defun rl-enable-smooth-lines ()
  (%gl-enable +gl-line-smooth+))

;; Disable line aliasing
(defun rl-disable-smooth-lines ()
  (%gl-disable +gl-line-smooth+))

;; Enable stereo rendering
(defun rl-enable-stereo-render ()
  (setf (%rls rls-stereo-render) t))

;; Disable stereo rendering
(defun rl-disable-stereo-render ()
  (setf (%rls rls-stereo-render) nil))

;; Check if stereo render is enabled
(defun rl-is-stereo-render-enabled ()
  (%rls rls-stereo-render))

;; Clear color buffer with color
(defun rl-clear-color (r g b a)
  ;; Color values clamp to 0.0f(0) and 1.0f(255)
  (let ((cr (/ (float r 1.0) 255))
        (cg (/ (float g 1.0) 255))
        (cb (/ (float b 1.0) 255))
        (ca (/ (float a 1.0) 255)))
    (%gl-clear-color cr cg cb ca)))

;; Clear used screen buffers (color and depth)
(defun rl-clear-screen-buffers ()
  (%gl-clear (logior +gl-color-buffer-bit+ +gl-depth-buffer-bit+))) ; Clear used buffers: Color and Depth (Depth is used for 3D)

;; Check and log OpenGL error codes
(defun rl-check-errors ()
  (loop
    (let ((err (%gl-get-error)))
      (case err
        (#.+gl-no-error+ (return))
        (#x0500 (trace-log +log-warning+ "GL: Error detected: GL_INVALID_ENUM"))
        (#x0501 (trace-log +log-warning+ "GL: Error detected: GL_INVALID_VALUE"))
        (#x0502 (trace-log +log-warning+ "GL: Error detected: GL_INVALID_OPERATION"))
        (#x0503 (trace-log +log-warning+ "GL: Error detected: GL_STACK_OVERFLOW"))
        (#x0504 (trace-log +log-warning+ "GL: Error detected: GL_STACK_UNDERFLOW"))
        (#x0505 (trace-log +log-warning+ "GL: Error detected: GL_OUT_OF_MEMORY"))
        (#x0506 (trace-log +log-warning+ "GL: Error detected: GL_INVALID_FRAMEBUFFER_OPERATION"))
        (t (trace-log +log-warning+ "GL: Error detected: Unknown error code: ~x" err))))))

;; Set blend mode
(defun rl-set-blend-mode (mode)
  (when (or (/= (%rls rls-current-blend-mode) mode)
            (and (or (= mode +rl-blend-custom+) (= mode +rl-blend-custom-separate+))
                 (%rls rls-gl-custom-blend-mode-modified)))
    (rl-draw-render-batch (rlgl-current-batch *rlgl*))
    (case mode
      (#.+rl-blend-alpha+ (%gl-blend-func +rl-src-alpha+ +rl-one-minus-src-alpha+) (%gl-blend-equation +rl-func-add+))
      (#.+rl-blend-additive+ (%gl-blend-func +rl-src-alpha+ +rl-one+) (%gl-blend-equation +rl-func-add+))
      (#.+rl-blend-multiplied+ (%gl-blend-func +rl-dst-color+ +rl-one-minus-src-alpha+) (%gl-blend-equation +rl-func-add+))
      (#.+rl-blend-add-colors+ (%gl-blend-func +rl-one+ +rl-one+) (%gl-blend-equation +rl-func-add+))
      (#.+rl-blend-subtract-colors+ (%gl-blend-func +rl-one+ +rl-one+) (%gl-blend-equation +rl-func-subtract+))
      (#.+rl-blend-alpha-premultiply+ (%gl-blend-func +rl-one+ +rl-one-minus-src-alpha+) (%gl-blend-equation +rl-func-add+))
      (#.+rl-blend-custom+
       ;; NOTE: Using GL blend src/dst factors and GL equation configured with rlSetBlendFactors()
       (%gl-blend-func (%rls rls-gl-blend-src-factor) (%rls rls-gl-blend-dst-factor))
       (%gl-blend-equation (%rls rls-gl-blend-equation)))
      (#.+rl-blend-custom-separate+
       ;; NOTE: Using GL blend src/dst factors and GL equation configured with rlSetBlendFactorsSeparate()
       (%gl-blend-func-separate (%rls rls-gl-blend-src-factor-rgb) (%rls rls-gl-blend-dest-factor-rgb)
                                (%rls rls-gl-blend-src-factor-alpha) (%rls rls-gl-blend-dest-factor-alpha))
       (%gl-blend-equation-separate (%rls rls-gl-blend-equation-rgb) (%rls rls-gl-blend-equation-alpha))))
    (setf (%rls rls-current-blend-mode) mode
          (%rls rls-gl-custom-blend-mode-modified) nil)))

;; Set blending mode factor and equation
(defun rl-set-blend-factors (gl-src-factor gl-dst-factor gl-equation)
  (when (or (/= (%rls rls-gl-blend-src-factor) gl-src-factor)
            (/= (%rls rls-gl-blend-dst-factor) gl-dst-factor)
            (/= (%rls rls-gl-blend-equation) gl-equation))
    (setf (%rls rls-gl-blend-src-factor) gl-src-factor
          (%rls rls-gl-blend-dst-factor) gl-dst-factor
          (%rls rls-gl-blend-equation) gl-equation
          (%rls rls-gl-custom-blend-mode-modified) t)))

;; Set blending mode factor and equation separately for RGB and alpha
(defun rl-set-blend-factors-separate (gl-src-rgb gl-dst-rgb gl-src-alpha gl-dst-alpha gl-eq-rgb gl-eq-alpha)
  (when (or (/= (%rls rls-gl-blend-src-factor-rgb) gl-src-rgb)
            (/= (%rls rls-gl-blend-dest-factor-rgb) gl-dst-rgb)
            (/= (%rls rls-gl-blend-src-factor-alpha) gl-src-alpha)
            (/= (%rls rls-gl-blend-dest-factor-alpha) gl-dst-alpha)
            (/= (%rls rls-gl-blend-equation-rgb) gl-eq-rgb)
            (/= (%rls rls-gl-blend-equation-alpha) gl-eq-alpha))
    (setf (%rls rls-gl-blend-src-factor-rgb) gl-src-rgb
          (%rls rls-gl-blend-dest-factor-rgb) gl-dst-rgb
          (%rls rls-gl-blend-src-factor-alpha) gl-src-alpha
          (%rls rls-gl-blend-dest-factor-alpha) gl-dst-alpha
          (%rls rls-gl-blend-equation-rgb) gl-eq-rgb
          (%rls rls-gl-blend-equation-alpha) gl-eq-alpha
          (%rls rls-gl-custom-blend-mode-modified) t)))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition - rlgl functionality
;;;----------------------------------------------------------------------------------

;; Initialize rlgl: OpenGL extensions, default buffers/shaders/textures, OpenGL states
(defun rlgl-init (width height)
  (setf *is-gpu-ready* t)

  ;; Init default white texture
  (let ((pixels (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(255 255 255 255)))) ; 1 pixel RGBA (4 bytes)
    (setf (%rls rls-default-texture-id) (rl-load-texture pixels 1 1 +rl-pixelformat-uncompressed-r8g8b8a8+ 1)
          (%rls rls-current-texture-id) (%rls rls-default-texture-id)))

  (if (/= (%rls rls-default-texture-id) 0)
      (trace-log +log-info+ "TEXTURE: [ID ~d] Default texture loaded successfully" (%rls rls-default-texture-id))
      (trace-log +log-warning+ "TEXTURE: Failed to load default texture"))

  ;; Init default Shader (customized for GL 3.3 and ES2)
  ;; Loaded: RLGL.State.defaultShaderId + RLGL.State.defaultShaderLocs
  (%rl-load-shader-default)
  (setf (%rls rls-current-shader-id) (%rls rls-default-shader-id)
        (%rls rls-current-shader-locs) (%rls rls-default-shader-locs))

  ;; Init default vertex arrays buffers
  ;; Simulate that the default shader has the location RL_SHADER_LOC_VERTEX_NORMAL to bind the normal buffer for the default render batch
  (setf (aref (%rls rls-current-shader-locs) +rl-shader-loc-vertex-normal+) +rl-default-shader-attrib-location-normal+)
  (setf (rlgl-default-batch *rlgl*) (rl-load-render-batch +rl-default-batch-buffers+ +rl-default-batch-buffer-elements+))
  (setf (aref (%rls rls-current-shader-locs) +rl-shader-loc-vertex-normal+) -1)
  (setf (rlgl-current-batch *rlgl*) (rlgl-default-batch *rlgl*))

  ;; Init stack matrices (emulating OpenGL 1.1)
  (dotimes (i +rl-max-matrix-stack-size+) (setf (aref (%rls rls-stack) i) (%rl-matrix-identity)))

  ;; Init internal matrices
  (setf (%rls rls-transform) (%rl-matrix-identity)
        (%rls rls-projection) (%rl-matrix-identity)
        (%rls rls-modelview) (%rl-matrix-identity)
        (%rls rls-current-matrix) :modelview)

  ;; Initialize OpenGL default states
  ;;----------------------------------------------------------
  ;; Init state: Depth test
  (%gl-depth-func +gl-lequal+)          ; Type of depth testing to apply
  (%gl-disable +gl-depth-test+)         ; Disable depth testing for 2D (only used for 3D)

  ;; Init state: Blending mode
  (%gl-blend-func +rl-src-alpha+ +rl-one-minus-src-alpha+) ; Color blending function (how colors are mixed)
  (%gl-enable +gl-blend+)               ; Enable color blending (required to work with transparencies)

  ;; Init state: Culling
  ;; NOTE: All shapes/models triangles are drawn CCW
  (%gl-cull-face +gl-back+)             ; Cull the back face (default)
  (%gl-front-face +gl-ccw+)             ; Front face are defined counter clockwise (default)
  (%gl-enable +gl-cull-face+)           ; Enable backface culling

  ;; Init state: Cubemap seamless
  (%gl-enable +gl-texture-cube-map-seamless+) ; Seamless cubemaps (not supported on OpenGL ES 2.0)

  ;; Store screen size into global variables
  (setf (%rls rls-framebuffer-width) width
        (%rls rls-framebuffer-height) height)

  ;; Init state: Color/Depth buffers clear
  (%gl-clear-color 0.0 0.0 0.0 1.0)     ; Set clear color (black)
  (%gl-clear-depth 1d0)                 ; Set clear depth value (default)
  (%gl-clear (logior +gl-color-buffer-bit+ +gl-depth-buffer-bit+)) ; Clear color and depth buffers (depth buffer required for 3D)

  (trace-log +log-info+ "RLGL: Default OpenGL state initialized successfully"))

;; Vertex Buffer Object deinitialization (memory free)
(defun rlgl-close ()
  (rl-unload-render-batch (rlgl-default-batch *rlgl*))
  (%rl-unload-shader-default)           ; Unload default shader
  (%gl-delete-one %gl-delete-textures (%rls rls-default-texture-id)) ; Unload default texture
  (trace-log +log-info+ "TEXTURE: [ID ~d] Default texture unloaded successfully" (%rls rls-default-texture-id))
  (setf *is-gpu-ready* nil))

(defun %gl-extension-supported-p (name)
  "GLAD_GL_<name>: Check if the extension is listed by glGetStringi(GL_EXTENSIONS, i)"
  (let ((count (%gl-get-integer +gl-num-extensions+)))
    (dotimes (i count nil)
      (let ((p (%gl-get-stringi +gl-extensions+ i)))
        (when (and (not (cffi:null-pointer-p p)) (string= (cffi:foreign-string-to-lisp p) name))
          (return t))))))

;; Load OpenGL extensions
;; NOTE: External loader function must be provided
(defun rl-load-extensions (loader)
  ;; NOTE: glad is generated and contains only required OpenGL 3.3 Core extensions (and lower versions)
  (if (not (%gl-load-functions loader))
      (trace-log +log-warning+ "GLAD: Cannot load OpenGL extensions")
      (trace-log +log-info+ "GLAD: OpenGL extensions loaded successfully"))

  ;; Get number of supported extensions
  (trace-log +log-info+ "GL: Supported extensions count: ~d" (%gl-get-integer +gl-num-extensions+))

  ;; Register supported extensions flags
  ;; OpenGL 3.3 extensions supported by default (core)
  (let ((ext (rlgl-ext-supported *rlgl*)))
    (setf (rlext-vao ext) t
          (rlext-instancing ext) t
          (rlext-tex-npot ext) t
          (rlext-tex-float32 ext) t
          (rlext-tex-float16 ext) t
          (rlext-tex-depth ext) t
          (rlext-max-depth-bits ext) 32
          (rlext-tex-aniso-filter ext) t
          (rlext-tex-mirror-clamp ext) t)

    ;; Optional OpenGL 3.3 extensions
    (setf (rlext-tex-comp-astc ext) (and (%gl-extension-supported-p "GL_KHR_texture_compression_astc_hdr")
                                         (%gl-extension-supported-p "GL_KHR_texture_compression_astc_ldr"))
          (rlext-tex-comp-dxt ext) (%gl-extension-supported-p "GL_EXT_texture_compression_s3tc") ; Texture compression: DXT
          (rlext-tex-comp-etc2 ext) (%gl-extension-supported-p "GL_ARB_ES3_compatibility")) ; Texture compression: ETC2/EAC

    ;; Check OpenGL information and capabilities
    ;;------------------------------------------------------------------------------
    ;; Show current OpenGL and GLSL version
    (trace-log +log-info+ "GL: OpenGL device information:")
    (trace-log +log-info+ "    > Vendor:   ~a" (%gl-string +gl-vendor+))
    (trace-log +log-info+ "    > Renderer: ~a" (%gl-string +gl-renderer+))
    (trace-log +log-info+ "    > Version:  ~a" (%gl-string +gl-version+))
    (trace-log +log-info+ "    > GLSL:     ~a" (%gl-string +gl-shading-language-version+))

    (setf (rlgl-loader *rlgl*) loader)

    ;; NOTE: Anisotropy levels capability is an extension
    (setf (rlext-max-anisotropy-level ext) (%gl-get-float +gl-max-texture-max-anisotropy-ext+))

    ;; Show some basic info about GL supported features
    (if (rlext-vao ext)
        (trace-log +log-info+ "GL: VAO extension detected, VAO functions loaded successfully")
        (trace-log +log-warning+ "GL: VAO extension not found, VAO not supported"))
    (if (rlext-tex-npot ext)
        (trace-log +log-info+ "GL: NPOT textures extension detected, full NPOT textures supported")
        (trace-log +log-warning+ "GL: NPOT textures extension not found, limited NPOT support (no-mipmaps, no-repeat)"))
    (when (rlext-tex-comp-dxt ext) (trace-log +log-info+ "GL: DXT compressed textures supported"))
    (when (rlext-tex-comp-etc1 ext) (trace-log +log-info+ "GL: ETC1 compressed textures supported"))
    (when (rlext-tex-comp-etc2 ext) (trace-log +log-info+ "GL: ETC2/EAC compressed textures supported"))
    (when (rlext-tex-comp-pvrt ext) (trace-log +log-info+ "GL: PVRT compressed textures supported"))
    (when (rlext-tex-comp-astc ext) (trace-log +log-info+ "GL: ASTC compressed textures supported"))
    (when (rlext-compute-shader ext) (trace-log +log-info+ "GL: Compute shaders supported"))
    (when (rlext-ssbo ext) (trace-log +log-info+ "GL: Shader storage buffer objects supported"))))

;; Get OpenGL procedure address
(defun rl-get-proc-address (proc-name)
  (cffi:foreign-funcall-pointer (rlgl-loader *rlgl*) () :string proc-name :pointer))

;; Get current OpenGL version
(defun rl-get-version ()
  +rl-opengl-33+)

;; Set current framebuffer width
(defun rl-set-framebuffer-width (width)
  (setf (%rls rls-framebuffer-width) width))

;; Set current framebuffer height
(defun rl-set-framebuffer-height (height)
  (setf (%rls rls-framebuffer-height) height))

;; Get default framebuffer width
(defun rl-get-framebuffer-width ()
  (%rls rls-framebuffer-width))

;; Get default framebuffer height
(defun rl-get-framebuffer-height ()
  (%rls rls-framebuffer-height))

;; Get default internal texture (white texture)
;; NOTE: Default texture is a 1x1 pixel UNCOMPRESSED_R8G8B8A8
(defun rl-get-texture-id-default ()
  (%rls rls-default-texture-id))

;; Get default shader id
(defun rl-get-shader-id-default ()
  (%rls rls-default-shader-id))

;; Get default shader locs
(defun rl-get-shader-locs-default ()
  (%rls rls-default-shader-locs))

;;;----------------------------------------------------------------------------------
;;; Render batch management
;;;----------------------------------------------------------------------------------

;; Load render batch
(defun rl-load-render-batch (num-buffers buffer-elements)
  (let ((batch (make-rl-render-batch)))
    (unless *is-gpu-ready*
      (trace-log +log-warning+ "GL: GPU is not ready to load data, trying to load before InitWindow()?")
      (return-from rl-load-render-batch batch))

    ;; Initialize CPU (RAM) vertex buffers (position, texcoord, color data and indexes)
    ;;--------------------------------------------------------------------------------------------
    (setf (rl-render-batch-vertex-buffer batch) (make-array num-buffers))
    (dotimes (i num-buffers)
      (let ((indices (make-array (* buffer-elements 6) :element-type '(unsigned-byte 32) :initial-element 0)))
        ;; Indices can be initialized right now
        (loop for j from 0 below (* 6 buffer-elements) by 6
              for k from 0
              do (setf (aref indices j) (* 4 k)
                       (aref indices (+ j 1)) (+ (* 4 k) 1)
                       (aref indices (+ j 2)) (+ (* 4 k) 2)
                       (aref indices (+ j 3)) (* 4 k)
                       (aref indices (+ j 4)) (+ (* 4 k) 2)
                       (aref indices (+ j 5)) (+ (* 4 k) 3)))
        (setf (svref (rl-render-batch-vertex-buffer batch) i)
              (make-rl-vertex-buffer
               :element-count buffer-elements
               :vertices (make-array (* buffer-elements 3 4) :element-type 'single-float :initial-element 0.0)  ; 3 float by vertex, 4 vertex by quad
               :texcoords (make-array (* buffer-elements 2 4) :element-type 'single-float :initial-element 0.0) ; 2 float by texcoord, 4 texcoord by quad
               :normals (make-array (* buffer-elements 3 4) :element-type 'single-float :initial-element 0.0)   ; 3 float by vertex, 4 vertex by quad
               :colors (make-array (* buffer-elements 4 4) :element-type '(unsigned-byte 8) :initial-element 0) ; 4 float by color, 4 colors by quad
               :indices indices)))                                                                             ; 6 int by quad (indices)
      (setf (%rls rls-vertex-counter) 0))

    (trace-log +log-info+ "RLGL: Render batch vertex buffers loaded successfully in RAM (CPU)")
    ;;--------------------------------------------------------------------------------------------

    ;; Upload to GPU (VRAM) vertex data and initialize VAOs/VBOs
    ;;--------------------------------------------------------------------------------------------
    (let ((locs (%rls rls-current-shader-locs)))
      (flet ((vbo (buffer index target data usage location size type normalized)
               (let ((id (%gl-gen-one %gl-gen-buffers)))
                 (setf (aref (rl-vertex-buffer-vbo-id buffer) index) id)
                 (%gl-bind-buffer target id)
                 (cffi:with-pointer-to-vector-data (p data)
                   (%gl-buffer-data target (%c-data-size data) p usage))
                 (when location
                   (%gl-enable-vertex-attrib-array location)
                   (%gl-vertex-attrib-pointer location size type normalized 0 (cffi:null-pointer))))))
        (dotimes (i num-buffers)
          (let ((buffer (svref (rl-render-batch-vertex-buffer batch) i)))
            (when (%rlext rlext-vao)
              ;; Initialize Quads VAO
              (setf (rl-vertex-buffer-vao-id buffer) (%gl-gen-one %gl-gen-vertex-arrays))
              (%gl-bind-vertex-array (rl-vertex-buffer-vao-id buffer)))
            ;; Quads - Vertex buffers binding and attributes enable
            ;; Vertex position buffer (shader-location = 0)
            (vbo buffer 0 +gl-array-buffer+ (rl-vertex-buffer-vertices buffer) +gl-dynamic-draw+
                 (aref locs +rl-shader-loc-vertex-position+) 3 +gl-float+ 0)
            ;; Vertex texcoord buffer (shader-location = 1)
            (vbo buffer 1 +gl-array-buffer+ (rl-vertex-buffer-texcoords buffer) +gl-dynamic-draw+
                 (aref locs +rl-shader-loc-vertex-texcoord01+) 2 +gl-float+ 0)
            ;; Vertex normal buffer (shader-location = 2)
            (vbo buffer 2 +gl-array-buffer+ (rl-vertex-buffer-normals buffer) +gl-dynamic-draw+
                 (aref locs +rl-shader-loc-vertex-normal+) 3 +gl-float+ 0)
            ;; Vertex color buffer (shader-location = 3)
            (vbo buffer 3 +gl-array-buffer+ (rl-vertex-buffer-colors buffer) +gl-dynamic-draw+
                 (aref locs +rl-shader-loc-vertex-color+) 4 +gl-unsigned-byte+ +gl-true+)
            ;; Fill index buffer
            (vbo buffer 4 +gl-element-array-buffer+ (rl-vertex-buffer-indices buffer) +gl-static-draw+
                 nil 0 0 0)))))

    (trace-log +log-info+ "RLGL: Render batch vertex buffers loaded successfully in VRAM (GPU)")

    ;; Unbind the current VAO
    (when (%rlext rlext-vao) (%gl-bind-vertex-array 0))
    ;;--------------------------------------------------------------------------------------------

    ;; Init draw calls tracking system
    ;;--------------------------------------------------------------------------------------------
    (setf (rl-render-batch-draws batch) (make-array +rl-default-batch-drawcalls+))
    (dotimes (i +rl-default-batch-drawcalls+)
      (setf (svref (rl-render-batch-draws batch) i)
            (make-rl-draw-call :mode +rl-quads+
                               :vertex-count 0
                               :vertex-alignment 0
                               :texture-id (%rls rls-default-texture-id))))

    (setf (rl-render-batch-buffer-count batch) num-buffers ; Record buffer count
          (rl-render-batch-draw-counter batch) 1           ; Reset draws counter
          (rl-render-batch-current-depth batch) -1.0)      ; Reset depth value
    ;;--------------------------------------------------------------------------------------------
    batch))

;; Unload default internal buffers vertex data from CPU and GPU
(defun rl-unload-render-batch (batch)
  ;; Unbind everything
  (%gl-bind-buffer +gl-array-buffer+ 0)
  (%gl-bind-buffer +gl-element-array-buffer+ 0)

  ;; Unload all vertex buffers data
  (dotimes (i (rl-render-batch-buffer-count batch))
    (let ((buffer (svref (rl-render-batch-vertex-buffer batch) i)))
      ;; Unbind VAO attribs data
      (when (%rlext rlext-vao)
        (%gl-bind-vertex-array (rl-vertex-buffer-vao-id buffer))
        (%gl-disable-vertex-attrib-array +rl-default-shader-attrib-location-position+)
        (%gl-disable-vertex-attrib-array +rl-default-shader-attrib-location-texcoord+)
        (%gl-disable-vertex-attrib-array +rl-default-shader-attrib-location-normal+)
        (%gl-disable-vertex-attrib-array +rl-default-shader-attrib-location-color+)
        (%gl-bind-vertex-array 0))

      ;; Delete VBOs from GPU (VRAM)
      (dotimes (j 5) (%gl-delete-one %gl-delete-buffers (aref (rl-vertex-buffer-vbo-id buffer) j)))

      ;; Delete VAOs from GPU (VRAM)
      (when (%rlext rlext-vao) (%gl-delete-one %gl-delete-vertex-arrays (rl-vertex-buffer-vao-id buffer)))))

  ;; Unload arrays
  (setf (rl-render-batch-vertex-buffer batch) #()
        (rl-render-batch-draws batch) #()))

(defun %rl-set-uniform-matrix-loc (location mat)
  "glUniformMatrix4fv(location, 1, false, rlMatrixToFloat(mat))"
  (let ((v (%rl-matrix-to-float mat)))
    (cffi:with-pointer-to-vector-data (p v)
      (%gl-uniform-matrix-4fv location 1 +gl-false+ p))))

;; Draw render batch
;; NOTE: Batch is reseted and current buffer is updated (for multi-buffer config)
(defun rl-draw-render-batch (batch)
  ;; Update batch vertex buffers
  ;;------------------------------------------------------------------------------------------------------------
  ;; NOTE: If there is not vertex data, buffers doesn't need to be updated (vertexCount > 0)
  (let ((vertex-counter (%rls rls-vertex-counter))
        (buffer (svref (rl-render-batch-vertex-buffer batch) (rl-render-batch-current-buffer batch))))
    (when (> vertex-counter 0)
      ;; Activate elements VAO
      (when (%rlext rlext-vao) (%gl-bind-vertex-array (rl-vertex-buffer-vao-id buffer)))

      (flet ((update (index data size)
               (%gl-bind-buffer +gl-array-buffer+ (aref (rl-vertex-buffer-vbo-id buffer) index))
               (cffi:with-pointer-to-vector-data (p data)
                 (%gl-buffer-sub-data +gl-array-buffer+ 0 size p))))
        ;; Vertex positions buffer
        (update 0 (rl-vertex-buffer-vertices buffer) (* vertex-counter 3 4))
        ;; Texture coordinates buffer
        (update 1 (rl-vertex-buffer-texcoords buffer) (* vertex-counter 2 4))
        ;; Normals buffer
        (update 2 (rl-vertex-buffer-normals buffer) (* vertex-counter 3 4))
        ;; Colors buffer
        (update 3 (rl-vertex-buffer-colors buffer) (* vertex-counter 4)))

      ;; Unbind the current VAO
      (when (%rlext rlext-vao) (%gl-bind-vertex-array 0)))
    ;;------------------------------------------------------------------------------------------------------------

    ;; Draw batch vertex buffers (considering VR stereo if required)
    ;;------------------------------------------------------------------------------------------------------------
    (let ((mat-projection (%rls rls-projection))
          (mat-model-view (%rls rls-modelview))
          (eye-count (if (%rls rls-stereo-render) 2 1)))
      (dotimes (eye eye-count)
        (when (= eye-count 2)
          ;; Setup current eye viewport (half screen width)
          (rl-viewport (floor (* eye (%rls rls-framebuffer-width)) 2) 0
                       (floor (%rls rls-framebuffer-width) 2) (%rls rls-framebuffer-height))
          ;; Set current eye view offset to modelview matrix
          (rl-set-matrix-modelview (%rl-matrix-multiply mat-model-view (aref (%rls rls-view-offset-stereo) eye)))
          ;; Set current eye projection matrix
          (rl-set-matrix-projection (aref (%rls rls-projection-stereo) eye)))

        ;; Draw buffers
        (when (> (%rls rls-vertex-counter) 0)
          (let ((locs (%rls rls-current-shader-locs)))
            ;; Set current shader and upload current MVP matrix
            (%gl-use-program (%rls rls-current-shader-id))

            ;; Create modelview-projection matrix and upload to shader
            (let ((mat-mvp (%rl-matrix-multiply (%rls rls-modelview) (%rls rls-projection))))
              (%rl-set-uniform-matrix-loc (aref locs +rl-shader-loc-matrix-mvp+) mat-mvp))

            (when (/= (aref locs +rl-shader-loc-matrix-projection+) -1)
              (%rl-set-uniform-matrix-loc (aref locs +rl-shader-loc-matrix-projection+) (%rls rls-projection)))

            ;; WARNING: For the following setup of the view, model, and normal matrices, it is expected that
            ;; transformations and rendering occur between rlPushMatrix() and rlPopMatrix()

            (when (/= (aref locs +rl-shader-loc-matrix-view+) -1)
              (%rl-set-uniform-matrix-loc (aref locs +rl-shader-loc-matrix-view+) (%rls rls-modelview)))

            (when (/= (aref locs +rl-shader-loc-matrix-model+) -1)
              (%rl-set-uniform-matrix-loc (aref locs +rl-shader-loc-matrix-model+) (%rls rls-transform)))

            (when (/= (aref locs +rl-shader-loc-matrix-normal+) -1)
              (%rl-set-uniform-matrix-loc (aref locs +rl-shader-loc-matrix-normal+)
                                          (%rl-matrix-transpose (%rl-matrix-invert (%rls rls-transform)))))

            (when (%rlext rlext-vao) (%gl-bind-vertex-array (rl-vertex-buffer-vao-id buffer)))

            ;; Setup some default shader values
            (%gl-uniform-4f (aref locs +rl-shader-loc-color-diffuse+) 1.0 1.0 1.0 1.0)
            (%gl-uniform-1i (aref locs +rl-shader-loc-map-diffuse+) 0) ; Active default sampler2D: texture0

            ;; Activate additional sampler textures
            ;; Those additional textures will be common for all draw calls of the batch
            (dotimes (i +rl-default-batch-max-texture-units+)
              (when (> (aref (%rls rls-active-texture-id) i) 0)
                (%gl-active-texture (+ +gl-texture0+ 1 i))
                (%gl-bind-texture +gl-texture-2d+ (aref (%rls rls-active-texture-id) i))))

            ;; Activate default sampler2D texture0 (one texture is always active for default batch shader)
            ;; NOTE: Batch system accumulates calls by texture0 changes, additional textures are enabled for all the draw calls
            (%gl-active-texture +gl-texture0+)

            (let ((vertex-offset 0))
              (dotimes (i (rl-render-batch-draw-counter batch))
                (let ((draw (svref (rl-render-batch-draws batch) i)))
                  ;; Bind current draw call texture, activated as GL_TEXTURE0 and bound to sampler2D texture0 by default
                  (%gl-bind-texture +gl-texture-2d+ (rl-draw-call-texture-id draw))

                  (if (or (= (rl-draw-call-mode draw) +rl-lines+) (= (rl-draw-call-mode draw) +rl-triangles+))
                      (%gl-draw-arrays (rl-draw-call-mode draw) vertex-offset (rl-draw-call-vertex-count draw))
                      ;; The number of indices to be processed needs to be defined: elementCount*6
                      ;; NOTE: The final parameter tells the GPU the offset in bytes from the
                      ;; start of the index buffer to the location of the first index to process
                      (%gl-draw-elements +gl-triangles+ (* (truncate (rl-draw-call-vertex-count draw) 4) 6) +gl-unsigned-int+
                                         (cffi:make-pointer (* (truncate vertex-offset 4) 6 4))))

                  (incf vertex-offset (+ (rl-draw-call-vertex-count draw) (rl-draw-call-vertex-alignment draw))))))

            (%gl-bind-texture +gl-texture-2d+ 0))) ; Unbind textures

        (when (%rlext rlext-vao) (%gl-bind-vertex-array 0)) ; Unbind VAO

        (%gl-use-program 0))            ; Unbind shader program

      ;; Restore viewport to default measures
      (when (= eye-count 2) (rl-viewport 0 0 (%rls rls-framebuffer-width) (%rls rls-framebuffer-height)))
      ;;------------------------------------------------------------------------------------------------------------

      ;; Reset batch buffers
      ;;------------------------------------------------------------------------------------------------------------
      ;; Reset vertex counter for next frame
      (setf (%rls rls-vertex-counter) 0)

      ;; Reset depth for next draw
      (setf (rl-render-batch-current-depth batch) -1.0)

      ;; Restore projection/modelview matrices
      (setf (%rls rls-projection) mat-projection
            (%rls rls-modelview) mat-model-view))

    ;; Reset RLGL.currentBatch->draws array
    (dotimes (i +rl-default-batch-drawcalls+)
      (let ((draw (svref (rl-render-batch-draws batch) i)))
        (setf (rl-draw-call-mode draw) +rl-quads+
              (rl-draw-call-vertex-count draw) 0
              (rl-draw-call-texture-id draw) (%rls rls-default-texture-id))))

    ;; Reset active texture units for next batch
    (fill (%rls rls-active-texture-id) 0)

    ;; Reset draws counter to one draw for the batch
    (setf (rl-render-batch-draw-counter batch) 1)
    ;;------------------------------------------------------------------------------------------------------------

    ;; Change to next buffer in the list (in case of multi-buffering)
    (incf (rl-render-batch-current-buffer batch))
    (when (>= (rl-render-batch-current-buffer batch) (rl-render-batch-buffer-count batch))
      (setf (rl-render-batch-current-buffer batch) 0))))

;; Set the active render batch for rlgl
(defun rl-set-render-batch-active (batch)
  (rl-draw-render-batch (rlgl-current-batch *rlgl*))
  (setf (rlgl-current-batch *rlgl*) (or batch (rlgl-default-batch *rlgl*))))

;; Update and draw internal render batch
(defun rl-draw-render-batch-active ()
  (rl-draw-render-batch (rlgl-current-batch *rlgl*))) ; NOTE: Stereo rendering is checked inside

;; Check internal buffer overflow for a given number of vertex
;; and force a rlRenderBatch draw call if required
(defun rl-check-render-batch-limit (v-count)
  (let ((overflow nil)
        (batch (rlgl-current-batch *rlgl*)))
    (when (>= (+ (%rls rls-vertex-counter) v-count)
              (* (rl-vertex-buffer-element-count
                  (svref (rl-render-batch-vertex-buffer batch) (rl-render-batch-current-buffer batch)))
                 4))
      (setf overflow t)
      ;; Store current primitive drawing mode and texture id
      (let ((current-mode (rl-draw-call-mode (%rl-last-draw batch)))
            (current-texture (rl-draw-call-texture-id (%rl-last-draw batch))))
        (rl-draw-render-batch batch)    ; NOTE: Stereo rendering is checked inside
        ;; Restore state of last batch so new vertices can be added
        (setf (rl-draw-call-mode (%rl-last-draw batch)) current-mode
              (rl-draw-call-texture-id (%rl-last-draw batch)) current-texture)))
    overflow))

;;;----------------------------------------------------------------------------------
;;; Textures data management
;;;----------------------------------------------------------------------------------

(defun %rl-set-swizzle (target format)
  "Grayscale/gray-alpha textures swizzle mask (GL_TEXTURE_SWIZZLE_RGBA)"
  (cond ((= format +rl-pixelformat-uncompressed-grayscale+)
         (cffi:with-foreign-object (mask :int 4)
           (setf (cffi:mem-aref mask :int 0) +gl-red+ (cffi:mem-aref mask :int 1) +gl-red+
                 (cffi:mem-aref mask :int 2) +gl-red+ (cffi:mem-aref mask :int 3) +gl-one+)
           (%gl-tex-parameteriv target +gl-texture-swizzle-rgba+ mask)))
        ((= format +rl-pixelformat-uncompressed-gray-alpha+)
         (cffi:with-foreign-object (mask :int 4)
           (setf (cffi:mem-aref mask :int 0) +gl-red+ (cffi:mem-aref mask :int 1) +gl-red+
                 (cffi:mem-aref mask :int 2) +gl-red+ (cffi:mem-aref mask :int 3) +gl-green+)
           (%gl-tex-parameteriv target +gl-texture-swizzle-rgba+ mask)))))

;; Convert image data to OpenGL texture (returns OpenGL valid Id)
;; NOTE: DATA is a byte vector (all mipmap levels), a foreign pointer or NIL
(defun rl-load-texture (data width height format mipmap-count)
  (let ((id 0))
    (unless *is-gpu-ready*
      (trace-log +log-warning+ "GL: GPU is not ready to load data, trying to load before InitWindow()?")
      (return-from rl-load-texture id))

    (%gl-bind-texture +gl-texture-2d+ 0) ; Free any old binding

    ;; Check texture format support by OpenGL 1.1 (compressed textures not supported)
    (when (and (not (%rlext rlext-tex-comp-dxt))
               (member format (list +rl-pixelformat-compressed-dxt1-rgb+ +rl-pixelformat-compressed-dxt1-rgba+
                                    +rl-pixelformat-compressed-dxt3-rgba+ +rl-pixelformat-compressed-dxt5-rgba+)))
      (trace-log +log-warning+ "GL: DXT compressed texture format not supported")
      (return-from rl-load-texture id))
    (when (and (not (%rlext rlext-tex-comp-etc1)) (= format +rl-pixelformat-compressed-etc1-rgb+))
      (trace-log +log-warning+ "GL: ETC1 compressed texture format not supported")
      (return-from rl-load-texture id))
    (when (and (not (%rlext rlext-tex-comp-etc2))
               (member format (list +rl-pixelformat-compressed-etc2-rgb+ +rl-pixelformat-compressed-etc2-eac-rgba+)))
      (trace-log +log-warning+ "GL: ETC2 compressed texture format not supported")
      (return-from rl-load-texture id))
    (when (and (not (%rlext rlext-tex-comp-pvrt))
               (member format (list +rl-pixelformat-compressed-pvrt-rgb+ +rl-pixelformat-compressed-pvrt-rgba+)))
      (trace-log +log-warning+ "GL: PVRT compressed texture format not supported")
      (return-from rl-load-texture id))
    (when (and (not (%rlext rlext-tex-comp-astc))
               (member format (list +rl-pixelformat-compressed-astc-4x4-rgba+ +rl-pixelformat-compressed-astc-8x8-rgba+)))
      (trace-log +log-warning+ "GL: ASTC compressed texture format not supported")
      (return-from rl-load-texture id))

    (%gl-pixel-storei +gl-unpack-alignment+ 1)

    (setf id (%gl-gen-one %gl-gen-textures)) ; Generate texture id

    (%gl-bind-texture +gl-texture-2d+ id)

    (let ((mip-width width)
          (mip-height height)
          (mip-offset 0))               ; Mipmap data offset
      (%with-c-data (data-ptr data :uchar)
        ;; Load the different mipmap levels
        (dotimes (i mipmap-count)
          (let ((mip-size (%rl-get-pixel-data-size mip-width mip-height format))
                (ptr (if data (cffi:inc-pointer data-ptr mip-offset) (cffi:null-pointer))))
            (multiple-value-bind (gl-internal-format gl-format gl-type) (rl-get-gl-texture-formats format)
              (trace-log +log-debug+ "TEXTURE: Load mipmap level ~d (~d x ~d), size: ~d, offset: ~d" i mip-width mip-height mip-size mip-offset)
              (when (/= gl-internal-format 0)
                (if (< format +rl-pixelformat-compressed-dxt1-rgb+)
                    (%gl-tex-image-2d +gl-texture-2d+ i gl-internal-format mip-width mip-height 0 gl-format gl-type ptr)
                    (%gl-compressed-tex-image-2d +gl-texture-2d+ i gl-internal-format mip-width mip-height 0 mip-size ptr))
                (%rl-set-swizzle +gl-texture-2d+ format)))
            (setf mip-width (truncate mip-width 2)
                  mip-height (truncate mip-height 2))
            (incf mip-offset mip-size)  ; Increment offset position to next mipmap
            ;; Security check for NPOT textures
            (when (< mip-width 1) (setf mip-width 1))
            (when (< mip-height 1) (setf mip-height 1))))))

    ;; Texture parameters configuration
    ;; NOTE: glTexParameteri does NOT affect texture uploading
    (%gl-tex-parameteri +gl-texture-2d+ +gl-texture-wrap-s+ +gl-repeat+) ; Set texture to repeat on x-axis
    (%gl-tex-parameteri +gl-texture-2d+ +gl-texture-wrap-t+ +gl-repeat+) ; Set texture to repeat on y-axis

    ;; Magnification and minification filters
    (%gl-tex-parameteri +gl-texture-2d+ +gl-texture-mag-filter+ +gl-nearest+) ; Alternative: GL_LINEAR
    (%gl-tex-parameteri +gl-texture-2d+ +gl-texture-min-filter+ +gl-nearest+) ; Alternative: GL_LINEAR

    (when (> mipmap-count 1)
      ;; Activate trilinear filtering if mipmaps are available
      (%gl-tex-parameteri +gl-texture-2d+ +gl-texture-mag-filter+ +gl-linear+)
      (%gl-tex-parameteri +gl-texture-2d+ +gl-texture-min-filter+ +gl-linear-mipmap-linear+)
      ;; Define the maximum number of mipmap levels to be used, 0 is base texture size
      (%gl-tex-parameteri +gl-texture-2d+ +gl-texture-base-level+ 0)
      (%gl-tex-parameteri +gl-texture-2d+ +gl-texture-max-level+ (1- mipmap-count)))

    ;; At this point texture is loaded in GPU and texture parameters configured

    ;; NOTE: If mipmaps were not in data, they are not generated automatically

    ;; Unbind current texture
    (%gl-bind-texture +gl-texture-2d+ 0)

    (if (> id 0)
        (trace-log +log-info+ "TEXTURE: [ID ~d] Texture loaded successfully (~dx~d | ~a | ~d mipmaps)"
                   id width height (rl-get-pixel-format-name format) mipmap-count)
        (trace-log +log-warning+ "TEXTURE: Failed to load texture"))
    id))

;; Load depth texture/renderbuffer (to be attached to fbo)
;; WARNING: OpenGL ES 2.0 requires GL_OES_depth_texture and WebGL requires WEBGL_depth_texture extensions
(defun rl-load-texture-depth (width height use-render-buffer)
  (let ((id 0))
    (unless *is-gpu-ready*
      (trace-log +log-warning+ "GL: GPU is not ready to load data, trying to load before InitWindow()?")
      (return-from rl-load-texture-depth id))

    ;; In case depth textures were not supported, force renderbuffer usage
    (unless (%rlext rlext-tex-depth) (setf use-render-buffer t))

    ;; NOTE: Letting the implementation to choose the best bit-depth
    ;; Possible formats: GL_DEPTH_COMPONENT16, GL_DEPTH_COMPONENT24, GL_DEPTH_COMPONENT32 and GL_DEPTH_COMPONENT32F
    (let ((gl-internal-format +gl-depth-component+))
      (if (and (not use-render-buffer) (%rlext rlext-tex-depth))
          (progn
            (setf id (%gl-gen-one %gl-gen-textures))
            (%gl-bind-texture +gl-texture-2d+ id)
            (%gl-tex-image-2d +gl-texture-2d+ 0 gl-internal-format width height 0 +gl-depth-component+ +gl-unsigned-int+ (cffi:null-pointer))

            (%gl-tex-parameteri +gl-texture-2d+ +gl-texture-min-filter+ +gl-nearest+)
            (%gl-tex-parameteri +gl-texture-2d+ +gl-texture-mag-filter+ +gl-nearest+)
            (%gl-tex-parameteri +gl-texture-2d+ +gl-texture-wrap-s+ +gl-clamp-to-edge+)
            (%gl-tex-parameteri +gl-texture-2d+ +gl-texture-wrap-t+ +gl-clamp-to-edge+)

            (%gl-bind-texture +gl-texture-2d+ 0)

            (trace-log +log-info+ "TEXTURE: Depth texture loaded successfully"))
          (progn
            ;; Create the renderbuffer that will serve as the depth attachment for the framebuffer
            ;; NOTE: A renderbuffer is simpler than a texture and could offer better performance on embedded devices
            (setf id (%gl-gen-one %gl-gen-renderbuffers))
            (%gl-bind-renderbuffer +gl-renderbuffer+ id)
            (%gl-renderbuffer-storage +gl-renderbuffer+ gl-internal-format width height)

            (%gl-bind-renderbuffer +gl-renderbuffer+ 0)

            (trace-log +log-info+ "TEXTURE: [ID ~d] Depth renderbuffer loaded successfully (~d bits)" id
                       (if (>= (%rlext rlext-max-depth-bits) 24) (%rlext rlext-max-depth-bits) 16)))))
    id))

;; Load texture cubemap
;; NOTE: Cubemap data is expected to be 6 images in a single data array (one after the other),
;; expected the following convention: +X, -X, +Y, -Y, +Z, -Z
(defun rl-load-texture-cubemap (data size format mipmap-count)
  (let ((id 0))
    (unless *is-gpu-ready*
      (trace-log +log-warning+ "GL: GPU is not ready to load data, trying to load before InitWindow()?")
      (return-from rl-load-texture-cubemap id))

    (let ((mip-size size)
          (data-offset 0)
          (data-size (%rl-get-pixel-data-size size size format)))

      (setf id (%gl-gen-one %gl-gen-textures))
      (%gl-bind-texture +gl-texture-cube-map+ id)

      (multiple-value-bind (gl-internal-format gl-format gl-type) (rl-get-gl-texture-formats format)
        (when (/= gl-internal-format 0)
          (%with-c-data (data-ptr data :uchar)
            ;; Load cubemap faces/mipmaps
            (dotimes (i (* 6 mipmap-count))
              (let ((mipmap-level (truncate i 6))
                    (face (mod i 6)))
                (if (null data)
                    (if (< format +rl-pixelformat-compressed-dxt1-rgb+)
                        (if (member format (list +rl-pixelformat-uncompressed-r32+ +rl-pixelformat-uncompressed-r32g32b32a32+
                                                 +rl-pixelformat-uncompressed-r16+ +rl-pixelformat-uncompressed-r16g16b16a16+))
                            (trace-log +log-warning+ "TEXTURES: Cubemap requested format not supported")
                            (%gl-tex-image-2d (+ +gl-texture-cube-map-positive-x+ face) mipmap-level gl-internal-format
                                              mip-size mip-size 0 gl-format gl-type (cffi:null-pointer)))
                        (trace-log +log-warning+ "TEXTURES: Empty cubemap creation does not support compressed format"))
                    (let ((ptr (cffi:inc-pointer data-ptr (+ data-offset (* face data-size)))))
                      (if (< format +rl-pixelformat-compressed-dxt1-rgb+)
                          (%gl-tex-image-2d (+ +gl-texture-cube-map-positive-x+ face) mipmap-level gl-internal-format
                                            mip-size mip-size 0 gl-format gl-type ptr)
                          (%gl-compressed-tex-image-2d (+ +gl-texture-cube-map-positive-x+ face) mipmap-level gl-internal-format
                                                       mip-size mip-size 0 data-size ptr))))

                (%rl-set-swizzle +gl-texture-cube-map+ format)

                (when (= face 5)
                  (setf mip-size (truncate mip-size 2))
                  (when data (incf data-offset (* data-size 6))) ; Increment data pointer to next mipmap
                  ;; Security check for NPOT textures
                  (when (< mip-size 1) (setf mip-size 1))
                  (setf data-size (%rl-get-pixel-data-size mip-size mip-size format)))))))))

    ;; Set cubemap texture sampling parameters
    (if (> mipmap-count 1)
        (%gl-tex-parameteri +gl-texture-cube-map+ +gl-texture-min-filter+ +gl-linear-mipmap-linear+)
        (%gl-tex-parameteri +gl-texture-cube-map+ +gl-texture-min-filter+ +gl-linear+))

    (%gl-tex-parameteri +gl-texture-cube-map+ +gl-texture-mag-filter+ +gl-linear+)
    (%gl-tex-parameteri +gl-texture-cube-map+ +gl-texture-wrap-s+ +gl-clamp-to-edge+)
    (%gl-tex-parameteri +gl-texture-cube-map+ +gl-texture-wrap-t+ +gl-clamp-to-edge+)
    (%gl-tex-parameteri +gl-texture-cube-map+ +gl-texture-wrap-r+ +gl-clamp-to-edge+) ; Flag not supported on OpenGL ES 2.0

    (%gl-bind-texture +gl-texture-cube-map+ 0)

    (if (> id 0)
        (trace-log +log-info+ "TEXTURE: [ID ~d] Cubemap texture loaded successfully (~dx~d)" id size size)
        (trace-log +log-warning+ "TEXTURE: Failed to load cubemap texture"))
    id))

;; Update already loaded texture in GPU with new data
;; WARNING: Not possible to know safely if internal texture format is the expected one...
(defun rl-update-texture (id offset-x offset-y width height format data)
  (%gl-bind-texture +gl-texture-2d+ id)
  (multiple-value-bind (gl-internal-format gl-format gl-type) (rl-get-gl-texture-formats format)
    (if (and (/= gl-internal-format 0) (< format +rl-pixelformat-compressed-dxt1-rgb+))
        (%with-c-data (ptr data :uchar)
          (%gl-tex-sub-image-2d +gl-texture-2d+ 0 offset-x offset-y width height gl-format gl-type ptr))
        (trace-log +log-warning+ "TEXTURE: [ID ~d] Failed to update for current texture format (~d)" id format))))

;; Get OpenGL internal formats and data type from raylib PixelFormat
;; NOTE: Returns (values glInternalFormat glFormat glType), 0 for unsupported formats
(defun rl-get-gl-texture-formats (format)
  (let ((gl-internal-format 0) (gl-format 0) (gl-type 0)
        (ext (rlgl-ext-supported *rlgl*)))
    (macrolet ((set3 (i f ty) `(setf gl-internal-format ,i gl-format ,f gl-type ,ty)))
      (case format
        (#.+rl-pixelformat-uncompressed-grayscale+ (set3 +gl-r8+ +gl-red+ +gl-unsigned-byte+))
        (#.+rl-pixelformat-uncompressed-gray-alpha+ (set3 +gl-rg8+ +gl-rg+ +gl-unsigned-byte+))
        (#.+rl-pixelformat-uncompressed-r5g6b5+ (set3 +gl-rgb565+ +gl-rgb+ +gl-unsigned-short-5-6-5+))
        (#.+rl-pixelformat-uncompressed-r8g8b8+ (set3 +gl-rgb8+ +gl-rgb+ +gl-unsigned-byte+))
        (#.+rl-pixelformat-uncompressed-r5g5b5a1+ (set3 +gl-rgb5-a1+ +gl-rgba+ +gl-unsigned-short-5-5-5-1+))
        (#.+rl-pixelformat-uncompressed-r4g4b4a4+ (set3 +gl-rgba4+ +gl-rgba+ +gl-unsigned-short-4-4-4-4+))
        (#.+rl-pixelformat-uncompressed-r8g8b8a8+ (set3 +gl-rgba8+ +gl-rgba+ +gl-unsigned-byte+))
        (#.+rl-pixelformat-uncompressed-r32+
         (when (rlext-tex-float32 ext) (setf gl-internal-format +gl-r32f+)) (setf gl-format +gl-red+ gl-type +gl-float+))
        (#.+rl-pixelformat-uncompressed-r32g32b32+
         (when (rlext-tex-float32 ext) (setf gl-internal-format +gl-rgb32f+)) (setf gl-format +gl-rgb+ gl-type +gl-float+))
        (#.+rl-pixelformat-uncompressed-r32g32b32a32+
         (when (rlext-tex-float32 ext) (setf gl-internal-format +gl-rgba32f+)) (setf gl-format +gl-rgba+ gl-type +gl-float+))
        (#.+rl-pixelformat-uncompressed-r16+
         (when (rlext-tex-float16 ext) (setf gl-internal-format +gl-r16f+)) (setf gl-format +gl-red+ gl-type +gl-half-float+))
        (#.+rl-pixelformat-uncompressed-r16g16b16+
         (when (rlext-tex-float16 ext) (setf gl-internal-format +gl-rgb16f+)) (setf gl-format +gl-rgb+ gl-type +gl-half-float+))
        (#.+rl-pixelformat-uncompressed-r16g16b16a16+
         (when (rlext-tex-float16 ext) (setf gl-internal-format +gl-rgba16f+)) (setf gl-format +gl-rgba+ gl-type +gl-half-float+))
        (#.+rl-pixelformat-compressed-dxt1-rgb+ (when (rlext-tex-comp-dxt ext) (setf gl-internal-format +gl-compressed-rgb-s3tc-dxt1-ext+)))
        (#.+rl-pixelformat-compressed-dxt1-rgba+ (when (rlext-tex-comp-dxt ext) (setf gl-internal-format +gl-compressed-rgba-s3tc-dxt1-ext+)))
        (#.+rl-pixelformat-compressed-dxt3-rgba+ (when (rlext-tex-comp-dxt ext) (setf gl-internal-format +gl-compressed-rgba-s3tc-dxt3-ext+)))
        (#.+rl-pixelformat-compressed-dxt5-rgba+ (when (rlext-tex-comp-dxt ext) (setf gl-internal-format +gl-compressed-rgba-s3tc-dxt5-ext+)))
        (#.+rl-pixelformat-compressed-etc1-rgb+ (when (rlext-tex-comp-etc1 ext) (setf gl-internal-format +gl-etc1-rgb8-oes+))) ; NOTE: Requires OpenGL ES 2.0 or OpenGL 4.3
        (#.+rl-pixelformat-compressed-etc2-rgb+ (when (rlext-tex-comp-etc2 ext) (setf gl-internal-format +gl-compressed-rgb8-etc2+))) ; NOTE: Requires OpenGL ES 3.0 or OpenGL 4.3
        (#.+rl-pixelformat-compressed-etc2-eac-rgba+ (when (rlext-tex-comp-etc2 ext) (setf gl-internal-format +gl-compressed-rgba8-etc2-eac+))) ; NOTE: Requires OpenGL ES 3.0 or OpenGL 4.3
        (#.+rl-pixelformat-compressed-pvrt-rgb+ (when (rlext-tex-comp-pvrt ext) (setf gl-internal-format +gl-compressed-rgb-pvrtc-4bppv1-img+))) ; NOTE: Requires PowerVR GPU
        (#.+rl-pixelformat-compressed-pvrt-rgba+ (when (rlext-tex-comp-pvrt ext) (setf gl-internal-format +gl-compressed-rgba-pvrtc-4bppv1-img+))) ; NOTE: Requires PowerVR GPU
        (#.+rl-pixelformat-compressed-astc-4x4-rgba+ (when (rlext-tex-comp-astc ext) (setf gl-internal-format +gl-compressed-rgba-astc-4x4-khr+))) ; NOTE: Requires OpenGL ES 3.1 or OpenGL 4.3
        (#.+rl-pixelformat-compressed-astc-8x8-rgba+ (when (rlext-tex-comp-astc ext) (setf gl-internal-format +gl-compressed-rgba-astc-8x8-khr+))) ; NOTE: Requires OpenGL ES 3.1 or OpenGL 4.3
        (t (trace-log +log-warning+ "TEXTURE: Current format not supported (~d)" format))))
    (values gl-internal-format gl-format gl-type)))

;; Unload texture from GPU memory
(defun rl-unload-texture (id)
  (%gl-delete-one %gl-delete-textures id))

;; Generate mipmap data for selected texture
;; NOTE: Only supports GPU mipmap generation
;; NOTE: Returns the number of mipmap levels generated, NIL if not generated (C mipmaps out-parameter unchanged)
(defun rl-gen-texture-mipmaps (id width height format)
  (declare (ignore format))
  (unless *is-gpu-ready*
    (trace-log +log-warning+ "GL: GPU is not ready to load data, trying to load before InitWindow()?")
    (return-from rl-gen-texture-mipmaps nil))
  (let ((mipmaps nil))
    (%gl-bind-texture +gl-texture-2d+ id)

    ;; Check if texture is power-of-two (POT)
    (let ((tex-is-pot (and (and (> width 0) (= (logand width (1- width)) 0))
                           (and (> height 0) (= (logand height (1- height)) 0)))))
      (if (or tex-is-pot (%rlext rlext-tex-npot))
          (progn
            ;;glHint(GL_GENERATE_MIPMAP_HINT, GL_DONT_CARE);   // Hint for mipmaps generation algorithm: GL_FASTEST, GL_NICEST, GL_DONT_CARE
            (%gl-generate-mipmap +gl-texture-2d+) ; Generate mipmaps automatically
            ;; NOTE: C computes log(0) = -inf for empty textures (undefined int conversion), use 1 level
            (setf mipmaps (if (> (max width height) 0)
                              (+ 1 (floor (/ (log (float (max width height) 1d0)) (log 2d0))))
                              1))
            (trace-log +log-info+ "TEXTURE: [ID ~d] Mipmaps generated automatically, total: ~d" id mipmaps))
          (trace-log +log-warning+ "TEXTURE: [ID ~d] Failed to generate mipmaps" id)))

    (%gl-bind-texture +gl-texture-2d+ 0)
    mipmaps))

;; Read texture pixel data
;; NOTE: Returns a byte vector in the texture pixel format (NIL if not supported)
(defun rl-read-texture-pixels (id width height format)
  (let ((pixels nil))
    (%gl-bind-texture +gl-texture-2d+ id)

    ;; NOTE: Each row written to or read from by OpenGL pixel operations like glGetTexImage are aligned to a 4 byte boundary by default, which may add some padding
    ;; Use glPixelStorei to modify padding with the GL_[UN]PACK_ALIGNMENT setting
    ;; GL_PACK_ALIGNMENT affects operations that read from OpenGL memory (glReadPixels, glGetTexImage, etc.)
    ;; GL_UNPACK_ALIGNMENT affects operations that write to OpenGL memory (glTexImage, etc.)
    (%gl-pixel-storei +gl-pack-alignment+ 1)

    (multiple-value-bind (gl-internal-format gl-format gl-type) (rl-get-gl-texture-formats format)
      (let ((size (%rl-get-pixel-data-size width height format)))
        (if (and (/= gl-internal-format 0) (< format +rl-pixelformat-compressed-dxt1-rgb+))
            (progn
              (setf pixels (make-array size :element-type '(unsigned-byte 8) :initial-element 0))
              (cffi:with-pointer-to-vector-data (p pixels)
                (%gl-get-tex-image +gl-texture-2d+ 0 gl-format gl-type p)))
            (trace-log +log-warning+ "TEXTURE: [ID ~d] Data retrieval not suported for pixel format (~d)" id format))))

    (%gl-bind-texture +gl-texture-2d+ 0)
    pixels))

;; Copy framebuffer pixel data to internal buffer
(defun rl-copy-framebuffer (x y width height format pixels)
  (declare (ignore x y width height format pixels)))

;; Resize internal framebuffer
(defun rl-resize-framebuffer (width height)
  (declare (ignore width height)))

;; Read screen pixel data (color buffer)
;; NOTE: Returns a RGBA8 byte vector
(defun rl-read-screen-pixels (width height)
  (let ((img-data (make-array (* width height 4) :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; NOTE: Buffer retrieved is GL_FRONT in single-buffered configurations
    ;; and GL_BACK in double-buffered configurations, make sure to call it at the end of frame
    ;;glReadBuffer(GL_BACK);

    ;; NOTE: glReadPixels() returns image flipped vertically -> (0,0) is the bottom left corner of the framebuffer
    ;; WARNING: Getting alpha channel! Be careful, it can be transparent if not cleared properly!
    (cffi:with-pointer-to-vector-data (p img-data)
      (%gl-read-pixels 0 0 width height +gl-rgba+ +gl-unsigned-byte+ p))

    ;; Flip image vertically
    ;; NOTE: Alpha value has already been applied to RGB in framebuffer, not needed anymore
    (loop for y from (1- height) downto (truncate height 2)
          do (loop for x from 0 below (* width 4) by 4
                   do (let ((s (+ (* (- (1- height) y) width 4) x))
                            (e (+ (* y width 4) x)))
                        (rotatef (aref img-data s) (aref img-data e))
                        (rotatef (aref img-data (+ s 1)) (aref img-data (+ e 1)))
                        (rotatef (aref img-data (+ s 2)) (aref img-data (+ e 2)))
                        (setf (aref img-data (+ s 3)) 255 ; Set alpha component value to 255 (no trasparent image retrieval)
                              (aref img-data (+ e 3)) 255)))) ; Ditto
    img-data))

;;;----------------------------------------------------------------------------------
;;; Framebuffer management (fbo)
;;;----------------------------------------------------------------------------------

;; Load a framebuffer to be used for rendering
;; NOTE: No textures attached
(defun rl-load-framebuffer ()
  (let ((fbo-id 0))
    (unless *is-gpu-ready*
      (trace-log +log-warning+ "GL: GPU is not ready to load data, trying to load before InitWindow()?")
      (return-from rl-load-framebuffer fbo-id))
    (setf fbo-id (%gl-gen-one %gl-gen-framebuffers)) ; Create the framebuffer object
    (%gl-bind-framebuffer +gl-framebuffer+ 0)          ; Unbind any framebuffer
    fbo-id))

;; Attach color buffer texture to a framebuffer object (unloads previous attachment)
;; NOTE: Attach type: 0-Color, 1-Depth renderbuffer, 2-Depth texture
(defun rl-framebuffer-attach (id tex-id attach-type tex-type mip-level)
  (%gl-bind-framebuffer +gl-framebuffer+ id)
  (cond ((<= +rl-attachment-color-channel0+ attach-type +rl-attachment-color-channel7+)
         (cond ((= tex-type +rl-attachment-texture2d+)
                (%gl-framebuffer-texture-2d +gl-framebuffer+ (+ +gl-color-attachment0+ attach-type) +gl-texture-2d+ tex-id mip-level))
               ((= tex-type +rl-attachment-renderbuffer+)
                (%gl-framebuffer-renderbuffer +gl-framebuffer+ (+ +gl-color-attachment0+ attach-type) +gl-renderbuffer+ tex-id))
               ((>= tex-type +rl-attachment-cubemap-positive-x+)
                (%gl-framebuffer-texture-2d +gl-framebuffer+ (+ +gl-color-attachment0+ attach-type)
                                            (+ +gl-texture-cube-map-positive-x+ tex-type) tex-id mip-level))))
        ((= attach-type +rl-attachment-depth+)
         (cond ((= tex-type +rl-attachment-texture2d+)
                (%gl-framebuffer-texture-2d +gl-framebuffer+ +gl-depth-attachment+ +gl-texture-2d+ tex-id mip-level))
               ((= tex-type +rl-attachment-renderbuffer+)
                (%gl-framebuffer-renderbuffer +gl-framebuffer+ +gl-depth-attachment+ +gl-renderbuffer+ tex-id))))
        ((= attach-type +rl-attachment-stencil+)
         (cond ((= tex-type +rl-attachment-texture2d+)
                (%gl-framebuffer-texture-2d +gl-framebuffer+ +gl-stencil-attachment+ +gl-texture-2d+ tex-id mip-level))
               ((= tex-type +rl-attachment-renderbuffer+)
                (%gl-framebuffer-renderbuffer +gl-framebuffer+ +gl-stencil-attachment+ +gl-renderbuffer+ tex-id)))))
  (%gl-bind-framebuffer +gl-framebuffer+ 0))

;; Verify render texture is complete
(defun rl-framebuffer-complete (id)
  (%gl-bind-framebuffer +gl-framebuffer+ id)
  (let ((status (%gl-check-framebuffer-status +gl-framebuffer+)))
    (when (/= status +gl-framebuffer-complete+)
      (case status
        (#.+gl-framebuffer-unsupported+ (trace-log +log-warning+ "FBO: [ID ~d] Framebuffer is unsupported" id))
        (#.+gl-framebuffer-incomplete-attachment+ (trace-log +log-warning+ "FBO: [ID ~d] Framebuffer has incomplete attachment" id))
        (#.+gl-framebuffer-incomplete-missing-attachment+ (trace-log +log-warning+ "FBO: [ID ~d] Framebuffer has a missing attachment" id))))
    (%gl-bind-framebuffer +gl-framebuffer+ 0)
    (= status +gl-framebuffer-complete+)))

;; Unload framebuffer from GPU memory
;; NOTE: All attached textures/cubemaps/renderbuffers are also deleted
(defun rl-unload-framebuffer (id)
  ;; Query depth attachment to automatically delete texture/renderbuffer
  (%gl-bind-framebuffer +gl-framebuffer+ id) ; Bind framebuffer to query depth texture type
  (cffi:with-foreign-objects ((depth-type :int) (depth-id :int))
    (setf (cffi:mem-ref depth-type :int) 0
          (cffi:mem-ref depth-id :int) 0)
    (%gl-get-framebuffer-attachment-parameteriv +gl-framebuffer+ +gl-depth-attachment+ +gl-framebuffer-attachment-object-type+ depth-type)
    ;; WARNING: WebGL: INVALID_ENUM: getFramebufferAttachmentParameter: invalid parameter name
    (%gl-get-framebuffer-attachment-parameteriv +gl-framebuffer+ +gl-depth-attachment+ +gl-framebuffer-attachment-object-name+ depth-id)
    (let ((depth-id-u (logand (cffi:mem-ref depth-id :int) #xffffffff))
          (type (cffi:mem-ref depth-type :int)))
      (cond ((= type +gl-renderbuffer+) (%gl-delete-one %gl-delete-renderbuffers depth-id-u))
            ((= type +gl-texture+) (%gl-delete-one %gl-delete-textures depth-id-u)))))

  ;; NOTE: If a texture object is deleted while its image is attached to the *currently bound* framebuffer,
  ;; the texture image is automatically detached from the currently bound framebuffer

  (%gl-bind-framebuffer +gl-framebuffer+ 0)
  (%gl-delete-one %gl-delete-framebuffers id)

  (trace-log +log-info+ "FBO: [ID ~d] Unloaded framebuffer from VRAM (GPU)" id))

;;;----------------------------------------------------------------------------------
;;; Vertex data management
;;;----------------------------------------------------------------------------------

;; Load a new attributes buffer
;; NOTE: SIZE in bytes, NIL means the size of the BUFFER vector
(defun rl-load-vertex-buffer (buffer size dynamic)
  (let ((id 0))
    (unless *is-gpu-ready*
      (trace-log +log-warning+ "GL: GPU is not ready to load data, trying to load before InitWindow()?")
      (return-from rl-load-vertex-buffer id))
    (setf id (%gl-gen-one %gl-gen-buffers))
    (%gl-bind-buffer +gl-array-buffer+ id)
    (%with-c-data (p buffer)
      (%gl-buffer-data +gl-array-buffer+ (or size (%c-data-size buffer)) p (if dynamic +gl-dynamic-draw+ +gl-static-draw+)))
    id))

;; Load a new attributes element buffer
(defun rl-load-vertex-buffer-element (buffer size dynamic)
  (let ((id 0))
    (unless *is-gpu-ready*
      (trace-log +log-warning+ "GL: GPU is not ready to load data, trying to load before InitWindow()?")
      (return-from rl-load-vertex-buffer-element id))
    (setf id (%gl-gen-one %gl-gen-buffers))
    (%gl-bind-buffer +gl-element-array-buffer+ id)
    (%with-c-data (p buffer :ushort)
      (%gl-buffer-data +gl-element-array-buffer+ (or size (%c-data-size buffer)) p (if dynamic +gl-dynamic-draw+ +gl-static-draw+)))
    id))

;; Enable vertex buffer (VBO)
(defun rl-enable-vertex-buffer (id)
  (%gl-bind-buffer +gl-array-buffer+ id))

;; Disable vertex buffer (VBO)
(defun rl-disable-vertex-buffer ()
  (%gl-bind-buffer +gl-array-buffer+ 0))

;; Enable vertex buffer element (VBO element)
(defun rl-enable-vertex-buffer-element (id)
  (%gl-bind-buffer +gl-element-array-buffer+ id))

;; Disable vertex buffer element (VBO element)
(defun rl-disable-vertex-buffer-element ()
  (%gl-bind-buffer +gl-element-array-buffer+ 0))

;; Update vertex buffer with new data
;; NOTE: dataSize and offset must be provided in bytes
(defun rl-update-vertex-buffer (id data data-size offset)
  (%gl-bind-buffer +gl-array-buffer+ id)
  (%with-c-data (p data)
    (%gl-buffer-sub-data +gl-array-buffer+ offset data-size p)))

;; Update vertex buffer elements with new data
;; NOTE: dataSize and offset must be provided in bytes
(defun rl-update-vertex-buffer-elements (id data data-size offset)
  (%gl-bind-buffer +gl-element-array-buffer+ id)
  (%with-c-data (p data :ushort)
    (%gl-buffer-sub-data +gl-element-array-buffer+ offset data-size p)))

;; Enable vertex array object (VAO)
(defun rl-enable-vertex-array (vao-id)
  (let ((result nil))
    (when (%rlext rlext-vao)
      (%gl-bind-vertex-array vao-id)
      (setf result t))
    result))

;; Disable vertex array object (VAO)
(defun rl-disable-vertex-array ()
  (when (%rlext rlext-vao) (%gl-bind-vertex-array 0)))

;; Enable vertex attribute index
(defun rl-enable-vertex-attribute (index)
  (%gl-enable-vertex-attrib-array index))

;; Disable vertex attribute index
(defun rl-disable-vertex-attribute (index)
  (%gl-disable-vertex-attrib-array index))

;; Draw vertex array
(defun rl-draw-vertex-array (offset count)
  (%gl-draw-arrays +gl-triangles+ offset count))

(defun %rl-elements-pointer (buffer offset)
  "(unsigned short *)buffer + offset, BUFFER is NIL (offset into the bound element buffer) or a pointer"
  (let ((base (if (and buffer (cffi:pointerp buffer)) (cffi:pointer-address buffer) 0)))
    (cffi:make-pointer (+ base (if (> offset 0) (* offset 2) 0)))))

;; Draw vertex array elements
(defun rl-draw-vertex-array-elements (offset count buffer)
  ;; NOTE: Added pointer math separately from function to avoid UBSAN complaining
  (%gl-draw-elements +gl-triangles+ count +gl-unsigned-short+ (%rl-elements-pointer buffer offset)))

;; Draw vertex array instanced
(defun rl-draw-vertex-array-instanced (offset count instances)
  (%gl-draw-arrays-instanced +gl-triangles+ offset count instances))

;; Draw vertex array elements instanced
(defun rl-draw-vertex-array-elements-instanced (offset count buffer instances)
  ;; NOTE: Added pointer math separately from function to avoid UBSAN complaining
  (%gl-draw-elements-instanced +gl-triangles+ count +gl-unsigned-short+ (%rl-elements-pointer buffer offset) instances))

;; Enable vertex state pointer
(defun rl-enable-state-pointer (vertex-attrib-type buffer)
  (declare (ignore vertex-attrib-type buffer)))

;; Disable vertex state pointer
(defun rl-disable-state-pointer (vertex-attrib-type)
  (declare (ignore vertex-attrib-type)))

;; Load vertex array object (VAO)
(defun rl-load-vertex-array ()
  (let ((vao-id 0))
    (unless *is-gpu-ready*
      (trace-log +log-warning+ "GL: GPU is not ready to load data, trying to load before InitWindow()?")
      (return-from rl-load-vertex-array vao-id))
    (when (%rlext rlext-vao) (setf vao-id (%gl-gen-one %gl-gen-vertex-arrays)))
    vao-id))

;; Set vertex attribute
(defun rl-set-vertex-attribute (index comp-size type normalized stride offset)
  ;; NOTE: Data type could be: GL_BYTE, GL_UNSIGNED_BYTE, GL_SHORT, GL_UNSIGNED_SHORT, GL_INT, GL_UNSIGNED_INT
  ;; Additional types (depends on OpenGL version or extensions):
  ;;  - GL_HALF_FLOAT, GL_FLOAT, GL_DOUBLE, GL_FIXED,
  ;;  - GL_INT_2_10_10_10_REV, GL_UNSIGNED_INT_2_10_10_10_REV, GL_UNSIGNED_INT_10F_11F_11F_REV
  (%gl-vertex-attrib-pointer index comp-size type (if (and normalized (not (eql normalized 0))) 1 0) stride
                             (cffi:make-pointer offset)))

;; Set vertex attribute divisor
(defun rl-set-vertex-attribute-divisor (index divisor)
  (%gl-vertex-attrib-divisor index divisor))

;; Unload vertex array object (VAO)
(defun rl-unload-vertex-array (vao-id)
  (when (%rlext rlext-vao)
    (%gl-bind-vertex-array 0)
    (%gl-delete-one %gl-delete-vertex-arrays vao-id)
    (trace-log +log-info+ "VAO: [ID ~d] Unloaded vertex array data from VRAM (GPU)" vao-id)))

;; Unload vertex buffer (VBO)
(defun rl-unload-vertex-buffer (vbo-id)
  (%gl-delete-one %gl-delete-buffers vbo-id))
;;TRACELOG(RL_LOG_INFO, "VBO: Unloaded vertex data from VRAM (GPU)");

;;;----------------------------------------------------------------------------------
;;; Shaders management
;;;----------------------------------------------------------------------------------

(defun %gl-info-log (id get-iv get-info-log)
  "Get shader/program info log string (NIL if empty)"
  (cffi:with-foreign-object (max-length :int)
    (setf (cffi:mem-ref max-length :int) 0)
    (funcall get-iv id +gl-info-log-length+ max-length)
    (let ((n (cffi:mem-ref max-length :int)))
      (when (> n 0)
        (cffi:with-foreign-objects ((length :int) (log :char n))
          (setf (cffi:mem-ref length :int) 0)
          (funcall get-info-log id n length log)
          (cffi:foreign-string-to-lisp log :count (cffi:mem-ref length :int)))))))

;; Load (compile) shader and return shader id
(defun rl-load-shader (code type)
  (let ((shader-id (%gl-create-shader type)))
    (cffi:with-foreign-string (s code)
      (cffi:with-foreign-object (strings :pointer)
        (setf (cffi:mem-ref strings :pointer) s)
        (%gl-shader-source shader-id 1 strings (cffi:null-pointer))))

    (%gl-compile-shader shader-id)
    (let ((success (cffi:with-foreign-object (v :int)
                     (setf (cffi:mem-ref v :int) 0)
                     (%gl-get-shaderiv shader-id +gl-compile-status+ v)
                     (cffi:mem-ref v :int))))
      (if (= success +gl-false+)
          (progn
            (case type
              (#.+gl-vertex-shader+ (trace-log +log-warning+ "SHADER: [ID ~d] Failed to compile vertex shader code" shader-id))
              (#.+gl-fragment-shader+ (trace-log +log-warning+ "SHADER: [ID ~d] Failed to compile fragment shader code" shader-id))
              ;;case GL_GEOMETRY_SHADER:
              (#.+gl-compute-shader+ (trace-log +log-warning+ "SHADER: Compute shaders not enabled. Define GRAPHICS_API_OPENGL_43")))

            (let ((log (%gl-info-log shader-id #'%gl-get-shaderiv #'%gl-get-shader-info-log)))
              (when log
                (trace-log +log-warning+ "SHADER: [ID ~d] Compile error: ~a" shader-id log)))

            ;; Unload object allocated by glCreateShader(),
            ;; despite failing in the compilation process
            (%gl-delete-shader shader-id)
            (setf shader-id 0))
          (case type
            (#.+gl-vertex-shader+ (trace-log +log-info+ "SHADER: [ID ~d] Vertex shader compiled successfully" shader-id))
            (#.+gl-fragment-shader+ (trace-log +log-info+ "SHADER: [ID ~d] Fragment shader compiled successfully" shader-id))
            ;;case GL_GEOMETRY_SHADER:
            (#.+gl-compute-shader+ (trace-log +log-warning+ "SHADER: Compute shaders not enabled. Define GRAPHICS_API_OPENGL_43")))))
    shader-id))

;; Load shader program from code strings
;; NOTE: If shader string is NULL, using default vertex/fragment shaders
(defun rl-load-shader-program (vs-code fs-code)
  (let ((id 0)                          ; Shader program id
        (vertex-shader-id 0)
        (fragment-shader-id 0))
    (unless *is-gpu-ready*
      (trace-log +log-warning+ "GL: GPU is not ready to load data, trying to load before InitWindow()?")
      (return-from rl-load-shader-program id))

    ;; Compile vertex shader (if provided)
    ;; NOTE: If not vertex shader is provided, use default one
    (setf vertex-shader-id (if vs-code (rl-load-shader vs-code +gl-vertex-shader+) (%rls rls-default-vshader-id)))

    ;; Compile fragment shader (if provided)
    ;; NOTE: If not vertex shader is provided, use default one
    (setf fragment-shader-id (if fs-code (rl-load-shader fs-code +gl-fragment-shader+) (%rls rls-default-fshader-id)))

    ;; In case vertex and fragment shader are the default ones, no need to recompile, assign the default shader program id
    (cond ((and (= vertex-shader-id (%rls rls-default-vshader-id)) (= fragment-shader-id (%rls rls-default-fshader-id)))
           (setf id (%rls rls-default-shader-id)))
          ((and (> vertex-shader-id 0) (> fragment-shader-id 0))
           ;; One of or both shader are new, a new shader program needs to be compiled
           (setf id (rl-load-shader-program-ex vertex-shader-id fragment-shader-id))

           ;; Detaching and deleting vertex/fragment shaders (if not default ones)
           ;; WARNING: Detach shader before deletion to make sure memory is freed
           (when (/= vertex-shader-id (%rls rls-default-vshader-id))
             ;; WARNING: Shader program linkage could fail and returned id is 0
             (when (> id 0) (%gl-detach-shader id vertex-shader-id))
             (%gl-delete-shader vertex-shader-id))
           (when (/= fragment-shader-id (%rls rls-default-fshader-id))
             ;; WARNING: Shader program linkage could fail and returned id is 0
             (when (> id 0) (%gl-detach-shader id fragment-shader-id))
             (%gl-delete-shader fragment-shader-id))

           ;; In case shader program loading failed, assign default shader
           (when (= id 0)
             ;; In case shader loading fails, reassigning default shader
             (trace-log +log-warning+ "SHADER: Failed to load custom shader code, using default shader")
             (setf id (%rls rls-default-shader-id)))))
    id))

;; Load shader program from already loaded shader ids
(defun rl-load-shader-program-ex (vs-id fs-id)
  (let ((program-id 0))
    (unless *is-gpu-ready*
      (trace-log +log-warning+ "GL: GPU is not ready to load data, trying to load before InitWindow()?")
      (return-from rl-load-shader-program-ex program-id))

    (setf program-id (%gl-create-program))

    (%gl-attach-shader program-id vs-id)
    (%gl-attach-shader program-id fs-id)

    ;; Default attribute shader locations must be bound before linking
    ;; NOTE: There is no problem with binding a generic attribute index to an attribute variable name
    ;; that is never used; if some attrib name is no found on the shader, it locations becomes -1
    (%gl-bind-attrib-location program-id +rl-default-shader-attrib-location-position+ +rl-default-shader-attrib-name-position+)
    (%gl-bind-attrib-location program-id +rl-default-shader-attrib-location-texcoord+ +rl-default-shader-attrib-name-texcoord+)
    (%gl-bind-attrib-location program-id +rl-default-shader-attrib-location-normal+ +rl-default-shader-attrib-name-normal+)
    (%gl-bind-attrib-location program-id +rl-default-shader-attrib-location-color+ +rl-default-shader-attrib-name-color+)
    (%gl-bind-attrib-location program-id +rl-default-shader-attrib-location-tangent+ +rl-default-shader-attrib-name-tangent+)
    (%gl-bind-attrib-location program-id +rl-default-shader-attrib-location-texcoord2+ +rl-default-shader-attrib-name-texcoord2+)
    (%gl-bind-attrib-location program-id +rl-default-shader-attrib-location-instancetransform+ +rl-default-shader-attrib-name-instancetransform+)
    (%gl-bind-attrib-location program-id +rl-default-shader-attrib-location-boneindices+ +rl-default-shader-attrib-name-boneindices+)
    (%gl-bind-attrib-location program-id +rl-default-shader-attrib-location-boneweights+ +rl-default-shader-attrib-name-boneweights+)

    (%gl-link-program program-id)

    ;; NOTE: All uniform variables are intitialised to 0 when a program links

    (let ((success (cffi:with-foreign-object (v :int)
                     (setf (cffi:mem-ref v :int) 0)
                     (%gl-get-programiv program-id +gl-link-status+ v)
                     (cffi:mem-ref v :int))))
      (if (= success +gl-false+)
          (progn
            (trace-log +log-warning+ "SHADER: [ID ~d] Failed to link shader program" program-id)
            (let ((log (%gl-info-log program-id #'%gl-get-programiv #'%gl-get-program-info-log)))
              (when log
                (trace-log +log-warning+ "SHADER: [ID ~d] Link error: ~a" program-id log)))
            (%gl-delete-program program-id)
            (setf program-id 0))
          ;; Get the size of compiled shader program (not available on OpenGL ES 2.0)
          ;; NOTE: If GL_LINK_STATUS is GL_FALSE, program binary length is zero
          (trace-log +log-info+ "SHADER: [ID ~d] Program shader loaded successfully" program-id)))
    program-id))

;; Load compute shader program
(defun rl-load-shader-program-compute (cs-id)
  (declare (ignore cs-id))
  (trace-log +log-warning+ "SHADER: Compute shaders not supported, enable GRAPHICS_API_OPENGL_43")
  0)

;; Delete shader
(defun rl-unload-shader (id)
  (%gl-delete-shader id)
  (trace-log +log-info+ "SHADER: [ID ~d] Unloaded shader data from VRAM (GPU)" id))

;; Unload shader program
(defun rl-unload-shader-program (id)
  (%gl-delete-program id)
  (trace-log +log-info+ "SHADER: [ID ~d] Unloaded shader program data from VRAM (GPU)" id))

;; Get shader location uniform
;; NOTE: First parameter refers to shader program id
(defun rl-get-location-uniform (id uniform-name)
  (%gl-get-uniform-location id uniform-name))

;; Get shader location attribute
;; NOTE: First parameter refers to shader program id
(defun rl-get-location-attrib (id attrib-name)
  (%gl-get-attrib-location id attrib-name))

;; Set shader value uniform
;; NOTE: VALUE is a number, vector (vec2/3/4), sequence, specialized vector or foreign pointer
(defun rl-set-uniform (loc-index value uniform-type count)
  (macrolet ((uniform (fn type) `(%with-c-data (p value ,type) (,fn loc-index count p))))
    (case uniform-type
      (#.+rl-shader-uniform-float+ (uniform %gl-uniform-1fv :float))
      (#.+rl-shader-uniform-vec2+ (uniform %gl-uniform-2fv :float))
      (#.+rl-shader-uniform-vec3+ (uniform %gl-uniform-3fv :float))
      (#.+rl-shader-uniform-vec4+ (uniform %gl-uniform-4fv :float))
      (#.+rl-shader-uniform-int+ (uniform %gl-uniform-1iv :int))
      (#.+rl-shader-uniform-ivec2+ (uniform %gl-uniform-2iv :int))
      (#.+rl-shader-uniform-ivec3+ (uniform %gl-uniform-3iv :int))
      (#.+rl-shader-uniform-ivec4+ (uniform %gl-uniform-4iv :int))
      (#.+rl-shader-uniform-uint+ (uniform %gl-uniform-1uiv :uint))
      (#.+rl-shader-uniform-uivec2+ (uniform %gl-uniform-2uiv :uint))
      (#.+rl-shader-uniform-uivec3+ (uniform %gl-uniform-3uiv :uint))
      (#.+rl-shader-uniform-uivec4+ (uniform %gl-uniform-4uiv :uint))
      (#.+rl-shader-uniform-sampler2d+ (uniform %gl-uniform-1iv :int))
      (t (trace-log +log-warning+ "SHADER: Failed to set uniform value, data type not recognized")))))

;; Set shader value attribute
(defun rl-set-vertex-attribute-default (loc-index value attrib-type count)
  (macrolet ((attrib (fn) `(%with-c-data (p value :float) (,fn loc-index p))))
    (case attrib-type
      (#.+rl-shader-attrib-float+ (when (= count 1) (attrib %gl-vertex-attrib-1fv)))
      (#.+rl-shader-attrib-vec2+ (when (= count 2) (attrib %gl-vertex-attrib-2fv)))
      (#.+rl-shader-attrib-vec3+ (when (= count 3) (attrib %gl-vertex-attrib-3fv)))
      (#.+rl-shader-attrib-vec4+ (when (= count 4) (attrib %gl-vertex-attrib-4fv)))
      (t (trace-log +log-warning+ "SHADER: Failed to set attrib default value, data type not recognized")))))

;; Set shader value uniform matrix
(defun rl-set-uniform-matrix (loc-index mat)
  (%rl-set-uniform-matrix-loc loc-index mat))

;; Set shader value uniform matrix
;; NOTE: MATRICES is a sequence of mat4 (sent transposed, C struct memory order)
(defun rl-set-uniform-matrices (loc-index matrices count)
  (let ((data (make-array (* 16 count) :element-type 'single-float :initial-element 0.0)))
    (loop for mat in (coerce matrices 'list)
          for i below count
          do (replace data (marr4 mat) :start1 (* i 16)))
    (cffi:with-pointer-to-vector-data (p data)
      (%gl-uniform-matrix-4fv loc-index count +gl-true+ p))))

;; Set shader value uniform sampler
(defun rl-set-uniform-sampler (loc-index texture-id)
  ;; Check if texture is already active
  (dotimes (i +rl-default-batch-max-texture-units+)
    (when (= (aref (%rls rls-active-texture-id) i) texture-id)
      (%gl-uniform-1i loc-index (+ 1 i))
      (return-from rl-set-uniform-sampler)))

  ;; Register a new active texture for the internal batch system
  ;; NOTE: Default texture is always activated as GL_TEXTURE0
  (dotimes (i +rl-default-batch-max-texture-units+)
    (when (= (aref (%rls rls-active-texture-id) i) 0)
      (%gl-uniform-1i loc-index (+ 1 i)) ; Activate new texture unit
      (setf (aref (%rls rls-active-texture-id) i) texture-id) ; Save texture id for binding on drawing
      (return))))

;; Set shader currently active (id and locations)
(defun rl-set-shader (id locs)
  (when (/= (%rls rls-current-shader-id) id)
    (rl-draw-render-batch (rlgl-current-batch *rlgl*))
    (setf (%rls rls-current-shader-id) id
          (%rls rls-current-shader-locs) locs)))

;; Dispatch compute shader (equivalent to *draw* for graphics pilepine)
(defun rl-compute-shader-dispatch (group-x group-y group-z)
  (declare (ignore group-x group-y group-z)))

;; Load shader storage buffer object (SSBO)
(defun rl-load-shader-buffer (size data usage-hint)
  (declare (ignore size data usage-hint))
  (trace-log +log-warning+ "SSBO: SSBO not enabled. Define GRAPHICS_API_OPENGL_43")
  0)

;; Unload shader storage buffer object (SSBO)
(defun rl-unload-shader-buffer (ssbo-id)
  (declare (ignore ssbo-id))
  (trace-log +log-warning+ "SSBO: SSBO not enabled. Define GRAPHICS_API_OPENGL_43"))

;; Update SSBO buffer data
(defun rl-update-shader-buffer (id data data-size offset)
  (declare (ignore id data data-size offset)))

;; Get SSBO buffer size
(defun rl-get-shader-buffer-size (id)
  (declare (ignore id))
  0)

;; Read SSBO buffer data (GPU->CPU)
(defun rl-read-shader-buffer (id dest count offset)
  (declare (ignore id dest count offset)))

;; Bind SSBO buffer
(defun rl-bind-shader-buffer (id index)
  (declare (ignore id index)))

;; Copy SSBO buffer data
(defun rl-copy-shader-buffer (dest-id src-id dest-offset src-offset count)
  (declare (ignore dest-id src-id dest-offset src-offset count)))

;; Bind image texture
(defun rl-bind-image-texture (id index format readonly)
  (declare (ignore id index format readonly))
  (trace-log +log-warning+ "TEXTURE: Image texture binding not enabled. Define GRAPHICS_API_OPENGL_43"))

;;;----------------------------------------------------------------------------------
;;; Matrix state management
;;;----------------------------------------------------------------------------------

;; Get internal modelview matrix
(defun rl-get-matrix-modelview ()
  (%rls rls-modelview))

;; Get internal projection matrix
(defun rl-get-matrix-projection ()
  (%rls rls-projection))

;; Get internal accumulated transform matrix
(defun rl-get-matrix-transform ()
  ;; TODO: Consider possible transform matrices in the RLGL.State.stack
  (%rls rls-transform))

;; Get internal projection matrix for stereo render (selected eye)
(defun rl-get-matrix-projection-stereo (eye)
  (aref (%rls rls-projection-stereo) eye))

;; Get internal view offset matrix for stereo render (selected eye)
(defun rl-get-matrix-view-offset-stereo (eye)
  (aref (%rls rls-view-offset-stereo) eye))

;; Set a custom modelview matrix (replaces internal modelview matrix)
(defun rl-set-matrix-modelview (view)
  (setf (%rls rls-modelview) view))

;; Set a custom projection matrix (replaces internal projection matrix)
(defun rl-set-matrix-projection (projection)
  (setf (%rls rls-projection) projection))

;; Set eyes projection matrices for stereo rendering
(defun rl-set-matrix-projection-stereo (right left)
  (setf (aref (%rls rls-projection-stereo) 0) right
        (aref (%rls rls-projection-stereo) 1) left))

;; Set eyes view offsets matrices for stereo rendering
(defun rl-set-matrix-view-offset-stereo (right left)
  (setf (aref (%rls rls-view-offset-stereo) 0) right
        (aref (%rls rls-view-offset-stereo) 1) left))

(defun %rl-draw-static-vertices (vertices stride attributes mode count)
  "Load VERTICES in a temporary VAO/VBO with ATTRIBUTES ((location size offset) ...), draw and delete them"
  (let ((vao (%gl-gen-one %gl-gen-vertex-arrays))
        (vbo 0)
        (data (make-array (length vertices) :element-type 'single-float
                                            :initial-contents (mapcar (lambda (v) (float v 1.0)) vertices))))
    ;; Gen VAO to contain VBO
    (%gl-bind-vertex-array vao)
    ;; Gen and fill vertex buffer (VBO)
    (setf vbo (%gl-gen-one %gl-gen-buffers))
    (%gl-bind-buffer +gl-array-buffer+ vbo)
    (cffi:with-pointer-to-vector-data (p data)
      (%gl-buffer-data +gl-array-buffer+ (* 4 (length data)) p +gl-static-draw+))
    ;; Bind vertex attributes
    (loop for (location size offset) in attributes
          do (%gl-enable-vertex-attrib-array location)
             (%gl-vertex-attrib-pointer location size +gl-float+ +gl-false+ (* stride 4) (cffi:make-pointer (* offset 4))))
    (%gl-bind-buffer +gl-array-buffer+ 0)
    ;; Draw
    (%gl-bind-vertex-array vao)
    (%gl-draw-arrays mode 0 count)
    (%gl-bind-vertex-array 0)
    ;; Delete buffers (VBO and VAO)
    (%gl-delete-one %gl-delete-buffers vbo)
    (%gl-delete-one %gl-delete-vertex-arrays vao)))

;; Load and draw a quad in NDC
(defun rl-load-draw-quad ()
  (%rl-draw-static-vertices
   ;; Positions         Texcoords
   '(-1.0  1.0 0.0   0.0 1.0
     -1.0 -1.0 0.0   0.0 0.0
      1.0  1.0 0.0   1.0 1.0
      1.0 -1.0 0.0   1.0 0.0)
   5 (list (list +rl-default-shader-attrib-location-position+ 3 0)   ; Positions
           (list +rl-default-shader-attrib-location-texcoord+ 2 3))  ; Texcoords
   +gl-triangle-strip+ 4))

;; Load and draw a cube in NDC
(defun rl-load-draw-cube ()
  (%rl-draw-static-vertices
   ;; Positions          Normals               Texcoords
   '(-1.0 -1.0 -1.0   0.0  0.0 -1.0   0.0 0.0
      1.0  1.0 -1.0   0.0  0.0 -1.0   1.0 1.0
      1.0 -1.0 -1.0   0.0  0.0 -1.0   1.0 0.0
      1.0  1.0 -1.0   0.0  0.0 -1.0   1.0 1.0
     -1.0 -1.0 -1.0   0.0  0.0 -1.0   0.0 0.0
     -1.0  1.0 -1.0   0.0  0.0 -1.0   0.0 1.0
     -1.0 -1.0  1.0   0.0  0.0  1.0   0.0 0.0
      1.0 -1.0  1.0   0.0  0.0  1.0   1.0 0.0
      1.0  1.0  1.0   0.0  0.0  1.0   1.0 1.0
      1.0  1.0  1.0   0.0  0.0  1.0   1.0 1.0
     -1.0  1.0  1.0   0.0  0.0  1.0   0.0 1.0
     -1.0 -1.0  1.0   0.0  0.0  1.0   0.0 0.0
     -1.0  1.0  1.0  -1.0  0.0  0.0   1.0 0.0
     -1.0  1.0 -1.0  -1.0  0.0  0.0   1.0 1.0
     -1.0 -1.0 -1.0  -1.0  0.0  0.0   0.0 1.0
     -1.0 -1.0 -1.0  -1.0  0.0  0.0   0.0 1.0
     -1.0 -1.0  1.0  -1.0  0.0  0.0   0.0 0.0
     -1.0  1.0  1.0  -1.0  0.0  0.0   1.0 0.0
      1.0  1.0  1.0   1.0  0.0  0.0   1.0 0.0
      1.0 -1.0 -1.0   1.0  0.0  0.0   0.0 1.0
      1.0  1.0 -1.0   1.0  0.0  0.0   1.0 1.0
      1.0 -1.0 -1.0   1.0  0.0  0.0   0.0 1.0
      1.0  1.0  1.0   1.0  0.0  0.0   1.0 0.0
      1.0 -1.0  1.0   1.0  0.0  0.0   0.0 0.0
     -1.0 -1.0 -1.0   0.0 -1.0  0.0   0.0 1.0
      1.0 -1.0 -1.0   0.0 -1.0  0.0   1.0 1.0
      1.0 -1.0  1.0   0.0 -1.0  0.0   1.0 0.0
      1.0 -1.0  1.0   0.0 -1.0  0.0   1.0 0.0
     -1.0 -1.0  1.0   0.0 -1.0  0.0   0.0 0.0
     -1.0 -1.0 -1.0   0.0 -1.0  0.0   0.0 1.0
     -1.0  1.0 -1.0   0.0  1.0  0.0   0.0 1.0
      1.0  1.0  1.0   0.0  1.0  0.0   1.0 0.0
      1.0  1.0 -1.0   0.0  1.0  0.0   1.0 1.0
      1.0  1.0  1.0   0.0  1.0  0.0   1.0 0.0
     -1.0  1.0 -1.0   0.0  1.0  0.0   0.0 1.0
     -1.0  1.0  1.0   0.0  1.0  0.0   0.0 0.0)
   8 (list (list +rl-default-shader-attrib-location-position+ 3 0)   ; Positions
           (list +rl-default-shader-attrib-location-normal+ 3 3)     ; Normals
           (list +rl-default-shader-attrib-location-texcoord+ 2 6))  ; Texcoords
   +gl-triangles+ 36))

;; Get name string for pixel format
(defun rl-get-pixel-format-name (format)
  (case format
    (#.+rl-pixelformat-uncompressed-grayscale+ "GRAYSCALE")       ; 8 bit per pixel (no alpha)
    (#.+rl-pixelformat-uncompressed-gray-alpha+ "GRAY_ALPHA")     ; 8*2 bpp (2 channels)
    (#.+rl-pixelformat-uncompressed-r5g6b5+ "R5G6B5")             ; 16 bpp
    (#.+rl-pixelformat-uncompressed-r8g8b8+ "R8G8B8")             ; 24 bpp
    (#.+rl-pixelformat-uncompressed-r5g5b5a1+ "R5G5B5A1")         ; 16 bpp (1 bit alpha)
    (#.+rl-pixelformat-uncompressed-r4g4b4a4+ "R4G4B4A4")         ; 16 bpp (4 bit alpha)
    (#.+rl-pixelformat-uncompressed-r8g8b8a8+ "R8G8B8A8")         ; 32 bpp
    (#.+rl-pixelformat-uncompressed-r32+ "R32")                   ; 32 bpp (1 channel - float)
    (#.+rl-pixelformat-uncompressed-r32g32b32+ "R32G32B32")       ; 32*3 bpp (3 channels - float)
    (#.+rl-pixelformat-uncompressed-r32g32b32a32+ "R32G32B32A32") ; 32*4 bpp (4 channels - float)
    (#.+rl-pixelformat-uncompressed-r16+ "R16")                   ; 16 bpp (1 channel - half float)
    (#.+rl-pixelformat-uncompressed-r16g16b16+ "R16G16B16")       ; 16*3 bpp (3 channels - half float)
    (#.+rl-pixelformat-uncompressed-r16g16b16a16+ "R16G16B16A16") ; 16*4 bpp (4 channels - half float)
    (#.+rl-pixelformat-compressed-dxt1-rgb+ "DXT1_RGB")           ; 4 bpp (no alpha)
    (#.+rl-pixelformat-compressed-dxt1-rgba+ "DXT1_RGBA")         ; 4 bpp (1 bit alpha)
    (#.+rl-pixelformat-compressed-dxt3-rgba+ "DXT3_RGBA")         ; 8 bpp
    (#.+rl-pixelformat-compressed-dxt5-rgba+ "DXT5_RGBA")         ; 8 bpp
    (#.+rl-pixelformat-compressed-etc1-rgb+ "ETC1_RGB")           ; 4 bpp
    (#.+rl-pixelformat-compressed-etc2-rgb+ "ETC2_RGB")           ; 4 bpp
    (#.+rl-pixelformat-compressed-etc2-eac-rgba+ "ETC2_RGBA")     ; 8 bpp
    (#.+rl-pixelformat-compressed-pvrt-rgb+ "PVRT_RGB")           ; 4 bpp
    (#.+rl-pixelformat-compressed-pvrt-rgba+ "PVRT_RGBA")         ; 4 bpp
    (#.+rl-pixelformat-compressed-astc-4x4-rgba+ "ASTC_4x4_RGBA") ; 8 bpp
    (#.+rl-pixelformat-compressed-astc-8x8-rgba+ "ASTC_8x8_RGBA") ; 2 bpp
    (t "UNKNOWN")))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition
;;;----------------------------------------------------------------------------------

;; Load default shader (just vertex positioning and texture coloring)
;; NOTE: This shader program is used for internal buffers
;; NOTE: Loaded: RLGL.State.defaultShaderId, RLGL.State.defaultShaderLocs
(defun %rl-load-shader-default ()
  (setf (%rls rls-default-shader-locs) (make-array +rl-max-shader-locations+ :initial-element -1))

  ;; NOTE: All locations must be reseted to -1 (no location)
  (let ((default-vshader-code
          (format nil "~{~a~%~}"
                  '("#version 330                       "
                    "in vec3 vertexPosition;            "
                    "in vec2 vertexTexCoord;            "
                    "in vec4 vertexColor;               "
                    "out vec2 fragTexCoord;             "
                    "out vec4 fragColor;                "
                    "uniform mat4 mvp;                  "
                    "void main()                        "
                    "{                                  "
                    "    fragTexCoord = vertexTexCoord; "
                    "    fragColor = vertexColor;       "
                    "    gl_Position = mvp*vec4(vertexPosition, 1.0); "
                    "}                                  ")))
        (default-fshader-code
          (format nil "~{~a~%~}"
                  '("#version 330       "
                    "in vec2 fragTexCoord;              "
                    "in vec4 fragColor;                 "
                    "out vec4 finalColor;               "
                    "uniform sampler2D texture0;        "
                    "uniform vec4 colDiffuse;           "
                    "void main()                        "
                    "{                                  "
                    "    vec4 texelColor = texture(texture0, fragTexCoord);   "
                    "    finalColor = texelColor*colDiffuse*fragColor;        "
                    "}                                  "))))

    ;; NOTE: Compiled vertex/fragment shaders are not deleted,
    ;; they are kept for re-use as default shaders in case some shader loading fails
    (setf (%rls rls-default-vshader-id) (rl-load-shader default-vshader-code +gl-vertex-shader+)   ; Compile default vertex shader
          (%rls rls-default-fshader-id) (rl-load-shader default-fshader-code +gl-fragment-shader+)) ; Compile default fragment shader

    (setf (%rls rls-default-shader-id) (rl-load-shader-program-ex (%rls rls-default-vshader-id) (%rls rls-default-fshader-id)))

    (let ((id (%rls rls-default-shader-id))
          (locs (%rls rls-default-shader-locs)))
      (if (> id 0)
          (progn
            (trace-log +log-info+ "SHADER: [ID ~d] Default shader loaded successfully" id)

            ;; Set default shader locations: attributes locations
            (setf (aref locs +rl-shader-loc-vertex-position+) (%gl-get-attrib-location id +rl-default-shader-attrib-name-position+)
                  (aref locs +rl-shader-loc-vertex-texcoord01+) (%gl-get-attrib-location id +rl-default-shader-attrib-name-texcoord+)
                  (aref locs +rl-shader-loc-vertex-color+) (%gl-get-attrib-location id +rl-default-shader-attrib-name-color+))

            ;; Set default shader locations: uniform locations
            (setf (aref locs +rl-shader-loc-matrix-mvp+) (%gl-get-uniform-location id +rl-default-shader-uniform-name-mvp+)
                  (aref locs +rl-shader-loc-color-diffuse+) (%gl-get-uniform-location id +rl-default-shader-uniform-name-color+)
                  (aref locs +rl-shader-loc-map-diffuse+) (%gl-get-uniform-location id +rl-default-shader-sampler2d-name-texture0+)))
          (trace-log +log-warning+ "SHADER: [ID ~d] Failed to load default shader" id)))))

;; Unload default shader
;; NOTE: Unloads: RLGL.State.defaultShaderId, RLGL.State.defaultShaderLocs
(defun %rl-unload-shader-default ()
  (%gl-use-program 0)

  (%gl-detach-shader (%rls rls-default-shader-id) (%rls rls-default-vshader-id))
  (%gl-detach-shader (%rls rls-default-shader-id) (%rls rls-default-fshader-id))
  (%gl-delete-shader (%rls rls-default-vshader-id))
  (%gl-delete-shader (%rls rls-default-fshader-id))

  (%gl-delete-program (%rls rls-default-shader-id))

  (setf (%rls rls-default-shader-locs) nil)

  (trace-log +log-info+ "SHADER: [ID ~d] Default shader unloaded successfully" (%rls rls-default-shader-id)))

;; Get pixel data size in bytes (image or texture)
;; NOTE: Size depends on pixel format
(defun %rl-get-pixel-data-size (width height format)
  (let ((data-size 0)                   ; Size in bytes
        (bpp 0)                         ; Bits per pixel
        (int-max 2147483647))
    (flet ((blocks (bytes-per-block)
             (let* ((block-width (truncate (+ width 3) 4))
                    (block-height (truncate (+ height 3) 4))
                    (data-size-bytes (* block-width block-height bytes-per-block)))
               (when (< data-size-bytes int-max) (setf data-size data-size-bytes)))))
      (case format
        (#.+rl-pixelformat-uncompressed-grayscale+ (setf bpp 8))
        ((#.+rl-pixelformat-uncompressed-gray-alpha+ #.+rl-pixelformat-uncompressed-r5g6b5+
          #.+rl-pixelformat-uncompressed-r5g5b5a1+ #.+rl-pixelformat-uncompressed-r4g4b4a4+) (setf bpp 16))
        (#.+rl-pixelformat-uncompressed-r8g8b8a8+ (setf bpp 32))
        (#.+rl-pixelformat-uncompressed-r8g8b8+ (setf bpp 24))
        (#.+rl-pixelformat-uncompressed-r32+ (setf bpp 32))
        (#.+rl-pixelformat-uncompressed-r32g32b32+ (setf bpp (* 32 3)))
        (#.+rl-pixelformat-uncompressed-r32g32b32a32+ (setf bpp (* 32 4)))
        (#.+rl-pixelformat-uncompressed-r16+ (setf bpp 16))
        (#.+rl-pixelformat-uncompressed-r16g16b16+ (setf bpp (* 16 3)))
        (#.+rl-pixelformat-uncompressed-r16g16b16a16+ (setf bpp (* 16 4)))
        ((#.+rl-pixelformat-compressed-dxt1-rgb+ #.+rl-pixelformat-compressed-dxt1-rgba+
          #.+rl-pixelformat-compressed-etc1-rgb+ #.+rl-pixelformat-compressed-etc2-rgb+
          #.+rl-pixelformat-compressed-pvrt-rgb+ #.+rl-pixelformat-compressed-pvrt-rgba+)
         (blocks 8))                    ; 8 bytes per each 4x4 block
        ((#.+rl-pixelformat-compressed-dxt3-rgba+ #.+rl-pixelformat-compressed-dxt5-rgba+
          #.+rl-pixelformat-compressed-etc2-eac-rgba+ #.+rl-pixelformat-compressed-astc-4x4-rgba+)
         (blocks 16))                   ; 16 bytes per each 4x4 block
        (#.+rl-pixelformat-compressed-astc-8x8-rgba+
         (blocks 4))))                  ; 4 bytes per each 4x4 block

    ;; Compute dataSize for uncompressed texture data (no blocks)
    (when (<= +rl-pixelformat-uncompressed-grayscale+ format +rl-pixelformat-uncompressed-r16g16b16a16+)
      (let ((data-size-bytes (ash (* width height bpp) -3))) ; Get size in bytes (dividing by 8)
        (when (< data-size-bytes int-max) (setf data-size data-size-bytes))))

    (when (= data-size 0) (trace-log +log-warning+ "Requested image size is larger than 2GB, it can not be allocated"))

    data-size))
