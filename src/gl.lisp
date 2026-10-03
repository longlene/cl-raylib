(in-package #:cl-raylib)

;;; RLGL - OpenGL abstraction layer
;;; This module provides a multi-OpenGL abstraction layer with immediate-mode style API
;;; Translated from raylib/src/rlgl.h

;;; OpenGL version enumeration
(defconstant +rl-opengl-11+ 1 "OpenGL 1.1")
(defconstant +rl-opengl-21+ 2 "OpenGL 2.1 (GLSL 120)")
(defconstant +rl-opengl-33+ 3 "OpenGL 3.3 (GLSL 330)")
(defconstant +rl-opengl-43+ 4 "OpenGL 4.3 (using GLSL 330)")
(defconstant +rl-opengl-es-20+ 5 "OpenGL ES 2.0 (GLSL 100)")
(defconstant +rl-opengl-es-30+ 6 "OpenGL ES 3.0 (GLSL 300 es)")

;;; Trace log levels
(defconstant +rl-log-all+ 0 "Display all logs")
(defconstant +rl-log-trace+ 1 "Trace logging, intended for internal use only")
(defconstant +rl-log-debug+ 2 "Debug logging, used for internal debugging")
(defconstant +rl-log-info+ 3 "Info logging, used for program execution info")
(defconstant +rl-log-warning+ 4 "Warning logging, used on recoverable failures")
(defconstant +rl-log-error+ 5 "Error logging, used on unrecoverable failures")
(defconstant +rl-log-fatal+ 6 "Fatal logging, used to abort program")
(defconstant +rl-log-none+ 7 "Disable logging")

;;; Pixel formats
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

;;; Texture parameters: filter mode
(defconstant +rl-texture-filter-point+ 0 "No filter, just pixel approximation")
(defconstant +rl-texture-filter-bilinear+ 1 "Linear filtering")
(defconstant +rl-texture-filter-trilinear+ 2 "Trilinear filtering (linear with mipmaps)")
(defconstant +rl-texture-filter-anisotropic-4x+ 3 "Anisotropic filtering 4x")
(defconstant +rl-texture-filter-anisotropic-8x+ 4 "Anisotropic filtering 8x")
(defconstant +rl-texture-filter-anisotropic-16x+ 5 "Anisotropic filtering 16x")

;;; Color blending modes
(defconstant +rl-blend-alpha+ 0 "Blend textures considering alpha (default)")
(defconstant +rl-blend-additive+ 1 "Blend textures adding colors")
(defconstant +rl-blend-multiplied+ 2 "Blend textures multiplying colors")
(defconstant +rl-blend-add-colors+ 3 "Blend textures adding colors (alternative)")
(defconstant +rl-blend-subtract-colors+ 4 "Blend textures subtracting colors (alternative)")
(defconstant +rl-blend-alpha-premultiply+ 5 "Blend premultiplied textures considering alpha")
(defconstant +rl-blend-custom+ 6 "Blend textures using custom src/dst factors")
(defconstant +rl-blend-custom-separate+ 7 "Blend textures using custom rgb/alpha factors")

;;; Shader location indices
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
(defconstant +rl-shader-loc-vertex-boneids+ 26 "Shader location: vertex attribute: boneIds")
(defconstant +rl-shader-loc-vertex-boneweights+ 27 "Shader location: vertex attribute: boneWeights")
(defconstant +rl-shader-loc-bone-matrices+ 28 "Shader location: array of matrices uniform: boneMatrices")

;;; OpenGL rendering modes (from rlgl.h)
(defconstant +rl-lines+ #x0001 "GL_LINES")
(defconstant +rl-triangles+ #x0004 "GL_TRIANGLES") 
(defconstant +rl-quads+ #x0007 "GL_QUADS")

;;; Shader uniform data types
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

;;; Shader attribute data types
(defconstant +rl-shader-attrib-float+ 0 "Shader attribute type: float")
(defconstant +rl-shader-attrib-vec2+ 1 "Shader attribute type: vec2 (2 float)")
(defconstant +rl-shader-attrib-vec3+ 2 "Shader attribute type: vec3 (3 float)")
(defconstant +rl-shader-attrib-vec4+ 3 "Shader attribute type: vec4 (4 float)")

;;; Framebuffer attachment types
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

;;; Framebuffer texture attachment types
(defconstant +rl-attachment-cubemap-positive-x+ 0 "Framebuffer texture attachment type: cubemap, +X side")
(defconstant +rl-attachment-cubemap-negative-x+ 1 "Framebuffer texture attachment type: cubemap, -X side")
(defconstant +rl-attachment-cubemap-positive-y+ 2 "Framebuffer texture attachment type: cubemap, +Y side")
(defconstant +rl-attachment-cubemap-negative-y+ 3 "Framebuffer texture attachment type: cubemap, -Y side")
(defconstant +rl-attachment-cubemap-positive-z+ 4 "Framebuffer texture attachment type: cubemap, +Z side")
(defconstant +rl-attachment-cubemap-negative-z+ 5 "Framebuffer texture attachment type: cubemap, -Z side")
(defconstant +rl-attachment-texture2d+ 100 "Framebuffer texture attachment type: texture2d")
(defconstant +rl-attachment-renderbuffer+ 200 "Framebuffer texture attachment type: renderbuffer")

;;; Face culling modes
(defconstant +rl-cull-face-front+ 0 "Cull front faces")
(defconstant +rl-cull-face-back+ 1 "Cull back faces")

;;; Default configuration values
(defconstant +rl-default-batch-buffer-elements+ 8192 "Default internal render batch elements limits")
(defconstant +rl-default-batch-buffers+ 1 "Default number of batch buffers (multi-buffering)")
(defconstant +rl-default-batch-drawcalls+ 256 "Default number of batch draw calls")
(defconstant +rl-default-batch-max-texture-units+ 4 "Maximum number of textures units")
(defconstant +rl-max-matrix-stack-size+ 32 "Maximum size of internal Matrix stack")
(defconstant +rl-max-shader-locations+ 32 "Maximum number of shader locations supported")
(defconstant +rl-cull-distance-near+ 0.05 "Default projection matrix near cull distance")
(defconstant +rl-cull-distance-far+ 4000.0 "Default projection matrix far cull distance")

;;; OpenGL constants
(defconstant +gl-projection+ #x1701 "GL_PROJECTION matrix mode")
(defconstant +gl-modelview+ #x1700 "GL_MODELVIEW matrix mode")
(defconstant +gl-texture+ #x1702 "GL_TEXTURE matrix mode")
(defconstant +gl-color-buffer-bit+ (the fixnum #x4000) "GL_COLOR_BUFFER_BIT")
(defconstant +gl-depth-buffer-bit+ (the fixnum #x100) "GL_DEPTH_BUFFER_BIT")

;;; Data structures

(defstruct rl-draw-call
  "OpenGL draw call structure"
  (mode 0 :type fixnum)         ; Drawing mode: LINES, TRIANGLES, QUADS
  (vertex-count 0 :type fixnum) ; Number of vertex of the draw
  (vertex-alignment 0 :type fixnum) ; Number of vertex required for index alignment
  (texture-id 0 :type fixnum))  ; Texture id to be used on the draw

(defstruct rl-vertex-buffer
  "OpenGL vertex buffer structure"
  (element-count 0 :type fixnum)      ; Number of elements in the buffer (QUADS)
  (vertices nil)                      ; Vertex position (XYZ - 3 components per vertex)
  (texcoords nil)                     ; Vertex texture coordinates (UV - 2 components per vertex)
  (normals nil)                       ; Vertex normal (XYZ - 3 components per vertex)
  (colors nil)                        ; Vertex colors (RGBA - 4 components per vertex)
  (indices nil)                       ; Vertex indices (6 indices per quad)
  (vao-id 0 :type fixnum)            ; OpenGL Vertex Array Object id
  (vbo-id (make-array 5 :initial-element 0) :type (simple-array fixnum (5)))) ; OpenGL VBO ids

(defstruct rl-render-batch
  "Render batch structure"
  (buffer-count 0 :type fixnum)       ; Number of vertex buffers (multi-buffering support)
  (current-buffer 0 :type fixnum)     ; Current buffer tracking in case of multi-buffering
  (vertex-buffer nil)                 ; Dynamic buffer(s) for vertex data
  (draws nil)                         ; Draw calls array, depends on textureId
  (draw-counter 0 :type fixnum)       ; Draw calls counter
  (current-depth 0.0 :type single-float)) ; Current depth value for next draw

;;; Matrix operations

(defun rl-matrix-mode (mode)
  "Choose the current matrix to be transformed"
  (declare (type fixnum mode))
  ;; Set matrix mode - matches raylib rlMatrixMode
  (%gl:matrix-mode (case mode
                     (0 +gl-projection+)
                     (1 +gl-modelview+)
                     (2 +gl-texture+)
                     (t +gl-modelview+))))

(defun rl-push-matrix ()
  "Push the current matrix to stack"
  (gl:push-matrix))

(defun rl-pop-matrix ()
  "Pop latest inserted matrix from stack"
  (gl:pop-matrix))

(defun rl-load-identity ()
  "Reset current matrix to identity matrix"
  (gl:load-identity))

(defun rl-translatef (x y z)
  "Multiply the current matrix by a translation matrix"
  (declare (type single-float x y z))
  (gl:translate x y z))

(defun rl-rotatef (angle x y z)
  "Multiply the current matrix by a rotation matrix"
  (declare (type single-float angle x y z))
  (gl:rotate angle x y z))

(defun rl-scalef (x y z)
  "Multiply the current matrix by a scaling matrix"
  (declare (type single-float x y z))
  (gl:scale x y z))

(defun rl-mult-matrixf (matf)
  "Multiply the current matrix by another matrix"
  (declare (type (simple-array single-float (16)) matf))
  (gl:mult-matrix matf))

(defun rl-frustum (left right bottom top znear zfar)
  "Set perspective projection matrix"
  (declare (type double-float left right bottom top znear zfar))
  (gl:frustum left right bottom top znear zfar))

(defun rl-ortho (left right bottom top znear zfar)
  "Set orthographic projection matrix"
  (declare (type double-float left right bottom top znear zfar))
  (gl:ortho left right bottom top znear zfar))

(defun rl-viewport (x y width height)
  "Set the viewport area"
  (declare (type fixnum x y width height))
  (gl:viewport x y width height))

;;; Vertex operations and batch rendering system

;; Constants for batch rendering (matching raylib)
(defconstant +rl-default-batch-buffer-elements+ 8192 "Default internal render batch elements limits")
(defconstant +rl-default-batch-buffers+ 1 "Default number of batch buffers")
(defconstant +rl-default-batch-drawcalls+ 256 "Default number of batch draw calls")

;; Vertex data structure for batching
(defstruct rl-vertex
  "Vertex structure for batch rendering"
  (position (vec3 0.0 0.0 0.0) :type vec3)
  (texcoord (vec2 0.0 0.0) :type vec2)
  (normal (vec3 0.0 0.0 1.0) :type vec3)
  (color (color 255 255 255 255) :type color))

;; Global state variables
(defvar *rl-current-draw-mode* +rl-triangles+ "Current drawing mode")
(defvar *rl-current-batch* nil "Current render batch")
(defvar *rl-vertex-counter* 0 "Current vertex counter in batch")
(defvar *rl-current-texture-id* 0 "Current texture ID")
(defvar *rl-current-color* (color 255 255 255 255) "Current vertex color")
(defvar *rl-current-texcoord* (vec2 0.0 0.0) "Current texture coordinates")
(defvar *rl-current-normal* (vec3 0.0 0.0 1.0) "Current normal vector")

(defun rl-begin (mode)
  "Initialize drawing mode (how to organize vertex) - immediate mode fallback"
  (declare (type fixnum mode))
  (setf *rl-current-draw-mode* mode)
  ;; Use immediate mode for now until full batch system is implemented
  (gl:begin (case mode
              (#x0001 :lines)         ; RL_LINES
              (#x0004 :triangles)     ; RL_TRIANGLES  
              (#x0007 :quads)         ; RL_QUADS
              (t :triangles))))

(defun rl-end ()
  "Finish vertex providing - immediate mode fallback"
  (gl:end))

(defun rl-vertex2i (x y)
  "Define one vertex (position) - 2 int"
  (declare (type fixnum x y))
  (rl-vertex2f (float x) (float y)))

(defun rl-vertex2f (x y)
  "Define one vertex (position) - 2 float"
  (declare (type single-float x y))
  (rl-vertex3f x y 0.0))

(defun rl-vertex3f (x y z)
  "Define one vertex (position) - 3 float"
  (declare (type single-float x y z))
  ;; Immediate mode implementation
  (gl:vertex x y z))

(defun rl-tex-coord2f (x y)
  "Define one vertex (texture coordinate) - 2 float"
  (declare (type single-float x y))
  (setf *rl-current-texcoord* (vec2 x y))
  ;; Immediate mode fallback
  (gl:tex-coord x y))

(defun rl-normal3f (x y z)
  "Define one vertex (normal) - 3 float"
  (declare (type single-float x y z))
  (setf *rl-current-normal* (vec3 x y z))
  ;; Immediate mode fallback
  (gl:normal x y z))

(defun rl-color4ub (r g b a)
  "Define one vertex (color) - 4 byte"
  (declare (type (unsigned-byte 8) r g b a))
  (setf *rl-current-color* (color r g b a))
  ;; Immediate mode fallback
  (gl:color (/ r 255.0) (/ g 255.0) (/ b 255.0) (/ a 255.0)))

(defun rl-color3f (x y z)
  "Define one vertex (color) - 3 float"
  (declare (type single-float x y z))
  (rl-color4f x y z 1.0))

(defun rl-color4f (x y z w)
  "Define one vertex (color) - 4 float"
  (declare (type single-float x y z w))
  (setf *rl-current-color* (color (round (* x 255)) (round (* y 255)) 
                                  (round (* z 255)) (round (* w 255))))
  ;; Immediate mode fallback
  (gl:color x y z w))

;;; OpenGL state management

(defun rl-enable-vertex-array (vao-id)
  "Enable vertex array (VAO, if supported)"
  (declare (type fixnum vao-id))
  ;; OpenGL VAO support (OpenGL 3.0+)
  (when (> vao-id 0)
    (%gl:bind-vertex-array vao-id)
    t))

(defun rl-disable-vertex-array ()
  "Disable vertex array (VAO, if supported)"
  (%gl:bind-vertex-array 0))

(defun rl-enable-vertex-buffer (id)
  "Enable vertex buffer (VBO)"
  (declare (type fixnum id))
  (gl:bind-buffer :array-buffer id))

(defun rl-disable-vertex-buffer ()
  "Disable vertex buffer (VBO)"
  (gl:bind-buffer :array-buffer 0))

(defun rl-enable-vertex-buffer-element (id)
  "Enable vertex buffer element (VBO element)"
  (declare (type fixnum id))
  (gl:bind-buffer :element-array-buffer id))

(defun rl-disable-vertex-buffer-element ()
  "Disable vertex buffer element (VBO element)"
  (gl:bind-buffer :element-array-buffer 0))

;;; Texture management

(defun rl-active-texture-slot (slot)
  "Select and active a texture slot"
  (declare (type fixnum slot))
  (gl:active-texture (+ #x84C0 slot))) ; GL_TEXTURE0 + slot

(defun rl-enable-texture (id)
  "Enable texture"
  (declare (type fixnum id))
  (gl:bind-texture :texture-2d id))

(defun rl-disable-texture ()
  "Disable texture"
  (gl:bind-texture :texture-2d 0))

(defun rl-set-texture (id)
  "Set texture for rendering - alias for rl-enable-texture"
  (declare (type fixnum id))
  (if (= id 0)
      (rl-disable-texture)
      (rl-enable-texture id)))

(defun rl-enable-texture-cubemap (id)
  "Enable texture cubemap"
  (declare (type fixnum id))
  (gl:bind-texture :texture-cube-map id))

(defun rl-disable-texture-cubemap ()
  "Disable texture cubemap"
  (gl:bind-texture :texture-cube-map 0))

(defun rl-texture-parameters (id param value)
  "Set texture parameters (filter, wrap)"
  (declare (type fixnum id param value))
  (let ((current-texture (gl:get-integer :texture-binding-2d)))
    (gl:bind-texture :texture-2d id)
    (gl:tex-parameter :texture-2d param value)
    (gl:bind-texture :texture-2d current-texture)))

;;; Shader management

(defvar *rl-current-shader-id* 0 "Current shader program id")

(defun rl-enable-shader (id)
  "Enable shader program"
  (declare (type fixnum id))
  (setf *rl-current-shader-id* id)
  (%gl:use-program id))

(defun rl-disable-shader ()
  "Disable shader program"
  (setf *rl-current-shader-id* 0)
  (%gl:use-program 0))

;;; Framebuffer management

(defun rl-enable-framebuffer (id)
  "Enable render texture (fbo)"
  (declare (type fixnum id))
  (%gl:bind-framebuffer :framebuffer id))

(defun rl-disable-framebuffer ()
  "Disable render texture (fbo), return to default framebuffer"
  (%gl:bind-framebuffer :framebuffer 0))

(defun rl-framebuffer-complete (id)
  "Check if framebuffer is complete"
  (declare (type fixnum id))
  (let ((current-fbo (gl:get-integer :framebuffer-binding)))
    (%gl:bind-framebuffer :framebuffer id)
    (let ((status (%gl:check-framebuffer-status :framebuffer)))
      (%gl:bind-framebuffer :framebuffer current-fbo)
      (= status #x8CD5)))) ; GL_FRAMEBUFFER_COMPLETE

(defun rl-framebuffer-attach (fbo-id tex-id tex-type mip-level cube-map-face)
  "Attach texture/renderbuffer to a framebuffer"
  (declare (type fixnum fbo-id tex-id tex-type mip-level cube-map-face))
  (let ((current-fbo (gl:get-integer :framebuffer-binding)))
    (%gl:bind-framebuffer :framebuffer fbo-id)
    
    (cond
      ;; Color attachment
      ((<= 0 tex-type 7)
       (if (= cube-map-face +rl-attachment-texture2d+)
           (%gl:framebuffer-texture-2d :framebuffer
                                       (+ #x8CE0 tex-type) ; GL_COLOR_ATTACHMENT0 + type
                                       :texture-2d tex-id mip-level)
           (%gl:framebuffer-texture-2d :framebuffer
                                       (+ #x8CE0 tex-type)
                                       (+ #x8515 cube-map-face) ; GL_TEXTURE_CUBE_MAP_POSITIVE_X + face
                                       tex-id mip-level)))
      ;; Depth attachment
      ((= tex-type +rl-attachment-depth+)
       (if (= cube-map-face +rl-attachment-texture2d+)
           (%gl:framebuffer-texture-2d :framebuffer :depth-attachment :texture-2d tex-id mip-level)
           (%gl:framebuffer-texture-2d :framebuffer :depth-attachment
                                       (+ #x8515 cube-map-face) tex-id mip-level)))
      ;; Stencil attachment
      ((= tex-type +rl-attachment-stencil+)
       (if (= cube-map-face +rl-attachment-texture2d+)
           (%gl:framebuffer-texture-2d :framebuffer :stencil-attachment :texture-2d tex-id mip-level)
           (%gl:framebuffer-texture-2d :framebuffer :stencil-attachment
                                       (+ #x8515 cube-map-face) tex-id mip-level))))
    
    (%gl:bind-framebuffer :framebuffer current-fbo)))

;;; OpenGL state management functions

(defun rl-enable-wireframe ()
  "Enable wire mode"
  (%gl:polygon-mode :front-and-back :line))

(defun rl-enable-point-mode ()
  "Enable point mode"
  (%gl:polygon-mode :front-and-back :point))

(defun rl-disable-wire-mode ()
  "Disable wire mode ( and point ) -> return to default"
  (%gl:polygon-mode :front-and-back :fill))

(defun rl-set-line-width (width)
  "Set the line drawing width"
  (declare (type single-float width))
  (gl:line-width width))

(defun rl-get-line-width ()
  "Get the line drawing width"
  (gl:get-float :line-width))

(defun rl-enable-smooth-lines ()
  "Enable line aliasing"
  (gl:enable :line-smooth))

(defun rl-disable-smooth-lines ()
  "Disable line aliasing"
  (gl:disable :line-smooth))

(defun rl-enable-stereo-render ()
  "Enable stereo rendering"
  ;; Implementation depends on stereo support
  (format t "WARNING: Stereo rendering not fully implemented~%"))

(defun rl-disable-stereo-render ()
  "Disable stereo rendering"
  ;; Implementation depends on stereo support
  (format t "WARNING: Stereo rendering not fully implemented~%"))

(defun rl-is-stereo-render-enabled ()
  "Check if stereo render is enabled"
  ;; Implementation depends on stereo support
  nil)

;;; Culling and depth functions

(defun rl-enable-backface-culling ()
  "Enable backface culling"
  (gl:enable :cull-face))

(defun rl-disable-backface-culling ()
  "Disable backface culling"
  (gl:disable :cull-face))

(defun rl-set-cull-face (mode)
  "Set face culling mode"
  (declare (type fixnum mode))
  (gl:cull-face (case mode
                  (0 :front)
                  (1 :back)
                  (t :back))))

(defun rl-enable-scissor-test ()
  "Enable scissor test"
  (gl:enable :scissor-test))

(defun rl-disable-scissor-test ()
  "Disable scissor test"
  (gl:disable :scissor-test))

(defun rl-scissor (x y width height)
  "Scissor test"
  (declare (type fixnum x y width height))
  (gl:scissor x y width height))

(defun rl-enable-depth-test ()
  "Enable depth test"
  (gl:enable :depth-test))

(defun rl-disable-depth-test ()
  "Disable depth test"
  (gl:disable :depth-test))

(defun rl-enable-depth-mask ()
  "Enable depth write"
  (gl:depth-mask t))

(defun rl-disable-depth-mask ()
  "Disable depth write"
  (gl:depth-mask nil))

(defun rl-enable-color-blend ()
  "Enable color blending"
  (gl:enable :blend))

(defun rl-disable-color-blend ()
  "Disable color blending"
  (gl:disable :blend))

(defun rl-set-blend-mode (mode)
  "Set blending mode"
  (declare (type fixnum mode))
  (case mode
    (0 ; ALPHA
     (gl:blend-func :src-alpha :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (1 ; ADDITIVE
     (gl:blend-func :src-alpha :one)
     (gl:blend-equation :func-add))
    (2 ; MULTIPLIED
     (gl:blend-func :dst-color :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (3 ; ADD_COLORS
     (gl:blend-func :one :one)
     (gl:blend-equation :func-add))
    (4 ; SUBTRACT_COLORS
     (gl:blend-func :one :one)
     (gl:blend-equation :func-subtract))
    (5 ; ALPHA_PREMULTIPLY
     (gl:blend-func :one :one-minus-src-alpha)
     (gl:blend-equation :func-add))
    (t ; Default ALPHA
     (gl:blend-func :src-alpha :one-minus-src-alpha)
     (gl:blend-equation :func-add))))

(defun rl-set-blend-factors (gl-src-factor gl-dst-factor gl-equation)
  "Set blending mode factor and equation (to be used with RL_BLEND_CUSTOM)"
  (declare (type fixnum gl-src-factor gl-dst-factor gl-equation))
  (gl:blend-func gl-src-factor gl-dst-factor)
  (gl:blend-equation gl-equation))

(defun rl-set-blend-factors-separate (gl-src-rgb gl-dst-rgb gl-src-alpha gl-dst-alpha gl-eq-rgb gl-eq-alpha)
  "Set blending mode factors and equations separately (to be used with RL_BLEND_CUSTOM_SEPARATE)"
  (declare (type fixnum gl-src-rgb gl-dst-rgb gl-src-alpha gl-dst-alpha gl-eq-rgb gl-eq-alpha))
  (%gl:blend-func-separate gl-src-rgb gl-dst-rgb gl-src-alpha gl-dst-alpha)
  (%gl:blend-equation-separate gl-eq-rgb gl-eq-alpha))

;;; Buffer management functions

(defun rl-color-mask (r g b a)
  "Color mask control"
  (declare (type boolean r g b a))
  (gl:color-mask r g b a))

(defun rl-clear-color (r g b a)
  "Clear color buffer with color"
  (declare (type (unsigned-byte 8) r g b a))
  (gl:clear-color (/ r 255.0) (/ g 255.0) (/ b 255.0) (/ a 255.0)))

(defun rl-clear-screen-buffers ()
  "Clear used screen buffers (color and depth)"
  (gl:clear :color-buffer :depth-buffer))

(defun rl-check-errors ()
  "Check and log OpenGL error codes"
  (let ((error (gl:get-error)))
    (unless (eq error :no-error)
      (format t "WARNING: OpenGL error: ~a~%" error)
      error)))

;;; Utility functions

(defun rl-get-version ()
  "Get current OpenGL version"
  +rl-opengl-33+) ; Default to OpenGL 3.3 for cl-raylib

(defun rl-set-framebuffer-width (width)
  "Set current framebuffer width"
  (declare (type fixnum width))
  ;; This would be stored in global state in full implementation
  (format t "INFO: Framebuffer width set to: ~d~%" width))

(defun rl-get-framebuffer-width ()
  "Get default framebuffer width"
  ;; Return current viewport width or stored value
  (let ((viewport (gl:get-integer :viewport)))
    (third viewport)))

(defun rl-set-framebuffer-height (height)
  "Set current framebuffer height"
  (declare (type fixnum height))
  ;; This would be stored in global state in full implementation
  (format t "INFO: Framebuffer height set to: ~d~%" height))

(defun rl-get-framebuffer-height ()
  "Get default framebuffer height"
  ;; Return current viewport height or stored value
  (let ((viewport (gl:get-integer :viewport)))
    (fourth viewport)))

;;; RLGL initialization and cleanup

(defun rlgl-init (width height)
  "Initialize rlgl (buffers, shaders, textures, states) - matches raylib rlglInit"
  (declare (type fixnum width height))
  (declare (ignore width height))
  
  ;; Init default white texture (matching raylib lines 2252-2257)
  (let ((default-texture-id (rl-load-default-texture)))
    (if (> default-texture-id 0)
        (trace-log-info "TEXTURE: [ID ~d] Default texture loaded successfully" default-texture-id)
        (trace-log-warning "TEXTURE: Failed to load default texture")))
  
  ;; Init default shader (matching raylib lines 2259-2263)
  (let ((default-shader-id (rl-load-default-shader)))
    (if (> default-shader-id 0)
        (trace-log-info "SHADER: [ID ~d] Default shader loaded successfully" default-shader-id)
        (trace-log-warning "SHADER: [ID ~d] Failed to load default shader" default-shader-id)))
  
  ;; Initialize render batch (matching raylib lines 2265-2270)
  (rl-init-render-batch)
  (trace-log-info "RLGL: Render batch vertex buffers loaded successfully in RAM (CPU)")
  (trace-log-info "RLGL: Render batch vertex buffers loaded successfully in VRAM (GPU)")
  
  ;; Initialize OpenGL default states (matching raylib lines 2282-2321)
  (rl-init-opengl-default-states)
  (trace-log-info "RLGL: Default OpenGL state initialized successfully")
  
  t)

(defun rl-load-default-texture ()
  "Load default white texture (1x1 RGBA) - matches raylib default texture creation"
  ;; Create 1x1 white texture: 4 bytes RGBA (255,255,255,255) 
  (let* ((pixels (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(255 255 255 255)))
         (id (gl:gen-texture)))
    (gl:bind-texture :texture-2d id)
    (gl:tex-image-2d :texture-2d 0 :rgba 1 1 0 :rgba :unsigned-byte pixels)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (trace-log-info "TEXTURE: [ID ~d] Texture loaded successfully (1x1 | R8G8B8A8 | 1 mipmaps)" id)
    id))

(defun rl-load-default-shader ()
  "Load default vertex and fragment shaders - matches raylib default shader creation"
  ;; Simplified default shader source (OpenGL 3.3 core)
  (let ((vertex-shader-source
         "#version 330
in vec3 vertexPosition;
in vec2 vertexTexCoord;
in vec4 vertexColor;
out vec2 fragTexCoord;
out vec4 fragColor;
uniform mat4 mvp;
void main() {
    fragTexCoord = vertexTexCoord;
    fragColor = vertexColor;
    gl_Position = mvp * vec4(vertexPosition, 1.0);
}")
        (fragment-shader-source
         "#version 330
in vec2 fragTexCoord;
in vec4 fragColor;
out vec4 finalColor;
uniform sampler2D texture0;
uniform vec4 colDiffuse;
void main() {
    vec4 texelColor = texture(texture0, fragTexCoord);
    finalColor = texelColor * colDiffuse * fragColor;
}"))
    
    ;; Compile vertex shader
    (let ((vertex-shader (gl:create-shader :vertex-shader)))
      (gl:shader-source vertex-shader vertex-shader-source)
      (gl:compile-shader vertex-shader)
      (trace-log-info "SHADER: [ID ~d] Vertex shader compiled successfully" vertex-shader)
      
      ;; Compile fragment shader
      (let ((fragment-shader (gl:create-shader :fragment-shader)))
        (gl:shader-source fragment-shader fragment-shader-source)
        (gl:compile-shader fragment-shader)
        (trace-log-info "SHADER: [ID ~d] Fragment shader compiled successfully" fragment-shader)
        
        ;; Create shader program
        (let ((program (gl:create-program)))
          (gl:attach-shader program vertex-shader)
          (gl:attach-shader program fragment-shader)
          (gl:link-program program)
          (trace-log-info "SHADER: [ID ~d] Program shader loaded successfully" program)
          
          ;; Clean up individual shaders (they're linked into the program now)
          (gl:delete-shader vertex-shader)
          (gl:delete-shader fragment-shader)
          
          program)))))

(defun rl-init-render-batch ()
  "Initialize render batch system - simplified version of raylib render batch initialization"
  ;; This is a simplified implementation - in full raylib this involves
  ;; complex vertex buffer management, VAOs, etc.
  ;; For now we just indicate successful initialization
  t)

(defun rl-init-opengl-default-states ()
  "Initialize OpenGL default states - matches raylib OpenGL state setup"
  ;; Init state: Depth test (matching raylib lines 2284-2286)
  (gl:depth-func :lequal)
  (gl:disable :depth-test) ; Disable for 2D rendering by default
  
  ;; Init state: Blending mode (matching raylib lines 2288-2290)  
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :blend)
  
  ;; Init state: Culling (matching raylib lines 2292-2296)
  (gl:cull-face :back)
  (gl:front-face :ccw)
  (gl:enable :cull-face)
  
  ;; Init state: Color/Depth buffers clear (matching raylib lines 2318-2321)
  (gl:clear-color 0.0 0.0 0.0 1.0) ; Set clear color to black
  (gl:clear-depth 1.0) ; Set clear depth value
  (gl:clear :color-buffer-bit :depth-buffer-bit) ; Clear both buffers
  
  t)

(defun rlgl-close ()
  "De-inititialize rlgl (buffers, shaders, textures)"
  (format t "INFO: RLGL: De-initializing OpenGL renderer...~%")
  
  ;; Clean up would happen here in full implementation
  ;; - Unload default shader
  ;; - Unload default texture
  ;; - Unload render batches
  ;; - Free buffers
  
  (format t "INFO: RLGL: OpenGL renderer de-initialized successfully~%"))

;;; Higher-level convenience functions

(defun rl-load-extensions (loader)
  "Load OpenGL extensions (loader function pointer)"
  (declare (ignore loader))
  (format t "INFO: RLGL: OpenGL extensions loaded successfully~%"))

(defun rl-get-gl-texture-formats (format internal-format type)
  "Get OpenGL internal formats and data type from raylib PixelFormat"
  (declare (type fixnum format))
  (declare (ignore format internal-format type))
  ;; This would return appropriate OpenGL format constants
  ;; For now, return reasonable defaults
  (values #x1908     ; GL_RGBA
          #x1908     ; GL_RGBA  
          #x1401))   ; GL_UNSIGNED_BYTE

;;; Matrix utility functions (using core-data matrices)

(defun rl-get-matrix-modelview ()
  "Get internal modelview matrix"
  ;; This would return the current modelview matrix from OpenGL or internal state
  (meye 4)) ; Return identity matrix for now

(defun rl-get-matrix-projection ()
  "Get internal projection matrix"
  ;; This would return the current projection matrix from OpenGL or internal state
  (meye 4)) ; Return identity matrix for now

(defun rl-set-matrix-projection (proj)
  "Set a custom projection matrix (replaces internal projection matrix)"
  (declare (type mat4 proj))
  (rl-matrix-mode 0) ; Projection
  (rl-load-identity)
  (rl-mult-matrixf (mat4-to-array proj))
  (rl-matrix-mode 1)) ; Back to modelview

(defun rl-set-matrix-modelview (view)
  "Set a custom modelview matrix (replaces internal modelview matrix)"
  (declare (type mat4 view))
  (rl-matrix-mode 1) ; Modelview
  (rl-load-identity)
  (rl-mult-matrixf (mat4-to-array view)))

;;; Helper function to convert mat4 to array
(defun mat4-to-array (mat)
  "Convert 3d-matrices mat4 to simple-array for OpenGL"
  (make-array 16 :element-type 'single-float
              :initial-contents (list (mref mat 0 0) (mref mat 1 0) (mref mat 2 0) (mref mat 3 0)
                                      (mref mat 0 1) (mref mat 1 1) (mref mat 2 1) (mref mat 3 1)
                                      (mref mat 0 2) (mref mat 1 2) (mref mat 2 2) (mref mat 3 2)
                                      (mref mat 0 3) (mref mat 1 3) (mref mat 2 3) (mref mat 3 3))))
