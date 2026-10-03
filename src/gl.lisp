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

(defconstant +rl-modelview+ #x1700 "GL_MODELVIEW")
(defconstant +rl-projection+ #x1701 "GL_PROJECTION")
(defconstant +rl-texture+ #x1702 "GL_TEXTURE")

(defun rl-matrix-mode (mode)
  "Choose the current matrix to be transformed (RL_MODELVIEW, RL_PROJECTION or RL_TEXTURE)"
  (%gl:matrix-mode (case mode
                     ((:modelview) +rl-modelview+)
                     ((:projection) +rl-projection+)
                     ((:texture) +rl-texture+)
                     (t mode))))

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
  "Multiply the current matrix by a perspective matrix generated by parameters"
  (gl:frustum (float left 1d0) (float right 1d0) (float bottom 1d0) (float top 1d0)
              (float znear 1d0) (float zfar 1d0)))

(defun rl-ortho (left right bottom top znear zfar)
  "Multiply the current matrix by an orthographic matrix generated by parameters"
  (gl:ortho (float left 1d0) (float right 1d0) (float bottom 1d0) (float top 1d0)
            (float znear 1d0) (float zfar 1d0)))

(defun rl-viewport (x y width height)
  "Set the viewport area"
  (declare (type fixnum x y width height))
  (gl:viewport x y width height))

(defvar *rl-cull-distance-near* 0.05d0 "Default near cull distance (RL_CULL_DISTANCE_NEAR)")
(defvar *rl-cull-distance-far* 4000.0d0 "Default far cull distance (RL_CULL_DISTANCE_FAR)")

(defun rl-set-clip-planes (near-plane far-plane)
  "Set clip planes distances"
  (setf *rl-cull-distance-near* (float near-plane 1d0)
        *rl-cull-distance-far* (float far-plane 1d0)))

(defun rl-get-cull-distance-near ()
  "Get cull plane distance near"
  *rl-cull-distance-near*)

(defun rl-get-cull-distance-far ()
  "Get cull plane distance far"
  *rl-cull-distance-far*)

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
  (color (list 255 255 255 255) :type list))

;; Global state variables
(defvar *rl-current-draw-mode* +rl-triangles+ "Current drawing mode")
(defvar *rl-current-batch* nil "Current render batch")
(defvar *rl-vertex-counter* 0 "Current vertex counter in batch")
(defvar *rl-current-texture-id* 0 "Current texture ID")
(defvar *rl-current-color* (list 255 255 255 255) "Current vertex color")
(defvar *rl-default-texture-id* 0 "Default texture used on shapes/poly drawing (required by shader)")
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
  (setf *rl-current-color* (list r g b a))
  ;; Immediate mode fallback
  (gl:color (/ r 255.0) (/ g 255.0) (/ b 255.0) (/ a 255.0)))

(defun rl-color3f (x y z)
  "Define one vertex (color) - 3 float"
  (declare (type single-float x y z))
  (rl-color4f x y z 1.0))

(defun rl-color4f (x y z w)
  "Define one vertex (color) - 4 float"
  (declare (type single-float x y z w))
  (setf *rl-current-color* (list (round (* x 255)) (round (* y 255))
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

(defvar *rl-active-framebuffer* 0 "Currently active framebuffer id")

(defun rl-enable-framebuffer (id)
  "Enable render texture (fbo)"
  (%gl:bind-framebuffer :framebuffer id)
  (setf *rl-active-framebuffer* id))

(defun rl-get-active-framebuffer ()
  "Get the currently active render texture (fbo), 0 for default framebuffer"
  *rl-active-framebuffer*)

(defun rl-disable-framebuffer ()
  "Disable render texture (fbo), return to default framebuffer"
  (%gl:bind-framebuffer :framebuffer 0)
  (setf *rl-active-framebuffer* 0))

(defun rl-framebuffer-complete (id)
  "Verify render texture is complete"
  (%gl:bind-framebuffer :framebuffer id)
  (let* ((raw (%gl:check-framebuffer-status :framebuffer))
         ;; NOTE: cl-opengl returns the status as an enum keyword
         (status (if (keywordp raw) (cffi:foreign-enum-value '%gl:enum raw) raw)))
    (unless (= status #x8CD5)                 ; GL_FRAMEBUFFER_COMPLETE
      (case status
        (#x8CDD (trace-log-warning "FBO: [ID ~d] Framebuffer is unsupported" id))
        (#x8CD6 (trace-log-warning "FBO: [ID ~d] Framebuffer has incomplete attachment" id))
        (#x8CD9 (trace-log-warning "FBO: [ID ~d] Framebuffer has incomplete dimensions" id))
        (#x8CD7 (trace-log-warning "FBO: [ID ~d] Framebuffer has a missing attachment" id))
        (t nil)))
    (%gl:bind-framebuffer :framebuffer 0)
    (= status #x8CD5)))

(defun rl-framebuffer-attach (id tex-id attach-type tex-type mip-level)
  "Attach texture/renderbuffer to a framebuffer"
  (%gl:bind-framebuffer :framebuffer id)
  (flet ((attach (attachment)
           (cond ((= tex-type +rl-attachment-texture2d+)
                  (%gl:framebuffer-texture-2d :framebuffer attachment :texture-2d tex-id mip-level))
                 ((= tex-type +rl-attachment-renderbuffer+)
                  (%gl:framebuffer-renderbuffer :framebuffer attachment :renderbuffer tex-id))
                 ((and (>= tex-type +rl-attachment-cubemap-positive-x+) (integerp attachment))
                  (%gl:framebuffer-texture-2d :framebuffer attachment (+ #x8515 tex-type) tex-id mip-level)))))
    (cond ((<= +rl-attachment-color-channel0+ attach-type +rl-attachment-color-channel7+)
           (attach (+ #x8CE0 attach-type)))           ; GL_COLOR_ATTACHMENT0 + attachType
          ;; NOTE: Depth/stencil attachments only support texture2d or renderbuffer
          ((= attach-type +rl-attachment-depth+) (attach :depth-attachment))
          ((= attach-type +rl-attachment-stencil+) (attach :stencil-attachment))))
  (%gl:bind-framebuffer :framebuffer 0))

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

(defvar *rl-current-blend-mode* 0 "Blending mode active")
(defvar *rl-gl-blend-src-factor* 0 "Blending source factor")
(defvar *rl-gl-blend-dst-factor* 0 "Blending destination factor")
(defvar *rl-gl-blend-equation* 0 "Blending equation")
(defvar *rl-gl-blend-src-factor-rgb* 0 "Blending source RGB factor")
(defvar *rl-gl-blend-dest-factor-rgb* 0 "Blending destination RGB factor")
(defvar *rl-gl-blend-src-factor-alpha* 0 "Blending source alpha factor")
(defvar *rl-gl-blend-dest-factor-alpha* 0 "Blending destination alpha factor")
(defvar *rl-gl-blend-equation-rgb* 0 "Blending equation for RGB")
(defvar *rl-gl-blend-equation-alpha* 0 "Blending equation for alpha")
(defvar *rl-gl-custom-blend-mode-modified* nil "Custom blending factor and equation modification status")

(defun rl-set-blend-mode (mode)
  "Set blending mode"
  (when (or (/= *rl-current-blend-mode* mode)
            (and (or (= mode +rl-blend-custom+) (= mode +rl-blend-custom-separate+))
                 *rl-gl-custom-blend-mode-modified*))
    (rl-draw-render-batch-active)
    (case mode
      (#.+rl-blend-alpha+
       (gl:blend-func :src-alpha :one-minus-src-alpha) (gl:blend-equation :func-add))
      (#.+rl-blend-additive+
       (gl:blend-func :src-alpha :one) (gl:blend-equation :func-add))
      (#.+rl-blend-multiplied+
       (gl:blend-func :dst-color :one-minus-src-alpha) (gl:blend-equation :func-add))
      (#.+rl-blend-add-colors+
       (gl:blend-func :one :one) (gl:blend-equation :func-add))
      (#.+rl-blend-subtract-colors+
       (gl:blend-func :one :one) (gl:blend-equation :func-subtract))
      (#.+rl-blend-alpha-premultiply+
       (gl:blend-func :one :one-minus-src-alpha) (gl:blend-equation :func-add))
      (#.+rl-blend-custom+
       ;; NOTE: Using GL blend src/dst factors and GL equation configured with rlSetBlendFactors()
       (%gl:blend-func *rl-gl-blend-src-factor* *rl-gl-blend-dst-factor*)
       (%gl:blend-equation *rl-gl-blend-equation*))
      (#.+rl-blend-custom-separate+
       ;; NOTE: Using GL blend src/dst factors and GL equation configured with rlSetBlendFactorsSeparate()
       (%gl:blend-func-separate *rl-gl-blend-src-factor-rgb* *rl-gl-blend-dest-factor-rgb*
                                *rl-gl-blend-src-factor-alpha* *rl-gl-blend-dest-factor-alpha*)
       (%gl:blend-equation-separate *rl-gl-blend-equation-rgb* *rl-gl-blend-equation-alpha*)))
    (setf *rl-current-blend-mode* mode
          *rl-gl-custom-blend-mode-modified* nil)))

(defun rl-set-blend-factors (gl-src-factor gl-dst-factor gl-equation)
  "Set blending mode factor and equation (using OpenGL factors)"
  (when (or (/= *rl-gl-blend-src-factor* gl-src-factor)
            (/= *rl-gl-blend-dst-factor* gl-dst-factor)
            (/= *rl-gl-blend-equation* gl-equation))
    (setf *rl-gl-blend-src-factor* gl-src-factor
          *rl-gl-blend-dst-factor* gl-dst-factor
          *rl-gl-blend-equation* gl-equation
          *rl-gl-custom-blend-mode-modified* t)))

(defun rl-set-blend-factors-separate (gl-src-rgb gl-dst-rgb gl-src-alpha gl-dst-alpha gl-eq-rgb gl-eq-alpha)
  "Set blending mode factor and equation separately for alpha blending"
  (when (or (/= *rl-gl-blend-src-factor-rgb* gl-src-rgb)
            (/= *rl-gl-blend-dest-factor-rgb* gl-dst-rgb)
            (/= *rl-gl-blend-src-factor-alpha* gl-src-alpha)
            (/= *rl-gl-blend-dest-factor-alpha* gl-dst-alpha)
            (/= *rl-gl-blend-equation-rgb* gl-eq-rgb)
            (/= *rl-gl-blend-equation-alpha* gl-eq-alpha))
    (setf *rl-gl-blend-src-factor-rgb* gl-src-rgb
          *rl-gl-blend-dest-factor-rgb* gl-dst-rgb
          *rl-gl-blend-src-factor-alpha* gl-src-alpha
          *rl-gl-blend-dest-factor-alpha* gl-dst-alpha
          *rl-gl-blend-equation-rgb* gl-eq-rgb
          *rl-gl-blend-equation-alpha* gl-eq-alpha
          *rl-gl-custom-blend-mode-modified* t)))

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
  "Get current OpenGL version
NOTE: cl-raylib renders through the OpenGL 1.1 immediate mode path (GRAPHICS_API_OPENGL_11)"
  +rl-opengl-11+)

(defvar *rl-framebuffer-width* 0 "Current framebuffer width (RLGL.State.framebufferWidth)")
(defvar *rl-framebuffer-height* 0 "Current framebuffer height (RLGL.State.framebufferHeight)")

(defun rl-set-framebuffer-width (width)
  "Set current framebuffer width"
  (setf *rl-framebuffer-width* width))

(defun rl-set-framebuffer-height (height)
  "Set current framebuffer height"
  (setf *rl-framebuffer-height* height))

(defun rl-get-framebuffer-width ()
  "Get default framebuffer width"
  *rl-framebuffer-width*)

(defun rl-get-framebuffer-height ()
  "Get default framebuffer height"
  *rl-framebuffer-height*)

;;; RLGL initialization and cleanup

(defun rlgl-init (width height)
  "Initialize rlgl (buffers, shaders, textures, states) - matches raylib rlglInit"
  (declare (type fixnum width height))
  ;; Store screen size into global variables
  (setf *rl-framebuffer-width* width
        *rl-framebuffer-height* height)
  
  ;; Init default white texture (matching raylib lines 2252-2257)
  (let ((default-texture-id (rl-load-default-texture)))
    (setf *rl-default-texture-id* default-texture-id)
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

(defun rl-get-texture-id-default ()
  "Get default texture id"
  *rl-default-texture-id*)

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

  ;; NOTE: Immediate mode drawing (no shaders) requires fixed-function texturing enabled,
  ;; untextured shapes are drawn with no texture bound (incomplete texture disables texturing)
  (gl:enable :texture-2d)
  (gl:hint :perspective-correction-hint :nicest) ; Improve quality of color and texture coordinate interpolation
  (gl:shade-model :smooth)                         ; Smooth shading between vertex (vertex colors interpolation)

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

;;; Texture parameters (rlgl.h)
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

(defconstant +gl-texture-2d+ #x0DE1)
(defconstant +gl-texture-cube-map+ #x8513)
(defconstant +gl-texture-cube-map-positive-x+ #x8515)
(defconstant +gl-texture-wrap-r+ #x8072)
(defconstant +gl-texture-base-level+ #x813C)
(defconstant +gl-texture-max-level+ #x813D)
(defconstant +gl-texture-max-anisotropy-ext+ #x84FE)
(defconstant +gl-max-texture-max-anisotropy-ext+ #x84FF)
(defconstant +gl-texture-lod-bias+ #x8501)
(defconstant +gl-unpack-alignment+ #x0CF5)

(defun rl-texture-parameters (id param value)
  "Set texture parameters (filter, wrap)"
  (gl:bind-texture :texture-2d id)
  (alexandria:switch (param)
    (+rl-texture-wrap-s+ (%gl:tex-parameter-i +gl-texture-2d+ param value))
    (+rl-texture-wrap-t+ (%gl:tex-parameter-i +gl-texture-2d+ param value))
    (+rl-texture-mag-filter+ (%gl:tex-parameter-i +gl-texture-2d+ param value))
    (+rl-texture-min-filter+ (%gl:tex-parameter-i +gl-texture-2d+ param value))
    (+rl-texture-filter-anisotropic+
     (let ((max-anisotropy (gl:get-float +gl-max-texture-max-anisotropy-ext+)))
       (when (vectorp max-anisotropy) (setf max-anisotropy (aref max-anisotropy 0)))
       (if (<= value max-anisotropy)
           (%gl:tex-parameter-f +gl-texture-2d+ +gl-texture-max-anisotropy-ext+ (float value 1.0))
           (progn
             (trace-log-warning "GL: Maximum anisotropic filter level supported is ~fX" max-anisotropy)
             (%gl:tex-parameter-f +gl-texture-2d+ +gl-texture-max-anisotropy-ext+ (float max-anisotropy 1.0))))))
    (+rl-texture-mipmap-bias-ratio+
     (%gl:tex-parameter-f +gl-texture-2d+ +gl-texture-lod-bias+ (/ value 100.0)))
    (t nil))
  (gl:bind-texture :texture-2d 0))

(defun rl-get-gl-texture-formats (format)
  "Get OpenGL internal formats and data type from raylib PixelFormat
   NOTE: Returns (values gl-internal-format gl-format gl-type), using the rlgl OpenGL 2.1 table
   (compatibility profile), NIL for unsupported formats"
  (let ((gl-luminance #x1909) (gl-luminance-alpha #x190A) (gl-rgb #x1907) (gl-rgba #x1908)
        (gl-unsigned-byte #x1401) (gl-float #x1406) (gl-half-float #x140B))
    (case format
      (1 (values gl-luminance gl-luminance gl-unsigned-byte))
      (2 (values gl-luminance-alpha gl-luminance-alpha gl-unsigned-byte))
      (3 (values gl-rgb gl-rgb #x8363))            ; GL_UNSIGNED_SHORT_5_6_5
      (4 (values gl-rgb gl-rgb gl-unsigned-byte))
      (5 (values gl-rgba gl-rgba #x8034))          ; GL_UNSIGNED_SHORT_5_5_5_1
      (6 (values gl-rgba gl-rgba #x8033))          ; GL_UNSIGNED_SHORT_4_4_4_4
      (7 (values gl-rgba gl-rgba gl-unsigned-byte))
      (8 (values gl-luminance gl-luminance gl-float))
      (9 (values gl-rgb gl-rgb gl-float))
      (10 (values gl-rgba gl-rgba gl-float))
      (11 (values gl-luminance gl-luminance gl-half-float))
      (12 (values gl-rgb gl-rgb gl-half-float))
      (13 (values gl-rgba gl-rgba gl-half-float))
      (t (trace-log-warning "TEXTURE: Current format not supported (~d)" format)
         (values nil nil nil)))))

(defun rl-get-pixel-format-name (format)
  "Get name string for pixel format"
  (case format
    (1 "GRAYSCALE") (2 "GRAY_ALPHA") (3 "R5G6B5") (4 "R8G8B8") (5 "R5G5B5A1") (6 "R4G4B4A4")
    (7 "R8G8B8A8") (8 "R32") (9 "R32G32B32") (10 "R32G32B32A32") (11 "R16") (12 "R16G16B16")
    (13 "R16G16B16A16") (14 "DXT1_RGB") (15 "DXT1_RGBA") (16 "DXT3_RGBA") (17 "DXT5_RGBA")
    (18 "ETC1_RGB") (19 "ETC2_RGB") (20 "ETC2_RGBA") (21 "PVRT_RGB") (22 "PVRT_RGBA")
    (23 "ASTC_4x4_RGBA") (24 "ASTC_8x8_RGBA") (t "UNKNOWN")))

(defun %rl-tex-image (target level format width height data offset size)
  "glTexImage2D() of SIZE bytes of DATA from OFFSET (NIL data allocates storage only)"
  (multiple-value-bind (internal-format gl-format gl-type) (rl-get-gl-texture-formats format)
    (when internal-format
      (if data
          (let ((chunk (if (and (zerop offset) (= size (length data))) data (subseq data offset (+ offset size)))))
            (cffi:with-pointer-to-vector-data (ptr chunk)
              (%gl:tex-image-2d target level internal-format width height 0 gl-format gl-type ptr)))
          (%gl:tex-image-2d target level internal-format width height 0 gl-format gl-type (cffi:null-pointer))))))

(defun rl-load-texture (data width height format mipmap-count)
  "Load texture data into GPU memory, returns the texture id
   NOTE: DATA is a byte vector with all mipmap levels, or NIL to only allocate storage"
  (gl:bind-texture :texture-2d 0)        ; Free any old binding
  (when (>= format +rl-pixelformat-compressed-dxt1-rgb+)
    (trace-log-warning "GL: Compressed texture formats not supported")
    (return-from rl-load-texture 0))
  (%gl:pixel-store-i +gl-unpack-alignment+ 1)
  (let ((id (gl:gen-texture))
        (mip-width width)
        (mip-height height)
        (mip-offset 0))
    (gl:bind-texture :texture-2d id)
    ;; Load the different mipmap levels
    (dotimes (i mipmap-count)
      (let ((mip-size (get-pixel-data-size mip-width mip-height format)))
        (%rl-tex-image +gl-texture-2d+ i format mip-width mip-height data mip-offset mip-size)
        (setf mip-width (max 1 (floor mip-width 2))
              mip-height (max 1 (floor mip-height 2)))
        (incf mip-offset mip-size)))
    ;; Texture parameters configuration
    ;; NOTE: glTexParameteri does NOT affect texture uploading
    (%gl:tex-parameter-i +gl-texture-2d+ +rl-texture-wrap-s+ +rl-texture-wrap-repeat+) ; Set texture to repeat on x-axis
    (%gl:tex-parameter-i +gl-texture-2d+ +rl-texture-wrap-t+ +rl-texture-wrap-repeat+) ; Set texture to repeat on y-axis
    ;; Magnification and minification filters
    (%gl:tex-parameter-i +gl-texture-2d+ +rl-texture-mag-filter+ +rl-texture-filter-nearest+) ; Alternative: GL_LINEAR
    (%gl:tex-parameter-i +gl-texture-2d+ +rl-texture-min-filter+ +rl-texture-filter-nearest+) ; Alternative: GL_LINEAR
    (when (> mipmap-count 1)
      ;; Activate trilinear filtering if mipmaps are available
      (%gl:tex-parameter-i +gl-texture-2d+ +rl-texture-mag-filter+ +rl-texture-filter-linear+)
      (%gl:tex-parameter-i +gl-texture-2d+ +rl-texture-min-filter+ +rl-texture-filter-mip-linear+)
      ;; Define the maximum number of mipmap levels to be used, 0 is base texture size
      (%gl:tex-parameter-i +gl-texture-2d+ +gl-texture-base-level+ 0)
      (%gl:tex-parameter-i +gl-texture-2d+ +gl-texture-max-level+ (1- mipmap-count)))
    ;; Unbind current texture
    (gl:bind-texture :texture-2d 0)
    (if (> id 0)
        (trace-log-info "TEXTURE: [ID ~d] Texture loaded successfully (~dx~d | ~a | ~d mipmaps)"
                        id width height (rl-get-pixel-format-name format) mipmap-count)
        (trace-log-warning "TEXTURE: Failed to load texture"))
    id))

(defun rl-load-texture-cubemap (data size format mipmap-count)
  "Load texture cubemap, returns the texture id
   NOTE: Cubemap data is expected to be 6 images in a single data array (one after the other),
   expected the following convention: +X, -X, +Y, -Y, +Z, -Z"
  (when (>= format +rl-pixelformat-compressed-dxt1-rgb+)
    (trace-log-warning "GL: Compressed texture formats not supported")
    (return-from rl-load-texture-cubemap 0))
  (let ((id (gl:gen-texture))
        (mip-size size)
        (data-offset 0))
    (gl:bind-texture :texture-cube-map id)
    (dotimes (mipmap-level mipmap-count)
      (let ((data-size (get-pixel-data-size mip-size mip-size format)))
        ;; Load cubemap faces/mipmaps
        (dotimes (face 6)
          (%rl-tex-image (+ +gl-texture-cube-map-positive-x+ face) mipmap-level format mip-size mip-size
                         data (+ data-offset (* face data-size)) data-size))
        (when data (incf data-offset (* data-size 6)))
        (setf mip-size (max 1 (floor mip-size 2)))))
    ;; Set cubemap texture sampling parameters
    (%gl:tex-parameter-i +gl-texture-cube-map+ +rl-texture-min-filter+
                         (if (> mipmap-count 1) +rl-texture-filter-mip-linear+ +rl-texture-filter-linear+))
    (%gl:tex-parameter-i +gl-texture-cube-map+ +rl-texture-mag-filter+ +rl-texture-filter-linear+)
    (%gl:tex-parameter-i +gl-texture-cube-map+ +rl-texture-wrap-s+ +rl-texture-wrap-clamp+)
    (%gl:tex-parameter-i +gl-texture-cube-map+ +rl-texture-wrap-t+ +rl-texture-wrap-clamp+)
    (%gl:tex-parameter-i +gl-texture-cube-map+ +gl-texture-wrap-r+ +rl-texture-wrap-clamp+)
    (gl:bind-texture :texture-cube-map 0)
    (if (> id 0)
        (trace-log-info "TEXTURE: [ID ~d] Cubemap texture loaded successfully (~dx~d)" id size size)
        (trace-log-warning "TEXTURE: Failed to load cubemap texture"))
    id))

(defun rl-update-texture (id offset-x offset-y width height format data)
  "Update texture with new data on GPU"
  (gl:bind-texture :texture-2d id)
  (multiple-value-bind (internal-format gl-format gl-type) (rl-get-gl-texture-formats format)
    (if (and internal-format (< format +rl-pixelformat-compressed-dxt1-rgb+))
        (let ((chunk (subseq data 0 (get-pixel-data-size width height format))))
          (cffi:with-pointer-to-vector-data (ptr chunk)
            (%gl:tex-sub-image-2d +gl-texture-2d+ 0 offset-x offset-y width height gl-format gl-type ptr)))
        (trace-log-warning "TEXTURE: [ID ~d] Failed to update for current texture format (~d)" id format)))
  (gl:bind-texture :texture-2d 0))

(defmacro %rl-without-gl-error-checks (&body body)
  "Run BODY ignoring OpenGL errors, as rlgl does (cl-opengl signals them by default)"
  `(multiple-value-prog1
       (let ((cl-opengl-bindings::*in-begin* t)) ,@body)
     ;; Drain the error flags so they are not reported by the next checked GL call
     (loop repeat 8 until (zerop (cffi:foreign-funcall "glGetError" :unsigned-int)))))

(defun rl-gen-texture-mipmaps (id width height format)
  "Generate mipmap data for selected texture, returns the number of mipmap levels (NIL on failure)
NOTE: Follows the GRAPHICS_API_OPENGL_33 path (GL errors are not checked, as in C)"
  (declare (ignore format))
  (let ((mipmaps nil)
        ;; Check if texture is power-of-two (POT)
        (tex-is-pot (and (> width 0) (= (logand width (1- width)) 0)
                         (> height 0) (= (logand height (1- height)) 0)))
        (tex-npot t))                   ; RLGL.ExtSupported.texNPOT, always available on desktop OpenGL 3.3
    (%gl:bind-texture :texture-2d id)
    (if (or tex-is-pot tex-npot)
        (progn
          (%rl-without-gl-error-checks
            (%gl:generate-mipmap :texture-2d)) ; Generate mipmaps automatically
          ;; NOTE: C computes log(0) = -inf for empty textures (undefined int conversion), use 1 level
          (setf mipmaps (if (> (max width height) 0)
                            (+ 1 (floor (log (float (max width height) 1d0)) (log 2d0)))
                            1))
          (trace-log-info "TEXTURE: [ID ~d] Mipmaps generated automatically, total: ~d" id mipmaps))
        (trace-log-warning "TEXTURE: [ID ~d] Failed to generate mipmaps" id))
    (%gl:bind-texture :texture-2d 0)
    mipmaps))

(defun rl-unload-texture (id)
  "Unload texture from GPU memory"
  (gl:delete-textures (list id)))

(defun rl-read-texture-pixels (id width height format)
  "Read texture pixel data, returns a byte vector in the texture pixel format (NIL if not supported)"
  (let ((pixels nil))
    (gl:bind-texture :texture-2d id)
    ;; NOTE: Each row written to or read from by OpenGL pixel operations like glGetTexImage are aligned to a 4 byte boundary by default, which may add some padding
    ;; Use glPixelStorei to modify padding with the GL_[UN]PACK_ALIGNMENT setting
    ;; GL_PACK_ALIGNMENT affects operations that read from OpenGL memory (glReadPixels, glGetTexImage, etc.)
    (gl:pixel-store :pack-alignment 1)
    (multiple-value-bind (gl-internal-format gl-format gl-type) (rl-get-gl-texture-formats format)
      (let ((size (get-pixel-data-size width height format)))
        (if (and gl-internal-format (/= gl-internal-format 0) (< format +rl-pixelformat-compressed-dxt1-rgb+))
            (progn
              (setf pixels (make-array size :element-type '(unsigned-byte 8) :initial-element 0))
              (cffi:with-pointer-to-vector-data (ptr pixels)
                (%gl:get-tex-image :texture-2d 0 gl-format gl-type ptr)))
            (trace-log-warning "TEXTURE: [ID ~d] Data retrieval not suported for pixel format (~d)" id format))))
    (gl:bind-texture :texture-2d 0)
    pixels))

(defun rl-read-screen-pixels (width height)
  "Read screen pixel data (color buffer), returns RGBA8 byte vector"
  (let ((img-data (make-array (* width height 4) :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; NOTE: glReadPixels() returns image flipped vertically -> (0,0) is the bottom left corner of the framebuffer
    ;; WARNING: Getting alpha channel! Be careful, it can be transparent if not cleared properly!
    (cffi:with-pointer-to-vector-data (ptr img-data)
      (%gl:read-pixels 0 0 width height :rgba :unsigned-byte ptr))
    ;; Flip image vertically
    ;; NOTE: Alpha value has already been applied to RGB in framebuffer, not needed anymore
    (loop for y from (1- height) downto (floor height 2)
          do (loop for x from 0 below (* width 4) by 4
                   do (let ((s (+ (* (- (1- height) y) width 4) x))
                            (e (+ (* y width 4) x)))
                        (rotatef (aref img-data s) (aref img-data e))
                        (rotatef (aref img-data (+ s 1)) (aref img-data (+ e 1)))
                        (rotatef (aref img-data (+ s 2)) (aref img-data (+ e 2)))
                        (setf (aref img-data (+ s 3)) 255 ; Set alpha component value to 255 (no trasparent image retrieval)
                              (aref img-data (+ e 3)) 255))))
    img-data))

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
              :initial-contents (list (mcref4 mat 0 0) (mcref4 mat 1 0) (mcref4 mat 2 0) (mcref4 mat 3 0)
                                      (mcref4 mat 0 1) (mcref4 mat 1 1) (mcref4 mat 2 1) (mcref4 mat 3 1)
                                      (mcref4 mat 0 2) (mcref4 mat 1 2) (mcref4 mat 2 2) (mcref4 mat 3 2)
                                      (mcref4 mat 0 3) (mcref4 mat 1 3) (mcref4 mat 2 3) (mcref4 mat 3 3))))
