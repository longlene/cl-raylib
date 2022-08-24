(in-package #:cl-raylib)
;;/**********************************************************************************************
;;*
;;*   raylib v4.0 - A simple and easy-to-use library to enjoy videogames programming (www.raylib.com)
;;*
;;*   FEATURES:
;;*       - NO external dependencies, all required libraries included with raylib
;;*       - Multiplatform: Windows, Linux, FreeBSD, OpenBSD, NetBSD, DragonFly,
;;*                        MacOS, Haiku, Android, Raspberry Pi, DRM native, HTML5.
;;*       - Written in plain C code (C99) in PascalCase/camelCase notation
;;*       - Hardware accelerated with OpenGL (1.1, 2.1, 3.3, 4.3 or ES2 - choose at compile)
;;*       - Unique OpenGL abstraction layer (usable as standalone module): [rlgl]
;;*       - Multiple Fonts formats supported (TTF, XNA fonts, AngelCode fonts)
;;*       - Outstanding texture formats support, including compressed formats (DXT, ETC, ASTC)
;;*       - Full 3d support for 3d Shapes, Models, Billboards, Heightmaps and more!
;;*       - Flexible Materials system, supporting classic maps and PBR maps
;;*       - Animated 3D models supported (skeletal bones animation) (IQM)
;;*       - Shaders support, including Model shaders and Postprocessing shaders
;;*       - Powerful math module for Vector, Matrix and Quaternion operations: [raymath]
;;*       - Audio loading and playing with streaming support (WAV, OGG, MP3, FLAC, XM, MOD)
;;*       - VR stereo rendering with configurable HMD device parameters
;;*       - Bindings to multiple programming languages available!
;;*
;;*   NOTES:
;;*       - One default Font is loaded on InitWindow()->LoadFontDefault() [core, text]
;;*       - One default Texture2D is loaded on rlglInit(), 1x1 white pixel R8G8B8A8 [rlgl] (OpenGL 3.3 or ES2)
;;*       - One default Shader is loaded on rlglInit()->rlLoadShaderDefault() [rlgl] (OpenGL 3.3 or ES2)
;;*       - One default RenderBatch is loaded on rlglInit()->rlLoadRenderBatch() [rlgl] (OpenGL 3.3 or ES2)
;;*
;;*   DEPENDENCIES (included):
;;*       [rcore] rglfw (Camilla Löwy - github.com/glfw/glfw) for window/context management and input (PLATFORM_DESKTOP)
;;*       [rlgl] glad (David Herberth - github.com/Dav1dde/glad) for OpenGL 3.3 extensions loading (PLATFORM_DESKTOP)
;;*       [raudio] miniaudio (David Reid - github.com/mackron/miniaudio) for audio device/context management
;;*
;;*   OPTIONAL DEPENDENCIES (included):
;;*       [rcore] msf_gif (Miles Fogle) for GIF recording
;;*       [rcore] sinfl (Micha Mettke) for DEFLATE decompression algorythm
;;*       [rcore] sdefl (Micha Mettke) for DEFLATE compression algorythm
;;*       [rtextures] stb_image (Sean Barret) for images loading (BMP, TGA, PNG, JPEG, HDR...)
;;*       [rtextures] stb_image_write (Sean Barret) for image writing (BMP, TGA, PNG, JPG)
;;*       [rtextures] stb_image_resize (Sean Barret) for image resizing algorithms
;;*       [rtext] stb_truetype (Sean Barret) for ttf fonts loading
;;*       [rtext] stb_rect_pack (Sean Barret) for rectangles packing
;;*       [rmodels] par_shapes (Philip Rideout) for parametric 3d shapes generation
;;*       [rmodels] tinyobj_loader_c (Syoyo Fujita) for models loading (OBJ, MTL)
;;*       [rmodels] cgltf (Johannes Kuhlmann) for models loading (glTF)
;;*       [raudio] dr_wav (David Reid) for WAV audio file loading
;;*       [raudio] dr_flac (David Reid) for FLAC audio file loading
;;*       [raudio] dr_mp3 (David Reid) for MP3 audio file loading
;;*       [raudio] stb_vorbis (Sean Barret) for OGG audio loading
;;*       [raudio] jar_xm (Joshua Reisenauer) for XM audio module loading
;;*       [raudio] jar_mod (Joshua Reisenauer) for MOD audio module loading
;;*
;;*
;;*   LICENSE: zlib/libpng
;;*
;;*   raylib is licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;*   BSD-like license that allows static linking with closed source software:
;;*
;;*   Copyright (c) 2013-2021 Ramon Santamaria (@raysan5)
;;*
;;*   This software is provided "as-is", without any express or implied warranty. In no event
;;*   will the authors be held liable for any damages arising from the use of this software.
;;*
;;*   Permission is granted to anyone to use this software for any purpose, including commercial
;;*   applications, and to alter it and redistribute it freely, subject to the following restrictions:
;;*
;;*     1. The origin of this software must not be misrepresented; you must not claim that you
;;*     wrote the original software. If you use this software in a product, an acknowledgment
;;*     in the product documentation would be appreciated but is not required.
;;*
;;*     2. Altered source versions must be plainly marked as such, and must not be misrepresented
;;*     as being the original software.
;;*
;;*     3. This notice may not be removed or altered from any source distribution.
;;*
;;**********************************************************************************************/
;;
;;#ifndef RAYLIB_H
;;#define RAYLIB_H
;;
;;#include <stdarg.h>     // Required for: va_list - Only used by TraceLogCallback
;;
;;#define RAYLIB_VERSION  "4.0"
;;
;;// Function specifiers in case library is build/used as a shared library (Windows)
;;// NOTE: Microsoft specifiers to tell compiler that symbols are imported/exported from a .dll
;;#if defined(_WIN32)
;;    #if defined(BUILD_LIBTYPE_SHARED)
;;        #define RLAPI __declspec(dllexport)     // We are building the library as a Win32 shared library (.dll)
;;    #elif defined(USE_LIBTYPE_SHARED)
;;        #define RLAPI __declspec(dllimport)     // We are using the library as a Win32 shared library (.dll)
;;    #endif
;;#endif
;;
;;#ifndef RLAPI
;;    #define RLAPI       // Functions defined as 'extern' by default (implicit specifiers)
;;#endif
;;
;;//----------------------------------------------------------------------------------
;;// Some basic Defines
;;//----------------------------------------------------------------------------------
;;#ifndef PI
;;    #define PI 3.14159265358979323846f
;;#endif
;;#ifndef DEG2RAD
;;    #define DEG2RAD (PI/180.0f)
;;#endif
;;#ifndef RAD2DEG
;;    #define RAD2DEG (180.0f/PI)
;;#endif
;;
;;// Allow custom memory allocators
;;#ifndef RL_MALLOC
;;    #define RL_MALLOC(sz)       malloc(sz)
;;#endif
;;#ifndef RL_CALLOC
;;    #define RL_CALLOC(n,sz)     calloc(n,sz)
;;#endif
;;#ifndef RL_REALLOC
;;    #define RL_REALLOC(ptr,sz)  realloc(ptr,sz)
;;#endif
;;#ifndef RL_FREE
;;    #define RL_FREE(ptr)        free(ptr)
;;#endif
;;
;;// NOTE: MSVC C++ compiler does not support compound literals (C99 feature)
;;// Plain structures in C++ (without constructors) can be initialized with { }
;;#if defined(__cplusplus)
;;    #define CLITERAL(type)      type
;;#else
;;    #define CLITERAL(type)      (type)
;;#endif
;;
;;// NOTE: We set some defines with some data types declared by raylib
;;// Other modules (raymath, rlgl) also require some of those types, so,
;;// to be able to use those other modules as standalone (not depending on raylib)
;;// this defines are very useful for internal check and avoid type (re)definitions
;;#define RL_COLOR_TYPE
;;#define RL_RECTANGLE_TYPE
;;#define RL_VECTOR2_TYPE
;;#define RL_VECTOR3_TYPE
;;#define RL_VECTOR4_TYPE
;;#define RL_QUATERNION_TYPE
;;#define RL_MATRIX_TYPE
;;
;;// Some Basic Colors
;;// NOTE: Custom raylib color palette for amazing visuals on WHITE background
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-rgba (r g b &optional (a #xFF))
    (logior
     r
     (ash g 8)
     (ash b 16)
     (ash a 24)))

  ;;#define LIGHTGRAY  CLITERAL(Color){ 200, 200, 200, 255 }   // Light Gray
  (define-constant +lightgray+ (make-rgba 200 200 200 255) :test #'equal)
    ;;#define GRAY       CLITERAL(Color){ 130, 130, 130, 255 }   // Gray


  (define-constant +gray+ (make-rgba 130 130 130 255) :test #'equal)
    ;;#define DARKGRAY   CLITERAL(Color){ 80, 80, 80, 255 }      // Dark Gray


  (define-constant +darkgray+ (make-rgba 80 80 80 255) :test #'equal)
    ;;#define YELLOW     CLITERAL(Color){ 253, 249, 0, 255 }     // Yellow

     
  (define-constant +yellow+ (make-rgba 253 249 0 255) :test #'equal)
    ;;#define GOLD       CLITERAL(Color){ 255, 203, 0, 255 }     // Gold


  (define-constant +gold+ (make-rgba 255 203 0 255) :test #'equal)
    ;;#define ORANGE     CLITERAL(Color){ 255, 161, 0, 255 }     // Orange


  (define-constant +orange+     (make-rgba 255 161 0 255 ) :test #'equal)
    ;;#define PINK       CLITERAL(Color){ 255, 109, 194, 255 }   // Pink


  (define-constant +pink+       (make-rgba 255 109 194 255) :test #'equal)
    ;;#define RED        CLITERAL(Color){ 230, 41, 55, 255 }     // Red


  (define-constant +red+        (make-rgba  230 41 55 255 ) :test #'equal)     
    ;;#define MAROON     CLITERAL(Color){ 190, 33, 55, 255 }     // Maroon


  (define-constant +maroon+      (make-rgba 190 33 55 255) :test #'equal) 
    ;;#define GREEN      CLITERAL(Color){ 0, 228, 48, 255 }      // Green


  (define-constant +green+       (make-rgba 0 228 48 255) :test #'equal) 
    ;;#define LIME       CLITERAL(Color){ 0, 158, 47, 255 }      // Lime


  (define-constant +lime+        (make-rgba 0 158 47 255) :test #'equal) 
    ;;#define DARKGREEN  CLITERAL(Color){ 0, 117, 44, 255 }      // Dark Green


  (define-constant +darkgreen+   (make-rgba 0 117 44 255) :test #'equal) 
    ;;#define SKYBLUE    CLITERAL(Color){ 102, 191, 255, 255 }   // Sky Blue


  (define-constant +skyblue+     (make-rgba 102 191 255 255) :test #'equal) 
    ;;#define BLUE       CLITERAL(Color){ 0, 121, 241, 255 }     // Blue


  (define-constant +blue+        (make-rgba 0 121 241 255) :test #'equal) 
    ;;#define DARKBLUE   CLITERAL(Color){ 0, 82, 172, 255 }      // Dark Blue


  (define-constant +darkblue+    (make-rgba 0 82 172 255) :test #'equal) 
    ;;#define PURPLE     CLITERAL(Color){ 200, 122, 255, 255 }   // Purple


  (define-constant +purple+      (make-rgba 200 122 255 255) :test #'equal) 
    ;;#define VIOLET     CLITERAL(Color){ 135, 60, 190, 255 }    // Violet


  (define-constant +violet+      (make-rgba 135 60 190 255) :test #'equal) 
    ;;#define DARKPURPLE CLITERAL(Color){ 112, 31, 126, 255 }    // Dark Purple


  (define-constant +darkpurple+  (make-rgba 112 31 126 255) :test #'equal) 
    ;;#define BEIGE      CLITERAL(Color){ 211, 176, 131, 255 }   // Beige


  (define-constant +beige+       (make-rgba 211 176 131 255) :test #'equal) 
    ;;#define BROWN      CLITERAL(Color){ 127, 106, 79, 255 }    // Brown


  (define-constant +brown+       (make-rgba 127 106 79 255) :test #'equal) 
    ;;#define DARKBROWN  CLITERAL(Color){ 76, 63, 47, 255 }      // Dark Brown


  (define-constant +darkbrown+   (make-rgba 76 63 47 255) :test #'equal) 
    ;;
    ;;#define WHITE      CLITERAL(Color){ 255, 255, 255, 255 }   // White


  (define-constant +white+       (make-rgba 255 255 255 255) :test #'equal) 
    ;;#define BLACK      CLITERAL(Color){ 0, 0, 0, 255 }         // Black


  (define-constant +black+       (make-rgba 0 0 0 255) :test #'equal) 
    ;;#define BLANK      CLITERAL(Color){ 0, 0, 0, 0 }           // Blank (Transparent)


  (define-constant +blank+       (make-rgba 0 0 0 0) :test #'equal) 
    ;;#define MAGENTA    CLITERAL(Color){ 255, 0, 255, 255 }     // Magenta

    
  (define-constant +magenta+     (make-rgba 255 0 255 255) :test #'equal) 
    ;;#define RAYWHITE   CLITERAL(Color){ 245, 245, 245, 255 }   // My own White (raylib logo)


  (define-constant +raywhite+    (make-rgba 245 245 245 255) :test #'equal))

;;//----------------------------------------------------------------------------------
;;// Structures Definition
;;//----------------------------------------------------------------------------------
;;// Boolean type
;;#if defined(__STDC__) && __STDC_VERSION__ >= 199901L
;;    #include <stdbool.h>
;;#elif !defined(__cplusplus) && !defined(bool)
;;    typedef enum bool { false, true } bool;
;;    #define RL_BOOL_TYPE
;;#endif
;;
;;// Vector2, 2 components
;;typedef struct Vector2 {
;;    float x;                // Vector x component
;;    float y;                // Vector y component
;;} Vector2;
(defcstruct (%vector2 :class vector2-type)
 "Vector2, 2 components"
 (x :float)
 (y :float))

(defmethod cffi:translate-into-foreign-memory ((object 3d-vectors:vec2) (type vector2-type) pointer)
  (cffi:with-foreign-slots ((x y) pointer (:struct %vector2))
    (setf x (3d-vectors:vx object))
    (setf y (3d-vectors:vy object))))

(defmethod cffi:translate-from-foreign (pointer (type vector2-type))
  (cffi:with-foreign-slots ((x y) pointer (:struct %vector2))
    (3d-vectors:vec x y)))

;;// Vector3, 3 components
;;typedef struct Vector3 {
;;    float x;                // Vector x component
;;    float y;                // Vector y component
;;    float z;                // Vector z component
;;} Vector3;
(defcstruct (%vector3 :class vector3-type)
 "Vector3 components"
 (x :float)
 (y :float)
 (z :float))

(defmethod cffi:translate-into-foreign-memory ((object 3d-vectors:vec3) (type vector3-type) pointer)
  (cffi:with-foreign-slots ((x y z) pointer (:struct %vector3))
    (setf x (3d-vectors:vx object))
    (setf y (3d-vectors:vy object))
    (setf z (3d-vectors:vz object))))

(defmethod cffi:translate-from-foreign (pointer (type vector3-type))
  (cffi:with-foreign-slots ((x y z) pointer (:struct %vector3))
    (3d-vectors:vec x y z)))

;;// Vector4, 4 components
;;typedef struct Vector4 {
;;    float x;                // Vector x component
;;    float y;                // Vector y component
;;    float z;                // Vector z component
;;    float w;                // Vector w component
;;} Vector4;
(defcstruct (%vector4 :class vector4-type)
 "Vector4, 4 components"
 (x :float)
 (y :float)
 (z :float)
 (w :float))

(defmethod cffi:translate-into-foreign-memory ((object 3d-vectors:vec4) (type vector4-type) pointer)
  (cffi:with-foreign-slots ((x y z w) pointer (:struct %vector4))
    (setf x (3d-vectors:vx object))
    (setf y (3d-vectors:vy object))
    (setf z (3d-vectors:vz object))
    (setf w (3d-vectors:vw object))))

(defmethod cffi:translate-from-foreign (pointer (type vector4-type))
  (cffi:with-foreign-slots ((x y z w) pointer (:struct %vector4))
    (3d-vectors:vec x y z w)))

;;
;;// Quaternion, 4 components (Vector4 alias)
;;typedef Vector4 Quaternion;
;;
;;// Matrix, 4x4 components, column major, OpenGL style, right handed
;;typedef struct Matrix {
;;    float m0, m4, m8, m12;  // Matrix first row (4 components)
;;    float m1, m5, m9, m13;  // Matrix second row (4 components)
;;    float m2, m6, m10, m14; // Matrix third row (4 components)
;;    float m3, m7, m11, m15; // Matrix fourth row (4 components)
;;} Matrix;
(defcstruct (%matrix :class matrix-type)
  "Matrix type (OpenGL style 4x4"
  (m0 :float) (m4 :float) (m8 :float) (m12 :float)
  (m1 :float) (m5 :float) (m9 :float) (m13 :float)
  (m2 :float) (m6 :float) (m10 :float) (m14 :float)
  (m3 :float) (m7 :float) (m11 :float) (m15 :float))

(defmethod translate-into-foreign-memory ((object 3d-matrices:mat4) (type matrix-type) pointer)
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

(defmethod translate-from-foreign (pointer (type matrix-type))
  (with-foreign-slots ((m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15) pointer (:struct %matrix))
    (3d-matrices:mat m0 m4 m8 m12
                     m1 m5 m9 m13
                     m2 m6 m10 m14
                     m3 m7 m11 m15)))

;;// Color, 4 components, R8G8B8A8 (32bit)
;;typedef struct Color {
;;    unsigned char r;        // Color red value
;;    unsigned char g;        // Color green value
;;    unsigned char b;        // Color blue value
;;    unsigned char a;        // Color alpha value
;;} Color;
#||
(defcstruct (%color :class color-type)
  "Color type, RGBA (32bit)"
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char)
  (a :unsigned-char))

(defmethod translate-into-foreign-memory (object (type color-type) pointer)
  (with-foreign-slots ((r g b a) pointer (:struct %color))
                      (setf r (nth 0 object))
                      (setf g (nth 1 object))
                      (setf b (nth 2 object))
                      (setf a (nth 3 object))))

(defmethod translate-from-foreign (pointer (type color-type))
  (with-foreign-slots ((r g b a) pointer (:struct %color))
   (list r g b a)))
||#

;;// Rectangle, 4 components
;;typedef struct Rectangle {
;;    float x;                // Rectangle top-left corner position x
;;    float y;                // Rectangle top-left corner position y
;;    float width;            // Rectangle width
;;    float height;           // Rectangle height
;;} Rectangle;
(defcstruct (%rectangle :class rectangle-type)
  "Rectangle type"
  (x :float)
  (y :float)
  (width :float)
  (height :float))

(defstruct rectangle
 x y width height)

(defmethod translate-into-foreign-memory (object (type rectangle-type) pointer)
  (with-foreign-slots ((x y width height) pointer (:struct %rectangle))
                      (setf x (coerce (rectangle-x object) 'float))
                      (setf y (coerce (rectangle-y object) 'float))
                      (setf width (coerce (rectangle-width object) 'float))
                      (setf height (coerce (rectangle-height object) 'float))))

(defmethod translate-from-foreign (pointer (type rectangle-type))
  (with-foreign-slots ((x y width height) pointer (:struct %rectangle))
                      (make-rectangle :x x :y y :width width :height height)))

;;// Image, pixel data stored in CPU memory (RAM)
;;typedef struct Image {
;;    void *data;             // Image raw data
;;    int width;              // Image base width
;;    int height;             // Image base height
;;    int mipmaps;            // Mipmap levels, 1 by default
;;    int format;             // Data format (PixelFormat type)
;;} Image;
(defcstruct (%image :class image-type)
"Image type, bpp always RGBA (32bit)"
  (data :pointer)
  (width :int)
  (height :int)
  (maps :int)
  (ft :int))

(defstruct image
 data width height maps ft)

(defmethod translate-into-foreign-memory (object (type image-type) pointer)
  (with-foreign-slots ((data width height maps ft) pointer (:struct %image))
                      (setf data (image-data object))
                      (setf width (image-width object))
                      (setf height (image-height object))
                      (setf maps (image-maps object))
                      (setf ft (image-ft object))))

(defmethod translate-from-foreign (pointer (type image-type))
  (with-foreign-slots ((data width height maps ft) pointer (:struct %image))
  (make-image :data data :width width :height height :maps maps :ft ft)))

;;// Texture, tex data stored in GPU memory (VRAM)
;;typedef struct Texture {
;;    unsigned int id;        // OpenGL texture id
;;    int width;              // Texture base width
;;    int height;             // Texture base height
;;    int mipmaps;            // Mipmap levels, 1 by default
;;    int format;             // Data format (PixelFormat type)
;;} Texture;
;;
;;// Texture2D, same as Texture
;;typedef Texture Texture2D;
;;
(defcstruct (%texture :class texture-type)
  "Texture type"
  (id :unsigned-int)
  (width :int)
  (height :int)
  (mipmaps :int)
  (format :int))

(defstruct texture
 id width height mipmaps format)

(defmethod translate-into-foreign-memory (object (type texture-type) pointer)
  (with-foreign-slots ((id width height mipmaps format) pointer (:struct %texture))
                      (setf id (texture-id object))
                      (setf width (texture-width object))
                      (setf height (texture-height object))
                      (setf mipmaps (texture-mipmaps object))
                      (setf format (texture-format object))))

(defmethod translate-from-foreign (pointer (type texture-type))
  (with-foreign-slots ((id width height mipmaps format) pointer (:struct %texture))
                      (make-texture :id id :width width :height height :mipmaps mipmaps :format format)))

;;// TextureCubemap, same as Texture
;;typedef Texture TextureCubemap;
(defctype texture-cubemap (:struct %texture))

;;// RenderTexture, fbo for texture rendering
;;typedef struct RenderTexture {
;;    unsigned int id;        // OpenGL framebuffer object id
;;    Texture texture;        // Color buffer attachment texture
;;    Texture depth;          // Depth buffer attachment texture
;;} RenderTexture;
;;
;;// RenderTexture2D, same as RenderTexture
;;typedef RenderTexture RenderTexture2D;
(defcstruct (%render-texture :class render-texture-type)
 "RenderTexture2D type, for texture rendering"
 (id :unsigned-int)
 (texture (:struct %texture))
 (depth (:struct %texture)))

(defstruct render-texture
 id texture depth)

(defmethod translate-into-foreign-memory (object (type render-texture-type) pointer)
  (with-foreign-slots ((id) pointer (:struct %render-texture))
                      (convert-into-foreign-memory (render-texture-texture object) '(:struct %texture) (foreign-slot-pointer pointer '(:struct %render-texture) 'texture))
                      (convert-into-foreign-memory (render-texture-depth object) '(:struct %texture) (foreign-slot-pointer pointer '(:struct %render-texture) 'depth))
                      (setf id (render-texture-id object))))

(defmethod translate-from-foreign (pointer (type render-texture-type))
  (with-foreign-slots ((id texture depth) pointer (:struct %render-texture))
    (make-render-texture :id id :texture texture :depth depth)))
;;
;;// NPatchInfo, n-patch layout info
;;typedef struct NPatchInfo {
;;    Rectangle source;       // Texture source rectangle
;;    int left;               // Left border offset
;;    int top;                // Top border offset
;;    int right;              // Right border offset
;;    int bottom;             // Bottom border offset
;;    int layout;             // Layout of the n-patch: 3x3, 1x3 or 3x1
;;} NPatchInfo;
(defcstruct (%patch-info :class patch-info-type)
 "n-patch layout info"
 (source (:struct %rectangle))
 (left :int)
 (top :int)
 (right :int)
 (bottom :int)
 (layout :int))

(defstruct patch-info
 source left top right bottom layout)

(defmethod translate-into-foreign-memory (object (type patch-info-type) pointer)
  (with-foreign-slots ((left top right bottom layout) pointer (:struct %patch-info))
                      (convert-into-foreign-memory (patch-info-source object) '(:struct %rectangle) (foreign-slot-pointer pointer '(:struct %patch-info) 'source))
                      (setf left (patch-info-left object))
                      (setf top (patch-info-top object))
                      (setf right (patch-info-right object))
                      (setf bottom (patch-info-bottom object))
                      (setf layout (patch-info-layout object))))

(defmethod translate-from-foreign (pointer (type patch-info-type))
  (with-foreign-slots ((source left top right bottom layout) pointer (:struct %patch-info))
    (make-patch-info :source source :left left :top top :right right :bottom bottom :layout layout)))

;;// GlyphInfo, font characters glyphs info
;;typedef struct GlyphInfo {
;;    int value;              // Character value (Unicode)
;;    int offsetX;            // Character offset X when drawing
;;    int offsetY;            // Character offset Y when drawing
;;    int advanceX;           // Character advance position X
;;    Image image;            // Character image data
;;} GlyphInfo;
(defcstruct (%glyph-info :class glyph-info-type)
 "font characters glyphs info"
 (value :int)
 (offset-x :int)
 (offset-y :int)
 (advance-x :int)
 (image (:struct %image)))

(defstruct glyph-info
 value offset-x offset-y advance-x image)

(defmethod translate-into-foreign-memory (object (type glyph-info-type) pointer)
  (with-foreign-slots ((value offset-x offset-y advance-x) pointer (:struct %glyph-info))
                      (convert-into-foreign-memory (glyph-info-image object) '(:struct %image) (foreign-slot-pointer pointer '(:struct %glyph-info) 'image))
                      (setf value (glyph-info-value object))
                      (setf offset-x (glyph-info-offset-x object))
                      (setf offset-y (glyph-info-offset-y object))
                      (setf advance-x (glyph-info-advance-x object))))

(defmethod translate-from-foreign (pointer (type glyph-info-type))
  (with-foreign-slots ((value offset-x offset-y advance-x image) pointer (:struct %glyph-info))
    (make-glyph-info :value value
                     :offset-x offset-x
                     :offset-y offset-y
                     :advance-x advance-x
                     :image image)))
 
;;
;;// Font, font texture and GlyphInfo array data
;;typedef struct Font {
;;    int baseSize;           // Base size (default chars height)
;;    int glyphCount;         // Number of glyph characters
;;    int glyphPadding;       // Padding around the glyph characters
;;    Texture2D texture;      // Texture atlas containing the glyphs
;;    Rectangle *recs;        // Rectangles in texture for the glyphs
;;    GlyphInfo *glyphs;      // Glyphs info data
;;} Font;
(defcstruct (%font :class font-type)
 "Font, font texture and GlyphInfo array data"
 (base-size :int)
 (glyph-count :int)
 (glyph-padding :int)
 (texture (:struct %texture))
 (recs :pointer)
 (glyphs (:pointer (:struct %glyph-info))))

(defstruct font
  base-size glyph-count glyph-padding texture recs glyphs)

(defmethod translate-into-foreign-memory (object (type font-type) pointer)
  (with-foreign-slots ((base-size glyph-count glyph-padding recs glyphs) pointer (:struct %font))
                      (convert-into-foreign-memory (font-texture object) '(:struct %texture) (foreign-slot-pointer pointer '(:struct %font) 'texture))
                      (setf base-size (font-base-size object))
                      (setf glyph-count (font-glyph-count object))
                      (setf glyph-padding (font-glyph-padding object))
                      (setf recs (font-recs object))
                      (setf glyphs (font-glyphs object))))

(defmethod translate-from-foreign (pointer (type font-type))
  (with-foreign-slots ((base-size glyph-count glyph-padding texture recs glyphs) pointer (:struct %font))
    (make-font :base-size base-size
               :glyph-count glyph-count
               :glyph-padding glyph-padding
               :texture texture
               :recs recs
               :glyphs glyphs)))

;;// Camera, defines position/orientation in 3d space
;;typedef struct Camera3D {
;;    Vector3 position;       // Camera position
;;    Vector3 target;         // Camera target it looks-at
;;    Vector3 up;             // Camera up vector (rotation over its axis)
;;    float fovy;             // Camera field-of-view apperture in Y (degrees) in perspective, used as near plane width in orthographic
;;    int projection;         // Camera projection: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
;;} Camera3D;
;;
;;typedef Camera3D Camera;    // Camera type fallback, defaults to Camera3D
(defcstruct (%camera3d :class camera3d-type)
  "Camera, defines position/orientation in 3d space"
  (position (:struct %vector3))
  (target (:struct %vector3))
  (up (:struct %vector3))
  (fovy :float)
  (projection :int))

(defstruct camera3d
  position target up fovy projection)

(defmethod translate-into-foreign-memory (object (type camera3d-type) pointer)
  (with-foreign-slots ((fovy projection) pointer (:struct %camera3d))
    (convert-into-foreign-memory (camera3d-position object) '(:struct %vector3) (foreign-slot-pointer pointer '(:struct %camera3d) 'position))
    (convert-into-foreign-memory (camera3d-target object) '(:struct %vector3) (foreign-slot-pointer pointer '(:struct %camera3d) 'target))
    (convert-into-foreign-memory (camera3d-up object) '(:struct %vector3) (foreign-slot-pointer pointer '(:struct %camera3d) 'up))
    (setf fovy (coerce (camera3d-fovy object) 'float)
          projection (camera3d-projection object))))

(defmethod translate-from-foreign (pointer (type camera3d-type))
  (with-foreign-slots ((position target up fovy projection) pointer (:struct %camera3d))
    (make-camera3d :position position
                   :target target
                   :up up
                   :fovy fovy
                   :projection projection)))

(defmacro update-camera3d-from-foreign (lisp-var ptr)
  `(cffi:with-foreign-slots ((position target up fovy projection) ,ptr (:struct %camera3d))
     (setf (camera3d-position ,lisp-var) position)
     (setf (camera3d-target ,lisp-var) target)
     (setf (camera3d-fovy ,lisp-var) fovy)
     (setf (camera3d-up ,lisp-var) up)
     (setf (camera3d-projection ,lisp-var) projection)))

;;// Camera2D, defines position/orientation in 2d space
;;typedef struct Camera2D {
;;    Vector2 offset;         // Camera offset (displacement from target)
;;    Vector2 target;         // Camera target (rotation and zoom origin)
;;    float rotation;         // Camera rotation in degrees
;;    float zoom;             // Camera zoom (scaling), should be 1.0f by default
;;} Camera2D;
(defcstruct (%camera2d :class camera2d-type)
 "Camera2D, defines position/orientation in 2d space"
 (offset (:struct %vector2))
 (target (:struct %vector2))
 (rotation :float)
 (zoom :float))

(defstruct camera2d
 offset target rotation zoom)

(defmethod translate-into-foreign-memory (object (type camera2d-type) pointer)
  (with-foreign-slots ((rotation zoom) pointer (:struct %camera2d))
                      (convert-into-foreign-memory (camera2d-offset object) '(:struct %vector2) (foreign-slot-pointer pointer '(:struct %camera2d) 'offset))
                      (convert-into-foreign-memory (camera2d-target object) '(:struct %vector2) (foreign-slot-pointer pointer '(:struct %camera2d) 'target))
                      (setf rotation (coerce (camera2d-rotation object) 'float))
                      (setf zoom (coerce (camera2d-zoom object) 'float))))

(defmethod translate-from-foreign (pointer (type camera2d-type))
  (with-foreign-slots ((offset target rotation zoom) pointer (:struct %camera2d))
    (make-camera2d :offset offset
                   :target target
                   :rotation rotation
                   :zoom zoom)))

;;// Mesh, vertex data and vao/vbo
;;typedef struct Mesh {
;;    int vertexCount;        // Number of vertices stored in arrays
;;    int triangleCount;      // Number of triangles stored (indexed or not)
;;
;;    // Vertex attributes data
;;    float *vertices;        // Vertex position (XYZ - 3 components per vertex) (shader-location = 0)
;;    float *texcoords;       // Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
;;    float *texcoords2;      // Vertex second texture coordinates (useful for lightmaps) (shader-location = 5)
;;    float *normals;         // Vertex normals (XYZ - 3 components per vertex) (shader-location = 2)
;;    float *tangents;        // Vertex tangents (XYZW - 4 components per vertex) (shader-location = 4)
;;    unsigned char *colors;      // Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
;;    unsigned short *indices;    // Vertex indices (in case vertex data comes indexed)
;;
;;    // Animation vertex data
;;    float *animVertices;    // Animated vertex positions (after bones transformations)
;;    float *animNormals;     // Animated normals (after bones transformations)
;;    unsigned char *boneIds; // Vertex bone ids, max 255 bone ids, up to 4 bones influence by vertex (skinning)
;;    float *boneWeights;     // Vertex bone weight, up to 4 bones influence by vertex (skinning)
;;
;;    // OpenGL identifiers
;;    unsigned int vaoId;     // OpenGL Vertex Array Object id
;;    unsigned int *vboId;    // OpenGL Vertex Buffer Objects id (default vertex data)
;;} Mesh;
(defcstruct (%mesh :class mesh-type)
 "Vertex data definning a mesh"
  (vertex-count :int)
  (triangle-count :int)
  (vertices (:pointer :float))
  (texcoords (:pointer :float))
  (texcoords2 (:pointer :float))
  (normals (:pointer :float))
  (tangents (:pointer :float))
  (colors (:pointer :unsigned-char))
  (indices (:pointer :unsigned-short))
  (anim-vertices (:pointer :float))
  (anim-normals (:pointer :float))
  (bone-ids (:pointer :unsigned-char))
  (bone-weights (:pointer :float))
  (vao-id :unsigned-int)
  (vbo-id (:pointer :unsigned-int)))

(defmethod translate-into-foreign-memory (object (type mesh-type) pointer)
  (with-foreign-slots ((vertex-count triangle-count vertices texcoords texcoords2 normals tangents colors indices anim-vertices anim-normals bone-ids bone-weights vao-id vbo-id) pointer (:struct %mesh))
                      (setf vertex-count (nth 0 object))
                      (setf triangle-count (nth 1 object))
                      (setf vertices (nth 2 object))
                      (setf texcoords (nth 3 object))
                      (setf texcoords2 (nth 4 object))
                      (setf normals (nth 5 object))
                      (setf tangents (nth 6 object))
                      (setf colors (nth 7 object))
                      (setf indices (nth 8 object))
                      (setf anim-vertices (nth 9 object))
                      (setf anim-normals (nth 10 object))
                      (setf bone-ids (nth 11 object))
                      (setf bone-weights (nth 12 object))
                      (setf vao-id (nth 13 object))
                      (setf vbo-id (nth 14 object))))

(defmethod translate-from-foreign (pointer (type mesh-type))
  (with-foreign-slots ((vertex-count triangle-count vertices texcoords texcoords2 normals tangents colors indices anim-vertices anim-normals bone-ids bone-weights vao-id vbo-id) pointer (:struct %mesh))
                      (list vertex-count triangle-count vertices texcoords texcoords2 normals tangents colors indices anim-vertices anim-normals bone-ids bone-weights vao-id vbo-id)))

;;// Shader
;;typedef struct Shader {
;;    unsigned int id;        // Shader program id
;;    int *locs;              // Shader locations array (RL_MAX_SHADER_LOCATIONS)
;;} Shader;
(defcstruct (%shader :class shader-type)
 "Shader"
 (id :unsigned-int)
 (locs (:pointer :int)))
 
 (defmethod translate-into-foreign-memory (object (type shader-type) pointer)
 (with-foreign-slots ((id locs) pointer (:struct %shader))
                      (setf id (nth 0 object))
                      (setf locs (nth 1 object))))

(defmethod translate-from-foreign (pointer (type shader-type))
  (with-foreign-slots ((id locs) pointer (:struct %shader))
    (list id locs)))

;;// MaterialMap
;;typedef struct MaterialMap {
;;    Texture2D texture;      // Material map texture
;;    Color color;            // Material map color
;;    float value;            // Material map value
;;} MaterialMap;
(defcstruct (%material-map :class material-map-type)
 "Material texture map"
 (texture (:struct %texture))
 (color :uint)
 (value :float))
 
(defmethod translate-into-foreign-memory (object (type material-map-type) pointer)
 (with-foreign-slots ((texture color value) pointer (:struct %material-map))
                      (setf texture (nth 0 object))
                      (setf color (nth 1 object))
                      (setf value (coerce (nth 2 object) 'float))))

(defmethod translate-from-foreign (pointer (type material-map-type))
  (with-foreign-slots ((texture color value) pointer (:struct %material-map))
    (list texture color value)))
;;
;;// Material, includes shader and maps
;;typedef struct Material {
;;    Shader shader;          // Material shader
;;    MaterialMap *maps;      // Material maps array (MAX_MATERIAL_MAPS)
;;    float params[4];        // Material generic parameters (if required)
;;} Material;
(defcstruct (%material :class material-type)
 "Material type"
 (shader (:struct %shader))
 (maps (:pointer (:struct %material-map)))
 (params (:pointer :float)))

(defmethod translate-into-foreign-memory (object (type material-type) pointer)
 (with-foreign-slots ((shader maps params) pointer (:struct %material))
                      (setf shader (nth 0 object))
                      (setf maps (nth 1 object))
                      (setf params (nth 2 object))))

(defmethod translate-from-foreign (pointer (type material-type))
  (with-foreign-slots ((shader maps params) pointer (:struct %material))
                      (list shader maps params)))

;;
;;// Transform, vectex transformation data
;;typedef struct Transform {
;;    Vector3 translation;    // Translation
;;    Quaternion rotation;    // Rotation
;;    Vector3 scale;          // Scale
;;} Transform;
(defcstruct (%transform :class transform-type)
 "Transformation properties"
 (translation (:struct %vector3))
 (rotation (:struct %vector4))
 (scale (:struct %vector3)))

(defmethod translate-into-foreign-memory (object (type transform-type) pointer)
 (with-foreign-slots ((translation rotation scale) pointer (:struct %transform))
                      (setf translation (nth 0 object))
                      (setf rotation (nth 1 object))
                      (setf scale (nth 2 object))))

(defmethod translate-from-foreign (pointer (type transform-type))
 (with-foreign-slots ((translation rotation scale) pointer (:struct %transform))
                     (list translation rotation scale)))

;;
;;// Bone, skeletal animation bone
;;typedef struct BoneInfo {
;;    char name[32];          // Bone name
;;    int parent;             // Bone parent
;;} BoneInfo;
(defcstruct (%bone-info :class bone-info-type)
 "Bone information"
 (name :string)
 (parent :int))

(defmethod translate-into-foreign-memory (object (type bone-info-type) pointer)
 (with-foreign-slots ((name parent) pointer (:struct %bone-info))
                      (setf name (nth 0 object))
                      (setf parent (nth 1 object))))

(defmethod translate-from-foreign (pointer (type bone-info-type))
 (with-foreign-slots ((name parent) pointer (:struct %bone-info))
                     (list name parent)))

;;
;;// Model, meshes, materials and animation data
;;typedef struct Model {
;;    Matrix transform;       // Local transform matrix
;;
;;    int meshCount;          // Number of meshes
;;    int materialCount;      // Number of materials
;;    Mesh *meshes;           // Meshes array
;;    Material *materials;    // Materials array
;;    int *meshMaterial;      // Mesh material number
;;
;;    // Animation data
;;    int boneCount;          // Number of bones
;;    BoneInfo *bones;        // Bones information (skeleton)
;;    Transform *bindPose;    // Bones base transformation (pose)
;;} Model;
(defcstruct (%model :class model-type)
 "Model type"
 (transform (:struct %matrix))
 (mesh-count :int)
 (material-count :int)
 (meshes (:pointer (:struct %mesh)))
 (materials (:pointer (:struct %material)))
 (mesh-material (:pointer :int))
 (bone-count :int)
 (bones (:struct %bone-info))
 (bind-pose (:struct %transform)))

(defmethod translate-into-foreign-memory (object (type model-type) pointer)
 (with-foreign-slots ((transform mesh-count material-count meshes materials mesh-material bone-count bones bind-pose) pointer (:struct %model))
                      (setf transform (nth 0 object))
                      (setf mesh-count (nth 1 object))
                      (setf material-count (nth 2 object))
                      (setf meshes (nth 3 object))
                      (setf materials (nth 4 object))
                      (setf mesh-material (nth 5 object))
                      (setf bone-count (nth 6 object))
                      (setf bones (nth 7 object))
                      (setf bind-pose (nth 8 object))))

(defmethod translate-from-foreign (pointer (type model-type))
 (with-foreign-slots ((transform mesh-count material-count meshes materials mesh-material bone-count bones bind-pose) pointer (:struct %model))
                     (list transform mesh-count material-count meshes materials mesh-material bone-count bones bind-pose)))
;;
;;// ModelAnimation
;;typedef struct ModelAnimation {
;;    int boneCount;          // Number of bones
;;    int frameCount;         // Number of animation frames
;;    BoneInfo *bones;        // Bones information (skeleton)
;;    Transform **framePoses; // Poses array by frame
;;} ModelAnimation;
(defcstruct (%model-animation :class model-animation-type)
 "ModelAnimation"
 (bone-count :int)
 (frame-count :int)
 (bones (:pointer (:struct %bone-info)))
 (frame-poses (:pointer)))

(defmethod translate-into-foreign-memory (object (type model-animation-type) pointer)
 (with-foreign-slots ((bone-count frame-count bones frame-poses) pointer (:struct %model-animation))
                      (setf bone-count (nth 0 object))
                      (setf frame-count (nth 1 object))
                      (setf bones (nth 2 object))
                      (setf frame-poses (nth 3 object))))

(defmethod translate-from-foreign (pointer (type model-animation-type))
 (with-foreign-slots ((bone-count frame-count bones frame-poses) pointer (:struct %model-animation))
                     (list bone-count frame-count bones frame-poses)))

;;
;;// Ray, ray for raycasting
;;typedef struct Ray {
;;    Vector3 position;       // Ray position (origin)
;;    Vector3 direction;      // Ray direction
;;} Ray;
(defcstruct (%ray :class ray-type)
 "Ray type (useful for raycast)"
 (position (:struct %vector3))
 (direction (:struct %vector3)))

(defmethod translate-into-foreign-memory (object (type ray-type) pointer)
 (with-foreign-slots ((position direction) pointer (:struct %ray))
                      (setf position (nth 0 object))
                      (setf direction (nth 1 object))))

(defmethod translate-from-foreign (pointer (type ray-type))
 (with-foreign-slots ((position direction) pointer (:struct %ray))
                      (list position direction)))

;;
;;// RayCollision, ray hit information
;;typedef struct RayCollision {
;;    bool hit;               // Did the ray hit something?
;;    float distance;         // Distance to nearest hit
;;    Vector3 point;          // Point of nearest hit
;;    Vector3 normal;         // Surface normal of hit
;;} RayCollision;
(defcstruct (%ray-collision :class ray-collision-type)
 "Raycast hit information"
 (hit :boolean)
 (distance :float)
 (point (:struct %vector3))
 (normal (:struct %vector3)))

(defmethod translate-into-foreign-memory (object (type ray-collision-type) pointer)
 (with-foreign-slots ((hit distance point normal) pointer (:struct %ray-collision))
                      (setf hit (nth 0 object))
                      (setf distance (nth 1 object))
                      (setf point (nth 2 object))
                      (setf normal (nth 3 object))))

(defmethod translate-from-foreign (pointer (type ray-collision-type))
 (with-foreign-slots ((hit distance point normal) pointer (:struct %ray-collision))
                     (list hit distance point normal)))

;;
;;// BoundingBox
;;typedef struct BoundingBox {
;;    Vector3 min;            // Minimum vertex box-corner
;;    Vector3 max;            // Maximum vertex box-corner
;;} BoundingBox;
(defcstruct (%bounding-box :class bounding-box-type)
 "Bounding box type"
 (min (:struct %vector3))
 (max (:struct %vector3)))

(defmethod translate-into-foreign-memory (object (type bounding-box-type) pointer)
 (with-foreign-slots ((min max) pointer (:struct %bounding-box))
                      (setf min (nth 0 object))
                      (setf max (nth 1 object))))

(defmethod translate-from-foreign (pointer (type bounding-box-type))
 (with-foreign-slots ((min max) pointer (:struct %bounding-box))
  (list min max)))

;;
;;// Wave, audio wave data
;;typedef struct Wave {
;;    unsigned int frameCount;    // Total number of frames (considering channels)
;;    unsigned int sampleRate;    // Frequency (samples per second)
;;    unsigned int sampleSize;    // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
;;    unsigned int channels;      // Number of channels (1-mono, 2-stereo, ...)
;;    void *data;                 // Buffer data pointer
;;} Wave;
(defcstruct (%wave :class wave-type)
 "Wave type, defines audio wave data"
 (frame-count :unsigned-int)
 (sample-rate :unsigned-int)
 (sample-size :unsigned-int)
 (channels :unsigned-int)
 (data :pointer))

(defmethod translate-into-foreign-memory (object (type wave-type) pointer)
 (with-foreign-slots ((frame-count sample-rate sample-size channels data) pointer (:struct %wave))
                      (setf frame-count (nth 0 object))
                      (setf sample-rate (nth 1 object))
                      (setf sample-size (nth 2 object))
                      (setf channels (nth 3 object))
                      (setf data (nth 4 object))))

(defmethod translate-from-foreign (pointer (type wave-type))
 (with-foreign-slots ((frame-count sample-rate sample-size channels data) pointer (:struct %wave))
 (list frame-count sample-rate sample-size channels data)))
;;
;;typedef struct rAudioBuffer rAudioBuffer;
;;
;;// AudioStream, custom audio stream
;;typedef struct AudioStream {
;;    rAudioBuffer *buffer;       // Pointer to internal data used by the audio system
;;
;;    unsigned int sampleRate;    // Frequency (samples per second)
;;    unsigned int sampleSize;    // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
;;    unsigned int channels;      // Number of channels (1-mono, 2-stereo, ...)
;;} AudioStream;
(defcstruct (%audio-stream :class audio-stream-type)
 "Audio stream type"
  (buffer :pointer)
  (sample-rate :unsigned-int)
  (sample-size :unsigned-int)
  (channels :unsigned-int))

(defmethod translate-into-foreign-memory (object (type audio-stream-type) pointer)
 (with-foreign-slots ((buffer sample-rate sample-size channels) pointer (:struct %audio-stream))
                      (setf buffer (nth 0 object))
                      (setf sample-rate (nth 1 object))
                      (setf sample-size (nth 2 object))
                      (setf channels (nth 3 object))))

(defmethod translate-from-foreign (pointer (type audio-stream-type))
  (with-foreign-slots ((buffer sample-rate sample-size channels) pointer (:struct %audio-stream))
                      (list buffer sample-rate sample-size channels)))

;;// Sound
;;typedef struct Sound {
;;    AudioStream stream;         // Audio stream
;;    unsigned int frameCount;    // Total number of frames (considering channels)
;;} Sound;
(defcstruct (%sound :class sound-type)
 "Sound source type"
 (stream (:struct %audio-stream))
 (frame-count :unsigned-int))

(defmethod translate-into-foreign-memory (object (type sound-type) pointer)
 (with-foreign-slots ((stream frame-count) pointer (:struct %sound))
                      (convert-into-foreign-memory (nth 0 object)
                                                   '(:struct %audio-stream)
                                                   (foreign-slot-pointer pointer '(:struct %sound) 'stream))
                      (setf frame-count (nth 1 object))))

(defmethod translate-from-foreign (pointer (type sound-type))
  (with-foreign-slots ((stream frame-count) pointer (:struct %sound))
                      (list stream frame-count)))
;;
;;// Music, audio stream, anything longer than ~10 seconds should be streamed
;;typedef struct Music {
;;    AudioStream stream;         // Audio stream
;;    unsigned int frameCount;    // Total number of frames (considering channels)
;;    bool looping;               // Music looping enable
;;
;;    int ctxType;                // Type of music context (audio filetype)
;;    void *ctxData;              // Audio context data, depends on type
;;} Music;
(defcstruct (%music :class music-type)
 "Music, audio stream, anything longer than ~10 seconds should be streamed"
 (stream (:struct %audio-stream))
 (frame-count :unsigned-int)
 (looping :boolean)
 (ctx-type :int)
 (ctx-data :pointer))

(defmethod translate-into-foreign-memory (object (type music-type) pointer)
 (with-foreign-slots ((stream frame-count looping ctx-type ctx-data) pointer (:struct %music))
                      (convert-into-foreign-memory (nth 0 object)
                                                   '(:struct %audio-stream)
                                                   (foreign-slot-pointer pointer '(:struct %music) 'stream))
                      (setf frame-count (nth 1 object))
                      (setf looping (nth 2 object))
                      (setf ctx-type (nth 3 object))
                      (setf ctx-data (nth 4 object))))

(defmethod translate-from-foreign (pointer (type music-type))
  (with-foreign-slots ((stream frame-count looping ctx-type ctx-data) pointer (:struct %music))
                      (list stream frame-count looping ctx-type ctx-data)))

;;// VrDeviceInfo, Head-Mounted-Display device parameters
;;typedef struct VrDeviceInfo {
;;    int hResolution;                // Horizontal resolution in pixels
;;    int vResolution;                // Vertical resolution in pixels
;;    float hScreenSize;              // Horizontal size in meters
;;    float vScreenSize;              // Vertical size in meters
;;    float vScreenCenter;            // Screen center in meters
;;    float eyeToScreenDistance;      // Distance between eye and display in meters
;;    float lensSeparationDistance;   // Lens separation distance in meters
;;    float interpupillaryDistance;   // IPD (distance between pupils) in meters
;;    float lensDistortionValues[4];  // Lens distortion constant parameters
;;    float chromaAbCorrection[4];    // Chromatic aberration correction parameters
;;} VrDeviceInfo;
(defcstruct (%vr-device-info :class vr-device-info-type)
  "Head-Mounted-Display device parameters"
  (h-resolution :int)
  (v-resolution :int)
  (h-screen-size :float)
  (v-screen-size :float)
  (v-screen-center :float)
  (eye-to-screen-distance :float)
  (lens-separation-distance :float)
  (interpupillary-distance :float)
  (lens-distortion-values :float :count 4)
  (chroma-ab-correction :float :count 4))

(defmethod translate-into-foreign-memory (object (type vr-device-info-type) pointer)
  (with-foreign-slots ((h-resolution v-resolution h-screen-size v-screen-size v-screen-center eye-to-screen-distance lens-separation-distance interpupillary-distance lens-distortion-values chroma-ab-correction) pointer (:struct %vr-device-info))
    (setf h-resolution (nth 0 object))
    (setf v-resolution (nth 1 object))
    (setf h-screen-size (coerce (nth 2 object) 'float))
    (setf v-screen-size (coerce (nth 3 object) 'float))
    (setf v-screen-center (coerce (nth 4 object) 'float))
    (setf eye-to-screen-distance (coerce (nth 5 object) 'float))
    (setf lens-separation-distance (coerce (nth 6 object) 'float))
    (setf interpupillary-distance (coerce (nth 7 object) 'float))
    (setf lens-distortion-values (nth 8 object))
    (setf chroma-ab-correction (nth 9 object))))

(defmethod translate-from-foreign (pointer (type vr-device-info-type))
  (with-foreign-slots ((h-resolution v-resolution h-screen-size v-screen-size v-screen-center eye-to-screen-distance lens-separation-distance interpupillary-distance lens-distortion-values chroma-ab-correction) pointer (:struct %vr-device-info))
    (list h-resolution v-resolution h-screen-size v-screen-size v-screen-center eye-to-screen-distance lens-separation-distance interpupillary-distance lens-distortion-values chroma-ab-correction)))

;;// VrStereoConfig, VR stereo rendering configuration for simulator
;;typedef struct VrStereoConfig {
;;    Matrix projection[2];           // VR projection matrices (per eye)
;;    Matrix viewOffset[2];           // VR view offset matrices (per eye)
;;    float leftLensCenter[2];        // VR left lens center
;;    float rightLensCenter[2];       // VR right lens center
;;    float leftScreenCenter[2];      // VR left screen center
;;    float rightScreenCenter[2];     // VR right screen center
;;    float scale[2];                 // VR distortion scale
;;    float scaleIn[2];               // VR distortion scale in
;;} VrStereoConfig;
(defcstruct (%vr-stereo-config :class vr-stereo-config)
  "VR stereo rendering configuration for simulator"
  (projection (:struct %matrix) :count 2)
  (view-offset (:struct %matrix) :count 2)
  (left-lens-center :float :count 2)
  (right-lens-center :float :count 2)
  (left-screen-center :float :count 2)
  (right-screen-center :float :count 2)
  (scale :float :count 2)
  (scale-in :float :count 2))

; TODO

;;//----------------------------------------------------------------------------------
;;// Enumerators Definition
;;//----------------------------------------------------------------------------------
;;// System/Window config flags
;;// NOTE: Every bit registers one state (use it with bit masks)
;;// By default all flags are set to 0
;;typedef enum {
;;    FLAG_VSYNC_HINT         = 0x00000040,   // Set to try enabling V-Sync on GPU
;;    FLAG_FULLSCREEN_MODE    = 0x00000002,   // Set to run program in fullscreen
;;    FLAG_WINDOW_RESIZABLE   = 0x00000004,   // Set to allow resizable window
;;    FLAG_WINDOW_UNDECORATED = 0x00000008,   // Set to disable window decoration (frame and buttons)
;;    FLAG_WINDOW_HIDDEN      = 0x00000080,   // Set to hide window
;;    FLAG_WINDOW_MINIMIZED   = 0x00000200,   // Set to minimize window (iconify)
;;    FLAG_WINDOW_MAXIMIZED   = 0x00000400,   // Set to maximize window (expanded to monitor)
;;    FLAG_WINDOW_UNFOCUSED   = 0x00000800,   // Set to window non focused
;;    FLAG_WINDOW_TOPMOST     = 0x00001000,   // Set to window always on top
;;    FLAG_WINDOW_ALWAYS_RUN  = 0x00000100,   // Set to allow windows running while minimized
;;    FLAG_WINDOW_TRANSPARENT = 0x00000010,   // Set to allow transparent framebuffer
;;    FLAG_WINDOW_HIGHDPI     = 0x00002000,   // Set to support HighDPI
;;    FLAG_MSAA_4X_HINT       = 0x00000020,   // Set to try enabling MSAA 4X
;;    FLAG_INTERLACED_HINT    = 0x00010000    // Set to try enabling interlaced video format (for V3D)
;;} ConfigFlags;

(define-constant   +flag-vsync-hint+         #x00000040)
(define-constant   +flag-fullscreen-mode+    #x00000002)
(define-constant   +flag-window-resizable+   #x00000004)
(define-constant   +flag-window-undecorated+ #x00000008)
(define-constant   +flag-window-hidden+      #x00000080)
(define-constant   +flag-window-minimized+   #x00000200)
(define-constant   +flag-window-maximized+   #x00000400)
(define-constant   +flag-window-unfocused+   #x00000800)
(define-constant   +flag-window-topmost+     #x00001000)
(define-constant   +flag-window-always-run+  #x00000100)
(define-constant   +flag-window-transparent+ #x00000010)
(define-constant   +flag-window-highdpi+     #x00002000)
(define-constant   +flag-msaa-4x-hint+       #x00000020)
(define-constant   +flag-interlaced-hint+    #x00010000)

;;// Trace log level
;;// NOTE: Organized by priority level
;;typedef enum {
;;    LOG_ALL = 0,        // Display all logs
;;    LOG_TRACE,          // Trace logging, intended for internal use only
;;    LOG_DEBUG,          // Debug logging, used for internal debugging, it should be disabled on release builds
;;    LOG_INFO,           // Info logging, used for program execution info
;;    LOG_WARNING,        // Warning logging, used on recoverable failures
;;    LOG_ERROR,          // Error logging, used on unrecoverable failures
;;    LOG_FATAL,          // Fatal logging, used to abort program: exit(EXIT_FAILURE)
;;    LOG_NONE            // Disable logging
;;} TraceLogLevel;
;;
;;// Keyboard keys (US keyboard layout)
;;// NOTE: Use GetKeyPressed() to allow redefining
;;// required keys for alternative layouts
;;typedef enum {
;;    KEY_NULL            = 0,        // Key: NULL, used for no key pressed
;;    // Alphanumeric keys
;;    KEY_APOSTROPHE      = 39,       // Key: '
;;    KEY_COMMA           = 44,       // Key: ,
;;    KEY_MINUS           = 45,       // Key: -
;;    KEY_PERIOD          = 46,       // Key: .
;;    KEY_SLASH           = 47,       // Key: /
;;    KEY_ZERO            = 48,       // Key: 0
;;    KEY_ONE             = 49,       // Key: 1
;;    KEY_TWO             = 50,       // Key: 2
;;    KEY_THREE           = 51,       // Key: 3
;;    KEY_FOUR            = 52,       // Key: 4
;;    KEY_FIVE            = 53,       // Key: 5
;;    KEY_SIX             = 54,       // Key: 6
;;    KEY_SEVEN           = 55,       // Key: 7
;;    KEY_EIGHT           = 56,       // Key: 8
;;    KEY_NINE            = 57,       // Key: 9
;;    KEY_SEMICOLON       = 59,       // Key: ;
;;    KEY_EQUAL           = 61,       // Key: =
;;    KEY_A               = 65,       // Key: A | a
;;    KEY_B               = 66,       // Key: B | b
;;    KEY_C               = 67,       // Key: C | c
;;    KEY_D               = 68,       // Key: D | d
;;    KEY_E               = 69,       // Key: E | e
;;    KEY_F               = 70,       // Key: F | f
;;    KEY_G               = 71,       // Key: G | g
;;    KEY_H               = 72,       // Key: H | h
;;    KEY_I               = 73,       // Key: I | i
;;    KEY_J               = 74,       // Key: J | j
;;    KEY_K               = 75,       // Key: K | k
;;    KEY_L               = 76,       // Key: L | l
;;    KEY_M               = 77,       // Key: M | m
;;    KEY_N               = 78,       // Key: N | n
;;    KEY_O               = 79,       // Key: O | o
;;    KEY_P               = 80,       // Key: P | p
;;    KEY_Q               = 81,       // Key: Q | q
;;    KEY_R               = 82,       // Key: R | r
;;    KEY_S               = 83,       // Key: S | s
;;    KEY_T               = 84,       // Key: T | t
;;    KEY_U               = 85,       // Key: U | u
;;    KEY_V               = 86,       // Key: V | v
;;    KEY_W               = 87,       // Key: W | w
;;    KEY_X               = 88,       // Key: X | x
;;    KEY_Y               = 89,       // Key: Y | y
;;    KEY_Z               = 90,       // Key: Z | z
;;    KEY_LEFT_BRACKET    = 91,       // Key: [
;;    KEY_BACKSLASH       = 92,       // Key: '\'
;;    KEY_RIGHT_BRACKET   = 93,       // Key: ]
;;    KEY_GRAVE           = 96,       // Key: `
(define-constant +key-null+ 0)
(define-constant +key-apostrophe+ 39)
(define-constant +key-comma+ 44)
(define-constant +key-minus+ 45)
(define-constant +key-period+ 46)
(define-constant +key-slash+ 47)
(define-constant +key-zero+ 48)
(define-constant +key-one+ 49)
(define-constant +key-two+ 50)
(define-constant +key-three+ 51)
(define-constant +key-four+ 52)
(define-constant +key-five+ 53)
(define-constant +key-six+ 54)
(define-constant +key-seven+ 55)
(define-constant +key-eight+ 56)
(define-constant +key-nine+ 57)
(define-constant +key-semicolon+ 59)
(define-constant +key-equal+ 61)
(define-constant +key-a+ 65)
(define-constant +key-b+ 66)
(define-constant +key-c+ 67)
(define-constant +key-d+ 68)
(define-constant +key-e+ 69)
(define-constant +key-f+ 70)
(define-constant +key-g+ 71)
(define-constant +key-h+ 72)
(define-constant +key-i+ 73)
(define-constant +key-j+ 74)
(define-constant +key-k+ 75)
(define-constant +key-l+ 76)
(define-constant +key-m+ 77)
(define-constant +key-n+ 78)
(define-constant +key-o+ 79)
(define-constant +key-p+ 80)
(define-constant +key-q+ 81)
(define-constant +key-r+ 82)
(define-constant +key-s+ 83)
(define-constant +key-t+ 84)
(define-constant +key-u+ 85)
(define-constant +key-v+ 86)
(define-constant +key-w+ 87)
(define-constant +key-x+ 88)
(define-constant +key-y+ 89)
(define-constant +key-z+ 90)
(define-constant +key-left-bracket+ 91)
(define-constant +key-backslash+ 92)
(define-constant +key-right-bracket+ 93)
(define-constant +key-grave+ 96)

;;    // Function keys
;;    KEY_SPACE           = 32,       // Key: Space
;;    KEY_ESCAPE          = 256,      // Key: Esc
;;    KEY_ENTER           = 257,      // Key: Enter
;;    KEY_TAB             = 258,      // Key: Tab
;;    KEY_BACKSPACE       = 259,      // Key: Backspace
;;    KEY_INSERT          = 260,      // Key: Ins
;;    KEY_DELETE          = 261,      // Key: Del
;;    KEY_RIGHT           = 262,      // Key: Cursor right
;;    KEY_LEFT            = 263,      // Key: Cursor left
;;    KEY_DOWN            = 264,      // Key: Cursor down
;;    KEY_UP              = 265,      // Key: Cursor up
;;    KEY_PAGE_UP         = 266,      // Key: Page up
;;    KEY_PAGE_DOWN       = 267,      // Key: Page down
;;    KEY_HOME            = 268,      // Key: Home
;;    KEY_END             = 269,      // Key: End
;;    KEY_CAPS_LOCK       = 280,      // Key: Caps lock
;;    KEY_SCROLL_LOCK     = 281,      // Key: Scroll down
;;    KEY_NUM_LOCK        = 282,      // Key: Num lock
;;    KEY_PRINT_SCREEN    = 283,      // Key: Print screen
;;    KEY_PAUSE           = 284,      // Key: Pause
;;    KEY_F1              = 290,      // Key: F1
;;    KEY_F2              = 291,      // Key: F2
;;    KEY_F3              = 292,      // Key: F3
;;    KEY_F4              = 293,      // Key: F4
;;    KEY_F5              = 294,      // Key: F5
;;    KEY_F6              = 295,      // Key: F6
;;    KEY_F7              = 296,      // Key: F7
;;    KEY_F8              = 297,      // Key: F8
;;    KEY_F9              = 298,      // Key: F9
;;    KEY_F10             = 299,      // Key: F10
;;    KEY_F11             = 300,      // Key: F11
;;    KEY_F12             = 301,      // Key: F12
;;    KEY_LEFT_SHIFT      = 340,      // Key: Shift left
;;    KEY_LEFT_CONTROL    = 341,      // Key: Control left
;;    KEY_LEFT_ALT        = 342,      // Key: Alt left
;;    KEY_LEFT_SUPER      = 343,      // Key: Super left
;;    KEY_RIGHT_SHIFT     = 344,      // Key: Shift right
;;    KEY_RIGHT_CONTROL   = 345,      // Key: Control right
;;    KEY_RIGHT_ALT       = 346,      // Key: Alt right
;;    KEY_RIGHT_SUPER     = 347,      // Key: Super right
;;    KEY_KB_MENU         = 348,      // Key: KB menu
(define-constant +key-space+ 32)
(define-constant +key-escape+ 256)
(define-constant +key-enter+ 257)
(define-constant +key-tab+ 258)
(define-constant +key-backspace+ 259)
(define-constant +key-insert+ 260)
(define-constant +key-delete+ 261)
(define-constant +key-right+ 262)
(define-constant +key-left+ 263)
(define-constant +key-down+ 264)
(define-constant +key-up+ 265)
(define-constant +key-page-up+ 266)
(define-constant +key-page-down+ 267)
(define-constant +key-home+ 268)
(define-constant +key-end+ 269)
(define-constant +key-caps-lock+ 280)
(define-constant +key-scroll-lock+ 281)
(define-constant +key-num-lock+ 282)
(define-constant +key-print-screen+ 283)
(define-constant +key-pause+ 284)
(define-constant +key-f1+ 290)
(define-constant +key-f2+ 291)
(define-constant +key-f3+ 292)
(define-constant +key-f4+ 293)
(define-constant +key-f5+ 294)
(define-constant +key-f6+ 295)
(define-constant +key-f7+ 296)
(define-constant +key-f8+ 297)
(define-constant +key-f9+ 298)
(define-constant +key-f10+ 299)
(define-constant +key-f11+ 300)
(define-constant +key-f12+ 301)
(define-constant +key-left-shift+ 340)
(define-constant +key-left-control+ 341)
(define-constant +key-left-alt+ 342)
(define-constant +key-left-super+ 343)
(define-constant +key-right-shift+ 344)
(define-constant +key-right-control+ 345)
(define-constant +key-right-alt+ 346)
(define-constant +key-right-super+ 347)
(define-constant +key-kb-menu+ 348)

;;    // Keypad keys
;;    KEY_KP_0            = 320,      // Key: Keypad 0
;;    KEY_KP_1            = 321,      // Key: Keypad 1
;;    KEY_KP_2            = 322,      // Key: Keypad 2
;;    KEY_KP_3            = 323,      // Key: Keypad 3
;;    KEY_KP_4            = 324,      // Key: Keypad 4
;;    KEY_KP_5            = 325,      // Key: Keypad 5
;;    KEY_KP_6            = 326,      // Key: Keypad 6
;;    KEY_KP_7            = 327,      // Key: Keypad 7
;;    KEY_KP_8            = 328,      // Key: Keypad 8
;;    KEY_KP_9            = 329,      // Key: Keypad 9
;;    KEY_KP_DECIMAL      = 330,      // Key: Keypad .
;;    KEY_KP_DIVIDE       = 331,      // Key: Keypad /
;;    KEY_KP_MULTIPLY     = 332,      // Key: Keypad *
;;    KEY_KP_SUBTRACT     = 333,      // Key: Keypad -
;;    KEY_KP_ADD          = 334,      // Key: Keypad +
;;    KEY_KP_ENTER        = 335,      // Key: Keypad Enter
;;    KEY_KP_EQUAL        = 336,      // Key: Keypad =
;;    // Android key buttons
;;    KEY_BACK            = 4,        // Key: Android back button
;;    KEY_MENU            = 82,       // Key: Android menu button
;;    KEY_VOLUME_UP       = 24,       // Key: Android volume up button
;;    KEY_VOLUME_DOWN     = 25        // Key: Android volume down button
;;} KeyboardKey;
(define-constant +key-kp-0+ 320)
(define-constant +key-kp-1+ 321)
(define-constant +key-kp-2+ 322)
(define-constant +key-kp-3+ 323)
(define-constant +key-kp-4+ 324)
(define-constant +key-kp-5+ 325)
(define-constant +key-kp-6+ 326)
(define-constant +key-kp-7+ 327)
(define-constant +key-kp-8+ 328)
(define-constant +key-kp-9+ 329)
(define-constant +key-kp-decimal+ 330)
(define-constant +key-kp-divide+ 331)
(define-constant +key-kp-multiply+ 332)
(define-constant +key-kp-subtract+ 333)
(define-constant +key-kp-add+ 334)
(define-constant +key-kp-enter+ 335)
(define-constant +key-kp-equal+ 336)

(define-constant +key-back+ 4)
(define-constant +key-menu+ 82)
(define-constant +key-volume-up+ 24)
(define-constant +key-volume-down+ 25)

;;// Add backwards compatibility support for deprecated names
;;#define MOUSE_LEFT_BUTTON   MOUSE_BUTTON_LEFT
;;#define MOUSE_RIGHT_BUTTON  MOUSE_BUTTON_RIGHT
;;#define MOUSE_MIDDLE_BUTTON MOUSE_BUTTON_MIDDLE
;;
;;// Mouse buttons
;;typedef enum {
;;    MOUSE_BUTTON_LEFT    = 0,       // Mouse button left
;;    MOUSE_BUTTON_RIGHT   = 1,       // Mouse button right
;;    MOUSE_BUTTON_MIDDLE  = 2,       // Mouse button middle (pressed wheel)
;;    MOUSE_BUTTON_SIDE    = 3,       // Mouse button side (advanced mouse device)
;;    MOUSE_BUTTON_EXTRA   = 4,       // Mouse button extra (advanced mouse device)
;;    MOUSE_BUTTON_FORWARD = 5,       // Mouse button fordward (advanced mouse device)
;;    MOUSE_BUTTON_BACK    = 6,       // Mouse button back (advanced mouse device)
;;} MouseButton;
(define-constant +mouse-button-left+ 0)
(define-constant +mouse-button-right+ 1)
(define-constant +mouse-button-middle+ 2)
(define-constant +mouse-button-side+ 3)
(define-constant +mouse-button-extra+ 4)
(define-constant +mouse-button-forward+ 5)
(define-constant +mouse-button-back+ 6)

;;// Mouse cursor
;;typedef enum {
;;    MOUSE_CURSOR_DEFAULT       = 0,     // Default pointer shape
;;    MOUSE_CURSOR_ARROW         = 1,     // Arrow shape
;;    MOUSE_CURSOR_IBEAM         = 2,     // Text writing cursor shape
;;    MOUSE_CURSOR_CROSSHAIR     = 3,     // Cross shape
;;    MOUSE_CURSOR_POINTING_HAND = 4,     // Pointing hand cursor
;;    MOUSE_CURSOR_RESIZE_EW     = 5,     // Horizontal resize/move arrow shape
;;    MOUSE_CURSOR_RESIZE_NS     = 6,     // Vertical resize/move arrow shape
;;    MOUSE_CURSOR_RESIZE_NWSE   = 7,     // Top-left to bottom-right diagonal resize/move arrow shape
;;    MOUSE_CURSOR_RESIZE_NESW   = 8,     // The top-right to bottom-left diagonal resize/move arrow shape
;;    MOUSE_CURSOR_RESIZE_ALL    = 9,     // The omni-directional resize/move cursor shape
;;    MOUSE_CURSOR_NOT_ALLOWED   = 10     // The operation-not-allowed shape
;;} MouseCursor;
(define-constant +mouse-cursor-default+       0)
(define-constant +mouse-cursor-arrow+         1)
(define-constant +mouse-cursor-ibeam+         2)
(define-constant +mouse-cursor-crosshair+     3)
(define-constant +mouse-cursor-pointing-hand+ 4)
(define-constant +mouse-cursor-resize-ew+     5)
(define-constant +mouse-cursor-resize-ns+     6)
(define-constant +mouse-cursor-resize-nwse+   7)
(define-constant +mouse-cursor-resize-nesw+   8)
(define-constant +mouse-cursor-resize-all+    9)
(define-constant +mouse-cursor-not-allowed+   10)

;;// Gamepad buttons
;;typedef enum {
;;    GAMEPAD_BUTTON_UNKNOWN = 0,         // Unknown button, just for error checking
;;    GAMEPAD_BUTTON_LEFT_FACE_UP,        // Gamepad left DPAD up button
;;    GAMEPAD_BUTTON_LEFT_FACE_RIGHT,     // Gamepad left DPAD right button
;;    GAMEPAD_BUTTON_LEFT_FACE_DOWN,      // Gamepad left DPAD down button
;;    GAMEPAD_BUTTON_LEFT_FACE_LEFT,      // Gamepad left DPAD left button
;;    GAMEPAD_BUTTON_RIGHT_FACE_UP,       // Gamepad right button up (i.e. PS3: Triangle, Xbox: Y)
;;    GAMEPAD_BUTTON_RIGHT_FACE_RIGHT,    // Gamepad right button right (i.e. PS3: Square, Xbox: X)
;;    GAMEPAD_BUTTON_RIGHT_FACE_DOWN,     // Gamepad right button down (i.e. PS3: Cross, Xbox: A)
;;    GAMEPAD_BUTTON_RIGHT_FACE_LEFT,     // Gamepad right button left (i.e. PS3: Circle, Xbox: B)
;;    GAMEPAD_BUTTON_LEFT_TRIGGER_1,      // Gamepad top/back trigger left (first), it could be a trailing button
;;    GAMEPAD_BUTTON_LEFT_TRIGGER_2,      // Gamepad top/back trigger left (second), it could be a trailing button
;;    GAMEPAD_BUTTON_RIGHT_TRIGGER_1,     // Gamepad top/back trigger right (one), it could be a trailing button
;;    GAMEPAD_BUTTON_RIGHT_TRIGGER_2,     // Gamepad top/back trigger right (second), it could be a trailing button
;;    GAMEPAD_BUTTON_MIDDLE_LEFT,         // Gamepad center buttons, left one (i.e. PS3: Select)
;;    GAMEPAD_BUTTON_MIDDLE,              // Gamepad center buttons, middle one (i.e. PS3: PS, Xbox: XBOX)
;;    GAMEPAD_BUTTON_MIDDLE_RIGHT,        // Gamepad center buttons, right one (i.e. PS3: Start)
;;    GAMEPAD_BUTTON_LEFT_THUMB,          // Gamepad joystick pressed button left
;;    GAMEPAD_BUTTON_RIGHT_THUMB          // Gamepad joystick pressed button right
;;} GamepadButton;
(define-constant +gamepad-button-unknown+ 0)
(define-constant +gamepad-button-left-face-up+ 1)
(define-constant +gamepad-button-left-face-right+ 2)
(define-constant +gamepad-button-left-face-down+ 3)
(define-constant +gamepad-button-left-face-left+ 4)
(define-constant +gamepad-button-right-face-up+ 5)
(define-constant +gamepad-button-right-face-right+ 6)
(define-constant +gamepad-button-right-face-down+ 7)
(define-constant +gamepad-button-right-face-left+ 8)
(define-constant +gamepad-button-left-trigger-1+ 9)
(define-constant +gamepad-button-left-trigger-2+ 10)
(define-constant +gamepad-button-right-trigger-1+ 11)
(define-constant +gamepad-button-right-trigger-2+ 12)
(define-constant +gamepad-button-middle-left+ 13)
(define-constant +gamepad-button-middle+ 14)
(define-constant +gamepad-button-middle-right+ 15)
(define-constant +gamepad-button-left-thumb+ 16)
(define-constant +gamepad-button-right-thumb+ 17)

;;// Gamepad axis
;;typedef enum {
;;    GAMEPAD_AXIS_LEFT_X        = 0,     // Gamepad left stick X axis
;;    GAMEPAD_AXIS_LEFT_Y        = 1,     // Gamepad left stick Y axis
;;    GAMEPAD_AXIS_RIGHT_X       = 2,     // Gamepad right stick X axis
;;    GAMEPAD_AXIS_RIGHT_Y       = 3,     // Gamepad right stick Y axis
;;    GAMEPAD_AXIS_LEFT_TRIGGER  = 4,     // Gamepad back trigger left, pressure level: [1..-1]
;;    GAMEPAD_AXIS_RIGHT_TRIGGER = 5      // Gamepad back trigger right, pressure level: [1..-1]
;;} GamepadAxis;
(define-constant +gamepad-axis-left-x+ 0)
(define-constant +gamepad-axis-left-y+ 1)
(define-constant +gamepad-axis-right-x+ 2)
(define-constant +gamepad-axis-right-y+ 3)
(define-constant +gamepad-axis-left-trigger+ 4)
(define-constant +gamepad-axis-right-trigger+ 5)

;;// Material map index
;;typedef enum {
;;    MATERIAL_MAP_ALBEDO    = 0,     // Albedo material (same as: MATERIAL_MAP_DIFFUSE)
;;    MATERIAL_MAP_METALNESS,         // Metalness material (same as: MATERIAL_MAP_SPECULAR)
;;    MATERIAL_MAP_NORMAL,            // Normal material
;;    MATERIAL_MAP_ROUGHNESS,         // Roughness material
;;    MATERIAL_MAP_OCCLUSION,         // Ambient occlusion material
;;    MATERIAL_MAP_EMISSION,          // Emission material
;;    MATERIAL_MAP_HEIGHT,            // Heightmap material
;;    MATERIAL_MAP_CUBEMAP,           // Cubemap material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
;;    MATERIAL_MAP_IRRADIANCE,        // Irradiance material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
;;    MATERIAL_MAP_PREFILTER,         // Prefilter material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
;;    MATERIAL_MAP_BRDF               // Brdf material
;;} MaterialMapIndex;
(define-constant +material-map-albedo+ 0)
(define-constant +material-map-metalness+ 1)
(define-constant +material-map-normal+ 2)
(define-constant +material-map-roughness+ 3)
(define-constant +material-map-occlusion+ 4)
(define-constant +material-map-emission+ 5)
(define-constant +material-map-height+ 6)
(define-constant +material-map-cubemap+ 7)
(define-constant +material-map-irradiance+ 8)
(define-constant +material-map-prefilter+ 9)
(define-constant +material-map-brdf+ 10)

;;// Shader location index
;;typedef enum {
;;    SHADER_LOC_VERTEX_POSITION = 0, // Shader location: vertex attribute: position
;;    SHADER_LOC_VERTEX_TEXCOORD01,   // Shader location: vertex attribute: texcoord01
;;    SHADER_LOC_VERTEX_TEXCOORD02,   // Shader location: vertex attribute: texcoord02
;;    SHADER_LOC_VERTEX_NORMAL,       // Shader location: vertex attribute: normal
;;    SHADER_LOC_VERTEX_TANGENT,      // Shader location: vertex attribute: tangent
;;    SHADER_LOC_VERTEX_COLOR,        // Shader location: vertex attribute: color
;;    SHADER_LOC_MATRIX_MVP,          // Shader location: matrix uniform: model-view-projection
;;    SHADER_LOC_MATRIX_VIEW,         // Shader location: matrix uniform: view (camera transform)
;;    SHADER_LOC_MATRIX_PROJECTION,   // Shader location: matrix uniform: projection
;;    SHADER_LOC_MATRIX_MODEL,        // Shader location: matrix uniform: model (transform)
;;    SHADER_LOC_MATRIX_NORMAL,       // Shader location: matrix uniform: normal
;;    SHADER_LOC_VECTOR_VIEW,         // Shader location: vector uniform: view
;;    SHADER_LOC_COLOR_DIFFUSE,       // Shader location: vector uniform: diffuse color
;;    SHADER_LOC_COLOR_SPECULAR,      // Shader location: vector uniform: specular color
;;    SHADER_LOC_COLOR_AMBIENT,       // Shader location: vector uniform: ambient color
;;    SHADER_LOC_MAP_ALBEDO,          // Shader location: sampler2d texture: albedo (same as: SHADER_LOC_MAP_DIFFUSE)
;;    SHADER_LOC_MAP_METALNESS,       // Shader location: sampler2d texture: metalness (same as: SHADER_LOC_MAP_SPECULAR)
;;    SHADER_LOC_MAP_NORMAL,          // Shader location: sampler2d texture: normal
;;    SHADER_LOC_MAP_ROUGHNESS,       // Shader location: sampler2d texture: roughness
;;    SHADER_LOC_MAP_OCCLUSION,       // Shader location: sampler2d texture: occlusion
;;    SHADER_LOC_MAP_EMISSION,        // Shader location: sampler2d texture: emission
;;    SHADER_LOC_MAP_HEIGHT,          // Shader location: sampler2d texture: height
;;    SHADER_LOC_MAP_CUBEMAP,         // Shader location: samplerCube texture: cubemap
;;    SHADER_LOC_MAP_IRRADIANCE,      // Shader location: samplerCube texture: irradiance
;;    SHADER_LOC_MAP_PREFILTER,       // Shader location: samplerCube texture: prefilter
;;    SHADER_LOC_MAP_BRDF             // Shader location: sampler2d texture: brdf
;;} ShaderLocationIndex;
(define-constant +shader-loc-vertex-position+ 0)
(define-constant +shader-loc-vertex-texcoord01+ 1)
(define-constant +shader-loc-vertex-texcoord02+ 2)
(define-constant +shader-loc-vertex-normal+ 3)
(define-constant +shader-loc-vertex-tangent+ 4)
(define-constant +shader-loc-vertex-color+ 5)
(define-constant +shader-loc-matrix-mvp+ 6)
(define-constant +shader-loc-matrix-view+ 7)
(define-constant +shader-loc-matrix-projection+ 8)
(define-constant +shader-loc-matrix-model+ 9)
(define-constant +shader-loc-matrix-normal+ 10)
(define-constant +shader-loc-vector-view+ 11)
(define-constant +shader-loc-color-diffuse+ 12)
(define-constant +shader-loc-color-specular+ 13)
(define-constant +shader-loc-color-ambient+ 14)
(define-constant +shader-loc-map-albedo+ 15)
(define-constant +shader-loc-map-metalness+ 16)
(define-constant +shader-loc-map-normal+ 17)
(define-constant +shader-loc-map-roughness+ 18)
(define-constant +shader-loc-map-occlusion+ 19)
(define-constant +shader-loc-map-emission+ 20)
(define-constant +shader-loc-map-height+ 21)
(define-constant +shader-loc-map-cubemap+ 22)
(define-constant +shader-loc-map-irradiance+ 23)
(define-constant +shader-loc-map-prefilter+ 24)
(define-constant +shader-loc-map-brdf+ 25)

;;#define SHADER_LOC_MAP_DIFFUSE      SHADER_LOC_MAP_ALBEDO
;;#define SHADER_LOC_MAP_SPECULAR     SHADER_LOC_MAP_METALNESS
;;
;;// Shader uniform data type
;;typedef enum {
;;    SHADER_UNIFORM_FLOAT = 0,       // Shader uniform type: float
;;    SHADER_UNIFORM_VEC2,            // Shader uniform type: vec2 (2 float)
;;    SHADER_UNIFORM_VEC3,            // Shader uniform type: vec3 (3 float)
;;    SHADER_UNIFORM_VEC4,            // Shader uniform type: vec4 (4 float)
;;    SHADER_UNIFORM_INT,             // Shader uniform type: int
;;    SHADER_UNIFORM_IVEC2,           // Shader uniform type: ivec2 (2 int)
;;    SHADER_UNIFORM_IVEC3,           // Shader uniform type: ivec3 (3 int)
;;    SHADER_UNIFORM_IVEC4,           // Shader uniform type: ivec4 (4 int)
;;    SHADER_UNIFORM_SAMPLER2D        // Shader uniform type: sampler2d
;;} ShaderUniformDataType;
(define-constant +shader-uniform-float+ 0)
(define-constant +shader-uniform-vec2+ 1)
(define-constant +shader-uniform-vec3+ 2)
(define-constant +shader-uniform-vec4+ 3)
(define-constant +shader-uniform-int+ 4)
(define-constant +shader-uniform-ivec2+ 5)
(define-constant +shader-uniform-ivec3+ 6)
(define-constant +shader-uniform-ivec4+ 7)
(define-constant +shader-uniform-sampler2d+ 8)

;;// Shader attribute data types
;;typedef enum {
;;    SHADER_ATTRIB_FLOAT = 0,        // Shader attribute type: float
;;    SHADER_ATTRIB_VEC2,             // Shader attribute type: vec2 (2 float)
;;    SHADER_ATTRIB_VEC3,             // Shader attribute type: vec3 (3 float)
;;    SHADER_ATTRIB_VEC4              // Shader attribute type: vec4 (4 float)
;;} ShaderAttributeDataType;
(define-constant +shader-attrib-float+ 0)
(define-constant +shader-attrib-vec2+ 1)
(define-constant +shader-attrib-vec3+ 2)
(define-constant +shader-attrib-vec4+ 3)

;;// Pixel formats
;;// NOTE: Support depends on OpenGL version and platform
;;typedef enum {
;;    PIXELFORMAT_UNCOMPRESSED_GRAYSCALE = 1, // 8 bit per pixel (no alpha)
;;    PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA,    // 8*2 bpp (2 channels)
;;    PIXELFORMAT_UNCOMPRESSED_R5G6B5,        // 16 bpp
;;    PIXELFORMAT_UNCOMPRESSED_R8G8B8,        // 24 bpp
;;    PIXELFORMAT_UNCOMPRESSED_R5G5B5A1,      // 16 bpp (1 bit alpha)
;;    PIXELFORMAT_UNCOMPRESSED_R4G4B4A4,      // 16 bpp (4 bit alpha)
;;    PIXELFORMAT_UNCOMPRESSED_R8G8B8A8,      // 32 bpp
;;    PIXELFORMAT_UNCOMPRESSED_R32,           // 32 bpp (1 channel - float)
;;    PIXELFORMAT_UNCOMPRESSED_R32G32B32,     // 32*3 bpp (3 channels - float)
;;    PIXELFORMAT_UNCOMPRESSED_R32G32B32A32,  // 32*4 bpp (4 channels - float)
;;    PIXELFORMAT_COMPRESSED_DXT1_RGB,        // 4 bpp (no alpha)
;;    PIXELFORMAT_COMPRESSED_DXT1_RGBA,       // 4 bpp (1 bit alpha)
;;    PIXELFORMAT_COMPRESSED_DXT3_RGBA,       // 8 bpp
;;    PIXELFORMAT_COMPRESSED_DXT5_RGBA,       // 8 bpp
;;    PIXELFORMAT_COMPRESSED_ETC1_RGB,        // 4 bpp
;;    PIXELFORMAT_COMPRESSED_ETC2_RGB,        // 4 bpp
;;    PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA,   // 8 bpp
;;    PIXELFORMAT_COMPRESSED_PVRT_RGB,        // 4 bpp
;;    PIXELFORMAT_COMPRESSED_PVRT_RGBA,       // 4 bpp
;;    PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA,   // 8 bpp
;;    PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA    // 2 bpp
;;} PixelFormat;
(define-constant +pixelformat-uncompressed-grayscale+ 1)
(define-constant +pixelformat-uncompressed-gray-alpha+ 2)
(define-constant +pixelformat-uncompressed-r5g6b5+ 3)
(define-constant +pixelformat-uncompressed-r8g8b8+ 4)
(define-constant +pixelformat-uncompressed-r5g5b5a1+ 5)
(define-constant +pixelformat-uncompressed-r4g4b4a4+ 6)
(define-constant +pixelformat-uncompressed-r8g8b8a8+ 7)
(define-constant +pixelformat-uncompressed-r32+ 8)
(define-constant +pixelformat-uncompressed-r32g32b32+ 9)
(define-constant +pixelformat-uncompressed-r32g32b32a32+ 10)
(define-constant +pixelformat-compressed-dxt1-rgb+ 11)
(define-constant +pixelformat-compressed-dxt1-rgba+ 12)
(define-constant +pixelformat-compressed-dxt3-rgba+ 13)
(define-constant +pixelformat-compressed-dxt5-rgba+ 14)
(define-constant +pixelformat-compressed-etc1-rgb+ 15)
(define-constant +pixelformat-compressed-etc2-rgb+ 16)
(define-constant +pixelformat-compressed-etc2-eac-rgba+ 17)
(define-constant +pixelformat-compressed-pvrt-rgb+ 18)
(define-constant +pixelformat-compressed-pvrt-rgba+ 19)
(define-constant +pixelformat-compressed-astc-4x4-rgba+ 20)
(define-constant +pixelformat-compressed-astc-8x8-rgba+ 21)

;;// Texture parameters: filter mode
;;// NOTE 1: Filtering considers mipmaps if available in the texture
;;// NOTE 2: Filter is accordingly set for minification and magnification
;;typedef enum {
;;    TEXTURE_FILTER_POINT = 0,               // No filter, just pixel aproximation
;;    TEXTURE_FILTER_BILINEAR,                // Linear filtering
;;    TEXTURE_FILTER_TRILINEAR,               // Trilinear filtering (linear with mipmaps)
;;    TEXTURE_FILTER_ANISOTROPIC_4X,          // Anisotropic filtering 4x
;;    TEXTURE_FILTER_ANISOTROPIC_8X,          // Anisotropic filtering 8x
;;    TEXTURE_FILTER_ANISOTROPIC_16X,         // Anisotropic filtering 16x
;;} TextureFilter;
(define-constant +texture-filter-point+ 0)
(define-constant +texture-filter-bilinear+ 1)
(define-constant +texture-filter-trilinear+ 2)
(define-constant +texture-filter-anisotropic-4x+ 3)
(define-constant +texture-filter-anisotropic-8x+ 4)
(define-constant +texture-filter-anisotropic-16x+ 5)

;;// Texture parameters: wrap mode
;;typedef enum {
;;    TEXTURE_WRAP_REPEAT = 0,                // Repeats texture in tiled mode
;;    TEXTURE_WRAP_CLAMP,                     // Clamps texture to edge pixel in tiled mode
;;    TEXTURE_WRAP_MIRROR_REPEAT,             // Mirrors and repeats the texture in tiled mode
;;    TEXTURE_WRAP_MIRROR_CLAMP               // Mirrors and clamps to border the texture in tiled mode
;;} TextureWrap;
(define-constant +texture-wrap-repeat+ 0)
(define-constant +texture-wrap-clamp+ 1)
(define-constant +texture-wrap-mirror-repeat+ 2)
(define-constant +texture-wrap-mirror-clamp+ 3)

;;// Cubemap layouts
;;typedef enum {
;;    CUBEMAP_LAYOUT_AUTO_DETECT = 0,         // Automatically detect layout type
;;    CUBEMAP_LAYOUT_LINE_VERTICAL,           // Layout is defined by a vertical line with faces
;;    CUBEMAP_LAYOUT_LINE_HORIZONTAL,         // Layout is defined by an horizontal line with faces
;;    CUBEMAP_LAYOUT_CROSS_THREE_BY_FOUR,     // Layout is defined by a 3x4 cross with cubemap faces
;;    CUBEMAP_LAYOUT_CROSS_FOUR_BY_THREE,     // Layout is defined by a 4x3 cross with cubemap faces
;;    CUBEMAP_LAYOUT_PANORAMA                 // Layout is defined by a panorama image (equirectangular map)
;;} CubemapLayout;
(define-constant +cubemap-layout-auto-detect+ 0)
(define-constant +cubemap-layout-line-vertical+ 1)
(define-constant +cubemap-layout-line-horizontal+ 2)
(define-constant +cubemap-layout-cross-three-by-four+ 3)
(define-constant +cubemap-layout-cross-four-by-three+ 4)
(define-constant +cubemap-layout-panorama+ 5)

;;// Font type, defines generation method
;;typedef enum {
;;    FONT_DEFAULT = 0,               // Default font generation, anti-aliased
;;    FONT_BITMAP,                    // Bitmap font generation, no anti-aliasing
;;    FONT_SDF                        // SDF font generation, requires external shader
;;} FontType;
(define-constant +font-default+ 0)
(define-constant +font-bitmap+ 1)
(define-constant +font-sdf+ 2)

;;// Color blending modes (pre-defined)
;;typedef enum {
;;    BLEND_ALPHA = 0,                // Blend textures considering alpha (default)
;;    BLEND_ADDITIVE,                 // Blend textures adding colors
;;    BLEND_MULTIPLIED,               // Blend textures multiplying colors
;;    BLEND_ADD_COLORS,               // Blend textures adding colors (alternative)
;;    BLEND_SUBTRACT_COLORS,          // Blend textures subtracting colors (alternative)
;;    BLEND_CUSTOM                    // Belnd textures using custom src/dst factors (use rlSetBlendMode())
;;} BlendMode;
(define-constant +blend-alpha+ 0)
(define-constant +blend-additive+ 1)
(define-constant +blend-multiplied+ 2)
(define-constant +blend-add-colors+ 3)
(define-constant +blend-subtract-colors+ 4)
(define-constant +blend-custom+ 5)

;;// Gesture
;;// NOTE: It could be used as flags to enable only some gestures
;;typedef enum {
;;    GESTURE_NONE        = 0,        // No gesture
;;    GESTURE_TAP         = 1,        // Tap gesture
;;    GESTURE_DOUBLETAP   = 2,        // Double tap gesture
;;    GESTURE_HOLD        = 4,        // Hold gesture
;;    GESTURE_DRAG        = 8,        // Drag gesture
;;    GESTURE_SWIPE_RIGHT = 16,       // Swipe right gesture
;;    GESTURE_SWIPE_LEFT  = 32,       // Swipe left gesture
;;    GESTURE_SWIPE_UP    = 64,       // Swipe up gesture
;;    GESTURE_SWIPE_DOWN  = 128,      // Swipe down gesture
;;    GESTURE_PINCH_IN    = 256,      // Pinch in gesture
;;    GESTURE_PINCH_OUT   = 512       // Pinch out gesture
;;} Gesture;
(define-constant +gesture-none+ 0)
(define-constant +gesture-tap+ 1)
(define-constant +gesture-doubletap+ 2)
(define-constant +gesture-hold+ 4)
(define-constant +gesture-drag+ 8)
(define-constant +gesture-swipe-right+ 16)
(define-constant +gesture-swipe-left+ 32)
(define-constant +gesture-swipe-up+ 64)
(define-constant +gesture-swipe-down+ 128)
(define-constant +gesture-pinch-in+ 256)
(define-constant +gesture-pinch-out+ 512)

;;// Camera system modes
;;typedef enum {
;;    CAMERA_CUSTOM = 0,              // Custom camera
;;    CAMERA_FREE,                    // Free camera
;;    CAMERA_ORBITAL,                 // Orbital camera
;;    CAMERA_FIRST_PERSON,            // First person camera
;;    CAMERA_THIRD_PERSON             // Third person camera
;;} CameraMode;
(define-constant +camera-custom+ 0)
(define-constant +camera-free+ 1)
(define-constant +camera-orbital+ 2)
(define-constant +camera-first-person+ 3)
(define-constant +camera-third-person+ 4)

;;// Camera projection
;;typedef enum {
;;    CAMERA_PERSPECTIVE = 0,         // Perspective projection
;;    CAMERA_ORTHOGRAPHIC             // Orthographic projection
;;} CameraProjection;
(define-constant +camera-perspective+ 0)
(define-constant +camera-orthographic+ 1)

;;// N-patch layout
;;typedef enum {
;;    NPATCH_NINE_PATCH = 0,          // Npatch layout: 3x3 tiles
;;    NPATCH_THREE_PATCH_VERTICAL,    // Npatch layout: 1x3 tiles
;;    NPATCH_THREE_PATCH_HORIZONTAL   // Npatch layout: 3x1 tiles
;;} NPatchLayout;
(define-constant +npatch-nine-patch+ 0)
(define-constant +npatch-three-patch-vertical+ 1)
(define-constant +npatch-three-patch-horizontal+ 2)

;;// Callbacks to hook some internal functions
;;// WARNING: This callbacks are intended for advance users
;;typedef void (*TraceLogCallback)(int logLevel, const char *text, va_list args);  // Logging: Redirect trace log messages
;;typedef unsigned char *(*LoadFileDataCallback)(const char *fileName, unsigned int *bytesRead);      // FileIO: Load binary data
;;typedef bool (*SaveFileDataCallback)(const char *fileName, void *data, unsigned int bytesToWrite);  // FileIO: Save binary data
;;typedef char *(*LoadFileTextCallback)(const char *fileName);       // FileIO: Load text data
;;typedef bool (*SaveFileTextCallback)(const char *fileName, char *text);     // FileIO: Save text data
;;
;;//------------------------------------------------------------------------------------
;;// Global Variables Definition
;;//------------------------------------------------------------------------------------
;;// It's lonely here...
;;
;;//------------------------------------------------------------------------------------
;;// Window and Graphics Device Functions (Module: core)
;;//------------------------------------------------------------------------------------
;;
;;#if defined(__cplusplus)
;;extern "C" {            // Prevents name mangling of functions
;;#endif
;;
;;// Window-related functions
;;RLAPI void InitWindow(int width, int height, const char *title);  // Initialize window and OpenGL context
(defcfun "InitWindow" :void
 "Initialize window and OpenGL context"
 (width :int)
 (height :int)
 (title :string))

;;RLAPI bool WindowShouldClose(void);                               // Check if KEY_ESCAPE pressed or Close icon pressed
(defcfun "WindowShouldClose" :bool
 "Check if KEY_ESCAPE pressed or Close icon pressed")

;;RLAPI void CloseWindow(void);                                     // Close window and unload OpenGL context
(defcfun "CloseWindow" :void
 "Close window and unload OpenGL context")

;;RLAPI bool IsWindowReady(void);                                   // Check if window has been initialized successfully
(defcfun "IsWindowReady" :boolean
 "Check if window has been initialized successfully")

;;RLAPI bool IsWindowFullscreen(void);                              // Check if window is currently fullscreen
(defcfun "IsWindowFullscreen" :boolean
 "Check if window is currently fullscreen")

;;RLAPI bool IsWindowHidden(void);                                  // Check if window is currently hidden (only PLATFORM_DESKTOP)
(defcfun "IsWindowHidden" :boolean
 "Check if window is currently hidden")
 
;;RLAPI bool IsWindowMinimized(void);                               // Check if window is currently minimized (only PLATFORM_DESKTOP)
(defcfun "IsWindowMinimized" :boolean
 "Check if window is currently minimized")

;;RLAPI bool IsWindowMaximized(void);                               // Check if window is currently maximized (only PLATFORM_DESKTOP)
(defcfun "IsWindowMaximized" :boolean
   "Check if window is currently maximized")

;;RLAPI bool IsWindowFocused(void);                                 // Check if window is currently focused (only PLATFORM_DESKTOP)
(defcfun "IsWindowFocused" :boolean
 "Check if window is currently focused")

;;RLAPI bool IsWindowResized(void);                                 // Check if window has been resized last frame
(defcfun "IsWindowResized" :boolean
 "Check if window has been resized last frame")

;;RLAPI bool IsWindowState(unsigned int flag);                      // Check if one specific window flag is enabled
(defcfun "IsWindowState" :boolean
  (flag :unsigned-int))

;;RLAPI void SetWindowState(unsigned int flags);                    // Set window configuration state using flags
(defcfun "SetWindowState" :void
  (flags :unsigned-int))

;;RLAPI void ClearWindowState(unsigned int flags);                  // Clear window configuration state flags
(defcfun "ClearWindowState" :void
  (flags :unsigned-int))
 
;;RLAPI void ToggleFullscreen(void);                                // Toggle window state: fullscreen/windowed (only PLATFORM_DESKTOP)
(defcfun "ToggleFullscreen" :void
 "Toggle window state: fullscreen/windowed")

;;RLAPI void MaximizeWindow(void);                                  // Set window state: maximized, if resizable (only PLATFORM_DESKTOP)
(defcfun "MaximizeWindow" :void
  "Set window state: maximized, if resizable")

;;RLAPI void MinimizeWindow(void);                                  // Set window state: minimized, if resizable (only PLATFORM_DESKTOP)
(defcfun "MinimizeWindow" :void
  "Set window state: minimized, if resizable")

;;RLAPI void RestoreWindow(void);                                   // Set window state: not minimized/maximized (only PLATFORM_DESKTOP)
(defcfun "RestoreWindow" :void
  "Set window state: not minimized/maximized")

;;RLAPI void SetWindowIcon(Image image);                            // Set icon for window (only PLATFORM_DESKTOP)
(defcfun "SetWindowIcon" :void
 "Set icon for window (only PLATFORM_DESKTOP)"
 (image (:struct %image)))

;;RLAPI void SetWindowTitle(const char *title);                     // Set title for window (only PLATFORM_DESKTOP)
(defcfun "SetWindowTitle" :void
 "Set title for window (only PLATFORM_DESKTOP)"
 (title :string))

;;RLAPI void SetWindowPosition(int x, int y);                       // Set window position on screen (only PLATFORM_DESKTOP)
(defcfun "SetWindowPosition" :void
 "Set window position on screen (only PLATFORM_DESKTOP)"
 (x :int)
 (y :int))

;;RLAPI void SetWindowMonitor(int monitor);                         // Set monitor for the current window (fullscreen mode)
(defcfun "SetWindowMonitor" :void
 "Set monitor for the current window (fullscreen mode)"
 (monitor :int))

;;RLAPI void SetWindowMinSize(int width, int height);               // Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
(defcfun "SetWindowMinSize" :void
 "Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)"
 (width :int)
 (height :int))

;;RLAPI void SetWindowSize(int width, int height);                  // Set window dimensions
(defcfun "SetWindowSize" :void
 "Set window dimensions"
 (width :int)
 (height :int))

;;RLAPI void *GetWindowHandle(void);                                // Get native window handle
(defcfun "GetWindowHandle" :pointer
 "Get native window handle")

;;RLAPI int GetScreenWidth(void);                                   // Get current screen width
(defcfun "GetScreenWidth" :int
 "Get current screen width")

;;RLAPI int GetScreenHeight(void);                                  // Get current screen height
(defcfun "GetScreenHeight" :int
 "Get current screen height")

;;RLAPI int GetMonitorCount(void);                                  // Get number of connected monitors
(defcfun "GetMonitorCount" :int
 "Get number of connected monitors")

;;RLAPI int GetCurrentMonitor(void);                                // Get current connected monitor
(defcfun "GetCurrentMonitor" :int
  "Get current connected monitor")

;;RLAPI Vector2 GetMonitorPosition(int monitor);                    // Get specified monitor position
(defcfun "GetMonitorPosition" (:struct %vector2)
  "Get specified monitor position"
  (monitor :int))

;;RLAPI int GetMonitorWidth(int monitor);                           // Get specified monitor width (max available by monitor)
(defcfun "GetMonitorWidth" :int
 "Get specified monitor width"
 (monitor :int))

;;RLAPI int GetMonitorHeight(int monitor);                          // Get specified monitor height (max available by monitor)
(defcfun "GetMonitorHeight" :int
 "Get specified monitor height"
 (monitor :int))

;;RLAPI int GetMonitorPhysicalWidth(int monitor);                   // Get specified monitor physical width in millimetres
(defcfun "GetMonitorPhysicalWidth" :int
 "Get specified monitor physical width in millimetres"
 (monitor :int))

;;RLAPI int GetMonitorPhysicalHeight(int monitor);                  // Get specified monitor physical height in millimetres
(defcfun "GetMonitorPhysicalHeight" :int
 "Get specified monitor physical height in millimetres"
 (monitor :int))

;;RLAPI int GetMonitorRefreshRate(int monitor);                     // Get specified monitor refresh rate
(defcfun "GetMonitorRefreshRate" :int
  "Get specified monitor refresh rate"
  (monitor :int))
 
;;RLAPI Vector2 GetWindowPosition(void);                            // Get window position XY on monitor
(defcfun "GetWindowPosition" (:struct %vector2)
 "Get window position XY on monitor")

;;RLAPI Vector2 GetWindowScaleDPI(void);                            // Get window scale DPI factor
(defcfun "GetWindowScaleDPI" (:struct %vector2)
  "Get window scale DPI factor")

;;RLAPI const char *GetMonitorName(int monitor);                    // Get the human-readable, UTF-8 encoded name of the primary monitor
(defcfun "GetMonitorName" :string
 "Get the human-readable, UTF-8 encoded name of the primary monitor"
 (monitor :int))

;;RLAPI void SetClipboardText(const char *text);                    // Set clipboard text content
(defcfun "SetClipboardText" :void
 "Set clipboard text content"
 (text :string))

;;RLAPI const char *GetClipboardText(void);                         // Get clipboard text content
(defcfun "GetClipboardText" :string
 "Get clipboard text content")

;;// Custom frame control functions
;;// NOTE: Those functions are intended for advance users that want full control over the frame processing
;;// By default EndDrawing() does this job: draws everything + SwapScreenBuffer() + manage frame timming + PollInputEvents()
;;// To avoid that behaviour and control frame processes manually, enable in config.h: SUPPORT_CUSTOM_FRAME_CONTROL
;;RLAPI void SwapScreenBuffer(void);                                // Swap back buffer with front buffer (screen drawing)
(defcfun "SwapScreenBuffer" :void
  "Swap back buffer with front buffer")

;;RLAPI void PollInputEvents(void);                                 // Register all input events
(defcfun "PollInputEvents" :void
  "Register all input events")

;;RLAPI void WaitTime(float ms);                                    // Wait for some milliseconds (halt program execution)
(defcfun "WaitTime" :void
  "Wait for some milliseconds"
  (ms :float))

;;// Cursor-related functions
;;RLAPI void ShowCursor(void);                                      // Shows cursor
(defcfun "ShowCursor" :void
 "Shows cursor")

;;RLAPI void HideCursor(void);                                      // Hides cursor
(defcfun "HideCursor" :void
 "Hides cursor")

;;RLAPI bool IsCursorHidden(void);                                  // Check if cursor is not visible
(defcfun "IsCursorHidden" :boolean
 "Check if cursor is not visible")

;;RLAPI void EnableCursor(void);                                    // Enables cursor (unlock cursor)
(defcfun "EnableCursor" :void
 "Enables cursor (unlock cursor)")

;;RLAPI void DisableCursor(void);                                   // Disables cursor (lock cursor)
(defcfun "DisableCursor" :void
 "Disables cursor (lock cursor)")

;;RLAPI bool IsCursorOnScreen(void);                                // Check if cursor is on the screen
(defcfun "IsCursorOnScreen" :boolean
  "Check if cursor is on the screen")

;;// Drawing-related functions
;;RLAPI void ClearBackground(Color color);                          // Set background color (framebuffer clear color)
(defcfun "ClearBackground" :void
 "Set background color (framebuffer clear color)"
 (color :uint))

;;RLAPI void BeginDrawing(void);                                    // Setup canvas (framebuffer) to start drawing
(defcfun "BeginDrawing" :void
 "Setup canvas (framebuffer) to start drawing")

;;RLAPI void EndDrawing(void);                                      // End canvas drawing and swap buffers (double buffering)
(defcfun "EndDrawing" :void
 "End canvas drawing and swap buffers (double buffering)")

;;RLAPI void BeginMode2D(Camera2D camera);                          // Begin 2D mode with custom camera (2D)
(defcfun "BeginMode2D" :void
 "Initialize 2D mode with custom camera (2D)"
 (camera (:struct %camera2d)))

;;RLAPI void EndMode2D(void);                                       // Ends 2D mode with custom camera
(defcfun "EndMode2D" :void
 "Ends 2D mode with custom camera")

;;RLAPI void BeginMode3D(Camera3D camera);                          // Begin 3D mode with custom camera (3D)
(defcfun "BeginMode3D" :void
 "Initializes 3D mode with custom camera (3D)"
 (camera (:struct %camera3d)))

;;RLAPI void EndMode3D(void);                                       // Ends 3D mode and returns to default 2D orthographic mode
(defcfun "EndMode3D" :void
 "Ends 3D mode and returns to default 2D orthographic mode")

;;RLAPI void BeginTextureMode(RenderTexture2D target);              // Begin drawing to render texture
(defcfun "BeginTextureMode" :void
 "Initializes render texture for drawing"
 (target (:struct %render-texture)))

;;RLAPI void EndTextureMode(void);                                  // Ends drawing to render texture
(defcfun "EndTextureMode" :void
 "Ends drawing to render texture")

;;RLAPI void BeginShaderMode(Shader shader);                        // Begin custom shader drawing
(defcfun "BeginShaderMode" :void
  "Begin custom shader drawing"
  (shader (:struct %shader)))

;;RLAPI void EndShaderMode(void);                                   // End custom shader drawing (use default shader)
(defcfun "EndShaderMode" :void
  "End custom shader drawing (use default shader)")
 
;;RLAPI void BeginBlendMode(int mode);                              // Begin blending mode (alpha, additive, multiplied, subtract, custom)
(defcfun "BeginBlendMode" :void
  "Begin blending mode"
  (mode :int))

;;RLAPI void EndBlendMode(void);                                    // End blending mode (reset to default: alpha blending)
(defcfun "EndBlendMode" :void
  "End blending mode")

;;RLAPI void BeginScissorMode(int x, int y, int width, int height); // Begin scissor mode (define screen area for following drawing)
(defcfun "BeginScissorMode" :void
 "Begin scissor mode (define screen area for following drawing)"
 (x :int)
 (y :int)
 (width :int)
 (height :int))

;;RLAPI void EndScissorMode(void);                                  // End scissor mode
(defcfun "EndScissorMode" :void
 "End scissor mode")

;;RLAPI void BeginVrStereoMode(VrStereoConfig config);              // Begin stereo rendering (requires VR simulator)
(defcfun "BeginVrStereoMode" :void
  "Begin stereo rendering (requires VR simulator)"
  (config (:struct %vr-stereo-config)))

;;RLAPI void EndVrStereoMode(void);                                 // End stereo rendering (requires VR simulator)
(defcfun "EndVrStereoMode" :void
  "End stereo rendering (requires VR simulator)")

;;// VR stereo config functions for VR simulator
;;RLAPI VrStereoConfig LoadVrStereoConfig(VrDeviceInfo device);     // Load VR stereo config for VR simulator device parameters
(defcfun "LoadVrStereoConfig" (:struct %vr-stereo-config)
  "Load VR stereo config for VR simulator device parameters"
  (device (:struct %vr-device-info)))

;;RLAPI void UnloadVrStereoConfig(VrStereoConfig config);           // Unload VR stereo config
(defcfun "UnloadVrStereoConfig" :void
  "Unload VR stereo config"
  (config (:struct %vr-stereo-config)))

;;// Shader management functions
;;// NOTE: Shader functionality is not available on OpenGL 1.1
;;RLAPI Shader LoadShader(const char *vsFileName, const char *fsFileName);   // Load shader from files and bind default locations
(defcfun "LoadShader" (:struct %shader)
  (vs-file-name :string)
  (fs-file-name :string))

;;RLAPI Shader LoadShaderFromMemory(const char *vsCode, const char *fsCode); // Load shader from code strings and bind default locations
(defcfun "LoadShaderFromMemory" (:struct %shader)
 "Load shader from code strings and bind default locations"
 (vs-code :string)
 (fs-code :string))

;;RLAPI int GetShaderLocation(Shader shader, const char *uniformName);       // Get shader uniform location
(defcfun "GetShaderLocation" :int
  "Get shader uniform location"
  (shader (:struct %shader))
  (uniform-name :string))

;;RLAPI int GetShaderLocationAttrib(Shader shader, const char *attribName);  // Get shader attribute location
(defcfun "GetShaderLocationAttrib" :int
  "Get shader attribute location"
  (shader (:struct %shader))
  (attrib-name :string))

;;RLAPI void SetShaderValue(Shader shader, int locIndex, const void *value, int uniformType);               // Set shader uniform value
(defcfun "SetShaderValue" :void
  (shader (:struct %shader))
  (loc-index :int)
  (value :pointer)
  (uniform-type :int))

;;RLAPI void SetShaderValueV(Shader shader, int locIndex, const void *value, int uniformType, int count);   // Set shader uniform value vector
(defcfun "SetShaderValueV" :void
  (shader (:struct %shader))
  (loc-index :int)
  (value :pointer)
  (uniform-type :int)
  (count :int))

;;RLAPI void SetShaderValueMatrix(Shader shader, int locIndex, Matrix mat);         // Set shader uniform value (matrix 4x4)
(defcfun "SetShaderValueMatrix" :void
  (shader (:struct %shader))
  (loc-index :int)
  (mat (:struct %matrix)))

;;RLAPI void SetShaderValueTexture(Shader shader, int locIndex, Texture2D texture); // Set shader uniform value for texture (sampler2d)
(defcfun "SetShaderValueTexture" :void
  (shader (:struct %shader))
  (loc-index :int)
  (texture (:struct %texture)))

;;RLAPI void UnloadShader(Shader shader);                                    // Unload shader from GPU memory (VRAM)
(defcfun "UnloadShader" :void
  (shader (:struct %shader)))

;;// Screen-space-related functions
;;RLAPI Ray GetMouseRay(Vector2 mousePosition, Camera camera);      // Get a ray trace from mouse position
(defcfun "GetMouseRay" (:struct %ray)
 "Get a ray trace from mouse position"
 (mouse-position (:struct %vector2))
 (camera (:struct %camera3d)))
 
;;RLAPI Matrix GetCameraMatrix(Camera camera);                      // Get camera transform matrix (view matrix)
(defcfun "GetCameraMatrix" (:struct %matrix)
 "Get camera transform matrix (view matrix)"
 (camera (:struct %camera3d)))

;;RLAPI Matrix GetCameraMatrix2D(Camera2D camera);                  // Get camera 2d transform matrix
(defcfun "GetCameraMatrix2D" (:struct %matrix)
 "Get camera 2d transform matrix"
 (camera (:struct %camera2d)))

;;RLAPI Vector2 GetWorldToScreen(Vector3 position, Camera camera);  // Get the screen space position for a 3d world space position
(defcfun "GetWorldToScreen" (:struct %vector2)
 "Get the screen space position for a 3d world space position"
 (position (:struct %vector3))
 (camera (:struct %camera3d)))
 
;;RLAPI Vector2 GetWorldToScreenEx(Vector3 position, Camera camera, int width, int height); // Get size position for a 3d world space position
;;RLAPI Vector2 GetWorldToScreen2D(Vector2 position, Camera2D camera); // Get the screen space position for a 2d camera world space position
;;RLAPI Vector2 GetScreenToWorld2D(Vector2 position, Camera2D camera); // Get the world space position for a 2d camera screen space position
;;
;;// Timing-related functions
;;RLAPI void SetTargetFPS(int fps);                                 // Set target FPS (maximum)
(defcfun "SetTargetFPS" :void
 "Set target FPS (maximum)"
 (fps :int))

;;RLAPI int GetFPS(void);                                           // Get current FPS
(defcfun "GetFPS" :int
 "Get current FPS")

;;RLAPI float GetFrameTime(void);                                   // Get time in seconds for last frame drawn (delta time)
(defcfun "GetFrameTime" :float
 "Get time in seconds for last frame drawn")

;;RLAPI double GetTime(void);                                       // Get elapsed time in seconds since InitWindow()
(defcfun "GetTime" :double
 "Get elapsed time in seconds since InitWindow()")

;;// Misc. functions
;;RLAPI void SetConfigFlags(unsigned int flags);                    // Setup window configuration flags (view FLAGS)
(defcfun "SetConfigFlags" :void
 "Setup window configuration flags (view FLAGS)"
 (flags :unsigned-int))

;;RLAPI void SetTraceLogLevel(int logType);                         // Set the current threshold (minimum) log level
(defcfun "SetTraceLogLevel" :void
 "Set the current threshold (minimum) log level"
 (log-type :int))

;;RLAPI void SetTraceLogExit(int logType);                          // Set the exit threshold (minimum) log level
(defcfun "SetTraceLogExit" :void
 "Set the exit threshold (minimum) log level"
 (log-type :int))

;;RLAPI void SetTraceLogCallback(TraceLogCallback callback);        // Set a trace log callback to enable custom logging
;;RLAPI void TraceLog(int logType, const char *text, ...);          // Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR)
(defcfun "TraceLog" :void
 "Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR)"
 (log-type :int)
 (text :string)
 &rest)

;;RLAPI void TakeScreenshot(const char *fileName);                  // Takes a screenshot of current screen (saved a .png)
(defcfun "TakeScreenshot" :void
 "Takes a screenshot of current screen (saved a .png)"
 (file-name :string))

;;RLAPI int GetRandomValue(int min, int max);                       // Get a random value between min and max (both included)
(defcfun "GetRandomValue" :int
 "Returns a random value between min and max (both included)"
 (min :int)
 (max :int))

;;// Files management functions
;;RLAPI unsigned char *LoadFileData(const char *fileName, unsigned int *bytesRead);     // Load file data as byte array (read)
(defcfun "LoadFileData" (:pointer :unsigned-char)
 "Load file data as byte array (read)"
 (file-name :string)
 (bytes-read (:pointer :unsigned-int)))

;;RLAPI void SaveFileData(const char *fileName, void *data, unsigned int bytesToWrite); // Save data to file from byte array (write)
(defcfun "SaveFileData" :void
 "Save data to file from byte array (write)"
 (file-name :string)
 (data :pointer)
 (bytes-to-write :unsigned-int))

;;RLAPI char *LoadFileText(const char *fileName);                   // Load text data from file (read), returns a '\0' terminated string
(defcfun "LoadFileText" (:pointer :char)
 "Load text data from file (read), returns a '\0' terminated string"
 (file-name :string))

;;RLAPI void SaveFileText(const char *fileName, char *text);        // Save text data to file (write), string must be '\0' terminated
(defcfun "SaveFileText" :void
 "Save text data to file (write), string must be '\0' terminated"
 (file-name :string)
 (text (:pointer :char)))

;;RLAPI bool FileExists(const char *fileName);                      // Check if file exists
(defcfun "FileExists" :boolean
 "Check if file exists"
 (filename :string))

;;RLAPI bool IsFileExtension(const char *fileName, const char *ext);// Check file extension
(defcfun "IsFileExtension" :boolean
 "Check file extension"
 (file-name :string)
 (ext :string))
 
;;RLAPI bool DirectoryExists(const char *dirPath);                  // Check if a directory path exists
(defcfun "DirectoryExists" :boolean
 "Check if a directory path exists"
 (dir-path :string))

;;RLAPI const char *GetExtension(const char *fileName);             // Get pointer to extension for a filename string
(defcfun "GetExtension" :string
 "Get pointer to extension for a filename string"
 (file-name :string))

;;RLAPI const char *GetFileName(const char *filePath);              // Get pointer to filename for a path string
(defcfun "GetFileName" :string
 "Get pointer to filename for a path string"
 (file-name :string))

;;RLAPI const char *GetFileNameWithoutExt(const char *filePath);    // Get filename string without extension (memory should be freed)
(defcfun "GetFileNameWithoutExt" :string
 "Get filename string without extension (memory should be freed)"
 (file-name :string))
;;RLAPI const char *GetDirectoryPath(const char *filePath);         // Get full path for a given fileName with path (uses static string)
(defcfun "GetDirectoryPath" :string
 "Get full path for a given fileName (uses static string)"
 (file-path :string))
 
;;RLAPI const char *GetPrevDirectoryPath(const char *dirPath);      // Get previous directory path for a given path (uses static string)
(defcfun "GetPrevDirectoryPath" :string
 "Get previous directory path for a given path (uses static string)"
 (dir-path :string))

;;RLAPI const char *GetWorkingDirectory(void);                      // Get current working directory (uses static string)
(defcfun "GetWorkingDirectory" :string
 "Get current working directory (uses static string)")

;;RLAPI char **GetDirectoryFiles(const char *dirPath, int *count);  // Get filenames in a directory path (memory should be freed)
(defcfun "GetDirectoryFiles" :pointer
 "Get filenames in a directory path (memory should be freed)"
 (dir-path :string)
 (count (:pointer :int)))

;;RLAPI void ClearDirectoryFiles(void);                             // Clear directory files paths buffers (free memory)
(defcfun "ClearDirectoryFiles" :void
 "Clear directory files paths buffers (free memory)")

;;RLAPI bool ChangeDirectory(const char *dir);                      // Change working directory, return true on success
(defcfun "ChangeDirectory" :bool
 "Change working directory, returns true if success"
 (dir :string))

;;RLAPI bool IsFileDropped(void);                                   // Check if a file has been dropped into window
(defcfun "IsFileDropped" :boolean
 "Check if a file has been dropped into window")

;;RLAPI char **GetDroppedFiles(int *count);                         // Get dropped files names (memory should be freed)
(defcfun "GetDroppedFiles" :pointer
 "Get dropped files names (memory should be freed)"
 (count :int))

;;RLAPI void ClearDroppedFiles(void);                               // Clear dropped files paths buffer (free memory)
(defcfun "ClearDroppedFiles" :void
 "Clear dropped files paths buffer (free memory)")

;;RLAPI long GetFileModTime(const char *fileName);                  // Get file modification time (last write time)
(defcfun "GetFileModTime" :long
 "Get file modification time (last write time)"
 (file-name :string))

;;// Compression/Encoding functionality
;;RLAPI unsigned char *CompressData(unsigned char *data, int dataLength, int *compDataLength);        // Compress data (DEFLATE algorithm)
(defcfun "CompressData" (:pointer :unsigned-char)
 "Compress data (DEFLATE algorithm)"
 (data (:pointer :unsigned-char))
 (data-length :int)
 (comp-data-length (:pointer :int)))

;;RLAPI unsigned char *DecompressData(unsigned char *compData, int compDataLength, int *dataLength);  // Decompress data (DEFLATE algorithm)
(defcfun "DecompressData" (:pointer :unsigned-char)
 "Decompress data (DEFLATE algorithm)"
 (comp-data (:pointer :unsigned-char))
 (comp-data-length :int)
 (data-length (:pointer :int)))

;;// Persistent storage management
;;RLAPI bool SaveStorageValue(unsigned int position, int value);    // Save integer value to storage file (to defined position), returns true on success
(defcfun "SaveStorageValue" :void
 "Save integer value to storage file (to defined position)"
 (position :unsigned-int)
 (value :int))
 
;;RLAPI int LoadStorageValue(unsigned int position);                // Load integer value from storage file (from defined position)
(defcfun "LoadStorageValue" :int
 "Load integer value from storage file (from defined position)"
 (position :unsigned-int))
 
;;RLAPI void OpenURL(const char *url);                              // Open URL with default system browser (if available)
(defcfun "OpenURL" :void
 "Open URL with default system browser (if available)"
 (url :string))

;;//------------------------------------------------------------------------------------
;;// Input Handling Functions (Module: core)
;;//------------------------------------------------------------------------------------
;;
;;// Input-related functions: keyboard
;;RLAPI bool IsKeyPressed(int key);                             // Check if a key has been pressed once
(defcfun "IsKeyPressed" :bool
 "Check if a key has been pressed once"
 (key :int))

;;RLAPI bool IsKeyDown(int key);                                // Check if a key is being pressed
(defcfun "IsKeyDown" :bool
 "Check if a key is being pressed"
 (key :int))

;;RLAPI bool IsKeyReleased(int key);                            // Check if a key has been released once
(defcfun "IsKeyReleased" :bool
 "Check if a key has been released once"
 (key :int))

;;RLAPI bool IsKeyUp(int key);                                  // Check if a key is NOT being pressed
(defcfun "IsKeyUp" :bool
 "Detect if a key is NOT being pressed"
 (key :int))
 
;;RLAPI void SetExitKey(int key);                               // Set a custom key to exit program (default is ESC)
(defcfun "SetExitKey" :void
 "Set a custom key to exit program (default is ESC)"
 (key :int))

;;RLAPI int GetKeyPressed(void);                                // Get key pressed (keycode), call it multiple times for keys queued, returns 0 when the queue is empty
(defcfun "GetKeyPressed" :int
 "Get key pressed, call it multiple times for chars queued")

;;RLAPI int GetCharPressed(void);                               // Get char pressed (unicode), call it multiple times for chars queued, returns 0 when the queue is empty
(defcfun "GetCharPressed" :int
  "Get char pressed (unicode), call it multiple times for chars queued, returns 0 when the queue is empty")

;;
;;// Input-related functions: gamepads
;;RLAPI bool IsGamepadAvailable(int gamepad);                   // Check if a gamepad is available
(defcfun "IsGamepadAvailable" :boolean
 "Check if a gamepad is available"
 (gamepad :int))

;;RLAPI bool IsGamepadName(int gamepad, const char *name);      // Check gamepad name (if available)
(defcfun "IsGamepadName" :boolean
 "Check gamepad name (if available)"
 (gamepad :int)
 (name :string))

;;RLAPI const char *GetGamepadName(int gamepad);                // Get gamepad internal name id
(defcfun "GetGamepadName" :string
 "Get gamepad internal name id"
 (gamepad :int))

;;RLAPI bool IsGamepadButtonPressed(int gamepad, int button);   // Check if a gamepad button has been pressed once
(defcfun "IsGamepadButtonPressed" :boolean
 "Check if a gamepad button has been pressed once"
 (gamepad :int)
 (button :int))

;;RLAPI bool IsGamepadButtonDown(int gamepad, int button);      // Check if a gamepad button is being pressed
(defcfun "IsGamepadButtonDown" :boolean
 "Check if a gamepad button is being pressed"
 (gamepad :int)
 (button :int))

;;RLAPI bool IsGamepadButtonReleased(int gamepad, int button);  // Check if a gamepad button has been released once
(defcfun "IsGamepadButtonReleased" :boolean
 "Check if a gamepad button has been released once"
 (gamepad :int)
 (button :int))

;;RLAPI bool IsGamepadButtonUp(int gamepad, int button);        // Check if a gamepad button is NOT being pressed
(defcfun "IsGamepadButtonUp" :boolean
 "Check if a gamepad button is NOT being pressed"
 (gamepad :int)
 (button :int))

;;RLAPI int GetGamepadButtonPressed(void);                      // Get the last gamepad button pressed
(defcfun "GetGamepadButtonPressed" :int
 "Get the last gamepad button pressed")

;;RLAPI int GetGamepadAxisCount(int gamepad);                   // Get gamepad axis count for a gamepad
(defcfun "GetGamepadAxisCount" :int
 "Get gamepad axis count for a gamepad"
 (gamepad :int))

;;RLAPI float GetGamepadAxisMovement(int gamepad, int axis);    // Get axis movement value for a gamepad axis
(defcfun "GetGamepadAxisMovement" :float
 "Get axis movement value for a gamepad axis"
 (gamepad :int)
 (axis :int))

;;// Input-related functions: mouse
;;RLAPI bool IsMouseButtonPressed(int button);                  // Check if a mouse button has been pressed once
(defcfun "IsMouseButtonPressed" :boolean
 "Check if a mouse button has been pressed once"
 (button :int))

;;RLAPI bool IsMouseButtonDown(int button);                     // Check if a mouse button is being pressed
(defcfun "IsMouseButtonDown" :boolean
 "Check if a mouse button is being pressed"
 (button :int))

;;RLAPI bool IsMouseButtonReleased(int button);                 // Check if a mouse button has been released once
(defcfun "IsMouseButtonReleased" :boolean
 "Check if a mouse button has been released once"
 (button :int))

;;RLAPI bool IsMouseButtonUp(int button);                       // Check if a mouse button is NOT being pressed
(defcfun "IsMouseButtonUp" :boolean
 "Check if a mouse button is NOT being pressed"
 (button :int))

;;RLAPI int GetMouseX(void);                                    // Get mouse position X
(defcfun "GetMouseX" :int
 "Get mouse position X")

;;RLAPI int GetMouseY(void);                                    // Get mouse position Y
(defcfun "GetMouseY" :int
 "Get mouse position Y")

;;RLAPI Vector2 GetMousePosition(void);                         // Get mouse position XY
(defcfun "GetMousePosition" (:struct %vector2)
 "Get mouse position XY")

;;RLAPI void SetMousePosition(int x, int y);                    // Set mouse position XY
(defcfun "SetMousePosition" :void
 "Set mouse position XY"
 (x :int)
 (y :int))

;;RLAPI void SetMouseOffset(int offsetX, int offsetY);          // Set mouse offset
(defcfun "SetMouseOffset" :void
 "Set mouse offset"
 (offset-x :int)
 (offset-y :int))

;;RLAPI void SetMouseScale(float scaleX, float scaleY);         // Set mouse scaling
(defcfun "SetMouseScale" :void
 "Set mouse scaling"
 (scale-x :float)
 (scale-y :float))

;;RLAPI float GetMouseWheelMove(void);                          // Get mouse wheel movement Y
(defcfun "GetMouseWheelMove" :float
  "Get mouse wheel movement Y")

;;RLAPI void SetMouseCursor(int cursor);                        // Set mouse cursor
(defcfun "SetMouseCursor" :void
  "Set mouse cursor"
  (cursor :int))

;;// Input-related functions: touch
;;RLAPI int GetTouchX(void);                                    // Get touch position X for touch point 0 (relative to screen size)
(defcfun "GetTouchX" :int
 "Get touch position X for touch point 0 (relative to screen size)")

;;RLAPI int GetTouchY(void);                                    // Get touch position Y for touch point 0 (relative to screen size)
(defcfun "GetTouchY" :int
 "Get touch position Y for touch point 0 (relative to screen size)")

;;RLAPI Vector2 GetTouchPosition(int index);                    // Get touch position XY for a touch point index (relative to screen size)
(defcfun "GetTouchPosition" (:struct %vector2)
 "Get touch position XY for a touch point index (relative to screen size)"
 (index :int))

;;//------------------------------------------------------------------------------------
;;// Gestures and Touch Handling Functions (Module: gestures)
;;//------------------------------------------------------------------------------------
;;RLAPI void SetGesturesEnabled(unsigned int flags);      // Enable a set of gestures using flags
(defcfun "SetGesturesEnabled" :void
 "Enable a set of gestures using flags"
 (flags :unsigned-int))

;;RLAPI bool IsGestureDetected(int gesture);              // Check if a gesture have been detected
(defcfun "IsGestureDetected" :boolean
 "Check if a gesture have been detected"
 (gesture :int))

;;RLAPI int GetGestureDetected(void);                     // Get latest detected gesture
(defcfun "GetGestureDetected" :int
 "Get latest detected gesture")

;;RLAPI float GetGestureHoldDuration(void);               // Get gesture hold time in milliseconds
(defcfun "GetGestureHoldDuration" :float
 "Get gesture hold time in milliseconds")

;;RLAPI Vector2 GetGestureDragVector(void);               // Get gesture drag vector
(defcfun "GetGestureDragVector" (:struct %vector2)
 "Get gesture drag vector")

;;RLAPI float GetGestureDragAngle(void);                  // Get gesture drag angle
(defcfun "GetGestureDragAngle" :float
 "Get gesture drag angle")

;;RLAPI Vector2 GetGesturePinchVector(void);              // Get gesture pinch delta
(defcfun "GetGesturePinchVector" (:struct %vector2)
 "Get gesture pinch delta")

;;RLAPI float GetGesturePinchAngle(void);                 // Get gesture pinch angle
(defcfun "GetGesturePinchAngle" :float
 "Get gesture pinch angle")

;;//------------------------------------------------------------------------------------
;;// Camera System Functions (Module: rcamera)
;;//------------------------------------------------------------------------------------
;;RLAPI void SetCameraMode(Camera camera, int mode);      // Set camera mode (multiple camera modes available)
(defcfun "SetCameraMode" :void
 "Set camera mode (multiple camera modes available)"
 (camera (:struct %camera3d))
 (mode :int))

;;RLAPI void UpdateCamera(Camera *camera);                // Update camera position for selected mode
(defcfun ("UpdateCamera" %update-camera) :void
  (camera (:pointer (:struct %camera3d))))

(defmacro update-camera (camera)
  (let ((foreign-camera (gensym)))
    `(cffi:with-foreign-object (,foreign-camera '(:struct %camera3d))
       (convert-into-foreign-memory ,camera '(:struct %camera3d) ,foreign-camera)
       (%update-camera ,foreign-camera)
       (update-camera3d-from-foreign ,camera ,foreign-camera))))

;;RLAPI void SetCameraPanControl(int keyPan);             // Set camera pan key to combine with mouse movement (free camera)
(defcfun "SetCameraPanControl" :void
 (key-pan :int))

;;RLAPI void SetCameraAltControl(int keyAlt);             // Set camera alt key to combine with mouse movement (free camera)
(defcfun "SetCameraAltControl" :void
 (key-alt :int))

;;RLAPI void SetCameraSmoothZoomControl(int keySmoothZoom); // Set camera smooth zoom key to combine with mouse (free camera)
(defcfun "SetCameraSmoothZoomControl" :void
 "Set camera smooth zoom key to combine with mouse (free camera)"
 (key-smooth-zoom :int))

;;RLAPI void SetCameraMoveControls(int keyFront, int keyBack, int keyRight, int keyLeft, int keyUp, int keyDown); // Set camera move controls (1st person and 3rd person cameras)
(defcfun "SetCameraMoveControls" :void
 "Set camera move controls (1st person and 3rd person cameras)"
 (front-key :int)
 (back-key :int)
 (right-key :int)
 (left-key :int)
 (up-key :int)
 (down-key :int))

;;//------------------------------------------------------------------------------------
;;// Basic Shapes Drawing Functions (Module: shapes)
;;//------------------------------------------------------------------------------------
;;
;;// Basic shapes drawing functions
;;RLAPI void DrawPixel(int posX, int posY, Color color);                                                   // Draw a pixel
(defcfun "DrawPixel" :void
 "Draw a pixel"
 (pos-x :int)
 (pos-y :int)
 (color :uint))

;;RLAPI void DrawPixelV(Vector2 position, Color color);                                                    // Draw a pixel (Vector version)
(defcfun "DrawPixelV" :void
 "Draw a pixel (Vector version)"
 (position (:struct %vector2))
 (color :uint))

;;RLAPI void DrawLine(int startPosX, int startPosY, int endPosX, int endPosY, Color color);                // Draw a line
(defcfun "DrawLine" :void
 "Draw a line"
 (start-pos-x :int)
 (start-pos-y :int)
 (end-pos-x :int)
 (end-pos-y :int)
 (color :uint))

;;RLAPI void DrawLineV(Vector2 startPos, Vector2 endPos, Color color);                                     // Draw a line (Vector version)
(defcfun "DrawLineV" :void
 "Draw a line (Vector version)"
 (start-pos (:struct %vector2))
 (end-pos (:struct %vector2))
 (color :uint))

;;RLAPI void DrawLineEx(Vector2 startPos, Vector2 endPos, float thick, Color color);                       // Draw a line defining thickness
(defcfun "DrawLineEx" :void
 "Draw a line defining thickness"
 (start-pos (:struct %vector2))
 (end-pos (:struct %vector2))
 (thick :float)
 (color :uint))

;;RLAPI void DrawLineBezier(Vector2 startPos, Vector2 endPos, float thick, Color color);                   // Draw a line using cubic-bezier curves in-out
(defcfun "DrawLineBezier" :void
 "Draw a line using cubic-bezier curves in-out"
 (start-pos (:struct %vector2))
 (end-pos (:struct %vector2))
 (thick :float)
 (color :uint))

;;RLAPI void DrawLineBezierQuad(Vector2 startPos, Vector2 endPos, Vector2 controlPos, float thick, Color color); // Draw line using quadratic bezier curves with a control point
(defcfun "DrawLineBezierQuad" :void
  (start-pos (:struct %vector2))
  (end-pos (:struct %vector2))
  (control-pos (:struct %vector2))
  (thick :float)
  (color :uint))

;;RLAPI void DrawLineBezierCubic(Vector2 startPos, Vector2 endPos, Vector2 startControlPos, Vector2 endControlPos, float thick, Color color); // Draw line using cubic bezier curves with 2 control points
(defcfun "DrawLineBezierCubic" :void
  (start-pos (:struct %vector2))
  (end-pos (:struct %vector2))
  (start-control-pos (:struct %vector2))
  (end-control-pos (:struct %vector2))
  (thick :float)
  (color :uint))

;;RLAPI void DrawLineStrip(Vector2 *points, int numPoints, Color color);                                   // Draw lines sequence
(defcfun "DrawLineStrip" :void
  "Draw lines sequence"
  (points (:pointer (:struct %vector2)))
  (num-points :int)
  (color :uint))

;;RLAPI void DrawCircle(int centerX, int centerY, float radius, Color color);                              // Draw a color-filled circle
(defcfun "DrawCircle" :void
  "Draw a color-filled circle"
  (center-x :int)
  (center-y :int)
  (radius :float)
  (color :uint))

;;RLAPI void DrawCircleSector(Vector2 center, float radius, int startAngle, int endAngle, int segments, Color color);     // Draw a piece of a circle
(defcfun "DrawCircleSector" :void
  "Draw a piece of a circle"
  (center (:struct %vector2))
  (radius :float)
  (start-angle :int)
  (end-angle :int)
  (segments :int)
  (color :uint))

;;RLAPI void DrawCircleSectorLines(Vector2 center, float radius, int startAngle, int endAngle, int segments, Color color);    // Draw circle sector outline
(defcfun "DrawCircleSectorLines" :void
  "Draw circle sector outline"
  (center (:struct %vector2))
  (radius :float)
  (start-angle :int)
  (end-angle :int)
  (segments :int)
  (color :uint))

;;RLAPI void DrawCircleGradient(int centerX, int centerY, float radius, Color color1, Color color2);       // Draw a gradient-filled circle
(defcfun "DrawCircleGradient" :void
  "Draw a gradient-filled circle"
  (center-x :int)
  (center-y :int)
  (radius :float)
  (color1 :uint)
  (color2 :uint))

;;RLAPI void DrawCircleV(Vector2 center, float radius, Color color);                                       // Draw a color-filled circle (Vector version)
(defcfun "DrawCircleV" :void
 "Draw a color-filled circle (Vector version)"
 (center (:struct %vector2))
 (radius :float)
 (color :uint))

;;RLAPI void DrawCircleLines(int centerX, int centerY, float radius, Color color);                         // Draw circle outline
(defcfun "DrawCircleLines" :void
  "Draw circle outline"
  (center-x :int)
  (center-y :int)
  (radius :float)
  (color :uint))
  
;;RLAPI void DrawEllipse(int centerX, int centerY, float radiusH, float radiusV, Color color);             // Draw ellipse
(defcfun "DrawEllipse" :void
 "Draw ellipse"
 (center-x :int)
 (center-y :int)
 (radius-h :float)
 (radius-v :float)
 (color :uint))

;;RLAPI void DrawEllipseLines(int centerX, int centerY, float radiusH, float radiusV, Color color);        // Draw ellipse outline
(defcfun "DrawEllipseLines" :void
 "Draw ellipse outline"
 (center-x :int)
 (center-y :int)
 (radius-h :float)
 (radius-v :float)
 (color :uint))

;;RLAPI void DrawRing(Vector2 center, float innerRadius, float outerRadius, int startAngle, int endAngle, int segments, Color color); // Draw ring
(defcfun "DrawRing" :void
  "Draw ring"
  (center (:struct %vector2))
  (inner-radius :float)
  (outer-radius :float)
  (start-angle :int)
  (end-angle :int)
  (segments :int)
  (color :uint))

;;RLAPI void DrawRingLines(Vector2 center, float innerRadius, float outerRadius, int startAngle, int endAngle, int segments, Color color);    // Draw ring outline
(defcfun "DrawRingLines" :void
  "Draw ring outline"
  (center (:struct %vector2))
  (inner-radius :float)
  (outer-radius :float)
  (start-angle :int)
  (end-angle :int)
  (segments :int)
  (color :uint))

;;RLAPI void DrawRectangle(int posX, int posY, int width, int height, Color color);                        // Draw a color-filled rectangle
(defcfun "DrawRectangle" :void
 "Draw a color-filled rectangle"
 (pos-x :int)
 (pos-y :int)
 (width :int)
 (height :int)
 (color :uint))

;;RLAPI void DrawRectangleV(Vector2 position, Vector2 size, Color color);                                  // Draw a color-filled rectangle (Vector version)
(defcfun "DrawRectangleV" :void
 "Draw a color-filled rectangle (Vector version)"
 (position (:struct %vector2))
 (size (:struct %vector2))
 (color :uint))

;;RLAPI void DrawRectangleRec(Rectangle rec, Color color);                                                 // Draw a color-filled rectangle
(defcfun "DrawRectangleRec" :void
 "Draw a color-filled rectangle"
 (rec (:struct %rectangle))
 (color :uint))

;;RLAPI void DrawRectanglePro(Rectangle rec, Vector2 origin, float rotation, Color color);                 // Draw a color-filled rectangle with pro parameters
(defcfun "DrawRectanglePro" :void
 "Draw a color-filled rectangle with pro parameters"
 (rec (:struct %rectangle))
 (origin (:struct %vector2))
 (rotation :float)
 (color :uint))

;;RLAPI void DrawRectangleGradientV(int posX, int posY, int width, int height, Color color1, Color color2);// Draw a vertical-gradient-filled rectangle
(defcfun "DrawRectangleGradientV" :void
 "Draw a vertical-gradient-filled rectangle"
 (pos-x :int)
 (pos-y :int)
 (width :int)
 (height :int)
 (color1 :uint)
 (color2 :uint))

;;RLAPI void DrawRectangleGradientH(int posX, int posY, int width, int height, Color color1, Color color2);// Draw a horizontal-gradient-filled rectangle
(defcfun "DrawRectangleGradientH" :void
 "Draw a horizontal-gradient-filled rectangle"
 (pos-x :int)
 (pos-y :int)
 (width :int)
 (height :int)
 (color1 :uint)
 (color2 :uint))

;;RLAPI void DrawRectangleGradientEx(Rectangle rec, Color col1, Color col2, Color col3, Color col4);       // Draw a gradient-filled rectangle with custom vertex colors
(defcfun "DrawRectangleGradientEx" :void
 "Draw a gradient-filled rectangle with custom vertex colors"
 (rec (:struct %rectangle))
 (col1 :uint)
 (col2 :uint)
 (col3 :uint)
 (col4 :uint))

;;RLAPI void DrawRectangleLines(int posX, int posY, int width, int height, Color color);                   // Draw rectangle outline
(defcfun "DrawRectangleLines" :void
 "Draw rectangle outline"
 (pos-x :int)
 (pos-y :int)
 (width :int)
 (height :int)
 (color :uint))
 
;;RLAPI void DrawRectangleLinesEx(Rectangle rec, float lineThick, Color color);                              // Draw rectangle outline with extended parameters
(defcfun "DrawRectangleLinesEx" :void
 "Draw rectangle outline with extended parameters"
 (rec (:struct %rectangle))
 (line-thick :float)
 (color :uint))

;;RLAPI void DrawRectangleRounded(Rectangle rec, float roundness, int segments, Color color);              // Draw rectangle with rounded edges
(defcfun "DrawRectangleRounded" :void
 "Draw rectangle with rounded edges"
 (rec (:struct %rectangle))
 (roundness :float)
 (segments :int)
 (color :uint))

;;RLAPI void DrawRectangleRoundedLines(Rectangle rec, float roundness, int segments, float lineThick, Color color); // Draw rectangle with rounded edges outline
(defcfun "DrawRectangleRoundedLines" :void
 "Draw rectangle with rounded edges outline"
 (rec (:struct %rectangle))
 (roundness :float)
 (segments :int)
 (line-thick :float)
 (color :uint))

;;RLAPI void DrawTriangle(Vector2 v1, Vector2 v2, Vector2 v3, Color color);                                // Draw a color-filled triangle
(defcfun "DrawTriangle" :void
 "Draw a color-filled triangle"
 (v1 (:struct %vector2))
 (v2 (:struct %vector2))
 (v3 (:struct %vector2))
 (color :uint))
 
;;RLAPI void DrawTriangleLines(Vector2 v1, Vector2 v2, Vector2 v3, Color color);                           // Draw triangle outline
(defcfun "DrawTriangleLines" :void
 "Draw triangle outline"
 (v1 (:struct %vector2))
 (v2 (:struct %vector2))
 (v3 (:struct %vector2))
 (color :uint))

;;RLAPI void DrawTriangleFan(Vector2 *points, int numPoints, Color color);                                 // Draw a triangle fan defined by points (first vertex is the center)
(defcfun "DrawTriangleFan" :void
 "Draw a triangle fan defined by points"
 (points (:pointer (:struct %vector2)))
 (num-points :int)
 (color :uint))
 
;;RLAPI void DrawTriangleStrip(Vector2 *points, int pointsCount, Color color);                             // Draw a triangle strip defined by points
(defcfun "DrawTriangleStrip" :void
 "Draw a triangle strip defined by points"
 (points (:pointer (:struct %vector2)))
 (points-count :int)
 (color :uint))

;;RLAPI void DrawPoly(Vector2 center, int sides, float radius, float rotation, Color color);               // Draw a regular polygon (Vector version)
(defcfun "DrawPoly" :void
 "Draw a regular polygon (Vector version)"
 (center (:struct %vector2))
 (sides :int)
 (radius :float)
 (rotation :float)
 (color :uint))
 
;;RLAPI void DrawPolyLines(Vector2 center, int sides, float radius, float rotation, Color color);          // Draw a polygon outline of n sides
(defcfun "DrawPolyLines" :void
 "Draw a polygon outline of n sides"
 (center (:struct %vector2))
 (sides :int)
 (radius :float)
 (rotation :float)
 (color :uint))

;;// Basic shapes collision detection functions
;;RLAPI bool CheckCollisionRecs(Rectangle rec1, Rectangle rec2);                                           // Check collision between two rectangles
(defcfun "CheckCollisionRecs" :bool
 (rec1 (:struct %rectangle))
 (rec2 (:struct %rectangle)))

;;RLAPI bool CheckCollisionCircles(Vector2 center1, float radius1, Vector2 center2, float radius2);        // Check collision between two circles
(defcfun "CheckCollisionCircles" :bool
 (center1 (:struct %vector2))
 (radius1 :float)
 (center2 (:struct %vector2))
 (radius2 :float))

;;RLAPI bool CheckCollisionCircleRec(Vector2 center, float radius, Rectangle rec);                         // Check collision between circle and rectangle
(defcfun "CheckCollisionCircleRec" :bool
 (center (:struct %vector2))
 (radius :float)
 (rec (:struct %rectangle)))

;;RLAPI Rectangle GetCollisionRec(Rectangle rec1, Rectangle rec2);                                         // Get collision rectangle for two rectangles collision
(defcfun "GetCollisionRec" (:struct %rectangle)
 (rec1 (:struct %rectangle))
 (rec2 (:struct %rectangle)))

;;RLAPI bool CheckCollisionPointRec(Vector2 point, Rectangle rec);                                         // Check if point is inside rectangle
(defcfun "CheckCollisionPointRec" :bool
 (point (:struct %vector2))
 (rec (:struct %rectangle)))

;;RLAPI bool CheckCollisionPointCircle(Vector2 point, Vector2 center, float radius);                       // Check if point is inside circle
(defcfun "CheckCollisionPointCircle" :bool
 (point (:struct %vector2))
 (center (:struct %vector2))
 (radius :float))

;;RLAPI bool CheckCollisionPointTriangle(Vector2 point, Vector2 p1, Vector2 p2, Vector2 p3);               // Check if point is inside a triangle
(defcfun "CheckCollisionPointTriangle" :bool
 (point (:struct %vector2))
 (p1 (:struct %vector2))
 (p2 (:struct %vector2))
 (p3 (:struct %vector2)))

;;//------------------------------------------------------------------------------------
;;// Texture Loading and Drawing Functions (Module: textures)
;;//------------------------------------------------------------------------------------
;;
;;// Image loading functions
;;// NOTE: This functions do not require GPU access
;;RLAPI Image LoadImage(const char *fileName);                                                             // Load image from file into CPU memory (RAM)
(defcfun "LoadImage" (:struct %image)
 (file-name :string))

;;RLAPI Image LoadImageEx(Color *pixels, int width, int height);                                           // Load image from Color array data (RGBA - 32bit)
(defcfun "LoadImageEx" (:struct %image)
 (pixels (:pointer :uint))
 (width :int)
 (height :int))
 
;;RLAPI Image LoadImagePro(void *data, int width, int height, int format);                                 // Load image from raw data with parameters
(defcfun "LoadImagePro" (:struct %image)
 "Load image from raw data with parameters"
 (data :pointer)
 (width :int)
 (height :int)
 (format :int))

;;RLAPI Image LoadImageRaw(const char *fileName, int width, int height, int format, int headerSize);       // Load image from RAW file data
(defcfun "LoadImageRaw" (:struct %image)
 "Load image from RAW file data"
 (filename :string)
 (width :int)
 (height :int)
 (format :int)
 (header-size :int))
 
;;RLAPI void UnloadImage(Image image);                                                                     // Unload image from CPU memory (RAM)
(defcfun "UnloadImage" :void
 "Unload image from CPU memory (RAM)"
 (image (:struct %image)))

;;RLAPI void ExportImage(Image image, const char *fileName);                                               // Export image data to file
(defcfun "ExportImage" :void
 "Export image data to file"
 (image (:struct %image))
 (filename :string))

;;RLAPI void ExportImageAsCode(Image image, const char *fileName);                                         // Export image as code file defining an array of bytes
(defcfun "ExportImageAsCode" :void
 "Export image as code file defining an array of bytes"
 (image (:struct %image))
 (filename :string))
 
;;RLAPI Color *GetImageData(Image image);                                                                  // Get pixel data from image as a Color struct array
(defcfun "GetImageData" :pointer
 "Get pixel data from image as a Color struct array"
 (image (:struct %image)))

;;RLAPI Vector4 *GetImageDataNormalized(Image image);                                                      // Get pixel data from image as Vector4 array (float normalized)
(defcfun "GetImageDataNormalized" :pointer
 "Get pixel data from image as Vector4 array (float normalized)"
 (image (:struct %image)))
 
;;// Image generation functions
;;RLAPI Image GenImageColor(int width, int height, Color color);                                           // Generate image: plain color
(defcfun "GenImageColor" (:struct %image)
 "Generate image: plain color"
 (width :int)
 (height :int)
 (color :uint))

;;RLAPI Image GenImageGradientV(int width, int height, Color top, Color bottom);                           // Generate image: vertical gradient
(defcfun "GenImageGradientV" (:struct %image)
 "Generate image: vertical gradient"
 (width :int)
 (height :int)
 (top :uint)
 (bottom :uint))

;;RLAPI Image GenImageGradientH(int width, int height, Color left, Color right);                           // Generate image: horizontal gradient
(defcfun "GenImageGradientH" (:struct %image)
 "Generate image: horizontal gradient"
 (width :int)
 (height :int)
 (left :uint)
 (right :uint))

;;RLAPI Image GenImageGradientRadial(int width, int height, float density, Color inner, Color outer);      // Generate image: radial gradient
(defcfun "GenImageGradientRadial" (:struct %image)
 "Generate image: radial gradient"
 (width :int)
 (height :int)
 (density :float)
 (inner :uint)
 (outer :uint))         
   
;;RLAPI Image GenImageChecked(int width, int height, int checksX, int checksY, Color col1, Color col2);    // Generate image: checked
(defcfun "GenImageChecked" (:struct %image)
 "Generate image: checked"
 (width :int)
 (height :int)
 (checks-x :int)
 (checks-y :int)
 (col1 :uint)
 (col2 :uint))

;;RLAPI Image GenImageWhiteNoise(int width, int height, float factor);                                     // Generate image: white noise
(defcfun "GenImageWhiteNoise" (:struct %image)
 "Generate image: white noise"
 (width :int)
 (height :int)
 (factor :float))

;;RLAPI Image GenImagePerlinNoise(int width, int height, int offsetX, int offsetY, float scale);           // Generate image: perlin noise
(defcfun "GenImagePerlinNoise" (:struct %image)
 "Generate image: perlin noise"
 (width :int)
 (height :int)
 (offset-x :int)
 (offset-y :int)
 (scale :float))

;;RLAPI Image GenImageCellular(int width, int height, int tileSize);                                       // Generate image: cellular algorithm. Bigger tileSize means bigger cells
(defcfun "GenImageCellular" (:struct %image)
 "Generate image: cellular algorithm. Bigger tileSize means bigger cells"
 (width :int)
 (height :int)
 (tile-size :int))

;;// Image manipulation functions
;;RLAPI Image ImageCopy(Image image);                                                                      // Create an image duplicate (useful for transformations)
(defcfun "ImageCopy" (:struct %image)
 "Create an image duplicate (useful for transformations)"
 (image (:struct %image)))
 
;;RLAPI Image ImageFromImage(Image image, Rectangle rec);                                                  // Create an image from another image piece
(defcfun "ImageFromImage" (:struct %image)
 "Create an image from another image piece"
 (image (:struct %image))
 (rec (:struct %rectangle)))

;;RLAPI Image ImageText(const char *text, int fontSize, Color color);                                      // Create an image from text (default font)
(defcfun "ImageText" (:struct %image)
 "Create an image from text (default font)"
 (text :string)
 (font-size :int)
 (color :uint))

;;RLAPI Image ImageTextEx(Font font, const char *text, float fontSize, float spacing, Color tint);         // Create an image from text (custom sprite font)
(defcfun "ImageTextEx" (:struct %image)
 "Create an image from text (custom sprite font)"
 (font (:struct %font))
 (text :string)
 (font-size :float)
 (spacing :float)
 (tint :uint))

;;RLAPI void ImageToPOT(Image *image, Color fillColor);                                                    // Convert image to POT (power-of-two)
(defcfun "ImageToPOT" :void
 (image (:pointer (:struct %image)))
 (fill-color :uint))

;;RLAPI void ImageFormat(Image *image, int newFormat);                                                     // Convert image data to desired format
(defcfun "ImageFormat" :void
 (image (:pointer (:struct %image)))
 (new-format :int))

;;RLAPI void ImageAlphaMask(Image *image, Image alphaMask);                                                // Apply alpha mask to image
(defcfun "ImageAlphaMask" :void
 (image (:pointer (:struct %image)))
 (alpha-mask (:struct %image)))

;;RLAPI void ImageAlphaClear(Image *image, Color color, float threshold);                                  // Clear alpha channel to desired color
(defcfun "ImageAlphaClear" :void
 "Clear alpha channel to desired color"
 (image :pointer)
 (color :uint)
 (threshold :float))

;;RLAPI void ImageAlphaCrop(Image *image, float threshold);                                                // Crop image depending on alpha value
(defcfun "ImageAlphaCrop" :void
 "Crop image depending on alpha value"
 (image :pointer)
 (threshold :float))

;;RLAPI void ImageAlphaPremultiply(Image *image);                                                          // Premultiply alpha channel
(defcfun "ImageAlphaPremultiply" :void
 "Premultiply alpha channel"
 (image :pointer))

;;RLAPI void ImageCrop(Image *image, Rectangle crop);                                                      // Crop an image to a defined rectangle
(defcfun "ImageCrop" :void
 "rop an image to a defined rectangle"
 (image :pointer)
 (crop (:struct %rectangle)))

;;RLAPI void ImageResize(Image *image, int newWidth, int newHeight);                                       // Resize image (Bicubic scaling algorithm)
(defcfun "ImageResize" :void
 "Resize image (Bicubic scaling algorithm)"
 (image :pointer)
 (new-width :int)
 (new-height :int))

;;RLAPI void ImageResizeNN(Image *image, int newWidth,int newHeight);                                      // Resize image (Nearest-Neighbor scaling algorithm)
(defcfun "ImageResizeNN" :void
 "Resize image (Nearest-Neighbor scaling algorithm)"
 (image :pointer)
 (new-width :int)
 (new-height :int))

;;RLAPI void ImageResizeCanvas(Image *image, int newWidth, int newHeight, int offsetX, int offsetY, Color color);  // Resize canvas and fill with color
(defcfun "ImageResizeCanvas" :void
 "Resize canvas and fill with color"
 (image :pointer)
 (new-width :int)
 (new-height :int)
 (offset-x :int)
 (offset-y :int)
 (color :uint))

;;RLAPI void ImageMipmaps(Image *image);                                                                   // Generate all mipmap levels for a provided image
(defcfun "ImageMipmaps" :void
 "Generate all mipmap levels for a provided image"
 (image :pointer))

;;RLAPI void ImageDither(Image *image, int rBpp, int gBpp, int bBpp, int aBpp);                            // Dither image data to 16bpp or lower (Floyd-Steinberg dithering)
(defcfun "ImageDither" :void
 "Dither image data to 16bpp or lower (Floyd-Steinberg dithering)"
 (image :pointer)
 (r-bpp :int)
 (g-bpp :int)
 (b-bpp :int)
 (a-bpp :int))
 
;;RLAPI void ImageFlipVertical(Image *image);                                                              // Flip image vertically
(defcfun "ImageFlipVertical" :void
 (image (:pointer (:struct %image))))

;;RLAPI void ImageFlipHorizontal(Image *image);                                                            // Flip image horizontally
(defcfun "ImageFlipHorizontal" :void
 (image (:pointer (:struct %image))))

;;RLAPI void ImageRotateCW(Image *image);                                                                  // Rotate image clockwise 90deg
(defcfun "ImageRotateCW" :void
 "Rotate image clockwise 90deg"
 (image :pointer))

;;RLAPI void ImageRotateCCW(Image *image);                                                                 // Rotate image counter-clockwise 90deg
(defcfun "ImageRotateCCW" :void
 "Rotate image counter-clockwise 90deg"
 (image :pointer))

;;RLAPI void ImageColorTint(Image *image, Color color);                                                    // Modify image color: tint
(defcfun "ImageColorTint" :void
 (image (:pointer (:struct %image)))
 (color :uint))

;;RLAPI void ImageColorInvert(Image *image);                                                               // Modify image color: invert
(defcfun "ImageColorInvert" :void
 (image (:pointer (:struct %image))))

;;RLAPI void ImageColorGrayscale(Image *image);                                                            // Modify image color: grayscale
(defcfun "ImageColorGrayscale" :void
 (image (:pointer (:struct %image))))

;;RLAPI void ImageColorContrast(Image *image, float contrast);                                             // Modify image color: contrast (-100 to 100)
(defcfun "ImageColorContrast" :void
 (image (:pointer (:struct %image)))
 (contrast :float))

;;RLAPI void ImageColorBrightness(Image *image, int brightness);                                           // Modify image color: brightness (-255 to 255)
(defcfun "ImageColorBrightness" :void
 (image (:pointer (:struct %image)))
 (brightness :int))

;;RLAPI void ImageColorReplace(Image *image, Color color, Color replace);                                  // Modify image color: replace color
(defcfun "ImageColorReplace" :void
 "Modify image color: replace color"
 (image :pointer)
 (color :uint)
 (replace :uint))
 
;;RLAPI Color *ImageExtractPalette(Image image, int maxPaletteSize, int *extractCount);                    // Extract color palette from image to maximum size (memory should be freed)
(defcfun "ImageExtractPalette" (:pointer :uint)
 "Extract color palette from image to maximum size (memory should be freed)"
 (image (:struct %image))
 (max-palette-size :int)
 (extract-count (:pointer :int)))

;;RLAPI Rectangle GetImageAlphaBorder(Image image, float threshold);                                       // Get image alpha border rectangle
(defcfun "GetImageAlphaBorder" (:struct %rectangle)
 "Get image alpha border rectangle"
 (image (:struct %image))
 (threshold :float))

;;// Image drawing functions
;;// NOTE: Image software-rendering functions (CPU)
;;RLAPI void ImageClearBackground(Image *dst, Color color);                                                // Clear image background with given color
;;RLAPI void ImageDrawPixel(Image *dst, int posX, int posY, Color color);                                  // Draw pixel within an image
;;RLAPI void ImageDrawPixelV(Image *dst, Vector2 position, Color color);                                   // Draw pixel within an image (Vector version)
;;RLAPI void ImageDrawLine(Image *dst, int startPosX, int startPosY, int endPosX, int endPosY, Color color); // Draw line within an image
;;RLAPI void ImageDrawLineV(Image *dst, Vector2 start, Vector2 end, Color color);                          // Draw line within an image (Vector version)
;;RLAPI void ImageDrawCircle(Image *dst, int centerX, int centerY, int radius, Color color);               // Draw circle within an image
;;RLAPI void ImageDrawCircleV(Image *dst, Vector2 center, int radius, Color color);                        // Draw circle within an image (Vector version)
;;RLAPI void ImageDrawRectangle(Image *dst, int posX, int posY, int width, int height, Color color);       // Draw rectangle within an image
;;RLAPI void ImageDrawRectangleV(Image *dst, Vector2 position, Vector2 size, Color color);                 // Draw rectangle within an image (Vector version)
;;RLAPI void ImageDrawRectangleRec(Image *dst, Rectangle rec, Color color);                                // Draw rectangle within an image 
;;RLAPI void ImageDrawRectangleLines(Image *dst, Rectangle rec, int thick, Color color);                   // Draw rectangle lines within an image
;;RLAPI void ImageDraw(Image *dst, Image src, Rectangle srcRec, Rectangle dstRec, Color tint);             // Draw a source image within a destination image (tint applied to source)
;;RLAPI void ImageDrawText(Image *dst, Vector2 position, const char *text, int fontSize, Color color);     // Draw text (default font) within an image (destination)
;;RLAPI void ImageDrawTextEx(Image *dst, Vector2 position, Font font, const char *text, float fontSize, float spacing, Color color); // Draw text (custom sprite font) within an image (destination)
;;
;;// Texture loading functions
;;// NOTE: These functions require GPU access
;;RLAPI Texture2D LoadTexture(const char *fileName);                                                       // Load texture from file into GPU memory (VRAM)
(defcfun "LoadTexture" (:struct %texture)
 "Load texture from file into GPU memory (VRAM)"
 (file-name :string))

;;RLAPI Texture2D LoadTextureFromImage(Image image);                                                       // Load texture from image data
(defcfun "LoadTextureFromImage" (:struct %texture)
 "Load texture from image data"
 (image (:struct %image)))

;;RLAPI TextureCubemap LoadTextureCubemap(Image image, int layoutType);                                    // Load cubemap from image, multiple image cubemap layouts supported
(defcfun "LoadTextureCubemap" texture-cubemap
 "Load cubemap from image, multiple image cubemap layouts supported"
 (image (:struct %image))
 (layout-type :int))

;;RLAPI RenderTexture2D LoadRenderTexture(int width, int height);                                          // Load texture for rendering (framebuffer)
(defcfun "LoadRenderTexture" (:struct %render-texture)
 "Load texture for rendering (framebuffer)"
 (width :int)
 (height :int))

;;RLAPI void UnloadTexture(Texture2D texture);                                                             // Unload texture from GPU memory (VRAM)
(defcfun "UnloadTexture" :void
 "Unload texture from GPU memory (VRAM)"
 (texture (:struct %texture)))

;;RLAPI void UnloadRenderTexture(RenderTexture2D target);                                                  // Unload render texture from GPU memory (VRAM)
(defcfun "UnloadRenderTexture" :void
 "Unload render texture from GPU memory (VRAM)"
 (target (:struct %render-texture)))

;;RLAPI void UpdateTexture(Texture2D texture, const void *pixels);                                         // Update GPU texture with new data
(defcfun "UpdateTexture" :void
 "Update GPU texture with new data"
 (texture (:struct %texture))
 (pixels :pointer))

;;RLAPI Image GetTextureData(Texture2D texture);                                                           // Get pixel data from GPU texture and return an Image
(defcfun "GetTextureData" (:struct %image)
 "Get pixel data from GPU texture and return an Image"
 (texture (:struct %texture)))

;;RLAPI Image GetScreenData(void);                                                                         // Get pixel data from screen buffer and return an Image (screenshot)
(defcfun "GetScreenData" (:struct %image)
 "Get pixel data from screen buffer and return an Image (screenshot)")

;;// Texture configuration functions
;;RLAPI void GenTextureMipmaps(Texture2D *texture);                                                        // Generate GPU mipmaps for a texture
(defcfun "GenTextureMipmaps" :void
 (texture (:pointer (:struct %texture))))

;;RLAPI void SetTextureFilter(Texture2D texture, int filterMode);                                          // Set texture scaling filter mode
(defcfun "SetTextureFilter" :void
 (texture (:struct %texture))
 (filter-mode :int))

;;RLAPI void SetTextureWrap(Texture2D texture, int wrapMode);                                              // Set texture wrapping mode
(defcfun "SetTextureWrap" :void
 (texture (:struct %texture))
 (wrap-mode :int))

;;// Texture drawing functions
;;RLAPI void DrawTexture(Texture2D texture, int posX, int posY, Color tint);                               // Draw a Texture2D
(defcfun "DrawTexture" :void
 (texture (:struct %texture))
 (pos-x :int)
 (pos-y :int)
 (tint :uint))

;;RLAPI void DrawTextureV(Texture2D texture, Vector2 position, Color tint);                                // Draw a Texture2D with position defined as Vector2
(defcfun "DrawTextureV" :void
 (texture (:struct %texture))
 (position (:struct %vector2))
 (tint :uint))

;;RLAPI void DrawTextureEx(Texture2D texture, Vector2 position, float rotation, float scale, Color tint);  // Draw a Texture2D with extended parameters
(defcfun "DrawTextureEx" :void
 (texture (:struct %texture))
 (position (:struct %vector2))
 (rotation :float)
 (scale :float)
 (tint :uint))

;;RLAPI void DrawTextureRec(Texture2D texture, Rectangle source, Vector2 position, Color tint);            // Draw a part of a texture defined by a rectangle
(defcfun "DrawTextureRec" :void
 "Draw a part of a texture defined by a rectangle"
 (texture (:struct %texture))
 (source (:struct %rectangle))
 (position (:struct %vector2))
 (tint :uint))

;;RLAPI void DrawTextureQuad(Texture2D texture, Vector2 tiling, Vector2 offset, Rectangle quad, Color tint);  // Draw texture quad with tiling and offset parameters
(defcfun "DrawTextureQuad" :void
 "Draw texture quad with tiling and offset parameters"
 (texture (:struct %texture))
 (tiling (:struct %vector2))
 (offset (:struct %vector2))
 (quad (:struct %rectangle))
 (tint :uint))

;;RLAPI void DrawTexturePro(Texture2D texture, Rectangle source, Rectangle dest, Vector2 origin, float rotation, Color tint);           // Draw a part of a texture defined by a rectangle with 'pro' parameters
(defcfun "DrawTexturePro" :void
 "Draw a part of a texture defined by a rectangle with 'pro' parameters"
 (texture (:struct %texture))
 (source (:struct %rectangle))
 (dest (:struct %rectangle))
 (origin (:struct %vector2))
 (rotation :float)
 (tint :uint))

;;RLAPI void DrawTextureNPatch(Texture2D texture, NPatchInfo nPatchInfo, Rectangle dest, Vector2 origin, float rotation, Color tint);   // Draws a texture (or part of it) that stretches or shrinks nicely
(defcfun "DrawTextureNPatch" :void
 "raws a texture (or part of it) that stretches or shrinks nicely"
 (texture (:struct %texture))
 (patch-info (:struct %patch-info))
 (dest-rec (:struct %rectangle))
 (origin (:struct %vector2))
 (rotation :float)
 (tint :uint))

;;// Color/pixel related functions
;;RLAPI Color Fade(Color color, float alpha);                                 // Get color with alpha applied, alpha goes from 0.0f to 1.0f
(defcfun "Fade" :uint
  "Get color with alpha applied, alpha goes from 0.0f to 1.0f"
  (color :uint)
  (alpha :float))

;;RLAPI int ColorToInt(Color color);                                          // Get hexadecimal value for a Color
(defcfun "ColorToInt" :int
 "Get hexadecimal value for a Color"
 (color :uint))

;;RLAPI Vector4 ColorNormalize(Color color);                                  // Get Color normalized as float [0..1]
(defcfun "ColorNormalize" (:struct %vector4)
 "Get color normalized as float [0..1]"
 (color :uint))
 
;;RLAPI Color ColorFromNormalized(Vector4 normalized);                        // Get Color from normalized values [0..1]
(defcfun "ColorFromNormalized" :uint
 "Get color from normalized values [0..1]"
 (normalized (:struct %vector4)))

;;RLAPI Vector3 ColorToHSV(Color color);                                      // Get HSV values for a Color, hue [0..360], saturation/value [0..1]
(defcfun "ColorToHSV" (:struct %vector3)
 "Get HSV values for a Color"
 (color :uint))

;;RLAPI Color ColorFromHSV(float hue, float saturation, float value);         // Get a Color from HSV values, hue [0..360], saturation/value [0..1]
(defcfun "ColorFromHSV" :uint
  (hue :float)
  (saturation :float)
  (value :float))

;;RLAPI Color ColorAlpha(Color color, float alpha);                           // Get color with alpha applied, alpha goes from 0.0f to 1.0f
(defcfun "ColorAlpha" :uint
  (color :uint)
  (alpha :float))

;;RLAPI Color ColorAlphaBlend(Color dst, Color src, Color tint);              // Get src alpha-blended into dst color with tint
(defcfun "ColorAlphaBlend" :uint
  (dst :uint)
  (src :uint)
  (tint :uint))

;;RLAPI Color GetColor(unsigned int hexValue);                                // Get Color structure from hexadecimal value
(defcfun "GetColor" :uint
 "Get a Color struct from hexadecimal value"
 (hex-value :unsigned-int))

;;RLAPI Color GetPixelColor(void *srcPtr, int format);                        // Get Color from a source pixel pointer of certain format
(defcfun "GetPixelColor" :uint
  (src-ptr :pointer)
  (format :int))

;;RLAPI void SetPixelColor(void *dstPtr, Color color, int format);            // Set color formatted into destination pixel pointer
(defcfun "SetPixelColor" :void
  (dst-ptr :pointer)
  (color :uint)
  (format :int))

;;RLAPI int GetPixelDataSize(int width, int height, int format);              // Get pixel data size in bytes for certain format
(defcfun "GetPixelDataSize" :int
  (width :int)
  (height :int)
  (format :int))
;;
;;//------------------------------------------------------------------------------------
;;// Font Loading and Text Drawing Functions (Module: text)
;;//------------------------------------------------------------------------------------
;;
;;// Font loading/unloading functions
;;RLAPI Font GetFontDefault(void);                                                            // Get the default Font
(defcfun "GetFontDefault" (:struct %font)
 "Get the default Font")

;;RLAPI Font LoadFont(const char *fileName);                                                  // Load font from file into GPU memory (VRAM)
(defcfun "LoadFont" (:struct %font)
 "Load font from file into GPU memory (VRAM)"
 (file-name :string))

;;RLAPI Font LoadFontEx(const char *fileName, int fontSize, int *fontChars, int glyphCount);  // Load font from file with extended parameters
(defcfun "LoadFontEx" (:struct %font)
  (file-name :string)
  (font-size :int)
  (font-chars (:pointer :int))
  (glyph-count :int))

;;RLAPI Font LoadFontFromImage(Image image, Color key, int firstChar);                        // Load font from Image (XNA style)
(defcfun "LoadFontFromImage" (:struct %font)
 "Load font from Image (XNA style)"
 (image (:struct %image))
 (key :uint)
 (first-char :int))

;;RLAPI CharInfo *LoadFontData(const char *fileName, int fontSize, int *fontChars, int charsCount, int type); // Load font data for further use
(defcfun "LoadFontData" :pointer
 "Load font data for further use"
 (file-name :string)
 (font-chars :int)
 (chars-count :int)
 (type :int))
 
;;RLAPI Image GenImageFontAtlas(const CharInfo *chars, Rectangle **recs, int charsCount, int fontSize, int padding, int packMethod);  // Generate image font atlas using chars info
;;RLAPI void UnloadFont(Font font);                                                           // Unload Font from GPU memory (VRAM)
(defcfun "UnloadFont" :void
 "Unload Font from GPU memory (VRAM)"
 (font (:struct %font)))

;;// Text drawing functions
;;RLAPI void DrawFPS(int posX, int posY);                                                     // Shows current FPS
(defcfun "DrawFPS" :void
 "Shows current FPS"
 (pos-x :int)
 (pos-y :int))

;;RLAPI void DrawText(const char *text, int posX, int posY, int fontSize, Color color);       // Draw text (using default font)
(defcfun "DrawText" :void
 "Draw text (using default font)"
 (text :string)
 (pos-x :int)
 (pos-y :int)
 (font-size :int)
 (color :uint))

;;RLAPI void DrawTextEx(Font font, const char *text, Vector2 position, float fontSize, float spacing, Color tint);                // Draw text using font and additional parameters
(defcfun "DrawTextEx" :void
 "Draw text using font and additional parameters"
 (font (:struct %font))
 (text :string)
 (position (:struct %vector2))
 (font-size :float)
 (spacing :float)
 (tint :uint))

;;RLAPI void DrawTextRec(Font font, const char *text, Rectangle rec, float fontSize, float spacing, bool wordWrap, Color tint);   // Draw text using font inside rectangle limits
(defcfun "DrawTextRec" :void
 "Draw text using font inside rectangle limits"
 (font (:struct %font))
 (text :string)
 (rec (:struct %rectangle))
 (font-size :float)
 (spacing :float)
 (word-wrap :boolean)
 (tint :uint))

;;RLAPI void DrawTextRecEx(Font font, const char *text, Rectangle rec, float fontSize, float spacing, bool wordWrap, Color tint,
;;                         int selectStart, int selectLength, Color selectTint, Color selectBackTint); // Draw text using font inside rectangle limits with support for text selection
(defcfun "DrawTextRecEx" :void
 "Draw text using font inside rectangle limits with support for text selection"
 (font (:struct %font))
 (text :string)
 (rec (:struct %rectangle))
 (font-size :float)
 (spacing :float)
 (word-wrap :boolean)
 (tint :uint)
 (select-start :int)
 (select-length :int)
 (select-tint :uint)
 (select-back-tint :uint))

;;RLAPI void DrawTextCodepoint(Font font, int codepoint, Vector2 position, float scale, Color tint);   // Draw one character (codepoint)
(defcfun "DrawTextCodepoint" :void
 "Draw one character (codepoint)"
 (font (:struct %font))
 (codepoint :int)
 (position (:struct %vector2))
 (scale :float)
 (tint :uint))

;;
;;// Text misc. functions
;;RLAPI int MeasureText(const char *text, int fontSize);                                      // Measure string width for default font
(defcfun "MeasureText" :int
  "Measure string width for default font"
  (text :string)
  (font-size :int))

;;RLAPI Vector2 MeasureTextEx(Font font, const char *text, float fontSize, float spacing);    // Measure string size for Font
(defcfun "MeasureTextEx" (:struct %vector2)
  (font (:struct %font))
  (text :string)
  (font-size :float)
  (spacing :float))

;;RLAPI int GetGlyphIndex(Font font, int codepoint);                                          // Get index position for a unicode character on font
(defcfun "GetGlyphIndex" :int
 "Get index position for a unicode character on font"
 (font (:struct %font))
 (codepoint :int))

;;// Text strings management functions (no utf8 strings, only byte chars)
;;// NOTE: Some strings allocate memory internally for returned strings, just be careful!
;;RLAPI int TextCopy(char *dst, const char *src);                                             // Copy one string to another, returns bytes copied
(defcfun "TextCopy" :int
 "Copy one string to another, returns bytes copied"
 (dst (:pointer :char))
 (src :string))

;;RLAPI bool TextIsEqual(const char *text1, const char *text2);                               // Check if two text string are equal
(defcfun "TextIsEqual" :boolean
 "Check if two text string are equal"
 (text1 :string)
 (text2 :string))

;;RLAPI unsigned int TextLength(const char *text);                                            // Get text length, checks for '\0' ending
(defcfun "TextLength" :unsigned-int
 "Get text length, checks for '\0' ending"
 (text :string))

;;RLAPI const char *TextFormat(const char *text, ...);                                        // Text formatting with variables (sprintf style)
(defcfun "TextFormat" :string
 "Text formatting with variables (sprintf style)"
 (text :string)
 &rest)

;;RLAPI const char *TextSubtext(const char *text, int position, int length);                  // Get a piece of a text string
(defcfun "TextSubtext" :string
 "Get a piece of a text string"
 (text :string)
 (position :int)
 (length :int))

;;RLAPI char *TextReplace(char *text, const char *replace, const char *by);                   // Replace text string (memory must be freed!)
(defcfun "TextReplace" (:pointer :char)
 "Replace text string (memory must be freed!)"
 (text (:pointer :char))
 (replace :string)
 (by :string))

;;RLAPI char *TextInsert(const char *text, const char *insert, int position);                 // Insert text in a position (memory must be freed!)
(defcfun "TextInsert" (:pointer :char)
 "Insert text in a position (memory must be freed!)"
 (text :string)
 (insert :string)
 (position :int))

;;RLAPI const char *TextJoin(const char **textList, int count, const char *delimiter);        // Join text strings with delimiter
(defcfun "TextJoin" :string
 "Join text strings with delimiter"
 (text-list (:pointer :string))
 (delimiter :string))

;;RLAPI const char **TextSplit(const char *text, char delimiter, int *count);                 // Split text into multiple strings
(defcfun "TextSplit" (:pointer :string)
 "Split text into multiple strings"
 (text :string)
 (delimiter :char)
 (count (:pointer :int)))

;;RLAPI void TextAppend(char *text, const char *append, int *position);                       // Append text at specific position and move cursor!
(defcfun "TextAppend" :void
 "Append text at specific position and move cursor!"
 (text (:pointer :char))
 (append :string)
 (position (:pointer :int)))

;;RLAPI int TextFindIndex(const char *text, const char *find);                                // Find first text occurrence within a string
(defcfun "TextFindIndex" :int
 "Find first text occurrence within a string"
 (text :string)
 (find :string))

;;RLAPI const char *TextToUpper(const char *text);                      // Get upper case version of provided string
(defcfun "TextToUpper" :string
 "Get upper case version of provided string"
 (text :string))

;;RLAPI const char *TextToLower(const char *text);                      // Get lower case version of provided string
(defcfun "TextToLower" :string
 "Get lower case version of provided string"
 (text :string))

;;RLAPI const char *TextToPascal(const char *text);                     // Get Pascal case notation version of provided string
(defcfun "TextToPascal" :string
 "Get Pascal case notation version of provided string"
 (text :string))

;;RLAPI int TextToInteger(const char *text);                            // Get integer value from text (negative values not supported)
(defcfun "TextToInteger" :int
 "Get integer value from text (negative values not supported)"
 (text :string))
 
;;RLAPI char *TextToUtf8(int *codepoints, int length);                  // Encode text codepoint into utf8 text (memory must be freed!)
(defcfun ("TextToUtf8" text-to-utf8) (:pointer :char)
 "Encode text codepoint into utf8 text (memory must be freed!)"
 (codepoints (:pointer :int))
 (length :int))

;;// UTF8 text strings management functions
;;RLAPI int *GetCodepoints(const char *text, int *count);               // Get all codepoints in a string, codepoints count returned by parameters
(defcfun "GetCodepoints" :int
 "Get all codepoints in a string, codepoints count returned by parameters"
 (text :string)
 (count (:pointer :int)))

;; example 
;; (let ((ptr (cffi:foreign-alloc :int)))
;;   (raylib:get-codepoints "hello" ptr)
;;   (cffi:mem-aref ptr) => 5

;;RLAPI int GetCodepointsCount(const char *text);                       // Get total number of characters (codepoints) in a UTF8 encoded string
(defcfun "GetCodepointsCount" :int
 "Get total number of characters (codepoints) in a UTF8 encoded string"
 (text :string))

;;RLAPI int GetNextCodepoint(const char *text, int *bytesProcessed);    // Returns next codepoint in a UTF8 encoded string; 0x3f('?') is returned on failure
(defcfun "GetNextCodepoint" :int
 "Returns next codepoint in a UTF8 encoded string; 0x3f('?') is returned on failure"
 (text :string)
 (bytes-processed (:pointer :int)))

;;RLAPI const char *CodepointToUtf8(int codepoint, int *byteLength);    // Encode codepoint into utf8 text (char array length returned as parameter)
(defcfun ("CodepointToUtf8" codepoint-to-utf8) :string
 "Encode codepoint into utf8 text (char array length returned as parameter)"
 (codepoint :int)
 (byte-length (:pointer :int)))

;;
;;//------------------------------------------------------------------------------------
;;// Basic 3d Shapes Drawing Functions (Module: models)
;;//------------------------------------------------------------------------------------
;;
;;// Basic geometric 3D shapes drawing functions
;;RLAPI void DrawLine3D(Vector3 startPos, Vector3 endPos, Color color);                                    // Draw a line in 3D world space
(defcfun "DrawLine3D" :void
 (start-pos (:struct %vector3))
 (end-pos (:struct %vector3))
 (color :uint))
 
;;RLAPI void DrawPoint3D(Vector3 position, Color color);                                                   // Draw a point in 3D space, actually a small line
(defcfun "DrawPoint3D" :void
 "Draw a point in 3D space, actually a small line"
 (position (:struct %vector3))
 (color :uint))

;;RLAPI void DrawCircle3D(Vector3 center, float radius, Vector3 rotationAxis, float rotationAngle, Color color); // Draw a circle in 3D world space
(defcfun "DrawCircle3D" :void
 (position (:struct %vector3))
 (radius :float)
 (rotation-axis (:struct %vector3))
 (rotation-angle :float)
 (color :uint))

;;RLAPI void DrawCube(Vector3 position, float width, float height, float length, Color color);             // Draw cube
(defcfun "DrawCube" :void
 (position (:struct %vector3))
 (width :float)
 (height :float)
 (length :float)
 (color :uint))

;;RLAPI void DrawCubeV(Vector3 position, Vector3 size, Color color);                                       // Draw cube (Vector version)
(defcfun "DrawCubeV" :void
 (position (:struct %vector3))
 (size (:struct %vector3))
 (color :uint))

;;RLAPI void DrawCubeWires(Vector3 position, float width, float height, float length, Color color);        // Draw cube wires
(defcfun "DrawCubeWires" :void
 "Draw cube wires"
 (position (:struct %vector3))
 (width :float)
 (height :float)
 (length :float)
 (color :uint))

;;RLAPI void DrawCubeWiresV(Vector3 position, Vector3 size, Color color);                                  // Draw cube wires (Vector version)
(defcfun "DrawCubeWiresV" :void
 "Draw cube wires (Vector version)"
 (position (:struct %vector3))
 (size (:struct %vector3))
 (color :uint))

;;RLAPI void DrawCubeTexture(Texture2D texture, Vector3 position, float width, float height, float length, Color color); // Draw cube textured
(defcfun "DrawCubeTexture" :void
 (texture (:struct %texture))
 (position (:struct %vector3))
 (width :float)
 (height :float)
 (length :float)
 (color :uint))

;;RLAPI void DrawSphere(Vector3 centerPos, float radius, Color color);                                     // Draw sphere
(defcfun "DrawSphere" :void
 "Draw sphere"
 (center-pos (:struct %vector3))
 (radius :float)
 (color :uint))

;;RLAPI void DrawSphereEx(Vector3 centerPos, float radius, int rings, int slices, Color color);            // Draw sphere with extended parameters
(defcfun "DrawSphereEx" :void
 "Draw sphere with extended parameters"
 (center-pos (:struct %vector3))
 (radius :float)
 (rings :int)
 (slices :int)
 (color :uint))

;;RLAPI void DrawSphereWires(Vector3 centerPos, float radius, int rings, int slices, Color color);         // Draw sphere wires
(defcfun "DrawSphereWires" :void
 (center-pos (:struct %vector3))
 (radius :float)
 (rings :int)
 (slices :int)
 (color :uint))

;;RLAPI void DrawCylinder(Vector3 position, float radiusTop, float radiusBottom, float height, int slices, Color color); // Draw a cylinder/cone
(defcfun "DrawCylinder" :void
 (position (:struct %vector3))
 (radius-top :float)
 (radius-bottom :float)
 (height :float)
 (slices :int)
 (color :uint))

;;RLAPI void DrawCylinderWires(Vector3 position, float radiusTop, float radiusBottom, float height, int slices, Color color); // Draw a cylinder/cone wires
(defcfun "DrawCylinderWires" :void
 (position (:struct %vector3))
 (radius-top :float)
 (radius-bottom :float)
 (height :float)
 (slices :int)
 (color :uint))

;;RLAPI void DrawPlane(Vector3 centerPos, Vector2 size, Color color);                                      // Draw a plane XZ
(defcfun "DrawPlane" :void
 (current-pos (:struct %vector3))
 (size  (:struct %vector2))
 (color :uint))

;;RLAPI void DrawRay(Ray ray, Color color);                                                                // Draw a ray line
(defcfun "DrawRay" :void
 (ray (:struct %ray))
 (color :uint))

;;RLAPI void DrawGrid(int slices, float spacing);                                                          // Draw a grid (centered at (0, 0, 0))
(defcfun "DrawGrid" :void
 (slices :int)
 (spacing :float))

;;RLAPI void DrawGizmo(Vector3 position);                                                                  // Draw simple gizmo
(defcfun "DrawGizmo" :void
 (position (:struct %vector3)))

;;//DrawTorus(), DrawTeapot() could be useful?
;;
;;//------------------------------------------------------------------------------------
;;// Model 3d Loading and Drawing Functions (Module: models)
;;//------------------------------------------------------------------------------------
;;
;;// Model management functions
;;RLAPI Model LoadModel(const char *fileName);                                                // Load model from files (meshes and materials)
(defcfun "LoadModel" (:struct %model)
 "Load model from files (meshes and materials)"
 (file-name :string))

;;RLAPI Model LoadModelFromMesh(Mesh mesh);                                                   // Load model from generated mesh (default material)
(defcfun "LoadModelFromMesh" (:struct %model)
 "Load model from generated mesh (default material)"
 (mesh (:struct %mesh)))

;;RLAPI void UnloadModel(Model model);                                                        // Unload model (including meshes) from memory (RAM and/or VRAM)
(defcfun "UnloadModel" :void
 (model (:struct %model)))

;;// Mesh loading/unloading functions
;;RLAPI Mesh *LoadMeshes(const char *fileName, int *meshCount);                                           // Load meshes from model file
(defcfun "LoadMeshes" :pointer
 "Load meshes from model file"
 (file-name :string)
 (mesh-count :int))

;;RLAPI void ExportMesh(Mesh mesh, const char *fileName);                                                 // Export mesh data to file
(defcfun "ExportMesh" :void
 "Export mesh data to file"
 (mesh (:struct %mesh))
 (file-name :string))

;;RLAPI void UnloadMesh(Mesh mesh);                                                                       // Unload mesh from memory (RAM and/or VRAM)
(defcfun "UnloadMesh" :void
 "Unload mesh from memory (RAM and/or VRAM)"
 (mesh (:struct %mesh)))

;;


;;// Model drawing functions
;;RLAPI void DrawModel(Model model, Vector3 position, float scale, Color tint);                           // Draw a model (with texture if set)
(defcfun "DrawModel" :void
  (model (:struct %model))
  (position (:struct %vector3))
  (scale (:struct %vector3))
  (tint :uint))

;;RLAPI void DrawModelEx(Model model, Vector3 position, Vector3 rotationAxis, float rotationAngle, Vector3 scale, Color tint); // Draw a model with extended parameters
(defcfun "DrawModelEx" :void
  (model (:struct %model))
  (position (:struct %vector3))
  (rotation-axis (:struct %vector3))
  (rotation-angle :float)
  (scale (:struct %vector3))
  (tint :uint))

;;RLAPI void DrawModelWires(Model model, Vector3 position, float scale, Color tint);                      // Draw a model wires (with texture if set)
(defcfun "DrawModelWires" :void
  (model (:struct %model))
  (position (:struct %vector3))
  (scale (:struct %vector3))
  (tint :uint))

;;RLAPI void DrawModelWiresEx(Model model, Vector3 position, Vector3 rotationAxis, float rotationAngle, Vector3 scale, Color tint); // Draw a model wires (with texture if set) with extended parameters
(defcfun "DrawModelWiresEx" :void
  (model (:struct %model))
  (position (:struct %vector3))
  (rotation-axis (:struct %vector3))
  (rotation-angle :float)
  (scale (:struct %vector3))
  (tint :uint))

;;RLAPI void DrawBoundingBox(BoundingBox box, Color color);                                               // Draw bounding box (wires)
(defcfun "DrawBoundingBox" :void
  (box (:struct %bounding-box))
  (color :uint))

;;RLAPI void DrawBillboard(Camera camera, Texture2D texture, Vector3 position, float size, Color tint);   // Draw a billboard texture
(defcfun "DrawBillboard" :void
  (camera (:struct %camera3d))
  (texture (:struct %texture))
  (position (:struct %vector3))
  (size :float)
  (tint :uint))

;;RLAPI void DrawBillboardRec(Camera camera, Texture2D texture, Rectangle source, Vector3 position, Vector2 size, Color tint); // Draw a billboard texture defined by source
(defcfun "DrawBillboardRec" :void
  (camera (:struct %camera3d))
  (texture (:struct %texture))
  (source (:struct %rectangle))
  (position (:struct %vector3))
  (size (:struct %vector2))
  (tint :uint))



;;
;;// Mesh generation functions
;;RLAPI Mesh GenMeshPoly(int sides, float radius);                                            // Generate polygonal mesh
(defcfun "GenMeshPoly" (:struct %mesh)
 "Generate polygonal mesh"
 (sides :int)
 (radius :float))

;;RLAPI Mesh GenMeshPlane(float width, float length, int resX, int resZ);                     // Generate plane mesh (with subdivisions)
(defcfun "GenMeshPlane" (:struct %mesh)
 "Generate plane mesh (with subdivisions)"
 (width :float)
 (length :float)
 (res-x :int)
 (res-z :int))

;;RLAPI Mesh GenMeshCube(float width, float height, float length);                            // Generate cuboid mesh
(defcfun "GenMeshCube" (:struct %mesh)
 "Generate cuboid mesh"
 (width :float)
 (height :float)
 (length :float))

;;RLAPI Mesh GenMeshSphere(float radius, int rings, int slices);                              // Generate sphere mesh (standard sphere)
(defcfun "GenMeshSphere" (:struct %mesh)
 "Generate sphere mesh (standard sphere)"
 (radius :float)
 (rings :int)
 (slices :int))

;;RLAPI Mesh GenMeshHemiSphere(float radius, int rings, int slices);                          // Generate half-sphere mesh (no bottom cap)
(defcfun "GenMeshHemiSphere" (:struct %mesh)
 "Generate half-sphere mesh (no bottom cap)"
 (radius :float)
 (rings :int)
 (slices :int))

;;RLAPI Mesh GenMeshCylinder(float radius, float height, int slices);                         // Generate cylinder mesh
(defcfun "GenMeshCylinder" (:struct %mesh)
 "Generate cylinder mesh"
 (radius :float)
 (height :float)
 (slices :int))

;;RLAPI Mesh GenMeshTorus(float radius, float size, int radSeg, int sides);                   // Generate torus mesh
(defcfun "GenMeshTorus" (:struct %mesh)
 "Generate torus mesh"
 (radius :float)
 (size :float)
 (rad-seg :int)
 (sides :int))

;;RLAPI Mesh GenMeshKnot(float radius, float size, int radSeg, int sides);                    // Generate trefoil knot mesh
(defcfun "GenMeshKnot" (:struct %mesh)
 "Generate trefoil knot mesh"
 (radius :float)
 (size :float)
 (rad-seg :int)
 (sides :int))

;;RLAPI Mesh GenMeshHeightmap(Image heightmap, Vector3 size);                                 // Generate heightmap mesh from image data
(defcfun "GenMeshHeightmap" (:struct %mesh)
 "Generate heightmap mesh from image data"
 (heightmap (:struct %image))
 (size (:struct %vector3)))

;;RLAPI Mesh GenMeshCubicmap(Image cubicmap, Vector3 cubeSize);                               // Generate cubes-based map mesh from image data
(defcfun "GenMeshCubicmap" (:struct %mesh)
 "Generate cubes-based map mesh from image data"
 (cubicmap (:struct %image))
 (cube-size (:struct %vector3)))

;;// Material loading/unloading functions
;;RLAPI Material *LoadMaterials(const char *fileName, int *materialCount);                    // Load materials from model file
(defcfun "LoadMaterials" (:struct %material)
 "Load materials from model file"
 (file-name :string)
 (material-count (:pointer :int)))

;;RLAPI Material LoadMaterialDefault(void);                                                   // Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)
(defcfun "LoadMaterialDefault" (:struct %material)
 "Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)")

;;RLAPI void UnloadMaterial(Material material);                                               // Unload material from GPU memory (VRAM)
(defcfun "UnloadMaterial" :void
 "Unload material from GPU memory (VRAM)"
 (material (:struct %material)))

;;RLAPI void SetMaterialTexture(Material *material, int mapType, Texture2D texture);          // Set texture for a material map type (MATERIAL_MAP_DIFFUSE, MATERIAL_MAP_SPECULAR...)
(defcfun "SetMaterialTexture" :void
 "Set texture for a material map type (MAP_DIFFUSE, MAP_SPECULAR...)"
 (material (:struct %material))
 (map-type :int)
 (texture (:struct %texture)))

;;RLAPI void SetModelMeshMaterial(Model *model, int meshId, int materialId);                  // Set material for a mesh
(defcfun "SetModelMeshMaterial" :void
 "Set material for a mesh"
 (model (:struct %model))
 (mesh-id :int)
 (material-id :int))

;;// Model animations loading/unloading functions
;;RLAPI ModelAnimation *LoadModelAnimations(const char *fileName, unsigned int *animCount);   // Load model animations from file
(defcfun "LoadModelAnimations" (:struct %model-animation)
 "Load model animations from file"
 (file-name :string)
 (animation-count (:pointer :unsigned-int)))

;;RLAPI void UpdateModelAnimation(Model model, ModelAnimation anim, int frame);               // Update model animation pose
(defcfun "UpdateModelAnimation" :void
 "Update model animation pose"
 (model (:struct %model))
 (anim (:struct %model-animation))
 (frame :int))

;;RLAPI void UnloadModelAnimation(ModelAnimation anim);                                       // Unload animation data
(defcfun "UnloadModelAnimation" :void
 "Unload animation data"
 (anim (:struct %model-animation)))

;;RLAPI bool IsModelAnimationValid(Model model, ModelAnimation anim);                         // Check model animation skeleton match
(defcfun "IsModelAnimationValid" :bool
 "Check model animation skeleton match"
 (model (:struct %model))
 (anim (:struct %model-animation)))

;;// Collision detection functions
;;RLAPI bool CheckCollisionSpheres(Vector3 center1, float radius1, Vector3 center2, float radius2);       // Check collision between two spheres
(defcfun "CheckCollisionSpheres" :bool
  (center-a (:struct %vector3))
  (radius-a :float)
  (center-b (:struct %vector3))
  (radius-b :float))

;;RLAPI bool CheckCollisionBoxes(BoundingBox box1, BoundingBox box2);                                     // Check collision between two bounding boxes
(defcfun "CheckCollisionBoxes" :bool
  (box1 (:struct %bounding-box))
  (box2 (:struct %bounding-box)))

;;RLAPI bool CheckCollisionBoxSphere(BoundingBox box, Vector3 center, float radius);                      // Check collision between box and sphere
(defcfun "CheckCollisionBoxSphere" :bool
  (box (:struct %bounding-box))
  (center (:struct %vector3))
  (radius :float))

;;RLAPI RayCollision GetRayCollisionModel(Ray ray, Model model);                                          // Get collision info between ray and model
(defcfun "GetRayCollisionModel" (:struct %ray-collision)
 "Get collision info between ray and model"
 (ray (:struct %ray))
 (model (:struct %model)))

;;RLAPI RayCollision GetRayCollisionTriangle(Ray ray, Vector3 p1, Vector3 p2, Vector3 p3);                // Get collision info between ray and triangle
(defcfun "GetRayCollisionTriangle" (:struct %ray-collision)
 "Get collision info between ray and triangle"
 (ray (:struct %ray))
 (p1 (:struct %vector3))
 (p2 (:struct %vector3))
 (p3 (:struct %vector3)))

;;RLAPI RayCollision GetRayCollisionQuad(Ray ray, Vector3 p1, Vector3 p2, Vector3 p3, Vector3 p4);        // Get collision info between ray and quad
(defcfun "GetRayCollisionQuad" (:struct %ray-collision)
  (ray (:struct %ray))
  (p1 (:struct %vector3))
  (p2 (:struct %vector3))
  (p3 (:struct %vector3))
  (p4 (:struct %vector3)))

;;//------------------------------------------------------------------------------------
;;// Audio Loading and Playing Functions (Module: audio)
;;//------------------------------------------------------------------------------------
;;
;;// Audio device management functions
;;RLAPI void InitAudioDevice(void);                                     // Initialize audio device and context
(defcfun "InitAudioDevice" :void
 "Initialize audio device and context")

;;RLAPI void CloseAudioDevice(void);                                    // Close the audio device and context
(defcfun "CloseAudioDevice" :void
 "Close the audio device and context")

;;RLAPI bool IsAudioDeviceReady(void);                                  // Check if audio device has been initialized successfully
(defcfun "IsAudioDeviceReady" :boolean
 "Check if audio device has been initialized successfully")

;;RLAPI void SetMasterVolume(float volume);                             // Set master volume (listener)
(defcfun "SetMasterVolume" :void
 "Set master volume (listener)"
 (volume :float))

;;// Wave/Sound loading/unloading functions
;;RLAPI Wave LoadWave(const char *fileName);                            // Load wave data from file
(defcfun "LoadWave" (:struct %wave)
 "Load wave data from file"
  (file-name :string))

;;RLAPI Wave LoadWaveFromMemory(const char *fileType, const unsigned char *fileData, int dataSize); // Load wave from memory buffer, fileType refers to extension: i.e. '.wav'
(defcfun "LoadWaveFromMemory" (:struct %wave)
  (file-type :string)
  (file-data :pointer)
  (data-size :int))

;;RLAPI Sound LoadSound(const char *fileName);                          // Load sound from file
(defcfun "LoadSound" (:struct %sound)
 "Load sound from file"
  (file-name :string))

;;RLAPI Sound LoadSoundFromWave(Wave wave);                             // Load sound from wave data
(defcfun "LoadSoundFromWave" (:struct %sound)
 "Load sound from wave data"
  (wave (:struct %wave)))

;;RLAPI void UpdateSound(Sound sound, const void *data, int sampleCount); // Update sound buffer with new data
(defcfun "UpdateSound" :void
 "Update sound buffer with new data"
  (sound (:struct %sound))
  (data :pointer)
  (sample-count :int))

;;RLAPI void UnloadWave(Wave wave);                                     // Unload wave data
(defcfun "UnloadWave" :void
 "Unload wave data"
  (wave (:struct %wave)))

;;RLAPI void UnloadSound(Sound sound);                                  // Unload sound
(defcfun "UnloadSound" :void
 "Unload sound"
 (sound (:struct %sound)))

;;RLAPI bool ExportWave(Wave wave, const char *fileName);               // Export wave data to file, returns true on success
(defcfun "ExportWave" :bool
 "Export wave data to file, return true on success"
 (wave (:struct %wave))
 (file-name :string))

;;RLAPI bool ExportWaveAsCode(Wave wave, const char *fileName);         // Export wave sample data to code (.h), returns true on success
(defcfun "ExportWaveAsCode" :bool
 "Export wave sample data to code (.h), return true on success"
 (wave (:struct %wave))
 (file-name :string))

;;// Wave/Sound management functions
;;RLAPI void PlaySound(Sound sound);                                    // Play a sound
(defcfun "PlaySound" :void
 "Play a sound"
  (sound (:struct %sound)))

;;RLAPI void StopSound(Sound sound);                                    // Stop playing a sound
(defcfun "StopSound" :void
 "Stop playing a sound"
 (sound (:struct %sound)))

;;RLAPI void PauseSound(Sound sound);                                   // Pause a sound
(defcfun "PauseSound" :void
 "Pause a sound"
  (sound (:struct %sound)))

;;RLAPI void ResumeSound(Sound sound);                                  // Resume a paused sound
(defcfun "ResumeSound" :void
 "Resume a paused sound"
  (sound (:struct %sound)))

;;RLAPI void PlaySoundMulti(Sound sound);                               // Play a sound (using multichannel buffer pool)
(defcfun "PlaySoundMulti" :void
 "Play a sound (using multichannel buffer pool)"
 (sound (:struct %sound)))

;;RLAPI void StopSoundMulti(void);                                      // Stop any sound playing (using multichannel buffer pool)
(defcfun "StopSoundMulti" :void
 "Stop any sound playing (using multichannel buffer pool)")

;;RLAPI int GetSoundsPlaying(void);                                     // Get number of sounds playing in the multichannel
(defcfun "GetSoundsPlaying" :int
 "Get number of sounds playing in the multichannel")

;;RLAPI bool IsSoundPlaying(Sound sound);                               // Check if a sound is currently playing
(defcfun "IsSoundPlaying" :bool
 "Check if a sound is currently playing"
  (sound (:struct %sound)))

;;RLAPI void SetSoundVolume(Sound sound, float volume);                 // Set volume for a sound (1.0 is max level)
(defcfun "SetSoundVolume" :void
 "Set volume for a sound (1.0 is max level)"
 (sound (:struct %sound))
 (volume :float))

;;RLAPI void SetSoundPitch(Sound sound, float pitch);                   // Set pitch for a sound (1.0 is base level)
(defcfun "SetSoundPitch" :void
 "Set pitch for a sound (1.0 is base level)"
 (sound (:struct %sound))
 (pitch :float))

;;RLAPI void WaveFormat(Wave *wave, int sampleRate, int sampleSize, int channels);  // Convert wave data to desired format
(defcfun "WaveFormat" :void
 "Convert wave data to desired format"
 (wave (:pointer (:struct %wave)))
 (sample-rate :int)
 (sample-size :int)
 (channels :int))

;;RLAPI Wave WaveCopy(Wave wave);                                       // Copy a wave to a new wave
(defcfun "WaveCopy" (:struct %wave)
 "Copy a wave to a new wave"
 (wave (:struct %wave)))

;;RLAPI void WaveCrop(Wave *wave, int initSample, int finalSample);     // Crop a wave to defined samples range
(defcfun "WaveCrop" :void
 "Crop a wave to defined samples range"
 (wave (:pointer (:struct %wave)))
 (init-sample :int)
 (final-sample :int))

;;RLAPI float *LoadWaveSamples(Wave wave);                              // Load samples data from wave as a floats array
(defcfun "GetWaveSamples" (:pointer :float)
 "Load samples data from wave as a floats array"
 (wave (:struct %wave)))

;;RLAPI void UnloadWaveSamples(float *samples);                         // Unload samples data loaded with LoadWaveSamples()
(defcfun "UnloadWaveSamples" :void
  (samples (:pointer :float)))

;;// Music management functions
;;RLAPI Music LoadMusicStream(const char *fileName);                    // Load music stream from file
(defcfun "LoadMusicStream" (:struct %music)
 "Load music stream from file"
 (file-name :string))

;;RLAPI Music LoadMusicStreamFromMemory(const char *fileType, unsigned char *data, int dataSize); // Load music stream from data
(defcfun "LoadMusicStreamFromMemory" (:struct %music)
  "Load music stream from data"
  (file-type :string)
  (data :pointer)
  (data-size :int))

;;RLAPI void UnloadMusicStream(Music music);                            // Unload music stream
(defcfun "UnloadMusicStream" :void
 "Unload music stream"
 (music (:struct %music)))

;;RLAPI void PlayMusicStream(Music music);                              // Start music playing
(defcfun "PlayMusicStream" :void
 "Start music playing"
 (music (:struct %music)))

;;RLAPI bool IsMusicStreamPlaying(Music music);                         // Check if music is playing
(defcfun "IsMusicStreamPlaying" :bool
  "Check if music is playing"
  (music (:struct %music)))

;;RLAPI void UpdateMusicStream(Music music);                            // Updates buffers for music streaming
(defcfun "UpdateMusicStream" :void
 "Updates buffers for music streaming"
 (music (:struct %music)))

;;RLAPI void StopMusicStream(Music music);                              // Stop music playing
(defcfun "StopMusicStream" :void
 "Stop music playing"
 (music (:struct %music)))

;;RLAPI void PauseMusicStream(Music music);                             // Pause music playing
(defcfun "PauseMusicStream" :void
 "Pause music playing"
 (music (:struct %music)))

;;RLAPI void ResumeMusicStream(Music music);                            // Resume playing paused music
(defcfun "ResumeMusicStream" :void
 "Resume playing paused music"
 (music (:struct %music)))

;;RLAPI void SeekMusicStream(Music music, float position);              // Seek music to a position (in seconds)
(defcfun "SeekMusicStream" :void
  "Seek music to a position (in seconds)"
  (music (:struct %music))
  (position :float))

;;RLAPI void SetMusicVolume(Music music, float volume);                 // Set volume for music (1.0 is max level)
(defcfun "SetMusicVolume" :void
 "Set volume for music (1.0 is max level)"
 (music (:struct %music))
 (volume :float))

;;RLAPI void SetMusicPitch(Music music, float pitch);                   // Set pitch for a music (1.0 is base level)
(defcfun "SetMusicPitch" :void
 "Set pitch for a music (1.0 is base level)"
 (music (:struct %music))
 (pitch :float))

;;RLAPI float GetMusicTimeLength(Music music);                          // Get music time length (in seconds)
(defcfun "GetMusicTimeLength" :float
 "Get music time length (in seconds)"
 (music (:struct %music)))

;;RLAPI float GetMusicTimePlayed(Music music);                          // Get current music time played (in seconds)
(defcfun "GetMusicTimePlayed" :float
 "Get current music time played (in seconds)"
 (music (:struct %music)))

;;// AudioStream management functions
;;RLAPI AudioStream LoadAudioStream(unsigned int sampleRate, unsigned int sampleSize, unsigned int channels); // Load audio stream (to stream raw audio pcm data)
(defcfun "LoadAudioStream" (:struct %audio-stream)
  "Init audio stream (to stream raw audio pcm data)"
  (sample-rate :unsigned-int)
  (sample-size :unsigned-int)
  (channels :unsigned-int))

;;RLAPI void UnloadAudioStream(AudioStream stream);                      // Unload audio stream and free memory
(defcfun "UnloadAudioStream" :void
 "Close audio stream and free memory"
 (stream (:struct %audio-stream)))

;;RLAPI void UpdateAudioStream(AudioStream stream, const void *data, int frameCount); // Update audio stream buffers with data
(defcfun "UpdateAudioStream" :void
 "Update audio stream buffers with data"
 (stream (:struct %audio-stream))
 (data :pointer)
 (frame-count :int))

;;RLAPI bool IsAudioStreamProcessed(AudioStream stream);                // Check if any audio stream buffers requires refill
(defcfun "IsAudioStreamProcessed" :bool
 "Check if any audio stream buffers requires refill"
 (stream (:struct %audio-stream)))

;;RLAPI void PlayAudioStream(AudioStream stream);                       // Play audio stream
(defcfun "PlayAudioStream" :void
 "Play audio stream"
 (stream (:struct %audio-stream)))

;;RLAPI void PauseAudioStream(AudioStream stream);                      // Pause audio stream
(defcfun "PauseAudioStream" :void
 "Pause audio stream"
 (stream (:struct %audio-stream)))

;;RLAPI void ResumeAudioStream(AudioStream stream);                     // Resume audio stream
(defcfun "ResumeAudioStream" :void
 "Resume audio stream"
 (stream (:struct %audio-stream)))

;;RLAPI bool IsAudioStreamPlaying(AudioStream stream);                  // Check if audio stream is playing
(defcfun "IsAudioStreamPlaying" :bool
 "Check if audio stream is playing"
 (stream (:struct %audio-stream)))

;;RLAPI void StopAudioStream(AudioStream stream);                       // Stop audio stream
(defcfun "StopAudioStream" :void
 "Stop audio stream"
 (stream (:struct %audio-stream)))

;;RLAPI void SetAudioStreamVolume(AudioStream stream, float volume);    // Set volume for audio stream (1.0 is max level)
(defcfun "SetAudioStreamVolume" :void
 "Set volume for audio stream (1.0 is max level)"
 (stream (:struct %audio-stream))
 (volume :float))

;;RLAPI void SetAudioStreamPitch(AudioStream stream, float pitch);      // Set pitch for audio stream (1.0 is base level)
(defcfun "SetAudioStreamPitch" :void
 "Set pitch for audio stream (1.0 is base level)"
 (stream (:struct %audio-stream))
 (pitch :float))

;;RLAPI void SetAudioStreamBufferSizeDefault(int size);                 // Default size for new audio streams
(defcfun "SetAudioStreamBufferSizeDefault" :void
 "Default size for new audio streams"
 (size :int))
;;
;;#if defined(__cplusplus)
;;}
;;#endif
;;
;;#endif // RAYLIB_H
