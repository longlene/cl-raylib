(asdf:defsystem #:cl-raylib
  :version "0.1.0"
  :author "loong0"
  :license "MIT"
  :description "Common Lisp implementation of Raylib - modular architecture matching raylib C structure"
  :depends-on (#:glfw
               #:float-features
               #:cffi
               #:bordeaux-threads
               #:3d-matrices
               #:3d-vectors
               #:alexandria
               #:uiop
               #:imago
               #:ieee-floats
               #:babel
               #:flexi-streams
               #:skippy
               #:zpng
               #:chipz
               #:salza2)
  :serial t
  :pathname "src"
  :components
  (;; Package definition
   (:file "package")
   (:file "raylib")
   (:file "math")                  ; raymath.h (included by rcore.c)
   (:file "gl")                    ; OpenGL abstraction layer (matches raylib rlgl.h functionality)
   (:file "gestures")              ; rgestures.h (included by rcore.c)
   (:file "core")                  ; rcore.c
   (:file "glfw")                  ; platforms/rcore_desktop_glfw.c (included by rcore.c after CORE data)
   (:file "camera2d")              ; 2D camera system (matches raylib Camera2D)
   (:file "camera3d")
   (:file "color")
   (:file "textures")
   (:file "utils")                 ; Utility functions and logging system (required for raylib compatibility)
   (:file "shapes")
   (:file "par-shapes")            ; external/par_shapes.h (used by rmodels.c)
   (:file "tinyobj")               ; external/tinyobj_loader_c.h (used by rmodels.c)
   (:file "vox")                   ; external/vox_loader.h (used by rmodels.c)
   (:file "gltf")                  ; external/cgltf.h (used by rmodels.c)
   (:file "m3d")                   ; external/m3d.h (used by rmodels.c)
   (:file "models")                ; rmodels.c
   (:file "truetype")              ; stb_truetype.h + stb_rect_pack.h (used by rtext.c)
   (:file "text")                  ; rtext.c
   (:file "miniaudio")             ; miniaudio.h subset: data conversion + PulseAudio playback device
   (:file "wav")                   ; dr_wav.h (used by raudio.c)
   (:file "vorbis")                ; stb_vorbis.c (used by raudio.c)
   (:file "mp3")                   ; dr_mp3.h (used by raudio.c)
   (:file "xm")                    ; jar_xm.h (used by raudio.c)
   (:file "mod")                   ; jar_mod.h (used by raudio.c)
   (:file "qoa")                   ; qoa.h + qoaplay.c (used by raudio.c)
   (:file "flac")                  ; dr_flac.h replacement (used by raudio.c)
   (:file "audio")                 ; raudio.c
   (:file "raygui")                ; GUI system (immediate mode GUI)
   (:file "macro")))
