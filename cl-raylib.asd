(asdf:defsystem #:cl-raylib
  :version "0.1.0"
  :author "loong0"
  :license "MIT"
  :description "Common Lisp implementation of Raylib - modular architecture matching raylib C structure"
  :depends-on (#:cl-opengl
               #:cl-glu
               #:glfw
               #:float-features
               #:3d-matrices
               #:3d-vectors
               #:3d-transforms
               #:3d-quaternions
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
   (:file "shaders")               ; Shader system (matches raylib rlgl.c shader functionality)
   (:file "utils")                 ; Utility functions and logging system (required for raylib compatibility)
   (:file "shapes")
   (:file "shapes3d")
   (:file "collision")             ; Collision detection (stateless, matches raylib design)
   (:file "models")                ; 3D models and meshes (matching raylib rmodels.c)
   (:file "text")                  ; Text rendering system (fixed implementation)
   (:file "audio")                 ; Audio system
   (:file "raygui")                ; GUI system (immediate mode GUI)
   (:file "macro")))
