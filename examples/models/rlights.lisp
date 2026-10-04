;;;; raylib.lights - Some useful functions to deal with lights data
;;;;
;;;; LICENSE: zlib/libpng
;;;;
;;;; Copyright (c) 2017-2024 Victor Fisac (@victorfisac) and Ramon Santamaria (@raysan5)
;;;;
;;;; This software is provided "as-is", without any express or implied warranty. In no event
;;;; will the authors be held liable for any damages arising from the use of this software.
;;;;
;;;; Permission is granted to anyone to use this software for any purpose, including commercial
;;;; applications, and to alter it and redistribute it freely, subject to the following restrictions:
;;;;
;;;;   1. The origin of this software must not be misrepresented; you must not claim that you
;;;;   wrote the original software. If you use this software in a product, an acknowledgment
;;;;   in the product documentation would be appreciated but is not required.
;;;;
;;;;   2. Altered source versions must be plainly marked as such, and must not be misrepresented
;;;;   as being the original software.
;;;;
;;;;   3. This notice may not be removed or altered from any source distribution.
;;;;
;;;; Common Lisp port of raylib/examples/models/rlights.h (same as examples/shaders/rlights.h)

(require :cl-raylib)

(defpackage #:rlights
  (:use #:cl #:raylib)
  (:export #:+max-lights+ #:+light-directional+ #:+light-point+
           #:light #:make-light #:light-type #:light-enabled #:light-position #:light-target
           #:light-color #:light-attenuation #:light-enabled-loc #:light-type-loc #:light-position-loc
           #:light-target-loc #:light-color-loc #:light-attenuation-loc
           #:create-light #:update-light-values))
(in-package #:rlights)

;;----------------------------------------------------------------------------------
;; Defines and Macros
;;----------------------------------------------------------------------------------
(defconstant +max-lights+ 4)            ; Max dynamic lights supported by shader

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------

;; Light data
(defstruct light
  (type 0)
  (enabled nil)
  (position (vec3 0.0 0.0 0.0))
  (target (vec3 0.0 0.0 0.0))
  (color (list 0 0 0 0))
  (attenuation 0.0)

  ;; Shader locations
  (enabled-loc 0)
  (type-loc 0)
  (position-loc 0)
  (target-loc 0)
  (color-loc 0)
  (attenuation-loc 0))

;; Light type
(defconstant +light-directional+ 0)
(defconstant +light-point+ 1)

;;----------------------------------------------------------------------------------
;; Global Variables Definition
;;----------------------------------------------------------------------------------
(defvar *lights-count* 0)               ; Current amount of created lights

;;----------------------------------------------------------------------------------
;; Module Functions Definition
;;----------------------------------------------------------------------------------

;; Create a light and get shader locations
(defun create-light (type position target color shader)
  (let ((light (make-light)))

    (when (< *lights-count* +max-lights+)
      (setf (light-enabled light) t
            (light-type light) type
            (light-position light) position
            (light-target light) target
            (light-color light) color)

      ;; NOTE: Lighting shader naming must be the provided ones
      (setf (light-enabled-loc light) (get-shader-location shader (text-format "lights[%i].enabled" *lights-count*))
            (light-type-loc light) (get-shader-location shader (text-format "lights[%i].type" *lights-count*))
            (light-position-loc light) (get-shader-location shader (text-format "lights[%i].position" *lights-count*))
            (light-target-loc light) (get-shader-location shader (text-format "lights[%i].target" *lights-count*))
            (light-color-loc light) (get-shader-location shader (text-format "lights[%i].color" *lights-count*)))

      (update-light-values shader light)

      (incf *lights-count*))

    light))

;; Send light properties to shader
;; NOTE: Light shader locations should be available
(defun update-light-values (shader light)
  ;; Send to shader light enabled state and type
  (set-shader-value shader (light-enabled-loc light) (if (light-enabled light) 1 0) +shader-uniform-int+)
  (set-shader-value shader (light-type-loc light) (light-type light) +shader-uniform-int+)

  ;; Send to shader light position values
  (let ((position (list (vx (light-position light)) (vy (light-position light)) (vz (light-position light)))))
    (set-shader-value shader (light-position-loc light) position +shader-uniform-vec3+))

  ;; Send to shader light target position values
  (let ((target (list (vx (light-target light)) (vy (light-target light)) (vz (light-target light)))))
    (set-shader-value shader (light-target-loc light) target +shader-uniform-vec3+))

  ;; Send to shader light color values
  (destructuring-bind (r g b a) (light-color light)
    (let ((color (list (/ (float r) 255.0) (/ (float g) 255.0) (/ (float b) 255.0) (/ (float a) 255.0))))
      (set-shader-value shader (light-color-loc light) color +shader-uniform-vec4+))))
