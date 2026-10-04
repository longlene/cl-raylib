;;;; raylib [shaders] example - basic pbr
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 5.0, last time updated with raylib 5.5
;;;;
;;;; Example contributed by Afan OLOVCIC (@_DevDad) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2023-2025 Afan OLOVCIC (@_DevDad)
;;;;
;;;; Model: "Old Rusty Car" (https://skfb.ly/LxRy) by Renafox,
;;;; licensed under Creative Commons Attribution-NonCommercial
;;;; (http://creativecommons.org/licenses/by-nc/4.0/)
;;;; Common Lisp port of raylib/examples/shaders/shaders_basic_pbr.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-basic-pbr
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-basic-pbr)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

(defconstant +max-lights+ 4)            ; Max dynamic lights supported by shader

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; Light type
(defconstant +light-directional+ 0)
(defconstant +light-point+ 1)
(defconstant +light-spot+ 2)

;; Light data
(defstruct light
  (type 0)
  (enabled 0)
  (position (vec3 0.0 0.0 0.0))
  (target (vec3 0.0 0.0 0.0))
  (color (list 0.0 0.0 0.0 0.0))
  (intensity 0.0)

  ;; Shader light parameters locations
  (type-loc 0)
  (enabled-loc 0)
  (position-loc 0)
  (target-loc 0)
  (color-loc 0)
  (intensity-loc 0))

;;----------------------------------------------------------------------------------
;; Global Variables Definition
;;----------------------------------------------------------------------------------
(defvar *light-count* 0)                ; Current number of dynamic lights that have been created

;;----------------------------------------------------------------------------------
;; Module Functions Definition
;;----------------------------------------------------------------------------------
;; Send light properties to shader
;; NOTE: Light shader locations should be available
(defun update-light (shader light)
  (set-shader-value shader (light-enabled-loc light) (light-enabled light) +shader-uniform-int+)
  (set-shader-value shader (light-type-loc light) (light-type light) +shader-uniform-int+)

  ;; Send to shader light position values
  (let ((position (list (vx (light-position light)) (vy (light-position light)) (vz (light-position light)))))
    (set-shader-value shader (light-position-loc light) position +shader-uniform-vec3+))

  ;; Send to shader light target position values
  (let ((target (list (vx (light-target light)) (vy (light-target light)) (vz (light-target light)))))
    (set-shader-value shader (light-target-loc light) target +shader-uniform-vec3+))

  (set-shader-value shader (light-color-loc light) (light-color light) +shader-uniform-vec4+)
  (set-shader-value shader (light-intensity-loc light) (light-intensity light) +shader-uniform-float+))

;; Create light with provided data
;; NOTE: It updated the global lightCount and it's limited to MAX_LIGHTS
(defun create-light (type position target color intensity shader)
  (let ((light (make-light)))

    (when (< *light-count* +max-lights+)
      (setf (light-enabled light) 1
            (light-type light) type
            (light-position light) position
            (light-target light) target
            (light-color light) (list (/ (float (first color)) 255.0) (/ (float (second color)) 255.0)
                                      (/ (float (third color)) 255.0) (/ (float (fourth color)) 255.0))
            (light-intensity light) intensity)

      ;; NOTE: Shader parameters names for lights must match the requested ones
      (setf (light-enabled-loc light) (get-shader-location shader (text-format "lights[%i].enabled" *light-count*))
            (light-type-loc light) (get-shader-location shader (text-format "lights[%i].type" *light-count*))
            (light-position-loc light) (get-shader-location shader (text-format "lights[%i].position" *light-count*))
            (light-target-loc light) (get-shader-location shader (text-format "lights[%i].target" *light-count*))
            (light-color-loc light) (get-shader-location shader (text-format "lights[%i].color" *light-count*))
            (light-intensity-loc light) (get-shader-location shader (text-format "lights[%i].intensity" *light-count*)))

      (update-light shader light)

      (incf *light-count*))

    light))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-msaa-4x-hint+)
    (init-window screen-width screen-height "raylib [shaders] example - basic pbr")

    ;; Define the camera to look into our 3d world
    (let* ((camera (make-camera3d :position (vec3 2.0 2.0 6.0) ; Camera position
                                  :target (vec3 0.0 0.5 0.0)   ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)       ; Camera up vector (rotation towards target)
                                  :fovy 45.0                   ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera projection type

           ;; Load PBR shader and setup all required locations
           (shader (load-shader (text-format "resources/shaders/glsl%i/pbr.vs" +glsl-version+)
                                (text-format "resources/shaders/glsl%i/pbr.fs" +glsl-version+)))
           (locs (shader-locs shader))
           (light-count-loc 0)
           (max-light-count +max-lights+)

           ;; Setup ambient color and intensity parameters
           (ambient-intensity 0.02)
           (ambient-color '(26 32 135 255))
           (ambient-color-normalized (vec3 (/ (first ambient-color) 255.0) (/ (second ambient-color) 255.0) (/ (third ambient-color) 255.0)))

           (metallic-value-loc 0) (roughness-value-loc 0) (emissive-intensity-loc 0) (emissive-color-loc 0) (texture-tiling-loc 0)
           (car nil)
           (floor nil)
           (car-texture-tiling (vec2 0.5 0.5))
           (floor-texture-tiling (vec2 0.5 0.5))
           (lights (make-array +max-lights+))
           (usage 1))

      (setf (aref locs +shader-loc-map-albedo+) (get-shader-location shader "albedoMap"))
      ;; WARNING: Metalness, roughness, and ambient occlusion are all packed into a MRA texture
      ;; They are passed as to the SHADER_LOC_MAP_METALNESS location for convenience,
      ;; shader already takes care of it accordingly
      (setf (aref locs +shader-loc-map-metalness+) (get-shader-location shader "mraMap"))
      (setf (aref locs +shader-loc-map-normal+) (get-shader-location shader "normalMap"))
      ;; WARNING: Similar to the MRA map, the emissive map packs different information
      ;; into a single texture: it stores height and emission data
      ;; It is binded to SHADER_LOC_MAP_EMISSION location an properly processed on shader
      (setf (aref locs +shader-loc-map-emission+) (get-shader-location shader "emissiveMap"))
      (setf (aref locs +shader-loc-color-diffuse+) (get-shader-location shader "albedoColor"))

      ;; Setup additional required shader locations, including lights data
      (setf (aref locs +shader-loc-vector-view+) (get-shader-location shader "viewPos"))
      (setf light-count-loc (get-shader-location shader "numOfLights"))
      (set-shader-value shader light-count-loc max-light-count +shader-uniform-int+)

      (set-shader-value shader (get-shader-location shader "ambientColor") ambient-color-normalized +shader-uniform-vec3+)
      (set-shader-value shader (get-shader-location shader "ambient") ambient-intensity +shader-uniform-float+)

      ;; Get location for shader parameters that can be modified in real time
      (setf metallic-value-loc (get-shader-location shader "metallicValue")
            roughness-value-loc (get-shader-location shader "roughnessValue")
            emissive-intensity-loc (get-shader-location shader "emissivePower")
            emissive-color-loc (get-shader-location shader "emissiveColor")
            texture-tiling-loc (get-shader-location shader "tiling"))

      ;; Load old car model using PBR maps and shader
      ;; WARNING: We know this model consists of a single model.meshes[0] and
      ;; that model.materials[0] is by default assigned to that mesh
      ;; There could be more complex models consisting of multiple meshes and
      ;; multiple materials defined for those meshes... but always 1 mesh = 1 material
      (setf car (load-model "resources/models/old_car_new.glb"))

      ;; Assign already setup PBR shader to model.materials[0], used by models.meshes[0]
      (let* ((material (aref (model-materials car) 0))
             (maps (material-maps material)))
        (setf (material-shader material) shader)

        ;; Setup materials[0].maps default parameters
        (setf (material-map-color (aref maps +material-map-albedo+)) +white+
              (material-map-value (aref maps +material-map-metalness+)) 1.0
              (material-map-value (aref maps +material-map-roughness+)) 0.0
              (material-map-value (aref maps +material-map-occlusion+)) 1.0
              (material-map-color (aref maps +material-map-emission+)) '(255 162 0 255))

        ;; Setup materials[0].maps default textures
        (setf (material-map-texture (aref maps +material-map-albedo+)) (load-texture "resources/old_car_d.png")
              (material-map-texture (aref maps +material-map-metalness+)) (load-texture "resources/old_car_mra.png")
              (material-map-texture (aref maps +material-map-normal+)) (load-texture "resources/old_car_n.png")
              (material-map-texture (aref maps +material-map-emission+)) (load-texture "resources/old_car_e.png")))

      ;; Load floor model mesh and assign material parameters
      ;; NOTE: A basic plane shape can be generated instead of being loaded from a model file
      (setf floor (load-model "resources/models/plane.glb"))
      ;;(let ((floor-mesh (gen-mesh-plane 10 10 10 10)))
      ;;  (gen-mesh-tangents floor-mesh)    ; TODO: Review tangents generation
      ;;  (setf floor (load-model-from-mesh floor-mesh)))

      ;; Assign material shader for our floor model, same PBR shader
      (let* ((material (aref (model-materials floor) 0))
             (maps (material-maps material)))
        (setf (material-shader material) shader)

        (setf (material-map-color (aref maps +material-map-albedo+)) +white+
              (material-map-value (aref maps +material-map-metalness+)) 0.8
              (material-map-value (aref maps +material-map-roughness+)) 0.1
              (material-map-value (aref maps +material-map-occlusion+)) 1.0
              (material-map-color (aref maps +material-map-emission+)) +black+)

        (setf (material-map-texture (aref maps +material-map-albedo+)) (load-texture "resources/road_a.png")
              (material-map-texture (aref maps +material-map-metalness+)) (load-texture "resources/road_mra.png")
              (material-map-texture (aref maps +material-map-normal+)) (load-texture "resources/road_n.png")))

      ;; Models texture tiling parameter can be stored in the Material struct if required (CURRENTLY NOT USED)
      ;; NOTE: Material.params[4] are available for generic parameters storage (float)

      ;; Create some lights
      (setf (aref lights 0) (create-light +light-point+ (vec3 -1.0 1.0 -2.0) (vec3 0.0 0.0 0.0) +yellow+ 4.0 shader)
            (aref lights 1) (create-light +light-point+ (vec3 2.0 1.0 1.0) (vec3 0.0 0.0 0.0) +green+ 3.3 shader)
            (aref lights 2) (create-light +light-point+ (vec3 -2.0 1.0 1.0) (vec3 0.0 0.0 0.0) +red+ 8.3 shader)
            (aref lights 3) (create-light +light-point+ (vec3 1.0 1.0 -2.0) (vec3 0.0 0.0 0.0) +blue+ 2.0 shader))

      ;; Setup material texture maps usage in shader
      ;; NOTE: By default, the texture maps are always used
      (set-shader-value shader (get-shader-location shader "useTexAlbedo") usage +shader-uniform-int+)
      (set-shader-value shader (get-shader-location shader "useTexNormal") usage +shader-uniform-int+)
      (set-shader-value shader (get-shader-location shader "useTexMRA") usage +shader-uniform-int+)
      (set-shader-value shader (get-shader-location shader "useTexEmissive") usage +shader-uniform-int+)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;---------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-orbital+)

               ;; Update the shader with the camera view vector (points towards { 0.0f, 0.0f, 0.0f })
               (let ((camera-pos (list (vx (camera3d-position camera)) (vy (camera3d-position camera)) (vz (camera3d-position camera)))))
                 (set-shader-value shader (aref locs +shader-loc-vector-view+) camera-pos +shader-uniform-vec3+))

               ;; Check key inputs to enable/disable lights
               (flet ((toggle (i) (setf (light-enabled (aref lights i)) (if (= (light-enabled (aref lights i)) 0) 1 0))))
                 (when (is-key-pressed +key-one+) (toggle 2))
                 (when (is-key-pressed +key-two+) (toggle 1))
                 (when (is-key-pressed +key-three+) (toggle 3))
                 (when (is-key-pressed +key-four+) (toggle 0)))

               ;; Update light values on shader (actually, only enable/disable them)
               (dotimes (i +max-lights+) (update-light shader (aref lights i)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +black+)

               (begin-mode-3d camera)

               (let ((floor-maps (material-maps (aref (model-materials floor) 0)))
                     (car-maps (material-maps (aref (model-materials car) 0))))
                 ;; Set floor model texture tiling and emissive color parameters on shader
                 (set-shader-value shader texture-tiling-loc floor-texture-tiling +shader-uniform-vec2+)
                 (let ((floor-emissive-color (color-normalize (material-map-color (aref floor-maps +material-map-emission+)))))
                   (set-shader-value shader emissive-color-loc floor-emissive-color +shader-uniform-vec4+))

                 ;; Set floor metallic and roughness values
                 (set-shader-value shader metallic-value-loc (material-map-value (aref floor-maps +material-map-metalness+)) +shader-uniform-float+)
                 (set-shader-value shader roughness-value-loc (material-map-value (aref floor-maps +material-map-roughness+)) +shader-uniform-float+)

                 (draw-model floor (vec3 0.0 0.0 0.0) 5.0 +white+) ; Draw floor model

                 ;; Set old car model texture tiling, emissive color and emissive intensity parameters on shader
                 (set-shader-value shader texture-tiling-loc car-texture-tiling +shader-uniform-vec2+)
                 (let ((car-emissive-color (color-normalize (material-map-color (aref car-maps +material-map-emission+)))))
                   (set-shader-value shader emissive-color-loc car-emissive-color +shader-uniform-vec4+))
                 (let ((emissive-intensity 0.01))
                   (set-shader-value shader emissive-intensity-loc emissive-intensity +shader-uniform-float+))

                 ;; Set old car metallic and roughness values
                 (set-shader-value shader metallic-value-loc (material-map-value (aref car-maps +material-map-metalness+)) +shader-uniform-float+)
                 (set-shader-value shader roughness-value-loc (material-map-value (aref car-maps +material-map-roughness+)) +shader-uniform-float+)

                 (draw-model car (vec3 0.0 0.0 0.0) 0.25 +white+)) ; Draw car model

               ;; Draw spheres to show the lights positions
               (dotimes (i +max-lights+)
                 (let* ((light (aref lights i))
                        (light-color (mapcar (lambda (c) (truncate (* c 255))) (light-color light))))
                   (if (/= (light-enabled light) 0)
                       (draw-sphere-ex (light-position light) 0.2 8 8 light-color)
                       (draw-sphere-wires (light-position light) 0.2 8 8 (color-alpha light-color 0.3)))))

               (end-mode-3d)

               (draw-text "Toggle lights: [1][2][3][4]" 10 40 20 +lightgray+)

               (draw-text "(c) Old Rusty Car model by Renafox (https://skfb.ly/LxRy)" (- screen-width 320) (- screen-height 20) 10 +lightgray+)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      ;; Unbind (disconnect) shader from car.material[0]
      ;; to avoid UnloadMaterial() trying to unload it automatically
      (dolist (model (list car floor))
        (let ((material (aref (model-materials model) 0)))
          (setf (material-shader material) (make-shader))
          (unload-material material)
          (setf (material-maps material) nil)
          (unload-model model)))

      (unload-shader shader)            ; Unload Shader

      (close-window))))                 ; Close window and OpenGL context

(main)
