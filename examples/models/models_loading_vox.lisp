;;;; raylib [models] example - loading vox
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 4.0, last time updated with raylib 4.0
;;;;
;;;; Example contributed by Johann Nadalutti (@procfxgen) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2021-2025 Johann Nadalutti (@procfxgen) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_loading_vox.c

(require :cl-raylib)
(load (merge-pathnames "rlights.lisp" *load-truename*)) ; Required for: lights

(defpackage #:raylib-examples/models-loading-vox
  (:use #:cl #:raylib #:rlights))
(in-package #:raylib-examples/models-loading-vox)

(defconstant +max-vox-files+ 4)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450)

        (vox-file-names #("resources/models/vox/chr_knight.vox"
                          "resources/models/vox/chr_sword.vox"
                          "resources/models/vox/monu9.vox"
                          "resources/models/vox/fez.vox")))

    (init-window screen-width screen-height "raylib [models] example - loading vox")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 10.0 10.0 10.0) ; Camera position
                                 :target (vec3 0.0 0.0 0.0)      ; Camera looking at point
                                 :up (vec3 0.0 1.0 0.0)          ; Camera up vector (rotation towards target)
                                 :fovy 45.0                      ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera projection type

          ;; Load MagicaVoxel files
          (models (make-array +max-vox-files+)))

      (dotimes (i +max-vox-files+)
        ;; Load VOX file and measure time
        (let ((t0 (* (get-time) 1000d0)))
          (setf (aref models i) (load-model (aref vox-file-names i)))
          (let ((t1 (* (get-time) 1000d0)))
            (trace-log +log-info+ (text-format "[%s] Model file loaded in %.3f ms" (aref vox-file-names i) (- t1 t0)))))

        ;; Compute model translation matrix to center model on draw position (0, 0 , 0)
        (let* ((bb (get-model-bounding-box (aref models i)))
               (center (vec3 (+ (vx (bounding-box-min bb)) (/ (- (vx (bounding-box-max bb)) (vx (bounding-box-min bb))) 2))
                             0.0
                             (+ (vz (bounding-box-min bb)) (/ (- (vz (bounding-box-max bb)) (vz (bounding-box-min bb))) 2))))
               (mat-translate (matrix-translate (- (vx center)) 0 (- (vz center)))))
          (setf (model-transform (aref models i)) mat-translate)))

      (let* ((current-model 0)
             (modelpos (vec3 0.0 0.0 0.0))
             (camerarot (vec3 0.0 0.0 0.0))

             ;; Load voxel shader
             (shader (load-shader (text-format "resources/shaders/glsl%i/voxel_lighting.vs" +glsl-version+)
                                  (text-format "resources/shaders/glsl%i/voxel_lighting.fs" +glsl-version+)))
             (ambient-loc 0)
             (lights (make-array +max-lights+)))

        ;; Get some required shader locations
        (setf (aref (shader-locs shader) +shader-loc-vector-view+) (get-shader-location shader "viewPos"))
        ;; NOTE: "matModel" location name is automatically assigned on shader loading,
        ;; no need to get the location again if using that uniform name
        ;;(setf (aref (shader-locs shader) +shader-loc-matrix-model+) (get-shader-location shader "matModel"))

        ;; Ambient light level (some basic lighting)
        (setf ambient-loc (get-shader-location shader "ambient"))
        (set-shader-value shader ambient-loc '(0.1 0.1 0.1 1.0) +shader-uniform-vec4+)

        ;; Assign out lighting shader to model
        (dotimes (i +max-vox-files+)
          (dotimes (j (model-material-count (aref models i)))
            (setf (material-shader (aref (model-materials (aref models i)) j)) shader)))

        ;; Create lights
        (setf (aref lights 0) (create-light +light-point+ (vec3 -20.0 20.0 -20.0) (vector3-zero) +gray+ shader)
              (aref lights 1) (create-light +light-point+ (vec3 20.0 -20.0 20.0) (vector3-zero) +gray+ shader)
              (aref lights 2) (create-light +light-point+ (vec3 -20.0 20.0 20.0) (vector3-zero) +gray+ shader)
              (aref lights 3) (create-light +light-point+ (vec3 20.0 -20.0 -20.0) (vector3-zero) +gray+ shader))

        (set-target-fps 60)             ; Set our game to run at 60 frames-per-second
        ;;--------------------------------------------------------------------------------------

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Update
                 ;;----------------------------------------------------------------------------------
                 (if (is-mouse-button-down +mouse-button-middle+)
                     (let ((mouse-delta (get-mouse-delta)))
                       (setf (vx camerarot) (* (vx mouse-delta) 0.05)
                             (vy camerarot) (* (vy mouse-delta) 0.05)))
                     (setf (vx camerarot) 0.0
                           (vy camerarot) 0.0))

                 (flet ((down (k1 k2) (if (or (is-key-down k1) (is-key-down k2)) 1.0 0.0)))
                   ;; Update camere movement, custom controls
                   (update-camera-pro camera
                                      (vec3 (- (* (down +key-w+ +key-up+) 0.1) (* (down +key-s+ +key-down+) 0.1))     ; Move forward-backward
                                            (- (* (down +key-d+ +key-right+) 0.1) (* (down +key-a+ +key-left+) 0.1)) ; Move right-left
                                            0.0)                                                                      ; Move up-down
                                      camerarot                                                                       ; Camera rotation
                                      (* (get-mouse-wheel-move) -2.0)))                                               ; Move to target (zoom)

                 ;; Cycle between models on mouse click
                 (when (is-mouse-button-pressed +mouse-button-left+) (setf current-model (mod (1+ current-model) +max-vox-files+)))

                 ;; Update the shader with the camera view vector (points towards { 0.0f, 0.0f, 0.0f })
                 (let ((camera-pos (list (vx (camera3d-position camera)) (vy (camera3d-position camera)) (vz (camera3d-position camera)))))
                   (set-shader-value shader (aref (shader-locs shader) +shader-loc-vector-view+) camera-pos +shader-uniform-vec3+))

                 ;; Update light values (actually, only enable/disable them)
                 (dotimes (i +max-lights+) (update-light-values shader (aref lights i)))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 ;; Draw 3D model
                 (begin-mode-3d camera)

                 (draw-model (aref models current-model) modelpos 1.0 +white+)
                 (draw-grid 10 1.0)

                 ;; Draw spheres to show where the lights are
                 (dotimes (i +max-lights+)
                   (let ((light (aref lights i)))
                     (if (light-enabled light)
                         (draw-sphere-ex (light-position light) 0.2 8 8 (light-color light))
                         (draw-sphere-wires (light-position light) 0.2 8 8 (color-alpha (light-color light) 0.3)))))

                 (end-mode-3d)

                 ;; Display info
                 (draw-rectangle 10 40 340 70 (fade +skyblue+ 0.5))
                 (draw-rectangle-lines 10 40 340 70 (fade +darkblue+ 0.5))
                 (draw-text "- MOUSE LEFT BUTTON: CYCLE VOX MODELS" 20 50 10 +blue+)
                 (draw-text "- MOUSE MIDDLE BUTTON: ZOOM OR ROTATE CAMERA" 20 70 10 +blue+)
                 (draw-text "- UP-DOWN-LEFT-RIGHT KEYS: MOVE CAMERA" 20 90 10 +blue+)
                 (draw-text (text-format "VOX model file: %s" (get-file-name (aref vox-file-names current-model))) 10 10 20 +gray+)

                 (end-drawing))
        ;;----------------------------------------------------------------------------------

        ;; De-Initialization
        ;;--------------------------------------------------------------------------------------
        ;; Unload models data (GPU VRAM)
        (dotimes (i +max-vox-files+) (unload-model (aref models i)))

        ;; Unload shader data
        (unload-shader shader)

        (close-window)))))              ; Close window and OpenGL context

(main)
