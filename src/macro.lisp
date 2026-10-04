(in-package #:cl-raylib)

;;; Convenience macros (cl-raylib.cffi API, not part of raylib)
;;; Each one calls the Begin*/Init*/Load* function first and then makes sure the matching
;;; End*/Close*/Unload* function runs, even on a non-local exit from BODY (unwind-protect)

(defmacro with-window ((width height title) &body body)
  "InitWindow(), BODY, CloseWindow()"
  `(progn (init-window ,width ,height ,title)
          (unwind-protect (progn ,@body)
            (close-window))))

(defmacro with-drawing (&body body)
  "BeginDrawing(), BODY, EndDrawing()"
  `(progn (begin-drawing)
          (unwind-protect (progn ,@body)
            (end-drawing))))

(defmacro with-mode-2d ((camera) &body body)
  "BeginMode2D(camera), BODY, EndMode2D()"
  `(progn (begin-mode-2d ,camera)
          (unwind-protect (progn ,@body)
            (end-mode-2d))))

(defmacro with-mode-3d ((camera) &body body)
  "BeginMode3D(camera), BODY, EndMode3D()"
  `(progn (begin-mode-3d ,camera)
          (unwind-protect (progn ,@body)
            (end-mode-3d))))

(defmacro with-texture-mode ((target) &body body)
  "BeginTextureMode(target), BODY, EndTextureMode()"
  `(progn (begin-texture-mode ,target)
          (unwind-protect (progn ,@body)
            (end-texture-mode))))

(defmacro with-shader-mode ((shader) &body body)
  "BeginShaderMode(shader), BODY, EndShaderMode()"
  `(progn (begin-shader-mode ,shader)
          (unwind-protect (progn ,@body)
            (end-shader-mode))))

(defmacro with-blend-mode ((mode) &body body)
  "BeginBlendMode(mode), BODY, EndBlendMode()"
  `(progn (begin-blend-mode ,mode)
          (unwind-protect (progn ,@body)
            (end-blend-mode))))

(defmacro with-scissor-mode ((x y width height) &body body)
  "BeginScissorMode(x, y, width, height), BODY, EndScissorMode()"
  `(progn (begin-scissor-mode ,x ,y ,width ,height)
          (unwind-protect (progn ,@body)
            (end-scissor-mode))))

;; NOTE: cl-raylib.cffi's with-vr-simulator/with-vr-drawing wrap InitVrSimulator()/BeginVrDrawing(),
;; removed from raylib (replaced by LoadVrStereoConfig()/BeginVrStereoMode())
(defmacro with-vr-stereo-mode ((config) &body body)
  "BeginVrStereoMode(config), BODY, EndVrStereoMode()"
  `(progn (begin-vr-stereo-mode ,config)
          (unwind-protect (progn ,@body)
            (end-vr-stereo-mode))))

(defmacro with-audio-device (&body body)
  "InitAudioDevice(), BODY, CloseAudioDevice()"
  `(progn (init-audio-device)
          (unwind-protect (progn ,@body)
            (close-audio-device))))

(defmacro with-audio-stream ((stream sample-rate sample-size channels) &body body)
  "Bind STREAM to LoadAudioStream(sample-rate, sample-size, channels) around BODY, then UnloadAudioStream()"
  `(let ((,stream (load-audio-stream ,sample-rate ,sample-size ,channels)))
     (unwind-protect (progn ,@body)
       (unload-audio-stream ,stream))))

(defmacro with-sound ((sound file-name) &body body)
  "Bind SOUND to LoadSound(file-name) around BODY, then UnloadSound()"
  `(let ((,sound (load-sound ,file-name)))
     (unwind-protect (progn ,@body)
       (unload-sound ,sound))))
