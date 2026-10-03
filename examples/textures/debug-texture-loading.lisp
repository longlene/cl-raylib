(ql:quickload :cl-raylib :silent t)
(in-package :cl-raylib)

(init-window 400 300 "Debug Texture Loading")

(format t "Current working directory: ~a~%" (uiop:getcwd))
(format t "File exists: ~a~%" (probe-file "resources/cyberpunk_street_background.png"))

(let ((tex (load-texture "resources/cyberpunk_street_background.png")))
  (format t "Loaded texture: ~a~%" tex)
  (format t "Texture ID: ~d~%" (texture-id tex))
  (format t "Texture width: ~d~%" (texture-width tex))
  (format t "Texture height: ~d~%" (texture-height tex))
  (format t "Texture format: ~d~%" (texture-format tex))
  (format t "Texture mipmaps: ~d~%" (texture-mipmaps tex))
  
  ;; Simple test - draw the texture
  (dotimes (i 10)
    (begin-drawing)
      (clear-background +black+)
      (draw-texture tex 10 10 +white+)
      (draw-text "Press ESC to exit" 10 270 20 +white+)
    (end-drawing)
    (sleep 0.1))
  
  (unload-texture tex))

(close-window)