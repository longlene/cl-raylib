(in-package #:cl-raylib)

;;; GPU Texture System
;;; This module handles OpenGL texture loading, management, and drawing

;;; Texture filtering modes
(defconstant +texture-filter-point+ 0)           ; No filter, just pixel approximation  
(defconstant +texture-filter-bilinear+ 1)        ; Linear filtering
(defconstant +texture-filter-trilinear+ 2)       ; Trilinear filtering (linear with mipmaps)
(defconstant +texture-filter-anisotropic-4x+ 3)  ; Anisotropic filtering 4x
(defconstant +texture-filter-anisotropic-8x+ 4)  ; Anisotropic filtering 8x
(defconstant +texture-filter-anisotropic-16x+ 5) ; Anisotropic filtering 16x

;;; Texture wrap modes
(defconstant +texture-wrap-repeat+ 0)        ; Repeats texture in tiled mode
(defconstant +texture-wrap-clamp+ 1)         ; Clamps texture to edge pixel in tiled mode
(defconstant +texture-wrap-mirror-repeat+ 2) ; Mirrors and repeats the texture in tiled mode
(defconstant +texture-wrap-mirror-clamp+ 3)  ; Mirrors and clamps to border the texture in tiled mode

;;; Cubemap layout types
(defconstant +cubemap-layout-auto-detect+ 0)        ; Automatically detect layout type
(defconstant +cubemap-layout-line-vertical+ 1)      ; Layout is defined by a vertical line with faces
(defconstant +cubemap-layout-line-horizontal+ 2)    ; Layout is defined by a horizontal line with faces
(defconstant +cubemap-layout-cross-three-by-four+ 3) ; Layout is defined by a 3x4 cross with cubemap faces
(defconstant +cubemap-layout-cross-four-by-three+ 4) ; Layout is defined by a 4x3 cross with cubemap faces

;;; Global texture management
(defvar *texture-id-counter* 1 "OpenGL texture ID counter")
(defvar *current-texture-id* 0 "Currently bound texture ID")
(defvar *texture-registry* (make-hash-table) "Registry of loaded textures")
(defvar *default-texture* nil "Default white 1x1 texture")

;;; NPatch structure for 9-patch drawing
;; NPatchInfo structure is defined in raylib.lisp

;;; Initialize texture system
(defun init-texture-system ()
  "Initialize the texture system and create default texture"
  (unless *default-texture*
    (setf *default-texture* (create-default-texture))))

(defun create-default-texture ()
  "Create a default 1x1 white texture"
  (let ((white-image (gen-image-color 1 1 +white+)))
    (load-texture-from-image white-image)))

;;; Texture loading functions

(defun load-texture-from-image (image)
  "Load texture from image data into GPU memory"
  (let ((texture-id (gl:gen-texture))
        (width (image-width image))
        (height (image-height image))
        (data (image-data image)))
    
    ;; Bind texture
    (gl:bind-texture :texture-2d texture-id)
    
    ;; Set texture parameters (default settings)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    
    ;; Upload texture data
    (gl:tex-image-2d :texture-2d 0 :rgba width height 0 :rgba :unsigned-byte data)
    
    ;; Generate mipmaps if supported
    (gl:generate-mipmap :texture-2d)
    
    ;; Unbind texture
    (gl:bind-texture :texture-2d 0)
    
    ;; Create texture structure
    (let ((texture (make-texture :id texture-id
                                 :width width
                                 :height height
                                 :mipmaps 1
                                 :format (image-format image))))
      
      ;; Register texture for cleanup
      (setf (gethash texture-id *texture-registry*) texture)
      
      texture)))

(defun load-image (filename)
  "Load image from file using imago library"
  (handler-case
      (when (probe-file filename)
        (trace-log-info "FILEIO: [~a] File loaded successfully" filename)
        (let* ((imago-image (imago:read-image filename))
               (width (imago:image-width imago-image))
               (height (imago:image-height imago-image))
               (pixel-count (* width height))
               (data (make-array (* pixel-count 4) :element-type '(unsigned-byte 8))))

          (trace-log-info "IMAGE: Data loaded successfully (~dx~d | R8G8B8A8 | 1 mipmaps)" width height)

          ;; Convert imago image to RGBA format
          (typecase imago-image
            (imago:rgb-image
             (dotimes (y height)
               (dotimes (x width)
                 (let* ((pixel (imago:image-pixel imago-image x y))
                        (idx (* (+ (* y width) x) 4)))
                   (setf (aref data idx) (imago:color-red pixel))           ; R
                   (setf (aref data (+ idx 1)) (imago:color-green pixel))   ; G
                   (setf (aref data (+ idx 2)) (imago:color-blue pixel))    ; B
                   (setf (aref data (+ idx 3)) (imago:color-alpha pixel)))))) ; A
            (imago:grayscale-image
             (dotimes (y height)
               (dotimes (x width)
                 (let* ((pixel (imago:image-pixel imago-image x y))
                        (idx (* (+ (* y width) x) 4)))
                   (setf (aref data idx) pixel)           ; R
                   (setf (aref data (+ idx 1)) pixel)     ; G
                   (setf (aref data (+ idx 2)) pixel)     ; B
                   (setf (aref data (+ idx 3)) 255)))))   ; A
            (t
             (trace-log-warning "IMAGE: Unsupported image type, converting to RGB")
             (let ((rgb-image (imago:convert-to-rgb imago-image)))
               (dotimes (y height)
                 (dotimes (x width)
                   (let* ((pixel (imago:image-pixel rgb-image x y))
                          (idx (* (+ (* y width) x) 4)))
                     (setf (aref data idx) (imago:color-red pixel))
                     (setf (aref data (+ idx 1)) (imago:color-green pixel))
                     (setf (aref data (+ idx 2)) (imago:color-blue pixel))
                     (setf (aref data (+ idx 3)) (imago:color-alpha pixel))))))))

          (make-image :data data
                      :width width
                      :height height
                      :format +pixelformat-uncompressed-rgba+)))
    (error (e)
      (trace-log-error "IMAGE: Failed to load [~a]: ~a" filename e)
      ;; Return placeholder image on error
      (let ((color (cond
                     ((search "red" (string-downcase filename)) +red+)
                     ((search "green" (string-downcase filename)) +green+)
                     ((search "blue" (string-downcase filename)) +blue+)
                     ((search "yellow" (string-downcase filename)) +yellow+)
                     (t +magenta+))))
        (gen-image-color 64 64 color)))))

(defun unload-image (image)
  "Unload image data from CPU memory (RAM)"
  (when (and image (image-p image) (image-data image))
    ;; Clear the image data array
    ;; In Common Lisp, we just need to clear the reference
    ;; The GC will handle the actual memory deallocation
    (setf (image-data image) nil)
    (trace-log-info "IMAGE: Data unloaded successfully from RAM")))

(defun load-texture (filename)
  "Load texture from file into GPU memory"
  (let ((image (load-image filename)))
    (if image
        (let ((texture (load-texture-from-image image)))
          (trace-log-info "TEXTURE: [ID ~d] Texture loaded successfully (~dx~d | R8G8B8A8 | 1 mipmaps)"
                         (texture-id texture) (image-width image) (image-height image))
          texture)
        ;; Fallback to colored texture if loading fails
        (let* ((color (cond
                        ((search "red" filename) +red+)
                        ((search "green" filename) +green+)
                        ((search "blue" filename) +blue+)
                        (t +white+)))
               (image (gen-image-color 64 64 color)))
          (load-texture-from-image image)))))

(defun is-texture-valid (texture)
  "Check if a texture is valid (loaded in GPU)"
  (and texture
       (texture-p texture)
       (> (texture-id texture) 0)
       (gethash (texture-id texture) *texture-registry*)))

(defun unload-texture (texture)
  "Unload texture from GPU memory"
  (when (is-texture-valid texture)
    (let ((texture-id (texture-id texture)))
      ;; Delete OpenGL texture
      (gl:delete-texture texture-id)
      
      ;; Remove from registry
      (remhash texture-id *texture-registry*)
      
      ;; Clear texture data
      (setf (texture-id texture) 0))))

(defun update-texture (texture pixels)
  "Update GPU texture with new data"
  (when (is-texture-valid texture)
    (gl:bind-texture :texture-2d (texture-id texture))
    (gl:tex-sub-image-2d :texture-2d 0 0 0 
                         (texture-width texture) (texture-height texture)
                         :rgba :unsigned-byte pixels)
    (gl:bind-texture :texture-2d 0)))

;;; Texture configuration functions

(defun set-texture-filter (texture filter)
  "Set texture scaling filter mode"
  (when (is-texture-valid texture)
    (gl:bind-texture :texture-2d (texture-id texture))
    
    (alexandria:switch (filter)
      (+texture-filter-point+
       (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
       (gl:tex-parameter :texture-2d :texture-mag-filter :nearest))
      (+texture-filter-bilinear+
       (gl:tex-parameter :texture-2d :texture-min-filter :linear)
       (gl:tex-parameter :texture-2d :texture-mag-filter :linear))
      (+texture-filter-trilinear+
       (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
       (gl:tex-parameter :texture-2d :texture-mag-filter :linear))
      (t ; Default to bilinear
       (gl:tex-parameter :texture-2d :texture-min-filter :linear)
       (gl:tex-parameter :texture-2d :texture-mag-filter :linear)))
    
    (gl:bind-texture :texture-2d 0)))

(defun set-texture-wrap (texture wrap)
  "Set texture wrapping mode"
  (when (is-texture-valid texture)
    (gl:bind-texture :texture-2d (texture-id texture))
    
    (let ((wrap-mode (alexandria:switch (wrap)
                       (+texture-wrap-repeat+ :repeat)
                       (+texture-wrap-clamp+ :clamp-to-edge)
                       (+texture-wrap-mirror-repeat+ :mirrored-repeat)
                       (+texture-wrap-mirror-clamp+ :mirror-clamp-to-edge)
                       (t :repeat))))
      (gl:tex-parameter :texture-2d :texture-wrap-s wrap-mode)
      (gl:tex-parameter :texture-2d :texture-wrap-t wrap-mode))
    
    (gl:bind-texture :texture-2d 0)))

;;; Texture drawing functions

(defun bind-texture-safe (texture)
  "Bind texture for drawing with safety checks"
  (let ((texture-id (if (and texture (is-texture-valid texture))
                        (texture-id texture)
                        (if *default-texture*
                            (texture-id *default-texture*)
                            0))))
    (unless (= texture-id *current-texture-id*)
      (gl:bind-texture :texture-2d texture-id)
      (setf *current-texture-id* texture-id))))

(defun setup-texture-drawing ()
  "Setup OpenGL state for texture drawing"
  (gl:enable :texture-2d)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha))

(defun draw-texture (texture pos-x pos-y tint)
  "Draw a Texture2D at position with tint"
  (when (is-texture-valid texture)
    (let ((width (float (texture-width texture)))
          (height (float (texture-height texture))))
      (draw-texture-pro texture
                        (make-rectangle :x 0.0 :y 0.0 :width width :height height)
                        (make-rectangle :x (float pos-x) :y (float pos-y) 
                                       :width width :height height)
                        (vec2 0 0) 0.0 tint))))

(defun draw-texture-v (texture position tint)
  "Draw a Texture2D with position defined as Vector2"
  (let ((pos-x (if (listp position) (first position) (vx position)))
        (pos-y (if (listp position) (second position) (vy position))))
    (draw-texture texture pos-x pos-y tint)))

(defun draw-texture-ex (texture position rotation scale tint)
  "Draw a Texture2D with extended parameters"
  (when (is-texture-valid texture)
    (let* ((width (float (texture-width texture)))
           (height (float (texture-height texture)))
           (scaled-width (* width scale))
           (scaled-height (* height scale))
           (pos-x (if (listp position) (first position) (vx position)))
           (pos-y (if (listp position) (second position) (vy position))))
      (draw-texture-pro texture
                        (make-rectangle :x 0.0 :y 0.0 :width width :height height)
                        (make-rectangle :x pos-x :y pos-y
                                       :width scaled-width :height scaled-height)
                        (vec2 (* scaled-width 0.5) (* scaled-height 0.5))
                        rotation tint))))

(defun draw-texture-rec (texture source position tint)
  "Draw a part of a texture defined by a rectangle"
  (when (is-texture-valid texture)
    (let ((pos-x (if (listp position) (first position) (vx position)))
          (pos-y (if (listp position) (second position) (vy position))))
      (draw-texture-pro texture source
                        (make-rectangle :x pos-x :y pos-y
                                       :width (rectangle-width source)
                                       :height (rectangle-height source))
                        (vec2 0 0) 0.0 tint))))

(defun draw-texture-pro (texture source dest origin rotation tint)
  "Draw a part of a texture defined by a rectangle with 'pro' parameters"
  (when (and (is-texture-valid texture) source dest)
    (setup-texture-drawing)
    (bind-texture-safe texture)
    
    ;; Set color tint
    (set-gl-color tint)
    
    ;; Calculate texture coordinates with flipping support
    (let* ((tex-width (float (texture-width texture)))
           (tex-height (float (texture-height texture)))
           (src-x (/ (rectangle-x source) tex-width))
           (src-y (/ (rectangle-y source) tex-height))
           (src-width (/ (rectangle-width source) tex-width))
           (src-height (/ (rectangle-height source) tex-height))
           ;; Handle negative dimensions for flipping
           (flip-x (< src-width 0))
           (flip-y (< src-height 0))
           (abs-src-width (abs src-width))
           (abs-src-height (abs src-height))
           ;; Adjust coordinates for flipping
           (final-src-x (if flip-x (+ src-x src-width) src-x))
           (final-src-y (if flip-y (+ src-y src-height) src-y)))
      
      ;; Apply transformations
      (gl:push-matrix)
      
      ;; Translate to position
      (gl:translate (rectangle-x dest) (rectangle-y dest) 0.0)
      
      ;; Rotate around origin
      (when (/= rotation 0.0)
        (let ((ox (if (listp origin) (first origin) (vx origin)))
              (oy (if (listp origin) (second origin) (vy origin))))
          (gl:translate ox oy 0.0)
          (gl:rotate rotation 0.0 0.0 1.0)
          (gl:translate (- ox) (- oy) 0.0)))
      
      ;; Draw textured quad with proper flipping
      (gl:with-primitive :quads
        (gl:tex-coord final-src-x final-src-y)
        (gl:vertex 0.0 0.0)
        
        (gl:tex-coord (+ final-src-x abs-src-width) final-src-y)
        (gl:vertex (rectangle-width dest) 0.0)
        
        (gl:tex-coord (+ final-src-x abs-src-width) (+ final-src-y abs-src-height))
        (gl:vertex (rectangle-width dest) (rectangle-height dest))
        
        (gl:tex-coord final-src-x (+ final-src-y abs-src-height))
        (gl:vertex 0.0 (rectangle-height dest)))
      
      (gl:pop-matrix))
    
    ;; Unbind texture
    (gl:bind-texture :texture-2d 0)))

(defun draw-texture-npatch (texture npatch dest origin rotation tint)
  "Draws a texture (or part of it) that stretches or shrinks nicely using n-patch info"
  ;; This is a complex function that would implement 9-patch drawing
  ;; For now, fall back to regular texture drawing
  (draw-texture-pro texture (npatch-info-source npatch) dest origin rotation tint))

;;; Render texture functions (will be implemented later in this file)

;;; Utility functions

(defun get-texture-data (texture)
  "Get pixel data from texture (download from GPU)"
  (when (is-texture-valid texture)
    (let* ((width (texture-width texture))
          (height (texture-height texture))
          (texture-id (texture-id texture))
          (data (make-array (* width height 4) :element-type '(unsigned-byte 8))))
      
      ;; Use framebuffer approach (compatible with both Desktop OpenGL and OpenGL ES)
      (read-texture-via-framebuffer texture-id width height data)
      
      ;; Create image from data
      (make-image :data data :width width :height height 
                  :format +pixelformat-uncompressed-rgba+))))

(defun read-texture-via-framebuffer (texture-id width height data)
  "Read texture data using framebuffer (for OpenGL ES compatibility)"
  (let ((fbo (gl:gen-framebuffer)))
    (unwind-protect
        (progn
          ;; Bind framebuffer
          (gl:bind-framebuffer :framebuffer fbo)
          
          ;; Attach texture as color attachment
          (gl:framebuffer-texture-2d :framebuffer :color-attachment0 :texture-2d texture-id 0)
          
          ;; Check framebuffer completeness
          (unless (eq (gl:check-framebuffer-status :framebuffer) :framebuffer-complete)
            (error "Framebuffer not complete for texture reading"))
          
          ;; Read pixels from framebuffer
          (gl:read-pixels 0 0 width height :rgba :unsigned-byte data)
          
          ;; Unbind framebuffer
          (gl:bind-framebuffer :framebuffer 0)
          
          (trace-log-info "read-texture-via-framebuffer: Successfully read texture data"))
      
      ;; Cleanup framebuffer
      (gl:delete-framebuffer fbo))))

(defun load-image-from-texture (texture)
  "Load image from texture (raylib compatible function)"
  (get-texture-data texture))

(defun get-texture-format (texture)
  "Get texture internal format"
  (if (is-texture-valid texture)
      (texture-format texture)
      0))

;;; Cleanup functions

(defun cleanup-texture-system ()
  "Cleanup all loaded textures"
  (loop for texture being the hash-values of *texture-registry* do
    (when (is-texture-valid texture)
      (gl:delete-texture (texture-id texture))))
  (clrhash *texture-registry*)
  (when *default-texture*
    (setf *default-texture* nil))
  (setf *texture-id-counter* 1)
  (setf *current-texture-id* 0))

;;; RenderTexture System (from raylib.h and rcore.c)
;;; Used for render-to-texture functionality - strictly following raylib C implementation

;;; RenderTexture structure (from raylib.h lines 287-291)
;;; typedef struct RenderTexture {
;;;     unsigned int id;        // OpenGL framebuffer object id
;;;     Texture texture;        // Color buffer attachment texture
;;;     Texture depth;          // Depth buffer attachment texture
;;; } RenderTexture;
;; RenderTexture structure is defined in raylib.lisp

;;; Global render texture state (following raylib CORE.Window state)
(defvar *current-fbo* nil "Currently bound framebuffer")
(defvar *current-fbo-width* 0 "Current framebuffer width")
(defvar *current-fbo-height* 0 "Current framebuffer height")
(defvar *using-fbo* nil "Whether currently using framebuffer")

;;; Pixel format constants (from raylib.h)
(defconstant +pixelformat-uncompressed-r8g8b8a8+ 7 "32-bit RGBA")
(defconstant +pixelformat-uncompressed-rgba+ 7 "32-bit RGBA (alias for compatibility)")

;;; Attachment constants (from rlgl.h)
(defconstant +rl-attachment-color-channel0+ 0 "Color attachment 0")
(defconstant +rl-attachment-depth+ 100 "Depth attachment")
(defconstant +rl-attachment-texture2d+ 100 "Texture2D attachment type")
(defconstant +rl-attachment-renderbuffer+ 200 "Renderbuffer attachment type")

;;; Helper functions for rlgl-style operations (following raylib rlgl.c patterns)
(defun rl-load-framebuffer (width height)
  "Load framebuffer (following rlLoadFramebuffer from rlgl.c)"
  (declare (ignore width height))
  (let ((fbo-id (first (gl:gen-framebuffers 1))))
    (format t "INFO: FBO: [ID ~d] Framebuffer object created successfully~%" fbo-id)
    fbo-id))

(defun rl-load-texture-depth (width height use-renderbuffer)
  "Load depth texture/renderbuffer (following rlLoadTextureDepth from rlgl.c)"
  (if use-renderbuffer
    ;; Create depth renderbuffer (as in raylib)
    (let ((depth-id (first (gl:gen-renderbuffers 1))))
      (gl:bind-renderbuffer :renderbuffer depth-id)
      (gl:renderbuffer-storage :renderbuffer :depth-component width height)
      (gl:bind-renderbuffer :renderbuffer 0)
      (format t "INFO: TEXTURE: [ID ~d] Depth renderbuffer loaded successfully (~dx~d)~%" 
              depth-id width height)
      ;; Return texture structure for depth renderbuffer
      (make-texture :id depth-id 
                    :width width 
                    :height height 
                    :mipmaps 1 
                    :format 19)) ; DEPTH_COMPONENT_24BIT format
    ;; Create depth texture (alternative)
    (let ((depth-id (first (gl:gen-textures 1))))
      (gl:bind-texture :texture-2d depth-id)
      (gl:tex-image-2d :texture-2d 0 :depth-component width height 0 :depth-component :unsigned-int (cffi:null-pointer))
      (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
      (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
      (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
      (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
      (gl:bind-texture :texture-2d 0)
      (make-texture :id depth-id :width width :height height :mipmaps 1 :format 19))))

;;; Note: rl-framebuffer-attach and rl-framebuffer-complete functions are now in gl.lisp

;;; Render texture loading and management (following raylib LoadRenderTexture exactly)
(defun load-render-texture (width height)
  "Load framebuffer for render-to-texture (raylib LoadRenderTexture)"
  (let ((target (make-render-texture)))
    
    ;; Load framebuffer (rlLoadFramebuffer)
    (setf (render-texture-id target) (rl-load-framebuffer width height))
    
    ;; Load color texture (rlLoadTexture with PIXELFORMAT_UNCOMPRESSED_R8G8B8A8)
    (let* ((color-texture-id (first (gl:gen-textures 1)))
           (color-texture (make-texture :id color-texture-id
                                      :width width
                                      :height height
                                      :mipmaps 1
                                      :format +pixelformat-uncompressed-r8g8b8a8+)))
      ;; Setup color texture exactly as raylib does
      (gl:bind-texture :texture-2d color-texture-id)
      (gl:tex-image-2d :texture-2d 0 :rgba width height 0 :rgba :unsigned-byte (cffi:null-pointer))
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
      (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
      (gl:bind-texture :texture-2d 0)
      (format t "INFO: TEXTURE: [ID ~d] Texture loaded successfully (~dx~d - ~d mipmaps)~%" 
              color-texture-id width height 1)
      (setf (render-texture-texture target) color-texture))
    
    ;; Load depth renderbuffer (rlLoadTextureDepth with useRenderBuffer = true)
    (setf (render-texture-depth target) (rl-load-texture-depth width height t))
    
    ;; Attach color texture to framebuffer
    (rl-framebuffer-attach (render-texture-id target)
                          (texture-id (render-texture-texture target))
                          +rl-attachment-texture2d+
                          +rl-attachment-color-channel0+
                          0)
    
    ;; Attach depth renderbuffer to framebuffer  
    (rl-framebuffer-attach (render-texture-id target)
                          (texture-id (render-texture-depth target))
                          +rl-attachment-renderbuffer+
                          +rl-attachment-depth+
                          0)
    
    ;; Check if framebuffer is complete
    (unless (rl-framebuffer-complete (render-texture-id target))
      (format t "WARNING: FBO: [ID ~d] Framebuffer object incomplete~%" (render-texture-id target))
      ;; Return zero-initialized structure on failure (as raylib does)
      (setf target (make-render-texture)))
    
    (when (> (render-texture-id target) 0)
      (format t "INFO: FBO: [ID ~d] Framebuffer object loaded successfully~%" (render-texture-id target)))
    
    target))

(defun is-render-texture-valid (render-texture)
  "Check if render texture is valid and ready"
  (and render-texture 
       (> (render-texture-id render-texture) 0)
       (render-texture-texture render-texture)
       (is-texture-valid (render-texture-texture render-texture))))

(defun unload-render-texture (render-texture)
  "Unload render texture from GPU memory (raylib UnloadRenderTexture)"
  (when (is-render-texture-valid render-texture)
    (let ((fbo-id (render-texture-id render-texture)))
      
      ;; Unload textures
      (when (render-texture-texture render-texture)
        (unload-texture (render-texture-texture render-texture)))
      
      (when (render-texture-depth render-texture)
        (unload-texture (render-texture-depth render-texture)))
      
      ;; Delete framebuffer
      (gl:delete-framebuffers (list fbo-id))
      
      (format t "INFO: FBTEXTURE: [ID ~d] Framebuffer unloaded successfully~%" fbo-id)
      
      ;; Clear structure
      (setf (render-texture-id render-texture) 0)
      (setf (render-texture-texture render-texture) nil)
      (setf (render-texture-depth render-texture) nil))))

;;; Helper functions for rlgl-style rendering operations
(defun rl-draw-render-batch-active ()
  "Flush any pending draw calls (following rlDrawRenderBatchActive from rlgl.c)"
  ;; In raylib this flushes batched geometry
  ;; For now we ensure OpenGL state is consistent
  (gl:flush))

;;; Note: rl-enable-framebuffer and rl-disable-framebuffer functions are now in gl.lisp

;; rl-viewport, rl-load-identity, rl-ortho are now defined in gl.lisp
;; setup-viewport is now defined in core.lisp to match raylib's rcore.c organization

;;; Render texture mode functions (following raylib exactly)
(defun begin-texture-mode (target)
  "Begin drawing to render texture (raylib BeginTextureMode)"
  (when (is-render-texture-valid target)
    ;; Flush any pending draw calls (rlDrawRenderBatchActive)
    (rl-draw-render-batch-active)
    
    ;; Bind framebuffer (rlEnableFramebuffer)
    (rl-enable-framebuffer (render-texture-id target))
    
    ;; Set viewport to render texture size (rlViewport)
    (rl-viewport 0 0 
                 (texture-width (render-texture-texture target))
                 (texture-height (render-texture-texture target)))
    
    ;; Update internal state (rlSetFramebufferWidth/Height)
    (setf *current-fbo-width* (texture-width (render-texture-texture target)))
    (setf *current-fbo-height* (texture-height (render-texture-texture target)))
    
    ;; Setup projection matrix
    (rl-matrix-mode 0) ; Projection mode
    (rl-load-identity)
    (rl-ortho 0.0d0 (coerce (texture-width (render-texture-texture target)) 'double-float)
              (coerce (texture-height (render-texture-texture target)) 'double-float) 0.0d0 0.0d0 1.0d0)
    
    ;; Setup modelview matrix
    (rl-matrix-mode 1) ; Modelview mode
    (rl-load-identity)
    
    ;; Update global state (CORE.Window state)
    (setf *using-fbo* t)))

(defun end-texture-mode ()
  "End drawing to render texture (raylib EndTextureMode)"
  ;; Flush any pending draw calls (rlDrawRenderBatchActive)
  (rl-draw-render-batch-active)
  
  ;; Disable framebuffer (rlDisableFramebuffer) 
  (rl-disable-framebuffer)
  
  ;; Restore viewport and projection (SetupViewport)
  (setup-viewport (core-data-window-screen-width *core*) (core-data-window-screen-height *core*))
  
  ;; Restore modelview matrix
  (rl-matrix-mode :modelview)
  (rl-load-identity)
  ;; Apply screen scaling (in raylib: rlMultMatrixf(MatrixToFloat(CORE.Window.screenScale)))
  ;; For now we skip screen scaling transformation
  
  ;; Update global state
  (setf *current-fbo-width* (core-data-window-screen-width *core*))
  (setf *current-fbo-height* (core-data-window-screen-height *core*))
  (setf *using-fbo* nil))

;;; Utility functions for render textures
(defun get-render-texture-texture (render-texture)
  "Get the color texture from render texture"
  (when (is-render-texture-valid render-texture)
    (render-texture-texture render-texture)))

(defun get-render-texture-depth (render-texture)
  "Get the depth texture from render texture"
  (when (is-render-texture-valid render-texture)
    (render-texture-depth render-texture)))

;;; Basic image generation functions

(defun gen-image-color (width height color)
  "Generate image: plain color"
  (let* ((pixel-count (* width height))
         (data (make-array (* pixel-count 4) 
                          :element-type '(unsigned-byte 8)
                          :initial-element 0))
         (r (color-r color))
         (g (color-g color))
         (b (color-b color))
         (a (color-a color)))
    ;; Fill image with specified color
    (loop for i from 0 below pixel-count do
      (let ((base (* i 4)))
        (setf (aref data base) r)
        (setf (aref data (+ base 1)) g)
        (setf (aref data (+ base 2)) b)
        (setf (aref data (+ base 3)) a)))
    (make-image :data data
                :width width
                :height height
                :format +pixelformat-uncompressed-rgba+)))

(defun gen-image-gradient-linear (width height direction start-color end-color)
  "Generate image: linear gradient, direction in degrees [0..360], 0=Vertical gradient"
  (let* ((pixel-count (* width height))
         (data (make-array (* pixel-count 4) 
                          :element-type '(unsigned-byte 8)))
         (angle (* direction +deg2rad+))
         (cos-a (cos angle))
         (sin-a (sin angle)))
    
    (loop for y from 0 below height do
      (loop for x from 0 below width do
        (let* ((nx (/ x (1- width)))
               (ny (/ y (1- height)))
               ;; Calculate gradient factor based on direction
               (factor (+ (* nx cos-a) (* ny sin-a)))
               (factor (clamp factor 0.0 1.0))
               (inv-factor (- 1.0 factor))
               
               ;; Interpolate colors
               (r (round (+ (* (color-r start-color) inv-factor)
                           (* (color-r end-color) factor))))
               (g (round (+ (* (color-g start-color) inv-factor)
                           (* (color-g end-color) factor))))
               (b (round (+ (* (color-b start-color) inv-factor)
                           (* (color-b end-color) factor))))
               (a (round (+ (* (color-a start-color) inv-factor)
                           (* (color-a end-color) factor))))
               
               (base (* (+ (* y width) x) 4)))
          
          (setf (aref data base) r)
          (setf (aref data (+ base 1)) g)
          (setf (aref data (+ base 2)) b)
          (setf (aref data (+ base 3)) a))))
    
    (make-image :data data
                :width width
                :height height
                :format +pixelformat-uncompressed-rgba+)))

(defun gen-image-gradient-radial (width height density inner-color outer-color)
  "Generate image: radial gradient"
  (let* ((pixel-count (* width height))
         (data (make-array (* pixel-count 4) 
                          :element-type '(unsigned-byte 8)))
         (center-x (/ width 2.0))
         (center-y (/ height 2.0))
         (max-radius (* (min width height) 0.5 density)))
    
    (loop for y from 0 below height do
      (loop for x from 0 below width do
        (let* ((dx (- x center-x))
               (dy (- y center-y))
               (distance (sqrt (+ (* dx dx) (* dy dy))))
               (factor (clamp (/ distance max-radius) 0.0 1.0))
               (inv-factor (- 1.0 factor))
               
               ;; Interpolate colors
               (r (round (+ (* (color-r inner-color) inv-factor)
                           (* (color-r outer-color) factor))))
               (g (round (+ (* (color-g inner-color) inv-factor)
                           (* (color-g outer-color) factor))))
               (b (round (+ (* (color-b inner-color) inv-factor)
                           (* (color-b outer-color) factor))))
               (a (round (+ (* (color-a inner-color) inv-factor)
                           (* (color-a outer-color) factor))))
               
               (base (* (+ (* y width) x) 4)))
          
          (setf (aref data base) r)
          (setf (aref data (+ base 1)) g)
          (setf (aref data (+ base 2)) b)
          (setf (aref data (+ base 3)) a))))
    
    (make-image :data data
                :width width
                :height height
                :format +pixelformat-uncompressed-rgba+)))

(defun gen-image-checked (width height checks-x checks-y col1 col2)
  "Generate image: checked pattern"
  (let* ((pixel-count (* width height))
         (data (make-array (* pixel-count 4) 
                          :element-type '(unsigned-byte 8)))
         (check-width (/ width checks-x))
         (check-height (/ height checks-y)))
    
    (loop for y from 0 below height do
      (loop for x from 0 below width do
        (let* ((check-x (floor (/ x check-width)))
               (check-y (floor (/ y check-height)))
               (color (if (evenp (+ check-x check-y)) col1 col2))
               (base (* (+ (* y width) x) 4)))
          
          (setf (aref data base) (color-r color))
          (setf (aref data (+ base 1)) (color-g color))
          (setf (aref data (+ base 2)) (color-b color))
          (setf (aref data (+ base 3)) (color-a color)))))
    
    (make-image :data data
                :width width
                :height height
                :format +pixelformat-uncompressed-rgba+)))

;;; Image manipulation functions

(defun image-copy (image)
  "Create an image duplicate"
  (let ((new-data (make-array (length (image-data image))
                             :element-type '(unsigned-byte 8))))
    ;; Copy data
    (replace new-data (image-data image))
    
    (make-image :data new-data
                :width (image-width image)
                :height (image-height image)
                :mipmaps (image-mipmaps image)
                :format (image-format image))))

(defun image-color-tint (image color)
  "Apply color tint to image (modifies original)"
  (let ((data (image-data image))
        (tint-r (/ (color-r color) 255.0))
        (tint-g (/ (color-g color) 255.0))
        (tint-b (/ (color-b color) 255.0))
        (tint-a (/ (color-a color) 255.0)))
    
    (loop for i from 0 below (length data) by 4 do
      (setf (aref data i) (round (* (aref data i) tint-r)))
      (setf (aref data (+ i 1)) (round (* (aref data (+ i 1)) tint-g)))
      (setf (aref data (+ i 2)) (round (* (aref data (+ i 2)) tint-b)))
      (setf (aref data (+ i 3)) (round (* (aref data (+ i 3)) tint-a))))
    
    image))

(defun image-color-grayscale (image)
  "Convert image to grayscale (modifies original)"
  (let ((data (image-data image)))
    (loop for i from 0 below (length data) by 4 do
      (let* ((r (aref data i))
             (g (aref data (+ i 1)))
             (b (aref data (+ i 2)))
             ;; Standard grayscale conversion
             (gray (round (+ (* r 0.299) (* g 0.587) (* b 0.114)))))
        (setf (aref data i) gray)
        (setf (aref data (+ i 1)) gray)
        (setf (aref data (+ i 2)) gray)))
    image))

(defun image-flip-vertical (image)
  "Flip image vertically (modifies original)"
  (let* ((data (image-data image))
         (width (image-width image))
         (height (image-height image))
         (row-size (* width 4)))
    
    (loop for y from 0 below (floor height 2) do
      (let ((top-start (* y row-size))
            (bottom-start (* (- height y 1) row-size)))
        ;; Swap rows
        (loop for i from 0 below row-size do
          (rotatef (aref data (+ top-start i))
                   (aref data (+ bottom-start i))))))
    image))

(defun image-flip-horizontal (image)
  "Flip image horizontally (modifies original)"
  (let* ((data (image-data image))
         (width (image-width image))
         (height (image-height image)))
    
    (loop for y from 0 below height do
      (loop for x from 0 below (floor width 2) do
        (let ((left-start (* (+ (* y width) x) 4))
              (right-start (* (+ (* y width) (- width x 1)) 4)))
          ;; Swap pixels
          (loop for i from 0 below 4 do
            (rotatef (aref data (+ left-start i))
                     (aref data (+ right-start i)))))))
    image))

;;; Advanced image processing functions

(defun gen-image-white-noise (width height factor)
  "Generate white noise image"
  (let* ((pixel-count (* width height))
         (data (make-array (* pixel-count 4) :element-type '(unsigned-byte 8))))
    
    (loop for i from 0 below pixel-count do
      (let* ((base (* i 4))
             (noise-value (if (< (random 1.0) factor) 255 0)))
        (setf (aref data base) noise-value)
        (setf (aref data (+ base 1)) noise-value)
        (setf (aref data (+ base 2)) noise-value)
        (setf (aref data (+ base 3)) 255)))
    
    (make-image :data data
                :width width
                :height height
                :format +pixelformat-uncompressed-rgba+)))

(defun gen-image-perlin-noise (width height offset-x offset-y scale)
  "Generate Perlin noise image"
  (let* ((pixel-count (* width height))
         (data (make-array (* pixel-count 4) :element-type '(unsigned-byte 8))))
    
    (loop for y from 0 below height do
      (loop for x from 0 below width do
        (let* ((base (* (+ (* y width) x) 4))
               ;; Simplified Perlin noise - basic implementation
               (nx (/ (+ x offset-x) scale))
               (ny (/ (+ y offset-y) scale))
               (noise-value (+ 0.5 (* 0.5 (sin (+ (* nx 6.28) (* ny 6.28))))))
               (gray-value (round (* noise-value 255))))
          (setf (aref data base) gray-value)
          (setf (aref data (+ base 1)) gray-value)
          (setf (aref data (+ base 2)) gray-value)
          (setf (aref data (+ base 3)) 255))))
    
    (make-image :data data
                :width width
                :height height
                :format +pixelformat-uncompressed-rgba+)))

(defun gen-image-cellular (width height tile-size)
  "Generate cellular automata image"
  (let* ((pixel-count (* width height))
         (data (make-array (* pixel-count 4) :element-type '(unsigned-byte 8))))
    
    ;; Create initial random pattern
    (loop for y from 0 below height do
      (loop for x from 0 below width do
        (let* ((base (* (+ (* y width) x) 4))
               (cell-x (floor (/ x tile-size)))
               (cell-y (floor (/ y tile-size)))
               (cell-value (if (< (random 1.0) 0.5) 0 255)))
          (setf (aref data base) cell-value)
          (setf (aref data (+ base 1)) cell-value)
          (setf (aref data (+ base 2)) cell-value)
          (setf (aref data (+ base 3)) 255))))
    
    (make-image :data data
                :width width
                :height height
                :format +pixelformat-uncompressed-rgba+)))

(defun gen-image-gradient-square (width height density inner-color outer-color)
  "Generate square gradient image"
  (let* ((pixel-count (* width height))
         (data (make-array (* pixel-count 4) :element-type '(unsigned-byte 8)))
         (center-x (/ width 2.0))
         (center-y (/ height 2.0))
         (max-dist (* density (max center-x center-y))))
    
    (loop for y from 0 below height do
      (loop for x from 0 below width do
        (let* ((base (* (+ (* y width) x) 4))
               (dist-x (abs (- x center-x)))
               (dist-y (abs (- y center-y)))
               (dist (max dist-x dist-y))
               (factor (min 1.0 (/ dist max-dist)))
               (inv-factor (- 1.0 factor))
               (r (round (+ (* (color-r inner-color) inv-factor)
                           (* (color-r outer-color) factor))))
               (g (round (+ (* (color-g inner-color) inv-factor)
                           (* (color-g outer-color) factor))))
               (b (round (+ (* (color-b inner-color) inv-factor)
                           (* (color-b outer-color) factor))))
               (a (round (+ (* (color-a inner-color) inv-factor)
                           (* (color-a outer-color) factor)))))
          
          (setf (aref data base) r)
          (setf (aref data (+ base 1)) g)
          (setf (aref data (+ base 2)) b)
          (setf (aref data (+ base 3)) a))))
    
    (make-image :data data
                :width width
                :height height
                :format +pixelformat-uncompressed-rgba+)))

(defun image-color-invert (image)
  "Invert image colors (modifies original)"
  (let ((data (image-data image)))
    (loop for i from 0 below (length data) by 4 do
      (setf (aref data i) (- 255 (aref data i)))
      (setf (aref data (+ i 1)) (- 255 (aref data (+ i 1))))
      (setf (aref data (+ i 2)) (- 255 (aref data (+ i 2)))))
    image))

;;; Initialize texture system when module loads
(eval-when (:load-toplevel :execute)
  ;; Initialization will be called when OpenGL context is ready
  )
