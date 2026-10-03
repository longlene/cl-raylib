;;;; raygui.lisp - raygui bindings for cl-raylib
;;;; Basic immediate mode GUI library bindings

(in-package #:cl-raylib)

;;; GUI State Constants
(defconstant +gui-state-normal+ 0)
(defconstant +gui-state-focused+ 1)
(defconstant +gui-state-pressed+ 2)
(defconstant +gui-state-disabled+ 3)

;;; GUI Control IDs
(defconstant +gui-control-default+ 0)
(defconstant +gui-control-label+ 1)
(defconstant +gui-control-button+ 2)
(defconstant +gui-control-toggle+ 3)
(defconstant +gui-control-slider+ 4)
(defconstant +gui-control-progressbar+ 5)
(defconstant +gui-control-checkbox+ 6)
(defconstant +gui-control-combobox+ 7)
(defconstant +gui-control-dropdownbox+ 8)
(defconstant +gui-control-textbox+ 9)
(defconstant +gui-control-valuebox+ 10)
(defconstant +gui-control-spinner+ 11)
(defconstant +gui-control-listview+ 12)
(defconstant +gui-control-colorpicker+ 13)
(defconstant +gui-control-scrollbar+ 14)
(defconstant +gui-control-statusbar+ 15)

;;; Global GUI state
(defvar *gui-state* +gui-state-normal+ "Global GUI state")
(defvar *gui-alpha* 1.0 "Global GUI alpha transparency")
(defvar *gui-locked* nil "Global GUI lock state")

;;; Basic GUI State Management
(defun gui-enable ()
  "Enable gui controls (global state)"
  (setf *gui-state* +gui-state-normal+))

(defun gui-disable ()
  "Disable gui controls (global state)"
  (setf *gui-state* +gui-state-disabled+))

(defun gui-lock ()
  "Lock gui controls (global state)"
  (setf *gui-locked* t))

(defun gui-unlock ()
  "Unlock gui controls (global state)"
  (setf *gui-locked* nil))

(defun gui-is-locked ()
  "Check if gui is locked (global state)"
  *gui-locked*)

(defun gui-set-alpha (alpha)
  "Set gui controls alpha (global state), alpha goes from 0.0 to 1.0"
  (setf *gui-alpha* (max 0.0 (min 1.0 alpha))))

(defun gui-set-state (state)
  "Set gui state (global state)"
  (setf *gui-state* state))

(defun gui-get-state ()
  "Get gui state (global state)"
  *gui-state*)

;;; Basic GUI Controls (Pure Common Lisp implementation)

(defun gui-button (bounds text)
  "Button control, returns true when clicked"
  (let ((mouse-point (get-mouse-position))
        (state +gui-state-normal+)
        (pressed nil))
    
    ;; Check button state
    (when (and (>= (vx mouse-point) (rectangle-x bounds))
               (<= (vx mouse-point) (+ (rectangle-x bounds) (rectangle-width bounds)))
               (>= (vy mouse-point) (rectangle-y bounds))
               (<= (vy mouse-point) (+ (rectangle-y bounds) (rectangle-height bounds))))
      (if (is-mouse-button-down +mouse-button-left+)
          (setf state +gui-state-pressed+)
          (setf state +gui-state-focused+))
      
      (when (is-mouse-button-released +mouse-button-left+)
        (setf pressed t)))
    
    ;; Draw control
    (let ((color (case state
                   (#.+gui-state-normal+ +lightgray+)
                   (#.+gui-state-focused+ +gray+)
                   (#.+gui-state-pressed+ +darkgray+)
                   (#.+gui-state-disabled+ (fade +lightgray+ 0.3))
                   (t +lightgray+))))
      (draw-rectangle-rec bounds (fade color *gui-alpha*)))
    
    ;; Draw border
    (draw-rectangle-lines-ex bounds 2 (fade +gray+ *gui-alpha*))
    
    ;; Draw text
    (let* ((text-width (measure-text text 10))
           (text-x (+ (rectangle-x bounds) (/ (- (rectangle-width bounds) text-width) 2)))
           (text-y (+ (rectangle-y bounds) (/ (- (rectangle-height bounds) 10) 2))))
      (draw-text text (truncate text-x) (truncate text-y) 10 
                 (fade (if (= state +gui-state-disabled+) +gray+ +darkgray+) *gui-alpha*)))
    
    pressed))

(defun gui-label (bounds text)
  "Label control"
  (let* ((text-width (measure-text text 10))
         (text-height 10)
         (text-x (+ (rectangle-x bounds) (/ (- (rectangle-width bounds) text-width) 2)))
         (text-y (+ (rectangle-y bounds) (/ (- (rectangle-height bounds) text-height) 2))))
    (draw-text text (truncate text-x) (truncate text-y) 10 
               (fade +darkgray+ *gui-alpha*))))

(defun gui-checkbox (bounds text checked)
  "Check Box control, returns new checked state"
  (let ((mouse-point (get-mouse-position))
        (state +gui-state-normal+))
    
    ;; Check mouse interaction (same logic as button)
    (when (and (>= (vx mouse-point) (rectangle-x bounds))
               (<= (vx mouse-point) (+ (rectangle-x bounds) (rectangle-width bounds)))
               (>= (vy mouse-point) (rectangle-y bounds))
               (<= (vy mouse-point) (+ (rectangle-y bounds) (rectangle-height bounds))))
      (if (is-mouse-button-down +mouse-button-left+)
          (setf state +gui-state-pressed+)
          (setf state +gui-state-focused+))
      
      (when (is-mouse-button-released +mouse-button-left+)
        (setf checked (not checked))))
    
    ;; Draw checkbox background (different color when checked)
    (draw-rectangle-rec bounds (fade (if checked +lightgray+ +white+) *gui-alpha*))
    
    ;; Draw checkbox border with state-based color
    (draw-rectangle-lines-ex bounds 2 
                            (fade (case state
                                    (#.+gui-state-focused+ +maroon+)
                                    (#.+gui-state-pressed+ +darkgreen+)
                                    (t (if checked +maroon+ +gray+)))
                                  *gui-alpha*))
    
    ;; Draw check mark if checked (more prominent)
    (when checked
      (draw-rectangle (truncate (+ (rectangle-x bounds) 2)) 
                      (truncate (+ (rectangle-y bounds) 2))
                      (truncate (- (rectangle-width bounds) 4))
                      (truncate (- (rectangle-height bounds) 4))
                      (fade +maroon+ *gui-alpha*))
      ;; Draw an X or checkmark pattern for better visibility
      (draw-rectangle (truncate (+ (rectangle-x bounds) 4))
                      (truncate (+ (rectangle-y bounds) (/ (rectangle-height bounds) 2) -1))
                      (truncate (- (rectangle-width bounds) 8))
                      2
                      (fade +white+ *gui-alpha*))
      (draw-rectangle (truncate (+ (rectangle-x bounds) (/ (rectangle-width bounds) 2) -1))
                      (truncate (+ (rectangle-y bounds) 4))
                      2
                      (truncate (- (rectangle-height bounds) 8))
                      (fade +white+ *gui-alpha*)))
    
    ;; Draw text
    (draw-text text 
               (truncate (+ (rectangle-x bounds) (rectangle-width bounds) 5))
               (truncate (+ (rectangle-y bounds) (/ (rectangle-height bounds) 2) -5))
               10 
               (fade +darkgray+ *gui-alpha*))
    
    checked))

(defun gui-slider (bounds text-left text-right value min-value max-value)
  "Slider control, returns new value (following raylib GuiSlider implementation)"
  (let* ((mouse-point (get-mouse-position))
         (state +gui-state-normal+)
         (slider-value value)
         (slider-width 16)  ; raylib SLIDER_WIDTH default
         (border-width 2)
         ;; Mouse over check
         (mouse-over (and (>= (vx mouse-point) (rectangle-x bounds))
                          (<= (vx mouse-point) (+ (rectangle-x bounds) (rectangle-width bounds)))
                          (>= (vy mouse-point) (rectangle-y bounds))
                          (<= (vy mouse-point) (+ (rectangle-y bounds) (rectangle-height bounds))))))
    
    ;; Update state and value (following raylib logic)
    (cond
      ;; If mouse is pressed and over slider, enter pressed mode
      ((and mouse-over (is-mouse-button-pressed +mouse-button-left+))
       (setf state +gui-state-pressed+))
      ;; If mouse is down (from previous press), stay in pressed mode even if outside bounds
      ((is-mouse-button-down +mouse-button-left+)
       (setf state +gui-state-pressed+))
      ;; If mouse is over but not pressed, enter focused mode  
      (mouse-over
       (setf state +gui-state-focused+)))
    
    ;; Update slider value if in pressed mode (raylib exclusive mode behavior)
    (when (= state +gui-state-pressed+)
      (let* ((mouse-x (vx mouse-point))
             ;; Calculate normalized position (raylib formula)
             (relative-x (- mouse-x (rectangle-x bounds) (/ slider-width 2)))
             (usable-width (- (rectangle-width bounds) slider-width))
             (normalized (max 0.0 (min 1.0 (/ relative-x usable-width)))))
        (setf slider-value (+ min-value (* normalized (- max-value min-value))))))
    
    ;; Draw slider background (raylib style)
    (draw-rectangle-rec bounds (fade +lightgray+ *gui-alpha*))
    (draw-rectangle-lines-ex bounds border-width (fade +gray+ *gui-alpha*))
    
    ;; Calculate slider position (raylib positioning)
    (let* ((normalized-pos (if (= min-value max-value) 
                               0.0 
                               (/ (- slider-value min-value) (- max-value min-value))))
           (slider-pos (+ (rectangle-x bounds) border-width
                          (* normalized-pos (- (rectangle-width bounds) slider-width (* 2 border-width)))))
           (slider-rect (make-rectangle 
                         :x slider-pos
                         :y (+ (rectangle-y bounds) border-width)
                         :width (float (- slider-width (* 2 border-width)))
                         :height (- (rectangle-height bounds) (* 2 border-width)))))
      
      ;; Draw slider handle with state-based color
      (draw-rectangle-rec slider-rect 
                          (fade (case state
                                  (#.+gui-state-normal+ +darkgray+)
                                  (#.+gui-state-focused+ +gray+)
                                  (#.+gui-state-pressed+ +maroon+)
                                  (t +darkgray+))
                                *gui-alpha*)))
    
    ;; Draw text labels (raylib style with padding)
    (when text-left
      (draw-text text-left 
                 (- (truncate (rectangle-x bounds)) (measure-text text-left 10) 5)
                 (truncate (+ (rectangle-y bounds) (/ (rectangle-height bounds) 2) -5))
                 10 (fade +gray+ *gui-alpha*)))
    
    (when text-right
      (draw-text text-right
                 (+ (truncate (+ (rectangle-x bounds) (rectangle-width bounds))) 5)
                 (truncate (+ (rectangle-y bounds) (/ (rectangle-height bounds) 2) -5))
                 10 (fade +gray+ *gui-alpha*)))
    
    slider-value))

(defun gui-progress-bar (bounds text-left text-right value min-value max-value)
  "Progress Bar control (following raylib GuiProgressBar implementation)"
  (let* ((border-width 2)
         (padding 1)  ; raylib PROGRESS_PADDING default
         ;; Calculate progress (raylib formula)
         (progress (if (= min-value max-value) 
                       0.0 
                       (max 0.0 (min 1.0 (/ (- value min-value) (- max-value min-value))))))
         ;; Calculate progress width (raylib calculation)
         (progress-width (* progress (- (rectangle-width bounds) (* 2 border-width))))
         ;; Progress rectangle (raylib positioning) 
         (progress-rect (make-rectangle
                         :x (+ (rectangle-x bounds) border-width)
                         :y (+ (rectangle-y bounds) border-width padding)
                         :width progress-width
                         :height (- (rectangle-height bounds) (* 2 border-width) (* 2 padding)))))
    
    ;; Draw progress bar background (raylib style)
    (draw-rectangle-rec bounds (fade +lightgray+ *gui-alpha*))
    (draw-rectangle-lines-ex bounds border-width (fade +gray+ *gui-alpha*))
    
    ;; Draw progress fill (raylib coloring)
    (when (> progress-width 1.0)  ; Only draw if meaningful progress
      (draw-rectangle-rec progress-rect (fade +maroon+ *gui-alpha*)))
    
    ;; Draw text labels (raylib style with padding)
    (when text-left
      (draw-text text-left 
                 (- (truncate (rectangle-x bounds)) (measure-text text-left 10) 5)
                 (truncate (+ (rectangle-y bounds) (/ (rectangle-height bounds) 2) -5))
                 10 (fade +gray+ *gui-alpha*)))
    
    (when text-right
      (draw-text text-right
                 (+ (truncate (+ (rectangle-x bounds) (rectangle-width bounds))) 5)
                 (truncate (+ (rectangle-y bounds) (/ (rectangle-height bounds) 2) -5))
                 10 (fade +gray+ *gui-alpha*)))))
